(* Schema coverage: every sum-type variant and every optional field the schema reaches from the
   entity root must be used by at least one entity of the data, or be named, with a reason, in
   the allowlist file. It reads the schema through the emitter's context (same modules, same name
   resolution, same annotations) and the data as raw JSON, walked by the schema's types. The
   allowlist can only shrink: a row naming something that is now used, or that is gone, fails. *)

open Types
open Annotations
open Generator_types

type kind =
  | Variant
  | Field

type item =
  { type_key : string * string; (* (module, ATD type) *)
    kind : kind;
    name : string (* the variant tag or the field's JSON name, as written in the data *)
  }

(* The allowlist key of an item: `module.type.name` *)
let key_of_item item =
  Printf.sprintf "%s.%s.%s" (fst item.type_key) (snd item.type_key) item.name
;;

let describe item =
  Printf.sprintf
    "%s `%s` of `%s.%s`"
    (match item.kind with
     | Variant -> "variant"
     | Field -> "optional field")
    item.name
    (fst item.type_key)
    (snd item.type_key)
;;

let json_name annot name = Option.value (get_json annot "name") ~default:name

let is_option = function
  | Atd.Ast.Option _ -> true
  | _ -> false
;;

(* A field that may be left out: `?f`, or a required `f : T option` *)
let is_optional_field kind te = kind = Atd.Ast.Optional || (kind = Atd.Ast.Required && is_option te)

(* ---- the schema side ---------------------------------------------------- *)

let items_of_entry (entry : entry) : item list =
  let item kind name = { type_key = key_of entry; kind; name } in
  match entry.expr with
  | Atd.Ast.Record (_, fields, _) ->
    List.filter_map
      (function
        | `Field (_, (name, kind, annot), te) when (not (is_ignored annot)) && is_optional_field kind te ->
          Some (item Field (json_name annot name))
        | _ -> None)
      fields
  | Atd.Ast.Sum (_, vs, _) -> List.map (fun v -> item Variant v.tag) (variants_of entry vs)
  | _ -> []
;;

(* Every defining entry reachable from [root] *)
let reachable ctx root : entry list =
  let seen = Hashtbl.create 64 in
  let rec visit (entry : entry) =
    if not (Hashtbl.mem seen (key_of entry))
    then (
      Hashtbl.replace seen (key_of entry) entry;
      List.iter
        (fun name ->
          Option.iter visit (resolve ctx ~module_name:entry.module_name name))
        (Main.names_in ~through_lists:true entry.expr))
  in
  Option.iter visit (resolve ctx ~module_name:(fst root) (snd root));
  Hashtbl.fold (fun _ e acc -> e :: acc) seen []
;;

let schema_items ctx root = List.concat_map items_of_entry (reachable ctx root)

(* ---- the data side: walk the JSON by the schema's types ----------------- *)

let tag_and_payload adapter (json : Yojson.Safe.t) : (string * Yojson.Safe.t option) option =
  match adapter, json with
  | _, `String tag -> Some (tag, None)
  | Some Type_field, `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String tag) -> Some (tag, Some json)
     | _ -> None)
  | None, `List [ `String tag ] -> Some (tag, None)
  | None, `List [ `String tag; payload ] -> Some (tag, Some payload)
  | _ -> None
;;

let rec walk ctx used ~module_name (te : Atd.Ast.type_expr) (json : Yojson.Safe.t) : unit =
  let recur = walk ctx used ~module_name in
  match te, json with
  | ( Atd.Ast.List
        ( _,
          Atd.Ast.Tuple (_, [ (_, Atd.Ast.Name (_, (_, "string", []), _), _); (_, value, _) ], _),
          annot ),
      `Assoc entries )
    when get_json annot "repr" = Some "object" ->
    List.iter (fun (_, v) -> recur value v) entries
  | Atd.Ast.List (_, a, _), `List items -> List.iter (recur a) items
  | (Atd.Ast.Option (_, a, _) | Atd.Ast.Nullable (_, a, _)), _ ->
    Option.iter (recur a) (some_of_atd_option json)
  | (Atd.Ast.Shared (_, a, _) | Atd.Ast.Wrap (_, a, _)), _ -> recur a json
  | Atd.Ast.Tuple (_, cells, _), `List items when List.length cells = List.length items ->
    List.iter2 (fun (_, t, _) j -> recur t j) cells items
  | Atd.Ast.Name (_, (_, name, _), _), _ when not (Utils.is_primitive name) ->
    Option.iter (fun e -> walk_entry ctx used e json) (resolve ctx ~module_name name)
  | _ -> ()

(* The value inside atdgen's `"None"` / `["Some", v]` *)
and some_of_atd_option (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match json with
  | `Null | `String "None" -> None
  | `List [ `String "Some"; v ] -> Some v
  | other -> Some other

and walk_entry ctx used (entry : entry) (json : Yojson.Safe.t) : unit =
  match entry.expr, json with
  | Atd.Ast.Record (_, fields, _), `Assoc members ->
    List.iter (walk_field ctx used entry members) fields
  | Atd.Ast.Sum (_, vs, sum_annot), _ ->
    let adapter = adapter_of_annot (entry.annot @ sum_annot) in
    (match tag_and_payload adapter json with
     | None -> ()
     | Some (tag, payload) ->
       Hashtbl.replace used { type_key = key_of entry; kind = Variant; name = tag } ();
       (match List.find_opt (fun v -> v.tag = tag) (variants_of entry vs), payload with
        | Some { payload = Some te; _ }, Some p ->
          walk ctx used ~module_name:entry.module_name te p
        | _ -> ()))
  | Atd.Ast.Record _, _ -> ()
  | expr, _ -> walk ctx used ~module_name:entry.module_name expr json

and walk_field ctx used (entry : entry) members (field : Atd.Ast.field) : unit =
  match field with
  | `Field (_, (name, kind, annot), te) when not (is_ignored annot) ->
    let jname = json_name annot name in
    (match List.assoc_opt jname members with
     | None -> ()
     | Some value ->
       let value_type, inner =
         match kind, te with
         | Atd.Ast.Optional, Atd.Ast.Option (_, a, _) -> (a, Some value)
         | Atd.Ast.Required, Atd.Ast.Option (_, a, _) -> (a, some_of_atd_option value)
         | _ -> (te, Some value)
       in
       if is_optional_field kind te && inner <> None
       then Hashtbl.replace used { type_key = key_of entry; kind = Field; name = jname } ();
       Option.iter (walk ctx used ~module_name:entry.module_name value_type) inner)
  | _ -> ()
;;

let used_items ctx root (documents : Yojson.Safe.t list) : (item, unit) Hashtbl.t =
  let used = Hashtbl.create 256 in
  (match resolve ctx ~module_name:(fst root) (snd root) with
   | Some entry -> List.iter (walk_entry ctx used entry) documents
   | None -> ());
  used
;;

(* ---- the allowlist ------------------------------------------------------ *)

(* One row per line, `module.type.name: reason`; blank lines and `#` comments are skipped *)
let parse_allowlist (text : string) : (string * string) list =
  String.split_on_char '\n' text
  |> List.map String.trim
  |> List.filter (fun line -> line <> "" && line.[0] <> '#')
  |> List.map (fun line ->
    match String.index_opt line ':' with
    | Some i ->
      ( String.trim (String.sub line 0 i),
        String.trim (String.sub line (i + 1) (String.length line - i - 1)) )
    | None -> (line, ""))
;;

(* ---- the check ---------------------------------------------------------- *)

(* The problems of the schema against the data and the allowlist; empty when it holds *)
let check ~allowlist_file ~root ~(modules : (string * Atd.Ast.module_body) list)
  ~(documents : Yojson.Safe.t list) ~(allowlist : (string * string) list) : string list
  =
  let modules = List.map (fun (path, body) -> (Utils.module_of_file path, body)) modules in
  let ctx = Main.build_context modules in
  let items = schema_items ctx root in
  let used = used_items ctx root documents in
  let is_used item = Hashtbl.mem used item in
  let listed key = List.mem_assoc key allowlist in
  let unused =
    List.filter_map
      (fun item ->
        if is_used item || listed (key_of_item item)
        then None
        else
          Some
            (Printf.sprintf
               "schema coverage: %s is used by no entity in the data; remove it from the \
                schema, or name `%s` with a reason in %s"
               (describe item)
               (key_of_item item)
               allowlist_file))
      items
  in
  let row_problems =
    List.filter_map
      (fun (key, reason) ->
        match List.find_opt (fun i -> key_of_item i = key) items with
        | None ->
          Some (Printf.sprintf "%s: row `%s` names nothing in the schema; remove it" allowlist_file key)
        | Some item when is_used item ->
          Some (Printf.sprintf "%s: row `%s` is used by the data now; remove it" allowlist_file key)
        | Some _ when reason = "" ->
          Some (Printf.sprintf "%s: row `%s` has no reason" allowlist_file key)
        | Some _ -> None)
      allowlist
  in
  unused @ row_problems
;;

(* Schema coverage: every sum-type variant and every field that may be left unset (optional,
   with a default, or a list) the schema reaches from the entity root must be used by at least one
   entity of the data, or be named, with a verdict and its evidence, in the allowlist file.
   A type no declared root reaches is a finding too. It reads the schema through the emitter's context (same modules, same name
   resolution, same annotations) and the data as raw JSON, walked by the schema's types. The
   allowlist can only shrink: a row naming something that is now used, or that is gone, fails. *)

open Types
open Annotations
open Generator_types

(* What makes a field "unset" in an entity: `?f` / `f : T option` (absent or null), `~f` (absent
   or null) and a required list (absent or empty) *)
type field_kind =
  | Optional_field
  | Defaulted
  | List_field

type kind =
  | Variant
  | Field of field_kind

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
     | Field Optional_field -> "optional field"
     | Field Defaulted -> "field with a default"
     | Field List_field -> "list field")
    item.name
    (fst item.type_key)
    (snd item.type_key)
;;

(* The way an entity can leave a field unset, if it can: `?f`, a required `f : T option`, `~f`,
   a required list *)
let field_kind_of kind te : field_kind option =
  match kind, te with
  | Atd.Ast.Optional, _ -> Some Optional_field
  | Atd.Ast.Required, Atd.Ast.Option _ -> Some Optional_field
  | Atd.Ast.With_default, _ -> Some Defaulted
  | Atd.Ast.Required, Atd.Ast.List _ -> Some List_field
  | _ -> None
;;

(* ---- the schema side ---------------------------------------------------- *)

let items_of_entry (entry : entry) : item list =
  let item kind name = { type_key = key_of entry; kind; name } in
  match entry.expr with
  | Atd.Ast.Record (_, fields, _) ->
    List.filter_map
      (function
        | `Field (_, (name, kind, annot), te) when not (is_ignored annot) ->
          Option.map (fun fk -> item (Field fk) (json_name annot name)) (field_kind_of kind te)
        | _ -> None)
      fields
  | Atd.Ast.Sum (_, vs, _) -> List.map (fun v -> item Variant v.tag) (variants_of entry vs)
  | _ -> []
;;

(* Every defining entry reachable from [roots] *)
let reachable ctx roots : entry list =
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
  List.iter (fun root -> Option.iter visit (resolve ctx ~module_name:(fst root) (snd root))) roots;
  Hashtbl.fold (fun _ e acc -> e :: acc) seen []
;;

let schema_items ctx root = List.concat_map items_of_entry (reachable ctx [ root ])

(* The types the emitter writes that no root reaches: nothing checks them against the data *)
let unreached ctx roots : entry list =
  let seen = Hashtbl.create 64 in
  List.iter (fun e -> Hashtbl.replace seen (key_of e) ()) (reachable ctx roots);
  Hashtbl.fold
    (fun _ (e : entry) acc ->
      if e.import_from <> None || Main.is_abstract e.expr || is_ignored e.annot || Hashtbl.mem seen (key_of e)
      then acc
      else e :: acc)
    ctx.entries
    []
  |> List.sort (fun a b -> compare (key_of a) (key_of b))
;;

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
  | _, `Assoc entries when object_map_value te <> None ->
    Option.iter (fun value -> List.iter (fun (_, v) -> recur value v) entries) (object_map_value te)
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
       let value_type, inner = field_value kind te value in
       (match field_kind_of kind te, inner with
        | Some fk, Some v when is_set fk v ->
          Hashtbl.replace used { type_key = key_of entry; kind = Field fk; name = jname } ()
        | _ -> ());
       Option.iter (walk ctx used ~module_name:entry.module_name value_type) inner)
  | _ -> ()

(* The type of a field's value and the part of the JSON that is a value: an atdgen option
   unwraps `"None"` / `["Some", v]`, and `null` is no value for an optional or defaulted field *)
and field_value kind te value : Atd.Ast.type_expr * Yojson.Safe.t option =
  let present = if value = `Null then None else Some value in
  match kind, te with
  | Atd.Ast.Optional, Atd.Ast.Option (_, a, _) -> (a, present)
  | Atd.Ast.Required, Atd.Ast.Option (_, a, _) -> (a, some_of_atd_option value)
  | Atd.Ast.With_default, _ -> (te, present)
  | _ -> (te, Some value)

(* A list field is set by a non-empty list or object; the others by any value *)
and is_set fk (v : Yojson.Safe.t) =
  match fk, v with
  | List_field, (`List [] | `Assoc []) -> false
  | _ -> true
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

(* The four verdicts of a row: `default`, `built`, `for #N` (an open issue) or `remove` *)
let verdict_ok verdict =
  List.mem verdict [ "default"; "built"; "remove" ]
  ||
  match String.split_on_char '#' verdict with
  | [ "for "; n ] -> n <> "" && String.for_all (fun c -> c >= '0' && c <= '9') n
  | _ -> false
;;

(* What is wrong with a row's reason `verdict: evidence`, if anything *)
let reason_problem reason : string option =
  if reason = ""
  then Some "has no reason"
  else (
    let verdict, evidence =
      match String.index_opt reason ':' with
      | Some i ->
        ( String.trim (String.sub reason 0 i),
          String.trim (String.sub reason (i + 1) (String.length reason - i - 1)) )
      | None -> (String.trim reason, "")
    in
    if not (verdict_ok verdict)
    then Some (Printf.sprintf "starts with `%s`, not a verdict (default, built, for #N, remove)" verdict)
    else if evidence = ""
    then Some "has a verdict and no evidence"
    else None)
;;

let line_to_add item =
  Printf.sprintf "%s: <default|built|for #N|remove>: <evidence>" (key_of_item item)
;;

(* ---- the check ---------------------------------------------------------- *)

(* The problems of the schema against the data and the allowlist; empty when it holds. [root] is
   the type of the documents; [other_roots] are declared roots whose data is checked elsewhere
   (they only count as reaching their types) *)
let check ~allowlist_file ~root ?(other_roots = []) ~(modules : (string * Atd.Ast.module_body) list)
  ~(documents : Yojson.Safe.t list) ~(allowlist : (string * string) list) () : string list
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
                schema, or add the line `%s` to %s"
               (describe item)
               (line_to_add item)
               allowlist_file))
      items
  in
  let orphans_list = unreached ctx (root :: other_roots) in
  let orphan_key (e : entry) = e.module_name ^ "." ^ e.atd_name in
  let reason_problem_of key reason =
    Option.map (fun p -> Printf.sprintf "%s: row `%s` %s" allowlist_file key p) (reason_problem reason)
  in
  let row_problem (key, reason) =
    match List.find_opt (fun i -> key_of_item i = key) items with
    | Some item when is_used item ->
      Some (Printf.sprintf "%s: row `%s` is used by the data now; remove it" allowlist_file key)
    | Some _ -> reason_problem_of key reason
    | None when List.exists (fun e -> orphan_key e = key) orphans_list -> reason_problem_of key reason
    | None -> Some (Printf.sprintf "%s: row `%s` names nothing in the schema (or a type a root reaches now); remove it" allowlist_file key)
  in
  let duplicates =
    List.filter_map
      (fun (key, _) ->
        if List.length (List.filter (fun (k, _) -> k = key) allowlist) > 1
        then Some (Printf.sprintf "%s: row `%s` is listed twice" allowlist_file key)
        else None)
      allowlist
    |> List.sort_uniq compare
  in
  let orphans =
    List.filter_map
      (fun e ->
        if listed (orphan_key e)
        then None
        else
          Some
            (Printf.sprintf
               "schema coverage: type `%s` is reached from no root; remove it, declare its \
                root, or add the line `%s: <default|built|for #N|remove>: <evidence>` to %s"
               (orphan_key e)
               (orphan_key e)
               allowlist_file))
      orphans_list
  in
  unused @ List.filter_map row_problem allowlist @ duplicates @ orphans
;;

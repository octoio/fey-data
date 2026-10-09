open Types
open Utils
open Annotations
open Generator_types
open Generator_serde

(* ---- context: every type of every module, plus the recursion analysis ---- *)

let is_abstract = function
  | Atd.Ast.Name (_, (_, "abstract", _), _) -> true
  | _ -> false
;;

let entries_of_module (module_name : string) (body : Atd.Ast.module_body) : entry list =
  List.map
    (fun (Atd.Ast.Type (_, (atd_name, _, annot), expr)) ->
      let import_from =
        if is_abstract expr
        then Option.map String.lowercase_ascii (get_ocaml annot "from")
        else None
      in
      let rust_name =
        Option.value (get_rs annot "name") ~default:(to_pascal_case atd_name)
      in
      { module_name; atd_name; rust_name; expr; annot; import_from })
    body
;;

(* Names of the user types a type expression contains inline (not behind a Vec) *)
let rec inline_names (te : Atd.Ast.type_expr) : string list =
  match te with
  | Atd.Ast.List _ -> []
  | Atd.Ast.Option (_, a, _)
  | Atd.Ast.Nullable (_, a, _)
  | Atd.Ast.Shared (_, a, _)
  | Atd.Ast.Wrap (_, a, _) -> inline_names a
  | Atd.Ast.Tuple (_, cells, _) -> List.concat_map (fun (_, t, _) -> inline_names t) cells
  | Atd.Ast.Record (_, fields, _) ->
    List.concat_map
      (function
        | `Field (_, (_, _, annot), t) -> if is_ignored annot then [] else inline_names t
        | `Inherit _ -> [])
      fields
  | Atd.Ast.Sum (_, variants, _) ->
    List.concat_map
      (fun (v : Atd.Ast.variant) ->
        match v with
        | Atd.Ast.Variant (_, _, Some t) -> inline_names t
        | _ -> [])
      variants
  | Atd.Ast.Name (_, (_, name, _), _) -> if is_primitive name then [] else [ name ]
  | Atd.Ast.Tvar _ -> []
;;

let build_context (modules : (string * Atd.Ast.module_body) list) : context =
  let entries = Hashtbl.create 512 in
  List.iter
    (fun (m, body) ->
      List.iter
        (fun e -> Hashtbl.replace entries (e.module_name, e.atd_name) e)
        (entries_of_module m body))
    modules;
  let ctx = { entries; recursive = Hashtbl.create 64 } in
  (* direct inline edges between defining entries *)
  let edges = Hashtbl.create 512 in
  Hashtbl.iter
    (fun key e ->
      if e.import_from = None && not (is_abstract e.expr)
      then (
        let targets =
          inline_names e.expr
          |> List.filter_map (fun n ->
            Option.map key_of (resolve ctx ~module_name:e.module_name n))
        in
        Hashtbl.replace edges key (List.sort_uniq compare targets)))
    entries;
  (* closure(b) = everything reachable from b through inline edges, b included *)
  let closure start =
    let seen = Hashtbl.create 16 in
    let rec visit k =
      if not (Hashtbl.mem seen k)
      then (
        Hashtbl.replace seen k ();
        List.iter visit (Option.value (Hashtbl.find_opt edges k) ~default:[]))
    in
    visit start;
    Hashtbl.fold (fun k () acc -> k :: acc) seen []
  in
  (* target b needs a Box for owner a when a is inside closure(b) *)
  Hashtbl.iter
    (fun key _ ->
      let reach = closure key in
      Hashtbl.replace ctx.recursive key reach)
    edges;
  ctx
;;

(* ---- one ATD module -> one Rust file ----------------------------------- *)

let generate_item ctx (entry : entry) : string option =
  if entry.import_from <> None || is_abstract entry.expr || is_ignored entry.annot
  then None
  else (
    match entry.expr with
    | Atd.Ast.Record (_, fields, _) -> Some (generate_struct ctx entry fields)
    | Atd.Ast.Sum (_, vs, sum_annot) ->
      let variants = variants_of entry vs in
      let has_payload = List.exists (fun v -> v.payload <> None) variants in
      (* `= [ ... ] <json adapter...>` annotates the sum, `type t <json ...> = ...` the definition *)
      let adapter = adapter_of_annot (entry.annot @ sum_annot) in
      if has_payload
      then
        Some (generate_payload_enum ctx entry variants ^ generate_impls entry variants adapter)
      else Some (generate_unit_enum entry variants)
    | other -> Some (generate_alias ctx entry other))
;;

let generate_module ctx (module_name, body) : rs_file =
  let entries = entries_of_module module_name body in
  let imports =
    List.filter_map
      (fun e ->
        match e.import_from with
        | Some m ->
          let target = lookup ctx ~module_name e.atd_name in
          Some (Printf.sprintf "use super::%s::%s;\n" m target.rust_name)
        | None -> None)
      entries
  in
  let items = List.filter_map (generate_item ctx) entries in
  let content =
    generated_comment_header
    ^ "\nuse serde::{Deserialize, Serialize};\n\nuse super::runtime;\n"
    ^ String.concat "" (List.sort_uniq compare imports)
    ^ String.concat "" (List.map (fun item -> "\n" ^ item) items)
  in
  { name = module_name; content }
;;

(* ---- entry point -------------------------------------------------------- *)

(* [atd_files] are (path, parsed module body); the module name is the file's base name *)
let generate_rust (atd_modules : (string * Atd.Ast.module_body) list) : rs_file list =
  let modules =
    atd_modules
    |> List.map (fun (path, body) -> (module_of_file path, body))
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let ctx = build_context modules in
  let module_files = List.map (generate_module ctx) modules in
  let mod_rs =
    generated_comment_header
    ^ "\npub mod runtime;\n"
    ^ String.concat "" (List.map (fun (m, _) -> Printf.sprintf "pub mod %s;\n" m) modules)
  in
  ({ name = "runtime"; content = Runtime.source }
   :: { name = "mod"; content = mod_rs }
   :: module_files)
;;

(* ============================================================================ *)
(* STRUCTS, ENUMS AND TYPE MAPPING                                             *)
(* ============================================================================ *)

open Types
open Utils
open Annotations

let key_of entry = (entry.module_name, entry.atd_name)

(* Follow `<ocaml from="X"> = abstract` imports to the defining entry *)
let rec resolve (ctx : context) ~module_name name : entry option =
  match Hashtbl.find_opt ctx.entries (module_name, name) with
  | None -> None
  | Some ({ import_from = Some m; _ } as _imp) -> resolve ctx ~module_name:m name
  | Some entry -> Some entry
;;

let lookup ctx ~module_name name =
  match resolve ctx ~module_name name with
  | Some entry -> entry
  | None ->
    failwith
      (Printf.sprintf "rust: unresolved type `%s` in module `%s`" name module_name)
;;

(* ATD type -> Rust type. [owner] is the type being generated (to decide on Box),
   [inline] is false below a Vec, where recursion needs no indirection. *)
let rec rust_type ctx ~module_name ~owner ~inline (te : Atd.Ast.type_expr) : string =
  let recur = rust_type ctx ~module_name ~owner in
  match te with
  | Atd.Ast.List (_, arg, _) -> Printf.sprintf "Vec<%s>" (recur ~inline:false arg)
  | Atd.Ast.Option (_, arg, _) | Atd.Ast.Nullable (_, arg, _) ->
    Printf.sprintf "Option<%s>" (recur ~inline arg)
  | Atd.Ast.Shared (_, arg, _) | Atd.Ast.Wrap (_, arg, _) -> recur ~inline arg
  | Atd.Ast.Tuple (_, cells, _) ->
    let items = List.map (fun (_, t, _) -> recur ~inline t) cells in
    (match items with
     | [ one ] -> Printf.sprintf "(%s,)" one
     | _ -> Printf.sprintf "(%s)" (String.concat ", " items))
  | Atd.Ast.Name (_, (_, name, args), _) ->
    if args <> []
    then failwith (Printf.sprintf "rust: type parameters are not supported (%s)" name)
    else if is_primitive name
    then primitive_rust_type name
    else (
      let target = lookup ctx ~module_name name in
      if inline && boxed ctx ~owner (key_of target)
      then Printf.sprintf "Box<%s>" target.rust_name
      else target.rust_name)
  | Atd.Ast.Tvar (_, v) -> failwith ("rust: type variable '" ^ v ^ " is not supported")
  | Atd.Ast.Sum _ | Atd.Ast.Record _ ->
    failwith "rust: anonymous sum/record types are not supported; name the type"

(* A reference owner -> target needs a Box when the target can contain the owner inline *)
and boxed ctx ~owner target =
  match Hashtbl.find_opt ctx.recursive target with
  | Some closure -> List.mem owner closure
  | None -> false
;;

let field_type ctx ~module_name ~owner annot te =
  match get_rs annot "usetype" with
  | Some t -> t
  | None -> rust_type ctx ~module_name ~owner ~inline:true te
;;

let attribute_lines indent attrs =
  List.map (fun a -> Printf.sprintf "%s%s\n" indent a) attrs
;;

let derives ~base annot =
  let extra = extra_derives annot in
  String.concat ", " (base @ List.filter (fun d -> not (List.mem d base)) extra)
;;

(* ---- records ----------------------------------------------------------- *)

let generate_field ctx ~module_name ~owner (field : Atd.Ast.field) : string =
  match field with
  | `Field (_, (name, kind, annot), te) ->
    if is_ignored annot
    then ""
    else (
      let buf = Buffer.create 128 in
      let ident = escape_ident name in
      let json_name = Option.value (get_json annot "name") ~default:name in
      let is_plain_option =
        kind = Atd.Ast.Required
        && (match te with
            | Atd.Ast.Option _ -> true
            | _ -> false)
      in
      let lenient =
        (* atdgen reads ints from JSON strings too; mirror that for the common shapes *)
        match get_rs annot "usetype", te with
        | None, Atd.Ast.Name (_, (_, "int", []), _) -> [ "deserialize_with = \"super::runtime::lenient_i64\"" ]
        | None, Atd.Ast.Option (_, Atd.Ast.Name (_, (_, "int", []), _), _)
          when kind = Atd.Ast.Optional ->
          [ "deserialize_with = \"super::runtime::lenient_i64_option\"" ]
        | None, Atd.Ast.List (_, Atd.Ast.Name (_, (_, "int", []), _), _) ->
          [ "deserialize_with = \"super::runtime::lenient_i64_vec\"" ]
        | _ -> []
      in
      let serde_args =
        lenient
        @ (if json_name <> name then [ Printf.sprintf "rename = \"%s\"" json_name ] else [])
        @ (match kind with
           | Atd.Ast.Optional ->
             [ "default"; "skip_serializing_if = \"Option::is_none\"" ]
           | Atd.Ast.With_default -> [ "default" ]
           | Atd.Ast.Required ->
             if is_plain_option
             then [ "with = \"super::runtime::atd_option\"" ]
             else [])
      in
      if serde_args <> []
      then Buffer.add_string buf (Printf.sprintf "    #[serde(%s)]\n" (String.concat ", " serde_args));
      List.iter (Buffer.add_string buf) (attribute_lines "    " (attributes annot));
      Buffer.add_string
        buf
        (Printf.sprintf "    pub %s: %s,\n" ident (field_type ctx ~module_name ~owner annot te));
      Buffer.contents buf)
  | `Inherit _ -> failwith "rust: record `inherit` is not supported"
;;

let generate_struct ctx (entry : entry) fields : string =
  let owner = key_of entry in
  let body =
    List.map (generate_field ctx ~module_name:entry.module_name ~owner) fields
    |> String.concat ""
  in
  let derive =
    derives ~base:[ "Debug"; "Clone"; "PartialEq"; "Serialize"; "Deserialize" ] entry.annot
  in
  String.concat
    ""
    (attribute_lines "" (attributes entry.annot)
     @ [ Printf.sprintf "#[derive(%s)]\npub struct %s {\n%s}\n" derive entry.rust_name body ])
;;

(* ---- sums -------------------------------------------------------------- *)

type variant =
  { tag : string; (* as written in JSON *)
    ident : string; (* Rust variant name *)
    payload : Atd.Ast.type_expr option
  }

(* `Self` cannot be a variant name, not even as a raw identifier *)
let variant_ident name =
  match to_pascal_case name with
  | "Self" -> "Self_"
  | ident -> ident
;;

let variants_of (entry : entry) (vs : Atd.Ast.variant list) : variant list =
  List.map
    (fun (v : Atd.Ast.variant) ->
      match v with
      | Atd.Ast.Variant (_, (name, annot), payload) ->
        let tag = Option.value (get_json annot "name") ~default:name in
        { tag; ident = variant_ident name; payload }
      | Atd.Ast.Inherit _ ->
        failwith ("rust: `inherit` is not supported in " ^ entry.atd_name))
    vs
;;

let generate_unit_enum (entry : entry) (variants : variant list) : string =
  let derive =
    derives
      ~base:
        [ "Debug"; "Clone"; "Copy"; "PartialEq"; "Eq"; "Hash"; "PartialOrd"; "Ord";
          "Serialize"; "Deserialize" ]
      entry.annot
  in
  let lines =
    List.map
      (fun v ->
        (if v.tag <> v.ident
         then Printf.sprintf "    #[serde(rename = \"%s\")]\n" v.tag
         else "")
        ^ Printf.sprintf "    %s,\n" v.ident)
      variants
  in
  String.concat
    ""
    (attribute_lines "" (attributes entry.annot)
     @ [ Printf.sprintf
           "#[derive(%s)]\npub enum %s {\n%s}\n"
           derive
           entry.rust_name
           (String.concat "" lines)
       ])
;;

(* Sum with payloads: the enum itself; its serde impls are in Generator_serde *)
let generate_payload_enum ctx (entry : entry) (variants : variant list) : string =
  let owner = key_of entry in
  let derive = derives ~base:[ "Debug"; "Clone"; "PartialEq" ] entry.annot in
  let lines =
    List.map
      (fun v ->
        match v.payload with
        | None -> Printf.sprintf "    %s,\n" v.ident
        | Some te ->
          Printf.sprintf
            "    %s(%s),\n"
            v.ident
            (rust_type ctx ~module_name:entry.module_name ~owner ~inline:true te))
      variants
  in
  String.concat
    ""
    (attribute_lines "" (attributes entry.annot)
     @ [ Printf.sprintf
           "#[derive(%s)]\npub enum %s {\n%s}\n"
           derive
           entry.rust_name
           (String.concat "" lines)
       ])
;;

let generate_alias ctx (entry : entry) te : string =
  Printf.sprintf
    "pub type %s = %s;\n"
    entry.rust_name
    (rust_type ctx ~module_name:entry.module_name ~owner:(key_of entry) ~inline:true te)
;;

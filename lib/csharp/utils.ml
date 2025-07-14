(* ============================================================================ *)
(* UTILITY FUNCTIONS                                                           *)
(* ============================================================================ *)

open Atd.Import

let parse_atd_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let atd_module = Atd.Parser.full_module Atd.Lexer.token lexbuf in
  close_in ic;
  atd_module
;;

let is_primitive_type type_name =
  match String.lowercase_ascii type_name with
  | "int" | "float" | "string" | "bool" | "uint" -> true
  | _ -> false
;;

let to_pascal_case str =
  str |> String.split_on_char '_' |> List.map String.capitalize_ascii |> String.concat ""
;;

let to_camel_case str =
  match String.split_on_char '_' str with
  | [] -> ""
  | head :: tail -> head ^ String.concat "" (List.map String.capitalize_ascii tail)
;;

let get_annotation_value (annot : Atd.Ast.annot) key =
  let rec find_annotation_value sections =
    match sections with
    | [] -> None
    | (name, (_, fields)) :: rest ->
      if name = "cs" || name = "json"
      then (
        match List.find_opt (fun (field_name, _) -> field_name = key) fields with
        | Some (_, (_, Some value)) -> Some value
        | _ -> find_annotation_value rest)
      else find_annotation_value rest
  in
  find_annotation_value annot
;;

let rec generate_csharp_type type_expr =
  match type_expr with
  | Atd.Ast.List (_, arg, _) ->
    let element_type = generate_csharp_type arg in
    Printf.sprintf "%s[]" element_type
  | Atd.Ast.Option (_, arg, _) ->
    let element_type = generate_csharp_type arg in
    Printf.sprintf "%s?" element_type
  | Atd.Ast.Nullable (_, arg, _) ->
    let element_type = generate_csharp_type arg in
    Printf.sprintf "%s?" element_type
  | Atd.Ast.Name (_, (_, type_name, _), _) ->
    if is_primitive_type type_name then type_name else to_pascal_case type_name
  | Atd.Ast.Sum (_, _variants, _) ->
    "string" (* Use string for polymorphic variants in structs *)
  | _ -> ""
;;

let extract_base_class_name internal_type_name =
  if String.ends_with ~suffix:"_internal" internal_type_name
  then String.sub internal_type_name 0 (String.length internal_type_name - 9)
  else internal_type_name
;;

let get_variant_info (variant : Atd.Ast.variant) =
  match variant with
  | Atd.Ast.Variant (_, (variant_name, _), opt_type) ->
    (match opt_type with
     | Some (Atd.Ast.Name (_, (_, type_name, _), _)) -> Some (variant_name, type_name)
     | _ -> None)
  | _ -> None
;;

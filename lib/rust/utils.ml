(* ============================================================================ *)
(* UTILITY FUNCTIONS                                                           *)
(* ============================================================================ *)

let parse_atd_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let atd_module = Atd.Parser.full_module Atd.Lexer.token lexbuf in
  close_in ic;
  atd_module
;;

let to_pascal_case str =
  str
  |> String.split_on_char '_'
  |> List.filter (fun s -> s <> "")
  |> List.map String.capitalize_ascii
  |> String.concat ""
;;

let rust_keywords =
  [ "as"; "break"; "const"; "continue"; "crate"; "else"; "enum"; "extern"; "false";
    "fn"; "for"; "if"; "impl"; "in"; "let"; "loop"; "match"; "mod"; "move"; "mut";
    "pub"; "ref"; "return"; "self"; "static"; "struct"; "super"; "trait"; "true";
    "type"; "unsafe"; "use"; "where"; "while"; "async"; "await"; "dyn"; "abstract";
    "become"; "box"; "do"; "final"; "macro"; "override"; "priv"; "typeof"; "unsized";
    "virtual"; "yield"; "try"; "gen" ]
;;

(* Keywords that cannot even be raw identifiers *)
let unrawable = [ "self"; "super"; "crate"; "Self" ]

let escape_ident name =
  if List.mem name unrawable
  then name ^ "_"
  else if List.mem name rust_keywords
  then "r#" ^ name
  else name
;;

(* "Skill" from `<ocaml from="Skill">` or the file skill.atd *)
let module_of_file filename =
  Filename.basename filename |> Filename.remove_extension |> String.lowercase_ascii
;;

let is_primitive name =
  match name with
  | "int" | "float" | "string" | "bool" | "unit" -> true
  | _ -> false
;;

let primitive_rust_type name =
  match name with
  | "int" -> "i64"
  | "float" -> "f64"
  | "string" -> "String"
  | "bool" -> "bool"
  | "unit" -> "()"
  | other -> failwith ("not a primitive: " ^ other)
;;

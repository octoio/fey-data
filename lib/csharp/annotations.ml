(* ============================================================================ *)
(* ANNOTATION AND GENERATOR CONFIGURATION                                     *)
(* ============================================================================ *)

open Atd.Import
open Types
open Utils

(* Parse generator configuration like "visitor(namespace=Octoio.Fey.Data.Dto,name=SkillActionNode)" *)
let parse_generator_config (config_str : string) : generator_config =
  let config_str = String.trim config_str in
  if String.contains config_str '(' && String.contains config_str ')'
  then (
    (* Generator with parameters *)
    let open_paren = String.index config_str '(' in
    let close_paren = String.rindex config_str ')' in
    let generator_type = String.sub config_str 0 open_paren |> String.trim in
    let params_str =
      String.sub config_str (open_paren + 1) (close_paren - open_paren - 1)
    in
    let params = String.split_on_char ',' params_str |> List.map String.trim in
    let namespace = ref None in
    let name = ref None in
    let enumname = ref None in
    let modifiers = ref None in
    List.iter
      (fun param ->
        if String.length param > 0
        then (
          let parts = String.split_on_char '=' param in
          match parts with
          | [ key; value ] ->
            let key = String.trim key in
            let value = String.trim value in
            if key = "namespace"
            then namespace := Some value
            else if key = "nameprefix"
            then name := Some value
            else if key = "enumname"
            then enumname := Some value
            else if key = "modifiers"
            then modifiers := Some (String.split_on_char ',' value |> List.map String.trim)
          | _ -> ()))
      params;
    { generator_type;
      namespace = !namespace;
      name = !name;
      enumname = !enumname;
      modifiers = !modifiers
    })
  else
    (* Simple generator without parameters *)
    { generator_type = config_str;
      namespace = None;
      name = None;
      enumname = None;
      modifiers = None
    }
;;

(* Split a generator string on commas while respecting parentheses *)
let split_generators (generators_str : string) : string list =
  let rec split_helper chars acc current paren_depth =
    match chars with
    | [] ->
      if String.length (String.trim current) > 0 then String.trim current :: acc else acc
    | '(' :: rest -> split_helper rest acc (current ^ "(") (paren_depth + 1)
    | ')' :: rest -> split_helper rest acc (current ^ ")") (paren_depth - 1)
    | ',' :: rest when paren_depth = 0 ->
      let trimmed = String.trim current in
      if String.length trimmed > 0
      then split_helper rest (trimmed :: acc) "" paren_depth
      else split_helper rest acc "" paren_depth
    | c :: rest -> split_helper rest acc (current ^ String.make 1 c) paren_depth
  in
  let chars = String.to_seq generators_str |> List.of_seq in
  List.rev (split_helper chars [] "" 0)
;;

let get_annotation_generators (annot : Atd.Ast.annot) =
  match get_annotation_value annot "generators" with
  | Some generators_str ->
    (* Split comma-separated generators while respecting parentheses *)
    split_generators generators_str |> List.map parse_generator_config
  | None -> []
;;

let has_generator (annot : Atd.Ast.annot) (generator : string) =
  let generators = get_annotation_generators annot in
  List.exists (fun config -> config.generator_type = generator) generators
;;

let get_generator_config (annot : Atd.Ast.annot) (generator : string)
  : generator_config option
  =
  let generators = get_annotation_generators annot in
  List.find_opt (fun config -> config.generator_type = generator) generators
;;

let get_json_name (annot : Atd.Ast.annot) = get_annotation_value annot "name"

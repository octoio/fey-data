open Atd.Import
open Types
open Utils
open Annotations

let generate_visitor_interface base_name variants namespace =
  let interface_name = Printf.sprintf "I%sVisitor" (to_pascal_case base_name) in
  let imports = [ "System" ] in
  let ns = [ (`Namespace, Printf.sprintf "namespace %s" namespace); open_bracket ] in
  let import_lines =
    List.map (fun imp -> (`Import, Printf.sprintf "using %s;" imp)) imports
  in
  let visit_methods =
    variants
    |> List.filter_map (fun variant ->
      match get_variant_info variant with
      | Some (_, type_name) ->
        let class_name = to_pascal_case type_name in
        let param_name = to_camel_case type_name in
        Some (`Field, Printf.sprintf "TResult Visit(Dto.%s %s);" class_name param_name)
      | None -> None)
  in
  { lines =
      import_lines
      @ blank_line_concat
      @ ns
      @ [ (`Class, Printf.sprintf "public interface %s<TResult>" interface_name);
          open_bracket
        ]
      @ visit_methods
      @ close_bracket_concat
      @ close_bracket_concat;
    file_kind = `Class;
    namespace;
    name = interface_name
  }
;;

(* Removed visitable interface - using simpler visitor pattern *)
let generate_abstract_base_class base_name namespace =
  let base_class_name = Printf.sprintf "%sBase" (to_pascal_case base_name) in
  let visitor_interface = Printf.sprintf "I%sVisitor" (to_pascal_case base_name) in
  let imports = [ "System" ] in
  let ns = [ (`Namespace, Printf.sprintf "namespace %s" namespace); open_bracket ] in
  let import_lines =
    List.map (fun imp -> (`Import, Printf.sprintf "using %s;" imp)) imports
  in
  { lines =
      import_lines
      @ blank_line_concat
      @ ns
      @ [ (`Class, Printf.sprintf "public abstract partial class %s" base_class_name);
          open_bracket;
          ( `Field,
            Printf.sprintf "public abstract T Accept<T>(%s<T> visitor);" visitor_interface
          )
        ]
      @ close_bracket_concat
      @ close_bracket_concat;
    file_kind = `Class;
    namespace;
    name = base_class_name
  }
;;

let generate_partial_class_implementation type_name base_name namespace visitor_namespace =
  let class_name = to_pascal_case type_name in
  let visitor_interface = Printf.sprintf "I%sVisitor" (to_pascal_case base_name) in
  let imports = [ "System" ] in
  let ns = [ (`Namespace, Printf.sprintf "namespace %s" namespace); open_bracket ] in
  let import_lines =
    List.map (fun imp -> (`Import, Printf.sprintf "using %s;" imp)) imports
  in
  let visitor_using =
    if visitor_namespace <> namespace
    then [ (`Import, Printf.sprintf "using %s;" visitor_namespace) ]
    else []
  in
  { lines =
      import_lines
      @ visitor_using
      @ blank_line_concat
      @ ns
      @ [ ( `Class,
            Printf.sprintf
              "public partial class %s : %s"
              class_name
              (to_pascal_case base_name) );
          open_bracket;
          ( `Field,
            Printf.sprintf
              "public override TResult Accept<%sVisitor, TResult>(%sVisitor visitor) \
               where %sVisitor : %s<TResult>"
              (to_pascal_case base_name)
              (to_pascal_case base_name)
              (to_pascal_case base_name)
              visitor_interface );
          open_bracket;
          (`Field, Printf.sprintf "return visitor.Visit(this);");
          close_bracket;
          close_bracket;
          blank_line
        ]
      @ close_bracket_concat
      @ close_bracket_concat;
    file_kind = `Class;
    namespace;
    name = class_name ^ "VisitorImpl"
  }
;;

let generate_consolidated_visitables base_name variants namespace visitor_namespace =
  let imports = [ "System" ] in
  let ns = [ (`Namespace, Printf.sprintf "namespace %s" namespace); open_bracket ] in
  let import_lines =
    List.map (fun imp -> (`Import, Printf.sprintf "using %s;" imp)) imports
  in
  let visitor_using =
    if visitor_namespace <> namespace
    then [ (`Import, Printf.sprintf "using %s;" visitor_namespace) ]
    else []
  in
  (* Generate abstract base class with Accept method *)
  let visitor_interface = Printf.sprintf "I%sVisitor" (to_pascal_case base_name) in
  let abstract_base_class =
    [ ( `Class,
        Printf.sprintf "public abstract partial class %s" (to_pascal_case base_name) );
      open_bracket;
      ( `Field,
        Printf.sprintf "public abstract T Accept<T>(%s<T> visitor);" visitor_interface );
      close_bracket;
      blank_line
    ]
  in
  (* Generate all partial class implementations *)
  let partial_implementations =
    variants
    |> List.filter_map (fun variant ->
      match get_variant_info variant with
      | Some (_, type_name) ->
        let class_name = to_pascal_case type_name in
        Some
          [ ( `Class,
              Printf.sprintf
                "public partial class %s : %s"
                class_name
                (to_pascal_case base_name) );
            open_bracket;
            ( `Field,
              Printf.sprintf
                "public override T Accept<T>(%s<T> visitor)"
                visitor_interface );
            open_bracket;
            (`Field, Printf.sprintf "return visitor.Visit(this);");
            close_bracket;
            close_bracket;
            blank_line
          ]
      | None -> None)
    |> List.flatten
  in
  { lines =
      import_lines
      @ visitor_using
      @ blank_line_concat
      @ ns
      @ abstract_base_class
      @ partial_implementations
      @ close_bracket_concat;
    file_kind = `Class;
    namespace;
    name = to_pascal_case base_name ^ "Visitables"
  }
;;

let generate_visitor_generators type_name type_expr annot =
  match type_expr with
  | Atd.Ast.Sum (_, variants, _) ->
    let base_name = extract_base_class_name type_name in
    let default_namespace =
      get_annotation_value annot "namespace" |> Option.value ~default:""
    in
    (* Get visitor configuration - if none exists, use defaults *)
    let visitor_config = get_generator_config annot "visitor" in
    let visitor_namespace =
      match visitor_config with
      | Some config ->
        (match config.namespace with
         | Some ns -> ns
         | None -> default_namespace)
      | None -> default_namespace
    in
    let visitor_name =
      match visitor_config with
      | Some config -> Option.value config.name ~default:base_name
      | None -> base_name
    in
    let visitor_files =
      [ generate_visitor_interface visitor_name variants visitor_namespace ]
    in
    (* Generate consolidated visitables file for all variants *)
    let consolidated_visitables_file =
      generate_consolidated_visitables
        base_name
        variants
        visitor_namespace
        visitor_namespace
    in
    visitor_files @ [ consolidated_visitables_file ]
  | _ -> []
;;

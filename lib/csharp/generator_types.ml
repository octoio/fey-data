open Atd.Import
open Types
open Utils
open Annotations

let generate_imports annot =
  match get_annotation_value annot "imports" with
  | Some imports ->
    List.map
      (fun imp -> (`Import, Printf.sprintf "using %s;" imp))
      (String.split_on_char ';' imports)
  | None -> []
;;

let generate_namespace annot =
  match get_annotation_value annot "namespace" with
  | Some ns -> [ (`Namespace, Printf.sprintf "namespace %s" ns); open_bracket ]
  | None -> []
;;

let generate_csharp_field (field : Atd.Ast.field) =
  match field with
  | `Field (_, (field_name, _, annot), type_expr) ->
    let open Option in
    let should_ignore = get_annotation_value annot "ignore" in
    should_ignore
    |> map (fun _ -> ignore_line_concat)
    |> value
         ~default:
           (let field_attributes =
              match get_annotation_value annot "attributes" with
              | Some attrs -> [ (`Attribute, Printf.sprintf "%s" attrs) ]
              | None -> []
            in
            let csharp_type =
              get_annotation_value annot "usetype"
              |> value ~default:(generate_csharp_type type_expr)
            in
            let field_name = get_json_name annot |> value ~default:field_name in
            let field_name = to_pascal_case field_name in
            field_attributes
            @ [ ( `Field,
                  Printf.sprintf
                    "public %s %s { get; private set; }"
                    csharp_type
                    field_name )
              ])
  | _ -> [ blank_line ]
;;

let generate_csharp_enum type_name (variants : Atd.Ast.variant list) annot : cs_file =
  let enum_name =
    match get_annotation_value annot "name" with
    | Some name -> name
    | None -> to_pascal_case type_name
  in
  let enum_values =
    let get_variant_name_with_index index (variant : Atd.Ast.variant) =
      match variant with
      | Atd.Ast.Variant (_, (variant_name, _), _) ->
        Some (Printf.sprintf "%s = %d" (to_pascal_case variant_name) index)
      | _ -> None
    in
    let rec build_values acc index = function
      | [] -> List.rev acc
      | variant :: rest ->
        (match get_variant_name_with_index index variant with
         | Some enum_value -> build_values (enum_value :: acc) (index + 1) rest
         | None -> build_values acc (index + 1) rest)
    in
    build_values [] 0 variants
  in
  let enum_values_with_commas =
    match List.rev enum_values with
    | [] -> []
    | last :: rest -> List.rev (List.map (fun value -> value ^ ",") rest) @ [ last ]
  in
  let attributes =
    match get_annotation_value annot "attributes" with
    | Some attrs -> [ (`Attribute, Printf.sprintf "%s" attrs) ]
    | None -> []
  in
  let imports = generate_imports annot in
  let ns = generate_namespace annot in
  { lines =
      (imports
       @ blank_line_concat
       @ ns
       @ attributes
       @ [ (`Enum, Printf.sprintf "public enum %s : byte" enum_name); open_bracket ]
       @ List.map (fun value -> (`Field, value)) enum_values_with_commas
       @ close_bracket_concat
       @ if ns <> [] then close_bracket_concat else []);
    file_kind = `Enum;
    namespace =
      (match get_annotation_value annot "namespace" with
       | Some ns -> ns
       | None -> "");
    name = enum_name
  }
;;

let generate_csharp_struct_from_type type_name type_expr annot : cs_file =
  match type_expr with
  | Atd.Ast.Record (_, fields, _) ->
    let csharp_fields = fields |> List.map generate_csharp_field in
    let struct_name = to_pascal_case type_name in
    let imports = generate_imports annot in
    let ns = generate_namespace annot in
    let attributes =
      match get_annotation_value annot "attributes" with
      | Some attrs -> [ (`Attribute, Printf.sprintf "%s" attrs) ]
      | None -> []
    in
    let style = get_annotation_value annot "style" in
    let is_struct =
      match style with
      | Some "struct" -> true
      | _ -> false
    in
    let extends = get_annotation_value annot "extends" in
    (* Check for visitor generators to determine inheritance *)
    let generators = get_annotation_generators annot in
    let visitor_generators =
      List.filter (fun config -> config.generator_type = "visitor") generators
    in
    let extends_clause =
      match (extends, visitor_generators) with
      | Some e, _ -> Printf.sprintf " : %s" e
      | None, generator :: _ ->
        (* If this type corresponds to the visitor base class, no interface needed *)
        (match generator.name with
         | Some base_name ->
           let base_class_name = to_pascal_case base_name in
           let current_class_name = to_pascal_case type_name in
           if current_class_name = base_class_name
           then (* This IS the base class, no extends needed *)
             ""
           else Printf.sprintf " : %s" base_class_name
         | None -> "")
      | None, [] -> ""
    in
    (* Abstract Accept method is now generated in visitables file, not in base class *)
    let abstract_accept_method = [] in
    let struct_keyword = if is_struct then "struct" else "class" in
    (* Check for generator configurations that might specify class modifiers *)
    let class_modifiers =
      let generators = get_annotation_generators annot in
      let generator_modifiers =
        generators
        |> List.filter_map (fun config -> config.modifiers)
        |> List.flatten
        |> List.map String.trim
        |> List.filter (fun m -> m <> "")
      in
      (* Also check main CS annotation for modifiers *)
      let main_modifiers =
        match get_annotation_value annot "modifiers" with
        | Some modifiers_str ->
          String.split_on_char ',' modifiers_str
          |> List.map String.trim
          |> List.filter (fun m -> m <> "")
        | None -> []
      in
      let all_modifiers = generator_modifiers @ main_modifiers in
      if List.length all_modifiers > 0 then String.concat " " all_modifiers ^ " " else ""
    in
    { lines =
        (imports
         @ blank_line_concat
         @ ns
         @ attributes
         @ [ ( `Struct,
               Printf.sprintf
                 "public %s%s %s%s"
                 class_modifiers
                 struct_keyword
                 struct_name
                 extends_clause );
             open_bracket
           ]
         @ abstract_accept_method
         @ List.flatten csharp_fields
         @ close_bracket_concat
         @ if ns <> [] then close_bracket_concat else []);
      file_kind = (if is_struct then `Struct else `Class);
      namespace =
        (match get_annotation_value annot "namespace" with
         | Some ns -> ns
         | None -> "");
      name = struct_name
    }
  | _ -> { lines = []; file_kind = `Unknown; namespace = ""; name = "" }
;;

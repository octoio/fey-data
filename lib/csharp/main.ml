open Atd.Import
open Types
open Utils
open Annotations
open Generator_types
open Generator_visitor
open Generator_converter

let generate_csharp_struct (module_body : Atd.Ast.module_body) : cs_file list =
  module_body
  |> List.map (fun module_item ->
    match module_item with
    | Atd.Ast.Type (_loc, (type_name, _, annot), type_expr) ->
      (match get_annotation_value annot "ignore" with
       | Some _ -> []
       | None ->
         (* Check if this should generate visitor pattern instead of regular class *)
         (match get_annotation_value annot "noclass" with
          | Some "true" -> generate_visitor_generators type_name type_expr annot
          | _ ->
            (* Check generators annotation for visitor or converter generators *)
            let has_visitor = has_generator annot "visitor" in
            let has_converter = has_generator annot "converter" in
            if has_visitor || has_converter
            then (
              (* For Record types with visitor generator, generate regular class that implements visitable interface *)
              (* For Sum types with visitor generator, generate visitor files *)
              let should_generate_class =
                match type_expr with
                | Atd.Ast.Record (_, _, _) -> true (* Always generate class for records *)
                | Atd.Ast.Sum (_, _, _) -> false (* Generate visitor files for sums *)
                | _ -> false
              in
              let class_files =
                if should_generate_class
                then [ generate_csharp_struct_from_type type_name type_expr annot ]
                else []
              in
              let visitor_files =
                if has_visitor && not should_generate_class
                then generate_visitor_generators type_name type_expr annot
                else []
              in
              let converter_files =
                if has_converter && not should_generate_class
                then generate_converter_generators type_name type_expr annot
                else []
              in
              class_files @ visitor_files @ converter_files)
            else [ generate_csharp_struct_from_type type_name type_expr annot ])))
  |> List.flatten
  |> List.filter (fun cs_file -> cs_file.file_kind <> `Unknown)
;;

let generate_csharp_enum_from_module (module_body : Atd.Ast.module_body) : cs_file list =
  module_body
  |> List.filter_map (fun module_item ->
    match module_item with
    | Atd.Ast.Type (_loc, (type_name, _, annot), Atd.Ast.Sum (_, variants, _)) ->
      (match get_annotation_value annot "ignore" with
       | Some _ -> None
       | None ->
         (* Check if noclass flag is set or has visitor/converter pattern - don't generate enum for it *)
         (match get_annotation_value annot "noclass" with
          | Some "true" -> None
          | _ ->
            if has_generator annot "visitor" || has_generator annot "converter"
            then None
            else Some (generate_csharp_enum type_name variants annot)))
    | _ -> None)
;;

let generate_csharp (module_bodies : Atd.Ast.module_body list) : cs_file list =
  module_bodies
  |> List.map (fun mb ->
    let enums = generate_csharp_enum_from_module mb in
    let structs = generate_csharp_struct mb in
    enums @ structs)
  |> List.flatten
  |> List.filter (fun cs_file -> cs_file.file_kind <> `Unknown)
;;

open Types

let remove_base_namespace base_ns full_ns =
  let clean_ns ns =
    if String.starts_with ~prefix:"namespace " ns
    then String.sub ns 10 (String.length ns - 10)
    else ns
  in
  let base_parts = String.split_on_char '.' base_ns in
  let full_parts = String.split_on_char '.' (clean_ns full_ns) in
  let rec remove_parts base full =
    match (base, full) with
    | [], rest -> rest
    | b :: bs, f :: fs when b = f -> remove_parts bs fs
    | _ -> full
  in
  let remaining_parts = remove_parts base_parts full_parts in
  String.concat "/" remaining_parts
;;

let indent_line (indent_level : int) (line : line) : string =
  let line_type, content = line in
  let indent = String.make (indent_level * 4) ' ' in
  match line_type with
  | `Blank -> ""
  | _ -> indent ^ content
;;

let rec print_lines (lines : line list) (indent_level : int) : string list =
  match lines with
  | [] -> []
  | (`Ignore, _) :: rest -> print_lines rest indent_level
  | ((`Bracket, "{") as line) :: rest ->
    indent_line indent_level line :: print_lines rest (indent_level + 1)
  | ((`Bracket, "}") as line) :: rest ->
    indent_line (indent_level - 1) line :: print_lines rest (indent_level - 1)
  | line :: rest -> indent_line indent_level line :: print_lines rest indent_level
;;

let string_of_csharp_code (lines : line list) : string =
  (print_lines lines 0 |> String.concat "\n") ^ "\n"
;;

let write_cs_file (base_filepath : string) (base_namespace : string) (cs_file : cs_file)
  : string
  =
  let formatted_code = string_of_csharp_code cs_file.lines in
  let relative_path = remove_base_namespace base_namespace cs_file.namespace in
  let file_path =
    Filename.concat base_filepath (relative_path ^ "/" ^ cs_file.name ^ ".cs")
  in
  Gamedata.Io.print_info @@ Printf.sprintf "Writing to file: %s" file_path;
  Gamedata.Io.write_file file_path formatted_code;
  Gamedata.Io.print_success "File written";
  file_path
;;

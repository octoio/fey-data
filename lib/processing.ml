open Validate
open Dataset

let process_file file_path dataset =
  Io.print_info @@ "Processing: " ^ file_path;
  try
    match String.split_on_char '.' @@ Filename.basename file_path with
    | [ _; _; "json" ] -> validate_entity_definition file_path dataset
    | _ -> file_pattern_invalid file_path dataset
  with
  | e -> error_while_processing_file file_path dataset e
;;

let process_directory_content dir dataset =
  try
    Io.find_all_files_in_directory (fun file -> Filename.check_suffix file ".json") dir
    |> List.fold_left (fun acc file -> process_file file acc) dataset
  with
  | e -> unexpected_error_while_processing_directory dir dataset e
;;

let post_process dataset = Validate.validate_entity_definitions dataset

let result_of_dataset dataset =
  match contains_error dataset with
  | true -> Error dataset
  | false -> Ok dataset
;;

let generate_dataset dir =
  { entity_index_container = { indices = [] }; definitions = []; errors = [] }
  |> process_directory_content dir
  |> post_process
  |> generate_indices
  |> result_of_dataset
;;

let print_errors (dataset : Gamedata.Dataset.dataset) =
  let print_error ({ error; file_path; origin_location } : Gamedata.Dataset.dataset_error)
    =
    match error with
    | None -> ()
    | _ ->
      Gamedata.Io.print_warning @@ Printf.sprintf "Origin: %s" origin_location;
      Gamedata.Io.print_warning @@ Printf.sprintf "File: %s" file_path;
      Gamedata.Validate.print_validation_error error
  in
  List.iter print_error dataset.errors
;;

let write_entity_index (dataset : Gamedata.Dataset.dataset) =
  Gamedata.Io.print_info "Generating entity reference indices...";
  Gamedata.Io.write_file
    Config.Constants.entity_reference_indices_file
    (Data.Entity_j.string_of_entity_index_container dataset.entity_index_container);
  Gamedata.Io.print_success "Entity reference indices generated successfully."
;;

let get_all_atd_files () =
  Gamedata.Io.find_all_files_in_directory
    (fun file -> Filename.check_suffix file ".atd")
    "lib"
  |> List.map (fun file ->
    Gamedata.Io.print_info @@ Printf.sprintf "Found ATD file: %s" file;
    file)
;;

let generate_rust_types atd_files =
  Gamedata.Io.print_info "Generating Rust files...";
  let modules =
    List.map (fun file -> (file, snd (Rust.Utils.parse_atd_file file))) atd_files
  in
  Rust.Main.generate_rust modules
  |> Rust.Output.write_rs_files Config.Constants.rust_output_path
  |> ignore;
  Gamedata.Io.print_success "Rust files written."
;;

let () =
  Gamedata.Io.print_info "Starting validation...";
  match Gamedata.Processing.generate_dataset Config.Constants.json_folder with
  | Error dataset ->
    print_errors dataset;
    exit 1
  | Ok dataset ->
    Gamedata.Io.print_success "Validation successful";
    write_entity_index dataset;
    Gamedata.Io.print_info "Reading ATD files...";
    generate_rust_types (get_all_atd_files ());
    Gamedata.Io.print_success "Process completed successfully."
;;

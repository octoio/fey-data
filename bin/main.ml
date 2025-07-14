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

let write_cs_file cs_file =
  Csharp.Output.write_cs_file
    Config.Constants.base_script_folder
    Config.Constants.base_namespace
    cs_file
;;

let remove_all_previously_generated_files () =
  let remove_file filename =
    Gamedata.Io.print_info @@ Printf.sprintf "Removing file: %s" filename;
    Gamedata.Io.remove_file filename;
    Gamedata.Io.print_success "File removed"
  in
  Gamedata.Io.print_info "Removing previously generated files...";
  Gamedata.Io.create_file_if_not_exists Config.Constants.generated_file;
  List.iter remove_file (Gamedata.Io.read_all_lines Config.Constants.generated_file)
;;

let create_generated_files_reference (file_paths : string list) : unit =
  Gamedata.Io.write_file Config.Constants.generated_file @@ String.concat "\n" file_paths
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
    remove_all_previously_generated_files ();
    Gamedata.Io.print_info "Reading ATD files...";
    let atd_files = get_all_atd_files () in
    Gamedata.Io.print_info "Parsing ATD files...";
    let atd_modules = List.map Csharp.Utils.parse_atd_file atd_files in
    let module_bodies = List.map snd atd_modules in
    Gamedata.Io.print_info "Generating C# files...";
    Gamedata.Io.print_info "Writing C# files to disk...";
    Csharp.Main.generate_csharp module_bodies
    |> List.map write_cs_file
    |> create_generated_files_reference;
    Gamedata.Io.print_success "Process completed successfully."
;;

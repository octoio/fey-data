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

let parse_atd_modules atd_files =
  List.map (fun file -> (file, snd (Rust.Utils.parse_atd_file file))) atd_files
;;

(* The entity files are all documents of the root type of entity.atd *)
let coverage_root = ("entity", "entity_definition_internal")

let check_schema_coverage modules =
  Gamedata.Io.print_info "Checking schema coverage...";
  let documents =
    Gamedata.Io.find_all_files_in_directory
      (fun file -> Filename.check_suffix file ".json")
      Config.Constants.json_folder
    |> List.map Yojson.Safe.from_file
  in
  let allowlist =
    Rust.Coverage.parse_allowlist
      (Gamedata.Io.read_file Config.Constants.coverage_allowlist_file)
  in
  match
    Rust.Coverage.check
      ~allowlist_file:Config.Constants.coverage_allowlist_file
      ~root:coverage_root
      ~modules
      ~documents
      ~allowlist
  with
  | [] -> Gamedata.Io.print_success "Schema coverage holds."
  | problems ->
    List.iter Gamedata.Io.print_warning problems;
    exit 1
;;

let generate_rust_types modules =
  Gamedata.Io.print_info "Generating Rust files...";
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
    Gamedata.Io.print_info "Reading ATD files...";
    let modules = parse_atd_modules (get_all_atd_files ()) in
    check_schema_coverage modules;
    write_entity_index dataset;
    generate_rust_types modules;
    Gamedata.Io.print_success "Process completed successfully."
;;

open Dataset

let print_validation_error error =
  match error with
  | Some error -> Io.print_error @@ Atdgen_runtime.Util.Validation.string_of_error error
  | None -> ignore ()
;;

let file_pattern_invalid file_path dataset =
  add_error
    __LOC__
    file_path
    (Some
       (Atdgen_runtime.Util.Validation.error
          ~msg:
            ("Invalid file pattern: "
             ^ file_path
             ^ ". Expected: <entity>.<entity_type>.json")
          [ `Field "file_pattern" ]))
    dataset
;;

let error_while_processing_file file_path dataset e =
  add_error
    __LOC__
    file_path
    (Some
       (Atdgen_runtime.Util.Validation.error
          ~msg:("Error while processing file: " ^ file_path ^ ". " ^ Printexc.to_string e)
          [ `Field "file_processing" ]))
    dataset
;;

let unexpected_error_while_processing_directory dir dataset e =
  add_error
    __LOC__
    dir
    (Some
       (Atdgen_runtime.Util.Validation.error
          ~msg:("Unexpected error: " ^ Printexc.to_string e)
          [ `Field "unexpected" ]))
    dataset
;;

let validate_entity_definition file_path dataset =
  let json = Io.read_file file_path in
  let entity_definition = Data.Entity_j.entity_definition_internal_of_string json in
  let e = Data.Entity_v.validate_entity_definition_internal [] entity_definition in
  dataset
  |> add_definition (file_path, entity_definition)
  |> add_error __LOC__ file_path e
;;

(* Every anchor must be owned by exactly one stage: an orphan anchor is dead data,
   and two stages sharing an anchor would fight over its transform/lifecycle *)
let anchor_ownership_error_messages definitions =
  let anchor_references =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Anchor _ -> Some (Dataset.entity_reference_of_entity_definition definition)
        | _ -> None)
      definitions
  in
  let stage_anchor_references =
    List.concat_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Stage { entity; _ } -> entity.anchors
        | _ -> [])
      definitions
  in
  List.filter_map
    (fun anchor_reference ->
      let owner_count =
        List.length (List.filter (fun r -> r = anchor_reference) stage_anchor_references)
      in
      if owner_count = 1
      then None
      else
        Some
          (Printf.sprintf
             "Anchor must be owned by exactly one stage (owned by %d): %s"
             owner_count
             (Data.Common_j.string_of_entity_reference anchor_reference)))
    anchor_references
;;

let validate_entity_definitions dataset =
  let entity_definition_references = extract_entity_reference_definition dataset in
  let entity_reference_definitions_duplication_errors =
    List.filter
      (fun entity_reference ->
        List.length
          (List.filter (fun er -> er = entity_reference) entity_definition_references)
        > 1)
      entity_definition_references
  in
  let entity_reference_definitions_error_messages =
    List.map
      (fun er -> "Duplicate definitions: " ^ Data.Common_j.string_of_entity_reference er)
      entity_reference_definitions_duplication_errors
  in
  let entity_references = extract_entity_reference dataset in
  let entity_definition_reference_missing_errors =
    List.filter
      (fun entity_reference ->
        not @@ List.mem entity_reference entity_definition_references)
      entity_references
  in
  let entity_reference_error_messages =
    List.map
      (fun er -> "Entity reference not found: " ^ Data.Common_j.string_of_entity_reference er)
      entity_definition_reference_missing_errors
  in
  let anchor_ownership_errors = anchor_ownership_error_messages (take_definitions dataset) in
  let errors =
    List.map
      (fun message ->
        { origin_location = __LOC__;
          file_path = "Unknown file path... post-processing error.";
          error = Some (Atdgen_runtime.Util.Validation.error ~msg:message [])
        })
      (entity_reference_definitions_error_messages
       @ entity_reference_error_messages
       @ anchor_ownership_errors)
  in
  { dataset with errors = errors @ dataset.errors }
;;

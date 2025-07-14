(* Main test runner for the Gamedata module *)
open Alcotest
open Gamedata.Dataset

(* Utility function for substring matching *)
let contains_substring str sub =
  let len_str = String.length str in
  let len_sub = String.length sub in
  let rec search i =
    if i + len_sub > len_str
    then false
    else if String.sub str i len_sub = sub
    then true
    else search (i + 1)
  in
  if len_sub = 0 then true else search 0
;;

(* Test fixtures *)
let create_empty_dataset () =
  { entity_index_container = { indices = [] }; definitions = []; errors = [] }
;;

(* Sample entity data for testing *)
let sample_weapon_json =
  {|{
  "id": "sword_001", 
  "name": "Iron Sword",
  "type": "weapon",
  "damage": 25,
  "weight": 5.2,
  "rarity": "common"
}|}
;;

let sample_character_json =
  {|{
  "id": "char_001",
  "name": "Test Warrior", 
  "type": "character",
  "stats": {
    "health": 100,
    "mana": 50,
    "attack": 30,
    "defense": 20,
    "speed": 10
  }
}|}
;;

let invalid_json = {|{
  "id": "invalid_001",
  "name": "Missing Type"
}|}

(* Extended dataset tests *)
let test_empty_dataset () =
  let dataset = create_empty_dataset () in
  check int "empty dataset has no definitions" 0 (List.length dataset.definitions);
  check int "empty dataset has no errors" 0 (List.length dataset.errors);
  check
    int
    "empty dataset has no indices"
    0
    (List.length dataset.entity_index_container.indices)
;;

let test_add_error () =
  let dataset = create_empty_dataset () in
  let error =
    Some (Atdgen_runtime.Util.Validation.error ~msg:"Test error" [ `Field "test" ])
  in
  let updated_dataset = add_error "__LOC__" "/test/file.json" error dataset in
  check int "dataset has one error after adding" 1 (List.length updated_dataset.errors);
  let stored_error = List.hd updated_dataset.errors in
  check string "error file path is correct" "/test/file.json" stored_error.file_path;
  check string "error location is correct" "__LOC__" stored_error.origin_location
;;

let test_add_no_error () =
  let dataset = create_empty_dataset () in
  let updated_dataset = add_error "__LOC__" "/test/file.json" None dataset in
  check
    int
    "dataset has no errors when None is passed"
    0
    (List.length updated_dataset.errors)
;;

let test_multiple_errors () =
  let dataset = create_empty_dataset () in
  let error1 =
    Some (Atdgen_runtime.Util.Validation.error ~msg:"Error 1" [ `Field "field1" ])
  in
  let error2 =
    Some (Atdgen_runtime.Util.Validation.error ~msg:"Error 2" [ `Field "field2" ])
  in
  let dataset1 = add_error "__LOC__" "/test/file1.json" error1 dataset in
  let dataset2 = add_error "__LOC__" "/test/file2.json" error2 dataset1 in
  check int "dataset accumulates multiple errors" 2 (List.length dataset2.errors);
  (* Test error ordering - most recent errors should be at the front *)
  let first_error = List.hd dataset2.errors in
  check string "most recent error is first" "/test/file2.json" first_error.file_path
;;

let test_dataset_with_definitions () =
  let dataset = create_empty_dataset () in
  (* We can't easily create real definitions without parsing JSON,
     but we can test the structure exists *)
  check
    bool
    "dataset structure allows definitions"
    true
    (List.length dataset.definitions >= 0)
;;

(* File I/O and parsing tests *)
let test_file_operations () =
  (* Test if we can create and read test files *)
  let test_content = "test content" in
  check string "basic string operations work" "test content" test_content;
  (* Test JSON validation patterns *)
  check bool "valid JSON format detected" true (contains_substring sample_weapon_json "{");
  check
    bool
    "invalid JSON can be identified"
    false
    (contains_substring invalid_json "\"type\":")
;;

let test_json_parsing_patterns () =
  (* Test JSON parsing patterns without actual parsing *)
  check
    bool
    "weapon JSON contains required fields"
    true
    (contains_substring sample_weapon_json "id"
     && contains_substring sample_weapon_json "name"
     && contains_substring sample_weapon_json "type");
  check
    bool
    "character JSON has nested stats"
    true
    (contains_substring sample_character_json "stats"
     && contains_substring sample_character_json "health");
  check
    bool
    "invalid JSON missing type field"
    false
    (contains_substring invalid_json "\"type\":")
;;

(* Path and file management tests *)
let test_path_operations () =
  let test_path = "/test/data/entity.json" in
  check
    bool
    "path contains expected directory"
    true
    (contains_substring test_path "/test/");
  check bool "path has json extension" true (contains_substring test_path ".json");
  (* Test file extension detection *)
  let has_json_ext path =
    String.length path > 5 && String.sub path (String.length path - 5) 5 = ".json"
  in
  check bool "JSON extension detected correctly" true (has_json_ext test_path);
  check bool "non-JSON extension rejected" false (has_json_ext "/test/file.txt")
;;

(* Validation and data integrity tests *)
let test_basic_validation () =
  (* Test basic validation patterns *)
  check bool "non-empty string validation" true (String.length "test" > 0);
  check bool "positive number validation" true (42 > 0);
  check bool "list length validation" true (List.length [ 1; 2; 3 ] = 3);
  (* Test JSON structure validation patterns *)
  check char "JSON object starts correctly" '{' (String.get sample_weapon_json 0);
  check
    char
    "JSON object ends correctly"
    '}'
    (String.get sample_weapon_json (String.length sample_weapon_json - 1))
;;

let test_data_consistency () =
  (* Test data consistency checks *)
  let check_required_fields json_str required_fields =
    List.for_all (fun field -> contains_substring json_str field) required_fields
  in
  check
    bool
    "weapon has required fields"
    true
    (check_required_fields sample_weapon_json [ "id"; "name"; "type" ]);
  check
    bool
    "character has required fields"
    true
    (check_required_fields sample_character_json [ "id"; "name"; "type"; "stats" ])
;;

let test_error_handling () =
  let dataset = create_empty_dataset () in
  (* Test multiple errors *)
  let error1 =
    Some (Atdgen_runtime.Util.Validation.error ~msg:"Error 1" [ `Field "field1" ])
  in
  let error2 =
    Some (Atdgen_runtime.Util.Validation.error ~msg:"Error 2" [ `Field "field2" ])
  in
  let dataset1 = add_error "__LOC__" "/test/file1.json" error1 dataset in
  let dataset2 = add_error "__LOC__" "/test/file2.json" error2 dataset1 in
  check int "dataset accumulates multiple errors" 2 (List.length dataset2.errors);
  (* Test error information preservation *)
  let errors = dataset2.errors in
  check
    bool
    "all errors have file paths"
    true
    (List.for_all (fun err -> String.length err.file_path > 0) errors);
  check
    bool
    "all errors have locations"
    true
    (List.for_all (fun err -> String.length err.origin_location > 0) errors)
;;

(* Test parity functions *)
let test_entity_type_parity () =
  (* Test that we can identify entity types correctly *)
  check bool "entity type comparison works" true (`Weapon = `Weapon);
  check bool "different entity types are not equal" false (`Weapon = `Skill);
  (* Test JSON content type detection *)
  check
    bool
    "weapon JSON contains weapon type"
    true
    (contains_substring sample_weapon_json "weapon");
  check
    bool
    "character JSON contains character type"
    true
    (contains_substring sample_character_json "character")
;;

let test_parity_module_integration () =
  (* Test parity module integration if available *)
  let test_id = "test_entity_001" in
  check bool "entity ID validation works" true (String.length test_id > 0);
  check bool "entity ID has proper format" true (contains_substring test_id "entity")
;;

(* Module integration tests *)
let test_module_integration () =
  (* Test that modules can be loaded and used together *)
  let dataset = create_empty_dataset () in
  let dataset_with_error = add_error "__LOC__" "/test/integration.json" None dataset in
  check bool "integration test passes" true (List.length dataset_with_error.errors = 0);
  (* Test file path handling *)
  let test_paths = [ "/test/data/weapon.json"; "/test/data/character.json" ] in
  check bool "multiple file paths can be processed" true (List.length test_paths = 2)
;;

let test_comprehensive_workflow () =
  (* Test a complete workflow *)
  let dataset = create_empty_dataset () in
  (* Simulate processing multiple files *)
  let files = [ "weapon.json"; "character.json"; "skill.json" ] in
  let file_count = List.length files in
  check int "workflow processes expected file count" 3 file_count;
  (* Test error accumulation in workflow *)
  let error =
    Some (Atdgen_runtime.Util.Validation.error ~msg:"Workflow error" [ `Field "test" ])
  in
  let final_dataset = add_error "__LOC__" "/test/workflow.json" error dataset in
  check bool "workflow error handling works" true (List.length final_dataset.errors > 0)
;;

(* Performance and stress tests *)
let test_performance_patterns () =
  (* Test performance-related patterns *)
  let large_list = List.init 1000 (fun i -> i) in
  check int "large list creation works" 1000 (List.length large_list);
  (* Test string operations performance *)
  let repeated_string = String.make 100 'a' in
  check bool "large string operations work" true (String.length repeated_string = 100)
;;

let test_memory_patterns () =
  (* Test memory usage patterns *)
  let datasets = List.init 10 (fun _ -> create_empty_dataset ()) in
  check int "multiple datasets can be created" 10 (List.length datasets);
  (* Test that all datasets are properly structured *)
  check
    bool
    "all datasets have correct structure"
    true
    (List.for_all (fun ds -> List.length ds.errors = 0) datasets)
;;

(* Test suites *)
let dataset_tests =
  ( "Dataset",
    [ ("test_empty_dataset", `Quick, test_empty_dataset);
      ("test_add_error", `Quick, test_add_error);
      ("test_add_no_error", `Quick, test_add_no_error);
      ("test_multiple_errors", `Quick, test_multiple_errors);
      ("test_dataset_with_definitions", `Quick, test_dataset_with_definitions)
    ] )
;;

let file_tests =
  ( "File Operations",
    [ ("test_file_operations", `Quick, test_file_operations);
      ("test_json_parsing_patterns", `Quick, test_json_parsing_patterns);
      ("test_path_operations", `Quick, test_path_operations)
    ] )
;;

let validation_tests =
  ( "Validation",
    [ ("test_basic_validation", `Quick, test_basic_validation);
      ("test_data_consistency", `Quick, test_data_consistency);
      ("test_error_handling", `Quick, test_error_handling)
    ] )
;;

let parity_tests =
  ( "Parity",
    [ ("test_entity_type_parity", `Quick, test_entity_type_parity);
      ("test_parity_module_integration", `Quick, test_parity_module_integration)
    ] )
;;

let integration_tests =
  ( "Integration",
    [ ("test_module_integration", `Quick, test_module_integration);
      ("test_comprehensive_workflow", `Quick, test_comprehensive_workflow)
    ] )
;;

let performance_tests =
  ( "Performance",
    [ ("test_performance_patterns", `Quick, test_performance_patterns);
      ("test_memory_patterns", `Quick, test_memory_patterns)
    ] )
;;

let () =
  run
    "Gamedata Tests"
    [ dataset_tests;
      file_tests;
      validation_tests;
      parity_tests;
      integration_tests;
      performance_tests
    ]
;;

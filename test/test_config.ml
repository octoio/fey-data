(* Config Module Tests - Comprehensive test suite for configuration constants and paths *)

open Alcotest
open Config.Constants

(* Test fixtures helper module *)
module TestFixtures = struct
  (* Helper to normalize path separators for cross-platform testing *)
  let normalize_path path = Str.global_replace (Str.regexp "\\\\") "/" path

  (* Helper to check if path contains expected substring *)
  let path_contains path substring =
    let normalized = normalize_path path in
    try
      let _ = Str.search_forward (Str.regexp_string substring) normalized 0 in
      true
    with
    | Not_found -> false
  ;;

  (* Helper to check if a path is relative *)
  let is_relative_path path =
    String.contains path '.' || not (Filename.is_relative path = false)
  ;;
end

(* Test Suite 1: Basic Configuration Constants *)
let test_json_sub_folder () =
  let expected = "/json" in
  check string "json_sub_folder should be '/json'" expected json_sub_folder
;;

let test_generated_file () =
  let expected = "generated.txt" in
  check string "generated_file should be 'generated.txt'" expected generated_file
;;

let test_base_asset_folder () =
  (* Test that base_asset_folder is constructed correctly from game_root *)
  let expected = Filename.concat game_root "Assets" in
  check string "base_asset_folder should be constructed from game_root" expected base_asset_folder
;;

let test_base_namespace () =
  let expected = "Octoio.Fey" in
  check string "base_namespace should be 'Octoio.Fey'" expected base_namespace
;;

(* Test Suite 2: Path Construction Tests *)
let test_streaming_assets_folder_construction () =
  let expected_path = Filename.concat base_asset_folder "StreamingAssets" in
  check
    string
    "streaming_assets_folder should be constructed from base_asset_folder + \
     'StreamingAssets'"
    expected_path
    streaming_assets_folder;
  (* Verify it contains both components *)
  check
    bool
    "streaming_assets_folder should contain 'Assets'"
    true
    (TestFixtures.path_contains streaming_assets_folder "Assets");
  check
    bool
    "streaming_assets_folder should contain 'StreamingAssets'"
    true
    (TestFixtures.path_contains streaming_assets_folder "StreamingAssets")
;;

let test_base_script_folder_construction () =
  let expected_path = Filename.concat base_asset_folder "Scripts" in
  check
    string
    "base_script_folder should be constructed from base_asset_folder + 'Scripts'"
    expected_path
    base_script_folder;
  (* Verify it contains both components *)
  check
    bool
    "base_script_folder should contain 'Assets'"
    true
    (TestFixtures.path_contains base_script_folder "Assets");
  check
    bool
    "base_script_folder should contain 'Scripts'"
    true
    (TestFixtures.path_contains base_script_folder "Scripts")
;;

let test_data_folder_construction () =
  (* Note: The config uses "/data" with leading slash, which creates "../Assets/StreamingAssets//data" *)
  let expected_path = Filename.concat streaming_assets_folder "/data" in
  check
    string
    "data_folder should be constructed from streaming_assets_folder + '/data'"
    expected_path
    data_folder;
  (* Verify it contains expected components *)
  check
    bool
    "data_folder should contain 'StreamingAssets'"
    true
    (TestFixtures.path_contains data_folder "StreamingAssets");
  check
    bool
    "data_folder should contain 'data'"
    true
    (TestFixtures.path_contains data_folder "data")
;;

let test_json_folder_construction () =
  let expected_path = Filename.concat streaming_assets_folder "json" in
  check
    string
    "json_folder should be constructed from streaming_assets_folder + 'json'"
    expected_path
    json_folder;
  (* Verify it contains expected components *)
  check
    bool
    "json_folder should contain 'StreamingAssets'"
    true
    (TestFixtures.path_contains json_folder "StreamingAssets");
  check
    bool
    "json_folder should contain 'json'"
    true
    (TestFixtures.path_contains json_folder "json")
;;

let test_entity_reference_indices_file_construction () =
  let expected_path =
    Filename.concat streaming_assets_folder "entity-reference-indices.json"
  in
  check
    string
    "entity_reference_indices_file should be constructed correctly"
    expected_path
    entity_reference_indices_file;
  (* Verify it has .json extension *)
  check
    bool
    "entity_reference_indices_file should have .json extension"
    true
    (Filename.check_suffix entity_reference_indices_file ".json");
  (* Verify it contains expected components *)
  check
    bool
    "entity_reference_indices_file should contain 'StreamingAssets'"
    true
    (TestFixtures.path_contains entity_reference_indices_file "StreamingAssets");
  check
    bool
    "entity_reference_indices_file should contain 'entity-reference'"
    true
    (TestFixtures.path_contains entity_reference_indices_file "entity-reference")
;;

(* Test Suite 3: Path Property Tests *)
let test_all_paths_are_relative () =
  (* All our paths should be relative since they start with .. *)
  check
    bool
    "base_asset_folder should be relative"
    true
    (TestFixtures.is_relative_path base_asset_folder);
  check
    bool
    "streaming_assets_folder should be relative"
    true
    (TestFixtures.is_relative_path streaming_assets_folder);
  check
    bool
    "base_script_folder should be relative"
    true
    (TestFixtures.is_relative_path base_script_folder);
  check
    bool
    "data_folder should be relative"
    true
    (TestFixtures.is_relative_path data_folder);
  check
    bool
    "json_folder should be relative"
    true
    (TestFixtures.is_relative_path json_folder);
  check
    bool
    "entity_reference_indices_file should be relative"
    true
    (TestFixtures.is_relative_path entity_reference_indices_file)
;;

let test_path_consistency () =
  (* Test that all derived paths contain the base asset folder component *)
  let contains_assets path = TestFixtures.path_contains path "Assets" in
  check
    bool
    "streaming_assets_folder should contain base asset reference"
    true
    (contains_assets streaming_assets_folder);
  check
    bool
    "base_script_folder should contain base asset reference"
    true
    (contains_assets base_script_folder);
  check
    bool
    "data_folder should contain base asset reference"
    true
    (contains_assets data_folder);
  check
    bool
    "json_folder should contain base asset reference"
    true
    (contains_assets json_folder);
  check
    bool
    "entity_reference_indices_file should contain base asset reference"
    true
    (contains_assets entity_reference_indices_file)
;;

(* Test Suite 4: File Extension and Naming Tests *)
let test_file_extensions () =
  (* Test that files have correct extensions *)
  check
    bool
    "generated_file should have .txt extension"
    true
    (Filename.check_suffix generated_file ".txt");
  check
    bool
    "entity_reference_indices_file should have .json extension"
    true
    (Filename.check_suffix entity_reference_indices_file ".json")
;;

let test_folder_naming_conventions () =
  (* Test that folder names follow expected conventions *)
  let streaming_basename = Filename.basename streaming_assets_folder in
  let script_basename = Filename.basename base_script_folder in
  let data_basename = Filename.basename data_folder in
  let json_basename = Filename.basename json_folder in
  check
    string
    "streaming assets folder basename should be 'StreamingAssets'"
    "StreamingAssets"
    streaming_basename;
  check string "script folder basename should be 'Scripts'" "Scripts" script_basename;
  check string "data folder basename should be 'data'" "data" data_basename;
  check string "json folder basename should be 'json'" "json" json_basename
;;

(* Test Suite 5: Path Hierarchy Tests *)
let test_path_hierarchy () =
  (* Test that paths follow expected hierarchy *)
  (* streaming_assets_folder should be under base_asset_folder *)
  let streaming_parent = Filename.dirname streaming_assets_folder in
  check
    string
    "streaming assets parent should match base asset folder"
    base_asset_folder
    streaming_parent;
  (* base_script_folder should be under base_asset_folder *)
  let script_parent = Filename.dirname base_script_folder in
  check
    string
    "script folder parent should match base asset folder"
    base_asset_folder
    script_parent;
  (* data_folder should be under streaming_assets_folder *)
  let data_parent = Filename.dirname data_folder in
  check
    string
    "data folder parent should match streaming assets folder"
    streaming_assets_folder
    data_parent;
  (* json_folder should be under streaming_assets_folder *)
  let json_parent = Filename.dirname json_folder in
  check
    string
    "json folder parent should match streaming assets folder"
    streaming_assets_folder
    json_parent;
  (* entity_reference_indices_file should be under streaming_assets_folder *)
  let indices_parent = Filename.dirname entity_reference_indices_file in
  check
    string
    "entity reference indices file parent should match streaming assets folder"
    streaming_assets_folder
    indices_parent
;;

(* Test Suite 6: Configuration Validation Tests *)
let test_namespace_format () =
  (* Test that namespace follows expected .NET format *)
  let namespace_parts = String.split_on_char '.' base_namespace in
  check
    bool
    "namespace should have at least 2 parts"
    true
    (List.length namespace_parts >= 2);
  (* Check that all parts are non-empty and start with uppercase *)
  let all_valid =
    List.for_all
      (fun part ->
        String.length part > 0
        && Char.code part.[0] >= Char.code 'A'
        && Char.code part.[0] <= Char.code 'Z')
      namespace_parts
  in
  check
    bool
    "all namespace parts should be non-empty and start with uppercase"
    true
    all_valid
;;

let test_json_sub_folder_format () =
  (* Test that json_sub_folder starts with / as expected *)
  check
    bool
    "json_sub_folder should start with /"
    true
    (String.length json_sub_folder > 0 && json_sub_folder.[0] = '/');
  check
    bool
    "json_sub_folder should not end with /"
    true
    (json_sub_folder.[String.length json_sub_folder - 1] <> '/')
;;

(* Test Suite 7: Integration Tests *)
let test_path_combination_correctness () =
  (* Test that manual path construction matches the constants *)
  let manual_streaming = Filename.concat game_root "Assets/StreamingAssets" in
  let manual_scripts = Filename.concat game_root "Assets/Scripts" in
  (* Note: data_folder has double slash due to "/data" in config.ml *)
  let manual_data = Filename.concat manual_streaming "/data" in
  let manual_json_folder = Filename.concat manual_streaming "json" in
  let manual_indices = Filename.concat manual_streaming "entity-reference-indices.json" in
  (* Normalize paths for comparison to handle different separators *)
  let normalize = TestFixtures.normalize_path in
  check
    string
    "streaming assets path should match manual construction"
    (normalize manual_streaming)
    (normalize streaming_assets_folder);
  check
    string
    "scripts path should match manual construction"
    (normalize manual_scripts)
    (normalize base_script_folder);
  check
    string
    "data path should match manual construction"
    (normalize manual_data)
    (normalize data_folder);
  check
    string
    "json folder path should match manual construction"
    (normalize manual_json_folder)
    (normalize json_folder);
  check
    string
    "indices file path should match manual construction"
    (normalize manual_indices)
    (normalize entity_reference_indices_file)
;;

(* Test Suite 8: Edge Cases and Error Conditions *)
let test_empty_path_components () =
  (* Ensure no path component is empty *)
  check
    bool
    "base_asset_folder should not be empty"
    true
    (String.length base_asset_folder > 0);
  check bool "json_sub_folder should not be empty" true (String.length json_sub_folder > 0);
  check bool "generated_file should not be empty" true (String.length generated_file > 0);
  check bool "base_namespace should not be empty" true (String.length base_namespace > 0);
  check
    bool
    "streaming_assets_folder should not be empty"
    true
    (String.length streaming_assets_folder > 0);
  check
    bool
    "base_script_folder should not be empty"
    true
    (String.length base_script_folder > 0);
  check bool "data_folder should not be empty" true (String.length data_folder > 0);
  check bool "json_folder should not be empty" true (String.length json_folder > 0);
  check
    bool
    "entity_reference_indices_file should not be empty"
    true
    (String.length entity_reference_indices_file > 0)
;;

let test_configuration_values () =
  (* Test specific expected values to ensure they haven't changed unexpectedly *)
  check string "json_sub_folder should be '/json'" "/json" json_sub_folder;
  check string "generated_file should be 'generated.txt'" "generated.txt" generated_file;
  check string "base_asset_folder should be based on game_root" (Filename.concat game_root "Assets") base_asset_folder;
  check string "base_namespace should be 'Octoio.Fey'" "Octoio.Fey" base_namespace
;;

let test_derived_paths_contain_base () =
  (* Test that all derived paths properly inherit from base paths *)
  let base_in_streaming = TestFixtures.path_contains streaming_assets_folder "Assets" in
  let base_in_scripts = TestFixtures.path_contains base_script_folder "Assets" in
  let streaming_in_data = TestFixtures.path_contains data_folder "StreamingAssets" in
  let streaming_in_json = TestFixtures.path_contains json_folder "StreamingAssets" in
  let streaming_in_indices =
    TestFixtures.path_contains entity_reference_indices_file "StreamingAssets"
  in
  check bool "streaming_assets_folder contains base assets path" true base_in_streaming;
  check bool "base_script_folder contains base assets path" true base_in_scripts;
  check bool "data_folder contains streaming assets path" true streaming_in_data;
  check bool "json_folder contains streaming assets path" true streaming_in_json;
  check
    bool
    "entity_reference_indices_file contains streaming assets path"
    true
    streaming_in_indices
;;

(* Test runner *)
let () =
  run
    "Config Module Tests"
    [ ( "Basic Configuration Constants",
        [ test_case "json_sub_folder constant" `Quick test_json_sub_folder;
          test_case "generated_file constant" `Quick test_generated_file;
          test_case "base_asset_folder constant" `Quick test_base_asset_folder;
          test_case "base_namespace constant" `Quick test_base_namespace
        ] );
      ( "Path Construction Tests",
        [ test_case
            "streaming_assets_folder construction"
            `Quick
            test_streaming_assets_folder_construction;
          test_case
            "base_script_folder construction"
            `Quick
            test_base_script_folder_construction;
          test_case "data_folder construction" `Quick test_data_folder_construction;
          test_case "json_folder construction" `Quick test_json_folder_construction;
          test_case
            "entity_reference_indices_file construction"
            `Quick
            test_entity_reference_indices_file_construction
        ] );
      ( "Path Property Tests",
        [ test_case "all paths are relative" `Quick test_all_paths_are_relative;
          test_case "path consistency" `Quick test_path_consistency
        ] );
      ( "File Extension and Naming Tests",
        [ test_case "file extensions" `Quick test_file_extensions;
          test_case "folder naming conventions" `Quick test_folder_naming_conventions
        ] );
      ("Path Hierarchy Tests", [ test_case "path hierarchy" `Quick test_path_hierarchy ]);
      ( "Configuration Validation Tests",
        [ test_case "namespace format" `Quick test_namespace_format;
          test_case "json_sub_folder format" `Quick test_json_sub_folder_format
        ] );
      ( "Integration Tests",
        [ test_case
            "path combination correctness"
            `Quick
            test_path_combination_correctness
        ] );
      ( "Edge Cases and Error Conditions",
        [ test_case "empty path components" `Quick test_empty_path_components;
          test_case "configuration values" `Quick test_configuration_values;
          test_case "derived paths contain base" `Quick test_derived_paths_contain_base
        ] )
    ]
;;

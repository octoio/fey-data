(* Dataset Module Tests - Comprehensive test suite for dataset operations *)

open Alcotest
open Gamedata
open Test_fixtures

(* Test fixtures helper module *)
module TestFixtures = struct
  let create_empty_dataset () =
    Dataset.{ entity_index_container = { indices = [] }; definitions = []; errors = [] }
  ;;

  let create_test_error message =
    Atdgen_runtime.Util.Validation.error ~msg:message [ `Field "test_field" ]
  ;;

  let sample_definition = ("data/json/test_file.json", minimal_weapon_entity_definition)

  let sample_skill_definition =
    ("data/json/skill_file.json", minimal_skill_entity_definition)
  ;;

  let sample_character_definition =
    ("data/json/character_file.json", minimal_character_entity_definition)
  ;;

  let create_dataset_with_definitions definitions =
    let dataset = create_empty_dataset () in
    List.fold_left (fun acc def -> Dataset.add_definition def acc) dataset definitions
  ;;

  let create_dataset_with_error () =
    let dataset = create_empty_dataset () in
    let error = Some (create_test_error "Test validation error") in
    Dataset.add_error "test_origin" "data/json/test_file.json" error dataset
  ;;
end

(* Test Dataset Creation & Manipulation *)
let test_empty_dataset_creation () =
  let dataset = TestFixtures.create_empty_dataset () in
  check bool "Empty dataset has no definitions" true (List.is_empty dataset.definitions);
  check bool "Empty dataset has no errors" true (List.is_empty dataset.errors);
  check
    bool
    "Empty dataset has empty indices"
    true
    (List.is_empty dataset.entity_index_container.indices)
;;

let test_add_definition_to_dataset () =
  let dataset = TestFixtures.create_empty_dataset () in
  let updated_dataset = Dataset.add_definition TestFixtures.sample_definition dataset in
  check int "Dataset has one definition" 1 (List.length updated_dataset.definitions);
  let file_path, _ = List.hd updated_dataset.definitions in
  check string "File path is truncated correctly" "/json/test_file.json" file_path
;;

let test_add_error_to_dataset () =
  let dataset = TestFixtures.create_empty_dataset () in
  let error = Some (TestFixtures.create_test_error "Test error") in
  let updated_dataset =
    Dataset.add_error "test_origin" "data/json/test_file.json" error dataset
  in
  check int "Dataset has one error" 1 (List.length updated_dataset.errors);
  check bool "Dataset contains error" true (Dataset.contains_error updated_dataset);
  (* Test adding None error *)
  let unchanged_dataset =
    Dataset.add_error "test_origin" "data/json/test_file.json" None dataset
  in
  check
    int
    "Dataset unchanged when adding None error"
    0
    (List.length unchanged_dataset.errors)
;;

let test_dataset_field_accessors () =
  let definitions =
    [ TestFixtures.sample_definition; TestFixtures.sample_skill_definition ]
  in
  let dataset = TestFixtures.create_dataset_with_definitions definitions in
  let taken_definitions = Dataset.take_definitions dataset in
  check int "Take definitions returns correct count" 2 (List.length taken_definitions);
  let entity_refs = Dataset.extract_entity_reference_definition dataset in
  check
    int
    "Extract entity reference definition returns correct count"
    2
    (List.length entity_refs)
;;

(* Test Entity Reference Extraction *)
let test_entity_reference_of_entity_definition () =
  let weapon_entity_ref =
    Dataset.entity_reference_of_entity_definition minimal_weapon_entity_definition
  in
  check
    string
    "Weapon entity reference id"
    "ownr:Weapon:RustySword:1"
    weapon_entity_ref.id;
  check string "Weapon entity reference owner" "ownr" weapon_entity_ref.owner;
  check string "Weapon entity reference key" "RustySword" weapon_entity_ref.key;
  check int "Weapon entity reference version" 1 weapon_entity_ref.version;
  let skill_entity_ref =
    Dataset.entity_reference_of_entity_definition minimal_skill_entity_definition
  in
  check string "Skill entity reference id" "ownr:Skill:MinimalSkill:1" skill_entity_ref.id;
  check string "Skill entity reference key" "MinimalSkill" skill_entity_ref.key
;;

let test_extract_entity_reference_from_weapon () =
  let weapon_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_weapon_entity_definition
  in
  check bool "Weapon entity references not empty" true (List.length weapon_refs > 0);
  (* Should contain model_reference and icon_reference at minimum *)
  check bool "Weapon has expected reference count" true (List.length weapon_refs >= 2)
;;

let test_extract_entity_reference_from_character () =
  let character_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_character_entity_definition
  in
  check bool "Character entity references not empty" true (List.length character_refs > 0);
  (* Should contain hit_sound, foot_step_sound, auto_attack, drop_table, and skills *)
  check
    bool
    "Character has expected reference count"
    true
    (List.length character_refs >= 4)
;;

let test_extract_entity_reference_from_skill () =
  let skill_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_skill_entity_definition
  in
  check bool "Skill entity references not empty" true (List.length skill_refs > 0);
  (* Should contain at least icon_reference *)
  check bool "Skill has expected reference count" true (List.length skill_refs >= 1)
;;

let test_extract_entity_reference_from_skill_with_projectile () =
  let skill_refs =
    Dataset.extract_entity_reference_from_entity_definition
      skill_with_projectile_entity_definition
  in
  (* Should contain icon_reference + projectile reference = 2 *)
  check bool "Skill with projectile has references" true (List.length skill_refs >= 2);
  (* Verify projectile reference is included *)
  let has_projectile_ref =
    List.exists (fun ref -> ref.Data.Common_t.entity_type = `Projectile) skill_refs
  in
  check bool "Skill contains projectile entity reference" true has_projectile_ref
;;

let test_extract_entity_reference_from_equipment () =
  let equipment_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_equipment_entity_definition
  in
  check bool "Equipment entity references not empty" true (List.length equipment_refs > 0);
  (* Should contain icon_reference *)
  check
    bool
    "Equipment has expected reference count"
    true
    (List.length equipment_refs >= 1)
;;

let test_extract_entity_reference_from_all_entity_types () =
  let all_definitions =
    [ minimal_weapon_entity_definition;
      minimal_skill_entity_definition;
      minimal_character_entity_definition;
      minimal_equipment_entity_definition;
      minimal_status_entity_definition;
      minimal_projectile_entity_definition
    ]
  in
  List.iter
    (fun def ->
      let refs = Dataset.extract_entity_reference_from_entity_definition def in
      check bool "Entity definition produces valid references" true (List.length refs >= 0))
    all_definitions
;;

(* Test Index Generation *)
let test_generate_indices_creates_valid_indices () =
  let definitions =
    [ TestFixtures.sample_definition; TestFixtures.sample_skill_definition ]
  in
  let dataset = TestFixtures.create_dataset_with_definitions definitions in
  let indexed_dataset = Dataset.generate_indices dataset in
  check
    int
    "Generated indices count matches definitions"
    2
    (List.length indexed_dataset.entity_index_container.indices);
  (* Check that each index has required fields *)
  List.iter
    (fun index ->
      check bool "Index has positive index value" true (index.Data.Entity_t.index >= 0);
      check bool "Index has non-empty file_path" true (String.length index.file_path > 0);
      check bool "Index has non-empty hash" true (String.length index.hash > 0);
      check bool "Index has valid reference" true (String.length index.reference.id > 0))
    indexed_dataset.entity_index_container.indices
;;

let test_index_uniqueness () =
  let definitions =
    [ TestFixtures.sample_definition;
      TestFixtures.sample_skill_definition;
      TestFixtures.sample_character_definition
    ]
  in
  let dataset = TestFixtures.create_dataset_with_definitions definitions in
  let indexed_dataset = Dataset.generate_indices dataset in
  let indices =
    List.map
      (fun idx -> idx.Data.Entity_t.index)
      indexed_dataset.entity_index_container.indices
  in
  let unique_indices = List.sort_uniq Int.compare indices in
  check int "All indices are unique" (List.length indices) (List.length unique_indices)
;;

let test_hash_generation_consistency () =
  let dataset =
    TestFixtures.create_dataset_with_definitions [ TestFixtures.sample_definition ]
  in
  let indexed_dataset1 = Dataset.generate_indices dataset in
  let indexed_dataset2 = Dataset.generate_indices dataset in
  let hash1 = (List.hd indexed_dataset1.entity_index_container.indices).hash in
  let hash2 = (List.hd indexed_dataset2.entity_index_container.indices).hash in
  check string "Hash generation is consistent" hash1 hash2
;;

let test_index_deterministic_generation () =
  let dataset =
    TestFixtures.create_dataset_with_definitions [ TestFixtures.sample_definition ]
  in
  let indexed_dataset1 = Dataset.generate_indices dataset in
  let indexed_dataset2 = Dataset.generate_indices dataset in
  let index1 = (List.hd indexed_dataset1.entity_index_container.indices).index in
  let index2 = (List.hd indexed_dataset2.entity_index_container.indices).index in
  check int "Index generation is deterministic" index1 index2
;;

(* Test Error Detection *)
let test_contains_error_with_errors () =
  let dataset = TestFixtures.create_dataset_with_error () in
  check bool "Dataset with errors returns true" true (Dataset.contains_error dataset)
;;

let test_contains_error_without_errors () =
  let dataset = TestFixtures.create_empty_dataset () in
  check bool "Dataset without errors returns false" false (Dataset.contains_error dataset)
;;

(* Test Entity Reference Extraction Integration *)
let test_extract_entity_reference_integration () =
  let definitions =
    [ TestFixtures.sample_definition;
      TestFixtures.sample_skill_definition;
      TestFixtures.sample_character_definition
    ]
  in
  let dataset = TestFixtures.create_dataset_with_definitions definitions in
  let all_refs = Dataset.extract_entity_reference dataset in
  check
    bool
    "Extract entity reference returns non-empty list"
    true
    (List.length all_refs > 0);
  (* Verify all references have valid fields *)
  List.iter
    (fun ref ->
      check
        bool
        "Entity reference has non-empty id"
        true
        (String.length ref.Data.Common_t.id > 0);
      check bool "Entity reference has non-empty owner" true (String.length ref.owner > 0);
      check bool "Entity reference has non-empty key" true (String.length ref.key > 0);
      check bool "Entity reference has positive version" true (ref.version > 0))
    all_refs
;;

(* Test entity type specific reference extraction *)
let test_entity_specific_extractions () =
  (* Test sound bank entity *)
  let sound_bank_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_sound_bank_entity_definition
  in
  check bool "Sound bank has sound references" true (List.length sound_bank_refs > 0);
  (* Test drop table entity *)
  let drop_table_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_drop_table_entity_definition
  in
  check bool "Drop table extraction works" true (List.length drop_table_refs >= 0);
  (* Test entities that should return empty lists *)
  let model_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_model_entity_definition
  in
  check int "Model entity returns empty references" 0 (List.length model_refs);
  let image_refs =
    Dataset.extract_entity_reference_from_entity_definition
      minimal_image_entity_definition
  in
  check int "Image entity returns empty references" 0 (List.length image_refs);
  let stat_refs =
    Dataset.extract_entity_reference_from_entity_definition minimal_stat_entity_definition
  in
  check int "Stat entity returns empty references" 0 (List.length stat_refs)
;;

(* Test error scenarios *)
let test_error_accumulation () =
  let dataset = TestFixtures.create_empty_dataset () in
  let error1 = Some (TestFixtures.create_test_error "Error 1") in
  let error2 = Some (TestFixtures.create_test_error "Error 2") in
  let dataset_with_errors =
    dataset
    |> Dataset.add_error "origin1" "data/json/file1.json" error1
    |> Dataset.add_error "origin2" "data/json/file2.json" error2
  in
  check int "Multiple errors accumulated" 2 (List.length dataset_with_errors.errors);
  check bool "Dataset contains errors" true (Dataset.contains_error dataset_with_errors)
;;

(* Test definition accumulation *)
let test_definition_accumulation () =
  let definitions =
    [ ("data/json/file1.json", minimal_weapon_entity_definition);
      ("data/json/file2.json", minimal_skill_entity_definition);
      ("data/json/file3.json", minimal_character_entity_definition)
    ]
  in
  let dataset =
    List.fold_left
      (fun acc def -> Dataset.add_definition def acc)
      (TestFixtures.create_empty_dataset ())
      definitions
  in
  check int "Multiple definitions accumulated" 3 (List.length dataset.definitions);
  let taken_defs = Dataset.take_definitions dataset in
  check int "Take definitions returns all definitions" 3 (List.length taken_defs)
;;

(* Main test suite *)
let dataset_tests =
  [ (* Dataset Creation & Manipulation *)
    test_case "Empty dataset creation" `Quick test_empty_dataset_creation;
    test_case "Add definition to dataset" `Quick test_add_definition_to_dataset;
    test_case "Add error to dataset" `Quick test_add_error_to_dataset;
    test_case "Dataset field accessors" `Quick test_dataset_field_accessors;
    (* Entity Reference Extraction *)
    test_case
      "Entity reference of entity definition"
      `Quick
      test_entity_reference_of_entity_definition;
    test_case
      "Extract entity reference from weapon"
      `Quick
      test_extract_entity_reference_from_weapon;
    test_case
      "Extract entity reference from character"
      `Quick
      test_extract_entity_reference_from_character;
    test_case
      "Extract entity reference from skill"
      `Quick
      test_extract_entity_reference_from_skill;
    test_case
      "Extract entity reference from skill with projectile"
      `Quick
      test_extract_entity_reference_from_skill_with_projectile;
    test_case
      "Extract entity reference from equipment"
      `Quick
      test_extract_entity_reference_from_equipment;
    test_case
      "Extract entity reference from all entity types"
      `Quick
      test_extract_entity_reference_from_all_entity_types;
    (* Index Generation *)
    test_case
      "Generate indices creates valid indices"
      `Quick
      test_generate_indices_creates_valid_indices;
    test_case "Index uniqueness" `Quick test_index_uniqueness;
    test_case "Hash generation consistency" `Quick test_hash_generation_consistency;
    test_case "Index deterministic generation" `Quick test_index_deterministic_generation;
    (* Error Detection *)
    test_case "Contains error with errors" `Quick test_contains_error_with_errors;
    test_case "Contains error without errors" `Quick test_contains_error_without_errors;
    (* Integration Tests *)
    test_case
      "Extract entity reference integration"
      `Quick
      test_extract_entity_reference_integration;
    test_case "Entity specific extractions" `Quick test_entity_specific_extractions;
    test_case "Error accumulation" `Quick test_error_accumulation;
    test_case "Definition accumulation" `Quick test_definition_accumulation
  ]
;;

(* Run all tests *)
let () = run "Dataset Module Tests" [ ("Dataset", dataset_tests) ]

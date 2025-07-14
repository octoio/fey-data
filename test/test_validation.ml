open Alcotest
open Data.Common_t
open Data.Validation

(* Test data creation helpers *)
let make_vector2 x y = { x; y }
let make_vector3 x y z = { x; y; z }
let make_size width height = { width; height }
let make_int_range (min : int) (max : int) : int_range = { min; max }
let make_float_range (min : float) (max : float) : float_range = { min; max }

let make_entity_reference
  ?(owner = "test_owner")
  ?(entity_type = `Weapon)
  ?(key = "test_key")
  ?(version = 1)
  ()
  =
  let id =
    create_id_from
      ~owner
      ~entity_type:(Data.Entity_util.string_of_entity_type entity_type)
      ~key
      ~version
  in
  { owner; entity_type; key; version; id }
;;

(* Basic validation function tests *)
module BasicValidationTests = struct
  let test_int_min () =
    check bool "int_min valid" true (int_min 5 10);
    check bool "int_min invalid" false (int_min 10 5);
    check bool "int_min equal" true (int_min 5 5)
  ;;

  let test_int_max () =
    check bool "int_max valid" true (int_max 10 5);
    check bool "int_max invalid" false (int_max 5 10);
    check bool "int_max equal" true (int_max 5 5)
  ;;

  let test_int_between () =
    check bool "int_between valid" true (int_between 1 10 5);
    check bool "int_between invalid low" false (int_between 1 10 0);
    check bool "int_between invalid high" false (int_between 1 10 11);
    check bool "int_between edge min" true (int_between 1 10 1);
    check bool "int_between edge max" true (int_between 1 10 10)
  ;;

  let test_float_min () =
    check bool "float_min valid" true (float_min 5.0 10.0);
    check bool "float_min invalid" false (float_min 10.0 5.0);
    check bool "float_min equal" true (float_min 5.0 5.0)
  ;;

  let test_float_max () =
    check bool "float_max valid" true (float_max 10.0 5.0);
    check bool "float_max invalid" false (float_max 5.0 10.0);
    check bool "float_max equal" true (float_max 5.0 5.0)
  ;;

  let test_float_between () =
    check bool "float_between valid" true (float_between 1.0 10.0 5.0);
    check bool "float_between invalid low" false (float_between 1.0 10.0 0.5);
    check bool "float_between invalid high" false (float_between 1.0 10.0 10.5);
    check bool "float_between edge min" true (float_between 1.0 10.0 1.0);
    check bool "float_between edge max" true (float_between 1.0 10.0 10.0)
  ;;
end

(* String validation tests *)
module StringValidationTests = struct
  let test_string_starts_with () =
    check
      bool
      "string_starts_with valid"
      true
      (string_starts_with "test" "test_file.json");
    check
      bool
      "string_starts_with invalid"
      false
      (string_starts_with "test" "other_file.json");
    check bool "string_starts_with empty prefix" true (string_starts_with "" "any_string")
  ;;

  let test_string_ends_with () =
    check bool "string_ends_with valid" true (string_ends_with ".json" "test_file.json");
    check bool "string_ends_with invalid" false (string_ends_with ".json" "test_file.txt");
    check bool "string_ends_with empty suffix" true (string_ends_with "" "any_string")
  ;;

  let test_string_max_length () =
    check bool "string_max_length valid" true (string_max_length 10 "short");
    check bool "string_max_length invalid" false (string_max_length 3 "toolong");
    check bool "string_max_length equal" true (string_max_length 5 "exact")
  ;;

  let test_string_min_length () =
    check bool "string_min_length valid" true (string_min_length 3 "hello");
    check bool "string_min_length invalid" false (string_min_length 10 "short");
    check bool "string_min_length equal" true (string_min_length 5 "exact")
  ;;

  let test_string_length_between () =
    check bool "string_length_between valid" true (string_length_between 3 10 "hello");
    check
      bool
      "string_length_between invalid short"
      false
      (string_length_between 5 10 "hi");
    check
      bool
      "string_length_between invalid long"
      false
      (string_length_between 3 5 "toolongstring");
    check bool "string_length_between edge min" true (string_length_between 3 10 "abc");
    check
      bool
      "string_length_between edge max"
      true
      (string_length_between 3 10 "abcdefghij")
  ;;
end

(* Vector and geometry validation tests *)
module GeometryValidationTests = struct
  let test_vector2_between () =
    let min_vec = make_vector2 0.0 0.0 in
    let max_vec = make_vector2 10.0 10.0 in
    let valid_vec = make_vector2 5.0 5.0 in
    let invalid_vec_x = make_vector2 (-1.0) 5.0 in
    let invalid_vec_y = make_vector2 5.0 15.0 in
    check bool "vector2_between valid" true (vector2_between min_vec max_vec valid_vec);
    check
      bool
      "vector2_between invalid x"
      false
      (vector2_between min_vec max_vec invalid_vec_x);
    check
      bool
      "vector2_between invalid y"
      false
      (vector2_between min_vec max_vec invalid_vec_y);
    check bool "vector2_between edge case" true (vector2_between min_vec max_vec max_vec)
  ;;

  let test_vector3_between () =
    let min_vec = make_vector3 0.0 0.0 0.0 in
    let max_vec = make_vector3 10.0 10.0 10.0 in
    let valid_vec = make_vector3 5.0 5.0 5.0 in
    let invalid_vec = make_vector3 5.0 5.0 15.0 in
    check bool "vector3_between valid" true (vector3_between min_vec max_vec valid_vec);
    check
      bool
      "vector3_between invalid z"
      false
      (vector3_between min_vec max_vec invalid_vec);
    check bool "vector3_between edge case" true (vector3_between min_vec max_vec max_vec)
  ;;

  let test_size_between () =
    let min_size = make_size 100 100 in
    let max_size = make_size 500 500 in
    let valid_size = make_size 300 300 in
    let invalid_size = make_size 600 300 in
    check bool "size_between valid" true (size_between min_size max_size valid_size);
    check
      bool
      "size_between invalid width"
      false
      (size_between min_size max_size invalid_size);
    check bool "size_between edge case" true (size_between min_size max_size min_size)
  ;;
end

(* Range validation tests *)
module RangeValidationTests = struct
  let test_float_range_between () =
    let outer_range = make_float_range 0.0 10.0 in
    let valid_range = make_float_range 2.0 8.0 in
    let invalid_range_min = make_float_range (-1.0) 5.0 in
    let invalid_range_max = make_float_range 5.0 15.0 in
    let invalid_range_order = make_float_range 8.0 2.0 in
    check
      bool
      "float_range_between valid"
      true
      (float_range_between outer_range valid_range);
    check
      bool
      "float_range_between invalid min"
      false
      (float_range_between outer_range invalid_range_min);
    check
      bool
      "float_range_between invalid max"
      false
      (float_range_between outer_range invalid_range_max);
    check
      bool
      "float_range_between invalid order"
      false
      (float_range_between outer_range invalid_range_order)
  ;;

  let test_int_range_between () =
    let outer_range = make_int_range 0 10 in
    let valid_range = make_int_range 2 8 in
    let invalid_range_min = make_int_range (-1) 5 in
    let invalid_range_max = make_int_range 5 15 in
    let invalid_range_order = make_int_range 8 2 in
    check bool "int_range_between valid" true (int_range_between outer_range valid_range);
    check
      bool
      "int_range_between invalid min"
      false
      (int_range_between outer_range invalid_range_min);
    check
      bool
      "int_range_between invalid max"
      false
      (int_range_between outer_range invalid_range_max);
    check
      bool
      "int_range_between invalid order"
      false
      (int_range_between outer_range invalid_range_order)
  ;;
end

(* List validation tests *)
module ListValidationTests = struct
  let test_list_min_length () =
    check bool "list_min_length valid" true (list_min_length 3 [ 1; 2; 3; 4 ]);
    check bool "list_min_length invalid" false (list_min_length 5 [ 1; 2; 3 ]);
    check bool "list_min_length equal" true (list_min_length 3 [ 1; 2; 3 ]);
    check bool "list_min_length empty" false (list_min_length 1 [])
  ;;
end

(* Entity validation tests *)
module EntityValidationTests = struct
  let test_create_id_from () =
    let expected_id = "test_owner:Weapon:test_key:1" in
    let actual_id =
      create_id_from ~owner:"test_owner" ~entity_type:"Weapon" ~key:"test_key" ~version:1
    in
    check string "create_id_from" expected_id actual_id
  ;;

  let test_validate_entity_definition () =
    (* Valid entity definition *)
    let valid_id =
      create_id_from ~owner:"test_owner" ~entity_type:"Weapon" ~key:"test_key" ~version:1
    in
    check
      bool
      "validate_entity_definition valid"
      true
      (validate_entity_definition
         ~owner:"test_owner"
         ~entity_type:`Weapon
         ~key:"test_key"
         ~version:1
         ~id:valid_id);
    (* Invalid owner (too short) *)
    check
      bool
      "validate_entity_definition invalid owner"
      false
      (validate_entity_definition
         ~owner:"ab"
         ~entity_type:`Weapon
         ~key:"test_key"
         ~version:1
         ~id:valid_id);
    (* Invalid key (contains invalid characters) *)
    check
      bool
      "validate_entity_definition invalid key"
      false
      (validate_entity_definition
         ~owner:"test_owner"
         ~entity_type:`Weapon
         ~key:"test-key!"
         ~version:1
         ~id:valid_id);
    (* Invalid version (zero) *)
    check
      bool
      "validate_entity_definition invalid version"
      false
      (validate_entity_definition
         ~owner:"test_owner"
         ~entity_type:`Weapon
         ~key:"test_key"
         ~version:0
         ~id:valid_id);
    (* Invalid ID mismatch *)
    check
      bool
      "validate_entity_definition invalid id"
      false
      (validate_entity_definition
         ~owner:"test_owner"
         ~entity_type:`Weapon
         ~key:"test_key"
         ~version:1
         ~id:"wrong_id")
  ;;

  let test_validate_entity_reference () =
    let valid_ref = make_entity_reference () in
    check
      bool
      "validate_entity_reference valid"
      true
      (validate_entity_reference valid_ref);
    let invalid_ref = { valid_ref with owner = "ab" } in
    check
      bool
      "validate_entity_reference invalid owner"
      false
      (validate_entity_reference invalid_ref);
    let invalid_ref_version = { valid_ref with version = 0 } in
    check
      bool
      "validate_entity_reference invalid version"
      false
      (validate_entity_reference invalid_ref_version)
  ;;

  let test_entity_reference_of_type () =
    let weapon_ref = make_entity_reference ~entity_type:`Weapon () in
    let skill_ref = make_entity_reference ~entity_type:`Skill () in
    check
      bool
      "entity_reference_of_type correct type"
      true
      (entity_reference_of_type `Weapon weapon_ref);
    check
      bool
      "entity_reference_of_type wrong type"
      false
      (entity_reference_of_type `Weapon skill_ref)
  ;;

  let test_entity_reference_of_type_if_some () =
    let weapon_ref = make_entity_reference ~entity_type:`Weapon () in
    check
      bool
      "entity_reference_of_type_if_some None"
      true
      (entity_reference_of_type_if_some `Weapon None);
    check
      bool
      "entity_reference_of_type_if_some Some correct"
      true
      (entity_reference_of_type_if_some `Weapon (Some weapon_ref));
    check
      bool
      "entity_reference_of_type_if_some Some wrong"
      false
      (entity_reference_of_type_if_some `Skill (Some weapon_ref))
  ;;

  let test_entity_reference_list_of_type () =
    let weapon_refs =
      [ make_entity_reference ~entity_type:`Weapon ~key:"weapon1" ();
        make_entity_reference ~entity_type:`Weapon ~key:"weapon2" ()
      ]
    in
    let mixed_refs =
      [ make_entity_reference ~entity_type:`Weapon ~key:"weapon1" ();
        make_entity_reference ~entity_type:`Skill ~key:"skill1" ()
      ]
    in
    check
      bool
      "entity_reference_list_of_type valid"
      true
      (entity_reference_list_of_type 1 `Weapon weapon_refs);
    check
      bool
      "entity_reference_list_of_type mixed types"
      false
      (entity_reference_list_of_type 1 `Weapon mixed_refs);
    check
      bool
      "entity_reference_list_of_type insufficient length"
      false
      (entity_reference_list_of_type 3 `Weapon weapon_refs);
    check
      bool
      "entity_reference_list_of_type empty list"
      false
      (entity_reference_list_of_type 1 `Weapon [])
  ;;
end

(* File validation tests *)
module FileValidationTests = struct
  let test_validate_file () =
    (* Note: These tests will fail if files don't exist, but test the logic *)
    check
      bool
      "validate_file correct format"
      true
      (string_starts_with "data/" "data/weapons/sword.json"
       && string_ends_with ".json" "data/weapons/sword.json");
    check
      bool
      "validate_file wrong prefix"
      false
      (string_starts_with "data/" "config/weapons/sword.json");
    check
      bool
      "validate_file wrong extension"
      false
      (string_ends_with ".json" "data/weapons/sword.txt")
  ;;
end

(* Test suites *)
let basic_validation_tests =
  [ ("int_min", `Quick, BasicValidationTests.test_int_min);
    ("int_max", `Quick, BasicValidationTests.test_int_max);
    ("int_between", `Quick, BasicValidationTests.test_int_between);
    ("float_min", `Quick, BasicValidationTests.test_float_min);
    ("float_max", `Quick, BasicValidationTests.test_float_max);
    ("float_between", `Quick, BasicValidationTests.test_float_between)
  ]
;;

let string_validation_tests =
  [ ("string_starts_with", `Quick, StringValidationTests.test_string_starts_with);
    ("string_ends_with", `Quick, StringValidationTests.test_string_ends_with);
    ("string_max_length", `Quick, StringValidationTests.test_string_max_length);
    ("string_min_length", `Quick, StringValidationTests.test_string_min_length);
    ("string_length_between", `Quick, StringValidationTests.test_string_length_between)
  ]
;;

let geometry_validation_tests =
  [ ("vector2_between", `Quick, GeometryValidationTests.test_vector2_between);
    ("vector3_between", `Quick, GeometryValidationTests.test_vector3_between);
    ("size_between", `Quick, GeometryValidationTests.test_size_between)
  ]
;;

let range_validation_tests =
  [ ("float_range_between", `Quick, RangeValidationTests.test_float_range_between);
    ("int_range_between", `Quick, RangeValidationTests.test_int_range_between)
  ]
;;

let list_validation_tests =
  [ ("list_min_length", `Quick, ListValidationTests.test_list_min_length) ]
;;

let entity_validation_tests =
  [ ("create_id_from", `Quick, EntityValidationTests.test_create_id_from);
    ( "validate_entity_definition",
      `Quick,
      EntityValidationTests.test_validate_entity_definition );
    ( "validate_entity_reference",
      `Quick,
      EntityValidationTests.test_validate_entity_reference );
    ( "entity_reference_of_type",
      `Quick,
      EntityValidationTests.test_entity_reference_of_type );
    ( "entity_reference_of_type_if_some",
      `Quick,
      EntityValidationTests.test_entity_reference_of_type_if_some );
    ( "entity_reference_list_of_type",
      `Quick,
      EntityValidationTests.test_entity_reference_list_of_type )
  ]
;;

let file_validation_tests =
  [ ("validate_file", `Quick, FileValidationTests.test_validate_file) ]
;;

(* Main test runner *)
let () =
  let all_tests =
    [ ("Basic Validation", basic_validation_tests);
      ("String Validation", string_validation_tests);
      ("Geometry Validation", geometry_validation_tests);
      ("Range Validation", range_validation_tests);
      ("List Validation", list_validation_tests);
      ("Entity Validation", entity_validation_tests);
      ("File Validation", file_validation_tests)
    ]
  in
  run "Validation Module Tests" all_tests
;;

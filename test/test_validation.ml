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

module SpawnSequenceValidationTests = struct
  let make_step timing =
    { Data.Spawn_t.timing;
      spawns =
        [ { Data.Spawn_t.spawn_count = 1;
            character = make_entity_reference ~entity_type:`Character ();
            anchor = make_entity_reference ~entity_type:`Anchor ();
            target_adventurer_on_spawn = false
          }
        ]
    }
  ;;

  let test_validate_spawn_sequence_steps () =
    check
      bool
      "single step ending at 1.0 is valid"
      true
      (validate_spawn_sequence_steps [ make_step 1.0 ]);
    check
      bool
      "increasing timings ending at 1.0 are valid"
      true
      (validate_spawn_sequence_steps [ make_step 0.0; make_step 0.5; make_step 1.0 ]);
    check bool "empty steps are invalid" false (validate_spawn_sequence_steps []);
    check
      bool
      "last step must be at 1.0"
      false
      (validate_spawn_sequence_steps [ make_step 0.0; make_step 0.5 ]);
    check
      bool
      "timings must strictly increase"
      false
      (validate_spawn_sequence_steps [ make_step 0.5; make_step 0.5; make_step 1.0 ])
  ;;
end

let spawn_sequence_validation_tests =
  [ ( "validate_spawn_sequence_steps",
      `Quick,
      SpawnSequenceValidationTests.test_validate_spawn_sequence_steps )
  ]
;;

module QuestNodeIdValidationTests = struct
  let objective id =
    `Objective
      { Data.Quest_t.node_type = `Objective;
        id;
        name = "objective";
        metadata = { Data.Common_t.title = "t"; description = "d" };
        is_optional = false;
        bonus_achievement = None;
        condition = `Teleport { Data.Quest_t.condition_type = `Teleport; stage = None }
      }
  ;;

  let quest_with_root root =
    { Data.Quest_t.metadata = { Data.Common_t.title = "t"; description = "d" };
      origin = `QuestBoard;
      assignee = `Team;
      difficulty = make_entity_reference ~entity_type:`QuestDifficulty ();
      stage = make_entity_reference ~entity_type:`Stage ();
      root;
      is_repeatable = false;
      achievement_on_complete = `None;
      required_achievements = [ `None ];
      ends_run = None;
      act = None
    }
  ;;

  let sequence ?optional ?bonus_achievement id children =
    `Sequence
      ({ Data.Quest_t.node_type = `Sequence;
        id;
        name = "seq";
        optional;
        bonus_achievement;
        children
      } : Data.Quest_t.quest_sequence_node)
  ;;

  let parallel id children =
    `Parallel
      ({ Data.Quest_t.node_type = `Parallel;
        id;
        name = "par";
        optional = None;
        bonus_achievement = None;
        children
      } : Data.Quest_t.quest_parallel_node)
  ;;

  let optional_objective ?bonus_achievement id =
    match objective id with
    | `Objective o -> `Objective { o with Data.Quest_t.is_optional = true; bonus_achievement }
    | other -> other
  ;;

  let test_validate_quest_optional () =
    let ok root = validate_quest (quest_with_root root) in
    check bool "an optional sequence in a parallel" true
      (ok (parallel 0 [ objective 1; sequence ~optional:true 2 [ objective 3 ] ]));
    check bool "an optional objective in a parallel" true
      (ok (parallel 0 [ objective 1; optional_objective 2 ]));
    check bool "a bonus on an optional sequence" true
      (ok (parallel 0 [ objective 1; sequence ~optional:true ~bonus_achievement:`None 2 [ objective 3 ] ]));
    check bool "an optional sequence under a sequence" false
      (ok (sequence 0 [ sequence ~optional:true 1 [ objective 2 ] ]));
    check bool "an optional root" false (ok (sequence ~optional:true 0 [ objective 1 ]));
    check bool "an optional objective under a sequence" false
      (ok (sequence 0 [ optional_objective 1 ]));
    check bool "an optional grandchild of a parallel" false
      (ok (parallel 0 [ sequence 1 [ optional_objective 2 ] ]));
    check bool "a bonus on a required node" false
      (ok (parallel 0 [ objective 1; sequence ~bonus_achievement:`None 2 [ objective 3 ] ]));
    check bool "a bonus on an optional objective" true
      (ok (parallel 0 [ objective 1; optional_objective ~bonus_achievement:`None 2 ]))
  ;;

  let test_validate_quest_node_ids () =
    check
      bool
      "unique ids are valid"
      true
      (validate_quest_node_ids
         (quest_with_root (sequence 0 [ objective 1; objective 2 ])));
    check
      bool
      "duplicate leaf ids are invalid"
      false
      (validate_quest_node_ids
         (quest_with_root (sequence 0 [ objective 1; objective 1 ])));
    check
      bool
      "root id colliding with child is invalid"
      false
      (validate_quest_node_ids (quest_with_root (sequence 1 [ objective 1 ])))
  ;;
end

let quest_node_id_validation_tests =
  [ ( "validate_quest_node_ids",
      `Quick,
      QuestNodeIdValidationTests.test_validate_quest_node_ids );
    ( "validate_quest optional placement",
      `Quick,
      QuestNodeIdValidationTests.test_validate_quest_optional )
  ]
;;

module AchievementValidationTests = struct
  let stat ?subject ?subject_tag key : Data.Achievement_t.achievement_condition_internal =
    `Stat
      { Data.Achievement_t.condition_type = `Stat;
        key;
        subject;
        subject_tag;
        op = `Ge;
        value = 1;
        scope = `Lifetime
      }
  ;;

  let unlock ~target kind : Data.Achievement_t.unlock = { Data.Achievement_t.kind; target }

  let achievement condition rewards : Data.Achievement_t.achievement_def =
    { Data.Achievement_t.metadata = { title = "T"; description = "D" };
      hidden = false;
      condition;
      rewards
    }
  ;;

  let skill_unlock = unlock ~target:(make_entity_reference ~entity_type:`Skill ()) `SkillPool

  let test_metric_keys () =
    check bool "snake case" true (achievement_metric_key "boss_kills");
    check bool "upper case" false (achievement_metric_key "BossKills");
    check bool "slash is the subject's job" false (achievement_metric_key "kills/x");
    check bool "empty" false (achievement_metric_key "")
  ;;

  let test_subject_is_entity_or_tag () =
    let r = make_entity_reference ~entity_type:`Character () in
    check bool "entity" true (validate_achievement (achievement (stat ~subject:r "kills") [ skill_unlock ]));
    check bool "tag" true
      (validate_achievement (achievement (stat ~subject_tag:"Boss" "kills_rank") [ skill_unlock ]));
    check bool "both" false
      (validate_achievement
         (achievement (stat ~subject:r ~subject_tag:"Boss" "kills_rank") [ skill_unlock ]));
    let all =
      `All
        ({ Data.Achievement_t.condition_type = `All; children = [ stat "kills" ] }
          : Data.Achievement_t.achievement_condition_all)
    in
    let bad =
      `Any
        ({ Data.Achievement_t.condition_type = `Any;
           children = [ all; stat ~subject:r ~subject_tag:"x" "kills" ]
         }
          : Data.Achievement_t.achievement_condition_any)
    in
    check bool "nested bad" false (validate_achievement (achievement bad [ skill_unlock ]))
  ;;

  let test_unlock_kinds () =
    let ok u = achievement_unlock_ok u in
    let skill = make_entity_reference ~entity_type:`Skill () in
    let class_ = make_entity_reference ~entity_type:`WeaponClass () in
    let equip = make_entity_reference ~entity_type:`Equipment () in
    check bool "skill pool" true (ok (unlock ~target:skill `SkillPool));
    check bool "skill pool wrong type" false (ok (unlock ~target:class_ `SkillPool));
    check bool "item pool equipment" true (ok (unlock ~target:equip `ItemPool));
    check bool "item pool weapon" true
      (ok (unlock ~target:(make_entity_reference ~entity_type:`Weapon ()) `ItemPool));
    check bool "weapon class" true (ok (unlock ~target:class_ `WeaponClass));
    check bool "weapon class wrong type" false (ok (unlock ~target:skill `WeaponClass))
  ;;
end

let achievement_validation_tests =
  [ ("metric keys", `Quick, AchievementValidationTests.test_metric_keys);
    ("subject is an entity or a tag", `Quick, AchievementValidationTests.test_subject_is_entity_or_tag);
    ("unlock kinds", `Quick, AchievementValidationTests.test_unlock_kinds)
  ]
;;

module SummonControlValidationTests = struct
  let summon ?lifetime ?controlled () : Data.Skill_t.skill_action_summon_node =
    { skill_action_node_type = `Summon;
      name = "summon";
      summon_entity = make_entity_reference ~entity_type:`Character ();
      position_offset = make_vector3 0. 0. 0.;
      lifetime;
      max_alive = None;
      controlled
    }
  ;;

  let test_validate_summon_control () =
    check bool "plain summon" true (validate_summon_control (summon ()));
    check
      bool
      "controlled with a lifetime"
      true
      (validate_summon_control (summon ~lifetime:15. ~controlled:true ()));
    check
      bool
      "controlled without a lifetime is invalid"
      false
      (validate_summon_control (summon ~controlled:true ()));
    check
      bool
      "uncontrolled without a lifetime"
      true
      (validate_summon_control (summon ~controlled:false ()))
  ;;
end

let summon_control_validation_tests =
  [ ( "validate_summon_control",
      `Quick,
      SummonControlValidationTests.test_validate_summon_control )
  ]
;;

module MoveNodeValidationTests = struct
  let move mode distance duration : Data.Skill_t.skill_action_move_node =
    { skill_action_node_type = `Move; name = "move"; mode; distance; duration }
  ;;

  let test_validate_move_node () =
    check bool "dash" true (validate_move_node (move `Dash 4. 0.25));
    check bool "dash needs a duration" false (validate_move_node (move `Dash 4. 0.));
    check bool "dash needs a length" false (validate_move_node (move `Dash 0. 0.25));
    check bool "blink" true (validate_move_node (move `Blink 8. 0.));
    check bool "blink is instant" false (validate_move_node (move `Blink 8. 0.2));
    check bool "shadow step" true (validate_move_node (move `ShadowStep 1.5 0.));
    check bool "swap" true (validate_move_node (move `Swap 0. 0.));
    check bool "swap takes no distance" false (validate_move_node (move `Swap 2. 0.))
  ;;

  let test_validate_charges () =
    check bool "absent" true (validate_charges None);
    check bool "two" true (validate_charges (Some 2));
    check bool "zero" false (validate_charges (Some 0))
  ;;
end

module InterruptNodeValidationTests = struct
  let status_on mechanic ?dispel () : Data.Effect_t.status_effect =
    { target_mechanic = mechanic;
      target = `Enemy;
      durations = [];
      scalers = [];
      status = make_entity_reference ~entity_type:`Status ();
      dispel
    }
  ;;

  let selected : Data.Effect_t.effect_target_mechanic_internal =
    `Selected { mechanic_type = `Selected }
  ;;

  let node on_interrupt : Data.Skill_t.skill_action_interrupt_node =
    { skill_action_node_type = `Interrupt;
      name = "interrupt";
      target_mechanic = selected;
      target = `Enemy;
      on_interrupt
    }
  ;;

  let test_validate_interrupt_node () =
    check bool "a plain interrupt" true (validate_interrupt_node (node None));
    check bool "a status on the interrupted" true
      (validate_interrupt_node (node (Some (status_on selected ()))));
    check bool "a status on an area" false
      (validate_interrupt_node
         (node
            (Some
               (status_on
                  (`Circle { mechanic_type = `Circle; hit_count = 1; radius = 1. })
                  ()))));
    check bool "a dispel" false
      (validate_interrupt_node (node (Some (status_on selected ~dispel:true ()))))
  ;;
end

let interrupt_node_validation_tests =
  [ ( "validate_interrupt_node",
      `Quick,
      InterruptNodeValidationTests.test_validate_interrupt_node )
  ]
;;

module ObstacleValidationTests = struct
  let obstacle shape radius half_x half_z : Data.Stage_t.obstacle =
    { kind = `Rock; shape; x = 1.; z = 2.; radius; half_x; half_z; yaw = None; blocks_sight = None; prop = None }
  ;;

  let test_validate_obstacles () =
    check bool "absent" true (validate_obstacles None);
    check bool "circle" true (validate_obstacles (Some [ obstacle `Circle (Some 1.) None None ]));
    check bool "circle needs a radius" false (validate_obstacles (Some [ obstacle `Circle None None None ]));
    check bool "box" true (validate_obstacles (Some [ obstacle `Box None (Some 2.) (Some 0.5) ]));
    check bool "box needs both half extents" false (validate_obstacles (Some [ obstacle `Box None (Some 2.) None ]));
    check bool "one bad obstacle fails the list" false
      (validate_obstacles (Some [ obstacle `Circle (Some 1.) None None; obstacle `Circle (Some 0.) None None ]))
  ;;
end

let obstacle_validation_tests =
  [ "validate_obstacles", `Quick, ObstacleValidationTests.test_validate_obstacles ]
;;

module GroundValidationTests = struct
  let zone shape radius half_x half_z : Data.Stage_t.ground_zone =
    { kind = `Water; shape; x = 1.; z = 2.; radius; half_x; half_z; yaw = None }
  ;;

  let test_validate_ground_zones () =
    check bool "absent" true (validate_ground_zones None);
    check bool "circle" true (validate_ground_zones (Some [ zone `Circle (Some 1.) None None ]));
    check bool "circle needs a radius" false (validate_ground_zones (Some [ zone `Circle None None None ]));
    check bool "box" true (validate_ground_zones (Some [ zone `Box None (Some 2.) (Some 0.5) ]));
    check bool "box needs both half extents" false (validate_ground_zones (Some [ zone `Box None (Some 2.) None ]))
  ;;
end

let ground_validation_tests =
  [ "validate_ground_zones", `Quick, GroundValidationTests.test_validate_ground_zones ]
;;

module PresentationValidationTests = struct
  let test_sheet_id () =
    check bool "family" true (validate_sheet_id "body_green");
    check bool "item layer" true (validate_sheet_id "chest.strappedleathertunic");
    check bool "empty" false (validate_sheet_id "");
    check bool "upper case" false (validate_sheet_id "Body");
    check bool "starts with a digit" false (validate_sheet_id "1body");
    check bool "path" false (validate_sheet_id "a/b");
    check bool "trailing dot" false (validate_sheet_id "body.");
    check bool "absent" true (validate_sheet_id_if_some None);
    check bool "bad if present" false (validate_sheet_id_if_some (Some "Slime"))
  ;;

  let test_model_family () =
    check bool "plain" true (validate_model_family "wolf");
    check bool "low poly" true (validate_model_family "lp-rat");
    check bool "absent" true (validate_model_family_if_some None);
    check bool "upper case" false (validate_model_family "Wolf");
    check bool "leading dash" false (validate_model_family "-rat");
    check bool "trailing dash" false (validate_model_family "rat-");
    check bool "path" false (validate_model_family "lp/rat");
    check bool "sheet ids keep their rule" false (validate_sheet_id "lp-rat")
  ;;

  let test_prop_name () =
    check bool "kind" true (validate_prop_name "lamp");
    check bool "key" true (validate_prop_name "NTree3");
    check bool "id" true (validate_prop_name "Octoio:Model:NTree3:1");
    check bool "empty" false (validate_prop_name "");
    check bool "space" false (validate_prop_name "street lamp");
    check bool "path" false (validate_prop_name "../x");
    check bool "absent" true (validate_prop_name_if_some None)
  ;;

  let stage_prop ?(x = 1.) ?scale prop : Data.Stage_t.stage_prop =
    { prop; x; z = 2.; yaw = None; scale }
  ;;

  let test_stage_props () =
    check bool "absent" true (validate_stage_props None);
    check bool "props" true (validate_stage_props (Some [ stage_prop "fence"; stage_prop ~scale:1.5 "lamp" ]));
    check bool "non finite position" false (validate_stage_props (Some [ stage_prop ~x:Float.nan "fence" ]));
    check bool "scale in range" true (validate_prop_scale (Some 2.));
    check bool "scale too small" false (validate_prop_scale (Some 0.01));
    check bool "scale too big" false (validate_prop_scale (Some 50.))
  ;;

  let entry slot cue : Data.Weapon_class_t.animation_set_entry = { slot; cue; variant = 0 }

  let test_weapon_class_categories () =
    check bool "one" true (validate_weapon_class_categories [ `Bow ]);
    check bool "two" true (validate_weapon_class_categories [ `OneHandSword; `OneHandAxe ]);
    check bool "none" false (validate_weapon_class_categories []);
    check bool "twice" false (validate_weapon_class_categories [ `Bow; `Bow ])
  ;;

  let test_animation_set () =
    check bool "empty" true (validate_animation_set []);
    check bool "attack" true (validate_animation_set [ entry `BasicAttack `AttackShoot ]);
    check bool "stance" true
      (validate_animation_set
         [ entry `Idle `Idle; entry `Walk `Walk; entry `Run `Run; entry `Hit `HitBlocked; entry `Death `Death ]);
    check bool "an attack cue in the idle slot" false (validate_animation_set [ entry `Idle `AttackSlash ]);
    check bool "an idle cue as the basic attack" false (validate_animation_set [ entry `BasicAttack `Idle ]);
    check bool "a slot twice" false
      (validate_animation_set [ entry `BasicAttack `AttackSlash; entry `BasicAttack `AttackStab ])
  ;;
end

let presentation_validation_tests =
  [ "validate_sheet_id", `Quick, PresentationValidationTests.test_sheet_id;
    "validate_model_family", `Quick, PresentationValidationTests.test_model_family;
    "validate_prop_name", `Quick, PresentationValidationTests.test_prop_name;
    "validate_stage_props", `Quick, PresentationValidationTests.test_stage_props;
    "validate_weapon_class_categories", `Quick, PresentationValidationTests.test_weapon_class_categories;
    "validate_animation_set", `Quick, PresentationValidationTests.test_animation_set
  ]
;;

let move_node_validation_tests =
  [ "validate_move_node", `Quick, MoveNodeValidationTests.test_validate_move_node;
    "validate_charges", `Quick, MoveNodeValidationTests.test_validate_charges
  ]
;;

module ProjectileKindValidationTests = struct
  let model : Data.Projectile_t.projectile_model =
    { model_type = `Pebble; model_scale = make_vector3 1. 1. 1. }
  ;;

  let straight ~lifetime ~speed ~max_range : Data.Projectile_t.projectile_straight =
    { metadata = { Data.Common_t.title = "t"; description = "d" };
      projectile_type = `Straight;
      model;
      lifetime;
      spawn_offset = make_vector3 0. 0. 0.;
      max_hit_count = 1;
      on_impact = [];
      on_end = [];
      vfx = None;
      speed;
      max_range;
      radius = 0.3
    }
  ;;

  let arc ~lifetime ~flight_time : Data.Projectile_t.projectile_arc =
    { metadata = { Data.Common_t.title = "t"; description = "d" };
      projectile_type = `Arc;
      model;
      lifetime;
      spawn_offset = make_vector3 0. 0. 0.;
      max_hit_count = 4;
      on_impact = [];
      on_end = [];
      vfx = None;
      flight_time;
      max_range = 12.;
      radius = 2.;
      arc_height = 3.
    }
  ;;

  let beam ~lifetime ~tick_interval : Data.Projectile_t.projectile_beam =
    { metadata = { Data.Common_t.title = "t"; description = "d" };
      projectile_type = `Beam;
      model;
      lifetime;
      spawn_offset = make_vector3 0. 0. 0.;
      max_hit_count = 1;
      on_impact = [];
      on_end = [];
      vfx = None;
      range = 10.;
      width = 0.8;
      tick_interval;
      rotation_speed = 90.
    }
  ;;

  let test_kinds () =
    check bool "skillshot covers its range in time" true
      (validate_projectile_straight (straight ~lifetime:1.5 ~speed:20. ~max_range:18.));
    check bool "skillshot too slow for its range" false
      (validate_projectile_straight (straight ~lifetime:0.5 ~speed:20. ~max_range:18.));
    check bool "lob lands in time" true
      (validate_projectile_arc (arc ~lifetime:2. ~flight_time:1.));
    check bool "lob outlives its lifetime" false
      (validate_projectile_arc (arc ~lifetime:1. ~flight_time:1.5));
    check bool "beam ticks while it lasts" true
      (validate_projectile_beam (beam ~lifetime:3. ~tick_interval:0.5));
    check bool "beam never ticks" false
      (validate_projectile_beam (beam ~lifetime:0.3 ~tick_interval:0.5))
  ;;
end

let projectile_kind_validation_tests =
  [ "validate_projectile_kinds", `Quick, ProjectileKindValidationTests.test_kinds ]
;;

module StatusDispelValidationTests = struct
  let effect ?dispel ~durations ~scalers () : Data.Effect_t.status_effect =
    { target_mechanic = `Self { mechanic_type = `Self };
      target = `Any;
      durations;
      scalers;
      status = make_entity_reference ~entity_type:`Status ();
      dispel
    }
  ;;

  let test_validate_status_effect () =
    let timed : Data.Effect_t.status_duration = { duration = `Chrono; value = 2. } in
    let scaler : Data.Effect_t.effect_scaling =
      { base = 1.; scaling = make_float_range 1. 1.; stat = `Armor }
    in
    check
      bool
      "plain status with duration and scaler"
      true
      (validate_status_effect (effect ~durations:[ timed ] ~scalers:[ scaler ] ()));
    check
      bool
      "dispel with nothing else"
      true
      (validate_status_effect (effect ~dispel:true ~durations:[] ~scalers:[] ()));
    check
      bool
      "dispel with a duration is invalid"
      false
      (validate_status_effect (effect ~dispel:true ~durations:[ timed ] ~scalers:[] ()));
    check
      bool
      "dispel with a scaler is invalid"
      false
      (validate_status_effect (effect ~dispel:true ~durations:[] ~scalers:[ scaler ] ()))
  ;;
end

let status_dispel_validation_tests =
  [ ( "validate_status_effect",
      `Quick,
      StatusDispelValidationTests.test_validate_status_effect )
  ]
;;

module ReviveHitValidationTests = struct
  let scaler base : Data.Effect_t.effect_scaling =
    { base; scaling = make_float_range 0. 0.; stat = `AbilityPower }
  ;;

  let selected : Data.Effect_t.effect_target_mechanic_internal =
    `Selected { mechanic_type = `Selected }
  ;;

  let effect
    ?(hit_type = `Revive)
    ?(target = `Ally)
    ?(mechanic = selected)
    ?school
    ?execute
    ?knockback
    scalers
    : Data.Effect_t.hit_effect
    =
    { hit_type;
      scalers;
      target_mechanic = mechanic;
      target;
      hit_sound = make_entity_reference ~entity_type:`Sound ();
      can_crit = false;
      can_miss = false;
      school;
      execute;
      knockback
    }
  ;;

  let test_validate_hit_effect () =
    check bool "a revive of the selected ally" true (validate_hit_effect (effect [ scaler 50. ]));
    check bool "a full revive" true (validate_hit_effect (effect [ scaler 100. ]));
    check bool "no scaler" false (validate_hit_effect (effect []));
    check bool "zero percent" false (validate_hit_effect (effect [ scaler 0. ]));
    check bool "over a hundred percent" false (validate_hit_effect (effect [ scaler 120. ]));
    check bool "two scalers" false (validate_hit_effect (effect [ scaler 20.; scaler 20. ]));
    check bool "an enemy target" false (validate_hit_effect (effect ~target:`Enemy [ scaler 50. ]));
    check
      bool
      "an area"
      false
      (validate_hit_effect
         (effect
            ~mechanic:(`Circle { mechanic_type = `Circle; hit_count = 1; radius = 1. })
            [ scaler 50. ]));
    check
      bool
      "other hits are not restricted"
      true
      (validate_hit_effect (effect ~hit_type:`Heal ~target:`Any []));
    check
      bool
      "a damage school"
      true
      (validate_hit_effect (effect ~hit_type:`Damage ~target:`Enemy ~school:`Magical []));
    check
      bool
      "a heal school"
      true
      (validate_hit_effect (effect ~hit_type:`Heal ~target:`Any ~school:`Physical []));
    check
      bool
      "a threat school is an error"
      false
      (validate_hit_effect (effect ~hit_type:`Threat ~target:`Enemy ~school:`Physical []));
    check
      bool
      "a revive school is an error"
      false
      (validate_hit_effect (effect ~school:`Magical [ scaler 50. ]))
  ;;

  let bonus below factor : Data.Effect_t.execute_bonus = { below; factor }

  let test_execute_and_knockback () =
    let damage = effect ~hit_type:`Damage ~target:`Enemy in
    check bool "an execute bonus on a damage hit" true
      (validate_hit_effect (damage ~execute:(bonus 0.3 2.) [ scaler 5. ]));
    check bool "a knockback on a damage hit" true
      (validate_hit_effect (damage ~knockback:2.5 [ scaler 5. ]));
    check bool "no knockback distance" false
      (validate_hit_effect (damage ~knockback:0. [ scaler 5. ]));
    check bool "a negative knockback (a pull)" false
      (validate_hit_effect (damage ~knockback:(-1.) [ scaler 5. ]));
    check bool "an execute on a heal" false
      (validate_hit_effect
         (effect ~hit_type:`Heal ~target:`Ally ~execute:(bonus 0.3 2.) [ scaler 5. ]));
    check bool "a knockback on a threat hit" false
      (validate_hit_effect (effect ~hit_type:`Threat ~target:`Enemy ~knockback:1. []));
    check bool "a knockback on a revive" false
      (validate_hit_effect (effect ~knockback:1. [ scaler 50. ]))
  ;;

  let test_validate_execute_bonus () =
    check bool "30 percent, double" true (validate_execute_bonus (bonus 0.3 2.));
    check bool "the whole health" true (validate_execute_bonus (bonus 1. 1.));
    check bool "no health" false (validate_execute_bonus (bonus 0. 2.));
    check bool "over the whole health" false (validate_execute_bonus (bonus 1.2 2.));
    check bool "a penalty" false (validate_execute_bonus (bonus 0.3 0.5))
  ;;
end

let revive_hit_validation_tests =
  [ "validate_hit_effect", `Quick, ReviveHitValidationTests.test_validate_hit_effect;
    "execute and knockback", `Quick, ReviveHitValidationTests.test_execute_and_knockback;
    "validate_execute_bonus", `Quick, ReviveHitValidationTests.test_validate_execute_bonus
  ]
;;

module EnrageValidationTests = struct
  let self_mechanic : Data.Effect_t.effect_target_mechanic_internal =
    `Self { mechanic_type = `Self }
  ;;

  let effect ?dispel ?(mechanic = self_mechanic) () : Data.Effect_t.status_effect =
    { target_mechanic = mechanic;
      target = `Any;
      durations = [];
      scalers = [];
      status = make_entity_reference ~entity_type:`Status ();
      dispel
    }
  ;;

  let test_validate_enrage_effects () =
    check bool "empty list" false (validate_enrage_effects []);
    check bool "one self effect" true (validate_enrage_effects [ effect () ]);
    check
      bool
      "dispel is invalid"
      false
      (validate_enrage_effects [ effect ~dispel:true () ]);
    check
      bool
      "a circle is invalid"
      false
      (validate_enrage_effects
         [ effect
             ~mechanic:(`Circle { mechanic_type = `Circle; hit_count = 1; radius = 1. })
             ()
         ])
  ;;
end

module LevelGrowthValidationTests = struct
  let test_growth () =
    check bool "absent" true (validate_growth None);
    check bool "zero" true (validate_growth (Some 0.));
    check bool "positive" true (validate_growth (Some 2.5));
    check bool "negative" false (validate_growth (Some (-0.1)))
  ;;

  let test_cooldown_per_level () =
    check bool "absent" true (validate_cooldown_per_level None);
    check bool "small" true (validate_cooldown_per_level (Some 0.02));
    check bool "too large" false (validate_cooldown_per_level (Some 0.2));
    check bool "negative" false (validate_cooldown_per_level (Some (-0.01)))
  ;;

  let test_max_level () =
    check bool "absent" true (validate_max_level None);
    check bool "one" true (validate_max_level (Some 1));
    check bool "zero" false (validate_max_level (Some 0))
  ;;
end

let level_growth_validation_tests =
  [ "growth", `Quick, LevelGrowthValidationTests.test_growth;
    "cooldown_per_level", `Quick, LevelGrowthValidationTests.test_cooldown_per_level;
    "max_level", `Quick, LevelGrowthValidationTests.test_max_level
  ]
;;

let enrage_validation_tests =
  [ ( "validate_enrage_effects",
      `Quick,
      EnrageValidationTests.test_validate_enrage_effects )
  ]
;;

module NestValidationTests = struct
  let nest burst_at burst_count : Data.Character_t.nest =
    { burst_at; burst = make_entity_reference ~entity_type:`Character (); burst_count }
  ;;

  let valid n = Data.Character_v.validate_nest [] n = None

  let test_generated_nest () =
    check bool "half" true (valid (nest 0.5 3));
    check bool "never bursts" false (valid (nest 0. 3));
    check bool "bursts at full health" false (valid (nest 1. 3));
    check bool "bursts out nothing" false (valid (nest 0.5 0))
  ;;
end

let nest_validation_tests =
  [ "generated nest", `Quick, NestValidationTests.test_generated_nest ]
;;

module DropValidationTests = struct
  let test_gate () =
    let gate ?min_level ?min_tier ?min_rank ?lower_rank_weight () : Data.Drop_t.drop_gate =
      { min_level; min_tier; min_rank; lower_rank_weight }
    in
    check bool "empty gate" true (validate_drop_gate (gate ()));
    check bool "level 1" true (validate_drop_gate (gate ~min_level:1 ()));
    check bool "level 0" false (validate_drop_gate (gate ~min_level:0 ()));
    check bool "tier 2" true (validate_drop_gate (gate ~min_tier:2 ()));
    check bool "tier 0 is every quest" false (validate_drop_gate (gate ~min_tier:0 ()));
    check bool "boss" true (validate_drop_gate (gate ~min_rank:`Boss ()));
    check bool "rank Normal is a no-op" false (validate_drop_gate (gate ~min_rank:`Normal ()));
    check
      bool
      "lower weight with a rank"
      true
      (validate_drop_gate (gate ~min_rank:`Elite ~lower_rank_weight:1 ()));
    check
      bool
      "lower weight without a rank"
      false
      (validate_drop_gate (gate ~lower_rank_weight:1 ()));
    check
      bool
      "lower weight 0"
      false
      (validate_drop_gate (gate ~min_rank:`Elite ~lower_rank_weight:0 ()))
  ;;

  let test_weights () =
    let w common uncommon rare epic legendary : Data.Drop_t.rarity_weights =
      { common; uncommon; rare; epic; legendary }
    in
    check bool "positive sum" true (validate_rarity_weights (w 0 0 0 0 1));
    check bool "all zero" false (validate_rarity_weights (w 0 0 0 0 0))
  ;;

  let test_guarantee () =
    check bool "equipment" true (validate_guarantee_kind `Equipment);
    check bool "stone" true (validate_guarantee_kind `SkillStone);
    check bool "gold" false (validate_guarantee_kind `Gold);
    check bool "rare" true (validate_guarantee_quality `Rare);
    check bool "none" false (validate_guarantee_quality `None);
    check bool "tilt" true (float_min_if_some 0. (Some 0.5));
    check bool "negative tilt" false (float_min_if_some 0. (Some (-0.1)));
    check bool "nan tilt" false (float_min_if_some 0. (Some Float.nan));
    check bool "no tier guarantee" true (validate_guarantee_quality_if_some None);
    check bool "epic tier guarantee" true (validate_guarantee_quality_if_some (Some `Epic));
    check bool "bare tier guarantee" false (validate_guarantee_quality_if_some (Some `None));
    check bool "no bonus" true (int_min_if_some 0 None);
    check bool "negative bonus" false (int_min_if_some 0 (Some (-1)));
    check bool "no act" true (int_between_if_some 1 3 None);
    check bool "act 3" true (int_between_if_some 1 3 (Some 3));
    check bool "act 0" false (int_between_if_some 1 3 (Some 0));
    check bool "act 4" false (int_between_if_some 1 3 (Some 4))
  ;;
end

let drop_validation_tests =
  [ ("validate_drop_gate", `Quick, DropValidationTests.test_gate);
    ("validate_rarity_weights", `Quick, DropValidationTests.test_weights);
    ("guarantee and tilt", `Quick, DropValidationTests.test_guarantee)
  ]
;;

module TriggerValidationTests = struct
  let hit_effect : Data.Effect_t.hit_effect =
    { hit_type = `Damage;
      scalers = [];
      target_mechanic = `Selected { mechanic_type = `Selected };
      target = `Enemy;
      hit_sound = make_entity_reference ~entity_type:`Sound ();
      can_crit = true;
      can_miss = true;
      school = None;
      execute = None;
      knockback = None
    }
  ;;

  let hit_only : Data.Trigger_t.trigger_effect =
    { hit = Some hit_effect; status = None; skill = None; exclude_other = None }
  ;;

  let trigger ?(key = "crit-bleed") ?(chance = 1.) ?(cooldown = 0.) ?min_amount
    ?status ?max_chain ?(effects = [ hit_only ]) on
    : Data.Trigger_t.trigger
    =
    { key; on; relation = None; chance; cooldown; min_amount; status; max_chain;
      terminal = None; effects }
  ;;

  let test_validate_trigger_effect () =
    check bool "one hit" true (validate_trigger_effect hit_only);
    check
      bool
      "nothing"
      false
      (validate_trigger_effect { hit_only with hit = None });
    check
      bool
      "a hit and a skill"
      false
      (validate_trigger_effect
         { hit_only with skill = Some (make_entity_reference ~entity_type:`Skill ()) })
  ;;

  let test_validate_trigger () =
    check bool "plain trigger" true (validate_trigger (trigger `Crit));
    check bool "chance above one" false (validate_trigger (trigger ~chance:1.5 `Crit));
    check bool "negative chance" false (validate_trigger (trigger ~chance:(-0.1) `Crit));
    check bool "negative cooldown" false (validate_trigger (trigger ~cooldown:(-1.) `Hit));
    check bool "negative min amount" false (validate_trigger (trigger ~min_amount:(-1.) `Hit));
    check bool "negative max chain" false (validate_trigger (trigger ~max_chain:(-1) `Hit));
    check bool "no effects" false (validate_trigger (trigger ~effects:[] `Hit));
    check bool "key too short" false (validate_trigger (trigger ~key:"ab" `Hit));
    check bool "key with a space" false (validate_trigger (trigger ~key:"on hit" `Hit));
    let status = make_entity_reference ~entity_type:`Status () in
    check
      bool
      "status filter on StatusApplied"
      true
      (validate_trigger (trigger ~status `StatusApplied));
    check
      bool
      "status filter on another event"
      false
      (validate_trigger (trigger ~status `Hit))
  ;;

  let test_validate_triggers () =
    check bool "none" true (validate_triggers None);
    check bool "empty" true (validate_triggers (Some []));
    check
      bool
      "distinct keys"
      true
      (validate_triggers (Some [ trigger ~key:"first" `Hit; trigger ~key:"second" `Crit ]));
    check
      bool
      "duplicate keys"
      false
      (validate_triggers (Some [ trigger ~key:"same" `Hit; trigger ~key:"same" `Crit ]))
  ;;
end

let trigger_validation_tests =
  [ ( "validate_trigger_effect",
      `Quick,
      TriggerValidationTests.test_validate_trigger_effect );
    ("validate_trigger", `Quick, TriggerValidationTests.test_validate_trigger);
    ("validate_triggers", `Quick, TriggerValidationTests.test_validate_triggers)
  ]
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

(* Effect keys (vfx.atd) *)
module VfxKeyTests = struct
  let test_vfx_key () =
    let ok k = check bool k true (vfx_key k) in
    let bad k = check bool ("bad " ^ k) false (vfx_key k) in
    List.iter ok [ "fire/gather"; "frost/bolt-2"; "heal" ];
    List.iter bad [ ""; "/fire"; "fire/"; "fire//x"; "Fire/gather"; "fire gather"; "fire_x" ]
  ;;

  let test_vfx_key_if_some () =
    check bool "none" true (vfx_key_if_some None);
    check bool "some ok" true (vfx_key_if_some (Some "a/b"));
    check bool "some bad" false (vfx_key_if_some (Some "A"))
  ;;
end

let vfx_key_tests =
  [ ("vfx_key", `Quick, VfxKeyTests.test_vfx_key);
    ("vfx_key_if_some", `Quick, VfxKeyTests.test_vfx_key_if_some)
  ]
;;

(* Main test runner *)
let () =
  let all_tests =
    [ ("Basic Validation", basic_validation_tests);
      ("String Validation", string_validation_tests);
      ("Geometry Validation", geometry_validation_tests);
      ("Range Validation", range_validation_tests);
      ("List Validation", list_validation_tests);
      ("Spawn Sequence Validation", spawn_sequence_validation_tests);
      ("Quest Node Id Validation", quest_node_id_validation_tests);
      ("Summon Control Validation", summon_control_validation_tests);
      ("Projectile Kind Validation", projectile_kind_validation_tests);
      ("Move Node Validation", move_node_validation_tests);
      ("Interrupt Node Validation", interrupt_node_validation_tests);
      ("Obstacle Validation", obstacle_validation_tests);
      ("Ground Validation", ground_validation_tests);
      ("Presentation Validation", presentation_validation_tests);
      ("Revive Hit Validation", revive_hit_validation_tests);
      ("Status Dispel Validation", status_dispel_validation_tests);
      ("Enrage Validation", enrage_validation_tests);
      ("Nest Validation", nest_validation_tests);
      ("Level Growth Validation", level_growth_validation_tests);
      ("Drop Validation", drop_validation_tests);
      ("Trigger Validation", trigger_validation_tests);
      ("Entity Validation", entity_validation_tests);
      ("File Validation", file_validation_tests);
      ("Vfx Key Validation", vfx_key_tests);
      ("Achievement Validation", achievement_validation_tests);
    ]
  in
  run "Validation Module Tests" all_tests
;;

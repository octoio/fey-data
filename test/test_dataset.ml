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

let test_extract_entity_reference_from_quest () =
  let quest_refs =
    Dataset.extract_entity_reference_from_entity_definition
      quest_with_spawns_entity_definition
  in
  (* difficulty + stage + spawn action (character + anchor) + spawn sequence (character + anchor) *)
  check int "Quest has expected reference count" 6 (List.length quest_refs);
  let count entity_type =
    List.length
      (List.filter (fun ref -> ref.Data.Common_t.entity_type = entity_type) quest_refs)
  in
  check int "Quest contains difficulty entity reference" 1 (count `QuestDifficulty);
  check int "Quest contains stage entity reference" 1 (count `Stage);
  check int "Quest contains spawn character references" 2 (count `Character);
  check int "Quest contains spawn anchor references" 2 (count `Anchor)
;;

let test_extract_entity_reference_from_stage () =
  let stage_refs =
    Dataset.extract_entity_reference_from_entity_definition minimal_stage_entity_definition
  in
  check int "Stage has expected reference count" 1 (List.length stage_refs);
  check
    bool
    "Stage contains anchor entity reference"
    true
    (List.exists (fun ref -> ref.Data.Common_t.entity_type = `Anchor) stage_refs)
;;

let test_kill_condition_character_is_a_reference () =
  let reference =
    { minimal_entity_reference with Data.Common_t.entity_type = `Character }
  in
  let condition character =
    `KillSpecific
      { Data.Quest_t.condition_type = `KillSpecific;
        character_types = [ `Slime ];
        character;
        amount = 1
      }
  in
  check
    int
    "A kill condition without a character names no entity"
    0
    (List.length (Dataset.extract_entity_reference_from_quest_condition (condition None)));
  check
    int
    "A kill condition with a character names it"
    1
    (List.length
       (Dataset.extract_entity_reference_from_quest_condition (condition (Some reference))))
;;

let test_teleport_pick_and_interact_conditions_name_their_references () =
  let reference entity_type = { minimal_entity_reference with Data.Common_t.entity_type } in
  let refs c = List.length (Dataset.extract_entity_reference_from_quest_condition c) in
  let teleport stage = `Teleport { Data.Quest_t.condition_type = `Teleport; stage } in
  let pick quest = `PickQuest { Data.Quest_t.condition_type = `PickQuest; quest } in
  check int "teleport anywhere names nothing" 0 (refs (teleport None));
  check int "teleport to a stage names it" 1 (refs (teleport (Some (reference `Stage))));
  check int "pick any names nothing" 0 (refs (pick None));
  check int "pick one quest names it" 1 (refs (pick (Some (reference `Quest))));
  check
    int
    "interact names its anchor"
    1
    (refs
       (`Interact
           { Data.Quest_t.condition_type = `Interact; anchor = reference `Anchor }))
;;

let test_character_starting_kit_is_a_reference () =
  let reference entity_type = { minimal_entity_reference with Data.Common_t.entity_type } in
  let kit : Data.Entity_t.entity_definition_internal =
    `Character
      { Data.Entity_t.owner = "ownr";
        entity_type = `Character;
        key = "Kit";
        version = 1;
        id = "ownr:Character:Kit:1";
        entity =
          { minimal_character with
            Data.Character_t.starting_equipment = Some [ reference `Equipment ];
            starting_weapons = Some [ reference `Weapon; reference `Weapon ]
          }
      }
  in
  let bare = minimal_character_entity_definition in
  let n d = List.length (Dataset.extract_entity_reference_from_entity_definition d) in
  check int "a starting kit adds its items to the references" (n bare + 3) (n kit)
;;

let test_timer_restart_and_new_fields_round_trip_through_json () =
  let json =
    {|{"type":"Timer","id":3,"name":"t","duration":20.0,"on_timeout":"Restart","child":{"type":"Objective","id":4,"name":"o","metadata":{"title":"t","description":"d"},"is_optional":false,"condition":{"type":"Interact","anchor":{"owner":"ownr","type":"Anchor","key":"A","version":1,"id":"ownr:Anchor:A:1"}}}}|}
  in
  let node = Data.Quest_j.quest_node_internal_of_string json in
  (match node with
   | `Timer { on_timeout = `Restart; child = `Objective { condition = `Interact _; _ }; _ }
     -> ()
   | _ -> Alcotest.fail "expected a Restart timer over an Interact objective");
  check
    bool
    "serializing and parsing again gives the same node"
    true
    (Data.Quest_j.quest_node_internal_of_string
       (Data.Quest_j.string_of_quest_node_internal node)
     = node)
;;

let test_status_full_stack_rule_round_trips_and_defaults_to_absent () =
  let stack rule =
    Printf.sprintf {|{"size":3,"scaling_strategy":"Additive"%s}|} rule
  in
  let absent = Data.Status_j.status_stack_of_string (stack "") in
  check bool "absent rule stays None" true (absent.full_stack = None);
  let weakest =
    Data.Status_j.status_stack_of_string (stack {|,"full_stack":"ReplaceWeakest"|})
  in
  check bool "rule parsed" true (weakest.full_stack = Some `ReplaceWeakest);
  check
    bool
    "rule survives a round trip"
    true
    (Data.Status_j.status_stack_of_string (Data.Status_j.string_of_status_stack weakest)
     = weakest)
;;

(* Anchor ownership: exactly one stage must own each anchor *)
let ownership_errors dataset =
  let validated = Validate.validate_entity_definitions dataset in
  List.filter
    (fun ({ error; _ } : Dataset.dataset_error) ->
      match error with
      | Some e ->
        Base.String.is_substring
          (Atdgen_runtime.Util.Validation.string_of_error e)
          ~substring:"owned by exactly one stage"
      | None -> false)
    validated.errors
;;

let test_anchor_owned_by_one_stage_is_valid () =
  let dataset =
    TestFixtures.create_dataset_with_definitions
      [ ("data/json/anchor_file.json", minimal_anchor_entity_definition);
        ("data/json/stage_file.json", minimal_stage_entity_definition)
      ]
  in
  check int "No ownership errors" 0 (List.length (ownership_errors dataset))
;;

let test_orphan_anchor_is_invalid () =
  let dataset =
    TestFixtures.create_dataset_with_definitions
      [ ("data/json/anchor_file.json", minimal_anchor_entity_definition) ]
  in
  check int "Orphan anchor produces an error" 1 (List.length (ownership_errors dataset))
;;

let test_anchor_owned_twice_is_invalid () =
  let second_stage : Data.Entity_t.entity_definition_internal =
    `Stage
      { Data.Entity_t.owner = "ownr";
        entity_type = `Stage;
        key = "SecondStage";
        version = 1;
        id = "ownr:Stage:SecondStage:1";
        entity = minimal_stage
      }
  in
  let dataset =
    TestFixtures.create_dataset_with_definitions
      [ ("data/json/anchor_file.json", minimal_anchor_entity_definition);
        ("data/json/stage_file.json", minimal_stage_entity_definition);
        ("data/json/second_stage_file.json", second_stage)
      ]
  in
  check int "Doubly owned anchor produces an error" 1 (List.length (ownership_errors dataset))
;;


(* Drop tables: rarity curve and guarantees are checked against the item definitions *)
let drop_errors dataset =
  let validated = Validate.validate_entity_definitions dataset in
  List.filter
    (fun ({ error; _ } : Dataset.dataset_error) ->
      match error with
      | Some e ->
        Base.String.is_substring
          (Atdgen_runtime.Util.Validation.string_of_error e)
          ~substring:"Drop table"
      | None -> false)
    validated.errors
;;

let equipment_ref =
  { Data.Common_t.owner = "ownr";
    entity_type = `Equipment;
    key = "MinimalEquipment";
    version = 1;
    id = "ownr:Equipment:MinimalEquipment:1"
  }
;;

let equipment_entry ?gate () : Data.Drop_t.drop_internal =
  `Equipment
    { Data.Drop_t.drop_type = `Equipment; weight = 1; equipment = equipment_ref; gate }
;;

let curve : Data.Drop_t.rarity_curve =
  { weights = { common = 10; uncommon = 5; rare = 2; epic = 1; legendary = 0 };
    per_level = None;
    elite_tilt = None;
    boss_tilt = None
  }
;;

let drop_table_dataset ?(equipment = minimal_equipment) ?curve:(rarity_curve = None) ?guarantees entries =
  let table =
    { minimal_drop_table with
      equipment_drops = entries;
      rarity_curve;
      guarantees
    }
  in
  let table_def : Data.Entity_t.entity_definition_internal =
    `DropTable
      { Data.Entity_t.owner = "ownr";
        entity_type = `DropTable;
        key = "T";
        version = 1;
        id = "ownr:DropTable:T:1";
        entity = table
      }
  in
  let equipment_def : Data.Entity_t.entity_definition_internal =
    `Equipment
      { Data.Entity_t.owner = "ownr";
        entity_type = `Equipment;
        key = "MinimalEquipment";
        version = 1;
        id = "ownr:Equipment:MinimalEquipment:1";
        entity = equipment
      }
  in
  TestFixtures.create_dataset_with_definitions
    [ ("data/json/t.json", table_def); ("data/json/e.json", equipment_def) ]
;;

let guarantee ?(min_quality = `Rare) () : Data.Drop_t.drop_guarantee =
  { min_rank = `Boss; kind = `Equipment; min_quality }
;;

let test_drop_table_without_curve_is_valid () =
  check
    int
    "no drop errors"
    0
    (List.length (drop_errors (drop_table_dataset [ equipment_entry () ])))
;;

let test_drop_curve_rejects_bare_items () =
  let bare = { minimal_equipment with quality = `None } in
  check
    int
    "a bare item has no rarity"
    1
    (List.length
       (drop_errors
          (drop_table_dataset ~equipment:bare ~curve:(Some curve) [ equipment_entry () ])));
  check
    int
    "without a curve it is fine"
    0
    (List.length (drop_errors (drop_table_dataset ~equipment:bare [ equipment_entry () ])))
;;

let test_drop_guarantee_needs_a_matching_entry () =
  let rare = { minimal_equipment with quality = `Rare } in
  check
    int
    "common item cannot meet a Rare guarantee"
    1
    (List.length
       (drop_errors
          (drop_table_dataset ~guarantees:[ guarantee () ] [ equipment_entry () ])));
  check
    int
    "rare item meets it"
    0
    (List.length
       (drop_errors
          (drop_table_dataset
             ~equipment:rare
             ~guarantees:[ guarantee () ]
             [ equipment_entry () ])));
  let elite_gate : Data.Drop_t.drop_gate =
    { min_level = None; min_tier = None; min_rank = Some `Boss; lower_rank_weight = None }
  in
  let elite_guarantee = { (guarantee ()) with min_rank = `Elite } in
  check
    int
    "a boss-only entry cannot meet an Elite guarantee"
    1
    (List.length
       (drop_errors
          (drop_table_dataset
             ~equipment:rare
             ~guarantees:[ elite_guarantee ]
             [ equipment_entry ~gate:elite_gate () ])));
  check
    int
    "a boss-only entry meets a Boss guarantee"
    0
    (List.length
       (drop_errors
          (drop_table_dataset
             ~equipment:rare
             ~guarantees:[ guarantee () ]
             [ equipment_entry ~gate:elite_gate () ])))
;;

(* Quest tiers: unique orders, an identity base tier, gates on tiers that exist *)
let tier_errors definitions =
  let dataset =
    TestFixtures.create_dataset_with_definitions
      (List.mapi (fun i d -> Printf.sprintf "data/json/tier%d.json" i, d) definitions)
  in
  let validated = Validate.validate_entity_definitions dataset in
  List.filter
    (fun ({ error; _ } : Dataset.dataset_error) ->
      match error with
      | Some e ->
        Base.String.is_substring
          (Atdgen_runtime.Util.Validation.string_of_error e)
          ~substring:"Quest tier"
      | None -> false)
    validated.errors
;;

let tier_definition key difficulty_type tier : Data.Entity_t.entity_definition_internal =
  `QuestDifficulty
    { Data.Entity_t.owner = "ownr";
      entity_type = `QuestDifficulty;
      key;
      version = 1;
      id = "ownr:QuestDifficulty:" ^ key ^ ":1";
      entity =
        { minimal_quest_difficulty with Data.Quest_t.difficulty_type; tier = Some tier }
    }
;;

let plain_tier order : Data.Quest_t.quest_tier =
  { order;
    level_bonus = None;
    extra_adds = None;
    affixes = None;
    rarity_tilt = None;
    guarantee_min_quality = None;
    gold_multiplier = None
  }
;;

let test_quest_tiers () =
  let normal = tier_definition "Normal" `Normal (plain_tier 0) in
  let hard tier = tier_definition "Hard" `Hard tier in
  check
    int
    "an identity base tier and a harder one are valid"
    0
    (List.length
       (tier_errors [ normal; hard { (plain_tier 1) with level_bonus = Some 2 } ]));
  check
    int
    "two tiers with one order are both reported"
    2
    (List.length (tier_errors [ normal; hard (plain_tier 0) ]));
  check
    int
    "the base tier must change nothing"
    1
    (List.length
       (tier_errors
          [ tier_definition "Normal" `Normal { (plain_tier 0) with rarity_tilt = Some 0.5 };
            hard (plain_tier 1)
          ]));
  let gated : Data.Drop_t.drop_gate =
    { min_level = None; min_tier = Some 2; min_rank = Some `Boss; lower_rank_weight = None }
  in
  let table : Data.Entity_t.entity_definition_internal =
    `DropTable
      { Data.Entity_t.owner = "ownr";
        entity_type = `DropTable;
        key = "T";
        version = 1;
        id = "ownr:DropTable:T:1";
        entity = { minimal_drop_table with equipment_drops = [ equipment_entry ~gate:gated () ] }
      }
  in
  check
    int
    "a gate on a tier above the hardest is reported"
    1
    (List.length (tier_errors [ normal; hard (plain_tier 1); table ]));
  check
    int
    "a gate on an existing tier is fine"
    0
    (List.length (tier_errors [ normal; hard (plain_tier 1); tier_definition "Insane" `Insane (plain_tier 2); table ]))
;;

(* Quest completability: KillSpecific objectives need preceding spawns *)
let completability_errors dataset =
  let validated = Validate.validate_entity_definitions dataset in
  List.filter
    (fun ({ error; _ } : Dataset.dataset_error) ->
      match error with
      | Some e ->
        Base.String.is_substring
          (Atdgen_runtime.Util.Validation.string_of_error e)
          ~substring:"not completable"
      | None -> false)
    validated.errors
;;

let kill_objective_node ?(amount = 1) id =
  { Data.Quest_t.node_type = `Objective;
    id;
    name = "kill";
    metadata = minimal_metadata;
    is_optional = false;
    condition =
      `KillSpecific
        { Data.Quest_t.condition_type = `KillSpecific;
          character_types = [ `Adventurer ];
          character = None;
          amount
        }
  }
;;

let spawn_action_node id =
  { Data.Quest_t.node_type = `Action;
    id;
    name = "spawn";
    action = `Spawn { Data.Quest_t.action_type = `Spawn; spawn = minimal_spawn }
  }
;;

let quest_definition_with_root root : Data.Entity_t.entity_definition_internal =
  `Quest
    { Data.Entity_t.owner = "ownr";
      entity_type = `Quest;
      key = "CompletabilityQuest";
      version = 1;
      id = "ownr:Quest:CompletabilityQuest:1";
      entity = { minimal_quest with root }
    }
;;

let completability_dataset root =
  TestFixtures.create_dataset_with_definitions
    [ ("data/json/character_file.json", minimal_character_entity_definition);
      ("data/json/quest_file.json", quest_definition_with_root root)
    ]
;;

let sequence_node id children =
  `Sequence { Data.Quest_t.node_type = `Sequence; id; name = "seq"; children }
;;

let interact_errors dataset =
  let validated = Validate.validate_entity_definitions dataset in
  List.filter
    (fun ({ error; _ } : Dataset.dataset_error) ->
      match error with
      | Some e ->
        Base.String.is_substring
          (Atdgen_runtime.Util.Validation.string_of_error e)
          ~substring:"which is not a Zone"
      | None -> false)
    validated.errors
;;

let test_interact_needs_a_zone_anchor () =
  let anchor_ref =
    Dataset.entity_reference_of_entity_definition minimal_anchor_entity_definition
  in
  let objective : Data.Quest_t.quest_node_internal =
    `Objective
      { Data.Quest_t.node_type = `Objective;
        id = 0;
        name = "use";
        metadata = minimal_metadata;
        is_optional = false;
        condition =
          `Interact { Data.Quest_t.condition_type = `Interact; anchor = anchor_ref }
      }
  in
  let quest = ("data/json/quest_file.json", quest_definition_with_root objective) in
  let zone = ("data/json/anchor_file.json", minimal_anchor_entity_definition) in
  let portal =
    ( "data/json/anchor_file.json",
      `Anchor
        { Data.Entity_t.owner = "ownr";
          entity_type = `Anchor;
          key = "MinimalAnchor";
          version = 1;
          id = "ownr:Anchor:MinimalAnchor:1";
          entity =
            `Portal
              { Data.Anchor_t.anchor_type = `Portal;
                metadata = minimal_metadata;
                transform = minimal_transform
              }
        } )
  in
  let errors defs = List.length (interact_errors (TestFixtures.create_dataset_with_definitions defs)) in
  check int "a Zone anchor can be interacted with" 0 (errors [ zone; quest ]);
  check int "a Portal cannot" 1 (errors [ portal; quest ])
;;

let test_kill_objective_without_spawn_is_not_completable () =
  let dataset = completability_dataset (`Objective (kill_objective_node 0)) in
  check
    int
    "Kill objective without spawns produces an error"
    1
    (List.length (completability_errors dataset))
;;

let test_kill_objective_after_spawn_is_completable () =
  let root =
    sequence_node 0 [ `Action (spawn_action_node 1); `Objective (kill_objective_node 2) ]
  in
  check
    int
    "Kill objective preceded by a spawn is fine"
    0
    (List.length (completability_errors (completability_dataset root)))
;;

let test_kill_objective_before_spawn_is_not_completable () =
  let root =
    sequence_node 0 [ `Objective (kill_objective_node 1); `Action (spawn_action_node 2) ]
  in
  check
    int
    "Spawn after the kill objective does not count"
    1
    (List.length (completability_errors (completability_dataset root)))
;;

let test_kill_objective_concurrent_with_spawn_is_completable () =
  let root =
    `Parallel
      ({ Data.Quest_t.node_type = `Parallel;
         id = 0;
         name = "par";
         children = [ `Objective (kill_objective_node 1); `Action (spawn_action_node 2) ]
       }
       : Data.Quest_t.quest_parallel_node)
  in
  check
    int
    "Spawn concurrent with the kill objective counts"
    0
    (List.length (completability_errors (completability_dataset root)))
;;

let test_kill_objective_amount_exceeding_spawns_is_not_completable () =
  let root =
    sequence_node
      0
      [ `Action (spawn_action_node 1); `Objective (kill_objective_node ~amount:2 2) ]
  in
  check
    int
    "Kill amount above total spawn count produces an error"
    1
    (List.length (completability_errors (completability_dataset root)))
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
      minimal_skill_stone_entity_definition;
      minimal_status_entity_definition;
      minimal_projectile_entity_definition;
      minimal_quest_entity_definition;
      minimal_quest_difficulty_entity_definition;
      minimal_anchor_entity_definition;
      minimal_stage_entity_definition
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
      "Extract entity reference from quest"
      `Quick
      test_extract_entity_reference_from_quest;
    test_case
      "Kill condition character is a reference"
      `Quick
      test_kill_condition_character_is_a_reference;
    test_case
      "Teleport, pick and interact conditions name their references"
      `Quick
      test_teleport_pick_and_interact_conditions_name_their_references;
    test_case "Interact needs a Zone anchor" `Quick test_interact_needs_a_zone_anchor;
    test_case
      "Character starting kit is a reference"
      `Quick
      test_character_starting_kit_is_a_reference;
    test_case
      "Timer Restart and Interact round trip through JSON"
      `Quick
      test_timer_restart_and_new_fields_round_trip_through_json;
    test_case
      "Extract entity reference from stage"
      `Quick
      test_extract_entity_reference_from_stage;
    test_case
      "Anchor owned by one stage is valid"
      `Quick
      test_anchor_owned_by_one_stage_is_valid;
    test_case "Orphan anchor is invalid" `Quick test_orphan_anchor_is_invalid;
    test_case "Anchor owned twice is invalid" `Quick test_anchor_owned_twice_is_invalid;
    test_case "Drop table without curve is valid" `Quick test_drop_table_without_curve_is_valid;
    test_case "Drop curve rejects bare items" `Quick test_drop_curve_rejects_bare_items;
    test_case
      "Drop guarantee needs a matching entry"
      `Quick
      test_drop_guarantee_needs_a_matching_entry;
    test_case "Quest tiers" `Quick test_quest_tiers;
    (* Quest Completability *)
    test_case
      "Kill objective without spawn is not completable"
      `Quick
      test_kill_objective_without_spawn_is_not_completable;
    test_case
      "Kill objective after spawn is completable"
      `Quick
      test_kill_objective_after_spawn_is_completable;
    test_case
      "Kill objective before spawn is not completable"
      `Quick
      test_kill_objective_before_spawn_is_not_completable;
    test_case
      "Kill objective concurrent with spawn is completable"
      `Quick
      test_kill_objective_concurrent_with_spawn_is_completable;
    test_case
      "Kill amount exceeding spawns is not completable"
      `Quick
      test_kill_objective_amount_exceeding_spawns_is_not_completable;
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
    test_case
      "Status full_stack rule round trip"
      `Quick
      test_status_full_stack_rule_round_trips_and_defaults_to_absent;
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

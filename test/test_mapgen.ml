(* Map generation data contract: the static spec checks (a port of the Rust generator's `spec_problems`,
   `quest::demands` and `quest::apply_demands`) and the stage / anchor / prefab rules of the dataset. *)

open Alcotest
open Gamedata
open Test_fixtures
module G = Data.Generator_t

let reference entity_type key : Data.Common_t.entity_reference =
  let type_name = Data.Entity_util.string_of_entity_type entity_type in
  { owner = "ownr";
    entity_type;
    key;
    version = 1;
    id = Printf.sprintf "ownr:%s:%s:1" type_name key
  }
;;

let nest = reference `RoomPrefab "CryptHollowNest"
let shrine = reference `RoomPrefab "CryptOssuaryShrine"
let forest_glade = reference `RoomPrefab "ForestGlade"

let prefab_kind (r : Data.Common_t.entity_reference) =
  if r = nest || r = shrine then Some `Crypt else if r = forest_glade then Some `Forest else None
;;

let requirement ?(hard = true) ?placement ?path ?min_spacing ?prefab role min max size
  : G.room_requirement
  =
  { role;
    min;
    max;
    hard = (if hard then None else Some false);
    size;
    placement;
    path;
    min_spacing;
    prefab;
    tags = None;
    weight = None
  }
;;

let placement ?(farthest = false) ?(min_depth = 0) ?(chokepoint = false) () : G.room_placement =
  { farthest = (if farthest then Some true else None);
    min_depth = (if min_depth = 0 then None else Some min_depth);
    max_depth = None;
    chokepoint = (if chokepoint then Some true else None)
  }
;;

let range min max : Data.Common_t.int_range = { min; max }

(* The shipped crypt spec *)
let crypt : G.generator_spec =
  { generator = `Crypt;
    version = 1;
    requirements =
      [ requirement `Entrance 1 1 `Medium;
        requirement
          `Boss
          1
          1
          `Huge
          ~path:`MainPath
          ~placement:(placement ~farthest:true ~min_depth:5 ())
          ~prefab:nest;
        requirement
          `Rest
          1
          2
          `Medium
          ~path:`MainPath
          ~placement:(placement ~min_depth:2 ())
          ~min_spacing:3;
        requirement
          `Ambush
          2
          2
          `Medium
          ~path:`MainPath
          ~placement:(placement ~min_depth:1 ~chokepoint:true ());
        requirement
          `Treasure
          1
          1
          `Small
          ~hard:false
          ~path:`Branch
          ~placement:(placement ~min_depth:3 ());
        requirement `Combat 3 6 `Large ~path:`Any
      ];
    connections =
      { main_path = range 8 10; loops = range 1 2; maze = None; dead_ends = true; branch_len = range 1 2 };
    forbidden_adjacent = Some [ { a = `Rest; b = `Boss }; { a = `Entrance; b = `Ambush } ];
    pinned = Some [ { prefab = shrine; role = `Rest; at = None; facing = None } ];
    party_size = Some 4;
    corridor_width = Some 8.0;
    fill = `Combat;
    max_attempts = Some 64;
    candidates = Some 3;
    par = None;
    seed_policy = `PerRun;
    fixed_seed = None
  }
;;

let problems spec = Mapgen_check.spec_problems ~prefab_kind spec

let has_problem ~substring spec =
  List.exists (fun m -> Base.String.is_substring m ~substring) (problems spec)
;;

let with_requirements f = { crypt with requirements = f crypt.requirements }
let replace_role role r = List.map (fun (x : G.room_requirement) -> if x.role = role then r else x)

let test_the_shipped_crypt_has_no_problems () =
  check (list string) "no problems" [] (problems crypt)
;;

let test_an_infeasible_spec_lists_every_problem () =
  (* the boss cannot be deeper than the longest main path *)
  let deep =
    with_requirements
      (replace_role
         `Boss
         (requirement
            `Boss
            1
            1
            `Huge
            ~path:`MainPath
            ~placement:(placement ~farthest:true ~min_depth:12 ())))
  in
  check bool "boss too deep" true (has_problem ~substring:"must be at depth >= 12" deep);
  (* nine ambushes on a main path of at most ten rooms *)
  let crowded =
    with_requirements
      (replace_role
         `Ambush
         (requirement
            `Ambush
            9
            9
            `Medium
            ~path:`MainPath
            ~placement:(placement ~chokepoint:true ())))
  in
  check
    bool
    "main path overfull"
    true
    (has_problem ~substring:"the main path needs" crowded);
  (* a short main path *)
  let short = { crypt with connections = { crypt.connections with main_path = range 1 3 } } in
  check bool "main path under 2" true (has_problem ~substring:"at least 2 rooms" short);
  (* min above max, a loop range upside down *)
  let reversed =
    { (with_requirements (replace_role `Combat (requirement `Combat 5 2 `Large))) with
      connections = { crypt.connections with loops = range 3 1 }
    }
  in
  check bool "min > max" true (has_problem ~substring:"combat: min 5 > max 2" reversed);
  check bool "loops" true (has_problem ~substring:"loops [3, 1]: min > max" reversed)
;;

let test_prefabs_must_exist_and_match_the_generator () =
  let unknown = { crypt with pinned = Some [ { prefab = reference `RoomPrefab "Nope"; role = `Rest; at = None; facing = None } ] } in
  check bool "unknown prefab" true (has_problem ~substring:"unknown prefab Nope" unknown);
  let wrong_kind =
    with_requirements
      (replace_role
         `Boss
         (requirement
            `Boss
            1
            1
            `Huge
            ~path:`MainPath
            ~placement:(placement ~farthest:true ~min_depth:5 ())
            ~prefab:forest_glade))
  in
  check
    bool
    "forest prefab in a crypt"
    true
    (has_problem ~substring:"prefab ForestGlade is not a Crypt prefab" wrong_kind)
;;

let test_impossible_pins_are_refused () =
  let pin prefab role at : G.pinned_prefab =
    { prefab; role; at = Some at; facing = Some `PosX }
  in
  let two_fixed =
    { crypt with
      pinned =
        Some
          [ pin nest `Boss { x = 0.; z = 0. };
            pin shrine `Rest { x = 40.; z = 0. }
          ]
    }
  in
  check
    bool
    "two fixed positions"
    true
    (has_problem ~substring:"only one pinned prefab may have a fixed position" two_fixed);
  let no_room =
    { crypt with
      pinned = Some [ { prefab = shrine; role = `Objective; at = None; facing = None } ]
    }
  in
  check
    bool
    "a pin for a role no requirement makes"
    true
    (has_problem ~substring:"needs a objective room" no_room);
  let one_fixed =
    { crypt with pinned = Some [ pin nest `Boss { x = 0.; z = 0. } ] }
  in
  check (list string) "one fixed boss arena is fine" [] (problems one_fixed)
;;

let test_the_seed_policy_and_par_must_agree () =
  check
    bool
    "Fixed needs a seed"
    true
    (has_problem ~substring:"needs fixed_seed" { crypt with seed_policy = `Fixed });
  check
    bool
    "PerRun with a seed"
    true
    (has_problem ~substring:"only used by seed policy Fixed" { crypt with fixed_seed = Some 1 });
  check
    (list string)
    "Fixed with a seed"
    []
    (problems { crypt with seed_policy = `Fixed; fixed_seed = Some 7 });
  check
    bool
    "par upside down"
    true
    (has_problem
       ~substring:"par boss_path"
       { crypt with par = Some { boss_path = { min = 200.; max = 150. } } })
;;

(* ---- quest demands ---- *)

let anchor_ref key = reference `Anchor key
let character = minimal_character_reference

let spawn_node id anchor : Data.Quest_t.quest_node_internal =
  `Action
    { Data.Quest_t.node_type = `Action;
      id;
      name = "spawn";
      action =
        `Spawn
          { Data.Quest_t.action_type = `Spawn;
            spawn =
              { Data.Spawn_t.spawn_count = 1;
                character;
                anchor;
                target_adventurer_on_spawn = false
              }
          }
    }
;;

let objective_node id ~optional anchor : Data.Quest_t.quest_node_internal =
  `Objective
    { Data.Quest_t.node_type = `Objective;
      id;
      name = "enter";
      metadata = minimal_metadata;
      is_optional = optional;
      bonus_achievement = None;
      condition = `EnterAnchor { Data.Quest_t.condition_type = `EnterAnchor; anchor; amount = 1. }
    }
;;

let quest_of children : Data.Quest_t.quest =
  { minimal_quest with
    stage = reference `Stage "GenStage";
    root =
      `Parallel
        { Data.Quest_t.node_type = `Parallel;
          id = 100;
          name = "all";
          optional = None;
          bonus_achievement = None;
          children
        }
  }
;;

let binding slot ordinal : G.anchor_binding =
  { slot; ordinal = (if ordinal = 0 then None else Some ordinal); every = None }
;;

let id_of key = (anchor_ref key).id

let test_demands_count_slots_by_ordinal () =
  let quest =
    quest_of
      [ spawn_node 1 (anchor_ref "Pack0");
        spawn_node 2 (anchor_ref "Pack2");
        spawn_node 3 (anchor_ref "Boss")
      ]
  in
  let bindings =
    [ id_of "Pack0", binding `PackSpawn 0;
      id_of "Pack2", binding `PackSpawn 2;
      id_of "Boss", binding `BossSpawn 0
    ]
  in
  match Mapgen_check.demands quest bindings with
  | Error _ -> fail "every anchor is bound"
  | Ok demands ->
    check
      (list (pair string int))
      "pack spawns count up to the highest ordinal"
      [ "PackSpawn", 3; "BossSpawn", 1 ]
      (List.map
         (fun ({ slot; count; _ } : Mapgen_check.slot_demand) ->
           ( Base.String.strip
               ~drop:(fun c -> c = '"')
               (Data.Generator_j.string_of_anchor_slot slot),
             count ))
         demands)
;;

let test_unbound_anchors_are_reported () =
  let quest = quest_of [ spawn_node 1 (anchor_ref "Loose") ] in
  match Mapgen_check.demands quest [] with
  | Error [ id ] -> check string "the unbound anchor" (id_of "Loose") id
  | _ -> fail "expected one unbound anchor"
;;

let test_optional_objectives_ask_for_a_branch () =
  let quest =
    quest_of
      [ spawn_node 1 (anchor_ref "Boss");
        objective_node 2 ~optional:true (anchor_ref "Vault");
        objective_node 3 ~optional:false (anchor_ref "Boss")
      ]
  in
  let bindings = [ id_of "Boss", binding `BossSpawn 0; id_of "Vault", binding `Chest 0 ] in
  match Mapgen_check.demands quest bindings with
  | Error _ -> fail "bound"
  | Ok demands ->
    let chest = List.find (fun (d : Mapgen_check.slot_demand) -> d.slot = `Chest) demands in
    let boss = List.find (fun (d : Mapgen_check.slot_demand) -> d.slot = `BossSpawn) demands in
    check bool "only optional objectives name the chest" true chest.optional;
    check bool "the boss is also named by a required one" false boss.optional
;;

let test_demands_raise_the_requirements () =
  let demands : Mapgen_check.slot_demand list =
    [ { slot = `PackSpawn; count = 7; optional = false };
      { slot = `Shrine; count = 1; optional = true };
      { slot = `Chest; count = 2; optional = true };
      { slot = `PlayerSpawn; count = 1; optional = false }
    ]
  in
  let raised = Mapgen_check.apply_demands crypt demands in
  let find role = List.find (fun (r : G.room_requirement) -> r.role = role) raised.requirements in
  (* combat 3 + ambush 2 hard rooms already cover 5 packs: 2 more combat rooms *)
  check int "combat raised by what is missing" 5 (find `Combat).min;
  check bool "combat 5 <= max 6" true ((find `Combat).max >= 5);
  (* the shrine requirement already has a hard room: untouched *)
  check bool "rest not on a branch" true ((find `Rest).path = Some `MainPath);
  (* the soft treasure room becomes hard, two of them, on a branch *)
  let treasure = find `Treasure in
  check int "treasure min" 2 treasure.min;
  check int "treasure max" 2 treasure.max;
  check bool "treasure hard" true (Mapgen_check.hard treasure);
  check bool "treasure on a branch" true (treasure.path = Some `Branch);
  (* a role no requirement has yet gets one *)
  let objective =
    Mapgen_check.apply_demands
      crypt
      [ { slot = `ObjectiveZone; count = 1; optional = true } ]
  in
  let added = List.nth objective.requirements (List.length crypt.requirements) in
  check bool "objective room added" true (added.role = `Objective && added.path = Some `Branch)
;;

(* a forest's nest has a pack guarding it, so objective rooms count as pack places *)
let test_a_forest_nest_guards_a_pack () =
  let forest : G.generator_spec =
    { crypt with
      generator = `Forest;
      requirements =
        [ requirement `Combat 2 2 `Large; requirement `Objective 3 3 `Large ~path:`Branch ]
    }
  in
  let demand = [ { Mapgen_check.slot = `PackSpawn; count = 5; optional = false } ] in
  let raised = Mapgen_check.apply_demands forest demand in
  check bool "2 combat rooms + 3 nests hold 5 packs" true (raised.requirements = forest.requirements);
  let crypt_like = { forest with generator = `Crypt } in
  let raised = Mapgen_check.apply_demands crypt_like demand in
  let combat = List.find (fun (r : G.room_requirement) -> r.role = `Combat) raised.requirements in
  check int "a crypt's objective rooms hold no pack: combat raised" 5 combat.min
;;

let test_a_trail_demand_asks_for_no_room () =
  let raised =
    Mapgen_check.apply_demands
      crypt
      [ { slot = `TrailSpawn; count = 5; optional = false } ]
  in
  check bool "requirements unchanged" true (raised.requirements = crypt.requirements)
;;

let test_water_and_cocoon_demands_ask_for_no_room () =
  let raised =
    Mapgen_check.apply_demands
      crypt
      [ { slot = `WaterSpawn; count = 8; optional = false };
        { slot = `Cocoon; count = 3; optional = false }
      ]
  in
  check bool "requirements unchanged" true (raised.requirements = crypt.requirements)
;;

(* ---- dataset rules ---- *)

let anchor_definition key binding : Data.Entity_t.entity_definition_internal =
  `Anchor
    { Data.Entity_t.owner = "ownr";
      entity_type = `Anchor;
      key;
      version = 1;
      id = (anchor_ref key).id;
      entity =
        `Portal
          { Data.Anchor_t.anchor_type = `Portal;
            metadata = minimal_metadata;
            transform = minimal_transform;
            binding
          }
    }
;;

let lamp_definition key binding : Data.Entity_t.entity_definition_internal =
  `Anchor
    { Data.Entity_t.owner = "ownr";
      entity_type = `Anchor;
      key;
      version = 1;
      id = (anchor_ref key).id;
      entity =
        `LampPost
          { Data.Anchor_t.anchor_type = `LampPost;
            metadata = minimal_metadata;
            transform = minimal_transform;
            binding;
            model = "Lamp"
          }
    }
;;

let wave_spot_definition key : Data.Entity_t.entity_definition_internal =
  `Anchor
    { Data.Entity_t.owner = "ownr";
      entity_type = `Anchor;
      key;
      version = 1;
      id = (anchor_ref key).id;
      entity =
        `WaveSpot
          { Data.Anchor_t.anchor_type = `WaveSpot;
            metadata = minimal_metadata;
            transform = minimal_transform
          }
    }
;;

let shrine_definition key binding : Data.Entity_t.entity_definition_internal =
  `Anchor
    { Data.Entity_t.owner = "ownr";
      entity_type = `Anchor;
      key;
      version = 1;
      id = (anchor_ref key).id;
      entity =
        `HealingShrine
          { Data.Anchor_t.anchor_type = `HealingShrine;
            metadata = minimal_metadata;
            transform = minimal_transform;
            binding;
            model = "Shrine";
            price = 20
          }
    }
;;

let prefab_definition key prefab_id generator : Data.Entity_t.entity_definition_internal =
  `RoomPrefab
    { Data.Entity_t.owner = "ownr";
      entity_type = `RoomPrefab;
      key;
      version = 1;
      id = (reference `RoomPrefab key).id;
      entity = { minimal_room_prefab with prefab_id; generator }
    }
;;

let stage_definition anchors generator : Data.Entity_t.entity_definition_internal =
  `Stage
    { Data.Entity_t.owner = "ownr";
      entity_type = `Stage;
      key = "GenStage";
      version = 1;
      id = "ownr:Stage:GenStage:1";
      entity = { minimal_stage with anchors = List.map anchor_ref anchors; generator }
    }
;;

let quest_definition quest : Data.Entity_t.entity_definition_internal =
  `Quest
    { Data.Entity_t.owner = "ownr";
      entity_type = `Quest;
      key = "GenQuest";
      version = 1;
      id = "ownr:Quest:GenQuest:1";
      entity = quest
    }
;;

let prefabs =
  [ prefab_definition "CryptHollowNest" "crypt.hollow_nest" `Crypt;
    prefab_definition "CryptOssuaryShrine" "crypt.ossuary_shrine" `Crypt
  ]
;;

let errors definitions = Validate.generator_error_messages definitions

let mentions ~substring errors =
  List.exists (fun m -> Base.String.is_substring m ~substring) errors
;;

let sound_world =
  prefabs
  @ [ anchor_definition "Spawn" (Some (binding `PlayerSpawn 0));
      anchor_definition "Pack0" (Some (binding `PackSpawn 0));
      anchor_definition "Pack1" (Some (binding `PackSpawn 1));
      anchor_definition "Boss" (Some (binding `BossSpawn 0));
      stage_definition [ "Spawn"; "Pack0"; "Pack1"; "Boss" ] (Some crypt);
      quest_definition
        (quest_of
           [ spawn_node 1 (anchor_ref "Pack0");
             spawn_node 2 (anchor_ref "Pack1");
             spawn_node 3 (anchor_ref "Boss")
           ])
    ]
;;

let test_a_sound_generated_stage_has_no_errors () =
  check (list string) "no errors" [] (errors sound_world)
;;

let test_a_static_stage_is_not_checked () =
  check
    (list string)
    "no generator, no bindings needed"
    []
    (errors [ anchor_definition "Spawn" None; stage_definition [ "Spawn" ] None ])
;;

let test_every_anchor_of_a_generated_stage_needs_a_binding () =
  let world =
    prefabs
    @ [ anchor_definition "Spawn" None; stage_definition [ "Spawn" ] (Some crypt) ]
  in
  check
    bool
    "unbound anchor named"
    true
    (mentions ~substring:"anchor ownr:Anchor:Spawn:1 needs a binding" (errors world))
;;

let test_a_wave_spot_of_a_generated_stage_needs_no_binding () =
  let world =
    prefabs
    @ [ anchor_definition "Spawn" (Some (binding `PlayerSpawn 0));
        wave_spot_definition "Wave";
        stage_definition [ "Spawn"; "Wave" ] (Some crypt)
      ]
  in
  check
    bool
    "the wave spot is not named"
    false
    (mentions ~substring:"anchor ownr:Anchor:Wave:1" (errors world))
;;

let test_ordinals_must_be_contiguous () =
  let world =
    prefabs
    @ [ anchor_definition "Pack0" (Some (binding `PackSpawn 0));
        anchor_definition "Pack2" (Some (binding `PackSpawn 2));
        stage_definition [ "Pack0"; "Pack2" ] (Some crypt)
      ]
  in
  check
    bool
    "gap in the ordinals"
    true
    (mentions ~substring:"ordinals of the PackSpawn anchors are 0, 2" (errors world));
  let repeated =
    prefabs
    @ [ anchor_definition "A" (Some (binding `Shrine 0));
        anchor_definition "B" (Some (binding `Shrine 0));
        stage_definition [ "A"; "B" ] (Some crypt)
      ]
  in
  check
    bool
    "repeated ordinal"
    true
    (mentions ~substring:"ordinals of the Shrine anchors are 0, 0" (errors repeated))
;;

let every_binding slot ordinal : G.anchor_binding = { slot; ordinal; every = Some true }

let test_every_binds_a_template_lamp_post_or_spawn_point_only () =
  let stage anchor = prefabs @ [ anchor; stage_definition [ "Lamp" ] (Some crypt) ] in
  check
    (list string)
    "a lamp post may repeat"
    []
    (errors (stage (lamp_definition "Lamp" (Some (every_binding `Marker None)))));
  check
    (list string)
    "a healing shrine may repeat"
    []
    (errors (stage (shrine_definition "Lamp" (Some (every_binding `Healing None)))));
  check
    bool
    "a portal may not"
    true
    (mentions
       ~substring:"bound with every, which only a LampPost, a HealingShrine or a SpawnPoint may be"
       (errors (stage (anchor_definition "Lamp" (Some (every_binding `Marker None))))));
  check
    bool
    "no ordinal with every"
    true
    (mentions
       ~substring:"bound with every, so it has no ordinal"
       (errors (stage (lamp_definition "Lamp" (Some (every_binding `Marker (Some 1)))))))
;;

let test_an_infeasible_spec_on_a_stage_is_a_build_error () =
  let deep =
    { crypt with
      requirements =
        replace_role
          `Boss
          (requirement
             `Boss
             1
             1
             `Huge
             ~path:`MainPath
             ~placement:(placement ~farthest:true ~min_depth:20 ()))
          crypt.requirements
    }
  in
  let world = prefabs @ [ stage_definition [] (Some deep) ] in
  check
    bool
    "stage named, rule named"
    true
    (mentions ~substring:"Stage ownr:Stage:GenStage:1 generator: boss must be at depth >= 20" (errors world))
;;

let test_a_missing_prefab_is_an_error () =
  let world = [ stage_definition [] (Some crypt) ] in
  check
    bool
    "unknown prefab"
    true
    (mentions ~substring:"unknown prefab CryptHollowNest" (errors world))
;;

let test_a_quest_the_spec_cannot_hold_is_refused () =
  (* a main path of exactly four rooms leaves two inner rooms; three shrines on it cannot fit *)
  let tight =
    { crypt with
      requirements =
        [ requirement `Entrance 1 1 `Medium;
          requirement
            `Boss
            1
            1
            `Huge
            ~path:`MainPath
            ~placement:(placement ~farthest:true ~min_depth:2 ());
          requirement `Rest 1 1 `Medium ~path:`MainPath;
          requirement `Combat 1 3 `Large
        ];
      connections = { crypt.connections with main_path = range 4 4 };
      pinned = None
    }
  in
  let shrines =
    [ anchor_definition "Shrine0" (Some (binding `Shrine 0));
      anchor_definition "Shrine1" (Some (binding `Shrine 1));
      anchor_definition "Shrine2" (Some (binding `Shrine 2));
      anchor_definition "Boss" (Some (binding `BossSpawn 0))
    ]
  in
  let quest names =
    quest_definition (quest_of (List.mapi (fun i n -> spawn_node (i + 1) (anchor_ref n)) names))
  in
  let world names =
    prefabs
    @ shrines
    @ [ stage_definition [ "Shrine0"; "Shrine1"; "Shrine2"; "Boss" ] (Some tight); quest names ]
  in
  check (list string) "one shrine fits" [] (errors (world [ "Shrine0"; "Boss" ]));
  let refused = errors (world [ "Shrine0"; "Shrine1"; "Shrine2"; "Boss" ]) in
  check
    bool
    "three shrines do not"
    true
    (mentions
       ~substring:"Quest ownr:Quest:GenQuest:1 cannot run on generated stage ownr:Stage:GenStage:1: the main path needs 3 required rooms"
       refused)
;;

let test_a_quest_anchor_without_a_binding_is_refused () =
  let world =
    prefabs
    @ [ anchor_definition "Boss" (Some (binding `BossSpawn 0));
        anchor_definition "Loose" None;
        stage_definition [ "Boss"; "Loose" ] (Some crypt);
        quest_definition (quest_of [ spawn_node 1 (anchor_ref "Loose") ])
      ]
  in
  check
    bool
    "quest anchor without binding"
    true
    (mentions ~substring:"names anchor ownr:Anchor:Loose:1, which has no binding" (errors world))
;;

let test_room_prefabs_are_checked () =
  let twin = prefabs @ [ prefab_definition "Twin" "crypt.hollow_nest" `Crypt ] in
  check
    bool
    "duplicate prefab_id"
    true
    (mentions ~substring:"prefab_id crypt.hollow_nest is used by another prefab" (errors twin));
  let outside : Data.Entity_t.entity_definition_internal =
    `RoomPrefab
      { Data.Entity_t.owner = "ownr";
        entity_type = `RoomPrefab;
        key = "Wide";
        version = 1;
        id = "ownr:RoomPrefab:Wide:1";
        entity =
          { minimal_room_prefab with
            slots = [ { slot = `Shrine; x = 30.; z = 0.; area = None } ]
          }
      }
  in
  check
    bool
    "slot outside the room"
    true
    (mentions ~substring:"slot at (30, 0) is outside the 18 x 18 room" (errors [ outside ]))
;;

let test_prefab_ids_have_a_dotted_lower_case_shape () =
  let valid id = Data.Validation.validate_prefab_id id in
  check bool "crypt.hollow_nest" true (valid "crypt.hollow_nest");
  check bool "upper case" false (valid "Crypt.Hollow");
  check bool "no theme" false (valid "hollow_nest");
  check bool "empty tail" false (valid "crypt.")
;;

let test_the_generator_spec_round_trips_through_json () =
  let json = Data.Generator_j.string_of_generator_spec crypt in
  check bool "round trip" true (Data.Generator_j.generator_spec_of_string json = crypt);
  check
    bool
    "optional fields are left out"
    false
    (Base.String.is_substring json ~substring:"fixed_seed")
;;

let () =
  run
    "Mapgen"
    [ ( "Spec feasibility",
        [ test_case "The shipped crypt has no problems" `Quick test_the_shipped_crypt_has_no_problems;
          test_case "An infeasible spec lists every problem" `Quick test_an_infeasible_spec_lists_every_problem;
          test_case "Prefabs must exist and match the generator" `Quick test_prefabs_must_exist_and_match_the_generator;
          test_case "Impossible pins are refused" `Quick test_impossible_pins_are_refused;
          test_case "Seed policy and par must agree" `Quick test_the_seed_policy_and_par_must_agree
        ] );
      ( "Quest demands",
        [ test_case "Demands count slots by ordinal" `Quick test_demands_count_slots_by_ordinal;
          test_case "Unbound anchors are reported" `Quick test_unbound_anchors_are_reported;
          test_case "Optional objectives ask for a branch" `Quick test_optional_objectives_ask_for_a_branch;
          test_case "Demands raise the requirements" `Quick test_demands_raise_the_requirements;
          test_case "A forest nest guards a pack" `Quick test_a_forest_nest_guards_a_pack;
          test_case "A trail demand asks for no room" `Quick test_a_trail_demand_asks_for_no_room;
          test_case "Water and cocoon demands ask for no room" `Quick test_water_and_cocoon_demands_ask_for_no_room
        ] );
      ( "Dataset rules",
        [ test_case "A sound generated stage has no errors" `Quick test_a_sound_generated_stage_has_no_errors;
          test_case "A static stage is not checked" `Quick test_a_static_stage_is_not_checked;
          test_case "Every anchor needs a binding" `Quick test_every_anchor_of_a_generated_stage_needs_a_binding;
          test_case "A wave spot needs no binding" `Quick test_a_wave_spot_of_a_generated_stage_needs_no_binding;
          test_case "Ordinals must be contiguous" `Quick test_ordinals_must_be_contiguous;
          test_case "Every binds a template lamp post only" `Quick test_every_binds_a_template_lamp_post_or_spawn_point_only;
          test_case "An infeasible spec is a build error" `Quick test_an_infeasible_spec_on_a_stage_is_a_build_error;
          test_case "A missing prefab is an error" `Quick test_a_missing_prefab_is_an_error;
          test_case "A quest the spec cannot hold is refused" `Quick test_a_quest_the_spec_cannot_hold_is_refused;
          test_case "A quest anchor without a binding is refused" `Quick test_a_quest_anchor_without_a_binding_is_refused;
          test_case "Room prefabs are checked" `Quick test_room_prefabs_are_checked;
          test_case "Prefab ids have a dotted lower case shape" `Quick test_prefab_ids_have_a_dotted_lower_case_shape;
          test_case "The generator spec round trips through JSON" `Quick test_the_generator_spec_round_trips_through_json
        ] )
    ]
;;

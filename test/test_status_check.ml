(* One status per meaning (fey-rs #427): a status is not titled after a stat, and is applied one way only,
   with a picture family that agrees. *)
open Alcotest
open Test_fixtures

let stat key stat_type title ~lower_is_better : Data.Entity_t.entity_definition_internal =
  `Stat
    { Data.Entity_t.owner = "ownr";
      entity_type = `Stat;
      key;
      version = 1;
      id = "ownr:Stat:" ^ key ^ ":1";
      entity =
        { minimal_stat with
          stat_type;
          metadata = { minimal_metadata with title };
          display = { minimal_stat.display with lower_is_better }
        }
    }
;;

let status key title ?vfx stat : Data.Entity_t.entity_definition_internal =
  `Status
    { Data.Entity_t.owner = "ownr";
      entity_type = `Status;
      key;
      version = 1;
      id = "ownr:Status:" ^ key ^ ":1";
      entity =
        { minimal_status with
          metadata = { minimal_metadata with title };
          mechanic = `StatChange { Data.Status_t.mechanic_type = `StatChange; stat };
          vfx =
            Option.map
              (fun effect -> { Data.Vfx_t.element = `Physical; effect })
              vfx
        }
    }
;;

let effect status_key base : Data.Effect_t.status_effect =
  { Data.Effect_t.target_mechanic = `Self { mechanic_type = `Self };
    target = `Ally;
    durations = [];
    scalers = [ { base; scaling = { min = 0.; max = 0. }; stat = `AttackPower } ];
    status =
      { minimal_entity_reference with
        entity_type = `Status;
        key = status_key;
        id = "ownr:Status:" ^ status_key ^ ":1"
      };
    dispel = None
  }
;;

let skill_with skill_key ?triggers child : Data.Entity_t.entity_definition_internal =
  `Skill
    { Data.Entity_t.owner = "ownr";
      entity_type = `Skill;
      key = skill_key;
      version = 1;
      id = "ownr:Skill:" ^ skill_key ^ ":1";
      entity =
        { minimal_skill with
          execution_root = { minimal_skill.execution_root with child };
          triggers
        }
    }
;;

(* A skill that puts [status_key] on its caster with a scaler of the given base *)
let applying skill_key status_key base =
  skill_with
    skill_key
    (`Status
      { Data.Skill_t.skill_action_node_type = `Status;
        name = "apply";
        status_effect = effect status_key base
      })
;;

let damage_taken = stat "DamageTakenModifier" `DamageTakenModifier "Damage Taken Modifier" ~lower_is_better:true
let movement = stat "MovementSpeed" `MovementSpeed "Movement Speed" ~lower_is_better:false
let count definitions = List.length (Gamedata.Status_check.error_messages definitions)
let titles definitions = List.length (Gamedata.Status_check.title_error_messages definitions)

let test_a_status_is_not_titled_after_a_stat () =
  check int "the stat's title" 1 (titles [ movement; status "Swift" "Movement Speed" `MovementSpeed ]);
  check int "the stat's key without its modifier" 1
    (titles [ damage_taken; status "Guard" "Damage Taken" `DamageTakenModifier ]);
  check int "the stat's key as a title" 1
    (titles [ damage_taken; status "Guard" "DamageTakenModifier" `DamageTakenModifier ]);
  let labelled : Data.Entity_t.entity_definition_internal =
    match damage_taken with
    | `Stat ({ entity; _ } as d) ->
      `Stat { d with entity = { entity with display = { entity.display with label = Some "Damage taken" } } }
    | other -> other
  in
  check int "the stat's display label" 1
    (titles [ labelled; status "Guard" "Damage Taken" `DamageTakenModifier ]);
  check int "another name" 0 (titles [ movement; damage_taken; status "Swift" "Swift" `MovementSpeed ])
;;

let test_a_mechanism_named_after_a_stat_is_caught () =
  let regen = stat "HealthRegen" `HealthRegen "Health Regeneration" ~lower_is_better:false in
  check int "Health Regen is the stat HealthRegen" 1 (titles [ regen; status "Regen" "Health Regen" `HealthRegen ])
;;

let test_a_status_applied_as_gain_and_loss_is_two_meanings () =
  let guard = status "Guard" "Guard" `DamageTakenModifier in
  check int "one way" 0 (count [ damage_taken; guard; applying "A" "Guard" (-0.3); applying "B" "Guard" (-0.6) ]);
  check int "both ways" 1 (count [ damage_taken; guard; applying "A" "Guard" (-0.3); applying "B" "Guard" 0.3 ])
;;

let test_the_picture_agrees_with_the_direction () =
  let guard vfx = status "Guard" "Guard" ~vfx `DamageTakenModifier in
  check int "less damage taken with a buff" 0
    (count [ damage_taken; guard "buff/ward"; applying "A" "Guard" (-0.3) ]);
  check int "less damage taken with a debuff" 1
    (count [ damage_taken; guard "debuff/vulnerable"; applying "A" "Guard" (-0.3) ]);
  check int "more damage taken with a buff" 1
    (count [ damage_taken; guard "buff/ward"; applying "A" "Guard" 0.3 ]);
  let swift vfx = status "Swift" "Swift" ~vfx `MovementSpeed in
  check int "a speed gain with a buff" 0 (count [ movement; swift "buff/haste"; applying "A" "Swift" 0.5 ]);
  check int "a speed gain with a debuff" 1 (count [ movement; swift "debuff/slow"; applying "A" "Swift" 0.5 ]);
  check int "a control picture says nothing" 0
    (count [ movement; swift "control/swap"; applying "A" "Swift" 0.5 ])
;;

let pinned definition expected =
  check
    (list (pair string int))
    "the applications found"
    expected
    (Gamedata.Status_check.all_applications [ definition ])
;;

(* Each carrier of a status application is pinned: a schema change that renames one must break here, not
   leave the rule blind *)
let test_every_carrier_of_an_application_is_found () =
  let swift = "ownr:Status:Swift:1" in
  pinned (applying "S" "Swift" 0.5) [ swift, 1 ];
  let trigger =
    { Data.Trigger_t.key = "t";
      on = `Hit;
      relation = None;
      chance = 1.;
      cooldown = 0.;
      min_amount = None;
      status = None;
      min_stacks = None;
      max_chain = None;
      terminal = None;
      effects = [ { Data.Trigger_t.hit = None; status = Some (effect "Swift" (-0.5)); skill = None; exclude_other = None } ]
    }
  in
  pinned (skill_with "T" ~triggers:[ trigger ] minimal_skill.execution_root.child) [ swift, -1 ];
  let projectile : Data.Entity_t.entity_definition_internal =
    `Projectile
      { Data.Entity_t.owner = "ownr";
        entity_type = `Projectile;
        key = "P";
        version = 1;
        id = "ownr:Projectile:P:1";
        entity =
          `Homing
            { minimal_projectile_homing with
              on_impact =
                [ `Status { Data.Projectile_t.impact_type = `Status; status_effect = effect "Swift" 0.5 } ]
            }
      }
  in
  pinned projectile [ swift, 1 ];
  let enraged : Data.Entity_t.entity_definition_internal =
    `Character
      { Data.Entity_t.owner = "ownr";
        entity_type = `Character;
        key = "C";
        version = 1;
        id = "ownr:Character:C:1";
        entity =
          { minimal_character with
            enrage = Some { Data.Character_t.after = 5.; effects = [ effect "Swift" 0.35 ] }
          }
      }
  in
  pinned enraged [ swift, 1 ]
;;

let test_a_walk_that_finds_nothing_fails () =
  check int "stat changes and no application" 1 (count [ movement; status "Swift" "Swift" `MovementSpeed ]);
  check int "an application found" 0
    (count [ movement; status "Swift" "Swift" `MovementSpeed; applying "A" "Swift" 0.5 ])
;;

let () =
  run
    "status_check"
    [ ( "status_check",
        [ test_case "A status is not titled after a stat" `Quick test_a_status_is_not_titled_after_a_stat;
          test_case "A mechanism named after a stat is caught" `Quick test_a_mechanism_named_after_a_stat_is_caught;
          test_case "Gain and loss are two meanings" `Quick test_a_status_applied_as_gain_and_loss_is_two_meanings;
          test_case "The picture agrees with the direction" `Quick test_the_picture_agrees_with_the_direction;
          test_case "Every carrier of an application is found" `Quick test_every_carrier_of_an_application_is_found;
          test_case "A walk that finds nothing fails" `Quick test_a_walk_that_finds_nothing_fails
        ] )
    ]
;;

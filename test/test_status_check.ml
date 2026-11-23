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

(* A skill that puts [status_key] on its caster with a scaler of the given base *)
let applying skill_key status_key base : Data.Entity_t.entity_definition_internal =
  let node =
    `Status
      { Data.Skill_t.skill_action_node_type = `Status;
        name = "apply";
        status_effect =
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
      }
  in
  `Skill
    { Data.Entity_t.owner = "ownr";
      entity_type = `Skill;
      key = skill_key;
      version = 1;
      id = "ownr:Skill:" ^ skill_key ^ ":1";
      entity =
        { minimal_skill with
          execution_root = { minimal_skill.execution_root with child = node }
        }
    }
;;

let damage_taken = stat "DamageTakenModifier" `DamageTakenModifier "Damage Taken Modifier" ~lower_is_better:true
let movement = stat "MovementSpeed" `MovementSpeed "Movement Speed" ~lower_is_better:false
let count definitions = List.length (Gamedata.Status_check.error_messages definitions)

let test_a_status_is_not_titled_after_a_stat () =
  check int "the stat's title" 1 (count [ movement; status "Swift" "Movement Speed" `MovementSpeed ]);
  check int "the stat's key without its modifier" 1
    (count [ damage_taken; status "Guard" "Damage Taken" `DamageTakenModifier ]);
  check int "the stat's key as a title" 1
    (count [ damage_taken; status "Guard" "DamageTakenModifier" `DamageTakenModifier ]);
  check int "another name" 0 (count [ movement; damage_taken; status "Swift" "Swift" `MovementSpeed ])
;;

let test_a_mechanism_named_after_a_stat_is_caught () =
  let regen = stat "HealthRegen" `HealthRegen "Health Regeneration" ~lower_is_better:false in
  check int "Health Regen is the stat HealthRegen" 1 (count [ regen; status "Regen" "Health Regen" `HealthRegen ])
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

let () =
  run
    "status_check"
    [ ( "status_check",
        [ test_case "A status is not titled after a stat" `Quick test_a_status_is_not_titled_after_a_stat;
          test_case "A mechanism named after a stat is caught" `Quick test_a_mechanism_named_after_a_stat_is_caught;
          test_case "Gain and loss are two meanings" `Quick test_a_status_applied_as_gain_and_loss_is_two_meanings;
          test_case "The picture agrees with the direction" `Quick test_the_picture_agrees_with_the_direction
        ] )
    ]
;;

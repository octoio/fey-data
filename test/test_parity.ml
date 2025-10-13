(* Professional Parity Tests for the Gamedata Library
 * This module provides comprehensive testing of polymorphic variant parity functions
 * using proper ATD creation functions instead of unsafe magic objects.
 *)

open Alcotest
open Gamedata.Parity
open Test_fixtures

(* Create a valid stat sheet for testing using the proper ATD creation function *)
let create_test_stat_sheet () =
  Data.Stat_v.create_stat_sheet
    ~vit:10
    ~str:15
    ~_int:8
    ~dex:12
    ~armor:20
    ~magic_resist:10
    ~health:100
    ~mana:50
    ~damage_taken_modifier:0.9
    ~damage_modifier:1.1
    ~movement_speed:5.0
    ~movement_speed_modifier:1.0
    ~attack_speed:1.2
    ~attack_power:25.0
    ~ability_power:18.0
    ~critical_chance:0.05
    ~critical_damage:1.5
    ~cooldown_reduction:0.1
    ~dodge_chance:0.02
    ~mana_regen:2.5
    ~health_regen:1.3
    ~experience_modifier:1.2
    ~gold_modifier:1.1
    ~life_steal:0.08
    ()
;;

(* Comprehensive test for entity parity - tests all supported entity types *)
let test_entity_parity_comprehensive () =
  let test_cases =
    [ ("weapon", minimal_weapon_entity_definition, `Weapon);
      ("skill", minimal_skill_entity_definition, `Skill);
      ("equipment", minimal_equipment_entity_definition, `Equipment);
      ("status", minimal_status_entity_definition, `Status);
      ("model", minimal_model_entity_definition, `Model);
      ("image", minimal_image_entity_definition, `Image);
      ("cursor", minimal_cursor_entity_definition, `Cursor);
      ("stat", minimal_stat_entity_definition, `Stat);
      ("quality", minimal_quality_entity_definition, `Quality);
      ("audio_clip", minimal_audio_clip_entity_definition, `AudioClip);
      ("sound", minimal_sound_entity_definition, `Sound);
      ("sound_bank", minimal_sound_bank_entity_definition, `SoundBank);
      ("drop_table", minimal_drop_table_entity_definition, `DropTable);
      ("character", minimal_character_entity_definition, `Character);
      ("animation_source", minimal_animation_source_entity_definition, `AnimationSource);
      ("animation", minimal_animation_entity_definition, `Animation);
      ("projectile", minimal_projectile_entity_definition, `Projectile)
    ]
  in
  List.iter
    (fun (name, entity_def, expected_type) ->
      let result = test_entity_parity entity_def in
      check
        (of_pp Data.Common_t.pp_entity_type)
        ("entity parity: " ^ name)
        expected_type
        result)
    test_cases
;;

(* Comprehensive test for stat sheet parity - tests all stat types *)
let test_stat_sheet_parity_comprehensive () =
  let stat_sheet = create_test_stat_sheet () in
  let test_cases =
    [ ("vit", `Vit, 10);
      ("str", `Str, 15);
      ("int", `Int, 8);
      ("dex", `Dex, 12);
      ("armor", `Armor, 20);
      ("magic_resist", `MagicResist, 10);
      ("health", `Health, 100);
      ("mana", `Mana, 50);
      ("damage_taken_modifier", `DamageTakenModifier, 0);
      (* float converted to int *)
      ("damage_modifier", `DamageModifier, 1);
      (* float converted to int *)
      ("movement_speed", `MovementSpeed, 5);
      (* float converted to int *)
      ("movement_speed_modifier", `MovementSpeedModifier, 1);
      (* float converted to int *)
      ("attack_speed", `AttackSpeed, 1);
      (* float converted to int *)
      ("attack_power", `AttackPower, 25);
      (* float converted to int *)
      ("ability_power", `AbilityPower, 18);
      (* float converted to int *)
      ("critical_chance", `CriticalChance, 0);
      (* float converted to int *)
      ("critical_damage", `CriticalDamage, 1);
      (* float converted to int *)
      ("cooldown_reduction", `CooldownReduction, 0);
      (* float converted to int *)
      ("dodge_chance", `DodgeChance, 0);
      (* float converted to int *)
      ("mana_regen", `ManaRegen, 2);
      (* float converted to int *)
      ("health_regen", `HealthRegen, 1);
      (* float converted to int *)
      ("experience_modifier", `ExperienceModifier, 1);
      (* float converted to int *)
      ("gold_modifier", `GoldModifier, 1);
      (* float converted to int *)
      ("life_steal", `LifeSteal, 0) (* float converted to int *)
    ]
  in
  List.iter
    (fun (name, stat_type, expected_value) ->
      let result = test_stat_sheet_parity stat_type stat_sheet in
      check int ("stat sheet parity: " ^ name) expected_value result)
    test_cases
;;

(* Test specific stat types that might have edge cases *)
let test_stat_sheet_parity_edge_cases () =
  let stat_sheet = create_test_stat_sheet () in
  (* Test integer stats *)
  let vit_result = test_stat_sheet_parity `Vit stat_sheet in
  check int "vit value should be 10" 10 vit_result;
  let str_result = test_stat_sheet_parity `Str stat_sheet in
  check int "str value should be 15" 15 str_result;
  (* Test float-to-int conversion for damage modifier *)
  let damage_mod_result = test_stat_sheet_parity `DamageModifier stat_sheet in
  check int "damage modifier should convert 1.1 to 1" 1 damage_mod_result;
  (* Test float-to-int conversion for attack power *)
  let attack_power_result = test_stat_sheet_parity `AttackPower stat_sheet in
  check int "attack power should convert 25.0 to 25" 25 attack_power_result
;;

(* Test animation parity with available animation sources *)
let test_animation_parity_comprehensive () =
  (* Only test with available fixtures from test_fixtures.ml *)
  let generic_animation_source_internal = `Generic minimal_generic_animation_source in
  let result = test_animation_parity generic_animation_source_internal in
  check
    (of_pp Data.Animation_t.pp_animation_type)
    "animation parity: generic"
    `Generic
    result
;;

let () =
  run
    "Professional Parity Tests"
    [ ( "entity_parity",
        [ test_case
            "comprehensive entity type testing"
            `Quick
            test_entity_parity_comprehensive
        ] );
      ( "stat_sheet_parity",
        [ test_case
            "comprehensive stat sheet testing"
            `Quick
            test_stat_sheet_parity_comprehensive;
          test_case "stat sheet edge cases" `Quick test_stat_sheet_parity_edge_cases
        ] );
      ( "animation_parity",
        [ test_case
            "comprehensive animation testing"
            `Quick
            test_animation_parity_comprehensive
        ] )
    ]
;;

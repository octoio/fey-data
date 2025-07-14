(* TODO: This file is a placeholder for the parity functions that will be used to test. Implement ocaml tests *)
let test_entity_parity (entity_definition : Data.Entity_t.entity_definition_internal)
  : Data.Common_t.entity_type
  =
  match entity_definition with
  | `Weapon _ -> `Weapon
  | `Skill _ -> `Skill
  | `Equipment _ -> `Equipment
  | `Status _ -> `Status
  | `Model _ -> `Model
  | `AudioClip _ -> `AudioClip
  | `Sound _ -> `Sound
  | `Image _ -> `Image
  | `Cursor _ -> `Cursor
  | `Stat _ -> `Stat
  | `Quality _ -> `Quality
  | `SoundBank _ -> `SoundBank
  | `DropTable _ -> `DropTable
  | `Character _ -> `Character
  | `AnimationSource _ -> `AnimationSource
  | `Animation _ -> `Animation
;;

let test_stat_sheet_parity
  (stat_type : Data.Stat_t.stat_type)
  ({ vit;
     str;
     _int;
     dex;
     armor;
     magic_resist;
     health;
     mana;
     damage_taken_modifier;
     damage_modifier;
     movement_speed;
     movement_speed_modifier;
     attack_speed;
     attack_power;
     ability_power;
     critical_chance;
     critical_damage;
     cooldown_reduction;
     dodge_chance;
     mana_regen;
     health_regen;
     experience_modifier;
     gold_modifier;
     life_steal
   } :
    Data.Stat_t.stat_sheet)
  : int
  =
  match stat_type with
  | `Vit -> vit
  | `Str -> str
  | `Int -> _int
  | `Dex -> dex
  | `Armor -> armor
  | `MagicResist -> magic_resist
  | `Health -> health
  | `Mana -> mana
  | `DamageTakenModifier -> Int.of_float damage_taken_modifier
  | `DamageModifier -> Int.of_float damage_modifier
  | `MovementSpeed -> Int.of_float movement_speed
  | `MovementSpeedModifier -> Int.of_float movement_speed_modifier
  | `AttackSpeed -> Int.of_float attack_speed
  | `AttackPower -> Int.of_float attack_power
  | `AbilityPower -> Int.of_float ability_power
  | `CriticalChance -> Int.of_float critical_chance
  | `CriticalDamage -> Int.of_float critical_damage
  | `CooldownReduction -> Int.of_float cooldown_reduction
  | `DodgeChance -> Int.of_float dodge_chance
  | `ManaRegen -> Int.of_float mana_regen
  | `HealthRegen -> Int.of_float health_regen
  | `ExperienceModifier -> Int.of_float experience_modifier
  | `GoldModifier -> Int.of_float gold_modifier
  | `LifeSteal -> Int.of_float life_steal
;;

let test_animation_parity (animation_source : Data.Animation_t.animation_source_internal)
  : Data.Animation_t.animation_type
  =
  match animation_source with
  | `Generic _ -> `Generic
  | `HumanoidDaggerAttack _ -> `HumanoidDaggerAttack
  | `HumanoidSwordAttack _ -> `HumanoidSwordAttack
  | `HumanoidSpearAttack _ -> `HumanoidSpearAttack
  | `HumanoidMaceAttack _ -> `HumanoidMaceAttack
  | `HumanoidTwoHandedStaffAttack _ -> `HumanoidTwoHandedStaffAttack
  | `HumanoidTwoHandedAxeAttack _ -> `HumanoidTwoHandedAxeAttack
  | `HumanoidItemAttack _ -> `HumanoidItemAttack
  | `HumanoidShieldAttack _ -> `HumanoidShieldAttack
  | `HumanoidTwoHandedSpearAttack _ -> `HumanoidTwoHandedSpearAttack
  | `HumanoidTwoHandedSwordAttack _ -> `HumanoidTwoHandedSwordAttack
  | `HumanoidUnarmedAttack _ -> `HumanoidUnarmedAttack
  | `HumanoidCast _ -> `HumanoidCast
  | `HumanoidAttackCast _ -> `HumanoidAttackCast
  | `HumanoidDualAttack _ -> `HumanoidDualAttack
  | `HumanoidBlockedHit _ -> `HumanoidBlockedHit
;;

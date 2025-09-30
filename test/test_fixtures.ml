(* Test fixtures for shared test data *)

(* Sample entity definitions for testing *)
let sample_weapon_json =
  {|
{
  "id": "test_sword_001",
  "owner": "player",
  "type": "Weapon",
  "key": "sword_basic",
  "version": 1,
  "entity": {
    "metadata": {
      "name": "Basic Sword",
      "description": "A simple iron sword"
    },
    "icon_reference": "sword_icon",
    "model_reference": "sword_model",
    "weapon_category": "Sword",
    "damage": 25.0,
    "attack_speed": 1.0,
    "range": 1.5,
    "requirements": [],
    "stat_modifiers": {
      "vit": 0, "str": 5, "_int": 0, "dex": 0,
      "armor": 0, "magic_resist": 0, "health": 0, "mana": 0,
      "damage_taken_modifier": 0.0, "damage_modifier": 0.0,
      "movement_speed": 0.0, "movement_speed_modifier": 0.0,
      "attack_speed": 0.0, "attack_power": 10.0, "ability_power": 0.0,
      "critical_chance": 0.0, "critical_damage": 0.0,
      "cooldown_reduction": 0.0, "dodge_chance": 0.0,
      "mana_regen": 0.0, "health_regen": 0.0,
      "experience_modifier": 0.0, "gold_modifier": 0.0,
      "life_steal": 0.0
    },
    "equip_index": "MainHand",
    "sheathe_location": "Back",
    "basic_attack": null
  }
}
|}
;;

let sample_character_json =
  {|
{
  "id": "test_char_001",
  "owner": "system",
  "type": "Character",
  "key": "warrior_basic",
  "version": 1,
  "entity": {
    "metadata": {
      "name": "Basic Warrior",
      "description": "A starting warrior character"
    },
    "icon_reference": "warrior_icon",
    "model_reference": "warrior_model",
    "character_class": "Warrior",
    "level": 1,
    "experience": 0,
    "stats": {
      "vit": 10, "str": 15, "_int": 5, "dex": 8,
      "armor": 5, "magic_resist": 2, "health": 100, "mana": 20
    }
  }
}
|}
;;

let sample_invalid_json =
  {|
{
  "id": "invalid_item",
  "owner": "",
  "type": "Unknown",
  "key": "",
  "version": -1,
  "entity": null
}
|}
;;

(* Test file paths *)
let test_data_dir = "test/data"
let sample_weapon_file = test_data_dir ^ "/sample_weapon.json"
let sample_character_file = test_data_dir ^ "/sample_character.json"
let sample_invalid_file = test_data_dir ^ "/sample_invalid.json"

(* Helper functions for creating test datasets *)
let create_empty_dataset () =
  Gamedata.Dataset.
    { entity_index_container = { indices = [] }; definitions = []; errors = [] }
;;

let create_test_error message field =
  Atdgen_runtime.Util.Validation.error ~msg:message [ `Field field ]
;;

let create_sample_dataset_with_error () =
  let dataset = create_empty_dataset () in
  let error = Some (create_test_error "Test validation error" "test_field") in
  Gamedata.Dataset.add_error "__LOC__" "test/sample.json" error dataset
;;

(* Test constants *)
let test_constants =
  object
    method max_test_entities = 100
    method default_timeout_ms = 5000
    method test_batch_size = 10
    method sample_entity_count = 3
  end
;;

(* Minimal valid metadata *)
let minimal_metadata = { Data.Common_t.title = "t"; description = "d" }

(* Minimal valid entity_reference *)
let minimal_entity_reference =
  { Data.Common_t.owner = "ownr";
    entity_type = `Image;
    key = "img";
    version = 1;
    id = "ownr:Image:img:1"
  }
;;

let minimal_int_range : Data.Common_t.int_range = { min = 0; max = 0 }
let minimal_float_range : Data.Common_t.float_range = { min = 0.0; max = 0.0 }

(* Minimal valid model_anchor_set *)
let minimal_model_anchor_set =
  { Data.Model_t.model_reference = minimal_entity_reference; anchors = [] }
;;

(* Minimal valid weapon *)
let minimal_weapon =
  { Data.Weapon_t.metadata = minimal_metadata;
    category = `OneHandSword;
    quality = `Common;
    sheathe_location = `Hips;
    icon_reference = minimal_entity_reference;
    base_stat_affixes = [];
    random_stat_affix_count = minimal_int_range;
    random_stat_affixes = [];
    basic_attack = None;
    model_anchor_set = minimal_model_anchor_set
  }
;;

(* Minimal valid skill *)
let minimal_skill =
  let open Data.Skill_v in
  let open Data.Requirement_v in
  let minimal_requirement_eval =
    create_requirement_evaluation ~operator:`All ~requirements:[] ()
  in
  let minimal_child_node =
    `Delay
      (create_skill_action_delay_node
         ~skill_action_node_type:`Delay
         ~name:"minimal"
         ~delay:0.0
         ())
  in
  let minimal_execution_root =
    create_skill_action_requirement_node
      ~skill_action_node_type:`Requirement
      ~name:"root"
      ~requirements:minimal_requirement_eval
      ~child:minimal_child_node
      ()
  in
  { Data.Skill_t.metadata = minimal_metadata;
    quality = `Common;
    icon_reference = minimal_entity_reference;
    categories = [ `Offense ];
    cost = { Data.Skill_t.mana = 0 };
    cooldown = 0.0;
    target_type = `Enemy;
    execution_root = minimal_execution_root;
    cast_distance = minimal_float_range;
    indicators = []
  }
;;

(* Minimal valid color *)
let minimal_color = { Data.Common_t.r = 0.0; g = 0.0; b = 0.0; a = 1.0 }

(* Minimal valid stat_affix *)
let minimal_stat_affix = { Data.Affix_t.stat = `Vit; value = minimal_float_range }

(* Minimal valid equipment *)
let minimal_equipment =
  { Data.Equipment_t.metadata = minimal_metadata;
    category = `Head;
    quality = `Common;
    icon_reference = minimal_entity_reference;
    base_stat_affixes = [];
    random_stat_affix_count = minimal_int_range;
    random_stat_affixes = []
  }
;;

let minimal_equipment_entity_definition : Data.Entity_t.entity_definition_internal =
  `Equipment
    { Data.Entity_t.owner = "ownr";
      entity_type = `Equipment;
      key = "MinimalEquipment";
      version = 1;
      id = "ownr:Equipment:MinimalEquipment:1";
      entity = minimal_equipment
    }
;;

(* Minimal valid status *)
let minimal_status =
  { Data.Status_t.metadata = minimal_metadata;
    mechanic = `StatChange { Data.Status_t.mechanic_type = `StatChange; stat = `Vit };
    stack = { Data.Status_t.size = 1; scaling_strategy = `Additive }
  }
;;

let minimal_status_entity_definition : Data.Entity_t.entity_definition_internal =
  `Status
    { Data.Entity_t.owner = "ownr";
      entity_type = `Status;
      key = "MinimalStatus";
      version = 1;
      id = "ownr:Status:MinimalStatus:1";
      entity = minimal_status
    }
;;

(* Minimal valid weapon_entity_definition wrapped in entity_definition_internal *)
let minimal_weapon_entity_definition : Data.Entity_t.entity_definition_internal =
  `Weapon
    { Data.Entity_t.owner = "ownr";
      entity_type = `Weapon;
      key = "RustySword";
      version = 1;
      id = "ownr:Weapon:RustySword:1";
      entity = minimal_weapon
    }
;;

(* Minimal valid skill_entity_definition wrapped in entity_definition_internal *)
let minimal_skill_entity_definition : Data.Entity_t.entity_definition_internal =
  `Skill
    { Data.Entity_t.owner = "ownr";
      entity_type = `Skill;
      key = "MinimalSkill";
      version = 1;
      id = "ownr:Skill:MinimalSkill:1";
      entity = minimal_skill
    }
;;

(* Minimal valid character *)
let minimal_vector3 = { Data.Common_t.x = 0.0; y = 0.0; z = 0.0 }

let minimal_stat_sheet =
  { Data.Stat_t.vit = 1;
    str = 1;
    _int = 1;
    dex = 1;
    armor = 1;
    magic_resist = 1;
    health = 1;
    mana = 1;
    damage_taken_modifier = 0.0;
    damage_modifier = 0.0;
    movement_speed = 0.0;
    movement_speed_modifier = 0.0;
    attack_speed = 0.0;
    attack_power = 0.0;
    ability_power = 0.0;
    critical_chance = 0.0;
    critical_damage = 0.0;
    cooldown_reduction = 0.0;
    dodge_chance = 0.0;
    mana_regen = 0.0;
    health_regen = 0.0;
    experience_modifier = 0.0;
    gold_modifier = 0.0;
    life_steal = 0.0
  }
;;

let minimal_character =
  { Data.Character_t.metadata = minimal_metadata;
    character_type = `Adventurer;
    variant = None;
    pivot_offset = minimal_vector3;
    vision_range = 1.0;
    auto_attack = minimal_entity_reference;
    skills = [];
    stat_sheet = minimal_stat_sheet;
    drop_table = minimal_entity_reference;
    foot_step_sound = minimal_entity_reference;
    hit_sound = minimal_entity_reference
  }
;;

let minimal_character_entity_definition : Data.Entity_t.entity_definition_internal =
  `Character
    { Data.Entity_t.owner = "ownr";
      entity_type = `Character;
      key = "MinimalCharacter";
      version = 1;
      id = "ownr:Character:MinimalCharacter:1";
      entity = minimal_character
    }
;;

(* Minimal valid model *)
let minimal_transform =
  { Data.Common_t.position = { x = 0.0; y = 0.0; z = 0.0 };
    rotation = { x = 0.0; y = 0.0; z = 0.0 };
    scale = { x = 1.0; y = 1.0; z = 1.0 }
  }
;;

let minimal_model =
  { Data.Model_t.path = "model/minimal.glb";
    transform = minimal_transform;
    shadow_casting_mode = `Off
  }
;;

let minimal_model_entity_definition : Data.Entity_t.entity_definition_internal =
  `Model
    { Data.Entity_t.owner = "ownr";
      entity_type = `Model;
      key = "MinimalModel";
      version = 1;
      id = "ownr:Model:MinimalModel:1";
      entity = minimal_model
    }
;;

(* Minimal valid image *)
let minimal_size = { Data.Common_t.width = 1; height = 1 }

let minimal_image =
  { Data.Image_t.image_type = `Texture; path = "image/minimal.png"; size = minimal_size }
;;

let minimal_image_entity_definition : Data.Entity_t.entity_definition_internal =
  `Image
    { Data.Entity_t.owner = "ownr";
      entity_type = `Image;
      key = "MinimalImage";
      version = 1;
      id = "ownr:Image:MinimalImage:1";
      entity = minimal_image
    }
;;

(* Minimal valid cursor *)
let minimal_vector2 = { Data.Common_t.x = 0.0; y = 0.0 }

let minimal_cursor =
  { Data.Cursor_t.cursor_type = `General;
    icon_reference = minimal_entity_reference;
    hot_spot = minimal_vector2
  }
;;

let minimal_cursor_entity_definition : Data.Entity_t.entity_definition_internal =
  `Cursor
    { Data.Entity_t.owner = "ownr";
      entity_type = `Cursor;
      key = "MinimalCursor";
      version = 1;
      id = "ownr:Cursor:MinimalCursor:1";
      entity = minimal_cursor
    }
;;

(* Minimal valid stat *)
let minimal_stat =
  { Data.Stat_t.metadata = minimal_metadata; stat_type = `Vit; color = minimal_color }
;;

let minimal_stat_entity_definition : Data.Entity_t.entity_definition_internal =
  `Stat
    { Data.Entity_t.owner = "ownr";
      entity_type = `Stat;
      key = "MinimalStat";
      version = 1;
      id = "ownr:Stat:MinimalStat:1";
      entity = minimal_stat
    }
;;

(* Minimal valid quality *)
let minimal_quality =
  { Data.Quality_t.metadata = minimal_metadata;
    quality_type = `Common;
    color = minimal_color;
    text_color = minimal_color;
    text_over_color = minimal_color
  }
;;

let minimal_quality_entity_definition : Data.Entity_t.entity_definition_internal =
  `Quality
    { Data.Entity_t.owner = "ownr";
      entity_type = `Quality;
      key = "MinimalQuality";
      version = 1;
      id = "ownr:Quality:MinimalQuality:1";
      entity = minimal_quality
    }
;;

(* Minimal valid audio_clip *)
let minimal_audio_clip =
  { Data.Audio_t.path = "audio/minimal.mp3"; relative_volume = 1.0 }
;;

let minimal_audio_clip_entity_definition : Data.Entity_t.entity_definition_internal =
  `AudioClip
    { Data.Entity_t.owner = "ownr";
      entity_type = `AudioClip;
      key = "MinimalAudioClip";
      version = 1;
      id = "ownr:AudioClip:MinimalAudioClip:1";
      entity = minimal_audio_clip
    }
;;

(* Minimal valid sound *)
let minimal_sound =
  { Data.Audio_t.audio_references = [ minimal_entity_reference ];
    spatial_blend = 0.0;
    volume = 1.0
  }
;;

let minimal_sound_entity_definition : Data.Entity_t.entity_definition_internal =
  `Sound
    { Data.Entity_t.owner = "ownr";
      entity_type = `Sound;
      key = "MinimalSound";
      version = 1;
      id = "ownr:Sound:MinimalSound:1";
      entity = minimal_sound
    }
;;

(* Minimal valid sound_bank *)
let minimal_sound_bank =
  { Data.Audio_t.hit_miss = minimal_entity_reference;
    item_received = minimal_entity_reference;
    gold_received = minimal_entity_reference;
    level_up = minimal_entity_reference;
    menu_open = minimal_entity_reference;
    menu_close = minimal_entity_reference;
    equipped = minimal_entity_reference;
    message = minimal_entity_reference
  }
;;

let minimal_sound_bank_entity_definition : Data.Entity_t.entity_definition_internal =
  `SoundBank
    { Data.Entity_t.owner = "ownr";
      entity_type = `SoundBank;
      key = "MinimalSoundBank";
      version = 1;
      id = "ownr:SoundBank:MinimalSoundBank:1";
      entity = minimal_sound_bank
    }
;;

(* Minimal valid drop_table *)
let minimal_drop_internal : Data.Drop_t.drop_internal =
  `Gold { Data.Drop_t.drop_type = `Gold; weight = 1; amount = minimal_int_range }
;;

let minimal_drop_table =
  { Data.Drop_t.metadata = minimal_metadata;
    gold_drops = [ minimal_drop_internal ];
    equipment_drops = [];
    weapon_drops = [];
    skill_drops = []
  }
;;

let minimal_drop_table_entity_definition : Data.Entity_t.entity_definition_internal =
  `DropTable
    { Data.Entity_t.owner = "ownr";
      entity_type = `DropTable;
      key = "MinimalDropTable";
      version = 1;
      id = "ownr:DropTable:MinimalDropTable:1";
      entity = minimal_drop_table
    }
;;

(* Minimal valid animation_source_internal *)
let minimal_generic_animation_source =
  { Data.Animation_t.requires_sheath_weapon = false;
    original_duration = 0.0;
    looping = false;
    animation_type = `Generic;
    generic_animation_type = `Attack;
    variant = 0
  }
;;

let minimal_animation_source_internal = `Generic minimal_generic_animation_source

let minimal_animation_source_entity_definition : Data.Entity_t.entity_definition_internal =
  `AnimationSource
    { Data.Entity_t.owner = "ownr";
      entity_type = `AnimationSource;
      key = "MinimalAnimationSource";
      version = 1;
      id = "ownr:AnimationSource:MinimalAnimationSource:1";
      entity = minimal_animation_source_internal
    }
;;

(* Minimal valid animation *)
let minimal_animation =
  { Data.Animation_t.metadata = minimal_metadata; duration = 0.0; sources = [] }
;;

let minimal_animation_entity_definition : Data.Entity_t.entity_definition_internal =
  `Animation
    { Data.Entity_t.owner = "ownr";
      entity_type = `Animation;
      key = "MinimalAnimation";
      version = 1;
      id = "ownr:Animation:MinimalAnimation:1";
      entity = minimal_animation
    }
;;

(* Minimal valid projectile *)
let minimal_projectile_homing =
  { Data.Projectile_t.metadata = minimal_metadata;
    locomotion_type = `Homing;
    model_reference = minimal_entity_reference;
    lifetime = 5.0;
    spawn_offset = minimal_vector3;
    max_hit_count = 1;
    speed = 10.0;
    rotation_speed = 180.0;
    on_hit = None;
    on_status = None
  }
;;

let minimal_projectile_internal : Data.Projectile_t.projectile_internal =
  `Homing minimal_projectile_homing
;;

let minimal_projectile_entity_definition : Data.Entity_t.entity_definition_internal =
  `Projectile
    { Data.Entity_t.owner = "ownr";
      entity_type = `Projectile;
      key = "MinimalProjectile";
      version = 1;
      id = "ownr:Projectile:MinimalProjectile:1";
      entity = minimal_projectile_internal
    }
;;

(* Add similar minimal entity_definition_internal values for other entity types as needed for parity tests *)

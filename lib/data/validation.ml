open Common_t

let int_min min (x : int) = min <= x
let int_max max x = (x : int) <= max
let int_between min max (x : int) = int_min min x && int_max max x
let float_min min (x : float) = min <= x
let float_max max (x : float) = x <= max
let float_between min max (x : float) = float_min min x && float_max max x
let string_starts_with prefix s = String.starts_with ~prefix s
let string_ends_with suffix s = String.ends_with ~suffix s
let string_max_length max s = String.length s <= max
let string_min_length min s = String.length s >= min
let string_length_between min max s = string_min_length min s && string_max_length max s

let vector2_between min max { x; y } =
  float_between min.x max.x x && float_between min.y max.y y
;;

let vector3_between (min : vector3) (max : vector3) ({ x; y; z } : vector3) =
  float_between min.x max.x x
  && float_between min.y max.y y
  && float_between min.z max.z z
;;

let size_between
  { width = min_width; height = min_height }
  { width = max_width; height = max_height }
  { width; height }
  =
  int_between min_width max_width width && int_between min_height max_height height
;;

let data_file_exist_starting_with prefix path =
  string_starts_with prefix path
  && (Sys.file_exists @@ Filename.concat Config.Constants.media_folder path)
;;

let validate_file prefix extension path =
  data_file_exist_starting_with prefix path && string_ends_with extension path
;;

let float_range_between { min; max } { min = r_min; max = r_max } =
  min <= r_min && r_max <= max && r_min <= r_max
;;

let int_range_between
  ({ min; max } : int_range)
  ({ min = r_min; max = r_max } : int_range)
  =
  min <= r_min && r_max <= max && r_min <= r_max
;;

let list_min_length min l = List.length l >= min

(* Node ids name tree nodes in network records and must be unique within a quest *)
let rec collect_quest_node_ids (node : Quest_t.quest_node_internal) =
  match node with
  | `Sequence { id; children; _ } | `Parallel { id; children; _ } | `Any { id; children; _ }
    -> id :: List.concat_map collect_quest_node_ids children
  | `Timer { id; child; _ } -> id :: collect_quest_node_ids child
  | `Objective { id; _ } | `Action { id; _ } -> [ id ]
;;

let validate_quest_node_ids (quest : Quest_t.quest) =
  let ids = collect_quest_node_ids quest.root in
  List.length ids = List.length (List.sort_uniq compare ids)
;;

(* Optional means "not needed for the Parallel above to complete", so a node may be optional only as a direct child
   of a Parallel (never the root, never under a Sequence, Any or Timer), and a bonus achievement needs an optional
   node to hang on. [optional] on an Objective is its [is_optional]. *)
let quest_node_flags (node : Quest_t.quest_node_internal) =
  match node with
  | `Sequence { optional; bonus_achievement; _ }
  | `Parallel { optional; bonus_achievement; _ }
  | `Any { optional; bonus_achievement; _ }
  | `Timer { optional; bonus_achievement; _ } -> optional = Some true, bonus_achievement <> None
  | `Objective { is_optional; bonus_achievement; _ } -> is_optional, bonus_achievement <> None
  | `Action _ -> false, false
;;

let rec quest_optional_placement_ok ~in_parallel (node : Quest_t.quest_node_internal) =
  let optional, bonus = quest_node_flags node in
  ((not optional) || in_parallel)
  && ((not bonus) || optional)
  &&
  match node with
  | `Parallel { children; _ } ->
    List.for_all (quest_optional_placement_ok ~in_parallel:true) children
  | `Sequence { children; _ } | `Any { children; _ } ->
    List.for_all (quest_optional_placement_ok ~in_parallel:false) children
  | `Timer { child; _ } -> quest_optional_placement_ok ~in_parallel:false child
  | `Objective _ | `Action _ -> true
;;

let validate_quest (quest : Quest_t.quest) =
  validate_quest_node_ids quest && quest_optional_placement_ok ~in_parallel:false quest.root
;;

(* A controlled summon is steered for its lifetime, so it needs one *)
let validate_summon_control (node : Skill_t.skill_action_summon_node) =
  match node.controlled with
  | Some true -> Option.is_some node.lifetime
  | Some false | None -> true
;;

(* A dash needs a length and a duration; the instant moves take none *)
let validate_move_node (node : Skill_t.skill_action_move_node) =
  match node.mode with
  | `Dash -> node.distance > 0. && node.duration > 0.
  | `Blink | `ShadowStep -> node.distance > 0. && node.duration = 0.
  | `Swap -> node.distance = 0. && node.duration = 0.
;;

(* A skillshot must be able to cover its range within its lifetime *)
let validate_projectile_straight (p : Projectile_t.projectile_straight) =
  p.max_range /. p.speed <= p.lifetime +. 1e-9
;;

(* A lob must land before its lifetime runs out *)
let validate_projectile_arc (p : Projectile_t.projectile_arc) = p.flight_time <= p.lifetime

(* A beam ticks at least once while it lasts *)
let validate_projectile_beam (p : Projectile_t.projectile_beam) = p.tick_interval <= p.lifetime

let validate_charges = function
  | Some n -> n >= 1
  | None -> true
;;

(* The status of an interrupt lands on the interrupted character: the Selected one, never a dispel *)
let validate_interrupt_node (n : Skill_t.skill_action_interrupt_node) =
  match n.on_interrupt with
  | None -> true
  | Some { target_mechanic = `Selected _; dispel; _ } -> dispel <> Some true
  | Some _ -> false
;;

(* An execute bonus: below a fraction of the health in (0, 1], a factor of at least 1 *)
let validate_execute_bonus (b : Effect_t.execute_bonus) = b.below > 0. && b.below <= 1. && b.factor >= 1.

(* A revive stands up the selected dead ally: one scaler whose base is the percent (above 0, at most 100) of its
   maximum health and mana it returns with. Only a Damage hit executes, knocks back or pulls, a knockback or a pull
   is a positive distance and a hit has not both *)
let validate_hit_effect (effect : Effect_t.hit_effect) =
  let damage_only = effect.execute = None && effect.knockback = None && effect.pull = None in
  match effect.hit_type with
  | `Revive ->
    effect.school = None
    && damage_only
    &&
    (match effect.target_mechanic, effect.target, effect.scalers with
     | `Selected _, `Ally, [ { base; _ } ] -> base > 0. && base <= 100.
     | _ -> false)
  | `Damage ->
    let positive = Option.fold ~none:true ~some:(fun d -> d > 0.) in
    positive effect.knockback && positive effect.pull && (effect.knockback = None || effect.pull = None)
  | `Heal -> damage_only
  | `Threat | `Mana -> effect.school = None && damage_only
;;

(* A dispel removes a status; there is nothing to scale or time *)
let validate_status_effect (effect : Effect_t.status_effect) =
  match effect.dispel with
  | Some true -> effect.scalers = [] && effect.durations = []
  | Some false | None -> true
;;

(* An enrage applies its effects to the monster itself: at least one, each applying (not dispelling) a
   status to Self *)
let validate_enrage_effects (effects : Effect_t.status_effect list) =
  effects <> []
  && List.for_all
       (fun (e : Effect_t.status_effect) ->
         e.dispel <> Some true
         &&
         match e.target_mechanic with
         | `Self _ -> true
         | _ -> false)
       effects
;;

(* A burst threshold is a fraction of the source's maximum health, in (0, 1] *)
let validate_break_burst (x : float option) =
  Option.fold ~none:true ~some:(fun f -> Float.is_finite f && f > 0. && f <= 1.) x
;;

(* Per-level growth of a skill's power: never negative (a level never weakens) *)
let validate_growth (x : float option) = Option.fold ~none:true ~some:(float_min 0.) x
let validate_cooldown_per_level (x : float option) =
  Option.fold ~none:true ~some:(float_between 0. 0.1) x
;;

let validate_max_level (x : int option) = Option.fold ~none:true ~some:(int_min 1) x

(* A trigger effect does exactly one thing *)
let validate_trigger_effect (effect : Trigger_t.trigger_effect) =
  List.length
    (List.filter
       Fun.id
       [ Option.is_some effect.hit
       ; Option.is_some effect.status
       ; Option.is_some effect.skill
       ])
  = 1
;;

let trigger_key_regex = Re.Perl.compile_pat "^[a-zA-Z0-9_-]{3,64}$"

(* Probability and cooldown in range, a key, at least one effect; the status filter only makes sense for StatusApplied *)
let validate_trigger (trigger : Trigger_t.trigger) =
  Re.execp trigger_key_regex trigger.key
  && float_between 0. 1. trigger.chance
  && float_min 0. trigger.cooldown
  && Option.fold ~none:true ~some:(float_min 0.) trigger.min_amount
  && Option.fold ~none:true ~some:(int_min 0) trigger.max_chain
  && trigger.effects <> []
  && (Option.is_none trigger.status || trigger.on = `StatusApplied)
;;

(* The keys of the triggers of one owner are unique *)
let validate_triggers (triggers : Trigger_t.trigger list option) =
  match triggers with
  | None -> true
  | Some triggers ->
    let keys = List.map (fun (t : Trigger_t.trigger) -> t.key) triggers in
    List.length keys = List.length (List.sort_uniq compare keys)
;;

(* Steps must be non-empty, strictly ordered by timing, and end at timing 1.0 *)
let validate_spawn_sequence_steps (steps : Spawn_t.spawn_sequence_step list) =
  let rec strictly_increasing = function
    | ({ Spawn_t.timing = a; _ } : Spawn_t.spawn_sequence_step)
      :: ({ Spawn_t.timing = b; _ } as second) :: tl ->
      a < b && strictly_increasing (second :: tl)
    | _ -> true
  in
  let last_is_one steps =
    match List.rev steps with
    | ({ Spawn_t.timing; _ } : Spawn_t.spawn_sequence_step) :: _ -> timing = 1.0
    | [] -> false
  in
  list_min_length 1 steps && strictly_increasing steps && last_is_one steps
;;
let common_regex = Re.Perl.compile_pat "^[a-zA-Z0-9_]{3,64}$"

let create_id_from ~owner ~entity_type ~key ~version =
  String.concat ":" [ owner; entity_type; key; string_of_int version ]
;;

let validate_entity_definition ~owner ~entity_type ~key ~version ~id =
  Re.execp common_regex owner
  && Re.execp common_regex key
  && int_min 1 version
  && create_id_from
       ~owner
       ~entity_type:(Entity_util.string_of_entity_type entity_type)
       ~key
       ~version
     = id
;;

let validate_entity_reference { entity_type; key; version; owner; id } =
  Re.execp common_regex owner
  && Re.execp common_regex key
  && int_min 1 version
  && create_id_from
       ~owner
       ~entity_type:(Entity_util.string_of_entity_type entity_type)
       ~key
       ~version
     = id
;;

let entity_reference_of_type (expected : Common_t.entity_type) { entity_type; _ } =
  entity_type = expected
;;

let entity_reference_of_type_if_some
  (expected : Common_t.entity_type)
  (entity_reference : entity_reference option)
  =
  match entity_reference with
  | None -> true
  | Some entity_reference -> entity_reference_of_type expected entity_reference
;;

let entity_reference_list_of_type
  min_length
  (expected : Common_t.entity_type)
  (entity_references : entity_reference list)
  =
  list_min_length min_length entity_references
  && List.for_all (entity_reference_of_type expected) entity_references
;;

let entity_reference_list_of_type_if_some
  (expected : Common_t.entity_type)
  (entity_references : entity_reference list option)
  =
  match entity_references with
  | None -> true
  | Some l -> List.for_all (entity_reference_of_type expected) l
;;

let validate_entity_definition_internal
  (entity_definition : Entity_t.entity_definition_internal)
  =
  match entity_definition with
  | `Weapon { owner; id; entity_type; key; version; _ }
  | `Skill { owner; id; entity_type; key; version; _ }
  | `Equipment { owner; id; entity_type; key; version; _ }
  | `Status { owner; id; entity_type; key; version; _ }
  | `Model { owner; id; entity_type; key; version; _ }
  | `AudioClip { owner; id; entity_type; key; version; _ }
  | `Sound { owner; id; entity_type; key; version; _ }
  | `Image { owner; id; entity_type; key; version; _ }
  | `SoundBank { owner; id; entity_type; key; version; _ }
  | `DropTable { owner; id; entity_type; key; version; _ }
  | `Character { owner; id; entity_type; key; version; _ }
  | `AnimationSource { owner; id; entity_type; key; version; _ }
  | `Animation { owner; id; entity_type; key; version; _ }
  | `Projectile { owner; id; entity_type; key; version; _ }
  | `Quest { owner; id; entity_type; key; version; _ }
  | `Anchor { owner; id; entity_type; key; version; _ }
  | `Stage { owner; id; entity_type; key; version; _ }
  | `SkillStone { owner; id; entity_type; key; version; _ }
  | `WeaponClass { owner; id; entity_type; key; version; _ }
  | `RoomPrefab { owner; id; entity_type; key; version; _ }
  | `Achievement { owner; id; entity_type; key; version; _ }
  | `Balance { owner; id; entity_type; key; version; _ } ->
    validate_entity_definition ~owner ~entity_type ~key ~version ~id
  | `QuestDifficulty { owner; id; entity_type; key; version; entity } ->
    validate_entity_definition ~owner ~entity_type ~key ~version ~id
    && key = Entity_util.string_of_quest_difficulty_type entity.difficulty_type
  | `Cursor { owner; id; entity_type; key; version; entity } ->
    validate_entity_definition ~owner ~entity_type ~key ~version ~id
    && key = Entity_util.string_of_cursor_type entity.cursor_type
  | `Stat { owner; id; entity_type; key; version; entity } ->
    validate_entity_definition ~owner ~entity_type ~key ~version ~id
    && key = Entity_util.string_of_stat_type entity.stat_type
  | `Quality { owner; id; entity_type; key; version; entity } ->
    validate_entity_definition ~owner ~entity_type ~key ~version ~id
    && key = Entity_util.string_of_quality_type entity.quality_type
;;

let drop_of_type (expected : Drop_t.drop_type) (drop : Drop_t.drop_internal) =
  match (drop, expected) with
  | `Gold _, `Gold -> true
  | `Equipment _, `Equipment -> true
  | `Weapon _, `Weapon -> true
  | `Skill _, `Skill -> true
  | `SkillStone _, `SkillStone -> true
  | _ -> false
;;

let drop_list_of_type
  min_length
  (expected : Drop_t.drop_type)
  (drops : Drop_t.drop_internal list)
  =
  list_min_length min_length drops && List.for_all (drop_of_type expected) drops
;;

let float_min_if_some min (x : float option) =
  match x with
  | None -> true
  | Some x -> Float.is_finite x && float_min min x
;;

(* An effect key (vfx.atd): lowercase words, digits and "-" in groups joined by "/". *)
let vfx_key (k : string) =
  let word_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-' in
  k <> ""
  && String.split_on_char '/' k
     |> List.for_all (fun g -> g <> "" && String.for_all word_char g)
;;

let vfx_key_if_some (k : string option) =
  match k with
  | None -> true
  | Some k -> vfx_key k
;;

let int_min_if_some min (x : int option) =
  match x with
  | None -> true
  | Some x -> int_min min x
;;

(* A drop gate asks for something: a level from 1, a tier from 1 (tier 0 is every quest), and a rank of
   Elite or above (Normal is the default and would be a no-op); the lower-rank weight needs a rank to be
   below and is at least 1 *)
let validate_drop_gate (gate : Drop_t.drop_gate) =
  (match gate.min_level with
   | None -> true
   | Some l -> int_min 1 l)
  && int_min_if_some 1 gate.min_tier
  && (match gate.min_rank with
      | None | Some `Elite | Some `Boss -> true
      | Some `Normal -> false)
  &&
  match gate.lower_rank_weight with
  | None -> true
  | Some w -> int_min 1 w && Option.is_some gate.min_rank
;;

(* The rarity curve needs at least one tier to roll *)
let validate_rarity_weights (w : Drop_t.rarity_weights) =
  w.common + w.uncommon + w.rare + w.epic + w.legendary > 0
;;

(* A guarantee is about items *)
let validate_guarantee_kind (kind : Drop_t.drop_type) =
  match kind with
  | `Equipment | `Weapon | `SkillStone -> true
  | `Gold | `Skill -> false
;;

(* Quality None marks a bare item: it is never a rarity to demand *)
let validate_guarantee_quality (quality : Quality_t.quality_type) =
  match quality with
  | `None -> false
  | `Common | `Uncommon | `Rare | `Epic | `Legendary -> true
;;

let validate_guarantee_quality_if_some (quality : Quality_t.quality_type option) =
  match quality with
  | None -> true
  | Some q -> validate_guarantee_quality q
;;

(* A circle needs a radius; a box needs both half extents. Coordinates must be finite. *)
let validate_obstacle (o : Stage_t.obstacle) =
  let pos = function Some x -> Float.is_finite x && x > 0. | None -> false in
  Float.is_finite o.x && Float.is_finite o.z
  && Option.fold ~none:true ~some:Float.is_finite o.yaw
  &&
  match o.shape with
  | `Circle -> pos o.radius
  | `Box -> pos o.half_x && pos o.half_z

let validate_obstacles (obstacles : Stage_t.obstacle list option) =
  Option.fold ~none:true ~some:(List.for_all validate_obstacle) obstacles

(* Same shape rule as an obstacle: a circle needs a radius, a box both half extents *)
let validate_ground_zone (z : Stage_t.ground_zone) =
  let pos = function Some x -> Float.is_finite x && x > 0. | None -> false in
  Float.is_finite z.x && Float.is_finite z.z
  && Option.fold ~none:true ~some:Float.is_finite z.yaw
  &&
  match z.shape with
  | `Circle -> pos z.radius
  | `Box -> pos z.half_x && pos z.half_z

let validate_ground_zones (zones : Stage_t.ground_zone list option) =
  Option.fold ~none:true ~some:(List.for_all validate_ground_zone) zones


(* The id of a layer of a character's look: lower case letters, digits, '_' and '.', starting with a letter *)
let validate_layer_id (id : string) =
  String.length id > 0
  && String.length id <= 64
  && (match id.[0] with 'a' .. 'z' -> true | _ -> false)
  && String.for_all (function 'a' .. 'z' | '0' .. '9' | '_' | '.' -> true | _ -> false) id
  && not (String.starts_with ~prefix:"." id || String.ends_with ~suffix:"." id)
;;

let validate_layer_id_if_some (id : string option) =
  Option.fold ~none:true ~some:validate_layer_id id
;;

(* The id of a model family (the key of its model in a pack): a layer id that may also hold '-' between words
   (the low-poly animal families are named lp-rat, lp-wolf, ...) *)
let validate_model_family (id : string) =
  validate_layer_id (String.map (function '-' -> '_' | c -> c) id)
  && not (String.starts_with ~prefix:"-" id || String.ends_with ~suffix:"-" id)
;;

let validate_model_family_if_some (id : string option) =
  Option.fold ~none:true ~some:validate_model_family id
;;

(* A prop is named by kind (lamp), model key (NTree3) or model id (Octoio:Model:NTree3:1) *)
let validate_prop_name (name : string) =
  String.length name > 0
  && String.length name <= 96
  && String.for_all
       (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | ':' -> true | _ -> false)
       name
;;

let validate_prop_name_if_some (name : string option) =
  Option.fold ~none:true ~some:validate_prop_name name
;;

(* A zone's model keys: at least one, each a prop name *)
let validate_model_keys_if_some (keys : string list option) =
  Option.fold ~none:true ~some:(fun l -> l <> [] && List.for_all validate_prop_name l) keys
;;

let validate_prop_scale (scale : float option) =
  Option.fold ~none:true ~some:(fun s -> Float.is_finite s && s >= 0.1 && s <= 10.) scale
;;

(* Scenery stands on finite ground coordinates *)
let validate_stage_props (props : Stage_t.stage_prop list option) =
  Option.fold
    ~none:true
    ~some:
      (List.for_all (fun (p : Stage_t.stage_prop) ->
         Float.is_finite p.x
         && Float.is_finite p.z
         && Option.fold ~none:true ~some:Float.is_finite p.yaw))
    props
;;

(* A class lists at least one category, none twice *)
let validate_weapon_class_categories (categories : Weapon_t.weapon_category list) =
  categories <> [] && List.length categories = List.length (List.sort_uniq compare categories)
;;

(* The cue a slot plays: an attack cue for the basic attack, the locomotion cues for idle, walk and run, the
   hit and death cues for the rest *)
let animation_slot_takes_cue (slot : Weapon_class_t.animation_slot) (cue : Animation_t.animation_cue)
  =
  match slot, cue with
  | `Idle, `Idle | `Walk, `Walk | `Run, `Run | `Death, `Death -> true
  | `Hit, (`Hit | `HitBlocked) -> true
  | ( `BasicAttack,
      ( `AttackGeneric | `AttackUnarmed | `AttackStab | `AttackSlash | `AttackThrust
      | `AttackCrush | `AttackWand | `AttackShield | `AttackDual | `AttackTwoHandSlash
      | `AttackTwoHandChop | `AttackTwoHandThrust | `AttackTwoHandStaff | `AttackShoot ) )
    -> true
  | _ -> false
;;

(* One entry per slot, each with a cue that fits its slot *)
let validate_animation_set (entries : Weapon_class_t.animation_set_entry list) =
  let slots = List.map (fun (e : Weapon_class_t.animation_set_entry) -> e.slot) entries in
  List.length slots = List.length (List.sort_uniq compare slots)
  && List.for_all
       (fun (e : Weapon_class_t.animation_set_entry) -> animation_slot_takes_cue e.slot e.cue)
       entries
;;

(* `crypt.hollow_nest`: lower case words joined by dots and underscores *)
let prefab_id_regex = Re.Perl.compile_pat "^[a-z0-9]+(\\.[a-z0-9_]+)+$"
let validate_prefab_id (id : string) = Re.execp prefab_id_regex id

(* Achievements: a condition names a ledger metric (a lower case snake id; the Rust side checks that it exists in
   fey_core::ledger::METRICS) and optionally a subject, by entity or by tag, never both. *)
let achievement_metric_key_regex = Re.Perl.compile_pat "^[a-z][a-z0-9_]{1,47}$"
let achievement_metric_key (k : string) = Re.execp achievement_metric_key_regex k
let achievement_subject_tag_regex = Re.Perl.compile_pat "^[A-Za-z0-9_@]{1,64}$"
let achievement_subject_tag (t : string) = Re.execp achievement_subject_tag_regex t

let achievement_subject_tag_if_some (t : string option) =
  Option.fold ~none:true ~some:achievement_subject_tag t
;;

let rec achievement_condition_ok (c : Achievement_t.achievement_condition_internal) =
  match c with
  | `Stat { subject; subject_tag; _ } -> subject = None || subject_tag = None
  | `All { children; _ } | `Any { children; _ } ->
    List.for_all achievement_condition_ok children
;;

(* An unlock's target is an entity of the pool its kind fills. *)
let achievement_unlock_ok (u : Achievement_t.unlock) =
  let target_is types = List.mem u.target.entity_type types in
  match u.kind with
  | `ItemPool -> target_is [ `Equipment; `Weapon ]
  | `WeaponClass -> target_is [ `WeaponClass ]
  | `SkillPool -> target_is [ `Skill ]
;;

let validate_achievement (a : Achievement_t.achievement_def) =
  achievement_condition_ok a.condition && List.for_all achievement_unlock_ok a.rewards
;;

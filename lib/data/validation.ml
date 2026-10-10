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
  && (Sys.file_exists @@ Filename.concat Config.Constants.data_folder path)
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

(* A revive stands up the selected dead ally: one scaler whose base is the percent (above 0, at most 100) of its
   maximum health and mana it returns with *)
let validate_hit_effect (effect : Effect_t.hit_effect) =
  match effect.hit_type with
  | `Revive ->
    (match effect.target_mechanic, effect.target, effect.scalers with
     | `Selected _, `Ally, [ { base; _ } ] -> base > 0. && base <= 100.
     | _ -> false)
  | `Damage | `Heal | `Threat | `Mana -> true
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

(* Per-level growth of a stat or of a skill's power: never negative (a level never weakens) *)
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
  | `SkillStone { owner; id; entity_type; key; version; _ } ->
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

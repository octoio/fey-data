type dataset_error =
  { origin_location : string;
    file_path : string;
    error : Atdgen_runtime.Util.Validation.error option
  }

type dataset =
  { entity_index_container : Data.Entity_t.entity_index_container;
    definitions : (string * Data.Entity_t.entity_definition_internal) list;
    errors : dataset_error list
  }
[@@deriving fields]

let add_error origin_location file_path error dataset =
  match error with
  | None -> dataset
  | Some _ ->
    { dataset with errors = { origin_location; file_path; error } :: dataset.errors }
;;

let add_definition (file_path, definition) dataset =
  let truncated_file_path = Util.String.remove_before_prefix Config.Constants.json_sub_folder file_path in
  { dataset with definitions = (truncated_file_path, definition) :: dataset.definitions }
;;

let entity_reference_of_entity_definition
  (entity_definition : Data.Entity_t.entity_definition_internal)
  : Data.Common_t.entity_reference
  =
  match entity_definition with
  | `Weapon { id; owner; entity_type; key; version; _ }
  | `Skill { id; owner; entity_type; key; version; _ }
  | `Equipment { id; owner; entity_type; key; version; _ }
  | `Status { id; owner; entity_type; key; version; _ }
  | `Model { id; owner; entity_type; key; version; _ }
  | `Image { id; owner; entity_type; key; version; _ }
  | `Cursor { id; owner; entity_type; key; version; _ }
  | `Stat { id; owner; entity_type; key; version; _ }
  | `Quality { id; owner; entity_type; key; version; _ }
  | `AudioClip { id; owner; entity_type; key; version; _ }
  | `Sound { id; owner; entity_type; key; version; _ }
  | `SoundBank { id; owner; entity_type; key; version; _ }
  | `DropTable { id; owner; entity_type; key; version; _ }
  | `Character { id; owner; entity_type; key; version; _ }
  | `AnimationSource { id; owner; entity_type; key; version; _ }
  | `Animation { id; owner; entity_type; key; version; _ }
  | `Projectile { id; owner; entity_type; key; version; _ } ->
    { id; owner; entity_type; key; version }
;;

let take_definition (_, entity_definition) = entity_definition
let take_definitions dataset = List.map take_definition dataset.definitions

let extract_entity_reference_definition dataset : Data.Common_t.entity_reference list =
  List.map entity_reference_of_entity_definition @@ take_definitions dataset
;;

let extract_entity_reference_from_skill_entity
  Data.Skill_t.{ icon_reference; indicators; execution_root; _ }
  =
  let rec extract_entity_reference_from_skill_action_node
    (node : Data.Skill_t.skill_action_node_internal)
    : Data.Common_t.entity_reference list
    =
    match node with
    | `Sequence { children; _ } | `Parallel { children; _ } ->
      List.fold_left
        (fun acc child -> acc @ extract_entity_reference_from_skill_action_node child)
        []
        children
    | `Requirement { child; _ } -> extract_entity_reference_from_skill_action_node child
    | `Animation { animations; _ } -> animations
    | `Sound { sound; _ } -> [ sound ]
    | `Hit { hit_effect; _ } -> [ hit_effect.hit_sound ]
    | `Delay _ -> []
    | `Status { status_effect; _ } -> [ status_effect.status ]
    | `Summon { summon_entity; _ } -> [ summon_entity ]
  in
  [ icon_reference ]
  @ List.map (fun (indicator : Data.Skill_t.skill_indicator) -> indicator.model_reference) indicators
  @ extract_entity_reference_from_skill_action_node execution_root.child
;;

let extract_entity_reference_from_sound_bank_entity
  Data.Audio_t.
    { hit_miss;
      item_received;
      gold_received;
      level_up;
      menu_open;
      menu_close;
      equipped;
      message
    }
  =
  [ hit_miss;
    item_received;
    gold_received;
    level_up;
    menu_open;
    menu_close;
    equipped;
    message
  ]
;;

let extract_entity_reference_from_drop_table_entity
  Data.Drop_t.{ skill_drops; equipment_drops; weapon_drops; _ }
  =
  let extract_entity_from_drop acc (drop : Data.Drop_t.drop_internal) =
    match drop with
    | `Gold _ -> acc
    | `Equipment { equipment; _ } -> equipment :: acc
    | `Weapon { weapon; _ } -> weapon :: acc
    | `Skill { skill; _ } -> skill :: acc
  in
  let extract_entity_from_drops (drops : Data.Drop_t.drop_internal list) =
    List.fold_left extract_entity_from_drop [] drops
  in
  extract_entity_from_drops skill_drops
  @ extract_entity_from_drops equipment_drops
  @ extract_entity_from_drops weapon_drops
;;

let extract_from_option_entity_reference o = Option.fold ~none:[] ~some:(fun x -> [ x ]) o

let extract_entity_reference_from_projectile_entity
  (projectile : Data.Projectile_t.projectile_internal)
  =
  match projectile with
  | `Homing { model_reference; on_hit; on_status; _ } ->
    let hit_refs = match on_hit with
      | Some hit_effect -> [ hit_effect.hit_sound ]
      | None -> []
    in
    let status_refs = match on_status with
      | Some status_effect -> [ status_effect.status ]
      | None -> []
    in
    [ model_reference ] @ hit_refs @ status_refs
;;

let extract_entity_reference_from_entity_definition
  (entity_definition : Data.Entity_t.entity_definition_internal)
  =
  match entity_definition with
  | `Weapon { entity; _ } ->
    [ entity.model_anchor_set.model_reference; entity.icon_reference ]
    @ extract_from_option_entity_reference entity.basic_attack
  | `Skill { entity; _ } -> extract_entity_reference_from_skill_entity entity
  | `Equipment { entity; _ } -> [ entity.icon_reference ]
  | `Cursor { entity; _ } -> [ entity.icon_reference ]
  | `Sound { entity; _ } -> entity.audio_references
  | `SoundBank { entity; _ } -> extract_entity_reference_from_sound_bank_entity entity
  | `DropTable { entity; _ } -> extract_entity_reference_from_drop_table_entity entity
  | `Character { entity; _ } ->
    [ entity.hit_sound; entity.foot_step_sound; entity.auto_attack; entity.drop_table ]
    @ entity.skills
  | `Animation { entity; _ } -> entity.sources
  | `Projectile { entity; _ } -> extract_entity_reference_from_projectile_entity entity
  | `AnimationSource _
  | `AudioClip _
  | `Quality _
  | `Stat _
  | `Image _
  | `Status _
  | `Model _ -> []
;;

let extract_entity_reference dataset : Data.Common_t.entity_reference list =
  List.map extract_entity_reference_from_entity_definition @@ take_definitions dataset
  |> List.flatten
;;

let contains_error dataset =
  List.exists (fun { error; _ } -> error <> None) dataset.errors
;;

let generate_indices dataset : dataset =
  let open Digestif in
  let generate_index
    ((file_path, entity_definition) : string * Data.Entity_t.entity_definition_internal)
    =
    let hash =
      SHA1.digest_string (Data.Entity_j.string_of_entity_definition_internal entity_definition)
      |> SHA1.to_hex
    in
    (* TODO: Move back to linear indices, this is for hotreload only when a new entity is added *)
    let index_string =
      SHA1.digest_string file_path |> SHA1.to_raw_string |> Base.Bytes.of_string
    in
    let index =
      Base.Bytes.unsafe_get_int64 index_string 0
      |> Base.Int64.to_int32_trunc
      |> Base.Int32.to_int_exn
      |> Base.abs
    in
    Data.Entity_t.
      { index;
        reference = entity_reference_of_entity_definition entity_definition;
        file_path;
        hash
      }
  in
  let rec handle_collisions o (indices : Data.Entity_t.entity_index list) =
    match indices with
    | [] -> []
    | first :: second :: tl ->
      let offset = if first.index >= second.index then o + 1 else o in
      first
      :: handle_collisions offset ({ second with index = second.index + offset } :: tl)
    | tl -> tl
  in
  let indices =
    List.map generate_index dataset.definitions
    |> List.sort Data.Entity_t.(fun a b -> compare a.index b.index)
    |> handle_collisions 0
  in
  { dataset with entity_index_container = { indices } }
;;

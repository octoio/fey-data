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
  | `Animation { owner; id; entity_type; key; version; _ } ->
    validate_entity_definition ~owner ~entity_type ~key ~version ~id
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
  | _ -> false
;;

let drop_list_of_type
  min_length
  (expected : Drop_t.drop_type)
  (drops : Drop_t.drop_internal list)
  =
  list_min_length min_length drops && List.for_all (drop_of_type expected) drops
;;

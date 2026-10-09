open Dataset

let print_validation_error error =
  match error with
  | Some error -> Io.print_error @@ Atdgen_runtime.Util.Validation.string_of_error error
  | None -> ignore ()
;;

let file_pattern_invalid file_path dataset =
  add_error
    __LOC__
    file_path
    (Some
       (Atdgen_runtime.Util.Validation.error
          ~msg:
            ("Invalid file pattern: "
             ^ file_path
             ^ ". Expected: <entity>.<entity_type>.json")
          [ `Field "file_pattern" ]))
    dataset
;;

let error_while_processing_file file_path dataset e =
  add_error
    __LOC__
    file_path
    (Some
       (Atdgen_runtime.Util.Validation.error
          ~msg:("Error while processing file: " ^ file_path ^ ". " ^ Printexc.to_string e)
          [ `Field "file_processing" ]))
    dataset
;;

let unexpected_error_while_processing_directory dir dataset e =
  add_error
    __LOC__
    dir
    (Some
       (Atdgen_runtime.Util.Validation.error
          ~msg:("Unexpected error: " ^ Printexc.to_string e)
          [ `Field "unexpected" ]))
    dataset
;;

let validate_entity_definition file_path dataset =
  let json = Io.read_file file_path in
  let entity_definition = Data.Entity_j.entity_definition_internal_of_string json in
  let e = Data.Entity_v.validate_entity_definition_internal [] entity_definition in
  dataset
  |> add_definition (file_path, entity_definition)
  |> add_error __LOC__ file_path e
;;

(* Every anchor must be owned by exactly one stage: an orphan anchor is dead data,
   and two stages sharing an anchor would fight over its transform/lifecycle *)
let anchor_ownership_error_messages definitions =
  let anchor_references =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Anchor _ -> Some (Dataset.entity_reference_of_entity_definition definition)
        | _ -> None)
      definitions
  in
  let stage_anchor_references =
    List.concat_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Stage { entity; _ } -> entity.anchors
        | _ -> [])
      definitions
  in
  List.filter_map
    (fun anchor_reference ->
      let owner_count =
        List.length (List.filter (fun r -> r = anchor_reference) stage_anchor_references)
      in
      if owner_count = 1
      then None
      else
        Some
          (Printf.sprintf
             "Anchor must be owned by exactly one stage (owned by %d): %s"
             owner_count
             (Data.Common_j.string_of_entity_reference anchor_reference)))
    anchor_references
;;

(* A quest may only use anchors owned by its bound stage *)
let quest_anchor_subset_error_messages definitions =
  let stage_anchors_by_reference =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Stage { entity; _ } ->
          Some (Dataset.entity_reference_of_entity_definition definition, entity.anchors)
        | _ -> None)
      definitions
  in
  List.concat_map
    (fun (definition : Data.Entity_t.entity_definition_internal) ->
      match definition with
      | `Quest { entity; _ } ->
        let quest_reference = Dataset.entity_reference_of_entity_definition definition in
        let quest_anchors =
          Dataset.extract_entity_reference_from_quest_node entity.root
          |> List.filter (fun (r : Data.Common_t.entity_reference) ->
            r.entity_type = `Anchor)
        in
        (match List.assoc_opt entity.stage stage_anchors_by_reference with
         | None -> []
         (* missing stage definition is already reported by the generic reference check *)
         | Some stage_anchors ->
           List.filter_map
             (fun anchor ->
               if List.mem anchor stage_anchors
               then None
               else
                 Some
                   (Printf.sprintf
                      "Quest %s uses anchor %s that is not owned by its stage %s"
                      (Data.Common_j.string_of_entity_reference quest_reference)
                      (Data.Common_j.string_of_entity_reference anchor)
                      (Data.Common_j.string_of_entity_reference entity.stage)))
             quest_anchors)
      | _ -> [])
    definitions
;;

(* A KillSpecific objective is only completable if enough characters of the
   required types are spawned by actions that are not strictly after it in
   execution order. Sequence children run in order; Parallel/Any/Timer branches
   run concurrently, so every spawn inside the same concurrent subtree counts. *)
let quest_completability_error_messages definitions =
  let character_type_by_reference =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Character { entity; _ } ->
          Some
            ( Dataset.entity_reference_of_entity_definition definition,
              entity.Data.Character_t.character_type )
        | _ -> None)
      definitions
  in
  let spawn_counts (spawn : Data.Spawn_t.spawn) =
    match List.assoc_opt spawn.character character_type_by_reference with
    (* a missing character definition is reported by the generic reference check *)
    | None -> []
    | Some character_type -> [ character_type, spawn.spawn_count ]
  in
  let action_spawn_counts (action : Data.Quest_t.quest_action_internal) =
    match action with
    | `Spawn { spawn; _ } -> spawn_counts spawn
    | `StartSpawnSequence { spawn_sequence; _ } ->
      List.concat_map
        (fun (step : Data.Spawn_t.spawn_sequence_step) ->
          List.concat_map spawn_counts step.spawns)
        spawn_sequence.steps
    | _ -> []
  in
  let rec subtree_spawns (node : Data.Quest_t.quest_node_internal) =
    match node with
    | `Action { action; _ } -> action_spawn_counts action
    | `Sequence { children; _ } -> List.concat_map subtree_spawns children
    | `Parallel { children; _ } -> List.concat_map subtree_spawns children
    | `Any { children; _ } -> List.concat_map subtree_spawns children
    | `Timer { child; _ } -> subtree_spawns child
    | `Objective _ -> []
  in
  let available_count character_types available =
    List.fold_left
      (fun acc (character_type, count) ->
        if List.mem character_type character_types then acc + count else acc)
      0
      available
  in
  let rec walk quest_reference (node : Data.Quest_t.quest_node_internal) available =
    match node with
    | `Action _ -> []
    | `Objective { condition; name; id; _ } ->
      (match condition with
       | `KillSpecific { Data.Quest_t.character_types; amount; _ } ->
         let spawned = available_count character_types available in
         if spawned >= amount
         then []
         else
           [ Printf.sprintf
               "Quest %s is not completable: objective '%s' (id %d) requires %d \
                kill(s) of %s but only %d spawn(s) precede it"
               (Data.Common_j.string_of_entity_reference quest_reference)
               name
               id
               amount
               (String.concat
                  "/"
                  (List.map Data.Character_j.string_of_character_type character_types))
               spawned ]
       | _ -> [])
    | `Sequence { children; _ } ->
      let _, errors =
        List.fold_left
          (fun (available, errors) child ->
            let errors = errors @ walk quest_reference child available in
            available @ subtree_spawns child, errors)
          (available, [])
          children
      in
      errors
    | `Parallel { children; _ } ->
      let concurrent = available @ List.concat_map subtree_spawns children in
      List.concat_map (fun child -> walk quest_reference child concurrent) children
    | `Any { children; _ } ->
      let concurrent = available @ List.concat_map subtree_spawns children in
      List.concat_map (fun child -> walk quest_reference child concurrent) children
    | `Timer { child; _ } ->
      walk quest_reference child (available @ subtree_spawns child)
  in
  List.concat_map
    (fun (definition : Data.Entity_t.entity_definition_internal) ->
      match definition with
      | `Quest { entity; _ } ->
        let quest_reference = Dataset.entity_reference_of_entity_definition definition in
        walk quest_reference entity.Data.Quest_t.root []
      | _ -> [])
    definitions
;;

(* Rarity order of a quality; None (a bare item) is below every tier *)
let quality_rank (q : Data.Quality_t.quality_type) =
  match q with
  | `None -> 0
  | `Common -> 1
  | `Uncommon -> 2
  | `Rare -> 3
  | `Epic -> 4
  | `Legendary -> 5
;;

let monster_rank_order (r : Data.Character_t.monster_rank) =
  match r with
  | `Normal -> 0
  | `Elite -> 1
  | `Boss -> 2
;;

(* Drop tables: every item entry of a table with a rarity curve has a rarity (not quality None), and
   every guarantee can be met by some entry that a monster of that rank may drop *)
let drop_table_error_messages definitions =
  let qualities =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        let reference = Dataset.entity_reference_of_entity_definition definition in
        match definition with
        | `Equipment { entity; _ } -> Some (reference, entity.Data.Equipment_t.quality)
        | `Weapon { entity; _ } -> Some (reference, entity.Data.Weapon_t.quality)
        | `SkillStone { entity; _ } -> Some (reference, entity.Data.Skill_stone_t.quality)
        | _ -> None)
      definitions
  in
  (* kind, reference, gate of every item entry *)
  let entries (t : Data.Drop_t.drop_table) =
    List.filter_map
      (fun (d : Data.Drop_t.drop_internal) ->
        match d with
        | `Equipment e -> Some (`Equipment, e.equipment, e.gate)
        | `Weapon w -> Some (`Weapon, w.weapon, w.gate)
        | `SkillStone s -> Some (`SkillStone, s.skill_stone, s.gate)
        | `Gold _ | `Skill _ -> None)
      (t.equipment_drops @ t.weapon_drops @ t.skill_stone_drops)
  in
  List.concat_map
    (fun (definition : Data.Entity_t.entity_definition_internal) ->
      match definition with
      | `DropTable { entity; _ } ->
        let name =
          Data.Common_j.string_of_entity_reference
            (Dataset.entity_reference_of_entity_definition definition)
        in
        let quality_of r = List.assoc_opt r qualities in
        let curve_errors =
          match entity.Data.Drop_t.rarity_curve with
          | None -> []
          | Some _ ->
            List.filter_map
              (fun (_, r, _) ->
                match quality_of r with
                | Some `None ->
                  Some
                    (Printf.sprintf
                       "Drop table %s has a rarity curve but %s has quality None"
                       name
                       (Data.Common_j.string_of_entity_reference r))
                | _ -> None)
              (entries entity)
        in
        let guarantee_errors =
          List.filter_map
            (fun (g : Data.Drop_t.drop_guarantee) ->
              let satisfiable =
                List.exists
                  (fun (kind, r, (gate : Data.Drop_t.drop_gate option)) ->
                    kind = g.kind
                    && (match quality_of r with
                        | Some q -> quality_rank q >= quality_rank g.min_quality
                        | None -> false)
                    &&
                    match gate with
                    | None -> true
                    | Some gate ->
                      Option.is_none gate.min_tier
                      &&
                      (match gate.min_rank with
                       | None -> true
                       | Some rank ->
                         monster_rank_order rank <= monster_rank_order g.min_rank
                         || Option.is_some gate.lower_rank_weight))
                  (entries entity)
              in
              if satisfiable
              then None
              else
                Some
                  (Printf.sprintf
                     "Drop table %s guarantees a %s of at least %s for a %s, but no entry can satisfy it"
                     name
                     (Data.Drop_j.string_of_drop_type g.kind)
                     (Data.Quality_j.string_of_quality_type g.min_quality)
                     (Data.Character_j.string_of_monster_rank g.min_rank)))
            (Option.value entity.guarantees ~default:[])
        in
        curve_errors @ guarantee_errors
      | _ -> [])
    definitions
;;

(* Quest tiers: orders are unique, the base tier (lowest order) changes nothing, and a drop gate never asks
   for a tier that does not exist *)
let tier_error_messages definitions =
  let tiers =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `QuestDifficulty { key; entity; _ } ->
          Option.map (fun t -> key, t) entity.Data.Quest_t.tier
        | _ -> None)
      definitions
  in
  let order_of (_, (t : Data.Quest_t.quest_tier)) = t.order in
  let duplicate_errors =
    List.filter_map
      (fun ((key, t) as tier) ->
        if List.exists (fun other -> other != tier && order_of other = order_of tier) tiers
        then
          Some
            (Printf.sprintf
               "Quest tier %s has order %d, which another tier has too"
               key
               t.Data.Quest_t.order)
        else None)
      tiers
  in
  let is_identity (t : Data.Quest_t.quest_tier) =
    Option.value t.level_bonus ~default:0 = 0
    && Option.value t.extra_adds ~default:0. = 0.
    && Option.value t.affixes ~default:[] = []
    && Option.value t.rarity_tilt ~default:0. = 0.
    && Option.is_none t.guarantee_min_quality
    && Option.value t.gold_multiplier ~default:1. = 1.
  in
  let base_errors =
    match List.sort (fun a b -> compare (order_of a) (order_of b)) tiers with
    | (key, t) :: _ when not (is_identity t) ->
      [ Printf.sprintf
          "Quest tier %s has the lowest order, so it is the base tier and must change nothing"
          key
      ]
    | _ -> []
  in
  let highest = List.fold_left (fun acc tier -> max acc (order_of tier)) 0 tiers in
  let gate_errors =
    List.concat_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `DropTable { entity; id; _ } ->
          List.filter_map
            (fun (d : Data.Drop_t.drop_internal) ->
              let gate =
                match d with
                | `Equipment e -> e.gate
                | `Weapon w -> w.gate
                | `SkillStone s -> s.gate
                | `Gold _ | `Skill _ -> None
              in
              match gate with
              | Some { min_tier = Some m; _ } when m > highest ->
                Some
                  (Printf.sprintf
                     "Quest tiers: drop table %s gates an entry on tier %d, but the highest tier is %d"
                     id
                     m
                     highest)
              | _ -> None)
            (entity.equipment_drops @ entity.weapon_drops @ entity.skill_stone_drops)
        | _ -> [])
      definitions
  in
  duplicate_errors @ base_errors @ gate_errors
;;

let validate_entity_definitions dataset =
  let entity_definition_references = extract_entity_reference_definition dataset in
  let entity_reference_definitions_duplication_errors =
    List.filter
      (fun entity_reference ->
        List.length
          (List.filter (fun er -> er = entity_reference) entity_definition_references)
        > 1)
      entity_definition_references
  in
  let entity_reference_definitions_error_messages =
    List.map
      (fun er -> "Duplicate definitions: " ^ Data.Common_j.string_of_entity_reference er)
      entity_reference_definitions_duplication_errors
  in
  let entity_references = extract_entity_reference dataset in
  let entity_definition_reference_missing_errors =
    List.filter
      (fun entity_reference ->
        not @@ List.mem entity_reference entity_definition_references)
      entity_references
  in
  let entity_reference_error_messages =
    List.map
      (fun er -> "Entity reference not found: " ^ Data.Common_j.string_of_entity_reference er)
      entity_definition_reference_missing_errors
  in
  let anchor_ownership_errors = anchor_ownership_error_messages (take_definitions dataset) in
  let quest_anchor_subset_errors =
    quest_anchor_subset_error_messages (take_definitions dataset)
  in
  let quest_completability_errors =
    quest_completability_error_messages (take_definitions dataset)
  in
  let drop_table_errors = drop_table_error_messages (take_definitions dataset) in
  let tier_errors = tier_error_messages (take_definitions dataset) in
  let errors =
    List.map
      (fun message ->
        { origin_location = __LOC__;
          file_path = "Unknown file path... post-processing error.";
          error = Some (Atdgen_runtime.Util.Validation.error ~msg:message [])
        })
      (entity_reference_definitions_error_messages
       @ entity_reference_error_messages
       @ anchor_ownership_errors
       @ quest_anchor_subset_errors
       @ quest_completability_errors
       @ drop_table_errors
       @ tier_errors)
  in
  { dataset with errors = errors @ dataset.errors }
;;

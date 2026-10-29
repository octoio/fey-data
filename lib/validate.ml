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

(* An Interact or Channel objective uses Zone anchors (the place a player stands in to interact) *)
let interact_error_messages definitions =
  let is_zone reference =
    List.exists
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Anchor { entity = `Zone _; _ } ->
          Dataset.entity_reference_of_entity_definition definition = reference
        | _ -> false)
      definitions
  in
  let rec interact_anchors (node : Data.Quest_t.quest_node_internal) =
    match node with
    | `Sequence { children; _ } | `Parallel { children; _ } | `Any { children; _ } ->
      List.concat_map interact_anchors children
    | `Timer { child; _ } -> interact_anchors child
    | `Objective { condition = `Interact { anchor; _ }; _ } -> [ anchor ]
    | `Objective { condition = `Channel { anchors; _ }; _ } -> anchors
    | `Objective _ | `Action _ -> []
  in
  List.concat_map
    (fun (definition : Data.Entity_t.entity_definition_internal) ->
      match definition with
      | `Quest { entity; _ } ->
        List.filter_map
          (fun anchor ->
            if is_zone anchor
            then None
            else
              Some
                (Printf.sprintf
                   "Quest %s interacts with anchor %s, which is not a Zone"
                   (Data.Common_j.string_of_entity_reference
                      (Dataset.entity_reference_of_entity_definition definition))
                   (Data.Common_j.string_of_entity_reference anchor)))
          (interact_anchors entity.root)
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
    Option.value t.rate ~default:1. = 1.
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

(* The run ending (fey-rs docs/design/runs.md): a board with quests must have a way to finish a run, so at least one
   board quest has [ends_run] and the board can reach it from the start. Reachability follows the rules of the
   board: a quest is offered once the party owns ANY ONE of its required flags ([None] is always owned), and
   clearing a quest grants its flag. A quest that is not on the board (origin other than QuestBoard) is never
   offered, so it cannot be the way out. *)
let run_ending_error_messages definitions =
  let board =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Quest { id; entity; _ } when entity.Data.Quest_t.origin = `QuestBoard ->
          Some (id, entity)
        | _ -> None)
      definitions
  in
  let offered owned (quest : Data.Quest_t.quest) =
    List.exists (fun flag -> flag = `None || List.mem flag owned) quest.required_achievements
  in
  let rec flags_earned owned =
    let grown =
      List.fold_left
        (fun acc (_, (quest : Data.Quest_t.quest)) ->
          if offered owned quest && not (List.mem quest.achievement_on_complete acc)
          then quest.achievement_on_complete :: acc
          else acc)
        owned
        board
    in
    if List.length grown = List.length owned then owned else flags_earned grown
  in
  let reachable = flags_earned [] in
  let endings =
    List.filter (fun (_, (q : Data.Quest_t.quest)) -> q.ends_run = Some true) board
  in
  match board, endings with
  | [], _ -> []
  | _, [] -> [ "Run ending: no quest of the board has ends_run, so a run could never be won" ]
  | _, _ ->
    List.filter_map
      (fun (id, quest) ->
        if offered reachable quest
        then None
        else
          Some
            (Printf.sprintf
               "Run ending: quest %s ends the run but the board never offers it (none of its required achievements can be earned)"
               id))
      endings
;;

(* A weapon category belongs to at most one weapon class, so a weapon finds its animation set without a tie *)
let weapon_class_error_messages definitions =
  let classes =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `WeaponClass { key; entity; _ } -> Some (key, entity.Data.Weapon_class_t.categories)
        | _ -> None)
      definitions
  in
  let categories =
    List.sort_uniq compare (List.concat_map (fun (_, categories) -> categories) classes)
  in
  List.filter_map
    (fun category ->
      match List.filter (fun (_, cs) -> List.mem category cs) classes with
      | _ :: _ :: _ as owners ->
        Some
          (Printf.sprintf
             "Weapon category %s is in the weapon classes %s"
             (Base.String.strip ~drop:(fun c -> c = '"')
                (Data.Weapon_j.string_of_weapon_category category))
             (String.concat ", " (List.sort compare (List.map fst owners))))
      | _ -> None)
    categories
;;

(* Map generation (docs/design/mapgen.md): a stage with a generator spec must be generatable. The spec is
   feasible (the static checks of the Rust generator), every anchor of the stage is bound to a generated slot
   with contiguous ordinals, and every quest played on the stage finds its anchors and its rooms in the spec.
   Room prefab ids are unique and their slots lie inside the room. *)
let generator_error_messages definitions =
  let ref_string (r : Data.Common_t.entity_reference) = r.id in
  let prefabs =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `RoomPrefab { entity; _ } ->
          Some (Dataset.entity_reference_of_entity_definition definition, entity)
        | _ -> None)
      definitions
  in
  let anchors =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Anchor { entity; _ } ->
          let binding =
            match entity with
            | `Zone a -> a.binding
            | `SpawnPoint a -> a.binding
            | `Portal a -> a.binding
            | `QuestBoard a -> a.binding
            | `LampPost a -> a.binding
            | `HealingShrine a -> a.binding
          in
          Some (Dataset.entity_reference_of_entity_definition definition, binding)
        | _ -> None)
      definitions
  in
  let template_anchors =
    List.filter_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Anchor { entity = `LampPost _ | `HealingShrine _ | `SpawnPoint _; _ } ->
          Some (Dataset.entity_reference_of_entity_definition definition).id
        | _ -> None)
      definitions
  in
  let prefab_kind reference =
    Option.map
      (fun (p : Data.Room_prefab_t.room_prefab) -> p.generator)
      (List.assoc_opt reference prefabs)
  in
  let prefab_errors =
    List.concat_map
      (fun (reference, (p : Data.Room_prefab_t.room_prefab)) ->
        let name = ref_string reference in
        let duplicate =
          if List.exists
               (fun (other, (o : Data.Room_prefab_t.room_prefab)) ->
                 other <> reference && o.prefab_id = p.prefab_id)
               prefabs
          then [ Printf.sprintf "Room prefab %s: prefab_id %s is used by another prefab" name p.prefab_id ]
          else []
        in
        let hx = p.size.x /. 2. and hz = p.size.z /. 2. in
        let outside =
          List.filter_map
            (fun (s : Data.Room_prefab_t.prefab_slot) ->
              if Float.abs s.x <= hx && Float.abs s.z <= hz
              then None
              else
                Some
                  (Printf.sprintf
                     "Room prefab %s: slot at (%g, %g) is outside the %g x %g room"
                     name
                     s.x
                     s.z
                     p.size.x
                     p.size.z))
            p.slots
        in
        let size =
          if p.size.x > 0. && p.size.z > 0.
          then []
          else [ Printf.sprintf "Room prefab %s: size must be positive" name ]
        in
        duplicate @ outside @ size)
      prefabs
  in
  let stage_errors =
    List.concat_map
      (fun (definition : Data.Entity_t.entity_definition_internal) ->
        match definition with
        | `Stage { entity = { generator = Some spec; anchors = stage_anchors; _ }; _ } ->
          let stage_reference = Dataset.entity_reference_of_entity_definition definition in
          let stage_name = ref_string stage_reference in
          let spec_errors =
            List.map
              (fun m -> Printf.sprintf "Stage %s generator: %s" stage_name m)
              (Mapgen_check.spec_problems ~prefab_kind spec)
          in
          let bindings =
            List.filter_map
              (fun (a : Data.Common_t.entity_reference) ->
                match List.assoc_opt a anchors with
                | Some (Some binding) -> Some (a.id, binding)
                | _ -> None)
              stage_anchors
          in
          let unbound_errors =
            List.filter_map
              (fun (a : Data.Common_t.entity_reference) ->
                match List.assoc_opt a anchors with
                | Some None ->
                  Some
                    (Printf.sprintf
                       "Stage %s has a generator, so anchor %s needs a binding"
                       stage_name
                       (ref_string a))
                | _ -> None)
              stage_anchors
          in
          let every (b : Data.Generator_t.anchor_binding) = b.every = Some true in
          let every_errors =
            List.filter_map
              (fun (id, (b : Data.Generator_t.anchor_binding)) ->
                if not (every b)
                then None
                else if not (List.mem id template_anchors)
                then
                  Some
                    (Printf.sprintf
                       "Stage %s: anchor %s is bound with every, which only a LampPost, a HealingShrine or a SpawnPoint may be"
                       stage_name
                       id)
                else if b.ordinal <> None
                then
                  Some
                    (Printf.sprintf
                       "Stage %s: anchor %s is bound with every, so it has no ordinal"
                       stage_name
                       id)
                else None)
              bindings
          in
          let bindings_counted = List.filter (fun (_, b) -> not (every b)) bindings in
          let ordinal_errors =
            let slots =
              List.sort_uniq
                compare
                (List.map (fun (_, (b : Data.Generator_t.anchor_binding)) -> b.slot) bindings_counted)
            in
            List.filter_map
              (fun slot ->
                let ordinals =
                  List.sort
                    compare
                    (List.filter_map
                       (fun (_, (b : Data.Generator_t.anchor_binding)) ->
                         if b.slot = slot
                         then Some (Option.value b.ordinal ~default:0)
                         else None)
                       bindings_counted)
                in
                if ordinals = List.init (List.length ordinals) Fun.id
                then None
                else
                  Some
                    (Printf.sprintf
                       "Stage %s: the ordinals of the %s anchors are %s, they must be 0..n-1 without repeats"
                       stage_name
                       (Base.String.strip
                          ~drop:(fun c -> c = '"')
                          (Data.Generator_j.string_of_anchor_slot slot))
                       (String.concat ", " (List.map string_of_int ordinals))))
              slots
          in
          let quest_errors =
            List.concat_map
              (fun (d : Data.Entity_t.entity_definition_internal) ->
                match d with
                | `Quest { entity = quest; _ } when quest.stage = stage_reference ->
                  let quest_name = ref_string (Dataset.entity_reference_of_entity_definition d) in
                  (match Mapgen_check.demands quest bindings with
                   | Error unbound ->
                     List.map
                       (fun id ->
                         Printf.sprintf
                           "Quest %s names anchor %s, which has no binding on its generated stage %s"
                           quest_name
                           id
                           stage_name)
                       unbound
                   | Ok demands ->
                     let raised = Mapgen_check.apply_demands spec demands in
                     (* only what the demands add: problems the spec has alone are reported once above *)
                     let base = Mapgen_check.spec_problems ~prefab_kind spec in
                     List.filter_map
                       (fun m ->
                         if List.mem m base
                         then None
                         else
                           Some
                             (Printf.sprintf
                                "Quest %s cannot run on generated stage %s: %s"
                                quest_name
                                stage_name
                                m))
                       (Mapgen_check.spec_problems ~prefab_kind raised))
                | _ -> [])
              definitions
          in
          spec_errors @ unbound_errors @ every_errors @ ordinal_errors @ quest_errors
        | _ -> [])
      definitions
  in
  prefab_errors @ stage_errors
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
  let interact_errors = interact_error_messages (take_definitions dataset) in
  let drop_table_errors = drop_table_error_messages (take_definitions dataset) in
  let tier_errors = tier_error_messages (take_definitions dataset) in
  let weapon_class_errors = weapon_class_error_messages (take_definitions dataset) in
  let generator_errors = generator_error_messages (take_definitions dataset) in
  let run_ending_errors = run_ending_error_messages (take_definitions dataset) in
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
       @ interact_errors
       @ drop_table_errors
       @ tier_errors
       @ weapon_class_errors
       @ generator_errors
       @ run_ending_errors)
  in
  { dataset with errors = errors @ dataset.errors }
;;

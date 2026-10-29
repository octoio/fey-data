(* Static checks of map generator specs: a port of fey_core::mapgen::{spec_problems, quest::demands,
   quest::apply_demands} (fey-rs), so a spec that can never be met, or a quest a generated stage cannot
   run, is a build error here and not a surprise at stage entry. The Rust code is the reference: change
   both together (their test cases mirror each other). *)

open Data.Generator_t

let role_name : room_role -> string = function
  | `Entrance -> "entrance"
  | `Combat -> "combat"
  | `Ambush -> "ambush"
  | `Rest -> "rest"
  | `Treasure -> "treasure"
  | `Objective -> "objective"
  | `Boss -> "boss"
  | `Filler -> "filler"
;;

let generator_name : generator_kind -> string = function
  | `Crypt -> "Crypt"
  | `Forest -> "Forest"
  | `Swamp -> "Swamp"
;;

let hard (r : room_requirement) = Option.value r.hard ~default:true
let path (r : room_requirement) : path_rule = Option.value r.path ~default:`Any
let min_spacing (r : room_requirement) = Option.value r.min_spacing ~default:0

let placement (r : room_requirement) =
  Option.value
    r.placement
    ~default:{ farthest = None; min_depth = None; max_depth = None; chokepoint = None }
;;

let farthest r = Option.value (placement r).farthest ~default:false
let min_depth r = Option.value (placement r).min_depth ~default:0
let chokepoint r = Option.value (placement r).chokepoint ~default:false

(* [prefab_kind] resolves a RoomPrefab reference to the generator kind of the prefab, None when it does not exist *)
let spec_problems
  ~(prefab_kind : Data.Common_t.entity_reference -> generator_kind option)
  (spec : generator_spec)
  : string list
  =
  let out = ref [] in
  let push s = out := s :: !out in
  let pushf fmt = Printf.ksprintf push fmt in
  let c = spec.connections in
  if c.main_path.min < 2 || c.main_path.min > c.main_path.max
  then
    pushf
      "main path [%d, %d] must be at least 2 rooms with min <= max"
      c.main_path.min
      c.main_path.max;
  if c.loops.min > c.loops.max then pushf "loops [%d, %d]: min > max" c.loops.min c.loops.max;
  if c.branch_len.min > c.branch_len.max
  then pushf "branch length [%d, %d]: min > max" c.branch_len.min c.branch_len.max;
  if Option.value spec.party_size ~default:4 = 0 then push "party size 0";
  let longest = c.main_path.max in
  let farthest_reqs = List.filter farthest spec.requirements in
  if List.length farthest_reqs > 1
  then push "more than one requirement claims the farthest room";
  let saturating_sub a b = if a > b then a - b else 0 in
  List.iter
    (fun (r : room_requirement) ->
      let n = role_name r.role in
      if r.min > r.max then pushf "%s: min %d > max %d" n r.min r.max;
      (match (placement r).max_depth with
       | Some m when m < min_depth r -> pushf "%s: max depth below min depth" n
       | _ -> ());
      if farthest r
      then (
        if r.max <> 1 || r.min > 1
        then pushf "%s: the farthest room is one room (min/max must be 1)" n;
        if path r = `Branch
        then pushf "%s: the farthest room is on the main path, not a branch" n;
        if min_depth r + 1 > longest
        then
          pushf
            "%s must be at depth >= %d but the main path has at most %d rooms (depth %d)"
            n
            (min_depth r)
            longest
            (saturating_sub longest 1));
      if chokepoint r && path r <> `MainPath
      then pushf "%s: a chokepoint must be on the main path" n;
      if r.role = `Entrance && (r.min > 1 || r.max <> 1)
      then push "entrance: exactly one";
      (match r.prefab with
       | None -> ()
       | Some p ->
         (match prefab_kind p with
          | None -> pushf "%s: unknown prefab %s" n p.key
          | Some kind when kind <> spec.generator ->
            pushf "%s: prefab %s is not a %s prefab" n p.key (generator_name spec.generator)
          | Some _ -> ()));
      if path r = `MainPath && hard r && (not (farthest r)) && r.min > 0
      then (
        let inner_max = saturating_sub longest 2 in
        let last_slot = saturating_sub longest 2 in
        if min_depth r > last_slot
        then
          pushf
            "%s must be at depth >= %d on the main path, whose last free slot is depth %d"
            n
            (min_depth r)
            last_slot;
        let needed = max (min_depth r) 1 + ((r.min - 1) * max (min_spacing r) 1) in
        if needed > max inner_max 1 && r.min > 1
        then
          pushf
            "%s: %d rooms %d apart from depth %d need a main path of %d rooms, at most %d"
            n
            r.min
            (max (min_spacing r) 1)
            (max (min_depth r) 1)
            (needed + 2)
            longest))
    spec.requirements;
  let main_hard =
    List.fold_left
      (fun acc (r : room_requirement) ->
        if hard r && path r = `MainPath && (not (farthest r)) && r.role <> `Entrance
        then acc + r.min
        else acc)
      0
      spec.requirements
  in
  let inner = saturating_sub longest (if farthest_reqs = [] then 1 else 2) in
  if main_hard > inner
  then
    pushf
      "the main path needs %d required rooms between the entrance and the end, but has at \
       most %d"
      main_hard
      inner;
  let pinned_at = ref 0 in
  List.iter
    (fun (pin : pinned_prefab) ->
      (match prefab_kind pin.prefab with
       | None -> pushf "pinned: unknown prefab %s" pin.prefab.key
       | Some kind when kind <> spec.generator ->
         pushf
           "pinned: prefab %s is not a %s prefab"
           pin.prefab.key
           (generator_name spec.generator)
       | Some _ -> ());
      let can =
        pin.role = `Entrance
        || pin.role = spec.fill
        || List.exists
             (fun (r : room_requirement) -> r.role = pin.role && r.max > 0)
             spec.requirements
      in
      if not can
      then
        pushf
          "pinned prefab %s needs a %s room but no requirement makes one"
          pin.prefab.key
          (role_name pin.role);
      if Option.is_some pin.at then incr pinned_at)
    (Option.value spec.pinned ~default:[]);
  if !pinned_at > 1 then push "only one pinned prefab may have a fixed position";
  (* data-only: the contract of the fields the Rust struct cannot get wrong *)
  if spec.seed_policy = `Fixed && Option.is_none spec.fixed_seed
  then push "seed policy Fixed needs fixed_seed";
  if spec.seed_policy = `PerRun && Option.is_some spec.fixed_seed
  then push "fixed_seed is only used by seed policy Fixed";
  (match spec.par with
   | Some { boss_path = { min; max } } when min < 0. || min > max ->
     pushf "par boss_path [%g, %g]: needs 0 <= min <= max" min max
   | _ -> ());
  List.rev !out
;;

(* ---- quest demands ---- *)

let slot_order : anchor_slot -> int = function
  | `PlayerSpawn -> 0
  | `EntryPortal -> 1
  | `PackSpawn -> 2
  | `BossSpawn -> 3
  | `ExitPortal -> 4
  | `Shrine -> 5
  | `Chest -> 6
  | `ObjectiveZone -> 7
  | `TrailSpawn -> 8
  | `WaterSpawn -> 9
  | `Cocoon -> 10
  | `Marker -> 11
  | `Healing -> 12
;;

type slot_demand =
  { slot : anchor_slot;
    count : int;
    optional : bool
  }

(* Every anchor id a quest names, with true when only optional objectives name it *)
let quest_anchors (quest : Data.Quest_t.quest) : (string * bool) list =
  let table = Hashtbl.create 16 in
  let order = ref [] in
  let note (id : string) opt =
    match Hashtbl.find_opt table id with
    | None ->
      Hashtbl.replace table id opt;
      order := id :: !order
    | Some e -> Hashtbl.replace table id (e && opt)
  in
  let rec walk optional (node : Data.Quest_t.quest_node_internal) =
    match node with
    | `Sequence { children; _ } | `Parallel { children; _ } | `Any { children; _ } ->
      List.iter (walk optional) children
    | `Timer { child; _ } -> walk optional child
    | `Objective { condition; is_optional; _ } ->
      let opt = optional || is_optional in
      (match condition with
       | `EnterAnchor { anchor; _ } | `StayInAnchor { anchor; _ } | `Interact { anchor; _ }
         -> note anchor.id opt
       | `Channel { anchors; spawns; _ } ->
         List.iter (fun (a : Data.Common_t.entity_reference) -> note a.id opt) anchors;
         List.iter (fun (s : Data.Spawn_t.spawn) -> note s.anchor.id opt) spawns
       | _ -> ())
    | `Action { action; _ } ->
      (match action with
       | `Spawn { spawn; _ } -> note spawn.anchor.id optional
       | `StartSpawnSequence { spawn_sequence; _ } ->
         List.iter
           (fun (step : Data.Spawn_t.spawn_sequence_step) ->
             List.iter
               (fun (s : Data.Spawn_t.spawn) -> note s.anchor.id optional)
               step.spawns)
           spawn_sequence.steps
       | `ActivateAnchor { anchor; _ }
       | `DeactivateAnchor { anchor; _ }
       | `ActivatePortalToStage { anchor; _ }
       | `ActivatePortalToQuestStage { anchor; _ } -> note anchor.id optional
       | _ -> ())
  in
  walk false quest.root;
  List.rev_map (fun id -> id, Hashtbl.find table id) !order
;;

(* The slot demands of a quest under [bindings] (anchor id -> binding); the anchors no binding maps are Error *)
let demands (quest : Data.Quest_t.quest) (bindings : (string * anchor_binding) list) =
  let by_slot = ref [] in
  let unbound = ref [] in
  List.iter
    (fun (anchor, optional) ->
      match List.assoc_opt anchor bindings with
      | None -> unbound := anchor :: !unbound
      | Some (b : anchor_binding) ->
        let ordinal = Option.value b.ordinal ~default:0 in
        (match List.assoc_opt b.slot !by_slot with
         | None -> by_slot := (b.slot, (ordinal + 1, optional)) :: !by_slot
         | Some (count, opt) ->
           by_slot
           := (b.slot, (max count (ordinal + 1), opt && optional))
              :: List.remove_assoc b.slot !by_slot))
    (quest_anchors quest);
  match List.rev !unbound with
  | _ :: _ as unbound -> Error unbound
  | [] ->
    Ok
      (List.sort (fun a b -> compare (slot_order a.slot) (slot_order b.slot))
         (List.map (fun (slot, (count, optional)) -> { slot; count; optional }) !by_slot))
;;

let role_of_slot : anchor_slot -> room_role option = function
  | `PackSpawn -> Some `Combat
  | `BossSpawn | `ExitPortal -> Some `Boss
  | `Shrine -> Some `Rest
  | `Chest -> Some `Treasure
  | `ObjectiveZone -> Some `Objective
  | `PlayerSpawn | `EntryPortal | `TrailSpawn | `WaterSpawn | `Cocoon | `Marker | `Healing -> None
;;

let new_requirement role n size : room_requirement =
  { role;
    min = n;
    max = n;
    hard = None;
    size;
    placement = None;
    path = None;
    min_spacing = None;
    prefab = None;
    tags = None;
    weight = None
  }
;;

(* The spec with its requirements raised so the layout carries every demanded anchor; a demanded room becomes hard *)
let apply_demands (spec : generator_spec) (demands : slot_demand list) : generator_spec =
  let apply (spec : generator_spec) d =
    match role_of_slot d.slot with
    | None -> spec
    | Some role ->
      let reqs = spec.requirements in
      let have =
        List.fold_left
          (fun acc (r : room_requirement) ->
            let counts =
              if role = `Combat
              then
                r.role = `Combat
                || r.role = `Ambush
                || (* a forest's nest has a pack guarding it *)
                (spec.generator = `Forest && r.role = `Objective)
              else r.role = role
            in
            if hard r && counts then acc + r.min else acc)
          0
          reqs
      in
      let need = d.count in
      if have >= need
      then spec
      else (
        let missing = need - have in
        let raised (r : room_requirement) =
          let min = if hard r then r.min + missing else missing in
          { r with
            min;
            max = Stdlib.max r.max min;
            hard = Some true;
            path = (if d.optional then Some `Branch else r.path)
          }
        in
        let rec first_match = function
          | [] -> None
          | (r : room_requirement) :: rest ->
            if r.role = role then Some (raised r :: rest) else Option.map (fun l -> r :: l) (first_match rest)
        in
        match first_match reqs with
        | Some requirements -> { spec with requirements }
        | None ->
          let r = new_requirement role need `Medium in
          let r =
            if role = `Boss
            then
              { r with
                size = `Huge;
                placement =
                  Some
                    { farthest = Some true;
                      min_depth = None;
                      max_depth = None;
                      chokepoint = None
                    };
                path = Some `MainPath
              }
            else r
          in
          let r = if d.optional then { r with path = Some `Branch } else r in
          { spec with requirements = reqs @ [ r ] })
  in
  List.fold_left apply spec demands
;;

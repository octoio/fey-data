# Quest & Stage System Redesign

## Status
**SHIPPED — merged to main in all three repos (July 12, 2026).** Play-tested; follow-up fixes (boar Gore skill, forest slime spawns, dormant zones, drop/loot crashes, completability validation) landed in the same PRs.
- fey-data: anchor/stage/quest-v2 ATD schemas, validations (anchor ownership, quest⊆stage anchor
  subset, unique node ids), fixtures — committed green on `quest-entity`.
- fey (Unity): full port on `quest-entity` (uncommitted): AnchorService with runtime-spawned
  zone/portal prefabs, uint stage/anchor indices through the whole interact/teleport chain,
  QuestService execution tree (Sequence/Parallel/Any/Timer/Objective/Action), condition modules,
  QuestActionExecutor, GUI rework. Legacy StageType/ZoneType/PortalType/StageData and the
  requirement/result module stack are deleted. `compile.fish` green, `test.fish` 721 green
  (includes new QuestNodeWalkerTests; QuestNodeWalker lives in the Data assembly).
- fey-console: quest-v2/anchor/stage models + dedicated Stage Editor tab (stage form, anchor
  cards with transforms/zone fields, entity-ref pickers, save-to-disk). 1160 tests, lint, build
  all green (uncommitted on `quest-entity`).
- Remaining: assign _portalPrefab/_overlapZonePrefab/_triggerZonePrefab on GameStorageData in
  the inspector; play-test; then logical green commits (this document stays local).

The five open questions were resolved with Octoio (see Decisions).

## Working Agreements
- **Every commit compiles and passes tests** (dune runtest, npm test, ./compile.fish, ./test.fish).
- **Refactor-first commits**: when reuse/cleanup enables a step, land it as its own green commit
  before building on top of it.

## Vision & Principles

1. **Data first.** A quest and the stage it runs on should be fully described by JSON entities.
   Unity scenes carry art (terrain, props, lighting, navmesh); *gameplay semantics live in data*.
2. **Contract over convention.** If a quest needs a zone, that dependency is an entity reference
   the OCaml pipeline validates at build time — not a `FindObjectsOfType` prayer at runtime.
3. **Composition over enumeration.** Behavior grows by composing nodes (like the skill execution
   tree), not by adding enum variants + switch cases + hand-written modules for every new idea.
4. **Engine, not game.** The Unity side interprets data through generated interfaces + visitors.
   Someone with the open data schema should be able to author a campaign without touching the editor.

### Accepted boundaries (deliberate shortcuts)
- Character models/animations stay editor-bound for now (animation-through-data is a separate,
  hard problem — current placeholder approach is fine).
- Scene environments (terrain, props, navmesh) stay as Unity scenes. We are NOT building a
  data-driven level editor; we are extracting *gameplay markers* from scenes into data.

---

## Current State & Problems

### 1. Quest structure is a fixed pipeline, not a tree
`quest → steps[] → objectives[] (AND) → requirements[]` with `completion_results[]` fired on
step transitions. Compare with skills: `execution_root` is a tree of nodes (Sequence, Parallel,
Requirement, Hit, Delay...) traversed by a generated visitor. Quests cannot express:
- branching ("kill the boss OR sneak past"), optional side objectives that gate bonus results
- racing/timed objectives ("defend for 80s WHILE keeping the farmer alive")
- mid-step results (everything waits for the step boundary)
- reuse of a sub-flow across quests

The step/objective/requirement *network records* (server-authoritative progress sync) are fine —
the problem is the static shape driving them.

### 2. The anchor problem (what started the JSON refactor)
Zones/portals/spawn points are scene GameObjects keyed by **global enums**:
- `BaseZoneController._zoneType : ZoneType` (`StartQuestZone`, `StayInZone` — global, grows forever)
- `PortalController._type : PortalType`, `CharacterSpawn._characterType`
- Discovered via `GameObject.FindObjectsOfType` in `StageEntityLoader`

Consequences:
- A quest's `ActivateZone(StartQuestZone)` has an invisible dependency on scene authoring.
  Wrong scene → silent no-op. No build-time validation possible.
- Two quests on one stage can't both use "a start zone" without sharing the same enum value.
- Authoring a new quest requires opening Unity to place/configure the zone. Not data first.

### 3. Stage is still a scriptable object
`StageData` (name, `StageType` enum, thumbnail, scene ref, theme color, quest id string) is the
last quest-adjacent SO. `StageType` is another global enum baked into portals, quests, records.

---

## Design

### A. Anchors are **entities**; stages own them

Anchors are a full entity type (`Anchor`, files `<name>.anchor.json`) rather than inline stage
fields. Rationale: entity references get the whole existing machinery for free — generic
"reference not found" validation, entity indices/hot reload, dataset extraction, and the
fey-console entity-reference picker. The only custom validation left is stage-scoping (below).

```ocaml
(* anchor.atd *)
type anchor_type <cs name="Anchor" namespace="Octoio.Fey.Data.Type"> =
  [ Zone | SpawnPoint | Portal | QuestBoard ]  (* extensible: Npc, Interactable, ... *)

type zone_shape_internal = [ Sphere of { radius: float } | Box of { half_extents: vector3 } ]

type anchor <cs ... modifiers="abstract,partial"> = {
  anchor_type <json name="type">: anchor_type;
  metadata: metadata;
  transform: transform;            (* relative to the stage's StageOrigin *)
}

type anchor_zone        = { ...; shape: zone_shape_internal; color: color; show_vfx: bool }
type anchor_spawn_point = { ...  (* pure location; WHAT spawns is the quest's business *) }
type anchor_portal      = { ... }
type anchor_internal    = [ Zone of ... | SpawnPoint of ... | Portal of ... | QuestBoard of ... ]
  <json adapter.ocaml="..."> (* visitor + converter generators *)

(* stage.atd *)
type stage <cs namespace="Octoio.Fey.Data.Dto"> = {
  metadata: metadata;
  scene_name: string;                     (* the art scene to load *)
  theme_color: color;
  thumbnail_reference: entity_reference;  (* Image entity, replaces Texture2D field *)
  anchors: entity_reference list;         (* `Anchor refs; the stage OWNS these *)
  ?quest: entity_reference option;        (* auto-started quest, replaces StageData._questId *)
}
```

- **Ownership direction**: stage → anchors. An anchor's transform is meaningless without its
  stage, so validation enforces every anchor is referenced by exactly one stage.
- **Runtime**: `AnchorFactory : IAnchorVisitor` instantiates zone/portal/spawn-point controllers
  from data at stage load (primitive trigger colliders + VFX from anchor config).
  `StageEntityLoader.FindObjectsOfType` dies. Controllers keep their current behavior
  (`ZoneStateChange` events etc.) but are *constructed from data* and identified by the anchor's
  entity index instead of a global enum.
- `ZoneType`/`PortalType`/`StageType` enums are eventually deleted. `StageType` in network
  records becomes the stage entity index (uint), same trick as the quest migration.
- Scenes keep only art. A `StageOrigin` transform in the scene defines the coordinate frame
  anchors are placed relative to (so data survives scene re-layouts).

### B. Quest ↔ stage linkage: anchor entity references, validated by the pipeline

Quest conditions/actions reference anchors as plain `entity_reference`
(`<ocaml valid="Validation.entity_reference_of_type `Anchor">`). Validation in `validate.ml`
post-processing (where cross-entity checks already live):

- generic: every referenced anchor definition exists (already free today)
- each anchor is owned by exactly one stage
- **subset check**: every anchor a quest references belongs to the quest's bound stage
- a stage's `quest` reference is a Quest entity

**This is the payoff of everything being entities**: the "can this quest run on this stage?"
question becomes a compile-time error in `dune exec gamedata`, not a runtime mystery. The
fey-console anchor pickers come free from the existing entity-reference-select component.

### C. Quest becomes an execution tree (skill pattern)

```ocaml
(* quest.atd, v2 *)
type quest_node_type = [ Sequence | Parallel | Any | Objective | Action | Timer ]

type quest_node <modifiers="abstract,partial"> = {
  node_type <json name="type">: ...;
  id: int;          (* unique within the quest tree (ushort range); names the node in network
                       records; survives data edits/hot reload. Console auto-assigns. *)
  name: string;
}

(* Control flow — mirrors skill_action_sequence/parallel *)
type quest_sequence_node = { children: quest_node_internal list }        (* in order *)
type quest_parallel_node = { children: quest_node_internal list }        (* all must complete *)
type quest_any_node      = { children: quest_node_internal list }        (* first to complete wins *)
type quest_timer_node    = { duration: float; child: quest_node_internal;
                             on_timeout: quest_timeout_behavior (* Fail | Complete | Restart *) }

(* Leaf: an objective = metadata + a condition the server tracks *)
type quest_objective_node = {
  metadata: metadata;
  is_optional: bool;
  condition: quest_condition_internal;   (* today's "requirement", event-driven *)
}

(* Leaf: fire-and-continue world mutation = today's "completion result" *)
type quest_action_node = {
  action: quest_action_internal;         (* Spawn | ActivateAnchor | DeactivateAnchor |
                                            ActivatePortal | Message | StartSpawnSequence |
                                            EnableQuestBoardQuest | GrantAchievement ... *)
}

type quest = {
  metadata; origin; assignee; difficulty;
  stage: entity_reference option;        (* the stage this quest is authored against *)
  execution_root: quest_node_internal;
  is_repeatable; required_achievements; ...
}
```

Notes:
- **Conditions** replace requirement variants: `KillSpecific`, `EnterAnchor(anchor_ref)`,
  `StayInAnchor(anchor_ref, seconds)`, `PickQuest`, `Teleport`, `Interact`. Each condition is ONE
  event-driven check; combining checks is the tree's job (Parallel/Any/Sequence) — see Decisions.
  The existing requirement *modules* survive almost intact — they become condition evaluators
  keyed by condition type, still driven by `ZoneStateChange`/`DeathReport`/etc. events.
- **Actions** absorb completion results AND start results (start results = actions sequenced
  before the first objective). `ActivateZone(enum)` → `ActivateAnchor(anchor_ref)`.
- `Message` becomes an explicit action node instead of a field on every result.
- Spawn actions target **spawn point anchors**: `Spawn { character; anchor; count }` — this kills
  the current hidden coupling where spawn position is baked into scene `CharacterSpawn` objects.

### D. Runtime: QuestTreeExecutor (server-side)

Mirror of `SkillTreeExecutor` but long-lived and network-synced:
- Generated `IQuestNodeVisitor` walks the tree; control nodes manage child activation.
- Each **objective node instance** maps to a `QuestObjectiveNetworkRecord` (today's records
  generalize: `Step` byte → `NodeId` ushort, the node's explicit id) so clients can mirror
  progress + GUI, robust to future runtime data changes.
- Completion bubbles up: objective completes → parent (Sequence advances / Parallel checks /
  Any cancels siblings) → root completes → quest completed.
- GUI renders the *active frontier* of the tree instead of `Objectives[step]`.

### E. What deliberately does NOT change
- Server-authoritative network records + storages (they generalize, not regenerate).
- The requirement/condition evaluation modules' event-driven core.
- Skill system, character/spawning services.
- Scenes as art + navmesh.

---

## Migration Phases (each ends green: dune runtest, npm test, ./compile.fish, ./test.fish)

### Phase 1 — Anchor + Stage entities (biggest unlock, independent of quest tree)
1. `anchor.atd`: anchor entity with Zone/SpawnPoint/Portal/QuestBoard variants
   (visitor/converter). Register `Anchor` entity type (per fey-entity-migration checklist).
2. `stage.atd`: stage entity referencing anchors. Register `Stage` entity type. Stage-scoping
   validation (each anchor owned by exactly one stage).
3. Convert 4 StageData assets → `*.stage.json` + `*.anchor.json` files; extract current
   zone/portal/spawn transforms from the scenes (one-off editor script can dump them).
4. Unity: `AnchorFactory`, load anchors on stage load; delete `StageEntityLoader` finds.
5. Keep quests on enums temporarily by mapping `StartQuestZone` → the stage's zone anchor.
6. fey-console: **dedicated stage editor** — stage form + owned-anchor list with per-variant
   fields (transform, zone shape/color), using entity-reference-select for pickers. Tests.

### Phase 2 — Quest v2 tree
1. `quest.atd` v2 (tree + conditions + actions with anchor entity refs). Subset validation
   (quest anchors ⊆ bound stage's anchors).
2. Regenerate; rewrite the 3 quests as trees (they are all expressible as one Sequence of
   Parallel groups — mechanical translation).
3. Unity: `QuestTreeExecutor` + node-id records; port requirement modules to condition
   evaluators; QuestResultModule → action visitor.
4. GUI: render active frontier. fey-console: quest tree editing can reuse the ReactFlow
   execution-tree editor infrastructure from skills.

### Phase 3 — Cleanup & flexibility dividends
1. Delete `ZoneType`/`PortalType`/`StageType` enums and `StageData` SO (order free, but every
   commit stays compilable/testable; prefer standalone refactor commits that land green first).
2. New content to prove flexibility: a branching quest (Any node) and a timed defense (Timer)
   with zero Unity edits.

## Decisions (resolved with Octoio, July 2026)
1. **Anchors are entities** referenced via `entity_reference` — not string keys. Stage owns its
   anchors (`anchors: entity_reference list`); validation enforces single ownership and the
   quest-anchors-⊆-stage-anchors subset check. Chosen because entity refs reuse all existing
   validation/indices/console machinery.
2. **Node identity = explicit `id` per node, validated unique.** Records need to name tree nodes
   over the network, and quest data may become runtime-mutable (hot reload) — so position-derived
   indices are out. Every quest node carries `id: int` (ushort range, fits the existing record
   fields); OCaml validation enforces uniqueness within a quest's tree. fey-console auto-assigns
   ids and keeps them out of the author's way. String ids were considered but numeric wins for
   network record compactness.
3. **No condition combinators.** A condition is a single event-driven check; AND/OR/ordering of
   checks is expressed with Parallel/Any/Sequence nodes in the tree. Revisit only if a case needs
   one *displayed* objective backed by compound logic.
4. **`Any` node does not roll back losing branches' actions.** Actions are fire-and-forget;
   authors place must-be-exclusive actions after the Any join.
5. **Retirement order is implementation-driven** with the hard rule: every commit compiles and
   passes all suites; reuse/refactor steps land as their own green commits before dependent work.

## Prior Art In-Repo
- `skill.atd` execution tree + `SkillTreeExecutor` + `ISkillActionNodeVisitor` — the pattern to copy.
- `effect.atd` extraction — how shared vocabulary gets its own file when a second consumer appears.
- Quest entity migration (this branch) — records-carry-index + positional-ordinal technique that
  the node-path records extend.

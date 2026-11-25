# fey-data - agent guide

OCaml pipeline: ATD schemas (`lib/data/*.atd`) + entity JSON -> validation -> generated Rust types. The ATD schemas are the source of truth for every Fey data type. Details for humans: [`README.md`](README.md).

## Commands
```sh
eval "$(opam env --set-switch)"       # from bash/zsh; the scripts (watch.fish, test.fish) assume fish
export PATH="$PATH:$HOME/.opam/default/bin"   # the local switch is ocaml-system; ocamlc lives in the default switch
dune build            # also regenerates lib/data/dune.inc from the *.atd glob
dune runtest
dune exec gamedata    # validate the JSON and media, then write the index and the Rust types
```
If linking fails against OpenSSL after a Homebrew upgrade, the `ssl` bindings were built against the old version: recreate the old Cellar version folder as a symlink to the new one, or reinstall `ssl`.

## Outputs (`fey-data.config`)
- Input JSON: `../fey-rs/data/json` (`json_path`), named `<name>.<type>.json`. The game data lives in `fey-rs`; the Unity project `fey` is a reference only, the pipeline neither reads nor writes it.
- Media the entities name (images, audio, models): `../fey-assets/game-media` (`media_path`, private repo); validation checks the files exist, so the pipeline needs `fey-assets` checked out next to it.
- Entity reference index: `../fey-rs/data/entity-reference-indices.json` (`entity_index_path`), rewritten on every run.
- Rust serde types: `../fey-rs/crates/fey_data/src/generated` (`rust_output_path`). Commit the regenerated output in `fey-rs`.
- Never hand-edit generated code; change the ATD schema and regenerate.

## Schema coverage
`dune exec gamedata` fails when a sum-type variant or an optional field (`?f`, or `f : T option`) reachable from the entity root is used by no entity of `data/json`, unless `coverage-allowlist.txt` names it as `module.type.name: reason` (`coverage_allowlist_path` of `fey-data.config`). `lib/rust/coverage.ml` walks the schema through the emitter's context (`Rust.Main.build_context`: one parser, one name resolution) and the JSON by the schema's types. The list can only shrink: a row that is used now, or names nothing in the schema, fails too. Delete the unused part of the schema, or give the row a real reason; do not add a row to make the run pass. Also a finding: a field that may be left unset (`?f`, `f : T option`, `~f`, or a required list) that no entity sets (null and an empty list are not a use), and a type no root reaches (roots: the entity root, `other_roots` in `bin/main.ml`: the portrait metadata and the entity index); a row `module.type` lists such a type. A row reads `key: verdict: evidence`, the verdict being `default`, `built`, `for #N` (an open issue with a tested reader) or `remove`; the check rejects another first word, a missing evidence and a duplicate key. Tests: `test/test_rust.ml` group `coverage`.

## Data stamp
A skill's description is at most `skill_description_max` (200) characters (`lib/validate.ml`, `skill_description_error_messages`; characters, not bytes): the skill card in fey-rs shows it whole and has no cut of its own, so the validation is the one owner of the number (#450). Test: `test/test_dataset.ml`.

After validation, coverage and the index passed, `gamedata` writes `data_stamp_path` (`../fey-rs/data/data-stamp.txt`, one line of hex): FNV-1a 64 over every file of `data/json` in sorted relative-path order (`lib/data_stamp.ml`; the Rust side is `fey_core::data::data_stamp`, same fixture and expected hex in `test/test_stamp.ml` and `fey-rs/crates/fey_core/tests/data_stamp.rs`). A fey-rs test refuses data whose bytes no longer match it, so run `gamedata` after any data edit; on a merge of two data branches rerun it (the stamp conflicts). It does not cover a schema change without a data change (the generated Rust can be stale with a matching stamp) nor the media. It is not the sim's data fingerprint (`GameData::fingerprint`, parsed gameplay entities, for replays and baselines).

## Adding an entity type (registration points)
1. `lib/data/<name>.atd` (the entity record; polymorphic parts are a sum with the `Type_field` JSON adapter).
2. `lib/data/common.atd`: add to the `entity_type` enum.
3. `lib/data/entity.atd`: abstract import, `<name>_entity_definition`, variant in `entity_definition_internal`.
4. `lib/dataset.ml`: `entity_reference_of_entity_definition` match and reference extraction.
5. `lib/data/validation.ml`: `validate_entity_definition_internal` match (enum-keyed entities also check the key against a helper in `entity_util.ml`).
6. `lib/parity.ml`: `test_entity_parity` match.
7. Tests: `test/test_fixtures.ml`, `test_parity.ml`, `test_dataset.ml` (all-entity-types lists).
8. Consumers: `../fey-console` (`src/models/common.types.ts` EntityType enum, a `<name>.types.ts` model, the entity count in `test/app.test.tsx`), `../fey-rs` (regenerate, round-trip test `crates/fey_data/tests/roundtrip.rs`).

## Map generation data
`generator.atd` (spec, `anchor_binding`), `room_prefab.atd` (the `RoomPrefab` entity), `stage.atd` (`?generator`), `anchor.atd` (`?binding`, repeated in every variant). `lib/mapgen_check.ml` is a port of the Rust generator's `spec_problems`, `quest::demands` and `quest::apply_demands` (fey-rs `crates/fey_core/src/mapgen`); `Validate.generator_error_messages` applies it to every stage with a `generator`. Keep both sides in step. Tests: `test/test_mapgen.ml`. Ranges reuse `int_range` / `float_range`.

## Visual effects (not an entity)
`vfx.atd` types the effect keys skills, projectiles and statuses name (`?vfx` on `skill`, on every `projectile` variant and on `status`): an element plus optional effect keys per phase (cast, release, impact, area for skills; body, trail, impact for projectiles; one effect for a status). Keys are `lowercase-words/groups` (`Validation.vfx_key`); the client owns the recipe behind a key, so the data holds no art and no colours.

## Status names (not an entity)
`lib/status_check.ml` (run by `Validate.validate_entity_definitions`): a status is never titled after a stat (its key or title, read without case, spaces or a trailing "Modifier"), a StatChange status is applied as a gain or as a loss only (the sign of its scalers against the stat's `lower_is_better`), and its `vfx.effect` family (`buff/` or `debuff/`) agrees. One status per meaning. Tests: `test/test_status_check.ml`.

## Portrait metadata (not an entity)
`lib/data/portrait.atd` types the portrait pipeline output (`portraits.json`); `dune exec portraitcheck -- <path>...` validates files against it (`lib/portrait_check.ml`). `(string * T) list <json repr="object">` is how JSON maps are declared (Rust `BTreeMap`). The pipeline lives in the private `fey-assets` repo; its `pipeline/build.py` calls the checker when `../fey-data` is present. Characters are 3D models: the `model_family` of a character (`character.atd`) is the family of its model in a pack.

## Rules
- Public repo: no personal details, absolute home paths or licensed content names (vendor names only) in code, docs or commits.
- Every commit builds and passes `dune runtest`.

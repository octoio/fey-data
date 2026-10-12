# fey-data - agent guide

OCaml pipeline: ATD schemas (`lib/data/*.atd`) + entity JSON -> validation -> generated code. The ATD schemas are the source of truth for every Fey data type. Details for humans: [`README.md`](README.md).

## Commands
```sh
eval "$(opam env --set-switch)"       # from bash/zsh; the scripts (watch.fish, test.fish) assume fish
export PATH="$PATH:$HOME/.opam/default/bin"   # the local switch is ocaml-system; ocamlc lives in the default switch
dune build            # also regenerates lib/data/dune.inc from the *.atd glob
dune runtest
dune exec gamedata    # validate the JSON, then write the generated code
```
If linking fails against OpenSSL after a Homebrew upgrade, the `ssl` bindings were built against the old version: recreate the old Cellar version folder as a symlink to the new one, or reinstall `ssl`.

## Outputs (`fey-data.config`)
- C# DTOs, types and mappers: `../fey/Assets/Scripts/Data/{Dto,Type,Mapper}` (`game_root = "../fey"`). The written files are listed in `generated.txt`, which the tool reads on the next run to delete previous outputs.
- Rust serde types: `../fey-rs/crates/fey_data/src/generated` (`rust_output_path`, skipped with a warning when `../fey-rs` is missing). Commit the regenerated Rust in `fey-rs`, the C# in `fey`.
- Input JSON: `../fey/Assets/StreamingAssets/json`, named `<name>.<type>.json`.
- Never hand-edit generated code; change the ATD schema and regenerate.

## Adding an entity type (registration points)
1. `lib/data/<name>.atd` (the entity record; polymorphic parts use the base + `<cs ignore>` variant pattern with the `Type_field` JSON adapter).
2. `lib/data/common.atd`: add to the `entity_type` enum.
3. `lib/data/entity.atd`: abstract import, `<name>_entity_definition`, variant in `entity_definition_internal`.
4. `lib/dataset.ml`: `entity_reference_of_entity_definition` match and reference extraction.
5. `lib/data/validation.ml`: `validate_entity_definition_internal` match (enum-keyed entities also check the key against a helper in `entity_util.ml`).
6. `lib/parity.ml`: `test_entity_parity` match.
7. Tests: `test/test_fixtures.ml`, `test_parity.ml`, `test_dataset.ml` (all-entity-types lists).
8. Consumers: `../fey-console` (`src/models/common.types.ts` EntityType enum, a `<name>.types.ts` model, the entity count in `test/app.test.tsx`), `../fey` (`GameStorageData.cs` implements the generated visitor: add a dictionary and a `Visit` method; new polymorphic families register their converter in `Data/Mapper/JsonDeserializer.cs`), `../fey-rs` (regenerate, round-trip test `crates/fey_data/tests/roundtrip.rs`).

## Map generation data
`generator.atd` (spec, `anchor_binding`), `room_prefab.atd` (the `RoomPrefab` entity), `stage.atd` (`?generator`), `anchor.atd` (`?binding`, repeated in every variant). `lib/mapgen_check.ml` is a port of the Rust generator's `spec_problems`, `quest::demands` and `quest::apply_demands` (fey-rs `crates/fey_core/src/mapgen`); `Validate.generator_error_messages` applies it to every stage with a `generator`. Keep both sides in step. Tests: `test/test_mapgen.ml`. Ranges reuse `int_range` / `float_range`; there are no tuples or payload sums in these types because the C# generator does not take them.

## Visual effects (not an entity)
`vfx.atd` types the effect keys skills, projectiles and statuses name (`?vfx` on `skill`, on every `projectile` variant and on `status`): an element plus optional effect keys per phase (cast, release, impact, area for skills; body, trail, impact for projectiles; one effect for a status). Keys are `lowercase-words/groups` (`Validation.vfx_key`); the client owns the recipe behind a key, so the data holds no art and no colours.

## Sprite metadata (not an entity)
`lib/data/sprite.atd` types the sprite pipeline output (`sprites.json` v3, `portraits.json`); every type is `<cs ignore>` (Rust only). `dune exec spritecheck -- <path>...` validates files against it (`lib/sprite_check.ml`). `(string * T) list <json repr="object">` is how JSON maps are declared (Rust `BTreeMap`). The pipeline lives in the private `fey-assets` repo; its `pipeline/build.sh` calls the checker when `../fey-data` is built.

## Rules
- Public repo: no personal details, absolute home paths or licensed content names (vendor names only) in code, docs or commits.
- Every commit builds and passes `dune runtest`.

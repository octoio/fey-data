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

## Rules
- Public repo: no personal details, absolute home paths or licensed content names (vendor names only) in code, docs or commits.
- Every commit builds and passes `dune runtest`.

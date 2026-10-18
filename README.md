# Fey Data

A type-safe data pipeline for game development that validates JSON entity definitions and generates the Rust serde types of the game.

## What it does

This OCaml-based tool processes JSON game data files (weapons, characters, skills, etc.) and generates the serde types of the Rust game (`fey-rs`), while validating entity relationships and ensuring data integrity.

**Input:** JSON entity definitions (in `fey-rs/data/json`) → **Output:** validation + Rust serde types + the entity reference index

## Quick Start

```bash
# Setup (requires OCaml 5.2.0+)
opam switch create . 5.2.0
eval $(opam env --set-switch)
opam install . --deps-only

# Configure paths (edit fey-data.config as needed)
# the paths default to the sibling repos fey-rs (the JSON, the index, the Rust output) and fey-assets (the media)

# Run the pipeline
dune exec gamedata

# Development mode (auto-rebuild on changes)
fish watch.fish
```

## How it works

```
JSON Files → Validation → Entity Processing → Rust Generation → fey-rs
```

1. **JSON Input**: Game entities in `json_path` (`../fey-rs/data/json/` with the default config)
   - Format: `<name>.<type>.json` (e.g., `sword.weapon.json`)
   - Types: weapon, character, skill, equipment, audio, animation, etc.

2. **Validation**: Ensures data integrity
   - Schema validation
   - Cross-reference checking
   - Type safety
   - The media files an entity names (images, audio, models) exist in `media_path` (`../fey-assets/game-media/`, private repo)

3. **Entity reference index**: written to `entity_index_path` (`../fey-rs/data/entity-reference-indices.json`).

4. **Rust Generation**: serde types written to `rust_output_path` (default `../fey-rs/crates/fey_data/src/generated`). See `lib/rust/README.md`.

## Repository Structure

```
├── lib/                    # Core OCaml modules
│   ├── config/             # Configuration constants
│   │   └── constants.ml    # Config.Constants.*
│   ├── data/               # Entity type definitions
│   │   ├── *.atd          # Entity type schemas
│   │   ├── entity_util.ml # Data.Entity_util.*
│   │   └── validation.ml  # Data.Validation.*
│   ├── util/               # Generic utilities
│   │   ├── string.ml      # Util.String.*
│   │   └── entity.ml      # (placeholder)
│   ├── rust/               # Rust (serde) code generation
│   ├── processing.ml       # Main pipeline orchestrator
│   ├── dataset.ml          # Data management
│   ├── validate.ml         # Validation logic
│   ├── io.ml              # File operations
│   └── server.ml          # Web interface
├── bin/                   # Executables
├── test/                  # Comprehensive test suite
├── fey-data.config       # Configuration file for paths
├── watch.fish            # Development file watcher
└── test.fish             # Test runner
```

## Module Structure

The codebase follows a clean modular architecture:

- **`Config.Constants.*`** - Configuration paths and settings
- **`Data.*`** - Entity types, validation, and utilities  
- **`Util.*`** - Generic utility functions
- **`Gamedata.*`** - Core processing and pipeline logic

## Configuration

The `fey-data.config` file allows customization of input/output paths:

```
# All paths are relative to this file
json_path = "../fey-rs/data/json"
media_path = "../fey-assets/game-media"
entity_index_path = "../fey-rs/data/entity-reference-indices.json"
rust_output_path = "../fey-rs/crates/fey_data/src/generated"
```

The pipeline reads and writes the sibling repos `fey-rs` and `fey-assets` (the latter is private; without it the media check fails).

## Entity Types

Supports various game entity types including:
- **Combat**: weapon, weapon_class (the animation set of a weapon category), character, skill, equipment, status
- **Assets**: audio, animation, model, image, cursor; `visual.atd` (the model layer an item or monster is drawn with)
- **System**: quality, stat, drop, affix, requirement
- **Map generation**: stage (an optional `generator` spec), anchor (an optional `binding` to a generated slot), room_prefab (a hand-made room); see below

Each type has its own ATD schema defining structure and validation rules.

### Map generation data

A stage may carry a `generator` spec (`lib/data/generator.atd`: room requirements, connection rules, forbidden adjacencies,
pinned prefabs with an optional fixed position and door side, par rules, seed policy), its anchors a `binding` (slot and
ordinal) and `RoomPrefab` entities (`room_prefab.atd`) the hand-made rooms. The generator itself lives in Rust only
(`fey-rs`, `fey_core::mapgen`). `lib/mapgen_check.ml` ports the generator's
static checks, so an infeasible spec or a quest the spec cannot hold is a build error (`dune exec gamedata`): spec
feasibility (counts against the main path, farthest and chokepoint rules, spacing, prefabs that exist and match the
generator, impossible pins, seed policy, par), every anchor of a generated stage bound with contiguous ordinals, the quest
demands (`demands` and `apply_demands`: the anchors a quest names raise the spec's requirements) still satisfiable, room
prefab ids unique and slots inside the room. The Rust code is the reference; change both together. Design:
`fey-rs/docs/design/mapgen.md`.

### Portrait metadata

`lib/data/portrait.atd` is not an entity: it is the schema of the asset pipeline's portrait output
(`portraits/portraits.json`). It generates the Rust types (`fey_data::portrait`). Validate the files with

```bash
dune exec portraitcheck -- <portraits.json | directory>...
```

which reads the shape through the generated reader (unknown fields are refused) and then checks what spans fields
(`lib/portrait_check.ml`): rects inside their atlas and cell, every layer having every frame, slots naming existing layers.

## Key Features

- **Type Safety**: OCaml's type system ensures correctness
- **Modular Architecture**: Clean separation of concerns
- **Validation**: Comprehensive error checking and reporting
- **Hot Reload**: File watcher for immediate feedback
- **Rust Integration**: Generated serde types for the game
- **Entity References**: Validates cross-entity relationships
- **Performance**: Efficient indexing and hashing

## Testing

```bash
# Run all tests
dune runtest

# Comprehensive test suite with setup
fish test.fish

# With coverage
dune runtest --instrument-with bisect_ppx
```

**Test Coverage**: Comprehensive tests across multiple test modules covering all core functionality.

## License

See LICENSE file for details.

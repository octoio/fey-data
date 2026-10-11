# Fey Data

A type-safe data pipeline for game development that converts JSON entity definitions into Unity C# classes and Rust serde types.

## What it does

This OCaml-based tool processes JSON game data files (weapons, characters, skills, etc.) and generates corresponding C# classes for Unity and serde types for the Rust port, while validating entity relationships and ensuring data integrity.

**Input:** JSON entity definitions → **Output:** Unity C# classes + Rust serde types + validation

## Quick Start

```bash
# Setup (requires OCaml 5.2.0+)
opam switch create . 5.2.0
eval $(opam env --set-switch)
opam install . --deps-only

# Configure paths (edit fey-data.config as needed)
# game_root defaults to ../fey (the game); rust_output_path points into ../fey-rs

# Run the pipeline
dune exec gamedata

# Development mode (auto-rebuild on changes)
fish watch.fish
```

## How it works

```
JSON Files → Validation → Entity Processing → C# + Rust Generation → Unity / Rust assets
```

1. **JSON Input**: Game entities in `<game_root>/Assets/StreamingAssets/json/` (`../fey/Assets/StreamingAssets/json/` with the default config)
   - Format: `<name>.<type>.json` (e.g., `sword.weapon.json`)
   - Types: weapon, character, skill, equipment, audio, animation, etc.

2. **Validation**: Ensures data integrity
   - Schema validation
   - Cross-reference checking
   - Type safety

3. **C# Generation**: Creates Unity-compatible classes
   - Output: `<game_root>/Assets/Scripts/Data/{Dto,Type,Mapper}/` (the folder follows the namespace `Octoio.Fey.Data.*`; the list of written files goes to `generated.txt`)
   - Includes `[Serializable]` attributes
   - Generates entity reference indices

4. **Rust Generation**: serde types written to `rust_output_path` (default `../fey-rs/crates/fey_data/src/generated`; skipped with a warning when that parent directory is missing). See `lib/rust/README.md`.

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
│   ├── csharp/             # C# code generation
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
# Game project root, relative to this file
game_root = "../fey"

# StreamingAssets folder location
streaming_assets_path = "Assets/StreamingAssets"

# JSON data subfolder
json_path = "json"

# Legacy C# folder setting; the generator currently writes the C# under
# <game_root>/Assets/Scripts (Data/Dto, Data/Type, Data/Mapper) whatever this says
scripts_path = "Assets/Scripts/Octoio/Fey/Data/Dto"

# Rust output directory (relative to this file); skipped when its parent is missing
rust_output_path = "../fey-rs/crates/fey_data/src/generated"
```

## Entity Types

Supports various game entity types including:
- **Combat**: weapon, character, skill, equipment, status
- **Assets**: audio, animation, model, image, cursor
- **System**: quality, stat, drop, affix, requirement

Each type has its own ATD schema defining structure and validation rules.

## Key Features

- **Type Safety**: OCaml's type system ensures correctness
- **Modular Architecture**: Clean separation of concerns
- **Validation**: Comprehensive error checking and reporting
- **Hot Reload**: File watcher for immediate feedback
- **Unity Integration**: Generated C# classes with proper attributes
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

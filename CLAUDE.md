# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains **fey-data**, an OCaml-based data processing pipeline that validates JSON game data definitions, manages entity relationships, and generates type-safe C# code for Unity integration. It serves as the data processing layer in the Fey Game Development Ecosystem.

## Key Commands

### Environment Setup
```bash
# Set up OCaml environment (requires OCaml 5.2.0+)
opam switch create . 5.2.0
eval $(opam env --set-switch)
opam install . --deps-only
```

### Build & Run
```bash
# Build the project
dune build

# Run the main data processing pipeline
dune exec gamedata

# Run the web server interface
dune exec server
```

### Testing
```bash
# Run all tests
dune runtest

# Run tests with coverage (if bisect_ppx is available)
dune runtest --instrument-with bisect_ppx

# Run comprehensive test suite with setup
fish test.fish

# Run specific test executable
dune exec test/test_gamedata.exe
```

### Development Workflow
```bash
# Start file watcher for automatic rebuild on changes
fish watch.fish

# Clean build artifacts
dune clean
```

## Architecture

### Core Data Flow
1. **JSON Input**: Game entity definitions from `../fey-game-mock/Assets/StreamingAssets/json/`
2. **Processing Pipeline**: `processing.ml` orchestrates file scanning and validation
3. **Dataset Management**: `dataset.ml` manages entity definitions and error collection
4. **Validation**: `validate.ml` ensures data integrity and reference consistency
5. **C# Generation**: `lib/csharp/` modules generate Unity-compatible C# classes
6. **Output**: Generated files for Unity and entity indices

### Key Modules
- **`processing.ml`**: Main pipeline orchestrator that processes JSON files
- **`dataset.ml`**: Core data structure for managing entity definitions and errors
- **`validate.ml`**: Validation logic for entity definitions and cross-references
- **`config.ml`**: Configuration constants for paths and namespaces
- **`lib/csharp/main.ml`**: C# code generation from ATD type definitions
- **`io.ml`**: File I/O operations and utilities
- **`server.ml`**: Web interface for the data pipeline

### Module Structure
The codebase follows a clean modular architecture:
- **`Config.Constants.*`** - Configuration paths and settings
- **`Data.*`** - Entity types, validation, and utilities
- **`Util.*`** - Generic utility functions
- **`Gamedata.*`** - Core processing and pipeline logic

### Entity System
The system supports 16 game entity types defined in ATD files:
- **Combat**: Weapon, Character, Skill, Equipment, Status
- **Assets**: AudioClip, Sound, SoundBank, Animation, AnimationSource, Model, Image, Cursor
- **Systems**: Stat, Quality, DropTable

### File Naming Convention
JSON files must follow: `<name>.<type>.json`
- Example: `sword.weapon.json`, `hero.character.json`

## Configuration

### Key Paths (configured in `config.ml`)
- JSON source: `../fey-game-mock/Assets/StreamingAssets/json/`
- C# output: `../fey-game-mock/Assets/Scripts/`
- Base namespace: `Octoio.Fey`
- Entity indices: `../fey-game-mock/Assets/StreamingAssets/entity-reference-indices.json`

### Configuration File
The `fey-data.config` file contains customizable paths:
- `game_root`: Path to Unity project
- `streaming_assets_path`: StreamingAssets folder path
- `json_path`: JSON data subfolder
- `scripts_path`: C# output path

## C# Code Generation

### Architecture
The C# generation system is modular with these key components:
- **`types.ml`**: Core type definitions and data structures
- **`utils.ml`**: Utility functions for type conversion and parsing
- **`annotations.ml`**: ATD annotation parsing and configuration
- **`generator_types.ml`**: Basic type generation (structs, classes, enums)
- **`generator_visitor.ml`**: Visitor pattern generation
- **`generator_converter.ml`**: JSON converter generation
- **`output.ml`**: File output and formatting
- **`main.ml`**: Main generation orchestration

### Generated Output
- C# classes with `[Serializable]` attributes for Unity
- Proper namespace organization (`Octoio.Fey.Data.Dto`)
- Type-safe property definitions
- Entity reference indices for runtime lookup

## Development Notes

### Testing Strategy
- Unit tests for core modules in `test/` directory
- Integration tests for full pipeline
- Property-based testing with QCheck
- Coverage reporting with bisect_ppx

### File Watching
The `watch.fish` script provides:
- Automatic OCaml environment setup
- File monitoring for JSON and OCaml changes
- Automatic test execution before build
- Filtered watching (excludes build artifacts)

### Error Handling
The system provides comprehensive error reporting for:
- Invalid JSON structure
- Missing entity references
- Type mismatches
- Duplicate entity definitions
- Invalid file naming patterns

### ATD Type Definitions
Entity types are defined in `lib/data/*.atd` files with:
- Type-safe field definitions
- Validation constraints
- C# generation annotations
- JSON serialization configuration

## Important Notes

### Data Integrity
- All entity references are validated against existing definitions
- Duplicate entity definitions are detected and reported
- Cross-reference validation ensures referential integrity

### Unity Integration
- Generated C# files are automatically placed in the Unity project
- Entity indices provide runtime lookup capabilities
- StreamingAssets contain both JSON data and reference indices

### Performance
- Efficient indexing using SHA1 hashes
- Collision handling for entity indices
- Optimized file watching with filtered patterns
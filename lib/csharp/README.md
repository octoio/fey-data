# C# Code Generation Module Structure

This document describes the modular structure of the C# code generation system that was split from the original monolithic `csharp.ml` file.

## Module Overview

The C# code generation functionality is now organized into the following modules:

### Core Modules

1. **`types.ml`** - Core type definitions
   - `line_type` - Defines different types of code lines
   - `line` - Represents a single line of code
   - `cs_file` - Represents a complete C# file
   - `generator_config` - Configuration for generators
   - Common constants like `blank_line`, `open_bracket`, etc.

2. **`utils.ml`** - Utility functions
   - `parse_atd_file` - Parses ATD files
   - `is_primitive_type` - Checks if a type is primitive
   - `to_pascal_case` / `to_camel_case` - Case conversion functions
   - `get_annotation_value` - Extracts annotation values
   - `generate_csharp_type` - Converts ATD types to C# types
   - `extract_base_class_name` - Extracts base class names
   - `get_variant_info` - Extracts variant information

3. **`annotations.ml`** - Annotation parsing and configuration
   - `parse_generator_config` - Parses generator configuration strings
   - `split_generators` - Splits comma-separated generators
   - `get_annotation_generators` - Gets all generators from annotations
   - `has_generator` / `get_generator_config` - Generator checks
   - `get_json_name` - Gets JSON field names from annotations

### Generator Modules

4. **`generator_types.ml`** - Basic type generation (structs, classes, enums)
   - `generate_imports` - Generates using statements
   - `generate_namespace` - Generates namespace declarations
   - `generate_csharp_field` - Generates class/struct fields
   - `generate_csharp_enum` - Generates enum definitions
   - `generate_csharp_struct_from_type` - Generates struct/class definitions

5. **`generator_visitor.ml`** - Visitor pattern generation
   - `generate_visitor_interface` - Creates visitor interfaces
   - `generate_abstract_base_class` - Creates abstract base classes
   - `generate_consolidated_visitables` - Creates visitable implementations
   - `generate_visitor_generators` - Main visitor generation orchestrator

6. **`generator_converter.ml`** - JSON converter generation
   - `generate_json_converter` - Creates Newtonsoft.Json converters
   - `generate_converter_generators` - Main converter generation orchestrator

### Output Module

7. **`output.ml`** - File output and formatting
   - `remove_base_namespace` - Removes base namespace from full paths
   - `indent_line` - Handles line indentation
   - `print_lines` - Formats lines with proper indentation
   - `string_of_csharp_code` - Converts lines to formatted string
   - `write_cs_file` - Writes C# files to disk

### Main Orchestrator

8. **`main.ml`** - Main generation orchestration
   - `generate_csharp_struct` - Orchestrates struct/class generation
   - `generate_csharp_enum_from_module` - Orchestrates enum generation  
   - `generate_csharp` - Main entry point for all generation

## Dependencies

The modules have the following dependency structure:

```
main.ml
├── types.ml
├── utils.ml (depends on types.ml)
├── annotations.ml (depends on types.ml, utils.ml)
├── generator_types.ml (depends on types.ml, utils.ml, annotations.ml)
├── generator_visitor.ml (depends on types.ml, utils.ml, annotations.ml)
├── generator_converter.ml (depends on types.ml, utils.ml, annotations.ml)
└── output.ml (depends on types.ml)
```

## Build Configuration

The modules are built as a single library called `gamedata.csharp` with dependencies on:
- `atd` - For ATD parsing
- `gamedata` - For IO utilities

## Usage

The main entry points from external code are:
- `Csharp.Utils.parse_atd_file` - Parse ATD files
- `Csharp.Main.generate_csharp` - Generate C# code from parsed modules
- `Csharp.Output.write_cs_file` - Write generated files to disk

## Benefits of the Split

1. **Modularity** - Each module has a clear, focused responsibility
2. **Maintainability** - Easier to understand and modify individual components
3. **Testability** - Each module can be tested independently
4. **Reusability** - Modules can be reused in different contexts
5. **Build Performance** - Only changed modules need recompilation

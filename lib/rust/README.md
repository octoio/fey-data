# Rust code generation

Emits serde types into `fey-rs/crates/fey_data/src/generated` (path: `rust_output_path` in `fey-data.config`; skipped with a warning when `fey-rs` is not checked out). Mirrors `lib/csharp/`.

| Module | Role |
|---|---|
| `types.ml` | `rs_file`, `entry` (one ATD type), `context` (all modules + recursion analysis) |
| `utils.ml` | ATD parsing, naming, Rust keyword escaping, primitive mapping |
| `annotations.ml` | `<rs ...>` and `<json ...>` annotation access |
| `generator_types.ml` | ATD type -> Rust type, structs, unit enums, payload enums, aliases |
| `generator_serde.ml` | hand-written `Serialize`/`Deserialize` for sums with payloads |
| `runtime.ml` | the fixed `runtime.rs` helper module (atdgen JSON conventions) |
| `output.ml`, `main.ml` | context building (imports, Box analysis), one file per ATD module, `mod.rs` |

## Mapping
- `int` -> `i64` (also read from JSON strings like `"1"`, as atdgen does), `float` -> `f64`, `string` -> `String`, `bool`, `T list` -> `Vec<T>`, tuples -> tuples.
- `?f: T option` -> `Option<T>` omitted when `None`; `f: T option` -> `"None"` / `["Some", x]`; `nullable` -> `Option<T>` as `null`.
- Records -> structs (`<json name="x">` -> `#[serde(rename)]`); sums without payloads -> enums that derive serde (`"Tag"`).
- Sums with payloads -> custom serde impls: `["Tag", payload]`, or `{"type": "Tag", ...}` with `<json adapter.ocaml="Atdgen_runtime.Json_adapter.Type_field">` (the payload is the whole object, as in atdgen).
- `type x <ocaml from="Mod"> = abstract` -> `use super::mod::X;`. Types that contain themselves inline (through fields, options, payloads, not `Vec`) get a `Box`.
- Unsupported (fails loudly): type parameters, anonymous nested records/sums, `inherit`, other JSON adapters.

## `<rs ...>` annotations (C# `<cs ...>` equivalents; `<cs ...>` is ignored)
`ignore="true"` (type or field), `name="Foo"` (type), `usetype="Foo"` (field), `attributes="#[a];#[b]"`, `derive="Eq,Hash"`.

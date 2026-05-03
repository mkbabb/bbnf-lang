# Cookbook — `pointer!` Path Macro

The `pointer!` macro is bbnf-lang's compile-time typed-path constructor. It accepts heterogeneous segments (`&str` keys for object indexing, `usize` indices for array indexing), validates them against the grammar's `pub const REGISTRY: StructRegistry` at compile time, and emits a `TypedPath<G, T>` whose terminal type `T` is known statically. Runtime evaluation of a typed path against a parsed value yields a `LazyValue<'a>` borrowed view; `.as_str()`, `.as_i64()`, `.as_<T>()` materialise on demand.

This page covers: the mental model (§1), the two syntax forms (§2), per-grammar examples (§3), error messages (§4), and troubleshooting (§5). The macro lands at `crates/path/src/path_macro.rs`; the lex/lower/validate logic lives at `crates/path-core/`.

## §1 Model

The `pointer!` macro builds a compile-time path AST and validates each segment against the grammar's emitted `StructRegistry`. The registry is a generated table — one entry per typed-record type — that the path-validation logic walks to type-check segment compatibility.

```text
pointer!(Grammar, [seg1, seg2, ...])
   |
   v
   1. parse syntax (proc-macro)
   2. lookup Grammar's StructRegistry via the typed alphabet
   3. type-resolve each segment: str-key → object-field; usize → array-index
   4. emit TypedPath<Grammar, TerminalType>
```

Compile-time validation:
- Segment 0 must address a typed-record root (the grammar's top-level value type).
- Segment 1 indexes into segment 0's resolved layout; if the layout is a struct, segment 1 is a field-name; if the layout is an array, segment 1 is a usize.
- Segment N continues the recursion until the terminal segment lands on a leaf type (string, number, bool, ...) or a recursive type.

Runtime evaluation:
- `<G>Value::pointer(input, &path) -> LazyValue<'a>` returns a borrowed view carrying the slice + a type tag.
- `LazyValue::as_<T>()` materialises the value (parses the slice into the typed terminal).
- `LazyValue::owned()` clones into owned scope when the input lifetime cannot extend.

The macro is grammar-aware: a `pointer!(Json, ...)` invocation resolves against `JsonRegistry`; a `pointer!(CssL4, ...)` invocation resolves against `CssL4Registry`. The 9 grammars each carry their own registry; the path's terminal type is grammar-specific.

## §2 Syntax

The macro accepts two forms per the syntax decision at `docs/tranches/BB/audit/W5-pointer-syntax-decision.md`:

### Explicit marker (always works)

```rust
let p = pointer!(Json, ["a", "b", 1]);
//      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
// → TypedPath<Json, JsonValue> (or whatever the resolved terminal is)
```

The first token is the grammar identifier; the second is a bracket-expression containing the segments. The grammar marker is mandatory in this form; the macro validates segments against the named grammar's registry at expansion time.

### Implicit (typed-context inference)

```rust
let p: JsonPath<JsonString> = pointer!["users", 0, "name"];
//                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
// → TypedPath<Json, JsonString> via call-site type ascription
```

The macro emits a `PointerInferred` wrapper carrying unresolved segments; the `From<PointerInferred> for TypedPath<G, T>` impl runs resolution at the call site where `G` (and the terminal type) are known. The implicit form REQUIRES a typed context — either a `let` ascription, a function-argument type, or a struct-field type.

### Decision flowchart

```text
                 Where will the path be used?
                          |
        +-----------------+-----------------+
        |                                   |
        v                                   v
   Hot-path callsite              Function-argument
   (no type context)              (typed receiver)
        |                                   |
        v                                   v
   pointer!(Grammar, [...])        pointer![...]
   (explicit form)                 (implicit form;
                                    typed-context infers)
```

When in doubt, use the explicit form. The implicit form is for ergonomic typed-context positions (struct-field initialisers, function-argument positions where the receiver constrains `<G, T>`).

## §3 Examples

### JSON

```rust
use bbnf::path::pointer;
use bbnf::grammar::json::JsonValue;

let json = r#"{"users": [{"name": "Ada"}, {"name": "Babbage"}]}"#;
let parsed = bbnf::grammar::json::parse(json)?;

// Explicit form
let p1 = pointer!(Json, ["users", 0, "name"]);
let v1 = parsed.pointer(&p1)?.as_str()?;  // → "Ada"

// Implicit form (typed-context inference)
let p2: JsonPath<JsonString> = pointer!["users", 1, "name"];
let v2 = parsed.pointer(&p2)?.as_str()?;  // → "Babbage"

// Argument-context inference
fn lookup(p: JsonPath<JsonString>) -> Result<&str, _> { ... }
lookup(pointer!["users", 0, "name"])?;  // implicit via fn signature
```

### CSS L4

```rust
use bbnf::path::pointer;
use bbnf::grammar::css_l4::CssTypedValue;

let css = r#".foo { color: red; }"#;
let sheet = bbnf::grammar::css_l4::parse(css)?;

// Path into the rule's declaration block
let p = pointer!(CssL4, ["rules", 0, "declarations", 0, "value"]);
let value = sheet.pointer(&p)?;  // LazyValue<'_> over the CSS value slice
let color = value.as_color()?;   // → CssColor::Named(NamedColor::Red)
```

### BBNF (self-host)

```rust
use bbnf::path::pointer;

let bbnf_src = r#"foo := bar | baz;"#;
let ast = bbnf::grammar::bbnf::parse(bbnf_src)?;

// Path into a rule's first alt
let p = pointer!(Bbnf, ["rules", 0, "body", "alts", 0]);
let alt = ast.pointer(&p)?.as_alt()?;
```

### Sheets (heterogeneous keys)

```rust
let sheet_formula = r#"=SUM(A1:B10) + AVERAGE(Sheet2!C:D)"#;
let parsed = bbnf::grammar::google_sheets::parse(sheet_formula)?;

let p = pointer!(GoogleSheets, ["body", "args", 0, "range_ref"]);
let range = parsed.pointer(&p)?.as_range_ref()?;
```

## §4 Errors

All trybuild-tested at `crates/path/tests/error_messages/`. Each error has a corresponding fixture verifying the verbatim text.

### Ambiguity (implicit form, no type context)

```text
error: pointer![] used outside a typed context; cannot infer grammar.
   --> src/main.rs:5:13
    |
  5 |     let p = pointer!["a", "b", 1];
    |             ^^^^^^^^^^^^^^^^^^^^^
    |
help: either annotate the binding with the typed-path destination
    |     let p: JsonPath<JsonValue> = pointer!["a", "b", 1];
help: or use the explicit marker form
    |     let p = pointer!(Json, ["a", "b", 1]);
```

### Segment-type mismatch

```text
error: pointer![] segment 2 of type 'usize' indexes into rule 'object',
       whose values are JsonObject — did you mean a string key?
   --> src/main.rs:8:18
    |
  8 |     pointer!(Json, ["users", 5, "name"])
    |                              ^
    |
note: rule 'users' resolves to JsonObject<{"name": JsonString, "email": JsonString, ...}>
```

The error names the offending segment (segment 2), the expected segment type (string key for an object), and the actual resolved layout (JsonObject with named fields). The fix is to use a string key matching one of the object's fields.

### Invalid grammar marker

```text
error: pointer!(<grammar>, ...) — unknown grammar 'JaSon'.
   --> src/main.rs:5:14
    |
  5 |     pointer!(JaSon, ["a", "b"])
    |              ^^^^^
    |
help: known grammars: Json, Bbnf, CssL4, GoogleSheets, Bnf, Csv, Ebnf, CssPretty, Math
```

The grammar marker is case-sensitive; typos in the grammar name produce this error with the full known-grammar list as a fix-it.

### Out-of-bounds index

```text
error: pointer![] segment 3 of type 'usize 5' is out of bounds;
       array layout has fixed length 3.
   --> src/main.rs:9:25
    |
  9 |     pointer!(Json, ["users", 5])
    |                              ^
```

When the array's compile-time length is known (e.g., a typed tuple), out-of-bounds usize segments fail at expansion. For dynamic-length arrays, this validation is deferred to runtime (returns `Err(ParseErr::out_of_bounds)`).

## §5 Troubleshooting

### "the macro expands but I don't know which grammar it picked"

You used the implicit form without a typed context. The macro emitted `PointerInferred`; the call site must constrain the type via ascription. Add `let p: JsonPath<...> = pointer![...];` or use the explicit form.

### "I have a `Vec<JsonValue<'_>>` in my struct field; the path doesn't compile"

The implicit form's typed-context inference walks one level only. For nested generics, use the explicit form: `pointer!(Json, ["data"])`. The explicit form fully resolves; the implicit form requires direct type ascription.

### "the `pointer!(CssL4, ...)` is rejected even though the path looks right"

CSS L4's path semantics differ from JSON: CSS values nest through `rules → declarations → value`. The error message names the resolved layout — read it carefully. The cookbook example at §3 shows the canonical CSS L4 path shape.

### "I want to traverse with wildcards"

The macro does not yet support wildcards (e.g., `pointer!(Json, ["users", *, "name"])` to traverse all users' names). The roadmap-deferred wildcard syntax routes through `pointer!(Json, ["users", _])` returning `LazyArray<'a>`, then `.iter().map(|item| item.pointer(...))`. Until wildcards land, manual iteration is the workaround.

### "my error message doesn't match the cookbook"

The trybuild test fixtures at `crates/path/tests/error_messages/` are the source of truth. If the cookbook's verbatim text diverges from the trybuild fixture, the fixture wins; file an issue. The fixtures verify the error message character-for-character against the macro's emitted diagnostic.

### Performance considerations

- `pointer!` macro expansion is compile-time only; runtime cost is zero.
- `LazyValue::pointer(input, &path)` is ≤ 0.1× full-parse cost (sonic-rs ratio per `audit/SOTA-2026-05-03.md:34`); it walks the input slice without re-parsing the entire document.
- `.as_<T>()` materialises only the terminal segment's slice; cost is bounded by slice length, not document length.
- `.owned()` clones the slice into owned storage; use only when the input lifetime cannot outlive the result.

The path-API is sonic-class: ≤ 0.1× full-parse for borrow-mode access; equal to the SOTA reference.

# Skinny Emitter Overfit Audit

**Scope:** `skinny/crates/codegen/src/` — the `runtime_generator.rs` dispatch spine and the three
emit bodies: `json_sink_direct/`, `css_scan_direct/`, and `general_descent/`.

---

## Architecture Overview

The emitter has a two-stage dispatch:

1. **Artifact selection** (`runtime_generator::lower_for_runtime`): attempts
   `lower::css_scan::derive_program` first; if it succeeds the artifact is
   `LoweredArtifact::Scan`; on failure it calls `lower_sink_only_from_source` and the artifact is
   `LoweredArtifact::SinkOnly`.

2. **Sink-only sub-dispatch** (`runtime_generator::emit_sink_only`): if the `SinkOnlyProgram`
   carries a rule named `"value"` the call goes to `json_sink_direct::render`; otherwise it goes
   to `general_descent::render`.

The documentation asserts both gates are purely shape-derived. The evidence below contradicts
that assertion for both stages.

---

## Stage 1: CSS (Scan) Path

### Grammar-derived items

`lower::css_scan::derive_program` reads the grammar's `.bbnf` source files and harvests a
`ScanAlphabet` and an `at_rule_branches` list from the rule terminals. The alphabet
(`BLOCK_OPEN`/`BLOCK_CLOSE`/`TERMINATOR`/`COLON`/`SKIP_ALPHABET` and the four STOP arrays)
is rendered into `generated.rs` as named constants and those constants are the only delimiter
arguments the driver calls `css_balanced_component_scan` with. A mutation to any invoking rule
propagates into the derived constant; the arg-mutation falsifier test at
`lower/css_scan.rs:691` confirms this is load-bearing.

### Hardwired grammar-name lookups in the CSS lowering stage

`lower/css_scan.rs::derive_alphabet` looks up four rule names by literal string:

- `"ruleBlock"` at line 490
- `"atRuleBody"` at line 501
- `"cssString"` at line 533
- `"atRule"` at line 617

`declaration_rule` (line 575) additionally filters on `rule.rel_path.ends_with("properties.bbnf")`
and `rule.name.ends_with("Decl")`. Both the file-name suffix and the naming convention
(`*Decl`) are CSS L4 specifics; no other grammar in the workspace uses this shape.

`collect_family_bytes` (line 592) is parameterized by suffix but its only call sites are
`"selectors.bbnf"`, again CSS L4 only.

**Verdict for CSS lowering:** The alphabet derivation is genuinely grammar-derived in the sense
that the byte values come from the grammar text. However, the discovery logic is hardwired to CSS
L4's exact rule names (`ruleBlock`, `atRuleBody`, `cssString`, `atRule`) and file naming
conventions (`properties.bbnf`, `*Decl`). A second CSS dialect with different structural rule
names would produce `CodegenError::Lowering` ("css_scan: missing `ruleBlock` rule") — not a
graceful Err(\_) that falls through to SinkOnly, but a named-rule failure. The `derive_program`
function is therefore not a generic "does this grammar have a balanced-delimiter structure?" probe;
it is a "does this grammar look exactly like css_l4?" gate.

### Hardwired literals in the CSS driver (`css_scan_direct/drivers.rs`)

The emitted `CssFullParser` contains five hardwired byte sequences that do not come from the
`ScanAlphabet`:

- `b"/*"` at line 209 — comment start (the alphabet carries only the single `/` lead, not the
  two-byte sequence)
- `b"<!--"` at line 218 — CDO marker
- `b"-->"` at line 221 — CDC marker
- `b"@charset"` at line 236 — charset directive prefix
- `b"@charset".len()` at line 239 — same

The CDO/CDC markers (`<!--` / `-->`) and the `@charset` skip are not derivable from the grammar
AST; they are CSS-specific tokenizer rules not expressed in any `.bbnf` file. They cannot be
parameterized without extending the grammar language itself.

### Hardwired projection types in `css_scan_direct/projection.rs`

`render_node_kind` (line 14) emits a fixed three-variant `CssNodeKind` enum
(`QualifiedRule`/`AtRule`/`Declaration`). `render_typed_value` (line 261) emits a six-variant
`CssTypedValue` enum (`Dimension`/`Number`/`Color`/`Function`/`Keyword`/`Other`) with hardwired
classification logic. Neither enum is derived from the grammar's rule structure; both are manual
encodings of CSS semantics. A CSS grammar with a different structural vocabulary (e.g.
`styleRule` instead of `ruleBlock`) would still emit these same fixed type names.

**Verdict for CSS emit body:** The delimiter bytes passed to `css_balanced_component_scan` are
grammar-derived. The driver control flow (parse_stylesheet / parse_at_rule / parse_qualified_rule
/ parse_block / parse_declaration), the tokenizer-level skips (`<!--`, `-->`, `@charset`, `/*`),
and the typed projection layer (`CssNodeKind`, `CssTypedValue`) are hardwired to CSS semantics.
Fraction of the CSS hot path that is grammar-derived: the scan alphabet (~30% of the
`drivers.rs` control surface); the remaining ~70% is CSS-only logic with no grammar derivation.

---

## Stage 2: JSON (json_sink_direct) Path

### The "value" gate is a grammar-name probe

`runtime_generator.rs:98`:

```rust
if !sink_only.has_rule("value") {
    return emit_general_descent(profile_id, sink_only);
}
```

This is the sole criterion for choosing `json_sink_direct`. Any grammar that happens to have a
rule named `"value"` — CBOR, TOML, MessagePack, any format with a conventional `value`
top-level dispatch — would be routed here and would likely fail in `validate()` or panic inside
the skeleton emitter.

### Hardwired rule names inside json_sink_direct

`json_sink_direct/mod.rs:98`:

```rust
.find(|rule| rule.name == "value")
```

`json_sink_direct/skeleton.rs:57`:

```rust
.container_bytes("object").expect("object rule open/close bytes")
```

`json_sink_direct/skeleton.rs:94`:

```rust
.container_bytes("array").expect("array rule open/close bytes")
```

`json_sink_direct/recognize.rs:94` and line 123: same `"object"` and `"array"` lookups. These
four lookups are the only way the emitter identifies which rules to render as containers. A
grammar with container rules named `"record"` / `"sequence"` would `panic!` on the `expect`.

`json_sink_direct/skeleton.rs:147` and line 189:

```rust
debug_assert!(self.rule("string").is_some(), "string rule present");
debug_assert!(self.rule("number").is_some(), "number rule present");
```

These are debug assertions, but they confirm the emitter expects exactly the JSON rule naming
convention. The full string and number emitters do not look up the rules by name for logic
purposes — they assume the `ArmKind::String` and `ArmKind::Number` dispatch arms produced by
`dispatch::arm_kind` map correctly to the hardwired `parse_string_direct` and
`parse_number_*_direct` function names, which are fixed literals in the emitter output.

### Hardwired literal verb mapping (`dispatch.rs:114–135`)

```rust
b"true"  => ("TRUE_LITERAL", "bool(true)"),
b"false" => ("FALSE_LITERAL", "bool(false)"),
b"null"  => ("NULL_LITERAL", "null()"),
other   => return Err(...)
```

This exhaustive match covers exactly JSON's boolean and null literals. Any grammar whose
literal-dispatch rule branches to values other than `true`, `false`, or `null` produces a hard
error in `derive_arms`. The sink verb suffixes (`bool(true)`, `null()`) are JSON-specific method
names on `JsonSink`.

### Hardwired object key/value separator

`skeleton.rs:79` and `recognize.rs:109` emit the literal byte `b':'` as the object key/value
separator rather than deriving it from the grammar's `ByteLiteral` nodes. The grammar has a `":"`
literal inside the `object` rule, but the emitter does not read it — it embeds `b':'` directly.
A hypothetical grammar using `"=>"` as the key separator would silently parse incorrectly.

### Three fixed sink-prefix variants

`json_sink_direct/mod.rs:211`: `PREFIXES` is a `[SinkPrefix; 3]` constant with hardwired
function names `parse_value_direct` / `parse_object_value_at_direct` / `parse_array_element_at_direct`.
The three-variant structure (top-level value / object-child / array-element) is the JSON
context-sensitivity model, not a general pattern.

**Verdict for JSON emit body:** Dispatch arm byte keys and the value-dispatch arm set are
grammar-derived (the `.bbnf`-mutation falsifier is load-bearing). Rule-body container detection
via `rule_has_byte` is structural. Everything else — rule name lookups (`"value"`, `"object"`,
`"array"`), literal verb mapping (`true`/`false`/`null`), sink method names, the colon separator,
the three-prefix monomorphization — is hardwired to JSON semantics. Fraction of the JSON hot
path that is grammar-derived: arm keys and arm counts (dispatch table itself, ~20%); the emitted
parser bodies are ~80% hardwired JSON idiom.

---

## Stage 3: general_descent Path

### What it handles correctly

`general_descent` is structurally general: it walks every `SinkOnlyExpr` variant for which it
has a recognizer (`Entry`, `Seq`, `Alt`, `RepeatLoop`, `OptionalBranch`, `ByteLiteral`,
`RegexProgram`, `CallRule`, `ValueProject`). Rule names, dispatch key bytes, and literal bodies
are all derived from the `SinkOnlyProgram`. The `{Prefix}Kind` enum and `{Prefix}Document` handle
types are derived from `grammar_pascal(grammar_name)`. No rule name is a literal in the emitter
source. It was validated against a third grammar (Google Sheets) with `->` projections and was
shown to produce a compiling output.

### What general_descent does NOT handle (missing from LoweredArtifact coverage)

**`SpanMark` and `TapeEmit` are silent no-ops** (`emit.rs:381–384`). In the Google Sheets grammar,
`SpanMark` nodes from `@pretty` directives and `TapeEmit` from CSS-style tape-point records are
simply dropped. No tape entry is written, which means the tape view will not see those structural
markers. For a grammar that relies on `TapeEmit` for its structural shape, the view would be
wrong.

**`Return` is a silent no-op** (`emit.rs:384`). A grammar that emits an early `Return` to signal
a successful sub-parse expects the recognizer to stop at that point; `general_descent` treats it
as a no-op, which can cause the recognizer to over-read.

**`DirectBuild` emits only `mark(tape, _node_start, kind)`** (`emit.rs:386–389`). The
`DirectBuildField` vector — the per-field materializer list the JSON typed path uses to build
struct values — is completely ignored. Any grammar whose rules carry `DirectBuild` with multiple
fields gets only a single boundary mark; the field-by-field payload is lost.

**`RegexProgram` uses a delimiter-scan fallback only.** The skinny runtime has no general regex
matcher. `emit.rs:365–374` writes `match_leaf(bytes, cursor, tape, first_lit, node_kind, nullable)`,
which consumes bytes up to the next structural stop or whitespace. For complex regex leaves
(multi-word identifiers, URLs, CSS value expressions) the fallback will often read too much or
too little. The module comment (`mod.rs:19–24`) documents this as deferred: "typed leaves
deferred to `Span`".

**Sink callbacks are absent.** `general_descent` emits a recognizer + tape-backed view, but
there is no `JsonSink`-style push-down callback interface. For grammars where the caller needs
real-time materialized values during parsing (as opposed to lazy span re-reads from the tape),
there is no path.

**The view's typed accessors (`as_f64`, `as_bool`, `as_tag`) require span re-parsing.** The
values are not materialized at parse time; they are recovered by re-parsing the raw span bytes in
the view accessor. For `as_f64` this is a `parse::<f64>().ok()` call; for `as_tag` it is a
linear scan through `TAG_TABLE`. This is a structural limitation: `general_descent` is a
recognizer-only emitter with lazy projections, not a materializing parser.

---

## Census of grammar-name and rule-name literals in production code paths

The following table covers non-test production code only (`src/` files, excluding `#[cfg(test)]`
blocks):

| Location | Literal | Kind |
|---|---|---|
| `runtime_generator.rs:98` | `"value"` | Rule-name gate — JSON shape probe |
| `json_sink_direct/mod.rs:98` | `"value"` | Rule-name lookup |
| `json_sink_direct/skeleton.rs:57` | `"object"` | Rule-name lookup (panics if missing) |
| `json_sink_direct/skeleton.rs:94` | `"array"` | Rule-name lookup (panics if missing) |
| `json_sink_direct/recognize.rs:94` | `"object"` | Rule-name lookup |
| `json_sink_direct/recognize.rs:123` | `"array"` | Rule-name lookup |
| `json_sink_direct/dispatch.rs:120–122` | `b"true"`, `b"false"`, `b"null"` | Value literals — JSON-only verb map |
| `json_sink_direct/skeleton.rs:79`, `recognize.rs:109` | `b':'` | JSON object separator — not derived |
| `lower/css_scan.rs:490` | `"ruleBlock"` | Rule-name lookup — CSS-only |
| `lower/css_scan.rs:501` | `"atRuleBody"` | Rule-name lookup — CSS-only |
| `lower/css_scan.rs:533` | `"cssString"` | Rule-name lookup — CSS-only |
| `lower/css_scan.rs:617` | `"atRule"` | Rule-name lookup — CSS-only |
| `lower/css_scan.rs:577` | `"properties.bbnf"`, `*Decl` | File + name convention — CSS-only |
| `css_scan_direct/drivers.rs:209` | `b"/*"` | CSS comment — not in grammar |
| `css_scan_direct/drivers.rs:218,221` | `b"<!--"`, `b"-->"` | CDO/CDC — CSS tokenizer, not grammar |
| `css_scan_direct/drivers.rs:236,239` | `b"@charset"` | CSS directive — not in grammar |

---

## Lock-14 Scope Gap

The live Lock-14 gate (`crates/ir/tests/lock14_grammar_generalization.rs`) scans only
`crates/ir/src/` and `crates/analysis/src/` for grammar-name tokens. It explicitly does not
cover `skinny/crates/codegen/src/`. The 16 hardwired rule-name and grammar-specific literals
enumerated above are all in the codegen crate and would not be caught by the existing gate.

---

## Recommendations

1. **Remove the `"value"` gate.** The dispatch between `json_sink_direct` and `general_descent`
   should be driven by a structural predicate computed during lowering — for example, presence of
   an `AltMode::Dispatch` root node with `Container` and `Sequence` arms — not a rule-name
   string. The correct generalisation is to let `json_sink_direct` read the container rule names
   from the dispatch arms (`ArmKind::Container { callee }` already carries the name), replacing
   the `container_bytes("object")` / `container_bytes("array")` hardwiring.

2. **Derive the object key/value separator from the grammar.** `render_object` and
   `render_recognize_object` should scan the `object` rule's `ByteLiteral` nodes for the
   separator byte, the same way `container_bytes` reads the open/close delimiters.

3. **Generalise the literal-verb mapping.** The `b"true"` / `b"false"` / `b"null"` → sink-verb
   table should be driven by the grammar's `->` projections, not a closed match. A grammar whose
   literal arms project to `-> true` / `-> false` should map to the same bool sink verb; a grammar
   projecting to `-> 0u8` should map to a tag verb. The projection metadata is already in
   `SinkOnlyExpr::ValueProject`.

4. **Extend the Lock-14 gate to `skinny/crates/codegen/src/`.** The current gate leaves the
   emitter crate unguarded. The roster check should include the codegen tree so that future
   grammar-name additions to dispatch logic are caught at CI.

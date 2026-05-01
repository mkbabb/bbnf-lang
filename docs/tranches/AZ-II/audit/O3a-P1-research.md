# AZ-II.cutover.O3a-P1 Research - Projection Totality and Generated View Residue

## Verdict

`bbnf::projection_totality projection_totality_runtime_call_count` is a
runtime projection test stale against StructDirect cutover. It still
expects the old generated `ValueRoot` / `<Grammar>Value` materializer
path to be the runtime evidence for CSS L4, Sheets, and BBNF, while
the generated `parse()` entrypoints now return document-owned
StructDirect runtime documents.

Ownership maps as follows:

- **Failing test owner**: `crates/core/tests/projection_totality.rs`.
  The runtime-call-count assertion must be rewritten to document-owned
  evidence for all StructDirect grammars, like the JSON block already
  does.
- **Generated-view residue owner**: O3. `grammar.rs` still emits
  tape-backed views, generated `ValueRoot`, generated value enums, and
  materializer fns unconditionally for StructDirect grammars.
- **Materializer ownership**: not the root cause of this failure.
  `value_materialize.rs` is still the generator for the obsolete
  tape-backed materializer surface, but the failing assertion is no
  longer a valid runtime proof once `parse()` returns a document.

O3 is sufficient if it performs its declared generated-view purge and
updates `projection_totality_runtime_call_count` to assert document
runtime projection for StructDirect grammars. I do not see evidence
requiring O3b; O3b should open only if O3 refuses to remove/suppress
the StructDirect `ValueRoot` emission while keeping public projection
coverage.

## Failing Test Root Cause

The test already acknowledges the cutover for JSON:
`JsonParser::parse` returns `JsonDocument`, `doc.to_value()` returns a
runtime `JsonValue`, and the old `"Projection"` debug marker no longer
applies (`crates/core/tests/projection_totality.rs:359`).

The same test still treats CSS L4, Sheets, and BBNF as tape-direct
grammars. It parses each grammar, calls `.to_value()`, then requires
the debug output to contain `"Projection"`:

- CSS L4: `crates/core/tests/projection_totality.rs:403`
- Sheets: `crates/core/tests/projection_totality.rs:417`
- BBNF: `crates/core/tests/projection_totality.rs:431`
- Shared assertion: `crates/core/tests/projection_totality.rs:336`

That is obsolete for current generated output. All nine generated
grammar parsers are StructDirect and return runtime documents:

| Generated file | Current parse return |
|---|---|
| `bbnf.rs` | `crate::runtime::bbnf::BbnfDocument<'_>` |
| `bnf.rs` | `crate::runtime::bnf::BnfDocument<'_>` |
| `css_l4.rs` | `crate::runtime::css_l4::CssDocument<'_>` |
| `css_pretty.rs` | `crate::runtime::css_pretty::CssPrettyDocument<'_>` |
| `csv.rs` | `crate::runtime::csv::CsvDocument<'_>` |
| `ebnf.rs` | `crate::runtime::ebnf::EbnfDocument<'_>` |
| `google_sheets.rs` | `crate::runtime::google_sheets::SheetsDocument<'_>` |
| `json.rs` | `crate::runtime::json::JsonDocument<'_>` |
| `math.rs` | `crate::runtime::math::MathDocument<'_>` |

Concrete examples:

- `crates/core/src/grammar/generated/json.rs:5024` returns
  `JsonDocument<'_>`.
- `crates/core/src/grammar/generated/css_l4.rs:166043` returns
  `CssDocument<'_>`.
- `crates/core/src/runtime/css_l4/document.rs:118` says
  `CssDocument::to_value()` lends the root `StyleSheet`.
- `crates/core/src/runtime/google_sheets/document.rs:83` says
  `SheetsDocument::to_value()` lends the root `SheetsValue`.
- `crates/core/src/runtime/bbnf/document.rs:90` says
  `BbnfDocument::to_value()` lends the root `BbnfValue`.

Those document-owned values should not contain generated
`<Grammar><Rule>Projection` debug markers. The failure therefore maps
to **runtime projection test ownership**, not to missing materializer
calls.

## Residue Inventory

The generator still emits tape-view and generated-value surfaces for
StructDirect grammars because `emit_type_definitions_impl` splices all
of them unconditionally:

- `generate_views(...)`: `crates/core/src/backend/rust/emitter/grammar.rs:843`
- `emit_direct_to_struct_projection(...)`: `crates/core/src/backend/rust/emitter/grammar.rs:871`
- `emit_value_surface(...)`: `crates/core/src/backend/rust/emitter/grammar.rs:880`
- `emit_materialize_fns(...)`: `crates/core/src/backend/rust/emitter/grammar.rs:890`
- all four are emitted into generated output at
  `crates/core/src/backend/rust/emitter/grammar.rs:895`

The strategy gate only chooses the parse body and parse return type:
`EmitStrategy::StructDirect` selects document parse output at
`crates/core/src/backend/rust/emitter/grammar.rs:1090` and
`crates/core/src/backend/rust/emitter/grammar.rs:1120`. It does not
gate the generated view/value/materializer surfaces.

The backend view producer is explicitly tape-backed:

- `crates/core/src/backend/rust/view/mod.rs:1` describes per-rule
  tape-view codegen.
- `crates/core/src/backend/rust/view/mod.rs:4` says each view holds a
  `TapeCursor`.
- `crates/core/src/backend/rust/view/mod.rs:94` says
  `generate_views` emits `<Rule>View<'p>` structs and a `Root` binding.
- `crates/core/src/backend/rust/view/value.rs:16` emits
  `impl ValueRoot for <Grammar>`.
- `crates/core/src/backend/rust/view/value.rs:481` and
  `crates/core/src/backend/rust/view/value.rs:678` use `TapeCursor`
  during generated value projection.

The generated output confirms the residue. Scan command:

```bash
for f in crates/core/src/grammar/generated/*.rs; do
  printf '%s\t' "$f"
  rg -c 'TapeCursor' "$f" | tr -d '\n'
  printf '\t'
  rg -c 'NodeView' "$f" | tr -d '\n'
  printf '\t'
  rg -c 'ValueRoot' "$f" | tr -d '\n'
  printf '\t'
  rg -c 'pub enum .*Value|project_value_|materialize_projection_|PROJECTION_DIRECT_TO_STRUCT' "$f" | tr -d '\n'
  printf '\n'
done
```

| Generated file | `TapeCursor` | `NodeView` | `ValueRoot` | generated value/projection hits |
|---|---:|---:|---:|---:|
| `bbnf.rs` | 262 | 573 | 1 | 95 |
| `bnf.rs` | 38 | 72 | 1 | 21 |
| `css_l4.rs` | 857 | 1881 | 1 | 289 |
| `css_pretty.rs` | 76 | 160 | 1 | 26 |
| `csv.rs` | 33 | 55 | 1 | 26 |
| `ebnf.rs` | 78 | 182 | 1 | 33 |
| `google_sheets.rs` | 184 | 331 | 1 | 67 |
| `json.rs` | 44 | 70 | 1 | 31 |
| `math.rs` | 19 | 26 | 1 | 21 |

Example residue inside a StructDirect generated parser:

- `crates/core/src/grammar/generated/json.rs:3840` emits
  `JsonParserValue<'p>`.
- `crates/core/src/grammar/generated/json.rs:3851` keeps
  `Unknown(JsonParserNodeView<'p>)`.
- `crates/core/src/grammar/generated/json.rs:4096` implements
  `ValueRoot for JsonParser`.
- `crates/core/src/grammar/generated/json.rs:3927` and
  `crates/core/src/grammar/generated/json.rs:4029` construct
  `TapeCursor`.
- `crates/core/src/grammar/generated/json.rs:4257` emits
  `materialize_projection_null_JsonParser`.

## Materializer Disposition

`value_materialize.rs` remains a generator for tape-backed projection
helpers:

- It declares one `materialize_projection_<rule>_<Grammar>` helper per
  direct-to-struct admission (`crates/core/src/backend/rust/emitter/shapes/value_materialize.rs:3`).
- Its documented input is the fused `Tape<R>` and tape payload/children
  surface (`crates/core/src/backend/rust/emitter/shapes/value_materialize.rs:24`).
- It treats runtime-call-count evidence as `Parsed::to_value()`
  producing a generated `<Grammar>Value` tree with a projection struct
  (`crates/core/src/backend/rust/emitter/shapes/value_materialize.rs:44`).
- Emitted helper signatures still take
  `&crate::runtime::tape::Tape<#grammar_ident>` at
  `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs:227`.

That surface is incompatible with the current StructDirect parse
contract. The materializer is not failing to run for CSS/Sheets/BBNF;
the test is asking document-owned values to prove an obsolete
generated-materializer path.

## Required O3 Close Shape

O3 should close P1 by doing both parts in one source wave:

1. Suppress/delete generated `generate_views`, `emit_value_surface`,
   and `emit_materialize_fns` output for `EmitStrategy::StructDirect`
   grammars, while preserving any document-owned runtime view APIs in
   `crates/core/src/runtime/<grammar>/`.
2. Rewrite `projection_totality_runtime_call_count` so every
   StructDirect grammar uses document evidence:
   JSON string/value, CSS stylesheet/declaration/value reachability,
   Sheets `SheetsValue`, and BBNF `BbnfValue`/compound reachability.

The existing O3 spec already owns these files and gates:
`crates/core/src/backend/rust/view/**`,
`crates/core/src/backend/rust/emitter/grammar.rs`,
`crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`,
`crates/core/tests/projection_totality.rs`, generated files, and the
zero-residue scan. No separate O3b is justified by this research.

## Focused Test Result

Focused command:

```bash
cargo nextest run -p bbnf --test projection_totality --cargo-profile ax-iter projection_totality_runtime_call_count --no-fail-fast -- --nocapture
```

Result: failed after a cold compile (`Finished ax-iter profile` in
3m30s). The first runtime failure is CSS L4 at
`crates/core/tests/projection_totality.rs:342`:

```text
CssL4Parser: to_value() tree carries no Projection-typed variant -
admission-driven materializer never fired at runtime. Rendered:
StyleSheet { rules: CssRuleListId(1) }
```

This matches the source diagnosis: CSS `parse()` returns
`CssDocument`, `CssDocument::to_value()` returns a document-owned
`StyleSheet`, and the old generated projection debug marker is no
longer valid runtime evidence. The committed failure baseline records
the same test as failed in the post-O2 run at
`docs/benchmarks/archive/AZ-II/cutover/O3a-test-failures.txt:3395` and
`docs/benchmarks/archive/AZ-II/cutover/O3a-test-failures.txt:6799`.

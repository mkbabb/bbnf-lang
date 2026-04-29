# AZ-II.cutover.O3a-P1 Redress Probe

Agent: AZ-II O3a-P1 redress/probe
Worktree: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-p1-redress`
Date: 2026-04-29

## Scope Boundary

No source redress was applied. This probe only reproduced the focused
`projection_totality` failure, scanned generated/backend residue, and
records the owner disposition required before O3 source work resumes.

Only this audit file was created.

## Commands

Focused reproduction, unique target dir:

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-p1-redress/target/azii-o3a-p1-redress \
  cargo nextest run -p bbnf --test projection_totality \
  --cargo-profile ax-iter projection_totality_runtime_call_count \
  --no-fail-fast -- --nocapture
```

Result: failed. After a cold compile in the unique target dir
(`Finished ax-iter ... in 3m 35s`), nextest ran 1 selected test twice.
Both attempts failed at `crates/core/tests/projection_totality.rs:342`:

```text
CssL4Parser: to_value() tree carries no Projection-typed variant
-- admission-driven materializer never fired at runtime.
Rendered: StyleSheet { rules: CssRuleListId(1) }
Summary: 1 test run: 0 passed, 1 failed, 3 skipped
```

Warm full file separation:

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-p1-redress/target/azii-o3a-p1-redress \
  cargo nextest run -p bbnf --test projection_totality \
  --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

Result: failed, but only the runtime witness failed. Structural
projection totality passed:

```text
projection_totality_aggregate_floor ... ok
projection_totality_per_grammar ... ok
AY-II.W0.d projection totality: JSON=3 CSS_L4=53 Sheets=10 BBNF=15 -> total=81
projection_totality_resolver_admissions_promoted ... ok
projection_totality_runtime_call_count ... FAILED
Summary: 4 tests run: 3 passed, 1 failed, 0 skipped
```

Residue scans:

```bash
rg -n --count-matches "TapeCursor" \
  crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf,ebnf,csv,math,bnf,css_pretty}.rs
rg -n --count-matches "NodeView" \
  crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf,ebnf,csv,math,bnf,css_pretty}.rs
rg -n --count-matches "impl crate::runtime::ValueRoot" \
  crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf,ebnf,csv,math,bnf,css_pretty}.rs
rg -n --count-matches "PROJECTION_DIRECT_TO_STRUCT|PROJECTION_MATERIALIZERS|PROJECTION_CONSUMERS" \
  crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf,ebnf,csv,math,bnf,css_pretty}.rs
rg -n "emit_value_surface|generate_views|emit_materialize_fns|emit_direct_to_struct_projection|ValueRoot|TapeCursor" \
  crates/core/src/backend/rust/emitter crates/core/src/backend/rust/view \
  crates/core/src/grammar/schema/emit/rust -g '*.rs'
```

## Reproduction Finding

The failing test is not a slice-totality failure. It is a stale
runtime-call-count witness for CSS L4 after CSS has already crossed to
the StructDirect document path.

Evidence:

- `projection_totality_per_grammar` passes with 81 total admissions and
  1:1:1 admission/materializer/consumer counts.
- `projection_totality_resolver_admissions_promoted` passes, so the
  named resolver admissions remain present.
- `projection_totality_runtime_call_count` fails before Sheets or BBNF
  are exercised because `CssL4Parser::parse(...).to_value()` returns
  `StyleSheet { rules: CssRuleListId(1) }`, not a generated
  `CssL4ParserValue::*Projection` tree.
- Checked generated CSS L4 parse output returns
  `crate::runtime::css_l4::CssDocument<'_>` at
  `crates/core/src/grammar/generated/css_l4.rs:166043`, and
  `CssDocument::to_value()` returns `&StyleSheet` at
  `crates/core/src/runtime/css_l4/document.rs:126`.
- The same generated CSS L4 file still emits a dead/legacy
  `CssL4ParserValue<'p>` projection enum around
  `crates/core/src/grammar/generated/css_l4.rs:124967` and an
  `impl crate::runtime::ValueRoot for CssL4Parser` at
  `crates/core/src/grammar/generated/css_l4.rs:128271`.

Interpretation: the runtime behavior is already document-owned for CSS
L4, while the generated `ValueRoot` projection surface still exists.
The test asks the document-owned path to prove that the old generated
`ValueRoot` materializer fired. That proof can no longer be valid after
StructDirect activation.

## Residue Scan Results

Generated StructDirect output still contains tape-view and value-root
residue across all nine generated grammars.

`TapeCursor` hits:

```text
csv.rs:37
ebnf.rs:82
bnf.rs:42
math.rs:23
css_pretty.rs:80
json.rs:48
bbnf.rs:266
google_sheets.rs:188
css_l4.rs:861
```

`NodeView` hits:

```text
csv.rs:55
math.rs:26
bnf.rs:72
ebnf.rs:182
json.rs:70
css_pretty.rs:160
google_sheets.rs:331
bbnf.rs:573
css_l4.rs:1881
```

`impl crate::runtime::ValueRoot` hits:

```text
bnf.rs:1
csv.rs:1
math.rs:1
css_pretty.rs:1
ebnf.rs:1
google_sheets.rs:1
json.rs:1
bbnf.rs:1
css_l4.rs:1
```

Projection metadata residue:

```text
bnf.rs:18
css_pretty.rs:20
math.rs:18
json.rs:22
ebnf.rs:22
bbnf.rs:46
csv.rs:20
google_sheets.rs:36
css_l4.rs:122
```

Backend/source producers for the residue are inside O3 file bounds:

- `crates/core/src/backend/rust/view/mod.rs:115` emits generated
  per-rule views, `NodeView`, and `Root::View` bindings over
  `TapeCursor`.
- `crates/core/src/backend/rust/view/value.rs:92` emits
  `<Grammar>Value`, `impl ValueRoot`, tape-backed path query surfaces,
  and projection arms.
- `crates/core/src/backend/rust/emitter/grammar.rs:843` wires
  `generate_views`; lines near `872`, `880`, and `890` wire direct
  projection metadata, `emit_value_surface`, and materializer emission.
- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs:68`
  emits `materialize_projection_*` helpers.
- `crates/core/src/grammar/schema/emit/rust/directives.rs:200` and
  `crates/core/src/grammar/schema/emit/rust/identifiers.rs:83` still
  emit schema helper APIs that consume `TapeCursor`.

## Likely Patch Files

O3 can own the fix without opening O3b. The patch is not a separate
projection-totality substrate; it is the O3 generated-view purge plus a
test witness update to document-owned APIs.

Likely source/test files:

- `crates/core/src/backend/rust/emitter/grammar.rs`
- `crates/core/src/backend/rust/view/mod.rs`
- `crates/core/src/backend/rust/view/value.rs`
- `crates/core/src/backend/rust/view/leaves.rs`
- `crates/core/src/backend/rust/view/seq.rs`
- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
- `crates/core/src/grammar/schema/emit/rust/directives.rs`
- `crates/core/src/grammar/schema/emit/rust/identifiers.rs`
- `crates/core/tests/projection_totality.rs`
- `crates/core/tests/regen_shape_goldens.rs`
- Generated files under `crates/core/src/grammar/generated/*.rs` in the
  orchestrator regen window only.

The test update should stop treating CSS L4 as a tape-direct grammar.
For StructDirect document grammars, the runtime witness should assert
document-owned typed reachability, for example `CssDocument::to_value()`
returns a `StyleSheet`, `walk_declarations()` reaches the declaration,
and `walk_values()` reaches the typed `rgb(...)` value. The old
`Projection` debug-string check should remain only for any grammar that
still intentionally routes through generated `ValueRoot`; after O3 it
should disappear for StructDirect output.

## Disposition

Ready for O3, no O3b required.

Rationale: the isolated failure is caused by the coexistence of a
document-owned StructDirect parse return with stale generated
`ValueRoot`/projection witness expectations. All producer files needed
to delete the stale surface are already in O3 file bounds, and O3
already requires `projection_totality.rs` to move to document-owned
APIs plus a zero-residue generated scan. Opening O3b would split the
same deletion/test-witness work across waves without a new substrate or
ownership boundary.

Halt condition: source redress remains halted until the O3 plan amendment
is accepted. The evidence above is sufficient for O3 to proceed with
P1 integrated and to gate close on:

```bash
cargo nextest run -p bbnf --test projection_totality --cargo-profile ax-iter -- --nocapture
rg 'TapeCursor|NodeView|impl crate::runtime::ValueRoot|ValueRoot|PROJECTION_DIRECT_TO_STRUCT|PROJECTION_MATERIALIZERS|PROJECTION_CONSUMERS' crates/core/src/grammar/generated/*.rs
```

Expected O3 close state: the test passes on document-owned APIs and the
generated StructDirect scan returns zero production hits for tape-view,
node-view, `ValueRoot`, and projection-materializer residue.

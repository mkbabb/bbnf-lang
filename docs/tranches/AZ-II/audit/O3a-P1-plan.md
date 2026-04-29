# AZ-II O3a-P1 Plan - Projection Totality and Generated View Residue

**Agent**: AZ-II O3a-P1 plan
**Date**: 2026-04-29
**Scope**: plan only; no source edits; no direct `O3.md` or `O3b.md` edit.

## Read Record

- `docs/instructions/README.md`
- `docs/instructions/PROFILING.md`
- `docs/tranches/AZ-II/AZ-II.md`
- `docs/tranches/AZ-II/PROGRESS.md`
- `docs/tranches/AZ-II/waves/cutover/O3a.md`
- `docs/tranches/AZ-II/waves/cutover/O3a-P1.md`
- `docs/tranches/AZ-II/waves/cutover/O3.md`
- `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt`

## Evidence

`docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt` records one P1
failure in the post-O2 baseline:

```text
bbnf::projection_totality projection_totality_runtime_call_count
```

Focused confirmation command run in this worktree:

```bash
cargo nextest run -p bbnf \
  --test projection_totality \
  --cargo-profile ax-iter \
  projection_totality_runtime_call_count \
  -- --nocapture \
  > /tmp/az-ii-o3a-p1-projection-totality.txt 2>&1
```

Observed failure:

```text
CssL4Parser: to_value() tree carries no Projection-typed variant -
admission-driven materializer never fired at runtime. Rendered:
StyleSheet { rules: CssRuleListId(1) }
```

The failure is not a missing admission/materializer/consumer slice. The
generated CSS L4 parser still emits `PROJECTION_DIRECT_TO_STRUCT`,
`PROJECTION_MATERIALIZERS`, `PROJECTION_CONSUMERS`, `CssL4ParserValue`,
`impl crate::runtime::ValueRoot for CssL4Parser`, and
`materialize_projection_*_CssL4Parser` functions. The runtime call-count
test is stale relative to the StructDirect document path: CSS L4
`parse()` now returns `CssL4Document`, and `CssL4Document::to_value()`
lends the concrete `StyleSheet`, not the old tape-backed
`<Grammar>Value` projection enum.

Generated residue scan evidence:

```bash
for f in crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf}.rs; do
  printf '%s\n' "$f"
  rg -c 'TapeCursor|NodeView|ValueRoot|Parsed<' "$f"
done
```

Current hit counts:

| Generated file | Residue hits |
|---|---:|
| `crates/core/src/grammar/generated/json.rs` | 116 |
| `crates/core/src/grammar/generated/css_l4.rs` | 2740 |
| `crates/core/src/grammar/generated/google_sheets.rs` | 517 |
| `crates/core/src/grammar/generated/bbnf.rs` | 837 |

Emitter ownership evidence:

- `crates/core/src/backend/rust/emitter/grammar.rs` unconditionally
  calls `view::generate_views`, `view::emit_value_surface`, and
  `shapes::value_materialize::emit_materialize_fns` from
  `emit_type_definitions_impl`, before the strategy-specific parse
  body returns a StructDirect document.
- `crates/core/src/backend/rust/view/mod.rs` emits
  `TapeCursor`-backed `<Rule>View` and `<Grammar>NodeView` types.
- `crates/core/src/backend/rust/view/value.rs` emits
  `<Grammar>Value`, `impl ValueRoot`, path-query walkers, and tape
  cursor descent.
- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
  emits `materialize_projection_*` helpers against
  `crate::runtime::tape::Tape<#grammar>`.

## Decision

**P1 closes inside O3. No O3b child spec is required now.**

Reason: the failing test and all generated residue map directly to
O3's declared file bounds and hard gate. O3 already owns:

- generated view suppression;
- `ValueRoot` and `materializer` removal for StructDirect grammars;
- document-owned projection/accessor tests;
- `projection_totality.rs`;
- the fleet regen and zero-residue scan artifact.

Creating O3b would split one emission-boundary repair into a second
wave without a file-boundary reason. The orchestrator should create
`docs/tranches/AZ-II/waves/cutover/O3b.md` only if O3 redress proves
that projection totality requires non-O3 owners such as `Parsed<R>`,
`TapeDirect`, `crates/tape/**`, workspace manifests, or benchmark
harnesses. This plan finds no such requirement.

## Owner Table

| Owner | Files | Responsibility |
|---|---|---|
| O3.P1-G1 - Type-definition emission gate | `crates/core/src/backend/rust/emitter/grammar.rs` | Gate `generate_views`, `emit_value_surface`, and `emit_materialize_fns` by `EmitStrategy`. StructDirect grammars must not emit tape-backed view, `ValueRoot`, or tape materializer surfaces. |
| O3.P1-V1 - Backend view carve | Exact list in the file-owner ledger below. | Delete or isolate tape-view generation so StructDirect output has no `<Grammar>NodeView`, `TapeCursor`, or `ValueRoot` production surface. No generated shim may preserve node-view names. |
| O3.P1-SP1 - Generated scan-policy doc carve | `crates/core/src/backend/rust/emitter/shapes/dispatcher/scan_policy.rs` | Remove generated `TapeCursor` doc links from StructDirect output without changing scan-policy runtime ownership. |
| O3.P1-SER1 - Tape-first serializer carve | `crates/core/src/generate/mod.rs`, `crates/core/src/generate/serialize/mod.rs`, `crates/core/src/generate/serialize/serialize.rs` | Gate `#[parser(serialize)]` NodeView serializer generation by `EmitStrategy`; StructDirect must not emit `serialize_*` methods over `<Grammar>NodeView<'_>`. |
| O3.P1-M1 - Value materializer deletion | `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs` | Stop emitting `materialize_projection_*` against `runtime::tape::Tape` for StructDirect grammars. Retain only any tape-direct path still needed before O4/O5, with strategy gating. |
| O3.P1-D1 - Document-owned runtime projection proof | Exact list in the file-owner ledger below. | Ensure public parse results expose document-owned root/accessor surfaces directly from runtime arenas, with no `Parsed<R>`, node-view, or generated tape-view adapter. |
| O3.P1-T1 - Projection totality test rewrite | `crates/core/tests/projection_totality.rs`, `crates/core/tests/typed_accessor_surface.rs`, `crates/core/tests/runtime_root.rs`, `crates/core/tests/regen_shape_goldens.rs` | Replace the runtime-call-count assertion with StructDirect document-owned totality: every StructDirect grammar has direct document projection/accessor evidence and no generated tape-view residue. |
| O3.P1-R1 - Orchestrator regen and residue scan | Exact list in the file-owner ledger below. | Run one canonical regen after O3 source commits, review generated diffs, and archive a zero-hit scan for StructDirect generated files. |

## Exact File-Owner Ledger

O3.P1-G1:

- `crates/core/src/backend/rust/emitter/grammar.rs`

O3.P1-V1:

- `crates/core/src/backend/rust/view/alt.rs`
- `crates/core/src/backend/rust/view/grammar.rs`
- `crates/core/src/backend/rust/view/leaves.rs`
- `crates/core/src/backend/rust/view/mod.rs`
- `crates/core/src/backend/rust/view/repeat.rs`
- `crates/core/src/backend/rust/view/seq.rs`
- `crates/core/src/backend/rust/view/value.rs`

O3.P1-SP1:

- `crates/core/src/backend/rust/emitter/shapes/dispatcher/scan_policy.rs`

O3.P1-SER1:

- `crates/core/src/generate/mod.rs`
- `crates/core/src/generate/serialize/mod.rs`
- `crates/core/src/generate/serialize/serialize.rs`

O3.P1-M1:

- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`

O3.P1-D1:

- `crates/core/src/runtime/bbnf/document.rs`
- `crates/core/src/runtime/bbnf/mod.rs`
- `crates/core/src/runtime/bnf/document.rs`
- `crates/core/src/runtime/bnf/mod.rs`
- `crates/core/src/runtime/css_l4/document.rs`
- `crates/core/src/runtime/css_l4/mod.rs`
- `crates/core/src/runtime/css_pretty/document.rs`
- `crates/core/src/runtime/css_pretty/mod.rs`
- `crates/core/src/runtime/csv/document.rs`
- `crates/core/src/runtime/csv/mod.rs`
- `crates/core/src/runtime/ebnf/document.rs`
- `crates/core/src/runtime/ebnf/mod.rs`
- `crates/core/src/runtime/google_sheets/document.rs`
- `crates/core/src/runtime/google_sheets/mod.rs`
- `crates/core/src/runtime/json/document.rs`
- `crates/core/src/runtime/json/mod.rs`
- `crates/core/src/runtime/math/document.rs`
- `crates/core/src/runtime/math/mod.rs`

O3.P1-T1:

- `crates/core/tests/projection_totality.rs`
- `crates/core/tests/typed_accessor_surface.rs`
- `crates/core/tests/runtime_root.rs`
- `crates/core/tests/regen_shape_goldens.rs`

O3.P1-R1:

- `crates/core/src/grammar/generated/bbnf.rs`
- `crates/core/src/grammar/generated/bnf.rs`
- `crates/core/src/grammar/generated/css_l4.rs`
- `crates/core/src/grammar/generated/css_pretty.rs`
- `crates/core/src/grammar/generated/csv.rs`
- `crates/core/src/grammar/generated/ebnf.rs`
- `crates/core/src/grammar/generated/google_sheets.rs`
- `crates/core/src/grammar/generated/json.rs`
- `crates/core/src/grammar/generated/math.rs`
- `docs/benchmarks/AZ-II/cutover/O3-generated-view-scan.txt`

## Failure Assignment

| Failure | Owner | Verification |
|---|---|---|
| `bbnf::projection_totality projection_totality_runtime_call_count` | O3.P1-T1, after O3.P1-G1/V1/M1/D1 | `cargo nextest run -p bbnf --test projection_totality --cargo-profile ax-iter projection_totality_runtime_call_count -- --nocapture` |
| Generated `TapeCursor` / node-view hits in StructDirect output | O3.P1-G1 + O3.P1-V1 + O3.P1-SP1 + O3.P1-SER1 + O3.P1-R1 | `rg -n 'TapeCursor|[A-Za-z0-9_]+NodeView|ValueRoot|materialize_projection_|PROJECTION_MATERIALIZERS|PROJECTION_CONSUMERS' crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf,csv,math,bnf,css_pretty,ebnf}.rs` returns zero hits. General `crate::runtime::tape` and `Parsed<` residue is classified under O4/O5 unless it preserves generated view compatibility. |
| Generated `ValueRoot` / `materialize_projection_*` hits in StructDirect output | O3.P1-G1 + O3.P1-M1 + O3.P1-R1 | `rg -n 'ValueRoot|materialize_projection_|PROJECTION_MATERIALIZERS|PROJECTION_CONSUMERS' crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf,csv,math,bnf,css_pretty,ebnf}.rs` returns zero production hits for StructDirect grammars. |
| Tests still importing generated node views after O3 | O3.P1-T1, with O3.J1-P1 for JSON-specific accessor accounting | `rg -n 'NodeView|from_cursor|TapeCursor|Parsed<' crates/core/tests/projection_totality.rs crates/core/tests/typed_accessor_surface.rs crates/core/tests/runtime_root.rs crates/core/tests/regen_shape_goldens.rs` returns no live StructDirect dependency. |

## Implementation Plan

1. **O3.P1-G1 emission gate**

   In `grammar.rs`, compute `EmitStrategy::for_grammar` before type
   definition emission or thread the resolved strategy into
   `emit_type_definitions_impl`. For `EmitStrategy::StructDirect`, do
   not emit tape-backed `generate_views`, `emit_value_surface`, or
   `emit_materialize_fns`. For `EmitStrategy::TapeDirect`, preserve the
   existing output until O4/O5 remove the return model and tape crate.

   Hard no: do not emit empty `<Grammar>NodeView`, empty `ValueRoot`,
   empty `PROJECTION_*`, or forwarding shims for StructDirect.

2. **O3.P1-V1 view carve**

   Carve `backend/rust/view/**` so generated tape-view code is never
   requested for StructDirect. Any compile-time helpers that remain for
   TapeDirect must stay behind strategy-named functions or modules.
   StructDirect callers consume document-owned runtime APIs, not a
   generated view compatibility surface.

3. **O3.P1-SP1 generated scan-policy doc carve**

   Remove generated `TapeCursor` doc links from
   `STRUCTURAL_SCAN_POLICY` comments. The runtime scan-policy table
   can continue to use tape substrate types until O4/O5; O3's close
   gate is that generated StructDirect files no longer advertise or
   preserve generated view compatibility through docs or APIs.

4. **O3.P1-SER1 tape-first serializer carve**

   Gate `crates/core/src/generate/serialize` at the call site in
   `generate_all`. The generator is explicitly tape-first and takes
   `<Grammar>NodeView<'a>` in every public method; StructDirect must
   skip it rather than reconstitute a compatibility node-view surface.
   Document-owned serializers live in runtime modules when needed.

5. **O3.P1-M1 materializer deletion**

   Delete StructDirect emission of `materialize_projection_*` and the
   `ValueRoot` dispatcher relationship that calls those helpers. If the
   file still emits materializers for TapeDirect during O3, the function
   name and call site must make that ownership explicit. Do not leave
   a generated no-op materializer set in StructDirect output.

6. **O3.P1-D1 document projection surfaces**

   Confirm every StructDirect runtime document exposes root and typed
   projection/accessor surfaces from the concrete arena/value graph.
   Where the old projection-totality test depended on debug rendering
   of `<Grammar>Value::*Projection`, replace it with assertions against
   concrete document APIs and typed values.

7. **O3.P1-T1 test rewrite**

   Keep structural admission tests only for codegen facts that still
   exist after O3. The runtime-call-count test should become a
   StructDirect document-owned proof:

   - JSON: `JsonDocument::to_value()` returns expected scalar/object
     values.
   - CSS L4: `CssL4Document` exposes stylesheet/rule/declaration/color
     access without node views.
   - Sheets: `SheetsDocument` exposes literals, operators, and range
     values from the concrete value tree.
   - BBNF: `BbnfDocument` exposes rule/directive/value-expression
     structure from the runtime document.

   The test should also assert the generator no longer publishes
   StructDirect `PROJECTION_MATERIALIZERS`, `PROJECTION_CONSUMERS`,
   `ValueRoot`, or node-view APIs.

8. **O3.P1-R1 orchestrator regen and scan**

   After source commits are accepted, the orchestrator runs one
   canonical regen and records the residue scan in
   `docs/benchmarks/AZ-II/cutover/O3-generated-view-scan.txt`. Generated
   files are never hand-patched.

## Verification Commands

Focused P1 gate:

```bash
cargo nextest run -p bbnf \
  --test projection_totality \
  --cargo-profile ax-iter \
  projection_totality_runtime_call_count \
  -- --nocapture \
  > /tmp/az-ii-o3a-p1-projection-totality.txt 2>&1
```

O3 document-owned projection/accessor gate:

```bash
cargo nextest run -p bbnf \
  --test projection_totality \
  --test typed_accessor_surface \
  --test runtime_root \
  --test regen_shape_goldens \
  --cargo-profile ax-iter \
  --no-fail-fast \
  > /tmp/az-ii-o3a-p1-o3-doc-projection.txt 2>&1
```

Canonical regen gate:

```bash
cargo xtask regen
cargo xtask regen --check > /tmp/az-ii-o3-regen-check.txt 2>&1
```

StructDirect generated residue scan:

```bash
{
  rg -n 'TapeCursor|[A-Za-z0-9_]+NodeView|ValueRoot|materialize_projection_|PROJECTION_MATERIALIZERS|PROJECTION_CONSUMERS' \
    crates/core/src/grammar/generated/{json,css_l4,google_sheets,bbnf,csv,math,bnf,css_pretty,ebnf}.rs || true
} > docs/benchmarks/AZ-II/cutover/O3-generated-view-scan.txt
```

Close condition: the scan artifact records zero hits for StructDirect
generated view/serializer/materializer output, including generated
comments. General `crate::runtime::tape` and `Parsed<` residue remains
O4/O5-owned unless it preserves a generated view compatibility surface.
Historical generated comments are not an O3 close exception; they must be
deleted at the producer before close.

Workspace confirmation after O3 integration:

```bash
scripts/test-tier.sh workspace --profile ax-iter --no-fail-fast \
  > /tmp/az-ii-o3-workspace.txt 2>&1
```

## O3 Amendment Text

Patch intent for the orchestrator. Do not apply from the P1 plan lane.

Insert after `AZ-II.cutover.O3.11 O3a P1 Integration`:

```markdown
### AZ-II.cutover.O3.13 O3a P1 Projection-Totality Close

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-P1-plan.md`.
Close `bbnf::projection_totality projection_totality_runtime_call_count`
inside O3 by replacing the stale tape-backed runtime-call-count proof
with document-owned StructDirect projection/accessor evidence. O3 also
owns the generated residue purge: StructDirect grammars must not emit
`TapeCursor`, generated node views, `ValueRoot`,
`materialize_projection_*`, `PROJECTION_MATERIALIZERS`, or
`PROJECTION_CONSUMERS`.

Files touched: exactly the O3.P1-G1, O3.P1-V1, O3.P1-SP1, O3.P1-SER1,
O3.P1-M1, O3.P1-D1, O3.P1-T1, and O3.P1-R1 files listed in
`docs/tranches/AZ-II/audit/O3a-P1-plan.md` under "Exact
File-Owner Ledger". No generated tape-view shim, empty node-view type,
empty `ValueRoot`, or forwarding projection adapter may be introduced.

Sub-gate: `cargo nextest run -p bbnf --test projection_totality --cargo-profile ax-iter projection_totality_runtime_call_count -- --nocapture` passes, and `docs/benchmarks/AZ-II/cutover/O3-generated-view-scan.txt` records zero production hits for `TapeCursor`, generated `NodeView` serializer/view surfaces, `ValueRoot`, `materialize_projection_`, `PROJECTION_MATERIALIZERS`, and `PROJECTION_CONSUMERS` in StructDirect generated files. General `crate::runtime::tape` and `Parsed<` residue remains O4/O5-owned unless it preserves generated view compatibility.
```

Append to O3 hard gate:

```markdown
8. O3a P1 closes inside O3: `projection_totality_runtime_call_count`
   passes on document-owned StructDirect APIs, `docs/tranches/AZ-II/audit/O3a-P1-plan.md`
   is cited, no `O3b.md` is required, and the generated residue scan
   records zero production `TapeCursor`, generated node-view
   serializer/view surfaces, `ValueRoot`, `materialize_projection_*`,
   `PROJECTION_MATERIALIZERS`, or `PROJECTION_CONSUMERS` hits for
   StructDirect generated output. O4/O5 own non-view `Parsed<` and
   general `crate::runtime::tape` retirement.
```

Append to O3 verification artifacts:

```markdown
- `/tmp/az-ii-o3a-p1-projection-totality.txt`
- `/tmp/az-ii-o3a-p1-o3-doc-projection.txt`
- `docs/tranches/AZ-II/audit/O3a-P1-plan.md`
```

## O3b Disposition

Do not create `docs/tranches/AZ-II/waves/cutover/O3b.md` for P1 at
this point. O3b becomes mandatory only if an O3 redress agent proves
one of these blockers with evidence:

1. the projection-totality failure requires `Parsed<R>` or
   `TapeDirect` deletion before a document-owned proof can compile;
2. the generated residue is produced by files outside O3's file bounds;
3. a runtime document API needed for projection totality cannot be
   added without touching O4/O5/O6-owned surfaces;
4. a zero-residue scan conflicts with preserving a non-StructDirect
   TapeDirect grammar before O4.

If any condition is proven, the O3 redress agent must halt and return
an `O3b.md` patch with exact file bounds before source redress
continues. Absent that proof, P1 is an O3 close gate.

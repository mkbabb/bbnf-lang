# AZ-III.W3c - Projection Consumption and Registry Authority

**Name**: W3c - Projection Consumption and Registry Authority
**Opens after**: W3a - Fact and Type Authority and W3b - CSP Strategy Globalization.
**Agents**: up to 10 parallel.
**Hard gate**: StructDirect emitter fallbacks deleted; the pipeline registry has explicit StructDirect bindings (or documented fixture verdicts) for every grammar identified in `W3a-0-pipeline-registry-research.md`; EBNF/CSS/Sheets/BBNF projection tests fail without the new authority and pass with it.
**Status**: planned

## Scope

1. Wire fact, type, and CSP authority (from W3a + W3b) into the
   StructDirect projection emitter and delete the dispatcher fallbacks
   that mask unsupported variants.
2. Resolve the `MultiPathParser`, `ImportPrettyParser`, and
   `SplitPrettyParser` panics at
   `crates/ir/src/registry/strategy.rs:257` per the verdicts in
   `docs/tranches/AZ-III/audit/W3a-0-pipeline-registry-research.md`:
   real grammars route to StructDirect, fixtures are documented or
   removed.
3. Delete or collapse the legacy emitter shims and orthogonal codepaths
   identified in REAUDIT lane 3 §3 and §5 that the AZ-III thesis
   forbids: prettify compatibility stubs at
   `crates/core/src/backend/rust/ir_types.rs:278-320`,
   `recognizer_plan.rs` if no consumer surfaces, the
   `is_fused_number_regex` shim duplication, the `emit_negated_scan_*`
   wrappers, and the trace-feature corpse that AZ-III lane 3 catalogued.
4. Ensure the unsupported StructDirect variants currently routed through
   dispatcher fallback either receive grammar-general implementations or
   keep W3c blocked; no compatibility shim closes the wave.
5. Add focused EBNF/CSS/Sheets/BBNF projection tests that fail without
   the authority and pass with it.

## File Bounds

| File | Access |
|---|---|
| `crates/core/src/backend/rust/emitter/**` | modify-carve (NON shape-specific `struct_direct.rs`; W2 owns shape-specific files inside `shapes/**`) |
| `crates/ir/src/registry/**` | modify |
| `crates/core/src/backend/rust/ir_types.rs` | modify-carve (delete prettify stubs region) |
| `crates/core/src/backend/recognizer_plan.rs` | delete or modify-carve per lane 3 §3 verdict |
| `crates/core/src/backend/rust/trace.rs` | delete (per lane 3 §3 corpse-feature finding) |
| `crates/core/src/generate/regex/emit/**` | modify-carve (collapse shim duplication) |
| `crates/core/src/grammar/generated/*.rs` | modify after regen only |
| `crates/core/tests/projection_*.rs` | modify/create |
| `crates/core/tests/pipeline_compile_request_*.rs` | modify/create |
| `crates/ir/tests/registry_*.rs` | modify/create |
| `docs/benchmarks/AZ-III/W3c-*.txt` | create |
| `docs/tranches/AZ-III/**` | modify |

Emitter carve: W2 owns ONLY shape-specific
`crates/core/src/backend/rust/emitter/shapes/**/struct_direct.rs` files
for parity-driven fixes (CSS, Sheets, BBNF, JSON parity root causes
inside the shape-specific projector). W3c owns the rest of
`crates/core/src/backend/rust/emitter/**` for projection-authority work
(non-shape projectors, dispatch helpers, registry binding glue, payload
wiring, fallback deletion).

Do NOT touch: `crates/ir/src/passes/types/**` (W3a owns; consume only),
`crates/ir/src/passes/csp_strategy/**` and CSP solver crate (W3b owns;
consume only), benchmark harnesses, BA path APIs, BB rewrite inference.

## Agent Units

### AZ-III.W3c.1 Projection Authority Wiring

- Mechanism: thread fact, type, and CSP authority into the StructDirect
  projection emitter; delete the dispatcher fallbacks that mask
  unsupported variants.
- Files: `crates/core/src/backend/rust/emitter/**` (excluding
  `shapes/**/struct_direct.rs`), focused projection tests.
- Sub-gate: focused EBNF/CSS/Sheets/BBNF projection tests fail without
  the authority and pass with it; archived in
  `W3c-projection-authority.txt`.

### AZ-III.W3c.2 Pipeline Registry Authority

- Mechanism: per the W3a.0 research verdict, route real grammars
  (`MultiPathParser`, `ImportPrettyParser`, `SplitPrettyParser` if
  classified as real) through explicit StructDirect bindings; document
  or remove fixtures.
- Files: `crates/ir/src/registry/strategy.rs`, IR registry tests, core
  pipeline tests.
- Sub-gate: `cargo test -p bbnf pipeline_compile_request --profile ax-iter`
  passes for every grammar named in
  `audit/W3a-0-pipeline-registry-research.md`; archived in
  `W3c-registry-authority.txt`.

### AZ-III.W3c.3 Emitter Shim and Fallback Deletion

- Mechanism: delete prettify compatibility stubs
  (`crates/core/src/backend/rust/ir_types.rs:278-320`),
  `crates/core/src/backend/rust/trace.rs` if the parser-trace feature is
  unused, the `emit_negated_scan_{plus,star}` wrappers in
  `crates/core/src/generate/regex/emit/simd.rs:264-274`, and the
  `is_fused_number_regex` shim per
  `crates/core/src/generate/regex/emit/mod.rs:71-86`.
  `crates/core/src/backend/recognizer_plan.rs` is consumer-audited per
  lane 3 §3 and either deleted or retained with named consumer.
- Files: emitter and codegen surfaces enumerated above; tests asserting
  the absence of the deleted shims.
- Sub-gate: `rg -n "scratch_index_for_elem|recovered_static_ident|emit_negated_scan_plus|emit_negated_scan_star|is_fused_number_regex\\b"`
  over `crates/` returns either zero hits or only canonical
  consolidated definitions; archived in `W3c-shim-deletion.txt`.

## Triumvirate Dispatch

If projection authority wiring exposes an unclear root cause, the
registry verdict from W3a.0 reveals scope outside W3a/W3b/W3c bounds, or
emitter shim deletion would force a same-wave change in shape-specific
files (W2 carve), pause that lane and dispatch research, plan
augment/synthesis, and redress/redeployment agents. The synthesis must
fold the change into W3c or open a same-tranche replacement wave before
implementation resumes. HARD CAP for any redress dispatch under W3c:
30 min.

## Hard Gate

1. `cargo test -p bbnf --profile ax-iter` focused
   `pipeline_compile_request`, projection, and StructDirect tests are
   archived and exit green.
2. `cargo test -p bbnf-ir --profile ax-iter` focused registry tests are
   archived and exit green.
3. `rg -n "BoxedEnum|fallback|shim" crates/core/src/backend/rust/emitter/`
   returns no live silent fallback or compatibility shim hit; archived
   in `W3c-no-emitter-fallback.txt`.
4. Every grammar in `audit/W3a-0-pipeline-registry-research.md` has an
   explicit StructDirect binding, an explicit fixture verdict, or
   leaves W3c blocked.
5. Unsupported StructDirect variants are implemented or keep W3c
   blocked; no compatibility shim closes the wave.

## Format And Lint Cadence

Run `cargo fmt --all -- --check`, focused IR/core tests, and
`git diff --check` after each accepted integration batch. Run
`cargo xtask regen --check` after emitter or generated-output changes.
Before W3c closes, rerun `cargo fmt --all -- --check`,
`git diff --check`, and the full W3c authority and registry test
packets.

## Verification Artefacts

- `docs/benchmarks/AZ-III/W3c-projection-authority.txt`
- `docs/benchmarks/AZ-III/W3c-registry-authority.txt`
- `docs/benchmarks/AZ-III/W3c-shim-deletion.txt`
- `docs/benchmarks/AZ-III/W3c-no-emitter-fallback.txt`

## Commit Plan

Expected scopes, each with an evidence-bearing body:

- `feat(projection/struct-direct): wire authority into emitter`
- `fix(registry/strategy): bind MultiPath/Import/Split or document fixtures`
- `fix(emitter/cleanup): delete prettify stubs and trace corpse`
- `fix(generate/regex): collapse fused-number and negated-scan shims`
- `test(projection/authority): cover EBNF/CSS/Sheets/BBNF projection`
- `docs(az-iii.W3c): close projection and registry authority evidence`

Each broad commit body cites the production consumer, the failing-before
test, the W3a.0 research verdict for any registry binding, and the gate
command it unblocks.

## Dependencies

- **Depends on**: W0 - Quarantine and Dispatch Repair, W0p - Throughput
  Substrate, W1 - O5 Reclose, W3a - Fact and Type Authority, W3b - CSP
  Strategy Globalization. Consumes the
  `audit/W3a-0-pipeline-registry-research.md` verdict.
- **Blocks**: W4 - Benchmark, Profile, and Workspace Truth, W5 -
  Terminal Close and Handoff.

## Archaeology

The 2026-04-30 REAUDIT lane 1 cluster
`bbnf::pipeline_compile_request::*` (six panics at
`crates/ir/src/registry/strategy.rs:257`) and lane 3 §3/§5 (16 shim and
orthogonal-codepath findings) are the substrate W3c demolishes. The
research sub-unit `W3a.0` (housed inside W3a) classifies
`MultiPathParser`, `ImportPrettyParser`, and `SplitPrettyParser` before
W3c dispatches. The W2 vs W3 emitter race that the prior single W3
introduced is resolved by the carve in this wave's File Bounds: W2 owns
shape-specific projection fixes; W3c owns the rest.

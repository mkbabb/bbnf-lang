# AZ-III.W3a - Fact and Type Authority

**Name**: W3a - Fact and Type Authority
**Opens after**: W0p - Throughput Substrate, W1 - O5 Reclose.
**Agents**: up to 6 parallel.
**Hard gate**: durable egraph/recognizer/node/projection facts are consumed by production passes, and the type obligation solver replaces every silent `BoxedEnum` fallback for cyclic and heterogeneous alternation with a named obligation or grammar-general type.
**Status**: planned

## Scope

1. Persist egraph, recognizer, node, and projection facts as a durable
   authority consumed by downstream layout, dispatch, type inference, and
   projection passes; each fact has a production consumer that fails
   without it.
2. Replace the silent `BoxedEnum` fallback at
   `crates/ir/src/passes/types/constraint/reference.rs:74` with a typed
   obligation that surfaces compound `Ref` resolution explicitly.
3. Replace the silent `BoxedEnum` fallback at
   `crates/ir/src/passes/types/constraint/revise.rs:123` with a typed
   obligation that surfaces heterogeneous alternation joins explicitly.
4. Add tests covering EBNF/CSS-like heterogeneous alternations and
   recursive grammar rules; tests must fail without the obligation
   substrate and pass with it.
5. Land a `W3a.0 Pipeline Registry Research` doc artefact (research
   sub-unit below) before any planning commit dispatches that touches
   `crates/ir/src/registry/strategy.rs`.

## File Bounds

| File | Access |
|---|---|
| `crates/ir/src/egraph/**` | modify |
| `crates/ir/src/passes/types/**` | modify |
| `crates/ir/src/passes/recognizers/**` | modify-carve |
| `crates/ir/src/passes/payload/**` | modify-carve |
| `crates/ir/src/passes/projection/**` | modify-carve |
| `crates/ir/src/passes/nodes/**` | modify-carve |
| `crates/ir/tests/**` | modify/create |
| `crates/core/tests/types_*.rs` | modify/create |
| `crates/core/tests/projection_*.rs` | modify/create |
| `docs/benchmarks/archive/AZ-III/W3a-*.txt` | create |
| `docs/tranches/AZ-III/audit/W3a-0-pipeline-registry-research.md` | create |
| `docs/tranches/AZ-III/**` | modify |

Do NOT touch: `crates/ir/src/passes/strategy/**` (W3b owns), CSP solver
crate (W3b owns), `crates/core/src/backend/rust/emitter/**`
(W2 + W3c carve, see W3c file bounds), generated grammar Rust outputs,
benchmark harnesses, BA/BB source.

## Agent Units

### AZ-III.W3a.0 Pipeline Registry Research

- Mechanism: enumerate every caller of `MultiPathParser`,
  `ImportPrettyParser`, and `SplitPrettyParser`; classify each as a
  test fixture or a real grammar; if real grammars, document the
  StructDirect routing they require; if test fixtures, document the
  cleanup or registry policy.
- Files:
  `docs/tranches/AZ-III/audit/W3a-0-pipeline-registry-research.md`.
- Sub-gate: research doc exists with verdict per caller and a binding
  recommendation that W3a planning commits consume; W3c emitter and
  registry work cites this doc as input.

### AZ-III.W3a.1 Durable Fact Authority

- Mechanism: thread egraph, recognizer, node, and projection facts
  through a single durable authority surface; each fact carries a named
  production consumer that breaks without it.
- Files: `crates/ir/src/egraph/**`,
  `crates/ir/src/passes/recognizers/**`,
  `crates/ir/src/passes/nodes/**`,
  `crates/ir/src/passes/projection/**`.
- Sub-gate: a production layout/dispatch consumer fails a focused test
  when the fact is removed; archived in `W3a-fact-authority.txt`.

### AZ-III.W3a.2 Type Obligation Solver - Compound Ref

- Mechanism: replace `crates/ir/src/passes/types/constraint/reference.rs:74`
  silent `BoxedEnum` fallback with an `UnresolvedCompoundRef` obligation;
  cycles produce a named diagnostic and a grammar-general layout.
- Files: `crates/ir/src/passes/types/constraint/reference.rs`,
  `crates/ir/src/passes/types/**`, IR and core tests.
- Sub-gate: heterogeneous EBNF alternation tests pass; the obligation
  surfaces in the diagnostic stream rather than being swallowed; archived
  in `W3a-types-obligations.txt`.

### AZ-III.W3a.3 Type Obligation Solver - Heterogeneous Alt

- Mechanism: replace `crates/ir/src/passes/types/constraint/revise.rs:123`
  silent `BoxedEnum` fallback with a `HeterogeneousAltJoin` obligation;
  CSS-like heterogeneous alts produce a named obligation and a
  grammar-general layout.
- Files: `crates/ir/src/passes/types/constraint/revise.rs`,
  `crates/ir/src/passes/types/**`, IR and core tests.
- Sub-gate: heterogeneous CSS alternation tests pass; archived in
  `W3a-types-obligations.txt`.

## Triumvirate Dispatch

If fact authority, type obligation work, or the W3a.0 research reveals an
unclear root cause, a file-bound conflict with W3b/W3c, or a registry
question whose verdict cannot be folded inside W3a bounds, pause that
lane and dispatch research, plan augment/synthesis, and
redress/redeployment agents. Their outputs must be folded into W3a or a
same-tranche replacement wave before implementation resumes. HARD CAP
for any redress dispatch under W3a: 30 min.

## Hard Gate

1. `cargo test -p bbnf-ir --profile ax-iter` focused authority and
   type-obligation tests are archived and exit green.
2. `cargo test -p bbnf --profile ax-iter` focused EBNF/CSS-like
   heterogeneous-alternation and recursive-rule tests are archived and
   exit green.
3. Each new fact, recognizer, node, or projection authority surface has
   a production consumer named in
   `docs/benchmarks/archive/AZ-III/W3a-fact-authority.txt`; disconnecting the
   consumer fails a test.
4. `rg -n "BoxedEnum" crates/ir/src/passes/types/constraint/` over the
   constraint solver returns no live silent fallback hit; archived in
   `W3a-no-silent-fallback.txt`.
5. `docs/tranches/AZ-III/audit/W3a-0-pipeline-registry-research.md`
   exists with explicit per-caller verdicts; W3c registry work consumes
   this doc as a dependency, named in W3c's archaeology section.

## Format And Lint Cadence

Run `cargo fmt --all -- --check`, focused IR/core tests for the integrated
lane, and `git diff --check` after each accepted integration batch. Run
`cargo xtask regen --check` only if recognizer or projection facts feed
generated outputs. Before W3a closes, rerun `cargo fmt --all -- --check`,
`git diff --check`, and the full W3a authority test packet.

## Verification Artefacts

- `docs/benchmarks/archive/AZ-III/W3a-fact-authority.txt`
- `docs/benchmarks/archive/AZ-III/W3a-types-obligations.txt`
- `docs/benchmarks/archive/AZ-III/W3a-no-silent-fallback.txt`
- `docs/tranches/AZ-III/audit/W3a-0-pipeline-registry-research.md`

## Commit Plan

Expected scopes, each with an evidence-bearing body:

- `docs(az-iii.W3a.0): pipeline registry research`
- `feat(facts/authority): durable egraph/recognizer/node/projection facts`
- `fix(types/obligations): replace silent BoxedEnum on compound Ref`
- `fix(types/obligations): replace silent BoxedEnum on heterogeneous Alt`
- `test(types/obligations): cover heterogeneous and cyclic shapes`
- `docs(az-iii.W3a): close fact and type authority evidence`

Each broad commit body cites the production consumer that prevents
substrate-only close, the failing-before test, and the gate command it
unblocks.

## Dependencies

- **Depends on**: W0 - Quarantine and Dispatch Repair, W0p - Throughput
  Substrate, W1 - O5 Reclose.
- **Blocks**: W3b - CSP Strategy Globalization (CSP consumes obligation
  outputs), W3c - Projection Consumption and Registry Authority, W4 -
  Benchmark, Profile, and Workspace Truth, W5 - Terminal Close and
  Handoff.

## Archaeology

The 2026-04-30 REAUDIT lane 3 named
`crates/ir/src/passes/types/constraint/reference.rs:74` and
`crates/ir/src/passes/types/constraint/revise.rs:123` as the two
production lines that swallow compound-Ref and heterogeneous-Alt joins
into `BoxedEnum`. Per AZ-III invariant 7 (no silent fallback) the
fallback must produce a named obligation. The audit also recorded that
durable egraph/node/projection facts have no consumer-paired production
surface today: substrate exists but is not consumed. W3a converts the
substrate into a consumed authority. The W3a.0 research sub-unit picks
up the AZ-III REAUDIT lane 1 recommendation that the
`pipeline_compile_request` cluster (`MultiPathParser`,
`ImportPrettyParser`, `SplitPrettyParser`) be classified as fixtures or
real grammars before W3c dispatches; the verdict gates W3c registry
work.

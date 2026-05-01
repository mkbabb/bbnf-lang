# AZ-III.W3b - CSP Strategy Globalization

**Name**: W3b - CSP Strategy Globalization
**Opens after**: W3a - Fact and Type Authority.
**Agents**: up to 6 parallel.
**Hard gate**: shape, layout, and dispatch CSP constraints are installed AND consumed by production passes; `shape_dict::install` is no longer a no-op; emitter/dispatch decisions cite CSP facts.
**Status**: planned

## Scope

1. Install shape, layout, and dispatch constraints in the CSP solver
   crate; each constraint has a production consumer.
2. Replace the no-op `shape_dict::install` body at
   `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:134-136`
   with a real installer that emits durable shape decisions.
3. Wire the global CSP decision surface so emitter and dispatch passes
   read shape, layout, and dispatch facts from the CSP authority rather
   than from sidecar comments or per-call recomputation.
4. Add tests that fail without the global CSP authority and pass with
   it; the obligation outputs from W3a feed the CSP solver.
5. Land the CSP-side documentation update in `crates/csp-solver/**` so
   the strategy domain matches the new global decision surface.

## File Bounds

| File | Access |
|---|---|
| `crates/csp-solver/**` | modify |
| `crates/ir/src/passes/strategy/**` | modify |
| `crates/ir/src/passes/csp_strategy/**` | modify |
| `crates/ir/src/passes/csp_strategy/constraints/**` | modify |
| `crates/ir/src/passes/recognizers/strategy/**` | modify-carve |
| `crates/ir/tests/**` | modify/create |
| `crates/core/tests/csp_*.rs` | modify/create |
| `docs/benchmarks/archive/AZ-III/W3b-*.txt` | create |
| `docs/tranches/AZ-III/**` | modify |

Do NOT touch: `crates/ir/src/passes/types/**` (W3a owns; consume only),
`crates/ir/src/registry/**` (W3c owns), `crates/core/src/backend/rust/emitter/**`
(W3c owns), generated grammar Rust outputs, benchmark harnesses, BA/BB
source.

## Agent Units

### AZ-III.W3b.1 Shape Constraint Installer

- Mechanism: replace the no-op `shape_dict::install` with a real
  installer that emits one durable shape decision per dispatch site;
  consumers read from the CSP fact surface.
- Files:
  `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs`,
  `crates/csp-solver/**`, `crates/ir/src/passes/csp_strategy/**`.
- Sub-gate: `cargo test -p bbnf-ir csp_shape --profile ax-iter` is
  archived and green; `rg -n "install\\(.*-> usize \\{ 0 \\}"` over the
  constraint surface is empty.

### AZ-III.W3b.2 Layout Constraint Installer

- Mechanism: install layout-shape decisions consumed by payload and
  recognizer passes; emit one decision per layout site.
- Files: `crates/ir/src/passes/csp_strategy/constraints/**`,
  `crates/ir/src/passes/strategy/**`, `crates/csp-solver/**`.
- Sub-gate: a payload-layout consumer fails without the layout
  decision; archived in `W3b-layout-consumer.txt`.

### AZ-III.W3b.3 Dispatch Constraint Installer

- Mechanism: install dispatch-strategy decisions consumed by recognizer
  strategy and emitter dispatch surfaces.
- Files: `crates/ir/src/passes/csp_strategy/constraints/**`,
  `crates/ir/src/passes/recognizers/strategy/**`,
  `crates/ir/src/passes/strategy/**`.
- Sub-gate: a recognizer dispatch consumer fails without the dispatch
  decision; archived in `W3b-dispatch-consumer.txt`.

### AZ-III.W3b.4 CSP Solver Crate Alignment

- Mechanism: ensure the CSP solver crate exposes the strategy domain in
  a form the IR consumer can read; close any leftover orthogonal API
  inside `crates/csp-solver/**` per `feedback_isomorphic_api` and
  `feedback_csp_always_optimize`.
- Files: `crates/csp-solver/**`.
- Sub-gate: solver-side tests for the new domain pass; archived in
  `W3b-csp-solver-tests.txt`.

## Triumvirate Dispatch

If a constraint installer cannot land its consumer in the same wave, if
the CSP solver API requires an orthogonal change to consume the new
domain, or if a file-bound conflict surfaces with W3a or W3c, pause that
lane and dispatch research, plan augment/synthesis, and
redress/redeployment agents. The synthesis must amend W3b or open a
same-tranche replacement wave before implementation resumes. HARD CAP
for any redress dispatch under W3b: 30 min.

## Hard Gate

1. `cargo test -p csp-solver --profile ax-iter` is archived and green.
2. `cargo test -p bbnf-ir csp_ --profile ax-iter` for the focused
   constraint test surface is archived and green.
3. `cargo test -p bbnf --profile ax-iter` focused CSP-consumer tests
   archived and green.
4. `rg -n "shape_dict::install|install\\(.*-> usize \\{ 0 \\}"` over
   `crates/ir/src/passes/csp_strategy/constraints/` returns no no-op
   hits; archived in `W3b-no-noop-installer.txt`.
5. Each shape, layout, and dispatch constraint has a named production
   consumer in `W3b-csp-authority.txt`; disconnecting the consumer
   fails a test.

## Format And Lint Cadence

Run `cargo fmt --all -- --check`, focused IR/csp-solver/core tests, and
`git diff --check` after each accepted integration batch. Before W3b
closes, rerun `cargo fmt --all -- --check`, `git diff --check`, and the
full W3b CSP test packet.

## Verification Artefacts

- `docs/benchmarks/archive/AZ-III/W3b-csp-authority.txt`
- `docs/benchmarks/archive/AZ-III/W3b-layout-consumer.txt`
- `docs/benchmarks/archive/AZ-III/W3b-dispatch-consumer.txt`
- `docs/benchmarks/archive/AZ-III/W3b-csp-solver-tests.txt`
- `docs/benchmarks/archive/AZ-III/W3b-no-noop-installer.txt`

## Commit Plan

Expected scopes, each with an evidence-bearing body:

- `feat(csp/shape-dict): real installer with consumer wiring`
- `feat(csp/layout): layout constraints with payload consumer`
- `feat(csp/dispatch): dispatch constraints with strategy consumer`
- `chore(csp-solver): align strategy domain with global decision surface`
- `test(csp/authority): cover shape, layout, dispatch consumers`
- `docs(az-iii.W3b): close CSP globalization evidence`

Each broad commit body cites the production consumer, the failing-before
test, and the no-op deletion proof.

## Dependencies

- **Depends on**: W0 - Quarantine and Dispatch Repair, W0p - Throughput
  Substrate, W1 - O5 Reclose, W3a - Fact and Type Authority (CSP
  consumes obligation outputs).
- **Blocks**: W3c - Projection Consumption and Registry Authority, W4 -
  Benchmark, Profile, and Workspace Truth, W5 - Terminal Close and
  Handoff.

## Archaeology

The 2026-04-30 REAUDIT recorded that CSP `shape_dict::install` returns
constant zero today (lane 3 row 9), that the strategy domain is split
between `crates/csp-solver` and `crates/ir/src/passes/strategy` without
a shared decision surface (lane 5 §7.1), and that
`feedback_csp_always_optimize` requires CSP to be foundational rather
than gated by profile share. W3b consolidates the shape, layout, and
dispatch axes into a single global authority, lands the no-op
replacement, and pairs each constraint with a production consumer.

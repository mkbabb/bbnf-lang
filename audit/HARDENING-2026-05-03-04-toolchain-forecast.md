# Hardening 04 - Toolchain Forecast

Date: 2026-05-03.
Worktree: `/Users/mkbabb/Programming/bbnf-lang`, branch `master`, HEAD `baf7df2d`.
Scope: BA / BB / BC toolchain forecast only. No source or tranche specs were edited.

Note: the worktree was already dirty before this lane (`docs/GESTALT.md`, several `playground/src/components/**` files, and `docs/benchmarks/AZ-I/`). This report did not touch those paths.

## Sources Read

- `docs/HARDENING-AUDIT-PROMPT.md` §Toolchain-Forecast: asks for cold/warm `cargo iter-check`, `cargo nextest --profile ax-iter`, regen wall after typed-record emission, per-shape-emitter compile cost where feasible.
- `docs/tranches/meta-audit/04-toolchain-pain.md`: predecessor method; its pre-B2 proc-macro bottleneck has mostly moved. Current risk is now xtask/CSS L4 regen and generated-code churn.
- `Makefile`: iteration aliases delegate to `.cargo/config.toml`; `regen`/`regen-check` route through `cargo xtask regen`; `iter-grammar` chains single grammar regen + `cargo iter-check` + filtered nextest.
- `.cargo/config.toml`: routine `iter-check` excludes `bbnf-bootstrap`, `gorgeous`, `bbnf-analysis`, `bbnf-lsp`; each excluded crate has a fast-path; `iter-check-full` is close-only; bench surfaces use `ay-final` and `bench-iter`.
- `.config/nextest.toml`: `ax-iter` fail-fast, no retries, `test-threads = 4`; close profile has 120 s slow windows.
- `docs/instructions/PROFILING.md`: `cargo xtask regen` is the post-B2 path; pre-B2 80+ min expand path retired; shared-target profiling rules prohibit concurrent cargo against one target.
- BA/BB/BC top-level + wave specs.

## Lightweight Wall-Clock Matrix

Commands were run from the main worktree. I did not run full samply, full bench matrices, or full workspace nextest because those are explicitly close/profiling-grade and a concurrent cargo test was already holding the shared `target/ax-iter` lock.

| # | Command | State | Wall | Exit | Notes |
|---|---|---:|---:|---|
| 1 | `cargo metadata --no-deps --format-version 1` | dry inventory | 0.11 s | 0 | Confirms 14 workspace members and 9 manifest grammars without compiling. |
| 2 | `cargo iter-check` | semi-cold/stale deps | 24.60 s | 0 | Rechecked deps and workspace-minus-heavy set. `bbnf` emitted 189 warnings, dominated by generated files. |
| 3 | `cargo iter-check` | warm | 0.47 s | 0 | Warm routine check remains sub-second, but warning output is still noisy. |
| 4 | `cargo xtask regen --check` | warm compile, full 9 grammar regen/check | 59.98 s | 0 | `regen --check: clean (9 of 9 grammars matched)`. CSS L4 dominates: `compile_paths_request` 52.53 s, `generate_all` 0.46 s, `prettyplease` 4.23 s. |
| 5 | `cargo xtask regen --grammar json --check` | blocked | killed after ~43 s wait | killed | Blocked on Cargo build-directory lock held by a pre-existing `cargo test -p bbnf-ir --test substrate_audit ... --profile ax-iter`. No source was changed. |
| 6 | `cargo nextest run --workspace --cargo-profile ax-iter` | not run | n/a | n/a | Full workspace nextest is close-gate scope and would have contended with the existing target lock. |
| 7 | full samply / fat-LTO bench matrix | not run | n/a | n/a | Explicitly outside lightweight lane scope. Forecast uses wave specs + `PROFILING.md` command contracts. |

Useful per-grammar regen facts from command #4:

| Grammar | `compile_paths_request` | `generate_all` | `prettyplease` | Forecast |
|---|---:|---:|---:|---|
| `bbnf` | 25 ms | 44 ms | 320 ms | Cheap enough; generated file is 21,503 LOC. |
| `json` | 4 ms | 8 ms | 55 ms | Cheap enough; useful smoke target. |
| `css_l4` | 52.53 s | 455 ms | 4.23 s | Dominant full-regen cost; emitted CSP `budget_exceeded` fallback. |
| `css_pretty` | 18 ms | 44 ms | 242 ms | Cheap. |
| `google_sheets` | 28 ms | 45 ms | 246 ms | Cheap. |
| `ebnf` | 16 ms | 19 ms | 141 ms | Cheap. |
| `bnf` | 4 ms | 8 ms | 58 ms | Cheap. |
| `csv` | 3 ms | 6 ms | 34 ms | Cheap. |
| `math` | 3 ms | 4 ms | 19 ms | Cheap. |

Generated/emitter inventory:

| Surface | LOC |
|---|---:|
| `crates/core/src/grammar/generated/*.rs` | 169,785 |
| `crates/core/src/grammar/generated/css_l4.rs` | 107,138 |
| shape emitter modules under `crates/core/src/backend/rust/emitter/shapes/**` | 9,624 |
| top-level Rust emitter modules sampled | 2,787 |
| `xtask/src/regen.rs` | 849 |

## Forecast Against BA / BB / BC

BA is toolchain-heavy before it is runtime-heavy. BA.W2 creates the document/value emitters and regenerates all 9 grammar outputs; BA.W3 adds checkpoint + predictive dispatch and regenerates again; BA.W4 collapses eager/lazy and regenerates again; BA.W5 removes `__path_plan` and regenerates affected grammars. BA.W6 then runs fat-LTO matrices and samply. Evidence: BA owns generated outputs at BA.md lines 108-115; BA.W2 lines 13-18 and 93-109; BA.W3 lines 13-18 and 88-98; BA.W4 lines 13-19 and 88-98; BA.W5 lines 13-18 and 84-94; BA.W6 lines 13-18 and 67-77.

BB adds toolchain pressure in a different place: BB.W0 recreates `crates/ir/src/rewrites/`; BB.W1-W3 add enumerator/oracle/ranker tests; BB.W4 wires grammar-colocated `rewrites/*.ron` into `cargo xtask regen`, requires generated `.rs` shrink, and verifies the rule admission chain with `BBNF_EGRAPH_REPORT=1` + `cargo expand`; BB.W5 and W6 add CI/bench matrices. Evidence: BB.md lines 84-90 and 108-127; BB.W4 lines 64-90.

BC is mostly cleanup but still gates on the same expensive surfaces: every wave repeats full regen/full workspace nextest, W1 splits large modules, W3 adds perf-claim CI, W5 touches sibling repos and path patches, and W6 repeats full matrices. Evidence: BC.md lines 84-90 and 108-124; BC.W1 lines 63-83; BC.W5 lines 76-96.

## Ranked Pain Points And Pre-W0 Mitigations

### 1. CSS L4 dominates full regen and will dominate every close gate

Friction x frequency: **high x every wave close**. `cargo xtask regen --check` took 59.98 s; 52.53 s was `css_l4` `compile_paths_request`, and the run printed `csp_strategy::solve_component budget_exceeded nodes_explored=1000000 constraints_added=1704 sites=483 component_size=187`. The rest of the 9-grammar fleet is seconds-scale.

Why the specs underestimate it: BA/BB/BC hard gates say `cargo xtask regen --check` 9/9 green at every wave close, but they do not budget CSS L4 as the dominant unit. BA.W2-W5 and BB.W4 specifically regenerate generated code, so a one-minute full check becomes repeated integration overhead; if direct-projection increases CSS L4 layout complexity, this can regress sharply.

Pre-W0 mitigation:

- BA.W0 `W0-dev-baseline.txt` should record per-grammar timings, not only command-level wall.
- Add a close threshold for CSS L4 `compile_paths_request` (current baseline 52.53 s; alert at >75 s, triumvirate at >120 s).
- Require `cargo xtask regen --check --staged` for per-agent pre-commit checks when source/generated paths are scoped; reserve full 9/9 regen for integration and close.
- Put the CSP `budget_exceeded` fallback row into the W0 baseline. If direct-projection increases the `component_size`/`sites` numbers, the regression is visible before W2 close.

### 2. Full workspace nextest is mandated everywhere but currently unbudgeted

Friction x frequency: **high x every wave close**. BA, BB, and BC all require `cargo nextest run --workspace --cargo-profile ax-iter` at close; many wave sub-gates also ask for full workspace nextest. I did not run it here because a pre-existing `cargo test -p bbnf-ir --test substrate_audit ... --profile ax-iter` held the target lock for more than 90 seconds, and full nextest would have contended.

Why the specs underestimate it: `.config/nextest.toml` caps test threads at 4 and uses fail-fast for `ax-iter`, but the wave specs do not distinguish focused test proof from full integration proof. BA.W0 already asks for row-by-row deltas for `cargo nextest run --workspace`, which is good; later waves repeat the command without a wall ceiling.

Pre-W0 mitigation:

- Make BA.W0's dev baseline the authority for full nextest wall; fail later wave-close if the wall regresses by >25% without a named source.
- Sub-agents should run focused nextest only; orchestrator integration runs the full workspace once per wave after merging owned slices.
- Record test binary compile wall separately from test execution wall with nextest's close artefacts; direct-projection may move cost from runtime tests into test-binary compile.

### 3. Generated Rust volume and warning noise can hide real compile regressions

Friction x frequency: **medium-high x every compile / every generated regen**. Generated files are already 169,785 LOC; `css_l4.rs` alone is 107,138 LOC. `cargo iter-check` emitted 189 warnings from `bbnf` in the current tree, largely generated-code warnings (`unused_comparisons`, `break_with_label_and_loop`, dead code). BA.W2-W5 and BB.W4 intentionally churn generated files.

Why the specs underestimate it: BA correctly names generated output as an owned surface, but it does not define a generated-warning budget or emitted-LOC budget. Direct `<Grammar>Document` and `<Grammar>Value` emission can increase monomorphized code and warning volume before runtime templates retire.

Pre-W0 mitigation:

- Add a generated-code warning census to BA.W0 baseline: warning count by lint and generated file, current observed `bbnf` warning count 189 during `iter-check`.
- For BA.W2, require per-grammar generated LOC delta and `cargo iter-check` wall delta in the regen commit body.
- Add generated-file `allow(...)` policy only for mechanical lints proven to be emitter artefacts; do not let warning output become the normal compile surface.
- For per-shape emitters, measure `cargo iter-check` after touching one shape module and after regenerating one grammar; use those two walls as the per-shape compile-cost proxy.

### 4. Target-lock contention is still the easiest way to waste lane time

Friction x frequency: **high x parallel audit/implementation waves**. The attempted `cargo xtask regen --grammar json --check` blocked on the build-directory lock while another cargo test was compiling `parse_that` under the same `target/ax-iter`. The probe was killed after about 43 seconds of waiting. This exactly matches `.cargo/config.toml` lines 22-26 and `PROFILING.md` lines 210-214.

Why the specs partially cover it: BA/BB/BC wave specs do assign distinct sibling `CARGO_TARGET_DIR`s. The risk is orchestration drift: one cargo command in the main worktree, or one profiling wave using the shared target outside the prepare-then-profile discipline, serializes unrelated work.

Pre-W0 mitigation:

- Before dispatch, run a cargo-process/lock preflight and record it in BA.W0's dev baseline.
- Every implementation lane prompt should export its declared `CARGO_TARGET_DIR` before any cargo command; do not rely on humans remembering the table.
- Main worktree cargo commands should be read-only orchestration probes only, never concurrent with active lane cargo.
- Profiling waves may share one target only after preparation; all cargo builds against the shared target are sequenced.

### 5. Final perf gates lack a cheap rehearsal path

Friction x frequency: **very high x W6 and any perf-citing mid-wave gate**. BA.W6, BB.W6, and BC.W6 require full fat-LTO matrices plus samply 7-artefact contracts. BA.W3 and BA.W4 also require samply for the checkpoint and sonic-class `get` claims. `PROFILING.md` has prepared-binary reuse, but the wave specs do not require a dry-run manifest before the expensive run.

Why the specs underestimate it: the specs are evidence-correct but operationally thin. They say "run samply" and "run full benchmark matrix"; they do not require `make profile` / `scripts/prepare-profile-wave.sh` rehearsal, binary reuse verification, or a wall ceiling before the close wave.

Pre-W0 mitigation:

- Add a pre-W0 dry rehearsal: `scripts/prepare-profile-wave.sh` with an absolute `CARGO_TARGET_DIR`, stopping before samply capture if binaries are already reusable.
- For each perf-citing hard gate, require a manifest row before the wave opens: bench target, feature set, profile tier, expected binary path, expected profile output directory, and whether the binary was `reused` or `rebuilt`.
- In BA.W3/W4, do not wait until W6 to prove the 7-artefact path works; save one small smoke profile contract immediately after the first perf-citing code lands.

## Commands Not Run And Why

- Full `cargo nextest run --workspace --cargo-profile ax-iter`: not run because it is close-gate scope and the main `target/ax-iter` was already locked by another cargo test.
- `cargo iter-check-full`: not run because it would compile the heavy excluded crates and compete for the same locked target. Use it as an integration/close measurement, not in this lightweight lane.
- Full fat-LTO `cargo bench-*`, `cargo final-bench`, samply: not run because the user explicitly requested lightweight clocks and no extremely long profiling.
- Cold cache rebuilds via deleting `target/`: not run because that would destroy shared local state and exceed the audit lane's scope.

## Bottom Line

BA.W0 is not ready to open without a toolchain baseline amendment: the current full regen gate is already one minute and CSS L4 accounts for nearly all of it; full nextest is mandated but not wall-budgeted; generated warnings are noisy enough to mask regressions; target-lock contention remains a live failure mode. The mitigation is not a new build system: record per-grammar regen timing, nextest wall, generated-warning counts, and target-lock preflight in BA.W0 before direct-projection emitter work begins.

# CH3 W0 V6 Hardening Challenge - Regression

## Verdict

ACCEPT, confidence 96%.

Target `6c0bc15d44142abf0b965d9daee7070b1f32dd99`
(`fix(sk-v8-wave0): fold hardening V5 row identity blockers`) folds the V5 CH1
and CH4 blockers without reopening a CH3 regression route. I found no material
evidence that W0 silently regresses an admitted row, reopens a `REDRESS.md`
route, permits behavior drift outside telemetry/report/gate code, or paper-closes
schema/report completion without live gate evidence.

This is a CH3 verdict only. ORCHESTRATOR CH3 is the regression lens
(`restart/prompts/ORCHESTRATOR.md:85`), CH6 separately guards anti-paper-close
(`restart/prompts/ORCHESTRATOR.md:88`), and convergence still requires two
consecutive qualifying ACCEPT cycles with no open critical defects
(`restart/prompts/ORCHESTRATOR.md:118`, `restart/prompts/ORCHESTRATOR.md:120`,
`restart/prompts/ORCHESTRATOR.md:123`). V5 reset that counter
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:61`).

## Reviewed Surfaces

- W0 authority and exit gates: required telemetry and gate consumption
  (`restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`), 38 current rows and +/-1.0%
  W0 target (`restart/skinny/tranches/sk-v8/SPEC.md:159`,
  `restart/skinny/tranches/sk-v8/SPEC.md:160`), W0 exit gate
  (`restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:325`,
  `restart/skinny/tranches/sk-v8/SPEC.md:333`,
  `restart/skinny/tranches/sk-v8/SPEC.md:336`), and inherited pre-blocks
  (`restart/skinny/tranches/sk-v8/SPEC.md:731`,
  `restart/skinny/tranches/sk-v8/SPEC.md:743`,
  `restart/skinny/tranches/sk-v8/SPEC.md:750`).
- W0 dispatch contract: telemetry-only W0
  (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:58`), explicit W0 redress
  checks (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:80`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:83`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:85`), downstream block
  (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:92`), and pre-blocked
  routes (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:164`).
- Current row state and sidecar caveats in the handoff
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:31`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:36`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:46`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`).
- Prior hardening: V4 required row/fingerprint folding
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:38`),
  and V5 specifically required exact outcome/verdict baselines plus row-scoped
  Criterion fingerprinting
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:40`).
- Current code: `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and Lock 14 frozen-root validator.
- Current report: `skinny/RESULTS.md` row table and W0 manifest.
- Relevant `skinny/REDRESS.md` route families named by SPEC Section 10, including
  PMULL/CTZ/B6 and retained/direct materialization residue
  (`skinny/REDRESS.md:2512`, `skinny/REDRESS.md:2544`,
  `skinny/REDRESS.md:2595`).

## Commands And Evidence

- `git rev-parse HEAD`: `6c0bc15d44142abf0b965d9daee7070b1f32dd99`.
- `git show --name-status --oneline --no-renames HEAD^..HEAD`: only
  `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs` changed.
- `git diff --name-only HEAD^ HEAD -- <frozen roots>`: no output for grammar,
  test data, runtime, IR, passes, codegen, grammar/parser, SIMD/build/ext,
  direct, typed, parity, scan, materialization, and real-typed schema roots.
- `git diff --check HEAD^..HEAD`: pass.
- `awk ... skinny/RESULTS.md`: current row counts match the packet: 16
  `parse_only S/NO-GO`, 1 `parse_only L/NO-GO`, 3
  `direct_to_struct A/GO`, 14 `direct_to_struct N-direct/NO-GO`, and 4
  `real_typed_struct A/GO`; example rows are rendered at
  `skinny/RESULTS.md:5`, `skinny/RESULTS.md:6`, `skinny/RESULTS.md:7`, and
  `skinny/RESULTS.md:10`.
- `cargo test -p bbnf-bench w0_ -- --nocapture`: pass, 12 report W0 tests and
  8 gate-bin W0 tests. This includes exact opening baseline acceptance and
  negative mutations for `twitter/parse_only S -> K` and
  `twitter/direct_to_struct N-direct/NO-GO -> A/GO`
  (`skinny/crates/bbnf-bench/src/report.rs:1896`,
  `skinny/crates/bbnf-bench/src/report.rs:1949`,
  `skinny/crates/bbnf-bench/src/report.rs:1958`).
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results`: pass against committed
  `skinny/RESULTS.md`; the gate validates W0 before stale/write handling
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:330`).
- Copied `/tmp/skv8-w0-target/criterion` to `/tmp/skv8-ch3-v6.WcndwY/criterion`,
  added only
  `json_canada/sonic_rs_real_typed_struct/new/estimates.json`, then ran
  `CARGO_TARGET_DIR=/tmp/skv8-ch3-v6.WcndwY RUSTFLAGS='-C target-cpu=native'
  cargo xtask gate-json --advisory --check-results`: pass. This confirms the
  V5 CH4 valid-fixture/unvalidated-row repro no longer perturbs the W0 report.

## Regression Findings

1. No admitted-row silent regression found. `Report::validate_sk_v8_w0()` now
   requires exact baseline row count, rejects duplicate or unknown row ids, binds
   each row's `outcome_id` and `verdict` to the W0 opening baseline, and then
   validates Track 1 and Track 2 deltas
   (`skinny/crates/bbnf-bench/src/report.rs:493`,
   `skinny/crates/bbnf-bench/src/report.rs:501`,
   `skinny/crates/bbnf-bench/src/report.rs:511`,
   `skinny/crates/bbnf-bench/src/report.rs:517`,
   `skinny/crates/bbnf-bench/src/report.rs:523`). The baseline model now stores
   `row_id`, `outcome_id`, `verdict`, Track 1, and Track 2
   (`skinny/crates/bbnf-bench/src/report.rs:646`,
   `skinny/crates/bbnf-bench/src/report.rs:648`).

2. No V5 CH4 fingerprint regression found. `criterion_fingerprint` includes only
   `estimates.json`/`metadata.toml` files whose fixture and workload resolve to a
   W0 baseline row (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:733`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:736`). The bench-to-workload map now
   excludes `real_typed_struct` inputs for fixtures without a baseline typed row
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:745`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:753`). The unit test covers
   `json_unvalidated_future`, `json_probes_*`, and the exact Canada
   unvalidated-real-typed comparator file
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1769`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1788`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1794`).

3. No behavior drift or REDRESS route reopening found. The V6 parent diff changes
   only W0 report/gate code, and Lock 14 still marks those surfaces as
   `bench_gate_schema` / `telemetry_only`
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:273`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:285`). Frozen roots include
   runtime, IR, passes, codegen, grammar/parser, SIMD/build/ext, direct, typed,
   parity, scan, materialization, and schema paths
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`). The gate runs the
   Lock 14 validator before fixture/report work
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:42`), and that validator rejects
   frozen-root working-tree or parent-diff drift
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:403`) plus
   `BackendShape`/`UnionTape` drift
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:485`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:488`).

4. No sidecar strict-admission reopening found. Non-strict W0 rows remain
   constrained to `deferred`, `view-boundary`, `parse_utf8=view-boundary`, and
   `escape_complete=yes`
   (`skinny/crates/bbnf-bench/src/report.rs:1004`,
   `skinny/crates/bbnf-bench/src/report.rs:1008`,
   `skinny/crates/bbnf-bench/src/report.rs:1020`,
   `skinny/crates/bbnf-bench/src/report.rs:1026`). Sidecar evidence rejects
   `sidecar-same-run` without a structured manifest
   (`skinny/crates/bbnf-bench/src/report.rs:1203`,
   `skinny/crates/bbnf-bench/src/report.rs:1227`), matching the packet caveat
   that W0 admits no structured sidecar same-run manifest
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:46`).

5. No paper close found for CH3. The accepted path is not merely a schema render:
   `gate-json --check-results` consumes the W0 manifest, validates row identity,
   outcome/verdict identity, Track 1/Track 2 baseline deltas, comparator evidence,
   Lock 14, metadata coherence, and then compares the rendered report to the
   committed artifact before returning success
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:329`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:332`).

## Material Blockers

None for CH3. I did not find a regression blocker requiring W0 rejection after
the V5 fold.

## Residual Risks

- I did not run a fresh full Criterion benchmark capture. This review relies on
  the committed `/tmp/skv8-w0-target` evidence root, gate replay, focused W0
  tests, and copied-root mutation evidence.
- The W0 baseline guard is explicitly focused on current row identity plus Track
  1/Track 2 row throughput; comparator values are gate-consumed for provenance
  and strict-admission refusal, but W1 still has to bind CostFacts and strict
  comparator evidence before behavior waves can admit route quality
  (`restart/skinny/tranches/sk-v8/research/wave-0-plan.md:75`,
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:87`,
  `restart/skinny/tranches/sk-v8/research/wave-0-results-baseline-research.md:151`,
  `restart/skinny/tranches/sk-v8/research/wave-0-results-baseline-research.md:159`,
  `restart/skinny/tranches/sk-v8/SPEC.md:50`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:110`).
- This ACCEPT does not dispatch W1-W6. W1-W6 remain blocked until W0 receives the
  required challenge convergence and each later wave satisfies its own entry
  gates (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:92`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:96`).

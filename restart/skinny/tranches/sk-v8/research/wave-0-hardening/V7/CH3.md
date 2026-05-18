# SK-V8 W0 Hardening V7 CH3 - Regression

Date: 2026-05-18.
Target: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

## Verdict

ACCEPT.

Confidence: 92%.

This is a CH3 regression verdict only. It does not close W0 or dispatch W1-W6;
the orchestrator still requires challenge consolidation and the pass convergence
rule before advancement (`restart/prompts/ORCHESTRATOR.md:110`,
`restart/prompts/ORCHESTRATOR.md:114`,
`restart/prompts/ORCHESTRATOR.md:118`,
`restart/prompts/ORCHESTRATOR.md:123`).

## Reviewed Surfaces

- ORCHESTRATOR CH3 and convergence governance:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:81`,
  `restart/prompts/ORCHESTRATOR.md:85`,
  `restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:118`.
- Live W0 contract and blocks:
  `restart/skinny/tranches/sk-v8/SPEC.md:63`,
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`,
  `restart/skinny/tranches/sk-v8/SPEC.md:760`,
  `restart/skinny/tranches/sk-v8/SPEC.md:803`.
- Live dispatch and handoff posture:
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:63`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:85`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:171`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:31`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:190`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:236`.
- Current row evidence and REDRESS route ledger:
  `skinny/RESULTS.md:3`,
  `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:85`,
  `skinny/RESULTS.md:138`,
  `skinny/RESULTS.md:141`,
  `skinny/REDRESS.md:2130`,
  `skinny/REDRESS.md:2152`,
  `skinny/REDRESS.md:2179`,
  `skinny/REDRESS.md:2589`,
  `skinny/REDRESS.md:2594`.
- V6 rejection and fold target:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:20`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:22`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:29`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:61`.
- W0 code paths:
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:660`,
  `skinny/crates/bbnf-bench/src/report.rs:942`,
  `skinny/crates/bbnf-bench/src/report.rs:1012`,
  `skinny/crates/bbnf-bench/src/report.rs:1083`,
  `skinny/crates/bbnf-bench/src/report.rs:1211`,
  `skinny/crates/bbnf-bench/src/report.rs:1976`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:414`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1075`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1385`,
  `skinny/crates/bbnf-bench/src/gate.rs:140`,
  `skinny/crates/bbnf-bench/src/gate.rs:163`.

## Disposition

No admitted row silently regressed. The current report still has 38 main rows and
38 W0 manifest rows; all manifest rows carry the exact same
`sk-v8-open:criterion-fnv64-9a37562ed3d0383a` run id. The current outcome mix is
16 `S`, 1 `L`, 14 `N-direct`, and 7 `A`, matching the SPEC/HANDOFF posture for
16 substrate-guard parse rows, one hard canada parse failure, 3 direct GO rows,
14 direct NO-GO rows, and 4 real-typed GO rows
(`restart/skinny/tranches/sk-v8/SPEC.md:148`,
`restart/skinny/tranches/sk-v8/SPEC.md:153`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:34`,
`skinny/RESULTS.md:48`,
`skinny/RESULTS.md:85`).

The V6 CH1 blocker is closed for CH3 purposes. V6 rejected because `run_id` was
only non-empty and accepted `sk-v8-open:test`; V7 adds an exact
`SK_V8_OPEN_RUN_ID`, validates every row against it, and tests both a single-row
mutation and a uniform stale prefix mutation
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:22`,
`skinny/crates/bbnf-bench/src/report.rs:336`,
`skinny/crates/bbnf-bench/src/report.rs:660`,
`skinny/crates/bbnf-bench/src/report.rs:1976`). Mixed run-id evidence is not
accepted because there is no row-local alternate run id path left: every row must
equal the exact constant before the report-level row coverage and baseline drift
checks can pass (`skinny/crates/bbnf-bench/src/report.rs:499`,
`skinny/crates/bbnf-bench/src/report.rs:529`).

No REDRESS route is reopened. The live SPEC and DISPATCH continue to block stale
sidecars, permissive/lossy evidence, sidecar/parallel substrate, parser-owned
facts, `UnionTape`, new directives/BIR/backend shape, Track 1/Track 2 coupling,
or behavior admission by analogy (`restart/skinny/tranches/sk-v8/SPEC.md:762`,
`restart/skinny/tranches/sk-v8/SPEC.md:775`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:171`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:178`). The executable
strict-admission guard rejects non-strict rows, measured-validation gaps, plane
mismatch, stale/historical/absent freshness, and sidecar-same-run without a
structured manifest (`skinny/crates/bbnf-bench/src/gate.rs:145`,
`skinny/crates/bbnf-bench/src/gate.rs:157`,
`skinny/crates/bbnf-bench/src/gate.rs:163`,
`skinny/crates/bbnf-bench/src/gate.rs:172`,
`skinny/crates/bbnf-bench/src/gate.rs:495`,
`skinny/crates/bbnf-bench/src/report.rs:1211`,
`skinny/crates/bbnf-bench/src/report.rs:1235`).

The behavior surface stayed frozen. The V7 fold changed docs plus
`report.rs` run-id validation/tests only; the behavior-freeze diff over grammar
input, runtime JSON/tape, SIMD, codegen, generated/product helpers, Track 2,
parity, scan, materialization, and the SIMD scan hook is empty. That satisfies
the V7 cost-governance condition that the larger W0 telemetry/report/gate scope
is admissible only while the frozen behavior surface remains empty
(`restart/skinny/tranches/sk-v8/SPEC.md:335`,
`restart/skinny/tranches/sk-v8/SPEC.md:357`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:154`).

## Commands And Evidence

- `git rev-parse HEAD`: `f452e8373ed717731dd5e720c1d947c086cc22c9`.
- `git show --stat --oneline HEAD`: V7 fold touched only
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md`,
  `restart/skinny/tranches/sk-v8/SPEC.md`, and
  `skinny/crates/bbnf-bench/src/report.rs`.
- `awk` over `skinny/RESULTS.md`: 38 main rows; outcomes `16 S`, `1 L`,
  `14 N-direct`, `7 A`; 38 manifest rows; all 38 manifest run ids are
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`.
- Frozen behavior-surface diff:
  `git diff --name-only 0bd16f6d..HEAD -- skinny/grammars/json.bbnf skinny/crates/runtime/src/grammars/json skinny/crates/runtime/src/tape skinny/crates/bbnf-simd skinny/crates/codegen skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/materialization.rs skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/track2 skinny/crates/parse-that-regex/src/integration/simd_scan_hook`:
  empty output.
- `cargo test -p bbnf-bench w0_ -- --nocapture`: passed 20 W0 tests
  (12 report tests, 8 gate-binary tests).
- `cargo test -p bbnf-bench`: passed 60 tests total across lib, gate binary, and
  doctests.
- `cargo xtask check-json`: passed.
- `cargo xtask check-real-typed`: passed.
- `cargo xtask check-conformance`: passed, `21 valid fixtures accepted; 7 invalid
  fixtures rejected`.
- `git diff --check`: passed.
- `cargo xtask gate-json --advisory --check-results` against the default local
  `skinny/target/criterion` did not accept stale evidence. It failed before
  report admission with `twitter SIMD metadata invalid: SIMD metadata has
  unsupported capture policy`. This local target has stale/non-native metadata,
  while the gate requires native policy and same-capture SIMD metadata
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:57`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:70`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1417`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1427`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1438`). I count this as evidence
  that stale local Criterion state fails closed, not as a W0 row admission.

## Material Blockers

None found for CH3.

I did not find an admitted-row regression, a REDRESS-route reopen, behavior
surface drift, mixed/stale run-id acceptance, or a schema-only close path that
passes the executable W0 gates.

## Residual Risks

- I did not run a fresh full Criterion `bench-json --advisory` in an isolated
  `CARGO_TARGET_DIR` during this challenge. The default local target was stale
  and failed closed, so this CH3 acceptance relies on the checked-in W0 manifest,
  the exact run-id/report validators, focused unit evidence, and the empty frozen
  behavior diff rather than a new full measurement replay.
- The pre-redress W0 plan still has stale prose saying parse rows remain
  substrate-guard non-admission `K`
  (`restart/skinny/tranches/sk-v8/research/wave-0-plan.md:126`,
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:129`). I do not treat
  that as a blocker because the live SPEC, HANDOFF, RESULTS, and executable W0
  validators now use `S` plus the preserved hard `L` failure, but future
  consolidation should avoid citing that older plan line as current authority.

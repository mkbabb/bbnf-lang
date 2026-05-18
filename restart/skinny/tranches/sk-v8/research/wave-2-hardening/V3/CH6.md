# SK-V8 W2 Hardening V3 - CH6

Role: CH6 anti-paper-close audit.
Target reviewed: unchanged W2 V2-folded packet at current HEAD `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

## Verdict

ACCEPT.

Confidence: 97%.

## Findings

1. No residual W2 strict-performance admission is visible. The W2 disposition is
   source/product parity for `apache_builds/real_typed_struct` and
   `citm_catalog/real_typed_struct`, not measured row-table admission
   (`skinny/REDRESS.md:2622-2625`). HANDOFF repeats that the current measured
   authority remains W0-rendered `skinny/RESULTS.md` with four measured
   `real_typed_struct` rows, and that W2 does not claim six measured
   `real_typed_struct A / GO` rows
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:33-40`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175-187`). The W2 proof remains
   checksum/source-product parity through generated Track 1 DirectBuild,
   serde_json as Track 2/oracle, and a separate sonic-rs checksum parity lane
   (`skinny/REDRESS.md:2632-2636`), not a strict throughput win.

2. The apparent benchmark gap is closed as a rejection, not hidden as deferral.
   The W2 plan says that if a benchmark refresh is attempted and the standard
   W0 validator rejects unrelated run-id or throughput drift, `RESULTS.md` stays
   unchanged, benchmark row-table admission is rejected for W2, and
   source/product parity is recorded without weakening W0 validation
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:46-50`). REDRESS 91
   executes that route explicitly: `skinny/RESULTS.md` is unchanged, W2 rejects
   benchmark row-table admission for this wave, and W2 admits source/product
   parity only (`skinny/REDRESS.md:2648-2652`). I do not find a live "benchmark
   later" TODO that would let W2 paper-close while carrying unadmitted row-table
   work forward.

3. W0 run-id validation remains strict. `TelemetryRow::validate_sk_v8_w0`
   rejects any telemetry row whose `run_id` differs from `SK_V8_OPEN_RUN_ID`
   (`skinny/crates/bbnf-bench/src/report.rs:336-340`), and the report validator
   still requires the exact W0 row count, row ids, outcomes, verdicts, and Track
   1/Track 2 baseline deltas
   (`skinny/crates/bbnf-bench/src/report.rs:494-532`). The baseline constant is
   still `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`
   (`skinny/crates/bbnf-bench/src/report.rs:655`), and tests still reject both
   single-row and uniform run-id drift
   (`skinny/crates/bbnf-bench/src/report.rs:2031-2039`). The W2 language points
   at this validator as a reason to reject benchmark admission; it does not
   create a W2 exception to the validator.

4. W3 handoff does not mask unfinished W2 evidence. HANDOFF names W3 as the next
   active wave only after its own research, plan, challenge, and redress gate
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-9`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:189`). W2's carried state is only
   the admitted typed source/product slice plus the explicit benchmark row-table
   rejection.

## Required Folds

None.

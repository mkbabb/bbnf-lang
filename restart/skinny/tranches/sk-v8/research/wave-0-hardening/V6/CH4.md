# SK-V8 W0 Hardening V6 CH4 Review

Verdict: REJECT.

Confidence: 94%.

Lens: CH4 COST. I reviewed current HEAD `6c0bc15d44142abf0b965d9daee7070b1f32dd99`
after the V5 fold, with special pressure on W0 budget, same-wave consumer,
revert practicality, production-behavior containment, gate runtime cost, and
whether the V5 row-manifest fingerprint blocker was closed without opening new
scope.

## Reviewed Surfaces

- Orchestrator CH4 and convergence rules: `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:81`, `restart/prompts/ORCHESTRATOR.md:86`,
  `restart/prompts/ORCHESTRATOR.md:104`, `restart/prompts/ORCHESTRATOR.md:120`,
  `restart/prompts/ORCHESTRATOR.md:125`.
- SK-V8 W0 budget, owner, exit, same-wave consumer, and revert contract:
  `restart/skinny/tranches/sk-v8/SPEC.md:218`,
  `restart/skinny/tranches/sk-v8/SPEC.md:226`,
  `restart/skinny/tranches/sk-v8/SPEC.md:230`,
  `restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:290`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:333`,
  `restart/skinny/tranches/sk-v8/SPEC.md:336`,
  `restart/skinny/tranches/sk-v8/SPEC.md:343`.
- Required W0 telemetry and gate-consumption contract:
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:110`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`.
- HANDOFF and DISPATCH budget and W0-only authority:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:142`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:226`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:47`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:51`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:80`.
- W0 plan and research cost expectations:
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:15`,
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:42`,
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:59`,
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:66`,
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:108`,
  `restart/skinny/tranches/sk-v8/research/wave-0-lock14-baseline-research.md:68`,
  `restart/skinny/tranches/sk-v8/research/wave-0-lock14-baseline-research.md:70`,
  `restart/skinny/tranches/sk-v8/research/wave-0-lock14-baseline-research.md:77`.
- Rendered report and W0 code:
  `skinny/RESULTS.md:44`, `skinny/RESULTS.md:46`,
  `skinny/RESULTS.md:48`, `skinny/RESULTS.md:141`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:329`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:733`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:745`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1770`,
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:493`,
  `skinny/crates/bbnf-bench/src/report.rs:511`,
  `skinny/crates/bbnf-bench/src/report.rs:666`,
  `skinny/crates/bbnf-bench/src/report.rs:1897`.

## Commands And Evidence

- `git rev-parse HEAD`: `6c0bc15d44142abf0b965d9daee7070b1f32dd99`.
- `git diff --numstat 0bd16f6d..HEAD -- skinny/RESULTS.md skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/gate.rs skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/lib.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/xtask/src/main.rs`: `3532 insertions(+), 253 deletions(-)` across 7 W0 report/gate/result files. Notable counts: `skinny/crates/bbnf-bench/src/report.rs` 1431 insertions, `skinny/crates/bbnf-bench/src/bin/gate.rs` 1215 insertions, `skinny/crates/bbnf-bench/src/lock14_baseline.rs` 611 insertions.
- `git diff --numstat HEAD^..HEAD -- skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/report.rs`: V5 fold alone is 336 insertions and 221 deletions across report/gate files.
- `awk ... skinny/RESULTS.md`: `main_rows=38`; manifest count check: `manifest_rows=38 gate_only=38 non_gate_only=0`.
- `git diff --exit-code 0bd16f6d..HEAD -- skinny/grammars/json.bbnf skinny/crates/runtime/src/grammars/json skinny/crates/runtime/src/tape skinny/crates/bbnf-simd skinny/crates/codegen skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/track2 skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/materialization.rs`: exit 0, so I found no parser/scanner/SIMD/codegen/product/generated-output diff in frozen surfaces.
- `(cd skinny && /usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-ch4-v6-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_criterion_fingerprint_excludes_derendered_probe_estimates -- --nocapture)`: PASS, 1 gate-bin test, real 9.08s with cold test target.
- `(cd skinny && /usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-ch4-v6-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture)`: PASS, 1 report test, real 0.11s after compile.
- `(cd skinny && /usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results)`: PASS against committed `skinny/RESULTS.md`, real 6.20s on the existing W0 evidence root.
- Copied-root V5 blocker repro: copied `/tmp/skv8-w0-target/criterion` to `/tmp/skv8-ch4-v6-row-target/criterion`, added `/tmp/skv8-ch4-v6-row-target/criterion/json_canada/sonic_rs_real_typed_struct/new/estimates.json`, then ran `(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-ch4-v6-row-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results)`. PASS, real 17.47s with cold copied target; the rendered run id stayed `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`.

## Material Blockers

1. BLOCKER: W0 no longer fits its stated report/gate/schema/test/doc budget.

   The live W0 cap is `0 production behavior LOC; <=350 report/gate/schema/test/doc LOC`
   (`restart/skinny/tranches/sk-v8/SPEC.md:218`) and the SPEC says a wave that
   exceeds LOC or the 90-minute implementation/redress cap must split before
   dispatch or return REVISE (`restart/skinny/tranches/sk-v8/SPEC.md:226`,
   `restart/skinny/tranches/sk-v8/SPEC.md:230`). DISPATCH repeats the same
   budget and split rule (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:51`). Using `0bd16f6d`
   (`docs(sk-v8-wave0-plan): lock telemetry gate redress`) as the pre-redress W0
   plan baseline, current HEAD changes 7 W0 report/gate/result files by 3532
   insertions and 253 deletions. This is not close to the authorized cap; the
   added `skinny/crates/bbnf-bench/src/lock14_baseline.rs` alone is 611 inserted
   LOC, above the entire W0 cap, and the original Lock 14 research estimated
   175-275 total source/test LOC for that slice
   (`restart/skinny/tranches/sk-v8/research/wave-0-lock14-baseline-research.md:70`,
   `restart/skinny/tranches/sk-v8/research/wave-0-lock14-baseline-research.md:77`).

   Repro:

   ```sh
   git diff --numstat 0bd16f6d..HEAD -- \
     skinny/RESULTS.md \
     skinny/crates/bbnf-bench/src/bin/gate.rs \
     skinny/crates/bbnf-bench/src/gate.rs \
     skinny/crates/bbnf-bench/src/report.rs \
     skinny/crates/bbnf-bench/src/lib.rs \
     skinny/crates/bbnf-bench/src/lock14_baseline.rs \
     skinny/xtask/src/main.rs
   ```

   This is a CH4 material rejection even though the latest V5 fold alone is
   within the 350-line envelope if counted in isolation. W0 is a wave budget, not
   a per-hardening-commit budget.

2. BLOCKER: the stated revert protocol exists, but the implemented slice is too
   large for the protocol to be realistic under the W0 cost cap.

   SPEC says to revert report/gate/schema/RESULTS changes as one slice and
   restore the opening RESULTS schema on rejection
   (`restart/skinny/tranches/sk-v8/SPEC.md:343`,
   `restart/skinny/tranches/sk-v8/SPEC.md:344`). DISPATCH says the 90-minute cap
   includes source edits, generation, verification, RESULTS/REDRESS updates, and
   rollback (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:47`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:48`). A 3532-insertion,
   multi-commit W0 gate/report rewrite with a generated `RESULTS.md` manifest is
   not a credible one-slice rollback inside that cap. This is not a source
   correctness failure; it is a CH4 cost/revert-practicality failure.

## Non-Blocking Findings

1. The V5 row-manifest fingerprint blocker is closed.

   Current `criterion_fingerprint` only accepts Criterion inputs that map through
   `w0_workload_for_bench` and then through an exact `sk_v8_open_baseline` row id
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:733`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:736`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:737`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:745`). The focused test now covers
   both an unvalidated corpus and a valid fixture with an unadmitted real-typed
   row (`skinny/crates/bbnf-bench/src/bin/gate.rs:1770`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1788`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1794`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1798`). My copied-root repro with a
   fake `json_canada/sonic_rs_real_typed_struct/new/estimates.json` also passed
   `gate-json --check-results`, so the prior CH4 stale-only failure no longer
   reproduces.

2. Same-wave consumer is realistic for W0 telemetry.

   The rendered report has 38 manifest rows and all are `gate_only`
   (`skinny/RESULTS.md:44`, `skinny/RESULTS.md:46`, `skinny/RESULTS.md:48`).
   `Report::validate_sk_v8_w0()` checks required text fields, row identity,
   outcome support, `SK-V8-open` baseline status, sample cost/count, profile/hot
   leaf binding, CostFacts placeholder, `same_wave_consumer_class=gate_only`,
   comparator evidence, and admission boundary
   (`skinny/crates/bbnf-bench/src/report.rs:275`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:369`,
   `skinny/crates/bbnf-bench/src/report.rs:370`). The gate calls
   `validate_schema_v3()` and `validate_sk_v8_w0()` before rendering comparison
   or writing (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:329`).

   This consumer is telemetry-only. It must not be reused as a W3/W4 production
   behavior consumer.

3. I found no production behavior LOC in frozen parser/scanner/SIMD/codegen
   surfaces.

   The freeze diff over grammar input, runtime JSON/tape, SIMD, codegen,
   generated/product helpers, Track 2, parity, scan, and materialization exited
   0. The changed source lives in `bbnf-bench` report/gate/Lock 14 code and
   `skinny/xtask/src/main.rs`, consistent with W0 owner paths
   (`restart/skinny/tranches/sk-v8/SPEC.md:290`). This does not offset the LOC
   budget blocker.

4. Gate runtime is not the blocker.

   The warmed W0 gate replay completed in 6.20s, and the copied-root row-manifest
   negative completed in 17.47s including cold rebuild. That is practical for a
   check gate. The impractical cost is the implementation and rollback size, not
   the current gate execution time.

## Residual Risks If Accepted Anyway

- Accepting W0 without re-authorizing the larger budget would make the
  <=350-LOC cap non-operative for later SK-V8 waves.
- The current `gate_only` consumer is correct for W0 telemetry, but accepting it
  without a clear warning may invite W3/W4 to treat telemetry rows as production
  behavior consumers.
- Rollback would depend on reverting a large multi-commit report/gate stack
  rather than the small one-slice revert protocol promised by SPEC.

## Disposition

Reject W0 for CH4. Do not count V6 as a qualifying ACCEPT cycle under
`restart/prompts/ORCHESTRATOR.md:120`. The minimum fold is not another
fingerprint tweak; it is a cost/scope redress decision:

- either explicitly re-authorize the larger W0 report/gate/Lock 14 scope with a
  revised budget and rollback plan, or
- split the current W0 implementation into a smaller admitted telemetry gate and
  route the excess Lock 14/report hardening into a separately budgeted wave.

Until that happens, W1-W6 remain blocked by the W0 rejection and W0 should not be
paper-closed on the now-fixed V5 fingerprint blocker alone.

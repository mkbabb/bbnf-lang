# SK-V12 W0 PIN Redress - Telemetry Revalidation Admit

Date: 2026-05-20.
Wave: W0 - Pin Telemetry And Gate Revalidation.
Disposition: ADMIT.
Gate: `G-W0-PIN-TELEMETRY` PASS.

## Summary

W0 admits the SK-V12 user-pin telemetry revalidation without changing parser
behavior, generated runtime output, benchmark bodies, gate semantics, report
schema, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.

Redress preflight proved that `skinny/RESULTS.md` is exact generated output:
adding W0 prose to the report makes `gate-json --check-results` fail stale.
The retained `SK-V9-open` heading and `sk-v9-open:criterion-fnv64-*` run ids
therefore remain frozen generated seed evidence. SK-V12 pin context is recorded
in this wave artifact instead of being hand-edited into the generated report.

## Evidence

- Worktree preflight was clean before redress.
- No cargo, rustc, xctrace, or samply process was running for this slice.
- The gate/report executable surface has no post-W0-lock drift:
  `git diff --stat f788eb97..HEAD -- skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/report.rs skinny/xtask/src/main.rs skinny/RESULTS.md`
  returned empty output.
- Pin profile artifacts exist:
  `/tmp/skv12-pin-p1/pmu/done.txt`,
  `/tmp/skv12-pin-p1/samply/done.txt`, and
  `/tmp/skv12-pin-p1/xctrace/done.txt`.
- PMU, samply, and xctrace capture status tables contain no non-PASS row:
  - `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
  - `/tmp/skv12-pin-p1/samply/capture_status.tsv`
  - `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`
- `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results`
  passed.
- `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`
  passed.

## Routed Remainder

W0 admits no CSS row, no JSON row movement, no SIMD primitive, no union
substrate route, and no ASM-gen route. CSS L4 generation, lightningcss
comparison, independent oracle proof, Lock 14 cleanup, and Lock 16 SIMD/ASM
admissions remain W1a/W1b/W2+ work under the user pin.

W1a may dispatch next. It must resolve the GrammarConfig legality surface
before generated CSS L4 emission is legal.

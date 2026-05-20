# SK-V12 W0 PIN Research Consolidated

Date: 2026-05-20.
Wave: W0 - Pin Telemetry And Gate Revalidation.
Phase: Research.

## Verdict

RESEARCH COMPLETE. W0 should proceed to plan with one required redress target:
reconcile the stale SK-V9-open manifest/status language in `skinny/RESULTS.md`
to the SK-V12 pin revalidation surface while preserving all row measurements
and outcomes.

## Lens Results

| Lens | Scope | Verdict | Load-bearing finding |
|---|---|---|---|
| A1 | Entry gate | PASS | W0 is dispatchable first after S-P3 convergence. |
| A2 | Profile artifacts | PASS | `/tmp/skv12-pin-p1` has complete PMU, samply, and xctrace evidence for W0. |
| A3 | Results surface | REVISE | JSON rows/floors hold, but `RESULTS.md` still advertises SK-V9-open manifest/run ids. |
| A4 | REDRESS and lock history | PASS | REDRESS stops at 120; no post-`f788eb97` gate/report drift. |
| A5 | Behavior drift | PASS | Scoped source and `RESULTS.md` have not moved since `f788eb97`. |
| A6 | Telemetry schema | PASS | W0 companion gate is valid for W0 only; W1b-2 must own CSS SOTA schema. |

## Plan Inputs

- Owner paths stay inside SPEC Section 3.
- No behavior source, generated runtime, benchmark body, parser, scanner,
  SIMD/ASM, or codegen behavior edit is authorized.
- `skinny/RESULTS.md` may be edited only to record unchanged-state
  reconciliation.
- `skinny/REDRESS.md` is edited only if W0 fails or records a measured
  disposition.
- W0 does not generate, benchmark, or admit CSS L4.
- W1b-2 remains the first wave that may satisfy the CSS L4 lightningcss
  admission gate.

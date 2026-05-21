# SK-V12 W1b-2b Challenge - CH2 Generality / Lock14

Lens: CH2 generality / Lock14.
Verdict: ACCEPT.

## Scope

This review covers the W1b-2b Section 7.2 plan, PLAN-AUDIT, and A4 JSON
guard/Lock14 research. It does not authorize source edits, new outcome
classes, or fallback topology changes.

## Findings

1. ACCEPT - Lock14 is process-owned, not report-owned.

   The plan may carry `lock14_status == pass:lock14_baseline::validate`, but
   A4 correctly binds that status to the same `gate-json` process running
   `lock14_baseline::validate(&workspace)` before companion report branching.
   This prevents a report JSON string from becoming Lock14 authority. The
   companion gate must fail if the baseline validator fails or if the report
   claims Lock14 without the process check.

2. ACCEPT - W1b-2b does not leak a generic JSON policy.

   The plan keeps the CSS SOTA report as a separate
   `sk-v12-css-l4-sota-v1` companion schema and explicitly says not to widen
   `sk-v12-nonjson-generated-v1`. JSON guard state remains a gate constraint:
   `not_refreshed:no_behavior_drift` is valid only when JSON-producing behavior
   did not move and `skinny/RESULTS.md` is unchanged; otherwise the gate must
   consume a populated JSON Criterion root and require a refreshed guards-pass
   state. A CSS-only Criterion root is rejected for JSON guard proof.

3. ACCEPT - Generality remains CSS-row bounded.

   The only admitted row is
   `css_l4/declaration_values/direct_to_struct/main`, with
   `input_bytes == 187`, the fixed W1b CSS fixture checksum, strict
   three-way equality, independent cssparser oracle evidence, and
   lightningcss comparator telemetry. Criterion consumption is limited to the
   three W1b-2a `new/` CSS lanes and derives Mbps, threshold, and margin rather
   than trusting report-provided values. This is a measured CSS row, not a
   broad non-JSON or substrate policy.

4. ACCEPT - RESULTS movement is fail-closed.

   Section 7.2 and A4 agree that PASS-MEASURED-BASELINE records REDRESS
   evidence and does not move `skinny/RESULTS.md`. The plan permits RESULTS
   movement only for a real CSS PASS-ADMIT-CANDIDATE row or an accepted
   measured JSON guard demotion. No-write companion mode rejects write/probe
   flags, so the Lock14/JSON guard path cannot silently mutate published
   results.

5. ACCEPT - No public substrate expansion is authorized.

   W1b-2b is report/gate/test scope only. The reviewed plan does not add or
   require any directive, BIR variant, `BackendShape` variant, public substrate
   API, parser-owned sidecar, SIMD/ASM claim, or public output-plane expansion.
   Lock16 is explicitly scalar-row context: `n/a:no_simd_or_asm_claim` is
   acceptable only when no SIMD/ASM admission is claimed and scalar/parity
   evidence is present for the equality path.

## Required Invariants For Implementation

- Run Lock14 baseline validation in the gate process before accepting the
  companion report.
- Reject CSS-only Criterion roots for JSON guard proof.
- Recompute threshold and margin from Criterion telemetry; do not trust JSON
  report math as authority.
- Keep W1b-2b to the single CSS L4 declaration-values row.
- Reject any directive, BIR, `BackendShape`, public substrate API, sidecar
  substrate, SIMD/ASM admission, or broader public substrate expansion.

## CH2 Verdict

ACCEPT. The W1b-2b plan is general enough for the CSS L4 SOTA admission gate
while staying bounded by Lock14, JSON guard, CSS-row, no-write, and
no-public-substrate constraints.

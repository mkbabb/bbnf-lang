# SK-V13 S-P1 V1 Hardening Consolidated

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-21.
Scope: consolidated CH1-CH6 challenge verdict for the SK-V13 S-P1 V1 profile cohort.
Output: this file.

## Verdict

`G-S-P1-V1-CONVERGED`: FAIL.

V1 is a useful profile inventory, but it does not meet the S-P1 convergence
bar. The cycle has two REJECT lenses, three REVISE lenses, and zero accepted
challenge cycles. It must fold to V2 before S-P1 can advance to S-P2.

| Lens | Disposition | Load-bearing reason |
|---|---|---|
| CH1 correctness | REJECT | P1-C has 0/17 mode-III coverage; direct samply artifacts are panic-path captures; several hot-leaf claims are unresolved or inconsistent. |
| CH2 generality | REVISE | Hot leaves remain JSON envelope or generated-typed paths, with no CSS/direct primitive attribution yet. |
| CH3 regression/REDRESS | REVISE | S-P2 carry-forward must cite REDRESS 96/97/98, 119/120, masking-probe history, and SIMD/orphan discipline explicitly. |
| CH4 cost/reproducibility | REJECT | Build provenance, direct samply, branch/L1/LLC counters, mode III, and parse comparator reproducibility are incomplete. |
| CH5 hidden coupling | REVISE | P1-D over-interprets Track1/Track2 c/B inversions as shared substrate cost; CSS needs plane fencing. |
| CH6 anti-paper-close | REJECT | Direct, mode III, structural-scan-only, and CSS hot-leaf surfaces are absent or invalid; parse samply is save-only/provisional. |

Acceptance rate: 0/6 = 0%. Consecutive accepted cycles: 0.

## Fold Actions For V2

1. Fix the direct samply workload path handling and recapture 17/17
   `direct_to_struct` Track 1/Track 2 profiles with non-panic workload logs.
2. Capture the P1-C mode-III matrix directly: 17/17 `host_call_eager_decode`,
   `alternate_scalar_plan`, `cold_first_parse`, and structural-scan-only
   profiles, plus PMU rows or explicit unsupported routing for the remaining
   probe names.
3. Capture CSS L4 hot-leaf artifacts or keep CSS fenced as
   `throughput_measured_only`; do not count CSS as JSON S-P1 convergence.
4. Export branch/L1/LLC counter fields from xctrace or explicitly mark the host
   export as unavailable; never infer zero misses.
5. Reconcile P1-A and P1-E parse hot-leaf extraction so the same raw profile
   yields one canonical row attribution, including unresolved-sample
   percentages.
6. Rephrase Track1/Track2 c/B inversions as observations only; do not infer
   shared substrate cost without measured file:line evidence.
7. Preserve REDRESS guardrails in every V2 anomaly: union-substrate routes cite
   96/97/98, direct-row reopens cite 119/120, and SIMD/orphan routes cite the
   USER PIN D4/D5 material-differential requirements.
8. Label `.json.syms.json` files as offline symbol-resolution metadata only,
   never parser events, cursor state, or substrate evidence.
9. Keep P1-F classifications as `profile_signal_not_gate_admission` until the
   missing profile evidence and gate-json provenance exist.

## Cycle Disposition

S-P1 V1 returns to profile fold. The V2 cohort should reuse the valid PMU,
xctrace, parse samply, typed subset, and CSS throughput evidence where
appropriate, but the blockers above must be addressed or explicitly preserved
as unresolved defects. V1 cannot be used as the empirical floor for S-P2
primitive scoping.

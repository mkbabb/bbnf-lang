# SK-V9 S-P1 V5 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V5 (post-fold of V4 CHALLENGE residuals).
Inputs: `restart/skinny/tranches/sk-v9/research/p1/hardening/V5/CH{1..6}.md`.

## Verdict — Five lenses fully converged; CH4 requires V6 confirmation

| Lens | V3 | V4 | V5 | 2-consecutive ≥95%? |
|---|---:|---:|---:|---|
| CH1 CORRECTNESS | 67% | 96.2% | 100% | **✓ converged (V4+V5)** |
| CH2 GENERALITY | 33% | 97.2% | 97.4% | **✓ converged (V4+V5)** |
| CH3 REGRESSION | 92% | 100% | 100% | **✓ converged (V4+V5)** |
| CH4 COST | 14% | 93.3% | 100% | **✗ 1 of 2** (V4 below bar) |
| CH5 HIDDEN COUPLING | 96% | 100% | 100% | **✓ converged (V4+V5)** |
| CH6 ANTI-PAPER-CLOSE | 89% | 97.0% | ACCEPT | **✓ converged (V4+V5)** |

Per `ORCHESTRATOR.md` §3Z, S-P1 converges when ALL six lenses return
≥95% ACCEPT for two consecutive cycles. Five lenses cleared at V4
and re-verified at V5. CH4 was the lone outlier at V4 (93.3%, 1.7pp
below the bar); V5 surgical fold closed all five named gaps and CH4 V5
returned 100%. CH4 therefore has one qualifying cycle (V5) and requires
one more for full pass-level convergence.

## V5 fold success

All six V5 surgical edits FOLDED:
1. V3-A §3 line 237 V2 "unambiguous" → "V2 baseline (superseded; see §4 / B §3.4)" (CH1-A4-9 ✓).
2. V3-C §5.3 "largest single cycle sink" → "among the largest" with arithmetic citation (CH1-C4-5 ✓).
3. V3-D §0 footer — 8 V3 publication errors enumerated, mirroring C §6 pattern (CH6-D ✓).
4. V3-B §0 footer — re-capture wall cost: CPU Counters ~12 min, Time Profiler ~22 min, lto=fat ~3-5 min, aggregate ~37-39 min (CH4-V05/V19/V20 ✓).
5. V3-B §0 footer — aggregate.py reproducibility-by-instruction at `/tmp/skv9-xctrace-v3/aggregate.py` (CH4-V23 ✓).
6. V3-F §4 — edit-dispatch hard cap ≤30 min total batch, sequenced SPEC→HANDOFF→DISPATCH-PROMPT, single git revert protocol (CH4-V21 ✓).

V5 introduced no new defects across any lens. The regression script
reproduces bit-for-bit; the PMU arithmetic checks; the strictness-plane
assertion holds; Lock-1 cardinality discipline preserved; Lock-14
grammar-neutrality preserved.

## V6 protocol — CH4 confirmation cycle

V5 substantive is the new state. V6 = unchanged substantive + a fresh
CHALLENGE on CH4 (the only lens needing the second consecutive
qualifying cycle). Per §3Z step 5, V6 substantive ≡ V5 since the V4
CH4 dispositions were fully folded at V5 and CH1/CH2/CH3/CH5/CH6 are
already converged.

V6 CH4 expected outcome: ACCEPT ≥95% (mechanical re-verification on
unchanged substantive). On ACCEPT, S-P1 reaches full pass-level
convergence and S-P2 dispatches per `restart/prompts/skinny/PASS-2-RESEARCH.md`.

## Convergence trajectory

V3 (failed): 4 of 6 lenses below bar. Diagnosis: paper-close drift in
C (sequenced), regression provenance gap in D, classifier prose
Lock-14 leaks across B/C/D/E, S-P1 wave-prescription overreach.

V4 (close): 5 of 6 lenses clear. Folding the V3 dispositions surfaced
the ~8× OLS coefficient correction in V3-D — the most load-bearing
honesty correction of the cycle. CH4 lone outlier.

V5 (5 of 6 fully converged + CH4 first qualifying): six surgical edits;
no new measurement, no agent dispatch heavier than narrative folds.

V6 (forecast: full convergence): CH4 re-verification on unchanged
substantive. ~20 min wall-clock for one parallel lens dispatch.

## After V6

If V6 CH4 ≥95%: S-P1 fully converges. The orchestrator advances to
S-P2 Research with the substrate-grounded SK-V9 baseline. S-P2's six
sub-agents take the V5 evidence (per-symbol Time Profiler exports,
per-row PMU table, structural correlation, primitive-class vocabulary,
cleanup manifest, REDRESS reconciliation, Lock-1 binding) and produce
the candidate-intervention shortlist.

If V6 CH4 surfaces regressions (very unlikely on unchanged substantive):
V7 fold per the same surgical protocol; §3Z hard ceiling V ≤ 5 means
V7+ requires user escalation.

# SK-V9 S-P2 V3 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V3 (post-fold of V2 CHALLENGE residuals).
Inputs: `restart/skinny/tranches/sk-v9/research/p2/hardening/V3/CH{1..6}.md`.

## Verdict — Four lenses converged; CH3 + CH6 need a V4 confirmation

| Lens | V1 | V2 | V3 | 2-consecutive ≥95%? |
|---|---:|---:|---:|---|
| CH1 CORRECTNESS | 96.7% | 98.2% | 97.6% | **✓ converged (V1+V2, re-verified V3)** |
| CH2 GENERALITY | 80.6% | 100% | 100% | **✓ converged (V2+V3)** |
| CH3 REGRESSION | 67.4% | 93.0% | 100% | ✗ 1 of 2 (V2 below bar; V3 first qualifying) |
| CH4 COST | 22.7% | 100% | 100% | **✓ converged (V2+V3)** |
| CH5 HIDDEN COUPLING | ACCEPT | ACCEPT | ACCEPT | **✓ converged (V1+V2+V3)** |
| CH6 ANTI-PAPER-CLOSE | 68% | 90.6% | 100% | ✗ 1 of 2 (V2 below bar; V3 first qualifying) |

Four of six lenses fully converged. CH3 + CH6 each cleared ≥95% at V3
(both 100%) but V2 was below the bar — they need one more consecutive
qualifying cycle (V4). The V3 fold closed every CH3 + CH6 residual; V4
is a re-CHALLENGE confirmation on unchanged substantive.

## V3 fold success

All 8 V3 surgical edits FOLDED cleanly:
- P2-D §5.3.1 EOR3 six-row no-regression gate (CH3) — verbatim mirror
  of the §4.4 CSSC CTZ gate; broadens the protected WIN-block.
- P2-F §5.2 inline REDRESS-33 citation (CH3) — demotes the sonic-rs
  pattern from implied-admission to explicit-lesson.
- P2-D EOR3 latency cite (CH6) — ARM DDI 0487 FEAT_SHA3, honestly
  flags M5-Max P-core specifics as Apple-unpublished, narrows the
  claim to the ordering EOR3 < PMULL.
- P2-F ContainerNext (`generated.rs:341`) + CollapsedStage
  (`ARCHITECTURE.md §7.3`) cites (CH6) — both resolve verbatim.
- P2-D §6.3 wording (CH6) — per-primitive checkasm carved out as
  same-wave precondition; only host-instrumentation infrastructure
  deferred.
- P2-D REDRESS 28/33 line ranges (CH1) — `:324-337` / `:394-418`,
  verified live (the V1/V2 ranges `1241-1278`/`1314-1343` were stale).
- P2-F P1-V3-B §1.5 path anchor (CH2) — cohort uniform.
- P2-D §0 cascade-sequencing footer (CH3/CH5) — the four
  "block on P2-A OR fail CH5" slices forbid a wave split.

## V4 fold requirement — one trivial 1-token correctness fix

CH1 V3 surfaced one new LOW REVISE (non-V3-origin, pre-existing):
P2-D §8 cites `match_tiny_plain_string_neon` at
`match_tiny_plain_string.rs:79`; the function is at line **81**
(line 79 is the `#[cfg]` attribute). One-token fix. Apply it, then V4
re-CHALLENGE CH1 + CH3 + CH6.

## V4 path

1. Apply the 1-token line-number fix to P2-D §8 (`:79` → `:81`).
2. Commit `docs(sk-v9-p2-v4): fold V3 CHALLENGE — 1-token correctness fix`.
3. Re-dispatch CHALLENGE V4 on CH1 (re-verify the fix), CH3 + CH6
   (second-consecutive confirmation). CH2/CH4/CH5 are converged and
   the V4 fold does not touch their surfaces — no re-dispatch needed.
4. Expected: all three return ≥95%. S-P2 fully converges; S-P3
   Synthesis-Plan dispatches.

## Convergence forecast

S-P2 V1→V2→V3 mirrors S-P1's trajectory: a failed V1, a substantive
V2 fold, surgical V3 + V4 confirmation. V4 is the smallest cycle —
one token. S-P2 converges at V4. The substantive S-P2 findings (union
event-model, retained-grammar proof, Apache/CITM admission, aarch64
ASM opportunities, unicode-escape codec, SOTA teardown) are then the
input to S-P3 Synthesis-Plan, which authors the SK-V9 SPEC wave plan.

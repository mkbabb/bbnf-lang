# SK-V8 S-P3 Hardening V2 Consolidated

Date: 2026-05-18.
Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Inputs: `restart/skinny/tranches/sk-v8/research/p3/hardening/V2/CH1.md`
through `CH6.md`.

## Verdict

REVISE.

V2 is not a qualifying S-P3 convergence cycle. CH2, CH3, CH4, CH5, and CH6
ACCEPT at 96% confidence. CH1 returns REVISE at 90% confidence on traceability:
the V2 fold fixed broken/stale line numbers by reducing many P3 citations to
bare local file paths, but CH1 requires either current file:line citations or
stable section references for material correctness claims.

## Lens Results

| Lens | Verdict | Confidence | Blocking disposition |
|---|---|---:|---|
| CH1 correctness | REVISE | 90% | Citation hygiene remains too coarse: P3-A through P3-F need line-resolved citations or stable section references, not repeated bare paths. |
| CH2 generality | ACCEPT | 96% | Lock 14, non-JSON proof gates, and no-new-surface constraints remain intact. |
| CH3 regression/pre-block | ACCEPT | 96% | REDRESS/pre-block coverage, strict-vs-strict discipline, typed/direct guard rows, and row-gated behavior movement are preserved. |
| CH4 cost/feasibility | ACCEPT | 96% | Per-wave source/edit LOC budgets and W3 pre-redress LOC/time split gate are fully folded into SPEC, DISPATCH, and HANDOFF. |
| CH5 hidden coupling | ACCEPT | 96% | No sidecar substrate, parser-owned facts/cursors, Track 1/Track 2 coupling, telemetry-only W3 consumer, or API/substrate drift admitted. |
| CH6 anti-paper-close | ACCEPT | 96% | G-Alpha, W0, W3, SK-V8 close, and SK-V9 planning remain unclosed until measured gates and required signoffs occur. |

Acceptance count: 5/6 verdict-level ACCEPT, 5/6 qualifying confidence. The
cycle fails because one lens is REVISE.

## Required V3 Fold

1. Replace material bare-path citations in P3-A through P3-F with either
   current file:line citations or stable section references naming the exact
   target section.
2. Keep the V2 W2 candidate table, W2 recomputation rule, W2 seed-table
   dispatch bound, W0 naming-pattern fix, dispatch lock, and LOC/time gates
   unchanged.
3. Re-run local reference validation so P3-A through P3-F, SPEC, DISPATCH,
   HANDOFF, V2 hardening files, and the V3 fold contain no broken local file
   references or unresolved future-artifact globs.

## Non-Blocking Notes

- The V2 challenge artifacts themselves had one CH5 citation-path spelling
  drift introduced during review; the aggregator normalized those local paths
  to existing files before consolidation. This did not change the CH5 verdict.
- P3-A through P3-E still carry V1 cycle labels. That is non-blocking for CH3,
  but the V3 traceability fold may note that the live folded authority is SPEC,
  DISPATCH, HANDOFF, P3-F V2, and the V3 fold.
- No implementation wave is dispatchable from V2. G-Alpha remains open, and
  `G-Alpha closed` would authorize W0 only.

## Disposition

Fold to V3 and re-challenge. V3 can qualify only if all six lenses ACCEPT with
confidence >=95%.

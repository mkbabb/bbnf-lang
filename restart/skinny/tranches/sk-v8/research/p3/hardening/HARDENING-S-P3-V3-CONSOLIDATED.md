# SK-V8 S-P3 Hardening V3 Consolidated

Date: 2026-05-18.
Pass: S-P3 Synthesis-Plan.
Cycle: V3.
Inputs: `restart/skinny/tranches/sk-v8/research/p3/hardening/V3/CH1.md`
through `CH6.md`.

## Verdict

REVISE.

V3 is not a qualifying S-P3 convergence cycle. CH2, CH3, CH4, CH5, and CH6
ACCEPT at >=96% confidence. CH1 returns REVISE at 88% confidence because the V3
citation labels are still too broad for material correctness claims. They
resolve paths and avoid stale line numbers, but labels such as "SPEC Sections
0.1-0.5, 2, and 3-11" do not name the exact target section supporting a claim.

## Lens Results

| Lens | Verdict | Confidence | Blocking disposition |
|---|---|---:|---|
| CH1 correctness | REVISE | 88% | Replace broad label bundles with exact named sections or current file:line references on material P3 claims. |
| CH2 generality | ACCEPT | 96% | V3 citation changes did not weaken Lock 14, non-JSON proof, no-new-surface constraints, W3 one-Tape/Tier-A boundaries, or grammar-neutral gates. |
| CH3 regression/pre-block | ACCEPT | 96% | V3 did not reopen REDRESS/pre-blocked routes, loosen strictness, lose guard rows, or admit behavior/status movement without gates. |
| CH4 cost/feasibility | ACCEPT | 97% | V3 preserved LOC budgets, W3 fit/split gate, 90-minute cap, generated-output diff audit, and revert accounting. |
| CH5 hidden coupling | ACCEPT | 96% | V3 did not hide sidecar substrate, parser-owned projection/cursor/facts, Track 1/Track 2 coupling, telemetry-only W3 consumer, or API/substrate drift. |
| CH6 anti-paper-close | ACCEPT | 96% | V3 does not paper-close G-Alpha, W0, W3, SK-V8 close, or SK-V9 planning. |

Acceptance count: 5/6 verdict-level ACCEPT, 5/6 qualifying confidence. The
cycle fails because one lens is REVISE.

## Required V4 Fold

1. Replace `SPEC Sections 0.1-0.5, 2, and 3-11` usages on material claims with
   exact targets such as `SPEC Section 0.2 Comparator Classes`, `SPEC Section
   0.4 Required Telemetry`, `SPEC Section 0.5 Opening Row Goalset`, `SPEC
   Section 3 W0`, `SPEC Section 4 W1`, `SPEC Section 5 W2`, `SPEC Section 6
   W3`, `SPEC Section 10 Pre-Blocked Routes`, or `SPEC Section 11 G-Alpha`.
2. Replace `HANDOFF Sections 2, 3a, and 4-10` with exact named handoff
   sections, or current file:line references where live dispatch state is the
   claim.
3. For RESULTS/REDRESS evidence, name the specific row, REDRESS entry, or
   current line span when the claim depends on measured state, blocked route,
   or threshold.
4. Preserve all substantive V2/V3 gates: W2 seed table and bound, W0 naming
   pattern, G-Alpha lock, LOC/time gates, strict-vs-strict, Lock 14, and
   no-new-surface constraints.

## Disposition

Fold to V4 and re-challenge. V4 can qualify only if all six lenses ACCEPT with
confidence >=95%.

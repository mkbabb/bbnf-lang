# T-P1 V1 Hardening Consolidated

Pass: T-P1 Excavation. Cycle: V1.
Date: 2026-05-21.
Scope: CH1-CH6 consolidation for the first SK-V13 totality evidence inventory.
Output: this file.

## Verdict

Disposition: REVISE.

V1 is a useful excavation floor, not a convergence cycle. All six lenses
returned REVISE, with no full-artifact REJECT. The inventories are therefore
kept as evidence, but V2 must fold the challenge set before T-P1 can advance.

Accept rate: 0/6 cycle-level ACCEPT, 6/6 cycle-level REVISE, 0/6 REJECT.

## Lens Dispositions

| Lens | Disposition | Load-bearing finding | Required fold |
|---|---|---|---|
| CH1 correctness | REVISE | One broken 1B path citation; 1A under-states live `LayoutFacts.backend_shape`; several negative-search claims lack captured output; 1F Lock 13 directory verdict is overbroad. | Fix the 1B path; distinguish live `passes::LayoutFacts.backend_shape` from incomplete cost/priority logic; convert uncaptured command-output claims to artifacts or UNKNOWNs; narrow Lock 13 fanout claims to mixed-concern evidence. |
| CH2 generality | REVISE | Lock 14 audit catches headline leaks but misses parts of the generic-crate census and grammar-shape leaks without grammar names; GrammarConfig is overstated as complete; Sheets and BBNF-self implications are thin. | Add `grammar`, root tests/proof fixtures, provider/templates, and pass role-mining to the census; split grammar-name leaks from grammar-shape leaks; downgrade GrammarConfig to partial row-level repair; add Sheets and BBNF-self rows. |
| CH3 regression | REVISE | The inventories preserve most REDRESS history but understate the SK-V13 pre-block/unblocked split; 1A misclassifies admitted SinkOnly direct evidence; 1A/1C risk demoting the admitted CSS fact-stream row. | Add current SK-V13 pre-block table; separate admitted SinkOnly from unaudited V1 event scheduling; preserve the CSS L4 row as admitted evidence while naming the V1 substrate/telemetry category gap. |
| CH4 cost | REVISE | LOC/risk estimates are often plausible, but hard-cap fields are absent globally; wave alignment is inconsistent outside 1E; SIMD/ASM and substrate-producing work lack same-wave consumer routing. | Add `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis` to divergence and amendment rows. |
| CH5 hidden coupling | REVISE | V1 catches the main Lock 1 danger, but under-classifies renamed scanner/sidecar planes: live `StructuralIndex`, CSS source-sidecar comparator evidence, Track 2 shared substrate helpers, and runtime proof aliases. | Add explicit rows for transient structural scanner plane, CSS source-sidecar comparator plane, Track 2 shared-substrate caveat, and proof-witness root coupling. |
| CH6 anti-paper-close | REVISE | UNKNOWN rows mostly have verify actions, but several rows use closure wording (`honoured`, `implemented pre-block`, `mostly implemented`, `proved`, `partial`) while also admitting unverified scope. | Weaken closure wording or add exact live evidence in-row; split VM replay UNKNOWN; change Lock 16 to partial/UNKNOWN traceability until allowlist trace is proven. |

## Non-Blocking Accepts To Preserve

- 1C runtime census and generated/hand-written split are structurally useful.
- 1D maps the major RESULTS and REDRESS evidence accurately enough to retain.
- 1E is the best cost-shape template because its amendment candidates include
  evidence, LOC/risk, and wave hints.
- 1F correctly records stale totality surfaces and prior skinny pre-blocks,
  but V2 must separate historical pre-block acceptance from live implementation
  closure.

## Required V2 Fold

1. Correct citations and command-evidence hygiene:
   - Fix `codegen/src/lib.rs:95-100` to `skinny/crates/codegen/src/lib.rs:95-100`.
   - Capture or downgrade negative-search claims, `wc -l` claims, and child-count scans.
   - Narrow Lock 13 directory fanout to proven mixed-concern directories.

2. Reframe substrate and admission wording:
   - Mark `LayoutFacts.backend_shape` as live in `passes`, while preserving cost/model drift.
   - Treat JSON SinkOnly direct parsing and CSS L4 fact-stream rows as admitted row evidence with V1 substrate-classification gaps, not as regressions to undo.
   - Classify `StructuralIndex` and CSS comparator facts as transient/cited side planes, not retained substrate authority.

3. Strengthen Lock 14:
   - Add full generic-crate census coverage for `grammar`, root runtime tests/proofs, codegen providers/templates, and pass role-mining.
   - Separate grammar-name leaks from grammar-shape leaks.
   - Add explicit Sheets and BBNF-self implications.

4. Add implementation-planning metadata:
   - Every divergence and amendment candidate gets `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis`.
   - Lock 16/SIMD rows require same-wave production consumer routing or remain UNKNOWN/partial.

5. Add current SK-V13 regression framing:
   - Distinguish `unblocked with fresh evidence` from `accepted route`.
   - Include the current SK-V13 hard pre-blocks from the pin-era SYNTHESIS/HANDOFF.
   - Preserve prior REDRESS history without treating REDRESS-119/120 as current close authority.

## Cycle Result

T-P1 V1 does not converge. V2 must overwrite the 1A-1F inventories with the
folded evidence and then re-dispatch CH1-CH6. This hardening cycle is still
valuable: it established that the main evidence set is salvageable and that
the failures are precision, generality, cost, and closure-wording issues rather
than a wholesale bad excavation.

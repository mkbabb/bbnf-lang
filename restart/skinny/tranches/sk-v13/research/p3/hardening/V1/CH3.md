# SK-V13 S-P3 V1 CH3 Regression / REDRESS

Pass: S-P3 Synthesis-Plan.
Cycle: V1 CHALLENGE.
Date: 2026-05-21.
Lens: CH3 REGRESSION / REDRESS.
Output: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH3.md`.

## Verdict

REVISE.

P3-E itself passes the CH3 ledger test: it enumerates historical route state and
maps pre-blocked REDRESS entries to wave families. The regression defect is in
the fold into SPEC/DISPATCH. Both still say P3-B through P3-E were absent at V1
draft time, and SPEC Section 20 compresses the pre-block list through P3-F
rather than carrying P3-E's route-state ledger and wave-family matrix.

This is not a REJECT because the current SPEC does preserve the G-Omega pre-W0
block, no-silent-demotion posture, REDRESS 96/97/98 material-differential rule,
REDRESS 119/120 history-only rule, REDRESS 121-127 preservation, and global
older-route blocks. It is still a convergence-blocking REVISE because dispatch
packets need the per-wave P3-E matrix, not a global reminder.

## Evidence Table

| Surface | Evidence | CH3 reading | Disposition |
|---|---|---|---|
| CH3 standard | The S-P3 prompt asks whether P3-E enumerates every REDRESS route each wave must not reopen, whether any P3-B wave silently reopens a pre-blocked route, and whether SPEC carries the full pre-block list (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:122`-`:126`). ORCHESTRATOR CH3 requires no reopened `skinny/REDRESS.md` route, a correct pre-block list, and no silent admitted-row regression (`restart/prompts/ORCHESTRATOR.md:81`-`:85`). | This lens is about both the P3-E ledger and the SPEC/DISPATCH fold. | ACCEPT |
| P3-E route-state coverage | P3-E's route-state ledger covers older string/substrate/SIMD/direct routes, REDRESS 96/97/98, REDRESS 119/120, and REDRESS 121-127 with explicit statuses such as `BLOCKED-HISTORICAL`, `REOPEN-CONDITIONAL`, `GATE-FEED`, `HISTORY-LIFTED`, and `MIXED` (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:60`-`:87`). | P3-E enumerates the historical route families at sufficient granularity. | ACCEPT |
| P3-E per-wave matrix | P3-E maps route families to W0/G-Omega, CSS, CSS SIMD, decision-engine, union, JSON direct, parse-only, and close wave families (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:91`-`:101`), then states P3-F must place that matrix into SPEC's pre-blocked-routes section (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:185`-`:199`). | The required per-wave ledger exists. The gap is downstream folding. | ACCEPT |
| Stale SPEC/DISPATCH authority | SPEC says P3-B, P3-C, P3-D, and P3-E were absent at V1 drafting time (`restart/skinny/tranches/sk-v13/SPEC.md:5`-`:8`). DISPATCH repeats that if they exist by dispatch, the orchestrator must read and fold them later (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:26`-`:28`). P3-E now exists (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:1`). | The current implementation contract still misstates its CH3 input set. This is the main REVISE. | REVISE |
| SPEC global pre-block list | SPEC Section 20 says every wave inherits a ledger and lists global blocks for directives, substrate, generic policy, stale comparators, orphan primitives, Track 1/Track 2 coupling, REDRESS 28/33, 50-55, 60-72, 80, 82-84, 88-90, 92, 96-98, 107/108, 113-120, and REDRESS 121-127 preservation (`restart/skinny/tranches/sk-v13/SPEC.md:874`-`:888`). | Good global guard, but it points to P3-F and lacks P3-E's per-wave route-state statuses. Dispatch agents could miss wave-specific inherited blocks. | REVISE |
| REDRESS 96/97/98 union history | REDRESS 96 rejected class-column structural-index integration, REDRESS 97 rejected the streaming-cursor revision, and REDRESS 98 retired the SK-V9 union-substrate gate while requiring future Alpha/S-P3 material differentiation (`skinny/REDRESS.md:2795`-`:2850`, `:2850`-`:2906`, `:2908`-`:2950`). SPEC requires a fresh union material differential against REDRESS 96/97/98 and forbids class columns, retained structural index, parser-owned cursor/list, aux table, sidecar vector, second scan, and public `UnionTape` (`restart/skinny/tranches/sk-v13/SPEC.md:58`-`:60`, `:664`-`:687`). | SPEC does not silently reopen the exact union routes. It still needs P3-E's union pre-block slice in the W9 packet: 50, 51, 53, 88, 89, 92, 96, 97, 98, and 126. | ACCEPT |
| REDRESS 119/120 direct/close history | REDRESS 119 closed W8 as measured direct fixpoint with no behavior source intervention and no RESULTS movement, and REDRESS 120 closed SK-V11 as measured fixpoint, not as overall direct GO (`skinny/REDRESS.md:3495`-`:3527`, `:3529`-`:3553`). SPEC says no ordinary fixpoint closes SK-V13 and REDRESS-119/120 are history only (`restart/skinny/tranches/sk-v13/SPEC.md:73`-`:77`); W11 requires citing REDRESS-119/120 and naming a fresh differential per row (`restart/skinny/tranches/sk-v13/SPEC.md:727`-`:747`). | No silent direct-row demotion or fixpoint close is authorized. The W11 packet still needs P3-E's explicit direct replay blocks. | ACCEPT |
| REDRESS 121-127 preservation | REDRESS 121 supplies Lock 14 GrammarConfig legality, REDRESS 122 supplies escape-mask/checkasm prerequisite, REDRESS 123-125 supply CSS scaffold/comparator/admission evidence, REDRESS 126 records microbench-only production split plus zero-orphan demotion, and REDRESS 127 records the one-row SK-V12 CSS close and zero final orphan state (`skinny/REDRESS.md:3555`-`:3879`). P3-E restates these as gate feed, not shortcuts (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:103`-`:121`). SPEC Section 21 requires Omega to fold REDRESS 121-127 (`restart/skinny/tranches/sk-v13/SPEC.md:890`-`:901`). | The preservation rule exists. SPEC must also put these exact gate-feed roles into affected wave packets, especially CSS, SIMD, generic-policy, and close waves. | REVISE |
| Older SIMD/string/substrate rejects | P3-E lists tiny-string NEON, side tables/cursors, decoded string sinks, source hooks, mantissa widen, unicode quartet, PMULL/CTZ, scanner/tape isomorphism, and proof-only escape limits (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:62`-`:82`). SPEC Section 20 globally lists many of these (`restart/skinny/tranches/sk-v13/SPEC.md:885`-`:886`), while individual CSS, decision, policy, union, direct, SIMD, typed, and parse sections only carry selective pre-block notes (`restart/skinny/tranches/sk-v13/SPEC.md:422`-`:423`, `:483`-`:484`, `:543`-`:544`, `:576`-`:577`, `:610`-`:611`, `:646`-`:647`). | No obvious silent reopen, but the wave sections are not dispatch-grade against P3-E. They need exact REDRESS entry lists by wave. | REVISE |
| Admitted rows and no silent demotion | SPEC G7 forbids silent demotion (`restart/skinny/tranches/sk-v13/SPEC.md:69`-`:71`), rolling delta covers all 51 JSON rows plus every CSS feature and fails backward margin movement (`restart/skinny/tranches/sk-v13/SPEC.md:231`-`:245`), W1 maintains the admitted CSS declaration-values row and JSON guards (`restart/skinny/tranches/sk-v13/SPEC.md:412`-`:418`), and W15 checks no silent demotion at close (`restart/skinny/tranches/sk-v13/SPEC.md:864`-`:869`). `skinny/RESULTS.md` records the admitted CSS row and current JSON/CSS oracle posture (`skinny/RESULTS.md:94`, `:145`-`:149`). | No silent-demotion gap found under CH3. Preserve this wording during fold. | ACCEPT |
| G-Omega pre-W0 | SPEC dispatch lock blocks Wave 0 and later redress until G-Omega and S-P3 convergence/user pin (`restart/skinny/tranches/sk-v13/SPEC.md:27`-`:38`). W0 entry requires G-Omega closed (`restart/skinny/tranches/sk-v13/SPEC.md:361`-`:365`). Section 21 repeats G-Omega before W0 and limits pre-gate work to planning/research plus read-only RESULTS/REDRESS inspection (`restart/skinny/tranches/sk-v13/SPEC.md:890`-`:906`). DISPATCH repeats the same global block (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:30`-`:41`). | G-Omega pre-W0 is preserved. | ACCEPT |

## Exact Fold Actions

1. Update SPEC and DISPATCH authority/status text to stop claiming P3-B, P3-C,
   P3-D, and P3-E are absent. Add P3-B, P3-C, P3-D, and P3-E to the authority
   list for V2/folded S-P3, or explicitly mark SPEC/DISPATCH as superseded until
   the fold lands.

2. Replace SPEC Section 20's compressed P3-F reference with P3-E's route-state
   model. The folded section must preserve the statuses `BLOCKED-HISTORICAL`,
   `REOPEN-CONDITIONAL`, `GATE-FEED`, `HISTORY-LIFTED`, and `MIXED`, not just a
   flat REDRESS-number list.

3. Add a `Pre-blocked REDRESS entries` subsection to every SPEC wave packet and
   mirror it in DISPATCH wave packets:

   | SPEC wave family | Folded pre-block slice |
   |---|---|
   | Pre-W0/W0 | 75, 77, 78, 99-102, 111, 119-127 as gate feed; 119/120 cannot close; no source, RESULTS, or REDRESS work before G-Omega. |
   | W1-W4 and W10.N CSS | 112, 113, 123-127 as gate feed; 28/33, 50-55, 60-72, 82-84, 88/89, and 126 whenever CSS uses string, escape, or SIMD routes; 123-125/127 are not full CSS close. |
   | W5-W7 decision engine | 84, 87, 114, 115 plus Lock 14/CostFacts route families 85-87 and 121; no JSON-specific generic branches, support-only resolver extraction, or old cascade fallback admission. |
   | W8 policy/sink/view | 121 for Lock 14; 54/55/66-69 for sink/source-hook/string routes; 80/82/84 for number/unicode/control routes; no public `GrammarConfig`, generic `JsonSink` acceleration, or JSON policy in generic code. |
   | W9 union | 50, 51, 53, 88, 89, 92, 96, 97, 98, and 126; exact class-column, streaming cursor, class-lane-only, `StructuralIndex`, parser-owned cursor/list, aux table, sidecar vector, second scan, and `UnionTape` routes remain blocked. |
   | W11.N direct | 54, 55, 66-69, 73, 80, 82, 84, 106-108, 114-119; cite 119/120 as history but never as close authority. |
   | W12 SIMD/ASM | 88, 89, 90, 122, 126, and relevant 121-127 gate feed; checkasm/microbench-only admission rejects; final orphan count must remain zero. |
   | W13 typed product | 70-72 and 103-110; typed product precedent is allowed, but direct digest rows, hidden typed sinks, proof-only escape routes, and no-op production rows are not typed admission. |
   | W14.N parse-only | 28, 33, 50, 51, 53, 60-65, 72 overgeneralization, 82-84, 88, 89, 92, 96-98, and 102; parse rows are target-eligible but docs-only `S` to `A` movement is blocked. |
   | W15 close | 119, 120, 123-127 plus the full-SOTA addendum; no ordinary fixpoint, implementation-limited miss, one-CSS-row close, or REDRESS-history close. |

4. Reconcile wave naming before the next CH3 pass. P3-E's W8/W12 union/SIMD and
   W11/W14 JSON labels must be mapped to the current SPEC W9/W12/W11/W14 layout,
   or P3-B/P3-E must be revised to use the same wave names as SPEC. Do not leave
   two route ledgers with different wave identifiers.

5. Preserve the accepted guardrails during fold: G-Omega remains a hard pre-W0
   gate; `SK-V13-open` and rolling delta remain mandatory; admitted JSON/CSS rows
   cannot silently demote; REDRESS 96/97/98 and 119/120 can be cited only with
   fresh material differentials; REDRESS 121-127 remain gate feed, not shortcuts.

6. After the fold, rerun CH3 against the revised SPEC/DISPATCH and verify that no
   wave packet can dispatch with a generic "inherits Section 20" pre-block in
   place of its exact REDRESS route list.

## S-P3 Convergence Impact

Blocks S-P3 convergence: yes.

This is an unresolved REVISE under ORCHESTRATOR Section 3Z. Hardening without
folding is paper-hardening, and the pass cannot advance with an orphan unresolved
REVISE (`restart/prompts/ORCHESTRATOR.md:112`-`:121`). Implementation remains
blocked independently by G-Omega and S-P3 convergence, so this CH3 finding does
not authorize any source, RESULTS, or REDRESS edit.

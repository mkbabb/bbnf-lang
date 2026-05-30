---
lens: CH6 ANTI-PAPER-CLOSE
pass: T-P3-synthesis
cycle: V1
reviewer: CH6
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
subject: restart/audit/totality/sk-v17/p3/{3a,3b,3c,3d,3e,3f}.md + 3c-locks-v+1-diff.md
contract: restart/prompts/totality/PASS-3-SYNTHESIS.md §3 CH6 + §8.5 + ORCHESTRATOR §3W
accept: 6
revise: 3
reject: 0
verdict: PASS-WITH-REVISE
---

# CH6 ANTI-PAPER-CLOSE — T-P3 SK-V17 Synthesis (cycle V1)

## Lens charge

Per PASS-3 §3 CH6: no synthesis artefact claims a delta "validated" without the
T-P1/T-P2 evidence chain; no delta deferred to "a future cycle" without a named
receiver + blocker + receiving gate; 3C DEFER dispositions name the re-entry
trigger; 3C's disposition matrix is complete (no silent drop — CH1+CH6 REJECT
class per §8.1/§8.5); 3F's next-cycle directive specifies concrete, measurable
entry conditions. No engineered-defer.

§3 line 99 sets the V1 expectation: **≥30% REVISE; an all-ACCEPT wave is
paper-close.** This review returns 3 REVISE / 6 ACCEPT / 0 REJECT (33% REVISE),
satisfying the anti-paper-close floor while finding no defect that blocks the
load-bearing 3C gate object.

## What was checked, and what resolved

- **Disposition completeness (3C).** 14 candidates declared in frontmatter
  (`3c-locks-crystallisation.md:53`-`54`); 14 disposition rows in the matrix
  (`:124`-`137`); tally 9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0 DEFER =
  14 (`:142`-`147`). **Zero silent drops** — the §8.5 CH1+CH6 REJECT class does
  not trigger. Every LAC and every ORQ carries a disposition.
- **No bare "validated".** A full-corpus grep for `validated|confirmed|guaranteed`
  unaccompanied by an evidence token (`*.md:`/`*.rs:`/`LAC-`/`2F-`/`1E-`/`SPEC`/
  `gate`/`checkasm`) returns **zero hits** across all seven p3 artefacts. Every
  delta cites a T-P1 finding-id, a T-P2 LAC/dossier, or a V1 surface at path:line.
- **The 3 ORQs are crystallised, not engineered-defers.** Each of 2F-FOLD-U1/U2/U3
  carries the full Receiver/Blocker/Gate triple in the matrix
  (`3c-locks-crystallisation.md:135`-`137`): U1 → SK-V18 W2 / AoS→SoA collapse /
  W2 substrate-union gate; U2 → W2 OnceCell-classification pre-gate / retained
  parallel index re-opens REDRESS-53 / W2 §9-condition-1; U3 → future
  2E-source-backed wave / `admits_collapsed_stage` x86-binding mechanically
  refuses / 5-shape BackendShape gate. The matching 2F UNKNOWN verify_actions
  resolve verbatim (`2f-fold-gaps.md:561`-`565`).
- **Two MODIFYs are honest, not laundered ACCEPTs.** LAC-2F-FOLD-05 + LAC-1E-SKV17-04
  (`:128`,`:132`) record BOTH priced Lock-2 paths and bar `LayoutFacts`-alone
  closure — they explicitly do NOT choose the SK-V18 route inside the lock, a
  genuine "don't decide implementation in locks" MODIFY mirroring the prior-totality
  `LAC-1E-V1-04` treatment (`:155`-`156`). Not paper-close.
- **3F next-cycle directive is concrete + measurable.** The 8-step directive
  (`3f-migration-handoff.md:151`-`191`) names mechanical gates: `grep` proves
  exactly-one-encoding (3F17-MH-03, `:64`); `git diff --exit-code` generated-equality
  for the 960-site rename (3F17-MH-04, `:65`); G-Omega + CRUD-LOG (3F17-MH-01,
  `:62`); SK-V18 W0 dispatches "only after" CRUD-4 cleanup + G-Omega authorisation
  + DISPATCH-PROMPT routing (`:174`-`179`). CRUD-4 cap handling forbids silent
  deferral — records a blocked/extension remainder naming remainder/receiver/
  blocker/gate (`:166`-`170`).
- **3C diff applies cleanly.** The hunk inserts at `restart/locks/LOCKS.md:608`-`609`
  (after the SK-V15 addendum Lock-16 clause at `:607`, before `## v+1 Governance
  Boundary` at `:610`) — verified against the live file; context lines match. The
  16-lock count, 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}` (`:107`-`108`), and Lock 1 (`:75`) are preserved verbatim.

## Disposition table (per artefact / load-bearing section)

| # | artefact / section | disposition | file:line | finding + concrete fix |
|---|---|---|---|---|
| 1 | **3C disposition matrix + 3c-locks-v+1-diff.md** (the G3 gate object) | **ACCEPT** | `3c-locks-crystallisation.md:114`-`147`; `3c-locks-v+1-diff.md:45`-`72` | 14/14 dispositioned, zero silent drop; ORQs crystallised with full triples; diff applies cleanly at `LOCKS.md:608`-`609`; 16-lock + 5-shape preserved verbatim. No paper-close. |
| 2 | **3C refutation rows** (the 5 REJECT-class clauses) | **ACCEPT** | `3c-locks-crystallisation.md:158`-`167` | 6th-shape / per-leaf-registry / AoS-SoA-dual / fleet-wide-value / x86-SVE each preserved as a REFUTED clause inside the addendum text, not a deferral. Each cites SPEC/LOCKS/ARCH path:line. Correct anti-paper-close posture. |
| 3 | **3A architecture deltas** (8 deltas incl. ORQ disposition D08) | **ACCEPT** | `3a-architecture-synthesis.md:57`,`:64`,`:77` | Every delta cites a T-P1 divergence-id + T-P2 LAC; D08 converts the 3 ORQs into named SK-V18 pre-gates with receiver+blocker+gate, explicitly "not open-ended deferrals". CH7-overfit Open Question correctly keeps the udot/i8mm orphan in the deferred appendix, not the wired set. |
| 4 | **3B wave reconciliation** (landed/refuted/pending/new) | **ACCEPT** | `3b-master-plan-reconciliation.md:73`,`:84`,`:174` | Refuted-wave revival barred (x86 close route refuted `:108`); the CH6 Open Question itself (`:174`) names the SK-V17 W0-W5 close evidence + Pass-Omega/G-Omega as the receiver+blocker+gate before SK-V18 W0, "no engineered-defer". |
| 5 | **3D skinny-fold** (monotonic; D08 direction clause) | **ACCEPT** | `3d-skinny-fold.md:92`,`:124` | The monotonic-direction invariant is enforced by a named gate, not asserted prose: any inversion is a CH3/CH5 REJECT "with a named receiver, not a future-cycle defer" (`:124`). Fact-stream refutation correctly framed as locks-strengthening, not a contradiction. |
| 6 | **3F MIGRATION/HANDOFF + next-cycle directive** | **ACCEPT** | `3f-migration-handoff.md:151`-`191` | 8 deltas, each receiver/blocker/gate; the directive is mechanical + measurable (grep, git-diff-exit-code, G-Omega, CRUD-LOG); CRUD-4 cap handling records blocked/extension remainder, never silent defer. Closes the engineered-defer aperture (3F17-MH-08, `:69`). |
| 7 | **3C ORQ U3 receiver naming** (aarch64 CollapsedStage) | **REVISE** | `3c-locks-crystallisation.md:137` | U3 names its receiver "a future SK-V18 2E source-backed aarch64-strategy wave" — but 3B's wave allocation ends at MP.SK18.W6 with NO such wave (`3b-master-plan-reconciliation.md` MP.SK18.W0..W6). A phantom-future-wave receiver is the soft edge of engineered-defer. **Fix**: re-anchor U3's receiver to the EXISTING standing gate — the 5-shape `BackendShape` gate + the G-Omega 6th-shape amendment (`LOCKS.md:109`) — as the actual receiving gate, and state the 2E-source-backed wave as the *blocker precondition* for any future ADD/dispatch, not as a named existing receiver. This is already 90% present in the rationale; tighten the matrix Receiver cell to point at the in-force gate, not an unallocated wave. No REJECT: the `admits_collapsed_stage` x86-binding is a hard mechanical blocker and a 6th shape is G-Omega-gated, so U3 cannot silently admit. |
| 8 | **3E EBNF/BNF/CSV/math "DEFER per 2C V4 selection"** | **REVISE** | `3e-grammar-generalisation.md:110` | This row uses the bare word **DEFER** in the BackendShape matrix without an inline receiver/blocker/gate triple on the row itself (unlike the Sheets/BBNF-self "by-construction (SK-V18 proof)" rows, which the §"by-construction-vs-by-exercise" clause D07 governs with a gate). A reader scanning the matrix sees "DEFER" with only a `2C V4 selection` pointer. **Fix**: rename the cell to "by-construction (DEFERRED per 2C V4 grammar-selection; receiver = SK-V18 onboarding wave; blocker = no `structural_index`/scan witness for math; gate = Lock-14 future-grammar onboarding test, 3E17-D07)" — folding it under the same D07 scoping clause that governs the other non-witnessed grammars, so no matrix cell carries an unqualified DEFER. The CH6 Open Question (`:201`) already names the gate; bind it to the row. |
| 9 | **3C disposition tally posture** (0 REJECT / 0 DEFER / all-effectively-accept) | **REVISE** | `3c-locks-crystallisation.md:142`-`147` | The disposition vocabulary includes REJECT and DEFER, and the tally is 0/0 — correct for LOCKED T-P2 inputs (accepting LOCKED LACs is right, not paper-close). BUT the artefact should state EXPLICITLY *why* the absence of any REJECT/DEFER is not paper-close: the inputs were §3Z-LOCKED at T-P2 (V2=98.6%+V3=100%, zero orphan REVISE), so T-P3 crystallises rather than re-adjudicates, and the REFUTATION rows (`:158`-`167`) ARE the REJECT-class content carried as clause text. **Fix**: add one sentence to the disposition-tally section asserting the LOCKED-input provenance as the reason a 0-REJECT tally is correct (not paper-close), cross-referencing the 5 refutation rows as the REJECT-equivalent. This makes the anti-paper-close reasoning explicit on the gate object itself, surviving a hostile re-read. |

## Counts

| disposition | count | % |
|---|---:|---:|
| ACCEPT | 6 | 67% |
| REVISE | 3 | 33% |
| REJECT | 0 | 0% |
| **total** | **9** | |

REVISE share 33% ≥ the §3 30% V1 floor; not a paper-close wave. Zero REJECT — no
delta lacks an evidence chain, no candidate is silently dropped, the gate object
applies cleanly. The three REVISEs are tightening directives (re-anchor a phantom
receiver, qualify one DEFER cell, make the 0-REJECT reasoning explicit), each with
a concrete fix and a path:line; none is an orphan and none blocks G3.

## Orphan-REVISE check (ORCHESTRATOR §3W)

All three REVISEs name the receiving 3X author + the exact edit:
- REVISE-7 → 3C author: re-anchor U3 Receiver cell (`3c-locks-crystallisation.md:137`).
- REVISE-8 → 3E author: qualify the math/EBNF DEFER cell under D07 (`3e-grammar-generalisation.md:110`).
- REVISE-9 → 3C author: add LOCKED-input-provenance sentence to the tally
  (`3c-locks-crystallisation.md:148`-`156`).

No orphan REVISE. CH6 verdict: **PASS-WITH-REVISE** — the synthesis is
anti-paper-close-clean on the load-bearing gate object; fold the three tightening
REVISEs into V2.

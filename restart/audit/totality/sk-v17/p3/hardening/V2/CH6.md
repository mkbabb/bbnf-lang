---
lens: CH6 ANTI-PAPER-CLOSE
pass: T-P3-synthesis
cycle: V2
reviewer: CH6
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
subject: restart/audit/totality/sk-v17/p3/{3a,3b,3c,3d,3e,3f}.md + 3c-locks-v+1-diff.md
contract: restart/prompts/totality/PASS-3-SYNTHESIS.md §3 CH6 + §8.1/§8.5 + ORCHESTRATOR §3W/§3Z
accept: 8
revise: 1
reject: 0
verdict: PASS-WITH-REVISE
---

# CH6 ANTI-PAPER-CLOSE — T-P3 SK-V17 Synthesis (cycle V2)

## Lens charge

Per PASS-3 §3 CH6: no synthesis artefact claims a delta "validated" without the
T-P1/T-P2 evidence chain; no delta deferred to "a future cycle" without a named
receiver + blocker + receiving gate; 3C DEFER dispositions name the re-entry
trigger; 3C's disposition matrix is complete (no silent drop — CH1+CH6 REJECT
class per §8.1/§8.5); 3F's next-cycle directive specifies concrete, measurable
entry conditions. No engineered-defer.

§4 sets the V2 expectation: V2 is a FOLD cycle whose job is to fold every V1
disposition. The convergence criterion is ≥95% ACCEPT for two consecutive cycles
with zero orphan REVISE. This review returns **1 REVISE / 8 ACCEPT / 0 REJECT
(89% ACCEPT)** — the V1 paper-close floor (≥30% REVISE) is a V1-only expectation
(§3 line 99); a V2 fold cycle that has genuinely folded its V1 REVISEs is
EXPECTED to converge upward. The single residual REVISE is a frontmatter-hygiene
defect on the load-bearing gate object, not a substantive paper-close hole.

## V1→V2 fold verification (the load-bearing check this cycle)

CH6 V1 (`hardening/V1/CH6.md:80`-`82`) raised three REVISEs. All three are folded:

- **REVISE-7 (U3 phantom-future-wave receiver)** — FOLDED. `3c-locks-crystallisation.md:138`
  now reads "Receiver: the EXISTING 5-shape `BackendShape` gate
  (`restart/locks/LOCKS.md:107`-`109`) plus the G-Omega 6th-shape amendment path
  … no phantom future wave is named as receiver. … The 2E-source wave is the
  blocker precondition for a future ADD, NOT a named existing receiver." The
  receiver is re-anchored to an in-force gate; the unallocated wave is demoted to
  a blocker precondition exactly as directed. 3A D08 (`3a-architecture-synthesis.md:69`)
  and the 3c diff Lock-10 clause (`3c-locks-v+1-diff.md:62`) carry the same
  re-anchored framing.
- **REVISE-8 (3E bare-DEFER cell)** — FOLDED. `3e-grammar-generalisation.md:140`
  now reads "by-construction (DEFERRED per 2C V4 grammar-selection; receiver =
  SK-V18 onboarding wave; blocker = no `structural_index`/scan witness for math;
  gate = Lock-14 future-grammar onboarding test, 3E17-D07)". The cell carries the
  full receiver/blocker/gate triple under D07, the same scoping clause that
  governs the other non-witnessed grammars. A corpus grep confirms zero remaining
  unqualified `DEFER` in any matrix cell.
- **REVISE-9 (LOCKED-input-provenance not explicit on the gate object)** — FOLDED.
  `3c-locks-crystallisation.md:159`-`170` now carries the "**Why a 0-REJECT /
  0-DEFER tally is not paper-close**" paragraph: it asserts the §3Z-LOCKED input
  provenance (T-P1 clean-final/G1-auto-pinned; T-P2 V2=98.6%+V3=100.0%, zero
  orphan REVISE) as the reason the 0-REJECT tally is correct, and cross-references
  the five refutation rows (`:172`-`181`) as the REJECT-equivalent clause content.
  The anti-paper-close reasoning now survives a hostile re-read on the gate object
  itself.

In addition, the cross-cycle CH1 REVISE on the gate object (the V1 hunk header
`@@ -606,6 +606,52 @@` mis-count) is folded: the header now reads
`@@ -606,7 +606,22 @@` and **`git apply --check` returns EXIT 0** against the
live `restart/locks/LOCKS.md` at master HEAD `2a76916ac` (verified this review).
3D (`3d-skinny-fold.md:82`) and 3F (`3f-migration-handoff.md:52`,`:157`-`161`)
both carry the corrected header as a tracked cross-artefact fold.

## What was checked, and what resolved

- **Disposition completeness (3C) — intact.** 14 candidate rows in the matrix
  (`3c-locks-crystallisation.md:124`-`138`); tally 9 ACCEPT + 3 ORQ-ACCEPT +
  2 MODIFY + 0 REJECT + 0 DEFER = 14 (`:142`-`148`). Zero silent drops; the
  §8.1/§8.5 CH1+CH6 REJECT class does not trigger. Every LAC and every ORQ carries
  a disposition. The two MODIFYs (`:128`,`:133`) remain honest "do-not-choose-the-
  route-in-the-lock" dispositions, not laundered ACCEPTs.
- **No bare "validated".** A whole-corpus grep for `validated|guaranteed`
  unaccompanied by an evidence token (`*.md:`/`*.rs:`/`LAC-`/`2F-`/`1E-`/`SK17L`/
  `SPEC`/`gate`/`checkasm`/`REDRESS`/`G-Omega`) returns **zero hits** across all
  seven p3 artefacts. Every delta cites a T-P1 finding-id, a T-P2 LAC/dossier, or
  a V1 surface at path:line.
- **The 3 ORQs are crystallised, not engineered-defers.** Each of 2F-FOLD-U1/U2/U3
  carries the full Receiver/Blocker/Gate triple (`:136`-`138`): U1 → SK-V18 W2 /
  AoS→SoA collapse-to-one / W2 substrate-union gate; U2 → W2 OnceCell-classification
  pre-gate / retained parallel index re-opens REDRESS-53 / W2 §9-condition-1; U3 →
  in-force 5-shape gate + G-Omega 6th-shape amendment / `admits_collapsed_stage`
  x86-binding mechanical refusal / 5-shape `BackendShape` gate (receiver re-anchored
  per REVISE-7). 3A D08, 3D's CH-row open questions (`3d-skinny-fold.md:140`-`145`),
  and 3F step 8 (`3f-migration-handoff.md:191`-`195`) carry the same three ORQs as
  SK-V18 entry conditions, never as future-cycle defers.
- **3F next-cycle directive is concrete + measurable.** The 8-step directive
  (`3f-migration-handoff.md:152`-`195`) names mechanical gates: `grep` proves
  exactly-one-encoding (3F17-MH-03, `:65`); `git diff --exit-code` generated-equality
  for the 960-site rename (3F17-MH-04, `:66`); G-Omega + CRUD-LOG (3F17-MH-01,
  `:63`); SK-V18 W0 dispatches "only after" CRUD-4 cleanup + G-Omega authorisation
  + DISPATCH-PROMPT routing (`:179`-`184`). CRUD-4 cap handling forbids silent
  deferral — records a blocked/extension remainder naming remainder/receiver/
  blocker/gate (`:171`-`175`,`:128`-`131`). Closes the engineered-defer aperture
  (3F17-MH-08, `:70`).
- **3A/3B ORQ→pre-gate conversions stand.** 3A D08 (`3a-architecture-synthesis.md:69`,`:82`)
  converts the three ORQs into named SK-V18 pre-gates with receiver+blocker+gate,
  explicitly "not open-ended deferrals". 3B's CH6 Open Question
  (`3b-master-plan-reconciliation.md:190`) names the SK-V17 W0-W5 skinny close
  evidence + Pass-Omega/G-Omega as the receiver+blocker+gate before SK-V18 W0
  dispatches — "no engineered-defer". Both fold their V1 REVISEs (3A: CH4-01
  40-file blast radius + CH5-V1-R01 `arena.rs:47` caller-path precision, `:56`,`:75`;
  3B: CH4-02 e-graph/CSP crate-size re-attribution, `:53`-`63`,`:163`).
- **3D monotonic-direction enforced by a named gate.** `3d-skinny-fold.md:145`
  keeps the CH6 invariant: any inversion is "a CH3/CH5 REJECT with a named
  receiver, not a future-cycle defer". The V2 SCOPE-HONESTY BANNER (`:45`-`51`)
  is the folded CH7 open question — it makes the by-construction-vs-by-exercise
  boundary explicit at exec-summary altitude for a G3 skim, an anti-paper-close
  sharpening, not a new claim.
- **Gate object applies clean.** `git apply --check` EXIT 0; the hunk inserts at
  `LOCKS.md:608`-`609` (after the SK-V15 Lock-16 clause at `:607`, before
  `## v+1 Governance Boundary` at `:610`); 16-lock count and 5-shape canon
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` preserved verbatim
  (`3c-locks-v+1-diff.md:86`-`90`, verified against live file).

## Disposition table (per artefact / load-bearing section)

| # | artefact / section | disposition | file:line | finding + concrete fix |
|---|---|---|---|---|
| 1 | **3C disposition matrix + 3c-locks-v+1-diff.md** (the G3 gate object) | **ACCEPT** | `3c-locks-crystallisation.md:124`-`148`; `3c-locks-v+1-diff.md:45`-`72` | 14/14 dispositioned, zero silent drop; ORQs crystallised with full triples; diff applies clean (`git apply --check` EXIT 0) at `LOCKS.md:608`-`609`; 16-lock + 5-shape preserved verbatim. REVISE-7 (U3 re-anchor) + REVISE-9 (provenance paragraph) + CH1 hunk-header all folded in body. No paper-close. |
| 2 | **3C refutation rows** (the 5 REJECT-class clauses) | **ACCEPT** | `3c-locks-crystallisation.md:172`-`181` | 6th-shape / per-leaf-registry / AoS-SoA-dual / fleet-wide-value / x86-SVE each preserved as a REFUTED clause, each citing SPEC/LOCKS/ARCH path:line. The §"Why 0-REJECT is not paper-close" paragraph (`:159`-`170`) now explicitly binds these as the REJECT-equivalent content. Correct anti-paper-close posture. |
| 3 | **3A architecture deltas** (8 deltas incl. ORQ disposition D08) | **ACCEPT** | `3a-architecture-synthesis.md:56`,`:69`,`:111` | Every delta cites a T-P1 divergence-id + T-P2 LAC; D08 converts the 3 ORQs into named pre-gates with receiver+blocker+gate, "not open-ended deferrals". V1 CH4-01/CH5-R01 folded (40-file blast radius; `arena.rs:47` unique-caller path). CH7 udot/i8mm orphan correctly stays in the deferred appendix (`:111`). |
| 4 | **3B wave reconciliation** (landed/refuted/pending/new) | **ACCEPT** | `3b-master-plan-reconciliation.md:114`-`125`,`:190` | Refuted-route revival barred (x86 close route `:124`, dual AoS/SoA `:125`); the CH6 Open Question (`:190`) names SK-V17 W0-W5 close evidence + Pass-Omega/G-Omega as receiver+blocker+gate before SK-V18 W0; "no engineered-defer". CH4-02 e-graph/CSP sizing folded. |
| 5 | **3D skinny-fold** (monotonic; D08 direction clause + scope-honesty banner) | **ACCEPT** | `3d-skinny-fold.md:45`-`51`,`:145` | Monotonic-direction invariant enforced by a named gate, not asserted prose: any inversion is a CH3/CH5 REJECT "with a named receiver, not a future-cycle defer" (`:145`). The folded CH7 SCOPE-HONESTY BANNER (`:45`-`51`) forces a G3 skim to read Sheets/BBNF-self as predicted/SK-V18-pending — anti-paper-close hardening. |
| 6 | **3F MIGRATION/HANDOFF + next-cycle directive** | **ACCEPT** | `3f-migration-handoff.md:152`-`195` | 8 deltas, each receiver/blocker/gate; the directive is mechanical + measurable (grep exactly-one-encoding, git-diff-exit-code, G-Omega, CRUD-LOG); CRUD-4 cap handling records blocked/extension remainder, never silent defer (`:171`-`175`). 3F17-MH-08 closes the engineered-defer aperture. |
| 7 | **3E grammar-generalisation** (math/EBNF DEFER cell + onboarding predicates) | **ACCEPT** | `3e-grammar-generalisation.md:140`,`:175`,`:257` | REVISE-8 folded: the math/EBNF matrix cell now carries the full receiver/blocker/gate triple under D07 (`:140`); zero unqualified DEFER remains. P6 value-axis firewall (`:175`) and the CH6 Open Question (`:257`) name receiver+blocker+gate; a missing Sheets/BBNF-self witness "cannot be deferred by prose" — needs owner path + blocker + scoping rule. Anti-paper-close clean. |
| 8 | **Cross-corpus assertion hygiene** (no bare "validated"/engineered-defer) | **ACCEPT** | whole corpus (grep) | Zero bare `validated`/`guaranteed` without an evidence token across all seven artefacts. Every ORQ/DEFER/pre-gate carries receiver+blocker+gate. No "a future cycle" defer lacks a named receiver. The anti-paper-close floor on assertions holds corpus-wide. |
| 9 | **3C frontmatter convergence-state** (cycle + folded-disposition blocks) | **REVISE** | `3c-locks-crystallisation.md:4`,`:14`-`18`,`:38`-`43`; `3c-locks-v+1-diff.md:4`,`:10`-`18` | The 3C body has folded three V2 REVISEs (REVISE-7 U3 re-anchor `:138`, REVISE-9 provenance `:159`-`170`) plus the CH1 hunk-header fix — but BOTH 3C artefacts still carry `cycle: V1` (`3c-locks-crystallisation.md:4`; `3c-locks-v+1-diff.md:4`), and `prior_cycle_dispositions_folded.revised: []` / `delta_summary.carried_from_prior_cycle: []` (`:14`,`:38`-`43`). Every other artefact (3A/3B/3D/3E/3F) carries `cycle: V2` with a populated `revised:` list. On the **G3 gate object** specifically, a frontmatter that reads `revised: []` falsely presents 3C as an un-folded first-cycle artefact when three REVISEs (CH6-V1-07/09 + CH1-V1) were in fact folded into it. Per §4 "the V{N} Delta Summary block is regenerated" each cycle; a stale block is hardening-without-fully-folding hygiene — a soft paper-close surface on the load-bearing artefact (it hides the convergence work). **Fix**: bump both 3C frontmatter blocks to `cycle: V2`; populate `prior_cycle_dispositions_folded.revised` with `[CH6-V1-07-u3-receiver-reanchor, CH6-V1-09-locked-input-provenance, CH1-V1-hunk-header-arithmetic]`; carry the five delta-ids in `carried_from_prior_cycle`; regenerate the V2 Delta Summary table (`3c-locks-crystallisation.md:83`-`88`) to a carried/revised shape mirroring 3A's `:50`-`56`. No REJECT: the body is V2-correct and the gate applies clean; this is a metadata-truth tightening so the gate object's frontmatter does not under-report its own fold. |

## Counts

| disposition | count | % |
|---|---:|---:|
| ACCEPT | 8 | 89% |
| REVISE | 1 | 11% |
| REJECT | 0 | 0% |
| **total** | **9** | |

V2 is a fold cycle; the three V1 CH6 REVISEs are all folded in substance, so the
ACCEPT share rises to 89% as a genuinely-converging fold should. The single
residual REVISE is a frontmatter convergence-state defect on the 3C gate object
(stale `cycle: V1` + empty `revised: []` despite three folded REVISEs) — a
metadata-truth gap, not a substantive paper-close hole; it does not block G3 and
carries a concrete, mechanical fix. Zero REJECT: no delta lacks an evidence
chain, no candidate is silently dropped, no defer lacks a receiver+blocker+gate,
the gate object applies clean.

## Orphan-REVISE check (ORCHESTRATOR §3W)

The single REVISE names its receiving 3X author + the exact edit:
- REVISE-9 → 3C author: bump both 3C artefacts to `cycle: V2`, populate
  `prior_cycle_dispositions_folded.revised` with the three folded finding-ids,
  carry the five delta-ids, regenerate the V2 Delta Summary table
  (`3c-locks-crystallisation.md:4`,`:14`-`18`,`:38`-`43`,`:83`-`88`;
  `3c-locks-v+1-diff.md:4`,`:10`-`18`).

No orphan REVISE. CH6 verdict: **PASS-WITH-REVISE** — the synthesis is
anti-paper-close-clean on every substantive axis (no bare validated, 14/14
dispositioned, every ORQ/DEFER triple-named, the gate object applies clean,
the three V1 REVISEs folded in body); the lone residual is a 3C frontmatter
convergence-state truth gap that the 3C author folds into V3 with a mechanical
metadata regeneration. Convergence is on track: zero REJECT, one tightening
REVISE, the load-bearing gate object intact.

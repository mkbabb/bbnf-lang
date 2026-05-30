---
lens: CH6 ANTI-PAPER-CLOSE
pass: T-P3-synthesis
cycle: V3
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

# CH6 ANTI-PAPER-CLOSE — T-P3 SK-V17 Synthesis (cycle V3)

## Lens charge

Per PASS-3 §3 CH6: no synthesis artefact claims a delta "validated" without the
T-P1/T-P2 evidence chain; no delta deferred to "a future cycle" without a named
receiver + blocker + receiving gate; 3C DEFER dispositions name the re-entry
trigger; 3C's disposition matrix is complete (no silent drop — CH1+CH6 REJECT
class per §8.1/§8.5); 3F's next-cycle directive specifies concrete, measurable
entry conditions. No engineered-defer.

§4 sets the V3 expectation: V3 is the second consecutive fold cycle whose job is
to fold every V2 disposition and demonstrate convergence (≥95% ACCEPT for two
consecutive cycles, zero orphan REVISE). This review returns **1 REVISE / 8
ACCEPT / 0 REJECT (89% ACCEPT)**. The V1 paper-close floor (≥30% REVISE) is a
V1-only expectation (§3 line 99); a converging V3 fold cycle that has genuinely
folded its V2 REVISE is EXPECTED to sit high. The single residual REVISE is a
forward-facing anti-silent-satisfy tightening on the 3C CH6 Open Question +
the bare-word "DEFERRED" in the 3E matrix cell — neither is a substantive
paper-close hole, both carry a concrete mechanical fix, neither blocks G3.

## V2→V3 fold verification (the load-bearing check this cycle)

My V2 review (`hardening/V2/CH6.md:134`) raised exactly one REVISE — the 3C
frontmatter convergence-state truth gap on the **G3 gate object**: both 3C
artefacts carried `cycle: V1` with `revised: []` despite three V1 REVISEs
(CH6-V1-07/09 + CH1-V1) folded into the body, under-reporting their own fold.
**FOLDED, verified this review:**

- `3c-locks-crystallisation.md:4` and `3c-locks-v+1-diff.md:4` both now read
  `cycle: V3` (corpus grep confirms all seven 3X artefacts carry `cycle: V3`).
- `prior_cycle_dispositions_folded.revised`
  (`3c-locks-crystallisation.md:43`-`47`; `3c-locks-v+1-diff.md:18`) is now
  populated with `[CH6-V1-07-u3-receiver-reanchor,
  CH6-V1-09-locked-input-provenance, CH1-V1-hunk-header-arithmetic,
  CH5-V2-R01-distribution-invariant-gate-object]`.
- `delta_summary.carried_from_prior_cycle` carries the five delta-ids
  (`3c-locks-crystallisation.md:15`-`20`).
- The V3 Delta Summary table is regenerated to a carried/revised shape
  (`3c-locks-crystallisation.md:92`-`98`; `3c-locks-v+1-diff.md:49`-`55`),
  mirroring the other six artefacts.

The gate object no longer under-reports its own convergence work; the
metadata-truth gap is closed.

The cross-cycle CH5-V2-R01 distribution-invariant fold is also CH6-relevant: a
distribution invariant present in the crystallisation doc but absent from the
travelling gate object is a silent-drop-by-distribution surface. **FOLDED:** the
distribution-invariant bullet now lives in the gate object's Invariant Check
(`3c-locks-v+1-diff.md:103`), so the apply-time fence rides the object Pass Omega
applies. Corpus grep confirms `distribution invariant` present 2× in BOTH 3C docs.

## What was checked, and what resolved

- **Disposition completeness (3C) — intact.** 14 candidate rows in the matrix
  (`3c-locks-crystallisation.md:135`-`148`); tally 9 ACCEPT + 3 ORQ-ACCEPT +
  2 MODIFY + 0 REJECT + 0 DEFER = 14 (`:154`-`158`). Zero silent drops; the
  §8.1/§8.5 CH1+CH6 REJECT class does not trigger. Every LAC, every T-P1
  antecedent, and every ORQ carries a disposition. The two MODIFYs
  (`:139`,`:143`) remain honest "do-not-choose-the-route-in-the-lock"
  dispositions, not laundered ACCEPTs.
- **No bare "validated".** A whole-corpus grep for `validated|guaranteed`
  unaccompanied by an evidence token returns ZERO substantive hits across all
  seven p3 artefacts. The single literal `validated` occurrence
  (`3a-architecture-synthesis.md:75`) is the CH6 self-charge meta-reference ("no
  delta claimed 'validated' without evidence chain"), not a validation assertion.
  Every delta cites a T-P1 finding-id, a T-P2 LAC/dossier, or a V1 surface at
  path:line.
- **The 3 ORQs are crystallised, not engineered-defers.** Each of
  2F-FOLD-U1/U2/U3 carries the full Receiver/Blocker/Gate triple
  (`3c-locks-crystallisation.md:146`-`148`): U1 → SK-V18 W2 / AoS→SoA
  collapse-to-one-encoding / W2 substrate-union gate; U2 → W2
  OnceCell-classification pre-gate / retained parallel index re-opens REDRESS-53 /
  W2 §9-condition-1; U3 → in-force 5-shape `BackendShape` gate + G-Omega 6th-shape
  amendment / `admits_collapsed_stage` x86-binding mechanical refusal + 2E-source
  wave as blocker-precondition-for-ADD / 5-shape gate. 3A D08
  (`3a-architecture-synthesis.md:89`), 3D's CH-row register
  (`3d-skinny-fold.md:120`,`:126`), and 3F step-by-step directive carry the same
  three ORQs as SK-V18 entry conditions, never as future-cycle defers.
- **3F next-cycle directive is concrete + measurable.** The 7+-step directive
  (`3f-migration-handoff.md:163`-`199`) names mechanical gates: `git apply --check`
  EXIT 0 on the gate object (step 1, re-verified this review); `grep` exactly-one-
  encoding (3F17-MH-03); `git diff --exit-code` generated-equality for the
  960-site rename (3F17-MH-04); G-Omega + CRUD-LOG (3F17-MH-01); SK-V18 W0
  dispatches "only after" CRUD-4 cleanup + G-Omega authorisation + DISPATCH-PROMPT
  routing (step 6). CRUD-4 cap handling forbids silent deferral — records a
  blocked/extension remainder naming remainder/receiver/blocker/gate (step 4,
  "No silent deferral." `:186`). Closes the engineered-defer aperture (3F17-MH-08).
- **3A/3B ORQ→pre-gate conversions stand.** 3A D08 (`3a-architecture-synthesis.md:89`)
  converts the three ORQs into named SK-V18 pre-gates with receiver+blocker+gate,
  explicitly "not open-ended deferrals". 3B's SK-V18 Fold Receiver Block
  (`3b-master-plan-reconciliation.md:132`-`142`) gives each NEW receiver row a
  same-wave consumer / gate (no orphan pre-gate, `:137`); the V2 CH4 D08
  blast-radius propagation REVISE (22→40 files via `grep -rl
  JsonStructBuilder|CssStructBuilder crates/`) is folded V3 (`:27` frontmatter).
- **3D monotonic-direction enforced by a named gate.** `3d-skinny-fold.md:127`
  (D08) keeps the CH6 invariant: the skinny→totality direction is a governance
  invariant; any inversion is a CH3/CH5 REJECT with a named receiver, not a
  future-cycle defer. The sheets/BBNF-self generality gap is routed to 3E (D07,
  `:126`), scope-honest by-construction-not-by-exercise, no fleet-wide over-claim.
- **3E onboarding test has a named owner + re-entry trigger.** The future-grammar
  onboarding tape-predicates (`3e-grammar-generalisation.md:121`, 3E17-D08) bind
  the leak-census monotonic decrease to MP.SK18.W3 with a fail-closed admitted
  catalogued non-zero baseline (7 `strategy.rs` sites) and a named re-entry trigger
  (next SK-V18 onboarding-wave leak-census gate) — not an unowned "HEAD→0" prose
  claim. CH6 anti-paper-close clean on the generality axis.
- **Gate object applies clean.** `git apply --check` returns **EXIT 0** against the
  live `restart/locks/LOCKS.md` at master HEAD `2a76916ac` (re-verified this
  review); the extracted hunk header `@@ -606,7 +606,22 @@` is arithmetic-correct
  (7 old-side context, 22 new-side = 7 context + 15 added; no removed lines). The
  hunk inserts at `LOCKS.md:608`-`609`. 16-lock count and 5-shape canon
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` preserved verbatim
  (`3c-locks-v+1-diff.md:100`-`101`).

## Disposition table (per artefact / load-bearing section)

| # | artefact / section | disposition | file:line | finding + concrete fix |
|---|---|---|---|---|
| 1 | **3C disposition matrix + 3c-locks-v+1-diff.md** (the G3 gate object) | **ACCEPT** | `3c-locks-crystallisation.md:135`-`158`; `3c-locks-v+1-diff.md:59`-`86` | 14/14 dispositioned, zero silent drop; 3 ORQs crystallised with full triples; diff applies clean (`git apply --check` EXIT 0) at `LOCKS.md:608`-`609`; 16-lock + 5-shape preserved verbatim. V2 REVISE-9 (frontmatter convergence-state) FOLDED: `cycle: V3` + populated `revised:` on both docs; CH5-V2-R01 distribution invariant now in the gate object Invariant Check (`:103`). No paper-close. |
| 2 | **3C refutation rows + "Why 0-REJECT is not paper-close"** | **ACCEPT** | `3c-locks-crystallisation.md:169`-`191` | The five refutations (6th-shape / per-leaf-registry / AoS-SoA-dual / fleet-wide-value / x86-SVE) each preserved as a REFUTED clause citing SPEC/LOCKS/ARCH path:line; the §"Why 0-REJECT is not paper-close" paragraph (`:169`-`181`) binds the LOCKED-input provenance as the reason the 0-REJECT tally is correct. Survives a hostile re-read. Correct anti-paper-close posture. |
| 3 | **3A architecture deltas** (incl. ORQ disposition D08) | **ACCEPT** | `3a-architecture-synthesis.md:75`,`:89`,`:131` | Every delta cites a T-P1 divergence-id + T-P2 LAC; D08 converts the 3 ORQs into named pre-gates with receiver+blocker+gate, "not open-ended deferrals". udot/i8mm orphan correctly confined to the deferred appendix (CH7 REFUTED, `:131`). |
| 4 | **3B wave reconciliation** (landed/refuted/pending/new + SK-V18 receiver block) | **ACCEPT** | `3b-master-plan-reconciliation.md:132`-`142`,`:27` | Each NEW SK-V18 receiver row carries a same-wave consumer / gate (no orphan pre-gate); refuted-route revival barred. V2 CH4 D08 blast-radius propagation REVISE (22→40 files, `grep -rl` re-verified) folded V3. No engineered-defer. |
| 5 | **3D skinny-fold** (monotonic D08 + scope-honesty) | **ACCEPT** | `3d-skinny-fold.md:120`,`:126`-`127` | Monotonic-direction invariant enforced by a named gate, not asserted prose: any inversion is a CH3/CH5 REJECT with a named receiver. D07 routes the sheets/BBNF-self generality gap to 3E by-construction-not-by-exercise, no fleet-wide over-claim. Anti-paper-close clean. |
| 6 | **3F MIGRATION/HANDOFF + next-cycle directive** | **ACCEPT** | `3f-migration-handoff.md:163`-`199` | Each delta receiver/blocker/gate; the directive is mechanical + measurable (`git apply --check` EXIT 0, grep exactly-one-encoding, `git diff --exit-code`, G-Omega, CRUD-LOG); CRUD-4 cap handling records blocked/extension remainder, never silent defer (`:182`-`186`, "No silent deferral."). 3F17-MH-08 closes the engineered-defer aperture. |
| 7 | **3E onboarding test + math/EBNF DEFER cell** | **REVISE** | `3e-grammar-generalisation.md:157`; `3c-locks-crystallisation.md:226`-`228` | The onboarding-test owner-binding (3E17-D08, `:121`) and the P6 value-axis firewall are anti-paper-close clean. TWO residual silent-satisfy surfaces remain on a hostile skim: (a) the math/EBNF matrix cell (`3e:157`) carries the full receiver/blocker/gate triple BUT leads with the bare word **`by-construction (DEFERRED …`** — on a G3 skim the parenthetical reads as a soft defer keyword before the qualifying triple; the V2-folded form (`hardening/V2/CH6.md:48`-`54` quoted `DEFERRED per 2C V4 …`) is already triple-qualified, so this is a wording-precedence tightening not a missing receiver. (b) The 3C CH6 Open Question (`3c-locks-crystallisation.md:228`) asks "can any candidate be treated as already satisfied by current `LOCKS.md` text (e.g. LAC-2F-FOLD-02 by the in-force LAC-1E-14 FactStream category)" — if Pass Omega answers YES, a dispositioned candidate becomes a silent no-op unless the answer is itself logged as an explicit ACCEPT-by-prior-text disposition. **Fix**: (a) re-order the `3e:157` cell so the receiver/blocker/gate triple LEADS and the disposition word is `DEFERRED-WITH-TRIPLE` (or drop the bare `DEFERRED` and read `by-construction; receiver = … blocker = … gate = …`) so no skim sees an unqualified defer keyword; (b) add a clause to the 3C CH6 Open Question blocker cell (`:228`): "answering 'yes' is itself a logged ACCEPT-satisfied-by-prior-text disposition in the Pass Omega CRUD-3 LOG, never a silent satisfy — a candidate dispositioned ACCEPT here may not be dropped to no-op by a later 'already satisfied' finding without that finding being recorded as its own disposition row." No REJECT: both bodies carry the substantive receiver/blocker/gate and the candidate is dispositioned; this is a silent-satisfy aperture hardening on the gate object's forward path. |
| 8 | **Cross-corpus assertion hygiene** (no bare "validated"/engineered-defer) | **ACCEPT** | whole corpus (grep) | Zero bare `validated`/`guaranteed` without an evidence token; the lone literal is the CH6 self-charge. Every ORQ/DEFER/pre-gate carries receiver+blocker+gate. No "a future cycle" defer lacks a named receiver. Anti-paper-close floor on assertions holds corpus-wide. |
| 9 | **3C frontmatter convergence-state** (V2 REVISE fold target) | **ACCEPT** | `3c-locks-crystallisation.md:4`,`:15`-`20`,`:43`-`47`,`:92`-`98`; `3c-locks-v+1-diff.md:4`,`:11`-`18`,`:49`-`55` | The V2 CH6 single REVISE is FOLDED: both 3C artefacts now read `cycle: V3`; `revised:` is populated with all three V1 finding-ids + CH5-V2-R01; `carried_from_prior_cycle` carries the five delta-ids; the V3 Delta Summary table is regenerated to a carried/revised shape mirroring the other artefacts. The gate object no longer under-reports its own fold. Metadata-truth gap closed. |

## Counts

| disposition | count | % |
|---|---:|---:|
| ACCEPT | 8 | 89% |
| REVISE | 1 | 11% |
| REJECT | 0 | 0% |
| **total** | **9** | |

V3 is the second consecutive converging fold cycle; the single V2 CH6 REVISE
(3C frontmatter convergence-state) is fully folded, lifting that surface to
ACCEPT. The single residual REVISE is a forward-facing silent-satisfy aperture on
the 3E matrix-cell defer-wording + the 3C CH6 Open Question — a hardening
tightening on the gate object's forward distribution/satisfy path, not a
substantive paper-close hole; it does not block G3 and carries a concrete,
mechanical fix. Zero REJECT: no delta lacks an evidence chain, no candidate is
silently dropped, no defer lacks a receiver+blocker+gate, the gate object applies
clean (`git apply --check` EXIT 0).

## Orphan-REVISE check (ORCHESTRATOR §3W)

The single REVISE names its receiving 3X authors + the exact edits:
- REVISE-7a → 3E author: re-order the math/EBNF matrix cell
  (`3e-grammar-generalisation.md:157`) so the receiver/blocker/gate triple LEADS
  and the disposition word is not a bare `DEFERRED` on skim.
- REVISE-7b → 3C author: add the anti-silent-satisfy clause to the CH6 Open
  Question blocker cell (`3c-locks-crystallisation.md:228`) — "answering 'yes' is
  itself a logged ACCEPT-satisfied-by-prior-text disposition, never a silent
  satisfy".

No orphan REVISE. CH6 verdict: **PASS-WITH-REVISE** — the synthesis is
anti-paper-close-clean on every substantive axis (no bare validated, 14/14
dispositioned, every ORQ/DEFER triple-named, the gate object applies clean at
EXIT 0, the V2 frontmatter REVISE fully folded); the lone residual is a
forward-path silent-satisfy aperture the 3E + 3C authors close with two mechanical
wording edits. Convergence is on track: zero REJECT, one tightening REVISE, the
load-bearing gate object intact across two consecutive cycles.

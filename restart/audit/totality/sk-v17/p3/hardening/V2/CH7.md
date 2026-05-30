---
lens: CH7 OVERFIT-PRUNE
pass: T-P3-synthesis
cycle: V2
reviewer: CH7 (V2)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
subject: restart/audit/totality/sk-v17/p3/{3a,3b,3c,3d,3e,3f}.md + 3c-locks-v+1-diff.md
focus: 16-lock count preserved (no silent renumber; ADD/RETIRE G-Omega-gated); no contrivance; the fold is genuinely general; no fabricated speed claim; lightningcss the fair bar
dispositions: { ACCEPT: 12, REVISE: 1, REJECT: 0 }
verdict: ACCEPT-WITH-ONE-REVISE (92.3% ACCEPT)
prior_cycle_dispositions_folded:
  accepted:
    - CH7-S17-R1-3c-tally-arithmetic        # FOLDED: 3c:55 now reads "9 ACCEPT, 3 ACCEPT (ORQ-crystallised), 2 MODIFY, 0 REJECT, 0 DEFER"
    - CH7-OQ-scope-honesty-banner           # FOLDED: 3D exec-summary now carries the SCOPE-HONESTY BANNER (3d:45-51)
---

# CH7 OVERFIT-PRUNE — T-P3 SK-V17 Synthesis (cycle V2)

## Lens scope

CH7 is the overfit-prune lens. It does not re-audit citation-resolution (CH1)
or coupling (CH5); it asks five questions and only those: (1) is the 16-lock
count genuinely preserved, with no silent renumber and ADD/RETIRE G-Omega-gated;
(2) is any delta a contrivance dressed up as principle; (3) is the
tape/`ValueRef<G>`/NEON fold *genuinely* general or JSON+CSS overfit wearing a
generality costume; (4) is any speed claim fabricated or over-stated; (5) is
lightningcss (and the JSON SOTA cohort) held as the fair, un-gamed bar. I
verified the load-bearing claims against ground truth at HEAD `2a76916ac`, not
against the prose's self-citations.

## V1 fold confirmation (both V1 CH7 items resolved)

| V1 item | V1 disposition | V2 status |
|---|---|---|
| **CH7-S17-R1** — 3c exec-summary tally read "9 ACCEPT, 5 MODIFY" contradicting the matrix's 2 MODIFY (`3c:55`) | REVISE | **FOLDED.** `3c-locks-crystallisation.md:55` now reads "**9 ACCEPT, 3 ACCEPT (ORQ-crystallised), 2 MODIFY, 0 REJECT, 0 DEFER**" — reconciles with the matrix tally (`3c:142`-`148`) and the diff (`3c-locks-v+1-diff.md:39`). Verified verbatim. |
| **CH7/CH4 open question** — should 3A/3D/3E exec summaries carry a proven-vs-by-construction banner so a G3 skim cannot read by-construction grammars as proven | open question | **FOLDED.** 3D exec summary now opens with the SCOPE-HONESTY BANNER (`3d:45`-`51`): JSON by-exercise-proven, CSS first-moved, Sheets/BBNF-self by-construction-not-by-exercise, classifier config-breadth but tape-consumer JSON-only-wired. The banner is honest and at the right altitude. |

## Verification ledger (ground-truth re-execution at HEAD)

| claim under test | source | re-executed result | verdict |
|---|---|---|---|
| 16 numbered locks, no renumber | `restart/locks/LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453` | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` = 16; highest-numbered = 16; the v+1 diff inserts an addendum at `:608`-`609`, adds/retires/renumbers ZERO numbered lock | TRUE |
| 5 BackendShape variants verbatim | `restart/locks/LOCKS.md:107`-`108` | `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` present; addendum + Lock-10 clause restate the five, add no 6th; 6th remains G-Omega gated (`:109`) | TRUE |
| v+1 diff applies clean | `3c-locks-v+1-diff.md:49` `@@ -606,7 +606,22 @@` | extracted the diff block and ran `git apply --check` → EXIT 0, "APPLIES CLEAN" (the V1 corrupt-header `@@ -606,6 +606,52 @@` is folded out) | TRUE |
| `StructLayout` = 960 sites in crates/ | `3c:94`, `3a:62`, `3b:163`, `3f:66` | `grep -rc StructLayout crates/` summed = 960 | TRUE |
| `backend_shape`/`LayoutFacts` = 0 in crates/ | `3c:94`, `3e:140` | `grep -rE 'backend_shape\|LayoutFacts' crates/ \| wc -l` = 0 | TRUE |
| 28-65×/983×/10583× regression | SPEC `:793`-`795` | SPEC text matches exactly (28-65 bbnf/sheets, 983 css bootstrap, 10583 WATCHDOG tailwind) | TRUE (cited, not invented) |
| JSON >SOTA carrier | `skinny/RESULTS.md` row 3 (the cited `:5-55` band) | twitter parse_only: Track 1 **8349.290** Mbps > sonic-rs strict **4913.095** Mbps (**+69.9%**), per-iter equality PASS — real measured row | TRUE (real measured row) |
| P1 leak baseline "7 hits, all strategy.rs" | `3e:169` | catalogued ARCH-3A-D09 leak surface; string-ident registry + doc-comments, NOT runtime `match grammar{}` arms; monotonic-decrease-to-zero rule applied | TRUE |
| **CSS >SOTA NOT met / UNMEASURED-PENDING** | `restart/skinny/tranches/sk-v17/HANDOFF.md:44`-`45`; `SPEC.md:207` | HANDOFF: "the **>SOTA bar is NOT met and nothing on the CSS path moved**"; SPEC: "ALL per-corpus lightningcss endpoints are **UNMEASURED-PENDING**" | TRUE — and this is the ground that exposes CH7-S17-V2-R1 below |

Every load-bearing JSON/lock/grammar number CH7 could falsify resolved to
ground truth. No fabrication surfaced in 3A/3B/3C/3D/3E. ONE over-stated CSS
speed framing surfaced in 3F (below).

## Section dispositions

### §3A ARCHITECTURE synthesis — ACCEPT
Eight deltas; each fold is conservative against the V1 surface (ARCH §7.3
already frames the five shapes as tape projections; the fold makes placement
explicit, invents no substrate). D04 discharges the "no silent 6th shape"
mandate in the negative on two independent grounds (LAC-1E-14 precedent +
`admits_collapsed_stage` aarch64-refusal). D03's "impl-exceeds-spec, 0-LOC
narrative fold" is true (classifier wired across 8-of-9 grammars,
alphabet-as-data). The CH7 open question on the `udot`/i8mm orphan kernel
(`3a:111`) is correctly disposed: the kernel is REFUTED (no CSS antecedent, no
live consumer) and stays in the deferred appendix, not the wired set — exactly
the overfit-prune outcome. The V1 CH4-01 fold (D01 blast radius corrected from
"22+ files" to 40) and CH5-V1-R01 fold (the `arena.rs:47` unique-caller
property) tighten honesty, not loosen it. No contrivance; no fabricated speed.

### §3B MASTER-PLAN reconciliation — ACCEPT
No refuted wave revived: AZ-IV 118× / per-leaf indirection / fact-stream-String
/ x86 rejections are carried as FENCES in the Refuted-Route Confirmation table
(`3b:114`-`125`), each pre-blocked, not re-derived. The V2 CH4-02 fold is a
genuine honesty correction: D07's 311/273 LOC were mis-attributed to
`crates/egraph`+`crates/csp-solver` (actual 1885/5882); they are the skinny
`backend_egraph.rs`/`decision_csp.rs` scaffolds — corrected with the right
citation, and the "wire, do not build" framing survives (the 600-1400 LOC
envelope sizes the WIRING). The 5-shape canon is preserved verbatim across §13
H.W4 / §13.5 / §13.1. No fabricated throughput.

### §3C LOCKS crystallisation — ACCEPT
The V1 R1 tally defect is FOLDED (`3c:55` reconciles with the matrix). 14
candidates dispositioned, zero silent drops; the 0-REJECT/0-DEFER tally is
defended on LOCKED-input provenance with the five refutation rows preserved as
REJECT-class clause text (`3c:172`-`181`), not laundered as ACCEPTs. The
disposition matrix is realistic. The diff applies clean (verified). The
16-lock count and 5-shape canon are preserved verbatim and the invariant-check
block (`3c-locks-v+1-diff.md:84`-`90`) re-states them. No lock renumbered;
ADD/RETIRE correctly G-Omega-gated.

### §3D skinny-fold — ACCEPT (the V1 open question is folded here)
Monotonic direction held: SKINNY wins → V1-authoritative; rejections →
locks-strengthening; totality never dictates back. The ONE load-bearing WIN is
correctly the SoA `Tape`+`ValueRef<G>`, grounded in `RESULTS.md` (verified
8349.290 > 4913.095). The SCOPE-HONESTY BANNER (`3d:45`-`51`) folds the V1
CH7/CH4 open question precisely: it tells a G3 skim that CSS is FIRST-MOVED (not
">SOTA proven"), that the tape CONSUMER is JSON-only-wired, and that
Sheets/BBNF-self are predicted/SK-V18-pending. This is the anti-overfit posture
the lens wants. Honest.

### §3E grammar-generalisation — ACCEPT (the strongest anti-overfit section)
This is where overfit would hide, and it does not. The per-grammar matrix tags
every non-witnessed shape cell *predicted (cost-model-pending)* at the CELL
level (`3e:128`-`141`), not only in a status column — a reader scanning the
dominant-shape column reads `predicted` on every non-JSON/non-CSS row. The
future-grammar onboarding test uses a monotonic-decrease-to-zero rule against a
live HEAD baseline (P1 = 7 hits) and carries TWO orthogonal generality axes
(P1/P3 classifier-leak + P6 value-plane firewall). The V1 S10 fold split P5
into P5a (CSS classifier-scan, wired/measured) vs P5b (CSS tape-consumer,
SK-V18-pending, NOT yet measured) — `3e:174` states this plainly. The classifier
generality is config-breadth (alphabet-as-data across 8 grammars), never
conflated with fleet-wide value-plane proof. The fail-closed clause scopes the
claim to the witnessed grammars and bars fleet-wide wording. General, not
overfit. Lock 14 grammar-neutrality preserved; no JSON-narrowing, no
CSS-narrowing.

### §3F MIGRATION/HANDOFF — REVISE (one defect) — see below
The migration receiver, the LAC receiver table, the single-encoding closure
gate, the 960-site rename row, the two regression fences, and the next-cycle
dispatch directive (Pass Omega CRUD-4 → G-Omega → SK-V18 W0) are all correct,
concrete, and measurable; the engineered-defer aperture is closed (`3f:175`,
`:189`-`190`). The CH1-SKV17-01 cross-fold (the corrected diff hunk header) is
real and verified. BUT the executive summary and the HANDOFF carrier over-state
the CSS speed posture — the one CH7 fabricated/over-stated-speed-claim hit.

## The one REVISE

**CH7-S17-V2-R1 — REVISE** (`restart/audit/totality/sk-v17/p3/3f-migration-handoff.md:31`
and the parallel carrier line `:84`)

3F states SK-V17 is "the converged skinny **contract proving CSS-on-tape /
lazy-`ValueRef` / shared-NEON >SOTA**" (`3f:31`) and the HANDOFF carrier
repeats "SK-V17 (the SKINNY tape-fold **proof of CSS-on-tape /
lazy-`ValueRef` / shared-NEON >SOTA**)" (`3f:84`). This attaches ">SOTA" to
**CSS** and calls it **proven**. Ground truth at HEAD contradicts it on three
counts:

- `restart/skinny/tranches/sk-v17/HANDOFF.md:44`-`45`: "roughly an order of
  magnitude (build plane) below the >SOTA bar. **The >SOTA bar is NOT met and
  nothing on the CSS path moved.**"
- `restart/skinny/tranches/sk-v17/SPEC.md:207`: "ALL per-corpus lightningcss
  endpoints are **UNMEASURED-PENDING**."
- 3E's own P5b (`restart/audit/totality/sk-v17/p3/3e-grammar-generalisation.md:174`):
  "CSS L4 **tape consumer** is the SK-V18 fold-target, **NOT yet measured**."

SK-V17 PROVED the **JSON** model >SOTA (8349 > 4913, verified) and CONVERGED on
a *contract/plan* whose CSS >SOTA bar is the SK-V18 PROOF OBLIGATION. 3F's prose
collapses "converged contract whose CSS objective is >SOTA" into "proof of CSS
... >SOTA". That is an over-stated speed claim — the precise CH7 failure mode —
and it is internally incoherent with 3D's SCOPE-HONESTY BANNER, 3E P5b, and
3F's own next-cycle text at `:202` (CH3 open question) which correctly treats
CSS >SOTA as an SK-V18-W0+ obligation. CH7 V1 itself recorded (`V1/CH7.md:138`)
"CSS-vs-lightningcss is correctly *not yet asserted met*" — but did not scan
3F's `proving … >SOTA` phrasing, so the defect carried into V2 unfolded.

Why CH7 (not CH1): the citation `HANDOFF.md:28-104` resolves; the words inside
the sentence are the overfit. A G3 reader skimming 3F's executive summary or the
HANDOFF carrier (the cold-start landing surface, `3f:108`-`137`) would read CSS
>SOTA as achieved when the bar is explicitly NOT met — eroding the
fabricated-speed-claim firewall this lens owns.

**Concrete fix** (two edits, both proposal-only text; no implementation):
- `3f:31`: replace "and SK-V17 is the converged skinny contract proving
  CSS-on-tape / lazy-`ValueRef` / shared-NEON >SOTA" with "and SK-V17 is the
  converged skinny **contract for** CSS-on-tape / lazy-`ValueRef` / shared-NEON
  — CSS the SK-V17 first-mover, the **CSS >SOTA bar UNMEASURED-PENDING and held
  as the SK-V18 proof obligation** (`restart/skinny/tranches/sk-v17/HANDOFF.md:44`-`45`,
  `SPEC.md:207`); the JSON model is >SOTA-proven (`skinny/RESULTS.md`)".
- `3f:84` (HANDOFF carrier): replace "SK-V17 (the SKINNY tape-fold proof of
  CSS-on-tape / lazy-`ValueRef` / shared-NEON >SOTA) S-P3 CONVERGED" with
  "SK-V17 (the SKINNY tape-fold **contract** for CSS-on-tape / lazy-`ValueRef` /
  shared-NEON; **JSON >SOTA-proven, CSS >SOTA the SK-V18 proof obligation, bar
  not yet met**) S-P3 CONVERGED".

This aligns 3F with the SK-V17 HANDOFF, 3D's banner, and 3E P5b. No other
artefact carries the over-claim (3A/3D/3E frame CSS as first-mover /
by-exercise / SK-V18-pending throughout).

## Overfit-prune findings: NONE beyond R1

- **16-lock count**: preserved, verified verbatim (`grep -cE` = 16); ADD/RETIRE
  G-Omega-gated; the addendum is the gate object, not an in-place edit; the diff
  applies clean. No silent renumber. ACCEPT.
- **5-shape canon**: `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}` verbatim; tape disposed as a substrate-manifest CATEGORY, not
  a silent 6th shape, on two independent grounds. ACCEPT.
- **Contrivance scan**: every delta answers a T-P1 divergence or T-P2 LAC with
  verified path:line; D04's two-ground refutation and the StructRegistry fence
  are principled, not bolted-on. The `udot`/i8mm orphan kernel is correctly
  REFUTED and kept in the deferred appendix. No contrivance.
- **Genuine generality**: §3E proves it with cell-level by-construction tagging,
  a live-baseline monotonic onboarding test, and the P6 value-axis firewall. The
  fold is general (alphabet-as-data, grammar-parametric `ValueRef<G>`), not
  JSON+CSS overfit. No fleet-wide over-claim survives.
- **No fabricated speed claim — except R1**: the JSON numbers (118×,
  28-65×/983×/10583×, 8349 Mbps) are all cited to SPEC/RESULTS and re-verified.
  The single over-stated claim is 3F's "proving CSS … >SOTA" (R1) — CSS >SOTA is
  an unmet SK-V18 obligation, not a proof.
- **lightningcss fair bar**: held fair for JSON via >SOTA + same-plane
  RESULTS rows; for CSS the bar is the strict same-run full-CSSOM comparator
  (`SPEC.md:122`,`:253`) and is correctly UNMEASURED-PENDING everywhere EXCEPT
  3F's two over-stated lines (R1). Fix R1 and the bar is held fair across all
  six artefacts.

## Open question (tagged)

| lens | question | receiver | gate |
|---|---|---|---|
| CH7/CH6 | After R1's fix, the cold-start HANDOFF carrier (`3f:108`-`137`) becomes the only G3 landing surface; should it carry a one-line "CSS >SOTA = SK-V18 obligation, NOT met" stamp adjacent to the SK-V18 dispatch line, mirroring 3D's banner, so no cold-start agent reads CSS >SOTA as achieved? | 3F author (V3) | CH6 anti-paper-close re-scan of the V3 HANDOFF carrier |

## Verdict

**ACCEPT-WITH-ONE-REVISE.** 12 ACCEPT, 1 REVISE, 0 REJECT (92.3% ACCEPT). The
16-lock count and 5-shape canon are preserved and verified verbatim; the diff
applies clean; the fold is genuinely general (not overfit) with cell-level
by-construction scoping and a live-baseline onboarding test; the JSON SOTA bar is
held fair and every JSON number re-verified. Both V1 CH7 items (R1 tally,
scope-honesty banner) are folded. The single new REVISE is an over-stated CSS
speed framing in 3F's executive summary + HANDOFF carrier — SK-V17 is the
converged CSS-on-tape *contract*, not a CSS >SOTA *proof*; the CSS >SOTA bar is
explicitly NOT met (UNMEASURED-PENDING) and is the SK-V18 proof obligation. The
fix is two proposal-only text edits, no candidate dropped, no lock touched, and
restores coherence with 3D's banner, 3E P5b, and 3F's own next-cycle text.

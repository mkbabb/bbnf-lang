---
lens: CH4 COST
pass: T-P3-synthesis
cycle: V3
reviewer: CH4 (V3)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac
subject: restart/audit/totality/sk-v17/p3/{3a..3f}.md + 3c-locks-v+1-diff.md
scope: PASS-3-SYNTHESIS §3 CH4 — every delta states LOC budget + propagation cost + risk class + wave alignment; 3B NEW SK-V18 adoption waves carry same-wave consumer; 3C dispositions realistic
dispositions: { accept: 33, revise: 1, reject: 0 }
verdict: PASS-with-REVISE (1 REVISE / 34 dispositions = 2.9%); ≥95% ACCEPT
prior_cycle_revises_folded: [CH4-V2-01]
orphan_revises: 0
---

# CH4 COST — T-P3 SK-V17 cycle V3

## Lens charge

CH4 scans T-P3's output for cost honesty: every proposed delta must state a LOC
budget, a propagation count (how many surfaces it touches), a risk class, and a
wave alignment; every NEW 3B SK-V18 adoption wave must carry a same-wave consumer
(no orphan pre-gate); and 3C's dispositions must be realistic — no DEFER that
launders wave overflow into challenge time, no ACCEPT whose cost the wave cannot
absorb. Artefacts reviewed: 3A (8 deltas), 3B (9 deltas + 7 SK-V18 receiver rows
W0-W6), 3C (5 clauses / 14 candidate dispositions + the v+1 diff), 3D (8 deltas),
3E (9 deltas), 3F (8 deltas). Dispositionable cost-units: 34 (the 6 delta sets'
cost posture + the 3B wave-consumer set + the 3C disposition realism) — same
accounting as V1/V2 for comparability.

## V2 → V3 fold audit (the CH4 charge against this cycle)

The single V2 CH4 REVISE is folded against a tree-verified figure:

| V2 finding | required fix | V3 state | verified |
|---|---|---|---|
| **CH4-V2-01** (3b:140): the MP.SK18.W1 receiver row priced the eager-`OpenFrame` retirement propagation as "22+ files" — the V1 CH4-01 "22+→40" correction had been applied to 3A but not propagated to 3B's parallel W1 wave row (incomplete-fold residual). | Replace "22+ files" with "40 files (`grep -rl JsonStructBuilder\|CssStructBuilder crates/`=40)" at the W1 row; LOC band 300-700 + HIGH risk unaffected. | **FOLDED in V3.** The W1 row (`3b-master-plan-reconciliation.md:145`) now reads "blast radius = 40 files via `grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/` (=40, master HEAD 2a76916ac; the 300-700 LOC band is the fold-edit envelope, not the touched-file count); HIGH". The fold-note (`3b:27`, `3b:62`, `3b:74`) records the correction; the lone surviving "22+" string at `3b:61` is the historical fold-note describing the prior error, not a live cost cell. | `grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/` = **40** (root totality tree, re-confirmed HEAD 2a76916ac). The 300-700 LOC band and HIGH risk are unchanged. |

**Orphan REVISE count: 0.** Every prior CH4 disposition (V1: CH4-01/02/03; V2:
CH4-V2-01) is closed. The two CH4-tagged OQs carried from V1/V2 (the D07 600-1400
wiring-envelope bound; the D01 40-file single-wave fit) remain CH4-tagged OQs with
named receiver+gate — one of them (the D07 envelope) is now tightenable against the
LOCKED source's `sink_only=270 LOC` lowerer-body precedent, which is the one new V3
finding below.

## V3 findings

### CH4-V3-01 — D07's 600-1400 "wiring" envelope omits a named scaffold→real-body cost band that the LOCKED source now sizes

**REVISE.** D07 (BackendShape selector wiring) prices the cost at "60-200 LOC
selector + 600-1400 LOC joint decision-engine wiring envelope" and explicitly,
correctly, scopes the band to the WIRING — `restart/audit/totality/sk-v17/p3/3b-master-plan-reconciliation.md:168`
("The 600-1400 LOC envelope sizes the WIRING, not the engine"),
`restart/audit/totality/sk-v17/p3/3a-architecture-synthesis.md:88,:101,:118`. The
scaffold-body risk is disclosed as a CH4-tagged OQ
(`3a-architecture-synthesis.md:128`: "Is the D07 decision-engine wiring envelope
(600-1400 LOC) bounded if the 4 skinny lowerers (17-LOC scaffolds) require real
per-shape lowering rather than wiring the existing engine?"). This disclosure is
honest and the non-fit route ("intrinsic-block or G-Omega wave amendment, no
overflow wave") is correct.

The residual is a precision gap, not a framing dishonesty. The LOCKED T-P2 source
the delta cites — `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:440`-`442` —
now enumerates the lowerer set with a real sizing precedent: "The 4 skinny lowerers
are 17-LOC scaffolds (`{eager_tape,offset_tape,event_tape,collapsed_stage}.rs` = 17
each, sink_only.rs = 270)". Tree-verified at master HEAD 2a76916ac:
`wc -l skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs`
= **17 each**; `sink_only.rs` = **270**. The 5-shape canon therefore carries FIVE
lowerers — four 17-LOC scaffolds plus the one real `sink_only` 270-LOC body — and
the all-five gate (the W5 consumer "the 5 real lowerers", `3b:149,:182`) is closed
only when all five are real lowerers. `sink_only`=270 is the standing precedent
for the LOC weight of ONE real shape lowerer; the four scaffolds, if they need real
per-shape bodies, are ~4×270 ≈ **1080 LOC** of NEW body code that the
"wiring-only" 600-1400 band by construction excludes. A reader of the D07 cost row
(`3b:182`, `3a:118`: "60-200 + 600-1400") reads the band as the D07 total; the
scaffold-body cost sits only in the OQ prose, not the cost cell. Because the LOCKED
source now hands T-P3 the per-lowerer precedent (270 LOC), the OQ can be tightened
from an open-ended "the envelope grows" into a bounded named band, which is the
cost-honesty CH4 demands once the sizing fact exists.

**Fix.** At the D07 cost row (`3b-master-plan-reconciliation.md:182` and the D07
rationale `:168`; mirror at `3a-architecture-synthesis.md:118` and `:88`) add a
third, named line-item to the D07 envelope: "+ scaffold→body band: if the W8/W9
all-five gate requires real per-shape bodies for the 4 17-LOC scaffolds
(`{eager_tape,offset_tape,event_tape,collapsed_stage}.rs`), bound by the
`sink_only.rs`=270 LOC precedent at ~4×270 ≈ 800-1100 LOC, SK-V18-measured before
sizing". Keep the OQ but re-anchor it to the bounded band rather than the
open-ended "grows". The 60-200 selector + 600-1400 wiring bands are unchanged; this
adds the previously-undisclosed-as-a-band scaffold-body cost so the D07 row's total
envelope is read-off-able without descending into the OQ prose. Non-fit route is
unchanged (intrinsic-block / G-Omega amendment; no overflow wave).

## ACCEPT findings (cost discipline confirmed)

### 3A — every delta carries the four CH4 dimensions
**ACCEPT.** 3A's Proposed Delta Table embeds a "cost / propagation" column on every
row (`3a:82`-89), the Consequences matrix (`3a:95`-102) carries LOC/risk/propagation,
and the dedicated CH4 Coverage Matrix (`3a:104`-119) carries LOC | propagation count
| risk | wave alignment | consumer/gate | hard-cap fit | fail action for all 8
deltas. The 0-LOC deltas (D04 canon, D05 fence, D08 ORQ-note) are honestly priced
at 0 with the risk borne by the *violation* (D05: "CRITICAL if violated"), not the
text. D01's Consequences row reads "blast radius = 40 files" (`3a:95`, CH4-01/CH4-V2-01
fully folded). D06's two-path pricing (path-(a) 960-site vs path-(b) "sizable core
materialisation, NOT ~0 LOC", `3a:87`,`:102`) correctly refuses to under-price the
side-table route; `grep LayoutFacts crates/`=0 verified. The only blemish is the
D07 scaffold-body band (CH4-V3-01); the D07 *wiring* framing itself is honest.

### 3B — NEW SK-V18 waves carry same-wave consumer (the core CH4 charge)
**ACCEPT (with CH4-V3-01 REVISE on the D07 scaffold-body band only).** The primary
CH4 charge is discharged: the SK-V18 Fold Receiver Block (`3b:139`-149) carries a
same-wave consumer/gate on all 7 rows (W0-W6), including the pre-gate W0 — the row
most at risk of being an orphan. W0 (`3b:139`): "Co-waved W1 tape-wiring IS the
consumer; classification GATES the wiring" — the correct construction (the pre-gate
is the gating predicate of a co-waved consumer, not free-standing). W1 (`3b:145`,
"40 files" folded) → flat-tape `push_plain_offset` consumer; W2 → W1
commit-by-construction consumes the SoA encoding; W3 → JSON `value_from_ref`
byte-equal re-emission gate; W4 → the W3 generator IS the consumer (resolves layout
ONCE at codegen); W5 → F5 tape consumer + F8 the 5 real lowerers (SK-V15 W8/W9
all-five gate); W6 → regen of 8 parsers. Each row carries LOC | risk | MASTER
alignment | cap-fit/fail-route. The Consequences matrix (`3b:172`-182) repeats the
full CH4 column set for all 9 deltas; non-fit routes are "intrinsic block,
REDRESS/revert, or G-Omega amendment" — no overflow wave, no engineered-defer. The
D07 engine-crate citation is correct (`crates/egraph` 1885 + `crates/csp-solver`
5882; skinny `backend_egraph` 311 + `decision_csp` 273 — CH4-02 folded, re-verified).

### 3C — dispositions realistic, costs proposal-only and honest
**ACCEPT.** 3C's Per-Clause Cost Matrix (`3c-locks-crystallisation.md:117`-125)
prices the 5 clauses (L01/L02/L10/L14/L16) at 3-8 doc LOC each with risk |
affected-SK-V18-waves | consumer/gate | propagation count | hard-cap fit | fail
action — the correct posture for a documentation-only proposal. Disposition realism
is sound: 9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0 DEFER (14 total,
`3c:154`-156). The 9 straight ACCEPTs each fold a LOCKED §3Z design or its T-P1
antecedent; none commits a cost the named SK-V18 wave cannot absorb (fence-class
deltas at 0 LOC). The 2 MODIFYs (LAC-2F-FOLD-05, LAC-1E-SKV17-04, `3c:139`,`:143`)
correctly *decline to choose* path-(a)/path-(b) inside a lock, recording both priced
paths (path-a 960-site rename; path-b non-zero side-table) and barring Lock-2
closure by `LayoutFacts` alone — the realistic disposition, since picking a 960-site
rename vs a non-zero side-table inside a lock is a cost-commitment a lock may not
make. The 3 ORQ-ACCEPTs (`3c:146`-148) are crystallised, each naming
receiver+blocker+gate, not deferred; 2F-FOLD-U3's receiver is the EXISTING 5-shape
`BackendShape` gate + the G-Omega 6th-shape path with the 2E-source wave as the
*blocker precondition for a future ADD*, not a phantom receiver. **0 DEFER** is
realistic: no candidate has an absent receiver, so no DEFER is owed; no silent drop
launders cost forward. Verified cost facts: `StructLayout`=**960**,
`backend_shape\|LayoutFacts`=**0** in `crates/` (path-(b) core realisation
genuinely non-zero), `compound_kind_for_layout` UNIQUE caller `arena.rs:47` (defn
`struct.rs:388` + sole caller `arena.rs:47`).

### 3C v+1 diff — the G3 gate object is cost-clean and applies
**ACCEPT.** The `3c-locks-v+1-diff.md` hunk header `@@ -606,7 +606,22 @@` is
arithmetically consistent and the anchor is tree-accurate: the SK-V15 Lock-16
addendum clause ends `LOCKS.md:607`, blanks `:608`-609, `## v+1 Governance
Boundary` at `:610` (re-verified). Hunk arithmetic independently recomputed:
context=7, additions(+)=15, total_new = 7+15 = 22 — balances the `@@ -606,7
+606,22 @@` header. `git apply --check` returns EXIT 0 against the current
`LOCKS.md` at HEAD 2a76916ac. The 5 clauses add no numbered lock, retire none,
renumber none — the 16-lock count (lock anchors `:75,:160,:170,:179,:181,:183,:200,
:202,:260,:269,:319,:328,:336,:349,:436,:453`) and the 5-shape canon
`{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` (`:107`-108) are
preserved verbatim (invariant-checked `3c-diff:100`-103, including the CH5-V2-R01
distribution-invariant bullet now present at `:103`). The gate object commits zero
wave LOC (≤22 added lines) and defers all implementation to G-Omega-gated SK-V18
waves with named consumers — the correct cost containment.

### 3D — full CH4 coverage, costs correctly attributed to SK-V18 not T-P3
**ACCEPT.** 3D's Consequences And CH4 Coverage matrix (`3d:129`-139) prices all 8
deltas with the crucial honesty that each delta's *own* T-P3 cost is 0 doc ("D01
itself 0 doc; the 200-700 LOC is SK-V18, not T-P3", `3d:133`) while the inherited
SK-V18 band, propagation count, risk, wave, consumer (each carries "same-wave"),
and fail action are stated. The fence-class deltas (D04/D05) are 0-LOC with
regression-class risk on violation (D05: 28-65×/983×/10583× class). Non-fit routes
to "parity-prove transiently then converge — never ship a parallel substrate," the
realistic disposition.

### 3E — per-delta CH4 matrix present, costs inherit named T-P2 carriers
**ACCEPT.** The 9-row per-delta CH4 matrix (`3e:269`-278) carries doc LOC |
propagation | risk | wave alignment | consumer/gate | fail action for each
`3E17-D01..D09`, each priced 0-impl-LOC + a small doc band, with the inherited
LOCKED T-P2 carrier band named (D01 inherits ARCH-3A-S17-D02 300-700; D03 inherits
200-700 tape band; D09 inherits the 28-65×/983×/10583× regression class). The
construction is honest: 3E owns 0 implementation LOC. The V1-orphan CH5-V1-R04
(D08 leak-census receiver) is folded: the 7 `strategy.rs` sites now route to
MP.SK18.W3 (the `ValueRef<G>` generator) OR carry an admitted catalogued non-zero
Lock-14 baseline with a re-entry trigger (`3e:278`) — a realistic cost disposition,
not a phantom "HEAD → 0".

### 3F — full consequences matrix with cost/LOC/risk/propagation/wave/fail
**ACCEPT.** 3F's Consequences matrix (`3f:152`-160) carries cost / LOC budget / risk
class | propagation | wave alignment | fail action for all 8 MH deltas. The doc-LOC
budgets (15-160 LOC) are realistic for migration/handoff receiver text. The 960-site
rename row (MH-04, `3f:70`,`:156`) explicitly corrects the mispricing ("Price it as
the 960-site surface, not 40-120 LOC") and budgets the rename as a generator+regen
surface — exactly the cost-honesty CH4 demands; the doc ROW itself is 20-35 LOC, the
rename is the 960-site generator-side surface routed through clean-regen
(`git diff --exit-code` gate). The 960-site single-wave-vs-split realism is carried
CH4-tagged (`3f:214`).

## CH4-specific cross-checks

1. **No delta lacks a wave alignment.** All 8 (3A) + 9 (3B) + 5 (3C clauses) + 8
   (3D) + 9 (3E) + 8 (3F) deltas name a wave or a Pass-Omega/G-Omega gate. ACCEPT.
2. **No 3B NEW SK-V18 wave is an orphan pre-gate.** W0 (the only pre-gate wave) is
   gated to a co-waved W1 consumer; classification GATES the wiring. ACCEPT (primary
   charge satisfied).
3. **No 3C disposition launders overflow.** 0 DEFER, 0 REJECT; the 2 MODIFYs and 3
   ORQ-ACCEPTs each carry a wave-bound route. ACCEPT.
4. **Cost facts re-verified against the tree (HEAD 2a76916ac):** 40 eager-builder
   files (root `crates/`) / 960 StructLayout / 0 backend_shape\|LayoutFacts / 1885
   egraph + 5882 csp-solver / 311 skinny backend_egraph + 273 decision_csp / 4×17-LOC
   scaffold lowerers + sink_only=270 (the fifth, real, lowerer) / 817 css-builder /
   231 json-builder / arena.rs:47 unique `compound_kind_for_layout` caller — all
   confirmed.
5. **The one residual figure is the D07 scaffold-body band** (CH4-V3-01) — the
   600-1400 "wiring" envelope is honestly scoped but the 4 scaffold→real-body cost
   (~4×270 ≈ 800-1100 LOC by the now-LOCKED `sink_only`=270 precedent) sits only in
   the OQ prose, not as a named band in the D07 cost cell, despite the source now
   supplying the per-lowerer sizing precedent.

## Verdict

**PASS-with-REVISE.** 33 ACCEPT, 1 REVISE, 0 REJECT (97.1% ACCEPT). Cost discipline
is sound: every delta states LOC + propagation + risk + wave across all six
artefacts; the 3B SK-V18 receiver waves W0-W6 each carry a same-wave consumer (W0
pre-gate correctly gated to its co-waved W1 consumer); the 3C dispositions are
realistic with no engineered-defer (0 DEFER justified by full-receiver coverage);
the G3 gate object commits zero wave LOC and applies cleanly (`git apply --check`
EXIT 0, hunk arithmetic balances). The single V2 CH4 REVISE (CH4-V2-01) is folded
against the tree-verified 40-file figure, zero orphan REVISE. The single V3 finding
is a cost-precision residual on D07: the "wiring" envelope is honestly framed, but
the LOCKED source now hands T-P3 the `sink_only`=270 LOC per-lowerer precedent that
lets the open-ended "the envelope grows" OQ become a bounded named scaffold-body
band in the D07 cost cell — correctable in V4 by one cost-row addition, disturbing
no existing LOC band, risk class, or wave allocation. No finding re-opens a REDRESS
route, narrows a lock to JSON, or breaks the 5-shape canon.

## Open questions tagged to lenses

| lens | question | receiver | gate |
|---|---|---|---|
| CH4 | Once CH4-V3-01 names the scaffold→body band (~800-1100 LOC), does the combined D07 envelope (60-200 selector + 600-1400 wiring + 800-1100 scaffold-bodies) still fit a single SK-V18 W5 wave, or must W5 split the selector-wiring from the lowerer-body authoring? | SK-V18 W5 owner + 3B wave governance | per-wave LOC cap + the SK-V15 W8/W9 all-five lowerer gate; non-fit is a split, never a dual-substrate transient shipped as closure. |
| CH4 | Does the D01 eager-retirement (40 files) fit a single SK-V18 W1 wave, or does W1 split? | SK-V18 W1 owner + 3B wave governance | per-wave LOC cap + same-wave tape consumer; non-fit is a split, never a dual-substrate transient shipped as closure. |

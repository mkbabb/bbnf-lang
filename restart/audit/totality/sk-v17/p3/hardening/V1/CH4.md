---
lens: CH4 COST
pass: T-P3-synthesis
cycle: V1
reviewer: CH4 (V1)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac
subject: restart/audit/totality/sk-v17/p3/{3a..3f}.md + 3c-locks-v+1-diff.md
scope: PASS-3-SYNTHESIS §3 CH4 — every delta states LOC budget + propagation cost + risk class + wave alignment; 3B NEW waves carry same-wave consumer; 3C dispositions realistic
dispositions: { accept: 31, revise: 3, reject: 0 }
verdict: PASS-with-REVISE (≥30% REVISE not required of a single lens; 3 REVISE / 34 dispositions = 8.8%)
---

# CH4 COST — T-P3 SK-V17 cycle V1

## Lens charge

CH4 scans T-P3's output for cost honesty: every proposed delta must state a LOC
budget, a propagation count (how many surfaces it touches), a risk class, and a
wave alignment; every NEW 3B wave must carry a same-wave consumer (no orphan
pre-gate); and 3C's dispositions must be realistic — no DEFER that launders wave
overflow into challenge time, no ACCEPT whose cost the wave cannot absorb. The
artefacts reviewed: 3A (8 deltas), 3B (9 deltas + 7 SK-V18 receiver rows), 3C
(5 clauses / 14 candidate dispositions + the v+1 diff), 3D (8 deltas), 3E (9
deltas), 3F (8 deltas). Total dispositionable cost-units: 34 (the 6 delta sets'
cost-coverage posture + the 3B wave-consumer set + the 3C disposition realism).

## Summary verdict

The cost discipline across this packet is **strong and largely uniform**. Five of
six artefacts (3A/3B/3C/3D/3F) carry an explicit per-delta CH4 matrix with
columns LOC | propagation count | risk | wave alignment | consumer/gate |
hard-cap fit | fail action. The 3B SK-V18 receiver block (MP.SK18.W0..W6) carries
a same-wave consumer on **every** row, including the W0 pre-gate (co-waved W1
tape-wiring is the consumer; the classification GATES the wiring), which directly
satisfies the CH4 "NEW waves carry same-wave consumer / no orphan pre-gate"
charge. 3C's dispositions are realistic: 9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY + 0
REJECT + 0 DEFER, with the two MODIFYs correctly refusing to pick the
implementation route inside a lock, and zero engineered-defers (the three ORQs
are crystallised, each naming receiver+blocker+gate, not deferred).

The verified cost facts hold under spot-check: `StructLayout` = 960 occurrences
in `crates/` (confirmed); `backend_shape`/`LayoutFacts` = 0 in `crates/`
(confirmed — path-(b) core realisation is genuinely non-zero); `arena.rs:47`
coupling exists (`StructRegistry::compound_kind_for_layout`); `css_l4/builder.rs`
= 817 LOC (confirmed); `json/builder.rs` = 231 LOC (confirmed). The v+1 diff
anchor (insert after the SK-V15 addendum's Lock-16 clause at `:607`, before
`## v+1 Governance Boundary` at `:610`) is structurally sound and applies cleanly.

Three REVISE findings, all cost-evidence precision issues, none load-bearing
against the G3 gate object's validity but each correctable in V2.

## Disposition table

| # | target | finding | disposition | fix |
|---|---|---|---|---|
| CH4-01 | 3a:57, consequences 70 | D01 propagation cites "22+ files via grep JsonStructBuilder\|CssStructBuilder"; actual `grep -rl` in `crates/` = **40 files**. The eager-retirement blast radius is ~1.8× the stated count — a material under-statement of D01's HIGH-risk propagation. | **REVISE** | Replace "22+ files" with "40 files (`grep -rl JsonStructBuilder\|CssStructBuilder crates/`=40)" at `3a-architecture-synthesis.md:57` and `:70`. The LOC band (200-700) is unaffected; only the file-count propagation figure is stale. |
| CH4-02 | 3b:147 (D07) | D07 rationale states "Consumes existing `crates/egraph` (311 LOC) + `crates/csp-solver` (273 LOC)". The 311/273 figures are **correct numbers attributed to the wrong crates**: per the cited T-P2 source `2f-fold-gaps.md:433-434`, 311 LOC = `skinny/crates/passes/src/backend_egraph.rs` and 273 LOC = `skinny/crates/passes/src/decision_csp.rs` (both verified). The actual `crates/egraph`=1885 LOC and `crates/csp-solver`=5882 LOC. The "wire, do not build" framing survives, but the cost-evidence label is wrong by an order of magnitude on the wired-target sizing. | **REVISE** | At `3b-master-plan-reconciliation.md:147` rewrite to "Consumes the existing decision-engine crates (`crates/egraph` 1885 LOC + `crates/csp-solver` 5882 LOC); the skinny lowerer scaffolds it wires are `backend_egraph` 311 LOC + `decision_csp` 273 LOC (`2f-fold-gaps.md:433-434`)." The 600-1400 LOC wiring envelope is unchanged — it sizes the WIRING, not the engine — but the citation must name the right artefacts so the OQ-CH4 envelope-bound question resolves against real numbers. |
| CH4-03 | 3e (whole) | 3E is the only artefact lacking a **per-delta** CH4 cost matrix. It carries a prose "Cost" paragraph (correctly: "3E owns 0-LOC of implementation… implementation costs inherit the LOCKED T-P2 carriers") and a prose "Propagation" paragraph (D05 highest-propagation, D01/D02/D03/D09 touch 2 surfaces, D04/D06 touch 2). This is defensible because 3E carries 0 implementation LOC and explicitly inherits T-P2 bands — but it is below the per-delta granularity its five siblings hold, and a reader cannot read per-delta risk/wave/fail-action off 3E. | **REVISE** | Add a compact 9-row CH4 matrix to `3e-grammar-generalisation.md` (delta | doc LOC | propagation count | risk | wave alignment | consumer/gate | fail action) mirroring 3A/3D's shape. Each row is 0 impl LOC + the inherited T-P2 band already named in the prose; this is a formatting completion, not new analysis. |

## ACCEPT findings (cost discipline confirmed)

### 3A — every delta carries the four CH4 dimensions
**ACCEPT.** 3A's Proposed Delta Table embeds a "cost / propagation" column on
every row, and the dedicated "CH4 Coverage Matrix" (`3a:79-94`) carries LOC |
propagation count | risk | wave alignment | consumer/gate | hard-cap fit | fail
action for all 8 deltas. The 0-LOC deltas (D04 canon, D05 fence, D08 ORQ-note)
are honestly priced at 0 with the risk borne by the *violation* (D05: "CRITICAL
if violated"), not the text. D06's two-path pricing (path-(a) 960-site vs path-(b)
"sizable core materialisation, NOT ~0 LOC") is the correct refusal to under-price
the side-table route, and the `grep LayoutFacts crates/`=0 evidence is verified.
The propagation counts (2/2/3/2/1/2/2/2 surfaces) are accurate to the cited
sections. Only CH4-01's file-count figure is stale.

### 3B — NEW waves carry same-wave consumer (the core CH4 charge)
**ACCEPT.** This is CH4's primary charge and 3B discharges it fully. The SK-V18
Fold Receiver Block (`3b:121-129`) carries a "same-wave consumer / gate" column on
all 7 rows:
- **W0** (the pre-gate, the row most at risk of being an orphan): "Co-waved W1
  tape-wiring IS the consumer; classification GATES the wiring." This is the
  correct construction — the pre-gate is not free-standing; it is the gating
  predicate of a co-waved consumer. No orphan.
- W1: "the flat-tape commit (`push_plain_offset`) is the same-wave consumer of the
  retired builders."
- W2: "the W1 commit-by-construction path consumes the converged SoA encoding."
- W3: "JSON `value_from_ref` byte-equal re-emission is the W3 gate consumer."
- W4: "the W3 generator IS the consumer — it resolves the layout ONCE at codegen."
- W5: "F5 consumer = the tape; F8 consumer = the 5 real lowerers (the SK-V15 W8/W9
  all-five gate)."
- W6: "the regen of all 8 parsers is the same-wave consumer of the rename."

Every row also carries LOC | risk | MASTER alignment | cap-fit/fail-route. The
Consequences matrix (`3b:153-163`) repeats LOC | propagation | risk | wave | consumer
| cap-fit | fail action for all 9 deltas. The Open Questions correctly route a
non-fit wave to "row-level intrinsic block, REDRESS/revert, or G-Omega amendment
before redress" — no overflow wave, no engineered-defer. The only blemish is
CH4-02's mislabelled engine-crate citation in D07's rationale.

### 3C — dispositions realistic, costs proposal-only and honest
**ACCEPT.** 3C's Per-Clause Cost Matrix (`3c-locks-crystallisation.md:99-112`)
prices each of the 5 clauses at 2-8 doc LOC with risk | affected-SK-V18-waves |
consumer/gate | propagation count | hard-cap fit | fail action — the correct
posture for a documentation-only proposal. The disposition realism is sound:
- The 9 straight ACCEPTs each fold a LOCKED §3Z design or its T-P1 antecedent;
  none commits cost the SK-V18 wave cannot absorb (the cost is borne by the named
  wave, with the fence-class deltas at 0 LOC).
- The 2 MODIFYs (LAC-2F-FOLD-05, LAC-1E-SKV17-04) correctly *decline to choose*
  the path-(a)/path-(b) route inside the lock, recording both priced paths and
  barring Lock-2 closure by `LayoutFacts` alone — the realistic disposition, since
  picking a 960-site rename vs a non-zero side-table inside a lock would be a
  cost-commitment a lock may not make. The precedent cited (prior-totality
  LAC-1E-V1-04 MODIFY) is apt.
- The 3 ORQ-ACCEPTs are crystallised, not deferred: each names receiver (SK-V18
  W2 / W2 pre-gate / future 2E-strategy wave), blocker (dual-encoding,
  REDRESS-53 parallel index, `admits_collapsed_stage` x86-binding), and gate.
  This satisfies CH6 and keeps CH4 clean of engineered-defer.
- **0 DEFER** is the realistic count here: there is no candidate whose receiver is
  absent, so no DEFER is owed. CH4 confirms no silent drop launders cost forward.

### 3C v+1 diff — the G3 gate object is cost-clean and applies
**ACCEPT.** The `3c-locks-v+1-diff.md` hunk (`@@ -606,6 +606,52 @@`) inserts after
the SK-V15 addendum (verified: SK-V15 addendum ends `:607`, blanks `:608-609`,
`## v+1 Governance Boundary` `:610`). The 5 clauses add no numbered lock, retire
none, renumber none — the 16-lock count and 5-shape canon are preserved verbatim
(invariant-checked at `:84-90`). The cost of the gate object itself is the
addendum text only (≤52 lines); it authorizes no implementation. From a CH4 lens
this is the correct cost containment: the load-bearing G3 object commits zero
wave LOC and defers all implementation cost to G-Omega-gated SK-V18 waves with
named consumers.

### 3D — full CH4 coverage, costs correctly attributed to SK-V18 not T-P3
**ACCEPT.** 3D's CH4 Coverage matrix (`3d:96-105`) prices all 8 deltas with the
crucial honesty that each delta's *own* T-P3 cost is 0 doc ("D01 itself 0 doc;
the 200-700 LOC is SK-V18, not T-P3") while the inherited SK-V18 band, propagation
count, risk, wave, consumer, and fail action are all stated. The "Cost And
Non-Fit Fold" section (`3d:107-113`) correctly routes the 200-700 LOC eager-
retirement non-fit to "parity-prove transiently then converge — never ship a
parallel substrate," which is the realistic non-fit disposition, not an overflow.

### 3F — full consequences matrix with cost/LOC/risk/propagation/wave/fail
**ACCEPT.** 3F's Consequences matrix (`3f:140-149`) carries cost / LOC budget /
risk class | propagation | wave alignment | fail action for all 8 MH deltas. The
doc-LOC budgets (30-160 LOC) are realistic for migration/handoff receiver text,
and the high-risk rows (MH-03 single-encoding gate, MH-05 AZ-IV fence) carry the
correct regression-class risk labels. The 960-site rename row (MH-04) explicitly
corrects the mispricing: "Price it as the 960-site surface, not 40-120 LOC" —
exactly the cost-honesty CH4 demands. Receiver/blocker/gate triple on every
delta row.

## CH4-specific cross-checks

1. **No delta lacks a wave alignment.** All 8 (3A) + 9 (3B) + 5 (3C) + 8 (3D) +
   9 (3E, via inherited bands) + 8 (3F) deltas name a wave or a Pass-Omega/G-Omega
   gate. ACCEPT.
2. **No 3B NEW wave is an orphan pre-gate.** W0 (the only pre-gate wave) is gated
   to a co-waved consumer. ACCEPT (primary charge satisfied).
3. **No 3C disposition launders overflow.** 0 DEFER, 0 REJECT; the 2 MODIFYs and
   3 ORQ-ACCEPTs each carry a wave-bound route. ACCEPT.
4. **Cost facts verified against the tree:** 960 StructLayout / 0 backend_shape /
   817 css-builder / 231 json-builder / arena.rs:47 coupling — all confirmed. The
   only stale figures are 3A's "22+ files" (actually 40, CH4-01) and 3B's
   engine-crate mislabel (CH4-02).

## Verdict

**PASS-with-REVISE.** 31 ACCEPT, 3 REVISE, 0 REJECT. The cost discipline is
sound: every delta states LOC + propagation + risk + wave; the 3B SK-V18 receiver
waves each carry a same-wave consumer (W0 pre-gate correctly gated to its co-waved
consumer); the 3C dispositions are realistic with no engineered-defer and the G3
gate object commits zero wave LOC. The three REVISE findings are cost-evidence
precision corrections (a stale file count, a crate-citation mislabel, and one
missing per-delta matrix), all correctable in V2 without disturbing any LOC band
or wave allocation. None re-opens a REDRESS route, narrows a lock to JSON, or
breaks the 5-shape canon.

## Open questions tagged to lenses

| lens | question | receiver | gate |
|---|---|---|---|
| CH4 | Is D07's 600-1400 LOC wiring envelope bounded if the 4 skinny lowerers (311+273 LOC scaffolds) need real per-shape bodies rather than wiring the existing engine? Resolving this requires the corrected crate-vs-scaffold citation (CH4-02). | 3B wave-map owner + SK-V18 lowerer owners | 3B cost matrix + 5-shape lowerer gate; non-fit intrinsic-blocked, no overflow wave. |
| CH4 | Does the D01 eager-retirement (40 files, not 22+) still fit a single SK-V18 W1 wave, or split W1? | SK-V18 W1 owner + 3B wave governance | per-wave LOC cap + same-wave tape consumer; non-fit is a split, never a dual-substrate transient shipped as closure. |
| CH1/CH4 | Should the 3E per-delta CH4 matrix (CH4-03) be added in V2, or is the prose Cost paragraph sufficient given 3E owns 0 impl LOC? | 3E author + aggregator | CH4 per-delta granularity parity with 3A/3B/3D/3F. |

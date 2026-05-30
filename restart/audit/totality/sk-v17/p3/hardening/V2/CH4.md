---
lens: CH4 COST
pass: T-P3-synthesis
cycle: V2
reviewer: CH4 (V2)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac
subject: restart/audit/totality/sk-v17/p3/{3a..3f}.md + 3c-locks-v+1-diff.md
scope: PASS-3-SYNTHESIS §3 CH4 — every delta states LOC budget + propagation cost + risk class + wave alignment; 3B NEW SK-V18 adoption waves carry same-wave consumer; 3C dispositions realistic
dispositions: { accept: 33, revise: 1, reject: 0 }
verdict: PASS-with-REVISE (1 REVISE / 34 dispositions = 2.9%); ≥95% ACCEPT
prior_cycle_revises_folded: [CH4-01, CH4-02, CH4-03]
orphan_revises: 0
---

# CH4 COST — T-P3 SK-V17 cycle V2

## Lens charge

CH4 scans T-P3's output for cost honesty: every proposed delta must state a LOC
budget, a propagation count (how many surfaces it touches), a risk class, and a
wave alignment; every NEW 3B SK-V18 adoption wave must carry a same-wave consumer
(no orphan pre-gate); and 3C's dispositions must be realistic — no DEFER that
launders wave overflow into challenge time, no ACCEPT whose cost the wave cannot
absorb. Artefacts reviewed: 3A (8 deltas), 3B (9 deltas + 7 SK-V18 receiver rows),
3C (5 clauses / 14 candidate dispositions + the v+1 diff), 3D (8 deltas), 3E (9
deltas), 3F (8 deltas). Dispositionable cost-units: 34 (the 6 delta sets' cost
posture + the 3B wave-consumer set + the 3C disposition realism) — same accounting
as V1 for comparability.

## V1 → V2 fold audit (the CH4 charge against this cycle)

All three V1 CH4 REVISEs are folded, each against a tree-verified figure:

| V1 finding | required fix | V2 state | verified |
|---|---|---|---|
| **CH4-01** (3a:57,:70): D01 "22+ files" understated; actual `grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/`=40. | Replace with "40 files"; LOC band 200-700 unaffected. | FOLDED in 3A: D01 live rows now read "blast radius = 40 files" (`3a:75`); the "22+" string survives ONLY in the historical fold-note (`3a:56`) describing the prior error. | `grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/`=**40** (re-confirmed, HEAD 2a76916ac). |
| **CH4-02** (3b:147 D07): 311/273 LOC mis-attributed to `crates/egraph`+`crates/csp-solver`; actual wired-target sizes 1885/5882; 311/273 are skinny scaffolds. | Rewrite D07 to name `crates/egraph` 1885 / `crates/csp-solver` 5882 and re-attribute 311/273 to `backend_egraph.rs`/`decision_csp.rs`. | FOLDED in 3B: D07 row (`3b:163`) + frontmatter (`3b:26`) + exec-summary (`3b:58`) now carry the corrected split; 600-1400 envelope unchanged (sizes the WIRING). | `find crates/{egraph,csp-solver}/src -name '*.rs' | xargs wc -l`: egraph=**1885**, csp-solver=**5882**; skinny `backend_egraph.rs`=**311**, `decision_csp.rs`=**273** (all re-confirmed). |
| **CH4-03** (3e whole): 3E lacked a per-delta CH4 cost matrix. | Add a 9-row matrix (doc LOC \| propagation \| risk \| wave \| consumer/gate \| fail action). | FOLDED in 3E: 9-row per-delta matrix added (`3e:235`-244), one row per `3E17-D01..D09`, each 0-impl-LOC + inherited T-P2 band + propagation/risk/wave/consumer/fail. | Matrix present and mirrors 3A/3D shape; verified by read. |

**Orphan REVISE count: 0.** Every V1 CH4 disposition is closed in V2. The two V1
open questions CH4 carried (D07 envelope bound; D01 40-files single-wave fit)
remain CH4-tagged OQs with named receiver+gate, not unresolved REVISEs.

## V2 findings

### CH4-V2-01 — 3B MP.SK18.W1 receiver row carries the stale "22+ files" figure CH4-01 corrected

**REVISE.** The V1 CH4-01 fold corrected 3A's eager-OpenFrame-retirement
file-count from "22+" to 40, but the SAME figure survives uncorrected in 3B's
parallel wave row. `restart/audit/totality/sk-v17/p3/3b-master-plan-reconciliation.md:140`
(MP.SK18.W1 "Eager-OpenFrame retire → flat-tape commit-by-construction", F1 /
LAC-2F-FOLD-01) prices the propagation as "300-700 generator-side + per-grammar
regen ×8; **22+ files**; HIGH". This is the identical eager-OpenFrame retirement
F-candidate D01/3A describes, so the file-count must match: actual
`grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/` = **40** (re-verified, HEAD
2a76916ac). The fold of CH4-01 was applied to 3A but not propagated to 3B's W1 row
— an incomplete-fold residual. The understatement is material on a HIGH-risk
eager-retirement wave: 40 is ~1.8× the stated 22+, and 3B is the wave-allocation
surface where the blast radius governs whether W1 fits a single SK-V18 wave or must
split (the exact question CH4's own carried OQ raises).

**Fix.** At `3b-master-plan-reconciliation.md:140` replace "22+ files" with
"40 files (`grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/`=40)". The
300-700 LOC band and HIGH risk are unaffected; only the touched-file propagation
figure is stale. Cross-check 3B has no other "22+" residual: the only other 3B
occurrence is in the historical fold-note context, none in a live cost cell.

## ACCEPT findings (cost discipline confirmed)

### 3A — every delta carries the four CH4 dimensions
**ACCEPT.** 3A's Proposed Delta Table embeds a "cost / propagation" column on every
row (`3a:62`-69) and the dedicated CH4 Coverage Matrix (`3a:90`-99) carries LOC \|
propagation count \| risk \| wave alignment \| consumer/gate \| hard-cap fit \|
fail action for all 8 deltas. The 0-LOC deltas (D04 canon, D05 fence, D08 ORQ-note)
are honestly priced at 0 with risk borne by the *violation* (D05: "CRITICAL if
violated"), not the text. D01's live rows now read "40 files" (CH4-01 folded). The
propagation counts (2/2/3/2/1/2/2/2) resolve to the cited sections. The D07
decision-engine wiring envelope (60-200 selector + 600-1400 joint wiring) is
honestly framed as a WIRE of the existing engine, and the residual risk —
that the 4 skinny lowerers are 17-LOC scaffolds needing real bodies — is carried as
a named CH4 OQ (`3a:108`) with receiver (3B wave-map + SK-V18 lowerer owners) and
gate (3B cost matrix + 5-shape lowerer gate; non-fit→intrinsic-block). Verified:
the 4 lowerers (`skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs`)
are genuinely 17 LOC each — the OQ is real and correctly tagged, not under-stated.

### 3B — NEW SK-V18 waves carry same-wave consumer (the core CH4 charge)
**ACCEPT (with CH4-V2-01 REVISE on the W1 file-count only).** The primary CH4 charge
is discharged: the SK-V18 Fold Receiver Block (`3b:137`-145) carries a same-wave
consumer/gate on all 7 rows, including the pre-gate W0 — the row most at risk of
being an orphan. W0 (`3b:139`) reads "Co-waved W1 tape-wiring IS the consumer;
classification GATES the wiring", the correct construction: the pre-gate is not
free-standing, it is the gating predicate of a co-waved consumer. W1→push_plain_offset
consumer; W2→W1 commit-by-construction consumes the SoA encoding; W3→JSON
value_from_ref byte-equal re-emission gate; W4→the W3 generator IS the consumer;
W5→F5 tape + F8 the 5 real lowerers (SK-V15 W8/W9 all-five gate); W6→regen of 8
parsers. Each row carries LOC \| risk \| MASTER alignment \| cap-fit/fail-route, and
the Consequences matrix (`3b:170`-179) repeats the full CH4 column set for all 9
deltas. Non-fit routes are "intrinsic block, REDRESS/revert, or G-Omega amendment"
— no overflow wave, no engineered-defer. The ONLY blemish is the stale W1 file-count
(CH4-V2-01).

### 3C — dispositions realistic, costs proposal-only and honest
**ACCEPT.** 3C's Per-Clause Cost Matrix (`3c-locks-crystallisation.md:107`-113)
prices the 5 clauses at 2-8 doc LOC each with risk \| affected-SK-V18-waves \|
consumer/gate \| propagation count \| hard-cap fit \| fail action — the correct
posture for a documentation-only proposal. Disposition realism is sound: 9 ACCEPT +
3 ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0 DEFER. The 9 straight ACCEPTs each fold a
LOCKED §3Z design or its T-P1 antecedent; none commits a cost the named SK-V18 wave
cannot absorb (fence-class deltas at 0 LOC). The 2 MODIFYs (LAC-2F-FOLD-05,
LAC-1E-SKV17-04) correctly *decline to choose* path-(a)/path-(b) inside a lock,
recording both priced paths (path-a 960-site rename; path-b non-zero side-table)
and barring Lock-2 closure by `LayoutFacts` alone — the realistic disposition, since
picking a 960-site rename vs a non-zero side-table inside a lock is a cost-commitment
a lock may not make. The 3 ORQ-ACCEPTs are crystallised, each naming
receiver+blocker+gate (`3c:136`-138), not deferred. **0 DEFER** is realistic here:
no candidate has an absent receiver, so no DEFER is owed; no silent drop launders
cost forward. Verified cost facts: `StructLayout`=**960**,
`backend_shape`/`LayoutFacts`=**0** in `crates/` (path-(b) core realisation is
genuinely non-zero), `arena.rs:47` coupling exists and is the UNIQUE
`compound_kind_for_layout` caller (`grep -rn compound_kind_for_layout crates/` =
`struct.rs:388` defn + `arena.rs:47` caller — re-confirmed).

### 3C v+1 diff — the G3 gate object is cost-clean and applies
**ACCEPT.** The `3c-locks-v+1-diff.md` hunk header `@@ -606,7 +606,22 @@` is
arithmetically consistent and the anchor is tree-accurate: SK-V15 addendum Lock-16
clause at `LOCKS.md:607`, blanks `:608`-609, `## v+1 Governance Boundary` at `:610`
(re-verified). Hunk arithmetic: 7 context lines (old count=7); 7 context + 15
content additions = 22 (new count=22) — the `+++ b/...` header line is the 16th
`+`-prefixed line and is not a content add, so the count balances. The 5 clauses add
no numbered lock, retire none, renumber none — 16-lock count and 5-shape canon
preserved verbatim (invariant-checked `3c-diff:84`-90). The gate object commits zero
wave LOC (≤22 added lines) and defers all implementation to G-Omega-gated SK-V18
waves with named consumers. From a CH4 lens this is the correct cost containment.

### 3D — full CH4 coverage, costs correctly attributed to SK-V18 not T-P3
**ACCEPT.** 3D's Consequences And CH4 Coverage matrix (`3d:117`-127) prices all 8
deltas with the crucial honesty that each delta's *own* T-P3 cost is 0 doc ("D01
itself 0 doc; the 200-700 LOC is SK-V18, not T-P3") while the inherited SK-V18 band,
propagation count, risk, wave, consumer (each carries "same-wave"), and fail action
are stated. The fence-class deltas (D04/D05) are 0-LOC with regression-class risk on
violation. Non-fit routes to "parity-prove transiently then converge — never ship a
parallel substrate", the realistic disposition.

### 3E — per-delta CH4 matrix now present (CH4-03 folded)
**ACCEPT.** The 9-row matrix (`3e:235`-244) is added per CH4-03, one row per
`3E17-D01..D09`, each priced 0-impl-LOC + a small doc band + the inherited LOCKED
T-P2 carrier band, with propagation count / risk / wave / consumer/gate / fail
action. The construction is honest: 3E owns 0 implementation LOC and the rows
inherit the named T-P2 carrier bands (D01 inherits ARCH-3A-S17-D02 300-700; D03
inherits 200-700 tape band; D09 inherits the 28-65×/983×/10583× regression class).
This is the formatting-completion CH4-03 demanded, now at per-delta granularity
parity with 3A/3D/3F.

### 3F — full consequences matrix with cost/LOC/risk/propagation/wave/fail
**ACCEPT.** 3F's Consequences matrix (`3f:141`-149) carries cost / LOC budget / risk
class \| propagation \| wave alignment \| fail action for all 8 MH deltas. The
doc-LOC budgets (20-160 LOC) are realistic for migration/handoff receiver text. The
960-site rename row (MH-04, `3f:66`,`:146`) explicitly corrects the mispricing
("Price it as the 960-site surface, not 40-120 LOC") and budgets the rename as a
generator+regen surface — exactly the cost-honesty CH4 demands; the doc ROW itself
is 20-35 LOC, the rename is the 960-site generator-side surface. Receiver/blocker/gate
triple on every delta. The 960-site realism OQ is carried CH4-tagged (`3f:203`).

## CH4-specific cross-checks

1. **No delta lacks a wave alignment.** All 8 (3A) + 9 (3B) + 5 (3C clauses) + 8
   (3D) + 9 (3E) + 8 (3F) deltas name a wave or a Pass-Omega/G-Omega gate. ACCEPT.
2. **No 3B NEW SK-V18 wave is an orphan pre-gate.** W0 (the only pre-gate wave) is
   gated to a co-waved W1 consumer; classification GATES the wiring. ACCEPT (primary
   charge satisfied).
3. **No 3C disposition launders overflow.** 0 DEFER, 0 REJECT; the 2 MODIFYs and 3
   ORQ-ACCEPTs each carry a wave-bound route. ACCEPT.
4. **Cost facts re-verified against the tree (HEAD 2a76916ac):** 40 eager-builder
   files / 960 StructLayout / 0 backend_shape / 1885 egraph + 5882 csp-solver / 311
   skinny backend_egraph + 273 decision_csp / 4×17-LOC lowerer scaffolds / 817
   css-builder / 231 json-builder / arena.rs:47 unique coupling — all confirmed.
5. **The one stale figure is 3B:140's "22+ files"** (CH4-V2-01) — an incomplete fold
   of the V1 CH4-01 correction into the parallel W1 wave row.

## Verdict

**PASS-with-REVISE.** 33 ACCEPT, 1 REVISE, 0 REJECT (97.1% ACCEPT). Cost discipline
is sound: every delta states LOC + propagation + risk + wave across all six
artefacts; the 3B SK-V18 receiver waves each carry a same-wave consumer (W0 pre-gate
correctly gated to its co-waved W1 consumer); the 3C dispositions are realistic with
no engineered-defer; the G3 gate object commits zero wave LOC and applies cleanly.
All three V1 CH4 REVISEs (CH4-01, CH4-02, CH4-03) are folded against tree-verified
figures, zero orphan REVISE. The single V2 finding is a cost-figure precision
residual: the V1 "22+→40 files" correction was applied to 3A but not propagated to
3B's parallel MP.SK18.W1 wave row — correctable in V3 by one figure edit, disturbing
no LOC band, risk class, or wave allocation. No finding re-opens a REDRESS route,
narrows a lock to JSON, or breaks the 5-shape canon.

## Open questions tagged to lenses

| lens | question | receiver | gate |
|---|---|---|---|
| CH4 | Is D07's 600-1400 LOC wiring envelope bounded if the 4 skinny lowerers (verified 17-LOC scaffolds at `skinny/crates/codegen/src/lower/`) need real per-shape bodies rather than wiring the existing 7767-LOC engine? | 3B wave-map owner + SK-V18 lowerer owners | 3B cost matrix + 5-shape lowerer gate; non-fit intrinsic-blocked, no overflow wave. |
| CH4 | Does the D01 eager-retirement (40 files) fit a single SK-V18 W1 wave once 3B:140 is corrected to 40, or does W1 split? | SK-V18 W1 owner + 3B wave governance | per-wave LOC cap + same-wave tape consumer; non-fit is a split, never a dual-substrate transient shipped as closure. |

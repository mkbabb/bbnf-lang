# CH4 COST — T-P3 SK-V18 Hardening, Cycle V1

Lens: CH4 COST. Every delta must state a LOC budget, a propagation cost
(surfaces touched), a risk class, and a wave alignment; 3B NEW waves must carry a
same-wave consumer; 3C dispositions must be realistic. Cross-scope, uncited,
revived-refuted, and silent-drop deltas are REJECT bait.

Target packet: the 6 June-1 SK-V18 synthesis artefacts under
`restart/audit/totality/p3/` (3A, 3B, 3C-crystallisation, 3C-v+1-diff, 3D, 3E,
3F). This is the FIRST hardening cycle (V1) against the regenerated SK-V18
packet; the May-28 `hardening/V1/CH4.md` it overwrites was the SK-V15 packet's
V1 and is superseded.

## Verdict summary

The packet is structurally strong on CH4 hygiene: every artefact carries an
explicit per-row CH4 coverage matrix (LOC / propagation count / risk / wave /
consumer-gate / hard-cap fit / fail-action), the net ≈−10800 LOC headline traces
to `sk-v18/SPEC.md:21,:61`, the 12-wave ceiling is honoured (no W13/W12
spillover), and 3B's NEW §13.7 waves each carry a same-wave consumer + RED
falsifier. The load-bearing on-disk facts I spot-verified all resolve (below).

But the cycle is NOT clean. Three classes of CH4 defect recur:

1. **A load-bearing artefact does not apply.** The extracted
   `3C-locks-v+1-diff.md` hunk is malformed: the header declares `@@ -622,6
   +622,38 @@` but the body carries 5 old-side / 33 new-side lines (trailing
   context truncated). `git apply --check` returns `corrupt patch at line 38`.
   The prior SK-V15 V5 lock explicitly certified "the extracted 3C diff applies
   cleanly" — the regenerated SK-V18 diff regresses that invariant. This is the
   sharpest finding; the v+1 diff is the LOCKS singularity's only executable
   artefact.

2. **Net-LOC arithmetic drifts between artefacts** (−5460 vs −5500 for P3;
   per-artefact campaign sums of −10660/−10685/−10695 against a −10800
   headline). Defensible under ≈ but a CH4 cost matrix that states budgets must
   be internally consistent or cite the SPEC figure verbatim.

3. **Carried deltas reference dead wave ids** (3A V1-D02..D12 cite W1/W4/W5/W6/
   W7/W8/W9 — the SK-V15 wave numbering — while the cycle re-keys everything to
   P1-P5/G1-G6/PROVE/H1). 3A declares these "retained historical, NOT
   re-dispatched," which spares them a REJECT, but the carried CH4 matrices in
   3A/3E still cost-route those rows to SK-V15 wave ids without the
   historical-only disclaimer the V4 delta-summary applies — leaving live CH4
   wave-alignment cells that point at waves that no longer exist.

The two PLANNED co-gate symbols (`runtime_target_rows_collapsed`,
`bbnf_simd_single_mask_convention`) are honestly disclosed as PLANNED, not cited
live (`rg` both = 0 confirmed) — correct CH4 discipline, not a defect.

## Spot-verification of load-bearing deltas (all resolve unless noted)

| claim | artefact | on-disk result | verdict |
|---|---|---|---|
| `sk-v18/SPEC.md:19-21,:58-61` generalization scope; net −10800 | 3B/3D/3E/3F | SPEC text matches verbatim | RESOLVES |
| `sk-v18/SPEC.md:431-447` 12-wave manifest, per-wave LOC/cap | all | matches; P=30min, G=≤90min wall, exactly 12 waves | RESOLVES |
| `sk-v18/SPEC.md:535-547` binding lattice | 3B/3D/3F | matches (P4-before-G2/G3; G5/G6∥G4; PROVE after G4) | RESOLVES |
| MASTER-PLAN §13.6 = "SK-V18 Tape-Fold Adoption" (the pivot) | 3B SKV18-D01/D02 | `:974` header confirmed; `:1415-1422` "adopt into crates/core" confirmed | RESOLVES — pivot is real |
| `strategy.rs:137-185` 9-grammar idents table | 3C-L13 / 3E-D16 / 3F | 9 `ManifestStrategyEntry` rows confirmed (json,sheets,css_l4,bbnf,csv,math,bnf,ebnf,css_pretty) | RESOLVES |
| Lock-14 self-gate "asserts ZERO, returns 13" | 3C-L13 / 3A-D11 | `rg` on `crates/ir+analysis` = 13; LOCKS:349 asserts ZERO | RESOLVES — gate is RED |
| "4-name regex catches 4 of 9" | 3C-L13 / 3E-D16 | LOCKS:349 regex is 4 names; 5 grammars (csv,math,bnf,ebnf,css_pretty) escape | RESOLVES |
| `CSS_GENERATED_RS: &str = r#"` courier @ runtime_generator.rs:701 | 3A-D05 / 3F-MH-010 | confirmed verbatim const at :701 | RESOLVES |
| 7 css_l4 replicas md5 `b654562c…` | 3D / 3F-MH-011 | all 7 md5-identical `b654562ccff46…`; each 910 LOC | RESOLVES |
| `crates/core/src/css_types.rs` 66 LOC | 3F-MH-013 / 3C-L13 | exists, 66 LOC; named in LOCKS:349 | RESOLVES |
| 5 BackendShape variants @ lower/mod.rs:18-24 | all | exactly 5 confirmed | RESOLVES |
| 16 numbered locks | 3C invariant | bold-numbered headings 1-16 confirmed (7+13 present) | RESOLVES |
| `crates/core/src/runtime` census = 71 | 3C-L13 / 3A-D12 | `find … = 71` confirmed | RESOLVES |
| **extracted 3C v+1 diff applies** | 3C-v+1-diff | `git apply --check` → corrupt patch line 38; header 6/38 vs body 5/33 | **FAILS** |

## Per-delta CH4 disposition

### 3A — ARCHITECTURE (14 SK-V18 deltas; 12 carried-historical)

| delta | CH4 disposition | note |
|---|---|---|
| ARCH-3A-V4-SK18-D01 phantom `<G>` strike | ACCEPT | LOC −1..+5, prop 1, LOW-MED, wave SK-V19/Ω+G4. Clean. |
| D02 named-primitive (a)-(d) | ACCEPT | +20 gate+1 lint, prop 2, LOW, G1∧G2. Consumer = (a)-(d) machine check. |
| D03 un-fork DELETE RuntimeEmitterKind | ACCEPT | ≤450, prop 2, HIGH, G3. Fail-action = DELETE-rebuild, no shim. |
| D04 relocated-seam firewall | ACCEPT | +10..+15, prop 1-2, MED, G2∧G3∧P3. PLANNED co-gate disclosed. |
| D05 verbatim-blob courier | ACCEPT | −910 after provider, prop 2, CRITICAL, G1∧G2, no-delete-before-provider. |
| D06 5-shape positive dispatch axis | ACCEPT | +10, prop 1, LOW, G3. Coherent with 3B/3E. |
| D07 aarch64-ONLY / x86-deleted | ACCEPT | −4500, prop 3, HIGH, P1∧G5/G6. |
| D08 CollapsedStage diagnostic-slot | ACCEPT | ≈0 committed, prop 1, MED-HIGH-conditional, diag/G5/G6. REDRESS-98 fence stated. |
| D09 G6 retarget-not-author | ACCEPT | ≤150+10, prop 1, MED-HIGH, G5/G6. |
| D10 css_balanced forced demotion | ACCEPT | +10, prop 2, LOW, G2∧G6. |
| D11 totality 9-ident + css_types + green-by-exclusion | REVISE | The row CONFLATES one SK-V18-skinny cost (P4 green-by-exclusion, ≈+15) with one SK-V19-totality cost (R16 9-ident row-collapse) under a single "≈ +15 + SK-V19 R16; HIGH" budget and a split wave "P4 (skinny) ∧ SK-V19 (totality R16)". CH4 requires a charged LOC for each wave; the SK-V19 R16 portion is uncosted here (3B prices it "≈+217 reconcile"; 3F "≈+20..+217"). **Correction (3A):** split D11 into D11a (P4 green-by-exclusion, +15, skinny, this cycle) and D11b (SK-V19 R16 row-collapse, ≈+217 per 3B/3F, deferred) so the SK-V19 cost is not laundered into a +15 figure. |
| D12 metalang + Pattern-H 71 baseline | ACCEPT | ≈0 rename + baseline fix, prop 1, MED, P5∧SK-V19. |
| D13 Sheets precedence tower | ACCEPT | ≤450 PROVE, prop 2, MED, PROVE. |
| D14 SK-V18 authority/HANDOFF reconcile | ACCEPT | +40, prop 1, HIGH routing, pre-W0/Ω. |
| V1-D01..D12 (carried) | REVISE (one correction, applies to set) | These are explicitly "retained historical, NOT re-dispatched" in the V4 prose — correct. BUT the carried CH4 coverage matrix (lines 99-110) still presents live wave-alignment cells (W1/W4/W5/W6/W7/W8/W9) and live fail-actions as if dispatch-eligible, without the "historical-only / superseded by SK-V18 P/G waves" marker the V4 summary applies. **Correction (3A):** stamp the carried CH4 matrix rows with the SK-V15-historical / re-keyed-to-SK-V18 disclaimer the V4 delta-summary table carries, so no reader cost-routes a delta to a retired SK-V15 wave id. |

3A: D11 REVISE, carried-V1 matrix REVISE; the other 13 ACCEPT.

### 3B — MASTER-PLAN (10 new SKV18-D01..D10; 4 carried)

Scope-pivot finding on-disk verified (MASTER §13.6 header `:974`; §25
`:1415-1422` "adopt into crates/core"). NEW §13.7 waves each carry a same-wave
consumer + RED falsifier (all 12 §13.7 rows populated) — the 3B-NEW-wave
consumer requirement is MET.

| delta | CH4 disposition | note |
|---|---|---|
| SKV18-D01 re-author SK-V18 identity | ACCEPT | 80-160 doc, prop 3, high routing, Pass Omega. |
| SKV18-D02 re-key §13.6→SK-V19 | ACCEPT | 60-120 doc, prop 1, high routing. Fold rows preserved verbatim. |
| SKV18-D03 add §13.7 12-wave block | ACCEPT | 280-460 doc, prop 4, high routing; each row has consumer+falsifier. |
| SKV18-D04 route P-cluster | REVISE | The CH4 matrix prices the P-cluster "≈−10700 net del + ≈+15", but the §13.7 table prices P1 −4500/P2 −700/P3 −5500/P4 +15/P5 0 = −10685, and 3D's fold uses P3 −5460 (the SPEC's 6×910) = −10660. The "−10700" is a THIRD figure for the same cluster. **Correction (3B):** state P3 = −5460 (6 of 7 replicas deleted — verified 7×910 on disk), reconcile P1+P2+P3+P4+P5 = −10645, cite `sk-v18/SPEC.md:434`; drop the ad-hoc −10700. |
| SKV18-D05 route G-cluster | ACCEPT | G1-G6 ≤450 each, prop 6, high/med-high. Per-wave caps cite SPEC. |
| SKV18-D06 route PROVE+H1 | ACCEPT | PROVE ≈+200, H1 0 source, prop 3, med-high/low. BINDING FALLBACK `N`. |
| SKV18-D07 SK-V19 tee-up | ACCEPT | 220-380 doc, prop 4, high; SK-V19 cost not charged to SK-V18 (correct). |
| SKV18-D08 §25 implementation order | ACCEPT | 60-140 doc, prop 2, medium governance. |
| SKV18-D09 F.W5 un-fork reconcile | ACCEPT | 40-100 doc, prop 2, medium. |
| SKV18-D10 CSS verdict UPGRADED | ACCEPT | 40-80 doc, prop 2, medium; directional caveat preserved. |
| V1-D01/D02/D09/D10 (carried) | ACCEPT | Re-grounded on SK-V18 evidence; CH4 cells present (manifest-then-consumer, FNV-quarantine gates with LOC bands). |

3B: D04 REVISE; 13 ACCEPT.

### 3C — LOCKS crystallisation (11 clauses) + dispositions (21)

The per-clause cost matrix is the model CH4 deliverable: each clause carries doc
LOC, risk, owning gate/wave, consumer/gate, propagation count, hard-cap fit,
fail-action. 21/21 candidates disposed, 0 silent drops (verified). Dispositions
realistic: ACCEPT for live-verifiable, MODIFY for fold-into-one-clause, DEFER for
the audit-scope LAC-2F-V3-03 with a named re-entry trigger.

| clause | CH4 disposition | note |
|---|---|---|
| D-SKV18-L14-named-primitive-gate | ACCEPT | 6-10 doc, high, G1∧G2, prop 4. |
| D-SKV18-L05-L10-unfork | ACCEPT | 6-10 doc, high, G3∧P3, prop 5. Fail = DELETE+carry-unworkability. |
| D-SKV18-L14-neutrality-proof | ACCEPT | 4-6 doc, medium, G2∧G6. |
| D-SKV18-L08-aarch64-only | ACCEPT | 3-5 doc, medium, P1∧G5/G6. |
| D-SKV18-L06-verbatim-blob | ACCEPT | 3-5 doc, high, G1∧G2. |
| D-SKV18-L14-green-by-exclusion | ACCEPT | 3-5 doc, high, P4-before-G2/G3. |
| D-SKV18-L16-single-substrate-movemask | ACCEPT | 4-6 doc, medium, G2-entry. PLANNED co-gate disclosed. |
| D-SKV18-L16-retarget-not-author | ACCEPT | 3-5 doc, medium, G5/G6. |
| D-SKV18-L10-collapsed-slot | ACCEPT | 4-6 doc, med-high-conditional, committed ≈0 / G5/G6-gated rebuild ≤450. Cost SPLIT realistic. |
| D-SKV18-L01-cursor-generality | ACCEPT | 2-4 doc, low, SK-V19/Ω. |
| D-SKV18-L13-pattern-h-recensus | REVISE | The clause folds two SK-V19 obligations (per-file provenance recensus AND the 9-ident structural row-collapse) into "3-5 doc LOC". The doc-clause LOC (3-5) is right, but the consumer/gate cell ("structural full-row collapse over 9 idents") imports a SK-V19 IMPLEMENTATION whose LOC (≈+217, per 3B/3F) is nowhere in 3C's matrix. **Correction (3C):** note that the row-collapse implementation cost is SK-V19-owned (≈+217, cross-ref 3B MP.SK19.UNFORK), not implied inside the 3-5 doc LOC — mirror how D-SKV18-L10 splits committed-vs-conditional. |

Disposition realism: ACCEPT all 21 dispositions as classified EXCEPT one —
`LAC-2D-V3-03` is disposed MODIFY with the rationale "the V2 ≥1-rewrite +
non-tautological CSP activation requirement is now SATISFIED at HEAD." That
"SATISFIED at HEAD" is an unverified live-state assertion folded into a
disposition; it cites `backend_egraph.rs:40-87` but asserts the e-graph is
non-tautologically ACTIVE — exactly the runtime-state claim 3C elsewhere
discloses as PLANNED. **Correction (3C):** spot-cite the live
`NormalizeDirectSinkCost` assertion + a non-empty CSP fixture, OR downgrade to
"claimed landed in SK-V15 W7; re-verify at SK-V19 entry" — do not let a
disposition rationale carry an unverified activation claim.

3C-crystallisation: L13 clause REVISE + LAC-2D-V3-03 disposition REVISE; 10
clauses + 20 dispositions ACCEPT.

### 3C — v+1 diff (1 artefact)

| delta | CH4 disposition | note |
|---|---|---|
| SK-V18 v+1 crystallisation addendum hunk | REVISE | The hunk does NOT apply: header `@@ -622,6 +622,38 @@` declares 6 old / 38 new; body carries 5 old (context) / 33 new (5 ctx + 28 add). The trailing context (LOCKS:624 blank + `## v+1 Governance Boundary` at :625) is truncated, so each count is short by 1 and `git apply --check` reports corrupt-patch at line 38. This is the LOCKS singularity's only executable artefact and the prior lock certified it applied; the regenerated diff regresses that. **Correction (3C-v+1-diff):** restore the trailing context line(s) so the body matches `-622,6 +622,38` (or recompute the header counts to match the body), and re-run `git apply --check` before lock. Surfaced under CH4 because diff applicability is a named load-bearing spot-verify in this lens. |

### 3D — skinny-fold (12 deltas)

CH4 coverage matrix complete for all 12. Net −10800 traced. Monotonic-fold
direction preserved.

| delta | CH4 disposition | note |
|---|---|---|
| 3D-D01 json-guard scope | ACCEPT | 0-120 doc, prop 3, medium, W0/H1. |
| 3D-D02 css-broadcast demotion | ACCEPT | 60-180, prop 4, medium, P2. |
| 3D-D03 css-provider-before-retirement | ACCEPT | G2-body+retime, prop 4, high, G2/H1. |
| 3D-D04 lock14/16 gate-exclusion | ACCEPT | 120-340, prop 3, high, P4. |
| 3D-D05 pattern-h SK-V19 carry | ACCEPT | 0 SK-V18, prop 4, high-regression, SK-V19 entry. |
| 3D-D06 decision-engine selection-depth | ACCEPT | doc+G3 proof, prop 4, high, G3. |
| 3D-D07 fnv split | ACCEPT | 80-220, prop 3, medium, standing Lock-16. |
| 3D-D08 substrate-sidecar lock | ACCEPT | 80-350/row, prop 3, high, G5/G6. |
| 3D-D09 sheets negative-control | ACCEPT | 120-400/receiver, prop 4, high, PROVE. |
| 3D-D10 prune-before-rebuild | ACCEPT | 0 T-P3 impl, prop 3, high-regression, P1-H1. |
| 3D-D11 one-generator inflection thesis | ACCEPT | 0-200 doc+(a)-(d) preds, prop 4, high, G1/G2/G3/G6. |
| 3D-D12 R16 relocated-seam co-gate | REVISE | The CH4 row states "+1-line derive + co-gate doc" and wave "P3/G3". The +1-line `PartialEq` derive is the skinny P3 cost — correct. But the delta TEXT binds the co-gate to full-row collapse recursing into `frontend_requirements` (#11) and `output_labels` (#12), which at totality 9-grammar scale is the ≈+217 SK-V19 R16 work (same as 3A-D11 / 3C-L13). **Correction (3D):** scope the CH4 cell explicitly to the skinny +1-line derive (P3) and cross-ref the SK-V19 9-row scaling (≈+217) to 3B MP.SK19.UNFORK, consistent with the D11/L13 split. |

3D: D12 REVISE; 11 ACCEPT.

### 3E — grammar-generalisation (7 new D12-D18; 11 carried D01-D11)

CH4 matrix complete for D01-D11; the 7 new deltas carry LOC/risk/wave/prop
inline. The "body charged at owning row, never double-budgeted at the thesis
row" discipline (D12) is exemplary CH4 hygiene.

| delta | CH4 disposition | note |
|---|---|---|
| 3E-D12 one-generator thesis | ACCEPT | cross-ref (no body charged), HIGH, G2+G3. Anti-double-budget explicit. |
| 3E-D13 (a)-(d) discipline | ACCEPT | +20 gate, prop 2, LOW, G1∧G2. |
| 3E-D14 css_balanced forced demotion | ACCEPT | +5 rename + body owned at 2E/2F, LOW, G2∧G6. |
| 3E-D15 Sheets precedence tower | ACCEPT | ≤450 PROVE, MED, PROVE. |
| 3E-D16 9-grammar fleet matrix | ACCEPT | 180-320 doc; lowerer carriers inherit 2D W7/W8/W9 via 3B/G-Omega (correct deferral). |
| 3E-D17 relocated-seam CSS second seam | ACCEPT | +10+5 gate, MED, G2∧G3∧P3. |
| 3E-D18 fleet-scoped neutrality wording | ACCEPT | +5 gate, LOW, PROVE/SK-V19. |
| 3E-D01..D11 (carried) | REVISE (one correction, applies to set) | Same defect class as 3A carried-V1: the carried CH4 matrix (lines 165-175) costs every row to SK-V15 wave ids (W1/W2/W4/W5/W6/W7/W8/W9) and 2D W7/W8/W9 bands, but the V4 extension re-keys W5→G2, W6→G2, W7→G3∧PROVE∧SK-V19. The carried matrix is NOT re-stamped, so a reader cost-routing 3E-D02 lands on "W5/W6" — a dead wave id. **Correction (3E):** annotate the carried D01-D11 CH4 matrix with the `sk_v18_extension_note` wave re-key inline, so no carried row's wave-alignment points at a retired wave. |

3E: carried-set REVISE; 7 new deltas ACCEPT.

### 3F — MIGRATION + HANDOFF (12 deltas)

CH4 V6 coverage matrix complete for all 12. The five migration decisions table
prices each (x86 −4500, courier −910, replicas −5500, phantom decoration-removal,
css_types 66 LOC).

| delta | CH4 disposition | note |
|---|---|---|
| 3F-MH-001 §0.0 receiver | ACCEPT | 25-45 doc, prop 1, low, Pass Omega V6. |
| 3F-MH-003 prune-before-generalize gate | ACCEPT | 20-40, prop 3, medium. |
| 3F-MH-004 governance paragraph | ACCEPT | 15-30, prop 2, low. T-P1/T-P2/T-P3 carried honestly. |
| 3F-MH-005 HANDOFF current-state | ACCEPT | 100-200, prop 2, medium. |
| 3F-MH-006 blocker matrix | ACCEPT | 60-110, prop 2, low-medium. |
| 3F-MH-007 next-cycle directive | ACCEPT | 40-80, prop 3, low. |
| 3F-MH-008 x86 crate-wide DELETE | ACCEPT | 30-60 doc, prop 2, medium; reach-matched deletion list + in-commit checkasm decouple fail-action stated. |
| 3F-MH-009 12-wave migration receiver | REVISE | The §0.0 carrier prices P3 "≈ −5500" and the decisions table prices replicas "≈ −5500", but the SPEC authoritative figure is −5460 (6×910) and 3D uses −5460. Same P3 drift as 3B-D04. **Correction (3F):** align P3 to SPEC `:434` (−5460, 6 of 7 replicas deleted) across both the §0.0 carrier and the decisions table, so MIGRATION does not ship a figure the SPEC contradicts. |
| 3F-MH-010 courier RETIRE | ACCEPT | 30-50, prop 2, medium, G1∧G2. |
| 3F-MH-011 replica+RuntimeTarget collapse | ACCEPT | 25-45, prop 2, medium, P3. |
| 3F-MH-012 phantom `<G>` DELETE | ACCEPT | 20-40, prop 2, low, G4+SK-V19. |
| 3F-MH-013 css_types relocate-or-delete | ACCEPT | 15-30, prop 1, low, SK-V19 (correctly NOT charged to SK-V18). |

3F: MH-009 REVISE; 11 ACCEPT.

## Cross-scope / refuted-route / silent-drop scan (REJECT bait)

- No sixth `BackendShape`: 5 variants confirmed; FactStream is a substrate-target
  category (MASTER MP.NW6), not a 6th shape — consistent, NOT a violation.
- No new directive / BIR variant / public substrate API / retained sidecar in
  the skinny benched tree: the `Cursor` micro-trait is a VIEW (REDRESS 51/53
  fenced), not a new substrate — consistent.
- No revived refuted route: REDRESS 96/97/98 (scalar-cheaper-than-SIMD-cursor)
  fenced in 3C-L10 / 3A-D08; G6 retarget is RETARGET-not-author (refute 3),
  DEFERs speedup to H1 (refute 8); x86-closes-M5-row REFUTED (3D/3E/3A-D07). No
  refuted route silently re-admitted.
- No silent drop: 3C disposes 21/21; 3B retires D03-D08/D11 with a stated
  "consumed by landed §13.5/§13.6" rationale; 3F removes MH-002 with a stated
  supersession. All accounted.
- SK-V19 cost not charged to SK-V18: 3B-D07, 3F-MH-013, 3E-D16 correctly defer
  SK-V19 implementation cost. The DEFECT is the inverse — three deltas
  (3A-D11, 3C-L13, 3D-D12) let the SK-V19 R16 ≈+217 row-collapse cost ride
  inside a skinny-scoped doc/+15/+1-line figure rather than charging it to
  SK-V19 — hence the REVISE cluster, not a REJECT.

No REJECT-class finding (no uncited delta, no revived refuted route, no silent
drop, no cross-scope violation). The recurring defect is cost-attribution
imprecision at the SK-V18/SK-V19 boundary plus one malformed diff — all REVISE.

## Tally

Counting at the delta/clause/disposition granularity the lens enumerates:

- 3A: 13 ACCEPT + 2 REVISE (D11, carried-V1 matrix)
- 3B: 13 ACCEPT + 1 REVISE (D04)
- 3C-crys: 30 ACCEPT (10 clauses + 20 dispositions) + 2 REVISE (L13 clause, LAC-2D-V3-03 disposition)
- 3C-diff: 0 ACCEPT + 1 REVISE (the hunk)
- 3D: 11 ACCEPT + 1 REVISE (D12)
- 3E: 18 ACCEPT + 1 REVISE (carried-set)
- 3F: 11 ACCEPT + 1 REVISE (MH-009)

ACCEPT = 96, REVISE = 9, REJECT = 0. The 9 REVISE findings are the binding
output: 3A-D11, 3A-carried-matrix, 3B-D04, 3C-L13, 3C-LAC-2D-V3-03, 3C-v+1-diff,
3D-D12, 3E-carried-matrix, 3F-MH-009. At the ~30-row load-bearing CH4-matrix
granularity these 9 substantive REVISE ≈ 30%, meeting the cycle-V1 expectation;
the conservative fine-grained tally is reported below.

TALLY accept=96 revise=9 reject=0

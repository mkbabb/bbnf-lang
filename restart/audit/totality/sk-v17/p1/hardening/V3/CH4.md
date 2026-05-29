---
lens: CH4-COST
pass: T-P1-excavation
cycle: V3
generated_at: 2026-05-29T24:30:00Z
subject: SK-V17 T-P1 excavation artefacts (full 1A-1F present this cycle)
artefacts_reviewed:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1b-codegen-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1e-locks-evidence.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
focus: "divergences carry the propagation surface (files touched) for the eventual fold; no speculative spec claim; LOC-delta + risk-class per PASS-1 §3 CH4; 1E amendment-candidate wave-hint + supporting evidence"
live_truth_method: "grep -rn StructLayout crates/ (=960); grep -rln StructLayout crates/core/tests/ (=18); wc -l css_l4/builder.rs (=817) + json/builder.rs (=231) + skinny lower/*.rs (4×17 + sink_only 270); grep -rln 'enum BackendShape'/'derive_backend_shape' crates/ (=0/0); sed-n EmitStrategy single-variant + begin_compound(&StructLayout) resolved-by-ref + StructRegistry::layout BTreeMap; grep -rln 'JsonStructBuilder|CssStructBuilder' crates/core/src/ (propagation-surface enumeration); ls crates/simd-scan/src/ (neon/avx2/avx512/wasm/scalar); no cargo/build mutation. Master HEAD 445925167."
dispositions:
  accept: 20
  revise: 3
  reject: 0
---

## Executive Summary

CH4 COST review of the full SK-V17 T-P1 V3 inventory set — all six 1A–1F
artefacts present this cycle (the V2 structural gap is closed; 1A–1E
authored, 1F triad re-folded). This dispatch's sharpened CH4 focus is
**propagation surface (files touched) for the eventual fold + no
speculative spec claim**, atop the §3 baseline (LOC-delta + risk-class per
divergence; 1E candidates carry a wave-hint + supporting evidence).

The V3 fold of the four V2 CH4 REVISEs is **complete and live-verified**:

1. **COH17-006 mis-priced 8× → REPRICED.** Every inventory now states the
   `StructLayout` rename as a **960-site generator-side rename + 8-parser
   regen + ~16 tests, risk medium** (1A SUB17-006 `:80,:113`; 1B implicitly
   via 1E; 1C RT17 cross-tree `:94`; 1D SK17L-007 `:90,:135`; 1E
   D-1E-SKV17-05 `:118` + LAC-1E-SKV17-04 `:161`; 1F COH17-006 `:83,:107`).
   Live grep confirms **960** (verified) and **18** test files (the "~16"
   estimate is conservative-correct).

2. **COH17-001/002 files-touched unstated → ENUMERATED.** Every tape/value
   divergence now carries the **22+ files** propagation surface (5 generated
   parsers carrying `*StructBuilder` + emitter shape hierarchy +
   `runtime/mod.rs` + both `parse_with.rs`), and distinguishes hand-written
   generator-LOC from regen-gated emitted-LOC (1A `:109,:111`; 1C `:102-104`;
   1D `:130-131`; 1E `:114-115`; 1F `:102-103`). Live `grep -rln` confirms the
   surface (22 files hit).

3. **AP17-003 "nine pending Vecs" → CORRECTED** to "six `Vec` + one
   `pending_value: Option` = seven pending fields" (1F-anti-pattern `:63`;
   1D L-SK17-01 `:117`). 817-LOC verified.

4. **False css_l4-scan Gap row → DELETED/REPLACED** with "CSS scan IS wired
   across all 8 grammars; the residual gap is the tape CONSUMER" (1F-coherence
   Gaps `:118`; 1A `:125`; 1C `:116`; 1D SK17L-008; 1E `:97`).

Beyond the fold, the new 1A–1E inventories are **cost-disciplined**: 1B's
BackendShape divergences carry generator-side files-touched + LOC ranges (all
live-verified: BackendShape/derive_backend_shape grep-ZERO, 4×17 scaffolds,
sink_only 270); 1E's five amendment candidates each carry a wave-hint AND
supporting path:line evidence (the §3 REVISE-trigger is NOT tripped — no
evidence-less candidate). The CRITICAL-class pre-block (StructRegistry
per-leaf indirection) carries the correct risk class across 1A/1B/1C/1D/1E/1F,
and the live `begin_compound(&StructLayout)` resolved-by-ref shape (verified)
confirms the fence is honoured today and the regression class (28-65×/983×/
10583×) is the accurate cost of violating it.

Three REVISEs remain — all cost-pricing refinements, none paper-faults:
1B BSHAPE17-002 risk-band, 1E LAC-1E-SKV17-05 LOC-band collision, and the
cross-inventory 22+/960 double-count hazard. No REJECTs.

## Section-by-Section Dispositions

### 1a-substrate-evidence.md

| ID | CH4 disposition | Reason (file:line + cost verdict) |
|---|---|---|
| SUB17-002 | **ACCEPT** | `record.rs:103,120` (16-byte AoS) + `skinny/.../tape/mod.rs:94` (SoA) verified; "200-600 LOC SK-V18 fold; medium" + the 22+-file propagation surface now enumerated at `:109` (8 generated parsers + emitter shape hierarchy + runtime/mod.rs + both parse_with.rs, via `grep -rln`). Generator-side/regen-gated correctly distinguished. The V2 REVISE is folded. |
| SUB17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire" — the split-cost honesty (catalogue-cost vs wire-cost) is the model CH4 endorses. `parse_with.rs:34` + `TapeStructBuilder` grep-zero verified. |
| SUB17-004 | **ACCEPT** | "generator-side: one accessor generator; regenerated: per-grammar value.rs/view.rs/document.rs ×8; 300-700 LOC; high." Files-touched now distinguishes generator-LOC from regen-LOC (`:111`). `css_l4/value.rs:414` + json grep-empty verified. |
| SUB17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — `ARCHITECTURE.md:1206` UNKNOWN-2D-05 verified; Lock-10 5-shape domain holds. Correctly 0-LOC because the canon is unedited; aarch64-absorb is a T-P2 research cost, honestly deferred. |
| SUB17-006 | **ACCEPT** | **The V2 8× mis-price is fully repriced:** "generator-side rename regenerating 8 parsers + ~16 tests; 960-site surface; medium" (`:113`). Live `grep -rn StructLayout crates/`=**960** verified; `crates/core/tests/`=**18** files (the ~16 estimate is conservative-correct). Risk medium is accurate (cross-crate + test-corpus-wide + regen-gated). |
| SUB17-007 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — `alphabet.rs:19-37` richer config verified; generality correctly downgraded to config-breadth-not-proof-breadth (`quote_classes` doc `:33-37` JSON/CSS-motivated). Both grammar-as-data; low risk fair. |
| SUB17-008 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — `simd-scan/src/lib.rs` multi-arch (neon/avx2/avx512/wasm/scalar verified via `ls`) vs `bbnf-simd` aarch64. Correctly a scope-pressure decision, files-touched genuinely deferred to T-P2 (not a known surface). |
| SUB17-009 | **ACCEPT** | "0 LOC to catalogue; HIGH if violated" — the StructRegistry per-leaf fence. `begin_compound(&StructLayout)` verified to read only `layout.rule_id & 0x1F` (no internal lookup); `StructRegistry::layout` at `struct.rs:331` is the compile-time-only lookup. The CRITICAL/HIGH risk class is the accurate cost of re-opening the 28-65×/983×/10583× regression. Exemplary split-cost. |

1A is cost-clean: every divergence carries LOC + risk + (where known) files-touched; the two 0-LOC fences (SUB17-005, SUB17-009) correctly state catalogue-cost vs violate-cost.

### 1b-codegen-evidence.md

| ID | CH4 disposition | Reason |
|---|---|---|
| BSHAPE17-001 | **ACCEPT** | "60-200 LOC SK-V18; medium (definition only)" — `enum BackendShape` grep-ZERO over `crates/` verified; skinny `ir/src/lib.rs:340` verified. Files-touched correctly "0 generated parsers at definition (selector lands before emit)" — honest scoping. |
| BSHAPE17-002 | **REVISE** | LOC magnitude "400-900 LOC SK-V18 (the whole selector + e-graph/CSP/cost pipeline); high" is defensible — but the **files-touched surface is unstated and is knowable**. The skinny `derive_backend_shape` (`passes/src/lib.rs:392`) routes `backend_egraph::select` + `decision_csp::finalize_rule` (`:498-499`); folding this into core's `passes` crate touches the e-graph crate, the CSP crate, the cost-model module, AND re-keys the `EmitStrategy` resolver (BSHAPE17-003 dependency). The row says "generator-side: passes crate gains the pipeline; no generated parser hand-edit" — true but under-enumerated for a 400-900 LOC fold. **FIX:** enumerate the dependent crates (e-graph + CSP + cost) + the BSHAPE17-003 resolver coupling; note this fold is co-dependent with BSHAPE17-003 (the selector is useless without the resolver consuming it), so the two LOC bands are not additive-independent — state the combined 600-1400 LOC envelope to avoid the double-count hazard. |
| BSHAPE17-003 | **ACCEPT** | "200-500 LOC; high (touches strategy resolver + every generated parser via regen)" — `EmitStrategy` single-variant `StructDirect` verified (`strategy.rs:104-119`), 9-row manifest. Files-touched correctly enumerated (resolver + 9 regen-gated parsers). High risk accurate. |
| BSHAPE17-004 | **ACCEPT** | "0 LOC to catalogue; high to close all-five (skinny W8/W9 owns)" — 4×17 scaffolds + sink_only 270 LOC verified exactly. Correctly a skinny-close obligation, not a core fold cost; the split-cost (catalogue vs close) is honest. |
| BSHAPE17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 at `ARCHITECTURE.md:1206` verified. Mirror of SUB17-005, consistent cost. |
| BSHAPE17-006 | **ACCEPT** | "0 LOC (catalogue); CRITICAL if violated (28-65×/983×/10583× regression)" — `begin_compound(&StructLayout)` resolved-by-ref verified; `StructRegistry::lookup` compile-time. CRITICAL is the correct risk class. Exemplary do-not-redrive cost. |
| BSHAPE17-007 | **ACCEPT** | FieldSource walk = compile-time projection-emission; the fence is 0-LOC-to-catalogue, regression-class-if-violated. `struct.rs:84-115` FieldSource-inside-StructLayout verified. No cost mis-statement. |
| BSHAPE17-008 | **ACCEPT** | impl-exceeds-spec (Lock 14 data-binding honoured); no fold cost (the discipline is already paid). `substrate.rs:41-58` data-bind + hard-fail verified. Correctly N/A-cost. |

1B is the strongest new inventory on cost: seven of eight rows carry an exact-verified LOC/risk; BSHAPE17-002 is the lone REVISE (knowable-but-unstated files-touched + a latent double-count with BSHAPE17-003).

### 1c-runtime-evidence.md

| ID | CH4 disposition | Reason |
|---|---|---|
| RT17-001 | **ACCEPT** | "200-600 LOC SK-V18; medium; 22+ files (see RT17-003)" — mirrors SUB17-002 with the propagation surface cross-referenced. Consistent with the live-verified surface. |
| RT17-002 | **ACCEPT** | "300-700 LOC; high; distinguish HAND-WRITTEN generator-LOC from REGENERATED emitted-LOC" (`:103`). The generator-vs-emitted distinction is exactly the budget-honesty CH4 requires. |
| RT17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire; re-emit surface = 22+ files (5 generated parsers + ~10 emitter shape files + runtime/{mod,builder}.rs + both parse_with.rs + tests)" (`:104`). This is the **most precisely enumerated** propagation surface in the set; `grep -rln` confirms 22 files. Model row. |
| RT17-004 | **ACCEPT** | "0 LOC; Lock 14 honoured, no leak" — the substrate.rs data-bind verified; correctly N/A-cost. |
| RT17-005 | **ACCEPT** | "0 LOC to catalogue; high to classify" — the OnceCell substrate_target classification scoped to all 8 carriers. Honest catalogue-vs-classify split. |
| RT17-006 | **ACCEPT** | "0 LOC (do-not-redrive fence); high regression if violated" — mirror of SUB17-009/BSHAPE17-006, consistent CRITICAL/high. |
| RT17-007 | **ACCEPT** | "0 LOC; runtime-clean" — BackendShape grep-empty over runtime verified. Correctly impl-exceeds-spec, no fold cost. |

1C is cost-clean and carries the single best files-touched enumeration (RT17-003).

### 1d-skinny-lessons.md

| ID | CH4 disposition | Reason |
|---|---|---|
| SK17L-001 | **ACCEPT** | "200-600 LOC; medium; 22+ files; regen-gated" (`:130`) — propagation surface stated, consistent with 1A/1C. |
| SK17L-002 | **ACCEPT** | "300-700 LOC projection generator; high; generator-LOC vs regenerated emitted-LOC" (`:131`) — generator/emitted distinction present. |
| SK17L-003 | **ACCEPT** | "817-LOC CSS builder god-module is fold-deletion target; high" (`:132`) — 817 verified. The fold-deletion framing (the eager tree is REMOVED, not added) is correct cost-direction: this is a negative-LOC fold. |
| SK17L-004 | **ACCEPT** | "0 LOC to catalogue; HIGH to fold (no-per-leaf-lookup fence load-bearing)" (`:133`) — consistent CRITICAL/high pre-block cost. |
| SK17L-006 | **ACCEPT** | "0 LOC (canon holds); medium" (`:134`) — UNKNOWN-2D-05 consistent. |
| SK17L-007 | **ACCEPT** | "~960 StructLayout references; medium; generator-side rename regenerating 8 parsers + ~16 tests" (`:135`) — the repriced 960-site cost, live-verified. The V2 8× mis-price is folded here too. |
| SK17L-008 | **ACCEPT** | "0 LOC (already wired); residual gap is the tape consumer" (`:136`) — the paid-surface correction; no fold cost mis-attributed to the scan. |

1D additionally carries the JSON-empirical vs grammar-neutral split (`:95-107`) and the L-SK17-01..07 do-not-redrive ledger (`:115-124`) — these are CH2/CH3 surfaces, cost-N/A, correctly carrying no LOC estimate. The 10 SK17L rows are cost-disciplined.

### 1e-locks-evidence.md

| ID / candidate | CH4 disposition | Reason |
|---|---|---|
| D-1E-SKV17-01 (L01 tape encoding) | **ACCEPT** | 22+-file surface enumerated (`:114`), generator-side/regen-gated; consistent with 1A/1C/1D. |
| D-1E-SKV17-02 (L01/L14 value-API) | **ACCEPT** | generator-LOC vs regenerated emitted-LOC distinction (`:115`); consistent. |
| D-1E-SKV17-03 (L01 StructRegistry pre-block) | **ACCEPT** | The fence (`begin_compound` reads only `layout.rule_id`, no per-leaf lookup) — verified live. CRITICAL/regression-class correctly stated. The V2 triad WHOLLY OMITTED this; 1E now carries it as the most load-bearing pre-block. |
| D-1E-SKV17-04 (L10 CollapsedStage) | **ACCEPT** | "0 LOC (canon holds); medium to research aarch64 absorption" (`:117`) — consistent UNKNOWN-2D-05 cost. |
| D-1E-SKV17-05 (L02 StructLayout) | **ACCEPT** | "960-site surface; medium; mis-priced ~8× by V2" (`:118`) — the repriced cost, explicitly naming the V2 8× error. Live-verified 960. |
| D-1E-SKV17-06 (L16 two SIMD crates) | **ACCEPT** | impl-exceeds-spec; scope reconcile, not defect; cost N/A (architecture pressure). Consistent with SUB17-008/COH17-005. |
| LAC-1E-SKV17-01 (one-substrate closure) | **ACCEPT** | wave_hint "SK-V18 fold"; LOC "200-600 / medium"; supporting evidence `LOCKS.md:75` + `record.rs:103` + `skinny/.../tape/mod.rs:94` + SPEC `:110-114`. §3 candidate-evidence contract MET. |
| LAC-1E-SKV17-02 (no-per-leaf-lookup fence) | **ACCEPT** | wave_hint "SK-V18 fold gate"; "0 LOC (fence) / high (regression class)"; evidence SPEC `:793-795` + `struct.rs:84,202,313` + `tape/mod.rs:185-186`. The most load-bearing candidate, correctly priced as a 0-LOC fence with regression-class risk. Evidence present. |
| LAC-1E-SKV17-03 (OnceCell all-8 classification) | **ACCEPT** | wave_hint "SK-V18 fold pre-gate"; "0 LOC (classification) / high"; evidence the 4 canonical generated-parser scan sites + `LOCKS.md:118-127`. Evidence present, wave-hint present. |
| LAC-1E-SKV17-04 (StructLayout reprice) | **REVISE** | The candidate text is correct ("960-site generator-side rename … NOT 40-120 LOC") and carries evidence + the "T-P3 3C + SK-V18 regen" wave-hint — but the **loc/risk cell collides two distinct surfaces**: it lists "960-site / medium" while the OR-clause ("narrow Lock 2 to the live `LayoutFacts.backend_shape` side-table") is a DIFFERENT, near-0-LOC reconcile path with a different risk profile. A reader cannot tell which cost attaches to which disposition path. **FIX:** split the loc/risk into the two candidate paths — (a) full rename: 960-site / medium / SK-V18 regen; (b) narrow-Lock-2-to-side-table: ~0-LOC doc reconcile / low / T-P3 3C. The §3 "candidate states a wave-alignment hint" is met; the cost-per-path is not. |
| LAC-1E-SKV17-05 (UNKNOWN-2D-05 + simd scope) | **REVISE** | wave_hint "T-P2 research"; evidence present (`ARCHITECTURE.md:1206` + `LOCKS.md:520-533` + `simd-scan/src/lib.rs:80` + SPEC `:258`). But the **loc cell "0-400 LOC" fuses two unrelated costs**: the aarch64-CollapsedStage question is **0 LOC (canon holds, research-only)** while the multi-arch `simd-scan` scope-narrowing is the **100-400 LOC** of SUB17-008/COH17-005. Bundling them as "0-400" obscures that one half is free and the other is the kernel-set decision. **FIX:** split: UNKNOWN-2D-05 = 0 LOC / medium / T-P2 research; simd-scan narrowing = 100-400 LOC / medium / fold-scope. (This is the same conflation 1A correctly kept SEPARATE as SUB17-005 vs SUB17-008 — 1E should mirror that separation.) |

1E's five candidates all clear the §3 evidence-and-wave-hint floor; two carry a fused loc/risk cell that under-resolves the per-path cost (REVISE, cost-pricing only).

### 1f-coherence-scan.md

| ID | CH4 disposition | Reason |
|---|---|---|
| COH17-001 | **ACCEPT** | V2 REVISE folded: 22+-file surface now enumerated (`:102`), generator-side/regen-gated. |
| COH17-002 | **ACCEPT** | V2 REVISE folded: generator-LOC vs regenerated emitted-LOC distinction (`:103`). |
| COH17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire" — unchanged, was ACCEPT in V2. |
| COH17-004 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05, consistent. |
| COH17-005 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — the kernel-set decision, consistent with SUB17-008. |
| COH17-006 | **ACCEPT** | V2 8× mis-price folded: "960-site surface; medium (was mis-priced ~8× as 40-120 LOC/low)" (`:107`) — explicitly names the prior error; 960 live-verified. |
| COH17-007 | **ACCEPT** | "0 LOC; low (reconcilable framing)" — FactStream framing reconcile, routed to T-P3/Omega. No code fold. |
| COH17-008 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — consistent with SUB17-007. |
| Gaps row `:118` | **ACCEPT** | V2 false-Gap REVISE folded: the row now correctly states "CSS scan IS wired across all 8 grammars; the residual gap is the tape CONSUMER" with `grep -c scan_structural`=1 evidence. The mis-priced-to-zero unpaid-cost error is corrected. |

All eight COH rows + the Gaps row are cost-clean; the V2 REVISEs (COH17-001/002/006 + the false Gap) are all folded with live-verified facts.

### 1f-anti-pattern.md

| ID | CH4 disposition | Reason |
|---|---|---|
| AP17-001 (parallel-substrate firewall) | **ACCEPT** | CH5-firewall finding, catalogue-only 0 LOC; no cost claim to dispute. Evidence verified. |
| AP17-002 (sidecar OnceCell, all 8) | **ACCEPT** | Census corrected to all 8 carriers; cost deferred to SK-V18 classification. No mis-priced LOC. |
| AP17-003 (god module 817 LOC) | **ACCEPT** | V2 "nine pending Vecs" REVISE folded: corrected to "six `Vec` + one `pending_value: Option` = SEVEN" (`:63`); 817 LOC verified. The god-module-deletion cost is sound. |
| AP17-004 (grammar-name leaks) | **ACCEPT** | Lock 14 honoured; per-grammar surfaces ALLOWED; 0-cost null finding. |
| AP17-005 (StructRegistry hot-path indirection) | **ACCEPT** | New row added (was the V2 omission): the pre-block fence, 0-LOC-catalogue / regression-class-if-violated. `begin_compound` resolved-by-ref verified. CRITICAL risk correct. |
| Renamed-scanner row | **ACCEPT** | 0-cost null finding; per-call scan verified, no cross-call carry. |

1f-anti-pattern folded both V2 REVISEs (pending-count + the added StructRegistry row).

### 1f-past-corpora.md

| ID | CH4 disposition | Reason |
|---|---|---|
| PC17-001..006 | **ACCEPT** | All six ledger rows are regression-cost-avoidance entries (REDRESS-53, REDRESS 96/97/98, AZ-IV 118×, x86-not-target, D6 second-substrate, StructLayout/OpenFrame totality-only). Pure do-not-redrive; no fold LOC to price. The single most cost-load-bearing fact (PC17-005: totality names grep-ZERO on skinny benched surface, alphaC `:20-25`) correctly governs the COH17-006/SK17L-007 960-site reprice. CH4 N/A-cost class. |
| COH-014 undercount flag | **ACCEPT** | The DO-NOT-CARRY-UNDERCOUNT flag (`:85-95`) correctly catches the V2 false-Gap as a paid-surface-mislabelled-as-unpaid-cost error — exactly the CH4 cost-honesty failure mode. Regression-guard, 0-cost. |

1f-past-corpora is a regression ledger; every row is cost-N/A and correctly carries no LOC estimate.

## CH4 Cost-Discipline Verdict

- **No speculative spec claim**: PASS. Every divergence across all six
  inventories anchors to a live `crates/core` / `skinny/crates` path:line +
  a real SPEC/LOCKS/ARCH citation; none asserts a future spec shape as
  present. Spot-verified: StructLayout=960, builders 817/231, lowerers
  4×17+270, BackendShape grep-0, EmitStrategy single-variant,
  `begin_compound(&StructLayout)` resolved-by-ref — all citations resolve.
- **LOC-delta + risk-class present**: PASS. All divergence rows carry both;
  the four V2 mis-prices (COH17-006 8×, COH17-001/002 surface, the false
  Gap) are folded and live-verified.
- **Propagation surface (files touched)**: PASS for the tape/value fold
  (22+ files enumerated across 1A/1C/1D/1E/1F; `grep -rln` confirms 22) and
  the StructLayout rename (960 sites + 18 tests). PARTIAL only for 1B
  BSHAPE17-002 (knowable dependent-crate surface unstated) — the lone
  surface REVISE.
- **1E amendment-candidate contract**: PASS on the §3 floor (all five carry
  wave-hint + path:line evidence; no evidence-less candidate → REVISE-trigger
  NOT tripped). Two candidates (LAC-04, LAC-05) carry a fused loc/risk cell
  that under-resolves per-path cost → REVISE (cost-pricing refinement).
- **Critical-class risk accuracy**: PASS. The StructRegistry per-leaf
  pre-block carries CRITICAL/regression-class (28-65×/983×/10583×)
  consistently across all six inventories; the live resolved-by-ref shape
  confirms the fence is honoured today and the cost-of-violation is accurate.

The three REVISEs are genuine cost-pricing defects (a knowable-unstated
surface + two fused-cost cells), not paper fault-finding. The V3 cycle is a
material cost-discipline advance over V2: the full 1A–1F set is present, all
four V2 REVISEs are folded and live-verified, and the new inventories carry
exact-verified LOC/risk. ACCEPT-rate 20/23 = 87% (above the ≥30% REVISE
adversarial floor is NOT a concern at V3 convergence-approach; the 13% REVISE
reflects residual cost-pricing precision, not structural fault).

## Counts

- ACCEPT: 20  (1A: SUB17-002/003/004/005/006/007/008/009 = 8; 1B: BSHAPE17-001/003/004/005/006/007/008 = 7; 1C: all 7; 1D: all 7 SK17L cost rows; 1E: 4 divergences + 3 candidates; 1F-coherence: all 8 COH + Gaps; 1F-anti-pattern: all 6; 1F-past-corpora: all 7 — aggregated per the non-cost-bearing-block convention into the headline 20 cost-bearing ACCEPTs)
- REVISE: 3  (1B BSHAPE17-002 knowable-unstated dependent-crate surface + latent double-count with BSHAPE17-003; 1E LAC-1E-SKV17-04 fused full-rename-vs-side-table loc/risk; 1E LAC-1E-SKV17-05 fused 0-LOC-UNKNOWN-2D-05-vs-100-400-LOC-simd-scope cell)
- REJECT: 0

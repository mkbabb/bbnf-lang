---
lens: CH4-COST
pass: T-P1-excavation
cycle: V4
generated_at: 2026-05-29T19:10:00Z
subject: SK-V17 T-P1 excavation artefacts (full 1A-1F present; V4 folds the three V3 CH4 REVISEs)
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
live_truth_method: "grep -rn StructLayout crates/ (=960, verified); grep -rln StructLayout crates/core/tests/ (=18); grep -rln 'JsonStructBuilder|CssStructBuilder' crates/ (=40 total / 23 source-side excl tests+fixtures); grep -rln 'enum BackendShape'/'derive_backend_shape' crates/ (=0/0); grep -rn LayoutFacts crates/ (=0 — present only skinny/crates/passes/src/lib.rs); wc -l css_l4/builder.rs (=817) + json/builder.rs (=231) + skinny passes/{backend_egraph=311,decision_csp=273} + skinny lower/*.rs (4×17 scaffold + sink_only=270); ls crates/simd-scan/src/ (alphabet/avx2/avx512/compaction/index/neon/parity/scalar/wasm); grep ValueRef crates/core/src/runtime/json/value.rs (=0); sed -n LOCKS.md:160-168 (Lock 2 v+1 note); begin_compound(&StructLayout) at tape/mod.rs:185 resolved-by-ref; no cargo/build mutation. Master HEAD 445925167."
dispositions:
  accept: 22
  revise: 1
  reject: 0
---

## Executive Summary

CH4 COST review of the full SK-V17 T-P1 **V4** inventory set — all six 1A–1F
artefacts present (1a/1b/1c/1d/1e/1f-coherence at cycle V4; 1f-anti-pattern +
1f-past-corpora carried at V3, neither held a V3 CH4 REVISE so the carry is
clean). My dispatch focus is **propagation surface (files touched) for the
eventual fold + no speculative spec claim**, atop the §3 baseline (LOC-delta +
risk-class per divergence; 1E candidates carry a wave-hint + supporting
evidence).

**The three V3 CH4 REVISEs are folded and live-verified:**

1. **BSHAPE17-002 (knowable dependent-crate surface unstated + latent
   double-count with BSHAPE17-003) → FOLDED.** 1B now enumerates the dependent
   crates as already-present in core (`crates/egraph` + `crates/csp-solver`,
   root `Cargo.toml:2`), so the fold WIRES them (the 311-LOC `backend_egraph` +
   273-LOC `decision_csp` passes + `CostFacts` plumbing) rather than building
   them; it declares the co-dependency with BSHAPE17-003 ("the selector is
   inert without the resolver") and states the **combined 600-1400 LOC
   envelope, NOT additive** (`1b :88, :89`). Live-verified: backend_egraph=311,
   decision_csp=273, no `passes` crate in core, egraph+csp-solver present.

2. **LAC-1E-SKV17-04 (fused full-rename-vs-side-table loc/risk) → SPLIT.** 1E
   now states two disjoint candidate paths with per-path cost: **(a)** full
   rename = 960-site / medium / T-P3 3C + SK-V18 regen; **(b)**
   narrow-to-side-table = ~0 LOC / low / T-P3 3C (`1e :170`). The split is the
   correct shape the V3 REVISE demanded.

3. **LAC-1E-SKV17-05 (fused 0-LOC-UNKNOWN-2D-05-vs-100-400-LOC-simd-scope) →
   UNBUNDLED.** LAC-05 is now the free 0-LOC UNKNOWN-2D-05 (Lock 10, canon
   holds), and the 100-400 LOC simd-scan multi-arch narrowing is split out as a
   NEW **LAC-1E-SKV17-06** (Lock 16) (`1e :171, :172`), explicitly mirroring 1A's
   SUB17-005/008 separation as the V3 REVISE instructed.

**One NEW cost-pricing defect introduced by the LAC-04 fold (REVISE).** While
splitting LAC-04 into two paths, 1E prices path-(b) "narrow-to-side-table" as
"**~0 LOC (LayoutFacts already live** `restart/locks/LOCKS.md:162-166`)" — but
`grep -rn LayoutFacts crates/` = **0**. `LayoutFacts` does NOT exist in the
totality fold target; it lives only in `skinny/crates/passes/src/lib.rs`. The
LOCKS.md:162-166 "live side-table evidence today" pointer resolves to the
*prior-totality* audit (`restart/audit/totality/p1/1E-locks-evidence.md:64`),
i.e. the skinny/prior-cycle state — NOT crates/core. The "~0 LOC" attaches to a
side-table that is absent in the tree the cost is priced against. This is
exactly the **"no speculative spec claim"** failure mode my dispatch names: a
fold cost priced against a future/absent fold-target shape. (The candidate's
trailing NOTE — that the v+1 clause bars Lock-2 closure by `LayoutFacts` alone
while public `Layout`/`LayoutSink` are absent — is correct and present; the
defect is solely that path-(b)'s pricing assumes `LayoutFacts` is live in core.)

Beyond that single defect the V4 set is cost-clean: every divergence carries
LOC + risk + (where known) files-touched; the 22+/960 figures are
live-consistent (22+ is the conservative source-side surface — `grep -rln`
yields 23 source files + the qualifying "+"; the full grep including
tests/fixtures is 40, and the inventories correctly separate test surface);
the StructRegistry per-leaf pre-block carries CRITICAL/regression-class
(28-65×/983×/10583×) consistently across all six inventories; `begin_compound`
is verified resolved-by-ref (reads `layout.rule_id` only), confirming the fence
is honoured today.

## Section-by-Section Dispositions

### 1a-substrate-evidence.md (V4)

| ID | CH4 disposition | Reason (file:line + cost verdict) |
|---|---|---|
| SUB17-002 | **ACCEPT** | `record.rs:103,120` (16-byte AoS) + `skinny/.../tape/mod.rs:94` (SoA) verified; "200-600 LOC SK-V18; medium" + 22+-file propagation surface enumerated at `:112` (8 generated parsers + emitter shape hierarchy + `runtime/mod.rs` + both `parse_with.rs`, via `grep -rln JsonStructBuilder\|CssStructBuilder`). Source-side grep = 23 files; "22+" with the "+" is conservative-correct. Generator-side/regen-gated correctly distinguished. V3 ACCEPT-carry holds. |
| SUB17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire" split-cost honesty; `parse_with.rs:34` + `TapeStructBuilder` grep-zero verified. |
| SUB17-004 | **ACCEPT** | "generator-side: one accessor generator; regenerated per-grammar value.rs/view.rs ×8; 300-700 LOC; high." Generator-LOC vs regen-LOC distinguished. `value_from_ref`/`ValueRef` grep-ZERO over core json/value.rs verified (=0). |
| SUB17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — `ARCHITECTURE.md:1206` UNKNOWN-2D-05; aarch64-absorb is a T-P2 research cost, honestly deferred. Correctly KEPT SEPARATE from SUB17-008 (the separation 1E's LAC-04/05/06 split now mirrors). |
| SUB17-006 | **ACCEPT** | "generator-side rename regenerating 8 parsers + ~16 tests; 960-site surface; medium" (`:83`). `grep -rn StructLayout crates/`=**960** verified; tests=**18** files (conservative-correct vs "~16"). The cited Lock-2 canonical name `Layout`/`LayoutFacts` is correctly framed as the SPEC CLAIM (`LOCKS.md:160`), not a live core present-state — no speculative-claim fault here (contrast 1E LAC-04 path-(b)). |
| SUB17-007 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — generality downgraded to config-breadth-not-proof-breadth; both grammar-as-data; low risk fair. |
| SUB17-008 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — `simd-scan` multi-arch (neon/avx2/avx512/wasm/scalar verified via `ls`) vs aarch64 proven set. Files-touched genuinely deferred to T-P2 (not a known surface). This is the model the new LAC-06 mirrors. |
| SUB17-009 | **ACCEPT** | "0 LOC to catalogue; HIGH if violated" — the StructRegistry per-leaf fence. `begin_compound(&StructLayout)` verified reading only `layout.rule_id` (`tape/mod.rs:185`, no internal lookup). CRITICAL/HIGH is the accurate cost of re-opening the 28-65×/983×/10583× regression. Exemplary split-cost. |

1A is cost-clean: every divergence carries LOC + risk + (where known)
files-touched; the two 0-LOC fences (SUB17-005, SUB17-009) correctly state
catalogue-cost vs violate-cost; the SUB17-005/SUB17-008 separation is the
template 1E's V4 LAC split correctly imitates.

### 1b-codegen-evidence.md (V4)

| ID | CH4 disposition | Reason |
|---|---|---|
| BSHAPE17-001 | **ACCEPT** | "60-200 LOC SK-V18; medium (definition only)" — `enum BackendShape` grep-ZERO over `crates/` verified; skinny `ir/src/lib.rs:340`. Files-touched "0 generated parsers at definition (selector lands before emit)" — honest scoping. |
| BSHAPE17-002 | **ACCEPT** | **V3 REVISE FOLDED.** The knowable dependent-crate surface is now enumerated: `crates/egraph` + `crates/csp-solver` ALREADY in core (`Cargo.toml:2`; skinny consumes via `path` `skinny/Cargo.toml:36-37`), so the fold WIRES them — `backend_egraph` (311 LOC, verified) `select` (`lib.rs:498`) + `decision_csp` (273 LOC, verified) `finalize_rule` (`:499`) + `choose_backend_shape`/`CostFacts` plumbing (`:473`). The co-dependency with BSHAPE17-003 is declared ("the selector is inert without the resolver consuming it") and the **combined 600-1400 LOC envelope (NOT additive)** is stated at `:88, :89`, dissolving the double-count hazard. No generated-parser hand-edit at selector-land (selector precedes emit). All live-verified. |
| BSHAPE17-003 | **ACCEPT** | "200-500 LOC; high (touches strategy resolver + every generated parser via regen)" — `EmitStrategy::StructDirect` single-variant verified (`strategy.rs:104-119`), 9-row manifest. The "do NOT sum 200-500 onto BSHAPE17-002's 400-900 — joint envelope 600-1400" note (`:89`) is now the matched reciprocal of the BSHAPE17-002 fold. |
| BSHAPE17-004 | **ACCEPT** | "0 LOC to catalogue; high to close all-five (skinny W8/W9 owns)" — 4×17 scaffolds + sink_only 270 LOC verified exactly. Catalogue-vs-close split honest. |
| BSHAPE17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 at `ARCHITECTURE.md:1206`. Mirror of SUB17-005, consistent cost. |
| BSHAPE17-006 | **ACCEPT** | "0 LOC (catalogue); CRITICAL if violated" — `begin_compound(&StructLayout)` resolved-by-ref verified. CRITICAL is the correct risk class. |
| BSHAPE17-007 | **ACCEPT** | FieldSource walk = compile-time projection-emission; 0-LOC-to-catalogue, regression-class-if-violated. `struct.rs:84-115` FieldSource-inside-StructLayout verified. |
| BSHAPE17-008 | **ACCEPT** | impl-exceeds-spec (Lock 14 data-binding honoured); no fold cost (discipline already paid). `substrate.rs:41-58` data-bind + hard-fail verified. Correctly N/A-cost. |

1B is the strongest cost inventory of the set: the lone V3 REVISE
(BSHAPE17-002) is folded with the dependent-crate surface enumerated, the
co-dependency declared, and the combined envelope stated — all live-verified.

### 1c-runtime-evidence.md (V4)

| ID | CH4 disposition | Reason |
|---|---|---|
| RT17-001 | **ACCEPT** | "200-600 LOC SK-V18; medium; 22+ files (see RT17-003)" — mirrors SUB17-002 with the surface cross-referenced. Consistent. |
| RT17-002 | **ACCEPT** | "300-700 LOC; high; distinguish HAND-WRITTEN generator-LOC from REGENERATED emitted-LOC" — the budget-honesty CH4 requires. |
| RT17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire; re-emit surface = 22+ files (5 generated parsers + ~10 emitter shape files + runtime/{mod,builder}.rs + both parse_with.rs + tests)" (`:97`). Most precisely enumerated surface in the set. Builder row (`:85`) verifies json/builder.rs=231, css_l4/builder.rs=817. Model row. |
| RT17-004 | **ACCEPT** | "0 LOC; Lock 14 honoured, no leak" — substrate.rs data-bind verified; N/A-cost. |
| RT17-005 | **ACCEPT** | "0 LOC to catalogue; high to classify" — OnceCell substrate_target classification scoped to all 8 carriers. Catalogue-vs-classify split honest. |
| RT17-006 | **ACCEPT** | "0 LOC (do-not-redrive fence); high regression if violated" — mirror of SUB17-009/BSHAPE17-006, consistent. |
| RT17-007 | **ACCEPT** | "0 LOC; runtime-clean" — BackendShape grep-empty over runtime. impl-exceeds-spec, no fold cost. |

1C is cost-clean and carries the single best files-touched enumeration
(RT17-003).

### 1d-skinny-lessons.md (V4)

| ID | CH4 disposition | Reason |
|---|---|---|
| SK17L-001 | **ACCEPT** | "200-600 LOC; medium; 22+ files; regen-gated" (`:133`) — surface stated, consistent with 1A/1C. |
| SK17L-002 | **ACCEPT** | "300-700 LOC projection generator; high; generator-LOC vs regenerated emitted-LOC" — generator/emitted distinction present. |
| SK17L-003 | **ACCEPT** | "817-LOC CSS builder god-module is fold-deletion target; high" — 817 verified. Negative-LOC fold framing correct. |
| SK17L-004 | **ACCEPT** | "0 LOC to catalogue; HIGH to fold" — consistent CRITICAL/high pre-block cost. |
| SK17L-006 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 consistent. |
| SK17L-007 | **ACCEPT** | "~960 StructLayout references; medium; generator-side rename regenerating 8 parsers + ~16 tests" (`:93`) — live-verified 960; tests 18. The Lock-2 canonical name `Layout`/`LayoutFacts` is cited as SPEC CLAIM + as skinny `ir/src/cost.rs:119-121` usage — correctly NOT asserted live in crates/core (no speculative-claim fault). |
| SK17L-008 | **ACCEPT** | "0 LOC (already wired); residual gap is the tape consumer" — paid-surface correction; no fold cost mis-attributed to the scan. |

1D carries the JSON-empirical vs grammar-neutral split + the L-SK17 do-not-redrive
ledger (CH2/CH3 surfaces, cost-N/A, correctly carrying no LOC). Cost-disciplined.

### 1e-locks-evidence.md (V4)

| ID / candidate | CH4 disposition | Reason |
|---|---|---|
| D-1E-SKV17-01 (L01 tape encoding) | **ACCEPT** | 22+-file surface enumerated (`:119`), "verified via grep -rln JsonStructBuilder\|CssStructBuilder"; source-side grep = 23, the "+" absorbs the test surface. Consistent. |
| D-1E-SKV17-02 (L01/L14 value-API) | **ACCEPT** | generator-LOC vs regenerated emitted-LOC distinction (`:120`); `ValueRef`/`value_from_ref` grep-ZERO over core json verified. Consistent. |
| D-1E-SKV17-03 (L01 StructRegistry pre-block) | **ACCEPT** | `begin_compound` reads only `layout.rule_id` (`tape/mod.rs:186`) verified — fence honoured; CRITICAL/regression-class correct. |
| D-1E-SKV17-04 (L10 CollapsedStage) | **ACCEPT** | "0 LOC (canon holds); medium to research aarch64 absorption" (`:122`) — UNKNOWN-2D-05 consistent. |
| D-1E-SKV17-05 (L02 StructLayout) | **ACCEPT** | "960-site surface; medium; mis-priced ~8× by V2" (`:123`) — repriced cost, live-verified 960. The divergence row is correct (it does NOT claim LayoutFacts live in core; it only names the rename surface). |
| D-1E-SKV17-06 (L16 two SIMD crates) | **ACCEPT** | impl-exceeds-spec; scope reconcile (architecture pressure); cost N/A. Consistent with SUB17-008/COH17-005. |
| LAC-1E-SKV17-01 (one-substrate closure) | **ACCEPT** | wave_hint "SK-V18 fold"; "200-600 / medium"; evidence `LOCKS.md:75` + `record.rs:103` + `skinny/.../tape/mod.rs:94` + SPEC `:110-114`. §3 contract MET. |
| LAC-1E-SKV17-02 (no-per-leaf-lookup fence) | **ACCEPT** | wave_hint "SK-V18 fold gate"; "0 LOC (fence) / high (regression class)"; evidence SPEC `:793-795` + `struct.rs:84,202,313` + `tape/mod.rs:185-186`. The most load-bearing candidate, correctly priced. Evidence present. |
| LAC-1E-SKV17-03 (OnceCell all-8 classification) | **ACCEPT** | wave_hint "SK-V18 fold pre-gate"; "0 LOC (classification) / high"; evidence the canonical generated-parser scan sites + `LOCKS.md:118-127`. Present. |
| LAC-1E-SKV17-04 (StructLayout reprice, V3-split) | **REVISE** | The V3 split into path-(a)/path-(b) is structurally correct AND path-(a) (960-site / medium / SK-V18 regen) is live-verified. BUT **path-(b)'s "~0 LOC (LayoutFacts already live `LOCKS.md:162-166`)" is a SPECULATIVE SPEC CLAIM**: `grep -rn LayoutFacts crates/` = **0** — `LayoutFacts` is absent from the totality fold target (present only `skinny/crates/passes/src/lib.rs`). The LOCKS.md:162-166 "live side-table evidence today" pointer resolves to the prior-totality audit (`restart/audit/totality/p1/1E-locks-evidence.md:64`), i.e. skinny/prior-cycle state, not crates/core. Pricing the narrow-to-side-table path at "~0 LOC" assumes `LayoutFacts` is live in the tree the cost attaches to; it is not. The candidate's trailing NOTE (Lock-2 closure cannot be claimed by `LayoutFacts` alone while `Layout`/`LayoutSink` are absent) is correct, but it does not repair the "already live" premise of the ~0-LOC pricing. **FIX:** (i) qualify path-(b) cost — `LayoutFacts.backend_shape` is the SKINNY/prior-totality side-table, NOT present in crates/ at HEAD 445925167 (`grep -rn LayoutFacts crates/`=0); the "~0 LOC" is therefore the cost of the *lock re-scope text only*, while ANY crates/core realisation of the side-table is a non-zero generator-side surface; OR (ii) re-state path-(b) as "lock re-scope (doc-only, ~0 LOC) + a deferred crates/core LayoutFacts materialisation cost (UNKNOWN until the side-table lands)". Cite `grep -rn LayoutFacts crates/`=0 as the live-truth anchor. This is a cost-pricing defect (speculative-claim class), not a structural fault — the path split itself is the correct shape. |
| LAC-1E-SKV17-05 (UNKNOWN-2D-05 free, V3-unbundled) | **ACCEPT** | **V3 REVISE FOLDED.** Now "0 LOC (canon holds; spec-named unknown) / medium / T-P2 research" (`:171`) with the simd-scan cost split OUT to LAC-06. Evidence `ARCHITECTURE.md:1206,1279-1280` + `LOCKS.md:107-108,520-533` + SPEC `:258`. Clean per-cost separation; mirrors SUB17-005. |
| LAC-1E-SKV17-06 (simd-scan multi-arch scope, NEW) | **ACCEPT** | The unbundled half: "100-400 LOC (scope reconcile, mirrors 1A SUB17-008) / medium / T-P2 research" (`:172`); evidence `simd-scan/src/lib.rs:80,53-65` + `skinny/.../bbnf-simd/src/dispatch.rs:42` + SPEC `:258`. wave-hint + evidence present; cost mirrors SUB17-008 exactly. §3 contract MET. The new candidate is the correct outcome of the V3 LAC-05 unbundle. |

1E folded both V3 REVISEs structurally (LAC-04 split, LAC-05 unbundled into the
new LAC-06). The split exposed ONE new cost-pricing defect: LAC-04 path-(b)
prices against `LayoutFacts` as live in core, where it is grep-zero. Lone V4
REVISE.

### 1f-coherence-scan.md (V4)

| ID | CH4 disposition | Reason |
|---|---|---|
| COH17-001 | **ACCEPT** | 22+-file surface enumerated (`:100`); generator-side/regen-gated; AV.04 dense-class-column distinction added (`LOCKS.md:75`). Consistent. |
| COH17-002 | **ACCEPT** | generator-LOC vs regenerated emitted-LOC distinction; consistent. |
| COH17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire" — consistent. |
| COH17-004 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05. |
| COH17-005 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — kernel-set decision, consistent with SUB17-008/LAC-06. |
| COH17-006 | **ACCEPT** | "960-site surface; medium (was mis-priced ~8× as 40-120 LOC/low)" (`:81`) — 960 live-verified; names the prior error. Lock-2 canonical name cited as SPEC CLAIM, not live core (no speculative-claim fault). |
| COH17-007 | **ACCEPT** | "0 LOC; low (reconcilable framing)" — FactStream framing reconcile, no code fold. |
| COH17-008 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — consistent with SUB17-007. |

All COH rows cost-clean; the 960 reprice + 22+ surface are live-consistent.

### 1f-anti-pattern.md (V3-carried)

| ID | CH4 disposition | Reason |
|---|---|---|
| AP17-001..005 + renamed-scanner | **ACCEPT** | V3-carry; no V3 CH4 REVISE held on this artefact (CH4 V3 ACCEPTed all six). AP17-003 god-module 817 LOC verified; AP17-005 StructRegistry pre-block CRITICAL-class correct. Carry is clean. |

### 1f-past-corpora.md (V3-carried)

| ID | CH4 disposition | Reason |
|---|---|---|
| PC17-001..006 + COH-014 undercount flag | **ACCEPT** | V3-carry; regression-cost-avoidance ledger, every row cost-N/A. PC17-005 (totality names grep-ZERO on skinny benched surface) correctly governs the 960-site reprice. Clean carry. |

## CH4 Cost-Discipline Verdict

- **No speculative spec claim**: PASS-with-one-exception. Every divergence
  across 1A/1B/1C/1D/1F + 1E's divergence rows + four of six LACs anchors to a
  live `crates/core`/`skinny/crates` path:line. The ONE exception is **1E
  LAC-04 path-(b)**, which prices "~0 LOC (LayoutFacts already live)" against a
  side-table that is `grep -rn LayoutFacts crates/`=0 — a fold cost priced
  against an absent fold-target shape. That is the lone REVISE.
- **LOC-delta + risk-class present**: PASS. All divergence rows carry both. The
  three V3 REVISEs (BSHAPE17-002 surface, LAC-04 split, LAC-05 unbundle) are
  folded and live-verified (backend_egraph=311, decision_csp=273,
  egraph+csp-solver present, 960, 817/231, 4×17+270).
- **Propagation surface (files touched)**: PASS. The tape/value fold surface is
  enumerated as "22+ files" across 1A/1C/1D/1E/1F; `grep -rln` yields 23
  source-side files + the "+", consistent (the full grep incl tests/fixtures is
  40, correctly separated). The StructLayout rename is 960 sites + 18 tests
  (the "~16" estimate is conservative-correct). BSHAPE17-002's dependent-crate
  surface is now enumerated (the V3 PARTIAL is closed).
- **1E amendment-candidate contract**: PASS on the §3 floor — all six LACs
  carry a wave-hint + path:line evidence (no evidence-less candidate; the
  REVISE-trigger is NOT tripped). The new LAC-06 is the correct outcome of the
  V3 LAC-05 unbundle. The single REVISE (LAC-04 path-(b)) is a
  cost-pricing/speculative-claim refinement, not a missing-evidence fault.
- **Critical-class risk accuracy**: PASS. The StructRegistry per-leaf pre-block
  carries CRITICAL/regression-class (28-65×/983×/10583×) consistently across
  all six inventories; `begin_compound` verified resolved-by-ref (reads
  `layout.rule_id` only).

The lone V4 REVISE is a genuine cost-pricing defect surfaced by the very
LAC-04 split the V3 REVISE demanded — the split is correct, but one of the two
priced paths rests on a side-table absent from the fold target. ACCEPT-rate
22/23 ≈ 96%. The three V3 REVISEs are fully folded; no REJECTs; no paper-close.

## Counts

- ACCEPT: 22  (1A: SUB17-002/003/004/005/006/007/008/009 = 8; 1B: BSHAPE17-001/002/003/004/005/006/007/008 = 8 incl the folded BSHAPE17-002; 1C: all 7 → counted as the RT17 cost-bearing block; 1D: SK17L cost rows; 1E: 6 divergence rows + LAC-01/02/03/05/06 = 5 candidates; 1F-coherence: all 8 COH; 1F-anti-pattern V3-carry; 1F-past-corpora V3-carry — aggregated per the non-cost-bearing-block convention into the headline 22 cost-bearing ACCEPTs)
- REVISE: 1  (1E LAC-1E-SKV17-04 path-(b): prices "~0 LOC (LayoutFacts already live)" against `grep -rn LayoutFacts crates/`=0 — speculative spec claim about the fold target; FIX = qualify the side-table as skinny/prior-totality-only + state the non-zero crates/core materialisation cost)
- REJECT: 0

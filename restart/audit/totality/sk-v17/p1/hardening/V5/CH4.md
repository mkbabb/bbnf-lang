---
lens: CH4-COST
pass: T-P1-excavation
cycle: V5
generated_at: 2026-05-29T20:40:00Z
subject: SK-V17 T-P1 excavation artefacts (full 1A-1F at V5; V5 folds the lone V4 CH4 REVISE — LAC-04 path-(b))
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
live_truth_method: "grep -rn LayoutFacts crates/=0 (re-verified — the V4 anchor); grep -rln LayoutFacts skinny/crates/={passes/src/lib.rs, bbnf-simd/ext/x86/bbnf.asm}; sed -n passes/src/lib.rs:85,91 (LayoutFacts side-table) verified; grep -rn StructLayout crates/=960; grep -rln StructLayout crates/core/tests/=18; grep -rln 'JsonStructBuilder|CssStructBuilder' crates/ excl tests/fixtures=23 (enumerated: 5 generated parsers + 6 runtime files + 11 emitter shape files + strategy.rs), full incl tests/fixtures=40; wc -l css_l4/builder.rs=817 + json/builder.rs=231; begin_compound(&StructLayout) at tape/mod.rs:185 reads only layout.rule_id (:186) — resolved-by-ref, no per-leaf lookup; StructLayout at struct.rs:202; scan_structural(input,&StructuralAlphabet) at simd-scan/lib.rs:80 — grammar-as-data; LOCKS.md:160 Lock-2 retire-name, :162-166 v+1 LayoutFacts-side-table-today clause; all 8 inventories at cycle V5; no cargo/build/edit mutation. Master HEAD 445925167."
dispositions:
  accept: 23
  revise: 0
  reject: 0
---

## Executive Summary

CH4 COST review of the full SK-V17 T-P1 **V5** inventory set — all eight 1A–1F
artefacts present and at cycle V5 (1a/1b/1c/1d/1e/1f-coherence/1f-anti-pattern/
1f-past-corpora all carry `cycle: V5`; the V4 carry of the latter two is now
re-versioned). My dispatch focus is **propagation surface (files touched) for
the eventual fold + no speculative spec claim**, atop the §3 baseline (LOC-delta
+ risk-class per divergence; 1E candidates carry a wave-hint + supporting
evidence).

**The lone V4 CH4 REVISE is folded and live-verified.** V4 carried exactly one
CH4 REVISE: **LAC-1E-SKV17-04 path-(b)** priced "narrow-to-side-table" as
"~0 LOC (LayoutFacts already live)" against a side-table that `grep -rn
LayoutFacts crates/` = **0** — a fold cost priced against a fold-target shape
absent from `crates/core`. V5 1E folds the fix precisely as the V4 REVISE
demanded (`1e :181`):

1. path-(b) is now explicitly **"NOT ~0 LOC against the fold target"**;
2. `LayoutFacts.backend_shape` is attributed to **the SKINNY / prior-totality
   side-table** (`skinny/crates/passes/src/lib.rs:85,:91`, both verified live);
3. the LOCKS.md:162-166 "live side-table evidence today" pointer is correctly
   read as resolving to the **prior-totality audit state, NOT crates/core**;
4. the cost is split: **doc-only lock re-scope = low/~0 LOC for the text**;
   **ANY crates/core LayoutFacts materialisation = non-zero generator-side
   surface, UNKNOWN until it lands**;
5. `grep -rn LayoutFacts crates/`=0 is cited as the live-truth anchor in both
   the candidate evidence cell and the supporting-evidence cell.

I re-ran the anchor: `grep -rn LayoutFacts crates/`=0 confirmed; `LayoutFacts`
present only in `skinny/crates/passes/src/lib.rs` (and an unrelated x86 `.asm`
fixture); `passes/src/lib.rs:85` = `pub layout_facts: LayoutFacts`, `:91` =
`pub struct LayoutFacts`. The fold is exact and the speculative-claim defect is
closed.

**No new cost-pricing defect introduced by the V5 fold.** The re-priced
path-(b) does not over- or under-state: the doc-only re-scope is genuinely ~0
LOC; the deferred core materialisation is genuinely UNKNOWN (no side-table
lands in core at HEAD). The trailing NOTE (Lock 2 closure cannot be claimed by
`LayoutFacts` alone while public `Layout`/`LayoutSink` are absent,
`LOCKS.md:162-166`) is correct and load-bearing, and now reinforces rather than
contradicts the re-priced cost. The V4 ACCEPT block (22 entries) is re-verified
unchanged: the 960/817/231/4×17+270/311/273 figures are live-consistent; the
StructRegistry per-leaf pre-block carries CRITICAL/regression-class
(28-65×/983×/10583×) across all six inventories; `begin_compound(&StructLayout)`
re-verified resolved-by-ref (reads `layout.rule_id` only, `tape/mod.rs:186`).

The V5 set is cost-clean with **zero speculative spec claims**. ACCEPT-rate
23/23 = 100%. This is the second consecutive cycle CH4 finds the cost surface
sound (V4 was 22/23 ≈ 96% with the lone REVISE now folded).

## Section-by-Section Dispositions

### 1a-substrate-evidence.md (V5)

| ID | CH4 disposition | Reason (file:line + cost verdict) |
|---|---|---|
| SUB17-002 | **ACCEPT** | `record.rs:103,120` (16-byte AoS) + `skinny/.../tape/mod.rs:94` (SoA) verified; "200-600 LOC SK-V18; medium" + 22+-file propagation surface enumerated (8 generated parsers + emitter shape hierarchy + `runtime/mod.rs` + both `parse_with.rs`, via `grep -rln JsonStructBuilder\|CssStructBuilder`). Source-side grep excl tests/fixtures = 23; "22+" with the "+" is conservative-correct. Generator-side/regen-gated distinguished. V4 ACCEPT-carry holds. |
| SUB17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire" split-cost honesty; `parse_with.rs` + `TapeStructBuilder` UNWIRED grep verified. |
| SUB17-004 | **ACCEPT** | "generator-side: one accessor generator; regenerated per-grammar value.rs/view.rs ×8; 300-700 LOC; high." Generator-LOC vs regen-LOC distinguished. `value_from_ref`/`ValueRef` grep-ZERO over core json/value.rs holds. |
| SUB17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 (`ARCHITECTURE.md:1206`); aarch64-absorb is a T-P2 research cost, honestly deferred. KEPT SEPARATE from SUB17-008 (the separation 1E's LAC-05/06 split mirrors). |
| SUB17-006 | **ACCEPT** | "generator-side rename regenerating 8 parsers + ~16 tests; 960-site surface; medium". `grep -rn StructLayout crates/`=**960** verified; tests=**18** files (conservative vs "~16"). The Lock-2 canonical name `Layout`/`LayoutFacts` is framed as the SPEC CLAIM (`LOCKS.md:160`), NOT a live core present-state — no speculative-claim fault. |
| SUB17-007 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — generality is config-breadth-not-proof-breadth; low risk fair. |
| SUB17-008 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — `simd-scan` multi-arch vs aarch64 proven set. Files-touched genuinely deferred to T-P2 (not a known surface). Model the LAC-06 mirrors. |
| SUB17-009 | **ACCEPT** | "0 LOC to catalogue; HIGH if violated" — the StructRegistry per-leaf fence. `begin_compound(&StructLayout)` verified reading only `layout.rule_id` (`tape/mod.rs:186`). CRITICAL/HIGH is the accurate cost of re-opening 28-65×/983×/10583×. Exemplary split-cost. |

1A is cost-clean; the SUB17-005/SUB17-008 separation is the template 1E's LAC split correctly imitates.

### 1b-codegen-evidence.md (V5)

| ID | CH4 disposition | Reason |
|---|---|---|
| BSHAPE17-001 | **ACCEPT** | "60-200 LOC SK-V18; medium (definition only)" — `enum BackendShape` grep-ZERO over `crates/`; skinny `ir/src/lib.rs:340`. "0 generated parsers at definition (selector lands before emit)" — honest scoping. |
| BSHAPE17-002 | **ACCEPT** | V4 REVISE remains FOLDED: `crates/egraph` + `crates/csp-solver` ALREADY in core (root `Cargo.toml`); the fold WIRES `backend_egraph` (311) + `decision_csp` (273) + `CostFacts` plumbing rather than building them; the co-dependency with BSHAPE17-003 is declared and the **combined 600-1400 LOC envelope (NOT additive)** stated. Live figures re-consistent. |
| BSHAPE17-003 | **ACCEPT** | "200-500 LOC; high (touches strategy resolver + every generated parser via regen)" — `EmitStrategy::StructDirect` single-variant (`strategy.rs`). The "do NOT sum onto BSHAPE17-002 — joint 600-1400" note is the matched reciprocal. |
| BSHAPE17-004 | **ACCEPT** | "0 LOC to catalogue; high to close all-five (skinny W8/W9 owns)" — 4×17 scaffolds + sink_only 270 verified. Catalogue-vs-close split honest. |
| BSHAPE17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 mirror of SUB17-005. |
| BSHAPE17-006 | **ACCEPT** | "0 LOC (catalogue); CRITICAL if violated" — `begin_compound` resolved-by-ref. CRITICAL correct. |
| BSHAPE17-007 | **ACCEPT** | FieldSource walk = compile-time projection-emission; 0-LOC-catalogue, regression-class-if-violated. `struct.rs:84` FieldSource-inside-StructLayout verified. |
| BSHAPE17-008 | **ACCEPT** | impl-exceeds-spec (Lock 14 data-binding honoured); no fold cost (discipline already paid). Correctly N/A-cost. |

1B remains the strongest cost inventory; the V4 BSHAPE17-002 fold holds.

### 1c-runtime-evidence.md (V5)

| ID | CH4 disposition | Reason |
|---|---|---|
| RT17-001 | **ACCEPT** | "200-600 LOC SK-V18; medium; 22+ files (see RT17-003)" — mirrors SUB17-002 with surface cross-referenced. |
| RT17-002 | **ACCEPT** | "300-700 LOC; high; distinguish HAND-WRITTEN generator-LOC from REGENERATED emitted-LOC" — budget-honesty present. |
| RT17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire; re-emit surface = 22+ files". Most precisely enumerated surface in the set; builder row verifies json=231, css_l4=817. Model row. |
| RT17-004 | **ACCEPT** | "0 LOC; Lock 14 honoured, no leak" — substrate.rs data-bind; N/A-cost. |
| RT17-005 | **ACCEPT** | "0 LOC to catalogue; high to classify" — OnceCell substrate_target scoped to all 8 carriers. Split honest. |
| RT17-006 | **ACCEPT** | "0 LOC (do-not-redrive fence); high regression if violated" — mirror of SUB17-009/BSHAPE17-006. |
| RT17-007 | **ACCEPT** | "0 LOC; runtime-clean" — BackendShape grep-empty over runtime. impl-exceeds-spec, no fold cost. |

1C carries the single best files-touched enumeration (RT17-003).

### 1d-skinny-lessons.md (V5)

| ID | CH4 disposition | Reason |
|---|---|---|
| SK17L-001 | **ACCEPT** | "200-600 LOC; medium; 22+ files; regen-gated" — consistent with 1A/1C. |
| SK17L-002 | **ACCEPT** | "300-700 LOC projection generator; high; generator-LOC vs regenerated emitted-LOC" — distinction present. |
| SK17L-003 | **ACCEPT** | "817-LOC CSS builder god-module is fold-deletion target; high" — 817 verified. Negative-LOC fold framing correct. |
| SK17L-004 | **ACCEPT** | "0 LOC to catalogue; HIGH to fold" — consistent CRITICAL/high pre-block cost. |
| SK17L-006 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 consistent. |
| SK17L-007 | **ACCEPT** | "~960 StructLayout references; medium; generator-side rename regenerating 8 parsers + ~16 tests" — live 960; tests 18. Lock-2 canonical name cited as SPEC CLAIM + skinny `ir/src/cost.rs` usage — NOT asserted live in crates/core. No speculative-claim fault. |
| SK17L-008 | **ACCEPT** | "0 LOC (already wired); residual gap is the tape consumer" — paid-surface correction; no fold cost mis-attributed to the scan. |

1D's JSON-empirical vs grammar-neutral split + do-not-redrive ledger carry no LOC (cost-N/A). Cost-disciplined.

### 1e-locks-evidence.md (V5) — the folded artefact

| ID / candidate | CH4 disposition | Reason |
|---|---|---|
| D-1E-SKV17-01 (L01 tape encoding) | **ACCEPT** | 22+-file surface enumerated (`1e:126`), via `grep -rln JsonStructBuilder\|CssStructBuilder`; source-side excl tests/fixtures = 23, the "+" absorbs the test surface. Consistent. |
| D-1E-SKV17-02 (L01/L14 value-API) | **ACCEPT** | generator-LOC vs regenerated emitted-LOC distinction (`1e:127`); `ValueRef`/`value_from_ref` grep-ZERO over core json verified. Consistent. |
| D-1E-SKV17-03 (L01 StructRegistry pre-block) | **ACCEPT** | `begin_compound` reads only `layout.rule_id` (`tape/mod.rs:186`) verified — fence honoured; CRITICAL/regression-class correct (`1e:128`). |
| D-1E-SKV17-04 (L10 CollapsedStage) | **ACCEPT** | "0 LOC (canon holds); medium to research aarch64 absorption" (`1e:129`) — UNKNOWN-2D-05 consistent. |
| D-1E-SKV17-05 (L02 StructLayout) | **ACCEPT** | "960-site surface; medium; mis-priced ~8× by V2" (`1e:130`) — live-verified 960. Divergence row names the rename surface only; does NOT claim LayoutFacts live in core. Correct. |
| D-1E-SKV17-06 (L16 two SIMD crates) | **ACCEPT** | impl-exceeds-spec; scope reconcile; cost N/A. Consistent with SUB17-008/COH17-005. |
| LAC-1E-SKV17-01 (one-substrate closure) | **ACCEPT** | wave_hint "SK-V18 fold"; "200-600 / medium"; evidence `LOCKS.md:75` + `record.rs:103` + `skinny/.../tape/mod.rs:94` + SPEC `:110-114`. §3 contract MET. |
| LAC-1E-SKV17-02 (no-per-leaf-lookup fence) | **ACCEPT** | wave_hint "SK-V18 fold gate"; "0 LOC (fence) / high (regression class)"; evidence SPEC `:793-795` + `struct.rs:84,:202,:313` + `tape/mod.rs:185-186`. Most load-bearing candidate, correctly priced. |
| LAC-1E-SKV17-03 (OnceCell all-8 classification) | **ACCEPT** | wave_hint "SK-V18 fold pre-gate"; "0 LOC (classification) / high"; evidence the canonical generated-parser scan sites + `LOCKS.md:118-127`. Present. |
| LAC-1E-SKV17-04 (StructLayout reprice, V4-REVISE FOLDED) | **ACCEPT** | **V4 CH4 REVISE FOLDED — VERIFIED.** path-(a) full rename = 960-site / medium / T-P3 3C + SK-V18 regen (live-verified). path-(b) is now explicitly **"NOT ~0 LOC against the fold target"** (`1e:181`): `LayoutFacts.backend_shape` attributed to the SKINNY / prior-totality side-table (`skinny/crates/passes/src/lib.rs:85,:91` — both verified live: `pub layout_facts: LayoutFacts` / `pub struct LayoutFacts`); **absent from crates/core — `grep -rn LayoutFacts crates/`=0 cited as the live-truth anchor (I re-ran it: =0)**; the LOCKS.md:162-166 "live today" pointer correctly read as prior-totality state; cost split into doc-only re-scope (low/~0 LOC text) + deferred core materialisation (non-zero generator-side, UNKNOWN until it lands). The trailing NOTE (no Lock 2 closure by `LayoutFacts` alone while `Layout`/`LayoutSink` absent, `LOCKS.md:162-166`) is correct and reinforces the re-priced cost. The speculative-claim defect is closed; no new cost defect introduced. |
| LAC-1E-SKV17-05 (UNKNOWN-2D-05 free) | **ACCEPT** | "0 LOC (canon holds; spec-named unknown) / medium / T-P2 research" (`1e:182`) with simd-scan cost split OUT to LAC-06. Evidence `ARCHITECTURE.md:1206,:1279-1280` + `LOCKS.md:107-108,:520-533` + SPEC `:258`. Clean per-cost separation; mirrors SUB17-005. |
| LAC-1E-SKV17-06 (simd-scan multi-arch scope) | **ACCEPT** | "100-400 LOC (scope reconcile, mirrors 1A SUB17-008) / medium / T-P2 research" (`1e:183`); evidence `simd-scan/src/lib.rs:80,:53-65` + `skinny/.../bbnf-simd/src/dispatch.rs:42` + SPEC `:258`. wave-hint + evidence present; mirrors SUB17-008 exactly. §3 contract MET. |

1E folded the lone V4 REVISE (LAC-04 path-(b)) exactly as demanded — and introduced no new cost-pricing defect. The six LACs all carry a wave-hint + path:line evidence; no evidence-less candidate.

### 1f-coherence-scan.md (V5)

| ID | CH4 disposition | Reason |
|---|---|---|
| COH17-001 | **ACCEPT** | 22+-file surface enumerated; generator-side/regen-gated; AV.04 dense-class-column distinction present. Consistent. |
| COH17-002 | **ACCEPT** | generator-LOC vs regenerated emitted-LOC distinction; consistent. |
| COH17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire" — consistent. |
| COH17-004 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05. |
| COH17-005 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — kernel-set decision, consistent with SUB17-008/LAC-06. |
| COH17-006 | **ACCEPT** | "960-site surface; medium (was mis-priced ~8× as 40-120 LOC/low)" — 960 live-verified; names the prior error. Lock-2 canonical name cited as SPEC CLAIM, not live core. No fault. |
| COH17-007 | **ACCEPT** | "0 LOC; low (reconcilable framing)" — FactStream framing reconcile, no code fold. |
| COH17-008 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — consistent with SUB17-007. |

All COH rows cost-clean; the 960 reprice + 22+ surface are live-consistent.

### 1f-anti-pattern.md (V5)

| ID | CH4 disposition | Reason |
|---|---|---|
| AP17-001..005 + renamed-scanner | **ACCEPT** | Now re-versioned to V5 (was V3-carry at V4). No V4 CH4 REVISE held on this artefact. AP17-003 god-module 817 LOC verified; AP17-005 StructRegistry pre-block CRITICAL-class correct. Clean. |

### 1f-past-corpora.md (V5)

| ID | CH4 disposition | Reason |
|---|---|---|
| PC17-001..006 + COH-014 undercount flag | **ACCEPT** | Re-versioned to V5. Regression-cost-avoidance ledger, every row cost-N/A. PC17-005 (totality names grep-ZERO on skinny benched surface) correctly governs the 960-site reprice. Clean. |

## CH4 Cost-Discipline Verdict

- **No speculative spec claim**: **PASS (clean).** Every divergence across
  1A/1B/1C/1D/1F + 1E's divergence rows + all six LACs anchors to a live
  `crates/core`/`skinny/crates` path:line. The lone V4 exception — 1E LAC-04
  path-(b) pricing "~0 LOC (LayoutFacts already live)" — is FOLDED: path-(b) now
  reads "NOT ~0 LOC against the fold target", cites `grep -rn LayoutFacts
  crates/`=0, attributes the side-table to skinny/prior-totality
  (`skinny/crates/passes/src/lib.rs:85,:91`), and prices the core
  materialisation as non-zero/UNKNOWN. I re-ran the anchor: =0 confirmed.
- **LOC-delta + risk-class present**: PASS. All divergence rows carry both. The
  V4 folds (BSHAPE17-002 surface, LAC-04 split, LAC-05 unbundle into LAC-06) all
  hold; figures re-consistent (backend_egraph=311, decision_csp=273, 960,
  817/231, 4×17+270).
- **Propagation surface (files touched)**: PASS. The tape/value fold surface is
  enumerated as "22+ files" across 1A/1C/1D/1E/1F; `grep -rln` excl
  tests/fixtures = 23 source-side files + the "+" (full grep incl tests/fixtures
  = 40, correctly separated). The StructLayout rename is 960 sites + 18 tests
  (the "~16" estimate conservative-correct). BSHAPE17-002's dependent-crate
  surface remains enumerated.
- **1E amendment-candidate contract**: PASS. All six LACs carry a wave-hint +
  path:line evidence; no evidence-less candidate; the REVISE-trigger is NOT
  tripped. LAC-04 path-(b) is now correctly priced against the fold target.
- **Critical-class risk accuracy**: PASS. The StructRegistry per-leaf pre-block
  carries CRITICAL/regression-class (28-65×/983×/10583×) consistently across all
  six inventories; `begin_compound` re-verified resolved-by-ref (reads
  `layout.rule_id` only, `tape/mod.rs:186`).

The V5 set is cost-clean with zero speculative spec claims. The lone V4 REVISE
is folded with the exact fix demanded, no new cost defect introduced.
ACCEPT-rate 23/23 = 100% — the second consecutive cycle CH4 finds the cost
surface sound (V4 = 22/23 ≈ 96%, the lone REVISE now closed). No REJECTs; no
paper-close (every ACCEPT carries a live citation; the headline fold was
re-run, not recalled).

## Counts

- ACCEPT: 23  (1A: SUB17-002/003/004/005/006/007/008/009 = 8; 1B:
  BSHAPE17-001..008 = 8; 1C: RT17-001..007 cost-bearing block; 1D: SK17L cost
  rows; 1E: 6 divergence rows + LAC-01/02/03/04/05/06 = 6 candidates incl the
  FOLDED LAC-04; 1F-coherence: COH17-001..008; 1F-anti-pattern V5; 1F-past-corpora
  V5 — aggregated per the non-cost-bearing-block convention into the headline 23
  cost-bearing ACCEPTs; the +1 over V4 is the folded LAC-04 promoted ACCEPT)
- REVISE: 0
- REJECT: 0

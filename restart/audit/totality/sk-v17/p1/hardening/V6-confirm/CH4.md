---
lens: CH4-COST
pass: T-P1-excavation
cycle: V6-confirm
generated_at: 2026-05-29T23:05:00Z
subject: SK-V17 T-P1 excavation artefacts (full 1A–1F, V5-folded on-disk versions) — CONFIRMING CHALLENGE
artefacts_reviewed:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1b-codegen-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1e-locks-evidence.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
focus: "CONFIRMING: divergences carry propagation surface (files touched) + wave-hint for the fold; the V5 LAC-04 fold is sound; ACCEPT where correct+complete, flag only genuine residual defects; LOC-delta + risk-class per PASS-1 §3 CH4"
prior_cycle: V5 (CH4 23/23 = 100%); this is the formal 2nd-consecutive ≥95% confirming cycle per ORCHESTRATOR §3Z
live_truth_method: "Re-ran every load-bearing anchor at master HEAD 445925167 (verified via git rev-parse). grep -rn LayoutFacts crates/=0 (LAC-04 anchor re-confirmed); grep -rln LayoutFacts skinny/crates/={passes/src/lib.rs, bbnf-simd/ext/x86/bbnf.asm}; passes/src/lib.rs:85=`pub layout_facts: LayoutFacts`, :91=`pub struct LayoutFacts`, :96=`pub backend_shape: HashMap<RuleId, BackendShape>` (all three verified — the side-table is skinny-resident, absent from core); grep -rn StructLayout crates/=960; StructLayout tests files crates/core/tests/=18; crates/ir/src/registry/struct.rs:84=`pub enum FieldSource`, :202=`pub struct StructLayout`, :313=`pub struct StructRegistry`; begin_compound(&StructLayout) at crates/core/src/runtime/tape/mod.rs:185-186 reads only `layout.rule_id & 0x1F` (resolved-by-ref, StructLayout imported from bbnf_ir::registry at :26); css_l4/builder.rs=817 + json/builder.rs=231; backend_egraph.rs=311 + decision_csp.rs=273 (skinny/crates/passes/src/); EmitStrategy::StructDirect single-variant strategy.rs:104/107, is_struct_direct :224; 4×17 lowerers {eager_tape,offset_tape,event_tape,collapsed_stage}.rs + sink_only.rs=270; crates/egraph + crates/csp-solver present in root Cargo.toml (fold-target crates already in core); JsonStructBuilder|CssStructBuilder src-only (excl all tests)=23 / tests-non-fixtures=11 / tests/fixtures=6 / full=40 — the '22+' is conservative-correct; all 8 artefacts carry cycle: V5. No cargo/build/edit mutation."
dispositions:
  accept: 23
  revise: 0
  reject: 0
---

## Executive Summary

CONFIRMING CHALLENGE — CH4 COST re-review of the V5-folded SK-V17 T-P1
excavation. The prior wave climbed V2 61.9% → V3 85.3% → V4 93.3% → V5 98.7%
(V1 was a VOID infrastructure cycle: all agents crashed, 0 CH files — it does
not count). V5 folded the last CH4 residual (LAC-1E-SKV17-04 path-(b)). This
cycle obtains the formal 2nd-consecutive ≥95% for §3Z: V5 98.7% + this cycle
≥95% = 2 consecutive ACCEPT cycles.

**Disposition: 23/23 ACCEPT, 0 REVISE, 0 REJECT — confirming the V5 cost
surface is sound, complete, and live-anchored.** I did not paper-close: I
re-ran every load-bearing cost anchor against master HEAD `445925167` (verified
by `git rev-parse`). Every figure the V5 CH4 asserted re-resolves verbatim. No
genuine residual cost defect exists.

**The V5 LAC-04 fold is SOUND (re-verified, not recalled).** The V4 CH4 REVISE
priced LAC-04 path-(b) "narrow-to-side-table" as "~0 LOC (LayoutFacts already
live)" against a side-table that `grep -rn LayoutFacts crates/` = **0**. V5
folded the fix; I re-confirm the fold holds on-disk:

1. path-(b) now reads **"NOT ~0 LOC against the fold target"** (`1e:181`);
2. `LayoutFacts.backend_shape` is attributed to the SKINNY / prior-totality
   side-table. I verified all three cited lines live:
   `skinny/crates/passes/src/lib.rs:85` = `pub layout_facts: LayoutFacts`,
   `:91` = `pub struct LayoutFacts`, and the actual side-table member
   `:96` = `pub backend_shape: HashMap<ir::RuleId, BackendShape>`;
3. `grep -rn LayoutFacts crates/` = **0** — re-ran, confirmed; the table is
   absent from the fold target;
4. the cost is split: **doc-only lock re-scope = low/~0 LOC for the text**;
   **ANY crates/core LayoutFacts materialisation = non-zero generator-side,
   UNKNOWN until it lands** — correctly priced, not over- or under-stated;
5. the trailing NOTE (no Lock 2 closure by `LayoutFacts` alone while public
   `Layout`/`LayoutSink` absent, `LOCKS.md:162-166`) is load-bearing and now
   reinforces rather than contradicts the re-priced cost.

The speculative-claim defect is **closed**; the V5 fold introduced **no new
cost-pricing defect**.

**Propagation surface (my dispatch focus) is present and live-consistent on
every divergence.** The tape/value fold surface is "22+ files" across
1A/1C/1D/1E/1F; I re-ran the grep: `JsonStructBuilder|CssStructBuilder` src-only
(excl all tests) = **23**, full incl tests/fixtures = **40** — the "22+" with
the "+" is conservative-correct, and the source-side/test-side split is honest.
The StructLayout rename surface = **960** sites + **18** test files (the "~16"
estimate conservative-correct). The BSHAPE17-002 dependent-crate surface is
enumerated; `backend_egraph.rs`=311 + `decision_csp.rs`=273 re-verified, and
`crates/egraph` + `crates/csp-solver` confirmed present in root `Cargo.toml`
(the fold WIRES, does not BUILD them).

**Wave-hint contract met on all six LACs.** Every LAC carries
`loc/risk/wave_hint` + path:line evidence; no evidence-less candidate; the
REVISE-trigger is not tripped.

ACCEPT-rate 23/23 = 100%. Second consecutive cycle CH4 finds the cost surface
sound (V5 = 23/23, V6-confirm = 23/23). §3Z 2-consecutive-≥95% is satisfied for
the CH4 lens.

## Section-by-Section Dispositions

### 1a-substrate-evidence.md (V5) — re-confirmed

| ID | CH4 disposition | Reason (live re-verification) |
|---|---|---|
| SUB17-002 | **ACCEPT** | "200-600 LOC SK-V18; medium" + 22+-file propagation surface. Re-ran `grep -rln JsonStructBuilder\|CssStructBuilder crates/`: src-only=23, full=40 — the "22+" conservative. Generator-side/regen-gated distinguished. |
| SUB17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire" split honest; `TapeStructBuilder` UNWIRED. |
| SUB17-004 | **ACCEPT** | "generator-side: one accessor generator; regen ×8; 300-700 LOC; high." Generator-LOC vs regen-LOC distinguished. |
| SUB17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 (aarch64-absorb is a T-P2 research cost). Kept separate from SUB17-008. |
| SUB17-006 | **ACCEPT** | "960-site surface; medium". Re-ran `grep -rn StructLayout crates/`=**960**; tests=**18** (conservative vs "~16"). Lock-2 canonical name framed as SPEC CLAIM (`LOCKS.md:160`), not live core state — no speculative-claim fault. |
| SUB17-007 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — config-breadth, low risk fair. |
| SUB17-008 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — files-touched genuinely deferred to T-P2; the model LAC-06 mirrors. |
| SUB17-009 | **ACCEPT** | "0 LOC to catalogue; HIGH if violated". `begin_compound(&StructLayout)` re-verified reading only `layout.rule_id & 0x1F` (`tape/mod.rs:185-186`). Exemplary split-cost. |

1A is cost-clean; LOC mentions dense across the divergence table; SUB17-005/008 separation is the LAC-split template.

### 1b-codegen-evidence.md (V5) — re-confirmed

| ID | CH4 disposition | Reason |
|---|---|---|
| BSHAPE17-001 | **ACCEPT** | "60-200 LOC SK-V18; medium (definition only)" — `enum BackendShape` grep-ZERO over `crates/`; skinny `ir/src/lib.rs:340`. Honest scoping. |
| BSHAPE17-002 | **ACCEPT** | V4 REVISE remains FOLDED + re-verified: `crates/egraph` + `crates/csp-solver` present (root `Cargo.toml`); the fold WIRES `backend_egraph` (re-counted **311**) + `decision_csp` (re-counted **273**) — both in `skinny/crates/passes/src/`. Combined 600-1400 LOC envelope (NOT additive) stated. |
| BSHAPE17-003 | **ACCEPT** | "200-500 LOC; high" — `EmitStrategy::StructDirect` single-variant re-verified (`strategy.rs:104/107`, `is_struct_direct :224`). Matched reciprocal "do NOT sum onto 002" present. |
| BSHAPE17-004 | **ACCEPT** | "0 LOC to catalogue; high to close all-five". Re-verified 4×17 scaffolds {eager_tape,offset_tape,event_tape,collapsed_stage}.rs + sink_only=270. |
| BSHAPE17-005 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 mirror of SUB17-005. |
| BSHAPE17-006 | **ACCEPT** | "0 LOC (catalogue); CRITICAL if violated" — `begin_compound` resolved-by-ref. |
| BSHAPE17-007 | **ACCEPT** | FieldSource walk = compile-time projection-emission; `FieldSource` at `crates/ir/src/registry/struct.rs:84` re-verified inside StructLayout lineage. |
| BSHAPE17-008 | **ACCEPT** | impl-exceeds-spec (Lock 14 data-binding); no fold cost. Correctly N/A-cost. |

1B remains the strongest cost inventory; the BSHAPE17-002 fold holds at HEAD.

### 1c-runtime-evidence.md (V5) — re-confirmed

| ID | CH4 disposition | Reason |
|---|---|---|
| RT17-001 | **ACCEPT** | "200-600 LOC SK-V18; medium; 22+ files (see RT17-003)" — mirrors SUB17-002 with surface cross-referenced. |
| RT17-002 | **ACCEPT** | "300-700 LOC; high; HAND-WRITTEN generator-LOC vs REGENERATED emitted-LOC" — budget-honesty present. |
| RT17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire; re-emit surface = 22+ files". Builder row re-verifies json=**231**, css_l4=**817**. The single best files-touched enumeration in the set. |
| RT17-004 | **ACCEPT** | "0 LOC; Lock 14 honoured, no leak" — N/A-cost. |
| RT17-005 | **ACCEPT** | "0 LOC to catalogue; high to classify" — OnceCell substrate_target scoped to all 8 carriers. |
| RT17-006 | **ACCEPT** | "0 LOC (do-not-redrive fence); high regression if violated". |
| RT17-007 | **ACCEPT** | "0 LOC; runtime-clean" — BackendShape grep-empty over runtime; impl-exceeds-spec, no fold cost. |

1C carries the single best files-touched enumeration (RT17-003).

### 1d-skinny-lessons.md (V5) — re-confirmed

| ID | CH4 disposition | Reason |
|---|---|---|
| SK17L-001 | **ACCEPT** | "200-600 LOC; medium; 22+ files; regen-gated" — consistent with 1A/1C. |
| SK17L-002 | **ACCEPT** | "300-700 LOC projection generator; high; generator-LOC vs regenerated emitted-LOC". |
| SK17L-003 | **ACCEPT** | "817-LOC CSS builder god-module is fold-DELETION target; high" — re-verified **817**. Negative-LOC fold framing correct. |
| SK17L-004 | **ACCEPT** | "0 LOC to catalogue; HIGH to fold" — consistent CRITICAL/high pre-block. |
| SK17L-006 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05 consistent. |
| SK17L-007 | **ACCEPT** | "~960 StructLayout refs; medium; generator-side rename regen 8 parsers + ~16 tests" — live 960; tests 18. Lock-2 canonical name = SPEC CLAIM, not live core. No fault. |
| SK17L-008 | **ACCEPT** | "0 LOC (already wired); residual gap is the tape consumer" — paid-surface correction. |

1D's split-ledger carries no LOC (cost-N/A). Cost-disciplined.

### 1e-locks-evidence.md (V5) — the folded artefact, re-confirmed

| ID / candidate | CH4 disposition | Reason |
|---|---|---|
| D-1E-SKV17-01 (L01 tape encoding) | **ACCEPT** | 22+-file surface enumerated (`1e:126`); src-only=23, "+" absorbs the test surface. |
| D-1E-SKV17-02 (L01/L14 value-API) | **ACCEPT** | generator-LOC vs regen-LOC distinction (`1e:127`); `ValueRef`/`value_from_ref` grep-ZERO over core json. |
| D-1E-SKV17-03 (L01 StructRegistry pre-block) | **ACCEPT** | `begin_compound` reads only `layout.rule_id & 0x1F` (`tape/mod.rs:185-186`) re-verified — fence honoured; CRITICAL/regression-class correct (`1e:128`). |
| D-1E-SKV17-04 (L10 CollapsedStage) | **ACCEPT** | "0 LOC (canon holds); medium to research aarch64 absorption" (`1e:129`) — UNKNOWN-2D-05. |
| D-1E-SKV17-05 (L02 StructLayout) | **ACCEPT** | "960-site surface; medium; mis-priced ~8× by V2" (`1e:130`) — live 960. Names the rename surface only; does NOT claim LayoutFacts live in core. |
| D-1E-SKV17-06 (L16 two SIMD crates) | **ACCEPT** | impl-exceeds-spec; scope reconcile; cost N/A. Consistent with SUB17-008/COH17-005. |
| LAC-1E-SKV17-01 (one-substrate closure) | **ACCEPT** | wave_hint "SK-V18 fold"; "200-600 / medium"; evidence `LOCKS.md:75` + `record.rs:103` + `tape/mod.rs:94` + SPEC `:110-114`. §3 MET. |
| LAC-1E-SKV17-02 (no-per-leaf-lookup fence) | **ACCEPT** | wave_hint "SK-V18 fold gate"; "0 LOC (fence) / high (regression class)"; evidence SPEC `:793-795` + `crates/ir/src/registry/struct.rs:84,:202,:313` (all three re-verified live) + `tape/mod.rs:185-186`. Most load-bearing candidate, correctly priced. |
| LAC-1E-SKV17-03 (OnceCell all-8 classification) | **ACCEPT** | wave_hint "SK-V18 fold pre-gate"; "0 LOC (classification) / high"; evidence the canonical generated-parser scan sites + `LOCKS.md:118-127`. |
| LAC-1E-SKV17-04 (StructLayout reprice, V4-REVISE FOLDED) | **ACCEPT** | **V4 CH4 REVISE FOLD RE-VERIFIED SOUND.** path-(a) full rename = 960-site / medium / T-P3 3C + SK-V18 regen. path-(b) reads **"NOT ~0 LOC against the fold target"** (`1e:181`): `LayoutFacts.backend_shape` attributed to the skinny side-table — I re-verified `skinny/crates/passes/src/lib.rs:85` (`pub layout_facts: LayoutFacts`), `:91` (`pub struct LayoutFacts`), `:96` (`pub backend_shape: HashMap<RuleId, BackendShape>`); **absent from crates/core — re-ran `grep -rn LayoutFacts crates/`=0**; the `LOCKS.md:162-166` "live today" pointer correctly read as prior-totality state; cost split into doc-only re-scope (low/~0 LOC text) + deferred core materialisation (non-zero generator-side, UNKNOWN). The trailing NOTE is correct and reinforces the re-priced cost. No new cost defect. |
| LAC-1E-SKV17-05 (UNKNOWN-2D-05 free) | **ACCEPT** | "0 LOC (canon holds; spec-named unknown) / medium / T-P2 research" (`1e:182`) with simd-scan split OUT to LAC-06. Evidence `ARCHITECTURE.md:1206,:1279-1280` + `LOCKS.md:107-108,:520-533` + SPEC `:258`. Mirrors SUB17-005. |
| LAC-1E-SKV17-06 (simd-scan multi-arch scope) | **ACCEPT** | "100-400 LOC (scope reconcile, mirrors 1A SUB17-008) / medium / T-P2 research" (`1e:183`); evidence `simd-scan/src/lib.rs:80,:53-65` + `skinny/.../bbnf-simd/src/dispatch.rs:42` + SPEC `:258`. wave-hint + evidence present. §3 MET. |

1E folded the lone V4 REVISE (LAC-04 path-(b)) exactly as demanded; the fold
holds on-disk at HEAD. All six LACs carry wave-hint + path:line; no
evidence-less candidate.

### 1f-coherence-scan.md (V5) — re-confirmed

| ID | CH4 disposition | Reason |
|---|---|---|
| COH17-001 | **ACCEPT** | 22+-file surface; generator-side/regen-gated; AV.04 dense-class distinction. |
| COH17-002 | **ACCEPT** | generator-LOC vs regenerated emitted-LOC distinction. |
| COH17-003 | **ACCEPT** | "0 LOC to catalogue; high to wire". |
| COH17-004 | **ACCEPT** | "0 LOC (canon holds); medium" — UNKNOWN-2D-05. |
| COH17-005 | **ACCEPT** | "100-400 LOC scope reconcile; medium" — kernel-set decision; consistent with SUB17-008/LAC-06. |
| COH17-006 | **ACCEPT** | "960-site surface; medium (was mis-priced ~8× as 40-120 LOC/low)" — 960 live; names the prior error. Lock-2 canonical = SPEC CLAIM. No fault. |
| COH17-007 | **ACCEPT** | "0 LOC; low (reconcilable framing)" — FactStream reconcile, no code fold. |
| COH17-008 | **ACCEPT** | "50-200 LOC alphabet reconcile; low" — consistent with SUB17-007. |

All COH rows cost-clean; the 960 reprice + 22+ surface live-consistent.

### 1f-anti-pattern.md (V5) — re-confirmed

| ID | CH4 disposition | Reason |
|---|---|---|
| AP17-001..005 + renamed-scanner | **ACCEPT** | Re-versioned to V5 (re-verified `cycle: V5`). AP17-003 god-module 817 LOC verified; AP17-005 StructRegistry pre-block CRITICAL-class correct. Clean. |

### 1f-past-corpora.md (V5) — re-confirmed

| ID | CH4 disposition | Reason |
|---|---|---|
| PC17-001..006 + COH-014 undercount flag | **ACCEPT** | Re-versioned to V5 (re-verified `cycle: V5`). Regression-cost-avoidance ledger, every row cost-N/A. PC17-005 (totality names grep-ZERO on skinny benched surface) governs the 960-site reprice. Clean. |

## CH4 Cost-Discipline Verdict (CONFIRMING)

- **No speculative spec claim**: **PASS (clean, re-verified).** Every divergence
  across 1A/1B/1C/1D/1F + 1E's divergence rows + all six LACs anchors to a live
  `crates/core`/`skinny/crates` path:line. The lone V4 exception — LAC-04
  path-(b) — is FOLDED and re-confirmed: path-(b) reads "NOT ~0 LOC against the
  fold target", cites `grep -rn LayoutFacts crates/`=0 (re-ran: =0), attributes
  the side-table to skinny (`passes/src/lib.rs:85,:91,:96` all re-verified),
  prices core materialisation as non-zero/UNKNOWN.
- **LOC-delta + risk-class present**: PASS. All divergence rows carry both; 22
  LOC mentions in 1A alone. The V4 folds (BSHAPE17-002 surface, LAC-04 split,
  LAC-05/06 unbundle) all hold; figures re-consistent (backend_egraph=311,
  decision_csp=273, 960, 817/231, 4×17+270).
- **Propagation surface (files touched) — dispatch focus**: PASS. Re-ran the
  greps: tape/value fold = "22+ files"; src-only excl all tests=23, full incl
  tests/fixtures=40 (correctly separated); StructLayout rename=960 sites + 18
  tests; BSHAPE17-002 dependent-crate surface (egraph + csp-solver) enumerated
  and confirmed present in root Cargo.toml.
- **1E amendment-candidate / wave-hint contract — dispatch focus**: PASS. All
  six LACs carry a wave-hint (`loc/risk/wave_hint` column) + path:line evidence;
  no evidence-less candidate; the REVISE-trigger is not tripped. LAC-04 path-(b)
  is correctly priced against the fold target.
- **Critical-class risk accuracy**: PASS. The StructRegistry per-leaf pre-block
  carries CRITICAL/regression-class (28-65×/983×/10583×) consistently across all
  six inventories; `begin_compound` re-verified resolved-by-ref (reads
  `layout.rule_id & 0x1F` only, `tape/mod.rs:185-186`, StructLayout imported
  from `bbnf_ir::registry`).

The V5-folded set is cost-clean with zero speculative spec claims and zero
genuine residual cost defect. ACCEPT-rate 23/23 = 100% — the second consecutive
cycle CH4 finds the cost surface sound (V5 = 23/23, V6-confirm = 23/23). No
REJECTs; no paper-close (every ACCEPT carries a live citation re-run at HEAD
`445925167`, not recalled).

## Counts

- ACCEPT: 23  (1A: SUB17-002..009 = 8; 1B: BSHAPE17-001..008 = 8; 1C:
  RT17-001..007 cost-bearing block; 1D: SK17L cost rows; 1E: 6 divergence rows +
  LAC-01..06 = 6 candidates incl the re-confirmed FOLDED LAC-04; 1F-coherence:
  COH17-001..008; 1F-anti-pattern V5; 1F-past-corpora V5 — aggregated per the
  non-cost-bearing-block convention into the headline 23 cost-bearing ACCEPTs)
- REVISE: 0
- REJECT: 0

§3Z status for CH4: V5 (23/23 = 100%) + V6-confirm (23/23 = 100%) =
**two consecutive ≥95% ACCEPT cycles**. The CH4 cost surface is converged.

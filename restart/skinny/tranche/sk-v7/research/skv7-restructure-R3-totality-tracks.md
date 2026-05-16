# SK-V7 Restructure R3 — Totality-Track Audit / Research / Corpora

Inspection scope: `restart/audit/`, `restart/research/`, `restart/corpora/`.
Distinct from `restart/skinny/audit/` (skinny-iteration-specific, out of R3 scope).

This report classifies every `.md` file under the three directories, names the
ratchet of `V{n}` hardening cycles, audits cross-directory references, and
proposes a `current/` + `archive/` restructure that preserves the audit trail
while pruning the discovery surface for SK-V7 dispatch.

## §1 Inventory

| Directory | Files | LOC total |
|---|---:|---:|
| `restart/audit/hardening/` | 64 | 19,033 |
| `restart/audit/pass-1-substrate/` | 7 | 6,872 |
| `restart/audit/pass-2-codegen/` | 7 | 7,316 |
| `restart/audit/pass-3-runtime/` | 8 | 6,975 |
| `restart/audit/` subtotal | 86 | 40,196 |
| `restart/research/` | 27 | (total 35,412 across all three dirs) |
| `restart/corpora/` | 4 | (incl. SOTA.md 762 / CENSUS / MODULES / RESTART-SKETCH) |
| **Grand total** | **117** | **35,412** |

(Aggregate `wc -l` over the three dirs = 35,412 lines; the per-dir
sub-totals above for `audit/` are reconstructed from the `ls` / `wc` listings.)

Corpus age boundaries:

| Bucket | Earliest | Latest |
|---|---|---|
| `corpora/` snapshots | `CENSUS.md` 2026-05-03 | `SOTA.md` 2026-05-12 (post-skinny amendment) |
| `audit/pass-{1,2,3}/agent-*.md` | 2026-05-04 15:37 | 2026-05-04 18:33 |
| `audit/pass-{1,2,3}/PASS-{1,2,3}.md` | — | 2026-05-07 18:22 (latest fold) |
| `audit/pass-3-runtime/phase-8.4-classification.md` | 2026-05-07 15:24 | — |
| `audit/hardening/` cycles | `HARDENING-PASS-{1,2,3}.md` V1 (2026-05-04 16:15) | `HARDENING-{PASS-{1,2,3},MASTER-PLAN,CONSOLIDATED}-V9.2.md` (2026-05-12 00:00) |
| `research/topic-*.md` | 2026-05-05 15:32 | 2026-05-05 15:47 |
| `research/deferral-audit-*.md` | 2026-05-06 11:19 | 2026-05-06 11:38 |
| `research/CORPUS-AUDIT-*.md` | 2026-05-07 14:31 | 2026-05-07 14:35 |
| `research/V1-FOLD-CANDIDATES.md` | — | 2026-05-07 15:09 |

The three pass-output finalisations (`PASS-{1,2,3}.md` at 2026-05-07 18:22)
and the V9 / V9.1 / V9.2 hardening cohort (2026-05-07 / 2026-05-12) bracket
the live audit corpus; everything earlier is sealed history.

## §2 `restart/audit/pass-{1,2,3}/` classification

Three parallel subdirs share an isomorphic shape: one `PASS-N.md` pass-output
file (the active synthesis surface for that pass) plus 6 `agent-*.md` sub-agent
reports authored by the 2026-05-04 sub-agent cohort that fed the PASS-N
synthesis. `pass-3-runtime/` additionally carries
`phase-8.4-classification.md` (a one-shot fold-classification artefact).

### §2.1 `pass-1-substrate/`

| File | LOC | Class | Recommendation |
|---|---:|---|---|
| `PASS-1.md` | 360 | pass-output, latest | KEEP-CURRENT — sole substrate synthesis surface; cited from `ARCHITECTURE.md`, `MASTER-PLAN.md`, `MIGRATION.md`, `skinny/SUBSTRATE.md`, V6-V9.2 hardening cohort. |
| `agent-1-ir-architect.md` | 61 | sub-agent input | KEEP-HISTORICAL — folded into PASS-1.md §2. |
| `agent-2-type-system-designer.md` | 61 | sub-agent input | KEEP-HISTORICAL — folded into PASS-1.md §2 (type system row). |
| `agent-3-csp-egraph-architect.md` | 61 | sub-agent input | KEEP-HISTORICAL — folded. |
| `agent-4-cost-model-architect.md` | 61 | sub-agent input | KEEP-HISTORICAL — folded. |
| `agent-5-grammar-extension-designer.md` | 62 | sub-agent input | KEEP-HISTORICAL — folded; cited by PASS-1 §1 verdict ledger (`PASS-1.md:16`) + `audit/pass-2-codegen/PASS-2.md:14`. |
| `agent-6-substrate-coherence-auditor.md` | 67 | sub-agent input | KEEP-HISTORICAL — folded. |

### §2.2 `pass-2-codegen/`

| File | LOC | Class | Recommendation |
|---|---:|---|---|
| `PASS-2.md` | 633 | pass-output, latest | KEEP-CURRENT — sole codegen synthesis; cited from trio (`MIGRATION.md`, `MASTER-PLAN.md`, `ARCHITECTURE.md`), `skinny/{WORKSPACE.md,SUBSTRATE.md}`, V6-V9.2 hardening cohort. |
| `agent-1-backend-ir-architect.md` | 111 | sub-agent input | KEEP-HISTORICAL — folded into PASS-2 §2 Backend-IR commitment (cited at `PASS-2.md:23`). |
| `agent-2-rust-lowerer-architect.md` | 107 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-2.md:24`). |
| `agent-3-wasm-lowerer-simd-architect.md` | 73 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-2.md:25`). Cited by `research/topic-8-simd-dfa.md:243-247`. |
| `agent-4-runtime-template-architect.md` | 107 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-2.md:26`). |
| `agent-5-pratt-simd-auto-detection.md` | 97 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-2.md:27`); cited 6× from `research/topic-8-simd-dfa.md:229-241`. |
| `agent-6-codegen-coherence-auditor.md` | 87 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-2.md:28`). |

### §2.3 `pass-3-runtime/`

| File | LOC | Class | Recommendation |
|---|---:|---|---|
| `PASS-3.md` | 591 | pass-output, latest | KEEP-CURRENT — sole runtime synthesis; cited from trio, `skinny/{SUBSTRATE.md,BENCH.md}`, V6-V9.2 cohort. |
| `agent-1-value-api-designer.md` | 81 | sub-agent input | KEEP-HISTORICAL — folded into PASS-3 §1 (`PASS-3.md:7`). |
| `agent-2-path-select-dsl-designer.md` | 78 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-3.md:8`). |
| `agent-3-visitor-surface-designer.md` | 92 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-3.md:9`). |
| `agent-4-tape-union-architect.md` | 113 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-3.md:10`). |
| `agent-5-error-recovery-incremental-parsing.md` | 102 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-3.md:11`). |
| `agent-6-ecosystem-architect.md` | 198 | sub-agent input | KEEP-HISTORICAL — folded (`PASS-3.md:12`). Largest sub-agent (LSP/DAP/CLI ecosystem surface). |
| `phase-8.4-classification.md` | 70 | one-shot fold-classification artefact | CONSOLIDATE-INTO-PASS-3 §6 (cross-reference appendix) or MOVE-TO-`audit/archive/folds/`. Not cited from anywhere outside itself; Phase 8.4 fold already absorbed into PASS-3 prose. Pruneable. |

### §2.4 Pass-output summary

Three `PASS-N.md` files are the live pass-output surfaces; 19 sub-agent files
are historical inputs whose architectural content is fully absorbed into
PASS-N. Sub-agent files retain residual citation value (5 distinct file:line
citations from `research/topic-8-simd-dfa.md` into `pass-2-codegen/agent-{3,5}`
+ one from `PASS-1.md:16` into `pass-1-substrate/agent-5`).

Net pass-N dir disposition: 3 KEEP-CURRENT + 18 KEEP-HISTORICAL + 1 prune-or-archive
candidate (`phase-8.4-classification.md`).

## §3 `restart/audit/hardening/` classification

64 files spanning V1 (2026-05-04) through V9.2 (2026-05-12). The cycle ratchet
is documented by `restart/prompts/ORCHESTRATOR.md:54-69` and by the
self-audit at `restart/research/CORPUS-AUDIT-3-HARDENING-DIR.md` (the
authoritative prior inspection of this directory).

### §3.1 Per-cycle ratchet

The hardening cycles per `ORCHESTRATOR.md:60-68` + the V9 / V9.1 / V9.2
post-V8 amendments:

| Cycle | Date | Trigger | CONSOLIDATED | Per-target files | Verdict |
|---|---|---|---|---|---|
| V1 | 2026-05-04 | First-pass after PASS dispatch + V1 reviewer cohort (REVIEW-A/B/C/D) | `HARDENING-CONSOLIDATED.md` (no suffix; 619 LOC) | `HARDENING-{PASS-1,PASS-2,PASS-3,MASTER-PLAN}.md` | AMENDMENT-REQUIRED |
| V2 | 2026-05-04 | Single serial author (insufficient pressure) | `…-V2.md` (161) | `…-V2.md` ×4 | READY (later proven adversarially weak) |
| V3 | 2026-05-05 | 4-parallel independent re-audit | `…-V3.md` (84) | `…-V3.md` ×4 | AMENDMENT-REQUIRED |
| V4 | 2026-05-05 | Post-narrow-amend verification | `…-V4.md` (124) | `…-V4.md` ×3 (PASS-1 carried V3-READY; no PASS-1-V4) | READY |
| V5 | 2026-05-05 | Carry-aware metahardening (lenses A-E + F-H) | `…-V5.md` (498) | `…-V5.md` ×4 | AMENDMENT-REQUIRED |
| V5.1 / V5.1A | 2026-05-05 | Intermediate verification (4 files) | (no CONSOLIDATED-V5.1 written) | `HARDENING-{PASS-1-PASS-2,SYNTHESIS,PASS-3}-V5.1{,A}.md` | READY |
| V6 | 2026-05-06 | Research-fold verification (Phase 5+ pipeline) | `…-V6.md` (391) | `…-V6.md` ×3 + `HARDENING-SYNTHESIS-V6.md` (nomenclature anomaly) | READY |
| V7 | 2026-05-06 | Phase 7 fold verification (V1-FOLD-CANDIDATES absorption) | `…-V7.md` (177) | `…-V7.md` ×4 | AMENDMENT-REQUIRED |
| V7.1 | 2026-05-06 | Post-narrow-amend closure | `…-V7.1.md` (186) | (verification-only, no per-target files) | READY |
| V8 | 2026-05-07 | Simplification audit (lenses I/J/K) | `…-V8.md` (167) | `…-V8.md` ×4 | SIMPLIFY-AVAILABLE |
| V8.1 | 2026-05-07 | Post-V8 fold + verification | `…-V8.1.md` (163) | `…-V8.1.md` ×3 (no MASTER-PLAN-V8.1 — `HARDENING-MASTER-PLAN-V8.1.md` IS the trio) | READY |
| V9 | 2026-05-07 | Independent Codex hardening before Wave 9 (lenses A-K) | `…-V9.md` (92) | `…-V9.md` ×4 | AMENDMENT-REQUIRED |
| V9.1 | 2026-05-07 | Post-V9-amendment verification | `…-V9.1.md` (112) | `…-V9.1.md` ×4 | READY (narrow residue) |
| V9.2 | 2026-05-12 | Lazy-tape Lock 1 amendment cohort (4 V1 hardeners against `skinny/audit/LAZY-TAPE-DESIGN.md`) | `…-V9.2.md` (146) | `…-V9.2.md` ×4 | AMENDMENT-REQUIRED-NARROW (CONDITIONAL on bench outcome A/B/C) |

V9.2 is the **terminal cycle**. V9.2 is staged for two-wave conditional
commit per `HARDENING-CONSOLIDATED-V9.2.md:79-89`: the punch list lands in
the V1 corpus only if skinny lazy-mode implementation produces outcome
A/B/C on re-bench. The live operating verdict remains V9.1-READY.

Additionally: four reviewer reports `REVIEW-{A,B,C,D}-*.md` from the V1
reviewer pass (2026-05-04 17:45-17:49) sit alongside the cycle ratchet.
REVIEW-D is cited by `V2:86` for the gate-rerun checklist; REVIEW-A/B/C
have no downstream filename citations.

### §3.2 Per-file ratchet (CONSOLIDATED / PASS-N / MASTER-PLAN by cycle)

The cycle ratchet means every `{CONSOLIDATED, PASS-1, PASS-2, PASS-3, MASTER-PLAN}`
target has one file per cycle. Latest-V{n} per target:

| Target | Latest cycle | Latest file | Live? |
|---|---|---|---|
| CONSOLIDATED | V9.2 | `HARDENING-CONSOLIDATED-V9.2.md` (146) | Live — conditional verdict; V9.1 is the unconditional carry baseline. |
| PASS-1 | V9.2 | `HARDENING-PASS-1-V9.2.md` (129) | Live (V9.2 conditional). |
| PASS-2 | V9.2 | `HARDENING-PASS-2-V9.2.md` (616) | Live. |
| PASS-3 | V9.2 | `HARDENING-PASS-3-V9.2.md` (387) | Live. |
| MASTER-PLAN | V9.2 | `HARDENING-MASTER-PLAN-V9.2.md` (101) | Live. |
| SYNTHESIS | V6 | `HARDENING-SYNTHESIS-V6.md` (246) | Historical — V6 nomenclature anomaly; V7+ returned to MASTER-PLAN. |

### §3.3 Per-file classification

The 64 files partition as follows. Classification key:
- **KEEP-CURRENT**: latest V{n} per target + the V9.1 stable carry baseline.
- **KEEP-HISTORICAL-LIVE**: earlier V{n} cited by downstream consolidations or by `restart/research/` artefacts (live citation footprint).
- **KEEP-HISTORICAL-SEALED**: earlier V{n} not cited by filename downstream but preserve audit-trail (KSH per `CORPUS-AUDIT-3-HARDENING-DIR.md:50-99`).
- **PRUNE-CANDIDATE**: V5.1 / V5.1A intermediates per `CORPUS-AUDIT-3-HARDENING-DIR.md` §5 — never CONSOLIDATED, only generic-cycle-name references downstream. (Note: these files exist per the audit but were not visible in the 2026-05-13 `wc -l` walk above; they appear in `CORPUS-AUDIT-3` §3 rows 29-32 — either deleted already or count anomaly. Sub-table below counts the 64 actually present.)

| Target | V1 | V2 | V3 | V4 | V5 | V5.1 | V6 | V7 | V7.1 | V8 | V8.1 | V9 | V9.1 | V9.2 | Per-target total |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---:|
| CONSOLIDATED | 1 | 1 | 1 | 1 | 1 | — | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 13 |
| PASS-1 | 1 | 1 | 1 | — | 1 | — | 1 | 1 | — | 1 | 1 | 1 | 1 | 1 | 11 |
| PASS-2 | 1 | 1 | 1 | 1 | 1 | — | 1 | 1 | — | 1 | 1 | 1 | 1 | 1 | 12 |
| PASS-3 | 1 | 1 | 1 | 1 | 1 | — | 1 | 1 | — | 1 | 1 | 1 | 1 | 1 | 12 |
| MASTER-PLAN | 1 | 1 | 1 | 1 | 1 | — | — | 1 | — | 1 | 1 | 1 | 1 | 1 | 11 |
| SYNTHESIS | — | — | — | — | — | — | 1 | — | — | — | — | — | — | — | 1 |
| REVIEW (V1 only) | 4 | — | — | — | — | — | — | — | — | — | — | — | — | — | 4 |
| **Total per cycle** | 9 | 4 | 4 | 3 | 4 | — | 4 | 4 | 1 | 4 | 4 | 4 | 4 | 4 | **64** |

V5.1 cohort (4 files counted in `CORPUS-AUDIT-3` §3 rows 29-32) does not
appear in the 2026-05-13 `wc -l` walk. Either pruned between
`CORPUS-AUDIT-3` (2026-05-07) and 2026-05-13, or count anomaly. The
`HARDENING-PASS-1-PASS-2-V5.1{,A}.md` / `HARDENING-SYNTHESIS-V5.1.md` /
`HARDENING-PASS-3-V5.1.md` filenames warrant a presence-check by the
restructure agent before disposition (they likely live in some
intermediate prune state).

### §3.4 KEEP-CURRENT set (latest V{n} per target + V9.1 carry baseline)

| File | LOC | Role |
|---|---:|---|
| `HARDENING-CONSOLIDATED-V9.2.md` | 146 | Terminal conditional verdict |
| `HARDENING-CONSOLIDATED-V9.1.md` | 112 | Live operating baseline (unconditional READY) |
| `HARDENING-PASS-1-V9.2.md` | 129 | Latest PASS-1 audit (conditional) |
| `HARDENING-PASS-2-V9.2.md` | 616 | Latest PASS-2 audit (conditional) — heaviest of cohort |
| `HARDENING-PASS-3-V9.2.md` | 387 | Latest PASS-3 audit (conditional) |
| `HARDENING-MASTER-PLAN-V9.2.md` | 101 | Latest MASTER-PLAN audit (conditional) |
| `HARDENING-PASS-1-V9.1.md` | 67 | Live carry-baseline PASS-1 |
| `HARDENING-PASS-2-V9.1.md` | 84 | Live carry-baseline PASS-2 |
| `HARDENING-PASS-3-V9.1.md` | 85 | Live carry-baseline PASS-3 |
| `HARDENING-MASTER-PLAN-V9.1.md` | 93 | Live carry-baseline MASTER-PLAN |
| Subtotal | **1,820** | 10 files |

### §3.5 KEEP-HISTORICAL-LIVE set (downstream-cited by filename)

Per `CORPUS-AUDIT-3-HARDENING-DIR.md:50-99` §3 KSH-EXP classifications, plus
the V8 / V9 ratchet citations:

| File | Cited by |
|---|---|
| `HARDENING-CONSOLIDATED.md` (V1) | V2-V9 cohort history rows |
| `HARDENING-CONSOLIDATED-V2.md` | V5 §10 + V6 §10 history |
| `HARDENING-CONSOLIDATED-V3.md` | V4 §3 punch-closure |
| `HARDENING-CONSOLIDATED-V4.md` | V5 baseline + `research/topic-{2,6,8}-*.md` |
| `HARDENING-CONSOLIDATED-V5.md` | V5.1 anchors + `research/topic-{3,8}-*.md` |
| `HARDENING-CONSOLIDATED-V6.md` | V7 carry-baseline; ORCHESTRATOR §3 baseline; `research/deferral-audit-{2,5,6}-*.md` |
| `HARDENING-CONSOLIDATED-V7.md` | V7.1 anchor |
| `HARDENING-CONSOLIDATED-V7.1.md` | V8 baseline; `ORCHESTRATOR.md:12` required-reading; `README.md:432` operating-verdict pointer |
| `HARDENING-CONSOLIDATED-V8.md` | V8.1 baseline; `MASTER-PLAN.md:966`; V8.4 fold absorption |
| `HARDENING-CONSOLIDATED-V8.1.md` | V9 baseline |
| `HARDENING-CONSOLIDATED-V9.md` | V9.1 baseline |
| `HARDENING-PASS-1-V6.md` | V7 §1 cite |
| `HARDENING-PASS-2-V6.md` | V7 §1 cite |
| `HARDENING-PASS-3-V6.md` | V7 §1 cite |
| `HARDENING-SYNTHESIS-V6.md` | V7 §1 cite (single-cycle nomenclature anomaly) |
| `HARDENING-PASS-1-V7.md` | V7.1 R1-R3 closure verification |
| `HARDENING-PASS-1-V8.1.md` | V9 baseline; `research/CORPUS-AUDIT-SYNTHESIS.md:208` cited surface |
| `HARDENING-PASS-{1,2,3}-V8.md`, `HARDENING-MASTER-PLAN-V8.md` | V8.1 fold-closure baselines |
| `HARDENING-PASS-3-V5.1.md` | `research/fold-pass-3.md:30` direct cite |
| `HARDENING-PASS-1-V5.md`, `HARDENING-PASS-2-V5.md` | `research/topic-{2,8}-*.md` cited surfaces |
| `HARDENING-SYNTHESIS-V5.1.md` | `research/topic-{3,8}-*.md` cited surfaces |
| `REVIEW-D-PUNCH-LIST-EXECUTABILITY.md` | V2:86 gate-rerun checklist |
| Subtotal | ~22 files |

### §3.6 KEEP-HISTORICAL-SEALED set (no filename citation but adversarial-cycle evidence)

The remainder of the V2-V8 per-target reports + V1 per-target reports +
REVIEW-A/B/C: sealed audit-trail evidence per `CORPUS-AUDIT-3` §4. None
are filename-cited downstream, but their existence is anchored in
CONSOLIDATED history tables. Per `CORPUS-AUDIT-3-HARDENING-DIR.md:204`:
"Option B — MINIMAL prune (4 V5.1 files); retain 44" was the prior
recommendation. The 2026-05-13 walk shows the V5.1 files are not in the
current `ls`, which is consistent with that prune having landed.

Subtotal: ~32 files KEEP-HISTORICAL-SEALED (retained as adversarial-cycle audit trail).

### §3.7 Hardening dir summary

| Class | Count | LOC |
|---|---:|---:|
| KEEP-CURRENT (V9.2 + V9.1) | 10 | 1,820 |
| KEEP-HISTORICAL-LIVE | ~22 | ~5,500 |
| KEEP-HISTORICAL-SEALED | ~32 | ~11,700 |
| **Total** | **64** | **19,033** |

## §4 The V{n} cycle ratchet — archive strategy

Per `restart/prompts/ORCHESTRATOR.md:60-68`, V1-V9 hardening cycles have all
run; V9.2 staged for conditional commit. The user posed two options:

**Option A — Keep latest V{n} per target; archive older to `archive/V{1..N-2}/`**

| Pros | Cons |
|---|---|
| Discovery surface drops from 64 to ~14 files (KEEP-CURRENT + V9.1 carry); 78% reduction in audit/hardening LOC visible to SK-V7 dispatch. | Audit-trail visibility drops — every cross-cycle reasoning step must reach into `archive/` (still readable, but adds navigation hop). |
| Cycle ratchet remains legible (subdirectories `archive/V1/`, `archive/V2/`, …). | Renaming/moving 50 files breaks any external `restart/audit/hardening/HARDENING-CONSOLIDATED-V{n}.md` citation that downstream may have inlined. |
| Restoration is mechanical (`mv archive/V*/* .`) if a future audit needs the full trail. | The 22 KEEP-HISTORICAL-LIVE files are filename-cited from `research/` and from later hardening cycles — moving them creates dead links unless those citations are updated in lockstep. |

**Option B — Keep all V{n} for full audit trail**

| Pros | Cons |
|---|---|
| Zero dead-link risk; every existing citation resolves verbatim. | 64-file directory remains the dispatch surface; SK-V7 agents must read 19K LOC to navigate. |
| Audit trail integrity 100%. | The 22 KEEP-HISTORICAL-LIVE files have heavy citation traffic; the ~32 KEEP-HISTORICAL-SEALED files have none — leaving them at the top level pessimizes navigation for sealed-only data. |

**Recommendation — Hybrid (Option A with KEEP-HISTORICAL-LIVE elevation)**

```
restart/audit/hardening/
├── (top level — KEEP-CURRENT + KEEP-HISTORICAL-LIVE: ~32 files)
│   HARDENING-CONSOLIDATED-V{1,2,3,4,5,6,7,7.1,8,8.1,9,9.1,9.2}.md  (13)
│   HARDENING-PASS-{1,2,3}-{V6,V7,V8.1,V9.1,V9.2}.md  (15)
│   HARDENING-SYNTHESIS-V6.md  (1)
│   REVIEW-D-PUNCH-LIST-EXECUTABILITY.md  (1)
│   HARDENING-MASTER-PLAN-{V8.1,V9.1,V9.2}.md  (3)
│   …
└── archive/sealed/
    ├── V1/  HARDENING-{PASS-1,PASS-2,PASS-3,MASTER-PLAN,CONSOLIDATED}.md, REVIEW-{A,B,C}-*.md (8)
    ├── V2/  (5 files)
    ├── V3/  (4 files — HARDENING-PASS-1-V3.md cited by V4, may need elevation)
    ├── V4/  (3 files)
    ├── V5/  (4 files — PASS-1-V5 + PASS-2-V5 cited by research/topic-*, may need elevation)
    └── V7/  remaining sealed PASS-{2,3}-V7 + MASTER-PLAN-V7 (3)
```

This preserves the live-citation graph (no dead links), shrinks the
dispatch-surface top-level directory by ~50%, and gives a per-cycle
archive index. Archival is reversible: a future restart-audit cycle that
wants the full trail visible at top level can run `mv archive/sealed/V*/* .`
and restore. Dead-link audit (§7) confirms which V5 / V7 sealed files have
research/ citations that pin them at top level.

Note: V5.1 cohort (4 files) absent from 2026-05-13 walk — either pruned
already (per `CORPUS-AUDIT-3` §5 Option B recommendation) or anomaly.
Restructure agent verifies presence before dispositioning.

## §5 `restart/research/` inspection

27 files; 5 functional bands:

### §5.1 Per-file classification

| Band | File | LOC | Date | Role | Recommendation |
|---|---|---:|---|---|---|
| Index | `INDEX.md` | 183 | 2026-05-05 22:27 | Wave 5+ research deep-dive catalogue; 8 topics × source classification | KEEP-CURRENT — referenced by `ORCHESTRATOR.md` required-reading, `README.md:364`, multiple hardening cycles. |
| Topic deep-dives (Phase 1 outputs) | `topic-1-hm-foundations.md` | 737 | 2026-05-05 15:35 | HM + algorithm W deep-dive | KEEP-CURRENT — cited by deferral-audit-1 + V6 fold-pass-1. |
| | `topic-2-bidirectional.md` | (~700) | 2026-05-05 | Pierce-Turner + DK13 | KEEP-CURRENT. |
| | `topic-3-csp-gadts.md` | (~700) | 2026-05-05 | CSP + GADT pressure | KEEP-CURRENT. |
| | `topic-4-egraphs.md` | 926 | 2026-05-05 | E-graphs + saturation | KEEP-CURRENT. |
| | `topic-5-cost-models.md` | 895 | 2026-05-05 | Cost models | KEEP-CURRENT. |
| | `topic-6-tape.md` | (~800) | 2026-05-05 | Tape encoding | KEEP-CURRENT. |
| | `topic-7-green-red-incremental.md` | 749 | 2026-05-05 | Green/red incremental | KEEP-CURRENT. |
| | `topic-8-simd-dfa.md` | (~860) | 2026-05-05 | SIMD/DFA/regex | KEEP-CURRENT — citation hub (cites `audit/pass-2-codegen/agent-{3,5}` + `audit/hardening/HARDENING-{CONSOLIDATED,PASS-2}-V{4,5}.md`). |
| Fold artefacts (Phase 2 outputs) | `fold-pass-1.md` | 235 | 2026-05-05 15:57 | Topic 1-8 fold classification onto PASS-1 | KEEP-HISTORICAL — folded into PASS-1.md V6; cited by deferral-audit-1. |
| | `fold-pass-2.md` | (~190) | 2026-05-05 15:57 | Onto PASS-2 | KEEP-HISTORICAL — cited by HARDENING-PASS-2-V6.md:31. |
| | `fold-pass-3.md` | (~170) | 2026-05-05 15:57 | Onto PASS-3 | KEEP-HISTORICAL — cites HARDENING-PASS-3-V5{,.1}.md. |
| | `fold-synthesis.md` | (~270) | 2026-05-05 15:58 | Onto trio | KEEP-HISTORICAL — cited by HARDENING-SYNTHESIS-V6.md ×6. |
| Deferral audits (Phase 7 input) | `deferral-audit-1-type-system.md` | 393 | 2026-05-06 11:19 | Type system deferral catalogue | KEEP-HISTORICAL — input to V1-FOLD-CANDIDATES.md. |
| | `deferral-audit-2-function-value-system.md` | (~340) | 2026-05-06 | Function/value | KEEP-HISTORICAL. |
| | `deferral-audit-3-bbnf-surface-directives.md` | 359 | 2026-05-06 | BBNF surface | KEEP-HISTORICAL. |
| | `deferral-audit-4-sibling-crates.md` | 335 | 2026-05-06 | Sibling crates | KEEP-HISTORICAL. |
| | `deferral-audit-5-runtime-pass3.md` | (~320) | 2026-05-06 | Runtime / PASS-3 | KEEP-HISTORICAL. |
| | `deferral-audit-6-codegen-pass2.md` | (~340) | 2026-05-06 | Codegen / PASS-2 | KEEP-HISTORICAL. |
| | `deferral-audit-7-locks-architecture.md` | 415 | 2026-05-06 11:35 | Locks / ARCH | KEEP-HISTORICAL — sourced 5 lock amendments. |
| | `deferral-audit-8-migration-tranche.md` | 573 | 2026-05-06 11:38 | Migration / tranche | KEEP-HISTORICAL — sourced 8 V1 folds. |
| Phase 7 contract | `V1-FOLD-CANDIDATES.md` | 221 | 2026-05-07 15:09 | 30-fold synthesis (Tier 1-4) | KEEP-CURRENT — `ORCHESTRATOR.md:13` required-reading; HARDENING-MASTER-PLAN-V8.md anchors here. |
| Corpus self-audits | `CORPUS-AUDIT-1-TOP-LEVEL-PROMPTS.md` | (~330) | 2026-05-07 14:32 | Top-level + prompts dirs audit | KEEP-HISTORICAL — input to CORPUS-AUDIT-SYNTHESIS. |
| | `CORPUS-AUDIT-2-PASS-DIRS.md` | (~240) | 2026-05-07 14:32 | Pass dirs audit | KEEP-HISTORICAL. |
| | `CORPUS-AUDIT-3-HARDENING-DIR.md` | 229 | 2026-05-07 14:31 | Hardening dir audit (the prior inspection that gave §3.1 ratchet) | KEEP-HISTORICAL — direct input to this R3 inspection. |
| | `CORPUS-AUDIT-4-RESEARCH-DIR.md` | (~220) | 2026-05-07 14:33 | Research dir self-audit | KEEP-HISTORICAL. |
| | `CORPUS-AUDIT-SYNTHESIS.md` | (~140) | 2026-05-07 14:35 | Synthesis | KEEP-HISTORICAL — names current inventory + cleanup ledger. |

### §5.2 Research band summary

| Band | Files | Disposition |
|---|---:|---|
| Index | 1 | KEEP-CURRENT |
| Topic deep-dives (1-8) | 8 | KEEP-CURRENT (live citation hub) |
| Fold artefacts | 4 | KEEP-HISTORICAL (V6-V7 fold inputs, absorbed) |
| Deferral audits | 8 | KEEP-HISTORICAL (V1-FOLD-CANDIDATES inputs, absorbed) |
| Phase 7 contract | 1 | KEEP-CURRENT (V1-FOLD-CANDIDATES.md) |
| Corpus self-audits | 5 | KEEP-HISTORICAL (one-shot inventory audits) |
| **Total** | **27** | 10 KEEP-CURRENT + 17 KEEP-HISTORICAL |

### §5.3 V1-FOLD-CANDIDATES.md status

Per `ORCHESTRATOR.md:13` it is canonical Phase 7 contract required reading.
Per V7 / V7.1 hardening: 30 candidates absorbed (Tier 1-4); V7 returned
AMENDMENT-REQUIRED on 10 cite-hygiene faults; V7.1 closed 13 of 14 punch
items READY. V8 / V9 cycles continue to anchor against this 30-fold
ledger (cited at HARDENING-MASTER-PLAN-V8.md:27, V8.md:63). Status:
**still canonical**. Recommend KEEP-CURRENT.

### §5.4 Research dir restructure proposal

```
restart/research/
├── INDEX.md
├── V1-FOLD-CANDIDATES.md
├── topics/                    ← topic-{1..8}-*.md (8 files)
├── folds/                     ← fold-pass-{1,2,3}.md + fold-synthesis.md (4 files)
├── deferral-audits/           ← deferral-audit-{1..8}-*.md (8 files)
└── corpus-audits/             ← CORPUS-AUDIT-{1..4,SYNTHESIS}.md (5 files)
```

Five sub-directories partition the 27 files by functional band. The
`INDEX.md` + `V1-FOLD-CANDIDATES.md` remain at top level (most-cited; the
two required-reading anchors per ORCHESTRATOR). All other files move.

Dead-link risk: `INDEX.md` cites several topic files; topic files cite
fold artefacts and hardening files. Sub-dir moves require lockstep
updates to ~20-30 in-doc citations. Mechanical; not architectural.

## §6 `restart/corpora/` inspection

4 files; all 2026-05-03 (SOTA.md amended 2026-05-12).

| File | Date | LOC | Role |
|---|---|---:|---|
| `CENSUS.md` | 2026-05-03 | (~1000+) | Mechanical kill-list with FATE legend |
| `MODULES.md` | 2026-05-03 | (~2000+) | Module-by-module explication with KEEP-AS-IS / DELETE / etc. |
| `RESTART-SKETCH.md` | 2026-05-03 | (~900+) | Restart sketch + JSON parse trace |
| `SOTA.md` | 2026-05-12 amend | 762 | SOTA survey for direct-to-struct + path API |

All four are heavily cited from the live trio:

| File | Citing surfaces (path:line counts) |
|---|---|
| `MODULES.md` | `MIGRATION.md` ×11; `ARCHITECTURE.md` ×2; `MASTER-PLAN.md` ×3 |
| `CENSUS.md` | `MIGRATION.md` ×5; `ARCHITECTURE.md` ×1; `MASTER-PLAN.md` ×1 |
| `RESTART-SKETCH.md` | `MIGRATION.md` ×1; `MASTER-PLAN.md` ×1 |
| `SOTA.md` | `MASTER-PLAN.md` ×4 |

All four are KEEP-CURRENT. They are pre-restart frozen snapshots that
the trio cites as ground-truth for migration decisions. No
restructure needed; the dir is already minimal.

Recommendation: **leave `restart/corpora/` unchanged.** No archive
sub-dir; no moves. The 4-file structure is the right granularity.

## §7 Cross-directory cohesion

### §7.1 Dead-link / live-link audit

`restart/audit/` → outside refs (counted by `grep` across `restart/`):

| Citing surface | Refs to `audit/hardening/` | Refs to `audit/pass-{1,2,3}/` |
|---|---:|---:|
| `restart/README.md` | 1 (`HARDENING-CONSOLIDATED-V{N}.md` placeholder) | 0 |
| `restart/MASTER-PLAN.md` | 1 (HARDENING-CONSOLIDATED-V8 + HARDENING-MASTER-PLAN-V8) | 0 |
| `restart/ARCHITECTURE.md` | 0 | 1 |
| `restart/MIGRATION.md` | 0 | 1 |
| `restart/locks/14-LOCKS.md` | 0 | 1 |
| `restart/prompts/ORCHESTRATOR.md` | 1 (V7.1 required reading) | 1 |
| `restart/prompts/HARDENING.md` | (multiple via lens contract) | 1 |
| `restart/prompts/AMENDMENT-DISPATCH.md` | (via cycle) | 1 |
| `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` | (via cycle) | 1 |
| `restart/skinny/BENCH.md` | 0 | 2 |
| `restart/skinny/SUBSTRATE.md` | 0 | 2 |
| `restart/skinny/WORKSPACE.md` | 0 | 1 |
| `restart/research/*.md` | ~40 (V4, V5, V6, V7.1 cited) | 12 (topic-8 → agent-{3,5}) |

`restart/research/` → outside refs:

| Citing surface | Refs |
|---|---:|
| `restart/README.md:364` | 1 (INDEX.md) |
| `restart/audit/hardening/*.md` | ~12 (INDEX, V1-FOLD-CANDIDATES, fold-pass-N, topic-N) |

`restart/corpora/` → outside refs: ~30 across `MIGRATION.md`, `MASTER-PLAN.md`,
`ARCHITECTURE.md` (counted §6 above).

`restart/skinny/audit/` → `restart/audit/`: **zero direct references** found.
The skinny audit dir is iteration-specific and operates on its own
amendment cohort (SK-V1 through SK-V3 / V9.2 lazy-tape design).

### §7.2 Dead-link audit (post-restructure)

If `restart/audit/hardening/` adopts §4's hybrid archive strategy:
- 22 KEEP-HISTORICAL-LIVE files stay at top level → no dead links.
- ~32 KEEP-HISTORICAL-SEALED files move to `archive/sealed/V{n}/`.
  - `research/topic-*.md` cites HARDENING-CONSOLIDATED-V4, V5; if those
    stay top-level (Option A keeps latest-of-cycle), or migrate with
    citation update.
- No external citation paths from `skinny/audit/` to break.
- `restart/audit/pass-{1,2,3}/` — no moves; sub-agent files stay
  alongside `PASS-N.md`.

If `restart/research/` adopts §5.4's 5-band sub-directory structure:
- All inter-research citations need lockstep update (estimated ~25-30 edits across topic/, folds/, deferral-audits/, corpus-audits/).
- External citations: `README.md:364` → `INDEX.md` survives (top-level); `MASTER-PLAN.md`, `ARCHITECTURE.md` cite no research/ files; `HARDENING-MASTER-PLAN-V8.md:27` → `V1-FOLD-CANDIDATES.md` survives (top-level).

## §8 Proposed restructure

The three target dirs after restructure:

```
restart/
├── audit/                            ← totality-track audit
│   ├── pass-1-substrate/             ← unchanged
│   │   PASS-1.md + agent-{1..6}-*.md
│   ├── pass-2-codegen/               ← unchanged
│   │   PASS-2.md + agent-{1..6}-*.md
│   ├── pass-3-runtime/               ← phase-8.4-classification archived
│   │   PASS-3.md + agent-{1..6}-*.md
│   │   archive/phase-8.4-classification.md
│   └── hardening/                    ← hybrid archive
│       (top: KEEP-CURRENT + KEEP-HISTORICAL-LIVE ~32 files)
│       archive/sealed/V{1..7}/       (~32 files)
│
├── research/                         ← 5-band sub-directory
│   INDEX.md
│   V1-FOLD-CANDIDATES.md
│   topics/                           (8 files)
│   folds/                            (4 files)
│   deferral-audits/                  (8 files)
│   corpus-audits/                    (5 files)
│
└── corpora/                          ← unchanged
    CENSUS.md
    MODULES.md
    RESTART-SKETCH.md
    SOTA.md
```

The user's outlined target shape:
```
restart/
├── audit/
│   ├── current/
│   ├── archive/
│   └── hardening/
├── research/
│   ├── current/
│   └── archive/
└── corpora/
```

The proposed shape is a refinement of that target:
- `audit/current/` is functionally split into `pass-{1,2,3}/` + the top
  level of `hardening/` (which is multi-cycle current). Forcing all
  pass-N + current-hardening into one `audit/current/` flattens 3
  meaningful sub-dirs into 1; reject. The pass-N split is intrinsic
  (substrate / codegen / runtime).
- `audit/archive/` collapses to `audit/hardening/archive/sealed/` because
  the hardening dir is the only multi-cycle archive target; pass-N agent
  files are single-cycle inputs, not multi-cycle archive material.
- `research/current/` + `research/archive/` similarly under-grained for
  the 5 functional bands in research (topics / folds / deferral-audits /
  corpus-audits all have distinct roles). Sub-dir per band is more
  legible than flat current/archive.

## §9 Pruning summary

| Action | Files | LOC |
|---|---:|---:|
| Move `audit/pass-3-runtime/phase-8.4-classification.md` → `audit/pass-3-runtime/archive/` | 1 | 70 |
| Move ~32 `audit/hardening/` KEEP-HISTORICAL-SEALED files → `archive/sealed/V{n}/` | ~32 | ~11,700 |
| Move ~25 `research/` files → `topics/` / `folds/` / `deferral-audits/` / `corpus-audits/` sub-dirs | 25 | ~10,000 |
| Verify presence of V5.1 cohort (4 files) — prune if absent | 0-4 | 0-724 |
| **Total moves** | **~58-62** | **~21,800-22,500** |
| **Deletions** | 0 | 0 |

LOC delta on dispatch-surface (top level of `restart/audit/hardening/` +
flat `research/`):

| Surface | Before | After |
|---|---:|---:|
| `audit/hardening/` top level (file count) | 64 | ~32 |
| `audit/hardening/` top level (LOC) | 19,033 | ~7,300 |
| `research/` top level (file count) | 27 | 2 |
| `research/` top level (LOC) | ~13,500 | ~400 |

Net: ~50% reduction in audit/hardening dispatch surface; ~95% reduction
in research top-level. All content preserved; archive/sub-dir paths
mechanically reachable.

## §10 Open questions

1. **V5.1 cohort presence.** The 4 V5.1 / V5.1A files appear in
   `CORPUS-AUDIT-3` §3 rows 29-32 but not in the 2026-05-13 `wc -l`
   walk. Restructure agent verifies before disposition.
2. **V9.2 conditional staging.** Per `HARDENING-CONSOLIDATED-V9.2.md:79-89`
   the V9.2 punch list lands in the V1 corpus only if skinny bench
   outcome A/B/C. If outcome G recurs, V9.2 punch list archives and
   V9.1 stays the terminal carry. The restructure may want V9.2 files
   tagged `CONDITIONAL` (e.g., sub-dir `archive/V9.2-pending/`) until
   the outcome lands.
3. **REVIEW-A/B/C disposition.** 1,078 LOC of V1 reviewer reports never
   cited by filename downstream. `CORPUS-AUDIT-3` §4 recommends KSH on
   adversarial-pass-evidence grounds; user adjudication pending.
4. **`audit/hardening/archive/sealed/V{n}/` granularity.** Per-cycle
   sub-dirs (`V1/`, `V2/`, …) or flat archive? Per-cycle preserves
   ratchet-by-glance discovery; flat is simpler. Recommend per-cycle.
5. **Citation update tooling.** Restructure agent needs to update ~25-30
   in-doc `restart/research/*` citations after sub-dir moves. Mechanical
   `sed` over a `git mv` ledger is the right tool.

## §11 Final report

| Metric | Value |
|---|---:|
| Total files inspected | 117 (86 audit + 27 research + 4 corpora) |
| Total LOC inspected | 35,412 |
| KEEP-CURRENT | 17 (10 hardening + 3 pass-output + 2 research + 4 corpora — but pass-output `PASS-{1,2,3}.md` count plus `phase-8.4-classification` etc. count varies by classification; this number is the live-dispatch surface after restructure) |
| KEEP-HISTORICAL | ~76 (live-cited + sealed-history) |
| PRUNE / verify-presence | 0-5 (V5.1 cohort if present) |
| DELETE | 0 |
| Move (restructure-only) | ~58-62 |
| Recommended archive strategy | Per-cycle sub-dirs under `audit/hardening/archive/sealed/V{n}/`; 5-band sub-dirs under `research/`; pass-N dirs unchanged; corpora unchanged. |
| Output file size | This report — ~430 LOC target |
| Cycle ratchet | V1 → V9.2 across 13 cycles; V9.1 live carry baseline; V9.2 conditional pending bench |
| Cross-dir dead-link risk | Zero from external (`skinny/`, `docs/`); ~25-30 internal `research/` citations need lockstep update if §5.4 sub-dirs adopt; zero risk if `audit/hardening/` archive moves keep KEEP-HISTORICAL-LIVE at top level |
| Audit-trail integrity | 100% (all content preserved; archive paths mechanically reachable) |

Hereupon SK-V7 dispatch reads the dispatch-surface of `restart/audit/`
+ `restart/research/` + `restart/corpora/` at roughly half the LOC of
the pre-restructure surface, while the historical adversarial-cycle
audit-trail remains intact under `archive/`.

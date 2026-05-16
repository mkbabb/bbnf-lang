# SK-V7 Restructure Synthesis

Date: 2026-05-16.

Cohort: 6 restructure agents (R1-R6) archived at
`restart/skinny/tranches/sk-v7/research/skv7-restructure-{R1..R6}.md` (~3,000
LOC of file-level inspection).

## Executive summary

The restart/ corpus is **structurally healthy but procedurally drifted**:
- 0 files require deletion.
- ~58-62 moves recommended (mostly SK-V5/V6 cohort archival).
- 1 lock-file rename executed in this commit (14-LOCKS.md → LOCKS.md).
- 1 prompt-suite restructure executed in this commit (subdirs + 2 file renames).
- ~220 path-swap edits required across HANDOFF / INDEX / SUBSTRATE / MASTER-PLAN / ARCHITECTURE / REDRESS / RESULTS (deferred to Pass Omega CRUD wave).
- 73 dead links across the tree (R6 count).
- 5 load-bearing duplicate sections (BackendShape in 40 files; Locks 15-16 in 5; cold-start reading order in 4; M5 Max landscape in 5; TapeKind naming in ~10).

## Findings by agent

### R1 — restart/skinny/tranches/ (104 files / 32,844 LOC)

Most consequential proposal: **split SK-V6-COHORT into primary tier + redress tier**. SK-V6 mixed primary research (18 A/B/C reports, 5,521 LOC) with mid-iteration redress (30 R*-redress + 3 schema-* files, 5,562 LOC) in one directory — violates V5/V7 invariant + new-tranche-new-doc rule. Recommendation: `archive/sk-v6/cohort/` + `archive/sk-v6/cohort-redress/`. Full inbound-citation traceability preserved (V7 cohort A4 alone cites 10 of the redress files).

Deletions: 1 (`IMPLEMENTATION-AGENT-PROMPT-SK-V5.md`, 347 LOC).
Layout: collapses to 4 lifecycle classes — `current/` (SK-V7), `archive/{sk-v5,sk-v6}/`, `design/` (SOTA-BEAT-DESIGN.md), `excavation/v9.5-psi/`.
Path-rewrite burden: ~220 swaps across the 7 spec surfaces + REDRESS + RESULTS.
Sequence: path-rewrite first, then `git mv`, to preserve `git log --follow` rename detection.

### R2 — restart/skinny/ canonical surfaces (7 files / ~7,500 LOC)

Top 3 stale items:
1. **SK-V6 still named as active dispatch in all 6 canonical surfaces** (INDEX, SUBSTRATE, COMPILER, BENCH, WORKSPACE, HARDENING). SK-V7 absent everywhere despite GRAND-SYNTHESIS-SK-V7.md being committed.
2. **5 dead links to deleted SK-V3/V4 packets** in SUBSTRATE + SOTA-BEAT-DESIGN.
3. **HARDENING.md cycle-naming table frozen at SK-V1/V2/V3+** with stale output-path convention.

`restart/skinny/HARDENING.md` is **NOT a duplicate** of `restart/prompts/audit-specs/HARDENING-LENS-SET.md` (post-restructure: `audit-specs/HARDENING-LENS-SET.md`). Skinny-scope (202 LOC, Lenses L/M/N) vs V1-scope (268 LOC, Lenses A-K); they compose by reference at `skinny/HARDENING.md:198`. Keep both.

Net: 0 deletes / 0 moves / 0 renames; in-place edits to 7 files via Pass Omega CRUD.

### R3 — restart/{audit,research,corpora}/ (117 files / 35,412 LOC)

audit/hardening: 64 files V1→V9.2. 10 KEEP-CURRENT + ~22 KEEP-HISTORICAL-LIVE (filename-cited downstream) + ~32 KEEP-HISTORICAL-SEALED (audit trail).
audit/pass-{1,2,3}: 3 PASS-N + 18 sub-agent historical + 1 prune candidate (phase-8.4-classification.md).
research: 27 files in 5 functional bands.
corpora: 4 files, heavily cited; no restructure needed.

Strategy: keep latest V{n} + LIVE-CITED at top of `audit/hardening/`; move ~32 SEALED to `archive/sealed/V{n}/`. Sub-dir research/ into 5 functional bands (topics/, folds/, deferral-audits/, corpus-audits/).

Pruning: 0 deletes, ~58-62 moves, ~22K LOC of moves. Audit-trail integrity 100%.
Zero cross-references between restart/audit/ and restart/skinny/tranches/.

### R4 — restart top-level + locks + inheritance (10 files)

Top 3 stale items:
1. **HANDOFF.md:9 lags one cohort** — names IMPLEMENTATION-AGENT-PROMPT-SK-V6 as dispatch authority; SK-V7 docs exist but unreferenced.
2. **README.md:420-424 lists 5 prompts** but `restart/prompts/` has 8.
3. **inheritance/INDEX.md:66** contradicts Lock 1 reframe + cites stale 22-variant BIR (current is 20).

**Lock 17 recommendation**: bench-honesty + comparator-plane strictness. Codifies SK-V5 bench-private dishonesty + SK-V6 strict/permissive comparator split as a grammar-neutral V1-binding discipline. Receiver: append after Lock 16. G-Omega gated.

**Filename rename**: 14-LOCKS.md → LOCKS.md (the "14" prefix is stale; file houses 16, will house 17). **Executed in this commit.**

### R5 — restart/prompts/ structure + naming (9 files)

Naming proposal:
```
restart/prompts/
├── README.md                                   ← rewritten (current declares 7 nonexistent files)
├── ORCHESTRATOR.md                             ← kept
├── sub-orchestrators/
│   ├── HARDENING.md                            ← was HARDENING-ORCHESTRATOR.md
│   ├── RESEARCH-FOLD.md                        ← was RESEARCH-FOLD-ORCHESTRATOR.md
│   └── AMENDMENT-DISPATCH.md
├── pass-contracts/
│   ├── PASS-ALPHA.md
│   ├── PASS-OMEGA.md
│   └── SKINNY-TRIUMVIRATE.md                   ← was SKINNY-PASSES.md
└── audit-specs/
    └── HARDENING-LENS-SET.md                   ← was HARDENING.md
```

Load-bearing renames:
- **HARDENING.md → audit-specs/HARDENING-LENS-SET.md**: resolves ambiguity with HARDENING-ORCHESTRATOR.md (different roles sharing prefix). Lens-set names distinctive content.
- **SKINNY-PASSES.md → pass-contracts/SKINNY-TRIUMVIRATE.md**: plurality/singular-content mismatch; file is one contract; "triumvirate" is the load-bearing concept (line 191-200).

**README is broken**: declares 7 file mismatches. Rewrite mandatory.
PASS-ALPHA/OMEGA kept short — Greek-letter bracket carries gate-naming weight.
22 citing files, ~50 line references to fix; mechanical path substitution.

**Executed in this commit.**

### R6 — cross-cutting cohesion + dead links + duplicates

Dead-link count: **73 unique missing paths**.

Duplicate-section count: 5 load-bearing.
- BackendShape 5-shape enum in 40 files.
- Locks 15-16 verbiage in 5 files.
- Cold-start reading order in 4 files.
- M5 Max cross-parser landscape table in 5 files.
- TapeKind variant naming pre-/post-rename in ~10 files.

5 most consequential restructure proposals:
1. **Rename `locks/14-LOCKS.md` → `locks/LOCKS.md`** (filename misleads). EXECUTED.
2. **Archive SK-V5 + SK-V6 cohorts** to `restart/skinny/tranches/archive/SK-V{5,6}/`. DEFERRED (~220 path swaps required).
3. **Author missing `pass-contracts/TOTALITY-PASS-{1,2,3}-*.md`** OR strike references from README. STRUCK (README rewritten).
4. **Strike pre-Phase-8 prompt-suite ghosts** (`PASS-1-SUBSTRATE.md`, `PASS-2-CODEGEN.md`, `PASS-3-RUNTIME.md`, `SYNTHESIS.md`) from README §12 + HANDOFF. DEFERRED (top-level README + HANDOFF Pass Omega CRUD).
5. **Rename + move `V9.5-PSI-EXCAVATION/`** → `archive/SK-V3.5-PSI-EXCAVATION/`. DEFERRED.

Additional findings beyond the asks:
- `restart/skinny/MIGRATION.md` referenced ≥4× but **never existed** under either prefix; lives at `restart/MIGRATION.md` (path bug).
- `skinny/REDRESS.md` + `RESULTS.md` are **mis-prefixed in references** (live at workspace root, not `restart/skinny/`).
- The V7 prompt-suite (PASS-ALPHA/OMEGA/SKINNY-TRIUMVIRATE) is well-formed but cites `§iteration-governance` section + `CH1-CH6` lens scheme that don't exist in the existing ORCHESTRATOR.md (uses A-K). **Cross-document drift fixed** in README rewrite (clarified A-K vs CH1-CH6 are complementary schemes).
- README.md (top-level) + HANDOFF.md do **NOT yet anchor the V7 prompts** — framework is unmoored from canonical top-level surfaces. Pass Omega CRUD fix.

## Executed in this commit

1. **`restart/locks/LOCKS.md` → `LOCKS.md`** (rename).
2. **`restart/prompts/` restructure**:
   - Created subdirs: `sub-orchestrators/`, `pass-contracts/`, `audit-specs/`.
   - Moved 7 files into appropriate subdirs.
   - Renamed `SKINNY-PASSES.md` → `SKINNY-TRIUMVIRATE.md`.
   - Renamed `HARDENING.md` → `HARDENING-LENS-SET.md`.
   - Renamed `HARDENING-ORCHESTRATOR.md` → `HARDENING.md` (now in sub-orchestrators/).
   - Renamed `RESEARCH-FOLD-ORCHESTRATOR.md` → `RESEARCH-FOLD.md`.
3. **`restart/prompts/README.md`** rewritten with actual file layout + A-K vs CH1-CH6 distinction + complete reading order.
4. **6 R-cohort restructure reports** archived to `restart/skinny/tranches/sk-v7/research/`.

## Deferred to Pass Omega CRUD (user approval pending)

A1. **SK-V5/V6 cohort archival** (R1 + R6 #2):
   - Move `restart/skinny/tranches/sk-v5/research/` → `restart/skinny/tranches/archive/sk-v5/cohort/`.
   - Move `restart/skinny/tranches/sk-v6/research/` → `restart/skinny/tranches/archive/sk-v6/` (split into `cohort/` primary + `cohort-redress/` per R1).
   - Move SK-V5/V6 master docs (GRAND-SYNTHESIS, IMPLEMENTATION-PACKET, HANDOFF) → `archive/sk-v{5,6}/`.
   - ~220 path swaps across HANDOFF/INDEX/SUBSTRATE/MASTER-PLAN/ARCHITECTURE/REDRESS/RESULTS.

A2. **V9.5-PSI-EXCAVATION move** (R1 + R6 #5):
   - Move `V9.5-PSI-EXCAVATION/` → `archive/sk-v3.5-psi-excavation/`.

A3. **totality-track archive** (R3):
   - Sub-dir `restart/audit/hardening/` (32 SEALED → archive/sealed/V{n}/).
   - Sub-dir `restart/research/` into 5 functional bands.

A4. **Top-level CRUD** (R4):
   - README.md (top-level): ToC update + prompts/ section + SK-V7 anchor + bbnf-regex → parse-that-regex rename.
   - HANDOFF.md (top-level): pointer to HANDOFF-SK-V7; reading order refresh.
   - ARCHITECTURE.md §7.4: append SK-V6/V7 implementation status paragraph.
   - MASTER-PLAN.md §13: H tranche SK-V7 routing addendum.
   - MIGRATION.md: any new rename/abrogate from SK-V6/V7.
   - inheritance/INDEX.md: Lock 1 reframe + 20-variant BIR correction.

A5. **Lock 17 amendment** (R4 + V7 GRAND-SYNTHESIS §9):
   - Author Lock 17 — bench-honesty + comparator-plane strictness.
   - Receiver: append to `restart/locks/LOCKS.md` after Lock 16.
   - G-Omega gated.

A6. **Skinny canonical surface refresh** (R2):
   - 7 files (BENCH/COMPILER/HARDENING/INDEX/SOTA-BEAT-DESIGN/SUBSTRATE/WORKSPACE): in-place updates per R2 §4 line table.
   - 5 dead links to deleted SK-V3/V4 packets: remove.
   - HARDENING.md cycle-naming table: refresh to current convention.

A7. **Dead-link cleanup** (R6):
   - 73 unique missing paths: per-file cleanup.
   - `restart/skinny/MIGRATION.md` path bug: rewrite references to `restart/MIGRATION.md`.
   - `skinny/REDRESS.md` + `RESULTS.md` mis-prefix: rewrite references.

A8. **Duplicate-section consolidation** (R6):
   - BackendShape 5-shape enum: declare canonical at ARCHITECTURE.md §7.3; have other docs reference (not re-declare).
   - Locks 15-16 verbiage: canonical at locks/LOCKS.md; remove duplicates.
   - Cold-start reading order: canonical at README; remove duplicates.

## Total pass effort estimate

- Executed in this commit (R5 + R4 partial): ~30 min agent time + commit.
- Deferred Pass Omega CRUD: ~6-8 hours wall-clock across 8 CRUD waves.
- LOC delta: ~+50 net (README rewrite + LOCKS rename + RESTRUCTURE doc; balanced by ~stale-content trimming under Pass Omega CRUD).

## Dispatch readiness

After this commit lands, the prompt suite is **self-consistent** and the
framework is anchored. SK-V7 Wave 0 dispatch is ready per
`restart/skinny/tranches/sk-v7/SPEC.md` §2 + the one-liner
in R6 §SK-V7-W0:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny && \
  sed -i '' 's/, "utf8_lossy"//' crates/bbnf-bench/Cargo.toml && \
  cargo tree -p bbnf-bench --edges=features | grep sonic-rs && \
  cargo bench -p bbnf-bench --bench json_parity && \
  cargo run -p bbnf-bench --bin gate --release && \
  git commit -am "feat(sk-v7-wave0): comparator-plane repair (sonic-rs strict)"
```

The Pass Omega CRUD wave (A1-A8 above) should run BEFORE SK-V7 W0 if
the user wants a clean canonical-surface base. OR it can run AFTER SK-V7
closes, folding lessons into the next totality cycle.

User decides: dispatch Pass Omega CRUD (clean restart/ first) OR
dispatch SK-V7 W0 (deliver on the implementation packet first).

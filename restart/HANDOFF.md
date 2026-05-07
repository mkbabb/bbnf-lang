# Handoff — bbnf-lang Greenfield Restart

Date: 2026-05-07
Status: V7.1 READY; per-tranche full-spec drafting (Wave 9+) unblocked; Wave 8 simplification audit pending.
Audience: the next agent or human picking up this work.

This document is the single source of truth for orienting cold. Read it end-to-end before reading anything else; it tells you what the project is, where the work has been, where it is now, and what the next move is. Every claim cites a path so you can verify.

---

## §1 — What this project is

bbnf-lang is a **grammar-driven, multi-backend parser generator** producing SOTA-class typed parsers from `.bbnf` grammar files. The user-facing API is familiar (sonic-rs lazy-value idioms; lightning-css visitor idioms; jq-style path access); the internals are the apotheosis (CSP-backed bidirectional type system; e-graph-driven rewrite engine; shape miner that auto-detects Pratt and SIMD opportunities; cost model unified across the parser and the regex engine; IR + per-backend lowerer).

The anthem: **everything is grammar-derived.** Every grammar plugs into the fleet via two declarative surfaces — (a) a grammar source file `<name>.bbnf` and (b) a workspace metadata block `[workspace.metadata.bbnf.grammars.<name>]` in the root `Cargo.toml`. Adding a 10th grammar requires nothing else: no new crate, no per-grammar match arm in any generic crate, no per-grammar hand-written runtime module. This is Lock 14 — full grammar generalisation; zero overfitting — and it is the single most consequential discipline of the restart.

bbnf is a **meta-grammar**: it generates parsers for extant target languages (Rust V1; WASM + TS deferred V2). bbnf is not itself a runtime; it banks on the host language's facilities (Rust borrow checker + lifetime system; WASM linear memory; TS GC) at the pre-lower layer where appropriate.

Read in order:

1. `restart/README.md` — gestalt synthesis. The architectural commitments, the BBNF extensions, the optimization apotheosis, the type system, the value API, the SOTA synthesis, the 14 locks, the process.
2. `restart/locks/14-LOCKS.md` — the 14 architectural commitments. Locks 4/5/6/7/8/10/12 amended at Phase 7.1; Lock 4 carries DK13 fold + GADT V1 user-facing surface (post-Phase-8.3.1) + closure-by-`&'i`; Lock 5 deferred TS+WASM post-V1 (Backend trait at ARCH §7.5 enables seamless V2 addition).
3. `docs/precepts/instructions/STYLE.md` — voice + discipline (governs all writing).
4. `docs/precepts/instructions/LESSONS-LEARNED.md` — failure-mode anatomy (governs all decisions).

---

## §2 — Where the work has been

Single-round greenfield restart began 2026-05-04 after a compounded-contrivance archive of the prior restart. Multiple waves landed:

| Wave | Phases | Outcome |
|---|---|---|
| 1 | PASS-1/2/3 dispatch + SYNTHESIS trio | Three pass syntheses + ARCHITECTURE/MIGRATION/MASTER-PLAN trio. |
| 2 | V1 hardening + V2 hardening + reviewer cohort (A/B/C/D) | 14 cross-target conflicts surfaced. |
| 3 | Reviewer reconciliation + amendment-dispatch | V3 hardening (4-parallel independent) surfaced 24 cross-document items V2 missed. |
| 4 | Wave 4.1 narrow-amendment + V4 hardening | V4 cohort READY (99% KEEP). |
| 5 | Research-fold pipeline (8 SOTA topic deep-dives) + V5 metahardening (8 lenses) + V6 hardening | V6 READY; research grounded in primary literature. |
| 7 | V1-FOLD-CANDIDATES synthesis + Phase 7.1 lock + ARCH amendments + Phase 7.2 4-parallel surface fold + V7 hardening + Phase 7.5 narrow-amendment + V7.1 verification | **V7.1 READY.** |

Total amendment commits: ~70+ across the cycle. Hardening cycles V1→V7.1 cumulative KEEP fraction climbed from 46% (V1) to 99% (V7.1).

---

## §3 — Current state

**Current operating verdict: `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` (SIMPLIFY-AVAILABLE; Phase 8.4 simplification fold pending).** Phase 8.3.1 corpus cleanup (this phase) lands the user's adjudications on the 8 corpus-audit questions before Phase 8.4 dispatches.

**Verdict ledger.** V7.1 READY survived V8 lens scrutiny across all four targets (PASS-1 / PASS-2 / PASS-3 / MASTER-PLAN trio); the V8 cohort surfaced 41 simplification candidates distributed across 5 tiers (α architectural cardinality / β diagnostic vocab / γ host-leverage / δ meta-grammar deferrals / ε hygiene). None invalidates V7.1; all are surface trims, host-leverage delegations, or aspirational deferrals routed to tranche bodies. Phase 8.4 folds the candidates; Phase 8.5 V8.1 verifies; per-tranche full-spec drafting (Wave 9+) unblocks at V8.1 READY.

**Phase 8 — simplification axis.** The user mandated lenses I (contrivance / over-engineering), J (host-language leverage), K (meta-grammar discipline) — these surface architectural complexity that exceeds the meta-grammar mandate. Phase 8 audits the V7.1-READY corpus for SIMPLIFY candidates and folds them; the simpler corpus then enters Wave 9.

**What has settled** (do not relitigate):
- 14 architectural locks (post-Phase-7.1 amendments).
- 35-question architectural interrogation.
- 10 user adjudications of V1-fold candidates (DK13 fold; GADT V1 user-facing surface per Phase-8.3.1; CHR-improvement V1 fold per Phase-8.3.1; closure-by-`&'i`; `@pretty` verbatim vocabulary; TS+WASM deferred V2; parse-that-regex naming; D wave growth).
- 30 V1 fold candidates absorbed (Tier 1 architecture-nailing; Tier 2 surface coherence; Tier 3 sibling-crate hygiene; Tier 4 architectural prerequisites).
- 8 lock amendments (Locks 4/5/6/7/8/10/12 + 3 NEW: Backend trait, egraph decoupling, 6-directive grammar).
- Backend trait at ARCH §7.5 (V1 RustBackend; V2 WasmBackend + TsBackend).
- 6-directive grammar: `@import`, `@host fn`, `@error(recover)`, `@layout`, `@pretty`, `@token`. Retired: `@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode`, `@debug` (host primitive), `@recover` standalone, `@ws` (folds into `@layout`).
- `path!` macro (renamed from `pointer!`; ~58 sites).
- `parse-that-regex` (renamed from `bbnf-regex`; canonical published name).
- `regex-automata` retired (parse-that-regex carries internal cross-engine parity).

**Open residue (1 non-blocking)**: `BBNF-PATTERN-NONEXHAUSTIVE` enumeration in ARCH §7.4 catalogue — rolls forward to whichever tranche-D spec wave first authors match-expression exhaustiveness. Friction-class only.

---

## §4 — Prompt structure (post-Phase-8.0 prune)

Five prompts at `restart/prompts/`:

1. `ORCHESTRATOR.md` (NEW Phase 8.1) — main entry; fans out to encapsulated sub-orchestrators per phase type. Single source of truth for phase dispatch.
2. `HARDENING-ORCHESTRATOR.md` — dispatches hardening cycles (V1 through V8+); orchestrates per-target hardener agents + consolidation.
3. `RESEARCH-FOLD-ORCHESTRATOR.md` — dispatches research deep-dives + fold cycles (Phase 5+).
4. `AMENDMENT-DISPATCH.md` — dispatches verify-then-patch amendment cycles (Wave 4.1, Phase 7.5, etc).
5. `HARDENING.md` — per-target audit specification (the contract each hardening agent reads). Phase 8.1 adds lenses I/J/K.

Stale dispatch prompts retired at Phase 8.0: `PASS-1-SUBSTRATE.md`, `PASS-2-CODEGEN.md`, `PASS-3-RUNTIME.md`, `SYNTHESIS.md`. The PASS syntheses + SYNTHESIS trio are committed at `restart/audit/pass-{1,2,3}-*/PASS-{1,2,3}.md` + `restart/{ARCHITECTURE,MIGRATION,MASTER-PLAN}.md`; their dispatch prompts have served their purpose.

Mid-cycle classification ledgers retired at Phase 8.0: 9 wave/phase classification artefacts in `restart/audit/pass-{1,2,3}-*/` plus `restart/research/PHASE-7.2-SYNTHESIS-CLASSIFICATION.md`. The amendment commits absorbed them; commit messages preserve audit trail.

---

## §5 — File map

| Path | Status | Purpose |
|---|---|---|
| `restart/README.md` | Live | Gestalt anchor; 14 locks; SOTA synthesis. |
| `restart/ARCHITECTURE.md` | Live (98K) | Executable architectural spec; Backend trait at §7.5; type system at §8; Directive production at §8.1. |
| `restart/MASTER-PLAN.md` | Live (62K) | Tranche A-J; carry ledger §24; cookbook §25. |
| `restart/MIGRATION.md` | Live (50K) | Per-file disposition for legacy code. |
| `restart/locks/14-LOCKS.md` | Live | 14 architectural commitments (post-Phase-7.1 amended). |
| `restart/inheritance/INDEX.md` | Live | BA-BD legacy survival map. |
| `restart/audit/pass-{1,2,3}-*/PASS-{1,2,3}.md` | Live | Per-pass synthesis (post-Phase-7.2 fold). |
| `restart/audit/pass-*/agent-{1-6}-*.md` | Reference | Sub-agent reports from Wave 1 PASS dispatch. |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V7.1.md` | Live | Terminal verdict (READY). |
| `restart/audit/hardening/HARDENING-{CONSOLIDATED,PASS-*,MASTER-PLAN,SYNTHESIS}-V{1..7}.md` | Reference | Sealed cycle history. |
| `restart/audit/hardening/REVIEW-{A,B,C,D}-*.md` | Reference | V1 reviewer reports. |
| `restart/research/INDEX.md` | Live | Research catalogue (8 topics). |
| `restart/research/topic-{1..8}-*.md` | Reference | SOTA deep-dives (~5,800 lines). |
| `restart/research/fold-{pass-1,pass-2,pass-3,synthesis}.md` | Reference | Phase 5 fold records. |
| `restart/research/deferral-audit-{1..8}-*.md` | Reference | Phase 7 inputs. |
| `restart/research/V1-FOLD-CANDIDATES.md` | Live | Phase 7 contract; 30-item synthesis. |
| `restart/corpora/{CENSUS,MODULES,RESTART-SKETCH,SOTA}.md` | Reference | Frozen 2026-05-03 snapshots. |
| `restart-archive-2026-05-04/` | Sealed | Prior restart's archived corpus; research signal only. |

---

## §6 — Verification rituals

Before any phase dispatch:

```bash
git log --oneline -10
git status --short
```

Commit head should be `aaeab682` (V7.1 verification) or later. Working tree should be clean.

For per-target verification (post-fold; pre-tranche):

```bash
# Lock 4 amendment landed
rg -n 'DK13|Dunfield|higher-rank|GADT user-facing surface lands V1|closure.*&.i' restart/locks/14-LOCKS.md

# 6-directive grammar
rg -n 'ImportDecl.*HostFn.*PrettyDecl' restart/audit/pass-1-substrate/PASS-1.md restart/ARCHITECTURE.md

# Backend trait at ARCH §7.5
rg -nC2 '7\.5 Backend|trait Backend|RustBackend.*Backend' restart/ARCHITECTURE.md

# path! macro canonical (pointer! retired except deletion archaeology)
rg -n 'path!' restart/audit/pass-3-runtime/PASS-3.md restart/MASTER-PLAN.md
rg -n 'pointer!' restart/ # only deletion archaeology

# parse-that-regex canonical
rg -n 'parse-that-regex' restart/
rg -n 'regex-automata|bbnf-regex' restart/ # only deletion archaeology
```

---

## §7 — Next move

**Phase 8 simplification cycle**, then **Wave 9 per-tranche full-spec drafting**.

Phase 8 sub-phases — current status:

| Phase | Status | Commit | Sub-orchestrator |
|---|---|---|---|
| 8.0 — Prune + HANDOFF rewrite | DONE | `94873cf0` | (direct edit) |
| 8.1 — Restructure prompts + add lenses I/J/K | DONE | `bc31560c` | (direct edit) |
| 8.2 — V8 simplification audit | DONE | `624b5af2` / `597ac678` / `cd6c2b4c` / `25addd94` | HARDENING-ORCHESTRATOR |
| 8.3 — V8 consolidation | DONE | `28987de4` | HARDENING-ORCHESTRATOR |
| 8.3.1 — Corpus cleanup | (THIS PHASE) | (commit upon completion) | (direct edit) |
| 8.4 — Simplification fold | PENDING | — | AMENDMENT-DISPATCH |
| 8.5 — V8.1 verification rerun | PENDING | — | HARDENING-ORCHESTRATOR |

After Phase 8.5 READY: **Wave 9** dispatches 10 parallel per-tranche full-spec agents (one per tranche A-J; ~3,000-5,000 lines per tranche).

---

## §8 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage; never cite "the prompt said" or "the user asked". Path:line citations on every concrete claim. Per-X tables for "all grammars" / "all backends" / "all topics" claims. Receiver / blocker / receiving-gate triple on every carry. No quick solutions. No legacy code uncontested. Lock 14 is the binding discipline — full grammar generalisation; zero overfitting.

---

## §9 — Closing posture

Hereupon the next move is Phase 8 dispatch. The simplification audit is the last greenfield-discipline pass before per-tranche full-spec drafting. Lenses I (contrivance), J (host-language leverage), K (meta-grammar discipline) surface what V1-V7 punch-list cycles structurally missed. The architecture has been hardened seven times; it can be hardened many more times. The orchestrator structure permits any phase to re-execute without contract drift.

The 14 locks govern. The precepts speak. The greenfield holds.

Read `restart/prompts/ORCHESTRATOR.md` (Phase 8.1 lands it) end-to-end. Then verify the most recent commit is post-V7.1. Then dispatch Phase 8 per the orchestrator's phase table.

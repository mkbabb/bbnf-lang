# Synthesizer — Master Plan from Three Passes (Greenfield Restart)

You are the synthesizer-orchestrator for the bbnf-lang greenfield restart. Three pass orchestrators (Pass A — parser front; Pass B — codegen + runtime + optimisers; Pass C — periphery + tooling + docs + commit chain) have completed. You consume all three and produce the master plan.

The master plan is the authoritative artefact governing the next ~6-12 months of work. It contains: the new workspace shape (crate layout); the new per-crate src/ trees; ≥10 fully-specified tranche stubs (named A through J or further) with fresh tranche numbering; the commit-chain disposition; the docs re-do plan; the migration timeline; the locks-honoured master table at greenfield-completion.

You do NOT dispatch sub-agents. You synthesise directly.

## Required reading (mandatory; in order)

**Suite directives:**
1. `/Users/mkbabb/Programming/bbnf-lang/docs/restart/README.md`
2. `/Users/mkbabb/Programming/bbnf-lang/docs/HARDENING-PLAN-PROMPT.md` — the 14 locks
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/CONSUMING.md`
5. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md`
6. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/ORCHESTRATION.md`
7. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/tranche/` — every file therein (tranche/SPEC.md, START.md, RESEARCH.md, CHALLENGE.md, WAVE_SPEC.md, AGENT_DISPATCH_TEMPLATE.md, DOC_UPDATE_WAVE.md, README.md)

**Three-pass outputs (ground truth; all must exist):**
8. `/Users/mkbabb/Programming/bbnf-lang/audit/restart/PASS-A-2026-MM-DD.md` — Pass A synthesis
9. `/Users/mkbabb/Programming/bbnf-lang/audit/restart/PASS-B-2026-MM-DD.md` — Pass B synthesis
10. `/Users/mkbabb/Programming/bbnf-lang/audit/restart/PASS-C-2026-MM-DD.md` — Pass C synthesis
11. `/Users/mkbabb/Programming/bbnf-lang/audit/restart/per-agent/pass-{a,b,c}-agent-{1..6}-*.md` — 18 sub-reports (read selectively for evidence; the pass syntheses are the primary input)

**Prior corpora (cited as needed):**
12. `audit/CENSUS-2026-05-03.md`, `audit/MODULES-2026-05-03.md`, `audit/RESTART-SKETCH-2026-05-03.md`, `audit/SOTA-2026-05-03.md`
13. `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md`, `audit/PHASE-4-SYNTHESIS-2026-05-03.md`

## Output Contract

You produce **two artefact families**:

### Family 1 — Master plan document

`/Users/mkbabb/Programming/bbnf-lang/audit/restart/MASTER-PLAN-2026-MM-DD.md`, ~3000-5000 lines, structured §1 through §15.

### Family 2 — Tranche stubs

`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/A/A.md` through `J/J.md` (or further), each ~150-300 lines (the stub specification level — full waves drafted later by per-tranche agents). Each stub has: gestalt, hard gates, wave summary table, carry-tags FROM / TO, locks-honoured cell map, risks, build/iter time gate, voice locks, closing posture.

The prior tranche set (Y → BD) does NOT relocate — Pass C decides its archive disposition; you ratify Pass C's recommendation in §10. The fresh tranche set lives at `docs/tranches/A/`, `B/`, … starting clean.

## Master Plan Structure

### §1 — Executive Summary

3-5 paragraphs. The thesis of the restart in archaic-permissive prose. What survives, what is reborn, what is buried. The locks-honoured posture at greenfield completion. The estimated calendar (6-12 months) and the gating reality (the commit-chain decision drives the pre-tranche-A operational sequence).

### §2 — Verdict ledger by pass

| Pass | Files in scope | KEEP-OUTRIGHT | KEEP-MODIFY | ABROGATE-DELETE | ABROGATE-MOVE | ABROGATE-REPLACE |
|---|---:|---:|---:|---:|---:|---:|
| A — parse front | N | n₁ | n₂ | n₃ | n₄ | n₅ |
| B — codegen mid | … | | | | | |
| C — periphery | … | | | | | |
| **Total** | | | | | | |

Brief commentary per row.

### §3 — New workspace shape

The new `Cargo.toml` `[workspace.members]`. Per-crate sketch:

| Crate | Role | Public API surface | Private internals | Dependencies | LOC budget | Migration source |
|---|---|---|---|---|---:|---|

Suggested target crates (ratify, modify, or replace per audit findings):

- `bbnf-parse/` — source acquisition + parse driver + scanner integration + lower (consolidates `crates/core/src/{source,parse,lower}/`)
- `bbnf-ir/` — IR types + passes + registry (consolidates `crates/ir/`; possibly merged into `bbnf-parse` per audit)
- `bbnf-codegen/` — codegen IR + per-backend lowerers + emitter trait (consolidates `crates/core/src/codegen/`)
- `bbnf-runtime/` — runtime substrate (consolidates `crates/core/src/runtime/`; with per-grammar runtime modules eliminated per Lock 14)
- `bbnf-runtime-template/` — the grammar-agnostic generator template that emits per-grammar runtime modules (NEW; per Lock 14)
- `bbnf-pipeline/` — pipeline coordinator (consolidates `crates/core/src/pipeline/`)
- `bbnf-host/` — host fn dispatch substrate (generic; not grammar-specific)
- `path/` — path crate (per Lock 7 consolidation)
- `path-core/` — non-proc-macro shared logic
- `path-ts/` — TS proc-macro shell
- `bbnf-error/` — unified error types (NEW; eliminates per-crate `Error` types)
- `bbnf-cost-model/` — cost model + extraction (NEW or extracted from egraph per Pass B)
- `bbnf-pratt/` — Pratt LUT + auto-detection (NEW or extracted from emitter)
- `bbnf-simd-detect/` — SIMD eligibility detection (NEW or extracted)
- `egraph/` + `egraph-derive/` — sister optimiser crates (path-deps until stable per Lock 11)
- `csp-solver/` — sister optimiser (path-dep until stable)
- `simd-scan/` — SIMD scanner kernels (workspace-internal per Lock 11)
- `parse-that/` — combinator library (path-dep; permanent or promoted per Pass A finding)
- `bbnf-regex/` — bespoke regex engine (path-dep; freeze per Pass A)
- `bbnf-bench/` — vitest-style bench harness (NEW; per `feedback_vitest-bench`)
- `bbnf-cli/` — user-facing CLI (NEW; replaces ad-hoc xtask invocation)
- `bbnf-test-fixtures/` — workspace-internal fixture crate (NEW; honours worktree fixture carry)
- `analysis/` — generic grammar analysis (consolidates `crates/analysis/` per Pass C; degrammar-coupled)
- `lsp/` — editor LSP server (possibly merged with analysis per Pass C)
- `bootstrap/` — meta-grammar bootstrap (consolidates `crates/bootstrap/`)
- Per-grammar declaration crates (one per grammar): `crates/json/`, `crates/css-l4/`, `crates/bbnf-meta/`, `crates/google-sheets/`, `crates/bnf/`, `crates/csv/`, `crates/ebnf/`, `crates/css-pretty/`, `crates/math/` — each carries grammar source + workspace metadata + host fn implementations only (Lock 14 enforced)

Adjust per audit findings; the list above is the prior-art baseline.

### §4 — New per-crate src/ tree

For every crate in §3, the proposed `src/` layout. Example:

```
bbnf-parse/src/
  source/        ← input acquisition
  scanner/       ← scanner integration
  parse/         ← parse driver
  lower/         ← grammar IR → optimisation IR
  errors.rs      ← parse errors
  lib.rs
```

Per Lock 13: each directory has 4-10 children mixing one cohesive concern. No god directories. No file >500 LOC outside `generated/`.

### §5 — Tranche set (fresh; ≥10 tranches)

Master tranche table:

| Tranche | Name | Gestalt | Wave count | Calendar | Carry FROM | Carry TO |
|---|---|---|---:|---|---|---|
| A | … | … | … | … | (none) | B |
| B | … | … | … | … | A | C |
| C | … | … | … | … | A, B | D |
| D | … | … | … | … | … | … |
| E | … | … | … | … | … | … |
| F | … | … | … | … | … | … |
| G | … | … | … | … | … | … |
| H | … | … | … | … | … | … |
| I | … | … | … | … | … | … |
| J | … | … | … | … | … | (close) |
| (further) | … | … | … | … | … | (close) |

Suggested gestalt allocation (ratify, modify, or replace per Pass synthesis):

- **A — Workspace genesis**: Cargo.toml restructure; per-crate skeletons; commit-chain disposition execution
- **B — Bbnf-error + bbnf-pipeline foundation**: unified error types; pipeline coordinator
- **C — Parse + IR foundation**: bbnf-parse + bbnf-ir; lower passes
- **D — Codegen IR contract**: bbnf-codegen-IR; emitter trait; Rust lowerer smoke
- **E — Runtime template + per-grammar declaration crates**: bbnf-runtime-template; one declaration crate per grammar
- **F — Optimiser pipeline**: egraph + csp-solver + miners + cost-model output-piping; Pratt + SIMD auto-detection
- **G — Slice-borrow API + pointer macro + visitor surface**: bbnf-runtime user-facing API
- **H — TS + WASM emitters**: bbnf-codegen TS / WASM activation
- **I — Sister-crate publication**: egraph / csp-solver / bbnf-regex publish; parse-that disposition
- **J — Cross-backend parity + close**: cross-backend parity matrix; final perf gates against sonic-rs / simdjson / lightning-css

Adjust per Pass A/B/C audit findings. Some passes may surface so much work that one substrate (e.g., bbnf-runtime-template) needs its own tranche.

For each tranche, write a stub at `docs/tranches/{X}/{X}.md` (≥150 lines per stub):

- **Gestalt** — central thesis in archaic-permissive prose
- **Hard gates** — every parse-throughput gate cites SOTA per Lock 8; engineering gates separately tabled
- **Wave summary table** — wave skeleton (5-15 waves expected per tranche)
- **Carry-tags FROM** — receiving carries with named gates
- **Carry-tags TO** — outgoing carries with named receiver
- **14-lock honoured table** — per-lock cell with status (honoured / partial / deferred-with-receiver)
- **Risks + mitigations**
- **Build/iter time gate** — generated-LOC budget; xtask regen-cycle budget
- **Voice locks**
- **Closing posture**

### §6 — Workspace + Cargo.toml schema

The full `[workspace]` block + `[workspace.metadata.bbnf]` block + per-crate `Cargo.toml` schema. Per Lock 14, per-grammar metadata is the dispatch surface. Schema (refining the prior `[workspace.metadata.bbnf-strategy]` pattern):

```toml
[workspace.metadata.bbnf.grammars.<ident>]
  source_path = "..."
  declaration_crate = "..."  # crates/<grammar>/
  recognisers = [...]
  host_fns = [...]
  pratt_eligibility = "auto" | "force" | "skip"
  simd_eligibility = "auto" | "force" | "skip"
  output_dir = "..."
  features = ["serialize", "prettify", "skip_recover", "structural"]
```

### §7 — Commit chain disposition

Pass C ratifies one of: rewrite-to-era-boundaries / squash-all / keep-verbatim / hybrid. You ratify or amend; if you amend, name the deciding lock or precept.

Operational sequence (concrete `git` operations, branch names, cutover protocol). Estimated time. Reversibility analysis.

### §8 — Docs re-do plan

Pass C inventories every doc; ratify or amend the disposition.

Master tables:
- Docs to keep verbatim (precepts/ submodule + any other Lock-honoured docs)
- Docs to rewrite (most of `docs/`)
- Docs to delete entirely (audit residue / dead tranche docs)
- Docs to relocate (e.g., `docs/audit/` ↔ `audit/`)
- New docs to create (master SPEC, architecture, migration page)

Sequencing: which docs land in which tranche.

### §9 — Migration timeline

The cutover from current state to greenfield state. Phases:

1. **Pre-A operational sequence** — commit-chain disposition execution + branch ceremony
2. **Tranche A execution** — workspace genesis (this is when the legacy `crates/` tree is dismantled and the new shape lands)
3. **Tranches B-J** — incremental landing per the tranche table
4. **Post-J close** — cross-backend parity + close ceremony

Estimated calendar per phase. Critical-path identification.

### §10 — Archive disposition

The prior tranche set (Y → BD) — confirm the Pass C decision: archive at `docs/tranches/archive/legacy-Y-BD/` (preserved); the prior `docs/tranches/{Y..BD}/` directories are renamed in the migration. The fresh `docs/tranches/A/` through `J/` (or further) is greenfield.

The `archive/` directory at workspace root (containing `crates/ser/`, `crates/gorgeous/`, possibly older content) — confirm Pass C's recommendation (keep / delete / relocate).

### §11 — 14-lock honoured table at greenfield completion

| Lock | Tranche owning honour | Wave honouring | Verification |
|---|---|---|---|
| 1 — Tape + columnar dead | A | A.W0 | rg returns 0 |
| 2 — Layout lowering canon | C, D | C.W?, D.W? | rg returns 0 retired terms |
| 3 — Cursor + byte-skip unified | C, D | … | … |
| 4 — Per-domain orthogonal optimisation | F | F.W? | dependency-DAG verification |
| 5 — IR + per-backend lower | D | D.W0 | IR contract document landed |
| 6 — xtask emits committed source | A | A.W? | regen artefact in git |
| 7 — `crates/path/` consolidated | A | A.W? | crate name verification |
| 8 — Surpass SOTA, not AU | All perf-tranches | All G-numbered gates | competitor numbers cited |
| 9 — Slice-borrow primary | G | G.W? | parse / parse_in / parse_owned API |
| 10 — Pratt + SIMD auto-detected | F | F.W? | no @pratt / @simd directives |
| 11 — Path-deps for sister crates | A, I | A.W?, I.W? | path-dep / publication |
| 12 — ser + gorgeous archive ceremony | A | A.W0 (precondition) | ceremony commit |
| 13 — No god directories | A | A.W? | file-size + child-count gates |
| 14 — Full grammar generalisation | A, E | A.W?, E.W? | rg + future-grammar onboarding test |

### §12 — Generated-LOC trajectory

Pre-restart baseline: 168,750 LOC across `crates/core/src/grammar/generated/`.

Tranche-by-tranche projection (with end-state):

| Tranche | Entry LOC | Net delta | Exit LOC | Notes |
|---|---:|---:|---:|---|
| A | 168,750 | … | … | workspace restructure shouldn't change generated content |
| B | … | … | … | |
| … | … | … | … | |
| J close | … | … | … | greenfield steady-state |

### §13 — Risks + mitigations across tranches

Consolidated risk table from the three passes. Per risk: which tranche owns mitigation; what the mitigation is.

### §14 — Voice locks

(Per `docs/precepts/instructions/STYLE.md` + the suite mandate. Every tranche stub honours.)

### §15 — Closing posture

The synthesizer's commitment in 1-2 paragraphs. Hereupon the master plan is ratified-pending-hardening; tranche A opens after `docs/restart/HARDENING.md` (target: master plan) returns *ready to execute*.

## Methodology

You synthesise directly (no sub-agent dispatch). You read the three pass syntheses end-to-end; cross-reference per-agent reports as evidence; reconcile cross-pass conflicts (e.g., Pass A proposes consolidating bbnf-error here; Pass B proposes consolidating it there — you adjudicate); produce the unified master plan + tranche stubs.

When passes disagree, cite the deciding lock / precept / locked-decision. Do NOT relitigate a settled lock or precept.

## Voice + discipline locks

(Per `docs/restart/README.md` §Voice. Calibrated; archaic-permissive; no metalanguage; path:line citations.)

## Hard cap

90 minutes. At minute 80 commit work-in-progress (master plan + as many tranche stubs as drafted; commit-message lists which stubs are pending). At minute 90 halt and report.

## Output contract

`docs(audit/restart/master-plan): synthesise greenfield master plan + tranche stubs A through {final letter}`.

The commit body lists every tranche stub committed by name.

## Cross-tranche scope boundary

You touch:
- `audit/restart/MASTER-PLAN-2026-MM-DD.md` (your synthesis)
- `docs/tranches/A/A.md` through `J/J.md` (or further) — the fresh tranche stubs
- (Optionally) `docs/tranches/A/waves/`, `B/waves/`, … skeletal directories created but waves deferred

You do NOT modify:
- The three pass syntheses (read-only)
- The 18 per-agent sub-reports (read-only)
- `docs/restart/` (this directory; suite definition)
- `docs/precepts/` (submodule; read-only)
- `crates/` (no source modification)
- The legacy tranche tree (Pass C decides archive disposition; you ratify in §10 but don't execute)
- The commit chain (you propose disposition in §7 but don't execute)

## Background

The synthesizer is the consolidation point. After the synthesizer commits the master plan, the user invokes the hardening prompt at `docs/restart/HARDENING.md` against the master plan; if the hardening returns *ready*, the user invokes per-tranche execution agents (one per tranche, in sequence or in compatible parallel) to draft the wave specifications. That execution is OUT OF SCOPE for this suite.

The greenfield mandate is the final discipline. The master plan must be ambitious-but-honest, idiomatic, gestalt-shaped, and free of legacy. No quick solutions. No workarounds. No carry-forward of the prior tranche-set's residue except by explicit ratification per file / per item.

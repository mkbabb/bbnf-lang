# PIPELINE ORCHESTRATOR — End-to-End Greenfield Restart (Single-Agent, Resumable)

You are the execution orchestrator for the greenfield restart pipeline. You walk six phases serially: produce PASS-1 / PASS-2 / PASS-3 syntheses; produce the SYNTHESIS trio (ARCHITECTURE.md + MIGRATION.md + MASTER-PLAN.md); audit four targets via hardening; consolidate into a single readiness verdict.

You are a single-agent system (Codex, or any agent without sub-agent dispatch capability). You execute all phases yourself, serially, committing between each phase. The pipeline is resumable across multiple invocations — every invocation begins by detecting which phase is next.

The PASS-N prompts at `restart/prompts/PASS-{1,2,3}.md` describe their work as "six sub-agents in parallel". Under your single-agent execution model, the six sub-agents collapse into **six analytical lenses you apply sequentially within ONE synthesis document per PASS.** You do NOT produce six per-lens files; you produce one PASS-N.md whose §3-§8 (or equivalent) carry the six lens findings as numbered sections.

## Resumption — start of every invocation

Before any work, determine the next phase by checking which artefacts exist on disk:

| Check | If absent / partial | Enter phase |
|---|---|---|
| `restart/audit/pass-1-substrate/PASS-1.md` | absent or carries `§Status: pending` | Phase 1 |
| `restart/audit/pass-2-codegen/PASS-2.md` | absent or partial | Phase 2 |
| `restart/audit/pass-3-runtime/PASS-3.md` | absent or partial | Phase 3 |
| `restart/MASTER-PLAN.md` (and `ARCHITECTURE.md`, `MIGRATION.md`) | any absent | Phase 4 |
| `restart/audit/hardening/HARDENING-PASS-1.md` | absent | Phase 5a |
| `restart/audit/hardening/HARDENING-PASS-2.md` | absent | Phase 5b |
| `restart/audit/hardening/HARDENING-PASS-3.md` | absent | Phase 5c |
| `restart/audit/hardening/HARDENING-MASTER-PLAN.md` | absent | Phase 5d |
| `restart/audit/hardening/HARDENING-CONSOLIDATED.md` | absent | Phase 6 |

Enter the lowest-numbered uncomitted phase. If all artefacts exist + are non-partial, the pipeline is complete; report and exit.

A document carries `§Status: pending` (or equivalent) when a prior invocation committed work-in-progress; treat it as needs-completion of that phase. Pick up where the prior session left off.

## Required reading (mandatory; first invocation only; subsequent invocations re-read on resume)

1. `restart/README.md` — gestalt anchor; settled positions Q1-Q35; the 14 locks; the BBNF extensions; the tape + direct-to-struct union; SOTA synthesis
2. `restart/locks/14-LOCKS.md` — the 14 architectural commitments
3. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md` (precepts submodule; init via `git submodule update --init --recursive` if absent)
4. `docs/precepts/instructions/tranche/SPEC.md` + `WAVE_SPEC.md` + `RESEARCH.md`
5. `docs/ffuzzy.md` — three primitives (only lookbehind survives; rewrite-mode rejected; Unicode-class deferred to bbnf-regex)
6. `restart/corpora/CENSUS.md`, `MODULES.md`, `RESTART-SKETCH.md`, `SOTA.md` — research signal corpus
7. `restart/inheritance/INDEX.md` — legacy BA-BD survival ledger
8. The current bbnf-lang source tree at `crates/` — read for inheritance signal

The reading is heavy. Each phase's prompt names additional reading specific to its scope.

## Phase 1 — PASS-1: substrate

Read `restart/prompts/PASS-1-SUBSTRATE.md` end-to-end.

Apply the **six lenses serially** to the substrate scope (IR / type system / CSP+egraph / cost model / grammar extensions / substrate coherence). Per lens, you read the relevant source + corpora + legacy waves; apply the Pro/Con/Explication/Challenge discipline (see HARDENING.md §"Per-Item Discipline"); produce that lens's findings.

Output: **`restart/audit/pass-1-substrate/PASS-1.md`** (~1500-2500 lines), structured §1-§10 per the PASS-1 prompt's §Synthesis section. The §3-§8 sections carry the six lens findings (per-lens table of items with Pro/Con/Explication/Challenge columns + KEEP/REINVENT/DISCARD verdicts).

Commit: `docs(restart/audit/pass-1-substrate): synthesise PASS-1 — substrate (six lenses serial)`

If you exhaust your hard cap (45-90 min recommended) before the synthesis is complete, commit work-in-progress with a `§Status: pending — completed §1-§4; pending §5-§10` line so the next invocation resumes the same file.

## Phase 2 — PASS-2: codegen

Read `restart/prompts/PASS-2-CODEGEN.md` end-to-end.

Apply six lenses serially to the codegen scope (Backend IR / Rust lowerer / WASM lowerer + SIMD / runtime template / Pratt+SIMD auto-detection / codegen coherence). Same per-lens discipline.

Output: **`restart/audit/pass-2-codegen/PASS-2.md`** (~1500-2500 lines).

Commit: `docs(restart/audit/pass-2-codegen): synthesise PASS-2 — codegen + runtime + backends (six lenses serial)`

## Phase 3 — PASS-3: runtime

Read `restart/prompts/PASS-3-RUNTIME.md` end-to-end.

Apply six lenses serially to the runtime scope (value API / path+select DSLs / visitor surface / **tape + direct-to-struct UNION (the architectural keystone)** / error recovery + incremental + LSP / ecosystem). Same per-lens discipline.

The tape lens is the most consequential. Per `restart/README.md` §8, tape is the substrate of the greenfield, properly implemented (no parallel substrate; no orthogonal codepath; no Vec<OpenFrame>::clone pathology). The 2,000-commit prior failure was implementation, not naming. Specify the tape layout (token-record byte layout; payload arena structure); the typed-value-borrow shape (`JsonValue<'i> { kind, span, tape: &'i Tape<'i>, idx: u32 }`); the materialisation cost; the slice-borrow + bumpalo + owned escape integration.

Output: **`restart/audit/pass-3-runtime/PASS-3.md`** (~1500-2500 lines).

Commit: `docs(restart/audit/pass-3-runtime): synthesise PASS-3 — user surface + ecosystem (six lenses serial)`

## Phase 4 — SYNTHESIS

Read `restart/prompts/SYNTHESIS.md` end-to-end. Read all three PASS outputs end-to-end.

Produce three documents per the SYNTHESIS prompt's §Output Contract:

- **`restart/ARCHITECTURE.md`** (~1500-2500 lines) — workspace + module structure + dependency DAG + per-file rationale + IR contract + BBNF formal specification
- **`restart/MIGRATION.md`** (~1500-2500 lines) — per-file disposition (KEEP-OUTRIGHT / KEEP-MODIFY / ABROGATE-DELETE / ABROGATE-MOVE / ABROGATE-REPLACE) for every file in current `crates/` + new facilities + migration sequencing + commit-chain disposition
- **`restart/MASTER-PLAN.md`** (~2500-4000 lines) — fresh tranche set ≥10 (named A through J or further) + per-tranche stub (~150-300 lines each) + 14-lock honoured table + generated-LOC trajectory

Reconcile cross-pass conflicts; cite the deciding lock / precept / settled position.

Commits (three separate, or one combined):
- `docs(restart): land ARCHITECTURE.md — workspace + IR + BBNF`
- `docs(restart): land MIGRATION.md — per-file disposition`
- `docs(restart): land MASTER-PLAN.md — fresh tranche set + execution sequence`

## Phase 5 — Hardening (four targets, serial)

Read `restart/prompts/HARDENING.md` end-to-end. The nine-lane audit discipline applies to each of four targets, executed serially.

### Phase 5a — HARDENING-PASS-1

Read `restart/audit/pass-1-substrate/PASS-1.md` (the target). Apply HARDENING.md's nine lanes (Lock-Adherence / Sequencing-N/A-for-single-pass / Cohesion / SOTA-Anchoring / Grammar-Authoritative / Generated-Code-Budget / Friction-Forecast / Carry-Deferral / Greenfield-Discipline) with Pro/Con/Explication/Challenge per-item discipline.

Output: **`restart/audit/hardening/HARDENING-PASS-1.md`** (~600-1500 lines), §1-§13 per HARDENING.md's §Output Contract.

Commit: `docs(restart/audit/hardening): hardening pass against PASS-1`

### Phase 5b — HARDENING-PASS-2

Same shape; target = PASS-2.md; output = `HARDENING-PASS-2.md`.

Commit: `docs(restart/audit/hardening): hardening pass against PASS-2`

### Phase 5c — HARDENING-PASS-3

Same shape; target = PASS-3.md; output = `HARDENING-PASS-3.md`.

Commit: `docs(restart/audit/hardening): hardening pass against PASS-3`

### Phase 5d — HARDENING-MASTER-PLAN

Target = the SYNTHESIS trio (`MASTER-PLAN.md` + `ARCHITECTURE.md` + `MIGRATION.md`). All nine lanes apply (Sequencing-Discipline lane particularly applies to the master plan's tranche stubs A-J). Read the three Phase-5a/b/c reports as ground-truth context — when MASTER-PLAN ratifies a PASS-N proposal that an earlier hardening pass flagged, surface the conflict.

Output: **`restart/audit/hardening/HARDENING-MASTER-PLAN.md`** (~1000-2000 lines).

Commit: `docs(restart/audit/hardening): hardening pass against MASTER-PLAN`

## Phase 6 — Consolidation

Read all four hardening reports. Synthesise into:

**`restart/audit/hardening/HARDENING-CONSOLIDATED.md`** (~600-1200 lines), structured §1-§7 per the original orchestrator design:

§1 — Target identifications (4-target table; commits; verdicts)
§2 — Cohort verdict (per-lane consolidated table; cumulative KEEP/REINVENT/DISCARD totals)
§3 — Cross-target conflicts (where MASTER-PLAN ratifies what PASS hardening flagged; where lanes disagree on shared substance)
§4 — Punch list consolidation (cumulative across four reports, deduplicated)
§5 — Final readiness verdict — one of:
- **READY** — every report returns ready (or amendment-required with surgeries narrow enough to fold without re-draft)
- **AMENDMENT-REQUIRED** — at least one report returns amendment-required with substantive surgeries
- **RE-DRAFT** — at least one report returns re-draft, OR cumulative findings surface architectural conflicts that re-draft must resolve
§6 — Voice + discipline locks
§7 — Closing posture (next step named)

Commit: `docs(restart/audit/hardening): consolidate four-target hardening — verdict {READY / AMENDMENT-REQUIRED / RE-DRAFT}`

## Hard cap discipline

You execute serially in one or more Codex invocations. Each invocation:

- Pick a single phase
- Allocate ~45-90 minutes for the phase's work + 5-10 minutes for the commit + report
- If the phase's work exceeds the budget, commit work-in-progress with `§Status: pending` named explicitly; the next invocation resumes
- If the phase completes, commit and either continue to the next phase (if budget remains) or report progress and halt for the next invocation

Recommended invocation cadence:
- Invocation 1: Phase 1 (PASS-1) — most reading-heavy
- Invocation 2: Phase 2 (PASS-2)
- Invocation 3: Phase 3 (PASS-3)
- Invocation 4: Phase 4 (SYNTHESIS) — heaviest writing
- Invocation 5: Phases 5a + 5b
- Invocation 6: Phases 5c + 5d
- Invocation 7: Phase 6 + final report

Seven invocations total; each ~60-90 minutes of execution. Adjust per your environment.

## Per-Item Discipline (carried through every phase)

Every claim, gate, decision, surgery, verdict, and proposal carries:

- **Explication** — what the item *means*; the underlying intent
- **Pros** — why the item earns its place; locks/precepts honoured
- **Cons** — costs the item imposes; locks/precepts strained
- **Challenge** — the adversarial counter-position; the steelman alternative

Verdicts: **KEEP** (pros outweigh cons; challenge defeated) / **REINVENT** (pros real but current shape carries surplus con; redesign named) / **DISCARD** (cons outweigh pros; challenge wins; replacement named).

Lanes / sections without per-item rows are fault. KEEP-without-challenge is per-row fault. The discipline applies in PASS-N syntheses (each lens has its per-item table); in SYNTHESIS reconciliation; in HARDENING (per-lane per-item tables); in HARDENING-CONSOLIDATED cross-target tables.

## Voice + Discipline Locks

(Per `restart/README.md` §13. Calibrated; archaic-permissive; mild poetic undercurrent; no metalanguage; no commit refs; no "the user said"; no soft hedging; path:line citations on every concrete claim; tables liberal; per-X tables for every "all-X" claim; no "TBD" / "user adjudicates" / "future without receiver"; no quick solutions; no workarounds; no legacy code uncontested; idiomatic gestalt; architectural transpositions for elegance, simplicity, performance.)

## Cross-tranche scope boundary

You touch ONLY:
- Phase 1: `restart/audit/pass-1-substrate/PASS-1.md`
- Phase 2: `restart/audit/pass-2-codegen/PASS-2.md`
- Phase 3: `restart/audit/pass-3-runtime/PASS-3.md`
- Phase 4: `restart/{ARCHITECTURE.md, MIGRATION.md, MASTER-PLAN.md}`
- Phases 5a-d: `restart/audit/hardening/HARDENING-{PASS-1, PASS-2, PASS-3, MASTER-PLAN}.md`
- Phase 6: `restart/audit/hardening/HARDENING-CONSOLIDATED.md`

You do NOT modify:
- `restart/prompts/` (suite definition; read-only — these are your contracts)
- `restart/README.md` (gestalt anchor; only the user amends)
- `restart/locks/`, `restart/corpora/`, `restart/inheritance/` (read-only)
- `crates/`, `docs/`, `restart-archive-2026-05-04/` (out of scope; read-only for inheritance signal)
- Any phase's output that is not your current phase (do not edit prior phases' outputs to fix issues; surface in the current phase's report)

## After Phase 6

The pipeline is complete. The user's next step (out of orchestrator scope):

- READY → user dispatches per-tranche full-spec drafting (one drafting agent per tranche A-J, ~3,000-5,000 lines per tranche, inheriting from BA-BD per `restart/inheritance/INDEX.md`)
- AMENDMENT-REQUIRED → user dispatches an amendment agent against the consolidated punch list; amendments commit; the user re-runs this orchestrator (or a narrow phase) to verify
- RE-DRAFT → user identifies which PASS or SYNTHESIS re-runs (or this orchestrator returns re-draft against itself if the conflict is cross-target architectural)

## Background

The greenfield restart's pipeline is single-round and serial under Codex. Six phases; each commits; each is resumable. The 14 locks are settled; the 35-answer interrogation is settled; Amendment 01 (no per-grammar declaration crates by default) is settled; the precepts are settled; the BBNF extensions (lookbehind in; rewrite-mode out; Unicode-class deferred to regex; @host fn / multi-function chaining / generics / @error / @layout) are settled; tape is the substrate (properly unioned with direct-to-struct).

You execute; you do not relitigate. Where conflicts surface across phases, the per-phase Pro/Con/Explication/Challenge discipline + Phase-6 cross-target consolidation resolves; the user adjudicates if Phase 6 returns conflicts above the orchestrator's resolution capacity.

After Phase 6's READY verdict, the per-tranche drafting opens. That is out of this orchestrator's scope.

---

**Begin: read this prompt end-to-end. Then execute the resumption logic at the top to determine your entry phase. Then enter that phase.**

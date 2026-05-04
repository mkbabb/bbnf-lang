# Pipeline Orchestrator — Greenfield Restart

You are the pipeline orchestrator. Your role is to coordinate the six phases of the greenfield restart by dispatching sub-agents in parallel where possible, awaiting their commits, and consolidating the final readiness verdict. You dispatch many agents; you do not execute the per-phase work yourself except for the Phase 6 consolidation.

The pipeline produces, in order: three PASS syntheses (parallel), one SYNTHESIS trio (serial after PASS), four hardening reports (parallel after SYNTHESIS), one consolidated readiness verdict (your own synthesis). At the end of Phase 6, the verdict gates per-tranche full-spec drafting.

## Required reading

1. `restart/README.md` — gestalt anchor; settled positions; the 14 locks; the BBNF extensions; the tape + direct-to-struct union; SOTA synthesis
2. `restart/locks/14-LOCKS.md` — the 14 architectural commitments
3. `restart/prompts/{PASS-1-SUBSTRATE, PASS-2-CODEGEN, PASS-3-RUNTIME, SYNTHESIS, HARDENING}.md` — the contracts you dispatch
4. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md` (precepts submodule; ensure initialised)
5. `restart/inheritance/INDEX.md` — legacy BA-BD survival ledger

The corpora at `restart/corpora/` and the source tree at `crates/` are ground-truth that the dispatched agents read; you do not need to read them yourself unless Phase 6 consolidation requires verification of a specific cross-target conflict.

## Phase 1 — Three PASS syntheses, parallel

Check for committed artefacts at:
- `restart/audit/pass-1-substrate/PASS-1.md`
- `restart/audit/pass-2-codegen/PASS-2.md`
- `restart/audit/pass-3-runtime/PASS-3.md`

For each absent artefact, dispatch one PASS orchestrator agent. The three dispatches run in parallel.

Each dispatched agent's contract:
- Reads `restart/prompts/PASS-{N}-{LAYER}.md` as its operational specification
- Internally dispatches its own six sub-agents per the PASS-N prompt's §Methodology (one per analytical lens: Inventory / Idiomaticity / Lock-Adherence / Architectural-Transposition / Replacement-Design / Cross-Cut, or the lens names the prompt specifies)
- Synthesises the six sub-agent reports into the PASS synthesis
- Commits the synthesis + the six sub-agent reports autonomously
- Returns when its own commit lands

The PASS orchestrator hard cap (per the PASS-N prompt) is ~75 minutes orchestrator + ~45 minutes per sub-agent. Three PASS dispatches in parallel; total wall ~2 hours.

Await all three PASS syntheses committed before entering Phase 2.

## Phase 2 — SYNTHESIS

Check for committed artefacts at:
- `restart/ARCHITECTURE.md`
- `restart/MIGRATION.md`
- `restart/MASTER-PLAN.md`

If any is absent, dispatch one SYNTHESIS agent. Its contract:
- Reads `restart/prompts/SYNTHESIS.md` as its operational specification
- Reads all three PASS syntheses + the 18 sub-agent reports
- Reconciles cross-pass conflicts per the SYNTHESIS prompt's §Methodology
- Produces the three documents per the SYNTHESIS prompt's §Output Contract
- Commits autonomously

Hard cap ~120 minutes. Single dispatch; serial.

Await the SYNTHESIS commit before entering Phase 3.

## Phase 3 — Four hardening passes, parallel

Check for committed artefacts at:
- `restart/audit/hardening/HARDENING-PASS-1.md`
- `restart/audit/hardening/HARDENING-PASS-2.md`
- `restart/audit/hardening/HARDENING-PASS-3.md`
- `restart/audit/hardening/HARDENING-MASTER-PLAN.md`

For each absent artefact, dispatch one hardening agent. The four dispatches run in parallel.

Each dispatched agent's contract:
- Reads `restart/prompts/HARDENING.md` as its operational specification, parameterised with the named target
- Reads the target's output(s)
- Applies the nine-lane audit (Lock-Adherence / Sequencing-Discipline / Cohesion / SOTA-Anchoring / Grammar-Authoritative / Generated-Code-Budget / Friction-Forecast / Carry-Deferral / Greenfield-Discipline) with Pro/Con/Explication/Challenge per-item discipline
- Produces `restart/audit/hardening/HARDENING-{TARGET}.md`
- Returns a verdict (READY / AMENDMENT-REQUIRED / RE-DRAFT) + punch list
- Commits autonomously

Hard cap per agent: 45 minutes for PASS-1/2/3 targets; 90 minutes for MASTER-PLAN target. Four dispatches in parallel; total wall ~90 minutes.

The MASTER-PLAN hardening agent reads the three PASS hardening reports as ground-truth context if they commit before its own work begins; if they have not committed, it proceeds without that signal. Per Phase 6 consolidation, cross-target conflicts surface there.

Await all four hardening reports committed before entering Phase 6.

## Phase 6 — Consolidation (your own synthesis)

This is the one phase you execute directly, not via dispatch. Read the four hardening reports end-to-end.

Produce `restart/audit/hardening/HARDENING-CONSOLIDATED.md`, ~600-1200 lines, structured §1-§7:

§1 — Target identifications (4-target table; commits; per-target verdicts)
§2 — Cohort verdict — per-lane consolidated table:

| Lane | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cumulative |
|---|---|---|---|---|---|
| 1 Lock-Adherence | … | … | … | … | … |
| 2 Sequencing | N/A | N/A | N/A | … | … |
| 3 Cohesion | … | … | … | … | … |
| 4 SOTA-Anchoring | … | … | … | … | … |
| 5 Grammar-Authoritative | … | … | … | … | … |
| 6 Generated-Code-Budget | … | … | … | … | … |
| 7 Friction-Forecast | … | … | … | … | … |
| 8 Carry-Deferral | … | … | … | … | … |
| 9 Greenfield-Discipline | … | … | … | … | … |

Cumulative KEEP / REINVENT / DISCARD totals.

§3 — Cross-target conflicts. Where the four reports disagree on shared substance: where MASTER-PLAN ratifies what a PASS hardening flagged; where lanes apply differently across targets; where one target's verdict implicitly contradicts another's. Per-conflict table:

| Conflict | Sources | Per-target verdicts | Resolution recommendation |
|---|---|---|---|

§4 — Punch list consolidation. Cumulative punch list across all four reports, deduplicated. Items appearing in multiple reports collapse to one entry citing all sources. Items where reports disagree on surgery carry the most surgical of the proposed surgeries.

§5 — Final readiness verdict. One of:
- **READY** — every report returns ready (or amendment-required with surgeries narrow enough to fold without re-draft)
- **AMENDMENT-REQUIRED** — at least one report returns amendment-required with substantive surgeries
- **RE-DRAFT** — at least one report returns re-draft, OR cumulative findings surface architectural conflicts that re-draft must resolve

§6 — Voice + discipline locks (per `restart/README.md` §13)

§7 — Closing posture. One paragraph naming the next step.

Commit: `docs(restart/audit/hardening): consolidate four-target hardening — verdict {READY / AMENDMENT-REQUIRED / RE-DRAFT}`

The commit body summarises the cohort verdict + cross-target conflicts (if any) + final decision in one paragraph.

## Idempotency

Every phase begins with the artefact-existence check above. If outputs exist, skip the phase's dispatch. If outputs are partial (a synthesis exists but commits a `§Status: pending` line, signalling work-in-progress from an interrupted prior dispatch), the agent that produced the partial output is re-dispatched against the same target.

This idempotency makes the orchestrator safe to re-invoke after any interruption — partial commits survive; completed phases skip; only outstanding work re-dispatches.

## Hard cap

Phase 1 wall: ~2 hours (longest of three parallel PASS orchestrators).
Phase 2 wall: ~2 hours (single SYNTHESIS).
Phase 3 wall: ~90 minutes (longest of four parallel hardenings; MASTER-PLAN target dominates).
Phase 6 (your own synthesis): ~45 minutes.

Total orchestrator wall: ~6-7 hours from Phase 1 dispatch to Phase 6 commit. The dispatched agents do their work; you wait.

## Methodology

You orchestrate; you do not audit per phase. Per-phase audit is the dispatched agents' role.

- **Phase 1, 3 dispatches: parallel.** Three (Phase 1) or four (Phase 3) Agent tool invocations in a single message; each `run_in_background: true`; each carries the per-target dispatch prompt the orchestrator composes.
- **Phase 2 dispatch: serial.** Single Agent invocation after Phase 1 commits.
- **Phase 6 synthesis: direct.** Read the four reports; consolidate; commit.
- **Cross-target conflicts surface in Phase 6.** Conflicts that no individual hardener caught are synthesis-level findings.
- **Do not relitigate per-target findings.** The per-target hardeners are the adversaries; you are the consolidator.

### Per-dispatch prompts you compose

For each dispatched agent, compose a prompt that includes:

- The agent's role + scope (e.g., "PASS-1 substrate orchestrator" / "HARDENING with target=PASS-2")
- Reference to the operational contract at `restart/prompts/{PASS-N, SYNTHESIS, HARDENING}.md`
- Reference to `restart/locks/14-LOCKS.md`
- The voice + discipline locks
- The output path and the commit message format
- The cross-tranche scope boundary (touch ONLY the agent's output path; do NOT modify other restart subdirs)
- The hard cap

The dispatch prompts are NOT pre-written here — you compose them at dispatch time. The contracts at `restart/prompts/` are the per-agent specifications; your role is to invoke them with the right scope.

## Output commits

You commit Phase 6 only. Phase-1, Phase-2, Phase-3 dispatched agents commit their own outputs autonomously per their contracts.

Phase 6 commit: `docs(restart/audit/hardening): consolidate four-target hardening — verdict {READY / AMENDMENT-REQUIRED / RE-DRAFT}`

## Cross-tranche scope boundary

You touch ONLY:
- The Agent dispatch invocations across Phases 1, 2, 3 (via the Agent tool)
- `restart/audit/hardening/HARDENING-CONSOLIDATED.md` (Phase 6)

You do NOT modify:
- `restart/prompts/` (suite definition; read-only — these are your contracts)
- `restart/README.md` (gestalt anchor)
- `restart/locks/`, `restart/corpora/`, `restart/inheritance/` (read-only)
- The dispatched agents' outputs (PASS syntheses, SYNTHESIS trio, hardening reports — they belong to the dispatched agents)
- `crates/`, `docs/`, `restart-archive-2026-05-04/`

## Background

The greenfield restart's pipeline is single-round and parallel-where-possible. Six phases (three of which are dispatched in parallel; one synthesis; four hardening passes parallel; one consolidation). The 14 locks are settled. The 35-answer interrogation is settled. Amendment 01 (no per-grammar declaration crates by default) is settled. The precepts are settled. The BBNF extensions (lookbehind in; rewrite-mode out; Unicode-class deferred to bbnf-regex; @host fn / multi-function chaining / generics / @error / @layout) are settled. Tape is the substrate (properly unioned with direct-to-struct).

You orchestrate; the dispatched agents execute; you consolidate at the end. The next step after Phase 6's verdict is per-tranche full-spec drafting (if READY), amendment dispatch (if AMENDMENT-REQUIRED), or PASS / SYNTHESIS re-dispatch (if RE-DRAFT) — out of this orchestrator's scope.

---

**Begin: read this prompt end-to-end + the required reading. Then check Phase 1's artefact existence and dispatch as needed.**

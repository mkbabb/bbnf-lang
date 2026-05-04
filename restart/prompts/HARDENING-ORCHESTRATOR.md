# HARDENING ORCHESTRATOR — Four-Target Sequencing (Greenfield Restart)

You are the hardening orchestrator. Your role is to invoke `restart/prompts/HARDENING.md` against the four output stages of the greenfield restart suite (PASS-1, PASS-2, PASS-3, SYNTHESIS), consolidate the four resulting reports, and return a single readiness verdict that gates per-tranche full-spec drafting.

You are not a hardening agent. You dispatch hardening agents. The single-round audit discipline holds — this orchestrator runs once, dispatches four target-parameterised hardening passes, consolidates, decides.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/README.md` — gestalt anchor; settled positions
2. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md`
3. `/Users/mkbabb/Programming/bbnf-lang/restart/prompts/HARDENING.md` — the per-target hardening contract you parameterise
4. `/Users/mkbabb/Programming/bbnf-lang/restart/prompts/{PASS-1-SUBSTRATE, PASS-2-CODEGEN, PASS-3-RUNTIME, SYNTHESIS}.md` — the prompts that produced the four targets
5. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md`

The four target outputs (read at dispatch time, not before):
- `restart/audit/pass-1-substrate/PASS-1.md` — PASS-1 synthesis
- `restart/audit/pass-2-codegen/PASS-2.md` — PASS-2 synthesis
- `restart/audit/pass-3-runtime/PASS-3.md` — PASS-3 synthesis
- `restart/MASTER-PLAN.md` + `restart/ARCHITECTURE.md` + `restart/MIGRATION.md` — SYNTHESIS output

## Dispatch Policy

The four hardening invocations dispatch in two phases.

### Phase 1 — Per-PASS hardening (parallel)

Three hardening agents dispatch in parallel, one per PASS:

| Agent | Target | Output |
|---|---|---|
| Hardener-1 | PASS-1 (substrate) | `restart/audit/hardening/HARDENING-PASS-1.md` |
| Hardener-2 | PASS-2 (codegen) | `restart/audit/hardening/HARDENING-PASS-2.md` |
| Hardener-3 | PASS-3 (runtime) | `restart/audit/hardening/HARDENING-PASS-3.md` |

Each agent is dispatched per the existing `restart/prompts/HARDENING.md` contract with `target=PASS-{N}`. Each runs the nine-lane audit (Lock-Adherence, Sequencing-N/A-for-single-pass, Cohesion, SOTA-Anchoring-when-applicable, Grammar-Authoritative, Generated-Code-Budget, Friction-Forecast, Carry-Deferral, Greenfield-Discipline) with Pro/Con/Explication/Challenge per-item discipline; each returns a verdict (ready / amendment-required / re-draft) + punch list.

Phase 1 entry condition: PASS-1, PASS-2, PASS-3 syntheses all committed. Phase 1 exit condition: all three Phase-1 hardening reports committed.

### Phase 2 — SYNTHESIS hardening (serial)

Once Phase 1 commits and SYNTHESIS commits its three documents (`ARCHITECTURE.md` + `MIGRATION.md` + `MASTER-PLAN.md`), one hardening agent dispatches:

| Agent | Target | Output |
|---|---|---|
| Hardener-4 | MASTER-PLAN (synthesis trio) | `restart/audit/hardening/HARDENING-MASTER-PLAN.md` |

This agent's nine-lane audit applies to the master plan + architecture + migration as a unified target. Sequencing-Discipline lane (Lane 2) applies — the master plan's tranche stubs A through J (or further) get sequenced-against-Era-V-failure-mode verification. Generated-Code-Budget (Lane 6) verifies tranche-level + wave-level decomposition. Carry-Deferral (Lane 8) verifies every legacy BA-BD inheritance carries a named receiver + blocker + receiving gate. Greenfield-Discipline (Lane 9) verifies the user's no-quick-solutions / no-workarounds / no-legacy-uncontested mandate.

Hardener-4 also reads the three Phase-1 reports as ground-truth context — when SYNTHESIS's master plan ratifies a PASS-N proposal that Phase-1 hardening flagged as REINVENT or DISCARD, Hardener-4 surfaces the conflict.

Phase 2 entry condition: Phase 1 complete + SYNTHESIS committed. Phase 2 exit condition: Hardener-4 report committed.

## Phase 3 — Consolidation (your output)

After all four Phase-1/2 reports commit, you consolidate. You do NOT dispatch a fifth agent; you synthesise directly from the four reports + the four targets.

Output: `restart/audit/hardening/HARDENING-CONSOLIDATED.md`, ~600-1200 lines.

### §1 — Target identifications

| Hardening report | Target | Commit | Lines | Verdict |
|---|---|---|---:|---|
| HARDENING-PASS-1.md | PASS-1 substrate | … | … | ready / amendment-required / re-draft |
| HARDENING-PASS-2.md | PASS-2 codegen | … | … | … |
| HARDENING-PASS-3.md | PASS-3 runtime | … | … | … |
| HARDENING-MASTER-PLAN.md | SYNTHESIS trio | … | … | … |

### §2 — Cohort verdict (cross-target)

Per-lane consolidated table:

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

Cumulative KEEP / REINVENT / DISCARD totals across all four targets.

### §3 — Cross-target conflicts

Where the four hardening reports disagree on shared substance:

- PASS-1 + PASS-2 + PASS-3 each surface a finding about a substrate (e.g., the tape + direct-to-struct union); SYNTHESIS ratifies one variant; Hardener-4 surfaces the conflict
- Where Phase-1 hardening flags X as REINVENT or DISCARD and SYNTHESIS's master plan ratifies X as KEEP, the conflict surfaces

Per-conflict table:

| Conflict | Sources | Hardener-N verdicts | Resolution recommendation |
|---|---|---|---|

### §4 — Punch list consolidation

Cumulative punch list across all four reports, deduplicated:

- Items appearing in multiple reports collapse to one entry citing all sources
- Items unique to one report carry that report's attribution
- Items where reports disagree on surgery carry the most surgical of the proposed surgeries

### §5 — Final readiness verdict

The cohort verdict feeds one of three decisions:

- **READY** — every report returns ready (or amendment-required with surgeries narrow enough to fold without re-draft)
- **AMENDMENT-REQUIRED** — at least one report returns amendment-required with substantive surgeries; an amendment agent runs against the punch list before tranche drafting opens
- **RE-DRAFT** — at least one report returns re-draft, OR cumulative findings surface architectural conflicts that re-draft must resolve

The verdict feeds the next phase:

- READY → user dispatches per-tranche full-spec drafting (one agent per tranche A-J, ~3,000-5,000 lines per tranche, inheriting from BA-BD per `restart/inheritance/INDEX.md`)
- AMENDMENT-REQUIRED → user dispatches an amendment agent against the consolidated punch list; amendments commit; the user re-runs HARDENING-ORCHESTRATOR (or a narrow-scope Hardener-N for affected targets) to verify
- RE-DRAFT → user identifies which PASS or SYNTHESIS re-runs (or the orchestrator returns re-draft against itself if the conflict is cross-target architectural)

### §6 — Voice + discipline locks

(Per `restart/README.md` §13. Calibrated; archaic-permissive; no metalanguage; path:line citations; tables liberal.)

### §7 — Closing posture

One paragraph summary of the consolidated verdict + named next step.

## Methodology

You orchestrate. You do not audit yourself.

- **Dispatch Phase 1 in parallel** when PASS-1/2/3 syntheses commit. Three Agent invocations in a single message; each `run_in_background: true`; each carries the per-target dispatch prompt below.
- **Dispatch Phase 2 serially** when Phase 1 + SYNTHESIS commit. One Agent invocation; `run_in_background: true`.
- **Synthesise Phase 3 directly** when Phase 1 + Phase 2 commit. Read the four reports; consolidate; commit `HARDENING-CONSOLIDATED.md`.
- **Do not relitigate** the per-target findings. The per-target hardeners are the adversaries; you are the consolidator.
- **Surface cross-target conflicts** explicitly. A conflict that the four targets implicitly disagreed about and no individual hardener caught is a synthesis-level finding.

### Per-target dispatch prompts

For each Phase-1 hardening agent (and the Phase-2 agent), compose a dispatch prompt that includes:

- The agent's target (PASS-1 / PASS-2 / PASS-3 / MASTER-PLAN)
- The full path to the target output(s) the agent reads
- Reference to the existing `restart/prompts/HARDENING.md` operational contract
- The 14 locks at `restart/locks/14-LOCKS.md` (settled)
- The voice + discipline locks
- The per-item Pro/Con/Explication/Challenge discipline
- The hard cap (45 minutes per PASS target; 90 minutes for MASTER-PLAN target; incremental-commit cadence for MASTER-PLAN)
- The output path and the commit message format
- The cross-tranche scope boundary (touch ONLY the agent's HARDENING-{TARGET}.md output; do NOT modify the target itself or any other restart subdir)

The dispatch prompts are NOT pre-written here — you compose them at dispatch time, parameterising for the specific target. The existing `restart/prompts/HARDENING.md` is the per-target template; your role is to invoke it with the right target.

## Hard cap

Phase 1 wait: ~75 minutes (longest-running Phase-1 hardener; MASTER-PLAN target excluded from Phase 1).
Phase 2 wait: ~120 minutes (90-min MASTER-PLAN cap + 30-min schedule slack).
Phase 3 (your synthesis): 45 minutes.

Total orchestrator wall: ~4 hours from Phase-1 dispatch to Phase-3 commit.

## Output commits

You commit Phase 3 only. Phase-1 and Phase-2 hardeners commit their own reports per `HARDENING.md` contract.

Phase 3 commit: `docs(restart/audit/hardening): consolidate four-target hardening — verdict {READY / AMENDMENT-REQUIRED / RE-DRAFT}`

The commit body summarises cohort verdict + cross-target conflicts (if any) + final decision in one paragraph.

## Cross-tranche scope boundary

You touch ONLY:
- The four Agent dispatch invocations (Phase 1 + Phase 2; via the Agent tool)
- `restart/audit/hardening/HARDENING-CONSOLIDATED.md` (Phase 3)

You do NOT modify:
- `restart/prompts/` (suite definition; read-only)
- `restart/locks/`, `restart/corpora/`, `restart/inheritance/` (read-only)
- `restart/README.md` (gestalt anchor; only the user amends)
- The four hardening reports (they are the per-target hardeners' outputs)
- The PASS-1/2/3 syntheses (they are the targets' outputs)
- The SYNTHESIS trio (`ARCHITECTURE.md` + `MIGRATION.md` + `MASTER-PLAN.md`; they are the targets' outputs)
- `crates/`, `docs/`, `restart-archive-2026-05-04/`

## Background

The greenfield restart's hardening is single-round. Four targets are audited; one consolidated verdict feeds the next phase. There is no Stage-2 hardening; there is no Stage-3 meta-review; the prior restart's three-stage compounding was contrivance.

After your Phase-3 commit, the user's next step is:

- READY → per-tranche full-spec drafting (out of orchestrator scope)
- AMENDMENT-REQUIRED → amendment agent dispatch against the consolidated punch list (out of orchestrator scope; the user invokes)
- RE-DRAFT → PASS / SYNTHESIS re-run (out of orchestrator scope)

The 14 locks are settled. The precepts are settled. The 35-answer interrogation is settled. The greenfield mandate is settled. You orchestrate; you consolidate; you do NOT relitigate.

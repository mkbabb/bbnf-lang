# AZ-III REAUDIT Lane 4 - Instructions, Precepts, Process

Date: 2026-04-30
Lane: 04 - Instructions / Precepts / Process
Mode: read-only audit
Repo head audited: HEAD (`d5179b8a docs(az-iii.W0): add build and test
baseline to quarantine ledger`)

Source files inspected:

- `docs/precepts/README.md`
- `docs/precepts/instructions/{README,ORCHESTRATION,CONSUMING,LESSONS-LEARNED}.md`
- `docs/precepts/instructions/tranche/{README,SPEC,WAVE_SPEC,AGENT_DISPATCH_TEMPLATE,RESEARCH,CHALLENGE,START,DOC_UPDATE_WAVE}.md`
- `docs/precepts/audits/overfitting-audit.md`
- `docs/tranches/AZ-II/audit/AZ-II-HARDENING-AUDIT-2026-04-29.md`
- `docs/tranches/AZ-II/audit/O3a-A1-{research,plan,redress}.md` (full triad)
- `docs/tranches/AZ-III/audit/{SIX-AGENT-SYNTHESIS-2026-04-30,W0-commit-repair-plan,W0-dispatch-packets,W0-state-ledger}.{md,txt}`
- `docs/tranches/AZ-III/AZ-III.md` and all six AZ-III wave specs
- `docs/tranches/AZ-II/PROGRESS.md` (last 200 lines)
- `git log --pretty -50` and full body inspection of last ~50 commits
- `~/.codex/skills/commit-discipline/SKILL.md` and `~/.codex/memories/commit-discipline.md`

## 1. Precepts Coverage Matrix

| Doc | Covers | Gaps | Duplication risk |
|---|---|---|---|
| `precepts/README.md` | top-level edicts, repo layout, submodule reference | does not name the lessons-learned ledger as canonical, no read-order summary inline | low |
| `instructions/README.md` | edicts, code/commit/gate discipline, body-required list | no rule for sibling-worktree-per-agent, no rule for parallel-write disjointness checks, no read-size preflight, no single-cargo-per-target rule | partial overlap with `commit-discipline` Codex skill (commit body list duplicated) |
| `instructions/ORCHESTRATION.md` | wave model, dispatch contract, integration verification, stalls/scope dilation, triumvirate, status | triumvirate triggers are listed but not auto-thresholded (no "JSONL quiet >15min" or "first-pass no-commit" trigger), no anti-polling rule, no `run_in_background`/Monitor guidance, no race-prevention rule for parallel agents on shared files | overlaps with `tranche/SPEC.md` §Scope Reveal |
| `instructions/CONSUMING.md` | submodule wiring, read order, local instruction split | no checklist for verifying local-vs-shared split when migrating | low |
| `instructions/LESSONS-LEARNED.md` | nine ledger entries; format defined | missing entries for: agent racing on shared files, empty sub-agent return, triumvirate auto-trigger thresholds, HARD CAP, read-size preflight, no-polling-loops, single-cargo-per-target, scope-reveal new-letter rule, dispatch-template HARD-CAP enforcement | low |
| `tranche/README.md` | read order, lifecycle | does not state when challenge is mandatory vs waivable in stronger terms | low |
| `tranche/SPEC.md` | plan shape, wave rules, scope reveal protocol, brittleness window, close criteria | scope reveal §"Default to absorb" is too soft and contradicts memory rule "scope pivots mid-tranche open a NEW tranche letter"; triumvirate dispatch listed only as last bullet, no HARD-CAP mention | mild duplication with `ORCHESTRATION.md` §Stalls |
| `tranche/WAVE_SPEC.md` | required sections (header, state, scope, file bounds, agent units, hard gate, lint cadence, artefacts, commit plan, dependencies, archaeology), prohibitions | no required §Triumvirate Dispatch section (W0-W5 of AZ-III added it ad-hoc); no §Parallel Write Bounds enforcement; no read-size preflight; no `Do NOT touch` regex/exclude examples; no hard cap on agent prompt size | low |
| `tranche/AGENT_DISPATCH_TEMPLATE.md` | basic prompt skeleton (~50 lines) | no HARD CAP slot, no sibling-worktree pin enforcement, no return-format-empty halt rule, no `run_in_background`/Monitor mention, no read-size preflight, no `CARGO_TARGET_DIR` per-agent rule, no commit-body checklist embedded | duplicates parts of `ORCHESTRATION.md` §Dispatch Contract loosely |
| `tranche/RESEARCH.md` | research wave shape, canonical angles, prompt skeleton | no rule that empty research return triggers triumvirate redispatch, no time-boxing | low |
| `tranche/CHALLENGE.md` | challenge wave purpose, half-research-count rule, dispatch shape, synthesis | no required artefact path under `audit/`, no rule that challenge findings must reach the plan | low |
| `tranche/START.md` | resume prompt | no rule that orchestrator must reconcile TaskList/process state before dispatch | low |
| `tranche/DOC_UPDATE_WAVE.md` | inputs, outputs, close check | does not require the doc-update task to also append to LESSONS-LEARNED when a new reusable failure surfaced | low |
| `audits/overfitting-audit.md` | dead-substrate audit prompt | not part of tranche read-order; orchestrators frequently miss it | not duplicated, but under-referenced |

**Net coverage**: edicts, wave model, dispatch contract, scope reveal, brittleness, doc-update, research/challenge are all covered. The orchestration **operating discipline** (sibling worktrees, parallel-write disjointness enforcement, anti-polling, single-cargo-per-target, read-size preflight, HARD CAP enforcement, empty-return redispatch, scope-pivot new-letter rule, triumvirate auto-thresholds) is **not codified**. Process artefacts in AZ-II/AZ-III invented those rules ad hoc and they are inconsistently applied.

## 2. Recurrent-Friction Inventory

Top 15 dispatch/wave/commit failures observed in AZ-II/AZ-III artefacts and recent git log, each with proof:

1. **Bodyless large/deletion/gate commits**.
   AZ-II cutover.O history was rewritten en masse by `W0-commit-repair-plan.md`
   because dozens of `fix(emitter/...): delete X` commits had no body. The
   message-only repair (commits `dcb41e67`..`d5179b8a`) added templated bodies
   that all read "Land the implementation slice named in the subject" -
   formulaic, not evidence. Lesson: `LESSONS-LEARNED.md` 2026-04-30 entry
   "Bodyless Large Commits Erase Gate Evidence" exists, but the rule must
   land **at commit time**, not after a wholesale reword.

2. **Generic `az-ii` tranche scope on source commits**.
   `instructions/README.md` §Commit Discipline already forbids this, but
   `863de6a5 chore(az-ii): cutover.{C,E,F}-PARTIAL move to audit/` and
   peers in the rewritten range proved it kept landing. Cited in
   `SIX-AGENT-SYNTHESIS-2026-04-30.md` finding 7.

3. **Templated/formulaic commit bodies**.
   The W0 commit-message rewrite produced 30+ commits with identical body
   shells (e.g. "Remove the obsolete surface named in the subject..."). This
   technically satisfies "body present" but defeats the rule's intent.
   Evidence: `git log --pretty -100`. Not currently codified - precepts say
   "body required" but never "body must be evidence-bearing not templated".

4. **Dirty main worktree blocking dispatch**.
   `SIX-AGENT-SYNTHESIS-2026-04-30.md` finding 6: "current dirty main/index
   state blocks further implementation dispatch until a quarantine wave
   records and cleans it". This caused W0 to be invented. `ORCHESTRATION.md`
   §Integration §3 ("Do not dispatch implementation agents from a dirty or
   ambiguous main worktree") exists but lacks an actionable preflight script.

5. **Agents racing on shared files**.
   Memory `feedback_agent_orchestration` records this. AZ-II.cutover saw
   triple-overlap on `crates/core/src/grammar/generated/*.rs` between regen
   commits and emitter fixes. Not in `ORCHESTRATION.md` or `WAVE_SPEC.md`.

6. **Empty sub-agent returns mistaken for "scope already done"**.
   Memory `feedback_redispatch_empty_return`. The W0-dispatch-packets.md
   shows redispatch was needed; precepts have no rule.

7. **Triumvirate triggered by user prompt, not autonomously**.
   AZ-II O3a triumvirate cohorts (J1/C1/S1/P1/A1) were dispatched after
   `SIX-AGENT-SYNTHESIS` synthesis - i.e., after a user-driven audit, not
   after autonomous detection. Memory `feedback_triumvirate_auto_trigger`
   says JSONL quiet >15min OR first-pass no-commit must auto-trigger.
   `ORCHESTRATION.md` §Triumvirate lists when to use it but has no
   measurable thresholds.

8. **Plan/Redress role confusion in O3a triads**.
   Memory `feedback_triumvirate_discipline` ("research commits attribution;
   plan commits plan; only then redress dispatches; never merge roles").
   `O3a-A1-redress.md` line 6: "This agent did not edit source. Source and
   archive redress waits for the O3a-A1 plan amendment." - i.e., redress
   stalled on missing plan amendment because plan agent did not amend the
   shared wave spec. Triumvirate spec `ORCHESTRATION.md` §Triumvirate
   describes it but does not enforce role outputs (research artefact, plan
   amendment, redress diff).

9. **No HARD CAP on agent prompts**.
   `AGENT_DISPATCH_TEMPLATE.md` has no HARD CAP slot. The user's dispatch
   prompts add it manually ("HARD CAP: 25 min"). Memory
   `feedback_dispatch_hard_cap`: "Every dispatch carries 'HARD CAP: N min.
   At 0.9N commit, at N halt'; defaults 20/15/30 research/plan/redress".
   Not in template.

10. **Scope reveal handled in-place instead of opening new letter**.
    `tranche/SPEC.md` §Scope Reveal step 1 says "Default to absorb". Memory
    `feedback_new_tranche_new_doc`: "Scope pivots mid-tranche open a NEW
    tranche letter + new docs/tranches/XX.md; never continue old
    numbering". AZ-II.cutover.O3a accumulated **five sub-cohort triads**
    (J1/C1/S1/P1/A1) in-place rather than opening AZ-II.5 / AZ-III earlier.
    AZ-III itself only opened on 2026-04-30 after audit pressure.

11. **No-polling rule absent**.
    Memory `feedback_no_polling_loops`, `feedback_bg_then_monitor`,
    `feedback_status_tick_cadence`: never poll, use `run_in_background` +
    Monitor, emit a 5-min status tick when silent. Not in
    `AGENT_DISPATCH_TEMPLATE.md` or `ORCHESTRATION.md`.

12. **Single-cargo-per-target violations**.
    Memory `feedback_single_cargo_per_target`: at most one cargo invocation
    per `CARGO_TARGET_DIR` because lock contention silently serialises.
    `O3a-A1-redress.md` lines 21-22 manually use a per-worktree
    `CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress/target/o3a-a1-redress`,
    but this is per-prompt convention, not enforced by template.

13. **Read-size preflight skipped**.
    Memory `feedback_read_size_preflight`: `wc -l` before Read on files
    >2K lines. `O3a-A1-research.md` cites span ranges manually because
    `bootstrap_parser.rs` is large. No rule in `RESEARCH.md`.

14. **Lint/format cadence saved for the end**.
    `LESSONS-LEARNED.md` 2026-04-29 entry "Docs Are Part Of Wave Close" and
    `WAVE_SPEC.md` §7 already require regular cadence, but
    `AGENT_DISPATCH_TEMPLATE.md` does not embed it - so individual agents
    skip it and the orchestrator scrambles. AZ-III W0 state ledger lines
    44-53 show four sibling repos with mixed clippy/test FAIL surfaces
    that built up because cadence drifted.

15. **Stale evidence artifacts cited as current truth**.
    AZ-II.cutover.O5 PROGRESS lines 660-691 lists "Blocked evidence:" with
    artefacts dated before O5 dirty state was reconciled. `WAVE_SPEC.md`
    requires "Verification Artefacts" but does not require **freshness
    timestamps** or invalidation rules.

## 3. Precepts Gaps

Friction patterns above that have **no codification anywhere in `precepts/instructions/`**:

| Gap | Where it should live | Proposed mechanism |
|---|---|---|
| Sibling-worktree-per-agent enforcement | `ORCHESTRATION.md` §Integration + `AGENT_DISPATCH_TEMPLATE.md` | Required `Worktree:` slot with absolute path; orchestrator preflight `git worktree list` matches dispatched agents |
| Parallel-write disjointness preflight | `WAVE_SPEC.md` §4 File Bounds | Add §4a "Disjointness Check" requiring orchestrator to assert no two units share a `modify` path |
| Empty-sub-agent-return halt rule | new `tranche/RETURNS.md` or new `ORCHESTRATION.md` §Returns | Empty return is not "no work needed"; orchestrator redispatches verbatim with prior-worktree pointer |
| Triumvirate auto-trigger thresholds | `ORCHESTRATION.md` §Triumvirate | JSONL quiet >15min, first-pass no-commit, three diagnostic-loop iterations -> mandatory triumvirate, not optional |
| HARD CAP slot in dispatch | `AGENT_DISPATCH_TEMPLATE.md` (mandatory section) | Defaults: research 20m, plan 15m, redress 30m, audit 25m |
| Anti-polling / `run_in_background` | `ORCHESTRATION.md` new §Polling | Forbid `tail -f`, `ps aux` loops; require Monitor + run_in_background for >60s commands |
| Read-size preflight | new `tranche/AGENT_RUNBOOK.md` or extend `RESEARCH.md` | `wc -l` before `Read` on files >2K lines; `grep+offset` for generated.rs |
| Single-cargo-per-target | `ORCHESTRATION.md` new §Build Concurrency | Only one cargo invocation per `CARGO_TARGET_DIR`; sibling worktrees set their own `target/` |
| Scope-pivot new-letter rule | `tranche/SPEC.md` §Scope Reveal step 1 | Replace "Default to absorb" with "Default to absorb only when file bounds widen by <=2 paths and hard gate is unchanged; otherwise open `{LETTER+1}.md` or `{LETTER}-II.md` per thesis-stable check" |
| Templated-body forbiddance | `instructions/README.md` §Commit Discipline + commit-discipline skill | Bodies must cite a runtime command output, scan path, file count, or commit hash; "Land the implementation slice named in the subject" is template-only and rejected |
| Status-tick cadence for orchestrator | `ORCHESTRATION.md` new §Status | One-line tick every ~5min of orchestrator-silent wait |
| Triumvirate role enforcement | `ORCHESTRATION.md` §Triumvirate | Research must produce artefact under `audit/{COHORT}-research.md`; Plan must amend wave spec or write `audit/{COHORT}-plan.md` with **Exact Wave-Amendment Text** section; Redress may not edit source until plan amendment is committed |
| Verification-artefact freshness | `WAVE_SPEC.md` §8 | Each artefact line includes timestamp and the exact base SHA the artefact was produced against |
| Quarantine wave pattern | `tranche/SPEC.md` and a new `tranche/QUARANTINE.md` | Codify W0 quarantine wave as a reusable pattern (state ledger, commit archaeology, dispatch packet authoring) |

## 4. Wave Formulation Gaps

`WAVE_SPEC.md` is detailed but reality has stretched it in five concrete areas. Each AZ-III wave (W0-W5) had to invent local content to compensate.

1. **No required §Triumvirate Dispatch section**.
   AZ-III W0-W5 each manually added `## Triumvirate Dispatch` (W0:73-79, W1:65-72, W2:77-84, etc.). Should be a required §3a section in `WAVE_SPEC.md`.

2. **No §Parallel Write Bounds**.
   `WAVE_SPEC.md` §4 File Bounds is per-unit, not cross-unit. AZ-III W1 has `crates/core/**` in W1.1 and W1.3 - silent overlap. Should require a `Disjointness:` line per unit confirming no other same-wave unit shares `modify` paths.

3. **No agent-prompt size cap**.
   `ORCHESTRATION.md` §Dispatch Contract advises ~700 words but does not enforce. Wave specs should enforce a per-unit prompt budget of ~600 words; if larger, split.

4. **No required §Worktree Plan**.
   AZ-III W0 archaeology (lines 124-128) and W0-state-ledger.txt show worktrees were assigned ad-hoc. `WAVE_SPEC.md` should add §4a `Worktree Plan` listing absolute-path worktrees per agent unit.

5. **Scope reveal default is too permissive**.
   `WAVE_SPEC.md` does not mention scope reveal at all (delegates to `SPEC.md`). Should add §5a: "If an agent unit hits scope reveal, halt; orchestrator opens triumvirate or new wave spec; this wave does not silently absorb."

## 5. Triumvirate Spec Status

Comparing `ORCHESTRATION.md` §Triumvirate to AZ-II O3a artefacts (J1/C1/S1/P1/A1 triads):

**What spec says** (lines 81-92):
- Research: identify root cause, prior attempts, exact expanded scope.
- Plan augment/synthesis: amend file bounds, hard gates, wave docs.
- Redress/redeployment: implement the amended scope on clean bounds.
- Use it for diagnostic loops, unusable first-pass returns, repeated empty returns, scope dilation whose mode/root cause is unclear.

**What O3a-A1 triad actually shows**:
- `O3a-A1-research.md` (296 lines): cites paths, line ranges, focused reruns, distinguishes live vs historical, gives delete/archive/repair options, recommends dispositions. **Aligned**.
- `O3a-A1-plan.md` (346 lines): includes a `## Exact Wave-Amendment Text` section with literal markdown blocks for `O5.md`, `O6.md`, `O7.md`. **Strongly aligned and exemplary** - this is the missing template piece.
- `O3a-A1-redress.md` (207 lines): explicitly halts at line 6 ("This agent did not edit source") because plan amendment was not yet integrated. **Aligned with intent but proves redress order is fragile**: redress was dispatched before plan amendment was committed; so redress did the only legal thing (reproduce + halt), which is correct discipline but wastes dispatch slot.

**What's missing from precepts**:
- The "Exact Wave-Amendment Text" section in O3a-A1-plan.md is **a template the triumvirate proves is needed** but is not in `ORCHESTRATION.md` or `tranche/CHALLENGE.md`. Plan agents need a concrete output spec, not just "synthesis".
- The redress halt-on-missing-plan-amendment behavior is not documented; it should be: "Redress must verify the wave spec was amended before editing source; if not, redress halts, reports, and the orchestrator runs the plan amendment first."
- `ORCHESTRATION.md` §Triumvirate does not name the **artefact paths**: research goes to `audit/{COHORT}-research.md`, plan goes to `audit/{COHORT}-plan.md` with a mandatory `## Exact Wave-Amendment Text` section, redress goes to `audit/{COHORT}-redress.md` (and only commits source after plan amendment lands).
- No mandatory time-boxing: research/plan/redress should each carry the HARD CAP defaults from feedback memory (20/15/30 min).
- Triumvirate should be **mandatory** (not optional) for: stalls >15min JSONL quiet, first-pass no-commit returns, three diagnostic-loop iterations, scope-pivot reveals.

## 6. Agent Dispatch Template Gaps

Current `AGENT_DISPATCH_TEMPLATE.md` (50 lines) is missing:

1. **HARD CAP slot** - mandatory.
2. **Sibling-worktree pin** - the template says "Worktree: {ABSOLUTE_WORKTREE_PATH}" but does not require the orchestrator to verify it via `git worktree list` before dispatching, nor does it require the agent to refuse work if the worktree is not the dispatched path.
3. **CARGO_TARGET_DIR per-agent** - missing.
4. **Read-size preflight rule** - missing (causes Read failures on large generated files).
5. **Empty-return halt rule** - "Return:" section lists 5 items but has no rule for what if all 5 are empty/null. Should include: "If scope is genuinely empty, return that explicitly with evidence; an empty/null return without evidence triggers redispatch."
6. **No-polling clause** - missing.
7. **Status-tick cadence** - missing (orchestrator-side, but should be in template comment for orchestrator awareness).
8. **Commit-body checklist embedded** - the agent's commit return-format slot (item 5) does not list which body categories trigger requirement. Should embed the body-required list inline.
9. **Triumvirate trigger conditions** - the bottom non-negotiables list "halt and report if scope reveal invalidates this prompt" but does not name the specific triumvirate triggers.
10. **Read budget** - no rule that the agent should preflight `wc -l` before reading large files; for a 10K-line generated file, a Read without offset is wasteful.
11. **Lint/format cadence** - the wave spec has it, but the agent prompt does not embed it. Each agent should run `cargo fmt --check` + `git diff --check` before returning.
12. **CARGO concurrency** - dispatched agents working in sibling worktrees should each have their own `CARGO_TARGET_DIR` to avoid lock contention.

## 7. Commit Discipline State

Spot-check of last 30 commits (HEAD..HEAD~30):

| Commit | Subject scope | Body present | Body evidence-bearing | Verdict |
|---|---|---|---|---|
| `d5179b8a` | `docs(az-iii.W0)` | yes | yes - cites cargo iter-check/test, parse-that, pprint surfaces | clean |
| `e11f3665` | `docs(az-iii.W0)` | yes | yes - cites git diff --check, fmt --all output | clean |
| `b20ea61b` | `docs(az-iii.waves)` | yes | yes - cites the six-agent audit findings, lists what changed | clean |
| `f5387c95` | `docs(bc)` | yes | yes - cites SHA `e490e8e` and clean state | clean |
| `740aa4a3` | `docs(precepts)` | yes | yes - cites staged diff scope, --check output | clean |
| `0fed1569` | `docs(az-iii)` | yes | yes - thesis-bearing | clean |
| `d31e64ab` | `docs(precepts)` | yes | yes | clean |
| `dcb41e67` | `refactor(lower/view-walk)` | yes | **NO - templated**: "Land the implementation slice named in the subject..." | templated post-rewrite |
| `fb46a734` | `chore(bench/cutover)` | yes | **NO - templated** | templated post-rewrite |
| `50b21cd8` | `docs(parity/tape)` | yes | **NO - templated** | templated post-rewrite |
| `99413e42` | `test(goldens/tape)` | yes | **NO - templated** | templated post-rewrite |
| `219eb086` | `refactor(runtime/compound-record)` | yes | **NO - templated** | templated post-rewrite |
| `11fcddf7` | `fix(dispatch/alt)` | yes | **NO - templated** | templated post-rewrite |
| `c3f86944` | `fix(grammar/generated)` | yes | **NO - templated** | templated post-rewrite |
| `38a13ef8` | `fix(bench/json-competitors)` | yes | **NO - templated** | templated post-rewrite |
| `ec18aaa6` | `fix(projection/materializer)` | yes | **NO - templated** | templated post-rewrite |
| `452aff1a` | `test(emitter/struct-direct)` | yes | **NO - templated** | templated post-rewrite |
| `6a6ca1fd` | `fix(runtime/tape)` | yes | **NO - templated** | templated post-rewrite |
| `5e99871d` | `refactor(lower/pratt)` | yes | **NO - templated** | templated post-rewrite |
| `15bd381a..72f05435` (10 commits) | various `fix(emitter/...)`, `fix(runtime/...)` | yes | **NO - all templated** | templated post-rewrite |
| `aa562f4a..566236bb` (8 commits) | various deletion/fix | yes | **NO - all templated** | templated post-rewrite |
| `0117cb52` | `fix(types/json)` | yes | templated | templated post-rewrite |

**Summary**: 7 of 30 (the recent docs commits) carry evidence-bearing bodies. The other 23 (the AZ-II cutover.O range) carry templated bodies that satisfy "body present" but not "body evidence-bearing". This is exactly the AZ-II commit archaeology problem: the W0 commit-message rewrite (recorded in `W0-commit-repair-plan.md`) chose to add formulaic bodies rather than reconstructing per-commit evidence. The repair plan acknowledges this on lines 36-39: "The repair deliberately does not claim that historical commits have newly passed tests; AZ-III W1-W5 own the current evidence."

**This is a known, accepted, but not yet codified state.** The lesson must land as a precept rule: bodies must cite a runtime command, scan path, file count, or commit hash - and post-hoc message rewrites can record only "history-repair, evidence routed elsewhere" but must not pretend per-commit evidence was new.

Subject scopes are clean: post-rewrite all use `fix(emitter/wrap-tape)`, `refactor(lower/view-walk)`, `chore(bench/cutover)`, etc. - concrete mechanism scopes, no `az-ii` generic scope. The `instructions/README.md` rule landed.

## 8. Concrete Refinement Proposals

Each is an exact diff specification. File paths are absolute.

### 8.1 `docs/precepts/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md`

Replace the entire prompt skeleton with the expanded skeleton below (additions marked `# NEW`). The template grows from 50 to ~95 lines but the per-task surface stays under 700 words.

```markdown
You are agent {AGENT_ID} for tranche {LETTER}, wave {WAVE}.

HARD CAP: {N} min. At 0.9N elapsed, commit current state. At N, halt.        # NEW
Defaults: research=20, plan=15, redress=30, audit=25.                        # NEW

Worktree:
{ABSOLUTE_WORKTREE_PATH}                                                     # MUST match `git worktree list` on the orchestrator side. Refuse if mismatched. # NEW

CARGO_TARGET_DIR:
{ABSOLUTE_WORKTREE_PATH}/target/{AGENT_ID}                                   # NEW: one cargo invocation per CARGO_TARGET_DIR

Read first:
1. `docs/precepts/instructions/README.md`
2. `docs/precepts/instructions/ORCHESTRATION.md`
3. `docs/precepts/instructions/tranche/SPEC.md`
4. `docs/tranches/{LETTER}/{LETTER}.md`
5. `docs/tranches/{LETTER}/waves/{WAVE}.md`
6. {task-specific files}

Read-size preflight:                                                         # NEW
- `wc -l` before `Read` on any file >2K lines.
- For generated.rs / large transcripts, use `grep`+offset, not full Read.

Scope:
{numbered scope bullets}

May modify:
{paths}

May read:
{paths}

Do not touch:
{paths}

Hard gate:
{gate and artefact expected}

Lint cadence (run before each return and before any commit):                  # NEW
- `cargo fmt --all -- --check`
- `cargo clippy --profile ax-iter` (if source touched)
- `git diff --check`

Long-running commands:                                                       # NEW
- Any command expected to run >60s uses run_in_background + Monitor.
- Never poll via `tail -f`, `ps aux` loops, or sleep loops.

Return:
1. summary (max 300 words);
2. files changed;
3. evidence path or command output (file path, not pasted output);
4. known misses or risks;
5. commit hash, subject, body summary, and verification evidence, if commits
   are part of this workflow.

Empty-return rule:                                                           # NEW
If the scope is genuinely empty, say so explicitly with the evidence that
proves it (commands run, paths inspected). An empty/null return without
evidence is treated as a failed dispatch and triggers triumvirate.

Non-negotiables:
- stay inside file bounds;
- no stubs or disabled paths;
- no substrate without consumer;
- no generic tranche-only scope for implementation commits;
- no bodyless broad, generated, deletion, gate, benchmark, profiling, or status
  commits;
- no templated commit bodies; bodies must cite a runtime command, scan path,
  file count, or commit hash;                                                # NEW
- halt and report if scope reveal invalidates this prompt;
- halt and report after three diagnostic-loop iterations;                    # NEW
- halt and report if a 60s+ command would block the prompt.                  # NEW
```

### 8.2 `docs/precepts/instructions/tranche/WAVE_SPEC.md`

Add three new required sections.

After current §3 Scope, insert:

```markdown
### 3a. Triumvirate Dispatch                                                 # NEW

State which class of stall, scope reveal, or unclear-root-cause condition in
this wave triggers a triumvirate (research + plan augment + redress). At
minimum, list:

- the file bounds whose expansion would invalidate the wave;
- the hard-gate failures that would not be local-edit-recoverable;
- the diagnostic loops whose third iteration must halt.

Triumvirate is mandatory, not optional, for these triggers; the orchestrator
may not redispatch the failing unit alone.
```

After current §4 File Bounds, insert:

```markdown
### 4a. Disjointness                                                         # NEW

Confirm that no two agent units share a `modify` or `modify-carve` path. If
two units must touch the same file, fold them into one unit or sequence them
across sub-waves; do not run them in parallel.

### 4b. Worktree Plan                                                        # NEW

| Agent unit | Sibling worktree absolute path | CARGO_TARGET_DIR |
|---|---|---|
| {LETTER}.W<N>.<x> | `/path/to/sibling-worktree-<x>` | `<worktree>/target/<x>` |

The orchestrator runs `git worktree list` and `git worktree add` before
dispatch.
```

In §8 Verification Artefacts, replace with:

```markdown
### 8. Verification Artefacts                                                # MODIFIED

Each artefact line MUST include a freshness timestamp and the base SHA the
artefact was produced against. Stale artefacts may not be cited as current
truth; rerun before close.

Format:

| Artefact | Path | Produced at SHA | Timestamp |
|---|---|---|---|
```

### 8.3 `docs/precepts/instructions/ORCHESTRATION.md`

In §Triumvirate (lines 81-92), expand to:

```markdown
## Triumvirate                                                               # MODIFIED

The triumvirate is the default recovery shape for non-environmental stalls.

### Roles and Required Artefacts

1. **Research**: identify root cause, prior attempts, and exact expanded
   scope. Output: `docs/tranches/{LETTER}/audit/{COHORT}-research.md` with
   evidence (file paths, line ranges, command output, focused reruns).

2. **Plan augment/synthesis**: amend file bounds, hard gates, and wave docs.
   Output: `docs/tranches/{LETTER}/audit/{COHORT}-plan.md` with a mandatory
   `## Exact Wave-Amendment Text` section containing literal markdown blocks
   that the orchestrator copies into the affected wave specs.

3. **Redress/redeployment**: implement the amended scope on clean bounds.
   Output: `docs/tranches/{LETTER}/audit/{COHORT}-redress.md` plus source
   commits if plan amendment is committed first. **Redress halts and reports
   if the plan amendment has not yet landed.**

### Auto-Triggers (Mandatory)                                                # NEW

The orchestrator must dispatch a triumvirate when any of these conditions
holds, without waiting for user prompt:

- a sub-agent's stdout/stderr (or JSONL transcript) has been quiet for >15
  minutes;
- a first-pass dispatch returns with no commit and no evidence;
- a sub-agent has run three diagnostic-loop iterations without isolating a
  root cause;
- scope reveal invalidates the current wave's file bounds, hard gate, or
  substrate-with-consumer wiring.

### HARD CAPs                                                                # NEW

Every triumvirate dispatch carries `HARD CAP: N min. At 0.9N commit, at N
halt.`. Defaults: research 20, plan 15, redress 30. Audit lanes default to 25.

### Triumvirate vs Direct                                                    # MODIFIED

Use the triumvirate for diagnostic loops, unusable first-pass returns,
repeated empty returns, or scope dilation. Do not use it for simple
mechanical edits or absorbable reveals the orchestrator can finish directly.
```

After §Status, append three new sections:

```markdown
## Build Concurrency                                                         # NEW

At most one cargo invocation in flight per `CARGO_TARGET_DIR`. Sibling
worktrees set their own `CARGO_TARGET_DIR=<worktree>/target/<agent>` to
avoid lock-file contention that silently serialises wall-clock.

## Long-Running Commands                                                     # NEW

Any command expected to run >60 seconds uses `run_in_background` + Monitor.
Never poll via `tail -f`, `ps aux`, or sleep loops; use the harness's
notification path. The orchestrator may emit a one-line status tick every
~5 minutes of orchestrator-silent wait.

## Returns                                                                   # NEW

An empty sub-agent return is treated as a failed dispatch, not as scope
reveal. The orchestrator must redispatch verbatim (with the prior worktree
pointer) before triumvirate; if the second dispatch also returns empty,
triumvirate is mandatory.
```

In §Stalls And Scope Dilation, modify §Scope dilation step 1 to:

```markdown
- absorb only when file-bound expansion is <=2 paths and the hard gate is
  unchanged;                                                                 # MODIFIED
- if the thesis still holds but bounds widen, open `{LETTER}-II` or
  `{LETTER}.5`;                                                              # MODIFIED
- if the thesis changes, close the current tranche honestly and open the
  next letter (`{LETTER+1}`).
```

### 8.4 `docs/precepts/instructions/LESSONS-LEARNED.md`

Append the following entries:

```markdown
## 2026-04-30 - Sibling Worktrees Prevent Agent Races

- **Source**: bbnf-lang AZ-II cutover.O, AZ-III W0 quarantine.
- **Failure**: parallel agents writing to overlapping paths in the same
  worktree silently corrupted each other's diffs.
- **Rule**: every parallel agent unit runs in a sibling worktree with its
  own `CARGO_TARGET_DIR`; the wave spec lists the worktree plan and asserts
  per-unit modify-path disjointness before dispatch.
- **Check**: orchestrator runs `git worktree list` before dispatch and
  rejects any wave whose units overlap on `modify` paths.

## 2026-04-30 - Empty Returns Are Failed Dispatches

- **Source**: bbnf-lang AZ-II.cutover sub-agent runs.
- **Failure**: empty/null sub-agent returns were misread as "scope already
  done" and the orchestrator advanced; later it became clear the agent
  silently aborted.
- **Rule**: empty returns are failed dispatches. The orchestrator
  redispatches verbatim once (with the prior worktree pointer); a second
  empty return triggers a mandatory triumvirate.
- **Check**: every return packet includes evidence (commands run, paths
  inspected) even when the scope was confirmed empty.

## 2026-04-30 - Triumvirate Auto-Triggers

- **Source**: bbnf-lang AZ-II O3a J1/C1/S1/P1/A1 triads (only dispatched
  after user-driven six-agent audit).
- **Failure**: the orchestrator waited for user instruction to dispatch a
  triumvirate; this delayed redress by hours.
- **Rule**: JSONL quiet >15 min, first-pass no-commit return, three
  diagnostic-loop iterations, or scope-pivot reveal each auto-trigger a
  triumvirate without user prompt.
- **Check**: orchestrator monitor records the auto-trigger condition and
  the triumvirate dispatch time alongside the wave's progress log.

## 2026-04-30 - HARD CAPs On Every Dispatch

- **Source**: bbnf-lang AZ-II O3a triads, observed time overruns.
- **Failure**: agents without a HARD CAP looped on diagnostic exploration
  past useful return time.
- **Rule**: every dispatch prompt carries `HARD CAP: N min. At 0.9N commit,
  at N halt.`. Defaults research 20, plan 15, redress 30, audit 25.
- **Check**: dispatch template enforces the slot; orchestrator verifies the
  cap is present before sending the prompt.

## 2026-04-30 - No Polling, Use Background + Monitor

- **Source**: bbnf-lang AZ-II/AZ-III orchestrator runs.
- **Failure**: `tail -f`, `ps aux` loops, and sleep-poll loops blocked the
  orchestrator on 60s+ commands and missed harness notifications.
- **Rule**: any command expected to run >60s runs in `run_in_background`
  followed by a Monitor call. Never poll.
- **Check**: prompts containing `sleep`, `tail -f`, or unbounded `ps aux`
  loops are rejected.

## 2026-04-30 - Single Cargo Per CARGO_TARGET_DIR

- **Source**: bbnf-lang AZ-II.cutover.O parallel cargo runs.
- **Failure**: multiple cargo invocations sharing a `CARGO_TARGET_DIR`
  silently serialised on the build lock and inflated wall-clock.
- **Rule**: at most one cargo invocation per `CARGO_TARGET_DIR`. Sibling
  worktrees set per-agent `CARGO_TARGET_DIR=<worktree>/target/<agent>`.
- **Check**: wave's Worktree Plan table enumerates a unique
  `CARGO_TARGET_DIR` per agent unit.

## 2026-04-30 - Read-Size Preflight Before Large Reads

- **Source**: bbnf-lang AZ-II/AZ-III research-wave Read failures on
  `generated.rs` files.
- **Failure**: full Reads on >2K-line files exhausted context budget or
  failed outright.
- **Rule**: `wc -l` before `Read` on any file >2K lines; use `grep`+offset
  for generated.rs, transcripts, and large audits.
- **Check**: research and audit prompts embed the preflight step.

## 2026-04-30 - Templated Commit Bodies Are Bodyless In Spirit

- **Source**: bbnf-lang AZ-II.cutover.O history rewrite (W0 commit-repair
  plan).
- **Failure**: 30+ post-hoc commit-message rewrites added formulaic body
  shells ("Land the implementation slice named in the subject...") that
  satisfy "body present" but cite no per-commit evidence.
- **Rule**: bodies must cite a runtime command output, scan path, file
  count, or commit hash. Templated bodies are rejected. Post-hoc rewrites
  may state "history repair, evidence routed elsewhere" but must not
  pretend per-commit evidence was new.
- **Check**: commit-discipline skill rejects bodies whose only specific
  content is the subject line restated.

## 2026-04-30 - Scope Pivots Open A New Letter

- **Source**: bbnf-lang AZ-II cutover.O3a accumulated five sub-cohort
  triads in-place rather than opening AZ-II.5 / AZ-III earlier.
- **Failure**: in-place scope absorption obscured architectural pivots,
  delayed honest close, and produced large unsynthesized waves.
- **Rule**: scope reveal absorbs only when file-bound expansion is <=2
  paths and the hard gate is unchanged. Larger pivots open `{LETTER}-II.md`
  or `{LETTER+1}.md` per the thesis-stable check.
- **Check**: `tranche/SPEC.md` §Scope Reveal lists the absorption ceiling
  and the new-letter rule.
```

### 8.5 `docs/precepts/instructions/tranche/SPEC.md`

In §Scope Reveal (lines 90-108), modify step 1 and add a new step:

```markdown
1. Default to absorb only when file-bound expansion is <=2 paths and the
   hard gate is unchanged.                                                   # MODIFIED
2. If the thesis still holds but bounds widen beyond the absorption
   ceiling, open `{LETTER}-II.md` or `{LETTER}.5/` with a fresh wave plan.   # MODIFIED
3. If the thesis changes, close the current tranche honestly and open the
   next letter (`{LETTER+1}.md`).
4. Escalate to the user only for environmental blockers, authorization
   boundaries, or irrecoverable state.
5. For non-environmental stalls, scope dilation whose mode/root cause is
   unclear, or auto-trigger conditions in `ORCHESTRATION.md` §Triumvirate
   Auto-Triggers, dispatch the triumvirate.

Never answer scope reveal by adding shadow APIs, compatibility shims, or
unconsumed scaffolding.

The orchestrator records the scope-reveal disposition in
`audit/SCOPE-REVEAL-{date}.md` before resuming implementation.              # NEW
```

## 9. AZ-III W0 Alignment

AZ-III W0 absorbs the lessons better than AZ-II ever did, but it leaves gaps that the precepts refinements above would close.

**What AZ-III W0 already does**:
- Records `git status --short`, staged diff, submodule state in `W0-state-ledger.txt` (aligned with `LESSONS-LEARNED.md` `Dirty Worktree Protocol`).
- Has a `## Triumvirate Dispatch` section in W0.md (lines 73-79) that lists trigger conditions: "unclear state split, history-rewrite risk, precepts mismatch, or dispatch-packet scope reveal".
- W1.md, W2.md, W3.md all carry the same `## Triumvirate Dispatch` section template - the AZ-III waves invented this section because precepts didn't have it.
- W0 commit plan requires bodies on both planned commits ("Both commits require bodies because they gate implementation dispatch").
- W0 archaeology section explicitly cites the AZ-II pattern it is preventing: "AZ-II cutover.O accumulated dirty main state, bodyless large commits, and mid-flight instruction migration."

**What AZ-III W0 still misses**:

1. **No HARD CAP on the W0 dispatch units.** W0.1-W0.4 list mechanism, files, sub-gate but no time cap. Refinement 8.1 fills this.

2. **No Worktree Plan table.** W0 does say "sibling worktree bounds" (line 18) but does not enumerate per-unit absolute paths. W1-W3 packets in `W0-dispatch-packets.md` say "Dispatch up to 10 agents after the root dirty slice is assigned" but never say which sibling worktree per agent. Refinement 8.2 §4b fills this.

3. **No Disjointness preflight.** W0.1 (state ledger), W0.2 (commit archaeology), W0.3 (precepts/instruction migration), W0.4 (dispatch packet authoring) - W0.3 and W0.4 both touch `docs/tranches/AZ-III/audit/`, which is silent overlap. Refinement 8.2 §4a flags this.

4. **No JSONL-quiet auto-trigger.** W0's triumvirate clause says "Any unclear state split, history-rewrite risk, precepts mismatch, or dispatch-packet scope reveal pauses implementation and dispatches three bounded agents". Refinement 8.3 §Auto-Triggers makes this measurable.

5. **No empty-return rule.** W0 dispatch packets do not say what to do if W1.1 returns empty. Refinement 8.4 (LESSONS-LEARNED entry) and 8.1 (template) fill this.

6. **No commit-body templated-rejection rule.** W0 commit-repair-plan.md acknowledges the templated bodies but does not codify the rule that future bodies must be evidence-bearing. Refinement 8.4 entry "Templated Commit Bodies Are Bodyless In Spirit" closes this.

7. **No scope-pivot new-letter rule applied to AZ-III itself.** AZ-III is itself the application of the rule (continuation tranche after AZ-II close), but the rule is not codified in `tranche/SPEC.md`. Refinement 8.5 closes this.

8. **No verification-artefact freshness on W1-W4 artefact lists.** W1.md §Verification Artefacts (lines 96-101) lists four `.txt` paths but no SHA / timestamp. Refinement 8.2 §8 closes this.

In short: AZ-III W0 is built on rules that are partially in precepts and partially invented by the wave. The five refinements above codify what AZ-III had to invent.

## Top-5 Refinement Proposals (Summary)

In order of leverage on observed friction:

1. **Expand `AGENT_DISPATCH_TEMPLATE.md`** to include HARD CAP, sibling-worktree pin, `CARGO_TARGET_DIR`, read-size preflight, lint cadence, no-polling clause, empty-return halt rule, and templated-body rejection. Closes friction patterns 1, 3, 5, 9, 11, 12, 13, 14.

2. **Add `## Triumvirate Auto-Triggers` and required artefact paths to `ORCHESTRATION.md`**. Make triumvirate mandatory (not optional) for JSONL quiet >15min, first-pass no-commit, three-iteration diagnostic loops, and scope-pivot reveals; require research/plan/redress to write `audit/{COHORT}-{role}.md` with the plan agent producing a mandatory `## Exact Wave-Amendment Text` section. Closes friction patterns 6, 7, 8.

3. **Add §3a Triumvirate Dispatch, §4a Disjointness, §4b Worktree Plan to `WAVE_SPEC.md`**. Each future wave spec inherits the structure that AZ-III W0-W5 had to invent. Closes friction patterns 4, 5, 15.

4. **Tighten `tranche/SPEC.md` §Scope Reveal** so step 1 absorbs only when file-bound expansion is <=2 paths and the hard gate is unchanged; otherwise open `{LETTER}-II.md` or `{LETTER+1}.md`. Closes friction pattern 10.

5. **Append nine new entries to `LESSONS-LEARNED.md`** covering sibling worktrees, empty returns, triumvirate auto-triggers, HARD CAPs, no-polling, single-cargo-per-target, read-size preflight, templated bodies, and scope-pivot new-letter. Closes the codification gap so each lesson has a Source/Failure/Rule/Check pattern future orchestrators consult during dispatch.

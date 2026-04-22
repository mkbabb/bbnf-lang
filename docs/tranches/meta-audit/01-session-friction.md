# Meta-Audit 01 — Session Friction Mining

Author: meta-audit agent 1 of 4
Scope: session-log friction mining across the 5 most recent long-session transcripts.
Method: offline JSONL scan + verbatim quote extraction. No speculation without
evidence from the transcripts.

## Sessions inspected

| Transcript ID (prefix) | Date             | Orchestrator-claimed focus (from opening user msg)                    |
| ---------------------- | ---------------- | ---------------------------------------------------------------------- |
| `4bec5721`             | 2026-04-22 PM    | Tranche AY-II — indefatigable execution, no deferrals                  |
| `32a81b26`             | 2026-04-22 AM    | Tranche B0 → AY starting-state continuation                            |
| `709259dc`             | 2026-04-21       | Tranche AY — "begin … no performance/testing baseline"                 |
| `8f33b00a`             | 2026-04-20       | Tranche AX restart — indefatigability + max parallelization            |
| `6ae1fca0`             | 2026-04-19       | Tranche AW-X → AX pivot                                                |

Aggregate tool-use counts (orchestrator side only, excludes sub-agent internals):

| Session    | Bash | Agent | TaskCreate | TaskUpdate | TaskStop | Monitor | ScheduleWakeup |
| ---------- | ---- | ----- | ---------- | ---------- | -------- | ------- | -------------- |
| 4bec5721   | 365  | 27    | 32         | 38         | 4        | 11      | 19             |
| 32a81b26   | 148  | 23    | 14         | 21         | 0        | 0       | 10             |
| 709259dc   | 158  | 30    | 24         | 34         | 3        | 0       | 0              |
| 8f33b00a   | 326  | 51    | 52         | 128        | 0        | 0       | 0              |
| 6ae1fca0   |  81  | 16    | 15         | 15         | 1        | 0       | 0              |
| **Total**  |**1078**|**147**|**137**   |**236**     |**8**     |**11**   |**29**          |

Shape of the five sessions: long, agent-heavy orchestration sessions where the
orchestrator maintained an in-flight swarm and used Bash to poll logs.
Monitor + ScheduleWakeup adoption is strictly an artefact of the most recent
session (`4bec5721`) — the four prior sessions used Bash-only polling.

## Friction patterns (ranked by frequency × severity)

### Pattern 1: Bash-tail polling instead of Monitor / ScheduleWakeup

- Evidence: **362 `tail`-log invocations across the 5 sessions**
  (126/57/40/117/22), against only **40 Monitor/ScheduleWakeup uses total**,
  29 of those concentrated in the single most recent session.
  Four of five sessions contain zero Monitor calls.
- Bash command preamble that repeated verbatim hundreds of times (truncated
  excerpt from `4bec5721`): *"tail this file — it is the full sub-agent JSONL
  transcript and reading it will overflow your context. If the user asks for
  progress, say the agent is still running; you'll get a completion
  notification."* — this same system warning reached the tool-result stream
  **26 times** in a single session.
- Root cause: the orchestrator's mental model is Bash-first. Monitor/Schedule
  are treated as optional ornaments, not as the default way to wait for a
  sub-agent. Long tails of transcript JSONL get read back into context and
  burn tokens that buy no information.
- Proposed remedy — new memory: **`use-monitor-not-tail`** — "Never tail a
  sub-agent JSONL to learn if it's done. Monitor is the one-shot wait; it
  delivers a completion notification without burning context. Bash `tail -f`
  on a task log is prohibited."

### Pattern 2: User "status" / "check progress" polling forced by silent orchestrator

- Evidence: **5 bare `Status` user messages** + **11 `Check … progress`
  messages** across 5 sessions (16 total). Concentrated in `4bec5721` where
  the user typed "Check bootstrap regen progress, continue if needed." at
  least four separate times (events 1877, 1888, 1909, 2297).
- Verbatim (`4bec5721 ev1920`): *"What is taking so long here? Status"*.
  (`32a81b26 ev967`): *"What has the agent been doing for so long? What is the
  issue it's contended with?"*
- Root cause: orchestrator waits on sub-agents without posting a short
  status tick. User has to interrupt the loop just to confirm nothing
  zombied. Silent waiting is user-frustrating and, in three observed cases,
  triggered full tranche pauses.
- Proposed remedy — new memory: **`status-tick-cadence`** — "Between
  sub-agent dispatches, if more than ~5 minutes of wall-clock elapses with
  no user-facing message, emit a one-line status tick: which sub-agents
  are live, which log file, expected ETA. Never make the user ask twice."

### Pattern 3: Zombie / runaway sub-agents the orchestrator does not notice

- Evidence: verbatim (`4bec5721 ev629`): *"The agent is still running, and
  we have 6 active tasks—what is going on? We cannot patch the generated.rs
  like this. Status?"* — followed by (`ev633`): *"That's not scope
  expansion, that's zombie tasks. Kill the processes if they're not needed
  then."*
- Verbatim (`6ae1fca0 ev398`): *"The last several agents have crashed my
  machine with rustc processes that consume 100GB."*
- Verbatim (`6ae1fca0 ev625`): *"These waves are taking FAR too long. This
  process in totality has been running for nearly 12 hours."*
- Orchestrator tool evidence: only **8 TaskStop calls total across 1078
  Bash invocations**; 4/5 sessions never invoked TaskStop at all.
- Root cause: no periodic reconciliation between the orchestrator's live
  task list and `ps`-reality. Sub-agents that exited abnormally are left
  as background ghosts; agents that went out of scope are not stopped.
- Proposed remedy — new memory: **`reconcile-task-census`** — "At each wave
  boundary and before every user-facing status summary, reconcile the
  TaskList against `ps aux | grep cargo` and the task JSONL mtimes. Any
  task whose JSONL has been mtime-quiet for >N minutes gets TaskStop'd or
  explicitly acknowledged as deliberately long. Never tell the user
  'N tasks running' without verifying each one is making progress."

### Pattern 4: Orchestrator hand-patches generated.rs (invariant violation)

- Evidence: verbatim (`4bec5721 ev629`): *"We cannot patch the generated.rs
  like this."* — user intercepts commit `f372e7ef` that hand-edited a
  generated file. This violates the existing memory
  `clean-regen-discipline`.
- Root cause: the sub-agent dispatched for a codegen fix took the shortest
  path (Edit on the generated file) rather than changing the grammar and
  regenerating. The orchestrator did not guard the sub-agent prompt
  against this.
- Proposed remedy — instruction-layer addition (agent 2's scope):
  **sub-agent briefs must carry a "forbidden edits" whitelist** derived
  from clean-regen-discipline; generated files listed by name. Also propose
  a pre-commit hook that hard-fails on any edit whose path is in the
  generated set.

### Pattern 5: Tranche-scope churn — "devise a path forward" ceremony

- Evidence: the literal paragraph *"Devise a path forward: audit the
  hitherto made changes and the remaining plan … NO quick solutions, NO
  workarounds: idiomatic, gestalt approaches. … NO legacy code."* appears
  **verbatim or near-verbatim in 5 separate user events** across 4 of the
  5 sessions (`4bec5721 ev633, ev792`; `709259dc ev859`; `8f33b00a
  ev1708, ev1713, ev1950, ev2026`; `6ae1fca0 ev632`). In `8f33b00a` it
  appears **four times in one session**.
- Root cause: the orchestrator repeatedly drifts into partial-solution /
  legacy-acceptance mode; the user has to re-incant the same edict. The
  edict is not folded into the standing instruction layer, so each
  session starts from scratch.
- Proposed remedy — instruction-layer addition: the gestalt/no-workarounds
  edict belongs in the always-on tranche preamble, not re-pasted by the
  user at every scope-audit point. Also: **a standing memory-entry trigger
  "plan-audit-on-drift"**: any time the orchestrator proposes ≥2 consecutive
  quick-fixes or ≥1 work-around, it must auto-dispatch a 4-agent deep
  audit without waiting for user prompt.

### Pattern 6: Cargo build/check runtime catastrophe (the B0 regression)

- Evidence: verbatim (`4bec5721 ev2391`): *"These processes are taking far
  too long. Totally unacceptable—we should not tolerate such long multi
  minute build, testing, and benching time."*
- Verbatim (`4bec5721 ev2577`): *"Nonsense. This has never taken so long
  previously—before our B0 changes, which were supposed to optimize this
  process, our builds were long but at least occasionally reasonable. …
  Kill all ongoing rustc process and properly assay and address."*
- Verbatim (`6ae1fca0 ev625`): *"Why is the begotten generated.rs still
  nearly 200k lines?"*
- Raw totals: **55 cargo build/check, 67 cargo test, 9 cargo bench** across
  the 5 sessions from the orchestrator alone (plus N more inside
  sub-agents). Zero `run_in_background=true` uses from the orchestrator;
  **67 Bash invocations with `timeout >= 5min`**.
- Root cause: the regenerated `generated.rs` (~200k lines) forces every
  rustc invocation into multi-minute territory, and the orchestrator
  did not adopt `cargo check -p <crate>` narrowing or a generated-code
  size budget.
- Proposed remedy — new memory: **`generated-size-budget`** — "The
  generated code has a hard line-count budget (set per-tranche). Any
  commit that exceeds the budget is a regression and blocks the wave
  until the O(N) in generator is traced. Long cargo-check times are a
  symptom, not a cost to tolerate."

### Pattern 7: Foreground long-running Bash instead of background + Monitor

- Evidence: **0 `run_in_background=true` invocations across all 1078 Bash
  calls**, yet **67 Bash invocations with explicit `timeout >= 5 minutes`**.
  Each of those blocks the orchestrator's turn for up to the timeout.
- Root cause: the orchestrator still composes multi-minute cargo runs as
  synchronous Bash calls. The Bash tool's `run_in_background` + a single
  Monitor is strictly superior (completion notification, context-cheap).
- Proposed remedy — new memory: **`bg-then-monitor`** — "Any Bash
  invocation expected to take >60s must set `run_in_background=true` and
  be followed by a Monitor call. Foreground long-running Bash is
  prohibited."

### Pattern 8: Triumvirate pattern invoked late + inconsistently

- Evidence: verbatim (`32a81b26 ev967`): *"When an agent faces a blocker
  like this, let's update our README and instructions edicts where
  befitting to have a clause that states: when blocked and debugging in
  too long of a task, that agent should relinquish control back to the
  orchestrator and a triumvariate set of research + plan + redressing
  agents should be deployed adhoc."*
- The W0'.d3 success (`4bec5721`, commits `bd563c1d`…`f768f50d`) was a
  triumvirate applied correctly: research → attribution doc → plan doc →
  O(1) `value_end_compound` fix. **It reduced cold-iter from 10+ min to
  11.3s in one pass.** But the triumvirate was invoked only after
  explicit user intervention, not auto-triggered on symptom.
- Root cause: triumvirate is a manual pattern, not a standing reflex on
  "sub-agent has spent >Tmax seconds without new commit".
- Proposed remedy — new memory: **`triumvirate-auto-trigger`** — already
  codified as pattern but not as standing reflex. Promote to instruction-
  layer rule with concrete trigger: any sub-agent whose JSONL mtime has
  been quiet for >15 minutes, or whose first diagnostic pass fails,
  must be halted and replaced with a 3-agent research/plan/redress set.

### Pattern 9: Mid-tranche scope pivot without new tranche letter

- Evidence: `32a81b26 ev1112`: *"AY should be split into AY/I, II—current
  is I, and then the remaining, synthesized, gestalt re-ordered items
  placed into II with audit/, waves/ PROGRESS—update our instruction
  edicts, too, to handle this sort of multi-tranche tranche."*
- This is the exact symptom of the existing `new-tranche-new-doc` memory
  — but the orchestrator did not apply it proactively; the user had to
  ask.
- Root cause: memory exists but is not consulted at scope-drift moments.
- Proposed remedy — already memoried; augment with an instruction-layer
  reminder *before* every wave-close commit: check whether current work
  still matches the tranche letter's opening thesis.

### Pattern 10: Context-overflow guardrails learned, then lost

- Evidence: `4bec5721` event stream contains at least two compaction
  markers ("This session is being continued from a previous conversation
  that ran out of context."). In `8f33b00a` the same tranche ends with
  a compaction and restarts in another session.
- Root cause: the orchestrator reads large artefacts (tail of
  transcripts, full `generated.rs`, full plan docs) into context instead
  of grepping or reading file slices.
- Proposed remedy — new memory: **`grep-before-read`** — "Before Read on
  any file >500 lines, use Grep/offset+limit. The only legitimate
  full-file Read is on files you authored this turn or known small
  docs. Generated artefacts, sub-agent transcripts, and large plan
  corpora are always grepped."

## Success patterns (preserve / amplify)

### Pattern A: Triumvirate → concrete one-shot fix (W0'.d3)

- Evidence: `4bec5721` — research (`5cb76753`), plan (`9a718199`), redress
  (`f768f50d`) produced the O(1) `value_end_compound` replacement for an
  accidentally O(N²) `direct_child_count`. Before: 10+ min cold iter-check.
  After: 11.3s. All within one wave.
- Why it worked: scope was *bounded*, the three roles were *distinct*, and
  the plan committed before the redress dispatched.
- Memory entry to lock it in: this is already reified as the
  `triumvirate-auto-trigger` proposal above. Add a twin: **`triumvirate-
  discipline`** — "Research writes attribution doc + commits. Plan writes
  plan doc + commits. Only then does redress dispatch. Never merge roles."

### Pattern B: Commit-before-parallelize discipline held

- Evidence: of 15 commits in `4bec5721`, 0 hit merge conflicts when
  multiple W0'.a/b/c sub-agents ran. Existing memory `agent-orchestration`
  ("Never let sub-agents race on shared files; commit before parallelizing;
  use worktrees for overlap") appears to be respected in every session
  that used worktrees. Worktree-cleanup commands (`git worktree remove
  --force …`) show up in `8f33b00a` after each wave.
- Why it worked: file-bound partitioning + worktree-per-agent is the
  one invariant the orchestrator does not violate.
- Memory entry to lock it in: existing `agent-orchestration` memory stands;
  propose no change, only upgrade its visibility by cross-referencing it
  from the tranche preamble.

### Pattern C: First-principles pivot resolved a multi-session loop

- Evidence: verbatim (`6ae1fca0 ev539`): *"The inlining blowup is a
  consequence of the DTA—why not just abrogate that entirely?"* — one
  user sentence collapsed a 12-hour stall. Captured later as the
  no-workarounds-arch direction.
- Why it worked: user applied first-principles; the orchestrator was
  local-optima-stuck (trying to fix DTA inlining rather than delete DTA).
- Memory entry to lock it in: **`abrogate-before-patch`** — "When a
  subsystem's failure mode is intrinsic to its architecture (DTA inlining
  blowup, duplicated-codepath churn, derive-macro combinatorics), the
  first question is *can we delete the subsystem* — not *can we patch it*.
  Applies especially to dev-time infra."

### Pattern D: Tight wave tool caps when the user demanded them

- Evidence: `4bec5721` dispatches *"Build-infra research, 20min cap"*,
  *"Infra plan, 15min cap"*, *"Infra redress, 30min cap"*. These caps
  held — all three agents committed within their window.
- Why it worked: hard caps in the dispatch prompt → sub-agent self-
  constrains to the essentials.
- Memory entry to lock it in: **`dispatch-hard-cap`** — "Every sub-agent
  dispatch must carry an explicit wall-clock cap in the prompt
  ('HARD CAP: N min. At 0.9N commit whatever you have'). Caps default
  to: research 20 min, plan 15 min, redress 30 min; tighter per scope."

### Pattern E: User's archaic diction carrying load-bearing specificity

- Evidence: "indefatigability", "gestalt", "abrogate", "triumvirate",
  "begotten" all recur. Each is a term-of-art with precise meaning in
  this codebase's lexicon.
- Why it worked: the existing memory `archaic-diction-is-voice` already
  insulates these from mis-reading. Reinforce only.

## Proposed memory additions

For each, the file path is under
`/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/`.

### 1. `feedback_use_monitor_not_tail.md`

```markdown
# use-monitor-not-tail

Never tail a sub-agent JSONL to check if it finished. Monitor is the
one-shot wait — it delivers a completion notification without reading
the transcript into context. Bash `tail -f` / `cat` on a task JSONL is
prohibited; the system will even warn you inline. Tail only when the
agent has completed and you need a specific field from its output — then
grep + offset, never full-file read.
```

### 2. `feedback_status_tick_cadence.md`

```markdown
# status-tick-cadence

Between sub-agent dispatches, if more than ~5 minutes of wall-clock
elapses with no user-facing message, emit a one-line status tick:
which sub-agents are live, which log file, expected ETA, what wave
boundary you are approaching. Never make the user ask "status?" twice
in the same session. Silent waits are a bug.
```

### 3. `feedback_reconcile_task_census.md`

```markdown
# reconcile-task-census

At each wave boundary and before every user-facing status summary,
reconcile the live TaskList against `ps aux | grep cargo` and task-JSONL
mtimes. Any task whose JSONL mtime has been quiet for >N minutes gets
TaskStop'd or explicitly acknowledged as deliberately long. Never tell
the user "N tasks running" without verifying each is making progress.
Zombie tasks are the single biggest source of user friction.
```

### 4. `feedback_generated_size_budget.md`

```markdown
# generated-size-budget

Generated code has a hard line-count budget set per tranche. Any commit
that exceeds the budget is a regression and blocks the wave until the
O(N) in the generator is traced. Long cargo-check / cargo-test wall
times are a symptom, not a cost to tolerate. If a generator produces
>50k lines for a workload the hand-written equivalent closes in <5k,
the generator is wrong — not the machine, not rustc.
```

### 5. `feedback_bg_then_monitor.md`

```markdown
# bg-then-monitor

Any Bash invocation expected to take >60s must set
`run_in_background=true` and be followed immediately by a Monitor on
the returned task id. Foreground long-running Bash blocks the turn
and steals context. Specifically: cargo build, cargo test, cargo
bench, cargo check on large generated crates — always background.
```

### 6. `feedback_triumvirate_discipline.md`

```markdown
# triumvirate-discipline

Research writes the attribution doc and commits.
Plan writes the plan doc and commits.
Only then does redress dispatch.
Never merge roles. A single agent doing "research + redress in one pass"
is the commonest failure mode — it ships a fix for a mis-attributed
root cause.
```

### 7. `feedback_triumvirate_auto_trigger.md`

```markdown
# triumvirate-auto-trigger

Any sub-agent whose JSONL mtime has been quiet for >15 minutes, or
whose first diagnostic pass fails to produce a committed artefact,
must be halted (TaskStop) and replaced with a 3-agent
research/plan/redress set. Do not wait for user prompt.
```

### 8. `feedback_dispatch_hard_cap.md`

```markdown
# dispatch-hard-cap

Every sub-agent dispatch carries an explicit wall-clock cap in the
prompt: "HARD CAP: N min. At 0.9N commit whatever you have; at N halt."
Defaults: research 20 min, plan 15 min, redress 30 min; tighter per
scope. Caps without the 0.9N checkpoint drift; caps without the halt
clause orphan-run.
```

### 9. `feedback_abrogate_before_patch.md`

```markdown
# abrogate-before-patch

When a subsystem's failure mode is intrinsic to its architecture —
DTA inlining blowup, duplicated-codepath churn, derive-macro
combinatoric expansion, orthogonal substrates — the first question is
*can we delete the subsystem?*, not *can we patch it?*. Applies
especially to dev-time infra where every saved second compounds across
thousands of agent turns.
```

### 10. `feedback_grep_before_read.md`

```markdown
# grep-before-read

Before Read on any file >500 lines, use Grep or Read with offset+limit.
The only legitimate full-file Read is on files you authored this turn
or known small docs (<200 lines). Generated artefacts, sub-agent
JSONL transcripts, and large plan corpora are always grepped or
offset-read. Full-file reads are the top cause of context compaction.
```

## Proposed instruction-layer additions (names only, agent 2 authors diffs)

- `no-hand-edit-generated` — codify the forbidden-edits whitelist as a
  sub-agent dispatch invariant (backed by pre-commit hook).
- `plan-audit-on-drift` — the gestalt/no-workarounds edict lifted from the
  5× user-repeated paragraph into the always-on preamble.
- `monitor-is-default` — instruction-layer statement that the completion
  notification system supersedes all tail-based polling.
- `status-tick-every-5min` — orchestrator obligation to emit user-visible
  tick between dispatches.
- `task-census-before-status` — obligation to reconcile task state before
  replying to any "status" query.
- `wave-close-checks-tranche-thesis` — pre-commit check against the
  tranche opening thesis (new-tranche-new-doc pre-flight).

## Quantitative summary

- Total distinct user corrections (frustration phrases): **20**
  (see Pattern-1 quote set; does not include ordinary "continue" / "yes").
- Sub-agent dispatch count (Agent tool): **147** across 5 sessions.
- Sub-agent dispatches explicitly stopped (TaskStop): **8**
  (implies a long tail of implicit abandonments).
- Tool-failure repetitions (same warning 26× in one session): **1 pattern,
  dozens of occurrences** — the "still running, don't tail" system warning
  appeared 26× in `4bec5721` alone.
- Redundant Bash polls (`tail`/`cat` of task logs): **362** across 5
  sessions. Monitor adoption: **11 calls** (one session).
- Foreground cargo builds/checks (never backgrounded): **124**
  (55 build/check + 67 test + 2 bench, zero with `run_in_background=true`).
- Work thrown away — commits reverted: **3** (`8f33b00a` `git revert
  --no-edit 7c3ea838 1ab22a9d 015d02af`). Hard resets (worktree rebase):
  multiple.
- Hand-patched generated file: **1 known instance** (`f372e7ef`, caught
  by user at `ev629` of `4bec5721`).
- Context-heavy operations / compaction events observed: **≥2** across the
  5 sessions (explicit "continued from a previous conversation" markers).
- User "status"/"progress-check" messages forced by orchestrator silence:
  **16** across 5 sessions.
- Repetitions of the verbatim "gestalt / no workarounds / no legacy" user
  paragraph: **5 across 4 sessions** (4 of them in `8f33b00a` alone).

## Top 3 recommendations

1. **Replace all tail-polling with Monitor.**
   Codify `use-monitor-not-tail` + `bg-then-monitor` as instruction-layer
   rules. Target: drop tail-log reads from 362/5-sessions to <10, and
   back every >60s Bash with `run_in_background=true`. This one change
   addresses Patterns 1, 2, 7 and eliminates the single largest source
   of context waste.

2. **Make triumvirate automatic on stall, not manual on user prompt.**
   Codify `triumvirate-auto-trigger` + `triumvirate-discipline` +
   `dispatch-hard-cap`. Any sub-agent quiet for >15 min is auto-halted
   and replaced with research/plan/redress on hard caps (20/15/30 min).
   This is the single highest-leverage process change — the one W0'.d3
   success already proved the pattern's power; we just need it to fire
   without user rescue.

3. **Enforce a generated-code size budget at the commit level.**
   Codify `generated-size-budget`. If `generated.rs` exceeds the
   per-tranche line-count ceiling, the commit is blocked until the
   generator regression is traced. This closes the loop on the "builds
   take 12 hours" / "rustc consumes 100GB" / "200k-line generated.rs"
   class of friction — which recurred in every one of the 5 sessions.

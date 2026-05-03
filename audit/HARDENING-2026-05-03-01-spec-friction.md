# HARDENING 2026-05-03 Lane 01 - Spec-Friction

Scope: audit-only lane for `docs/HARDENING-AUDIT-PROMPT.md` §Spec-Friction.
No source or tranche specs edited.

Method: offline JSONL scan of the latest two long sessions under
`/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/`.
Selection rule: latest transcripts by mtime whose line count and size indicate
long orchestration sessions. The newer `097ba56b` transcript is 79 lines and
therefore excluded.

Canonical anchors consulted:

- `docs/HARDENING-AUDIT-PROMPT.md:77` requires Bash-poll vs Monitor,
  parallel-agent overlap, worktree contention, redispatch-after-empty, and
  status-tick quantification.
- `docs/tranches/meta-audit/01-session-friction.md:36` names Bash-tail polling
  as the historic top friction pattern; lines 159-169 name foreground
  long-running Bash.
- `docs/precepts/instructions/ORCHESTRATION.md:120-157` mandates triumvirate
  auto-trigger and verbatim redispatch after empty return.
- `docs/precepts/instructions/ORCHESTRATION.md:139-149` requires unique
  `CARGO_TARGET_DIR` per sibling worktree and background+Monitor for commands
  expected to exceed 60 seconds.
- `docs/precepts/instructions/ORCHESTRATION.md:179-186` defines the ~5 minute
  status-tick cadence and required contents.

## Sessions inspected

| Session | Transcript | Timespan | Lines | Focus observed |
|---|---:|---:|---:|---|
| S1 | `17cd5cc4` | 2026-05-01 20:28Z -> 2026-05-03 04:39Z | 5,278 | AZ-IV execution, BA/BB/BC spec surgery, hardening handoff |
| S2 | `c6be030c` | 2026-05-01 00:31Z -> 2026-05-01 20:38Z | 4,260 | AZ-III close, AZ-IV planning, hardening pass setup |

Extraction commands:

```sh
ls -lt /Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/*.jsonl | head -20
wc -l /Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/{17cd5cc4-8355-4f49-a563-0397db891a55,c6be030c-09d9-4956-8881-61b9437ae4d1}.jsonl
node <offline-jsonl-tool-use-scan>
```

## Tool-use frequency

| Session | Bash | Read | Edit | Write | Agent | TaskCreate | TaskUpdate | TaskStop | Monitor | ScheduleWakeup | ToolSearch | PushNotification |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `17cd5cc4` | 462 | 86 | 78 | 15 | 81 | 24 | 42 | 1 | 6 | 4 | 3 | 0 |
| `c6be030c` | 387 | 88 | 65 | 15 | 38 | 14 | 28 | 1 | 9 | 22 | 3 | 1 |

Derived orchestration counters:

| Session | Bash commands containing `tail` | Bash tails of task output | `Monitor` calls | `ScheduleWakeup` calls | `run_in_background=true` Bash | Cargo Bash | Cargo Bash not backgrounded | Agent launches | Empty-return incidents | External user status nags |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `17cd5cc4` | 249 | 12 | 6 | 4 | 16 | 79 | 63 | 81 | 0 observed | 0 |
| `c6be030c` | 140 | 2 | 9 | 22 | 3 | 49 | 46 | 38 | 1 observed | 0 |

Parallel-agent overlap:

- `17cd5cc4`: 24 task records but 81 agent launches. W0/W1/W2/W3/W4/W5 each
  opened as parallel waves; transcript line 198 says "5 sub-agents running ...
  plus `bkfkfmza3`", and line 1076 says "Three W1 redispatches in flight".
- `c6be030c`: 14 task records but 38 agent launches. Transcript line 1083 says
  "W2 agents (4) dispatched in parallel with disjoint test surfaces"; line 1609
  schedules a heartbeat for "5 agents in flight".

Status-tick cadence:

- No external bare `Status` / "what is taking so long" messages were found in
  the selected two logs, unlike the older exemplar corpus.
- Cadence is PARTIAL, not fully honoured: several status messages identify
  live agents and worktrees, but most do not name JSONL transcript mtimes or a
  next decision point. The best examples are `c6be030c:399`, `c6be030c:799`,
  and `17cd5cc4:198`. The weaker form repeats "Waiting" without the required
  transcript ledger.

Redispatch-after-empty:

- One explicit empty-return incident appears. `c6be030c:751` says "W1.2
  returned empty"; the orchestrator then backfilled evidence directly. Verbatim
  redispatch rate for observed empty returns: 0/1.

## Ranked friction patterns

### F1. Bash-tail polling remains the dominant reflex even after Monitor exists

Severity: high. Frequency: high.

Evidence:

- `17cd5cc4`: 249 Bash commands containing `tail` vs 6 Monitor calls.
- `c6be030c`: 140 Bash commands containing `tail` vs 9 Monitor calls.
- The historic exemplar already identified this as the top pattern:
  `docs/tranches/meta-audit/01-session-friction.md:36-55`.
- Verbatim transcript excerpt, `17cd5cc4:120`: "`git worktree add
  /Users/mkbabb/Programming/bbnf-wt-aziv-w0-truth master 2>&1 | tail -10`".
- Verbatim transcript excerpt, `c6be030c:503`: "`cat
  /private/tmp/claude-504/.../tasks/bifkms663.output 2>&1 | tail -50`".

Assessment:

Monitor adoption improved from the older exemplar corpus, but it is still
secondary. Worse, several Monitor calls embed shell polling loops internally,
for example `17cd5cc4:3512` watches a task output file via `while true` and
`grep`, and `c6be030c:510` waits on `/tmp/w0p-cold.log` with `sleep 5`.
That is better than foreground Bash polling because the harness notifies, but
it preserves the poll-script mental model.

Mechanism-level fix:

Add a dispatch preflight that refuses any orchestrator wait command matching
`tail .*tasks/.*output`, `while .*sleep`, or `until .*sleep` unless it is inside
a `Monitor` call with a documented terminal event. The preflight should print:
"Use Monitor or ScheduleWakeup; do not read task logs to learn whether the
agent is done." For Monitor bodies, require a `description` naming the exact
event that ends the wait.

### F2. Foreground cargo remains common; background+Monitor is applied selectively

Severity: high. Frequency: high.

Evidence:

- `docs/precepts/instructions/ORCHESTRATION.md:145-149` says commands expected
  to run more than 60 seconds use `run_in_background` + Monitor.
- `17cd5cc4`: 79 Bash cargo invocations; 63 were not backgrounded.
- `c6be030c`: 49 Bash cargo invocations; 46 were not backgrounded.
- Verbatim transcript excerpt, `c6be030c:752`: "`cargo build -p bbnf
  --no-default-features --profile ax-iter 2>&1 | tail -3`"; the result at
  `c6be030c:753` records "Finished `ax-iter` ... in 44.33s" after an earlier
  assistant claim at `c6be030c:751` of "1m 06s".
- Positive counterexample, `17cd5cc4:1044`: "`cargo xtask regen --check`" was
  run with `"run_in_background":true` and then notification landed at
  `17cd5cc4:1050`.

Assessment:

The operator has learned background+Monitor for some explicit long checks, but
the default for cargo is still foreground plus output truncation. The next
BA/BB/BC dispatch inherits heavier codegen and benchmark surfaces; this pattern
will turn friction into lost wall-clock and stale status unless cargo commands
are classified before execution.

Mechanism-level fix:

Introduce a shell wrapper or Make alias for audit/agent cargo commands:
`cargo-observe <label> -- <command>`. It should default to background execution
and emit the matching Monitor command. Only commands declared `short-ok` may
run foreground, and the declaration must include the last observed wall-clock.

### F3. Empty-return handling regressed against the current edict

Severity: high. Frequency: low in selected corpus, high blast radius.

Evidence:

- `docs/precepts/instructions/ORCHESTRATION.md:152-157` requires verbatim
  redispatch after an empty sub-agent return.
- Verbatim transcript excerpt, `c6be030c:751`: "W1.2 returned empty. Since I
  already verified `cargo build -p bbnf --no-default-features --profile
  ax-iter` is GREEN ... let me capture the canonical evidence directly as
  orchestrator integration."
- Observed redispatch-after-empty rate: 0/1. No parser-level empty `Agent`
  result was counted, so this incident is detected from the orchestrator's own
  text rather than blank JSON content.

Assessment:

This is exactly the failure the edict tries to prevent: an empty agent return
was treated as permission for orchestrator backfill. That can be technically
correct for a narrow command, but it destroys the evidence trail and weakens
the agent contract. BA.W0 hardening cannot afford evidence backfills where
direct-projection gates require named production consumers.

Mechanism-level fix:

Add an "empty return gate" to the integration checklist. If an assistant text
or task result matches `returned empty|no evidence|no commit`, the next allowed
actions are only: verbatim redispatch with prior worktree pointer, or record a
triage note explaining why the return was not actually empty. Direct evidence
backfill is blocked until after the required redispatch returns.

### F4. Worktree and shared-state setup still causes avoidable contention

Severity: medium-high. Frequency: medium.

Evidence:

- `docs/precepts/instructions/ORCHESTRATION.md:139-143` requires isolated
  target dirs to avoid lock contention.
- `17cd5cc4:120-129` attempted five worktrees at `master`; each failed with
  "fatal: 'master' is already used by worktree at
  '/Users/mkbabb/Programming/bbnf-lang'". Lines 133-142 then repeated the
  setup correctly with `git worktree add -b ... 2678ed44`.
- Verbatim transcript excerpt, `c6be030c:639`: "The W0p.3 agent installed its
  new hook into the shared `.git/hooks/` before its xtask code landed in main
  — chicken-and-egg..."
- Agent prompts increasingly specify `CARGO_TARGET_DIR`, which is good, but
  shared `.git/hooks/` remains outside worktree isolation.

Assessment:

The orchestrator has mostly internalized per-worktree `CARGO_TARGET_DIR`, but
setup remains hand-assembled. The failed `master` worktree sequence is a small
cost; the shared hook mutation is a real coupling hazard because it lets an
agent change main-worktree behaviour before its source-of-truth commit lands.

Mechanism-level fix:

Use one worktree factory command for all lanes:
`mk-agent-worktree --branch <branch> --base <sha> --path <path> --target-label
<label>`. It must always create a branch from the base SHA, export a unique
`CARGO_TARGET_DIR`, and forbid writes to `.git/hooks/`. Separately set
`core.hooksPath scripts/hooks` once, so hook source is versioned and agents do
not mutate shared hook copies.

### F5. Status ticks improved, but the required ledger fields are not habitual

Severity: medium. Frequency: medium.

Evidence:

- `docs/precepts/instructions/ORCHESTRATION.md:179-186` requires live agents,
  worktrees, JSONL transcript mtimes, and the next decision point.
- Good transcript excerpt, `c6be030c:399`: "Status: W0.5 ... and W0.6 ...
  agents are running in parallel. Main worktree is clean..."
- Good but incomplete excerpt, `17cd5cc4:198`: "5 sub-agents running in
  `bbnf-wt-aziv-w0-{truth,topology,regen,map,metadata}` worktrees... Waiting
  on first completion."
- Incomplete recurring excerpt, `c6be030c:1275`: "W2 agents still working.
  Waiting."

Assessment:

The selected corpus has zero user status nags, which is a material improvement
over the older meta-audit sample. The weakness is fidelity: the "which JSONL
transcripts were last touched" clause is almost never present, and "Waiting"
messages omit a next decision point. The cadence is therefore socially better
but still not machine-auditable.

Mechanism-level fix:

Adopt a fixed one-line tick template for any wait over ~5 minutes:
`Status: live=<agents>; worktrees=<paths>; logs=<jsonl/output mtimes>;
next=<decision/event>; wake=<Monitor/ScheduleWakeup id>`. The orchestrator
should generate it from `TaskList`/Monitor metadata rather than prose memory.

## Cross-lane inheritance risk for BA/BB/BC

The next dispatch inherits an improved but not yet disciplined orchestration
surface. Monitor and ScheduleWakeup are available and used, but Bash remains
the first resort; cargo command classification is manual; empty-return handling
can still be bypassed by orchestrator backfill; worktree setup is repetitive;
status ticks are readable but not ledger-complete.

Pre-W0 mechanism package:

1. Install the worktree factory command or pasteable shell function before
   launching lanes.
2. Require `cargo-observe` for all cargo commands in agent prompts and
   orchestrator integration.
3. Add the empty-return gate to the integration checklist.
4. Use the fixed status-tick template whenever sub-agents are live.
5. Forbid direct `.git/hooks/` mutation from agent worktrees; use versioned
   `scripts/hooks` only.


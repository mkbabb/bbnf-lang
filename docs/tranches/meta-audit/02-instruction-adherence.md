# Meta-Audit 02 — Instruction-Layer Adherence

## Scope

Audit of `docs/instructions/{README.md,PROFILING.md,tranche/*.md}` against
observed orchestrator behaviour in the latest recorded session
(`4bec5721-12ea-4148-8a93-d6052152a90f.jsonl`, 5.9 MB / 2835 events), to
identify edicts violated, ambiguities, contradictions, and stale
prescriptions. Memory proposals scoped to newly-surfaced disciplines
only; cross-referenced to agent 1's session-mining and agents 3/4's
tranche/B1 work where relevant.

## Edicts violated in recent practice

### V1. Heavy-surface routine default (SPEC §Three-tier command surface; §Edicts "NO heavy-surface routine defaults")

**Edict (verbatim, `tranche/SPEC.md` lines 533-541):**

> **NO heavy-surface routine defaults.** Routine iteration runs on
> `iter-check` / `iter-test-{leaf,grammar,ws}` / `ay-expand-*` /
> `ay-test-*`; profiling prep on `profiling-prep` +
> `ay-prepare-profile-wave` + `ay-samply-*`; heavy close-gate proof on
> `ay-bench-close WAVE=close` / `test-close` / `final-bench`. `cargo
> check --workspace`, `cargo test --workspace`, and `cargo bench` are
> the heavy close-proof surface, not the routine one.

**Violation.** The session shows the orchestrator routinely invoking
`cargo check -p bbnf --tests` (close-proof surface — links the full
derive-Parser aggregate at ~26 GB RSS per the README memory note) for
iteration. `grep` over the session:

| Invocation                                       | Occurrences    |
|--------------------------------------------------|----------------|
| `cargo check -p bbnf --tests` (no profile flag)  | frequent       |
| `cargo check --profile ax-iter -p bbnf --tests`  | present        |
| `make iter-check` / `cargo iter-check`           | 60 (mostly docs refs, not invocations) |

The `--profile ax-iter` flag is applied inconsistently; the bare-dev
variant survives. The edict names heavy surfaces as forbidden-for-
routine but **does not enumerate `cargo check -p bbnf --tests` by
name**, and does not forbid the bare-profile form.

**Proposed disambiguation:** add `cargo check -p bbnf --tests` (and
any `cargo check` over a crate that links ≥ 4 derive-Parser sites) to
the heavy-surface list explicitly, and require `--profile ax-iter` on
every iteration-loop `cargo check` / `cargo test` invocation. See
§"Proposed instruction edits" Edit 1.

### V2. Indefatigability vs. diagnostic-loop relinquish (README §Indefatigability; SPEC §Diagnostic-loop relinquish)

**Edict (verbatim, `README.md` lines 91-94):**

> **Relinquish when stuck in diagnostic loops.** A sub-agent that is
> deep in multi-cycle diagnostic probing — three-plus iterations
> without a commit, or wall-clock over ~30 minutes without forward
> motion — stops, reports the current state with every diagnostic
> artefact …

**Edict (verbatim, `tranche/SPEC.md` lines 371-377):**

> A sub-agent that finds itself in a multi-cycle diagnostic loop
> (three-plus iterations without a commit, or ~30+ minutes wall time
> without forward motion) **halts, reports, and relinquishes to the
> orchestrator**. Indefatigability binds the orchestrator, not the
> individual sub-agent.

**Violation.** The relinquish rule binds sub-agents. It does not bind
the research/plan/redress triumvirate members, nor does it bind the
orchestrator's own diagnostic loops. The session shows the
orchestrator itself running extended (> 30 min) local diagnostic loops
— repeated `ps aux | grep rustc` polling (37+), repeated
`tail`/`wc`/re-run cycles on the same bench output (11+), and manual
re-dispatches of agents that returned empty — without escalating.

**Proposed disambiguation:** add explicit 20/15/30-min wall-time caps
for research / plan / redress triumvirate agents respectively, and a
single orchestrator escalation threshold (~60 min without a cherry-
pick landing) after which the orchestrator itself reports to the user.
See Edit 2.

### V3. Sub-agent polling via `ps aux` instead of Monitor / background notification

**Edict.** None. This is a missing edict — but it is violated in
practice: the orchestrator ran `ps aux | grep rustc` patterns 37 times
in one session, plus 11+ `tail`/polling cycles, against 13
`run_in_background:true` dispatches whose completion events are
already delivered to the harness. See §"Ambiguous or missing edicts"
A1.

### V4. Concurrent background cargo invocations contending on `target/` lock

**Edict.** None — the SPEC's "no trammelling" rule (README lines
289-296) forbids *sub-agent* concurrent writes and orchestrator-on-
sub-agent writes, but does not forbid the orchestrator spawning
multiple concurrent `cargo` commands on the same `target/`. The
session shows 13 `run_in_background:true` bash dispatches, several of
which are parallel `cargo check`/`cargo test` on the main target and
deadlock on the workspace `.cargo-lock`. Agent 1 may have separate
evidence; we reconcile at integration.

See §"Ambiguous or missing edicts" A2.

### V5. Large-file Read without prior `wc -l`

**Edict (verbatim, `README.md` lines 354-356):**

> **Never read large output files line-by-line.** `cargo expand`
> output routinely exceeds 100K lines. Use targeted `grep -n`, `awk`,
> `sed` to extract the slice you need. Know the file size before
> reading.

**Violation.** The session includes a direct `Read` on
`crates/core/src/grammar/generated.rs` (generated file, routinely
> 30 K lines) with no preceding `wc -l` size check. The edict scopes
to "cargo expand output" and "large output files"; `generated.rs` is
neither — it is source. The rule is functionally the same (large
file, should grep), but the current text leaves `generated.rs` outside
the named scope.

See Edit 3.

### V6. Artefact-commit discipline under cherry-pick conflict

**Edict.** The SPEC at §Commit discipline (`tranche/SPEC.md` lines
159-166) names a consolidation-commit template for shared-file waves,
but does not specify cherry-pick conflict-resolution protocol when a
wave's cherry-pick sequence hits a 3-way merge conflict *not* on
`mod.rs`-style disjoint hunks. The session shows 12 cherry-pick /
conflict events; no documented protocol triggered.

See Edit 4.

## Ambiguous or missing edicts

### A1. Sub-agent polling anti-pattern (missing)

**Gap.** No edict forbids `ps aux | grep rustc`, `tail -n` on a
background process, or repeated `sleep`-then-check loops for monitor-
ing sub-agent progress. The harness delivers background-completion
events as tool-call results; the Monitor tool streams stdout-line
notifications; `run_in_background:true` plus waiting for the auto-
notification is the intended path. Observed: 37 `ps aux` polls + 11+
`tail` polls in one session.

**Proposed addition (SPEC §Orchestrator role, appended paragraph):**

> **Sub-agent progress monitoring.** The orchestrator never polls
> sub-agent or background-command progress via `ps aux | grep rustc`,
> `lsof`, or repeated `tail -f` cycles. Background commands dispatched
> with `run_in_background:true` deliver completion notifications
> automatically; the Monitor tool streams stdout-line events for
> long-running tasks that need intermediate signal. Polling loops burn
> context and produce no actionable state the harness doesn't already
> surface. If a sub-agent's worktree state must be inspected mid-
> dispatch, use a one-shot `scripts/worktree-status.sh` invocation on
> the named worktree, not a process-level poll.

### A2. Concurrent cargo invocations on shared target (missing)

**Gap.** The orchestrator's own concurrent `cargo` invocations on the
main `target/` are not governed by the "no trammelling" rule, which
scopes to *agent-on-agent* and *orchestrator-on-agent* file writes.
Concurrent `cargo build` / `cargo test` on the same target deadlock
on `target/.cargo-lock`.

**Proposed addition (README §Parallel agent orchestration §Worktree
isolation, after the build-cache-symlink paragraph at line 207):**

> **Single-cargo-per-target invariant.** At most one `cargo` invoca-
> tion is in flight per `CARGO_TARGET_DIR` at any instant — orchestra-
> tor and sub-agents combined. Workspace-lock contention on
> `target/.cargo-lock` silently serialises concurrent invocations with
> indeterminate ordering; benches, samply prepare, and build-cache-
> sensitive workflows observe non-deterministic artefacts under the
> contention. Parallel agents that each need a cargo invocation
> either operate in distinct `CARGO_TARGET_DIR`s (per-worktree, no
> symlink) or are serialised by the orchestrator. The symlinked-
> target pattern (README §Worktree isolation) is for one-active-
> cargo-at-a-time workflows.

### A3. Triumvirate per-agent wall-time caps (ambiguous)

**Current text (`tranche/SPEC.md` lines 371-395):**

> A sub-agent that finds itself in a multi-cycle diagnostic loop
> (three-plus iterations without a commit, or ~30+ minutes wall time
> without forward motion) **halts, reports, and relinquishes** …

**Gap.** The 30-min cap is one-size-fits-all. Research agents read-
only and rarely move fast; plan agents author a fix plan (smaller
scope); redress agents execute concrete file edits (larger scope).
The present session included redress agents flagged "running long" at
the 25-30 min mark where the caller had no per-role budget to compare
against.

**Proposed addition (SPEC §Diagnostic-loop relinquish, new paragraph
after the 3-agent enumeration at line 392):**

> **Per-role wall-time caps.** Research agents: 20 min hard cap —
> read-only work; longer windows signal over-scoped reads, not deeper
> analysis. Plan agents: 15 min hard cap — plan-authoring is dense
> and focused; longer signals the redress should split into sub-waves
> instead. Redress agents: 30 min hard cap — concrete edits and
> verification. At cap, the agent writes its deliverable with
> whatever it has, commits probe artefacts uncommitted-fix separately
> per the partial-commit rule, and returns. The orchestrator's own
> escalation threshold is ~60 min without a cherry-pick landing onto
> master; at that threshold the orchestrator reports to the user
> rather than dispatching another round.

### A4. Redispatch-after-empty-return formalisation (missing)

**Gap.** Sub-agents occasionally return empty (usage-limit truncation,
tool-failure mid-task, silent halt). The SPEC at §Orchestrator role
says the orchestrator "verifies sub-agent claims against saved
artefacts" (line 487) and at §Scope-reveal says "re-plan with more
agents" (line 301), but there is no named protocol for
*same-brief redispatch* when the return is empty (no claim to
verify, no scope reveal, just absence). Observed: 16 redispatch
events in the session, each re-deriving the brief manually.

**Proposed addition (SPEC §Orchestrator role, bullet after line 499):**

> - **Redispatch protocol for empty / truncated returns.** When a
>   sub-agent returns with no commits, no artefact citations, and no
>   scope-reveal diagnosis (i.e., the dispatch produced nothing to
>   verify or absorb), the orchestrator re-dispatches the same brief
>   to a fresh worktree with two additions: (a) a pointer to the
>   prior worktree's git log so the new agent inherits any partial
>   probes; (b) a "prior dispatch returned empty at $TIMESTAMP"
>   preamble so the new agent knows it is not racing a live sibling.
>   Empty returns are not scope-reveal — do not re-plan; redispatch
>   verbatim. Three consecutive empty returns against the same brief
>   escalate to user per §Indefatigability.

### A5. Read-size preflight (ambiguous)

**Current text (`README.md` lines 354-362):** scopes to `cargo expand`
output and "large output files" — source files implicitly excluded.

**Gap.** `generated.rs`, monolithic `AUDIT-*.md` docs, session
transcripts, and long PROGRESS files are routinely > 5 K lines yet
fall outside the named scope. See Edit 3.

## Contradictions between instructions

### C1. Workspace-green cadence vs. mid-wave unworkability

**Passage 1 (`tranche/SPEC.md` lines 128-133):**

> **Workspace green at every wave boundary**, unless the plan
> declares intentional unworkability for a specific window (delete-
> then-swap). The unworkability window carries a named restoration
> wave.

**Passage 2 (`tranche/SPEC.md` lines 451-459, "Transitional fallback
during elimination waves"):**

> An in-transit fallback path whose elimination is the tranche's
> principal work is work-in-progress, not a workaround. The
> one-codegen-path invariant binds at tranche close, not at every
> wave close. AX.W0a kept walker fallback green across seven sub-
> waves precisely to eliminate it in the eighth.

**Contradiction.** Passage 1 says workspace-green at *every* wave
boundary; Passage 2 says the one-codegen-path invariant binds at
*tranche* close, not wave close. The cases converge in practice — an
elimination-wave sequence can keep a named transitional path green —
but the text reads as a hard conflict when read in isolation.

**Proposed reconciliation (Edit 5):** fold the transitional-fallback
paragraph into the delete-then-swap exception explicitly, naming it
as the same pattern.

### C2. "Commit frequently" vs. "Cherry-pick then dispatch"

**Passage 1 (`README.md` line 157):**

> **Commit frequently with `/commit`.** Each natural milestone (phase
> sub-item, artefact landing, invariant restoration) gets its own
> commit.

**Passage 2 (`tranche/SPEC.md` lines 156-157):**

> **Master clean before every wave dispatch.** Cherry-pick-then-
> dispatch. No in-flight concurrency on shared files.

**Tension.** The two rules compose cleanly only if sub-agents commit
in *worktrees* (true per `README.md` §Parallel agent orchestration
lines 223-225) and the orchestrator cherry-picks at wave close. The
text does not explicitly rule out orchestrator-itself committing
frequently to master while a wave is in flight — which would violate
master-clean-before-dispatch if a wave hasn't yet closed.

**Proposed reconciliation (Edit 6):** the orchestrator's own
commit-frequently cadence applies *between* waves, not *during*.
Orchestrator commits to master at wave close (cherry-pick + any
consolidation), not in parallel with an active wave.

## Stale or out-of-date instructions

### S1. Bench-matrix enumeration (README §Tranche completion lines 314-318)

**Current text:**

> The VM, WASM, TS, and competitors benches are **not** included.

**Why stale.** The benchmarks docs (`docs/benchmarks/post-B0-W0-mid.
json`, recent `post-AY-*` artefacts) include additional bench entries
not listed in the enumeration — `json_value` appears in the PROFILING
template at line 256, but README's completion enumeration does not
name it. The B0 handoff contract (per PROFILING.md lines 152-162)
also references `make ay-bench-close` outputs covering five benches
(json, css, sheets, bbnf, compile), not the four the README lists.

**Proposed replacement (Edit 7):** reconcile the bench enumeration in
README's completion section against the actual
`docs/instructions/PROFILING.md` close-matrix table.

### S2. Clean-regen template (README lines 475-505)

**Why stale.** The referenced commits (`87f65214`, `49656fd4`,
`AW-I.W4ζ` pass structure) are ~10 tranches old at this point; AY-II
has its own regen escape pattern via the bootstrap script's refreshed
circular-dependency handling. The walkthrough still works, but the
named commit anchors are stale.

**Proposed replacement (Edit 8):** add a post-AY regen anchor
reference; keep the AW-I recipe as the authoritative template.

### S3. AX.W0a reference in README (line 207)

**Current text:**

> rebuilding the entire workspace per sub-agent burns cycles and RSS.
> W2.1's agent reported 24 "failures" that were all missing
> `data/{bbnf,css,json}`. Do not repeat.

**Why stale.** AX.W0a and W2.1 are old-tranche references. The
lesson generalises (seed every worktree) but naming W2.1 in the README
ties a general rule to a specific tranche's PROGRESS entry that few
current readers will pull up. Low priority; replace with a generic
phrasing or update to the latest tranche that hit the same issue.

## Proposed instruction edits (concrete diffs)

### Edit 1: `docs/instructions/tranche/SPEC.md` §Edicts — NO heavy-surface routine defaults

**Before (lines 533-541):**

```
- **NO heavy-surface routine defaults.** Routine iteration runs
  on `iter-check` / `iter-test-{leaf,grammar,ws}` / `ay-expand-*`
  / `ay-test-*`; profiling prep on `profiling-prep` +
  `ay-prepare-profile-wave` + `ay-samply-*`; heavy close-gate
  proof on `ay-bench-close WAVE=close` / `test-close` /
  `final-bench`. `cargo check --workspace`, `cargo test
  --workspace`, and `cargo bench` are the heavy close-proof
  surface, not the routine one; see §"Three-tier command
  surface" under §Bench contract.
```

**After (full replacement):**

```
- **NO heavy-surface routine defaults.** Routine iteration runs
  on `iter-check` / `iter-test-{leaf,grammar,ws}` / `ay-expand-*`
  / `ay-test-*`; profiling prep on `profiling-prep` +
  `ay-prepare-profile-wave` + `ay-samply-*`; heavy close-gate
  proof on `ay-bench-close WAVE=close` / `test-close` /
  `final-bench`. The following commands are the heavy close-proof
  surface and MUST NOT appear in iteration loops:
    - `cargo check --workspace` (any profile)
    - `cargo check -p bbnf --tests` (any profile — links the full
      aggregate derive-Parser surface at ~26 GB peak RSS; see
      `README.md` §"Memory discipline for aggregate test binaries")
    - `cargo test --workspace`
    - `cargo bench`
  Every iteration-loop `cargo check` / `cargo test` invocation
  carries `--profile ax-iter` explicitly; bare `cargo check
  -p <crate> --tests` (no profile) is heavy-surface by default.
  See §"Three-tier command surface" under §Bench contract.
```

**Rationale.** The current text names forbidden invocations but
omits `cargo check -p bbnf --tests` — the single most-observed
violation in the recent session. Making the list explicit and
naming the bare-dev-profile form as heavy closes the gap.

### Edit 2: `docs/instructions/tranche/SPEC.md` §Diagnostic-loop relinquish

Append after line 395, after the paragraph ending
"grinding is the incorrect one.":

**After (new paragraph):**

```
**Per-role wall-time caps.** Research agents: 20 min hard cap —
read-only work; longer windows signal over-scoped reads, not deeper
analysis. Plan agents: 15 min hard cap — plan-authoring is dense and
focused; longer signals the redress should split into sub-waves
instead. Redress agents: 30 min hard cap — concrete edits and
verification. At cap, the agent writes its deliverable with whatever
it has, commits probe artefacts separately from attempted fixes per
the partial-commit rule, and returns. The orchestrator's own
escalation threshold is ~60 min without a cherry-pick landing onto
master; at that threshold the orchestrator reports to the user
rather than dispatching another round.
```

**Rationale.** The current 30-min cap is role-blind. Per-role caps
shape dispatch expectations upstream and give the orchestrator a
concrete escalation threshold that matches Indefatigability without
swallowing user input time.

### Edit 3: `docs/instructions/README.md` §Expensive commands — always file-first

**Before (lines 354-362):**

```
**Never read large output files line-by-line.** `cargo expand`
output routinely exceeds 100K lines. Use targeted `grep -n`,
`awk`, `sed` to extract the slice you need. Know the file size
before reading.

```bash
grep -n 'fn __declaration' /tmp/expand-css.txt
awk 'NR>=5000 && NR<=5100' /tmp/expand-css.txt
awk '/fn __declaration/,/^        fn __/' /tmp/expand-css.txt > /tmp/decl.txt
wc -l /tmp/decl.txt
```
```

**After (full replacement):**

```
**Never read large files line-by-line.** Any file over ~2 K lines
— `cargo expand` output, `generated.rs`, monolithic audit docs,
session transcripts (`.jsonl`), long PROGRESS files, bench logs —
is read via `grep -n`, `awk`, `sed` with explicit line ranges, not
whole-file `Read`. Know the file size via `wc -l` before the first
access; if > 2 K lines, every subsequent access uses targeted
extraction. This rule covers both generated artefacts and source
files; `generated.rs` (emitter output, typically > 30 K lines) is
file-first like any other large artefact.

```bash
wc -l crates/core/src/grammar/generated.rs
grep -n 'fn __declaration' /tmp/expand-css.txt
awk 'NR>=5000 && NR<=5100' /tmp/expand-css.txt
awk '/fn __declaration/,/^        fn __/' /tmp/expand-css.txt > /tmp/decl.txt
wc -l /tmp/decl.txt
```
```

**Rationale.** The current text scopes to "cargo expand output" and
"large output files"; source files — `generated.rs`, audit markdown —
are implicitly excluded. The rule is identical in spirit; the
rephrasing makes it explicit and names `generated.rs` directly.

### Edit 4: `docs/instructions/tranche/SPEC.md` §Commit discipline

Append after line 166 (after the AW-I.W4β template paragraph):

**After (new paragraph):**

```
**Cherry-pick conflict resolution.** When a wave's cherry-pick
sequence hits a 3-way merge conflict on a file *not* pre-declared as
an N-agent-shared-file consolidation target, the orchestrator (a)
stops cherry-picking, (b) resets master to the last successful pick,
(c) inspects the conflicting hunks directly, (d) either rewrites the
conflicting agent's change as a fresh orchestrator-authored commit
citing the agent's worktree, OR re-dispatches the agent on a fresh
worktree seeded at current master HEAD. Option (a)/(b)/(c)/(d) is
faster for a single small conflict; redispatch is preferred when the
conflict signals the agent's change was against stale code. The
cherry-pick `--strategy recursive -X theirs` form is forbidden for
conflict resolution — it silently discards orchestrator-merged
content.
```

**Rationale.** The current text covers preventative
consolidation-planning but not reactive conflict resolution. The
session showed 12 conflict events without a documented protocol.

### Edit 5: `docs/instructions/tranche/SPEC.md` §Transitional fallback during elimination waves

**Before (lines 451-460):**

```
### Transitional fallback during elimination waves

An in-transit fallback path whose elimination is the tranche's
principal work is work-in-progress, not a workaround. The
one-codegen-path invariant binds at tranche close, not at every
wave close. AX.W0a kept walker fallback green across seven sub-
waves precisely to eliminate it in the eighth. A wave may revert
an admission-widening commit to preserve master-green while the
follow-on wave lands the consumer-side fix; the revert is
Absorb-mode, not deferral, when the follow-on wave is named in
PROGRESS.md at revert time.
```

**After (full replacement):**

```
### Transitional fallback during elimination waves

An in-transit fallback path whose elimination is the tranche's
principal work is a declared delete-then-swap window (per §Wave
stipulation, "Workspace green at every wave boundary" exception).
The one-codegen-path invariant binds at tranche close, not at every
wave close, precisely *because* the plan's escape clause names the
transitional path and the wave that eliminates it. AX.W0a kept
walker fallback green across seven sub-waves to eliminate it in the
eighth; the eighth wave is the named restoration wave. A wave may
revert an admission-widening commit to preserve master-green while
the follow-on wave lands the consumer-side fix; the revert is
Absorb-mode, not deferral, when the follow-on wave is named in
PROGRESS.md at revert time. The workspace-green-at-every-wave
invariant and the tranche-close-one-path invariant are not in
conflict — the plan's escape clause reconciles them.
```

**Rationale.** The existing text reads as a conflict with §Wave
stipulation's workspace-green rule. Naming the exception as a
delete-then-swap window (the same exception §Wave stipulation
already defines) resolves the read-level ambiguity.

### Edit 6: `docs/instructions/tranche/SPEC.md` §Commit discipline (second addition)

Prepend to line 153 (above "Sub-agents commit inside worktrees"):

**After (new lead paragraph):**

```
Commit cadence has two surfaces: sub-agent cadence inside worktrees,
and orchestrator cadence onto master. They run at different
rhythms.

- **Sub-agents commit frequently** inside their worktrees at every
  natural milestone (phase sub-item, artefact landing, invariant
  restoration), per `README.md` §Code discipline.
- **Orchestrator commits to master at wave boundaries**, via cherry-
  pick-then-dispatch. Master clean before every wave dispatch is the
  hard invariant; orchestrator commits *during* a wave (on master)
  break that invariant and are forbidden outside the named
  consolidation-commit template.
```

**Rationale.** Resolves C2 contradiction between README's
"commit-frequently" and SPEC's "master-clean-before-dispatch" — the
two are both true once the surface is named.

### Edit 7: `docs/instructions/README.md` §Tranche completion requirements, item 2

**Before (lines 313-322):**

```
2. **`docs/benchmarks/post-{LETTER}.json` exists** covering the
   full parse-bench matrix:
   - `json_monolithic` × {data, twitter, citm, canada, data_xl}
   - `css_l4` × {normalize, bootstrap, tailwind}
   - `google_sheets_monolithic` × {parse_simple, parse_nested, parse_stress}
   - `bbnf_monolithic` × {json, ebnf, css_pretty, google_sheets, bbnf_self, css_l4_grammar}

   The VM, WASM, TS, and competitors benches are **not** included.
   Numbers come from a fresh cold bench run on master after all
   tranche commits have landed.
```

**After (full replacement):**

```
2. **`docs/benchmarks/post-{LETTER}.json` exists** covering the
   full close matrix (five benches; same as `make ay-bench-close
   WAVE=close` in `docs/instructions/PROFILING.md` §"AY W5-W7 gate
   commands"):
   - `json_monolithic` × {data, twitter, citm, canada, data_xl}
   - `css_l4` × {normalize, bootstrap, tailwind}
   - `google_sheets_monolithic` × {parse_simple, parse_nested, parse_stress}
   - `bbnf_monolithic` × {json, ebnf, css_pretty, google_sheets, bbnf_self, css_l4_grammar}
   - `compile_pipeline` × tranche-selected entries

   The VM, WASM, TS, and competitors benches are **not** included.
   `json_value` is included when the tranche touches the Value
   API surface. Numbers come from a fresh cold bench run on master
   after all tranche commits have landed, via `make ay-bench-close
   WAVE=close` (fat LTO).
```

**Rationale.** Reconciles README's enumeration with the actual
PROFILING close-matrix table and the `json_value` bench that
PROFILING names but README omits.

### Edit 8: `docs/instructions/README.md` §Sub-agent polling (new section)

Insert new section between §Expensive commands and §Memory
discipline (after line 363):

**After (new section):**

```
## Sub-agent progress monitoring

The harness delivers background-process completion events to the
orchestrator automatically. Explicit polling of sub-agent or
background-command progress is forbidden:

- No `ps aux | grep rustc`, `ps aux | grep cargo`, or equivalent
  process-level probes.
- No repeated `tail -f` or re-read cycles on a log file a background
  command is writing.
- No `sleep`-then-check loops.

Background commands dispatched with `run_in_background:true`
deliver completion notifications as tool-call results; the Monitor
tool streams stdout-line events for long-running tasks that need
intermediate signal. If a sub-agent's worktree state must be
inspected mid-dispatch, use a one-shot
`scripts/worktree-status.sh <worktree>` invocation; this reads
`git status` and recent commits in one call, not a poll loop.

Polling burns context and produces no actionable state the harness
does not already surface. The orchestrator's time is spent on
cherry-pick, verification, and re-planning — not on monitoring.

## Concurrent cargo invocations — one per target

At most one `cargo` invocation is in flight per `CARGO_TARGET_DIR`
at any instant. `target/.cargo-lock` silently serialises concurrent
invocations with indeterminate ordering; benches, samply prepare,
and build-cache-sensitive workflows observe non-deterministic
artefacts under the contention.

- Parallel sub-agents share one target via symlink per §Worktree
  isolation — these sub-agents do not run cargo concurrently
  against that shared target. Orchestrator sequences them.
- Parallel sub-agents that each need a concurrent cargo invocation
  operate in distinct `CARGO_TARGET_DIR`s (per-worktree target, no
  symlink) and accept the rebuild cost.
- Orchestrator never spawns its own concurrent cargo invocations
  against the main target while a wave's agents are active.
```

**Rationale.** Fills A1 + A2 gaps with explicit prose.

## Proposed new memory entries (1-5)

Per the audit's scope — newly-surfaced disciplines that are not yet
in memory. Agent 1 is also proposing memory entries; we reconcile at
orchestrator integration.

### M1. `feedback_no_polling_loops.md` — Monitor / run_in_background, not ps aux

**Summary.** The harness surfaces background-process completion as
tool-call results. Polling via `ps aux | grep rustc`, `tail -f`,
or `sleep`-then-check loops is forbidden — burns context, produces
no signal the harness does not already deliver. Use the Monitor
tool for streaming progress; use `run_in_background:true` for
one-shot completion.

### M2. `feedback_single_cargo_per_target.md` — one cargo invocation per CARGO_TARGET_DIR

**Summary.** `target/.cargo-lock` serialises concurrent cargo
invocations with indeterminate ordering; bench artefacts become
non-deterministic under contention. Sub-agents sharing one target
via symlink do not run cargo concurrently. The orchestrator does
not spawn concurrent cargo invocations against the main target.

### M3. `feedback_iter_profile_always.md` — always `--profile ax-iter` for iteration `cargo check`/`cargo test`

**Summary.** Bare `cargo check -p bbnf --tests` links the full
derive-Parser aggregate at ~26 GB peak RSS. Every iteration-loop
invocation carries `--profile ax-iter` explicitly. The heavy-
surface list (in SPEC §Edicts) enumerates `cargo check -p bbnf
--tests` specifically; extend whenever a new derive-heavy test
binary appears.

### M4. `feedback_read_size_preflight.md` — `wc -l` before any Read > 2 K lines

**Summary.** The file-first rule covers *source* files too —
`generated.rs` (~30 K lines), monolithic audit docs, session
transcripts (`.jsonl`), long PROGRESS files. Every Read against
a likely-large file is preceded by `wc -l` and, if > 2 K lines,
switched to targeted `grep -n` / `awk` extraction.

### M5. `feedback_redispatch_empty_return.md` — empty returns are not scope-reveal

**Summary.** A sub-agent returning with no commits, no artefact
citations, and no scope-reveal diagnosis triggers same-brief
redispatch on a fresh worktree with a pointer to the prior
worktree's git log. Not a re-plan, not an escalation. Three
consecutive empty returns against the same brief escalate to user
per Indefatigability.

## Cross-references to agent 1/3/4 work

- **Agent 1 (session-mining).** Likely surfaces the same
  ps-aux-polling, redispatch, and heavy-surface-default patterns.
  Reconcile at orchestrator integration:
  - If agent 1's memory proposals overlap M1-M5, merge under one
    entry per concern; do not duplicate.
  - If agent 1 finds user corrections explicitly forbidding
    behaviours enumerated here as "ambiguous", upgrade those gaps
    from §"Missing" to §"Violated" and point to the
    correction-commit as the authoritative edict.

- **Agent 3 (tranche plans).** Likely flags the heavy-surface
  default creep inside tranche-plan hard gates (e.g., plans
  citing `cargo test --workspace` as the iteration gate).
  Proposed Edit 1 + M3 give them language to cite.

- **Agent 4 (B1 tranche).** B1's wave specs should flag any
  gates that close on `cargo check --workspace` or bare-profile
  `cargo check -p bbnf --tests`. If B1 carries such gates, the
  SPEC.md Edit 1 rewrite applies to those gates retroactively at
  edit-land time.

- **Redispatch protocol (A4 / M5).** This is a harness-layer
  concern (how the orchestrator loops on empty returns); agent 1's
  session mining is the ground truth for whether the 16 observed
  redispatch events followed a consistent pattern or were ad-hoc.
  Our proposed addition should be ratified against agent 1's
  findings before landing.

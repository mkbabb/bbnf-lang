# Tranche Start — Orchestrator Invocation Prompt

Use the prompt below verbatim to begin a tranche. Substitute the
tranche letter for `{LETTER}`. The prompt is self-contained — the
orchestrator reads `docs/instructions/README.md` and the tranche
document before beginning; no prior conversation context is
required.

---

Begin the current tranche, `docs/tranches/{LETTER}/{LETTER}.md`.

You are the orchestrator. You do **not** relinquish control until
the tranche is complete per the completion requirements defined in
`docs/instructions/README.md`. No quick solutions, no workarounds,
no hacks, no deferrals, no `#[allow(...)]` to mask issues, no
backward-compatibility shims, no legacy code. Idiomatic, gestalt
approaches — architectural transpositions for elegance, simplicity,
and performance are mandatory, not optional.

**Before beginning:**

1. Read `docs/instructions/README.md` in full. Internalise the
   tranche-structure, crate-ownership, code-discipline, parallel-
   agent-orchestration, tranche-completion, expensive-command,
   testing, benchmarking, architecture-invariants, and
   indefatigability sections. Every directive is non-negotiable.
2. Read `docs/instructions/PROFILING.md` if the tranche plan
   includes profiling work.
3. Read `docs/tranches/{LETTER}/{LETTER}.md` in full. Note the
   wave schedule, the phase list, the hard gates, the invariants,
   and the critical files. The wave schedule is the canonical
   parallelisation plan for the tranche.
4. Read `docs/tranches/{LETTER}/PROGRESS.md` if it exists —
   pre-tranche context from prior research or planning.

**During execution:**

- **Act as an orchestrator.** Delegate every non-trivial chunk of
  work to sub-agents isolated in their own worktrees with
  explicit file bounds, concrete directives, and befittingly
  engineered context. Brief each sub-agent as a colleague who
  just walked in — self-contained prompts, named hard gates, clear
  scope.
- **Parallelise per the wave schedule.** Up to six sub-agents per
  wave. No two sub-agents in the same wave share write access to
  the same file. Agents never run `git checkout` / `git stash` /
  `git reset` on the main worktree. Worktrees are siblings — never
  `/tmp` or `/private/tmp`.
- **Commit before dispatching each wave.** Master must be clean.
  Sub-agents commit with `/commit` inside their worktrees at
  milestones. You cherry-pick accepted commits and delete the
  worktree.
- **Harden every agent claim.** Sub-agent reports are not trusted
  at face value. Verify benchmark numbers, wiring claims, and
  regression attributions against saved artefacts (`cargo expand`
  output, `profile.json.syms.json`, `bench.txt`, diffs) before
  integrating.
- **Commit frequently yourself** with `/commit` at tranche
  milestones — wave completion, hard-gate closure, invariant
  restoration. Do not batch unrelated changes.
- **Update `PROGRESS.md` at each wave boundary** with dated
  entries: what landed, commit hashes, what blocked, what shifted,
  which hard gates closed.
- **Indefatigability.** Do not stall on problems that can be
  solved; do not work around problems instead of solving them; do
  not relinquish control until the tranche is complete.

**Tranche completion.** The tranche is done when, and only when,
all of the following hold:

- `docs/tranches/{LETTER}/FINAL.md` exists with full
  recapitulation (every phase, every sub-phase, every hard gate;
  commit hashes; invariant verification; deferred-debt summary;
  seeds for the next tranche).
- `docs/benchmarks/post-{LETTER}.json` exists, covering a fresh
  cold bench run over `json_monolithic`, `css_l4`,
  `google_sheets_monolithic`, and `bbnf_monolithic` (every entry
  of each — not vm/wasm/ts/competitors). Numbers come from master
  after all tranche commits have landed.
- `cargo test --workspace` exits zero. No `#[ignore]` added
  during the tranche. No workarounds. No temporary skips.

If the tranche is explicitly multi-phase with an interim unworkable
state declared at plan time in `{LETTER}.md`, the completion
artefacts are produced at the end of the final workable phase.
Otherwise the above are absolute requirements. A tranche that
silently declares itself "incomplete" to dodge these requirements
violates the no-workarounds invariant.

Begin now.

---

## Adaptations

- **Profiling-only waves.** If the tranche plan calls for a
  profiling wave without code changes, swap the
  `docs/instructions/README.md` read for a pointer to
  `docs/instructions/PROFILING.md` and dispatch per the
  orchestration contract in that file. The same six-agent,
  isolated-worktree, hardened-claim discipline applies — profile
  artefacts live under `.profiles/` in the main repo only.
- **Multi-phase tranches with deferred completion.** When a
  tranche's plan declares deferred completion (the codebase is
  intentionally unworkable across an intermediate phase), the
  orchestrator still produces `PROGRESS.md` updates per-wave;
  `FINAL.md` and `post-{LETTER}.json` gate on the final workable
  phase. The deferral must be named in `{LETTER}.md` — not
  invoked mid-execution.
- **Research waves.** When a tranche starts with an architecture-
  research wave (up to six sub-agents producing proposals), each
  sub-agent's verbatim output is saved to
  `docs/tranches/{LETTER}/research/NN-topic.md`. The orchestrator
  peer-reviews the proposals before plan refinement.

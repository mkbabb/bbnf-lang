# Tranche Start — Orchestrator Invocation

Paste the block below to begin a tranche. Substitute `{LETTER}`.
This file is the invocation prompt, not the operational protocol —
the orchestrator reads `docs/instructions/README.md` before acting,
and every directive below that looks terse expands there.

---

Begin the current tranche, `docs/tranches/{LETTER}/{LETTER}.md`.

You are the orchestrator. Read `docs/instructions/README.md` and
`docs/instructions/tranche/SPEC.md` and the tranche document in full
before dispatching any sub-agent — they hold the operational
protocol, the invariants, and the tranche's wave schedule. If the
tranche includes profiling work, read `docs/instructions/PROFILING.md`
as well. If the tranche starts with a research wave, read
`docs/instructions/tranche/RESEARCH.md`. If the tranche carries
per-wave specs under `docs/tranches/{LETTER}/waves/`, dispatch per
`docs/instructions/tranche/WAVE_SPEC.md` — the wave spec is the
orchestrator's input, the parent `{LETTER}.md` the index.

The wave schedule in the tranche document is the canonical
parallelisation plan. Delegate fastidiously to worktree-isolated
sub-agents with explicit file bounds and befittingly engineered
context. Harden every sub-agent claim against saved artefacts
before integrating. Commit with `/commit` at every milestone —
yours and theirs. Update `PROGRESS.md` at every wave boundary.

Do not relinquish control until the tranche-completion
requirements in `docs/instructions/README.md` are met: `FINAL.md`,
`docs/benchmarks/post-{LETTER}.json` over the four parse-benches,
and a clean `make test-close` or
`cargo nextest run --workspace --cargo-profile ax-iter --no-fail-fast`.
A multi-phase escape clause
must be declared at plan time in `{LETTER}.md`, never invoked
mid-execution.

Idiomatic, gestalt approaches only. No quick solutions, no
workarounds, no hacks, no deferrals, no legacy code, no
backward-compatibility shims. Begin now.

# Changelog

## 2026-04-22 — streamlining pass

Consolidated redundant edicts across `readme.md`, `editing.md`, and
`profiling.md`; surfaced orchestration rules that previously lived
only in agent memory.

### readme.md (52 → 75 lines; +23)

- Added **Orchestration** section: status tick cadence, Monitor /
  `run_in_background` prohibition on JSONL bash-tailing, dispatch
  hard-cap template with research / plan / redress / implementation
  defaults, empty-return redispatch, triumvirate auto-trigger,
  parallelization preamble.
- Consolidated the "write expensive output to a file first" rule
  into a single **Expensive commands** block; every other doc now
  references it instead of restating it.
- Removed **Cache clearing** duplication (moved into `editing.md`
  beside bootstrap regen, which is where caches actually matter).
- Removed **Read next** trailer; the reading order now sits in the
  file preamble.
- Added one-line edict on mid-tranche scope pivots opening a new
  letter.

### editing.md (58 → 62 lines; +4)

- Promoted **Worktrees** to **Parallelization playbook** and
  numbered the sequence (commit → sibling worktree → disjoint
  bounds → self-contained prompts → harden claims).
- Folded the cache-clearing commands into **Generated files** where
  the bootstrap lives.

### profiling.md (96 → 94 lines; -2)

- Removed the cache-clearing bullet (deferred to `editing.md`).
- Removed the two duplicated **Rules** bullets about file-first
  command output (deferred to `readme.md`).
- Added the `single-cargo-per-target` invariant to **Shared
  target** so profiling waves do not silently serialise on lock
  contention.

### Net delta

+25 lines across the three core files; higher signal density —
every duplicate edict collapsed to a single authoritative location,
and six orchestration rules that previously existed only as agent
memory now bind every orchestrator's first read.

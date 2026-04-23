# Changelog

## 2026-04-23 — second streamlining pass

Applied the W4 streamlining thesis to the expanded uppercase surface
(`README.md` at 653 LOC, `PROFILING.md` at 363 LOC) that master-side
meta-audit integrations produced. Target: reduce LOC while raising
actionable-instruction density; preserve every rule.

### README.md (653 → 520 lines; -133, ~20%)

- Collapsed multi-paragraph Code-discipline rules
  (execute-the-plan, relinquish-when-stuck, substrate-with-consumer,
  audits-analyse-expand-begotten, no-god-modules) into tighter
  statements of the same edict — each paragraph now answers "what
  changes in the next agent message" in its first sentence.
- Folded Sub-agent progress monitoring into the Orchestration
  section under **No bash-tail on JSONL**, alongside Monitor /
  `run_in_background` guidance; the free-standing section disappears.
- Promoted orchestration rules that were previously only in agent
  memory into Orchestration as first-read material: **Dispatch
  hard-cap template**, **Status tick cadence**, **Empty-return
  redispatch**, **Triumvirate auto-trigger**.
- Tightened Wave verification ledger bullets to load-bearing
  sentences only; Symbol / Wire-contract / Samply attribution /
  Substrate-without-consumer now each fit in 3-5 lines instead of
  8-12.
- Compressed Bootstrap self-host escape recipe from ~40 to ~28
  lines while preserving all four numbered steps and commit-template
  references (`87f65214`, `49656fd4`).
- Reduced "Hoist emitter-known data into emitted code" to the
  codegen-form / runtime-indirection contrast; dropped the ThinLTO
  speculation tangent.
- Removed meta-narration preamble ("Companion documents:" list) in
  favour of one inline sentence.

### PROFILING.md (363 → 293 lines; -70, ~19%)

- Merged "Shared-target discipline" and "Parallel probe discipline"
  into one section; cross-referenced `README.md` §Concurrent cargo
  instead of restating the lock-contention rule.
- Merged redundant "B0 close proof" and "W2 close proof" tables
  into one B0 close proof table; the AY W5-W7 gate commands table
  remains authoritative for per-gate mapping, and the close proof
  references it rather than duplicating rows.
- Inlined Required artefacts (previously 7 bulleted lines) as a
  single prose sentence.
- Compressed Forbidden in sub-agents bullet list into one sentence
  of comma-separated prohibitions.
- Dropped decorative shared-target headline preamble; inlined the
  bullets into flowing prose.

### Rule locations (single authoritative location each)

- **Expensive commands → file first** — `README.md` §Expensive
  commands. PROFILING.md no longer restates; it inherits via the
  opening reference.
- **Cache clearing** — `README.md` §Cache clearing. Other sections
  show the invocation as part of a recipe but do not restate the
  rule.
- **Worktree isolation** — `README.md` §Orchestration,
  Worktree isolation. PROFILING.md §Shared-target discipline only
  notes worktrees are *optional* for profiling.
- **Dispatch hard-cap template** — `README.md` §Orchestration,
  Dispatch hard-cap template.
- **No bash-tail on JSONL** — `README.md` §Orchestration,
  No bash-tail on JSONL.
- **Status tick cadence** — `README.md` §Orchestration, Status tick
  cadence.
- **Empty-return redispatch** — `README.md` §Orchestration,
  Empty-return redispatch.
- **Triumvirate auto-trigger** — `README.md` §Orchestration,
  Triumvirate auto-trigger; referenced from §Code discipline,
  Relinquish when stuck.
- **Single cargo per target** — `README.md` §Concurrent cargo.
  PROFILING.md §Shared-target discipline cross-references.

### Net delta

-203 lines across README.md + PROFILING.md. No rule dropped. Every
edict now resides at exactly one authoritative location; every other
reference is a pointer.

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

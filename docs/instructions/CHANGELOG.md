# Changelog

## 2026-04-29 — AZ-II partial close snapshot + cutover.O alignment

AZ-II's implemented-state record is
`docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`: cutover.A
through cutover.M landed, cutover.N dispatched and halted at
organizational usage limit, and cutover.N landed no code commits. The
live terminal sequence is AZ-II `cutover.O.0` through `cutover.O.7`:
tooling preflight, StructDirect builder transactions, EBNF direct
projection, generated view purge, `Parsed<R>` / `TapeDirect` deletion,
`crates/tape` deletion, semantic/perf refresh, and FINAL conversion.

## 2026-04-28 — AZ-I W0/W1/W2-substrate close

AZ-I opened against post-B7 substrate and closed three waves in
sequence: W0 (CLASSIFIER-UNIFICATION.md locked-split disposition +
typed-`->` audit pass with three-way `MarkerStatus` and pluggable
`StructRegistryProbe` trait), W1 (`StructRegistry` + `StructLayout`
+ `LayoutKind`; `project_types` populates the registry inside the
fixed-point pass; per-grammar wire-contract tests on JSON / Sheets
/ CSS L4; emitter registry-read consumer in bridge mode;
`TypeDesc::has_scalar_payload` recursion closing the keyword-
discriminator Tuple-wrapped audit gate), W2 substrate
(`StructBuilder` trait + JSON runtime types + `EmitStrategy` enum +
parse_body two-path emission + nine per-shape struct-direct
emitters covering Object / Array / Number / String / Scalar /
Keyword / Wrap / AltDispatch / Flat + dispatcher signature
threading + JSON parity harness scaffold).

W2 closed substrate-only per `W2.md` §Reversal: the resolver
returns `TapeDirect` for every grammar pending W2-act follow-on
landing the JsonDocument view/value accessor API, the parity
harness recoding, and the cargo bench gate. The wave's underlying
patterns — inline `bbnf_ir::registry::StructLayout` literal,
fully-qualified trait method calls, dispatcher signature
parameterized by strategy — are proven and integrated; activation
is a single resolver-arm flip plus three downstream consumer
migrations away.

Workspace verification at AZ-I.W2 close: 1546 / 1546 nextest
passed, 27 skipped. master HEAD `409b835d`.

## 2026-04-27 — B-series (B2-B7) cross-cutting closures

Six tranches landed in sequence post-B1: B3 (parser-baseline
restoration), B4 (codegen syn::parse2 emit-fix), B2 (build-time
codegen transposition; `cargo xtask regen` canonical), B5 (substrate
restoration; FusedBuilder dissolved into `Tape<R>` over `Columns`;
eight waves), B6 (dev-loop annex; xtask cycle fix delivered 192× cold-
wall speedup), B7 (cross-repo modernization; divan + nextest unified
across bbnf-lang / parse-that / pprint; 10 agents, 20 commits, ~15 min
real wall).

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

# AY-II.W0' — Pause Snapshot (2026-04-21)

Orchestrator paused W0' dispatch mid-wave after 2 of 3 sub-agents
returned clean and the third was interrupted at a stable commit
boundary. This document captures the exact state so resumption is
deterministic.

This is a historical snapshot, not an active dispatch surface. The
live execution order is B1 close -> AY-II.W0' close -> AY-II W1-W5.

## Master HEAD

`60968449` — `docs(ay-ii): update W1-W5 sub-wave docs per plan-audit findings`.

No W0' sub-agent commits have been cherry-picked to master. Every
worktree commit sits on a detached HEAD inside the sibling worktree.

## Sub-agent state

### W0'.a — FusedBuilder collapse

- Worktree: `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0p-a`
- Detached HEAD at `8fc504fa` (7 commits ahead of master).
- Status: **returned complete**.
- Commits (oldest → newest):
  - `c8986f95` — `refactor(tape): collapse TapeBuilder + ValueBuilder into FusedBuilder (AY-II.W0'.a)`
  - `3c180fe9` — `refactor(tape): rename finish -> finish_fused; keep finish tape-only (AY-II.W0'.a)`
  - `d74b4d6f` — `refactor(runtime): retire standalone ValueBuilder; Parsed holds FusedOutput (AY-II.W0'.a)`
  - `69414aa4` — `refactor(emitter): single FusedBuilder parse-entry; retire parse_with_visitor (AY-II.W0'.a)`
  - `a2383661` — `fix(runtime): preserve 4-arg new_fused pre-regen; emitter uses new_fused_output (AY-II.W0'.a)`
  - `684b3109` — `refactor(tape): ungate FusedBuilder new-call counter for downstream tests (AY-II.W0'.a)`
  - `8fc504fa` — `docs(tape, runtime): scrub hard-gate pattern matches from doc comments (AY-II.W0'.a)`

Key deliverables:

- `FusedBuilder` (non-generic) in `crates/tape/src/builder/mod.rs`
  absorbs `TapeBuilder` + `ValueBuilder` state.
- `FusedOutput<R>` in `crates/tape/src/builder/output.rs` (+212 LOC,
  NEW file) — tape + value output bundle.
- `ValueFrame` / `PayloadTag` / `ValueFramesOutput<R>` in
  `crates/tape/src/builder/value.rs` (+374 LOC, NEW file).
- `crates/core/src/runtime/value_builder.rs` — DELETED (-708 LOC).
- `parse_with_visitor` emission path retired at
  `crates/core/src/backend/rust/emitter/grammar.rs:1163-1334`.
- Net: +266 LOC across the refactor.

Transient compose-escape aliases (FLAG for close-cleanup pass):

- `pub type TapeBuilder = FusedBuilder;` — lets pre-W0'-regen
  `generated.rs` compile.
- `_ValueBuilderShim` / `ValueBuilder<R>` ZST — preserves
  `value_api_apples_to_apples.rs`'s counter imports.
- `pub type ValueBuilderOutput<R> = FusedOutput<R>;` — alias for
  W0.c's exported output type.
- `Parsed::new_fused(tape, input, root_off, value)` 4-arg shim
  alongside canonical `Parsed::new_fused_output(output, input, root_off)`.

Each alias exists ONLY to bridge the bootstrap escape window
(pre-regen `generated.rs` references the old names). Every alias
retires at W0' close after post-regen `generated.rs` uses the
canonical names.

Known outstanding (out of W0'.a file bounds):

- `crates/tape/tests/{tape_basic,close_compound,packed_cache}.rs`
  still call `push_compound` / `mark_children` directly.
- `crates/core/tests/tape_walker_allocs.rs` — same.
- `crates/json-prototype/src/visitor.rs` — same.

These must migrate to `begin_compound` / `end_compound` at W0' close
(orchestrator-owned or a W0'.d cleanup sub-agent). Template:
`crates/tape/src/visitor.rs` already migrated inside W0'.a.

Bootstrap regen in-worktree was intractable (7+ min rustc expand
without output; target locks held by sibling worktrees). Orchestrator
owns regen at close.

### W0'.b — Projection-consumer wiring

- Worktree: `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0p-b`
- Detached HEAD at `41f54161` (2 commits ahead of master).
- Status: **interrupted at stable commit boundary**. Agent was
  validating when stopped by orchestrator; no work discarded.
- Commits (oldest → newest):
  - `b1273bf5` — `feat(view,emitter,tests): route project_value_<G> through materialize_projection_*_<G> (AY-II.W0'.b)`
  - `41f54161` — `fix(view,emitter): raw_name for materializer lookup + runtime tape path (AY-II.W0'.b)`

Unresolved question at interruption: final shape of the
`<Grammar>Value::Unknown` retirement ledger per grammar. The
W0'.b return was mid-write when paused — no final summary in
agent transcript.

Known deliverables (inferred from commit messages):

- `emit_project_value_<grammar>` routes per-admission arms through
  `materialize_projection_<rule>_<Grammar>`.
- Raw-name lookup in view/emitter for materializer resolution.
- Runtime tape-path fallback for admissions not covered by fused
  slab reads.

Resumption requires reading `b1273bf5` + `41f54161` diffs + running
`cargo test --test projection_totality` + `cargo test --test
value_api_apples_to_apples` to confirm runtime-call-count
assertion green.

### W0'.c — Structural-scan policy splice + dead_code retirement

- Worktree: `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0p-c`
- Detached HEAD at `f3ca796e` (3 commits ahead of master).
- Status: **returned complete**.
- Commits (oldest → newest):
  - `a61b69ca` — `feat(view): splice STRUCTURAL_SCAN_POLICY into emit_path_walk at codegen (AY-II.W0'.c)`
  - `42d19886` — `chore(emitter): retire W0-era #[allow(dead_code)] at 8 sites (AY-II.W0'.c)`
  - `f3ca796e` — `fix(view): route scan-policy match arms through raw rule names (AY-II.W0'.c)`

Key deliverables:

- `__path_walk` splices `cursor.object_key_seek` /
  `bounded_lookahead` / `scan_structural_bounded` at codegen per
  `STRUCTURAL_SCAN_POLICY` flag. No runtime dispatch.
- `#[allow(dead_code)]` count: 11 → 1 (-10 retired).
- Dead helpers deleted: `emit_regex_attempt` (alt_dispatch.rs),
  ShapeTag self-assertion, `ProjectionFieldKind::CursorChild.ty`
  (W2-staged slot never consumed), `__SHAPE_ESCAPE_HELPER_MARKER`,
  `__VISITOR_ESCAPE_HELPER_MARKER`.

Retention flagged (scope-reveal):

- `keyword_dispatch.rs:146` `__phf_*_dispatch_*` fn —
  zero walker callers but exercised by `tests/phf_keyword_dispatch.rs`.
  Retirement requires cross-file test migration; out of W0'.c bounds.
  Routes to W0' close cleanup pass OR a follow-on sub-agent.

## Cherry-pick plan (when resuming)

1. Verify W0'.b state first (2 commits; unclear if complete). Read
   the diff; run its test targets in its worktree; either accept or
   redispatch.
2. Cherry-pick in dependency order: W0'.a (7 commits) → W0'.c (3
   commits) → W0'.b (2 commits). W0'.a provides FusedBuilder +
   FusedOutput types; W0'.c + W0'.b both depend on those APIs.
3. Conflict risk: W0'.c edits `view/value.rs::emit_path_walk`;
   W0'.b edits `view/value.rs::emit_project_value_*`. These are
   disjoint regions in the same file; cherry-pick sequence may auto-
   merge or produce a trivial conflict resolved by keeping both
   regions.
4. Orchestrator regen: `bash scripts/bootstrap-bbnf.sh` post-compose.
5. Verify double-regen idempotency.
6. Retire transient aliases from W0'.a (`TapeBuilder` → drop alias,
   `_ValueBuilderShim` → delete, `ValueBuilderOutput` → drop alias,
   4-arg `new_fused` → drop shim) after regen produces
   canonical-name call sites.
7. Migrate out-of-bounds tests: `crates/tape/tests/*` +
   `crates/core/tests/tape_walker_allocs.rs` +
   `crates/json-prototype/src/visitor.rs` + `phf_keyword_dispatch`
   test from `push_compound` / `mark_children` / `__phf_*_dispatch_*`
   to the FusedBuilder API.
8. Close ceremony: fat-LTO 5-bench matrix + samply per grammar + nm
   per bench binary.

## No processes running

All rustc / cargo processes killed at pause. `ps aux | grep -E
'cargo|rustc' | grep -v grep` returns empty.

## Worktree inventory

```
/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0p-a  — W0'.a, 7 commits
/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0p-b  — W0'.b, 2 commits
/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0p-c  — W0'.c, 3 commits
```

All worktrees clean (only `?? target` symlinks). Sibling AY / AZ
worktrees from prior tranches remain; not touched by W0'.

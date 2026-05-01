# AX.W0a.2.d — scope-reveal halt + diagnosis

## Status

Partial. Only the substrate (`shapes/inline.rs` helper module) landed.
The consumer-side wiring + predicate widening scoped larger than one
sub-agent could close within the memory + time bounds. Orchestrator
re-plan required.

## What landed

Commit `8bf331e8` — `feat(shapes/inline): inline-position emitter
module for dispatch retirement (AX.W0a.2.d)`:

- `crates/core/src/backend/rust/emitter/shapes/inline.rs` created.
- `pub(crate) mod inline;` exported from `shapes/mod.rs`.

The module exports two entry points:

- `emit_inline_position_tape(node, variant_idx, support_mod,
  grammar_suffix, ir) -> TokenStream`
- `emit_inline_position_visitor(node, support_mod, grammar_suffix, ir)
  -> TokenStream`

Per-node emission:

- `Alt(branches, _)` → `TapeKind::Alt` compound + byte-dispatch over
  branch first-byte sets, linear-retry fallback, porting
  `alt_dispatch.rs`'s branch-body logic wrapped in outer Alt compound
  push/close.
- `Regex(sid)` → grammar regex-scan adapter + `TapeKind::Span` leaf.
- `Negate(inner)` → guard-only rewind-on-success, no tape record.
- `Minus(primary, excluded)` → probe excluded; fail on success;
  otherwise emit primary.
- `TokenDispatch { token, arms, fallback }` → TokenDispatch compound
  + token parse + per-arm pattern-byte equality + continuation,
  mirroring the VM interpreter's `TokenDispatchArm.patterns`
  semantics.

Shared helpers (`branch_first_bytes`, `unwrap_trivia`, `flatten`,
`emit_seq_position`) mirror `alt_dispatch.rs` verbatim.

The module compiles clean. `cargo check --workspace` exits 0 after
`cargo clean -p bbnf-analysis` (nightly ICE unrelated to this
tranche).

## What did NOT land

1. **Position-core wiring in `flat.rs` / `arglist.rs` / `wrap.rs` /
   `unordered.rs` / `pratt.rs` / `array.rs` / `object.rs` /
   `scalar.rs`.** Attempted + reverted — see §Scope-reveal below.
2. **`body_has_dispatcher_fallback_position` deletion from
   `shapes/mod.rs`.** Deferred — its removal requires the inline
   helpers consumed first.
3. **`gate_predicate_wire_contract.rs` widening for 6 non-JSON
   grammars.** Deferred — until helpers are consumed, the predicate
   must remain false for those grammars or parse() would loop.
4. **Bootstrap regen idempotency.** Not achieved — regen with widened
   predicate produced a truncated 23-line stub on the second cycle,
   indicating the inline-emission path fails
   `BbnfBootstrap::parse(bbnf.bbnf)`, the self-hosted loop entry point.

## Scope-reveal

### 1. Compile-time explosion

`cargo test -p bbnf --test tape_parity --no-run` with all 5
Parser-derive sites (JSON / CSS L4 / BBNF / Sheets / EBNF) consuming
the inline helpers peaked at **26 GB RSS for a single rustc process**
(monitored over 3m+; killed pre-OOM with 4069 free pages = 65 MB
remaining). The tape_parity binary links all five derive expansions
into one translation unit with `debuginfo=2`; each CSS L4 `*Decl`
Flat rule (28 of them) carries 1-3 inline Alt positions (e.g.
`fontDecl = … , ((fontWeightKeyword | value) ?w) * …`), each of
which expands to a `match first { … }` with per-branch attempt
blocks. Cumulative token-stream size pushed LLVM codegen into
pathological growth.

Single-grammar test binaries (e.g. `cargo test -p bbnf --test css_l4
--no-run`) compile in ~11 s with no memory pressure — the issue is
specific to multi-grammar aggregate test binaries.

### 2. Self-hosted bootstrap failure

After wiring + predicate widening + regen, the bootstrap script's
second regen cycle emitted a 23-line stub instead of the full
96 434-line `generated.rs`. Per README §Self-host circular-dependency
escape, this is the canonical symptom: the post-rewrite
`BbnfBootstrap::parse` fails on `bbnf.bbnf` before the emitter can
regenerate the correct table. My inline emission for BBNF's Flat /
Unordered / Pratt rules is semantically incorrect against the
walker's tape — the walker parses the file successfully, shape
dispatch does not.

### 3. Gorgeous consumers also fail

`cargo test -p bbnf --test bbnf_parity` revealed `gorgeous`'s
derive-Parser expansions for JSON / BBNF / EBNF / BNF / CSS pretty /
Google Sheets all fail with `import error: Parse error in
<path>.bbnf: Failed to parse grammar`. Same root cause — the
widened-admission BbnfBootstrap can't parse grammar files.

## Root-cause analysis

The inline helpers' tape emission is not walker-identical for
structural positions where the walker consults its frame-stack /
`advance_or_pop_with` / `try_branch` / dual-cursor machinery:

- Walker's Alt arm pushes a frame with `parent_rec`, `child_mark`,
  `variant_idx`, configures savepoints (`start_depth`, `start_pos`,
  `start_slot`, `psi_len`, `pay_agg_len`, `pending_variant_idx`),
  then enters `try_branch` per branch; on failure it `truncate`s
  columns / `psi.truncate` / `stack.restore(sp_after_push)` /
  restores `pending_variant_idx`.
- My inline Alt emission pushes `Alt` compound, does linear branch
  attempts with *p rollback only, closes the compound. No PSI
  truncation. No frame-stack state. No `pending_variant_idx`
  management.

For grammars where Alt branches parse via Ref calls to classified
targets (the common case), the branch-call itself handles its own
PSI / stack frames, but the Alt-compound-level variant_idx
discrimination and the per-branch child stamping don't match walker
byte-for-byte.

## Suggested re-plan

The orchestrator should open a follow-on sub-wave (W0a.2.e or split
into multiple agents on disjoint file bounds):

1. **Agent E1 — walker-fidelity audit.** Compare inline helper
   emission against walker arms position-by-position; identify every
   divergence (variant_idx sources, PSI interaction, compound
   bracketing). Write the parity-gap to
   `docs/benchmarks/post-AX-W0a2e-parity-gaps.md`.
2. **Agent E2 — compile-cost analysis.** `cargo expand` on a single
   CSS L4 `*Decl` rule post-inline emission; measure token-stream
   size growth factor. Identify any emission pattern where a
   per-position body expands super-linearly (e.g. a Minus containing
   an Alt containing a Minus, if such nests exist in grammars).
3. **Agent E3 — incremental rollout.** Rather than widening the
   predicate for all 6 non-JSON grammars in one wave, split by
   grammar: land BBNF first (smallest; self-hosted so the bootstrap
   loop forces correctness), then Sheets, then CSS L4. Each grammar's
   widening is its own commit + regen cycle.

## Artefacts

- Commit `8bf331e8` — inline.rs substrate.
- `/tmp/ax-w0a2d-check5.txt` — clean `cargo check` after wiring all
  emitters (pre-revert).
- `/tmp/ax-w0a2d-gate2.txt` — `gate_predicate_wire_contract.rs` post-
  widening: all 7 tests pass with 6 non-JSON predicates flipping
  false → true.
- `/tmp/ax-w0a2d-bbnf-parity.txt` — `cargo test -p bbnf --test
  bbnf_parity` compile failures post-regen: `import error: Parse
  error in <grammar>.bbnf: Failed to parse grammar` for 6 grammars.

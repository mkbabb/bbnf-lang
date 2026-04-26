# Tranche B5 — Agent Dispatch Templates

This document carries one concrete dispatch template per wave.
Each populates the standard tranche dispatch form: meta block,
context, scope, allow-list, forbidden-list, hard-gate items,
verification artefacts, and return format. The orchestrator
issues exactly one dispatch per wave, in order, opening W<N+1>
only after W<N>'s hard gate passes.

Every dispatch carries the directive in full: **no quick
solutions, no workarounds; idiomatic, gestalt approaches;
architectural transpositions for elegance / simplicity /
performance are mandatory; no legacy code; no
backward-compatibility shims; no deferrals.**

## W0 dispatch — Test-debt closure

**You are the B5.W0 sub-agent.** Read `docs/tranches/B5/waves/W0.md`
in full; that spec is the authoritative source for scope. This
dispatch repeats only what the agent must hold before reading.

### Meta

- **Wall budget**: 90 min. **HARD CAP: 90 min.** At 81 min commit
  whatever is done; at 90 min halt.
- **Worktree**: orchestrator-created; isolated branch; commit
  there; report back.
- **LOC budget**: ~80 LOC across ~6 source files plus a per-grammar
  regen sweep at close.
- **Dependencies**: tranche open (no prior wave gate).

### Scope (per W0.md)

Closes the four β clusters identified by the pre-tranche audit:

- **Cluster A** (6 failures): hoist `unwrap_structural_wrappers`
  into a shared `crates/core/src/backend/rust/view/peel.rs`
  module; replace `peel_body` callsite. ~5 LOC.
- **Cluster B** (1 failure): `MaterializerKind::SpanFromFrame`
  arm; remove `TypeDesc::Span` from `is_scalar_payload()`; stop
  synthesising `[U32, U32]` for non-Tuple kernel admissions.
  ~30-50 LOC + per-grammar regen sweep.
- **Cluster C** (2 failures): `ParserAttributes::with_paths(paths)`
  constructor; delete the runtime assert. ~15 LOC.
- **Cluster D** (1 failure): convert recursive `walk()` to
  iterative `Vec<TapeCursor>` worklist. ~15 LOC.
- **Bonus**: `payload_bytes` PAYLOAD_IN_ARENA_BIT precondition.
  ~5 LOC.

### Allow-list (files the agent may modify)

- `crates/core/src/backend/rust/view/peel.rs` (create)
- `crates/core/src/backend/rust/view/value.rs`
- `crates/core/src/backend/rust/view/named_types.rs`
- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
- `crates/ir/src/types/type_desc.rs`
- `crates/core/src/backend/rust/ir_enums.rs`
- `crates/core/src/backend/rust/ir_types.rs`
- `crates/core/tests/pipeline_compile_request.rs`
- `crates/core/tests/sheets_parity.rs`
- `crates/tape/src/tape.rs`
- `crates/core/src/grammar/generated/{bbnf,json,css_l4,css_pretty,csv,ebnf,bnf,google_sheets,math}.rs` (regen via `cargo xtask regen`, not hand-edits)

### Forbidden-list

- No edits to `crates/tape/src/builder/` (W1's scope).
- No edits to `crates/core/src/lib.rs` (W1's scope).
- No `#[allow(...)]` introductions of any kind.
- No legacy code, no backward-compat shim, no deferral.
- No band-aid that closes a single test without addressing the
  cluster's root cause.

### Hard gate

1. Workspace nextest 1490/1490 (full green).
2. `cargo xtask regen --check` exit 0 across all 9 grammars.
3. `cargo iter-check-full` exit 0 < 1 s warm.
4. The 10 specific test names listed in W0.md all green.
5. `payload_bytes` carries the PAYLOAD_IN_ARENA_BIT precondition.

### Verification artefacts (return in agent report)

- Commit SHAs in order with one-line descriptions.
- nextest output line count and pass/fail summary.
- `cargo xtask regen --check` output (head -5).
- `cargo iter-check-full` warm time.
- Per-cluster confirmation that the named tests pass.

### Return format

≤ 400 words. Commit SHAs, files modified, hard-gate verification,
any deviation from the W0 spec (with rationale; do not silently
re-scope).

---

## W1 dispatch — Substrate boundary restoration (the gestalt move)

**You are the B5.W1 sub-agent.** Read `docs/tranches/B5/waves/W1.md`
in full. This is the gestalt move; the wave spec is detailed.

### Meta

- **Wall budget**: 3-4 hours. **HARD CAP: 240 min.** At 216 min
  commit whatever is done; at 240 min halt.
- **Worktree**: orchestrator-created; isolated.
- **LOC churn**: ~3000 (mostly generated; net source negative).
- **Dependencies**: W0 closed at gate. **W0 must be green at this
  dispatch's start** — verify before beginning.

### Scope (per W1.md)

The five-part transposition:

1. Promote `value_frames`, `value_payloads_narrow`,
   `value_payloads_wide`, `value_open_stack` into `Columns`.
2. Delete `crates/tape/src/builder/{mod,output,value}.rs`.
3. Restore `Parsed<'p, R>` to 3-field record at
   `crates/core/src/runtime/parsed.rs`.
4. Remove `extern crate self as bbnf;` from `crates/core/src/lib.rs:6`.
5. Collapse rollback triplication; introduce `Tape::position() -> u32`;
   regen sweep retires 1061 `columns_mut().len()` callsites.

### Allow-list

Per W1.md "Files (allow-list)" section. Major surface:
`crates/tape/src/{builder/,columns.rs,tape.rs,lib.rs}`,
`crates/core/src/{runtime/parsed.rs,lib.rs,backend/rust/emitter/...}`,
plus the regen sweep across all 9 generated grammars.

### Forbidden-list

- No edits to `crates/core/src/lower/expression.rs`,
  `backend/rust/emitter/shapes/{dispatcher,inline,flat,pratt}.rs`,
  or `crates/tape/src/columns.rs`'s SIMD module (W3's scope).
- No edits to W2's scope (`packed_cache`, `pay_wide`/`pay_f64`
  merge, `leftmost_descendant_offset`, `_frame_depth`).
- No edits to W4's scope (cousin-leak guard, Pratt
  `child_off`).
- **No alias retention.** The `frame`/`value_frame_at` pairs
  collapse fully; no transitional alias.
- No `#[allow(...)]`, no legacy shim, no `pub use` re-exports
  to mask a rename.

### Hard gate

1. All W0 gates non-regressing.
2. Workspace nextest 1490/1490.
3. `cargo xtask regen --check` exit 0 across all 9 grammars.
4. `cargo iter-check-full` exit 0 < 1 s warm.
5. `cargo bench -p bbnf --bench compile_pipeline -- compile_bbnf`
   median within 5% of B4 baseline (2.831 ms).
6. `rg -nF 'FusedBuilder|FusedOutput|ValueFramesOutput|columns_mut|frame_depth_mut|extern crate self as bbnf' --type rust crates/`
   returns zero matches outside `Columns` itself.

### Return format

≤ 400 words. Commit SHAs, transposition summary, gate verification,
bench median + delta vs 2.831 ms, deviations.

---

## W2 dispatch — Bookkeeping consolidation + redundant-column cleanup

**You are the B5.W2 sub-agent.** Read `docs/tranches/B5/waves/W2.md`.

### Meta

- **Wall budget**: 90 min. **HARD CAP: 90 min.**
- **LOC**: ~250 net-negative.
- **Dependencies**: W1 closed at gate.

### Scope (per W2.md)

- `ValueCheckpoint::first_child` captured at `begin_compound`;
  `end_compound` O(N) forward scan deleted; `_frame_depth` dead
  parameter removed.
- Compound-row depth stamp deferred to close;
  `leftmost_descendant_offset` post-order bump cascade deleted.
- `packed_cache: OnceLock<Vec<PackedRecord>>` deleted from
  `Columns`.
- `pay_wide` and `pay_f64` columns merged via unified `pay_wide`;
  `PAYLOAD_F64_DIRECT_BIT` deleted if no longer load-bearing.

### Allow-list

`crates/tape/src/{columns.rs,builder.rs,finaliser.rs}`,
`crates/core/src/backend/rust/emitter/` callsites that pass
`_frame_depth`, `crates/core/src/grammar/generated/*.rs` (regen).

### Forbidden-list

- No edits to W3's scope (god-module splits, SIMD extraction).
- No edits to W4's scope.
- No alias retention.

### Hard gate

1. All W1 gates non-regressing.
2. `packed_cache: OnceLock` no longer appears in `Columns`.
3. `pay_wide` and `pay_f64` are the same column.
4. `leftmost_descendant_offset` post-order bump cascade deleted.
5. `_frame_depth: u8` no longer appears at any callsite.

---

## W3 dispatch — Module decomposition + simd extraction

**You are the B5.W3 sub-agent.** Read `docs/tranches/B5/waves/W3.md`.

### Meta

- **Wall budget**: 120 min. **HARD CAP: 120 min.**
- **Dependencies**: W2 closed at gate.
- **Behaviour change**: none. Pure refactor.

### Scope (per W3.md)

- `crates/core/src/lower/expression.rs` (2140 LOC) →
  `lower/expression/{alt,repeat,pratt,wrap,closures,mod}.rs`.
- `backend/rust/emitter/shapes/dispatcher.rs` (1969 LOC) →
  `dispatcher/{cross_shape,symbol_composition,support,mod}.rs`.
- `inline.rs` (1687 LOC) and `flat.rs` (1342 LOC) per-concern
  directory splits.
- `crates/tape/src/columns.rs` SIMD module hoisted to
  `crates/tape/src/simd/`.
- Audit `schema/emit/rust.rs` (~1286 LOC) and
  `backend/rust/emitter/grammar.rs` (1286 LOC); split iff natural
  boundary < 800 LOC after.
- Rename or fold any `helpers.rs`/`utils.rs` kitchen sinks under
  `crates/core/src/backend/{wasm,ts}/` and
  `crates/ir/src/passes/{types,constraint}/`.

### Allow-list

The four target directories and their imports.

### Forbidden-list

- **No behaviour change.** Pure mechanical split. If a function's
  behaviour shifts, the split is wrong.
- No `helpers.rs`/`utils.rs`/`common.rs` modules in any new
  split.
- No file in `crates/core/src/lower/`,
  `crates/core/src/backend/rust/emitter/shapes/`, or
  `crates/tape/src/` exceeds 800 LOC after the split.

### Hard gate

1. All W2 gates non-regressing.
2. Line-count audit: no file in target directories > 800 LOC.
3. No kitchen-sink module name in target directories.
4. SIMD module lives at `crates/tape/src/simd/`.

---

## W4 dispatch — Cousin-leak migration + Pratt cleanup

**You are the B5.W4 sub-agent.** Read `docs/tranches/B5/waves/W4.md`.

### Meta

- **Wall budget**: 60 min. **HARD CAP: 60 min.**
- **LOC**: ~50.
- **Dependencies**: W3 closed at gate.

### Scope (per W4.md)

- Cousin-leak guard moved from
  `crates/core/src/lower/expression/...` and
  `crates/core/src/lower/value_expr.rs:351-358` into
  `crates/tape/src/cursor.rs::ChildIter`. Duplicates deleted.
- Pratt outer's `child_off` post-call surgery → introduce
  `PRATT_CHILD_OFF_OVERRIDE` flag passed to `begin_compound`;
  honour at `end_compound`-time. Post-call
  `columns_mut().set_child_off_at` deleted (W1 retired
  `columns_mut`; this preserves Pratt-specific semantics in the
  close path).

### Allow-list

`crates/core/src/lower/`, `crates/tape/src/cursor.rs`,
`crates/tape/src/finaliser.rs`,
`crates/core/src/backend/rust/emitter/shapes/pratt.rs`.

### Forbidden-list

- No alias retention.
- No "compatibility wrapper" around the migrated guard.

### Hard gate

1. All W3 gates non-regressing.
2. Cousin-leak guard appears once in `cursor.rs::ChildIter`.
3. `crates/core/src/lower/expression/` directory has no
   `body_hi` filter callsites.
4. `pratt.rs` no longer reaches into `Columns` directly.

---

## W5 dispatch — FINAL + cross-tranche updates

**You are the B5.W5 sub-agent.** Read `docs/tranches/B5/waves/W5.md`.

### Meta

- **Wall budget**: 60 min. **HARD CAP: 60 min.**
- **Dependencies**: W4 closed at gate.

### Scope (per W5.md)

- Author `docs/tranches/B5/FINAL.md` (standalone prose; no
  metalanguage; per `feedback_no_metalanguage_docs`).
- Author `docs/benchmarks/post-B5.json` (close-matrix benchmarks).
- Update `docs/tranches/REMAINING-TRAJECTORY.md`: B5 row
  complete; AY-II.W1 next.
- Update `docs/RISK-PERF-MATRIX.md`: post-B5 probabilities.
- Update `AY-II/AY-II.md`, `AZ-I/AZ-I.md`, `AZ-II/AZ-II.md`,
  `BA/BA.md`, `BB/BB.md` for `FusedBuilder` →
  `Columns` and `Parsed::value_frames_output` →
  `Parsed::frames` API renames.

### Allow-list

`docs/tranches/B5/FINAL.md`, `docs/benchmarks/post-B5.json`,
`docs/tranches/{REMAINING-TRAJECTORY.md,RISK-PERF-MATRIX.md}`,
`docs/tranches/{AY-II,AZ-I,AZ-II,BA,BB}/`.

### Forbidden-list

- No source-code edits.
- No new tranche docs (B5 is terminal cleanup).
- No metalanguage in FINAL.md (no "this plan", "the plan
  above", "see conversation"; standalone authoritative prose).

### Hard gate

1. All W4 gates non-regressing.
2. `docs/tranches/B5/FINAL.md` lands.
3. `docs/benchmarks/post-B5.json` lands with full close-matrix.
4. `rg -nF 'FusedBuilder|columns_mut|value_frames_output' docs/tranches/`
   returns zero matches in plan docs (find/replaced) outside
   FINAL's archaeology section.

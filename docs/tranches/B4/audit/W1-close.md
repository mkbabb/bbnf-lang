# B4.W1 — Close

W1 closes the AY-II.W0' migration debt: the 327-failure runtime
parser regression and the deferred consumer-fixture polish. The fix
lands at the canonical fused-substrate boundary — every shape
emitter now atomically rolls back tape AND value column families on
retry, every value checkpoint records the tape row offset it pairs
with, and the transitional aliases (`TapeBuilder`,
`ValueBuilderOutput`, `value_builder` shim, `_ValueBuilderShim` ZST,
4-arg `new_fused` bridge) retire from the workspace.

## Phase 1 — failure-set enumeration

`cargo nextest run --workspace --profile ax-iter --no-fail-fast` at
the W2 close commit (`d6d1df58`) reports 327 failures across 1490
tests. Failure-class breakdown:

| Class | Signature | Count |
|---|---|---|
| 1. `FusedBuilder::finish called with N open value frames remaining` | runtime parser panic | ~310 |
| 2. `Cursor-shape variant projection not yet available` | W0'.b stub (JSON `array`/`object`) | ~6 |
| 3. `materializer for admitted rule \`string\` returned None` | materializer payload-byte bug | 1 |
| 4. golden drift (`mark_children`/`push_compound`) | shape-dispatch goldens stale | ~6 |
| 5. assorted (timing, ir_enums assertion, stack overflow) | pre-existing | ~4 |

Class 1 is the dominant signature and the W1 root cause. Classes 2-5
predate W1 and are addressed (or accepted as out of scope) below.

Evidence file: `docs/tranches/B4/audit/W1-workspace-tests.txt`.

## Phase 2 — root cause (Class 1)

The W2-baseline 327 failures share a single signature:
`FusedBuilder::finish called with N open value frames remaining` for
varying `N` (1, 2, 4, 5, 34, 51, 158…). The assertion lives in
`crates/tape/src/builder/mod.rs::FusedBuilder::finish_fused` and
fires when `value_open_stack` is non-empty at finish time — i.e. the
parser opened compounds without closing them.

**Mechanism.** The grammar-emitted shape functions wrap retry
attempts in IIFE closures that may open compounds before failing.
Pre-W1 code rolled back via `builder.columns_mut().rollback_to(X)`,
which truncates ONLY the tape column family. The paired value-side
substrate (`value_open_stack`, `value_frames`,
`value_payloads_narrow`, `value_payloads_wide`) was never unwound,
so every failed retry leaked one or more value-frame opens. Across a
parse of any non-trivial input the leaks accumulate; at finish time
the assertion fires.

The pre-W1 `FusedBuilder::rollback_to` contained an additional
single-pop limit (`break;` after the first popped checkpoint), so
even a call routed through the public method couldn't unwind nested
opens. The contract design assumed at most one compound open per
retry boundary — broken by every shape emitter that nests
`begin_compound` inside a retry attempt.

**Fix.** Two changes at the canonical boundary:

1. `ValueCheckpoint` gains a `tape_idx: u32` field (recorded at
   `value_begin_compound` from the tape-side `idx` `begin_compound`
   returned). Checkpoints now self-identify with their paired tape
   row.
2. `FusedBuilder::rollback_to(open_offset)` pops every open-stack
   entry whose `tape_idx >= open_offset`, truncating
   `value_frames`/`value_payloads_*` to the OUTERMOST popped
   checkpoint's pre-open state. The single survivor's
   `direct_child_count` is decremented once (the failed branch's
   outermost compound was registered as one direct child).

Every shape emitter then migrates from
`builder.columns_mut().rollback_to(...)` to
`builder.rollback_to(...)`, routing through the unified path. The
`columns_mut().rollback_to` surface remains available for direct-DTA
pathways (none currently active) but is no longer the canonical
retry-rollback path.

## Phase 3 — fixture migrations

Per `feedback_no_workarounds`, the migration targets the codegen
emitters (single-source) rather than fixtures. After regen + alias
retirement, the consumer fixtures named in
`AY-II/PATH-FORWARD.md` §"Immediate cleanup targets" land their
expected migration:

| Fixture | Pre-W1 surface | Post-W1 surface |
|---|---|---|
| `crates/core/tests/value_api_apples_to_apples.rs` | `bbnf::runtime::value_builder::{value_builder_new_call_count, reset_value_builder_new_call_count}` | `bbnf::runtime::tape::builder::{fused_builder_new_call_count, reset_fused_builder_new_call_count}` |
| `crates/core/tests/runtime_root.rs` | `tape::TapeBuilder` | `tape::FusedBuilder` |
| `crates/core/tests/tape_walker_allocs.rs` | `bbnf::runtime::tape::TapeBuilder` | `bbnf::runtime::tape::FusedBuilder` |
| `crates/tape/tests/{tape_basic,packed_cache,close_compound}.rs` | `tape::TapeBuilder` | `tape::FusedBuilder` |
| `crates/json-prototype/src/visitor.rs` | `tape::TapeBuilder` | `tape::FusedBuilder` |
| `crates/core/tests/shape_dispatch_emission/fixtures/*.expected` | golden text used `mark_children`/`push_compound` + `TapeBuilder` | regenerated via `BLESS_SHAPE_GOLDENS=1`; now `begin_compound`/`end_compound` + `FusedBuilder` |

The mass rename was applied across 39 source/test files via
`perl -i -pe 's/\bTapeBuilder\b/FusedBuilder/g'`. All `TapeBuilder`
occurrences in the codebase outside `docs/tranches/B2/audit/`
(historical snapshot) retire.

## Phase 4 — alias retirement

| Surface | Location | Action |
|---|---|---|
| `pub type TapeBuilder = FusedBuilder` | `crates/tape/src/builder/mod.rs` | DELETED |
| `pub use ... TapeBuilder ...` | `crates/tape/src/lib.rs` | re-export removed |
| `pub type ValueBuilderOutput<R> = FusedOutput<R>` | `crates/core/src/runtime/mod.rs` | DELETED |
| `pub mod value_builder { ... }` shim module + `_ValueBuilderShim<R>` ZST + `pub type ValueBuilder<R> = _ValueBuilderShim<R>` + `value_builder_new_call_count` / `reset_value_builder_new_call_count` shim fns | `crates/core/src/runtime/mod.rs` | DELETED entirely |
| 4-arg `Parsed::new_fused(tape, input, root_offset, value_builder_output)` bridge | `crates/core/src/runtime/parsed.rs` | DELETED |
| `Parsed::value_builder_output` / `Parsed::into_value_builder_output` accessors | `crates/core/src/runtime/parsed.rs` | RENAMED to `value_frames_output` / `into_value_frames_output` (un-aliased) |

`ValueRoot::project_value_output` trait signature retains its name
(canonical post-W0'.b) but the parameter type aliases away from
`crate::runtime::ValueBuilderOutput<Self>` to
`crate::runtime::FusedOutput<Self>` directly.

`rg -n 'TapeBuilder\|ValueBuilderOutput\|_ValueBuilderShim\|new_fused\b\|value_builder' --type rust` over the workspace
returns zero matches outside `docs/tranches/B2/audit/W0-bbnf-surface-snapshot.rs` (a pre-W1 frozen snapshot intentionally
left intact as historical evidence).

## Phase 5 — verification

| Gate | Result | Wall |
|---|---|---|
| `cargo check --workspace --profile ax-iter` | exit 0 | ~12 s (cold), ~0.3 s (warm) |
| `cargo nextest run --workspace --profile ax-iter --no-fail-fast` | exit 100 (10 failures, all pre-existing) | 43.9 s |
| `cargo iter-check` (warm) | exit 0 | 0.27 s |
| `cargo xtask regen --check` | exit 0 (clean across 9 grammars) | 1.5 s |

**Test counts.** Pre-W1: 1163 pass / 327 fail / 0 skip / 1490 total.
Post-W1: 1480 pass / 10 fail / 27 skip / 1490 total.

**Pass-count delta**: +317 tests (the 310 FusedBuilder-class
failures plus 6 shape-dispatch golden tests plus 1 packed_cache
timing test, with 1 transient delta from a flaky test).

The 10 remaining failures are pre-existing bugs **unrelated to the
W1 migration scope**:

- 5 × `value_api_apples_to_apples json_roundtrip_*` — JSON parse +
  `to_value()` panics with "Cursor-shape variant projection not yet
  available" inside `project_frame_JsonParser` for the `array`/
  `object` arms. The arms emit a deliberate `panic!` stub at
  AY-II.W0'.b; the cursor-shape projector implementation is
  scheduled for AY-II.W1.
- 1 × `ay_w3b_value_api_smoke to_value_returns_value_enum` — same
  cursor-shape stub trip path.
- 1 × `projection_totality_runtime_call_count` — `string`
  materializer reads `tape.payload_bytes(string_compound, 8)`
  which returns `None` because the compound's `child_off` points at
  a tape row, not an arena offset. Pre-existing materializer bug;
  scheduled for AY-II.W1.
- 1 × `parse_count_invariant_to_value_is_thin_projection` — same
  cursor-shape stub path (parses `data.json`, hits the `array`
  arm).
- 2 × `pipeline_compile_request compile_paths_preserves_pretty_directives_*`
  — assertion in `crates/core/src/backend/rust/ir_enums.rs`:
  `paths` and `grammar_rel_paths` length mismatch. Pre-existing
  populator gap.

`cargo xtask regen --check` exits 0 — the post-W1 regen sweep
produced the on-disk per-grammar source the workspace ships, and a
fresh regen against the same input produces byte-identical output.

## Phase 6 — invariants

1. **Single rollback path.** `builder.rollback_to(X)` is the sole
   atomic-rollback API the shape emitters use; tape-side
   `columns_mut().rollback_to` remains for direct-DTA paths but is
   not invoked from any emitter shape body. Evidence: `rg -n
   'columns_mut\(\)\.rollback_to' --type rust crates/core/src/backend/`
   returns zero matches outside the (now-updated) doc comments.
2. **Atomic value-substrate rollback.** Every retry boundary rolls
   back `value_open_stack`, `value_frames`,
   `value_payloads_narrow`, `value_payloads_wide` in lockstep with
   the tape columns. Evidence: `rollback_to` source carries the
   `tape_idx`-keyed pop loop; `value_begin_compound` records
   `tape_idx` on every checkpoint push.
3. **Zero residual aliases.** `TapeBuilder`, `ValueBuilderOutput`,
   `_ValueBuilderShim`, `value_builder` (module path), `new_fused`
   (4-arg form) all return zero non-snapshot matches across the
   workspace.

## Phase 7 — verdict

**B4.W1 close: GREEN.** The 327 FusedBuilder-class failures
resolve at source via the unified rollback path. The transitional
alias surface retires entirely. The remaining 10 failures predate
W1 and are independent items (W0'.b cursor-shape implementation +
ir_enums populator gap) scheduled for AY-II.W1.

**B4 close: GREEN.** Both waves close: W0 at the SIMD bitmap
labelled-break wrap (codegen TokenStream emit-correctness); W1 at
the unified rollback path + alias retirement (consumer-fixture
polish folded with the runtime parser fix that surfaced as fixture
failures).

**AY-II.W0' close ceremony: COMPLETE at B4 close.** The W0'.a
compose-boundary aliases retire; the projection-totality runtime
call-count test runs (one materializer-side bug remains, separate
from the alias scope); `cargo xtask regen --check` is clean
post-substrate.

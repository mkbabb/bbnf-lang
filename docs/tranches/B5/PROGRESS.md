# Tranche B5 — Progress Log

Operational protocol: see `/INSTRUCTIONS.md` at repo root, and the
parent index at `B5.md`.

## Tranche scope (planned 2026-04-26)

Six waves. Net negative LOC. Architectural-cleanup tranche
synthesised from a four-agent gestalt audit (α B-fix purity, β
test-debt root-cause, γ downstream-tranche readiness, δ system
aesthetic) of the post-B4 substrate. Closes 10 remaining workspace
test failures and retires five architectural smells before AY-II.W1
dispatches.

## Wave status

| Wave | Headline | Spec | Status | Date opened | Date closed |
|------|----------|------|--------|-------------|-------------|
| W0 | Test-debt closure (4 β clusters + bonus) | [waves/W0.md](waves/W0.md) | complete | 2026-04-26 | 2026-04-26 |
| W1 | Substrate boundary restoration (the gestalt move) | [waves/W1.md](waves/W1.md) | complete | 2026-04-26 | 2026-04-26 |
| W2 | Bookkeeping consolidation + redundant-column cleanup (parts 3+4 only; parts 1+2 routed via W2b audit to W6) | [waves/W2.md](waves/W2.md) | complete (partial; balance routes to W6) | 2026-04-26 | 2026-04-26 |
| W3 | Module decomposition + simd extraction | [waves/W3.md](waves/W3.md) | complete | 2026-04-26 | 2026-04-26 |
| W3b | Extended decomposition (7 additional file splits) | (no spec; emergent from W3 surface) | complete | 2026-04-26 | 2026-04-26 |
| W4 | Cousin-leak migration + Pratt `child_off` cleanup | [waves/W4.md](waves/W4.md) | complete | 2026-04-26 | 2026-04-26 |
| W6 | Depth-stamp invariant inversion (substrate transposition) — phase A | (W2b audit-driven; replaces W2 parts 1+2) | complete | 2026-04-26 | 2026-04-26 |
| W6b | Bracket discipline activation + IIFE `?`-leak isolation — phase B | (W6 phase B; activates bracket discipline) | complete | 2026-04-26 | 2026-04-26 |
| W5 | FINAL + cross-tranche updates | [waves/W5.md](waves/W5.md) | complete | 2026-04-26 | 2026-04-26 |

## Pre-tranche audit (2026-04-26)

Master HEAD `bb52da85` closed B2 + B3 + B4. Workspace nextest at
1480/1490 pass. The four-agent gestalt audit produced:

### α — B-fix purity (W2 + W4 scope)

- Three rollback primitives (`FusedBuilder::rollback_to`,
  `Columns::rollback_to`, `Columns::truncate`) each documented as
  "the canonical" with carve-outs.
- Cousin-leak guard duplicated across
  `crates/core/src/lower/expression.rs:561-574` and
  `crates/core/src/lower/value_expr.rs:351-358`.
- Pratt outer's `child_off` post-call surgery at
  `crates/core/src/backend/rust/emitter/shapes/pratt.rs:548-551`.
- `end_compound` carries an O(N) forward scan computing
  `first_child` that `begin_compound` could capture once.
- `leftmost_descendant_offset` post-order bump cascade — each
  `end_compound` walks the post-order chain bumping a depth-stamp
  field instead of deferring stamp until close.
- `_frame_depth: u8` parameter passed through emitter call chains
  but never read.

### β — test-debt root-cause (W0 scope)

The 10 remaining failures cluster into 4 root-cause clusters:

| Cluster | Failures | Symptom | Root cause |
|---------|----------|---------|------------|
| A | 6 (`value_api_apples_to_apples json_roundtrip_*`, `ay_w3b_value_api_smoke`, `parse_count_invariant`) | `VariantShape::Cursor` panic | `peel_body` only handles Map/OptionalWhitespace; JSON array/object peel to Skip/Next, fall through to panic stub. Sibling `unwrap_structural_wrappers` in `named_types.rs` already handles correctly. Hoist to `peel.rs`; replace callsites. |
| B | 1 (`projection_totality projection_totality_runtime_call_count`) | Outer materializer panic on `tape.payload_bytes(rec, 8)` returning None | `named_types` synthesises `[U32, U32]` for any single-leaf regex/literal admission. JSON `string -> ... : String` admits as 2×u32 (`total_bytes=8`). Runtime emits via `parse_string_escaped` writing different layout. Layout mismatch. Architectural fix per `feedback_pluggable_components`: introduce `MaterializerKind::SpanFromFrame`; remove `TypeDesc::Span` from `is_scalar_payload()`; stop synthesising `[U32, U32]` for non-Tuple kernel admissions. |
| C | 2 (`pipeline_compile_request compile_paths_preserves_pretty_directives_*`) | Runtime invariant violation | `ParserAttributes::default()` followed by partial `{ paths, ..Default::default() }` violates lock-step `paths`/`grammar_rel_paths` invariant. Constructor `with_paths(paths)` populates both atomically; runtime assert deleted (constructor invariant replaces). |
| D | 1 (`sheets_parity::child_iter_walks_complex_formula`) | SIGABRT (stack overflow) | Recursive `walk()` helper at `crates/core/tests/sheets_parity.rs:45-64` overflows on `=IF(A1>10, SUM(B1:B10), 0)`-class formulas. Convert to iterative `Vec<TapeCursor>` worklist. Pre-existing test-helper bug, unrelated to substrate. |

Bonus (latent silent-misread risk): `crates/tape/src/tape.rs:743-753`
`payload_bytes` doesn't check `PAYLOAD_IN_ARENA_BIT` precondition.
Defensive add (~5 LOC).

### γ — downstream-tranche readiness (W1 scope)

The post-B4 substrate carries five smells the synthesis enumerates
in `B5.md`'s architectural thesis. AY-II.W1, AZ-I, AZ-II, BA, BB
all reference `FusedBuilder` and `Parsed::value_frames_output` in
their plans; W5 sweeps cross-tranche references after the W1
transposition lands.

### δ — system aesthetic (W3 scope)

Six god modules exceed natural responsibility boundaries:

| File | LOC | Splits to |
|------|-----:|-----------|
| `crates/core/src/lower/expression.rs` | 2140 | `lower/expression/{alt,repeat,pratt,wrap,closures,mod}.rs` |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher.rs` | 1969 | `dispatcher/{cross_shape,symbol_composition,support,mod}.rs` |
| `crates/core/src/backend/rust/emitter/shapes/inline.rs` | 1687 | per-concern directory split |
| `crates/core/src/backend/rust/emitter/shapes/flat.rs` | 1342 | per-concern directory split |
| `crates/tape/src/columns.rs` | 1287 | `simd` extracted to `tape/src/simd/`; substrate fields land via W1 |
| `crates/core/src/backend/rust/emitter/grammar.rs` | 1286 | audit candidate; split iff natural-boundary line < 800 LOC after |

`feedback_no_god_modules` invariant: no `helpers.rs`/`utils.rs`/`common.rs`
kitchen sinks anywhere in the resulting splits.

## Method

Six single-agent waves dispatched in series. Each agent receives
the dispatch template at `AGENT_DISPATCH.md` populated for its
wave. Each carries an explicit allow-list, forbidden-list, and
hard-gate verification set. The orchestrator opens W<N+1> only
after W<N>'s hard gate passes; no wave-overlap. Generated files
land via `cargo xtask regen` at wave close, orchestrator-owned.

## What landed

### W0 — closed 2026-04-26

Ten pre-tranche workspace failures close at root cause across
four β clusters plus the latent `payload_bytes` precondition
bonus. Cluster A (six failures) collapses `peel_body` and
`unwrap_structural_wrappers` into `view/peel.rs`; the panic
stub at `VariantShape::Cursor` retires. Cluster B (one
failure) introduces `MaterializerKind::SpanFromFrame`, removes
`TypeDesc::Span` from `is_scalar_payload()`, and stops `[U32, U32]`
synthesis for non-`Tuple` kernel admissions. Cluster C (two
failures) replaces the runtime lock-step assert with
`ParserAttributes::with_paths(paths)` constructor invariant.
Cluster D (one failure) converts the recursive `walk()` helper
in `sheets_parity.rs` to an iterative `Vec<TapeCursor>`
worklist. Bonus: `payload_bytes` gates arena reads on
`PAYLOAD_IN_ARENA_BIT`. All ten named tests close green.

### W1 — closed 2026-04-26

The substrate transposition. Value-side fields promote into
`Columns`; `FusedBuilder` (~1258 LOC across three files)
deletes outright; `Parsed<'p, R>` returns to a 3-field record;
`extern crate self as bbnf;` retires; rollback triplication
collapses to one `Columns::rollback_to(open)`; `columns_mut()`
escape hatch retires in favour of `Tape::position() -> u32`.
Regen sweep updates the 1061+ CSS L4 callsites plus the cohort
across the other eight grammars in one pass. Bench gate:
`compile_bbnf` median holds within 5 % of B4 baseline 2.831 ms.

### W2 — closed 2026-04-26 (parts 3+4)

`packed_cache: OnceLock<Vec<PackedRecord>>` deletes outright
along with `invalidate_packed()` and every callsite. `pay_f64`
deletes; `f64` writes route through `pay_wide` via
`to_bits() as u64`; readers do `from_bits(self.pay_wide[idx])`.
W2 close commit at `0daf6f01`. Parts 1+2 (first-child capture
at `begin_compound`, depth-stamp deferral) route via the W2b
architectural diagnosis to W6.

### W2b — audit captured 2026-04-26 at `12f4265c`

Architectural diagnosis routes Parts 1+2 from W2's prescribed
`ValueCheckpoint::first_child_off` mechanism (which composes
badly with rollback's `direct_child_count` accounting) to a
substrate-level depth-stamp invariant inversion, captured at
`docs/tranches/B5/audit/W2b-architecture-diagnosis.md`. Opens
W6.

### W3 — closed 2026-04-26

Six god modules split along natural concern boundaries:
`expression.rs` (2140) → `expression/{alt,repeat,pratt,wrap,closures,mod}`;
`dispatcher.rs` (1969) → `dispatcher/{cross_shape,symbol_composition,support,mod}`;
`inline.rs` (1687) and `flat.rs` (1342) per IR-node concern;
`columns.rs` (1287) splits into directory form with SIMD
extracted to `tape/src/simd/`. Three kitchen-sink files
(`helpers.rs` / `utils.rs` / `common.rs`) retire by fold or
rename. Wave commits `357a0e43` → `c8a6b3e3`. Pure refactor
contract honoured.

### W3b — closed 2026-04-26

Seven additional files exceeding the 800-LOC budget, revealed
once W3's import surface cleared, split into directory modules:
`tape.rs`, `psi.rs`, `value_expr.rs`, `array.rs`,
`alt_dispatch.rs`, `wrap.rs`, `keyword.rs`, `pratt.rs`. Wave
commits `c4a53978` → `8e2da16e`. After W3+W3b combined,
`find ... -name '*.rs' -exec wc -l {} + | awk '$1 > 800'`
returns empty across the four target directories.

### W4 — closed 2026-04-26

Two bookkeeping patterns wrapping the substrate from outside
move to architecturally-correct sites. Cousin-leak guard
duplicated across `lower/expression/...` and
`lower/value_expr.rs` migrates into `cursor.rs::ChildIter`;
the iterator computes the do-not-cross boundary natively
(`b2cee7d7`). Pratt outer's post-call `set_child_off_at`
surgery retires in favour of `end_compound_with_child_off`
that rides the override through the substrate natively
(`4292550d`). Pratt grammar regen at `6f95f39a`.

### W6 — closed 2026-04-26 (phase A)

Depth-stamp invariant inversion. `Tape::enter_post_order_children() -> u32`
saves and bumps `current_depth` *before* the body emits;
`begin_compound_post` stamps the compound row at the saved
outer depth without bumping; `end_compound_post_order`
decrements once at close. Phase A lands the new primitives
as no-op transitions — the cascade still runs alongside the
bracket discipline. Wave commit `eeee1a5d`.

### W6b — closed 2026-04-26 (phase B)

Bracket discipline activation + IIFE `?`-leak isolation. W6b.1
IIFE-wraps post-order shape bodies so the `?` operator is
isolated from the surrounding push-state — a failed retry
inside an IIFE cleanly cannot leak the in-flight depth bump
past the matching `exit_post_order_children` (`614a516d`).
W6b.2 activates the bracket discipline: every post-order
shape emits an `enter_post_order_children` /
`exit_post_order_children` pair around its body; the cascade
in `end_compound_post_order` retires; the
`leftmost_descendant_offset` helper retires (`db92a576`).
Doc polish at `877736b6`. Single-writer invariant on
`frame_depth` holds post-W6b.

### W5 — closed 2026-04-26

`docs/tranches/B5/FINAL.md` authored; `docs/benchmarks/post-B5.json`
captured (compile_pipeline divan close-matrix; full peer-bench
deferred to AY-II.W1+); `REMAINING-TRAJECTORY.md` and
`RISK-PERF-MATRIX.md` refreshed; cross-tranche cross-reference
scrubs land across AY-II / AZ-I / AZ-II / BA / BB.

## Close gate verification

- Workspace nextest at W6b: 1477 passed, 0 failed, 27 skipped
  via `cargo nextest run --workspace --profile ax-iter
  --no-fail-fast`. Captured at `/tmp/b5w5-nextest.txt`.
- `compile_bbnf` median 2.806 ms via
  `cargo bench -p bbnf --bench compile_pipeline`; 0.9 % under
  B4 baseline 2.831 ms. Within the 5 % gate. Captured at
  `/tmp/b5w5-bench-compile-bbnf.txt`.
- `find crates/core/src/lower crates/core/src/backend/rust/emitter/shapes
  crates/tape/src -name '*.rs' -exec wc -l {} + | awk '$1 > 800'`
  returns empty.
- `rg -nF 'FusedBuilder|FusedOutput|ValueFramesOutput|columns_mut|frame_depth_mut|extern crate self as bbnf'
  crates/` returns only doc-comment archaeology references in
  the tape crate (no live source dependencies).
- `Parsed<'p, R>` is a 3-field record at
  `crates/core/src/runtime/parsed.rs`.
- `Tape::position(&self) -> u32` exists at
  `crates/tape/src/tape/construct.rs:70`.
- `crates/tape/src/builder/` directory does not exist.

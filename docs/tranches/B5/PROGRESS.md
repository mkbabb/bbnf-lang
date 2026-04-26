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
| W0 | Test-debt closure (4 β clusters + bonus) | [waves/W0.md](waves/W0.md) | planned | — | — |
| W1 | Substrate boundary restoration (the gestalt move) | [waves/W1.md](waves/W1.md) | planned | — | — |
| W2 | Bookkeeping consolidation + redundant-column cleanup | [waves/W2.md](waves/W2.md) | planned | — | — |
| W3 | Module decomposition + simd extraction | [waves/W3.md](waves/W3.md) | planned | — | — |
| W4 | Cousin-leak migration + Pratt `child_off` cleanup | [waves/W4.md](waves/W4.md) | planned | — | — |
| W5 | FINAL + cross-tranche updates | [waves/W5.md](waves/W5.md) | planned | — | — |

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

(populated as waves close)

### W0 — pending

### W1 — pending

### W2 — pending

### W3 — pending

### W4 — pending

### W5 — pending

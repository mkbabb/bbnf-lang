# SK-V7 Wave 4 Plan: Single-Quartet Unicode Escape Classifier

## Intervention

Single-quartet Unicode escape classifier.

W4 will add a small `parse-that-regex::unicode::escape_decode` helper that
decodes one JSON `\uXXXX` unit through the existing AArch64
`bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` primitive, with the
current scalar logic preserved as the non-AArch64 fallback. The same helper will
feed both the named `unescape_json_string` materializer fallback and the
existing per-unit validator read used by parse-only string recognition.

This is intentionally not the REDRESS 64 four-unit run validator. It does not
add a new intrinsic body, does not require four contiguous escapes, and does not
change generated parser control, sink hooks, decoded scratch, semantic string
facts, or byte-output materialization.

## Owner Paths

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/parse-that-regex/src/unicode/mod.rs`
- `skinny/crates/parse-that-regex/src/unicode/escape_decode.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` (reuse only; no new
  body expected)
- `skinny/crates/bbnf-simd/tests/checkasm_unicode_escape.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Source Shape

- Add `unicode::escape_decode::read_hex_unit_with_error_offset` as the single
  source for decoding one four-byte hex unit.
- On AArch64, call the existing `unescape_uxxxx_neon` single-quartet primitive.
- On other targets, retain the existing scalar nibble classifier behavior.
- Add `unicode::escape_decode::decode_json_unicode_escape` with the same public
  behavior and error offsets as the current `decode_json_unicode_escape`.
- Keep the public `decode_json_unicode_escape` symbol in `lib.rs` as a wrapper
  so API users and tests do not churn.
- Wire the `unescape_json_string` `Some(b'u')` fallback to the new helper after
  the existing dense x4 path returns `None`.
- Wire the existing parse validator's per-unit read through the same helper,
  without changing `validate_json_unicode_escape_run` control flow or batching.
- Add dedicated `checkasm_unicode_escape` coverage for all BMP codepoints,
  surrogate-pair boundaries, representative invalid hex, x4 invalid-lane
  reduction, and existing helper parity. This is test-only hardening around the
  existing SIMD body.

## Same-Wave Consumer Declaration

The same-wave consumers are the existing `unicode_escapes` and
`y_string_unicode` parse_only and direct_to_struct rows. Parse_only consumes the
classifier through the unchanged validator read path; direct_to_struct consumes
it through `unescape_json_string` when generated direct sinks materialize
escaped strings.

## Falsifiability Gate

Per `restart/skinny/tranches/sk-v7/SPEC.md §6`:

- `unicode_escapes` parse must reach at least 95% of same-run sonic-rs strict.
- `y_string_unicode` parse must reach at least 70% of same-run sonic-rs strict.
- Both named rows must also cross the same thresholds on direct_to_struct.
- No measured same-row Track 1 or Track 2 value regresses by at least 3%.
- `checkasm_unicode_escape` and `primitive-checkasm` must pass.

The gate is intentionally strict. If the classifier only moves direct rows, or
only moves `unicode_escapes`, W4 is rejected rather than widened into a
previously blocked materializer or validator family.

## Verification

- `cargo test -p parse-that-regex unicode_escape -- --nocapture`
- `cargo test -p bbnf-simd --test checkasm_unicode_escape`
- `cargo run -p xtask --release -- primitive-checkasm`
- `cargo test --workspace`
- `cargo bench -p bbnf-bench --bench json_parity -- 'json/(unicode_escapes|y_string_unicode)/(track1_generated|track2_handcoded|sonic_rs_anchor|sonic_rs_lossy|serde_json|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'`
- `cargo run -p bbnf-bench --bin gate --release -- --advisory`

## Hard Cap

125 minutes total for W4:

- research already closed;
- implementation and focused correctness within 60 minutes;
- measurement within 30 minutes;
- REDRESS close within 15 minutes.

## Revert Protocol

If the falsifiability gate fails, save the complete source candidate to
`/tmp/skv7-wave-4-rejected.patch`, revert the parse-that and checkasm source
edits plus refreshed `RESULTS.md`, and land a REDRESS rejection entry naming the
measured failure mode and next candidate shape. The next candidate must not be
parser-owned decoded scratch, byte-output unescape, semantic string facts,
direct-only source hooks, or the REDRESS 64 four-unit validator.

## Pre-Blocked Routes

Per `restart/skinny/tranches/sk-v7/HANDOFF.md §3` and `skinny/REDRESS.md`:

- Do not reopen the twice-rejected Class A tiny-string wiring route
  (REDRESS 28+33).
- Do not reopen SK-V5 UTF-8 fusion routes (REDRESS 50-55).
- Do not reopen SK-V6 retained-parse/direct-materialization routes
  (REDRESS 60-72), especially REDRESS 64, 66, 67, 68, and 69.
- Do not add new BBNF directives, BIR variants, substrates, parse side tables,
  parser-owned decoded scratch, capacity prescans, EventCursor prepasses, or
  source-hook semantic facts.

## Redress Decision Rule

Admit only if both target corpora pass parse and direct thresholds with no
>=3% same-row regression and checkasm parity green. Otherwise reject, preserve
the patch evidence, update `skinny/REDRESS.md`, and leave the worktree free of
the rejected source route.

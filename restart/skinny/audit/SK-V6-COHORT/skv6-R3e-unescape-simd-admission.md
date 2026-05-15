# SK-V6 Wave 3 R3e: Unescape SIMD Admission

Scope: read-only research on `/Users/mkbabb/Programming/bbnf-lang`. No repository files were edited.

## Decision

The next unescape close should **not** admit a new `bbnf-simd` AArch64 primitive or a new checkasm test first.

Admit it as a **scalar/reference materializer rewrite in `parse-that-regex` that reuses existing SIMD calls**:

- `bbnf_simd::aarch64::string_block::scan_string_special_block` for quote/backslash/control/non-ASCII block classification when a full quoted scan is useful.
- `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` / `unescape_uxxxx_x4_neon` and `join_surrogate_pair_neon` for fixed-width hex-unit decode.
- Existing scalar fallback paths for non-AArch64 and for non-contiguous or boundary-crossing escape shapes.

Reason: the vector semantics needed by JSON unescape are already present and parity-covered. The remaining risk is not "can NEON decode a hex quartet"; it is the materializer control shape, segment copying, allocation/reuse policy, exact error offsets, and same-wave direct consumption.

## Evidence

`bbnf-simd` already has the required primitive layer:

- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40` defines the scalar parity anchor for one `\uXXXX` quartet.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74` implements the AArch64 quartet decoder.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125` implements the x4 packed quartet decoder.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:169` implements surrogate pair join validation.
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31` has the scalar string-special reference.
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:57` has the 16-byte AArch64 string-special scanner.

The existing harness already exercises this surface:

- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:446` anchors `unescape_uxxxx_scalar`.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:623` covers aligned `string_block` parity.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:649` runs valid and invalid hex cases across alignments.
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:59` covers `unescape_uxxxx_x4_neon` plus surrogate join.

`parse-that-regex` already consumes the primitives:

- `skinny/crates/parse-that-regex/src/lib.rs:434` is the scalar JSON Unicode escape decoder.
- `skinny/crates/parse-that-regex/src/lib.rs:518` batches four contiguous `\uXXXX` escapes and calls `unescape_uxxxx_x4_neon`.
- `skinny/crates/parse-that-regex/src/lib.rs:854` is the current standalone `unescape_json_string` materializer.
- `skinny/crates/parse-that-regex/src/lib.rs:913` already attempts the AArch64 four-escape batch before scalar fallback.
- `skinny/crates/parse-that-regex/src/lib.rs:1267` through `:1350` test simple escapes, surrogate pairs, invalid offsets, lone surrogates, and non-character acceptance.

The current generated direct consumer is also clear:

- `skinny/crates/runtime/src/grammars/json/generated.rs:599` returns `ParsedString { raw, needs_unescape }`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:430`, `:470`, `:510`, and `:551` route strings to `JsonSink::*_source`.
- `skinny/crates/runtime/src/grammars/json/sink.rs:17`, `:28`, `:44`, and `:85` call `unescape_json_string(raw)` when `needs_unescape` is true.

Prior Wave 3 measurements narrow the route:

- REDRESS 66 rejected direct source-hook receiver folding; escaped-string materialization remained dominant.
- REDRESS 67 rejected parser-owned decoded scratch; `unicode_escapes` regressed by 44.03%, so folding decode into generated parser control should not be reopened.
- REDRESS 67 leaves only two admissible directions: a standalone decoded-string materializer that beats `unescape_json_string`, or a different DirectBuild field-fact plan.

## Admission Rule

Use a scalar materializer rewrite when the change only reorganizes:

- scan/control flow,
- segment append/copy strategy,
- capacity/reuse policy inside the materializer,
- exact simple-escape and Unicode-escape handling,
- opportunistic calls to already-admitted SIMD helpers.

Do **not** add a new `bbnf-simd` primitive or checkasm file unless the implementation introduces new vector semantics not already represented by `string_block`, `unescape_uxxxx`, UTF-8 block validation, or existing byte-class primitives.

If new vector semantics are introduced later, Lock 16 applies: add a scalar executable spec under `bbnf-simd/src/scalar/`, a target dispatch surface, checkasm parity/bench coverage, and a same-wave generated/runtime consumer. An orphan SIMD helper is not admissible.

## Scalar / Reference Requirements

The new materializer must be proven against the current public semantic oracle:

- `unescape_json_string(raw)` remains the public reference behavior, or the new implementation replaces it behind the same API with an internal `*_reference` used in tests.
- Return value must match borrowed vs owned behavior where observable: no-backslash raw content should stay `Cow::Borrowed` after control-byte validation.
- Error kinds and offsets must match current tests for invalid escapes, short hex, bad hex, lone high surrogate, lone low surrogate, bad low surrogate, and unescaped controls.
- JSON non-character codepoints must remain accepted, including `\uDBFF\uDFFE` -> `U+10FFFE`.
- Raw UTF-8 validity assumptions must stay unchanged: trusted direct callers pass `&str`; byte-mode/retained validation behavior must not be weakened.
- AArch64 fast paths must be optional and semantics-preserving. Non-AArch64 fallback must use the same scalar/reference logic.

Minimum test additions for the rewrite:

- Pairwise parity between the reference and candidate for all simple JSON escapes: `"`, `\`, `/`, `b`, `f`, `n`, `r`, `t`.
- Dense Unicode escape runs with mixed BMP, surrogate pairs, x4-aligned runs, and x4 boundary fallback.
- Non-contiguous escape mixes, for example `\u0041\n\u0042`, simple escapes between Unicode escapes, and trailing plain segments.
- Invalid cases with exact offsets: bad first quartet, bad second quartet, high surrogate at x4 boundary, low surrogate without high, and short input.
- Borrowed fast path with escaped and unescaped controls separated: no backslash plus no control borrows; no backslash plus control errors.

## Same-Wave Consumer

The same-wave consumer should be the generated direct Track 1 string path, but it does not require a new generated parser shape if the public API stays `unescape_json_string`:

- Preferred narrow route: replace/improve `parse-that-regex::unescape_json_string` itself. The existing generated direct parser already reaches it through `JsonSink::*_source(raw, true)`, so the consumer is `runtime/src/grammars/json/generated.rs` -> `runtime/src/grammars/json/sink.rs` -> rewritten `unescape_json_string`.
- If the API changes, update both `skinny/crates/runtime/src/grammars/json/sink.rs` and `skinny/crates/codegen/src/json_templates/generated.rs` / generated output in the same wave. Do not leave a new helper unused.
- Do not reintroduce parser-owned scratch, direct source hooks, sink-local decoded stats, or quote-source streaming hash. Those are REDRESS 54/55/66/67 recurrences.

Acceptance must be measured on production `profile_direct`, not just unit tests. Required focus rows remain `unicode_escapes`, `unicode_mixed`, and `y_string_unicode`, with guard rows for plain strings and number-heavy corpora. Attribution should show the rewritten materializer as a named boundary or a clearly reduced `unescape_json_string` cost; if the work vanishes into anonymous inlined frames, add attribution-only noinline boundaries and rerun.

## Bottom Line

Proceed with a standalone scalar/reference materializer rewrite using the existing AArch64 `string_block` and `unescape_uxxxx` calls. New SIMD/checkasm is unnecessary and should be rejected unless a later profile proves a missing vector operation rather than a materializer-shape problem.

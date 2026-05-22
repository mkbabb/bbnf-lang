# SK-V13 W16.1 Research - Unicode Escape-Run Validation

Date: 2026-05-22.
Wave: W16.1.
Target rows: `json/unicode_escapes/parse_only/main`,
`json/y_string_unicode/parse_only/main`.

## Authority

W16.1 continues the SK-V13 addendum bar: every JSON plane is admission-eligible
and every behavior wave must move a row or record a measured architectural
block. W14.5 exhausted the report-only parse admission pattern, so W16.1 must
land a real implementation or reject with a saved patch and fresh measurement.

## Current Rows

The current rolling rows are both OPEN under the strict sonic + 1 Mbps bar:

| row | Track 1 | sonic strict + 1 | margin |
|---|---:|---:|---:|
| `json/unicode_escapes/parse_only/main` | 13550 | 19274 | -5724 |
| `json/y_string_unicode/parse_only/main` | 6590 | 13861 | -7271 |

`skinny/RESULTS.md` classifies both rows as legacy `S / NO-GO`, but the SK-V13
pin makes parse-only rows row-moving again when strict equality and same-run
Criterion evidence pass.

## Source Shape

`skinny/crates/parse-that-regex/src/lib.rs` has two distinct unicode escape
surfaces:

- `unescape_string` already consumes
  `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon` through
  `unescape_four_unicode_escapes`. This is the SK-V10 W8 proof surface.
- `match_string_at_quote` and `match_string_at_quote_trusted_utf8` still call
  `validate_string_escape`, which dispatches `\u` to
  `validate_unicode_escape_run`. That validation loop reads every unicode unit
  with scalar `read_hex_unit_with_error_offset`, checks surrogate pairs, and
  then advances to the next `\u`.

Parse-only rows exercise the validation surface, not the materialization-only
`unescape_string` x4 fast path. That leaves a real same-consumer candidate:
validate runs of four consecutive non-surrogate `\uXXXX` units with the
existing x4 NEON hex decoder before falling back to the scalar surrogate-aware
validator.

## Prior REDRESS Boundary

The material differential is intentionally narrower than the historical
unicode attempts:

- REDRESS 82 used a per-quartet materializer/helper route and missed the parse
  and direct gates. W16.1 does not add another one-quartet helper.
- REDRESS 107 proved the existing x4 materializer caller in `unescape_string`
  but moved no row. W16.1 consumes the x4 primitive in the parse-only validation
  caller instead.
- REDRESS 108 rejected production because the W8 materializer caller was
  already wired. W16.1 has a new production source delta: validation-time
  batching in `validate_unicode_escape_run`.

Surrogate pairs stay scalar unless the batch proves that all four decoded units
are outside `U+D800..=U+DFFF`. Invalid hex must retain the existing
`InvalidUnicodeEscape` error kind and offset at the slash owning the failed
batch.

## Candidate Gate

Dispatch W16.1 only after a same-host micro-proof shows that the validation
batch is faster than the scalar loop on a corpus-derived unicode escape slice.
The redress gate then admits only if:

- `json/unicode_escapes/parse_only/main` exceeds same-run sonic strict parse
  by at least 1 Mbps, or records a measured reject with the patch saved;
- `json/y_string_unicode/parse_only/main` is measured as a secondary row and
  does not silently regress if it was touched by the same route;
- parse parity and parse-that-regex escape tests pass;
- bbnf-simd checkasm parity for `unescape_uxxxx_x4_neon` remains green;
- `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and `REDRESS.md` consume any admitted
  or rejected result.

## Owner Paths

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` only if the existing
  primitive needs a validation helper
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs` only if a retained
  micro-proof bench is needed
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w16.1/`

Dirty CSS parity sidecar JSON files are out of scope.

## Verification Candidates

```text
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd unescape_uxxxx_x4_matches_scalar -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd sk_v3_intrinsic_parity_aarch64 -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p parse-that-regex unescape -- --nocapture
cargo test -p bbnf-bench parity -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/(unicode_escapes|y_string_unicode)/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
```

The wave is high-risk because the measured gap is large. It is still
dispatchable because it consumes an existing accepted primitive at a production
validation call site that prior waves left scalar.

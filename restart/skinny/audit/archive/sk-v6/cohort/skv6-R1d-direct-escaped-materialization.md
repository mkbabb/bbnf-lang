# SK-V6 Wave 3 R1d: Direct Escaped-String Materialization Candidate

Candidate: parser-owned decoded-scratch materializer for generated `SinkOnly`
direct strings.

## Dominant Code Paths

Current Track 1 direct string flow:

- `skinny/crates/runtime/src/grammars/json/generated.rs:398` enters
  generated `parse_direct`.
- `generated.rs:429`, `generated.rs:469`, `generated.rs:509`, and
  `generated.rs:551` call `parse_string_direct` for root, object-value,
  array-value, and object-key strings.
- `generated.rs:599` returns only `ParsedString { raw, needs_unescape }`.
  The full-string path calls
  `match_json_string_at_quote_trusted_utf8`, sets `cursor = span.raw_end`,
  and leaves actual decoding for the sink.
- `skinny/crates/runtime/src/grammars/json/sink.rs:17`,
  `sink.rs:28`, `sink.rs:44`, and `sink.rs:85` allocate/decode escaped
  strings through `unescape_json_string(raw)?`.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:59`,
  `direct_struct.rs:123`, and `direct_struct.rs:184` then compute exact
  digest facts from the semantic decoded `&str` using contiguous
  `hash_bytes(value.as_bytes())`.
- `skinny/crates/parse-that-regex/src/lib.rs:854` creates a fresh
  `String::with_capacity(raw_content.len())` for every escaped string.
  Unicode decode runs through `decode_json_unicode_escape`
  (`lib.rs:434`) and the AArch64 four-escape batch helper (`lib.rs:518`).

The focused fixtures match the profile diagnosis:

| row | bytes | strings | escaped strings | backslashes | `\uXXXX` |
|---|---:|---:|---:|---:|---:|
| `unicode_escapes` | 1,050,797 | 5,636 | 1,877 | 230,134 | 136,682 |
| `unicode_mixed` | 1,053,086 | 25,121 | 1,959 | 65,489 | 0 |

The §9 profile says `unicode_escapes` direct is 46.9%
`unescape_json_string` and 43.4% `parse_string_direct`; `unicode_mixed`
is 22.8% `unescape_json_string` and 51.1% `parse_string_direct`.
REDRESS 66 then showed source-hook/receiver folding only moved
`unicode_escapes` +0.99% and `unicode_mixed` +0.11%, so the remaining
candidate has to attack escaped decode/materialization, not receiver shape.

## Candidate Mechanism

Add a generated-direct-only path where the parser owns one reusable decoded
string scratch buffer and materializes escaped strings into that buffer while
it scans/validates the quoted source. The sink receives the already-decoded
semantic `&str`; `JsonDigestSink` continues to hash contiguous decoded bytes
with the existing `hash_bytes` path.

Important shape:

- Plain/tiny strings stay borrowed and keep the existing no-allocation path.
- Escaped strings do not call `JsonSink::*_source(raw, true)`.
- The escaped path does not stream a hash. It writes decoded UTF-8 into a
  reusable contiguous `String`, then calls `key`, `string`, `array_string`, or
  `object_string` with `scratch.as_str()`.
- `JsonDigestSink` is not given raw source hooks or decoded-stat helpers. If
  the candidate requires any `JsonDigestSink::*_source` override, reject it as
  a REDRESS 54/66 recurrence.

This preserves the measured advantage of contiguous `hash_bytes` from the
checked-in baseline while removing the duplicated escape walk and most
per-string allocation churn.

## Exact File Changes

1. `skinny/crates/parse-that-regex/src/lib.rs`

   Add a public trusted helper, name bikesheddable:

   ```rust
   pub enum JsonStringValue<'i> {
       Borrowed(&'i str),
       Decoded,
   }

   pub struct JsonStringMaterialized<'i> {
       pub raw_end: usize,
       pub value: JsonStringValue<'i>,
   }

   pub fn materialize_json_string_at_quote_trusted_utf8_into<'i>(
       input: &'i str,
       offset: usize,
       out: &mut String,
   ) -> Result<JsonStringMaterialized<'i>, RegexError>
   ```

   The helper must scan from the opening quote, validate escapes/control
   bytes, and append decoded UTF-8 into `out` only when an escape is seen. It
   should reuse/factor the existing simple escape, `decode_json_unicode_escape`,
   `unescape_four_unicode_escapes`, and `find_next_escape_or_control` logic so
   AArch64 Unicode batching remains in force. It must not implement a second
   sink-local hash/stat path.

2. `skinny/crates/codegen/src/json_sink_direct.rs`

   Render a direct string scratch through generated direct parsing:

   - `parse_direct` creates `let mut string_scratch = String::new();`.
   - `parse_value_direct`, `parse_object_value_at_direct`,
     `parse_array_element_at_direct`, `parse_object_direct`, and
     `parse_array_direct` accept `&mut String`.
   - Replace `ParsedString { raw, needs_unescape }` with an enum equivalent to
     borrowed-or-scratch-decoded.
   - In each string emission site:
     - borrowed value: keep current semantic behavior, preferably
       `*_source(raw, false)` to avoid changing non-escaped receiver shape;
     - decoded scratch value: call `key(decoded)`, `string(decoded)`,
       `array_string(decoded)`, or `object_string(decoded)` directly.
   - Keep number/literal/container lowering unchanged.

3. `skinny/crates/runtime/src/grammars/json/generated.rs`

   Regenerate with `cargo xtask regen-json` after the renderer change. The
   generated file should show scratch threaded through the direct parse helpers
   and should no longer call `*_source(raw, true)` on escaped strings.

4. `skinny/crates/runtime/src/grammars/json/sink.rs`

   No required behavior change. Leave the admitted source-hook defaults in
   place as the generic fallback. Do not add direct-only source hooks for this
   candidate.

5. `skinny/crates/bbnf-bench/src/direct_struct.rs`

   No direct materialization override. Keep `JsonDirectDigest::string`,
   `fold_string_scalar`, and `fold_key` as the only string digest consumers so
   the measurement proves parser-owned materialization, not sink-local stats or
   receiver folding. Existing parity tests should be extended only if current
   fixture coverage misses escaped keys, dense `\uXXXX`, and surrogate pairs.

## Expected Row Impact

Primary impact is `unicode_escapes`. The row has 1,877 long escaped strings,
230,134 backslashes, and 136,682 Unicode escapes. The current path validates
the string in `parse_string_direct`, then allocates and decodes the raw content
again in `unescape_json_string`. A fused parser-owned materializer should
remove the second validation walk and most per-string allocation, while keeping
the baseline's contiguous decoded-byte hash.

`unicode_mixed` is lower-ceiling: only 7.8% of strings contain escapes and
there are no `\uXXXX` units in the checked fixture, but the escaped strings are
still relatively long. Expected movement is smaller and comes from avoiding
fresh allocation plus the second simple-escape scan.

Expected direct smoke target:

- `unicode_escapes`: +20% or better Track 1 Mbps.
- `unicode_mixed`: +15% or better Track 1 Mbps to satisfy the existing Wave 3
  direct gate; below +10% means the candidate is probably too narrow.
- `y_string_unicode`: should move if dense Unicode escapes are present.
- `distinct_values` and `gsoc-2018`: should remain within noise unless their
  direct string path contains escaped strings; they are guards for accidental
  non-escaped receiver churn.

## Falsifiability Gate

Correctness gate:

- `cargo fmt`
- `CARGO_TARGET_DIR=/tmp/skv6-R1d-materialize-target cargo test -p parse-that-regex --profile ax-iter`
- `CARGO_TARGET_DIR=/tmp/skv6-R1d-materialize-target cargo test -p runtime --profile ax-iter`
- `CARGO_TARGET_DIR=/tmp/skv6-R1d-materialize-target cargo test -p bbnf-bench --profile ax-iter`
- `CARGO_TARGET_DIR=/tmp/skv6-R1d-materialize-target cargo xtask check-json`
- `CARGO_TARGET_DIR=/tmp/skv6-R1d-materialize-target cargo xtask check-conformance`

Throughput gate, production `profile_direct` medians against same-tree
baseline/candidate binaries:

- Focus rows: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`.
- Guard rows: `distinct_values`, `gsoc-2018`, `unicode_basic`,
  `apache_builds`, `github_events`, `canada`, `numbers`.
- Required: `unicode_escapes >= +20%`, `unicode_mixed >= +15%`, and one of
  `y_string_unicode >= +8%` or escaped-string allocation count down at least
  90%; no guard row may regress more than 5%.

Profile gate:

- In Track 1 direct attribution, `unescape_json_string` should disappear or
  become negligible for generated direct escaped strings.
- New parser-owned materializer plus `parse_string_direct` combined self-time
  must be at least 20% lower than baseline
  `parse_string_direct + unescape_json_string` on `unicode_escapes` and
  `unicode_mixed`.
- `hash_bytes` may rise as a share only because decode got cheaper; absolute
  throughput must still clear the row gate.
- No `JsonDigestSink::*_source` override or direct-only source hook may appear
  in the candidate diff.

Reject conditions:

- If the helper wraps current `match_json_string_at_quote_trusted_utf8` and
  then calls an `unescape_*` pass, reject before benchmarking; that is only
  allocation reuse, not parser-owned materialization.
- If the decoded buffer is hashed during escape validation, reject as a
  REDRESS 55 recurrence.
- If the sink computes decoded length/hash facts from raw source, reject as a
  REDRESS 54 recurrence.
- If the measured lift is mostly on `distinct_values`/`gsoc-2018` receiver
  symbols rather than escaped rows, reject as a REDRESS 66 recurrence.

## Why It Avoids REDRESS 54/55/66

- Avoids REDRESS 54: the sink does not compute exact decoded stats or
  sink-local decoded hashes from raw source. The parser materializes the
  semantic string, and the existing digest code consumes normal `&str` values.
- Avoids REDRESS 55: it is not quote-source streaming hash. The candidate keeps
  a contiguous decoded materialization and then runs the existing contiguous
  `hash_bytes` path, which is the part the checked-in baseline still wins on.
- Avoids REDRESS 66: it does not add direct source hooks and does not specialize
  `JsonDigestSink` receiver folding. Non-escaped strings should keep the
  current path; the measured delta must come from escaped decode/materialization
  on `unicode_escapes` and `unicode_mixed`.

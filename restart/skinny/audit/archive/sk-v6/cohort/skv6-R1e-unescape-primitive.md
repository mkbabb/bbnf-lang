# SK-V6 Wave 3 R1e: standalone decoded-string primitive proposal

## Candidate

Add one grammar-neutral byte-output decoded-string primitive under
`parse-that-regex::unescape_json_string`, not under the generated parser or
sink:

`append_json_decoded_escape_run(input: &[u8], slash: usize, out: &mut String) -> Option<Result<usize, RegexError>>`

Call it from
`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/parse-that-regex/src/lib.rs:911`
before the existing scalar `decode_json_unicode_escape` fallback. Keep
`unescape_json_string(raw_content: &str) -> Result<Cow<'_, str>, RegexError>`
as the public API and keep the existing generated direct consumer:

- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:429`
  still calls `sink.string_source(value.raw, value.needs_unescape)`.
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:470`
  still calls `sink.object_string_source(...)`.
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:510`
  still calls `sink.array_string_source(...)`.
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:551`
  still calls `sink.key_source(...)`.
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/sink.rs:17`,
  `:28`, `:44`, and `:85` still call `unescape_json_string(raw)?` when
  `needs_unescape`.

The primitive should write decoded UTF-8 bytes directly into `out` through a
small stack buffer and one append per escape run, then return the new source
cursor. It replaces only the local decoded-output step now split across
`unescape_four_unicode_escapes` and scalar simple-escape dispatch:

- Current Unicode path:
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/parse-that-regex/src/lib.rs:518`
  copies four hex quartets into `[u8; 16]`, calls
  `bbnf_simd::aarch64::unescape_uxxxx_x4_neon`, converts each unit or
  surrogate pair through `char::from_u32`, stores into `[char; 4]`, then calls
  `String::push` once per decoded char at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/parse-that-regex/src/lib.rs:587`.
- Current simple path:
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/parse-that-regex/src/lib.rs:878`
  dispatches each escaped byte through eight match arms and one `String::push`.

The proposed primitive keeps the same validation behavior and error offsets,
but changes the output unit from `char` pushes to byte appends:

1. For contiguous `\uXXXX` runs, gather the hex quartets from the raw
   `\uXXXX\uXXXX...` source, reuse or extend
   `bbnf_simd::aarch64::unescape_uxxxx_x4_neon`, join surrogate pairs, encode
   decoded scalars into a stack `[u8; 32]`, and append that byte slice to
   `out` once. If the fourth unit is a high surrogate or a malformed boundary
   is seen, return `None` and let the existing scalar fallback preserve exact
   offsets.
2. For runs of simple escapes (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`,
   `\t`), use a small LUT from escaped byte to decoded byte and append the
   decoded bytes as one stack slice. Stop at the first non-simple escape,
   invalid escape, or non-escape byte so the surrounding `unescape_json_string`
   loop still owns raw segment copying and public error construction.

This is deliberately a standalone decoded-string materializer primitive. It
does not scan quotes, does not hash, does not compute exact decoded stats, does
not add parser scratch, and does not add or override any generated direct sink
hook.

## Why this differs from rejected routes

REDRESS 54 rejected `JsonDigestSink` exact decoded length plus exact hash
because the generated Track 1 sink paid a two-pass stats/hash cost on escaped
strings. This proposal is not sink-local and not two-pass. It still returns the
same `Cow<str>` materialization from `unescape_json_string`; it only reduces
the cost of building the `String`.

REDRESS 55 rejected quote-source fused decode plus streaming hash. This
proposal does not move from raw-content source hooks to quote-source hooks, and
it does not stream into the digest. The default allocate-then-contiguous-hash
consumer remains intact; the allocation result is just built with fewer
per-escape operations.

REDRESS 66 rejected direct source-hook field-layout receiver shortcuts. This
proposal adds no `JsonSink` hooks, no field-layout receiver path, no BIR
surface, and no generated parser change. The call chain remains generated
`parse_direct` -> `*_source(raw, needs_unescape)` -> `unescape_json_string`.

REDRESS 67 rejected parser-owned decoded scratch. This proposal keeps decoded
storage owned by `unescape_json_string`, not by `parse_direct`; it does not
thread a scratch buffer through parser control or change escaped strings to
call semantic sink methods directly.

It also avoids reopening the REDRESS 64 retained Unicode-escape validator. That
candidate accelerated validation in `validate_json_unicode_escape_run`; this
one targets decoded string output in `unescape_json_string`. The local fact is
not "four-unit runs validate faster"; it is "the current materializer already
finds the right escaped strings, but spends too much per escape emitting bytes."

## Corpus facts from the checked-in fixtures

`unicode_escapes`:

- Size: 1,050,797 bytes.
- Escaped strings: 1,877 strings, 1,008,672 raw string-content bytes.
- Unicode escape units: 136,682.
- Simple escapes: 86,192.
- Contiguous `\uXXXX` coverage: 100% of Unicode units are in runs of at
  least four.
- Current x4 materializer coverage by source simulation: 33,787 x4 batches,
  135,148 units handled by the x4 helper, 1,534 units handled by scalar
  fallback.

`unicode_mixed`:

- Size: 1,053,086 bytes.
- Escaped strings: 1,959 strings, 335,831 raw string-content bytes.
- Unicode escape units: 0.
- Simple escapes: 53,644.
- Raw non-ASCII bytes inside escaped strings: 75,318.

Implication: a Unicode-only primitive can help `unicode_escapes` but has no
direct mechanism for `unicode_mixed`. The primitive must include the simple
escape byte-output subpath or it is not a credible Wave 3 direct-string
candidate for both requested rows.

## Expected row impact

Expected impact should be measured on direct-to-struct, because REDRESS 66 and
67 are direct-row redresses and the generated consumer is preserved.

Using the current REDRESS direct baseline:

- `unicode_escapes` Track 1 direct baseline: 5,262 Mbps. Expected candidate
  range: about +12% to +20% if byte-output x4 removes enough per-unit
  `String::push` and quartet-packing overhead. This would put Track 1 around
  5,900 to 6,315 Mbps. It will not close the sonic-rs gap by itself, but it is
  large enough to decide whether standalone materializer work is still live.
- `unicode_mixed` Track 1 direct baseline: 4,633 Mbps. Expected candidate
  range: about +5% to +10% only if the simple-escape LUT/stack append path is
  included. This would put Track 1 around 4,865 to 5,095 Mbps. A Unicode-only
  implementation should be expected to land near noise on this row.
- Guard expectation: `unicode_basic` should stay within +/-2%, because plain
  strings should keep the borrowed/no-unescape path and avoid the new
  primitive.

## Falsifiability gate

Implement only as a same-wave primitive plus existing generated consumer smoke.
Reject and revert if any condition fails:

1. Correctness:
   `CARGO_TARGET_DIR=/tmp/skv6-r1e-unescape-target cargo test -p parse-that-regex --profile ax-iter`
   passes, including focused tests for simple escape runs, BMP `\uXXXX` runs,
   surrogate pairs, boundary-crossing surrogate pairs, lone surrogates, invalid
   hex, and invalid simple escapes with exact offsets.
2. SIMD parity:
   add or extend a checkasm/scalar parity test for the raw-run byte-output
   primitive. It must compare scalar and AArch64 output bytes and cursor/error
   behavior, not just decoded code units.
3. Direct smoke:
   run baseline and candidate release binaries from the same HEAD through
   `profile_direct` on `unicode_escapes`, `unicode_mixed`, `unicode_basic`,
   `y_string_unicode`, and one non-string guard such as `numbers` or
   `distinct_values`.
4. Admission threshold:
   `unicode_escapes` Track 1 direct >= +12%, `unicode_mixed` Track 1 direct
   >= +5%, `unicode_basic` no worse than -2%, `y_string_unicode` no worse than
   -5%, and no guard row worse than -2%.

If `unicode_escapes` improves but `unicode_mixed` is noise, record that the
Unicode x4 output half is too narrow and do not relabel it as a Wave 3
decoded-string close. If `unicode_mixed` regresses, the simple-escape subpath
is branch/cache negative and the primitive should be rejected even if
`unicode_escapes` clears its threshold.

# SK-V11 W6 R1 - Parse-That Escape Segments And Hex Decode

Date: 2026-05-20.

Scope: read-only diagnosis for SPEC Section 10 W6. This artifact inspects the
parse-that-regex escaped-string segment and hex-decode surfaces, current callers,
the admissible scalar-oracle shape, and the REDRESS pre-blocks. No source was
edited.

## Disposition

W6 does not have an already-landed new source delta to consume. The current
production path already validates escapes in the string matcher, marks
`needs_decode`, and decodes through `unescape_string`; on aarch64,
`unescape_string` already calls the four-quartet NEON path when four consecutive
JSON `\uXXXX` escapes are present. Therefore an x4 wrapper, feature re-gate,
renamed helper, or "now consumed" claim around the current `unescape_string`
caller is not an admissible W6 delta.

The only plausible W6 source delta is a new scalar escaped-segment or hex-run
oracle that is consumed by a new direct/typed/non-JSON product caller in the same
wave. The new caller must materially differ from the existing
`unescape_string -> decoded Cow<str> -> length/hash/string sink` family. A
current direct-digest streaming length/hash shortcut is REDRESS-adjacent and
should be treated as CHALLENGE-high-risk unless it states a material
differential from REDRESS 54/55/66-69.

## Entry Facts

SPEC Section 10 defines W6 as the escaped segment and hex decode slice. The
entry gate requires W5 disposition and a plan that names "a new source delta
beyond the already-consuming `unescape_string` path"; owner paths include
`parse-that-regex`, `bbnf-simd` unescape, generated JSON direct/typed callers,
direct benches, parity benches, `RESULTS.md`, and `REDRESS.md`
(`restart/skinny/tranches/sk-v11/SPEC.md:595`-`646`).

The dispatch prompt mirrors the W6 budget and requires CHALLENGE before redress;
redress cannot begin until CHALLENGE accepts (`DISPATCH-PROMPT.md:61`-`63`,
`DISPATCH-PROMPT.md:102`-`117`). The W6 exit gate is
`G-W6-ESCAPE-SEGMENT-DIRECT`, and direct row floors are the Section 10 rows:
`unicode_escapes >= 3441`, `unicode_mixed >= 2588`, and
`y_string_unicode >= 3950`; existing `unescape_string` reuse alone is a gate
failure (`p3b-wave-sequencing.md:101`-`110`).

W5 admits no reusable span API. REDRESS 116 records W5 blocked before source
redress, no source patch attempted, and no rejected-but-reusable scalar proof;
W6 may dispatch only through the independent Section 10 segment route
(`skinny/REDRESS.md:3411`-`3432`,
`restart/skinny/tranches/sk-v11/research/w5/redress/w5-redress-entry-blocked.md:30`-`40`).

The non-JSON axis is also blocked in this tranche. REDRESS 113 says W2 could not
consume a generated non-JSON baseline because W1b admitted no
`W1b_css_baseline_mbps`, and downstream W3-W8 may continue only as direct-plane
closure/fixpoint waves with that block carried (`skinny/REDRESS.md:3340`-`3355`).
So a CSS/BBNF escaped-string or hex-color consumer is only a theoretical W6
shape unless the plan can make it measurable without creating the first
non-JSON baseline inside W6, which current authority forbids.

## Existing Parse-That Surface

`match_string_at_quote_trusted_utf8` starts at a trusted quote, scans plain
segments, validates escapes through `validate_string_escape`, marks
`HAS_ESC | NEEDS_DECODE`, rejects controls, and returns a raw span
(`skinny/crates/parse-that-regex/src/lib.rs:162`-`209`). The generic
`match_string_at_quote` does the same with UTF-8/grammar-string modes and
`StringFlags` (`lib.rs:227`-`281`).

The escape validator already understands JSON simple escapes and Unicode
escapes. `validate_string_escape` routes `\u` to `validate_unicode_escape_run`
(`lib.rs:284`-`293`), and `validate_unicode_escape_run` loops over contiguous
JSON Unicode escapes while enforcing surrogate-pair validity (`lib.rs:347`-`381`).
The scalar decode path is `decode_unicode_escape`, backed by
`read_hex_unit_with_error_offset`, `read_hex_unit_scalar`, and `hex_nibble`
(`lib.rs:302`-`344`, `lib.rs:919`-`965`).

`unescape_string` is already a segment materializer. It returns borrowed data
when no backslash is present after `classify_string_content`; otherwise it walks
raw spans, pushes raw segments, handles simple escapes, and handles `\u` escapes
(`lib.rs:718`-`810`). Its scan for the next escape/control byte is already
word-at-a-time scalar through `find_next_escape_or_control` and
`string_escape_control_mask` (`lib.rs:812`-`840`).

On aarch64, `unescape_string` already attempts `unescape_four_unicode_escapes`
for four consecutive JSON Unicode escapes before falling back to scalar
`decode_unicode_escape` (`lib.rs:775`-`785`). That helper packs four quartets,
calls `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon`, applies JSON
surrogate policy, pushes chars into the output string, and advances by 24 bytes
on success (`lib.rs:384`-`459`). This is the already-consuming path W6 cannot
reclaim as new production.

The SIMD primitive itself has an x1 scalar reference, x1 NEON body, x4 NEON
body, and surrogate-pair helper in `unescape_uxxxx.rs`
(`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33`-`193`). Existing
checkasm coverage includes broad x1 valid/invalid/alignment parity in
`checkasm_parity.rs` (`checkasm_parity.rs:550`-`662`), but the separate x4 test
is a single valid packed example (`checkasm_utf8_block.rs:58`-`67`). That matches
the P3 finding that x4 production still needs strict x4 checkasm over invalid,
mixed-validity, alignment, surrogate, and tail cases
(`p3a-candidate-shortlist.md:201`-`209`).

Existing parse-that unit tests cover all simple escapes, surrogate pairs,
invalid Unicode offsets, control rejection, and noncharacter acceptance
(`lib.rs:1129`-`1213`). They are good regression oracles, but they are not a new
W6 product consumer.

## Current Callers

Generated JSON direct Track 1 calls `parse_string_direct` for root string
values, object string values, array string values, and object keys
(`skinny/crates/runtime/src/grammars/json/generated.rs:437`-`444`,
`:477`-`:484`, `:517`-`:524`, `:561`-`:564`). `parse_string_direct` first tries
the cap-8 plain string helper, then falls back to
`parse_that_regex::match_string_at_quote_trusted_utf8`, returns a raw slice plus
`needs_unescape`, and advances the cursor (`generated.rs:610`-`640`). The codegen
template emits the same shape from `sink_direct.rs` (`sink_direct.rs:120`-`241`,
`:315`-`:350`).

The generated direct sink methods decode by calling `unescape_string` whenever
`needs_unescape` is true for keys, root strings, array strings, or object strings
(`skinny/crates/runtime/src/grammars/json/sink.rs:16`-`35`, `:43`-`:51`,
`:84`-`:92`). This is the main Track 1 production route.

The independent hand direct Track 2 parser in `bbnf-bench` repeats the same
shape: a cap-8 local tiny-string path, fallback to
`match_string_at_quote_trusted_utf8`, then `unescape_string` if
`span.needs_decode()` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`-`561`).
Its digest plane consumes decoded strings by length and byte hash
(`direct_struct.rs:59`-`63`, `:123`-`:127`, `:184`-`:186`, `:301`-`:312`).

Generated typed direct parsing also uses the same materializer: `parse_string`
guards for an opening quote, tries its cap-32 tiny-string path, falls back to
`match_string_at_quote_trusted_utf8`, and calls `unescape_string` on
`needs_decode` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1649`-`1670`;
codegen template at `skinny/crates/codegen/src/typed_direct.rs:480`-`502`).
Its `skip_string_raw` path validates and advances but does not decode
(`generated_real_typed.rs:1796`-`1807`).

Retained JSON views lazily call `unescape_string` in `JsonString::as_str` if the
tape flags contain `HAS_ESC` (`skinny/crates/runtime/src/grammars/json/view.rs:199`-`215`).
Retained parse/view is diagnostic for W6; it cannot admit SK-V11 direct SOTA.

## New-Delta Assessment

Not new:

- exposing `unescape_uxxxx_x4_neon` through a wrapper;
- adding a feature flag or constant around the current x4 call;
- claiming production because `unescape_string` already reaches x4;
- moving JSON surrogate logic inside a generic helper;
- replacing `String::push` with a byte writer inside the same `Cow<str>` API;
- adding parser-owned decoded scratch or semantic string facts;
- adding source hooks whose only purpose is to bypass the current receiver shape
  around the same decoded materialization.

Potentially new, but only if CHALLENGE accepts the consumer:

- a scalar segment-stream oracle that reports raw spans, simple-escape tokens,
  and Unicode scalar events without allocating a decoded `String`;
- a neutral hex-run/quartet oracle that decodes x1/x4 hex units and preserves
  valid/invalid/mixed-lane status without applying JSON surrogate policy in a
  generic crate;
- a generated JSON direct or typed escaped-string product consumer that consumes
  those events in a way materially different from current `unescape_string`
  materialization;
- a non-JSON escaped-string or hex-color consumer only under authority that makes
  a generated non-JSON baseline measurable despite REDRESS 113.

The strongest scalar oracle shape is therefore not "fast unescape_string." It is
a no-allocation event stream, for example:

```text
EscapedSegment::Raw(&str)
EscapedSegment::SimpleEscape(byte)
EscapedSegment::UnicodeUnit(u16) or UnicodeScalar(char)
```

If this helper lives in `parse-that-regex`, it must keep grammar policy outside
the generic core: JSON simple-escape mapping and surrogate joining belong in the
generated JSON caller; CSS variable-width escapes and BBNF literal policy remain
their own generated/host policy. If x4 is routed, the scalar x4 oracle must call
the x1 oracle four times and preserve lane-level invalid/mixed/tail behavior
before any NEON body is measured.

## Pre-Blocked Routes

SPEC Section 10 pre-blocks REDRESS 64, 66-69, 82, 83, 107, 108, and existing
`unescape_string` reuse as same-wave production (`SPEC.md:638`-`646`). The P3
ledger adds the same W6 block: no x4 proof-to-production through the
already-wired `unescape_string` caller, no JSON surrogate policy in generic
code, and no single-quartet materializer (`p3e-preblocked-ledger.md:31`-`40`).

Load-bearing prior failures:

- REDRESS 64 rejected a retained four-unit Unicode escape validator despite green
  correctness; it is the dense x4 validator family (`skinny/REDRESS.md:1584`-`1598`).
- REDRESS 66-69 rejected direct source-hook folding, parser-owned decoded
  scratch, byte-output unescape materialization, and DirectBuild semantic string
  facts under the current direct digest workload (`skinny/REDRESS.md:1688`-`1732`,
  `:1736`-`:1785`, `:1789`-`:1835`, `:1839`-`:1886`).
- REDRESS 82 rejected a single-quartet Unicode escape classifier consumed by
  decode/materialization; correctness was green but direct rows missed and
  `y_string_unicode` Track 2 regressed (`skinny/REDRESS.md:2287`-`2313`).
- REDRESS 107 admitted only the W8 micro-proof for the existing
  `unescape_string` caller; it moved no `RESULTS.md` row and wired no new
  production behavior (`skinny/REDRESS.md:3172`-`3196`).
- REDRESS 108 rejected production reuse of that proof because the exact
  `unescape_string` caller already consumed x4 before W9; the targeted direct
  rows still failed, and future production reuse requires a new route naming a
  real source delta (`skinny/REDRESS.md:3198`-`3222`).
- REDRESS 116 blocks W5-derived span reuse; W6 must carry W5's lack of admitted
  span API and select an independent segment plan (`skinny/REDRESS.md:3411`-`3432`).

## Recommendations

1. Do not dispatch an x4-only W6 plan. It is already consumed by
   `unescape_string` and is explicitly REDRESS 107/108 pre-blocked.

2. If W6 proceeds, make the plan prove a real source delta before source redress:
   a scalar segment-stream or hex-run oracle, exact malformed-offset parity, and
   a named generated direct/typed consumer. The plan should reject before redress
   if the consumer is only a wrapper around `JsonSink::*_source` calling
   `unescape_string`.

3. Treat direct-digest streaming length/hash as REDRESS-adjacent, not obviously
   free. It touches the same rejected decoded-stats/hash/fact family as REDRESS
   54/55/69 unless CHALLENGE names a material output-contract differential.

4. Keep Track 2 independent. A W6 Track 2 proof may share stable parse-that
   primitives, as it already does, but it must not call generated Track 1 helpers
   or a hidden shared parser. It needs its own caller path and strict digest
   equality against `serde_json` and `sonic-rs`.

5. Preserve the W5 malformed-string lesson. Any generated string caller touched
   by W6 needs a release-mode opening-quote guard and malformed key/value/array
   fixtures across generated Track 1, independent Track 2, `serde_json`, and
   `sonic-rs`; `match_string_at_quote_trusted_utf8` only has a debug assertion at
   the quote boundary (`parse-that-regex/src/lib.rs:162`-`167`).

6. If no such consumer can be named, W6 should be CHALLENGE-blocked rather than
   patched. The honest next step would be W8 fixpoint/uncloseable proof for the
   Unicode direct residuals, carrying REDRESS 107/108/116.

## Risks

- `unicode_mixed` is weak x4 evidence: REDRESS 107 recorded zero eligible x4
  slices because its `\u` text was escaped-backslash data, not valid JSON Unicode
  escape syntax (`skinny/REDRESS.md:3185`-`3188`).
- A generic parse-that segment helper can violate Lock 14 if it bakes JSON
  surrogate/simple-escape policy into generic behavior. SPEC requires per-grammar
  generated policy and same-wave non-JSON proof for generic/codegen/runtime-
  outside-JSON edits (`SPEC.md:230`-`245`).
- The direct JSON digest output plane currently hashes decoded string bytes and
  tracks decoded byte length. That makes many "no allocation" escape routes look
  like REDRESS 54/55/69 under a new name unless the product contract changes.
- x4 strict checkasm is incomplete for production. Existing x4 coverage is a
  single valid packed example; production needs invalid, mixed-validity,
  alignment 0..63, surrogate, unpaired-surrogate, and boundary/tail cases.

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md:595`-`646`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:61`-`63`,
  `:102`-`:117`, `:146`-`:182`
- `restart/skinny/tranches/sk-v11/HANDOFF.md:91`-`119`
- `skinny/REDRESS.md:1584`-`1598`, `:1688`-`:1886`,
  `:2287`-`:2313`, `:3172`-`:3222`, `:3340`-`:3355`,
  `:3411`-`:3432`
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:182`-`226`
- `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:68`-`110`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:31`-`40`,
  `:47`-`:60`, `:84`-`:94`, `:190`-`:194`
- `skinny/crates/parse-that-regex/src/lib.rs:162`-`209`,
  `:284`-`:344`, `:347`-`:459`, `:718`-`:840`, `:919`-`:965`,
  `:1129`-`:1213`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33`-`193`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:550`-`662`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58`-`67`
- `skinny/crates/runtime/src/grammars/json/generated.rs:437`-`444`,
  `:477`-`:484`, `:517`-`:524`, `:561`-`:564`, `:610`-`:640`
- `skinny/crates/runtime/src/grammars/json/sink.rs:16`-`35`,
  `:43`-`:51`, `:84`-`:92`
- `skinny/crates/bbnf-bench/src/direct_struct.rs:541`-`561`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1649`-`1670`,
  `:1796`-`:1807`
- `skinny/crates/codegen/src/sink_direct.rs:120`-`241`, `:315`-`:350`
- `skinny/crates/codegen/src/typed_direct.rs:480`-`502`

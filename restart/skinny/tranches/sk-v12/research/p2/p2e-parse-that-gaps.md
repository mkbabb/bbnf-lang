# SK-V12 P2-E: Parse-That Primitive Gaps

Pass: S-P2 Research. Cycle: V12.
Date: 2026-05-20.
Scope: parse-that primitive vocabulary gaps demanded by accepted S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan; container_dispatch; unicode_escape_hex_decode; number_digit_span; simd_movemask; string_escape_decode; output_digest_hash; ascii_whitespace_skip; typed_direct_projection; serde_json_oracle_read_parse.
Lock surface: both (Lock 1 and Lock 14; Lock 16 applies only to allowable SIMD/checkasm proofs).

## §1 — Findings

S-P1 accepted the P1-A/P1-B/P1-C/P1-D/P1-E/P1-F evidence set and converged on ten hot families for S-P2: bounded_plain_string_scan, container_dispatch, unicode_escape_hex_decode, number_digit_span, simd_movemask, string_escape_decode, output_digest_hash, ascii_whitespace_skip, typed_direct_projection, and serde_json_oracle_read_parse. Only five of those expose parse-that vocabulary gaps. The other five are support, generated-control, projection, or oracle surfaces and should not be converted into parse-that primitives in this packet.

The candidate-carrying antecedents are:

| Antecedent | Why it is a parse-that gap |
| --- | --- |
| ascii_whitespace_skip | `parse-that-regex` has a JSON-specific `skip_ascii_whitespace` and a narrow `skip_ascii_spaces`, but no grammar-neutral byte-set run skipper. |
| bounded_plain_string_scan | Generated JSON has a local tiny plain-string matcher, and `parse-that-regex` has private plain-string scanners, but no public bounded plain-string end primitive that generated non-JSON parsers can consume. |
| number_digit_span | `parse-that-regex::number` exposes JSON number recognition and materialization helpers, while the useful digit-run scan/accumulate pieces remain private and JSON-policy-adjacent. |
| unicode_escape_hex_decode | `parse-that-regex` has private scalar hex helpers and `bbnf-simd` has AArch64 `\uXXXX` helpers, but there is no grammar-neutral public hex-unit primitive. |
| string_escape_decode | `parse-that-regex::unescape_string` materializes a `String`/`Cow<str>` path; generated direct/typed consumers need a segment-producing primitive that can avoid mandatory materialization. |

The non-candidate accepted leaves remain visible only as constraints:

| Antecedent | P2-E disposition |
| --- | --- |
| container_dispatch | Generated FIRST-set/control dispatch, not a parse-that primitive. It may consume byte-set/string primitives, but should not become a retained structural sidecar. |
| simd_movemask | Layer 0 support in `bbnf-simd`, not a parse-that vocabulary gap by itself. It needs a same-wave consumer if routed. |
| output_digest_hash | Output/oracle/benchmark host sink. REDRESS 118 blocks treating digest work as a primitive route. |
| typed_direct_projection | Runtime/codegen projection surface, not parse-that. |
| serde_json_oracle_read_parse | Independent oracle/comparator lane, never a parse-that primitive. |

Current local code confirms the gaps:

| Surface | Relevant source facts |
| --- | --- |
| `skinny/crates/parse-that-regex/src/lib.rs` | Hard-coded JSON whitespace (`skip_ascii_whitespace`), private plain-string scan helpers, private scalar hex helpers, and materializing `unescape_string`. |
| `skinny/crates/parse-that-regex/src/number/mod.rs` | Public JSON number span matcher plus private digit-run and digit-parse helpers. |
| `skinny/crates/bbnf-simd/src/lib.rs` | Layer 0 vocabulary already includes table/eq-set classification, prefix XOR, next-bit, bulk emit, and EOB clamp; these can support candidates but cannot carry grammar policy. |
| `skinny/crates/bbnf-simd/src/aarch64/string_block.rs` | AArch64 string-special block detector with scalar executable reference. Useful support for bounded string scans, but not itself a parse-that API. |
| `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` | Scalar and NEON `\uXXXX` helpers exist, including x4, but are shaped around Unicode escape decoding rather than a public grammar-neutral hex primitive. |
| `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs` | Digit dot-product helpers exist, but product routing needs a parse-that Layer 1 shape and checkasm parity before use. |
| `skinny/crates/runtime/src/grammars/json/generated.rs` | Generated JSON still owns local tiny string matching, delimiter/container dispatch, and direct parse call sites; it is evidence of demand, not license for JSON-only SK-V12 routing. |

SK-V12 handoff and REDRESS 120 keep generated non-JSON baseline work ahead of JSON direct residuals. This artifact therefore names primitive gaps and same-wave consumer requirements only. It does not select waves, rows, or implementation order.

## §2 — Candidate primitives

### 1. `pt_byte_set_run_skip`

Shape:

```rust
pub fn skip_byte_set_run(input: &[u8], offset: usize, set: ByteSet) -> usize
```

`ByteSet` must be a grammar-neutral description of membership, not a JSON whitespace enum. A compact shape can be a small inline set, 256-bit bitmap, or classifier table chosen by codegen before calling parse-that. The primitive returns the first non-member offset and retains no mask, cursor sidecar, or structural index.

Scalar reference sketch:

```rust
let mut cursor = offset;
while cursor < input.len() && set.contains(input[cursor]) {
    cursor += 1;
}
cursor
```

Layer placement:

| Layer | Placement |
| --- | --- |
| Layer 1 / parse-that | Public run-skip primitive used by generated grammars for whitespace, trivia, separator, or grammar-local byte classes. |
| Layer 0 / bbnf-simd | Optional backing through `byte_class_from_eq_set_64` for <=8-byte sets or `byte_class_from_table_64` for table-shaped sets. Masks are transient only. |

Arch:

| Arch | Status |
| --- | --- |
| Scalar | Required reference path and product fallback. |
| AArch64 | Eligible through existing NEON eq-set/table classifier vocabulary, subject to strict checkasm parity. |
| x86 | Existing generic vocabulary may have x86 routes, but SK-V12 handoff says no x86 implementation target. |

P1 antecedent: ascii_whitespace_skip. P1-A attributed parse samples to parse-that-regex whitespace skipping, and P1-E carried ascii_whitespace_skip as an accepted hot family.

checkasm expectation: scalar reference tests in parse-that plus `bbnf-simd` parity if a Layer 0 classifier is used. Required coverage: offset 0..63, tails shorter than one block, empty input, all-member spans, first-byte miss, last-byte miss, repeated delimiter bytes, small eq-set and full-table membership, and strict `BBNF_SIMD_STRICT=1` checkasm for the chosen kernel.

Same-wave consumer note: a legal packet must include a same-wave generated consumer such as a non-JSON layout/trivia skipper or a JSON guard call site that replaces existing code in the same wave. Telemetry-only exposure, retained cursors, or parse-only proof is insufficient.

Grammar-neutrality status: admissible if the primitive accepts byte-set data and leaves grammar-specific whitespace/comment policy to generated parsers. A JSON-only whitespace helper remains inadmissible under Lock 14.

REDRESS risks: Lock 1 sidecar risk; W3/structural cursor reopening blocked by REDRESS 50, 51, 53, 96, 97, 98, 102, 114, 119, and 120. Container or delimiter wins must not be represented as a retained structural side table.

### 2. `pt_bounded_plain_string_end`

Shape:

```rust
pub fn bounded_plain_string_end(
    input: &[u8],
    quote_or_start: usize,
    cap: usize,
    policy: PlainStringPolicy,
) -> Option<usize>
```

The return is the terminator/end offset when a plain string/literal body finishes within `cap`; otherwise `None`. `PlainStringPolicy` carries terminator byte, escape byte, minimum control byte, UTF-8 validation mode, and whether `quote_or_start` points at the opening delimiter or body start. It must not encode JSON-specific escape semantics.

Scalar reference sketch:

```rust
let mut cursor = body_start;
let limit = input.len().min(body_start.saturating_add(cap));
while cursor < limit {
    let byte = input[cursor];
    if byte == policy.terminator {
        return Some(cursor);
    }
    if byte == policy.escape || byte < policy.min_unescaped_byte {
        return None;
    }
    cursor += 1;
}
None
```

Layer placement:

| Layer | Placement |
| --- | --- |
| Layer 1 / parse-that | Public bounded plain-string end primitive for generated tiny literal/key/string paths. |
| Layer 0 / bbnf-simd | Optional support from `string_block::scan_string_special_block`, `match_tiny_plain_string`, eq-set classification, or movemask. Layer 0 remains byte scanning only. |

Arch:

| Arch | Status |
| --- | --- |
| Scalar | Required executable reference, including short caps and all tail cases. |
| AArch64 | Eligible through existing NEON string-special/tiny-string vocabulary only after strict parity. |
| x86 | Not a SK-V12 implementation target. |

P1 antecedent: bounded_plain_string_scan. P1-A and P1-E show bounded plain string scan as the dominant parse hot leaf and P1-B carried it into direct attribution.

checkasm expectation: scalar parse-that tests for every cap in the generated range, terminator at each byte, escape/control at each byte, non-ASCII before terminator, unterminated tails, input end before cap, and `quote_or_start` modes. If any Layer 0 AArch64 support is product-routed, require checkasm parity against the scalar reference with strict mode enabled; existing smoke coverage is not enough for a product primitive.

Same-wave consumer note: a legal packet must replace a generated string/literal/key path in the same wave. A JSON-only direct residual row is preblocked before generated non-JSON baseline evidence; a non-JSON string/literal consumer is the clean route if selected later.

Grammar-neutrality status: admissible only when delimiter, escape, control, and UTF-8 behavior are parameters or generated caller policy. It must not assume JSON `"` strings, JSON control rules, or JSON Unicode escape policy.

REDRESS risks: prior string routes were blocked or rejected by REDRESS 54, 55, 60-69, 72, 82, 83, 116, 117, and 119. Avoid StringBlock16 retreads, retained decoded-byte sidecars, eager materialization, and Class A tiny-string wiring without a legal same-wave consumer.

### 3. `pt_digit_run_span_accumulate`

Shape:

```rust
pub struct DigitRun {
    pub end: usize,
    pub digits: usize,
    pub prefix_value: u64,
    pub prefix_digits: u8,
    pub truncated: bool,
}

pub fn digit_run_span_accumulate(
    input: &[u8],
    offset: usize,
    max_prefix_digits: u8,
) -> DigitRun
```

The primitive scans only ASCII decimal digits. It may accumulate a bounded prefix value for generated callers that need small integer fast paths, but it does not decide sign, leading-zero, decimal point, exponent, unit suffix, or numeric type policy.

Scalar reference sketch:

```rust
let mut cursor = offset;
let mut value = 0_u64;
let mut prefix_digits = 0_u8;
while cursor < input.len() && input[cursor].is_ascii_digit() {
    if prefix_digits < max_prefix_digits {
        value = value * 10 + u64::from(input[cursor] - b'0');
        prefix_digits += 1;
    }
    cursor += 1;
}
DigitRun {
    end: cursor,
    digits: cursor - offset,
    prefix_value: value,
    prefix_digits,
    truncated: (cursor - offset) > usize::from(max_prefix_digits),
}
```

Layer placement:

| Layer | Placement |
| --- | --- |
| Layer 1 / parse-that | Public digit-run primitive below grammar-specific number recognition. JSON number matching may consume it, but it is not itself a JSON number parser. |
| Layer 0 / bbnf-simd | Optional support from digit classification and `digit_mac` dot-product helpers for bounded groups. No slot reuse or numeric materialization side table. |

Arch:

| Arch | Status |
| --- | --- |
| Scalar | Required reference and fallback. |
| AArch64 | Eligible through dotprod/digit helpers only with strict parity and a same-wave consumer. |
| x86 | Not a SK-V12 implementation target. |

P1 antecedent: number_digit_span. P1-A, P1-B, and P1-E retained numeric digit spans in parse, direct, and typed attribution.

checkasm expectation: parse-that scalar tests for zero-length runs, runs 1..128 bytes, all offsets and block boundaries, overflow/truncation boundaries, non-digit terminators, and malformed caller contexts. If AArch64 digit helpers are routed, require checkasm that compares scalar and SIMD accumulation for all 4-digit groups, mixed-length prefixes, and unaligned loads; existing primitive smoke tests do not by themselves admit product routing.

Same-wave consumer note: a legal packet must wire a same-wave generated number/literal consumer. It must not recreate the rejected JSON direct numeric slot route. Non-JSON numeric literals, if chosen by a later wave, would be a valid consumer class; this artifact selects no such wave.

Grammar-neutrality status: admissible because decimal digit runs are grammar-neutral. Any grammar policy around signs, exponents, decimal points, suffixes, units, or numeric type materialization must stay in generated code.

REDRESS risks: REDRESS 80 and 114 block generic numeric fallback and JSON numeric slot reuse routes. Avoid f64 fallback changes, mantissa widening as an unconsumed primitive, or any claim that digit-run telemetry alone moves direct results.

### 4. `pt_hex_quad_decode`

Shape:

```rust
pub enum HexQuadError {
    Short,
    Invalid { offset: usize },
}

pub fn hex_quad_decode(input: &[u8], offset: usize) -> Result<u16, HexQuadError>
```

The primitive decodes exactly four ASCII hex nibbles beginning at `offset`. A future generic `hex_run_decode<const N: usize>` can be considered only if a generated consumer needs widths other than four; this packet should keep the first shape narrow because the accepted antecedent is Unicode escape hex decode.

Scalar reference sketch:

```rust
fn nibble(byte: u8) -> Option<u16> {
    match byte {
        b'0'..=b'9' => Some(u16::from(byte - b'0')),
        b'a'..=b'f' => Some(u16::from(byte - b'a' + 10)),
        b'A'..=b'F' => Some(u16::from(byte - b'A' + 10)),
        _ => None,
    }
}

let bytes = input.get(offset..offset + 4).ok_or(HexQuadError::Short)?;
let mut value = 0_u16;
for (idx, byte) in bytes.iter().copied().enumerate() {
    value = (value << 4) | nibble(byte).ok_or(HexQuadError::Invalid {
        offset: offset + idx,
    })?;
}
Ok(value)
```

Layer placement:

| Layer | Placement |
| --- | --- |
| Layer 1 / parse-that | Public hex-quad primitive used by generated escape decoders. It returns a code unit only, not a Unicode scalar, string, or surrogate decision. |
| Layer 0 / bbnf-simd | Optional support from `unescape_uxxxx_scalar`, `unescape_uxxxx_neon`, and `unescape_uxxxx_x4_neon` only after their behavior is framed as hex decoding rather than JSON `\u` policy. |

Arch:

| Arch | Status |
| --- | --- |
| Scalar | Required reference and fallback. |
| AArch64 | Eligible through NEON table/nibble decode and x4 helpers, subject to strict checkasm and invalid-input parity. |
| x86 | Not a SK-V12 implementation target. |

P1 antecedent: unicode_escape_hex_decode. P1-A and P1-E attributed unicode escape hex decode as an accepted hot parse leaf, and current parse-that helpers are private.

checkasm expectation: exhaustive valid-domain scalar/SIMD equality for 65,536 four-nibble values is cheap and should be required for any x4/product route. Invalid-domain coverage must include each byte position, mixed valid/invalid lanes, lowercase/uppercase mixes, short tails, unaligned offsets, and first-failing-offset parity. Current `unescape_uxxxx_x4` smoke coverage is insufficient if used as a product primitive.

Same-wave consumer note: a legal packet must include a generated escape consumer in the same wave. That consumer may be JSON or non-JSON only after the SK-V12 non-JSON baseline priority is satisfied by the wave plan. A lone single-quartet proof is not enough.

Grammar-neutrality status: admissible because it only decodes four hex nibbles. Surrogate pairing, `\u` prefix handling, CSS escape width, replacement policy, and scalar validity remain generated grammar policy.

REDRESS risks: REDRESS 82, 107, and 108 block single-quartet/x4 proofs that are not consumed by a legal row. String-materialization and decoded-byte risks from REDRESS 54, 55, 60-69, 72, 116, 117, and 119 also apply.

### 5. `pt_escaped_string_segments`

Shape:

```rust
pub enum EscapedSegment<'a> {
    Raw(&'a [u8]),
    Byte(u8),
    Scalar(char),
}

pub fn escaped_string_segments<'a, F>(
    input: &'a [u8],
    body: core::ops::Range<usize>,
    policy: EscapePolicy,
    visitor: F,
) -> Result<(), EscapeError>
where
    F: FnMut(EscapedSegment<'a>) -> Result<(), EscapeError>;
```

The primitive walks an already bounded string/literal body and emits raw spans plus decoded escape segments to a caller-supplied sink. It must not allocate or force a `String`. `EscapePolicy` names simple escapes, hex escape form, control-byte validity, and UTF-8/surrogate responsibilities.

Scalar reference sketch:

```rust
let mut raw_start = body.start;
let mut cursor = body.start;
while cursor < body.end {
    match input[cursor] {
        b if b == policy.escape => {
            if raw_start < cursor {
                visitor(EscapedSegment::Raw(&input[raw_start..cursor]))?;
            }
            let decoded = decode_one_escape(input, cursor, policy)?;
            visitor(decoded.segment)?;
            cursor = decoded.end;
            raw_start = cursor;
        }
        b if b < policy.min_unescaped_byte => {
            return Err(EscapeError::Control { offset: cursor });
        }
        _ => cursor += 1,
    }
}
if raw_start < body.end {
    visitor(EscapedSegment::Raw(&input[raw_start..body.end]))?;
}
Ok(())
```

Layer placement:

| Layer | Placement |
| --- | --- |
| Layer 1 / parse-that | Public segment visitor below materialization. Existing `unescape_string` can be rebuilt on top, but the primitive itself is allocation-neutral. |
| Layer 0 / bbnf-simd | Optional support from string-special block scan, `escape_mask_64`, byte-context helpers, and `pt_hex_quad_decode`/AArch64 x4 hex support. Layer 0 never owns grammar escape policy. |

Arch:

| Arch | Status |
| --- | --- |
| Scalar | Required executable reference and oracle for all escape/error paths. |
| AArch64 | Eligible only for plain-span search and hex substeps with strict parity. |
| x86 | Not a SK-V12 implementation target. |

P1 antecedent: string_escape_decode, with unicode_escape_hex_decode as a sub-antecedent. P1-B carried string escape decode in direct attribution, and P1-A/P1-E kept escape and unicode hex leaves live.

checkasm expectation: parse-that scalar tests must cover no escapes, dense escapes, every simple escape, invalid escapes, control bytes, unterminated escapes, invalid UTF-8 according to policy, valid and invalid hex escapes, surrogate-policy handoff, and visitor error propagation. If AArch64 support is routed, require strict checkasm for each Layer 0 subprimitive plus end-to-end parse-that parity against the scalar segment stream.

Same-wave consumer note: a legal packet must have a same-wave consumer that uses the segment stream for a generated direct/typed/non-JSON output path. Replacing only `unescape_string` internals without a product consumer does not satisfy S-P2 CH4.

Grammar-neutrality status: admissible if the escape table and scalar policy are caller-supplied. JSON string semantics, CSS escape length, Sheets quoting, or BBNF literal rules must remain outside the generic crate.

REDRESS risks: prior decoded-byte, retained segment, eager materialization, and digest-adjacent string routes are blocked by REDRESS 54, 55, 60-69, 72, 82, 83, 116, 117, 118, and 119. The primitive cannot introduce a decoded sidecar, output statistics lane, or host-sink dependency.

## §3 — Grammar-neutrality

| Candidate | Grammar-neutrality status |
| --- | --- |
| `pt_byte_set_run_skip` | Neutral if set data is supplied by generated code and no built-in JSON whitespace/comment policy exists. |
| `pt_bounded_plain_string_end` | Neutral if delimiter, escape, control, cap, and UTF-8 handling are parameters. |
| `pt_digit_run_span_accumulate` | Neutral because it scans ASCII digit runs only; number grammar remains generated policy. |
| `pt_hex_quad_decode` | Neutral because it returns a four-nibble code unit only; Unicode scalar/surrogate decisions stay outside. |
| `pt_escaped_string_segments` | Neutral if escape tables and scalar validity policy are caller supplied and no allocation/materialization policy is forced. |

The common rule is that parse-that may expose byte/string/digit/hex mechanics, while generated grammar code owns syntax, policy, and output projection. Lock 14 rejects any helper whose name or behavior bakes JSON, CSS, Sheets, or BBNF-self semantics into `parse-that-regex`, `parse-that`, or `bbnf-simd`.

Lock 1 also constrains every candidate: SIMD masks, segment boundaries, and classifier results are transient implementation details unless emitted into the canonical tape by the existing substrate. No candidate here admits a parallel structural stream, decoded-byte cache, retained cursor list, or grammar-specific sidecar.

## §4 — Risks

1. JSON direct residual reopening. SK-V12 HANDOFF and REDRESS 120 route the cycle to generated non-JSON baseline work before JSON direct residual work. These candidates may explain JSON hot leaves, but they do not authorize JSON-only direct wave selection.

2. Same-wave consumer requirement. S-P2 CH4 requires scalar reference status, checkasm-parity expectation, and same-wave consumer notes. A primitive proof without a product consumer is research inventory only.

3. SIMD proof overreach. `simd_movemask`, `string_block`, `digit_mac`, and `unescape_uxxxx_x4` are support primitives. They need strict checkasm parity and a Layer 1 consumer before any product claim. Existing smoke tests are not sufficient where this artifact calls out broader parity.

4. REDRESS preblocks:

| Risk family | Relevant REDRESS guardrails |
| --- | --- |
| Structural/sidecar/container cursor routes | 50, 51, 53, 96, 97, 98, 102, 114, 119, 120 |
| String/escape/materialization/decoded-byte routes | 54, 55, 60-69, 72, 82, 83, 106, 107, 108, 116, 117, 119 |
| Numeric fallback or slot reuse | 80, 114 |
| Container parser-control shortcuts | 63, 65, 84, 115 |
| Movemask/body-fill-only microproofs | 88, 89, 90 |
| Digest/host-sink paths | 118 |
| Non-JSON report/baseline confusion | 111, 112, 113 |

5. Lock 16 scope. AArch64 primitives are allowed only inside the established allowlist and must remain grammar-neutral. x86 may remain in existing generic code, but SK-V12 should not target new x86 implementation work.

## §5 — Sources

Read inputs:

| Source | Use |
| --- | --- |
| `restart/prompts/skinny/PASS-2-RESEARCH.md` | Output contract, P2-E scope, CH4/CH5 requirements. |
| `restart/skinny/tranches/sk-v12/HANDOFF.md` | SK-V12 constraints, non-JSON-first route, refusal conditions. |
| `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md` | Accepted S-P1 hot families and current go/no-go surface. |
| `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md` | Parse-only source map and primitive family attribution. |
| `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md` | Direct/typed product hot families. |
| `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md` | Mode III absence boundary and W0 diagnostic status. |
| `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md` | PMU/cycles context. |
| `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md` | Accepted hot-leaf distribution, source loci, non-JSON baseline blocker, REDRESS preblocks. |
| `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md` | RESULTS/REDRESS mapping and unchanged surface. |
| `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md` | Capture provenance and replay ledger status. |
| `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` | Authoritative replay command surface. |
| `skinny/RESULTS.md` | Current benchmark admission state. |
| `skinny/REDRESS.md` | Guardrails and rejected prior routes. |
| `restart/locks/LOCKS.md` | Lock 1, Lock 14, and Lock 16 constraints. |
| `skinny/crates/parse-that-regex` | Existing parse-that-regex primitive surface and private helpers. |
| `skinny/crates/bbnf-simd` | Existing Layer 0 SIMD/scalar vocabulary and checkasm shape. |
| `skinny/crates/runtime/src/grammars/json/generated.rs` | Runtime generated JSON source demand and local helper evidence. |
| `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md` | Prior P2 context only; SK-V12 conclusions are based on SK-V12 reads above. |

No external sources were used.

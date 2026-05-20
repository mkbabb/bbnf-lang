# W5 R1: Parse-That String-Span Primitives

Pass: W5 Phase 1 research.
Date: 2026-05-20.
Scope: R1, parse-that string-span primitives for bounded string span and
special-byte scan.
Output: this file only.

## Finding

W5 should treat the parse-that string-span work as a scalar-first factoring of
existing offset-producing string behavior, not as a new retained string
substrate. SPEC Section 9 names C2 bounded special-byte scan, P2-D D3
`borrowed_string_span`, and P2-E `pt_bounded_plain_string_end`; its entry gate
requires CHALLENGE to select the scalar span shape, exactly one string/key
caller, a cap, and at most two target rows before behavior work
(`SPEC.md:540`-`561`). The required task is an oracle returning offsets and
decode-needed status, with optional SIMD only after strict parity and caller
microbench (`SPEC.md:563`-`568`).

The viable R1 boundary is:

- expose a small borrowed span result with `content_start`, `content_end`,
  `raw_end`, and `needs_decode`;
- add or reuse a bounded plain-string helper that returns that span only when a
  closing delimiter is found inside the selected cap before any escape/control
  byte;
- on miss, leave the cursor unchanged and fall back to the existing full
  parse-that string matcher;
- keep decoded materialization, surrogate policy, and grammar-specific escape
  rules outside the W5 primitive.

This matches P2-D D3's consumer primitive shape and proof gate:
`{ content_start, content_end, raw_end, needs_decode }`; retained parse is only
a guard/micro-proof surface; generated direct and typed consumers are the
eligible product planes (`p2d-substrate-tape.md:34`, `p2d-substrate-tape.md:46`).
It also matches P2-E's bounded helper shape and scalar-first fallback rule
(`p2e-parse-that-gaps.md:48`, `p2e-parse-that-gaps.md:62`-`68`).

## Current Scalar References

Current parse-that already has most of the semantic reference:

- `StringFlags` records `HAS_ESC`, `HAS_CONTROL`, `HAS_NON_ASCII`,
  `NEEDS_DECODE`, and `UTF8_VALIDATED`; `StringMatch` records `raw_start`,
  `raw_end`, and exposes `content_start`, `content_end`, and `needs_decode`
  (`skinny/crates/parse-that-regex/src/lib.rs:50`-`108`).
- `match_string_at_quote_trusted_utf8` scans from an opening quote over trusted
  UTF-8 input, returns `StringMatch`, marks `NEEDS_DECODE` only after an
  escape, rejects controls, and reports unterminated strings at the opening
  offset (`lib.rs:157`-`209`).
- `match_string_at_quote` is the validating form for `Utf8`, `GrammarString`,
  and `ByteString`; it validates raw UTF-8 when the mode requires it, preserves
  byte-string non-validation, validates escapes, and reports controls and
  invalid UTF-8 at current offsets (`lib.rs:211`-`281`).
- `skip_string_plain` and `skip_string_plain_trusted` are the local plain-scan
  references. They stop at quote, slash, or control; the validating path also
  tracks non-ASCII and validates UTF-8 (`lib.rs:462`-`543`,
  `lib.rs:546`-`573`). The scalar `string_special_mask` defines quote/slash/
  control detection over 8-byte blocks (`lib.rs:576`-`587`).
- `classify_string_content` and `unescape_string` are materialization-side
  references only. They prove current escape/control behavior and lazy borrowed
  return, but W5 must not turn them into a decoded scratch or segment side table
  (`lib.rs:634`-`662`, `lib.rs:718`-`725`).

Current generated and bench consumers already contain the bounded plain loops
that R1 should factor:

- generated retained JSON has `match_tiny_plain_string_with_cap`, used with cap
  16 for retained strings and cap 8 for direct strings (`generated.rs:161`-`185`);
- generated direct JSON uses `ParsedString { raw, needs_unescape }`,
  `parse_string_direct`, `sink.key_source`, and string/source callbacks
  (`generated.rs:402`-`405`, `generated.rs:548`-`564`,
  `generated.rs:608`-`640`);
- `direct_struct` has an 8-byte tiny plain-string loop before the trusted
  parse-that matcher and `unescape_string` fallback (`direct_struct.rs:541`-`561`,
  `direct_struct.rs:564`-`576`);
- generated typed has cap 32 for `parse_string` and cap 96 for
  `skip_string_raw`, both falling back to the trusted parse-that matcher
  (`generated_real_typed.rs:1648`-`1670`,
  `generated_real_typed.rs:1795`-`1835`);
- retained view materialization uses existing sparse `HAS_ESC` flags and lazily
  calls `unescape_string`; this remains a guard, not the W5 admission plane
  (`view.rs:199`-`216`).

The current tests already cover escape state, raw offsets, UTF-8 modes, dense
unicode escapes, invalid UTF-8, invalid escapes, surrogate pairs, error
offsets, controls, and unescape behavior (`lib.rs:994`-`1213`). W5 should add
new cases around cap and offset boundaries rather than weakening those tests.

## Candidate API Shape

Preferred scalar shape:

```rust
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BorrowedStringSpan {
    pub content_start: usize,
    pub content_end: usize,
    pub raw_end: usize,
    pub needs_decode: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct StringSpanSpec {
    pub delimiter: u8,
    pub escape: u8,
    pub control_limit: u8,
}

pub fn bounded_plain_string_span_at_quote(
    input: &[u8],
    quote_offset: usize,
    max_bytes_after_quote: usize,
    spec: StringSpanSpec,
) -> Option<BorrowedStringSpan>;
```

Semantics:

- `quote_offset` is the delimiter byte's offset; callers that have already
  dispatched on the delimiter can use an `at_quote` form with a debug assertion.
- `max_bytes_after_quote` includes the closing delimiter byte in the examined
  window. This mirrors current loops that set `limit = offset + 1 + CAP` and
  scan `cursor < limit`; spelling this out avoids cap off-by-one drift.
- Return `Some(span)` only when the closing delimiter appears inside the cap
  before any escape or byte `< control_limit`; `needs_decode` is always `false`
  for this bounded plain helper.
- Return `None` on escape, control, missing delimiter, delimiter beyond cap, or
  unsupported delimiter precondition. The caller must then run the existing full
  matcher and must not advance the cursor on the `None` path.
- Keep all fields as offsets into the original input. Do not return `&str`,
  `Cow`, decoded bytes, or a retained block wrapper from parse-that.

If W5 wants a single consumer-facing span type for the full matcher, add a
lossless conversion from current `StringMatch` to `BorrowedStringSpan` or expose
a thin wrapper:

```rust
pub fn borrowed_string_span_at_quote_trusted_utf8(
    input: &[u8],
    quote_offset: usize,
) -> Result<BorrowedStringSpan, RegexError>;
```

That wrapper should delegate to `match_string_at_quote_trusted_utf8` and map
`raw_start + 1`, `raw_end - 1`, `raw_end`, and `needs_decode()`. It should not
fork the full JSON string validator.

## Strict Correctness Cases

The scalar oracle must lock these cases before any caller routing:

- empty string: `""` returns `content_start == content_end == quote_offset + 1`,
  `raw_end == quote_offset + 2`, `needs_decode == false`;
- tiny/plain strings at every selected cap boundary: close before cap returns
  span; close at the first byte outside the cap returns `None` and leaves the
  full matcher responsible for success;
- offsets: valid strings starting at non-zero offsets produce absolute
  `content_start`, `content_end`, and `raw_end`; invalid paths keep current
  error offsets from the full matcher;
- escape before delimiter: bounded helper returns `None`; full matcher returns
  `needs_decode == true` for valid escapes and current `InvalidEscape` or
  unicode/surrogate errors for invalid escapes;
- control before delimiter: bounded helper returns `None`; full matcher reports
  `ControlCharacter` at the control byte;
- unterminated input and tail shorter than the cap: bounded helper returns
  `None`; full matcher reports `UnterminatedString` at the opening quote;
- trusted UTF-8 input: raw non-ASCII bytes are not special for the bounded
  helper; the caller's trusted input invariant owns UTF-8 validity;
- validating modes: current `Utf8`/`GrammarString` invalid UTF-8 rejection and
  `ByteString` non-validation must remain unchanged;
- delimiter, escape, and control-limit parameterization: JSON uses `"`, `\`,
  and `0x20`, but the helper must not bake JSON policy into generic code;
- overflow and tails: `quote_offset + 1 + cap` must be computed without
  wrapping and no read may pass `input.len()`.

If an AArch64 block body is proposed later, strict parity must cover every quote
offset, cap, alignment, escape/control byte position, non-ASCII placement, and
tail length before caller row evidence counts. P3-A explicitly rejects
primitive parity alone and requires caller microbench evidence for the exact
selected string/key loop (`p3a-candidate-shortlist.md:157`-`166`).

## Consumer Requirements

W5 cannot admit a parse-only or helper-only win. The consumer packet must name:

- one generated direct or typed string/key caller selected by CHALLENGE;
- at most two target rows from the W5 string-heavy set:
  `twitter >= 13740`, `github_events >= 13403`,
  `update_center >= 10059`, `random >= 7878`, `gsoc-2018 >= 3737`,
  `distinct_values >= 2658`, and `y_string_unicode >= 3950`
  (`p3b-wave-sequencing.md:109`);
- generated Track 1 and independent Track 2/oracle on the same output plane;
- scalar/no-op fallback to the current local tiny loop plus full parse-that
  matcher;
- `cargo test -p parse-that-regex` for scalar/product parity, and
  `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --tests` if a native body is
  routed (`p2e-parse-that-gaps.md:37`-`43`);
- direct and typed guard floors from SPEC Section 0.5 hold
  (`SPEC.md:137`-`157`).

Preferred first consumer is generated direct JSON object keys:
`parse_object_direct` calls `parse_string_direct` then `sink.key_source`, and
`parse_string_direct` already has the exact `raw` plus `needs_unescape` shape
needed by `BorrowedStringSpan` (`generated.rs:548`-`564`,
`generated.rs:608`-`640`). This is narrower than changing every retained,
direct, typed, and bench-local string loop at once. A typed consumer is valid
only if the same wave names it explicitly and proves the typed output equality.

If this API touches generic codegen or grammar-neutral parse-that surface beyond
JSON-owned generated code, the same wave needs the existing non-JSON string or
literal proof path. P3-B sequences W5 after W2 because generic C1-C7 claims
need exercised non-JSON generality, not prose (`p3b-wave-sequencing.md:69`,
`p3b-wave-sequencing.md:85`-`88`).

## Likely Reject Boundary

Reject or keep proof-only if any of these happen:

- no same-wave generated direct/typed string/key consumer is wired;
- selected direct rows do not clear their floors, or no selected target shows
  at least the P2-E minimum useful movement with guard no-regression
  (`p2e-parse-that-gaps.md:43`);
- Track 2 depends on generated Track 1 or a hidden shared sidecar;
- the helper returns decoded data, `Cow`, retained string side tables, decoded
  scratch, semantic string-field facts, or a `StringBlock16`/wide-scan retained
  wrapper;
- the change reuses REDRESS-preblocked string materialization routes,
  primitive-parity-only production, 64-byte retained scans, or W6 escape/x4
  proof-to-production routes (`SPEC.md:580`-`582`,
  `p2e-parse-that-gaps.md:88`-`96`);
- JSON escape or surrogate policy moves into a grammar-neutral bounded helper;
- cap semantics are ambiguous or differ between generated, direct_struct, and
  typed callers;
- any existing string correctness case changes, including `ByteString` raw UTF-8
  behavior or current invalid escape/error offsets;
- generic/codegen changes land without the same-wave non-JSON literal/string
  proof required by Lock 14 and W5.

Downstream W6 may consume the span API only if W5 admits it or CHALLENGE
accepts a compatible rejected-but-reusable scalar proof with no behavior source,
per SPEC Section 9 (`SPEC.md:589`-`590`).

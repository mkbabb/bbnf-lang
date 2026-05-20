# SK-V11 W5 CH1 Correctness Challenge

Date: 2026-05-20.

Disposition: REVISE.

The scalar bounded string-span route is not rejected on semantic grounds. A
JSON-local direct helper returning `{ content_start, content_end, raw_end,
needs_decode }` can preserve current valid-string behavior, cap semantics, and
fallback escape/control handling. The W5 plan is not acceptable as written
because it does not lock the opening-delimiter precondition on the selected
object-key caller, leaves the helper path insufficiently testable at the cap
boundary, and needs tighter malformed-string parity obligations before source
redress.

## Evidence Read

- SPEC Section 9 requires a scalar span oracle returning offsets and
  decode-needed status, one generated direct/typed string/key consumer, a
  selected cap, at most two target rows, Unicode residual monitoring, and no
  decoded scratch, retained string side table, retained `StringBlock16`
  wrapper, primitive-only production, or retained 64-byte scan
  (`restart/skinny/tranches/sk-v11/SPEC.md:540`-`587`).
- R1 defines the intended helper contract: an `at_quote` span helper whose cap
  includes the closing delimiter byte, returns `Some` only for a closing
  delimiter before escape/control, leaves the caller cursor unchanged on
  `None`, preserves absolute offsets, and computes cap/tail limits without
  wrapping
  (`restart/skinny/tranches/sk-v11/research/w5/w5-R1-parse-that-string-span.md:119`-`176`).
- The W5 plan selects generated direct `parse_string_direct`, object keys via
  `sink.key_source`, cap 8, and exactly `random/direct_to_struct/main` as the
  row gate
  (`restart/skinny/tranches/sk-v11/research/w5/w5-plan-string-span-implementation.md:19`-`38`).
- Current generated direct object parsing calls `parse_string_direct` directly
  for keys after whitespace, without prior quote dispatch
  (`skinny/crates/runtime/src/grammars/json/generated.rs:554`-`564`).
- Current generated direct `parse_string_direct` immediately tries the tiny
  helper and then calls `match_string_at_quote_trusted_utf8`; neither path has a
  release-mode opening-quote check
  (`skinny/crates/runtime/src/grammars/json/generated.rs:610`-`640`,
  `skinny/crates/parse-that-regex/src/lib.rs:162`-`206`).
- The renderer has the same shape, so redress must fix
  `skinny/crates/codegen/src/sink_direct.rs`, not hand-patch generated output
  (`skinny/crates/codegen/src/sink_direct.rs:319`-`350`).
- Current hand Track 2 `direct_struct` mirrors the selected object-key path and
  also calls its string helper without an opening-quote check
  (`skinny/crates/bbnf-bench/src/direct_struct.rs:483`-`562`).
- Other local references already show the safer contract: the retained Track 2
  JSON key path consumes a quote before scanning, and the typed generated parser
  checks `bytes.get(cursor) == Some(&b'"')` before its tiny path
  (`skinny/crates/bbnf-bench/src/track2/json.rs:96`-`120`,
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1648`-`1668`).

## Assessment

Helper shape: conditionally sound. The selected JSON-local helper can be a
plane-specific specialization of R1's span shape without editing
`parse-that-regex`, as long as it remains private to generated direct SinkOnly
code and does not claim generic non-JSON authority. For the bounded plain path,
`needs_decode` must always be `false`; fallback to the trusted parse-that
matcher must remain the only route for escaped strings.

Cap semantics: conditionally sound. The plan's cap-8 wording matches current
direct behavior: scanning starts at `offset + 1`, examines eight bytes while
`cursor < offset + 1 + 8`, admits content lengths 0 through 7, and leaves
content length 8 to the fallback matcher
(`skinny/crates/runtime/src/grammars/json/generated.rs:161`-`185`). This must
be tested at non-zero offsets and through both object keys and string values.

Cursor semantics: conditionally sound. On helper miss for escape, control,
unterminated input, cap miss, short tail, overflow risk, or unsupported
precondition, `parse_string_direct` and hand Track 2 must leave the cursor at
the opening quote until the fallback or explicit error path owns the result.
The bounded helper itself must not advance `cursor` on `None`.

Malformed behavior: not locked. The selected object-key caller does not
dispatch on a quote before calling `parse_string_direct`, and the current
fallback is an `at_quote` matcher with only a debug assertion. A revised plan
must require a release-mode opening-delimiter guard before the bounded helper
and before fallback:

```rust
let start = *cursor;
if bytes.get(start) != Some(&b'"') {
    return Err(direct_error(input, start, ParseErrorKind::ExpectedValue));
}
```

The hand Track 2 parser needs the analogous guard before its tiny path. Without
this, malformed object-key inputs can be accepted or misclassified if a quote
appears shortly after a non-quote byte, and generated/direct parity may stay
coupled on the same bug instead of proving JSON correctness against serde/sonic.

Generated/direct parity: under-specified for invalid strings. The plan requires
valid digest equality and says malformed string fixtures must reject, but CH1
needs exact fixture classes and both Track 1 and independent Track 2 rejection.
For invalid strings, serde_json and sonic-rs should be rejection oracles, not
only same-shape comparators after valid parsing.

## Required Revision Before Redress

Revise the plan to state this exact generated direct contract:

```rust
fn parse_string_direct<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
) -> Result<ParsedString<'i>, ParseError<'i>> {
    let start = *cursor;
    if bytes.get(start) != Some(&b'"') {
        return Err(direct_error(input, start, ParseErrorKind::ExpectedValue));
    }
    if let Some(span) = bounded_plain_string_span_direct(bytes, start, 8) {
        *cursor = span.raw_end;
        return Ok(ParsedString {
            raw: unsafe {
                std::str::from_utf8_unchecked(&bytes[span.content_start..span.content_end])
            },
            needs_unescape: false,
        });
    }
    // existing match_string_at_quote_trusted_utf8 mapping
}
```

The helper contract must say:

- `start` is the opening quote offset and must already be validated by the
  caller.
- Cap 8 includes the closing quote byte in the examined window.
- Return `Some` only when the close quote appears within cap before `\` or a
  control byte `< 0x20`.
- Return `None` on escape, control, cap miss, missing close, short tail, or
  overflow risk.
- Produce absolute offsets: `content_start = start + 1`, `content_end =
  close`, `raw_end = close + 1`.
- Never decode, allocate, retain side state, or alter parse-that escape and
  surrogate policy.

The hand Track 2 parser in `direct_struct.rs` must independently implement the
same observable contract, including the opening-quote guard and cap boundary,
without calling generated Track 1 helpers or generated span symbols.

## Required Tests Before Measurement

Add generated Track 1 and hand Track 2 tests that run before any probe or
Criterion measurement:

- Valid plain strings: `""`, one-byte, seven-byte content, eight-byte content,
  and longer content at non-zero offsets, through both object keys and root,
  object, and array string values.
- Cap boundary: content length 7 takes the bounded path; content length 8
  falls back but returns the same raw slice, cursor `raw_end`, and
  `needs_unescape = false`.
- Escape behavior: escape before the close returns through fallback with
  `needs_unescape = true`; invalid escape, invalid unicode escape, and invalid
  surrogate pair reject with the existing parse-that offset.
- Control behavior: control before close rejects at the control byte after
  fallback, and the cursor is not advanced by the bounded helper miss.
- Unterminated behavior: unterminated string and tail shorter than cap reject at
  the opening quote under the existing `InvalidString` mapping.
- Opening delimiter: object-key inputs such as `{a":1}`, `{a:1}`, `{ :1}`,
  `{, "b":1}`, and EOF after `{` reject in generated Track 1, hand Track 2,
  serde_json, and sonic-rs, with generated Track 1 reporting
  `ParseErrorKind::ExpectedValue` at the key cursor.
- Non-ASCII trusted input: raw UTF-8 bytes inside plain strings are not treated
  as special by the bounded helper, and valid generated Track 1 / Track 2
  digests match serde_json and sonic-rs.
- Cursor preservation: after every helper `None` case, fallback success or
  error must be attributable to the original quote offset, not to a partially
  advanced scan cursor.
- Independence: source-level or symbol-level tests must prove hand Track 2 does
  not call `runtime::generated_json`, `parse_string_direct`, or any generated
  bounded-span helper.

Product parity tests must distinguish valid-output equality from invalid-input
rejection. Valid rows require exact generated Track 1 versus hand Track 2 digest
equality with serde_json and sonic-rs as same-run shape comparators under the
existing contract. Malformed string fixtures require all four parsers to reject;
a shared generated/hand acceptance bug is not admissible evidence.

## CH1 Disposition

The W5 scalar plan can return to ACCEPT after it adds the opening-quote guard,
states the exact helper contract above, mirrors the guard independently in hand
Track 2, and names the malformed-string fixture set as pre-measurement tests.
As written, it is REVISE, not REJECT.

DISPOSITION: REVISE

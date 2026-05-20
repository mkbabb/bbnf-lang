# SK-V11 W6 CH1 Correctness - Escaped Segment Digest Fold

Date: 2026-05-20.
Scope: CH1 correctness review for
`restart/skinny/tranches/sk-v11/research/w6/w6-plan-escaped-segment-digest-fold.md`.
Output: this file only.

## Verdict

REVISE.

The selected direction can be made correctness-complete, but the current plan
does not yet bind enough of the decoded-byte contract to authorize source
redress. The main gap is not source-method reachability: overriding
`JsonDigestSink::{key_source,string_source,array_string_source,object_string_source}`
inside `skinny/crates/bbnf-bench/src/direct_struct.rs` is a real Track 1
consumer path without editing `runtime/src/grammars/json/sink.rs`. The gap is
that the plan still leaves the escaped decoder/folder semantics, Track 2
independence decision, and serde/sonic oracle strength partly open.

W6 may proceed to redress only after the plan is revised with the required
changes below.

## Required Changes

1. Add a line-cited source-method coverage table.

   The plan must explicitly bind all generated Track 1 string contexts to the
   four sink overrides:

   | Context | Generated caller | Required override |
   |---|---|---|
   | Root string value | `skinny/crates/runtime/src/grammars/json/generated.rs:440-443` | `string_source` |
   | Object string value | `skinny/crates/runtime/src/grammars/json/generated.rs:480-483` | `object_string_source` |
   | Array string value | `skinny/crates/runtime/src/grammars/json/generated.rs:520-523` | `array_string_source` |
   | Object key | `skinny/crates/runtime/src/grammars/json/generated.rs:561-564` | `key_source` |

   The current trait defaults allocate through `unescape_string`
   (`skinny/crates/runtime/src/grammars/json/sink.rs:17-35`,
   `:44-51`, `:85-92`), and the current bench sink does not override any
   source method (`skinny/crates/bbnf-bench/src/direct_struct.rs:259-399`).
   The revision must state that redress implements all four overrides or
   rejects before measurement.

2. Specify the decoded-byte fold contract before implementation.

   The plan must name the exact scalar contract used by the local fold helper:

   - Raw segments are hashed byte-for-byte as UTF-8.
   - Simple escapes produce exactly JSON decoded bytes for `"`, `\`, `/`,
     backspace, form-feed, newline, carriage-return, and tab.
   - Unicode escapes decode four hex nibbles, join valid high/low surrogate
     pairs, reject lone high surrogates, reject lone low surrogates, reject
     invalid hex, and encode accepted scalar values to UTF-8 before updating
     `string_bytes` and `fingerprint`.
   - Control characters before the closing quote remain parser rejection cases,
     not sink-side acceptances.
   - JSON surrogate policy may live only in the JSON direct output consumer
     selected by W6; no generic parse-that or SIMD API may present it as a
     grammar-neutral rule.

   It is acceptable for the sink override to rely on
   `match_string_at_quote_trusted_utf8` having validated generated Track 1 raw
   slices (`skinny/crates/runtime/src/grammars/json/generated.rs:624-640`;
   `skinny/crates/parse-that-regex/src/lib.rs:162-209`), but the fold helper's
   own unit tests must still compare valid and invalid raw escaped segments
   against `unescape_string` or a clearly named scalar oracle. Otherwise a bug
   in the no-allocation folder could pass production fixtures only because the
   parser pre-filtered malformed input.

3. Strengthen serde/sonic agreement to full escaped-fixture equality.

   The existing direct contract is not sufficient for W6 correctness. Today
   `assert_direct_struct_parity` requires exact Track 1 == Track 2 equality,
   but compares serde/sonic only by shape
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:420-425`,
   `:190-202`). W6's escaped-string fixtures must add a stricter oracle that
   asserts full `JsonDirectDigest` equality, including `string_bytes` and
   `fingerprint`, across generated Track 1, hand Track 2, `serde_json`, and
   `sonic-rs`.

   This is mandatory because a shared or duplicated escaped-byte folder can
   compute the wrong decoded hash while still preserving object/array/string
   counts. Shape-only serde/sonic checks would miss that class of bug.

4. Make the Track 2 independence choice explicit.

   CH1 accepts a shared local pure fold helper in `direct_struct.rs` only if:

   - Track 2 keeps independent parser control (`HandParser::value_at`,
     `object`, `array`, and `string` remain the parsing route at
     `skinny/crates/bbnf-bench/src/direct_struct.rs:465-560`).
   - The shared helper is output-plane only: no cursor movement, no generated
     parser call, no generated sink helper call, no hidden shared parser.
   - The strict escaped-fixture oracle in item 3 compares full digests against
     serde and sonic.
   - A source-level test or runtime guard proves the Track 2 escaped-string path
     does not call `runtime::generated_json::parse_direct` or generated
     source-method helpers.

   If the plan chooses duplicated Track 1 and Track 2 fold implementations
   instead, it must require cross-implementation tests for every fixture class
   in item 5. Leaving this decision to redress is not acceptable.

5. Expand malformed and context fixtures into a binding table.

   The plan's current fixture list is directionally right but not yet binding.
   The revision must require release-mode tests covering at least:

   - root string, object string value, array string value, and object key;
   - escaped quote, escaped backslash, escaped slash, `\b`, `\f`, `\n`, `\r`,
     `\t`;
   - valid BMP Unicode scalar, valid non-BMP surrogate pair, adjacent surrogate
     pairs, escaped-backslash text that looks like `uXXXX` but is data;
   - invalid escape letter, invalid hex in each nibble position, lone high
     surrogate, lone low surrogate, high surrogate followed by non-low
     surrogate, unterminated string, and unescaped control byte before close.

   Valid fixtures must assert full Track 1 == Track 2 == serde == sonic digest
   equality. Invalid fixtures must assert all four implementations reject. For
   generated Track 1 and hand Track 2, internal rejection should preserve the
   existing string-error class; exact serde/sonic error text does not need to
   match.

6. Add file:line citations for correctness claims.

   `SKINNY-TRIUMVIRATE.md` CH1 asks whether the plan cites file:line for every
   correctness claim. The current plan names many paths but rarely cites line
   numbers. The revised plan must cite the generated callers, trait defaults,
   current bench sink, hand Track 2 string parser, serde/sonic digest visitors,
   and parse-that validation functions it relies on.

## Acceptance Conditions After Revision

CH1 will accept the W6 plan if the revised plan:

- implements or requires all four Track 1 source-method overrides;
- states the decoded-byte fold semantics and surrogate policy precisely;
- strengthens W6 escaped fixtures to full digest equality against serde and
  sonic;
- makes Track 2 independence explicit rather than deferring it to redress;
- binds malformed-input oracles for valid/invalid escape and surrogate cases;
- keeps the change inside `direct_struct.rs` plus tests/gate/report/REDRESS,
  with no `runtime/src/grammars/json/sink.rs` edit and no generic JSON policy
  leak.

Until those revisions land, the plan remains too under-specified for a
correctness-complete W6 redress.

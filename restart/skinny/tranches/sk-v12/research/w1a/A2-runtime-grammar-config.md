# SK-V12 W1a A2 - Runtime GrammarConfig Research

Scope: runtime generic config plus tape/value surface under
`skinny/crates/runtime/src/tape/**` and
`skinny/crates/runtime/src/grammars/json/**`. No source, staging, or commit
changes were made for this research output.

## Authority read

- W1a purpose is to make CSS L4 emission legal before any CSS generated parser
  is emitted: `restart/skinny/tranches/sk-v12/SPEC.md:314-317`.
- W1a tasks require `GrammarConfig` or equivalent generated metadata for
  structural alphabet, FIRST/follow tables, layout/trivia, escape policy,
  number policy, flag semantics, and sink/view/kind bindings:
  `restart/skinny/tranches/sk-v12/SPEC.md:332-339`.
- W1a exit forbids a new directive, BIR variant, `BackendShape`, or public
  substrate API: `restart/skinny/tranches/sk-v12/SPEC.md:341-347`.
- Lock 14 forbids JSON structural alphabet, string escape policy, number
  policy, object key policy, JSON `OffsetFlags` meaning, and `JsonSink` method
  shape in generic code: `restart/skinny/tranches/sk-v12/SPEC.md:261-270`.
- The user pin makes CSS L4 authoritative and requires semantic parity with
  lightningcss, then `lightningcss_mbps + 1` throughput:
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`.
- The pin carries Lock 14 and says the seven leaks in the value API audit must
  be resolved by W1's `GrammarConfig` surface before CSS L4 emission is legal:
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:90-106`.
- The value API audit names 5 major plus 2 embedded leaks:
  `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-107`.
- The same audit says Tape, ValueRef, TapeBuilder, OffsetFlags, ValueRef
  lifetimes, and PayloadArena should remain unchanged for the minimal route:
  `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:160-207`.

## Current runtime JSON policy placement

### Tape substrate

- `skinny/crates/runtime/src/tape/mod.rs:16-24` defines `OffsetFlags` plus
  `HAS_ESC` and `HAS_CONTROL`. The storage type is generic, but the exported
  bit names carry JSON string-escape/control meaning.
- `skinny/crates/runtime/src/tape/mod.rs:94-100` stores source bytes, offsets,
  sparse flag cursors/values, payloads, and tape id without grammar config.
- `skinny/crates/runtime/src/tape/mod.rs:144-150` returns opaque
  `OffsetFlags` from `flags_at`; interpretation is external.
- `skinny/crates/runtime/src/tape/mod.rs:175-180` gives `ValueRef` a grammar
  phantom `G: EventGrammar`, but current JSON wrappers mostly use the default
  `AnyGrammar`.
- `skinny/crates/runtime/src/tape/mod.rs:227-232` exposes `DocumentView` as
  root/tape/source only, with no grammar policy hook.
- `skinny/crates/runtime/src/tape/assembler.rs:42-48` defines `TapeBuilder`
  as source plus offset/flag/payload vectors, not a grammar-parametric builder.
- `skinny/crates/runtime/src/tape/assembler.rs:61-67` pushes offsets and
  patches flags only when bits are nonzero; no semantic flag interpretation is
  in the builder.
- `skinny/crates/runtime/src/tape/assembler.rs:94-113` requires flag patches
  in cursor order and stores raw flag bits.
- `skinny/crates/runtime/src/tape/event_grammar.rs:4-14` has an existing
  `EventGrammar` hook for structural class count and facts, but it has no
  alphabet, layout, escape, number, sink, or view binding surface.

### JSON generated parser and scanner

- `skinny/crates/runtime/src/grammars/json/generated.rs:10-15` places the JSON
  structural alphabet `b"{}[],:\""` and a JSON-only debug assertion in the
  generated parser.
- `skinny/crates/runtime/src/grammars/json/scan.rs:6-20` duplicates the JSON
  structural alphabet and a 64-entry low-bit table for `"`, `,`, `:`, `[`,
  `]`, `{`, and `}`.
- `skinny/crates/runtime/src/grammars/json/scan.rs:96-105` counts the exact
  structural set as `{`, `}`, `[`, `]`, `,`, `:`, and `"`.
- `skinny/crates/runtime/src/grammars/json/scan.rs:130-160` implements
  JSON-specific string masking: quote toggles string state, backslash escapes,
  and only JSON punctuation emits outside strings.
- `skinny/crates/runtime/src/grammars/json/scan.rs:164-198` carries the same
  quote/backslash string-body policy for 64-bit masks.
- `skinny/crates/runtime/src/grammars/json/scan.rs:213-233` passes JSON quote,
  JSON backslash, and control threshold `0x20` into the aarch64 classifier.
- `skinny/crates/runtime/src/grammars/json/generated.rs:47-58` dispatches JSON
  values from FIRST bytes `{`, `[`, `"`, `-`, `0..=9`, `t`, `f`, and `n`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:62-79` parses JSON
  objects as `{ ... }` containers.
- `skinny/crates/runtime/src/grammars/json/generated.rs:83-116` parses object
  members as quoted string keys followed by colon, with ASCII whitespace
  skipping around the colon.
- `skinny/crates/runtime/src/grammars/json/generated.rs:121-137` parses JSON
  arrays as `[ ... ]` with comma-separated values.
- `skinny/crates/runtime/src/grammars/json/generated.rs:142-156` parses JSON
  string values and patches `OffsetFlags::HAS_ESC` when the regex span needs
  decode.
- `skinny/crates/runtime/src/grammars/json/generated.rs:171-185` defines the
  tiny-string fast path using `"` terminator, `\` escape, and `0x00..=0x1f`
  control rejection.
- `skinny/crates/runtime/src/grammars/json/generated.rs:189-200` delegates
  long strings to `match_string_at_quote_trusted_utf8`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:205-217` delegates
  number matching to JSON-shaped `match_number_span_from_first`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:221-235` recognizes
  JSON literals from literal byte strings such as `true`, `false`, and `null`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:240-242` defines JSON
  layout as ASCII whitespace skipping.
- `skinny/crates/runtime/src/grammars/json/generated.rs:275-306` treats
  `:`, `,`, `{`, `}`, `[`, `]`, and `"` as structural consumers.
- `skinny/crates/runtime/src/grammars/json/generated.rs:310-378` hardcodes
  JSON comma/close handling for object and array tails.
- `skinny/crates/runtime/src/grammars/json/generated.rs:397-409` binds the
  direct parser to `JsonSink`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:427-463`,
  `:468-503`, and `:508-543` duplicate JSON value dispatch for root, object
  value, and array element direct modes.
- `skinny/crates/runtime/src/grammars/json/generated.rs:548-577` parses direct
  JSON objects with quoted keys, colon separators, comma separators, and
  `JsonSink` begin/end/key callbacks.
- `skinny/crates/runtime/src/grammars/json/generated.rs:582-605` parses direct
  JSON arrays with bracket and comma policy.
- `skinny/crates/runtime/src/grammars/json/generated.rs:610-640` implements
  direct JSON string parsing and reports whether JSON unescape is required.
- `skinny/crates/runtime/src/grammars/json/generated.rs:645-686` repeats JSON
  number matching for root/object/array direct contexts.
- `skinny/crates/runtime/src/grammars/json/generated.rs:690-775` materializes
  JSON numbers into root/object/array `JsonSink` number callbacks.

### JSON value, view, sink, and visitor surface

- `skinny/crates/runtime/src/grammars/json/value.rs:12-26` defines
  `JsonNodeKind` with JSON object, array, string, number, boolean, null, comma,
  and colon variants.
- `skinny/crates/runtime/src/grammars/json/value.rs:29-46` classifies node
  kind by reading source bytes at a tape cursor and matching JSON bytes.
- `skinny/crates/runtime/src/grammars/json/value.rs:69-76` defines
  `JsonValue` as the JSON value enum.
- `skinny/crates/runtime/src/grammars/json/value.rs:90-100` canonicalizes via
  JSON serialization rules and JSON literals.
- `skinny/crates/runtime/src/grammars/json/value.rs:120-133` defines
  JSON-specific parse errors, including colon and comma/object/array errors.
- `skinny/crates/runtime/src/grammars/json/value.rs:143-170` maps generic
  `ValueRef` to JSON wrappers by `JsonNodeKind`.
- `skinny/crates/runtime/src/grammars/json/sink.rs:3-14` defines JSON direct
  sink callbacks for object/array/string/number/bool/null.
- `skinny/crates/runtime/src/grammars/json/sink.rs:16-36` decodes JSON key and
  root string sources with `unescape_string`.
- `skinny/crates/runtime/src/grammars/json/sink.rs:38-119` defines array/object
  context variants for JSON sink callbacks and JSON unescape.
- `skinny/crates/runtime/src/grammars/json/view.rs:11-60` exposes `JsonRoot`,
  JSON canonicalization, token stream, and JSON visitor traversal.
- `skinny/crates/runtime/src/grammars/json/view.rs:67-80` implements
  `DocumentView` with `RootKind` but leaves the `ValueRef` grammar parameter at
  its default.
- `skinny/crates/runtime/src/grammars/json/view.rs:83-138` defines JSON object
  pairs, key lookup, and canonical object serialization.
- `skinny/crates/runtime/src/grammars/json/view.rs:140-172` defines JSON array
  values and canonical array serialization.
- `skinny/crates/runtime/src/grammars/json/view.rs:182-190` maps a JSON pair
  to a `JsonString` key and `JsonValue` value.
- `skinny/crates/runtime/src/grammars/json/view.rs:199-215` decodes JSON string
  refs by checking `OffsetFlags::HAS_ESC` and calling `unescape_string`.
- `skinny/crates/runtime/src/grammars/json/view.rs:224-238` treats JSON numbers
  as raw spans plus serde-compatible numeric materialization.
- `skinny/crates/runtime/src/grammars/json/view.rs:264-298` assumes object pair
  layout is `string-key, value, next-string-or-object-close`.
- `skinny/crates/runtime/src/grammars/json/view.rs:306-329` assumes array item
  layout is values until `ArrayClose`.
- `skinny/crates/runtime/src/grammars/json/view.rs:332-380` computes spans and
  sibling cursors with JSON container/string/scalar kinds.
- `skinny/crates/runtime/src/grammars/json/view.rs:383-388` recomputes string
  body range using JSON UTF-8 string matching.
- `skinny/crates/runtime/src/grammars/json/view.rs:391-413` computes scalar
  spans using JSON literal lengths and JSON number terminators.
- `skinny/crates/runtime/src/grammars/json/view.rs:415-450` emits JSON tokens
  and JSON payload classes.
- `skinny/crates/runtime/src/grammars/json/visitor.rs:5-39` defines and walks a
  JSON-only visitor shape.
- `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs:10-24`
  provides a JSON `EventGrammar` witness, but only for fact/class proof; it
  does not drive parsing, scanning, view generation, or sink binding.

## Minimal legal config shape

Use an equivalent generated metadata surface instead of a public
`crate::tape::GrammarConfig` API. The legal W1a shape is module-owned and
`pub(crate)`:

```rust
// generated per grammar, e.g. grammars/json/config.rs and grammars/css_l4/config.rs
pub(crate) struct JsonConfig;

pub(crate) struct StructuralPolicy {
    pub bytes: &'static [u8],
    pub class_count: u8,
    pub lo6_table: [u8; 64],
}

pub(crate) struct LayoutPolicy {
    pub skip: fn(&[u8], usize) -> usize,
}

pub(crate) struct StringPolicy {
    pub quote: u8,
    pub escape: Option<u8>,
    pub reject_control_below: Option<u8>,
    pub trusted_utf8: bool,
}

pub(crate) struct NumberPolicy {
    pub allow_sign: bool,
    pub allow_leading_dot: bool,
    pub allow_leading_zero: bool,
    pub exponent_marks: &'static [u8],
    pub suffix_mode: NumberSuffixMode,
}

pub(crate) struct FlagPolicy {
    pub escape_or_decode_bit: u8,
    pub control_or_invalid_bit: u8,
}

impl JsonConfig {
    pub(crate) const STRUCTURAL: StructuralPolicy = /* generated */;
    pub(crate) const LAYOUT: LayoutPolicy = /* generated */;
    pub(crate) const STRING: StringPolicy = /* generated */;
    pub(crate) const NUMBER: NumberPolicy = /* generated */;
    pub(crate) const FLAGS: FlagPolicy = /* generated */;

    pub(crate) fn node_kind_at_cursor(tape: &crate::tape::Tape<'_>, cursor: u32) -> JsonNodeKind;
    pub(crate) fn parse_value_at(state: &mut ParserState<'_>) -> Result<(), ParseError<'_>>;
}
```

Notes:

- Keep `Tape`, `TapeBuilder`, `ValueRef`, `OffsetFlags`, and `PayloadArena`
  storage unchanged for W1a. The generated parser reads config and then calls
  existing `TapeBuilder::push_plain_offset` / `patch_flags`.
- Put structural bytes, FIRST dispatch, object/pair rules, layout skipping,
  string policy, number policy, flag-bit interpretation, node-kind mapping,
  sink trait binding, and view wrappers in generated grammar modules.
- If shared helper types are needed, keep them `pub(crate)` under
  `runtime/src/grammars/` or duplicate them in generated modules. Do not export
  a new `pub mod grammar_config` from `runtime/src/lib.rs`, do not add a public
  `tape::GrammarConfig`, and do not add BIR/directive/`BackendShape` variants.
- For JSON, generated metadata maps the existing bits to the current behavior.
  For CSS L4, generated metadata must define CSS string/comment/escape/layout
  and token policies without touching generic tape semantics.

## Risks and guardrails

- `ValueRef` risk: the `G: EventGrammar` marker exists at
  `skinny/crates/runtime/src/tape/mod.rs:175-180`, but JSON roots and wrappers
  leave it at `AnyGrammar` (`skinny/crates/runtime/src/grammars/json/view.rs:67-80`).
  If W1a rewires generated views to `ValueRef<..., JsonEventGrammar>` or a CSS
  grammar marker, it must preserve layout and lifetimes and avoid erasing `G`
  through helper functions like `value_from_ref`.
- `ValueRef` contamination risk: non-JSON views must not reuse
  `JsonNodeKind::at_cursor` from
  `skinny/crates/runtime/src/grammars/json/value.rs:29-46` or JSON sibling
  traversal from `skinny/crates/runtime/src/grammars/json/view.rs:332-380`.
  Generate grammar-specific node kind and traversal instead.
- `TapeBuilder` risk: making `TapeBuilder` generic over config would be a
  public substrate/API change and would propagate through parser state and
  tests. Keep builder storage-only for W1a; let generated parser/scan code own
  policy and pass only offsets/opaque flags to the builder.
- `TapeBuilder` ordering risk: `patch_flags` assumes sorted cursor order at
  `skinny/crates/runtime/src/tape/assembler.rs:94-113`. Any generated CSS
  parser that patches decode/comment/control facts later must still patch the
  original cursor before emitting later flag-bearing cursors, or it needs a
  measured builder change with a separate redress.
- `OffsetFlags` risk: `HAS_ESC` and `HAS_CONTROL` at
  `skinny/crates/runtime/src/tape/mod.rs:20-24` are the embedded leak. Do not
  add CSS meanings to those constants. Treat `OffsetFlags(u8)` as opaque slots
  and move names/interpretation into generated `FlagPolicy`; JSON can continue
  mapping slot 0 to JSON decode-needed until a compatibility-safe rename lands.
- Scanner risk: JSON scan masks strings with quote/backslash only
  (`skinny/crates/runtime/src/grammars/json/scan.rs:130-198`). CSS L4 requires
  comment/string/escape aware scanning; reusing JSON scanner config by only
  replacing `STRUCTURAL_BYTES` is not legal.
- Sink risk: `JsonSink` shape is JSON-specific at
  `skinny/crates/runtime/src/grammars/json/sink.rs:3-119`. CSS L4 needs its own
  generated sink/view surface; a generic sink enum or public super-sink would
  be a new substrate surface and should be rejected for W1a.
- Public-surface risk: adding `pub trait GrammarConfig` under `tape` would
  conflict with the W1a exit gate. Prefer `pub(crate)` generated metadata or a
  private codegen/runtime helper that is not exported from `runtime/src/lib.rs`.

## Verification commands

Run from `skinny/` unless noted.

```sh
cargo test -p runtime
cargo test -p runtime --features proof tape::event_grammar_tests -- --nocapture
cargo xtask check-json
cargo test -p bbnf-bench lock14_baseline -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results
```

Lock 14/public surface scans from repo root:

```sh
rg -n "JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap|grammar-name|STRUCTURAL_ALPHABET_JSON|b\"\\{\\}\\[\\],:\\\\\"\"|OffsetFlags::HAS_ESC|JsonSink" skinny/crates/runtime/src/tape skinny/crates/codegen/src skinny/crates/ir/src
rg -n "pub (mod|trait|struct|enum).*GrammarConfig|BackendShape|UnionTape|directive" skinny/crates/runtime/src skinny/crates/ir/src skinny/crates/codegen/src
git diff --name-status -- skinny/crates/runtime/src skinny/crates/codegen/src skinny/crates/ir/src skinny/RESULTS.md skinny/REDRESS.md
```

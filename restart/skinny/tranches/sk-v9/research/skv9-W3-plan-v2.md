# SK-V9 Wave W3 Plan V2: Streaming Union Cursor

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 6;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research-v2.md`;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
§5; `skinny/REDRESS.md` Items 92, 95, and 96.

Disposition: select a revised strict W3 redress shape. The plan keeps the W3
exit gate intact: `consume_structural` is deleted, the JSON retained view reads
a class column, and the parser consumes the structural producer in the same
wave. The material change from REDRESS 96 is that the structural producer is a
streaming cursor, not a full `Vec<u32>` pre-scan asserted after scalar
delimiter discovery.

## Intervention

Land the W3 union substrate as:

1. A packed retained class lane on the existing `Tape`.
2. Explicit JSON event-class writes at existing generated emit sites.
3. A parser-private streaming structural cursor that yields the next
   structural byte without allocating a full position vector.
4. Generated parser lowering that asks the structural cursor for delimiters
   and string quotes instead of rediscovering structural bytes through
   `consume_structural`.

The structural cursor is transient and dies with `parse(input)`. It is not a
new retained substrate, public substrate API, `UnionTape`, `BackendShape`, or
sidecar. The retained substrate remains one tape: offsets, packed classes,
sparse flags, payloads, and id.

## Owner Paths

Redress may edit only these paths:

- `skinny/crates/runtime/src/tape/{mod,assembler}.rs`: add packed class-lane
  storage, `class_at(cursor) -> Option<u8>`, `class_bytes()`, and
  `push_offset_with_class(offset, class)`. Keep `push_plain_offset` for
  non-migrated callers by appending class zero.
- `skinny/crates/runtime/src/grammars/json/scan.rs`: add
  `JsonStructuralCursor`, a streaming scalar fallback, and an aarch64 fast path
  using the existing TBL classify / quote / backslash / prefix-XOR ingredients
  without compacting into a `Vec<u32>`.
- `skinny/crates/runtime/src/grammars/json/parser.rs`: own the transient
  `JsonStructuralCursor` in `ParserState` for the duration of parse and expose
  event emit and structural-walk helpers.
- `skinny/crates/runtime/src/grammars/json/generated.rs`: delete
  `consume_structural`; lower object/array opens, closes, delimiters, and
  string opening/closing quotes through the streaming cursor; write explicit
  JSON event classes for retained offsets.
- `skinny/crates/runtime/src/grammars/json/value.rs`: add JSON event class
  ordinals and make `JsonNodeKind::at_cursor` read `tape.class_at(cursor)`.
- `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs`: extend
  the JSON event-class domain to cover retained JSON event classes
  (`ObjectOpen`, `ObjectClose`, `ArrayOpen`, `ArrayClose`, `String`, `Number`,
  `True`, `False`, `Null`). This is an event grammar domain, not the SIMD
  structural alphabet.
- `skinny/crates/codegen/src/json_templates/{generated,parser,view,value}.rs`:
  mirror the checked-in generated runtime output.
- `skinny/crates/runtime/tests/checkasm_scan_structurals.rs`: end-to-end parity
  for `scan_structurals` and the streaming cursor over fixture and generated
  strings.
- `skinny/crates/bbnf-bench/src/{parity,track2/json}.rs`: write and compare
  class streams in Track 2/parity.

No `bbnf-simd` public structural-index layout change is owned. Lower primitive
checkasm remains with existing `bbnf-simd` primitive tests; the W3 harness is
JSON scanner/cursor parity in `runtime/tests`.

## Key Design Decisions

- **No REDRESS 96 full pre-scan.** Redress must not call
  `scan_structurals(input).into_positions()` in `parse`. The cursor scans a
  64-byte stripe, caches its emit mask, returns the next set bit, and advances.
- **No post-hoc scalar delimiter search.** Parser code must not first call
  `skip_ascii_whitespace` to discover the delimiter and then ask the cursor to
  confirm it. The cursor is the delimiter locator for structural bytes.
- **Packed classes, not `Vec<u8>` classes.** Store two four-bit event classes
  per byte. Class zero is unset/invalid; JSON retained events use classes 1-9.
  This halves REDRESS 96's retained class write footprint while preserving the
  existing `u32` offset range.
- **String ownership stays explicit.** The cursor consumes opening and closing
  quotes. `match_string_at_quote_trusted_utf8` remains the string validator and
  span producer for W3; W4a owns string-block widening.
- **Scalar anchors stay parser-owned events.** Numbers and literals are not in
  the SIMD structural alphabet. The parser writes their classes at the existing
  number/literal emit sites.

## Falsifiability Gate

`G-W3-UNION-SUBSTRATE` remains SPEC Section 6's gate, with additional
REDRESS 96 regression falsifiers:

- `rg -n 'consume_structural' skinny/crates/runtime/src skinny/crates/codegen/src`
  returns zero matches.
- `rg -n 'into_positions\\(|structural_positions' skinny/crates/runtime/src/grammars/json`
  returns zero matches.
- `JsonNodeKind::at_cursor` contains no `tape.source()[offset]` event-class
  rediscovery.
- `JsonStructuralCursor` parity matches `scan_structurals` and the scalar
  cursor on generated strings and at least one corpus fixture.
- `cargo test --manifest-path skinny/Cargo.toml -p runtime --test checkasm_scan_structurals -- --nocapture`
  passes.
- Runtime, bbnf-bench parity/materialization/track2, codegen, and proof checks
  pass.
- Native Criterion with `RUSTFLAGS="-C target-cpu=native"` measures W3
  must-improve rows and W10b maintain rows against SPEC floors.
- The diagnostic split records scan-only, grow-only parse, capacity-plan-C
  parse, and streaming-cursor parse evidence. Capacity-plan-C is expected to
  remain a diagnostic nonproducer, not a gate producer.

## Redress Extension

This plan requests the W3 CHALLENGE-granted redress cap of ≤110 minutes. The
slice touches tape representation, a streaming scanner/cursor, generated JSON
parser lowering, codegen templates, parity, Track 2, proof witness, and
measurement. That is the HIGH-risk W3 case described in SPEC Section 6.

## Revert Protocol

- Revert tape packed class-lane fields and restore offset-only finish.
- Revert JSON scanner cursor additions, leaving `scan_structurals` unchanged.
- Revert parser/generated/template changes to the `attach_structural_index`
  no-op and `consume_structural` lowering.
- Revert `JsonNodeKind::at_cursor` to source-byte rediscovery.
- Revert JSON witness class-count expansion.
- Revert parity/Track 2 class comparisons and writes.

Any correctness failure, cursor parity failure, W10b regression, or W3
must-improve miss rejects the whole W3 redress. W3 has no partial source
admission under this plan.

## Pre-Blocked Routes

- REDRESS 50/51/53 sidecar/parser-owned cursor: cleared only if the streaming
  cursor is parse-frame-local, retained nowhere, and no public substrate API
  exposes it.
- REDRESS 92 storage-only swap: cleared because retained offsets remain the
  parser-event stream and classes are explicit event classes.
- REDRESS 96 full-Vec integration: cleared only if parse never allocates or
  moves a full structural-position vector and never validates after scalar
  delimiter discovery.
- REDRESS 83/84/88/89 remain orthogonal; W3 changes no string-block widening,
  direct route, PMULL, or CTZ default body.

## CHALLENGE Questions

1. Accept or reject treating a parse-frame-local streaming cursor as the
   allowed "move-consumed SIMD index" for W3, despite the non-negotiable
   against retained parser-owned structural cursors.
2. Accept or reject packed four-bit classes as the W3 class column.
3. Accept or reject extending `JsonEventGrammar::STRUCTURAL_CLASS_COUNT` from
   the structural alphabet count to the retained JSON event-class domain.
4. Confirm that class-consumer-only W3 is rejected for this wave because it
   cannot move `parse_only` rows.
5. Confirm the ≤110-minute W3 redress extension.

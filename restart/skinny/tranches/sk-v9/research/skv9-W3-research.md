# SK-V9 Wave W3 Research: Union Event-Model Source Shape

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 6;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
§5; W2 commit `99b2399f`.

W2 is closed: `ValueRef` now has separate node-kind and event-grammar marker
axes. W3 can therefore reopen REDRESS 92's retained tape event-model mismatch.

## Current Runtime Shape

- `runtime/src/tape/mod.rs` still has only offset, sparse flag, payload, and
  tape id columns. `TapeBuilder` still emits offsets through
  `push_plain_offset` / `push_offset`, and `Tape` has no `class_at` consumer.
- `runtime/src/grammars/json/parser.rs` owns private `ParserState` with
  `input`, `bytes`, `cursor`, and `TapeBuilder`. `parse()` calls
  `generated::attach_structural_index(&mut state)`, but the generated function
  is currently a no-op.
- `runtime/src/grammars/json/generated.rs` is the live Track 1 parser output.
  It still has `consume_structural(state, byte)` and seven retained-tape emit
  sites:
  object open, array open, string/key quote, number start, literal start,
  object close, and array close. Colon and comma are walk-only delimiters.
- `runtime/src/grammars/json/value.rs::JsonNodeKind::at_cursor` still
  rediscoveres the event class by reading `tape.source()[offset]`. This is the
  same-wave consumer W3 must replace with `tape.class_at(cursor)`.
- `codegen/src/json_templates/{generated,parser,view,value}.rs` mirror the
  generated runtime files, so W3 must edit templates and the checked-in
  generated JSON output together.

## SIMD Producer Shape

- `runtime/src/grammars/json/scan.rs` already has a JSON-aware structural scan.
  Scalar and aarch64 paths return `StructuralIndex` positions only.
- The aarch64 scan already uses the P2-D §5 ingredients: a 64-byte low-six-bit
  TBL classify table, structural/quote/backslash/control masks,
  `escape_mask_64`, `prefix_xor_64`, and compacted position emission.
- `bbnf-simd::StructuralIndex` currently stores only `positions: Vec<u32>` and
  `ScanBackend`. No structural class vector exists for the parser to consume.
  W3 can still consume the position stream by move and read the structural byte
  at each position; adding a second structural-class vector would be extra
  retained producer work and should require CHALLENGE justification.
- Existing checkasm surfaces cover the lower primitives, including
  `checkasm_structural_terminator_64.rs`, but there is no end-to-end
  `scan_structurals` checkasm/parity harness for the JSON scanner.

## Hidden Coupling For Plan

- `bbnf-bench/src/track2/json.rs` builds a `JsonRoot` with the same
  `runtime::tape::TapeBuilder`. Once `JsonNodeKind::at_cursor` reads the class
  column, Track 2 needs class writes or the runtime needs a fallback. The W3
  owner table names `bbnf-bench/src/parity.rs` but not Track 2. A plan that
  leaves this unhandled will break the parity oracle or keep the source-byte
  rediscovery path alive.
- A generic `TapeBuilder::push_plain_offset` cannot infer JSON event classes
  without violating Lock 14. The class write must be explicit at generated JSON
  emit sites, or confined to benchmark-only Track 2 if the owner table is
  amended by CHALLENGE.
- The SPEC gate text saying the class column must not leak
  `Number`/`Literal` ordinals into the structural alphabet should be read as a
  structural-index constraint, not as a tape-event constraint. The tape class
  column must carry number/literal/bool/null event classes for retained view
  consumers; the SIMD structural alphabet remains `{ } [ ] , : "`.

## Redress Fit

W3 is not a 75-minute redress in the current source shape. The minimum
redress slice spans:

- tape column and builder API;
- generated JSON parser output and four JSON templates;
- transient structural-index walker state in `ParserState`;
- class-read consumer in `JsonNodeKind::at_cursor`;
- Track 2/parity accommodation;
- end-to-end structural scan parity/checkasm evidence;
- RESULT/REDRESS/HANDOFF status.

This matches the SPEC's HIGH-risk estimate. The W3 plan should request the
CHALLENGE-gated redress extension to the ≤110-minute cap before source redress.

## Recommended Plan Direction

- Preserve `bbnf_simd::StructuralIndex` as a transient positions-only producer
  unless CHALLENGE requires class-vector support. Consume it by move in the
  JSON parser and advance a private structural cursor.
- Add `Tape::class_at(cursor)` and `TapeBuilder::push_offset_with_class`.
  Keep `push_plain_offset` only for non-migrated test/benchmark callers; do not
  make it infer grammar policy.
- Replace Track 1 JSON emit sites with explicit event classes:
  object/array open+close, string, number, true, false, null.
- Delete `consume_structural` from generated runtime and template output. Add
  walker helpers with names that do not preserve the rejected hot leaf.
- Either add class writes to `bbnf-bench/src/track2/json.rs` under a CHALLENGE
  owner-table amendment or explicitly reject W3 redress until the owner table
  includes the parity-oracle consumer.

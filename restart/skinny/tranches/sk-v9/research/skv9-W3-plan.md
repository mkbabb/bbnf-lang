# SK-V9 Wave W3 Plan: Union Event-Model Class Column

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 6;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research.md`;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
§5; `skinny/REDRESS.md` Item 92 and Item 95.

Intervention: land the retained tape class column and make the JSON parser
consume the structural index by move. The retained substrate remains one tape:
offsets plus a co-indexed parser-event class column. The structural index is a
transient producer consumed during parse and is not retained.

## Owner Paths

This plan folds two SPEC Section 6 corrections:

- `bbnf-bench/src/track2/json.rs` is added to A.8. Track 2 builds a
  `JsonRoot` with the same `TapeBuilder`; once `JsonNodeKind::at_cursor` reads
  classes, the parity oracle must write classes too.
- The class-column gate is clarified: the retained tape class column carries
  parser-event ordinals, including number/literal/bool/null scalar anchors.
  The SIMD structural index alphabet remains structural-only.

Redress owner paths:

- `skinny/crates/runtime/src/tape/mod.rs` and
  `skinny/crates/runtime/src/tape/assembler.rs`: add `classes: Vec<u8>`,
  `classes()`, `class_at(cursor)`, and `push_offset_with_class`.
- `skinny/crates/runtime/src/grammars/json/parser.rs`: construct
  `ParserState` with the move-consumed `StructuralIndex`; expose
  `emit_event_offset(offset, class)`, structural-position cursor helpers, and
  walk-only delimiter advancement.
- `skinny/crates/runtime/src/grammars/json/generated.rs`: replace every
  retained emit site with explicit event classes, delete
  `consume_structural`, advance the structural index for closing quotes,
  colons, and commas.
- `skinny/crates/runtime/src/grammars/json/value.rs`: make
  `JsonNodeKind::at_cursor` read `tape.class_at(cursor)`.
- `skinny/crates/codegen/src/json_templates/{generated,parser,view,value}.rs`:
  mirror the checked-in generated runtime output.
- `skinny/crates/runtime/src/grammars/json/scan.rs`: keep the existing
  positions-only `StructuralIndex` producer; use it as the transient parser
  input. Source changes here are allowed only if the scan/checkasm harness
  needs a public parity helper.
- `skinny/crates/runtime/tests/checkasm_scan_structurals.rs` (new):
  end-to-end JSON structural scan parity over generated random strings and at
  least one corpus fixture. This runtime-side location avoids a dependency
  cycle and keeps JSON policy out of `bbnf-simd`.
- `skinny/crates/bbnf-bench/src/parity.rs`: compare class streams in addition
  to offsets/flags and keep materialization parity.
- `skinny/crates/bbnf-bench/src/track2/json.rs`: write the same JSON event
  class bytes as Track 1 for the benchmark oracle.

No `BackendShape`, BIR, directive, public `UnionTape`, parser-owned retained
sidecar, `path!`, direct-to-struct, SinkOnly, fixture, or generic JSON policy
path is owned by W3.

## Redress Extension

The plan requests the W3 CHALLENGE-gated redress extension to ≤110 minutes.
The minimum redress spans tape layout, parser state, generated runtime,
templates, parity oracle, Track 2, and checkasm. That is the exact HIGH-risk
case described in SPEC §2.2/§6 and cannot be responsibly fit into the 75-minute
target without cutting the same-wave consumer or parity harness.

## Falsifiability Gate

`G-W3-UNION-SUBSTRATE` passes only if the SPEC Section 6 gate passes after the
corrections above. Load-bearing implementation checks:

- `rg -n 'consume_structural' skinny/crates/runtime/src skinny/crates/codegen/src`
  returns zero after redress.
- `JsonNodeKind::at_cursor` has no `tape.source()[offset]` event-class
  rediscovery.
- `rg -n 'StructuralIndex' skinny/crates/runtime/src` returns only private JSON
  parser/scan matches and no tape field or public substrate API.
- `cargo test -p runtime` passes.
- `cargo test -p bbnf-bench parity materialization -- --nocapture` passes, or
  the exact parity/materialization test names replacing that filter pass.
- `cargo test -p runtime --test checkasm_scan_structurals -- --nocapture`
  passes.
- Native Criterion with `RUSTFLAGS="-C target-cpu=native"` measures the W3
  must-improve rows and the W10b maintain block against SPEC floors. Any W10b
  row below floor rejects the wave.
- `samply` confirms `consume_structural` disappears from the affected rows and
  `JsonNodeKind::at_cursor` is ≤1% self-time.

## Revert Protocol

- A.1 tape column: remove `classes` and restore offset-only `TapeBuilder`.
- A.2 parser state: restore `ParserState::new(input)` and the no-op
  `attach_structural_index` call.
- A.3 generated parser/template: restore `consume_structural` and
  `emit_plain_offset`.
- A.4 value consumer: restore source-byte class rediscovery.
- A.8 parity/checkasm/Track 2: remove the class-stream parity additions and
  Track 2 class writes.

Any failed correctness, checkasm, Lock 14, W10b, or hot-leaf gate reverts the
whole wave. W3 has no partial admission.

## CHALLENGE Questions

- Accept or reject the owner-table correction adding `bbnf-bench/src/track2/json.rs`.
- Accept or reject using positions-only `StructuralIndex` plus source-byte
  reads at structural positions, rather than adding a structural-class vector.
- Accept or reject the ≤110-minute redress extension.
- Confirm that parser-event scalar classes in the tape do not violate the
  structural-only SIMD alphabet constraint.
- Confirm that JSON scan parity belongs in `runtime/tests`, while generic
  primitive parity remains in the existing `bbnf-simd` checkasm harnesses.

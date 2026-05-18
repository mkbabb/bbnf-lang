# SK-V8 W3 Research: Tape Plus Structural-Projection Union

Date: 2026-05-18.
Scope: W3 Tier A after W0, W1, and W2 closure.

## Finding

W3's lead hypothesis remains correct at the architectural level: the JSON
scanner can build a structural index and the retained parser currently rebuilds
its tape offsets scalar-by-scalar. The current implementation, however, is not
a storage-only swap. The scanner's `StructuralIndex` and the retained `Tape`
do not have the same event semantics.

Current scanner surface:

- `skinny/crates/runtime/src/grammars/json/scan.rs` scans JSON structural
  punctuation plus real quotes.
- `skinny/crates/bbnf-simd/src/lib.rs` stores only `positions: Vec<u32>` and a
  `ScanBackend` in `StructuralIndex`.
- `structural_capacity_for(CapacityPlan::OneShotSimd)` uses
  `scan_structurals(source).positions().len() + 8` only as reserve sizing.
- `generated::attach_structural_index` is a no-op.

Current retained tape surface:

- `skinny/crates/runtime/src/tape/assembler.rs` exposes
  `TapeBuilder::push_plain_offset` and patches sparse flags by tape cursor.
- `skinny/crates/runtime/src/grammars/json/generated.rs` emits offsets from
  the recursive-descent parser: container opens/closes, opening quotes, number
  starts, and literal starts.
- `skinny/crates/runtime/src/grammars/json/view.rs` and
  `value.rs` derive `ValueRef` traversal from that event stream with
  `JsonNodeKind::at_cursor`.
- Track 1/Track 2 parity requires identical tape offsets and flag rows.

The executable example is already in the benchmark tests:

```text
input: {"a":[1,true]}
current retained tape offsets: [0, 1, 5, 6, 8, 12, 13]
scanner structural positions:  [0, 1, 3, 4, 5, 7, 12, 13]
```

The retained tape omits the key closing quote, colon, and comma, and adds the
number and literal starts. A scan-produced position vector therefore cannot be
moved into `Tape` without changing the JSON view/value cursor contract. Doing
that correctly means redesigning generated retained parsing and view traversal
around structural ordinals plus scalar-span facts, not just retaining the
stage-1 index.

## Falsifiability Against W3 Entry

W3 requires all of these in one slice:

- generated JSON retained parser as same-wave production consumer;
- exactly one retained tape;
- deletion of the old offset append API/parser-owned cursor path;
- retained view/`ValueRef` parity;
- Track 2 independence;
- no new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate
  API, sidecar substrate, aux table, or parser-owned structural cursor/facts;
- selected structural-heavy parse rows above threshold and all current rows
  within maintain budget;
- Lock 14 and non-JSON proof if generic crates move.

The current owner surface needed to satisfy those simultaneously is larger than
the W3 cap:

- `skinny/crates/bbnf-simd/src/lib.rs` would need class ordinals or a grammar
  class side channel, plus scalar parity for positions/classes.
- `skinny/crates/runtime/src/grammars/json/scan.rs` would need scan-written
  co-indexed positions/classes with quote/body semantics preserved.
- `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}` would need a tape
  layout that stores scan classes while still representing scalar events.
- `skinny/crates/runtime/src/grammars/json/generated.rs` would need to consume
  the scan/tape stream instead of writing offsets.
- `skinny/crates/runtime/src/grammars/json/{view.rs,value.rs}` and matching
  codegen templates would need cursor semantics rewritten from source-byte
  classification to retained classes/facts.
- `skinny/crates/bbnf-bench/src/parity.rs` and materialization tests would need
  new parity assertions and row evidence.

That is a representation redesign across SIMD, runtime tape, generated parser,
generated view/value, codegen templates, and bench gates. It is not feasible
inside the W3 default 450 source/test LOC, the exceptional 650 source/test LOC,
and the 90-minute implementation/redress cap while preserving scalar/reference
proof, row measurement, generated-output audit, full-table maintain, and
rollback.

## Historical Route Checks

This is not a request to reopen prior cursor sidecars:

- REDRESS 51 rejected the byte-class whitespace/event cursor route.
- REDRESS 53 rejected the parser-local structural-mask cursor even after it
  consumed the JSON emit mask and carried quote/backslash state.
- P3-E keeps sidecar producers, parser-owned projections/cursors, aux tables,
  and `tape_vs_tape` as W3 consumer blocked.

The remaining admissible shape is still one retained substrate where scanner
output becomes the production tape and generated retained parsing consumes it.
This research says only that the current W3 slice cannot implement that shape
without first splitting the event-model redesign.

## Research Verdict

Do not patch W3 source in this wave. Write a W3 plan that rejects/routs Tier A
implementation for SK-V8, records the exact event-model mismatch, and feeds a
split precursor into SK-V9/Pass Omega:

1. define the scan-class/event grammar that can represent numbers/literals and
   string quote ownership without a side substrate;
2. prove the retained `ValueRef` cursor contract over that grammar;
3. only then implement a measured structural-heavy parse row wave.

Verification run during research:

- `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`
- `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`

# SK-V9 Wave W3 Research V2: REDRESS 96 Follow-Up

Inputs: `skinny/REDRESS.md` Item 96;
`/tmp/skv9-waveW3-rejected.patch`;
`restart/skinny/tranches/sk-v9/SPEC.md` Section 6;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-plan.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v2.md`.

Purpose: explain why the first W3 redress passed correctness but failed every
row gate, and identify the next W3 plan boundary before any further source
redress.

## Finding

REDRESS 96 falsified the concrete implementation shape, not the W2 proof or
the need for a retained event-class consumer. The rejected patch coupled three
costs in one parse path:

1. A full `scan_structurals(input)` allocation at parse entry, stored as
   `structural_positions: Vec<u32>` in `ParserState`.
2. A co-indexed `Vec<u8>` class column appended on every retained tape event.
3. Post-hoc validation of structural positions after the parser had already
   found the same byte with the old scalar whitespace/delimiter search.

That third point is load-bearing. The patch deleted the
`consume_structural` symbol, but the generated parser still used
`skip_ascii_whitespace` and source-byte checks to find each delimiter before
calling the structural-index assertion helpers. It therefore paid both the old
scalar delimiter walk and the new SIMD pre-scan/class-write costs.

## Current Code Boundaries

- `runtime/src/grammars/json/generated.rs:292` is the live scalar
  rediscovery helper, `consume_structural`.
- `runtime/src/grammars/json/parser.rs:47` still builds `ParserState::new`
  and calls generated `attach_structural_index`, which is a no-op.
- `runtime/src/grammars/json/scan.rs:22` returns a positions-only
  `StructuralIndex`; `bbnf-simd::StructuralIndex` stores those positions in a
  `Vec<u32>`.
- `runtime/src/grammars/json/value.rs:29` rediscoveres event class by reading
  `tape.source()[offset]`. Retained view traversal calls that path repeatedly.
- `runtime/src/tape/{mod,assembler}.rs` still retain one offset tape plus
  sparse flags and payloads. `ValueRef` remains a `&Tape + cursor` contract
  with W2's type-only grammar marker.

## Measurement Probe

Two read-only targeted Criterion runs compared the current grow-only capacity
plan with `BBNF_CAPACITY_PLAN=C`, which performs a SIMD structural scan for
capacity sizing without consuming the index:

```text
RUSTFLAGS="-C target-cpu=native" BBNF_CAPACITY_PLAN=D \
  CRITERION_HOME=/tmp/skv9-w3-research-d \
  cargo bench -p bbnf-bench --bench json_parity -- \
  'json/(twitter|apache_builds|update_center|distinct_values|canada)/track1_generated'

RUSTFLAGS="-C target-cpu=native" BBNF_CAPACITY_PLAN=C \
  CRITERION_HOME=/tmp/skv9-w3-research-c \
  cargo bench -p bbnf-bench --bench json_parity -- \
  'json/(twitter|apache_builds|update_center|distinct_values|canada)/track1_generated'
```

The filter intentionally measured only the Track 1 parse rows; Criterion
reported missing comparison samples for the unmeasured sibling functions, but
the requested `track1_generated` estimates were written.

| Row | Grow-only Mbps | Unconsumed scan Mbps | Delta |
|---|---:|---:|---:|
| twitter | 15586 | 8889 | -43.0% |
| apache_builds | 12374 | 7979 | -35.5% |
| update_center | 11517 | 7009 | -39.1% |
| distinct_values | 9378 | 6122 | -34.7% |
| canada | 16949 | 12166 | -28.2% |

This confirms the REDRESS 96 diagnosis: paying for a full scan without using
it as the parser's delimiter locator is too expensive to clear W3 or preserve
W10b. The next redress must either avoid the full pre-scan or use it to
replace the scalar delimiter walk, not assert after the fact.

## Revised Plan Boundary

There are two viable next-plan shapes, and CHALLENGE must choose one before
source redress:

1. **Strict W3 cursor-over-index plan.** Replace the delimiter search with a
   true index-driven parser walker. The structural cursor must supply the next
   structural byte directly to object/array/string/delimiter sites, so the old
   `skip_ascii_whitespace` search is not paid before the index check. To avoid
   REDRESS 96's allocation cost, the preferred producer is streaming
   64-byte-stripe state, not a retained `Vec<u32>` moved into `ParserState`.
   This needs CHALLENGE because the current SPEC permits a move-consumed SIMD
   index but also forbids parser-owned retained structural cursors.
2. **Class-consumer-first plan.** Land a lower-overhead retained class lane and
   make `JsonNodeKind::at_cursor` read it, while deferring structural-index
   parser control. This uses the existing emit sites, avoids a full scan, and
   tests whether the view/materialization rediscovery cost is enough to move
   W3. If selected, CHALLENGE must explicitly revise the Section 6 gate because
   `consume_structural` would not be eliminated by a SIMD producer in that
   wave.

The second shape should use a packed class lane, not REDRESS 96's `Vec<u8>`:
two four-bit event classes per byte, class zero reserved for unset/invalid,
offsets left as full `u32`, `push_offset_with_class(offset, class)` explicit at
generated JSON emit sites, and `class_at(cursor)` decoding one nibble. Sparse
flags are not suitable because `flags_at` is binary-search based.

## Constraints For Plan

- Do not retry parse-entry `scan_structurals(input).into_positions()` plus
  post-hoc `take_indexed_byte` assertions.
- Do not add JSON policy to generic tape code. Generic tape stores opaque
  packed classes; generated JSON owns class meanings.
- Do not change `ValueRef` layout or carry runtime grammar state in it; W2's
  size/lifetime proof must keep passing.
- Resolve the event-class domain explicitly. Source-free
  `JsonNodeKind::at_cursor` needs object open/close, array open/close, string,
  number, true, false, and null classes. The current JSON event grammar witness
  admits seven structural classes, so the plan must either extend the event
  grammar domain or keep a narrow source-byte read for literal subkind.
- Track 2 and parity must be updated in the same redress if retained classes
  become the production consumer.
- The next plan must add a diagnostic split to measurement: scan-only,
  current parse, scan-for-capacity-only, and any scan-plus-indexed-parse probe.
  Standalone scan throughput alone is no longer informative; it already cleared
  the scanner floor while W3 failed.

## Recommendation

Plan V2 should not dispatch W4 and should not reuse the rejected full-Vec
integration. The strongest W3 follow-up is a CHALLENGE-gated plan that either
implements a true streaming cursor-over-index parser, or explicitly narrows W3
to a packed class-lane consumer and revises the Section 6 gate before redress.
The current SPEC text still names the strict cursor-over-index outcome, so any
class-consumer-first plan is a SPEC amendment, not a silent reinterpretation.

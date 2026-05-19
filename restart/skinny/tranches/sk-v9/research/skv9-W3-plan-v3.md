# SK-V9 Wave W3 Plan V3: No Source Redress Under Current Gate

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 6;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research-v3.md`;
`skinny/REDRESS.md` Items 96 and 97.

Disposition: do not dispatch a third W3 source redress under the current
`G-W3-UNION-SUBSTRATE` gate. The only materially distinct remaining source
shape is emit-site class-lane-only, and that shape cannot satisfy the current
W3 contract because it does not move the parse-only structural producer path.

## Candidate Reviewed

The reviewed candidate is a narrow class-lane substrate:

1. Add a mandatory co-indexed class lane to the existing offset tape.
2. Write JSON event classes at existing parser emit sites.
3. Make `JsonNodeKind::at_cursor` read `tape.class_at(cursor)` instead of
   rediscovering the source byte.
4. Keep the current parser walk, `skip_ascii_whitespace` delimiter discovery,
   and current scan module unchanged.

This is mechanically feasible and materially different from both rejected W3
source shapes:

- Unlike REDRESS 96, it does not allocate or move a full
  `scan_structurals(input).into_positions()` vector into `ParserState`.
- Unlike REDRESS 97, it does not add a streaming `JsonStructuralCursor` or any
  scanner/cursor pass inside retained parsing.

## Why It Is Not Selected

The current W3 SPEC Section 6 requires the class column and the same-wave
structural producer to close together. It also requires deletion of the
structural rediscovery hot leaf and numeric improvement on the parse-only
Track 1 rows. Class-lane-only does not meet that intervention:

- The class lane is parser-produced, not scan-produced.
- The old scalar delimiter and whitespace walk remains the parser control
  path.
- No aarch64 structural-bitmap producer is consumed by the parser.
- The `track1_generated` Criterion benchmark parses and black-boxes the root;
  it does not traverse retained views where `JsonNodeKind::at_cursor` would
  otherwise be the direct consumer.
- The change would add class writes during parse, so the most likely numeric
  outcome is neutral or negative on the binding W3/W10b rows.

Dispatching that patch as W3 would be a paper-close: it could satisfy a
source-free `at_cursor` proof, but it would not satisfy the measured
parse-only producer gate that defines W3.

## Non-Selected Owner Set

If a later SPEC amendment deliberately creates a preparatory class-lane wave,
the expected owner set is:

- `skinny/crates/runtime/src/tape/{mod,assembler}.rs`.
- `skinny/crates/runtime/src/grammars/json/{parser,generated,value}.rs`.
- `skinny/crates/codegen/src/json_templates/{parser,generated,value}.rs`.
- `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs`.
- `skinny/crates/runtime/src/tape/event_grammar_tests.rs`.
- `skinny/crates/bbnf-bench/src/{parity,track2/json}.rs`.

That owner set is not sufficient for the current W3 gate because it excludes
`runtime/src/grammars/json/scan.rs`, `bbnf-simd`, and
`runtime/tests/checkasm_scan_structurals.rs`, which are the structural-producer
surfaces required by Section 6.

## Falsifiability

No source redress is authorized by this plan. A future plan may proceed only if
one of these conditions holds:

1. The plan names a materially new parse-only structural producer that is
   neither the REDRESS 96 full-position-vector route nor the REDRESS 97
   streaming-cursor route, with a credible path to the current W3/W10b numeric
   floors.
2. The orchestrator amends the SPEC to split W3 into a preparatory class-lane
   proof wave and a separate parse-only producer wave, with new gates and
   dependency order.
3. The orchestrator retires the current W3 candidate and resequences SK-V9
   around a different shortlist route.

Absent one of those changes, redress should not edit source. The current
implementation bracket is blocked at W3 by gate feasibility, not by local
compiler or test failures.

## Challenge Request

CHALLENGE should reject this V3 plan for source redress under the current W3
contract and escalate to the orchestrator:

`BLOCKED: W3 current G-W3-UNION-SUBSTRATE has no remaining admissible
implementation route after REDRESS 96 and REDRESS 97; user/orchestrator must
amend SPEC, split W3, resequence, or abandon the W3 gate.`

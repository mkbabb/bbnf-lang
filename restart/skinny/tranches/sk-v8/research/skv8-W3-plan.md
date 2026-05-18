# SK-V8 W3 Plan: Tier A Tape Plus Structural Projection

Date: 2026-05-18.
Status: Plan returns REVISE/reject before implementation.
Authority: W0/W1/W2 closed; W3 research artifact
`restart/skinny/tranches/sk-v8/research/skv8-W3-tape-structural-research.md`.

## Candidate

The only admissible W3 candidate is SPEC Section 6 Tier A: retain the stage-1
structural projection inside the single retained `Tape`, add scan-written
opaque class ordinals, migrate generated retained JSON Track 1 parsing plus
retained view/`ValueRef` to consume that tape, and delete scalar structural
rediscovery.

The selected structural-heavy parse rows would be:

- `twitter/parse_only`, floor 16225 Mbps.
- `apache_builds/parse_only`, floor 12857 Mbps.

Guard rows would be:

- `canada/parse_only`, floor 17410 Mbps Track 1 and 16729 Mbps Track 2.
- `mesh/parse_only`, floor 13980 Mbps Track 1 and 13022 Mbps Track 2.
- `numbers/parse_only`, floor 20197 Mbps Track 1 and 18144 Mbps Track 2.
- `marine_ik/parse_only`, floor 13522 Mbps Track 1 and 12137 Mbps Track 2.

The full-table maintain budget would remain no worse than -2.0% Track 1 and
Track 2 versus `SK-V8-open`.

## Fit Gate

The plan fails the pre-redress fit gate. The current scanner index and retained
tape are not isomorphic:

- scanner positions are structural punctuation plus real quotes;
- retained tape offsets are generated parser events: container opens/closes,
  opening quotes, number starts, and literal starts;
- view/`ValueRef` traversal depends on the retained event tape, not a raw
  structural-punctuation cursor.

Implementing Tier A correctly therefore requires a split event-model redesign,
not a bounded patch. The needed owner paths are:

- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/value.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_templates/value.rs`
- generated JSON output under `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/src/parity.rs`
- `skinny/crates/bbnf-bench/src/materialization.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Estimated hand-edited source/test scope exceeds the W3 450 LOC default budget,
the exceptional 650 LOC budget, and the 90-minute implementation/redress cap.
Generated output, row measurement, Lock 14 proof, full-table maintain, and
rollback would add additional unbounded cost.

## Same-Wave Consumer

No valid same-wave production consumer can be named for SK-V8 W3 without the
event-model split. `tape_vs_tape`, `simd_structural_scan`, Track 2, comparator
rows, and retained-view-only checks are not W3 production consumers. A valid
future W3-like wave must make generated JSON retained Track 1 parsing consume
the retained tape positions/classes in the measured row and prove retained
view/`ValueRef` parity in the same slice.

## Pre-Blocked Routes

The plan does not reopen:

- REDRESS 51 byte-class whitespace/event cursor.
- REDRESS 53 parser-local structural-mask cursor / second scanner.
- REDRESS 50-55 parse-time aux/projection side tables, decoded stats sinks, or
  quote-source fused materializers.
- REDRESS 60-72 direct/materialization families.
- REDRESS 82-84 Unicode/tiny-probe/object-pair routes.
- REDRESS 88-89 PMULL and CTZ/bulk bodies.
- new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate
  API, sidecar substrate, parser-owned cursor/facts, or parallel substrate.
- Tier B string-boundary, quote/backslash/parity, density-policy, or
  CostFacts-template claims inside W3 Tier A.

## Redress Plan

After challenge, W3 should reject/rout implementation for SK-V8:

1. Add a REDRESS entry recording the scanner/tape event-model mismatch and the
   failed W3 fit gate.
2. Update HANDOFF to mark W3 rejected/routed and unblock W4.
3. Feed SK-V9/Pass Omega with a split precursor:
   - define the retained class/event grammar including numbers/literals and
     string quote ownership;
   - prove the `ValueRef` cursor contract over that grammar;
   - then measure structural-heavy parse rows in a later wave.

## Verification Required For This Plan

- `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`
- `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`
- `git diff --exit-code HEAD -- skinny/RESULTS.md`
- `git diff --check`

No W3 source implementation is authorized by this plan.

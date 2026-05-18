# SK-V8 W3 Hardening V1 - CH1

Verdict: ACCEPT
Confidence: 94%

## Findings

- No blocker found. ACCEPT means accepting the W3 plan's pre-source-edit
  rejection/routing, not authorizing a Tier A implementation in SK-V8 W3.
- The scanner/tape event mismatch is real in HEAD `fc91c217`. The JSON scanner
  emits structural punctuation plus real quotes from `STRUCTURAL_BYTES =
  b"{}[],:\""`, and `StructuralIndex` stores only `positions: Vec<u32>` plus a
  backend. The retained tape is still generated-parser-owned: `ParserState`
  constructs a `TapeBuilder`, `attach_structural_index` is a no-op, and parser
  paths call `emit_plain_offset` / `push_plain_offset` for semantic events.
- The concrete mismatch in the research is consistent with source and tests.
  For `{"a":[1,true]}`, the retained tape test fixes offsets at
  `[0, 1, 5, 6, 8, 12, 13]`: container opens/closes, opening quote, number
  start, and literal start. The scanner would produce punctuation and real
  quote positions `[0, 1, 3, 4, 5, 7, 12, 13]`, including the key closing quote,
  colon, and comma, while lacking number/literal starts. That is not an
  isomorphic storage substitution.
- Retained view and `ValueRef` traversal depend on the parser event stream.
  `JsonNodeKind::at_cursor` classifies by source byte at the retained offset,
  object/array iterators advance by sibling cursors, strings advance by one
  cursor, and scalar spans depend on cursor offsets at number/literal starts.
  Feeding raw scanner positions into the current tape would expose commas,
  colons, and closing string quotes as value cursors and would omit scalar
  starts, breaking the retained cursor contract.
- Rejecting W3 before source edits is correct under SPEC Section 6. Section 6
  requires representation replacement inside one retained `Tape`, deletion of
  the old offset append path and parser-owned cursor/fact slots, generated JSON
  retained Track 1 as the same-wave production consumer, retained
  view/`ValueRef` parity, Track 2 independence, scalar/checkasm parity, Lock 14,
  full-table maintain, and W3 LOC/time caps. The present implementation cannot
  meet those requirements as a bounded patch because the scanner lacks class
  ordinals and scalar facts, the parser still owns tape emission, and view/value
  semantics are built over the current semantic offset stream.

## Verification

- Reviewed HEAD: `fc91c2173e8451dd06733381346bd800b0711f6e`
  (`docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`).
- Reviewed:
  - `restart/skinny/tranches/sk-v8/research/skv8-W3-tape-structural-research.md`
  - `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md`
  - `restart/skinny/tranches/sk-v8/SPEC.md` Section 6
- Source checks:
  - `skinny/crates/runtime/src/grammars/json/scan.rs`
  - `skinny/crates/bbnf-simd/src/lib.rs`
  - `skinny/crates/runtime/src/tape/assembler.rs`
  - `skinny/crates/runtime/src/grammars/json/parser.rs`
  - `skinny/crates/runtime/src/grammars/json/generated.rs`
  - `skinny/crates/runtime/src/grammars/json/view.rs`
  - `skinny/crates/runtime/src/grammars/json/value.rs`
  - `skinny/crates/bbnf-bench/src/parity.rs`
  - `skinny/crates/bbnf-bench/src/materialization.rs`
- Commands run:
  - `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture` PASS
  - `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture` PASS
  - `git diff --exit-code HEAD -- skinny/RESULTS.md` PASS
  - `git diff --check` PASS

## Required Folds

- Fold the W3 rejection into `skinny/REDRESS.md`, naming the scanner/tape
  event-model mismatch and the failed Section 6 fit gate.
- Fold W3 disposition into `restart/skinny/tranches/sk-v8/HANDOFF.md` as
  rejected/routed so W4 can proceed only under the Section 6 downstream rule.
- Fold the future work into SK-V9/Pass Omega as a split precursor: define the
  retained class/event grammar, including number/literal starts and string
  quote ownership, prove the retained `ValueRef` cursor contract over that
  grammar, and only then reopen structural-heavy parse row measurement.
- No W3 source implementation is authorized by this challenge result.

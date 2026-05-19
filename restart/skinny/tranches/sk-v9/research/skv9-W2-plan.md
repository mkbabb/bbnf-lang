# SK-V9 Wave W2 Plan: Retained Class/Event Grammar + ValueRef Proof

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 5;
`restart/skinny/tranches/sk-v9/research/skv9-W2-research.md`;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`;
`skinny/REDRESS.md` Item 92.

Intervention: land the proof-only retained class/event grammar contract that
REDRESS 92 routed before W3 can reopen structural-heavy parse changes. W2 adds
`EventGrammar`, an `AnyGrammar` default, JSON and Sheets compile-only
witnesses, and proof tests for `ValueRef<'tape, 'src, K, G>`.

## Owner Paths

The plan folds one owner-table correction into `SPEC.md`: Section 5 names
`#[cfg(any(test, feature = "proof"))]`, but `runtime` has no `proof` feature.
The first CHALLENGE pass additionally rejected the direct `K -> G`
replacement because current generated JSON views use `K` as node-kind identity.
The redress owner list is therefore:

- `skinny/crates/runtime/src/tape/event_grammar.rs` (new):
  `EventGrammar`, `AnyGrammar`, and opaque fact-id helpers only.
- `skinny/crates/runtime/src/tape/event_grammar_tests.rs` (new):
  compile-only proof tests, including the negative borrow-check fixture.
- `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs` (new):
  JSON witness with opaque class/fact ids.
- `skinny/crates/runtime/src/grammars/sheets_witness/mod.rs` and
  `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs`
  (new): non-JSON Lock 14 witness.
- `skinny/crates/runtime/src/tape/mod.rs`: add the event-grammar module,
  preserve `ValueRef<'doc, 'input, K = AnyKind>` as the existing node-kind
  marker, and add `G: EventGrammar = AnyGrammar` as a fourth zero-sized
  retained event-grammar marker.
- `skinny/crates/runtime/src/lib.rs`: add parent `cfg(any(test, feature =
  "proof"))` module declarations for the two witnesses.
- `skinny/crates/runtime/Cargo.toml`: declare `proof = []`; no dependency,
  default-feature, or production build change.

No `skinny/RESULTS.md`, generated JSON runtime file, codegen template,
benchmark crate, fixture, or parser/scanner source path is owned by W2.

## Falsifiability Gate

`G-W2-RETAINED-PROOF` passes only if:

- `cargo check -p runtime --features proof` succeeds.
- `cargo test -p runtime event_grammar --features proof -- --nocapture`
  succeeds, including the three compile witnesses:
  `JsonEventGrammar`, `SheetsEventGrammar`, and `AnyGrammar`.
- The negative proof fixture attempts to construct
  `ValueRef<'static, 'static, AnyKind, JsonEventGrammar>` from a local tape
  and is rejected by the borrow checker.
- `cargo build -p runtime` succeeds without `proof`; witness modules are cfg
  excluded from the default library.
- `git diff --exit-code HEAD -- skinny/RESULTS.md` stays clean.
- Lock 14 audits hold:
  `rg -n 'admits_(fact|class)|STRUCTURAL_CLASS_COUNT|FactId' skinny/crates/runtime/src`
  shows matches only in `tape/event_grammar.rs`, witness files, or proof tests;
  `rg 'event_grammar|event_grammar_witness' skinny/crates/bbnf-bench/`
  returns zero.

## CHALLENGE Questions

The mandatory W2 CHALLENGE must accept or reject:

- The SPEC owner-table correction adding `runtime/Cargo.toml` for
  `proof = []`.
- The parent-module cfg shape in `runtime/src/lib.rs`, including the decision
  not to hand-edit generated `grammars/json/mod.rs`.
- The no-dependency negative proof harness, which invokes a temporary cargo
  check against `runtime` with `features = ["proof"]` and expects failure.
- The "behavior-identical default build" wording: byte-identical output is not
  credible after changing public generic parameters, but no default witness
  module or production parser path is linked.

## Revert Protocol

Five slices map to the P2-B plan:

- S1 trait/default: revert `event_grammar.rs` and the `tape/mod.rs` module
  export.
- S2 JSON witness: revert only the JSON witness and its parent cfg declaration.
- S3 Sheets witness: revert only the `sheets_witness` directory and its parent
  cfg declaration.
- S4 `ValueRef` parameterization: revert the fourth marker/default addition in
  `tape/mod.rs`. If generated-view call-site edits become necessary, stop
  redress and revise to a wrapper alias design.
- S5 cfg/tests: revert `event_grammar_tests.rs` and `proof = []`.

A failed proof reverts the whole wave; W2 has no partial admission and blocks
W3 until accepted.

## Pre-Blocked Routes

- REDRESS 92: W2 is the routed proof precursor, not a production structural
  reopen.
- REDRESS 50/51/53 and 60-72: W2 adds no parser-written side table, parser
  cursor/fact slot, retained-parse hot-path change, or row movement.
- REDRESS 71: W2 is retained-tape proof, not the admitted typed DirectBuild
  contract.
- Lock 14: generic runtime code receives no grammar `match` arm or grammar role
  enum; JSON/Sheets facts remain witness-local opaque ordinals.

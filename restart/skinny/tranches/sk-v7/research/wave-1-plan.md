# SK-V7 Wave 1 Plan: descriptor-preserving TapeKind rename

Inputs: `restart/skinny/tranches/sk-v7/SPEC.md` §3,
`restart/skinny/tranches/sk-v7/HANDOFF.md` §3,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md` §4,
`restart/skinny/tranches/sk-v7/research/wave-1-r1-ir-rename.md`,
`restart/skinny/tranches/sk-v7/research/wave-1-r2-passes-materialization.md`,
and `restart/skinny/tranches/sk-v7/research/wave-1-r3-codegen-regen.md`.

Intervention: descriptor-preserving TapeKind rename.

## Owner Paths

- `skinny/crates/ir/src/lib.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs` (verification only;
  expected unchanged)
- `skinny/crates/runtime/src/grammars/json/generated.rs` (regen check only;
  expected unchanged)
- `skinny/RESULTS.md` (expected unchanged)
- `skinny/REDRESS.md`

## Implementation Shape

- Rename `TapeKind::{Object,Array,Pair,String,Number,Bool,Null}` to
  `TapeKind::{Container,Sequence,KeyValuePair,StringValue,NumberValue,BoolValue,NullValue}`.
  Keep `Member` and `Element` unchanged.
- Rename `DirectBuildDecode::{JsonString,JsonNumber}` to
  `DirectBuildDecode::{EscapedString,NumberScalar}`. These names follow the
  Lock 14 audit prescription and distinguish decode policy from tape value kind.
- Delete `passes::materialization_for_rule`.
- Preserve behavior by replacing the split helper with one local
  `materialization_descriptor(name)` that returns the renamed `TapeKind`,
  existing `Json*` shape string, and existing DirectBuild field roster together.
  The old shape strings stay because broad codegen JSON rebranding is W8 scope,
  not W1.
- Add a focused passes test that compiles the JSON grammar and checks all seven
  materialized rules for the renamed `TapeKind`, existing shape name, and field
  roster shape. This closes the current test gap where only `object` is
  asserted for `TapeEmit` + `DirectBuild`.

## Falsifiability Gate

- `rg -n 'TapeKind::(Object|Array|Pair|String|Number|Bool|Null)|DirectBuildDecode::(JsonString|JsonNumber)|fn materialization_for_rule' skinny/crates`
  returns no matches.
- `cargo run -p xtask --release -- check-json` passes, proving generated JSON
  runtime output is byte-identical.
- `cargo run -p xtask --release -- check-real-typed` passes if DirectBuild
  lowering is touched by the patch.
- `cargo test --workspace` passes.
- `git diff -- skinny/RESULTS.md` is empty.
- `git diff -- skinny/crates/codegen/src/json_templates/generated.rs skinny/crates/runtime/src/grammars/json/generated.rs`
  is empty.
- Lock 14 targeted HIGH leak count drops by three: the old grammar-named
  `TapeKind` variant group, the `DirectBuildDecode::JsonString` policy name,
  and the `DirectBuildDecode::JsonNumber` policy name.

## Hard Cap

90 minutes total. At 0.9x cap, commit the current accepted or rejected state
with measurement. At cap, halt W1 with REDRESS evidence rather than deferring.

## Revert Protocol

If tests or regen checks fail because generated output changes unexpectedly,
save the patch to `/tmp/skv7-wave-1-rejected.patch`, revert the W1 source
edits, and record a REDRESS rejection naming the changed generated surface.

If deleting `materialization_for_rule` drops any `TapeEmit` or `DirectBuild`
descriptor, reject the helper-deletion shape and name the next candidate:
fact-table materialization descriptor with explicit seven-rule regression
tests.

## Same-Wave Consumer

The same-wave consumers are `passes::extract::single_plan` and the codegen
lowering path exercised by `cargo run -p xtask --release -- check-json`,
`cargo run -p xtask --release -- check-real-typed`, and
`cargo test --workspace`.

## Pre-Blocked Routes

Per `restart/skinny/tranches/sk-v7/HANDOFF.md` §3, W1 does not reopen
REDRESS 28+33 Class A `match_tiny_plain_string` wiring, REDRESS 50-55 SK-V5
UTF-8 fusion routes, or REDRESS 60-72 SK-V6 retained-parse and direct
materialization routes. W1 also does not implement W2+ number parsing, W3
DirectBuild mesh specialisation, W4/W5 string kernels, or W7/W8 broad Lock 14
codegen/parse-that-regex cleanup.

# SK-V7 W7 R4 Verification Gate Research

Date: 2026-05-16.

Scope: read-only review for W7 Phase 1. Inputs inspected: SK-V7 SPEC
Section 9, HANDOFF Section 3, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and
the current xtask/codegen/pass/parse-that-regex test surfaces. No source edit
is recommended by this artifact.

## W7 Gate Authority

W7 is a Lock 14 cleanup/refactor wave, not a throughput intervention. SPEC
Section 9 scopes Phase A to `parse-that-regex/src/lib.rs` and Phase B to
`passes/src/lib.rs`. The exact exit gate is:

| Gate item | Required proof |
|---|---|
| Parse primitive API is grammar-neutral | No JSON-prefixed public types remain in `parse-that-regex`; specifically the current `JsonStringMatch` and `JsonNumberMatch` public structs and return types must collapse to grammar-neutral string/number span types. |
| Pass pipeline is grammar-neutral at compile entry | `passes::compile(&GrammarIr)` must consume the grammar parameter without hardcoded `shapes_for_json`, `nominate_json`, or literal rule-name materialization as the decision source. JSON shape names may remain as generated grammar-local output facts, but generic pass selection may not match on JSON rule names as the only route. |
| Workspace correctness | `cargo test --workspace` must pass after Phase A and after Phase B, per SPEC Section 9. |
| Lock 14 count | The Lock 14 HIGH leak count must drop by 20, at least a 44% reduction. |
| Generated output | Generated runtime output must be byte-identical except for mechanically necessary symbol renames caused by the parse-that-regex API rebrand. No semantic generated-output drift is allowed. |

The current code confirms the risk surface: `parse-that-regex/src/lib.rs`
still exposes `JsonStringMatch`, `JsonNumberMatch`, `match_json_number*`, and
`match_json_string*`; `passes/src/lib.rs` still calls
`shapes::shapes_for_json()` and `recognizers::nominate_json()` inside
`compile()`, and `extract::materialization_descriptor()` still maps literal
rule names such as `object`, `array`, `pair`, `string`, `number`, `bool`, and
`null` to JSON shape names.

## Correctness Commands

Minimum W7 redress verification:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p parse-that-regex
cargo test -p passes
cargo test -p codegen
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-real-typed
cargo run -p xtask --release -- check-conformance
cargo test --workspace
```

`check-json` is the byte-for-byte generated runtime check. In
`skinny/crates/codegen/src/lib.rs`, `emit_json_from_source()` renders an
`EmittedSource`, and `EmittedSource::check_dir()` reads every file in
`crates/runtime/src/grammars/json` and returns `DifferentFile(path)` if any
actual file string differs from the emitted string. The same pattern applies to
`check-real-typed` for the generated direct typed module.

Lock 14 grep checks for W7 should include at least:

```bash
rg -n 'JsonStringMatch|JsonNumberMatch' crates/parse-that-regex crates/passes
rg -n 'shapes_for_json|nominate_json|materialization_descriptor|materialization_for_rule' crates/passes/src/lib.rs
rg -n 'rule_by_name\("json"\)|StructuralAlphabet::json\(\)' crates/passes/src/lib.rs
```

The first command should return zero in `parse-that-regex` after Phase A,
apart from any compatibility alias explicitly approved by the plan. The passes
commands should prove that JSON selection is not baked into generic compile
entry or literal-name materialization. If a temporary compatibility alias is
kept for downstream staged compilation, it must be outside the final W7 exit
gate because SPEC requires no JSON-prefixed types in the parse-that-regex public
API.

## RESULTS.md

`skinny/RESULTS.md` should remain unchanged for W7. The wave does not name a
performance row, does not alter the comparator plane, and does not claim a
throughput falsifier. Its evidence is a Lock 14 count reduction plus correctness
and byte-identical generated output. If a bench or gate command is run as a
sanity check, any Criterion noise or `RESULTS.md` rewrite is not W7 evidence
and should not be staged unless a later plan explicitly broadens the wave.

Use this post-check before the W7 redress commit:

```bash
git diff --exit-code -- skinny/RESULTS.md
```

A non-empty diff means the W7 candidate has leaked into gate authority or
captured benchmark noise. For this refactor wave, the correct disposition is to
restore `RESULTS.md` before committing, unless the orchestrator explicitly
opens a measured performance sub-cycle.

## Generated-Output Invariant

The implementation should snapshot generated output before the refactor and
compare after the refactor:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
git diff -- skinny/crates/runtime/src/grammars/json
cd skinny
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-real-typed
```

For a pure W7 Phase A+B close, acceptable generated-runtime differences are
limited to API symbol spelling that follows the parse-that-regex rename, such
as return types and imports moving from JSON-prefixed match types to
grammar-neutral span types. Byte-identical means no changed parser control,
offset emission, sink-only direct code, structural alphabet, tape flags, view
logic, error offsets, or generated file ordering. `codegen` already has tests
for deterministic emission and expected file ordering; these should remain
green.

If the implementation touches a codegen template to follow the API rename,
`runtime/src/grammars/json/generated.rs` and
`codegen/src/json_templates/generated.rs` must stay in lockstep. The clean proof
is `xtask check-json` plus an inspected diff showing only the mechanical rename.

## Pre-Blocked Routes To Cite

W7 plans and redress entries must cite HANDOFF Section 3 and the current
REDRESS ledger. The following rejected route clusters are especially relevant
because they can masquerade as a Lock 14 cleanup but would reopen measured
performance/substrate work:

| Route cluster | Why it is blocked for W7 |
|---|---|
| REDRESS 28+33 Class A NEON tiny-string wiring | Twice rejected as the parse-G fix; W7 must not use API renaming to rewire string matching. |
| REDRESS 50-55 UTF-8 fusion routes | Parse-time retained projection, byte-class whitespace cursor, parser-local structural-mask cursor, decoded-string stats sink, and quote-source fused streaming materializer are blocked. |
| REDRESS 60-72 retained/direct materialization routes | Includes delayed-wide retained string scan, Unicode escape run validator, object next-key carry, global tiny-string cap, direct source-hook layout materializer, parser-owned decoded scratch, byte-output unescape, and DirectBuild semantic string facts. |
| REDRESS 83 W5 StringBlock16 tiny probe | The latest W5 generated-retained 16-byte string-block probe failed same-row throughput and must not be reopened as a grammar-neutral API cleanup. |
| REDRESS 84 W6 object-pair value-byte compaction | The latest control/key compaction was correctness-green but failed W6 measurements; W7 must not reintroduce value-byte carry, object next-key carry, separator elision, function-pointer dispatch, generic SWAR whitespace, or EventCursor sidecars. |
| Earlier blocked churn | 12-byte token width churn, pair-token fusion, function-pointer dispatch table, capacity prescan, generic SWAR whitespace skipper, separator elision, raw f64 shortcut, PSI/DTA Rust-codegen automaton, and EventCursor parallel prepass remain closed. |

## Plan Recommendation

The W7 plan should be phrased as a refactor with an invariant, not a benchmark
optimization. A suitable intervention name is "Lock 14 Phase A+B API and pass
neutralization." Its same-wave consumer is compilation of the existing JSON
grammar through `passes::compile()` and `codegen::emit_json_from_source()`;
there is no new runtime consumer and no new SIMD primitive. The falsifier is
any one of: workspace tests fail, generated output drifts beyond mechanical
renames, `RESULTS.md` changes, JSON-prefixed parse-that-regex public types
remain, passes still hardcode JSON selection in `compile()`, or the Lock 14
HIGH count reduction is below 20.

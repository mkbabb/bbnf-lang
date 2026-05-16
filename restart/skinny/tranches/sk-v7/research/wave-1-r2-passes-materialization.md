# SK-V7 W1 R2 -- passes materialization diagnosis

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: read-only diagnosis for SPEC section 3, HANDOFF section 3, and `skinny/crates/passes/src/lib.rs` around `materialization_for_rule`.
Repo edits: this report only.

## Authority

- SPEC section 3 defines W1 as the lowest-risk Lock 14 slice: rename `TapeKind` and `DirectBuildDecode`, delete `passes::materialization_for_rule`, re-run codegen, require `cargo test --workspace`, unchanged RESULTS, and Lock 14 HIGH leak count -3.
- HANDOFF section 3 pre-blocks REDRESS 50-55 UTF-8 fusion routes, REDRESS 60-72 retained-parse/direct-materialization routes, REDRESS 28+33 Class A tiny-string wiring as a parse-G fix, plus older token-width and pair-token-fusion churn.
- B3 cohort context says the seven `TapeKind::` consumers are clustered in `passes::materialization_for_rule`; deleting or absorbing that helper is the intended low-risk way to remove those consumers before the broader Lock 14 cleanup.

## Current Surface

`extract::single_plan` lowers every grammar rule body, then immediately calls `materialize_rule(&rule.name, expr)` before wrapping the entry rule in `BackendExpr::Entry` (`passes/src/lib.rs:661-669`). Entry discovery is still JSON-specific through `rule_by_name("json")` (`passes/src/lib.rs:658-660`), but that is outside the narrow R2 owner slice.

`materialize_rule` is the only caller of `materialization_for_rule` (`passes/src/lib.rs:719-739`). For matching rules it emits:

1. `SpanMark { Start, label: name }`
2. lowered rule body
3. `SpanMark { End, label: name }`
4. `TapeEmit { kind }`
5. `DirectBuild { shape, fields: direct_fields_for_rule(name) }`
6. `Return`

For non-matching rules it returns the lowered body unchanged (`passes/src/lib.rs:719-722`). This makes `materialization_for_rule` a gate for both retained tape emission and DirectBuild shape extraction.

## Seven JSON-Named Arms

| Rule name | Current tape kind | Current shape | Current DirectBuild fields |
|---|---|---|---|
| `object` | `TapeKind::Object` | `JsonObject` | `members` from repeated rule `pair` |
| `array` | `TapeKind::Array` | `JsonArray` | `elements` from repeated rule `value` |
| `pair` | `TapeKind::Pair` | `JsonPair` | `key` from child rule `string`; `value` from child rule `value` |
| `string` | `TapeKind::String` | `JsonString` | `span` from span label `string` |
| `number` | `TapeKind::Number` | `JsonNumber` | `span` from span label `number` |
| `bool` | `TapeKind::Bool` | `JsonBool` | `value` from empty literal bytes |
| `null` | `TapeKind::Null` | `JsonNull` | no fields |

The seven arms live at `passes/src/lib.rs:742-750`; the field rosters live in `direct_fields_for_rule` at `passes/src/lib.rs:755-808`. The two helpers are coupled: a rule can only get fields after `materialization_for_rule` has admitted it.

## Consumer Contract

The sink-only lowerer ignores the concrete tape kind value but preserves `TapeEmit` as a structural marker and turns `DirectBuild { shape, fields }` into the program's direct shape roster (`codegen/src/lower/sink_only.rs:160-176`). `json_sink_direct::validate` requires all seven `Json*` shapes and exact field rosters (`codegen/src/json_sink_direct.rs:8-16`, `codegen/src/json_sink_direct.rs:52-112`). `schema_direct::lower_program` also requires the same seven shape names before typed DirectBuild lowering (`codegen/src/lower/schema_direct.rs:16-29`).

Therefore deleting `materialization_for_rule` without a replacement is not behavior-preserving: `SinkOnlyProgram.direct_shapes` becomes empty, JSON sink rendering fails with missing DirectBuild shapes, and typed direct rendering fails with missing sink-only shapes.

## Tests That Pin The Surface

- `passes::tests::compiles_json_to_single_plan_bir` asserts the JSON compile path produces 15 rules and that the `object` rule contains both `TapeEmit` and `DirectBuild` (`passes/src/lib.rs:828-850`). It does not pin all seven materialized rules, shape names, field rosters, or renamed `TapeKind` variants.
- `passes::tests::json_shapes_are_curated` asserts the curated shape count is 9 (`passes/src/lib.rs:865-869`), but it tests `shapes_for_json`, not the extracted BIR materialization.
- Codegen tests `refuses_direct_parser_without_direct_builds` and `refuses_typed_emission_without_direct_builds` strip `DirectBuild` nodes and require lowering failure (`codegen/src/lib.rs:320-345`, `codegen/src/lib.rs:350-377`). They pin the existence of DirectBuild nodes only indirectly.
- `json_sink_direct::validate` and `schema_direct::lower_program` provide runtime lowering gates for the seven shape names and required fields; these are stronger than the passes unit test but are not a focused W1 regression test.

Test gap: no current test asserts that all seven rules are materialized, that each rule gets the intended renamed `TapeKind`, or that `DirectBuild` field rosters survive the helper deletion.

## Candidate Implementation Shape

Behavior-preserving W1 should not remove materialization semantics. It should remove the JSON-named tape consumers and the standalone `materialization_for_rule` helper.

Recommended W1 redress shape:

1. Rename `TapeKind` in `ir/src/lib.rs` to grammar-neutral semantic events, for example:
   - `Object` -> `Container`
   - `Array` -> `Bucket`
   - `Pair` -> `KeyValuePair`
   - `String` -> `StringValue`
   - `Number` -> `NumberValue`
   - `Bool` -> `BoolValue`
   - `Null` -> `NullValue`
   - keep `Member` and `Element` if no better semantic split is needed in W1.
2. Rename `DirectBuildDecode::{JsonString, JsonNumber}` to grammar-neutral variants such as `StringValue` and `NumberValue`; these currently have no non-definition consumers in skinny, so this should be IR-local.
3. Delete `materialization_for_rule` and absorb the seven decisions into `materialize_rule` through one local descriptor match or table that returns a complete descriptor: rule name, renamed `TapeKind`, shape string, and fields. This keeps one admission point and avoids a separate helper whose only purpose is JSON name mapping.
4. Keep `direct_fields_for_rule` only if W1 needs a minimal diff, but the cleaner behavior-preserving form is a single descriptor producer so shape admission and field roster cannot diverge. Full fact-driven deletion of rule-name mapping belongs to the later Lock 14/CostFacts work, not this W1 slice, unless the wave plan is explicitly widened.
5. Add a focused passes unit test that compiles the JSON grammar and checks the seven materialized rules, renamed `TapeKind` values, shape names, and field names. This gives W1 a precise regression pin before broader codegen tests run.

This shape preserves current codegen output because shape strings and field rosters stay unchanged; only internal enum variant names and helper boundaries change.

## Risks

- The SPEC asks to delete `passes::materialization_for_rule`, but B3's broader Class A recommendation deletes name-based materialization by consuming future `DirectFieldFacts`. W1 cannot honestly achieve full Class A generality without pulling later substrate into this wave. Treat W1 as helper deletion plus grammar-neutral enum rename, not full fact-driven Lock 14 closure.
- If `materialize_rule` drops any of the seven DirectBuild shapes, `json_sink_direct` and typed direct lowering will fail even though the `TapeKind` rename compiles.
- The existing passes unit test only inspects `object`; a broken `array`, `pair`, `string`, `number`, `bool`, or `null` materialization can escape until codegen lowering.
- Renaming serialized enum variants can affect any persisted BIR snapshots if they exist outside the current test tree. `rg` found no skinny consumer of `DirectBuildDecode::{JsonString, JsonNumber}` beyond the definition, but external consumers should be considered at release boundaries.
- Running `cargo fmt -p bbnf-bench` is unrelated to W1 and can touch generated files; W1 should keep formatting scoped to changed crates or regenerate generated fixtures afterward.

## Verification Commands

Minimum W1 verification:

```sh
cargo test -p passes
cargo test -p codegen
cargo test --workspace
cargo run -p xtask -- regen-real-typed
git diff -- skinny/crates/codegen/src/json_templates/generated.rs skinny/crates/runtime/src/grammars/json/generated.rs
git diff -- skinny/RESULTS.md
rg -n 'TapeKind::(Object|Array|Pair|String|Number|Bool|Null)|DirectBuildDecode::(JsonString|JsonNumber)|fn materialization_for_rule' skinny/crates
```

Expected outcomes:

- passes and codegen tests green.
- workspace tests green.
- generated JSON code byte-identical except for intentional rename-only differences if codegen prints debug-derived enum names.
- `skinny/RESULTS.md` unchanged.
- no remaining `TapeKind::Object`, `TapeKind::Array`, `TapeKind::Pair`, `TapeKind::String`, `TapeKind::Number`, `TapeKind::Bool`, `TapeKind::Null`, `DirectBuildDecode::JsonString`, `DirectBuildDecode::JsonNumber`, or `materialization_for_rule` hits.

## Findings

1. `materialization_for_rule` is not blocked on performance work; it is a seven-arm helper with a single caller.
2. It is semantically load-bearing because it gates `TapeEmit`, `DirectBuild`, and `Return` insertion for the seven JSON value-shape rules.
3. The actual behavior contract is the DirectBuild shape/field roster, not the old `TapeKind` names. Codegen validates `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, and `JsonNull`.
4. A W1 implementation can delete the helper and rename enum variants without row delta, but a fully grammar-neutral materialization substrate is later-wave work unless W1 is widened.
5. Add the focused seven-rule materialization test before or with redress; existing tests are too coarse for this exact surface.

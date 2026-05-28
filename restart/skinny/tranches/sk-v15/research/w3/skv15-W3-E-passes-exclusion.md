# SK-V15 W3-E Research: Generic Passes Exclusion

Scope: `skinny/crates/passes/src`.

Status: read-only research.

## Findings

`passes::compile()` runs generic stages and does not branch on
`grammar.name == "json"` or `css_l4` in production control flow. The relevant
pipeline starts around `passes/src/lib.rs:37-48`.

The recognizer code is JSON-shaped by byte alphabet, not by grammar family.
It scans structural literal bytes and quoted-string regexes for any grammar
around `passes/src/lib.rs:332-355`, then emits `Recognizer::SimdScan`. This
is not the same live leak family as runtime profile selection.

DirectBuild materialization uses structural roles around
`passes/src/lib.rs:1354-1418`: string/number regex roles, boolean/null
literals, object/array delimiters, member separators, and value/pair
references. The test at `passes/src/lib.rs:1798-1840` compiles a renamed
`sample_json` grammar and expects generated `SampleJson*` shapes, proving the
logic is name-independent.

`decision_csp.rs` has static JSON/CSS evidence labels, but they are status
fields for Decision Engine work. Those route to W7 rather than W3.

## Recommendation

Exclude `skinny/crates/passes/src` from W3 implementation. The current W3
owner should be codegen profile/provider/runtime-generator metadata. Pulling
structural recognizer/materialization debt into W3 would widen the wave and
risk violating the 150-320 manual LOC envelope.

## Routed Work

Generic structured-value materialization naming debt can be routed as future
Decision/value generalization only after W5/W7 provide typed CSS and active
Decision Engine proof. It is not a W3 blocker because it is not a live
grammar-family branch.

## Grep Terms

```sh
rg -n "grammar\\.name\\s*==|==\\s*\"json\"|==\\s*\"css|css_l4|parse_json_grammar|Recognizer::SimdScan|JsonObject|SampleJson" skinny/crates/passes/src
```

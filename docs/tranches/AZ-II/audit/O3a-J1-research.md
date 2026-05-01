# AZ-II O3a-J1 JSON Research

Status: research-only. No source files were edited.

Worktree: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-research`.
Branch: `codex/azii-o3a-j1-research`.
Date: 2026-04-29.

## Evidence Commands

- `cargo nextest run -p bbnf --profile ax-iter --test json_parity bool_false_materialises_to_bool_false bool_true_materialises_to_bool_true every_declared_leaf_reaches_the_document nested_object_preserves_typed_payloads --no-fail-fast -- --nocapture`
  - Result: 4/4 failed.
  - Key outputs: `true` / `false` roots failed `JsonValue::Bool`; `every_declared_leaf_reaches_the_document` failed at `items[1]` expecting `JsonValue::Bool(true)`; `nested_object_preserves_typed_payloads` counted 4 nulls where 2 were expected.
- `cargo nextest run -p bbnf --profile ax-iter --test json_value_parity simdjson_parity_flat_object simdjson_parity_mixed_array json_parses_nested_object simdjson_parity_scalars --test structural structural_object_two_pairs --test wrap_compound_elision json_object_of_scalars_record_ceiling json_scalar_at_top_level_emits_one_record --test typed_accessor_surface json_accessor_surface json_compile_time_accessors --no-fail-fast -- --nocapture`
  - Result: 9/9 failed.
  - Key outputs: nested object saw `bbnf=Null, serde=Bool(true)`; simd-json object/array saw `bbnf=Number but simd-json=U64`; structural object expected >=5 nodes but got 3; JSON accessor audit reported `Alt:0`; wrap tests saw `Null` where `Bool` was expected.
- `cargo nextest run -p bbnf --profile ax-iter --test json_canonical_parity canonical_parity_twitter --test json_parity parity_twitter_json --test json_parity_struct native_parity_serde_twitter_json native_parity_serde_canada_json --test sonic_rs_parity sonic_rs_parity_twitter sonic_rs_parity_data_xl --no-fail-fast -- --nocapture`
  - Result: 6/6 failed.
  - Key outputs: twitter parity first diverged at `$.statuses[0].truncated` as `bbnf=Null` vs external `Bool(false)`; `sonic_rs_parity_data_xl` first diverged at `data_xl.json[0].active`; `native_parity_serde_canada_json` failed on numeric precision, `43.47470900000013` vs `43.474709000000125`.
- `rg -n "data_xl|timeout|timed|2\\.478|json_monolithic|FAIL|error" docs/benchmarks/archive/post-AY-az-ii-doc-baseline-json.txt`
  - Result: `json_monolithic::data_xl` exceeded the 1s timeout at `2.478697958s`.

## Root-Cause Classes

### J1.RC1 - JSON bool keyword branches materialise as null

The generated struct-direct JSON keyword parser emits `push_leaf_with_unit()` for both `false` and `true`:

- `crates/core/src/grammar/generated/json.rs:1077-1104` dispatches the `bool` rule and pushes unit for both branches.
- The grammar requires bool payloads: `grammar/json/json.bbnf:2` is `bool = "true" -> true | "false" -> false ;`.
- The runtime contract requires bools to reach `push_leaf_with_bool`: `crates/core/src/runtime/json/builder.rs:31-37`, with the implementation at `builder.rs:352`.
- The keyword emitter source has a bool arm that would emit `push_leaf_with_bool` when the rule type is `TypeDesc::Bool`: `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs:48-84`. The generated output proves that this condition is not true for current JSON IR, or that branch payload typing is lost before emission.

Exact next probe: dump `ir.types`, `ir.payload_layouts`, and `alt_branch_payload_value` for the JSON `bool` rule immediately before `emit_parse_keyword_struct_direct`. The fix owner must prove whether the loss happens in value-expression lowering / project-types typing or inside `keyword/struct_direct.rs`, then regenerate `json.rs`.

### J1.RC2 - JSON numbers always project through f64, while parity tests compare against integer-preserving oracles

The generated number parser always calls `builder.push_leaf_with_f64(value)`:

- `crates/core/src/grammar/generated/json.rs:1243-1257` computes a floating value and pushes f64.
- `grammar/json/json.bbnf:4` declares `number ... -> f64`.
- `JsonNumber` has integral variants, but `JsonNumber::Float` is the grammar's current projection path: `crates/core/src/runtime/json/value.rs:55-79`.

This explains:

- simd-json object/array failures: test output reports `bbnf=Number but simd-json=U64` for `$.a` and `$[0]`.
- Canada serde parity failure: generated number parse produced `43.47470900000013`; serde produced `43.474709000000125`.

Exact next probe: isolate `parse_number_JsonParser_number` on the failing Canada coordinate and on integer literals used by simd-json parity, compare generated fast path vs `parse_number_fallback`, `serde_json::Number::as_f64`, and `sonic_rs::Value::as_f64`. The owner must decide whether O6 parity compares at the declared f64 layer or the grammar/runtime must preserve integer/decimal witnesses.

### J1.RC3 - JSON accessor audit expects Alt views that current IR no longer classifies as Alt views

`json_accessor_surface` is not failing on parser output. The focused run printed:

```text
total rules: 8 (1 transparent, 7 view-emitted)
Aggregate: 3
KvPair: 0
LeafSpan: 2
LeafScalar: 1
Seq: 1
Alt: 0
Repeat: 0
```

Then it failed `JSON must emit at least one Alt view`.

Relevant source:

- `crates/core/tests/typed_accessor_surface.rs:333-344` expects JSON `bool` or `value` to count as an Alt view.
- The classifier gives payload layouts precedence over body shape at `typed_accessor_surface.rs:187-194`.
- Transparent rules are skipped at `typed_accessor_surface.rs:293-296`; JSON `value` is the transparent skipped rule in the focused output.

Exact next probe: inspect the compiled JSON IR for `bool` and `value`: if `bool` has a payload layout, the audit expectation is stale and should assert the new payload class; if it should not have a payload layout, project-types is over-classifying bool. This is an accessor/test-classification root cause, not evidence of object projection loss.

### J1.RC4 - `structural_object_two_pairs` retains a tape-style node-count invariant

The parser correctly produced two object pairs before the failing assertion. The failure was:

```text
expected at least 5 nodes for '{"a": 1, "b": "hello"}', got 3
```

Relevant source:

- `crates/core/tests/structural.rs:24-45` counts scalar values as 1 and object nodes as `1 + sum(pair.value)`.
- That helper does not count keys or pair edges.
- `structural_object_two_pairs` then expects at least 5 nodes at `crates/core/tests/structural.rs:202-212`.

For `{"a": 1, "b": "hello"}`, the helper's own semantics produce 3: root object, number value, string value. This is a stale migrated assertion, not object materialization loss.

Exact next probe: decide whether the struct-tree invariant should count keys/pairs explicitly or lower the expected count to the helper's current semantics. Keep this in the test/runtime-view lane, not the JSON parser lane.

### J1.RC5 - `json_monolithic::data_xl` times out on full document materialization

The bench artifact records the timeout:

- `docs/benchmarks/archive/post-AY-az-ii-doc-baseline-json.txt:1457-1470` runs `json_monolithic`; `data_xl` panics because one iteration took `2.478697958s` against a 1s limit.

The bench calls the materializing parse path:

- `crates/core/benches/json/monolithic.rs:28-44` calls `JsonParser::parse`, then uses `parsed.view()` for sanity.
- Generated `JsonParser::parse` constructs `JsonStructBuilder::new()` and finalises a full `JsonDocument`: `crates/core/src/grammar/generated/json.rs:5024-5068`.
- `JsonStructBuilder::with_capacity` exists at `crates/core/src/runtime/json/builder.rs:135-151`, but the generated parse path does not use it.
- `JsonArena` stores one `Vec` per non-empty array/object slab: `crates/core/src/runtime/json/arena.rs:95-146`.

Exact next probe: after RC1/RC2 semantic repair, profile `json_monolithic::data_xl` with allocation attribution and compare three variants: current `new()`, emitted `with_capacity` using registry/profile hints, and the existing visitor path for monolithic parse if O6 intends parse-throughput rather than materialization-throughput.

## Failure Map

| Failed item | Root-cause class | Evidence |
|---|---|---|
| `bbnf::json_parity bool_false_materialises_to_bool_false` | J1.RC1 | focused output: root not `JsonValue::Bool(false)`; generated bool branch pushes unit |
| `bbnf::json_parity bool_true_materialises_to_bool_true` | J1.RC1 | focused output: root not `JsonValue::Bool(true)`; generated bool branch pushes unit |
| `bbnf::json_value_parity json_parses_bools` | J1.RC1 | same bool parser path; `json_parses_nested_object` showed `Null` vs `Bool(true)` |
| `bbnf::json_value_parity simdjson_parity_scalars` | J1.RC1 and J1.RC2 | focused output first hit `Null` vs simd-json `Bool`; integer scalar cases also use RC2 |
| `bbnf::structural structural_scalar_bool_false` | J1.RC1 | same generated bool parser path |
| `bbnf::structural structural_scalar_bool_true` | J1.RC1 | same generated bool parser path |
| `bbnf::serialize_roundtrip json_false` | J1.RC1 | serializer sees parsed `JsonValue::Null`, so `false` emits as `null` |
| `bbnf::serialize_roundtrip json_true` | J1.RC1 | serializer sees parsed `JsonValue::Null`, so `true` emits as `null` |
| `bbnf::json_parity every_declared_leaf_reaches_the_document` | J1.RC1 | focused output failed at `items[1]` expecting `Bool(true)` |
| `bbnf::json_parity nested_object_preserves_typed_payloads` | J1.RC1 | focused output counted 4 nulls instead of 2; bools were materialised as nulls |
| `bbnf::json_value_parity json_parses_nested_object` | J1.RC1 | focused output: `$.outer.inner[2]` was `Null` vs `Bool(true)` |
| `bbnf::json_value_parity simdjson_parity_flat_object` | J1.RC2 | focused output: `$.a` was bbnf Number vs simd-json U64 |
| `bbnf::json_value_parity simdjson_parity_mixed_array` | J1.RC2 | focused output: `$[0]` was bbnf Number vs simd-json U64 |
| `bbnf::structural structural_object_two_pairs` | J1.RC4 | focused output: expected >=5 nodes, got 3; count helper excludes keys/pair edges |
| `bbnf::typed_accessor_surface json_accessor_surface` | J1.RC3 | focused output: JSON `Alt:0`; test requires at least one Alt view |
| `bbnf::typed_accessor_surface json_compile_time_accessors` | J1.RC1 | focused output: `JsonParser::parse("true") must yield a Bool root` |
| `bbnf::wrap_compound_elision json_object_of_scalars_record_ceiling` | J1.RC1 | focused output: pair value for `true` was `Null` |
| `bbnf::wrap_compound_elision json_scalar_at_top_level_emits_one_record` | J1.RC1 | focused output: scalar `"true"` got `Null`, wanted `Bool(true)` |
| `bbnf::json_canonical_parity canonical_parity_twitter` | J1.RC1 | twitter contains many booleans; corpus parity run showed boolean fields materialise as null |
| `bbnf::json_parity parity_twitter_json` | J1.RC1 | focused corpus output: `$.statuses[0].truncated` bbnf `Null` vs sonic-rs Boolean |
| `bbnf::json_parity_struct native_parity_serde_twitter_json` | J1.RC1 | focused corpus output: `$.statuses[0].truncated` bbnf `Null` vs serde `Bool(false)` |
| `bbnf::json_parity_struct native_parity_serde_canada_json` | J1.RC2 | focused corpus output: coordinate f64 precision divergence |
| `bbnf::sonic_rs_parity sonic_rs_parity_twitter` | J1.RC1 | focused corpus output: variant mismatch at `twitter.json.statuses[0].truncated` |
| `bbnf::sonic_rs_parity sonic_rs_parity_data_xl` | J1.RC1 | focused corpus output: variant mismatch at `data_xl.json[0].active` |
| `json_monolithic::data_xl` bench timeout | J1.RC5 | bench artifact line reports 2.478697958s > 1s; bench uses full `JsonDocument` materialization |

## Bottom Line

The J1 cohort does not point to general JSON object/projection loss. The dominant semantic failure is bool payload loss in generated struct-direct keyword emission. A separate number-oracle mismatch explains simd-json object/array failures and the Canada serde drift. Accessor and structural failures are test/audit contract drift against the post-O2 struct-tree shape. The `data_xl` bench timeout is a materializing-parser performance issue, with immediate evidence that emitted parse uses `JsonStructBuilder::new()` despite an available capacity constructor.

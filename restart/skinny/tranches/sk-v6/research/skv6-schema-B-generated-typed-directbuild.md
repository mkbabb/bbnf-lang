# SK-V6 Schema-Source Plan B: Generated Typed DirectBuild

## Read Scope

Read requested code and audit context:

- `skinny/crates/codegen/src/lower/`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` section 14
- `skinny/REDRESS.md` item 70

Current facts:

- `BackendExpr::DirectBuild { shape, fields }` exists, but current fields are only `name + DirectBuildSource`.
- Current `DirectBuildSource` is grammar-local: `Span`, `ChildRule`, `RepeatedRule`, `Literal`, `Empty`.
- `passes/src/lib.rs` hard-codes JSON rule shapes (`JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`).
- `lower/sink_only.rs` preserves those shape/field facts but does not specialize by output type.
- `json_sink_direct.rs` renders a JSON-specific `parse_direct<S: JsonSink>`, and runtime `JsonSink` is an event API with `begin_object`, `key`, scalar hooks, and context-specific scalar hooks.
- `bbnf-bench/src/direct_struct.rs` Track 1 is still a generic JSON event sink with a `Vec<DigestFrame>` stack. It proves generated JSON event emission, not grammar-general typed DirectBuild.

## Core Finding

Do not implement the next attempt as `impl JsonSink for UpdateCenterSink` passed to `runtime::generated_json::parse_direct`. That keeps the generic JSON event stream, dynamic key routing, and container stack shape that REDRESS 70 rejects.

The next Track 1 must be a generated schema-specialized DirectBuild parser/receiver:

- Input schema comes from the host/API output contract, not the JSON grammar.
- Generated code returns the owned Rust output type directly.
- Generated code walks JSON and the output schema together.
- No generic JSON event stack, no `serde_json::Value`, no `JsonSink` implementation, and no benchmark-private hand sink.

## No-Go Boundary

Unchanged IR is not enough.

The existing exact `DirectBuildField { name, source }` and `DirectBuildSource` roster cannot carry:

- host Rust type path,
- JSON key literal for a host field,
- required/optional/default/null policy,
- scalar target type (`String`, `bool`, `i64`, `u64`, `f64`, etc.),
- array element type,
- map key/value type,
- recursive type references,
- duplicate/unknown field policy,
- construction policy for the owned output type.

Also, the current rule-level shape model has only one `DirectBuild` for the generic JSON `object` rule. That cannot represent `TwitterSearch`, `Status`, `User`, `UpdateCenter`, `Plugin`, map values, and dynamic map entries at the same time.

Therefore:

- No-go if "existing IR" means no `DirectBuildField`/schema-fact payload additions and no schema-specialized lowered program.
- Go if the `BackendExpr::DirectBuild` variant is retained and the schema is lowered into richer DirectBuild facts or a typed lowered program derived from those facts. This does not require a new `BackendExpr` variant.
- No-go if the plan adds a grammar directive for output types.
- No-go if the plan hides the schema in benchmark code instead of passing it as an explicit codegen/API contract.

## API Shape

Add an explicit schema-source API to `bbnf-codegen`; emit generated source into the consuming crate, not into `runtime`.

Sketch:

```rust
pub fn emit_json_typed_from_source(
    grammar_source: &str,
    schema: &DirectSchemaSet,
) -> Result<EmittedSource, CodegenError>;

pub struct DirectSchemaSet {
    pub module_name: String,
    pub roots: Vec<DirectRootSchema>,
    pub types: Vec<DirectTypeSchema>,
    pub schema_hash: String,
}

pub struct DirectRootSchema {
    pub function_name: String,
    pub rust_type: String,
    pub type_id: String,
}
```

Generated consuming-crate API:

```rust
pub fn parse_twitter_search(input: &str) -> Result<crate::real_typed_struct::TwitterSearch, DirectBuildError<'_>>;
pub fn parse_update_center(input: &str) -> Result<crate::real_typed_struct::UpdateCenter, DirectBuildError<'_>>;
```

The schema should model:

- `Struct`: Rust path, fields, construction mode (`StructLiteral` first), unknown field policy.
- `Field`: JSON key bytes, Rust field name, type ref, required/optional/default/null policy, duplicate policy.
- `Vec<T>`: array value parser.
- `Map<String, T>`: object parser that materializes dynamic keys and parses values as `T`.
- `Option<T>`: null-or-value parser.
- scalars: `String`, `bool`, signed/unsigned integer widths needed by fixtures, `f64`, null/unit.
- recursion: type IDs rather than inlined recursive schema expansion.

Initial schema source may be a Rust builder/macro alongside the typed structs in `bbnf-bench`, but it must reference the actual Rust paths and fields so generated construction type-checks. A derive macro can come later; the first proof does not need Rust reflection.

## Generated Code Shape

Generated Track 1 should look like a schema-state parser, not an event sink.

Example shape:

```rust
pub fn parse_update_center(input: &str) -> Result<UpdateCenter, DirectBuildError<'_>> {
    let bytes = input.as_bytes();
    let mut cursor = 0;
    let out = parse_update_center_at(input, bytes, &mut cursor)?;
    cursor = skip_json_whitespace(bytes, cursor);
    if cursor == bytes.len() { Ok(out) } else { Err(trailing(input, cursor)) }
}

fn parse_update_center_at<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
) -> Result<UpdateCenter, DirectBuildError<'i>> {
    expect_object_start(input, bytes, cursor)?;
    let mut field_a = None;
    let mut field_b = Vec::new();
    let mut seen: u64 = 0;

    while !take_object_end(bytes, cursor)? {
        let key = parse_key(input, bytes, cursor)?;
        expect_colon(input, bytes, cursor)?;
        match_update_center_key(key, input, bytes, cursor, &mut field_a, &mut field_b, &mut seen)?;
        take_comma_or_end(...)?;
    }

    Ok(UpdateCenter {
        field_a: required(field_a, "field_a", cursor)?,
        field_b,
    })
}
```

Required properties:

- object fields are local variables plus a bitmask, not entries in a generic stack;
- arrays push directly into `Vec<T>`;
- maps insert directly into the target map type;
- nested objects are direct function calls returning their typed value;
- recursion uses normal Rust recursion over typed parser functions;
- unknown-field skipping, if allowed, is a generated JSON `skip_value` scanner that does not materialize and does not call `JsonSink`;
- key matching is generated per object type, using raw byte comparison for unescaped keys and decoding only when an escaped key is actually present;
- scalar materializers are field-specific (`parse_u64_field`, `parse_i64_field`, `parse_f64_field`, `parse_string_owned_field`, etc.).

String policy:

- output fields are owned, so plain strings use `String::from(raw)`/`raw.to_owned()`;
- escaped strings use the existing unescape implementation and convert to owned `String`;
- key strings are not allocated except for dynamic map keys or escaped static-key fallback comparison.

Number policy:

- use `match_number_span_from_first` once;
- materialize only the field target type;
- report mismatch/range errors as typed DirectBuild errors;
- do not classify through generic `JsonSink::{i64,u64,f64}` routing first.

## Codegen Owner Files

Add or modify these owners:

- `skinny/crates/codegen/src/schema_source.rs` or `direct_schema.rs`
  - Public schema-source data model.
  - Validation for duplicate Rust fields, duplicate JSON keys, unsupported scalar kinds, map key policy, recursive references, and root type existence.

- `skinny/crates/codegen/src/lower/schema_direct.rs`
  - New lowerer from `(SinkOnlyProgram, DirectSchemaSet)` to `TypedDirectProgram`.
  - Validates that the JSON grammar still exposes required DirectBuild capabilities: object members, array elements, pair key/value, string span, number span, bool literal, null.
  - Produces contextual schema states; this is where generic JSON `object` is specialized into `UpdateCenter`, `Plugin`, `TwitterSearch`, etc.

- `skinny/crates/codegen/src/lower/sink_only.rs`
  - Keep as the grammar capability lowerer.
  - Add query helpers if useful; do not put host schema logic here.

- `skinny/crates/codegen/src/json_typed_direct.rs`
  - Renderer for the generated typed parser module.
  - Owns key-match rendering, object/array/map parser rendering, scalar materializer rendering, typed error rendering, and static provenance comments.

- `skinny/crates/codegen/src/lib.rs`
  - Add `emit_json_typed_from_source` / `emit_json_typed_with_layout`.
  - Keep current `emit_json_from_source` behavior for runtime JSON unchanged.
  - Ensure stripping `DirectBuild` still refuses typed emission.

- `skinny/crates/codegen/src/json_sink_direct.rs`
  - Leave as the existing generic `JsonSink` renderer for the digest stressor.
  - Do not extend this into the typed output path.

- `skinny/crates/ir/src/lib.rs`
  - Prefer no new `BackendExpr` variant.
  - If schema facts must be persisted in IR, add payload/fact structs around `DirectBuildField` or a `DirectBuildSchemaFacts` field on `BackendIr`; do not add `BackendExpr::TypedObject`/`SchemaObject`.

## Runtime Trait Changes

No `JsonSink` trait change is needed for the typed DirectBuild path.

Reason:

- `JsonSink` is exactly the generic event API REDRESS 70 wants to avoid for the representative typed proof.
- Adding more `JsonSink` hooks would still require dynamic key/container state inside the sink.

Allowed runtime/code sharing:

- Reuse public `ParseError`/`ParseErrorKind` if sufficient.
- Prefer a generated `DirectBuildError<'i>` that wraps `ParseError<'i>` and adds schema errors such as missing required field, duplicate field, unexpected null, integer range, and type mismatch.
- Copy or factor direct helper code (`skip_json_whitespace`, string matcher, number matcher, literal consumption) in codegen output. Factoring into runtime helper modules is optional and should not introduce a typed trait.

Crate boundary:

- Runtime must not depend on `bbnf-bench` structs.
- Generated typed parser source should live in the consumer crate (`bbnf-bench` for the proof), with imports to runtime parse error types and `parse_that_regex` helpers as needed.

## Benchmark Harness Changes

Keep `bbnf-bench/src/direct_struct.rs` as the renamed/reported `semantic_full_digest_stressor`. It remains useful but must not be the representative typed closure gate.

Add a separate real typed plane:

- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
  - Owns `TwitterSearch`, `UpdateCenter`, nested structs, typed checksums, and schema-source builder.
  - Track 1 calls generated typed functions, not `runtime::generated_json::parse_direct`.
  - Track 2 remains structurally independent and returns the same owned types.
  - Sonic-rs and serde_json deserialize the same owned types.

- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
  - Checked-in generated output from `bbnf-codegen` or generated by a deterministic build step.
  - Must contain provenance: grammar hash, schema hash, codegen version, and direct shape roster.

- `skinny/crates/bbnf-bench/src/lib.rs`
  - Export `real_typed_struct` and generated module.

- `skinny/crates/bbnf-bench/benches/json_parity.rs`
  - Add `track1_real_typed_struct`, `track2_real_typed_struct`, `sonic_rs_real_typed_struct`, `serde_json_real_typed_struct`.
  - Limit initial rows to `twitter` and `update_center`.
  - Keep existing direct digest rows, but label/report them as `semantic_full_digest_stressor`.

- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`
  - Either add `real_typed_track1/track2/sonic/serde` modes or create `profile_real_typed.rs`.
  - The profile mode must black-box the owned output checksum after parse, not compute checksum during parse.

- `skinny/crates/bbnf-bench/src/gate.rs`, `src/bin/gate.rs`, `src/report.rs`
  - Add a `RealTypedStructInput` gate.
  - Keep digest stressor correctness green but do not let digest throughput veto real typed closure.
  - Report schema hash and generated source hash in metadata.

## Falsifiability Gates

Static/codegen gates:

- Typed emission fails if `DirectBuild` nodes are stripped.
- Typed emission fails if the schema names a field/type/map shape the generator cannot lower.
- Typed emission fails if the schema omits a live required output field.
- Generated Track 1 source must not contain:
  - `impl JsonSink`
  - `parse_direct(`
  - `serde_json::Value`
  - `DigestFrame`
  - a generic `Vec<Frame>`/container event stack
- Generated Track 1 may contain a non-materializing `skip_value` for unknown fields if the schema policy allows unknowns.

Correctness gates:

- For `twitter` and `update_center`, generated Track 1, independent Track 2, sonic-rs, and serde_json return identical owned Rust outputs.
- Post-parse checksum agrees across all four tracks.
- Add focused fixtures/tests for:
  - missing required field,
  - null optional field,
  - null required field,
  - duplicate field policy,
  - unknown field policy,
  - escaped static key,
  - escaped dynamic map key,
  - integer boundary values for each target integer type,
  - recursive nested `retweeted_status` shape if present.

Performance gates:

- Same-HEAD paired scout, five samples per mode.
- Generated Track 1 must be within `1.10x` sonic-rs time on at least one typed fixture and no worse than `1.25x` sonic-rs time on the other, matching the Candidate 11 scout gate.
- Track 2 is reported separately; if it is slower, it is reference-parser residual and not allowed to hide Track 1.
- Existing `semantic_full_digest_stressor` rows must remain correctness-green. Throughput misses stay visible as stressor misses.

Attribution gates:

- Profile generated Track 1 on `update_center`.
- Hot symbols should be typed parser functions, field-specific scalar/string materializers, output allocation/insertion (`Vec`, map), and checksum.
- Reject if hot symbols show generic `JsonSink` event methods, generic key dispatch through `sink.key`, `serde_json::Value`, or a generic container stack.

## Implementation Order

1. Add schema-source model and validation in codegen.
2. Add `TypedDirectProgram` lowerer that combines `SinkOnlyProgram` and schema-source states.
3. Add renderer for generated typed parser source.
4. Add codegen tests:
   - emits deterministic typed parser for a tiny schema,
   - refuses missing DirectBuild,
   - refuses unsupported schema feature,
   - generated source lacks forbidden event-stack markers.
5. Add `real_typed_struct` module and schema for `twitter` / `update_center`.
6. Generate/check in `generated_real_typed.rs`.
7. Wire Track 1/Track 2/serde/sonic correctness tests.
8. Wire profile modes and criterion rows.
9. Run scout performance and attribution gates.
10. Only then update gate/report docs to make `real_typed_struct` the representative DirectBuild closure row.

## Decision Summary

The viable path is not another JSON sink. It is a schema-specialized generated parser/receiver emitted into the consumer crate from an explicit host output schema, validated against existing JSON `DirectBuild` grammar capabilities.

No new `BackendExpr` variant is necessary if richer schema facts are allowed around `DirectBuild` and lowered into a typed codegen program. If the exact current IR payload must remain unchanged, this is a no-go: the current `DirectBuildField` model cannot express the user output schema required by REDRESS 70.

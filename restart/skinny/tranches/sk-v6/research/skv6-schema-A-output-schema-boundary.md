# SK-V6 Schema-Source Research Agent A

Workspace: `/Users/mkbabb/Programming/bbnf-lang`

Committed context: `10abb7b0` rejects SK-V6 Wave 3 Candidate 11 as proof of grammar-general DirectBuild.

## Answer

The host/API output schema should enter at the API/schema boundary before Backend IR extraction materializes direct output fields, not in BBNF syntax and not as a new BIR variant.

Concretely, a V1 caller should supply a typed consumer schema as a schema input alongside grammar metadata / schema facts. In the full architecture this naturally belongs beside `SchemaSet`: `Backend::emit_artefacts` already takes `(grammar, schemas)`, and `SchemaSet` is the bundle for value/visitor/path schemas (`restart/ARCHITECTURE.md:1205-1224`). For direct owned output, the missing member is a direct-output consumer schema inside that schema bundle, or an equivalent side-table input earlier in the pipeline.

In the skinny implementation, the immediate entry point is the fact-construction step that currently hard-codes JSON shapes and field rosters:

- `passes::extract::single_plan` accepts `shape_facts: ShapeFacts` and builds `BackendIr` (`skinny/crates/passes/src/lib.rs:652-682`).
- `materialize_rule` emits `BackendExpr::DirectBuild { shape, fields }` (`skinny/crates/passes/src/lib.rs:719-739`).
- `direct_fields_for_rule` currently hard-codes JSON field rosters (`skinny/crates/passes/src/lib.rs:755-802`).

The conforming replacement is: an external host/API output schema feeds the `ShapeFacts` plus `DirectBuildField` roster that `single_plan` receives or constructs. The BBNF grammar remains unchanged, and the BIR alphabet remains unchanged.

## Why This Is The Right Boundary

REDRESS item 70 says the first `real_typed_struct` implementation was correct but not an architectural close because JSON itself does not contain `TwitterSearch` or `UpdateCenter` (`skinny/REDRESS.md:1876-1886`, `skinny/REDRESS.md:1917-1926`). Grand Synthesis §14 states the same finding directly: sonic-rs gets those shapes from Serde, so BBNF must admit an explicit host/API output type contract lowered into existing `DirectBuild { shape, fields }` (`restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md:1006-1012`).

This boundary also avoids violating the metadata rules. Workspace metadata may name files, profiles, and feature flags, but may not name Rust parser types, generated modules, or builder structs (`restart/ARCHITECTURE.md:671-735`). Therefore `TwitterSearch` / `UpdateCenter` should not be hidden in BBNF or generic grammar metadata as a pseudo-directive. They are API consumer schemas.

## Existing Carriers

No new top-level BIR variant is needed.

Existing carriers:

- `BackendIr.shape_facts: ShapeFacts` carries the available shape catalogue (`skinny/crates/ir/src/lib.rs:325-332`, `skinny/crates/ir/src/lib.rs:460-510`).
- `Shape::{Struct, Enum}` and `ShapeField { name, ty }` can name host/API-owned output shapes, including root structs, nested structs, enums, and typed field signatures (`skinny/crates/ir/src/lib.rs:494-510`).
- `BackendExpr::DirectBuild { shape, fields }` already carries the direct materialization payload (`skinny/crates/ir/src/lib.rs:349-385`).
- `DirectBuildField { name, source }` and `DirectBuildSource::{Span, ChildRule, RepeatedRule, Literal, Empty}` already carry the field/source roster (`skinny/crates/ir/src/lib.rs:445-458`).
- `SinkOnlyProgram`, `SinkOnlyRule.direct_shape`, `DirectShape { shape, fields }`, and `SinkOnlyExpr::DirectBuild` preserve this payload for the direct renderer (`skinny/crates/codegen/src/lower/sink_only.rs:5-39`, `skinny/crates/codegen/src/lower/sink_only.rs:95-122`, `skinny/crates/codegen/src/lower/sink_only.rs:172-177`).
- `BackendShape::SinkOnly` is already the lowering choice for direct typed-field writes with no retained document (`skinny/crates/ir/src/lib.rs:334-341`; `restart/ARCHITECTURE.md:1047-1085`).

The likely payload refinement is inside `DirectBuildField`, not a new `BackendExpr` variant. Current fields identify `name` and `source`; owned typed output needs additional field facts such as target type, presence/null/default policy, scalar materializer, collection kind, map key policy, and unknown/duplicate key behavior. This is consistent with Primitive 6, which names `BorrowSpan`, `NumberScalar`, `LiteralMap`, `Child`, `Repeated`, and `Empty` as direct materialization policies and says REDRESS 70 requires the host/API type contract to lower into existing field facts (`restart/skinny/COMPILER.md:330-357`). Architecture §7.4 adds `map` to the same list and says `SinkOnlyProgram` preserves those facts (`restart/ARCHITECTURE.md:1114-1118`).

## Minimal Metadata For JSON `twitter` / `update_center`

The minimal schema-source contract is not a parser implementation. It is a typed output mapping from JSON value paths to host/API fields.

Common required metadata:

- `grammar = "json"` and `entry = "json"`.
- `output_name`: benchmark/API row name, e.g. `twitter` or `update_center`.
- `root_shape`: `TwitterSearch` or `UpdateCenter`; Grand Synthesis names these initial fixture mappings (`restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md:933-940`).
- `root_source`: top-level JSON value.
- For each output field: host field name, source object key/path, target type, direct materialization policy, presence policy, null policy, default policy, and whether unknown object keys are ignored, rejected, or captured.
- For scalar fields: string policy (`borrow_span` if unescaped, owned decoded string on `needs_unescape`), number policy (`u64`, `i64`, `f64`, decimal/string, with overflow behavior), bool/literal policy, and null handling.
- For nested fields: child output shape and source rule/path.
- For arrays: element shape/type and repeated policy.
- For maps: source object path, key materializer, value shape/type, key preservation/order policy, and duplicate-key policy.

`twitter -> TwitterSearch` needs at least:

- Root object fields `statuses: Vec<Status>` and `search_metadata: SearchMetadata`.
- Nested `Status` shape covering nested objects, arrays, optionals, booleans, integers, text fields, Unicode strings, and recursive `retweeted_status` style child status output, matching the coverage described in Grand Synthesis (`restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md:933-937`).
- Nested user/entities/metadata shapes, with optional/null policies for fields that are absent or null across the fixture.
- String materialization policy per field so plain strings can borrow and escaped strings decode only when required.

`update_center -> UpdateCenter` needs at least:

- Root object fields for `connectionCheckUrl`, `core`, `id`, `plugins`, `signature`, and `updateCenterVersion`.
- `core: Core`.
- `plugins: Map<String, Plugin>` sourced from the dynamic object under `plugins`; map keys must be preserved because the fixture relies on dynamic plugin names.
- `Plugin` fields covering dependencies/developers/labels arrays, long strings, optionals, and scalar version/timestamp/checksum-style fields, matching Grand Synthesis (`restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md:938-940`).
- `Developer` and dependency/label element shapes with array policies and optional/null handling.

This metadata is enough to generate a typed sink because it tells DirectBuild what Serde already knows: which output fields exist, where each field comes from in the input grammar's value space, how to materialize each scalar, and what to do with absence, nulls, maps, arrays, and unknown keys.

## Generalization Beyond JSON

The rule is grammar-general:

If the target output type is implied by the grammar's own schema, schema mining can produce `ShapeFacts` and `DirectBuildField` facts directly. If the target output type is not implied by the grammar, the host/API type contract supplies those facts.

For non-JSON grammars, the source side changes but the DirectBuild carrier does not:

- CSV: output fields map from named/header/indexed columns to struct fields; arrays/maps come from repeated rows or grouped columns.
- CSS: output fields map from captured component values, rules, selectors, declarations, and host primitive conversions.
- Google Sheets formulas: output fields map from Pratt spine nodes, ranges, literals, and host-typed function chains.
- BBNF/EBNF/YAML: output fields map from grammar captures, child rules, repeated rules, literal variants, layout-aware blocks, or host/API schemas.

The lowerer still consumes `BackendIr` plus side-table facts. It should not know whether the schema came from JSON object keys, CSV headers, CSS declarations, Serde derive data, TypeScript declarations, JSON Schema, or a future language-neutral sidecar. That distinction belongs to the schema-source adapter before BIR extraction.

## Proposed Invariants

1. No BBNF directive selects or embeds a host/API owned output type.
2. No new top-level BIR variant is introduced for direct typed output; all owned-output facts lower through `BackendExpr::DirectBuild { shape, fields }`.
3. If the output schema is not implied by the grammar, its source must be named as a host/API type contract.
4. `ShapeFacts` must contain every target output shape referenced by `DirectBuild.shape`.
5. Every `DirectBuildField` must resolve to a grammar source (`Span`, child rule, repeated rule, literal, empty, or refined map/source policy) and a host/API target field.
6. Field facts must include target type, cardinality, presence/null/default policy, and materialization policy.
7. Map fields must declare key source/materializer, value shape, duplicate-key policy, and key preservation/order policy.
8. String and number materializers must be explicit field facts, not sink-local shortcuts.
9. `SinkOnlyProgram` must preserve DirectBuild field facts losslessly from `BackendIr`.
10. Generated direct code may consume field facts to build owned output in the parse loop, but may not add a sidecar scanner, retained queryable document, parse-time checksum-only sink, or benchmark-private Track 1 parser.
11. Generic crates may not branch on grammar names such as JSON to recover missing schema facts.
12. `real_typed_struct` can count as representative DirectBuild evidence only after the typed sink is generated from the schema-source contract rather than hand-authored.

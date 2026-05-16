# SK-V6 R3d Direct Generality Audit

Scope: read-only audit of `/Users/mkbabb/Programming/bbnf-lang`; no repository files edited.

## Bottom Line

Direct-to-struct materialization does not need a new BIR variant or a new BBNF directive. The existing `DirectBuild { shape, fields }` BIR node is the right hook. The missing piece is that its field roster must be produced from grammar-neutral shape/direct facts keyed by `RuleId`, `ExprId`/binding ids, shape ids, and field ids, not from JSON rule-name switches.

For JSON skinny now, the least risky landing is:

1. Keep the existing BIR variant `DirectBuild`.
2. Move the JSON direct shape/field roster into a data-only grammar-side fixture, e.g. `skinny/grammars/json.direct-build.toml`, or an equivalent generated sidecar beside `skinny/grammars/json.bbnf`.
3. Change extraction to consume supplied `ShapeFacts` + `DirectBuildFacts` and inject `DirectBuild` generically.
4. Leave `json_sink_direct.rs` only as a bounded skinny renderer if schedule requires, but do not add more JSON logic to `passes`; its deletion receiver is the generic SinkOnly emitter.

For the greater spec, the same fact shape should be mined by shape mining and consumed by generic Rust/VM/WASM lowerers. JSON names may appear only in grammar source/metadata or generated runtime output, not in generic crates.

## Current State Anchors

Relevant architecture/doc facts:

- ARCH §7.2 already defines `DirectBuild` as a BIR variant whose payload is "Shape ID, field slots, source refs"; no BIR addition is needed (`restart/ARCHITECTURE.md:928`, `:971`).
- ARCH §7.2 says the `LayoutFacts.backend_shape` lowering amendment changes only lowerer emission, not BIR payloads, and is not a user-visible directive (`restart/ARCHITECTURE.md:940-950`).
- ARCH §7.2 invariant: `TapeEmit` and `DirectBuild` are scheduled together from side tables (`restart/ARCHITECTURE.md:1008-1009`).
- ARCH §7.4 explicitly says the remaining remediation is not another directive or BIR variant (`restart/ARCHITECTURE.md:1118`).
- ARCH §9.2 says `SinkOnly` direct outputs are projections over the same accepted event stream, with no second authoritative tree (`restart/ARCHITECTURE.md:1511-1517`).
- Lock 14 forbids grammar-specific code in generic crates (`restart/locks/LOCKS.md:60`).

Relevant code facts:

- `ir` already has `BackendExpr::DirectBuild { shape, fields }` and `DirectBuildField { name, source }` (`skinny/crates/ir/src/lib.rs:349-380`, `:445-458`).
- `SinkOnlyProgram` already preserves the expression tree and cloned `DirectBuildField` rosters from BIR (`skinny/crates/codegen/src/lower/sink_only.rs:4-39`, `:95-123`, `:172-177`).
- `passes::compile` hardcodes JSON shape and recognizer sources (`skinny/crates/passes/src/lib.rs:26-32`).
- `passes::extract::single_plan` hardcodes entry `"json"` and wraps only literal JSON rule names in `TapeEmit`/`DirectBuild` (`skinny/crates/passes/src/lib.rs:652-660`, `:719-802`).
- `json_sink_direct.rs` validates and emits against JSON rule/shape/literal names (`skinny/crates/codegen/src/json_sink_direct.rs:4-16`, `:32-114`).

## Audit Finding

The current prototype has the right BIR surface but the wrong authority for field facts.

`DirectBuildSource::{Span, ChildRule, RepeatedRule, Literal, Empty}` is directionally correct, but the current payload is too lossy and too name-based:

- `ChildRule { rule: String }` and `RepeatedRule { rule: String }` cannot distinguish multiple occurrences of the same rule inside one parent.
- `Literal { bytes: Vec::new() }` for `JsonBool.value` loses the actual consumed literal-to-bool mapping.
- `Span { label: String }` does not say which span occurrence, which scalar kind, or which payload flags are needed.
- Shape field completeness is unclear: `JsonString` shape facts include `needs_unescape`, but current direct field facts list only `span`.
- `TapeKind::{Object, Array, Pair, ...}` is also JSON-shaped and should eventually become a grammar-neutral node/event kind id, but that is adjacent to the direct-field issue rather than a reason to add a new BIR node.

The clean generalization is to make `DirectBuild` carry precomputed field facts that are specific enough for a generic lowerer, while keeping the BIR variant unchanged.

## Candidate Fact Shape

Prefer extending `ShapeFacts` with direct-build facts rather than adding a new public side table name. ARCH already says `ShapeFacts` is produced by shape mining and consumed by the direct builder, Value API, and path typing. A sibling `DirectBuildFacts` type can exist in code, but conceptually it should be part of the shape side-table family.

Sketch:

```rust
pub struct ShapeFacts {
    pub shapes: Vec<Shape>,
    pub direct_builds: Vec<DirectRuleBuild>,
}

pub struct DirectRuleBuild {
    pub rule: RuleId,
    pub shape: ShapeId,
    pub node_kind: NodeKindId,
    pub span_label: Option<SpanLabelId>,
    pub fields: Vec<DirectFieldFact>,
}

pub struct DirectFieldFact {
    pub field: FieldId,
    pub name: String,
    pub ty: TypeRef,
    pub cardinality: DirectCardinality,
    pub source: DirectSourceRef,
    pub materializer: DirectMaterializerRef,
}

pub enum DirectCardinality {
    One,
    Optional,
    Many,
}

pub enum DirectSourceRef {
    RuleResult {
        binding: BindingId,
        rule: RuleId,
    },
    RepeatedRuleResult {
        binding: BindingId,
        rule: RuleId,
        element_rule: RuleId,
    },
    Span {
        label: SpanLabelId,
        span_kind: SpanKind,
        flags: Vec<PayloadFlag>,
    },
    LiteralChoice {
        binding: BindingId,
        choices: Vec<LiteralChoiceFact>,
    },
    Unit,
}

pub struct LiteralChoiceFact {
    pub literal_expr: ExprId,
    pub bytes: Vec<u8>,
    pub const_value: ConstValue,
}

pub enum DirectMaterializerRef {
    Identity,
    BorrowSpan,
    ScalarPrimitive {
        primitive: PrimitiveId,
        input: PrimitiveInputShape,
    },
    LiteralMap,
    ComputedAccessor,
}
```

Names above are illustrative. The important properties are:

- Rule and expression references are ids, not strings.
- Field references are ids into `ShapeFacts`, not ad hoc field names.
- Repetition and child fields use occurrence/binding ids so the same rule can appear more than once.
- Literal-derived fields carry the consumed literal choices and typed constants.
- Scalar materialization is an opaque primitive reference plus input shape; generic crates do not branch on "JSON string" or "JSON number".
- Computed/accessor-only fields are explicit, so retained views and `SinkOnly` do not silently disagree about fields such as `needs_unescape`.

This remains grammar-neutral. It can represent JSON, CSS declaration values, Sheets arrays, or BBNF AST nodes as field facts over grammar rules and mined shapes.

## JSON Example Facts

The JSON skinny roster currently hardcoded in `materialization_for_rule` / `direct_fields_for_rule` maps directly to data:

```toml
[[direct_build]]
rule = "object"
shape = "JsonObject"
node_kind = "object"
fields = [
  { name = "members", cardinality = "many", source = { repeated_rule = "pair" } },
]

[[direct_build]]
rule = "array"
shape = "JsonArray"
node_kind = "array"
fields = [
  { name = "elements", cardinality = "many", source = { repeated_rule = "value" } },
]

[[direct_build]]
rule = "pair"
shape = "JsonPair"
node_kind = "pair"
fields = [
  { name = "key", cardinality = "one", source = { child_rule = "string", occurrence = 0 } },
  { name = "value", cardinality = "one", source = { child_rule = "value", occurrence = 0 } },
]

[[direct_build]]
rule = "string"
shape = "JsonString"
node_kind = "string"
fields = [
  { name = "span", source = { span = "string", kind = "string", flags = ["needs_unescape"] }, materializer = "borrow_span" },
  { name = "needs_unescape", source = { span_flag = "needs_unescape" }, materializer = "computed_or_stored_flag" },
]

[[direct_build]]
rule = "number"
shape = "JsonNumber"
node_kind = "number"
fields = [
  { name = "span", source = { span = "number", kind = "number" }, materializer = "borrow_span" },
]

[[direct_build]]
rule = "bool"
shape = "JsonBool"
node_kind = "bool"
fields = [
  { name = "value", source = { literal_choice = [
      { literal = "true", const = true },
      { literal = "false", const = false },
  ] }, materializer = "literal_map" },
]

[[direct_build]]
rule = "null"
shape = "JsonNull"
node_kind = "null"
fields = []
```

The TOML uses names because it is human-readable grammar-side data. The compiled facts should resolve them to `RuleId`, `ExprId`, `ShapeId`, and `FieldId` before BIR extraction.

## Extraction Shape

`passes::extract::single_plan` should not ask "is this rule named object/string/bool?". It should ask whether a resolved direct-build fact exists for the current `RuleId`.

Generic extraction algorithm:

```rust
fn materialize_rule(rule: &Rule, body: BackendExpr, facts: &ShapeFacts) -> BackendExpr {
    let Some(build) = facts.direct_build_for_rule(rule.id) else {
        return body;
    };

    BackendExpr::Seq(vec![
        BackendExpr::SpanMark {
            kind: SpanMarkKind::Start,
            label: build.span_label,
        },
        body,
        BackendExpr::SpanMark {
            kind: SpanMarkKind::End,
            label: build.span_label,
        },
        BackendExpr::TapeEmit {
            kind: build.node_kind,
        },
        BackendExpr::DirectBuild {
            shape: build.shape,
            fields: build.fields,
        },
        BackendExpr::Return,
    ])
}
```

That preserves the existing BIR alphabet. It also keeps lowerers BIR-only: codegen receives `DirectBuild` with field/source facts and never inspects Grammar IR.

## SinkOnly Lowering Shape

`codegen/src/lower/sink_only.rs` is the right generic staging point. It already walks `BackendIr` and preserves direct shape fields. The next shape should make `SinkOnlyProgram` a real direct materialization program rather than a JSON renderer input:

```rust
pub struct SinkOnlyProgram {
    pub entry_rule: RuleId,
    pub rules: Vec<SinkOnlyRule>,
    pub shapes: ShapeFacts,
    pub direct_builds: Vec<DirectRuleBuild>,
    pub scalar_primitives: Vec<PrimitiveUse>,
    pub dispatch_alt_count: usize,
}
```

The generic Rust emitter can then generate:

- per-grammar sink trait names from `grammar_name`, not hardcoded `JsonSink`;
- begin/end hooks from `DirectRuleBuild.shape`;
- field hooks from `DirectFieldFact`;
- raw-span or scalar primitive calls from `DirectMaterializerRef`;
- array/object-like nesting from cardinality and child/repeated sources, not JSON names.

JSON-specific method names such as `object_string_source` and `array_i64` are an emitted naming choice, not something a generic crate should know in source.

## Landing Recommendation

### JSON Skinny Now

Do the smallest generality-preserving move:

1. Add a data-only JSON direct-build fact fixture beside the skinny grammar, e.g. `skinny/grammars/json.direct-build.toml`.
2. Keep `ir::BackendExpr::DirectBuild` unchanged. If necessary, enrich `DirectBuildField` and `DirectBuildSource` payload structs, but do not add BIR variants.
3. Change `passes::compile` or add a `compile_with_facts` path so JSON shape/direct facts are supplied by the skinny driver instead of by `shapes_for_json()` in `passes`.
4. Change `extract::single_plan` to resolve entry rule and materialization from facts, not from `"json"` or JSON rule-name matches.
5. Keep `codegen/src/lower/sink_only.rs` as the generic lowerer.
6. Treat `codegen/src/json_sink_direct.rs` as a temporary skinny-only renderer. If R3d can afford one more step, make it consume the fixture-derived facts for validation instead of `REQUIRED_RULES`/`REQUIRED_SHAPES`; otherwise record its deletion receiver explicitly.

This removes the worst Lock 14 violation from `passes` without forcing the full generic direct emitter in the same step.

### Greater Spec Later

Replace the JSON fixture with mined facts:

1. `passes::shapes` mines `ShapeFacts.direct_builds` from Grammar IR + type/layout facts + corpus/profile signal.
2. `passes::extract` consumes only Grammar IR + public side tables.
3. Cost/CSP selects `BackendShape::SinkOnly` when output mode is direct-only and no retained traversal is required.
4. Generic codegen emits runtime/view/sink/value/path from the same shape/direct facts.
5. Per-grammar runtime files remain generated artefacts under `runtime/src/grammars/<name>/`; generic crates contain no JSON modules, no JSON type names, and no grammar-name switches.

## Non-Goals

- Do not add `DirectBuildObject`, `DirectBuildArray`, `DirectBuildScalar`, or similar BIR variants.
- Do not add `@direct`, `@shape`, `@sink`, or any user-facing directive.
- Do not put JSON rule-name maps in `passes`, `ir`, `codegen`, `runtime`, or `parse-that` production source.
- Do not make direct-to-struct a second tree. It remains a projection/materialization over the accepted event stream.

## Close Gate

For the JSON skinny cleanup, a useful close gate is:

- BIR snapshot still contains `DirectBuild` nodes with the same JSON field roster.
- `passes/src/lib.rs` has no production `match` over `"object"`, `"array"`, `"pair"`, `"string"`, `"number"`, `"bool"`, `"null"`.
- `passes::extract` can materialize a tiny non-JSON fixture from direct-build facts without code changes.
- `json_sink_direct.rs` either has no hardcoded `REQUIRED_RULES` / `REQUIRED_SHAPES`, or has a named deletion receiver for the generic SinkOnly emitter.
- No repository source adds a new BIR variant or grammar directive.

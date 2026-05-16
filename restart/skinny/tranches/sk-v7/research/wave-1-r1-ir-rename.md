# SK-V7 W1 R1 — IR Rename Diagnosis

Scope: read-only diagnosis for SPEC §3 Wave 1, HANDOFF §3 pre-blocked
routes, and `skinny/crates/ir/src/lib.rs`.

## Read Set

- `restart/skinny/tranches/sk-v7/SPEC.md` §3.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` §3.
- `skinny/crates/ir/src/lib.rs`.
- Reference sweep: `rg -n "TapeKind|TapeKind::|DirectBuildDecode|DirectBuildDecode::|materialization_for_rule|DirectBuild|TapeEmit" skinny/crates`.

## Diagnosis

W1 is mechanically small, but it touches public IR spellings. The current
generic IR exposes JSON-shaped tags in two places:

- `TapeKind` at `skinny/crates/ir/src/lib.rs:433-443`:
  `Object`, `Array`, `Pair`, `String`, `Number`, `Bool`, `Null`,
  `Member`, `Element`.
- `DirectBuildDecode` at `skinny/crates/ir/src/lib.rs:510-515`:
  `Raw`, `JsonString`, `JsonNumber`, `Literal`.

The actual constructor reference set under `skinny/crates/*` is narrow:

- `TapeKind::Object` through `TapeKind::Null` appear exactly seven times,
  all in `passes::materialization_for_rule` at
  `skinny/crates/passes/src/lib.rs:742-750`.
- `TapeKind` is also carried structurally by `BackendExpr::TapeEmit { kind }`
  at `skinny/crates/ir/src/lib.rs:374-376`.
- `DirectBuildDecode` is stored in `DirectBuildTarget.decode` at
  `skinny/crates/ir/src/lib.rs:463-470`, but `rg` finds no
  `DirectBuildDecode::...` constructors outside the enum definition.

Codegen currently does not branch on `TapeKind`. `lower::sink_only` lowers
`BackendExpr::TapeEmit { .. }` to `SinkOnlyExpr::TapeEmit` and discards the
kind at `skinny/crates/codegen/src/lower/sink_only.rs:171`. Codegen tests and
generated templates depend on `DirectBuild` shape strings such as
`JsonObject`/`JsonString`, not on `TapeKind` variants. Therefore a pure enum
rename plus matching the seven pass constructors should be behavior-neutral
and should not change generated JSON parser output.

The SPEC also says to delete `passes::materialization_for_rule`. That helper
currently supplies both the tape kind and the `DirectBuild` shape string to
`materialize_rule` at `skinny/crates/passes/src/lib.rs:719-739`. Removing it
without an equivalent same-wave source for `(kind, shape)` would drop
`TapeEmit`, `DirectBuild`, and `Return` materialization for the seven JSON
rules. The W1 plan must either preserve the current materialization data under
a grammar-neutral helper/facts shape or treat helper deletion as coupled to a
known replacement. A literal deletion with no replacement is not behavior
neutral.

## Mechanical Rename Map

Recommended `TapeKind` map:

| Current | Recommended |
|---|---|
| `TapeKind::Object` | `TapeKind::Container` |
| `TapeKind::Array` | `TapeKind::Sequence` |
| `TapeKind::Pair` | `TapeKind::KeyValuePair` |
| `TapeKind::String` | `TapeKind::StringValue` |
| `TapeKind::Number` | `TapeKind::NumberValue` |
| `TapeKind::Bool` | `TapeKind::BoolValue` |
| `TapeKind::Null` | `TapeKind::NullValue` |
| `TapeKind::Member` | unchanged |
| `TapeKind::Element` | unchanged |

Recommended `DirectBuildDecode` map:

| Current | Recommended |
|---|---|
| `DirectBuildDecode::Raw` | unchanged |
| `DirectBuildDecode::JsonString` | `DirectBuildDecode::EscapedString` |
| `DirectBuildDecode::JsonNumber` | `DirectBuildDecode::NumberScalar` |
| `DirectBuildDecode::Literal` | unchanged |

`EscapedString` and `NumberScalar` match the prior Lock 14 sequence/audit
prescription and avoid colliding conceptually with `TapeKind::StringValue` and
`TapeKind::NumberValue`. SPEC §3 allows `StringValue`/`NumberValue` for
`DirectBuildDecode`; the plan phase should explicitly resolve that naming
tension before implementation.

## Serde, Debug, and API Implications

`TapeKind` and `DirectBuildDecode` derive `Serialize`, `Deserialize`, and
`Debug`. Renaming variants changes:

- Rust source compatibility for any external crate matching on the public IR.
- Debug output spelling.
- Serde externally tagged variant strings in any serialized `BackendIr`,
  `BackendExpr::TapeEmit`, or `DirectBuildTarget.decode` payload.

No compatibility aliases exist today. Adding `#[serde(alias = "...")]` would
preserve old deserialization but would keep the old grammar-shaped names in
generic IR source. If W1 is meant to reduce Lock 14 leak count cleanly, the
safer implementation is a direct spelling break with tests updated to the new
names, unless a documented migration gate requires aliases.

## Findings

- The seven `TapeKind::...` consumer sites are isolated to
  `passes::materialization_for_rule`; there are no codegen consumers of the
  concrete variants.
- `DirectBuildDecode::JsonString` and `DirectBuildDecode::JsonNumber` are
  definition-only today; the rename is IR/API cleanup rather than behavior
  change.
- `DirectBuild` shape names (`JsonObject`, `JsonString`, etc.) are still
  JSON-specific and intentionally outside this R1 rename scope; they are
  separate Lock 14 work in later waves.
- `json_templates/generated.rs` should be byte-identical after a pure enum
  rename because generated output observes `DirectBuild` shapes, literals,
  spans, and rules, not `TapeKind` names.
- The helper-deletion clause is the only non-mechanical part of W1. It must
  preserve the current `materialize_rule` output shape for `object`, `array`,
  `pair`, `string`, `number`, `bool`, and `null`.

## Risk List

- **Serde/API break:** public enum variant spelling changes serialized IR and
  downstream pattern matches.
- **Helper deletion behavior risk:** deleting `materialization_for_rule`
  without a replacement removes materialization and can alter generated output.
- **Naming tension:** SPEC §3 suggests `DirectBuildDecode::StringValue` /
  `NumberValue`; prior Lock 14 research recommends `EscapedString` /
  `NumberScalar`.
- **False Lock 14 closure risk:** renaming `TapeKind` does not close remaining
  JSON-specific shape strings or template module names.
- **Generated-file churn risk:** formatting or regeneration can touch generated
  files unrelated to the rename; compare generated output carefully.

## Suggested Verification Commands

```sh
rg -n 'TapeKind::(Object|Array|Pair|String|Number|Bool|Null)|DirectBuildDecode::(JsonString|JsonNumber)' skinny/crates
rg -n 'pub enum TapeKind|pub enum DirectBuildDecode|materialization_for_rule' skinny/crates/ir/src/lib.rs skinny/crates/passes/src/lib.rs
cargo test --workspace
cargo run -p xtask -- regen-real-typed
git diff -- skinny/crates/codegen/src/json_templates/generated.rs skinny/crates/runtime/src/grammars/json/generated.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs
cargo run -p bbnf-bench --bin gate --release
```

Expected post-rename spot checks:

- No `TapeKind::Object`, `TapeKind::Array`, `TapeKind::Pair`,
  `TapeKind::String`, `TapeKind::Number`, `TapeKind::Bool`, or
  `TapeKind::Null` constructor references remain.
- No `DirectBuildDecode::JsonString` or `DirectBuildDecode::JsonNumber`
  references remain.
- `cargo test --workspace` is green.
- `RESULTS.md` is unchanged for W1 because the wave is a naming cleanup, not a
  performance intervention.

## Blocked Routes

HANDOFF §3 keeps these routes closed for W1:

- REDRESS 28+33: Class A NEON tiny-string wiring as a parse-G fix.
- REDRESS 50-55: SK-V5 UTF-8 fusion routes.
- REDRESS 60-72: SK-V6 retained-parse and direct-materialization routes.
- Earlier rejected routes: 12-byte token width churn, pair-token fusion,
  function-pointer dispatch table, capacity prescan, generic SWAR whitespace
  skipper, separator elision, raw f64 shortcut, PSI/DTA Rust-codegen automaton,
  and EventCursor parallel prepass.

W1 should remain an IR naming and materialization-preservation wave. It should
not reopen parse-speed, retained-parse, UTF-8 fusion, or new-substrate work.

## Owner Paths

- `skinny/crates/ir/src/lib.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/codegen/src/lower/sink_only.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- Any generated consumer changed by regeneration verification only.

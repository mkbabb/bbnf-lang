# SK-V7 W8 R2 Sink Direct Renderer Research

Date: 2026-05-16.

Scope: read-only research for W8 Phase C. Inputs inspected: `SPEC.md` Section
10, `skv7-A5-lock-audit.md` Section 2.4, `skv7-B3-lock14-sequence.md` Class C,
`skinny/crates/codegen/src/json_sink_direct.rs`, and
`skinny/crates/codegen/src/lower/sink_only.rs`. No source code was edited.

## Gate Authority

SPEC Section 10 makes W8 a Lock 14 codegen/IR cleanup wave. The codegen part is
the riskiest Phase C surface: replace `emit_json_*` with grammar-level emitters,
collapse hardcoded JSON shape rosters into grammar-derived facts, and preserve
byte-identical generated `generated.rs` output. The explicit invariant is
`xtask gen --check` in the packet text, but the current xtask command surface is
`cargo run -p xtask --release -- check-json`, which byte-compares emitted JSON
runtime files against `crates/runtime/src/grammars/json`.

A5 Section 2.4 classifies `json_sink_direct.rs` as an admitted V6 SinkOnly
emitter that still leaks Lock 14 in a generic crate. B3 Class C gives the
target direction: rename the renderer/module surface, remove JSON rule/shape
constants, validate against field facts, and eventually emit dispatch from a
grammar route table. That target is not fully backed by current data structures:
`SinkOnlyProgram.expected_shapes` does not exist today. The current available
facts are `entry_rule`, `rules`, `direct_shapes`, `span_kinds`, `literals`, and
`dispatch_alt_count`.

## Hardcoded Inventory

| Renderer site | Current hardcode | Notes |
|---|---|---|
| `skinny/crates/codegen/src/json_sink_direct.rs:4-6` | Required rule names: `json`, `value`, `object`, `array`, `pair`, `string`, `number`, `bool`, `null`. | This is a JSON entry/rule roster, not derived from `BackendIr.rules`. |
| `skinny/crates/codegen/src/json_sink_direct.rs:8-16` | Required shapes: `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`. | This duplicates shape facts already visible through `SinkOnlyProgram.direct_shapes`. |
| `skinny/crates/codegen/src/json_sink_direct.rs:32-38` | `program.entry_rule != "json"` and JSON-specific error text. | `SinkOnlyProgram.entry_rule` is already derived from `BackendIr.entry_rule` at `lower/sink_only.rs:115-117`. |
| `skinny/crates/codegen/src/json_sink_direct.rs:40-50` | Missing-rule validation against fixed `REQUIRED_RULES`. | Can be replaced by checking renderer-required handlers against grammar facts, or kept only as a JSON compatibility assertion during a byte-identical shim step. |
| `skinny/crates/codegen/src/json_sink_direct.rs:52-62` | Missing-shape validation against fixed `REQUIRED_SHAPES`. | Can be expressed from current `program.direct_shapes` once the expected roster is supplied by caller/facts. |
| `skinny/crates/codegen/src/json_sink_direct.rs:64-71` | Required literals `true`, `false`, `null`. | Current `SinkOnlyProgram.literals` has the observed literal set, but no semantic label saying which literal maps to which sink event. |
| `skinny/crates/codegen/src/json_sink_direct.rs:73-84` | Required span kinds `String`, `Number`, `Whitespace`. | Current `SinkOnlyProgram.span_kinds` can validate presence, but primitive policy remains JSON-specific. |
| `skinny/crates/codegen/src/json_sink_direct.rs:86-94` | Field roster: `JsonObject.members`, `JsonArray.elements`, `JsonPair.key/value`, `JsonString.span`, `JsonNumber.span`, `JsonBool.value`, `JsonNull` empty. | Current per-rule `DirectShape.fields` preserves these field names and `DirectBuildField` sources. |
| `skinny/crates/codegen/src/json_sink_direct.rs:117-143` | Header emits `use super::sink::JsonSink`. | Needs grammar sink type naming input before it can be neutral. |
| `skinny/crates/codegen/src/json_sink_direct.rs:145-167` | Entry body calls `parse_value_direct` and uses `S: JsonSink`. | The emitted body is JSON code; only names/signature are readily parameterizable now. |
| `skinny/crates/codegen/src/json_sink_direct.rs:169-294` | Value dispatch functions hardcode `parse_object_direct`, `parse_array_direct`, string/number/literal branches, sink callbacks, and bytes `{`, `[`, `"`, `t`, `f`, `n`. | B3 says future `RecognizerRoute.dispatch_table`; current `SinkOnlyProgram` only has `dispatch_alt_count`, not branch-byte-to-handler mapping. |
| `skinny/crates/codegen/src/json_sink_direct.rs:296-362` | Object/array grammar syntax: `{}`, `[]`, `:`, `,`, key string rule, begin/end sink calls. | This must remain JSON emitted code unless a container grammar lowering model is added. |
| `skinny/crates/codegen/src/json_sink_direct.rs:364-403` | JSON string primitive via `parse_that_regex::match_string_at_quote_trusted_utf8` and JSON error mapping. | Current facts identify a string span exists, not delimiter/escape policy. |
| `skinny/crates/codegen/src/json_sink_direct.rs:405-497` | Number parser/emitter and sink callbacks `i64/u64/f64`, plus `-0` handling. | Current facts identify a number span exists, not numeric sink event semantics. |
| `skinny/crates/codegen/src/json_sink_direct.rs:499-563` | Literal/byte helpers and error construction. | Helpers are grammar-agnostic mechanically, but their call sites and `InvalidLiteral` payloads are JSON-driven. |

## Existing Facts Available Now

`SinkOnlyProgram` is already a useful renderer input:

- `lower/sink_only.rs:5-12` defines `entry_rule`, `rules`, `direct_shapes`,
  `span_kinds`, `literals`, and `dispatch_alt_count`.
- `lower/sink_only.rs:95-123` builds the program from `BackendIr`, copying
  `backend.entry_rule` and collecting lowered rule facts.
- `lower/sink_only.rs:172-177` records each `DirectBuild { shape, fields }`.
- `lower/sink_only.rs:184-200` extracts a rule-level `direct_shape`.
- `ir/src/lib.rs:326-332` shows `BackendIr` also carries `grammar_name`,
  `entry_rule`, `recognizers`, `rules`, and `shape_facts`.
- `ir/src/lib.rs:344-379` shows each `BackendRule` has a name and expression,
  and `DirectBuild` carries `shape` plus `Vec<DirectBuildField>`.
- `ir/src/lib.rs:446-459` shows each `DirectBuildField` has a field `name` and
  structured source (`Span`, `ChildRule`, `RepeatedRule`, `Literal`, `Empty`).

These current facts can parameterize now:

| Candidate change | Can use now | Constraint |
|---|---|---|
| Entry-rule check and error text | `program.entry_rule` / `BackendIr.entry_rule`. | For byte-identical generated JSON, keep the emitted entry function body unchanged and only neutralize validation names internally. |
| Actual rule roster validation | `program.rules.iter().map(|r| r.name)` and `program.has_rule`. | Expected rule roster still needs to come from grammar metadata or a compatibility profile; current program gives observed, not required, rules. |
| Actual shape roster validation | `program.direct_shapes` and `program.has_shape`. | Expected shape roster is not separately present; B3's `expected_shapes` field is not in current code. |
| Field roster validation | `program.rules[*].direct_shape.fields[*].name`. | Expected field roster can be derived only if the caller supplies field facts, or by treating observed `DirectBuild` rosters as authoritative and deleting fixed JSON expectations. |
| Literal presence | `program.literals` and `program.has_literal`. | Mapping from literal bytes to semantic sink calls remains JSON-specific. |
| Span-kind presence | `program.span_kinds`. | String/number/whitespace primitive policies remain JSON-specific. |
| Generated header comment | `program.entry_rule`, `program.direct_shapes`, `program.dispatch_alt_count`. | Already parameterized at `json_sink_direct.rs:117-128`. |

The lowest-risk W8 3a cut is therefore a neutral wrapper/profile around the
current renderer:

1. Rename/rebrand the module and public error text.
2. Replace fixed `REQUIRED_SHAPES` validation with a supplied compatibility
   roster or observed `DirectShape` roster validation.
3. Keep the JSON emitted Rust body byte-identical.

That reduces Lock 14 in the generic validation layer without pretending the
existing `SinkOnlyProgram` can synthesize arbitrary grammar parser bodies.

## Must Remain JSON Emitted Code

The emitted Rust from `render_entry` through `render_utility_rules` is currently
a hand-authored JSON parser generator. It cannot be generalized from current
`SinkOnlyProgram` alone because the program lacks:

- a first-byte dispatch table mapping byte classes to rule handlers and sink
  events;
- delimiter/separator/container facts for `{}`, `[]`, `:`, and `,`;
- literal semantic labels mapping `true`, `false`, and `null` to sink callbacks;
- sink trait/type naming facts beyond the hardcoded `JsonSink`;
- primitive string policy facts such as delimiter, escape rules, trusted UTF-8
  contract, and error mapping;
- primitive number policy facts beyond `SinkOnlySpanKind::Number`;
- per-context sink callback mapping for top-level, object value, and array
  element number/string/bool/null events.

B3's proposed `RecognizerRoute.dispatch_table`, `DirectFieldFacts`, and
`PrimitiveFacts` are the correct long-term homes for those missing facts, but
they are not present in the current inspected code. Until they exist, byte
literals such as `b'{'`, `b'['`, `b'"'`, `b't'`, `b'f'`, and `b'n'` should be
treated as JSON emitted code, not generic renderer metadata.

## Byte-Identical Strategy

The invariant should be enforced as a generated-output contract, not inferred
from tests alone.

1. Snapshot the generated JSON runtime before the W8 3a change:

   ```bash
   cd /Users/mkbabb/Programming/bbnf-lang
   git diff -- skinny/crates/runtime/src/grammars/json/generated.rs
   ```

2. Refactor only the renderer shell first: module name, function names, neutral
   validation helpers, and optional profile structs. Do not edit the raw string
   bodies emitted by `render_entry`, `render_value_dispatch`,
   `render_container_rules`, `render_string_rule`, `render_number_rules`, or
   `render_utility_rules` in the first cut.

3. Run the runtime byte check:

   ```bash
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   cargo run -p xtask --release -- check-json
   ```

   Current xtask implements this at `skinny/xtask/src/main.rs:127-132` by
   calling `codegen::emit_json_from_source()` and `EmittedSource::check_dir()`.
   A mismatch returns `DifferentFile(path)` from
   `skinny/crates/codegen/src/lib.rs:54-64`.

4. Inspect the generated file diff after the refactor:

   ```bash
   cd /Users/mkbabb/Programming/bbnf-lang
   git diff --exit-code -- skinny/crates/runtime/src/grammars/json/generated.rs
   ```

5. If a later W8 subcut changes parse-that or public symbol spelling, permit
   only mechanical rename drift that is already approved by the sub-plan. For
   this sink-direct R2 scope, the recommended target is strict zero-byte drift in
   `generated.rs`.

## Verification Commands

Minimum focused checks for a W8 sink-direct shell refactor:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p codegen
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-real-typed
cargo run -p xtask --release -- check-conformance
cargo test --workspace
```

Lock 14 spot checks after the refactor:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
rg -n 'mod json_sink_direct|json_sink_direct::render|JSON SinkOnly renderer' skinny/crates/codegen/src
rg -n 'const REQUIRED_RULES|const REQUIRED_SHAPES|JsonObject|JsonArray|JsonPair|JsonString|JsonNumber|JsonBool|JsonNull' skinny/crates/codegen/src/sink_direct.rs skinny/crates/codegen/src/json_sink_direct.rs
git diff --exit-code -- skinny/crates/runtime/src/grammars/json/generated.rs
```

The second grep needs interpretation: JSON names are still acceptable inside
emitted JSON runtime text if the file remains a JSON emitter/profile. They are
not acceptable as generic validation rosters in the codegen shell after the W8
3a close.

## Recommendation

Do not start W8 by rewriting the emitted parser body. Start with a byte-stable
renderer-shell split: move the hardcoded validation roster behind current
`SinkOnlyProgram`/`BackendIr` facts, rename the module surface, and leave the
JSON parser text untouched. That creates a reviewable checkpoint where
`check-json` proves the runtime is unchanged. The subsequent generalization of
dispatch bytes and primitive policies should wait for explicit route and
primitive facts; current `SinkOnlyProgram` can validate observed shapes and
fields, but it cannot author a grammar-neutral recursive-descent sink parser by
itself.

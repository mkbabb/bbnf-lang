# SK-V11 W4 R2: JSON Generated Runtime Research

## Scope

Research-only trace of generated JSON direct dispatch and byte-set control in:

- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`

No source edits were made outside this markdown artifact.

## Finding

Generated JSON direct dispatch is already emitted and consumed through the
existing JSON sink-only path. The generated runtime file is the codegen JSON
template prefix plus an appended `sink_direct::render` section. The six W4
candidate rows (`canada`, `mesh`, `random`, `update_center`, `github_events`,
`twitter`) do not have row-specific generated functions; they all execute the
same benchmark function and parser call chain:

`json/{row}/track1_direct_to_struct` -> `direct_struct::track1_digest` ->
`runtime::generated_json::parse_direct` -> generated direct sink dispatch.

No generic JSON policy, grammar directive, or BIR extension is required for the
current direct-dispatch/byte-set surface. The existing BIR carries enough signal
through `AltMode::Dispatch`, `ByteLiteral`, `RegexProgram`, `DirectBuild`, and
`Recognizer::SimdScan { alphabet, site }`.

## Emission Today

The base generated parser is authored in
`skinny/crates/codegen/src/json_templates/generated.rs`. It emits:

- `STRUCTURAL_ALPHABET_JSON = b"{}[],:\""` and a recognizer comment at lines
  10-12.
- `attach_structural_index`, currently a debug assertion/no-op consumer of that
  structural alphabet at lines 14-17.
- Tree-parser value dispatch in `parse_value_at` and `dispatch_value` at lines
  37-58.
- Array follow-on dispatch through `ContainerNext::Next(byte)` back into
  `dispatch_value` at lines 121-137.
- Shared string fast path `match_tiny_plain_string_direct` at lines 165-168.
- Structural byte consumption in `consume_structural`,
  `consume_container_next`, and `consume_array_next` at lines 292-377.

The runtime file starts with that same template. A direct comparison confirmed
`skinny/crates/codegen/src/json_templates/generated.rs` matches the first 393
lines of `skinny/crates/runtime/src/grammars/json/generated.rs`; the runtime
file then appends the sink-only direct parser.

The append happens in `skinny/crates/codegen/src/lib.rs`: `emit_with_layout`
loads `json_provider::generated_rs()`, obtains the lowered sink-only program,
then appends `sink_direct::render(sink_only)` before writing `generated.rs`
at lines 118-125. The codegen test
`direct_parser_is_authored_from_sink_only_lowering` asserts the sink-only program
has JSON shapes/literals and that emitted `generated.rs` contains the sink-only
header plus `parse_direct` at lines 285-308.

The direct parser body is emitted by
`skinny/crates/codegen/src/sink_direct.rs`:

- `render` calls `render_header`, `render_entry`, `render_value_dispatch`,
  `render_container_rules`, `render_string_rule`, `render_number_rules`, and
  `render_utility_rules` at lines 4-15.
- `render_header` writes the direct-shape roster and `dispatch_alt_count`
  comment at lines 68-79.
- `render_entry` emits `parse_direct` at lines 96-118.
- `render_value_dispatch` emits `parse_value_direct`,
  `parse_object_value_at_direct`, and `parse_array_element_at_direct` at lines
  120-245.
- `render_container_rules` emits `parse_object_direct` and
  `parse_array_direct` at lines 247-313.
- `render_number_rules` emits root/object/array number dispatch and calls
  `render_number_emitter` to materialize the context-specific number sinks at
  lines 356-448.
- `render_utility_rules` emits `consume_literal_direct`, `consume_direct`,
  `take_direct`, and error helpers at lines 450-500+.

The sink-only facts come from existing BIR, not from a directive or policy knob.
`lower::sink_only::SinkOnlyProgram` stores `direct_shapes`, `span_kinds`,
`literals`, and `dispatch_alt_count` at lines 19-27. `lower_program` walks every
BIR rule and collects those facts at lines 112-139. `lower_expr` increments
`dispatch_alt_count` for `AltMode::Dispatch`, records byte literals, records
regex span kinds, and records `DirectBuild` shapes at lines 151-190.

## Runtime Consumption Today

`skinny/crates/runtime/src/grammars/json/generated.rs` consumes the emitted
direct parser after the sink-only header:

- Header: `// sink-only lowered from BackendIr: entry=json
  direct_shapes=JsonArray,JsonBool,JsonNull,JsonNumber,JsonObject,JsonPair,JsonString
  dispatch_alt_count=8` at line 395.
- Public entry: `parse_direct<'i, S: JsonSink>` at lines 409-423.
- Root dispatch: `parse_value_direct` at lines 427-462.
- Object-value dispatch: `parse_object_value_at_direct` at lines 468-504.
- Array-element dispatch: `parse_array_element_at_direct` at lines 508-542.
- Container loops: `parse_object_direct` and `parse_array_direct` at lines
  548-606.
- String direct fast path: `parse_string_direct`, which calls
  `match_tiny_plain_string_direct` from the shared template helper, at lines
  610-643.
- Number dispatch/materialization: `parse_number_direct`,
  `parse_number_object_direct`, `parse_number_array_direct`,
  `emit_number_direct`, `emit_number_object_direct`, and
  `emit_number_array_direct` at lines 645-776.
- Literal/delimiter helpers: `consume_literal_direct`, `consume_direct`, and
  `take_direct` at lines 780-821.

The direct parser does not use `ParserState`, `attach_structural_index`, the
offset tape, or `scan_structurals`. It is a cursor-over-bytes sink path:
`skip_ascii_whitespace`, `bytes.get(*cursor)`, literal byte matches, and direct
calls into `JsonSink`.

The tree parser still consumes the structural byte surface through
`ParserState`:

- `runtime/src/grammars/json/parser.rs` computes capacity through
  `scan::structural_capacity_for` at lines 16-23.
- `parse` calls `generated::attach_structural_index` before `parse_json` at
  lines 47-51.
- `parse_direct` is re-exported from `runtime/src/grammars/json/mod.rs` at
  lines 11-13.

## Byte-Set Control

There are two byte-set surfaces today, and neither requires a generic directive
change for W4 R2:

1. Generated tree parser structural alphabet:
   `STRUCTURAL_ALPHABET_JSON` in both the codegen template and runtime generated
   file is fixed to `{}[],:\"`. `attach_structural_index` only asserts that
   value today. Tree-parser structural consumption is still local scalar byte
   logic (`consume_structural`, container-next helpers) plus parser capacity
   planning.

2. Runtime scanner byte set:
   `runtime/src/grammars/json/scan.rs` has its own
   `STRUCTURAL_BYTES = b"{}[],:\""`, scalar scan, and aarch64 scan table. The
   direct sink parser does not call this scanner.

Direct dispatch itself is byte controlled inside emitted match/take sites:
root/object/array dispatch match on `{`, `[`, `"`, number starts, `t`, `f`, and
`n`; container loops use direct byte takes for `{`, `}`, `[`, `]`, `:`, and `,`.
That control is already local to `sink_direct.rs` and the appended runtime
section.

## Candidate Rows

The W4 candidate set is explicitly selected from `canada`, `mesh`, `random`,
`update_center`, `github_events`, and `twitter` in
`restart/skinny/tranches/sk-v11/SPEC.md` lines 522-524. The same SPEC records
these as `N-direct / NO-GO` direct-to-struct rows at lines 123-128.

All six names are canonical fixtures in
`skinny/crates/test-fixtures/src/lib.rs` lines 7-15. The benchmark harness loops
over available fixtures in `bench_json_parity` and calls `run_fixture` for each
fixture at `skinny/crates/bbnf-bench/benches/json_parity.rs` lines 10-28.
`run_fixture` creates Criterion group `json/{fixture.name}` at line 38 and the
same `track1_direct_to_struct` benchmark at lines 181-186 for every fixture.
Metadata is written under `json_{corpus}/track1_direct_to_struct/metadata.toml`
by `metadata_path` at lines 497-513.

Per-row mapping:

| Row | Criterion group | Generated direct consumer | Comparator |
|---|---|---|---|
| `canada/direct_to_struct` | `json/canada` | `track1_direct_to_struct` -> `track1_digest` -> `parse_direct` | `track2_direct_to_struct` -> `track2_digest` |
| `mesh/direct_to_struct` | `json/mesh` | `track1_direct_to_struct` -> `track1_digest` -> `parse_direct` | `track2_direct_to_struct` -> `track2_digest` |
| `random/direct_to_struct` | `json/random` | `track1_direct_to_struct` -> `track1_digest` -> `parse_direct` | `track2_direct_to_struct` -> `track2_digest` |
| `update_center/direct_to_struct` | `json/update_center` | `track1_direct_to_struct` -> `track1_digest` -> `parse_direct` | `track2_direct_to_struct` -> `track2_digest` |
| `github_events/direct_to_struct` | `json/github_events` | `track1_direct_to_struct` -> `track1_digest` -> `parse_direct` | `track2_direct_to_struct` -> `track2_digest` |
| `twitter/direct_to_struct` | `json/twitter` | `track1_direct_to_struct` -> `track1_digest` -> `parse_direct` | `track2_direct_to_struct` -> `track2_digest` |

The named consumer functions are in
`skinny/crates/bbnf-bench/src/direct_struct.rs`:

- `impl JsonSink for JsonDigestSink` receives generated direct events at lines
  259-399.
- `track1_digest` creates `JsonDigestSink` and calls
  `runtime::generated_json::parse_direct` at lines 401-405.
- `track2_digest` calls the independent hand parser at lines 408-410.
- The hand comparator has its own byte dispatch in `HandParser::value_at` and
  container/string/number helpers; its string tiny path is capped at 8 bytes at
  lines 541-582.

## Policy, Directive, And BIR Conclusion

No generic JSON policy/directive/BIR change is needed for this lane.

Reasons:

- The grammar directive parser only admits `@import` and `@token`; adding a JSON
  direct-dispatch or byte-set directive would be a new surface area, but the
  needed dispatch facts are already present in lowered BIR.
- BIR already has the relevant generic carriers:
  `BackendExpr::Alt { mode }`, `ByteLiteral`, `RegexProgram`, `DirectBuild`,
  and `Recognizer::SimdScan { mode, alphabet, site }` in
  `skinny/crates/ir/src/lib.rs` lines 416-475.
- The passes already materialize JSON direct build shapes from rule structure:
  literal/regex/ref based role discovery in `passes/src/lib.rs` lines 1238-1318,
  `DirectBuild` insertion in `materialize_rule` lines 1132-1157, and tests that
  seven direct-build rules exist at lines 1517-1530.
- Codegen already rejects generic runtime emission outside grammar profile
  `json` through `json_provider::ensure_runtime_profile`, so any W4 source work
  against JSON generated direct dispatch can stay in the JSON sink renderer,
  runtime generated output, scanner/template, or benchmark consumer without
  changing BBNF surface syntax or BIR shape.

Restart rule of thumb: if W4 later changes generated JSON direct behavior,
start in `skinny/crates/codegen/src/sink_direct.rs` and regenerate/check
`skinny/crates/runtime/src/grammars/json/generated.rs`. Touch
`skinny/crates/codegen/src/json_templates/generated.rs` only for tree-parser
shared helper or structural-byte changes, not for the appended sink-only direct
parser.

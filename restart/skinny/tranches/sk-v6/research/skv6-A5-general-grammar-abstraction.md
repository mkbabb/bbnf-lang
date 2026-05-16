# SK-V6 A5: Arbitrary Grammar Generalization Plan

Workspace read: `/Users/mkbabb/Programming/bbnf-lang`
Artifact: `/tmp/skv6-A5-general-grammar-abstraction.md`
Date: 2026-05-15
Repo edits: none.

## Position

The generalization route is a fact-model and lowering cleanup, not a new
language feature. Keep the current BIR carriers:

- `SimdScan`
- `TapeEmit`
- `DirectBuild`
- `ValueProject`
- `Alt { mode }`
- existing side tables, especially `LayoutFacts.backend_shape`, `ShapeFacts`,
  `RecognizerFacts`, and `CostFacts`

Do not add a BBNF directive. Do not add a BIR variant. Grammar-specific behavior
must enter as grammar source, workspace metadata, generated per-grammar data, or
host/API output schema facts.

The current skinny has the right structural hooks but still contains JSON-only
waivers: `passes::compile` calls `shapes_for_json()` and `nominate_json()`
(`skinny/crates/passes/src/lib.rs:26-32`), `passes::shapes` builds `Json*`
shapes by hand (`skinny/crates/passes/src/lib.rs:208-239`),
`StructuralAlphabet::json()` exists in generic IR
(`skinny/crates/ir/src/lib.rs:411-416`), and codegen entry points are
JSON-branded (`skinny/crates/codegen/src/lib.rs:68-95`). Those are prototype
waivers, not architecture.

## Constraints Read Back

- Lock 1: structural projection is the tape when retained; no sidecar offset
  stream and no parallel substrate (`restart/skinny/SUBSTRATE.md:213-250`).
- Lock 5: lowerers consume Backend IR only (`restart/ARCHITECTURE.md:1004-1012`).
- Lock 10: Pratt, SIMD, and materialization shape are mined/cost-derived, not
  grammar-author directives (`restart/locks/LOCKS.md:52`).
- Lock 14: adding a grammar is `.bbnf` plus metadata, with zero generic-crate
  Rust changes (`restart/locks/LOCKS.md:60`,
  `restart/ARCHITECTURE.md:1697-1733`).
- Lock 16: primitive additions require an allowed architectural primitive,
  scalar parity, and a same-wave consumer (`restart/MASTER-PLAN.md:535-588`).
- Direct typed output must use `DirectBuild { shape, fields }`; the output
  schema comes from grammar-derived facts when possible or from a host/API type
  contract when not (`restart/skinny/COMPILER.md:330-357`,
  `restart/ARCHITECTURE.md:1511-1534`).

## 1. Structural Projection / Tape Union

### Target Shape

For every grammar, structural projection is a per-rule or per-entry event stream
with these data products:

1. `StructuralClassTable`: byte or codepoint classes derived from first sets,
   delimiters, quote/open/close tokens, trivia/comment starts, and regex
   prefilter facts.
2. `EventProjectionPlan`: which emitted positions become retained tape offsets,
   compact event cells, or direct sink events.
3. `PayloadFlagPlan`: sparse flags or event-cell bits for needs-decode,
   payload class, recovery/layout state, scalar policy, and skip/next facts.
4. `TapeBuilderPlan`: capacity and write policy for offsets, flags, and payload
   arenas.

These are side-table/data facts. They do not create another substrate.

Retained `OffsetTape` means scanner/lowerer writes `offsets` into the eventual
`Tape`; the parser consumes `cursor -> offsets[cursor]`. Current tape storage
already matches the needed direction: `Tape` owns `source`, `offsets`, sparse
flag vectors, payloads, and `TapeId` (`skinny/crates/runtime/src/tape/mod.rs:90-168`);
`TapeBuilder` writes offsets and sparse flags directly
(`skinny/crates/runtime/src/tape/assembler.rs:42-123`).

### General Grammar Rule

For byte-oriented grammars, `event.byte()` may be recovered from
`source[offsets[cursor]]`. For grammars that require side facts per event
layout, recovery, token class, multiline trivia, indentation, or typed payload
class, select `EventTape` and store compact cells indexed by the same cursor.

Do not materialize:

- a retained `StructuralIndex` beside the tape;
- a whitespace bitmap sidecar;
- parser-local source-byte scanners that duplicate the projection;
- dense parse-time aux columns unless the cost model chooses `EventTape` and the
  row pays for them.

### Required Replacement

Replace all JSON structural special cases with generated `StructuralClassTable`
data:

- `b"{}[],:\""` becomes a generated alphabet from grammar facts.
- JSON string quote/backslash/control handling becomes a generated
  `DelimitedRegionPlan`.
- capacity estimation counts class ids, not hardcoded `{`, `}`, `[`, `]`, `:`,
  `,`, and `"`.

## 2. BackendShape Taxonomy

The taxonomy remains exactly the five variants already present in `ir`
(`skinny/crates/ir/src/lib.rs:334-341`) and documented in ARCH
(`restart/ARCHITECTURE.md:1047-1087`):

| Shape | Retains queryable document? | Grammar-neutral trigger | Lowering |
|---|---:|---|---|
| `EagerTape` | yes | recovery, parse-time host decode, layout scope, or first-set overlap requiring speculative parse | source cursor + bounded checkpoints |
| `OffsetTape` | yes | byte-finite, disjoint first sets; lazy scalar/string payloads; no per-cursor side facts needed | typed cursor over retained offsets |
| `EventTape` | yes | retained document plus per-cursor payload/recovery/layout side facts | typed cursor over compact event cells |
| `SinkOnly` | no | public output does not need path/value traversal after parse | direct typed-field writes in parse loop |
| `CollapsedStage` | no or custom retained event sink | target ISA + grammar hub admits mask-held state walk and per-grammar `.asm` author exists | Rust shim + codegen `.data` + handwritten wrapper macro composition |

This taxonomy is enough for arbitrary grammars. CSS tends to require
`EagerTape`/`EventTape` around recovery/layout-heavy rules, Sheets/math use
`PrattSpine` plus `OffsetTape`/`SinkOnly` around expression outputs, CSV can
select `SinkOnly` or `OffsetTape` depending on row access, and JSON remains
mostly `OffsetTape` plus direct `SinkOnly` for typed output.

## 3. Cost Model

### Inputs

The cost model consumes existing facts only:

- Grammar IR rule graph, first sets, nullable/progress facts, bounded
  lookbehind facts.
- Directive facts from the six existing directives only.
- `ShapeFacts` / direct field facts.
- `RecognizerFacts`: Pratt candidates, exact SIMD candidates, prefilter
  candidates, literal-set density.
- Target features: ISA, available admitted kernels, `CollapsedStage` author
  availability.
- Output requirements: retained document, direct-only typed output, path/visitor
  need, host/API schema.
- Bench/profile priors: hot-call graph, event density, string/number/Unicode
  distribution, memory/RSS constraints.

### Decision Order

Use ARCH's eight-step decision tree as the safety rail:

1. Recovery -> `EagerTape`.
2. Parse-time host decode -> `EagerTape`.
3. Layout scope -> `EagerTape`.
4. First-set overlap -> `EagerTape` with speculative `Alt`.
5. Direct-only output and no post-parse traversal -> `SinkOnly`.
6. Target features plus byte-disjoint hub and wrapper author -> `CollapsedStage`.
7. Required retained side facts -> `EventTape`.
8. Else -> `OffsetTape`.

The implementation already has a first pass of this in
`derive_backend_shape_with_diagnostics`
(`skinny/crates/passes/src/lib.rs:287-331`), including fallback diagnostics for
missing collapsed-stage author (`skinny/crates/passes/src/lib.rs:303-312`).
The generalization work is to remove grammar-name inputs and record the decision
as `CostFacts`, not to change BIR.

### Evidence Shape

Every `CostDecision` should record:

- selected shape;
- rejected alternatives;
- dominated alternatives;
- objective vector: throughput, memory, generated LOC, i-cache/hot-function
  budget, correctness risk, implementation availability;
- scalarization profile;
- target ISA/profile;
- benchmark or static extraction method.

Probe wins emit a cost diagnostic and route to H work; they do not become a
directive. BENCH already requires alternate plan probes for event cursor,
primitive kernel, capacity, and fused string sink plans
(`restart/skinny/BENCH.md:1254-1283`).

## 4. Primitive Vocabulary

Split primitives into grammar-neutral operations plus generated per-grammar
data. The primitive function name must never contain `json`, `css`, `yaml`, or
any other grammar name in a generic crate.

### Core Byte/Event Primitives

| Primitive | Inputs | Output | Consumers |
|---|---|---|---|
| `byte_class_from_table_64` | 64 bytes + 256-byte class table | class/emit mask | exact structural scan, token prefilters |
| `byte_class_from_eq_set_64` | 64 bytes + small byte set | membership mask | delimiter, whitespace, literal starts |
| `bitmap_prefix_xor_64` | quote/terminator mask + carry | in-region mask | strings, comments, heredocs, quoted regions |
| `bitmap_next_set_bit` | bitmap + cursor | next event lane | event cursor |
| `bulk_emit_positions_64` | base + mask + dst | offset count | `OffsetTape` builder |
| `eob_pad_clamp` | tail bytes | safe fixed-width block | scalar/SIMD tail handling |
| `digit_block_accumulate` | digit bytes + radix/policy | integer chunk | number materializers |
| `hex_nibble_decode_16/32/64` | escaped hex bytes | scalar units + validity mask | Unicode escape decode |
| `utf8_validate_block` | bytes | validity mask/state | borrowed string validation |
| `skip_class_run` | class table + cursor | new cursor | trivia/comment/space runs |

The current `bbnf-simd` already has the right generic direction for alphabet
tables and class-table scanning (`skinny/crates/bbnf-simd/src/lib.rs:19-50`,
`skinny/crates/bbnf-simd/src/lib.rs:106-123`) and generic primitive dispatch
wrappers (`skinny/crates/bbnf-simd/src/lib.rs:169-180`). Keep that surface and
move any JSON-specific scanner logic into generated grammar data/output.

### Layering

- `bbnf-simd`: fixed-width byte/class/bitmap kernels and scalar references.
- `parse-that/string`: delimited-region matching, escape detection, lazy decode.
- `parse-that/unicode`: UTF-8 validation, Unicode scalar construction, escape
  policy, surrogate policy.
- `parse-that/number`: integer/float span match and materialization.
- generated runtime: tables and calls, never bespoke primitive loops.

Admit no primitive without a scalar reference, checkasm/parity gate, and a
same-wave generated consumer.

## 5. String, Number, And Unicode Primitives

### String Plan

Represent strings as a `DelimitedRegionPlan`:

```text
open_delimiter: byte sequence or token class
close_delimiter: byte sequence or token class
escape_prefix: optional byte sequence
forbidden_ranges: byte/codepoint classes
line_policy: single-line | multiline | folded
raw_span_policy: include_delimiters | body_only
decode_policy: none | escape_table | unicode_escape | grammar_host
```

Generated code should emit:

- fast tiny/plain matcher for escape-free spans;
- long matcher using byte-class and prefix-XOR primitives;
- `needs_decode` / `has_control` flags on the same event cursor;
- lazy borrow for retained views;
- field-layout decode materializer for `SinkOnly` typed outputs when required.

JSON strings, CSS strings, BBNF literals, quoted YAML scalars, and Sheets string
literals all fit this shape by changing data, not code.

### Number Plan

Represent numbers as `NumberLexemePlan` plus `NumberMaterializerPlan`:

```text
sign_policy
radix
integer_digits
fraction_policy
exponent_policy
separator_policy
suffix_policy
target: lazy_span | i64 | u64 | f64 | decimal | host_scalar
overflow_policy
negative_zero_policy
```

The lexer records `(start, end, class)` once. Retained views can lazy-parse from
the span. `SinkOnly` direct fields materialize only the target scalar requested
by `DirectBuildField.target`; they must not route through a generic event sink
that tries i64/u64/f64 in sequence for every field.

Use Eisel-Lemire / integer fast paths where admitted, with AVX/NEON digit MAC
only as a primitive selected by the cost model.

### Unicode Plan

Unicode is not a BBNF directive and not a BIR variant. It is internal to regex,
string, and host primitive facts.

Represent Unicode escape and validation as:

```text
encoding: utf8 | bytes | host
escape_forms: \uXXXX | \u{...} | \xNN | named | none
surrogate_policy: reject | combine_pairs | pass_through
scalar_policy: UnicodeScalar | code_unit | byte
normalization_policy: none | NFC | host
class_algebra: regex-owned
```

Rules:

- borrowed strings must validate exactly once or inherit trusted input facts;
- escaped direct strings decode in the same field materializer that constructs
  the target field;
- Unicode class algebra stays under `parse-that-regex`; BBNF sees opaque regex
  facts and verifier routes;
- no parser-side eager decode for retained documents unless the cost model
  selected `EagerTape` due to parse-time host decode.

## 6. Direct-To-Struct Output

### Authority Boundary

Direct typed output is a `DirectBuild` payload refinement. Existing IR already
has:

- `BackendExpr::DirectBuild { shape, fields }`
  (`skinny/crates/ir/src/lib.rs:377-380`);
- `DirectBuildField`
  (`skinny/crates/ir/src/lib.rs:445-450`);
- target/presence/cardinality/representation/decode fields
  (`skinny/crates/ir/src/lib.rs:462-514`);
- `SinkOnlyProgram` preserving direct shapes and fields
  (`skinny/crates/codegen/src/lower/sink_only.rs:1-39`).

The missing part is source authority. If the grammar implies the output type
(AST-like grammar output), shape mining supplies `ShapeFacts` and field facts.
If the output type is host/API-owned (`TwitterSearch`, `UpdateCenter`, user
structs), a schema source supplies the same field facts. This is data, not BBNF
syntax.

### Field-Fact Minimum

Each direct field fact needs:

- source reference: span, child binding, repeated binding, literal choice, map
  entry, empty/default, host primitive result;
- target field id/path and target type;
- cardinality: one, optional, repeated, map;
- presence/null/default policy;
- duplicate and unknown-field policy where object-like;
- representation: borrowed, owned, borrowed-or-owned;
- materializer: borrow span, decode string, number scalar, literal map, child,
  repeated, map, empty, host scalar;
- error policy and diagnostic context.

String and number facts must be semantic materializer policies, not
JSON-named variants. `DirectBuildDecode::JsonString` / `JsonNumber` are useful
prototype markers, but the general contract should become `EscapedString`,
`NumberScalar`, `Literal`, and `Raw` under the existing DirectBuild payload.

### Generated Code Shape

`SinkOnly` typed output should generate schema-state parser functions:

- object fields as local variables plus seen bitsets;
- arrays as direct `Vec<T>` pushes;
- maps as direct key materialization and insertion;
- nested objects as typed parser calls returning the nested type;
- unknown-field skip as generated skip over the grammar value shape;
- scalar fields materialized according to field target type.

It should not generate:

- a generic event sink stack as the representative typed close;
- `serde_json::Value` as an intermediate;
- checksum-only parse-time sinks;
- benchmark-private hand parsers;
- generic-crate branches on JSON shape names.

BENCH has already split the guard and representative rows: the semantic digest
stressor remains visible, while `real_typed_struct` is representative only after
host/API schema facts feed generated DirectBuild (`restart/skinny/BENCH.md:778`,
`restart/skinny/BENCH.md:857-868`).

## 7. Required Replacement Plan

### A. Fact Model

1. Replace `shapes_for_json()` with `derive_shape_facts(grammar, schemas)`.
2. Add resolved direct field facts to the shape side-table family. Names from
   sidecars are resolved to ids before extraction.
3. Replace `StructuralAlphabet::json()` with generated structural alphabets from
   `RecognizerFacts`.
4. Replace `TapeKind::{Object, Array, ...}` in generic IR with grammar-neutral
   node/event kind ids when implementation reaches that boundary. This is a
   payload type cleanup, not a BIR variant addition.

### B. Recognizer Mining

1. Replace `nominate_json()` with `nominate_recognizers(grammar, metadata)`.
2. Mine byte-disjoint first sets, literal sets, regex prefilters, Pratt spines,
   delimiter/comment/string regions, and structural alphabets.
3. Emit `RecognizerFacts` containing tables and verifier routes.

### C. BIR Extraction

1. `extract::single_plan` selects the entry from metadata, not the rule name
   `json`.
2. `materialize_rule` checks resolved direct-build facts by `RuleId`, not rule
   names like `object`, `string`, `number`.
3. Schedule `TapeEmit` and `DirectBuild` together from side tables, preserving
   the invariant at `restart/ARCHITECTURE.md:1008-1009`.

### D. Codegen

1. Replace `emit_json_*` with generic `emit_grammar_*` entry points, keeping
   generated output under `runtime/src/grammars/<name>/`.
2. Keep per-shape lowerer dispatch as already modeled in
   `codegen/src/lower/rust.rs:26-61`.
3. Generate per-grammar tables/data into the generated module; do not encode
   them in generic crates.
4. Keep typed direct output in the consuming crate when the schema is host/API
   owned; runtime must not depend on benchmark structs.

### E. Runtime / SIMD

1. Make `TapeBuilder` the only retained offset writer for `OffsetTape`.
2. Add `EventTape` compact cells only when cost-selected.
3. Keep `bbnf-simd` primitive APIs table-driven.
4. Move any grammar-specific scanner state machines into generated tables and
   generated wrappers.

## 8. Gates

Static gates:

- `cargo xtask lint-grammar-generalization`: no grammar names in generic crates.
- Future grammar test: add `yaml.bbnf` plus metadata only; generic crate diff
  must be empty except generated output.
- Codegen import-deny: backend lowerers cannot import Grammar IR.
- BIR snapshot: no new variants; `DirectBuild` payload facts expand in place.
- Generated header includes grammar hash, metadata hash, schema hash, recognizer
  facts hash, and cost decision hash.

Correctness gates:

- scalar/SIMD structural parity per grammar alphabet;
- retained `DocumentView` and direct typed output share spans/events where a
  retained document exists;
- `SinkOnly` has no retained document identity;
- direct typed output parity against independent Track 2 and host/API oracle;
- Unicode/string/number materialization parity for retained and direct outputs.

Performance gates:

- report per-shape `CostFacts`;
- report materialization policy, string ownership, scalar-cache policy, direct
  field count, repeated-access class, and source ownership mode;
- compare parse-only, traversal, semantic digest, and real typed output as
  separate rows;
- preserve same-plane sidecars and host hardware metadata.

## 9. Summary Recommendation

The arbitrary-grammar abstraction should land as:

1. side-table fact generalization;
2. generated per-grammar structural/event data;
3. table-driven SIMD/string/number/Unicode primitives;
4. existing `BackendShape` per-rule cost selection;
5. existing `DirectBuild { shape, fields }` payload refinement for typed output.

That route handles JSON, CSS, Sheets, BBNF-self, CSV, YAML, and future grammars
without new directives, without new BIR variants, and without a second substrate.
The current JSON-only functions are deletion targets. The reusable architecture
is already present in the contracts: structural projection equals tape when
retained, direct output is a projection over the same accepted event stream, and
all grammar-specific differences are data consumed by generic lowerers.

# SK-V6 C6 Generality and Lock 14 / CostFacts Audit

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-15
Repo edits: none. This report is the only written artifact.

## Scope and Method

Read sources:

- `restart/skinny/audit/SK-V6-COHORT/skv6-A5-general-grammar-abstraction.md`
- `restart/skinny/audit/SK-V6-COHORT/skv6-B6-spec-edit-map.md`
- `restart/ARCHITECTURE.md` sections 7.3 and 7.5
- `restart/skinny/COMPILER.md`
- `restart/skinny/SUBSTRATE.md`
- `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md` for current wave ownership

Search surface:

```sh
rg -n --glob '!**/generated/**' --glob '!**/runtime/src/grammars/json/**' --glob '!**/*.json' \
  -e 'json|Json|JSON|object|Object|array|Array|pair|Pair|string|String|number|Number|bool|Bool|null|Null|css|Css|CSS|sheets|Sheets|bbnf|Bbnf|BBNF|yaml|Yaml|YAML' \
  skinny/crates/passes skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/parse-that-regex skinny/crates/runtime/src/tape
```

I treated generated JSON runtime modules under `runtime/src/grammars/json` as excluded. I did not exclude `skinny/crates/codegen/src/json_templates/*`, because those templates live in the generic `codegen` crate and are therefore part of the generic-crate Lock 14 surface.

## Governing Facts

- A5 says the route is fact-model and lowering cleanup, not a new directive or BIR variant. Existing carriers stay: `LayoutFacts.backend_shape`, `ShapeFacts`, `RecognizerFacts`, and `CostFacts` (`skv6-A5...md:8-23`).
- A5 identifies the current prototype waivers: `shapes_for_json`, `nominate_json`, `StructuralAlphabet::json`, and JSON-branded codegen entry points (`skv6-A5...md:25-32`).
- A5 requires generated `StructuralClassTable` data to replace hardcoded JSON structural alphabets and delimiter handling (`skv6-A5...md:92-101`).
- A5 requires `derive_backend_shape_with_diagnostics` decisions to be recorded as `CostFacts`, not just written directly to side tables (`skv6-A5...md:155-160`), with selected, rejected, dominated, objective-vector, scalarization, target/profile, and extraction-method evidence (`skv6-A5...md:162-178`).
- ARCH 7.3 defines the public side tables, including `CostFacts`, `DirectFieldFacts`, and `PrimitiveFacts` (`restart/ARCHITECTURE.md:1033-1058`).
- ARCH 7.5 makes `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` the Lock 14 diagnostic (`restart/ARCHITECTURE.md:1135-1183`).
- COMPILER says the remaining JSON-name helpers in generic crates are cleanup targets, not V1 patterns (`restart/skinny/COMPILER.md:29-36`).
- SUBSTRATE says generalized substrate facts are `StructuralClassTable`, `DirectFieldFacts`, `CostFacts`, and `PrimitiveFacts`, with JSON only one instance (`restart/skinny/SUBSTRATE.md:23-31`).

## Wave Map Used

The checked-in packet is the current implementation owner map:

- Wave 1: DAV1D-grade checkasm hardening (`IMPLEMENTATION-PACKET...md:64-90`).
- Wave 2: retained parse recovery, including `parse-that-regex` and `bbnf-simd` interventions (`IMPLEMENTATION-PACKET...md:91-118`).
- Wave 3: generated DirectBuild field-layout materialization and `DirectFieldFacts` (`IMPLEMENTATION-PACKET...md:119-149`).
- Wave 4: grammar-neutral `CostFacts` and Lock 14 cleanup (`IMPLEMENTATION-PACKET...md:150-176`).
- Wave 5: primitive bodies with consumers (`IMPLEMENTATION-PACKET...md:177-199`).

B6's outline differs slightly: it puts "Fact Model And Lock 14 Cleanup" in its proposed Wave 3 and "DirectBuild Schema Facts" in its proposed Wave 4 (`skv6-B6...md:744-811`). For this audit, I assign owners to the checked-in SK-V6 packet and note B6 conflicts only where they affect execution.

## Blocking Production Leaks

| Surface | Exact leak | Replacement fact | Current packet owner |
|---|---|---|---|
| `passes` | `skinny/crates/passes/src/lib.rs:30` calls `shapes::shapes_for_json()` for every grammar. | `DirectFieldFacts` plus grammar/schema-derived `ShapeFacts`; no JSON shape constructor. | Wave 4 deletes the generic leak; Wave 3 supplies `DirectFieldFacts`. |
| `passes` | `skinny/crates/passes/src/lib.rs:31` calls `recognizers::nominate_json(&normalized)`. | `RecognizerRoute` derived by `nominate_recognizers(grammar, metadata)`. | Wave 4. |
| `passes` | `skinny/crates/passes/src/lib.rs:211-238` hardcodes `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, and `JsonNull`. | `DirectFieldFacts` / schema-derived shape facts resolved to ids before extraction. | Wave 3 creates the facts; Wave 4 removes the JSON helper. |
| `passes` | `skinny/crates/passes/src/lib.rs:245-249` defines `nominate_json` and calls `StructuralAlphabet::json()`. | `StructuralClassTable` plus `RecognizerRoute` tables and verifier routes. | Wave 4. |
| `passes` | `skinny/crates/passes/src/lib.rs:577-579` prefers `rule_by_name("json")` as the hot-path root. | `CostFacts` hot-rule/profile evidence keyed by `RuleId`, with entry route from metadata. | Wave 4. |
| `passes` | `skinny/crates/passes/src/lib.rs:658-660` requires entry rule `"json"`. | `RecognizerRoute` / metadata-selected entry rule, not a literal rule name. | Wave 4. |
| `passes` | `skinny/crates/passes/src/lib.rs:742-750` maps literal rule names to `TapeKind::{Object, Array, Pair, String, Number, Bool, Null}` and `Json*` shapes. | `DirectFieldFacts` plus grammar-derived node/event kind ids; no generic `TapeKind` JSON enum. | Wave 4; the direct-field payload lands in Wave 3. |
| `passes` | `skinny/crates/passes/src/lib.rs:755-807` hardcodes JSON field rosters for `object`, `array`, `pair`, `string`, `number`, `bool`, and `null`. | `DirectFieldFacts` keyed by `RuleId`, `BindingId`, and field/source ids. | Wave 3 for facts, Wave 4 for deleting generic matches. |
| `codegen` | `skinny/crates/codegen/src/lib.rs:2-3` declares `json_sink_direct` and `json_typed_direct` modules in the generic crate. | Generic emitters consuming `RecognizerRoute`, `DirectFieldFacts`, and `PrimitiveFacts`; JSON-specific code generated under per-grammar output. | Wave 4 for generic codegen cleanup; Wave 3 for typed direct generation. |
| `codegen` | `skinny/crates/codegen/src/lib.rs:68-97` exposes `emit_json*` entry points and calls `grammar::parse_json_grammar`. | `emit_grammar_*` selected from grammar source and workspace metadata. | Wave 4. |
| `codegen` | `skinny/crates/codegen/src/lib.rs:117`, `131-154` call JSON-specific direct renderers. | Shape-generic renderer driven by `DirectFieldFacts` and selected `BackendShape`. | Wave 3 for DirectBuild materialization; Wave 4 for Lock 14 deletion. |
| `codegen` | `skinny/crates/codegen/src/lib.rs:180-188` emits `JsonSink`, `JsonValue`, `JsonVisitor`, `JsonRoot`, and related reexports from generic codegen text. | Per-grammar generated module names emitted from metadata and schema facts. | Wave 4. |
| `codegen` | `skinny/crates/codegen/src/lib.rs:201-226` includes `json_templates/*` and `runtime/src/grammars/json/{scan,sink}.rs` from the generic crate. | Per-grammar generated modules under `runtime/src/grammars/<name>/`, not generic-crate includes of JSON runtime files. | Wave 4. |
| `codegen` | `skinny/crates/codegen/src/json_sink_direct.rs:4-15` requires JSON rule and shape rosters. | `DirectFieldFacts` and `RecognizerRoute` rosters from Backend IR facts. | Wave 3 supplies facts; Wave 4 removes JSON rosters. |
| `codegen` | `skinny/crates/codegen/src/json_sink_direct.rs:33-68` validates entry `json` and JSON literals. | Metadata entry route plus grammar-derived literal facts. | Wave 4. |
| `codegen` | `skinny/crates/codegen/src/json_sink_direct.rs:86-109` validates hardcoded `Json*` field rosters. | `DirectFieldFacts` roster preserved through `SinkOnlyProgram`. | Wave 3. |
| `codegen` | `skinny/crates/codegen/src/json_sink_direct.rs:131-560` emits a JSON parser and `JsonSink` direct path from generic source. | Generated SinkOnly code from `RecognizerRoute`, `DirectFieldFacts`, `PrimitiveFacts`, and `CostFacts` selected routes. | Wave 3 for typed materialization, Wave 4 for genericity, Wave 5 for new primitive bodies. |
| `codegen` | `skinny/crates/codegen/src/lower/schema_direct.rs:16-35` requires `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`, and JSON literal recognizers. | `DirectFieldFacts` plus grammar-derived literal/skip facts. | Wave 3. |
| `codegen` | `skinny/crates/codegen/src/json_typed_direct.rs:25-28`, `417`, `466-473`, `528-533`, `603-604` call `match_json_*`, `skip_json_whitespace`, and `unescape_json_string`. | `PrimitiveFacts` selected string/number/whitespace primitives, with `CostFacts` recording selected/rejected routes. | Wave 4 for naming cleanup; Wave 5 if new primitive bodies are required. |
| `codegen` | `skinny/crates/codegen/src/json_typed_direct.rs:263-294` uses `field.json_key`; `direct_schema.rs:35-46` stores `json_key`; `direct_schema.rs:163-194` validates "JSON key". | `DirectFieldFacts` field/source ids and grammar-neutral field key/path labels. | Wave 3. |
| `codegen` | `skinny/crates/codegen/src/json_templates/{generated,parser,value,view,visitor}.rs` are JSON templates inside the generic crate, with representative anchors `generated.rs:10`, `generated.rs:20`, `parser.rs:47-50`, `value.rs:12-75`, `view.rs:11-260`, `visitor.rs:4-12`. | Generated per-grammar runtime templates emitted from `RecognizerRoute`, `StructuralClassTable`, `DirectFieldFacts`, and `PrimitiveFacts`. | Wave 4. |
| `parse-that-regex` | `skinny/crates/parse-that-regex/src/lib.rs:34-45` exposes `JsonStringMatch`, `StringMode::StrictJson`, and `StrictJsonTrustedUtf8`. | `PrimitiveFacts` string plan, with mode data from a generated string/delimited-region plan. | Wave 4 should own; see owner-path gap below. |
| `parse-that-regex` | `skinny/crates/parse-that-regex/src/lib.rs:120-178`, `260-265` exposes JSON number match and validation APIs. | `PrimitiveFacts` number lexeme/materializer plan selected by `CostFacts`. | Wave 4 cleanup; Wave 5 for any new number primitive body. |
| `parse-that-regex` | `skinny/crates/parse-that-regex/src/lib.rs:127-139` exposes `skip_json_whitespace`. | `StructuralClassTable` / `PrimitiveFacts` `skip_class_run` over generated trivia classes. | Wave 4 cleanup; Wave 5 if a new primitive body is required. |
| `parse-that-regex` | `skinny/crates/parse-that-regex/src/lib.rs:268-341` exposes JSON string match wrappers. | `PrimitiveFacts` delimited-region match over generated delimiter/escape/control policy. | Wave 4 cleanup. |
| `parse-that-regex` | `skinny/crates/parse-that-regex/src/lib.rs:416-514`, `434-475`, `479-514` hardcode JSON escape and Unicode escape validation/decode names. | `PrimitiveFacts` Unicode/string escape policy facts; generated grammar data selects forms like `\uXXXX`. | Wave 4 cleanup; Wave 5 for admitted SIMD bodies. |
| `parse-that-regex` | `skinny/crates/parse-that-regex/src/lib.rs:594-719`, `679-719` use `skip_json_string_plain*` and `json_string_special_mask`. | `PrimitiveFacts` string special-class plan and `StructuralClassTable` bytes for terminator/escape/control. | Wave 4 cleanup. |
| `parse-that-regex` | `skinny/crates/parse-that-regex/src/lib.rs:766-854`, `854-968` expose `classify_json_string_content`, `validate_json_string`, `unescape_json_string`, and `json_string_escape_control_mask`. | `PrimitiveFacts` string content classifier and escape materializer selected by `CostFacts`. | Wave 4 cleanup; Wave 5 for SIMD materializer bodies. |
| `runtime/tape` | No production grammar-name leak found. `rg` hits were only generic `bool` methods at `runtime/src/tape/mod.rs:25` and `:57`. | None. Keep as grammar-neutral tape storage. | None. |

## Adjacent Leak Outside Requested Crate List

`skinny/crates/ir/src/lib.rs` is not in the requested scan surface, but the requested leaks point to it:

- `skinny/crates/ir/src/lib.rs:411-416` defines `StructuralAlphabet::json()` with `b"{}[],:\""`.
- `skinny/crates/ir/src/lib.rs:433-443` defines JSON-shaped `TapeKind::{Object, Array, Pair, String, Number, Bool, Null, Member, Element}`.
- `skinny/crates/ir/src/lib.rs:510-515` defines `DirectBuildDecode::{JsonString, JsonNumber}`.

Replacement: generated `StructuralClassTable`, grammar-derived node/event kind ids, and `DirectFieldFacts` materializer policies (`EscapedString`, `NumberScalar`, `Literal`, `Raw`) under existing DirectBuild payloads. Owner: Wave 4 for `StructuralAlphabet::json` / `TapeKind` cleanup; Wave 3 for DirectBuild field/materializer facts.

## bbnf-simd Status

Production `bbnf-simd/src` is mostly grammar-neutral:

- `skinny/crates/bbnf-simd/src/lib.rs:19-50` builds a table-driven `StructuralAlphabet`.
- `skinny/crates/bbnf-simd/src/lib.rs:106-123` scans from a supplied alphabet.
- `skinny/crates/bbnf-simd/src/dispatch.rs:42-98` selects scalar or NEON by alphabet admissibility, not grammar name.

No production symbol equivalent to `scan_json_*`, `classify_json_*`, or `NeonJson` exists in `src`. The remaining hits are:

- provenance comments referencing `asmjson`, `simdjson`, JSON papers, or example grammars, such as `ext/x86/bbnf.asm:16-21`, `:44`, `:170-175`, `:351`, `:408`;
- JSON-biased test/report fixtures:
  - `skinny/crates/bbnf-simd/tests/classifier_parity.rs:3`
  - `skinny/crates/bbnf-simd/tests/corpus_parity.rs:3`
  - `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:30`, `:91-92`, `:236-250`, `:348-359`, `:461-496`
  - `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:298-323`
  - `skinny/crates/bbnf-simd/tests/checkasm_structural_terminator_64.rs:7`, `:49-51`
  - `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:12`, `:20`, `:105-123`, `:175-178`, `:238-240`
  - `skinny/crates/bbnf-simd/CONCRETIZATION-REPORT.md:126-129`

Replacement for tests: fixture alphabets should be generated `StructuralClassTable` samples or explicitly annotated scalar-oracle fixtures; primitive admission evidence belongs in `PrimitiveFacts`. Owner: Wave 1 for checkasm harness normalization, Wave 4 for Lock 14 naming cleanup in `src`, Wave 5 for admitted primitive bodies with consumers.

## CostFacts Audit

No `CostFacts` or `CostDecision` implementation exists in the requested crates. `rg -n "CostFacts|CostDecision|cost|Cost"` over `passes`, `codegen`, `bbnf-simd`, `parse-that-regex`, `runtime/tape`, and `ir` found only documentation/test incidental hits, not a side-table implementation.

Current behavior writes shape choices directly:

- `skinny/crates/passes/src/lib.rs:33-39` assigns `layout_facts.backend_shape = shape_plan.backend_shape`.
- `skinny/crates/passes/src/lib.rs:287-331` selects `BackendShape` by rule checks and diagnostics, with no selected/rejected/dominated alternative evidence.
- `restart/skinny/COMPILER.md:853-858` explicitly says the cost model is stubbed and there is no `CostFacts`, `CostDecision`, scalar score, or Pareto frontier.

Replacement: Wave 4 must add `CostFacts` records for backend shape, tiny-string cap, quoted-span strategy, direct materializer, capacity policy, and primitive route, matching A5's evidence shape and ARCH 7.3. This is not optional cleanup: without `CostFacts`, JSON-specific thresholds and parser/materializer choices will keep reappearing as hardcoded codegen or parse-that-regex policy.

## Packet Ownership Gap

The current packet's Wave 4 exit gate scans `skinny/crates/{passes,codegen,bbnf-simd,parse-that-regex}/src` (`IMPLEMENTATION-PACKET...md:172-174`), but Wave 4 owner paths list only:

- `skinny/crates/passes/`
- `skinny/crates/codegen/`
- `skinny/crates/bbnf-simd/`
- docs (`restart/ARCHITECTURE.md`, `restart/skinny/COMPILER.md`)

`parse-that-regex` is omitted from Wave 4 owner paths even though it has the largest production JSON primitive API surface. Wave 2 owns `parse-that-regex` for retained parse recovery, but not genericity cleanup. The packet should route `parse-that-regex` JSON API replacement to Wave 4, with Wave 5 owning any new admitted primitive bodies.

## Prioritized Receiver List

1. Wave 4: remove `passes` `shapes_for_json`, `nominate_json`, entry `"json"`, rule-name materialization switches, and hot-path `"json"` seed. Add `CostFacts` evidence at the same time.
2. Wave 3 then Wave 4: introduce `DirectFieldFacts` and make `codegen` consume them; then delete `json_sink_direct`, `json_typed_direct`, `json_templates`, `emit_json_*`, and `json_key` naming from generic codegen.
3. Wave 4: make `parse-that-regex` expose grammar-neutral string/number/trivia/Unicode primitive APIs. Keep JSON behavior as generated data or compatibility wrappers outside generic crates only if explicitly fenced.
4. Wave 1/Wave 5: keep `bbnf-simd` primitive APIs table-driven; convert JSON-biased tests to generated class-table fixtures or scalar-oracle samples, and record admitted primitives as `PrimitiveFacts`.
5. No action for `runtime/tape`: it passed the requested grammar-name scan.

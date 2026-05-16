# SK-V7 W7 R3 - Lock 14 leak accounting and public consumer audit

Scope: read-only research for W7 Phase 1. No source mutation is proposed here.

## Required-reading anchors

- W7 owner paths and gate are `parse-that-regex/src/lib.rs` plus `passes/src/lib.rs`; the wave explicitly names Phase A parse-that-regex renames, Phase B passes refactor, `cargo test --workspace`, no JSON-prefixed parse-that-regex public types, `passes::compile()` without literal-name matching, and Lock 14 HIGH count `-20` (`restart/skinny/tranches/sk-v7/SPEC.md:306`, `restart/skinny/tranches/sk-v7/SPEC.md:309`, `restart/skinny/tranches/sk-v7/SPEC.md:310`, `restart/skinny/tranches/sk-v7/SPEC.md:313`, `restart/skinny/tranches/sk-v7/SPEC.md:314`, `restart/skinny/tranches/sk-v7/SPEC.md:315`, `restart/skinny/tranches/sk-v7/SPEC.md:318`, `restart/skinny/tranches/sk-v7/SPEC.md:319`, `restart/skinny/tranches/sk-v7/SPEC.md:320`).
- Lock 14 forbids grammar-specific code, modules, public types, feature flags, and grammar match arms in generic crates including `parse-that-regex` and `passes` consumers through IR/codegen/runtime boundaries (`restart/locks/LOCKS.md:60`).
- The current SK-V7 accounting says the baseline audit found about 46 HIGH leaks split as `passes` 11, `codegen` about 18, `parse-that-regex` 9, `ir` 3, plus 5 file-location violations; B3 sequencing is Phase A `parse-that-regex` 9 HIGH then Phase B `passes` 11 HIGH (`restart/skinny/tranches/sk-v7/SYNTHESIS.md:151`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:152`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:162`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:163`).
- HANDOFF pre-blocks unrelated implementation routes, including SK-V5 UTF-8 fusion, SK-V6 retained/direct materialization routes, tiny-string wiring, pair-token fusion, function-pointer dispatch, capacity prescan, generic SWAR whitespace, separator elision, raw f64 shortcut, and EventCursor prepass (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`, `restart/skinny/tranches/sk-v7/HANDOFF.md:71`, `restart/skinny/tranches/sk-v7/HANDOFF.md:75`, `restart/skinny/tranches/sk-v7/HANDOFF.md:81`, `restart/skinny/tranches/sk-v7/HANDOFF.md:84`, `restart/skinny/tranches/sk-v7/HANDOFF.md:85`, `restart/skinny/tranches/sk-v7/HANDOFF.md:86`, `restart/skinny/tranches/sk-v7/HANDOFF.md:87`, `restart/skinny/tranches/sk-v7/HANDOFF.md:88`, `restart/skinny/tranches/sk-v7/HANDOFF.md:89`, `restart/skinny/tranches/sk-v7/HANDOFF.md:90`, `restart/skinny/tranches/sk-v7/HANDOFF.md:91`, `restart/skinny/tranches/sk-v7/HANDOFF.md:93`).

## Current HIGH leak names

### `parse-that-regex`

The count-driving Phase A surface is the public parser primitive API. The strict gate is public API, so these names must not remain public under aliases.

| Current name | Citation | Required shape |
|---|---:|---|
| `JsonStringMatch` | `skinny/crates/parse-that-regex/src/lib.rs:34` | Collapse to existing `StringMatch`; content bounds derive from `raw_start/raw_end` and flags. |
| `StringMode::StrictJson` | `skinny/crates/parse-that-regex/src/lib.rs:44` | Replace grammar-named mode with a structural string plan or special-byte policy. |
| `StringMode::StrictJsonTrustedUtf8` | `skinny/crates/parse-that-regex/src/lib.rs:45` | Same as above; trust is an input fact, not a grammar name. |
| `JsonNumberMatch` | `skinny/crates/parse-that-regex/src/lib.rs:120` | Collapse into `number::NumberSpan`, already public at `skinny/crates/parse-that-regex/src/number/mod.rs:5`. |
| `skip_json_whitespace` | `skinny/crates/parse-that-regex/src/lib.rs:127` | Rename/generalize to a class-run skipper over a whitespace/trivia class. |
| `match_json_number` | `skinny/crates/parse-that-regex/src/lib.rs:164` | Use `number::match_number_span` or a grammar-neutral number scanner. |
| `match_json_number_from_first` | `skinny/crates/parse-that-regex/src/lib.rs:174` | Use `number::match_number_span_from_first` or equivalent. |
| `validate_json_number` | `skinny/crates/parse-that-regex/src/lib.rs:260` | `validate_number` over the same grammar-neutral number plan. |
| `match_json_string` | `skinny/crates/parse-that-regex/src/lib.rs:268` | Use `match_string` with a structural policy. |
| `match_json_string_at_quote` | `skinny/crates/parse-that-regex/src/lib.rs:280` | Use `match_string_at_quote` with a structural policy. |
| `match_json_string_at_quote_trusted_utf8` | `skinny/crates/parse-that-regex/src/lib.rs:298` | Trusted UTF-8 should be a policy/input fact, not a JSON public symbol. |
| `decode_json_unicode_escape` | `skinny/crates/parse-that-regex/src/lib.rs:434` | Move behind `unicode` policy naming; JSON surrogate semantics must be explicit data. |
| `classify_json_string_content` | `skinny/crates/parse-that-regex/src/lib.rs:766` | Classify delimited content over a `SpecialByteSet`/string policy. |
| `validate_json_string` | `skinny/crates/parse-that-regex/src/lib.rs:847` | `validate_string` over the same string policy. |
| `unescape_json_string` | `skinny/crates/parse-that-regex/src/lib.rs:854` | Generic escaped-string materializer over an escape policy. |

Prior A5 inventory already called the `JsonStringMatch`, `StringMode::StrictJson`, `JsonNumberMatch`, `skip_json_whitespace`, JSON number scanner, JSON string scanner, JSON escape/unicode, and JSON plain-string mask regions grammar leaks (`restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:71`, `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:72`, `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:73`, `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:74`, `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:75`, `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:76`, `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:77`, `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md:78`).

### `passes`

The Phase B surface is production `passes/src/lib.rs`, not the generated JSON runtime. Current production leaks are:

| Current name or literal | Citation | Required shape |
|---|---:|---|
| `compile()` hardwires `shapes::shapes_for_json()` | `skinny/crates/passes/src/lib.rs:30` | Derive shape facts from grammar/schema facts. |
| `compile()` hardwires `recognizers::nominate_json()` | `skinny/crates/passes/src/lib.rs:31` | Derive recognizers from grammar metadata and target features. |
| `shapes_for_json` | `skinny/crates/passes/src/lib.rs:211` | Generic shape derivation surface. |
| `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull` shape names | `skinny/crates/passes/src/lib.rs:213`, `skinny/crates/passes/src/lib.rs:215`, `skinny/crates/passes/src/lib.rs:225`, `skinny/crates/passes/src/lib.rs:226`, `skinny/crates/passes/src/lib.rs:228`, `skinny/crates/passes/src/lib.rs:232`, `skinny/crates/passes/src/lib.rs:235`, `skinny/crates/passes/src/lib.rs:236`, `skinny/crates/passes/src/lib.rs:237` | Schema-derived shape ids/names supplied by grammar metadata. |
| `nominate_json` | `skinny/crates/passes/src/lib.rs:245` | Grammar-neutral recognizer nomination. |
| `StructuralAlphabet::json()` consumer | `skinny/crates/passes/src/lib.rs:248` | Alphabet derived from grammar structural classes; note the JSON constructor itself is in IR at `skinny/crates/ir/src/lib.rs:412`. |
| Hot-path entry fallback `rule_by_name("json")` | `skinny/crates/passes/src/lib.rs:577` | Use grammar entry or metadata hint. |
| Extract entry `rule_by_name("json")` / `MissingEntry("json")` | `skinny/crates/passes/src/lib.rs:659`, `skinny/crates/passes/src/lib.rs:660` | Use grammar entry rule, not literal JSON. |
| `materialization_descriptor` literal-name match | `skinny/crates/passes/src/lib.rs:750` | DirectFieldFacts/schema facts keyed by rule ids, not literal names. |
| Materialization arms for `object`, `array`, `pair`, `string`, `number`, `bool`, `null` with `Json*` shapes | `skinny/crates/passes/src/lib.rs:752`, `skinny/crates/passes/src/lib.rs:763`, `skinny/crates/passes/src/lib.rs:774`, `skinny/crates/passes/src/lib.rs:794`, `skinny/crates/passes/src/lib.rs:805`, `skinny/crates/passes/src/lib.rs:816`, `skinny/crates/passes/src/lib.rs:825` | Metadata/schema-driven materialization descriptors. |

Inline tests in the same source file are noisy but secondary. If the eventual grep gate scans `src/lib.rs` without excluding `#[cfg(test)]`, tests using `JSON_GRAMMAR`, `parse_json_grammar`, `shapes_for_json`, and `Json*` shape assertions will fail the same textual gate (`skinny/crates/passes/src/lib.rs:850`, `skinny/crates/passes/src/lib.rs:854`, `skinny/crates/passes/src/lib.rs:891`, `skinny/crates/passes/src/lib.rs:904`, `skinny/crates/passes/src/lib.rs:914`, `skinny/crates/passes/src/lib.rs:915`, `skinny/crates/passes/src/lib.rs:916`, `skinny/crates/passes/src/lib.rs:917`).

## Public consumer audit

Renaming parse-that-regex public symbols is source-breaking unless consumers migrate in the same wave.

| Consumer | Current dependency | Risk |
|---|---:|---|
| Generated retained runtime imports `match_json_number_from_first`, `match_json_string_at_quote_trusted_utf8`, `skip_json_whitespace`, `JsonNumberMatch`, and `JsonStringMatch`. | `skinny/crates/runtime/src/grammars/json/generated.rs:5`, `skinny/crates/runtime/src/grammars/json/generated.rs:6`, `skinny/crates/runtime/src/grammars/json/generated.rs:7` | Checked-in generated runtime must change with the template or compilation breaks. |
| Generated retained runtime returns `JsonStringMatch` and `JsonNumberMatch` from helper wrappers. | `skinny/crates/runtime/src/grammars/json/generated.rs:186`, `skinny/crates/runtime/src/grammars/json/generated.rs:189`, `skinny/crates/runtime/src/grammars/json/generated.rs:212` | Type collapse must preserve `raw_end`, `needs_unescape`, `start`, `end`, and `is_integer` semantics. |
| Codegen retained template mirrors the same imports and helper return types. | `skinny/crates/codegen/src/json_templates/generated.rs:5`, `skinny/crates/codegen/src/json_templates/generated.rs:6`, `skinny/crates/codegen/src/json_templates/generated.rs:7`, `skinny/crates/codegen/src/json_templates/generated.rs:186`, `skinny/crates/codegen/src/json_templates/generated.rs:189`, `skinny/crates/codegen/src/json_templates/generated.rs:212` | Template and checked-in generated runtime must stay byte/behavior aligned after regeneration. |
| Track2 hand parser imports JSON-named scan functions and depends on `needs_unescape`, `raw_end`, `start`, and `end`. | `skinny/crates/bbnf-bench/src/track2/json.rs:1`, `skinny/crates/bbnf-bench/src/track2/json.rs:2`, `skinny/crates/bbnf-bench/src/track2/json.rs:106`, `skinny/crates/bbnf-bench/src/track2/json.rs:116`, `skinny/crates/bbnf-bench/src/track2/json.rs:120`, `skinny/crates/bbnf-bench/src/track2/json.rs:187`, `skinny/crates/bbnf-bench/src/track2/json.rs:189`, `skinny/crates/bbnf-bench/src/track2/json.rs:190` | Bench Track2 must migrate or W7 will fail bench compilation. |
| Direct codegen uses JSON-named parse-that APIs. | `skinny/crates/codegen/src/json_typed_direct.rs:25`, `skinny/crates/codegen/src/json_typed_direct.rs:26`, `skinny/crates/codegen/src/json_typed_direct.rs:28`; `skinny/crates/codegen/src/json_sink_direct.rs:153`, `skinny/crates/codegen/src/json_sink_direct.rs:179`, `skinny/crates/codegen/src/json_sink_direct.rs:383` | W8 owns most codegen rebrand, but W7 parse-that public API changes still force compatibility edits or temporary internal adapters. |
| Runtime views and sinks use `match_json_string` and `unescape_json_string`. | `skinny/crates/runtime/src/grammars/json/view.rs:4`, `skinny/crates/runtime/src/grammars/json/view.rs:213`, `skinny/crates/runtime/src/grammars/json/view.rs:384`; `skinny/crates/runtime/src/grammars/json/sink.rs:1`, `skinny/crates/runtime/src/grammars/json/sink.rs:19`, `skinny/crates/runtime/src/grammars/json/sink.rs:30`, `skinny/crates/runtime/src/grammars/json/sink.rs:46`, `skinny/crates/runtime/src/grammars/json/sink.rs:87` | Retained view/decode semantics must be preserved; panic messages can mention JSON because this is a generated JSON runtime, not a generic crate. |

## Alias verdict

Public aliases violate the W7 gate. `pub type JsonStringMatch = StringMatch`, `pub use StringMatch as JsonStringMatch`, `pub type JsonNumberMatch = NumberSpan`, or deprecated public wrapper functions keep JSON-prefixed public API in `parse-that-regex`, directly contradicting "No JSON-prefixed types in parse-that-regex public API" (`restart/skinny/tranches/sk-v7/SPEC.md:318`) and Lock 14's zero grammar-specific public types in generic crates (`restart/locks/LOCKS.md:60`).

Private, non-exported aliases inside an implementation patch are not a public API violation, but they remain grep-gate risk if the gate is textual over `src/`. The cleaner implementation plan is same-wave consumer migration to generic names with no public JSON alias. If compatibility is needed temporarily, put adapters in JSON generated runtime/codegen-owned modules, not in `parse-that-regex`.

## Expected reduction

W7 should reduce the Lock 14 HIGH count by 20: 9 from parse-that-regex Phase A and 11 from passes Phase B (`restart/skinny/tranches/sk-v7/SYNTHESIS.md:151`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:152`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:162`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:163`; `restart/skinny/tranches/sk-v7/SPEC.md:320`). Relative to the 46-HIGH audit baseline, W7 should leave about 26 HIGH before W8's codegen/IR phase. Relative to already-landed W1 `-3`, cumulative Lock 14 reduction after W7 should be about `-23`.

## Compatibility and implementation risks

- `NumberSpan` already carries more facts than `JsonNumberMatch`; consumers using only `start`, `end`, and `is_integer` can migrate mechanically, but direct materializers already import `number::match_number_span_from_first` elsewhere, so duplicate number surfaces must not diverge (`skinny/crates/parse-that-regex/src/number/mod.rs:5`, `skinny/crates/codegen/src/json_typed_direct.rs:27`).
- `StringMatch` lacks stored `content_start`, `content_end`, and `needs_unescape` fields, but exposes `content_start()`, `content_end()`, and `needs_decode()` (`skinny/crates/parse-that-regex/src/lib.rs:96`, `skinny/crates/parse-that-regex/src/lib.rs:104`, `skinny/crates/parse-that-regex/src/lib.rs:109`, `skinny/crates/parse-that-regex/src/lib.rs:114`). Consumers must update field access, not recreate `JsonStringMatch` as a public compatibility shell.
- `StringMode` currently encodes JSON policy as enum variants; simply renaming return types while leaving `StrictJson`/`StrictJsonTrustedUtf8` public only partially satisfies Phase A and leaves Lock 14 type-system leakage (`skinny/crates/parse-that-regex/src/lib.rs:42`, `skinny/crates/parse-that-regex/src/lib.rs:44`, `skinny/crates/parse-that-regex/src/lib.rs:45`).
- `passes::compile()` currently does not know a grammar entry except by literal `json`; the refactor must use `GrammarIr` entry metadata or a deterministic fallback, otherwise non-JSON grammars still cannot compile (`skinny/crates/passes/src/lib.rs:26`, `skinny/crates/passes/src/lib.rs:659`).
- Deleting `shapes_for_json` without replacing `ShapeFacts` construction will break codegen lowerers expecting `Json*` shapes. W7 should install the grammar-neutral source of the same facts for JSON, then W8 can rebrand the codegen-owned public JSON template surface (`skinny/crates/passes/src/lib.rs:211`, `skinny/crates/passes/src/lib.rs:681`).
- The pre-blocked routes are unrelated to this cleanup. W7 should not use the rename as cover for new scanners, side tables, dispatch tables, capacity prescans, EventCursor, pair-token fusion, or direct-materialization redesigns (`restart/skinny/tranches/sk-v7/HANDOFF.md:71`, `restart/skinny/tranches/sk-v7/HANDOFF.md:75`, `restart/skinny/tranches/sk-v7/HANDOFF.md:84`, `restart/skinny/tranches/sk-v7/HANDOFF.md:85`, `restart/skinny/tranches/sk-v7/HANDOFF.md:86`, `restart/skinny/tranches/sk-v7/HANDOFF.md:87`, `restart/skinny/tranches/sk-v7/HANDOFF.md:88`, `restart/skinny/tranches/sk-v7/HANDOFF.md:93`).

## Test and audit commands for W7

```bash
cargo test -p parse-that-regex
cargo test -p passes
cargo run -p xtask --release -- check-json
cargo test --workspace
```

```bash
rg -n 'pub (struct|enum|type|fn).*(Json|json)|StrictJson|StrictJsonTrustedUtf8' \
  skinny/crates/parse-that-regex/src

rg -n 'shapes_for_json|nominate_json|rule_by_name\("json"\)|MissingEntry\("json"|StructuralAlphabet::json\(\)|"Json(Root|Value|Object|Array|Pair|String|Number|Bool|Null)"' \
  skinny/crates/passes/src/lib.rs
```

For no-regression evidence after the mechanical rename:

```bash
cargo bench -p bbnf-bench --bench json_parity -- 'json/(twitter|citm_catalog|instruments)/(track1_generated|track2_handcoded|sonic_rs_anchor|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct)$'
cargo run -p bbnf-bench --bin gate --release -- --advisory
```

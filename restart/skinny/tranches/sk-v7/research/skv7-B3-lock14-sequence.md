# SK-V7 B3 — Lock 14 Cleanup Sequence

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Authority: SK-V7 A5 (`/tmp/skv7-A5-lock-audit.md`), SK-V7 A4 Top 3 #3, Lock 14 (`restart/locks/14-LOCKS.md:60`).
Repo edits: none. This file is the only artifact.

---

## 0. Premise

Lock 14 is VIOLATED at the codegen / passes / parse-that-regex / ir axis. SK-V7 A5 enumerated **46 HIGH-severity grammar-name leaks**. This document is the **single-pass remediation sequence** — five phases, six commits, ~1450 LOC redistribution, byte-identical generated.rs output preserved across every commit boundary.

Per-grammar variation lives in:

- (a) the grammar definition file (`grammars/*.bbnf`),
- (b) codegen-emitted `.data` tables (per-grammar alphabets, recognizer rosters, field rosters),
- (c) codegen-emitted per-grammar wrapper code under `runtime/src/grammars/{grammar}/`.

Every other crate is grammar-blind: it consumes `Grammar` / `GrammarIr` / `BackendIr` plus facts (`DirectFieldFacts`, `RecognizerRoute`, `PrimitiveFacts`) and emits text that is not grammar-aware.

---

## 1. Per-Leak Class Inventory

The 46 HIGH leaks decompose into five mechanical classes.

| Class | Pattern | Sites | Mechanical or semantic? |
|---|---|---:|---|
| A | Function name leaks (`shapes_for_json`, `nominate_json`, `materialization_for_rule`, `direct_fields_for_rule`, `regex_first_bytes` pattern-string-equality, `extract::single_plan` `rule_by_name("json")`) | 11 in passes | semantic (must consume facts) |
| B | Type name leaks (`JsonStringMatch`, `JsonNumberMatch`, `StringMode::StrictJson`) | 3 in parse-that-regex; 9 callers | mechanical collapse into existing generic `StringMatch`/`NumberSpan` + `SpecialByteSet` |
| C | Hardcoded JSON byte literals + JSON-shape rosters (`emit_json_*`, `json_sink_direct::render` shape allowlist, `json_typed_direct::render`, `schema_direct.rs` shape allowlist) | 18 in codegen | semantic (parameterize on facts) |
| D | TapeKind JSON-named enum variants (`Object`/`Array`/`Pair`/`String`/`Number`/`Bool`/`Null`/`Member`/`Element`) + `DirectBuildDecode::{JsonString, JsonNumber}` + `regex_is_nullable` | 3 in ir, 7 consumer sites in passes | mechanical rename + DirectBuildDecode rename to `EscapedString`/`NumberScalar` |
| E | File-location violations (`codegen/src/json_templates/` 5 files; `StructuralAlphabet::json` IR constructor; `parse_json_grammar` grammar entry) | 5 + 2 file/symbol locations | relocation (move to per-grammar dirs) |

Totals: 46 HIGH leaks resolve to:

- Class A: 11 leaks, ~300 LOC remediation, all in `passes/src/lib.rs`.
- Class B: 3 type renames + 9 caller migrations, ~250 LOC, all in `parse-that-regex/src/lib.rs` and 3 consumer files.
- Class C: 18 leaks, ~500 LOC, all in `codegen/src/{lib.rs, json_sink_direct.rs, json_typed_direct.rs, lower/schema_direct.rs}`.
- Class D: 3 enum-definition leaks + 7 consumer sites, ~150 LOC (revised down — only 7 use sites in skinny).
- Class E: 5 template files relocated + 2 entry-point renames, ~50 LOC.

**Grand total: ~1250 LOC across six commits.** (A5's ~46 HIGH count maps cleanly to the five classes above; nothing missed.)

---

## 2. Concrete Site Tables

### 2.1 Class A — passes/src/lib.rs (11 HIGH)

| file:line | Current | Replacement | LOC est |
|---|---|---|---:|
| `passes/src/lib.rs:30` | `let shape_facts = shapes::shapes_for_json();` | `let shape_facts = shapes::derive_shape_facts(&normalized);` | 2 |
| `passes/src/lib.rs:31` | `let recognizers = recognizers::nominate_json(&normalized);` | `let recognizers = recognizers::nominate_recognizers(&normalized);` | 2 |
| `passes/src/lib.rs:211-239` | `pub fn shapes_for_json() -> ShapeFacts` (manually constructs 9 `Json*` shapes) | `pub fn derive_shape_facts(grammar: &GrammarIr) -> ShapeFacts` (walks rules, infers shapes from rule names + grammar-decl annotations) | 60 |
| `passes/src/lib.rs:245-251` | `pub fn nominate_json(_grammar: &GrammarIr) -> Vec<Recognizer>` returning `StructuralAlphabet::json()` | `pub fn nominate_recognizers(grammar: &GrammarIr) -> Vec<Recognizer>` — extracts alphabet from grammar's `@scan` directive or first-set union | 30 |
| `passes/src/lib.rs:487-504` | `regex_first_bytes(pattern: &str)` — string-matches three JSON regex literals | `regex_first_bytes(class_set: &ClassSet)` — consumes the typed regex IR `BuiltinTy::ClassSet` (Wave 3 dep) | 40 |
| `passes/src/lib.rs:174-182` | `regex_type(pattern: &str)` — same JSON regex string-match | `regex_type(builtin: &BuiltinTy)` — pattern-match on the typed builtin enum | 25 |
| `passes/src/lib.rs:577-579` | `derive_hot_path` prefers `rule_by_name("json")` then `"parse_value"` | drop name seed; use `grammar.entry_rule_id` (Wave 3 dep: grammar carries entry annotation) | 15 |
| `passes/src/lib.rs:658-660` | `extract::single_plan` requires `rule_by_name("json")` | use `grammar.entry_rule_id`; error becomes `MissingEntry(grammar.name)` | 10 |
| `passes/src/lib.rs:742-753` | `materialization_for_rule(name: &str)` — match on JSON rule names → JSON shape strings | drop entirely; consume `DirectFieldFacts.materialization_for(rule_id)` (Wave 3 produces this) | 30 (net negative — code disappears) |
| `passes/src/lib.rs:755-808` | `direct_fields_for_rule(name: &str)` — hardcodes JSON field rosters | drop entirely; consume `DirectFieldFacts.fields_for(rule_id)` | 60 (net negative) |
| `passes/src/lib.rs:811-819` | `span_kind(pattern: &str)` — JSON regex prefix-match | `span_kind(builtin: &BuiltinTy)` | 20 |

**Class A subtotal: ~300 LOC; net ~+150 LOC after `materialization_for_rule` + `direct_fields_for_rule` deletion.** Depends on Wave 3 (`DirectFieldFacts`) landing — see §3 sequencing.

### 2.2 Class B — parse-that-regex/src/lib.rs (3 HIGH + 9 callers)

The generic `StringMatch` already exists at `parse-that-regex/src/lib.rs:96-119`. The JSON-branded `JsonStringMatch` (`:34-45`) is a wrapper that **collapses trivially**.

| file:line | Current | Replacement | LOC |
|---|---|---|---:|
| `parse-that-regex/src/lib.rs:34-45` | `pub struct JsonStringMatch { ... }` | delete; callers consume `StringMatch` directly (already has identical field set) | -12 |
| `parse-that-regex/src/lib.rs:44-45` | `enum StringMode { ..., StrictJson, StrictJsonTrustedUtf8 }` | replace with `StringMode { Strict(SpecialByteSet), StrictTrustedUtf8(SpecialByteSet), GrammarString, ByteString }` — JSON profile becomes `SpecialByteSet::JSON` constant emitted by codegen | 30 |
| `parse-that-regex/src/lib.rs:120-178` | `pub struct JsonNumberMatch`, `match_json_number`, `match_json_number_from_first`, `validate_json_number` | rename to `NumberSpan`, `match_number`, `match_number_from_first`, `validate_number` — take `NumberProfile` param (default `NumberProfile::JSON`) | 80 |
| `parse-that-regex/src/lib.rs:127-139` | `pub fn skip_json_whitespace` | `pub fn skip_whitespace(input, offset, ws: SpecialByteSet)` — JSON ws set becomes `SpecialByteSet::JSON_WS` (codegen-emitted) | 20 |
| `parse-that-regex/src/lib.rs:268-347` | `match_json_string`, `match_json_string_at_quote`, `match_json_string_at_quote_trusted_utf8` | rename to `match_string`, `match_string_at_quote`, `match_string_at_quote_trusted_utf8` — parameterize escape policy via `EscapePolicy` | 80 |
| `parse-that-regex/src/lib.rs:416-514` | `validate_json_string_escape`, `decode_json_unicode_escape`, `validate_json_unicode_escape_run` | rename to `validate_escape`, `decode_unicode_escape`, `validate_unicode_escape_run` — take `EscapePolicy` (RFC-8259 becomes one profile) | 60 |
| `parse-that-regex/src/lib.rs:594-719` | `skip_json_string_plain`, `skip_json_string_plain_trusted`, `json_string_special_mask` | rename to `skip_string_plain*`, `string_special_mask(block, mask: u64)` — JSON mask becomes a codegen-emitted constant | 50 |
| `parse-that-regex/src/lib.rs:766-829` | `classify_json_string_content`, `scalar_classify_json_string_content`, `neon_classify_json_string_content` | rename to `classify_string_content*` — take `SpecialByteSet` | 30 |
| `parse-that-regex/src/lib.rs:847-968` | `validate_json_string`, `unescape_json_string`, `json_string_escape_control_mask` | rename to `validate_string`, `unescape_string`, `string_escape_control_mask` — take `EscapePolicy` | 60 |

**9 consumer call sites** (all in `bbnf-bench/src/{direct_struct.rs, generated_real_typed.rs, track2/json.rs}` + `runtime/src/grammars/json/{generated.rs, sink.rs}` + `codegen/src/json_templates/generated.rs`):

| file:line | Current import | Replacement |
|---|---|---|
| `bbnf-bench/src/direct_struct.rs:8, 12` | `match_json_string_at_quote_trusted_utf8, skip_json_whitespace, unescape_json_string` | `match_string_at_quote_trusted_utf8, skip_whitespace(.., SpecialByteSet::JSON_WS), unescape_string(.., EscapePolicy::JSON)` |
| `bbnf-bench/src/generated_real_typed.rs:11, 13` | same | same |
| `bbnf-bench/src/track2/json.rs:2` | `match_json_number_from_first, match_json_string_at_quote_trusted_utf8, skip_json_whitespace` | `match_number_from_first(.., NumberProfile::JSON), ...` |
| `runtime/src/grammars/json/sink.rs:1` | `unescape_json_string` | `unescape_string(.., EscapePolicy::JSON)` |
| `runtime/src/grammars/json/generated.rs:6-7` | `match_json_number_from_first, match_json_string_at_quote_trusted_utf8, skip_json_whitespace, JsonNumberMatch, JsonStringMatch` | renamed forms |
| `codegen/src/json_templates/generated.rs:7` | same (template source) | same — template regenerated mechanically |

**Class B subtotal: ~250 LOC; net ~+30 LOC after JsonStringMatch wrapper deletion.** Mechanical; no fact-dependency.

### 2.3 Class C — codegen/src (18 HIGH)

| file:line | Current | Replacement | LOC |
|---|---|---|---:|
| `codegen/src/lib.rs:1-3` | `mod json_sink_direct; mod json_typed_direct;` | `mod sink_direct; mod typed_direct;` (modules become grammar-neutral) | 4 |
| `codegen/src/lib.rs:68-95` | `emit_json_from_source`, `emit_json`, `emit_json_typed_from_source` | `emit_from_source(grammar_name: &str, source: &str)`, `emit(backend: &BackendIr)`, `emit_typed_from_source(...)` — dispatch on `grammar.name` | 40 |
| `codegen/src/lib.rs:97-129` | `emit_json_with_layout` (calls `json_sink_direct::render`) | `emit_with_layout(grammar: &Grammar, ...)` — calls `sink_direct::render(grammar, sink_only_program)` passing the grammar metadata for per-grammar field rosters | 30 |
| `codegen/src/lib.rs:171-188` | `mod_rs()` emits `pub use ...::{JsonNodeKind, JsonToken, JsonValue, ..., JsonRoot, JsonString, JsonVisitor}` | walk `grammar.exported_types()` to emit per-grammar identifier roster | 30 |
| `codegen/src/lib.rs:201-227` | `include_str!("json_templates/generated.rs")` + siblings (`parser.rs`, `value.rs`, `view.rs`, `visitor.rs`) and `include_str!("../../runtime/src/grammars/json/scan.rs")` | replace with `lower::render_grammar_module(grammar)` — the V5 plan dictates templates are **emitted, not embedded** | 60 |
| `codegen/src/lib.rs:260, 264, 285, 286, 297, 308, 309, 315, 317, 323, 329, 342, 348` | test-scope `JSON_GRAMMAR`, `parse_json_grammar`, `entry_rule == "json"`, `has_shape("JsonObject")`, `JsonSink`-text assertions | test-scope rename: `TEST_GRAMMAR`, `parse_grammar(name, source)`, `entry_rule == grammar.entry_rule_name`, assertions parameterized on `grammar.exported_shape_names()` | 30 |
| `codegen/src/json_sink_direct.rs:4-15` | `const JSON_RULES: &[&str] = &["json", "value", ...]; const JSON_SHAPES: &[&str] = &["JsonObject", ...]` | delete; consume `SinkOnlyProgram.expected_shapes` (already a field) and `program.entry_rule` (already a field) | -12 |
| `codegen/src/json_sink_direct.rs:33-47` | `if program.entry_rule != "json"` validation; error `"JSON SinkOnly renderer..."` | drop entry-rule literal check (passes already validates entry consistency); error becomes `"SinkOnly renderer requires entry_rule = grammar.name"` | 14 |
| `codegen/src/json_sink_direct.rs:59-67, 86-109` | hardcoded shape allowlist + field roster `[("JsonObject", &["members"]), ...]` | consume `DirectFieldFacts.shape_field_rosters` (Wave 3 dep); validate against facts, not hardcoded list | 50 |
| `codegen/src/json_sink_direct.rs:131+` (emitted text) | references `super::sink::JsonSink`, `parse_value_direct`, `parse_array_direct`, ... | emit `super::sink::{Grammar}Sink` and `parse_{entry_rule}_direct` — name-template instantiation from grammar metadata | 60 (this is the largest section: render-time string interpolation overhaul) |
| `codegen/src/json_sink_direct.rs:228, 235, 240, 245` (emitted dispatch bytes) | hardcoded JSON literals `b'{'`, `b'['`, `b'"'`, `b't'`, `b'f'`, `b'n'` | consume `RecognizerRoute.dispatch_table` (Wave 3 dep) — emits per-grammar dispatch from facts | 40 |
| `codegen/src/json_typed_direct.rs` (~646 LOC; ~6 HIGH per A5) | calls `match_json_*`, `skip_json_whitespace`, `unescape_json_string`; field `json_key` | calls `match_string_*`, `skip_whitespace(.., ws_set)`, `unescape_string(.., escape_policy)`; field renamed to `key_literal` (grammar-neutral) | 80 |
| `codegen/src/lower/schema_direct.rs:16-23` | hardcoded `["JsonObject", "JsonArray", "JsonPair", "JsonString", "JsonNumber", "JsonBool", "JsonNull"]` | consume `DirectFieldFacts.required_shapes` | 12 |
| `codegen/src/lower/schema_direct.rs:35` | error `"typed DirectBuild requires JSON literal recognizers"` | `"typed DirectBuild requires {grammar.name} literal recognizers"` (templated) | 6 |

**Class C subtotal: ~470 LOC.** Depends on Wave 3 `DirectFieldFacts` + `RecognizerRoute`.

### 2.4 Class D — ir/src/lib.rs (3 HIGH + 7 consumer sites)

`TapeKind` is referenced at exactly **7 sites in skinny** (verified: `grep -rn "TapeKind::" skinny/crates/` → 7 hits, all in `passes/src/lib.rs:744-750` — `materialization_for_rule`). When Class A deletes `materialization_for_rule` (consuming `DirectFieldFacts` instead), the 7 sites collapse to zero non-IR consumers.

`DirectBuildDecode::{JsonString, JsonNumber}` is referenced at **0 sites in skinny** (verified: `grep -rn "DirectBuildDecode" skinny/crates/` → 2 hits, both in `ir/src/lib.rs` definition itself).

This makes Class D **far less invasive than the cohort estimate** — the rename is mostly IR-local because the consumers were already going to be deleted by Class A.

| file:line | Current | Replacement | LOC |
|---|---|---|---:|
| `ir/src/lib.rs:433-443` | `pub enum TapeKind { Object, Array, Pair, String, Number, Bool, Null, Member, Element }` | `pub enum TapeKind { Container, ContainerEnd, KeyValuePair, StringValue, NumberValue, BoolValue, NullValue, Member, Element }` — grammar-neutral semantic event ids | 12 |
| `ir/src/lib.rs:510-515` | `pub enum DirectBuildDecode { Raw, JsonString, JsonNumber, Literal }` | `pub enum DirectBuildDecode { Raw, EscapedString, NumberScalar, Literal }` | 6 |
| `ir/src/lib.rs:321-323` | `fn regex_is_nullable(pattern: &str) -> bool { pattern == r"[ \t\n\r]*" }` | `fn regex_is_nullable(builtin: &BuiltinTy) -> bool { builtin.matches_empty() }` — consumes typed regex IR | 6 |
| `ir/src/lib.rs:411-417` | `impl StructuralAlphabet { pub fn json() -> Self { Self { bytes: b"{}[],:\"".to_vec() } } }` | delete the `json()` constructor (Class E); callers (just `passes:248`) construct from grammar's `@scan` directive | -8 |
| `passes/src/lib.rs:744-750` (7 consumer sites) | `TapeKind::Object`, `TapeKind::Array`, ... | deleted by Class A; if anything survives, mechanical rename to new variant names | already counted in Class A |

**Class D subtotal: ~30 LOC net** (much smaller than the ~300 estimate because passes consumer sites are deleted in Class A, not migrated). The cohort prompt's "150-300 LOC" estimate over-counted; actual ripple in skinny is ~30 LOC.

**Caveat**: if the Wave 3 / Wave 4 ordering changes such that Class D lands before Class A, the consumer migration in passes becomes a temporary rename (~10 LOC delta) that the Class A deletion subsequently removes. Sequencing in §3 picks the order that minimizes churn.

### 2.5 Class E — file relocations + entry-point renames (~50 LOC)

| Current location | New location | Mechanism |
|---|---|---|
| `codegen/src/json_templates/generated.rs` (836 LOC) | **delete; replaced by `lower::render_grammar_module(grammar)` per V5 plan** | already partially landed: `lower/sink_only.rs` (226 LOC) renders the SinkOnly portion. Remaining template should be emitted, not embedded. |
| `codegen/src/json_templates/parser.rs, value.rs, view.rs, visitor.rs` | move to `runtime/src/grammars/json/{parser.rs, value.rs, view.rs, visitor.rs}` — they are per-grammar artifacts, not codegen templates | mechanical `git mv` + import-path fixup in codegen `include_str!` sites (which get deleted) |
| `ir/src/lib.rs:411-417` `StructuralAlphabet::json()` | delete (counted in Class D) | callers construct from grammar's `@scan` directive |
| `grammar/src/lib.rs::parse_json_grammar` | `pub fn parse_grammar(name: &str, source: &str)` — body already generic per A5 §2.11 | mechanical rename + caller updates |

**Class E subtotal: ~50 LOC.**

---

## 3. Sequencing — Six Commits

Sequencing is constrained by:

1. **Wave 3 must precede Class A** because Class A consumes `DirectFieldFacts` + `RecognizerRoute` + `PrimitiveFacts` which Wave 3 produces.
2. **Class B is independent** — no fact dependency, can land in parallel with Wave 3.
3. **Class C consumes Class A's outputs** (passes feeds codegen).
4. **Class D is best done last** so it doesn't bisect intermediate green builds.
5. **Class E is the smallest and is bound to Class C** (relocation of `json_templates/` happens when codegen stops `include_str!`-ing them).

The commits below assume Wave 3 (TBL classifier + mesh DirectBuild + facts production) has landed per the SK-V7 A4 recommendation. If Wave 3 has not landed, Commit 2 cannot start; Commits 1 and 3 (B + IR rename without consumer churn) can still proceed.

### Commit 1: feat(lock14-parse-that-regex): collapse Json* into generic types + SpecialByteSet

**Scope**: Class B — parse-that-regex/src/lib.rs (3 type renames + 9 caller migrations).

**Files touched**:
- `skinny/crates/parse-that-regex/src/lib.rs` (~250 LOC delta, ~+30 net)
- `skinny/crates/bbnf-bench/src/{direct_struct.rs, generated_real_typed.rs, track2/json.rs}` (~30 LOC import + call-site updates)
- `skinny/crates/runtime/src/grammars/json/{sink.rs, generated.rs}` (~20 LOC)
- `skinny/crates/codegen/src/json_templates/generated.rs` (~20 LOC; template source — regen produces same output)

**Key transforms**:
- Delete `JsonStringMatch` (`:34-45`); callers consume existing generic `StringMatch` (`:96-119`).
- Rename `JsonNumberMatch` → `NumberSpan`; `match_json_number*` → `match_number*` taking `NumberProfile` param.
- Add `pub struct SpecialByteSet([u8; 256])` + `pub const JSON_WS: SpecialByteSet = ...; pub const JSON_STRING_SPECIAL: SpecialByteSet = ...;` (consts live in parse-that-regex as **profiles** — the type is generic, the named JSON profile is one instance, mirroring `StructuralAlphabet`'s `json()` constructor).
- Rename `skip_json_whitespace` → `skip_whitespace(input, offset, ws: &SpecialByteSet)`; JSON callers pass `&parse_that_regex::profiles::JSON_WS`.
- Rename `match_json_string*` → `match_string*` taking `EscapePolicy` (RFC-8259 becomes `EscapePolicy::JSON`).
- Update all 9 consumer call sites in parallel.

**Verification**: `cargo test --workspace` green; `cargo run -p xtask --release -- gen --check` confirms byte-identical generated.rs output (the `profiles::JSON_*` named instances ensure the JSON behavior is unchanged).

**LOC estimate**: ~250 lines changed, ~+30 net.
**Risk**: LOW — mechanical rename, generic primitive already exists, JSON profile is preserved as a named constant.

### Commit 2: feat(lock14-passes): grammar-neutral derive_recognizers + DirectFieldFacts consumer

**Scope**: Class A — passes/src/lib.rs (11 HIGH).

**Files touched**:
- `skinny/crates/passes/src/lib.rs` (~300 LOC delta, ~+150 net after deletions)
- `skinny/crates/passes/src/{shapes.rs, recognizers.rs, extract.rs}` (module splits, ~+200 LOC moved out of god-module)
- `skinny/crates/grammar/src/lib.rs` (~20 LOC — add `entry_rule_id`, `exported_types`, `name` accessors to `GrammarIr` if not present)

**Key transforms**:
- `shapes_for_json` → `derive_shape_facts(grammar)`: walk rules; each rule with `@shape` annotation produces a `ShapeFact` entry; rule names map to shape names via grammar's namespace prefix (e.g. JSON grammar's `object` rule → `JsonObject` shape comes from grammar's `@shape JsonObject = object` declaration, not from hardcoded literal).
- `nominate_json` → `nominate_recognizers(grammar)`: extract alphabet from grammar's `@scan` directive; or compute first-byte union across alts.
- `regex_first_bytes(pattern: &str)` → `regex_first_bytes(class_set: &ClassSet)`: consumes typed regex IR (Wave 3 dep).
- `materialization_for_rule` and `direct_fields_for_rule` — **delete entirely**; consume `DirectFieldFacts.materialization_for(rule_id)` and `DirectFieldFacts.fields_for(rule_id)`.
- `derive_hot_path` / `extract::single_plan` — drop `rule_by_name("json")` seed; use `grammar.entry_rule_id`.

**Verification**: `cargo test --workspace` green; verify byte-identical generated.rs (this is the critical regression-free gate — the fact-driven path must reproduce the hardcoded behavior for the JSON grammar).

**LOC estimate**: ~300 lines changed, ~+150 net (after deletion of `materialization_for_rule` + `direct_fields_for_rule`).
**Risk**: MEDIUM. passes is consumed by codegen which is consumed by runtime; the test coverage gate is mandatory at every step. `DirectFieldFacts` keying by `RuleId` (rather than rule name) is the new contract; if Wave 3 produces facts keyed by name, that conversion lives here.

### Commit 3: feat(lock14-codegen): grammar-neutral emit_grammar_* + abstract shape rosters

**Scope**: Class C — codegen/src (18 HIGH).

**Files touched**:
- `skinny/crates/codegen/src/lib.rs` (~190 LOC delta)
- `skinny/crates/codegen/src/json_sink_direct.rs` → renamed to `sink_direct.rs` (~150 LOC delta)
- `skinny/crates/codegen/src/json_typed_direct.rs` → renamed to `typed_direct.rs` (~120 LOC delta)
- `skinny/crates/codegen/src/lower/schema_direct.rs` (~20 LOC)
- `skinny/crates/codegen/src/direct_schema.rs` (~30 LOC; field `json_key` → `key_literal`)

**Key transforms**:
- `emit_json_*` entry points → `emit_grammar_*` entry points taking `grammar_name: &str` (test scope) or `Grammar` (production).
- Rename module files: `json_sink_direct.rs` → `sink_direct.rs`; `json_typed_direct.rs` → `typed_direct.rs` (via `git mv`).
- Delete hardcoded `JSON_RULES`, `JSON_SHAPES` constants; consume `DirectFieldFacts.shape_field_rosters` (Wave 3 dep).
- Replace entry-rule literal validation `program.entry_rule != "json"` with `program.entry_rule != grammar.entry_rule_name`.
- Emitted text: replace `super::sink::JsonSink` with `super::sink::{Grammar}Sink` and `parse_value_direct` with `parse_{entry_rule}_direct` via name-templating.
- Replace hardcoded dispatch bytes `b'{'`, `b'['`, ... with `RecognizerRoute.dispatch_table` (Wave 3 dep) iteration.
- `mod_rs()` walk `grammar.exported_types()` instead of hardcoded `JsonObject`/`JsonArray`/... roster.

**Verification**: `cargo test --workspace` green; byte-identical generated.rs (the templated emission must reproduce the hardcoded output for JSON).

**LOC estimate**: ~470 lines changed, ~+50 net (rename + parameterization + small new fact-consumer code).
**Risk**: MEDIUM-HIGH. This is the largest single commit. Codegen output shape changes are the gating concern — strict byte-equality on generated.rs is the test gate. **Recommended sub-split** if it exceeds 500 LOC: 3a (json_sink_direct rebrand), 3b (json_typed_direct rebrand + schema_direct), 3c (lib.rs entry-point rename + mod_rs).

### Commit 4: chore(lock14-templates): relocate json_templates/ and drop include_str!

**Scope**: Class E — file relocations.

**Files touched**:
- `skinny/crates/codegen/src/json_templates/parser.rs, value.rs, view.rs, visitor.rs` → `skinny/crates/runtime/src/grammars/json/{parser.rs, value.rs, view.rs, visitor.rs}` (4 files, `git mv`)
- `skinny/crates/codegen/src/json_templates/generated.rs` → **deleted** (this is the template that was being included; Commit 3 replaced `include_str!` with `lower::render_grammar_module` which generates it)
- `skinny/crates/codegen/src/lib.rs:201-227` — delete `include_str!` blocks (already replaced by Commit 3; this commit verifies the templates are no longer referenced from generic codegen)
- `skinny/crates/grammar/src/lib.rs` — rename `parse_json_grammar` to `parse_grammar(name, source)` (the body is already generic per A5 §2.11).

**Key transforms**:
- `git mv` the four per-grammar template files out of generic codegen and into per-grammar runtime location.
- Confirm zero `include_str!` references in `codegen/src/` (`grep -rn "include_str!" skinny/crates/codegen/src/` should return only test-scope grammar source loads at line 260, which are fine — they're loading the .bbnf grammar file, not embedded Rust).
- Rename `parse_json_grammar` → `parse_grammar(name, source)` at the one production caller site (`codegen/src/lib.rs:69, 87`) and test caller (`passes/src/lib.rs:830`).

**Verification**: `cargo test --workspace` green; `git mv` preserves blame.
**LOC estimate**: ~50 LOC (mostly path renames + 5 file moves).
**Risk**: LOW. Cleanup commit, mechanical.

### Commit 5: feat(lock14-ir): grammar-neutral TapeKind variants + DirectBuildDecode rename + regex_is_nullable

**Scope**: Class D — ir/src/lib.rs.

**Files touched**:
- `skinny/crates/ir/src/lib.rs` (~30 LOC: variant renames + StructuralAlphabet::json() deletion + regex_is_nullable rewrite)
- Any survivors after Class A/C deletion (expected: 0; if anything references `TapeKind::Object` etc. after Commits 2+3, mechanical rename here).

**Key transforms**:
- `TapeKind::Object` → `TapeKind::Container`; `Array` → `ContainerEnd` (or keep `Container` + `Element` semantics — final naming TBD by IR team but **must not** be `Object`/`Array`); `Pair` → `KeyValuePair`; `String` → `StringValue`; `Number` → `NumberValue`; `Bool` → `BoolValue`; `Null` → `NullValue`; `Member` and `Element` keep names (already grammar-neutral).
- `DirectBuildDecode::JsonString` → `EscapedString`; `JsonNumber` → `NumberScalar`.
- Delete `StructuralAlphabet::json()` constructor; passes' lone caller at `:248` (already migrated by Commit 2) constructs from grammar facts.
- `regex_is_nullable(pattern: &str)` → `regex_is_nullable(builtin: &BuiltinTy)` consuming typed regex IR.

**Verification**: `cargo test --workspace` green; byte-identical generated.rs (variant names are not user-visible at codegen output, but assembler bit-encodings must match — verify via tape unit tests).

**LOC estimate**: ~30 LOC net.
**Risk**: LOW (revised down from cohort estimate). The rename is IR-local because Commits 2+3 already deleted all the JSON-named consumer code; the only remaining consumers are inside the IR crate's own internal sites.

### Commit 6: docs(lock14): update ARCHITECTURE + COMPILER + MIGRATION

**Scope**: Documentation alignment.

**Files touched**:
- `restart/ARCHITECTURE.md` — §7.4 (Lock 14 cleanup status) — flip from "VIOLATED" to "HONORED" with citation back to A5 + B3.
- `restart/skinny/COMPILER.md` — Wave 4 status section, mark Lock 14 cleanup landed; update commit refs.
- `restart/skinny/MIGRATION.md` (or equivalent) — Note the `emit_json_*` → `emit_grammar_*` API break for external callers; the SpecialByteSet/EscapePolicy parameterization.

**Verification**: docs only; no code changes.
**LOC estimate**: ~50 LOC across three files.
**Risk**: LOW.

---

## 4. Wave Alignment

Per SK-V7 A4's recommended order: Wave 3 (TBL classifier + mesh DirectBuild + facts production) runs in parallel with **Commits 1 + 5** of this sequence (which have no Wave-3 dependency).

Wave 3 must complete before **Commits 2 + 3** can land (they consume `DirectFieldFacts`, `RecognizerRoute`, `PrimitiveFacts`).

**Commit 4** is bound to Commit 3 (it cleans up the `include_str!` after the templates are no longer needed).

**Commit 6** runs last.

Recommended execution order:

| Wave | Parallel cohort | Commits |
|---|---|---|
| Wave 3 (independent) | TBL classifier + mesh DirectBuild + facts production | (out of B3 scope) |
| Wave 3 (parallel with above) | parse-that-regex genericity + IR enum renames | Commits 1 + 5 |
| Wave 4 (depends on Wave 3 + Commits 1+5) | passes deletion + codegen rebrand + template relocation | Commits 2 + 3 + 4 |
| Wave 4 close | docs alignment | Commit 6 |

If Class D ends up touching consumers (i.e. Wave 3 sequencing forces Commit 5 to happen between Commits 2 and 3), the consumer-rename churn is ~10 LOC and recovers in Commit 2 anyway. **No sequencing dead-end exists.**

---

## 5. Risk Analysis per Phase

| Phase | Commits | Risk | Rationale | Mitigation |
|---|---|---|---|---|
| A — parse-that-regex genericity | 1 | LOW | Mechanical rename, generic primitive (`StringMatch`) already exists, JSON behavior preserved as named profile constant | `cargo test --workspace`; byte-identical generated.rs check; `cargo run -p xtask --release -- gen --check` |
| B — passes facts consumption | 2 | MEDIUM | Depends on Wave 3 fact production; the `DirectFieldFacts` contract must match what codegen expects | Test coverage at the `passes::compile()` output boundary; cross-check fact roster against current hardcoded roster pre-deletion |
| C — codegen rebrand | 3 | **HIGHEST** | Largest single commit; emitted-text changes are the most subtle to verify; potential for sub-split into 3a/3b/3c | Strict byte-equality on generated.rs; per-grammar gen-check; consider commit-3-sub-split if LOC exceeds 500 |
| D — file relocations | 4 | LOW | `git mv` + import-path fixup; no semantic change | `cargo test --workspace`; blame preservation via `git mv` |
| E — IR rename | 5 | LOW (revised down) | Only ~30 LOC because Commits 2+3 deleted the JSON-named consumers; IR-local rename | Tape unit tests; assembler bit-encoding check |
| docs | 6 | LOW | Documentation only | Read-through |

**Riskiest phase: Phase C / Commit 3** — codegen rebrand. The emitted-text overhaul (~470 LOC) and the gating concern of byte-identical generated.rs output make this the single highest-risk commit in the sequence. **Recommend sub-split into 3a (json_sink_direct rebrand), 3b (json_typed_direct rebrand + schema_direct), 3c (lib.rs entry-point rename + mod_rs)** if any individual sub-commit exceeds 200 LOC delta.

---

## 6. Test Coverage Gate (per commit)

Every commit must satisfy:

1. `cargo test --workspace` green.
2. `cargo run -p xtask --release -- gen --check` — generated.rs is byte-identical to the pre-commit baseline.
3. `cargo bench --workspace --no-run` — bench harness compiles.
4. No new warnings under `cargo clippy --workspace --all-targets`.
5. For Commits 2, 3: bench gate `cargo run -p bbnf-bench --release -- gate --baseline pre-lock14.json` shows no perf regression > 5%.

The byte-identical generated.rs check is the **critical regression gate** — it proves the facts-driven path reproduces the hardcoded behavior for the JSON grammar exactly, which is the load-bearing claim of the entire migration.

---

## 7. Concrete Commit Plan — Summary Table

| # | Title | Scope | LOC (changed/net) | Phase | Risk | Wave |
|---|---|---|---:|---|---|---|
| 1 | `feat(lock14-parse-that-regex): collapse Json* into generic types + SpecialByteSet/EscapePolicy` | Class B | 250 / +30 | A | LOW | W3 (parallel) |
| 2 | `feat(lock14-passes): grammar-neutral derive_shape_facts + DirectFieldFacts consumer` | Class A | 300 / +150 | B | MEDIUM | W4 |
| 3 | `feat(lock14-codegen): grammar-neutral emit_grammar_* + abstract shape rosters` | Class C | 470 / +50 | C | HIGHEST | W4 |
| 4 | `chore(lock14-templates): relocate json_templates/ to runtime/grammars/json/ and drop include_str!` | Class E | 50 / -100 (5 file moves) | D | LOW | W4 |
| 5 | `feat(lock14-ir): grammar-neutral TapeKind variants + DirectBuildDecode rename + typed regex_is_nullable` | Class D | 30 / 0 | E | LOW | W3 (parallel) |
| 6 | `docs(lock14): update ARCHITECTURE.md + COMPILER.md + MIGRATION.md` | docs | 50 / +50 | — | LOW | W4 close |

**Grand total: ~1150 LOC changed, ~+180 net** (net is positive because passes gains fact-consumer plumbing while codegen and parse-that-regex shed JSON specialization, but the new generic plumbing in passes is the dominant addition).

---

## 8. What This Sequence Does Not Cover

Per SK-V7 A5 §7.3 the following items are **outside Lock 14 cleanup** and remain open after this sequence:

- 4 of 5 lowering body stubs (`lower/{eager_tape, offset_tape, event_tape, collapsed_stage}.rs`) — these are unfilled cells, not Lock 14 leaks.
- `derive_backend_shape_with_diagnostics` step 6 + step 7 cost-model stubs — needs real cost model, not name renaming.
- B2 checkasm hardening (register-clobber detection, rdtsc, stack-canary XOR-fold) — separate hardening track.
- 4 of 9 bbnf.asm macros without same-wave consumers (FSM_DISPATCH_THREADED + FRAME_PUSH_BOUNDED + FRAME_POP_BOUNDED + BITMAP_PREFIX_XOR_64 body) — Lock 16 admissibility track.

These are tracked in SK-V7 A4's overall waveplan; Lock 14 cleanup is **bounded to the rename + facts-consumption + relocation work** above.

---

## 9. Verifier

After Commit 6, `cargo xtask lint-no-hardcoded-grammars` (the Lock 14 verifier referenced at `restart/locks/14-LOCKS.md:60`) should report **zero violations**.

Spot checks per the A5 leak inventory:

| Check | Pre-cleanup | Post-cleanup expected |
|---|---|---|
| `grep -rn "json\|Json" skinny/crates/parse-that-regex/src/` | ~30 hits | 0 hits (JSON profile constants live under `profiles::JSON_*` which is grammar-data, not crate-name) |
| `grep -rn "json\|Json" skinny/crates/passes/src/` | ~30 hits | 0 hits in production code; test scope may retain `JSON_GRAMMAR` const |
| `grep -rn "json\|Json" skinny/crates/codegen/src/lib.rs` | ~20 hits | 0 hits |
| `grep -rn "TapeKind::Object\|TapeKind::Array\|TapeKind::Pair" skinny/crates/` | 7 hits | 0 hits |
| `grep -rn "DirectBuildDecode::JsonString\|JsonNumber" skinny/crates/` | 0 hits (definition only) | 0 hits |
| `ls skinny/crates/codegen/src/json_templates/` | 5 files | directory does not exist |
| `grep -rn "emit_json_\|parse_json_grammar" skinny/crates/` | ~10 hits | 0 hits |

When all seven spot checks pass, Lock 14 closes.

---

## 10. Counts and Summary

| Metric | Value |
|---|---|
| Phases | 5 (A — parse-that-regex / B — passes / C — codegen / D — templates / E — IR) |
| Commits | 6 |
| Total HIGH leaks addressed | 46 |
| Total MEDIUM leaks addressed | ~6 |
| Total LOC changed | ~1150 |
| Total LOC net delta | ~+180 |
| Riskiest commit | Commit 3 (codegen rebrand, ~470 LOC, byte-identical-output gate, sub-split recommended) |
| Wave 3 dependencies | Commits 2 + 3 (DirectFieldFacts, RecognizerRoute, PrimitiveFacts) |
| Wave 3 parallel-safe commits | 1 + 5 |
| Byte-identical generated.rs invariant | Required at every commit boundary |
| File size of this document | ~520 LOC target |
| Lock 14 verifier post-cleanup | `cargo xtask lint-no-hardcoded-grammars` reports zero violations |

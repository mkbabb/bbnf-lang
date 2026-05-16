# Wave 8 R1: codegen public API and module rebrand

Date: 2026-05-16

Scope: research-only inventory for SK-V7 W8 Phase C on the codegen public API and module rebrand. No source edits in this subtask.

## Inputs read

- `restart/skinny/tranches/sk-v7/SPEC.md:325-338`: W8 owner paths are `codegen/src/lib.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, `lower/schema_direct.rs`, plus IR residue; Phase C replaces `emit_json_*` with `emit_grammar_*`, collapses hardcoded JSON rosters into grammar-derived facts, and gates on byte-identical `generated.rs`.
- `restart/skinny/tranches/sk-v7/research/skv7-A5-lock-audit.md:212-226`: current `codegen/src/lib.rs` Lock 14 inventory.
- `restart/skinny/tranches/sk-v7/research/skv7-B3-lock14-sequence.md:97-116`: Class C replacement table and dependency on Wave 3 `DirectFieldFacts` + `RecognizerRoute`.
- `skinny/crates/codegen/src/lib.rs`: current public entry points, module declarations, template includes, and tests.

## Current leak inventory

Primary codegen API and module leaks:

| File/line | Current leak | Severity | Note |
|---|---|---|---|
| `skinny/crates/codegen/src/lib.rs:2-3` | `mod json_sink_direct; mod json_typed_direct;` | MEDIUM | Generic crate has JSON-branded private module names. |
| `skinny/crates/codegen/src/lib.rs:68-75` | `pub fn emit_json_from_source(source: &str)` and `grammar::parse_json_grammar(source)` | HIGH | Public API is JSON-branded and source compile path hardcodes JSON parser entry. |
| `skinny/crates/codegen/src/lib.rs:78-80` | `pub fn emit_json(backend: &BackendIr)` | HIGH | Public BackendIr emission API is JSON-branded despite taking generic backend IR. |
| `skinny/crates/codegen/src/lib.rs:83-95` | `pub fn emit_json_typed_from_source(...)` and `grammar::parse_json_grammar(source)` | HIGH | Typed DirectBuild public API is JSON-branded. |
| `skinny/crates/codegen/src/lib.rs:97-129` | `emit_json_with_layout` plus `json_sink_direct::render(sink_only)` at `:117` | HIGH | Only sink direct renderer wired from `lib.rs` is JSON-branded. |
| `skinny/crates/codegen/src/lib.rs:131-156` | `emit_json_typed_with_layout` plus `json_typed_direct::render(&typed)` at `:154` | HIGH | Typed renderer path is JSON-branded. |
| `skinny/crates/codegen/src/lib.rs:168-190` | `mod_rs()` emits `JsonSink`, `JsonNodeKind`, `JsonToken`, `JsonValue`, `JsonArray`, `JsonBool`, `JsonDocument`, `JsonNull`, `JsonNumber`, `JsonObject`, `JsonPair`, `JsonRoot`, `JsonString`, `JsonVisitor` | HIGH | Generic codegen emits a JSON runtime module export roster directly. |
| `skinny/crates/codegen/src/lib.rs:193-199` | Host template comment says JSON is host-fn-free | LOW | Generated comment leak; easy to neutralize once module emission is grammar-aware. |
| `skinny/crates/codegen/src/lib.rs:201-227` | `include_str!("json_templates/...")` and `include_str!("../../runtime/src/grammars/json/{scan,sink}.rs")` | HIGH | Generic codegen embeds per-JSON runtime/template files. This is Class E-adjacent but called from the API layer. |
| `skinny/crates/codegen/src/lib.rs:251-435` | Test-only `JSON_GRAMMAR`, `emit_json_*`, `parse_json_grammar`, `entry_rule == "json"`, `Json*` assertions, and `json_key` fixtures | LOW to MEDIUM | Test scope, but the assertions currently lock in JSON-branded implementation details. |

Adjacent Class C leaks that constrain the public API rename:

| File/line | Current leak | Severity | Note |
|---|---|---|---|
| `skinny/crates/codegen/src/json_sink_direct.rs:5-15` | `JSON_RULES` / `JSON_SHAPES` rosters | HIGH | Hardcoded JSON grammar roster. |
| `skinny/crates/codegen/src/json_sink_direct.rs:33-35` | Entry rule must equal `"json"` and error says `JSON SinkOnly renderer` | HIGH | Blocks grammar-neutral sink renderer API. |
| `skinny/crates/codegen/src/json_sink_direct.rs:87-93` | Hardcoded shape-field roster for `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull` | HIGH | Must be fact-driven before a true generic renderer can exist. |
| `skinny/crates/codegen/src/json_sink_direct.rs:131-149` | Emitted code imports and bounds `super::sink::JsonSink` | HIGH | Renderer name can be rebranded mechanically, but emitted sink trait name requires grammar metadata or a compatibility shim. |
| `skinny/crates/codegen/src/json_typed_direct.rs:267,296` | Typed renderer consumes `field.json_key` | MEDIUM | Direct schema field name is JSON-branded; B3 lists rename to `key_literal`. |
| `skinny/crates/codegen/src/lower/schema_direct.rs:17-23` | Hardcoded JSON shape allowlist | HIGH | Typed lowering must consume `DirectFieldFacts.required_shapes` or equivalent facts. |
| `skinny/crates/codegen/src/direct_schema.rs:36,45,167,175,184,187,192,195,198` | Public schema fields and validation use `json_key` / `json_keys` | MEDIUM | Public typed schema API leak; not named in SPEC owner list, but B3 Commit 3 includes it because `json_typed_direct.rs` depends on it. |
| `skinny/crates/bbnf/src/lib.rs:57-58` | `compile_json_source` delegates to `codegen::emit_json_from_source` | Downstream caller | Must be updated or compatibility-exported when codegen API changes. |
| `skinny/xtask/src/main.rs:120-150` | `regen-json`, `check-json`, `regen-real-typed`, `check-real-typed` call `emit_json_*` | Downstream caller | Regeneration/check gates must move with the API rename. |

## Recommended minimal W8 intervention

Use a compatibility-first rebrand in three slices. This keeps W8 aligned with SPEC while minimizing behavioral drift.

1. Rebrand module files and internal calls first:
   - `json_sink_direct.rs` -> `sink_direct.rs`
   - `json_typed_direct.rs` -> `typed_direct.rs`
   - `mod json_sink_direct; mod json_typed_direct;` -> `mod sink_direct; mod typed_direct;`
   - Change `json_sink_direct::render` / `json_typed_direct::render` call sites to the new module names.
   - Do not change emitted text in this slice except unavoidable module path changes. Expected generated output should remain byte-identical.

2. Add grammar-neutral public API names while retaining deprecated JSON aliases for one transition commit:
   - Add `emit_from_source(grammar_name: &str, source: &str)`.
   - Add `emit(backend: &BackendIr)`.
   - Add `emit_typed_from_source(grammar_name: &str, source: &str, schema: &DirectSchemaSet)`.
   - Add internal `emit_with_layout(...)` / `emit_typed_with_layout(...)`.
   - Keep `emit_json_from_source`, `emit_json`, and `emit_json_typed_from_source` as thin wrappers initially so `bbnf` and `xtask` can be migrated without a large atomic break. The wrappers should call the new API with `"json"`.

3. Move callers to the new names, then delete the wrappers only after grep proves no production callers remain:
   - `skinny/crates/bbnf/src/lib.rs:57-58`
   - `skinny/xtask/src/main.rs:120-150`
   - `skinny/crates/codegen/src/lib.rs` tests
   - Any remaining `rg -n "emit_json|json_sink_direct|json_typed_direct" skinny/crates -g '*.rs'` hits.

What not to do in R1/W8-minimal:

- Do not attempt full template relocation in the same patch as the public API rename unless the slice remains under the SPEC sub-split cap. SPEC §10 says sub-split if any sub-commit exceeds 200 LOC.
- Do not delete JSON emitted identifiers from `mod_rs()` until grammar-derived exported-type metadata exists. For JSON output, the generated module still legitimately exports `Json*` names; the Lock 14 issue is that generic codegen hardcodes that roster.
- Do not make `emit_from_source` pretend to support arbitrary grammars unless grammar parsing and exported-type facts are actually wired. If only JSON is supported in the transition, fail non-`"json"` names explicitly with a clear `CodegenError::Lowering` message.

## Byte-identical/generated gate

SPEC §10 names `cargo run -p xtask --release -- gen --check`, but the current `xtask` does not expose that command. `skinny/xtask/src/main.rs:7-30` exposes `check-json` and `check-real-typed`, and `skinny/REDRESS.md:1571-1574` records `gen --check` as stale. For current workspace W8, the runnable generated gates are:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-real-typed
git diff -- crates/runtime/src/grammars/json/generated.rs crates/bbnf-bench/src/generated_real_typed.rs
```

The intended invariant is:

- `check-json` succeeds without rewriting `crates/runtime/src/grammars/json/generated.rs`.
- `check-real-typed` succeeds without rewriting `crates/bbnf-bench/src/generated_real_typed.rs`.
- `git diff -- ...generated...` is empty after checks.
- If `xtask gen --check` is added before W8 implementation, it should be equivalent to running both current check commands above.

For a public-API/module-only sub-slice, generated files should be byte-identical. If generated bytes change, reject the slice unless the diff is mechanically explained and explicitly authorized by the W8 owner.

## Risks

- Highest risk: changing renderer inputs before facts are ready. B3 Class C depends on Wave 3 `DirectFieldFacts` + `RecognizerRoute`; deleting hardcoded rosters too early can create a generic-looking API that still smuggles JSON assumptions elsewhere.
- API break risk: `codegen::emit_json_from_source` is consumed by `bbnf` and `xtask`. A wrapper transition avoids a wide break while preserving a clean final grep gate.
- False genericity risk: `emit_from_source(grammar_name, source)` should not silently parse all input as JSON. Non-JSON names should fail until `grammar::parse_grammar(name, source)` and grammar-derived exported metadata are real.
- Generated output risk: `mod_rs()`, sink direct rendering, and typed direct rendering can alter checked-in generated files through whitespace, ordering, or import roster changes. Byte equality is the critical gate.
- Scope risk: `direct_schema.rs` is outside the SPEC §10 owner-path bullet, but `json_typed_direct.rs` uses `field.json_key`; fully removing typed-direct JSON naming will require either including `direct_schema.rs` in the implementation scope or leaving a documented residue.

## Exact grep/test commands

Inventory before implementation:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
rg -n "json_sink_direct|json_typed_direct|emit_json|parse_json_grammar|json_templates|JsonSink|Json(Value|Token|NodeKind|Visitor|Root|Object|Array|Pair|String|Number|Bool|Null)|JSON_GRAMMAR|json_key" skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_sink_direct.rs skinny/crates/codegen/src/json_typed_direct.rs skinny/crates/codegen/src/lower/schema_direct.rs skinny/crates/codegen/src/direct_schema.rs
rg -n "emit_json|json_sink_direct|json_typed_direct" skinny/crates -g '*.rs'
rg -n "include_str!\\(\"json_templates|runtime/src/grammars/json" skinny/crates/codegen/src -g '*.rs'
```

Post-slice grep gates for the API/module rebrand:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
rg -n "json_sink_direct|json_typed_direct" skinny/crates/codegen/src skinny/crates -g '*.rs'
rg -n "emit_json" skinny/crates -g '*.rs'
rg -n "pub fn emit_(from_source|typed_from_source)|fn emit_with_layout|fn emit_typed_with_layout" skinny/crates/codegen/src/lib.rs
```

Expected after final W8 API cleanup: first two commands return no production hits. If compatibility wrappers are intentionally retained for an intermediate commit, `rg -n "emit_json" ...` may return only the wrapper definitions plus tests, and the commit message/research follow-up should say so explicitly.

Build and generated checks:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo fmt --all --check
cargo test -p codegen
cargo test -p bbnf
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-real-typed
cargo test --workspace
git diff -- crates/runtime/src/grammars/json/generated.rs crates/bbnf-bench/src/generated_real_typed.rs
```

Optional SPEC-alignment check if `xtask gen --check` is implemented before the W8 patch lands:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- gen --check
```

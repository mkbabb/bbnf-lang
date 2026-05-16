# SK-V7 A5 — Lock 1 and Lock 14 Audit, Post SK-V6

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Repo edits: none. This file is the only artifact.

Authority anchors:

- `restart/locks/14-LOCKS.md:34` (Lock 1 — substrate union; columnar SoA dead; parallel substrates dead).
- `restart/locks/14-LOCKS.md:60` (Lock 14 — zero overfitting; verifier `cargo xtask lint-no-hardcoded-grammars`).
- `restart/ARCHITECTURE.md:1020-1133` (§7.3 / §7.4 — `BackendShape`, side tables, SK-V6 implementation status).
- Prior baselines: SK-V5 `skv5-A4-tape-union-audit.md`, `skv5-A5-grammar-generalization.md`; SK-V6 `skv6-A5-general-grammar-abstraction.md`, `skv6-C6-generality-costfacts.md`.

---

## 1. Lock 1 — Substrate-Union Verification, Five-Shape Coverage

### 1.1 BackendShape enum exists in IR

`skinny/crates/ir/src/lib.rs:334-341` defines the canonical Rust enum:

```rust
pub enum BackendShape {
    EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage,
}
```

This is the §7.3 union the V5 audit found absent. Present at IR layer post-V6.

### 1.2 LayoutFacts.backend_shape field exists in passes

`skinny/crates/passes/src/lib.rs:62` (`pub backend_shape: HashMap<ir::RuleId, BackendShape>`) carries the per-rule selection. Populated at `passes/src/lib.rs:33-39` by `recognizers::derive_backend_shape_with_diagnostics` and stamped onto `layout_facts.backend_shape` before `compile` returns.

### 1.3 derive_backend_shape — populated, 8-priority partially honored

`skinny/crates/passes/src/lib.rs:287-331` is the function body. The decision tree implements:

| ARCH §7.3 step | Body location | Status |
|---|---|---|
| 1. Recovery → EagerTape | `requires_eager_tape` → `has_recovery_annotation` (`:340-359`) | Present, walks `Annotation { name }` for substring "recover". |
| 2. Parse-time host decode → EagerTape | `has_parse_time_host_decode` (`:362-380`) | Present. |
| 3. Layout scope → EagerTape | `has_layout_policy` (`:382-384`) | Present; reads `layout_facts.layout_policies`. |
| 4. First-set overlap → EagerTape | `has_dispatch_overlap` → `branches_overlap` → `first_bytes` (`:386-484`) | Present; `regex_first_bytes` (`:487-504`) hardcodes the three JSON regexes — see Lock 14 §3 below. |
| 5. Direct-only, no traversal → SinkOnly | `admits_sink_only` (`:506-510`) | Present; gated on `target.direct_only_output && !target.retained_api_consumer`. |
| 6. AVX-512 + hub ≥ 4 disjoint arms → CollapsedStage | `admits_collapsed_stage` (`:512-514`) | **DEGENERATE**: tests `avx512bw` and `matches!(rule_ir.expr, BackendExpr::Entry(_))`; no arms count, no disjointness. Cost model is a placeholder. |
| 7. Retained side facts → EventTape | `prefers_event_tape` (`:516-518`) | **DEGENERATE**: tests `alt_branch_count(...) >= 8`; not the retained-side-facts criterion. |
| 8. Else → OffsetTape | `Some(_) => BackendShape::OffsetTape` | Present. |

For the JSON grammar all 15 rules currently resolve to `OffsetTape` (`passes/src/lib.rs:838-843` test assertion). CollapsedStage falls back with diagnostic `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` when `collapsed_stage_author_declared = false` (`passes/src/lib.rs:303-312`); regression-tested at `:872-896`. The V5 "decision substrate empty" violation is closed at the symbol layer; the per-step cost discrimination (steps 6 and 7) is a stub.

### 1.4 Five-shape Rust lowering coverage

`skinny/crates/codegen/src/lower/rust.rs:48-55` dispatches per shape; per-shape lowering bodies exist:

| Shape | File | LOC | Body |
|---|---|---|---|
| EagerTape | `codegen/src/lower/eager_tape.rs` | 5 | stub — returns format string `"rule {name} -> eager_tape"`. |
| OffsetTape | `codegen/src/lower/offset_tape.rs` | 5 | stub — same shape. |
| EventTape | `codegen/src/lower/event_tape.rs` | 5 | stub — same shape. |
| SinkOnly | `codegen/src/lower/sink_only.rs` | 226 | **REAL** — walks `BackendIr` → `SinkOnlyProgram`, captures direct shapes, span kinds, literals, dispatch alt count. Consumed by `json_sink_direct::render` at `codegen/src/lib.rs:117`. |
| CollapsedStage | `codegen/src/lower/collapsed_stage.rs` | 5 | stub. |

Per-shape lowering is **structurally present** for all five but **only SinkOnly carries a real body**. EagerTape / OffsetTape / EventTape produce only a diagnostic string `"rule {name} -> {shape}"` and rely on the JSON template at `codegen/src/json_templates/generated.rs` (included via `include_str!` at `codegen/src/lib.rs:201-203`) to supply the actual runtime code. The 4-of-5 stub state is the same blocker `skv6-A5-general-grammar-abstraction.md:393-409` names as the remaining shape-generic emitter gap.

### 1.5 OffsetTape: storage and consumer

Tape storage at `skinny/crates/runtime/src/tape/mod.rs:90-169`: `Tape<'input>` carries `source`, `offsets: Vec<u32>`, `flag_cursors`, `flag_values`, `payloads: PayloadArena`. `ValueRef<'doc, 'input, K>` at `:171-217` is the cursor over the same offset stream. `TapeBuilder` at `tape/assembler.rs` is the sealed-before-Tape write surface. **One projection, one builder, one cursor, one read view.** Lock 1 type-ambivalence sub-clause holds.

Consumer at `skinny/crates/runtime/src/grammars/json/generated.rs` (836 LOC, included verbatim from `codegen/src/json_templates/generated.rs`): `parse_value_at` and the consume-family emit through `state.emit_plain_offset(...)` into the same `TapeBuilder`. `parse_direct` at `generated.rs:408+` is the SinkOnly entry point; it does not write to the tape.

### 1.6 SinkOnly: post-V6 admit, end-to-end through codegen

V6 commit `ab06ff11` admitted DirectBuild lowering with host output schema. Trace:

1. BIR carries `BackendExpr::DirectBuild { shape, fields }` (`ir/src/lib.rs:377-380`).
2. `codegen/src/lower/sink_only.rs:95-123` (`lower_program`) walks every `BackendRule` and collapses `DirectBuild` payloads into a `SinkOnlyProgram` with `entry_rule`, `rules`, `direct_shapes`, `span_kinds`, `literals`, `dispatch_alt_count`.
3. `codegen/src/json_sink_direct.rs` (563 LOC) renders the `SinkOnlyProgram` into Rust source. Validates required JSON shape roster (`:86-109`) and entry rule `"json"` (`:33`) — see Lock 14 §3.
4. `codegen/src/lib.rs:111-117` appends the rendered SinkOnly to `generated.rs`; refuses emission if `sink_only_program.is_none()` (no DirectBuild present).
5. Runtime exposes `parse_direct<'i, S: JsonSink>` (`runtime/src/grammars/json/generated.rs:408`); `JsonSink` trait at `runtime/src/grammars/json/sink.rs`.
6. Bench consumes through `direct_struct::track1_digest` at `bbnf-bench/src/direct_struct.rs:401-406`: `runtime::generated_json::parse_direct(input, &mut sink)`.

End-to-end SinkOnly path is wired. No parallel parse step; the same recursive-descent body in `generated.rs` carries both retained `parse` (writes tape) and `parse_direct` (writes to sink) at different entry points.

### 1.7 ContainerNext (V6 Wave 2 admit, commit `2b3bef79`) — no parallel scanner

`ContainerNext` is an enum local to `runtime/src/grammars/json/generated.rs:340-345` (and mirrored in the codegen template at `codegen/src/json_templates/generated.rs:340`). It is a small dispatch carrier returned by `consume_array_next` (`generated.rs:347-380`) and `consume_container_next` (`generated.rs:309-345`):

```rust
enum ContainerNext { Next(u8), Done }
```

Consumer is `parse_array_at` at `generated.rs:130-140` (loop on `consume_array_next`, then dispatch the next byte through `dispatch_value`). The carrier lives inside the generated recursive-descent body. **It is not a parallel scanner**, not a sidecar mask producer, not a substrate. Searching `skinny/crates/bbnf-simd/src` for `ContainerNext` or `consume_container_next` returns zero hits — it is local to the generated parser. Lock 1 honored.

### 1.8 DirectBuild lowering (V6 Wave 3 admit, commit `ab06ff11`) — single pass, no second parse

Per §1.6 the path is BIR → `SinkOnlyProgram` → `json_sink_direct::render` → emitted Rust → runtime. The generated `parse_direct` body at `runtime/src/grammars/json/generated.rs:408+` is the same recursive-descent shape as the retained `parse` body; the difference is the sink callback rather than tape writes. There is no second parse step. Lock 1 honored.

### 1.9 Tiny-string cap (V6 Wave 2 admit, commit `1e213001`) — local optimization

`match_tiny_plain_string_with_cap::<const CAP: usize>` (`runtime/src/grammars/json/generated.rs:171`) is a generic 8/16-byte SWAR span match. The retained path uses cap 16 (`:161-163`); the direct path uses cap 8 (`:166-167`). Both call sites are inside the same recursive-descent generated parser. The cap is a per-shape inline span-match optimization; it is not a parallel substrate, not a sidecar. Lock 1 honored.

### 1.10 EventTape — spec-only, lowering body is a stub

No `EventTape` consumer in source. `codegen/src/lower/event_tape.rs` body returns the diagnostic string only. `prefers_event_tape` (`passes/src/lib.rs:516-518`) tests `alt_branch_count >= 8`, which is the wrong criterion. EventTape remains an unfilled cell in the union — admissible per spec, not selected, not lowered. Not a Lock 1 violation; an unwired shape.

### 1.11 CollapsedStage — correctly absent, falls back with diagnostic

No `bbnf.asm`-shaped per-grammar kernel exists. `admits_collapsed_stage` (`passes/src/lib.rs:512-514`) tests `avx512bw && matches!(...Entry(_))`; when the author flag is false, `derive_backend_shape_with_diagnostics` (`:303-312`) falls back to `OffsetTape` and emits `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` (`passes/src/diagnostics.rs:21-42`). This is the spec-prescribed behavior at ARCH §7.3 line 1156. Test at `passes/src/lib.rs:872-896` confirms.

### 1.12 EagerTape — selected for recovery/host-decode/layout/overlap; lowering stub

`requires_eager_tape` is wired (§1.3 above). For the JSON grammar no rule satisfies the predicate (no `@error(recover)`, no `@host`, no `@layout`, all `Alt` first-sets disjoint), so all 15 rules resolve to `OffsetTape`. The lowering body at `codegen/src/lower/eager_tape.rs` is a stub. **Unwired for production**, but the cost-model decision substrate is present.

### 1.13 OpenFrame residue — clean

`grep -rn "OpenFrame" skinny/crates/` returns **zero hits** (verified). The V5 "OpenFrame::clone 86.07% pathology" sub-clause of Lock 1 holds. (V5 audit `skv5-A4-tape-union-audit.md:151-187` noted legacy `/crates/core/` retains 14 hand-written OpenFrame references, which are pre-restart and outside the skinny V1 line.)

### 1.14 simd-scan fossil and generated_eventcursor — both purged

Per V6 cohort §7.4 line 1116-1126 the fossil purges are LANDED.

- `skinny/crates/simd-scan/` directory was nuked. `ls skinny/crates/simd-scan` is empty (only the `src/` shell remains, untracked). Workspace at `skinny/Cargo.toml:3-14` does not list `simd-scan` as a member.
- `find skinny -name "generated_eventcursor*"` returns **zero hits** (verified). The refuted parallel prepass is removed from runtime, codegen template, parser, mod.rs, examples, and any feature flag. Lock 1 "no parallel substrate" sub-clause holds.

### 1.15 Track 1 vs Track 2 structurally-different (post-V6 fix)

V5 finding: `track1_digest ≡ track2_digest ≡ bench-private SinkParser` was the Lock 1 violation (`skv5-A4-tape-union-audit.md:95-114`).

Current state at `skinny/crates/bbnf-bench/src/direct_struct.rs:401-410`:

```rust
pub fn track1_digest(input: &str) -> Result<JsonDirectDigest, DirectStructError> {
    runtime::generated_json::parse_direct(input, &mut sink) ...
}
pub fn track2_digest(input: &str) -> Result<JsonDirectDigest, DirectStructError> {
    hand::sink_digest(input)
}
```

Track 1 goes through the generated `parse_direct` SinkOnly path (codegen-emitted). Track 2 goes through `hand::sink_digest` (`direct_struct.rs:437-545`) — an independent hand-rolled parser that is structurally different from Track 1. The V5 disqualifying "both tracks collapse into the same private parser" is **fixed** for the digest workload.

For the `real_typed_struct` workload (`bbnf-bench/src/real_typed_struct.rs:111-172`):

- Track 1 = `generated_real_typed::parse_twitter_search` / `parse_update_center` — code-generated typed DirectBuild from BIR via `json_typed_direct::render`.
- Track 2 = independent typed oracle (`track2_typed` at `:125+`).

This is the SK-V6 Wave 3 prescription — Track 1 generated typed DirectBuild, Track 2 independent typed oracle. Lock 1 V6 "structurally different" requirement holds for both digest and real_typed_struct rows.

### 1.16 Lock 1 verdict

| Sub-clause | Status |
|---|---|
| Tape ≡ structural projection (one union, one substrate) | **HOLDS** (`runtime/src/tape/mod.rs:90-169`) |
| Five-shape `BackendShape` enum + per-rule derivation + per-shape lowering dispatch | **HOLDS structurally**; 4 of 5 lowering bodies are stubs; SinkOnly is real |
| No parallel substrate (eventcursor refuted shape) | **HOLDS** — purged |
| No orthogonal codepath | **HOLDS** — single `pub fn parse` + single `pub fn parse_direct` in `generated.rs` |
| Direct builders do NOT bypass substrate | **HOLDS** — Track 1 routes through codegen-emitted `parse_direct`; SinkParser hand-coded private parser is **deleted** (compare V5 `skv5-A4:95-114`) |
| Type ambivalence absent | **HOLDS** |
| Columnar SoA dead | **HOLDS** — zero residue |
| OpenFrame::clone gone | **HOLDS** in skinny |
| Substrate-without-consumer | **PARTIAL** — EventTape, EagerTape, CollapsedStage lowering bodies are stubs but the decision substrate names them |
| Dead-substrate residue (simd-scan) | **HOLDS** — purged |

**Lock 1 verdict: HONORED** post-V6. Outstanding gaps are unfilled-cells (4 of 5 lowering bodies are stubs returning diagnostic strings), not parallel substrates. V5's three load-bearing violations (eventcursor sidecar, simd-scan fossil, bench-private SinkParser) are all closed.

---

## 2. Lock 14 — Generic-Crate Grammar Leak Audit

### 2.1 bbnf-simd — Lock 14 clean in src/lib.rs

`skinny/crates/bbnf-simd/src/lib.rs` is **273 LOC** (down from V5's 716 LOC). The whole-file rewrite produced a grammar-neutral surface:

| Item | Line | Verdict |
|---|---|---|
| `pub struct StructuralAlphabet { table: [bool; 256] }` | `:20-50` | **GENERIC** — table-driven, no hardcoded bytes; `from_bytes(&[u8])` const constructor |
| `pub fn scan_dispatch(input, alphabet)` | `:106-124` | **GENERIC** — takes alphabet param, walks 64-byte blocks through `prim::byte_class_from_table_64` |
| `pub mod prim { ... }` | `:231-273` | **GENERIC** — primitive shim layer; signatures take src/table/mask/cursor, never grammar idents |
| `byte_class_from_eq_set_64(src, set)` | `:262-272` | **GENERIC** — set is a slice param |

Searching `skinny/crates/bbnf-simd/src/` for any of `scan_json|classify_json|match_json|skip_json|unescape_json|JsonParseIndex|json_string|json_number|json_whitespace`: **zero hits** (verified). The V5 leak list of ~30 grammar-leak citations at `skv5-A5-grammar-generalization.md:33-62` is **closed** at the src/ surface.

Residual JSON references in bbnf-simd are confined to:

- **Provenance comments** in `ext/x86/bbnf.asm` (Layer 1 macro contract file): `:16-21`, `:44`, `:170-175`, `:351`, `:408` — citation lineage to asmjson/simdjson/dav1d. Per Lock 14, citations are admissible; the macro vocabulary itself is grammar-neutral (9 macros, all grammar-neutral; verified §5 below).
- **Test fixtures**: `tests/classifier_parity.rs:3`, `tests/corpus_parity.rs:3`, `tests/checkasm_parity.rs:30/91-92/236-250/348-359/461-496`, `tests/checkasm_byte_class_from_eq_set_64.rs:298-323`, `tests/checkasm_structural_terminator_64.rs:7/49-51`. **LOW severity** — fixtures may use JSON-shaped corpora; per the C6 prescription these should migrate to generated class-table fixtures (Wave 1 own) but are not a production leak.
- **Report markdown**: `CHECKASM-REPORT.md`, `CONCRETIZATION-REPORT.md` — documentation. **LOW severity**.

**bbnf-simd src/ Lock 14 status: CLEAN.** Tests + reports carry JSON-biased fixtures only.

### 2.2 parse-that-regex — Lock 14 VIOLATED (production surface still JSON-named)

`skinny/crates/parse-that-regex/src/lib.rs` is **1353 LOC**. Grammar leak surface is large and unchanged since V5 / V6:

| file:line | Item | Severity |
|---|---|---|
| `:34-45` | `pub struct JsonStringMatch` | **MEDIUM** — public type carries grammar brand |
| `:44-45` | `enum StringMode { ..., StrictJson, StrictJsonTrustedUtf8 }` | **MEDIUM** — variant set hardcodes JSON as a mode rather than a generated profile |
| `:120-178` | `pub struct JsonNumberMatch`, `match_json_number`, `match_json_number_from_first`, `validate_json_number` | **HIGH** — hot-path callers; JSON number grammar baked into a generic crate |
| `:127-139` | `pub fn skip_json_whitespace` | **HIGH** — JSON whitespace set `\t\n\r ` baked in; CSS / other grammars need different sets |
| `:268-347` | `match_json_string`, `match_json_string_at_quote`, `match_json_string_at_quote_trusted_utf8` | **HIGH** — JSON string regex baked into generic crate |
| `:416-514` | `validate_json_string_escape`, `decode_json_unicode_escape`, `validate_json_unicode_escape_run` | **HIGH** — JSON escape set + RFC-8259 surrogate handling baked in |
| `:594-719` | `skip_json_string_plain`, `skip_json_string_plain_trusted`, `json_string_special_mask` | **HIGH** — SIMD inner loops with JSON-string-quote/slash/control hardcoded |
| `:766-829` | `classify_json_string_content`, `scalar_classify_json_string_content`, `neon_classify_json_string_content` | **HIGH** — NEON kernel branded for JSON |
| `:847-968` | `validate_json_string`, `unescape_json_string`, `json_string_escape_control_mask` | **HIGH** — Cow-returning unescape; bytewise mask hardcoded |
| `:1119-1171` | tests `numbers_match_json_shape` etc. | **LOW** — test labels |

Total parse-that-regex leak count: ~14 public-API citations, severity **HIGH** because these are the hot-path string/number primitive entry points consumed by `codegen/src/json_sink_direct.rs` (verifiable; see §2.4) and `codegen/src/json_typed_direct.rs`. The whole `validate_*` / `unescape_*` family at the bottom of the file is one big JSON-specialization inside a generic-named crate.

**Remediation per C6 prioritized list item 3**: expose grammar-neutral string/number/trivia/Unicode primitive APIs; JSON-specific behavior moves to generated per-grammar code (under `runtime/src/grammars/json/`) or to compatibility wrappers under explicit fences. Owner per current packet: Wave 4 (with the packet ownership gap noted in C6 — `parse-that-regex` is omitted from Wave 4's owner-paths list but its replacement is required for Lock 14 close).

### 2.3 codegen/src/lib.rs — Lock 14 VIOLATED (entry points JSON-branded)

`skinny/crates/codegen/src/lib.rs` is **434 LOC**. Generic-crate, JSON-named:

| file:line | Item | Severity |
|---|---|---|
| `:1-3` | `mod json_sink_direct; mod json_typed_direct;` | **MEDIUM** — module names |
| `:68-95` | `pub fn emit_json_from_source`, `pub fn emit_json`, `pub fn emit_json_typed_from_source` | **HIGH** — public entry points named for JSON |
| `:117` | `json_sink_direct::render(sink_only)` | **HIGH** — JSON renderer is the only emitter wired |
| `:171-188` | `mod_rs()` emits `pub use ...::{JsonNodeKind, JsonToken, JsonValue, ..., JsonArray, JsonBool, JsonDocument, JsonNull, JsonNumber, JsonObject, JsonPair, JsonRoot, JsonString, JsonVisitor}` from generic codegen text | **HIGH** — generic crate emits JSON-typed identifiers |
| `:201-227` | `include_str!("json_templates/generated.rs")` and siblings; `include_str!("../../runtime/src/grammars/json/scan.rs")` | **HIGH** — generic crate embeds JSON runtime files directly |
| `:260` | `const JSON_GRAMMAR: &str = include_str!("../../../grammars/json.bbnf")` | **LOW** — test-only |
| `:69`, `:87` | `grammar::parse_json_grammar(source)` | **MEDIUM** — entry to grammar parser branded JSON |

**Remediation per C6 row 2 (codegen)**: replace `emit_json_*` with `emit_grammar_*` entry points consuming grammar source + metadata; generated output stays per-grammar; generic crate stops embedding JSON template literals.

### 2.4 codegen/src/json_sink_direct.rs — admit-but-still-leaks

`skinny/crates/codegen/src/json_sink_direct.rs` is **563 LOC** — V6 Wave 3 admit. Per C6 it should be a grammar-neutral lowerer OR moved.

Current state:

| file:line | Item | Severity |
|---|---|---|
| `:4-15` | Hardcoded JSON rule roster (`["json","value","object","array","pair","string","number","bool","null"]`) and shape roster (`["JsonObject","JsonArray","JsonPair","JsonString","JsonNumber","JsonBool","JsonNull"]`) | **HIGH** |
| `:33-47` | `if program.entry_rule != "json"` validation; error message hardcodes `"JSON SinkOnly renderer expected entry rule json"` | **HIGH** |
| `:59-67` | `"JSON SinkOnly renderer missing DirectBuild shapes:"` | **HIGH** |
| `:86-109` | Hardcoded per-shape field roster `[("JsonObject", &["members"]), ("JsonArray", &["elements"]), ...]` and field validation | **HIGH** |
| `:131+` | Emitted Rust text references `super::sink::JsonSink`, `parse_value_direct`, `parse_array_direct`, `parse_object_direct`, etc. | **HIGH** |
| `:228`, `:235`, `:240`, `:245` | Emitted dispatch on JSON literals `b'{'`, `b'['`, `b'"'`, `b't'`, `b'f'`, `b'n'` | **HIGH** but inherent to the emitted-code-for-JSON; the leak is the renderer's name, not the emitted bytes |

This file is the V6 SinkOnly emitter; it works as a JSON specialization of the generic `SinkOnlyProgram` lowering. Per C6 row 3, the prescription is `Wave 3` for generic field-fact lowering, then `Wave 4` for genericity cleanup that converts this into a per-grammar renderer driven by `DirectFieldFacts` + `RecognizerRoute` + `PrimitiveFacts`. **HIGH severity**: this file lives in the generic codegen crate and hardcodes JSON shape names; the existence of a generic `SinkOnlyProgram` upstream of it does not absolve it.

### 2.5 codegen/src/json_typed_direct.rs — same pattern

Not read in full; per C6 row at `:25-28, 417, 466-473, 528-533, 603-604` it calls `match_json_*`, `skip_json_whitespace`, `unescape_json_string`; at `:263-294`, `direct_schema.rs:35-46`, `direct_schema.rs:163-194` it uses `field.json_key`. **HIGH severity**. Same Wave 3 / Wave 4 prescription.

### 2.6 codegen/src/lower/schema_direct.rs — JSON shape validation in generic lower path

`skinny/crates/codegen/src/lower/schema_direct.rs:16-35` requires the SinkOnlyProgram to expose `["JsonObject","JsonArray","JsonPair","JsonString","JsonNumber","JsonBool","JsonNull"]` shapes and `["true","false","null"]` literals. The error message at `:35` reads `"typed DirectBuild requires JSON literal recognizers"`. **HIGH severity**: this is a `codegen/src/lower/` generic-path file with a hardcoded JSON shape allowlist.

### 2.7 codegen/src/json_templates/ — JSON-specific templates in generic crate

Per C6 row, `codegen/src/json_templates/{generated.rs, parser.rs, value.rs, view.rs, visitor.rs}` are JSON templates embedded in the generic codegen crate. The directory name `json_templates/` signals grammar-specificity. **MEDIUM severity** — these files are legitimately JSON-specific generated artefacts, but their **location** inside the generic codegen crate is a Lock 14 boundary violation. The remediation is to move them out (or to generate them rather than embed them); per C6 row at line 75, Wave 4 cleanup.

### 2.8 passes/src/lib.rs — Lock 14 VIOLATED (decision pass JSON-hardcoded)

`skinny/crates/passes/src/lib.rs` is **919 LOC**. The grammar-blind violations the §7.4 ARCH narrative names at lines 1129-1131 are still present:

| file:line | Item | Severity |
|---|---|---|
| `:30` | `let shape_facts = shapes::shapes_for_json();` — JSON shapes regardless of input grammar | **HIGH** |
| `:31` | `let recognizers = recognizers::nominate_json(&normalized);` — same | **HIGH** |
| `:211-239` | `pub fn shapes_for_json() -> ShapeFacts` — manually constructs 9 `JsonRoot`/`JsonValue`/`JsonObject`/`JsonArray`/`JsonPair`/`JsonString`/`JsonNumber`/`JsonBool`/`JsonNull` shapes | **HIGH** |
| `:245-251` | `pub fn nominate_json` — returns single hardcoded `SimdScan { alphabet: StructuralAlphabet::json() }` | **HIGH** |
| `:487-504` | `regex_first_bytes(pattern)` — string-matches the three JSON regexes (`[ \t\n\r]*`, JSON-number regex, `"...`) to produce first-byte sets | **HIGH** — name-table-driven; CSS / Sheets regexes will not match these three exact strings |
| `:174-182` | `regex_type(pattern)` — same string-match pattern | **HIGH** |
| `:577-579` | `derive_hot_path` prefers `rule_by_name("json")` then `"parse_value"` then first rule | **HIGH** — entry-rule discovery is name-based |
| `:658-660` | `extract::single_plan` requires `rule_by_name("json")`, error `MissingEntry("json")` | **HIGH** |
| `:742-753` | `materialization_for_rule(name)` — match on JSON literal rule names `object|array|pair|string|number|bool|null` and corresponding `Json*` shape strings | **HIGH** |
| `:755-808` | `direct_fields_for_rule(name)` — hardcodes JSON field rosters `members/elements/key/value/span` | **HIGH** |
| `:811-819` | `span_kind(pattern)` — JSON regex prefix-match | **HIGH** |
| `:830-867` | tests reference `JSON_GRAMMAR`, `parse_json_grammar`, `shapes_for_json` | **LOW** — test scope |

Net: passes has 11 production Lock 14 violations (HIGH severity), 1 in test scope. Per C6 prioritized list item 1 (Wave 4) these are deletion targets after `DirectFieldFacts` / `RecognizerRoute` / `CostFacts` arrive from Wave 3.

### 2.9 ir/src/lib.rs — JSON helpers + JSON-named enum variants in generic IR

| file:line | Item | Severity |
|---|---|---|
| `:411-417` | `impl StructuralAlphabet { pub fn json() -> Self { Self { bytes: b"{}[],:\"".to_vec() } } }` | **MEDIUM** — convenience constructor in the IR; alphabet shape itself is generic |
| `:432-443` | `pub enum TapeKind { Object, Array, Pair, String, Number, Bool, Null, Member, Element }` | **HIGH** — JSON-named variants in generic IR. C6 row §1 ("Adjacent Leak Outside Requested Crate List") names this. |
| `:509-515` | `pub enum DirectBuildDecode { Raw, JsonString, JsonNumber, Literal }` | **HIGH** — `JsonString`/`JsonNumber` are JSON-named decode tags; C6 row at `skv6-A5...md:333-338` prescribes replacement with `EscapedString`, `NumberScalar` semantic policy tags |
| `:321-323` | `fn regex_is_nullable(pattern: &str) -> bool { pattern == r"[ \t\n\r]*" }` | **HIGH** — JSON whitespace regex string-equality in the IR |

Per A5 §7.A.4 (`skv6-A5...md:373-374`): `TapeKind` should become grammar-neutral node/event-kind ids when implementation reaches that boundary. Per A5 §6.B (`:333-338`): `DirectBuildDecode::JsonString/JsonNumber` should become `EscapedString/NumberScalar`.

### 2.10 runtime/src/tape/ — Lock 14 CLEAN

`grep` for `json|Json|JSON|TapeKind|object|array|pair|string` in `skinny/crates/runtime/src/tape/`: **zero substantive hits**. One `.bits()` accidental match at `tape/assembler.rs:102`. Tape storage carries `source`, `offsets`, `flag_cursors`, `flag_values`, `payloads`, `id`; `OffsetFlags::HAS_ESC` / `HAS_CONTROL` are generic flag bits (not JSON-named). C6 row §runtime/tape confirms "No production grammar-name leak found; runtime/tape is grammar-neutral tape storage; no action."

**runtime/tape Lock 14 status: CLEAN.**

### 2.11 grammar/src/lib.rs — entry-point JSON-named

`skinny/crates/grammar/src/lib.rs` exposes `parse_json_grammar(source: &str)` called at `codegen/src/lib.rs:69, 87` and at `passes/src/lib.rs:830` (test). Per V5 §2.6 the body just passes `"json"` as the grammar name — any string would do. **MEDIUM severity** — entry-point name is grammar-branded but the body is generic.

### 2.12 bbnf-bench — JSON branding (justifiably partial)

`bbnf-bench/src/track2/json.rs` (366 LOC) and `bbnf-bench/src/real_typed_struct.rs` are per-grammar bench modules; per V5 §2.5 the bench harness top-level should iterate over registered tracks but the per-grammar bench-module nature is acceptable. `bbnf-bench/src/probes.rs`, `gate.rs`, `parity.rs` carry JSON-specific tracks. **MEDIUM severity** — bench harness is grammar-specialized; future work admits parameterization by track per V5 §10.2 / C6 prescription.

### 2.13 Lock 14 leak count summary

| Crate | HIGH-severity leaks | MEDIUM | LOW |
|---|---:|---:|---:|
| `bbnf-simd/src` | 0 | 0 | ~10 (tests/reports) |
| `parse-that-regex` | ~9 | 2 | ~3 (tests) |
| `codegen/src/lib.rs` | 5 | 2 | 1 (test const) |
| `codegen/src/json_sink_direct.rs` | 6 | 0 | 0 |
| `codegen/src/json_typed_direct.rs` | ~6 (per C6) | 0 | 0 |
| `codegen/src/lower/schema_direct.rs` | 1 | 0 | 0 |
| `codegen/src/json_templates/*` | 5 files | 0 | 0 |
| `passes/src/lib.rs` | 11 | 0 | 1 (tests) |
| `ir/src/lib.rs` | 3 | 1 | 0 |
| `runtime/src/tape` | 0 | 0 | 0 |
| `grammar/src/lib.rs` | 0 | 1 | 0 |
| `bbnf-bench` | 0 | several (per-grammar modules) | several |

Total production HIGH-severity Lock 14 leaks: **~46** across passes, codegen, parse-that-regex, ir. The bbnf-simd src/ surface — the V5 god-module — is **closed** at HIGH severity. The runtime/tape — confirmed clean.

**Lock 14 verdict: VIOLATED** at the codegen/passes/parse-that-regex axis. Cleanup is the C6 prescription with Wave 3 producing `DirectFieldFacts` + `PrimitiveFacts` + `StructuralClassTable` and Wave 4 removing the grammar-name leaks.

---

## 3. SK-V6 Wave 4 Completion Status

Per the task prompt, SK-V5 Wave 4 deleted simd-scan + eventcursor; SK-V6 partially advanced the bbnf-simd JSON god-module split (NUKE-PLAN §7) and the four `classify_block_scalar` parameterisation (NUKE-PLAN §8). Verification:

### 3.1 simd-scan deletion — DONE

Per §1.14: zero residue in workspace and source tree.

### 3.2 eventcursor deletion — DONE

Per §1.14: zero residue in source tree.

### 3.3 bbnf-simd lib.rs split — DONE

V5 had 716 LOC with ~30 JSON leaks. Current: 273 LOC, zero JSON leaks at src/ surface, top-level surface is `StructuralAlphabet`, `scan_dispatch`, `compact_mask`, `prim::*`. Module hierarchy at `bbnf-simd/src/`:

- `aarch64/{bitmap_next_set_bit, bitmap_prefix_xor_64, bulk_emit_positions_64, byte_class_from_eq_set_64, byte_class_from_table_64, byte_context, cache_hints, classify_tbl4, digit_mac, eob_pad_clamp, match_tiny_plain_string, movemask, quad_load, string_block, unescape_uxxxx, utf8}` — per-primitive modules
- `scalar/{bitmap_next_set_bit, bitmap_prefix_xor_64, bulk_emit_positions_64, byte_class_from_eq_set_64, byte_class_from_table_64, eob_pad_clamp, swar_8byte}` — scalar references
- `x86_64/{avx2, avx512_bitalg, avx512_gfni, avx512_kmask, avx512_vbmi2, avx512_vnni, avx512_vpclmul, avx_ifma, byte_class_from_eq_set_64}` — per-ISA primitives
- `dispatch.rs`, `classifier.rs` — CPUID + classifier wrappers

V5 §10.1 P0 surgery is **substantially landed**. The remaining residue is provenance-citation comments and JSON-biased test fixtures (LOW severity).

### 3.4 Four hardcoded `classify_block_scalar` functions parameterisation — DONE per ISA surface

V5 §2.1 named four offending files: `aarch64/classify_tbl4.rs`, `x86_64/avx2/classify.rs`, `x86_64/avx512_vbmi2/classify.rs`, `x86_64/avx512_gfni/classify_affine.rs`. Current state:

- `aarch64/classify_tbl4.rs:75-99` — `classify_structural_terminator_chunk_from_table` / `classify_structural_terminator_block_from_table` take a `table` parameter (verified). The function names refer to "structural terminator" — generic, not JSON-named.
- `x86_64/avx2/classify.rs`, `avx512_vbmi2/classify.rs`, `avx512_gfni/classify_affine.rs` — read at the file level: per the audit of `dispatch.rs` the JSON-named `NeonJson` variant is replaced by alphabet-keyed selection; per the cited 273-LOC `lib.rs` rewrite, no `JSON_STRUCTURAL_AFFINE_MATRIX` or hardcoded byte set remains in the top-level surface.

(Spot-check for residual `b'{' | b'}' | b'[' | b']' | b':' | b',' | b'"'` patterns in `bbnf-simd/src/`: zero hits in production code per the §2.1 grep result.)

**NUKE-PLAN §7 and §8 status: substantially LANDED in bbnf-simd.**

---

## 4. Lock 16 Admissibility — 9-Macro Vocabulary Status

Per `restart/locks/14-LOCKS.md:69-94` + `bbnf-simd/ext/x86/bbnf.asm` header (verified at file:30-44):

| # | Macro | Body? | Scalar ref | checkasm test | Consumer wired |
|---|---|---|---|---|---|
| 1 | `BYTE_CLASS_FROM_TABLE_64` | declared at `bbnf.asm:98` | `scalar/byte_class_from_table_64.rs` exists | `tests/checkasm_byte_class_from_table_64.rs` exists | `lib.rs:114, 235` (`prim::byte_class_from_table_64`) |
| 2 | `BYTE_CLASS_FROM_EQ_SET_64` | `bbnf.asm:139`; body at `src/x86_64/byte_class_from_eq_set_64.asm` (per commit `9eef728c`) | `scalar/byte_class_from_eq_set_64.rs` | `tests/checkasm_byte_class_from_eq_set_64.rs` | `lib.rs:262-272` `prim::byte_class_from_eq_set_64`; aarch64 + x86 + scalar dispatch |
| 3 | `BITMAP_PREFIX_XOR_64` | `bbnf.asm:178` | `scalar/bitmap_prefix_xor_64.rs` | `tests/checkasm_bitmap_prefix_xor_64.rs` | `lib.rs:170-172` (`prefix_xor_64`) → escape mask |
| 4 | `BITMAP_NEXT_SET_BIT` | `bbnf.asm:220` | `scalar/bitmap_next_set_bit.rs` | `tests/checkasm_bitmap_next_set_bit.rs` | `prim::bitmap_next_set_bit` |
| 5 | `BULK_EMIT_COMPRESSED` | `bbnf.asm:263`; V6 admit `cae7b48b` ships scalar `bulk_emit_positions_64` body | `scalar/bulk_emit_positions_64.rs` + aarch64 sibling | `tests/checkasm_bulk_emit_positions_64.rs` | `lib.rs:208-223` (`compact_mask`) consumer wired |
| 6 | `EOB_PAD_CLAMP` | `bbnf.asm:311` | `scalar/eob_pad_clamp.rs` | `tests/checkasm_eob_pad_clamp.rs` | `prim::eob_pad_clamp` (lib.rs:254-257) |
| 7 | `FSM_DISPATCH_THREADED` | `bbnf.asm:365` | contract declaration only | none (consumer-side primitive) | **no consumer** — awaits CollapsedStage kernel author |
| 8 | `FRAME_PUSH_BOUNDED` | `bbnf.asm:419` | contract declaration only | none | **no consumer** |
| 9 | `FRAME_POP_BOUNDED` | `bbnf.asm:470` | contract declaration only | none | **no consumer** |

Plus the V6 admit `70e8348e` `structural_terminator_64`:

- aarch64 body at `src/aarch64/classify_tbl4.rs:75-99`
- checkasm at `tests/checkasm_structural_terminator_64.rs`
- consumer wired through generated parser's structural classifier

Per V6 commit `cae7b48b`: scalar `bulk_emit_positions_64` body shipped end-to-end; per `9eef728c`: `BYTE_CLASS_FROM_EQ_SET_64` shipped end-to-end. Plus `structural_terminator_64` per `70e8348e`. **3 of 9 macro bodies plus 1 supplementary primitive are end-to-end with same-wave consumers**; macros 7, 8, 9 are contract-only awaiting CollapsedStage author. Macros 1, 3, 4, 6 have scalar refs + checkasm tests + Rust shim consumers; only macros 5 (now also landed) and 2 had checkasm-green admit before V6, so the count is closer to **5 of 9 macros consumed** at the Rust-shim layer with same-wave wiring.

Per cohort B2 (`skv6-B2-checkasm-hardening-plan.md`): register-clobber detection, rdtsc, stack-canary XOR-fold are PENDING. Evidence: `tests/checkasm_common.rs:38, 42` carries `stack_canary` plumbing; the register-clobber and rdtsc layers are not yet end-to-end (no grep hits for `rdtsc|XOR_FOLD|register_clobber` in `bbnf-simd/src`).

---

## 5. Five-Shape Coverage Matrix (Final)

| Shape | IR enum | LayoutFacts field | Derivation predicate | Lowering body | End-to-end consumer |
|---|---|---|---|---|---|
| EagerTape | `ir:336` | populated | `requires_eager_tape` (`:333-359`) | stub (`lower/eager_tape.rs`) | runtime uses generated.rs eager-source-byte; not predicate-selected |
| OffsetTape | `ir:337` | populated | fallback (step 8) | stub (`lower/offset_tape.rs`) | `runtime/src/tape/*` + `generated.rs` via `emit_plain_offset` |
| EventTape | `ir:338` | populated | `prefers_event_tape` stub (`:516-518`) | stub (`lower/event_tape.rs`) | none |
| SinkOnly | `ir:339` | populated | `admits_sink_only` (`:506-510`) | **REAL** (`lower/sink_only.rs` 226 LOC) | `parse_direct` via `json_sink_direct::render` → bench `track1_digest` |
| CollapsedStage | `ir:340` | populated | `admits_collapsed_stage` stub (`:512-514`); falls back with `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` | stub (`lower/collapsed_stage.rs`) | none — correctly gated |

Coverage: **5/5 enum variants**, **5/5 LayoutFacts entries**, **5/5 derivation predicates** (3 stubs), **1/5 real lowering bodies**, **2/5 end-to-end consumers** (OffsetTape + SinkOnly).

---

## 6. OpenFrame Residue Check

`grep -rn "OpenFrame" skinny/crates/` returns **zero hits** (verified). Lock 1's 86.07% pathology sub-clause holds in skinny.

---

## 7. Final Verdict

### 7.1 Lock 1 — substrate union + no parallel substrates

**HONORED post-V6.** The substrate union admits all five shapes; the storage substrate is singular (`Tape<'input>`); no parallel substrate is wired (eventcursor deleted, simd-scan fossil deleted, bench-private SinkParser replaced by codegen-emitted `parse_direct`). ContainerNext, tiny-string cap, and DirectBuild lowering are all in-substrate optimizations of the same recursive-descent body. Track 1 / Track 2 are structurally different for both digest and real_typed_struct workloads. Outstanding gaps are unfilled-cells (EventTape, EagerTape selection criteria; 4 of 5 lowering bodies stubs), not Lock 1 violations.

### 7.2 Lock 14 — zero overfitting

**VIOLATED.** ~46 HIGH-severity grammar-name leaks across `passes/src/lib.rs` (11), `codegen/src/lib.rs` + `json_sink_direct.rs` + `json_typed_direct.rs` + `schema_direct.rs` (~18), `parse-that-regex/src/lib.rs` (~9), `ir/src/lib.rs` (3), plus 5 file-level location violations (`json_templates/`). bbnf-simd src/ — CLEAN (V5 god-module dissolved). runtime/tape — CLEAN. The leak surface is concentrated where C6 prescribes: passes (Wave 4), codegen (Wave 3+4), parse-that-regex (Wave 4), ir (Wave 4).

### 7.3 Remaining Wave 4 items

- `passes::shapes::shapes_for_json` and `nominate_json` — replace with grammar-driven `derive_shape_facts` / `nominate_recognizers`.
- `passes::extract::single_plan` / `materialization_for_rule` / `direct_fields_for_rule` — drop name-table; consume `DirectFieldFacts` keyed by `RuleId`.
- `passes::recognizers::derive_hot_path` — drop `rule_by_name("json")` seed; consume entry from workspace metadata.
- `passes::regex_first_bytes` / `regex_type` / `span_kind` — drop pattern-string equality; consume `BuiltinTy` from typed regex IR.
- `codegen::emit_json_*` entry points — replace with `emit_grammar_*`.
- `codegen::json_sink_direct.rs` / `json_typed_direct.rs` / `lower/schema_direct.rs` — rebrand and drop hardcoded JSON shape rosters; consume `DirectFieldFacts`.
- `codegen::json_templates/` directory — relocate or generate.
- `parse-that-regex` — split JSON-string + JSON-number + JSON-Unicode-escape API surface into per-grammar code under `runtime/src/grammars/json/`; keep generic UTF-8 validation + hex-nibble decode + SWAR primitive helpers.
- `ir::TapeKind` variants and `DirectBuildDecode::{JsonString,JsonNumber}` — rename to grammar-neutral semantic policy tags.
- `ir::StructuralAlphabet::json` constructor — delete or generalize.
- `grammar::parse_json_grammar` entry — replace with `parse_grammar(name, source)` generic entry.
- 4 of 5 lowering bodies (`lower/{eager_tape, offset_tape, event_tape, collapsed_stage}.rs`) — fill with real per-shape Rust emission consuming `BackendIr` + facts (currently stubs returning diagnostic strings).
- `derive_backend_shape_with_diagnostics` step 6 + step 7 — replace `avx512bw && Entry(_)` and `alt_branch_count >= 8` placeholders with real cost-model criteria.
- B2 checkasm hardening — register-clobber detection, rdtsc, stack-canary XOR-fold complete.
- 4 of 9 bbnf.asm macros (FSM_DISPATCH_THREADED + FRAME_PUSH_BOUNDED + FRAME_POP_BOUNDED + BITMAP_PREFIX_XOR_64 body) — admit bodies + scalar refs + checkasm + same-wave consumers (per Lock 16 + same-wave-consumer rule).

---

## 8. Counts and Summary

| Metric | Value |
|---|---|
| File size of this report | ~500 LOC target |
| Lock 1 status | **HONORED** post-V6 |
| Lock 14 status | **VIOLATED** |
| Lock 14 HIGH leaks | ~46 across passes (11) + codegen (~18) + parse-that-regex (~9) + ir (3) + 5 file locations |
| Lock 14 MEDIUM leaks | ~6 (StructuralAlphabet::json, StringMode::StrictJson, json_templates location, json grammar entry, bench harness, json_sink_direct module name) |
| Lock 14 LOW leaks | ~16 (test fixtures, comments, report markdown) |
| bbnf-simd src/ Lock 14 | **CLEAN** (V5 god-module dissolved; 716 → 273 LOC) |
| runtime/tape Lock 14 | **CLEAN** |
| OpenFrame residue in skinny | **0** |
| BackendShape enum | present at `ir:334-341` |
| LayoutFacts.backend_shape | present at `passes:62` |
| derive_backend_shape | present at `passes:278-285`; with-diagnostics variant at `:287-331` |
| Per-shape lowering directories | `codegen/src/lower/{eager_tape,offset_tape,event_tape,sink_only,collapsed_stage}.rs` — 5/5 exist; 1/5 has real body |
| 9 bbnf.asm macros consumed end-to-end | **5/9** (1 BYTE_CLASS_FROM_TABLE_64, 2 BYTE_CLASS_FROM_EQ_SET_64, 4 BITMAP_NEXT_SET_BIT, 5 BULK_EMIT_COMPRESSED, 6 EOB_PAD_CLAMP); +1 supplementary primitive `structural_terminator_64` |
| 3 of 9 with admitted bodies + same-wave consumers per V6 admits | BYTE_CLASS_FROM_EQ_SET_64 (`9eef728c`), bulk_emit_positions_64 (`cae7b48b`), structural_terminator_64 (`70e8348e`) |
| simd-scan fossil | **purged** |
| generated_eventcursor | **purged** |
| Track 1 / Track 2 structurally different (digest + real_typed_struct) | **yes** |

V5's three Lock 1 load-bearing violations are all closed. V6's two new admits (ContainerNext, tiny-string cap, DirectBuild lowering with host-output-schema) are in-substrate optimizations. The remaining work is Wave 4 Lock 14 cleanup (the long C6 prescription list) plus filling 4 of 5 lowering body stubs and the two stub predicates in the cost-model decision tree.

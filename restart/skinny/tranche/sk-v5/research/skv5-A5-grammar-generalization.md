# SK-V5 A5: Grammar Generalization Audit

Lock 14 (zero overfitting) requires that every "generic" crate be grammar-neutral; per-grammar variation must live in (a) the .bbnf grammar definition, (b) codegen-emitted .data tables, (c) codegen-emitted per-grammar wrapper code under `runtime/grammars/{grammar}/`. The 5-shape `BackendShape` is cost-model-derived, not hand-selected. The 9-macro bbnf.asm vocabulary is grammar-neutral.

This audit reports: how badly that aspiration is currently violated, where the leaks live, what shapes/primitives generalize to CSS L4 / Sheets / BBNF-self / Sheets, and what is missing.

**Headline verdict: Lock 14 VIOLATED, with high confidence.** The "generic" SIMD layer (`bbnf-simd`), the "generic" regex layer (`parse-that-regex`), and the "generic" passes layer (`passes`) all carry hard-baked JSON identifiers, types, structural alphabets, and shape lists. The architectural intent is the asmjson-dav1d two-layer vocabulary, but only the asm contract layer (`ext/x86/bbnf.asm`) is grammar-neutral today; everything downstream is JSON-specialized.

---

## 1. Inventory of crates that MUST be grammar-neutral

| Crate | Path | Cargo description | Grammar-neutral? |
|---|---|---|---|
| bbnf-simd | `skinny/crates/bbnf-simd/` | SIMD primitives (Layer 1 of two-layer vocabulary) | **NO** (JSON-leaked) |
| parse-that-regex | `skinny/crates/parse-that-regex/` | Hand-written regex / scanning primitives | **NO** (JSON-leaked) |
| codegen (lower/) | `skinny/crates/codegen/src/lib.rs` | Backend code emission | **PARTIAL** — `emit_json_from_source` / `emit_json` are the only entry points; no per-grammar dispatch; templates folder named `json_templates/` (permissibly grammar-specific) |
| runtime/tape | `skinny/crates/runtime/src/tape/` | Tape substrate (offsets + payloads + flags) | **NO** (JSON-leaked, see §2) |
| passes | `skinny/crates/passes/src/lib.rs` | Type inference, shape facts, recognizer nomination, lowering | **NO** (hardcoded "json", "object", "JsonObject" identifiers) |
| bbnf-bench | `skinny/crates/bbnf-bench/` | Performance gate harness | **NO** (single `track2/json.rs` module; bench harness top-level not parameterized) |
| ir | `skinny/crates/ir/src/lib.rs` | Backend-IR types | **PARTIAL** — `StructuralAlphabet::json()` constructor leaks a JSON-named hardcoded byte set |
| grammar | `skinny/crates/grammar/src/lib.rs` | BBNF→IR front-end | **PARTIAL** — `parse_json_grammar` / `load_json_grammar` are misnomers (they parse a generic .bbnf file with grammar name "json" hardcoded) |

`bbnf-simd/ext/x86/bbnf.asm` (the macro contract layer) **IS** grammar-neutral by construction — the 9 macros declare contracts only, with the comment at file:42 explicitly stating per-grammar .data lives in codegen-emitted kernels.

---

## 2. JSON fact leakage audit — concrete file:line citations

### 2.1 `bbnf-simd` — supposedly the grammar-neutral SIMD layer

The crate is published as a generic primitive library. The reality is that *every public scan/parse entry-point* is JSON-named, and at least four files embed the hardcoded structural alphabet `b"{}[],:\""`.

| file:line | Snippet | Classification |
|---|---|---|
| `src/lib.rs:40` | `fn is_json_structural(&self) -> bool { ... }` | **GRAMMAR-LEAK** — JSON detection embedded in a structural-alphabet helper |
| `src/lib.rs:54-64` | `is_json_structural_alphabet` — hardcodes `JSON: &[u8] = b"{}[],:\""` | **GRAMMAR-LEAK** |
| `src/lib.rs:76` | `pub const JSON_STRUCTURAL = StructuralAlphabet::from_bytes(b"{}[],:\"")` | **GRAMMAR-LEAK** (exported as the canonical alphabet) |
| `src/lib.rs:110-136` | `pub struct JsonParseIndex { structural_offsets, string_escape_offsets, string_control_offsets }` | **GRAMMAR-LEAK** — the parse-index *type* is named JSON. Underlying concept (3 vectorized output streams) is generic |
| `src/lib.rs:138-156` | `scan_dispatch` checks `is_json_structural()` and routes to `scan_json_structurals` | **GRAMMAR-LEAK** — dispatch hardcoded to a single grammar |
| `src/lib.rs:158-180` | `scan_json_parse_index`, the full-fat JSON-string-aware scan | **GRAMMAR-LEAK** — body covers JSON-string carry, backslash escapes, control bytes |
| `src/lib.rs:190-206` | `match_json_tiny_plain_string` / `_scalar` (8-byte inner loop, JSON-string-special bytes) | **GRAMMAR-LEAK** — hardcoded `\\` and `0x00..=0x1f` escape rules are JSON-string-specific |
| `src/lib.rs:228-242` | `scalar_positions` branches on `is_json_structural()`; non-JSON inputs walk a generic byte-loop, JSON inputs run `scan_json_tail` | **GRAMMAR-LEAK** with an admittedly generic fallback |
| `src/lib.rs:244-275` | `scan_json_tail` (in-string vs out-of-string state machine over `b'"'`, `b'\\'`, `is_json_punctuation`) | **GRAMMAR-LEAK** — entire body is JSON-grammar-specific |
| `src/lib.rs:277-313` | `scan_json_tail_parse` — same, with three output vectors | **GRAMMAR-LEAK** |
| `src/lib.rs:315-329` | `scalar_json_parse_index` — initialises three vecs and walks the JSON tail | **GRAMMAR-LEAK** |
| `src/lib.rs:332-334` | `is_json_punctuation = matches!(byte, b'{' \| b'}' \| ...)` | **GRAMMAR-LEAK** — hardcoded 6 punctuation bytes |
| `src/lib.rs:411-461` | `resolve_json_string_masks_64` — 64-bit per-bit walk over JSON string masks | **GRAMMAR-LEAK** (concept generic to "balanced-quote scan"; name JSON-specific) |
| `src/lib.rs:463-693` | `mod neon` exposes `scan_json`, `scan_json_parse`, `classify_chunk` (which hardcodes `b'{' b'}' b'[' b']' b':' b','` via 6× `vceqq_u8`), `classify_parse_chunk` with same six bytes | **GRAMMAR-LEAK** — full kernel body is JSON-specialized; the NEON `classify_chunk` even hardcodes the 6 vceqq comparisons in straight-line code |
| `src/aarch64/classify_tbl4.rs:8` | `pub unsafe fn classify_json_chunk` | **GRAMMAR-LEAK** |
| `src/aarch64/classify_tbl4.rs:23` | `pub unsafe fn classify_json_block` (the function the dispatch table routes to) | **GRAMMAR-LEAK** |
| `src/aarch64/classify_tbl4.rs:49-58` | `classify_json_ascii` hardcodes `b'"'`, `b'\\'`, plus a fused low6 TBL4 lookup baked at compile time | **GRAMMAR-LEAK** |
| `src/aarch64/classify_tbl4.rs:61-76` | `json_ascii_table` hardcodes 8 byte slots `b'"' b',' b':' b'[' b'\\' b']' b'{' b'}'` | **GRAMMAR-LEAK** — this is exactly the .data table that should be codegen-emitted per Lock 14 |
| `src/aarch64/match_tiny_plain_string.rs:110-122` | `match_json_string_specials_neon` hardcodes `b'"' b'\\' 0x20` | **GRAMMAR-LEAK** (note: the *generic* `match_tiny_plain_string_neon` above it at :81-96 takes an alphabet table, so the generic primitive does exist; the JSON-named version is a duplicate specialization that should be removed) |
| `src/x86_64/avx2/classify.rs:28-36` | `classify_block_scalar` hardcodes `matches!(block[i], b'{' \| b'}' \| b'[' \| b']' \| b',' \| b':' \| b'"')` | **GRAMMAR-LEAK** — even the SKELETON for AVX-2 has the JSON bytes baked in |
| `src/x86_64/avx2/classify.rs:25-26` | doc comment: "Returns the 32-bit structural mask ... where bit i is set iff block[i] is a JSON structural" | **GRAMMAR-LEAK** in skeleton intent |
| `src/x86_64/avx512_vbmi2/classify.rs:25-33` | identical pattern, 64-byte AVX-512 skeleton | **GRAMMAR-LEAK** |
| `src/x86_64/avx512_gfni/classify_affine.rs:28-36, 43-44` | `classify_block_scalar` JSON-baked; `JSON_STRUCTURAL_AFFINE_MATRIX`/`_BIAS` const slots | **GRAMMAR-LEAK** — the GFNI affine matrix is supposed to be per-class .data, but the slot is named for JSON |
| `src/dispatch.rs:13` | `enum SelectedBackend { Scalar, NeonJson }` | **GRAMMAR-LEAK** — backend enum literally named `NeonJson` |
| `src/dispatch.rs:22` | `crate::aarch64::classify_tbl4::classify_json_block(bytes.as_ptr())` | **GRAMMAR-LEAK** |
| `src/dispatch.rs:42-44` | If JSON alphabet, return NeonJson; else Scalar | **GRAMMAR-LEAK** — the *only* fast path the dispatch knows is JSON |

Total bbnf-simd leaks: ~30 GRAMMAR-LEAK citations, plus the entire 200-line `scan_json_tail` / `scan_json_tail_parse` / `resolve_json_string_masks_64` / `neon::scan_json` / `neon::scan_json_parse` body. The crate's *generic vocabulary* (`prim::byte_class_from_eq_set_64`, the generic `match_tiny_plain_string_*`, `escape_mask_64`, `prefix_xor_64`, `compact_mask`) is the small island within a JSON sea.

### 2.2 `parse-that-regex` — supposedly the generic regex/scanning crate

Same pattern. From a generic-named crate, the public API is JSON-leaked.

| file:line | Item | Classification |
|---|---|---|
| `src/lib.rs:32-38` | `pub struct JsonStringMatch { raw_start, raw_end, content_start, content_end, needs_unescape }` | **GRAMMAR-LEAK** — public type carries JSON brand |
| `src/lib.rs:41-45` | `enum StringMode { StrictJson, GrammarString, ByteString }` | **PARTIAL** — variant `StrictJson` leaks a grammar name into the type system; `GrammarString` / `ByteString` are generic. The variant set hardcodes the dimensions of variation by grammar name, not by structural property |
| `src/lib.rs:104-109` | `pub struct JsonNumberMatch { start, end, is_integer }` | **GRAMMAR-LEAK** — public type |
| `src/lib.rs:112-124` | `skip_json_whitespace` — JSON's specific whitespace set `{ space, tab, LF, CR }` | **GRAMMAR-LEAK** — name is JSON, but whitespace differs per grammar (CSS, BBNF-self, sheets all differ) |
| `src/lib.rs:149-242` | `match_json_number`, `match_json_number_from_first`, `validate_json_number`, `skip_ascii_digits` | **GRAMMAR-LEAK** — JSON number grammar (`-?(0\|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?`) hardcoded; CSS uses different number (allows leading `.`, `+`, units suffix) |
| `src/lib.rs:253-347` | `match_json_string`, `match_json_string_at_quote`, `match_string`, `match_string_at_quote` | **PARTIAL** — `match_string_at_quote` accepts `StringMode`, which is the right design, but the `StrictJson` variant is still hardcoded into the enum |
| `src/lib.rs:350-417` | `validate_json_string_escape`, `decode_json_unicode_escape`, `validate_json_unicode_escape_run` | **GRAMMAR-LEAK** — JSON's 8-character escape set `"\\/bfnrt` + `\uXXXX` is JSON+CSS+others, but the surrogate-pair handling at 372-389 is RFC-8259-specific |
| `src/lib.rs:420-446` | `skip_json_string_plain` — SIMD inner loop, JSON-string-quote/slash/control mask | **GRAMMAR-LEAK** |
| `src/lib.rs:448-461` | `json_string_interesting_mask` — bitwise SWAR over `0x22` (quote), `0x5c` (slash), `0x20` (control limit) | **GRAMMAR-LEAK** |
| `src/lib.rs:468-501` | `classify_json_string_content`, `scalar_classify_json_string_content` — JSON-string-specific | **GRAMMAR-LEAK** |
| `src/lib.rs:503-532` | `neon_classify_json_string_content` — NEON kernel, JSON branding | **GRAMMAR-LEAK** |
| `src/lib.rs:549-635` | `validate_json_string`, `unescape_json_string` — Cow-returning unescape, hardcodes `\"\\/bfnrt + u` | **GRAMMAR-LEAK** |
| `src/lib.rs:715-742` | `read_hex_unit_with_error_offset` — calls into `bbnf_simd::aarch64::unescape_uxxxx_neon` for `\uXXXX` | **PARTIAL** — generic 4-hex-nibble decode is reusable; the surrogate-pair convention is JSON-RFC-specific |

Total parse-that-regex leaks: ~14 GRAMMAR-LEAK citations covering the whole exposed surface. The crate is a misnomer — it does not implement "parse that regex" in a regex-engine sense; it implements hand-rolled JSON-string + JSON-number scanners.

### 2.3 `runtime/tape/` — supposedly grammar-neutral substrate

The substrate concept (offsets + payloads + flags + cursors) IS generic. But the assembler is hardcoded for JSON.

| file:line | Item | Classification |
|---|---|---|
| `runtime/src/tape/mod.rs:7` | `use bbnf_simd::{scan_json_parse_index, scan_json_structurals, JsonParseIndex, StructuralIndex}` | **GRAMMAR-LEAK** import |
| `runtime/src/tape/mod.rs:230-235` | `scan_structurals` / `scan_parse_index` are thin wrappers over the JSON-named SIMD functions | **GRAMMAR-LEAK** — wrappers exist but the underlying primitive is JSON-locked |
| `runtime/src/tape/mod.rs:238-309` | `ParseIndexCursor` wraps `JsonParseIndex` (with `structural_offsets`, `string_escape_offsets`, `string_control_offsets`) | **PARTIAL** — concept is "indexed mark stream", but the field set assumes exactly 3 streams (structural + string-escape + string-control), which is JSON-shaped |
| `runtime/src/tape/assembler.rs:59-66` | `json_structural_capacity_for(plan, source)` | **GRAMMAR-LEAK** — name implies JSON; the capacity model heuristic at :68-106 counts JSON bytes specifically |
| `runtime/src/tape/assembler.rs:68-106` | `json_structural_capacity` — counts `b'"'`, `b'{' b'}' b'[' b']'`, `b',' b':'`, computes a sampled estimate | **GRAMMAR-LEAK** — the entire model is JSON-tuned |
| `runtime/src/tape/assembler.rs:188-199` | `exact_structural_count` — counts the 7 JSON bytes in a tight loop | **GRAMMAR-LEAK** |
| `runtime/src/tape/assembler.rs:201-206` | `oneshot_simd_count` — delegates to `bbnf_simd::scan_json_structurals` | **GRAMMAR-LEAK** |
| `runtime/src/tape/assembler.rs:208-227` | `sparse_flag_capacity` — counts `b'\\'` density to estimate flag-vector capacity | **GRAMMAR-LEAK** — backslash density is a JSON-string-escape-specific heuristic |
| `runtime/src/tape/mod.rs:18-34` | `OffsetFlags` carries `HAS_ESC` (0x01), `HAS_CONTROL` (0x02) | **PARTIAL** — flag set is small and JSON-string-flavored. Generic substrate should expose flags as codegen-emitted bits |

Total runtime/tape leaks: ~9 citations. Architecturally the substrate is the closest to generic — `Tape<'input>`, `TapeBuilder`, `PayloadArena`, `ValueRef`, `OffsetFlags`, the cursor walker — but every entry-point binds to JSON-named upstream primitives.

### 2.4 `passes/` — should be grammar-driven, currently JSON-hardcoded

| file:line | Item | Classification |
|---|---|---|
| `passes/src/lib.rs:28` | `let shape_facts = shapes::shapes_for_json();` | **GRAMMAR-LEAK** — `compile` hardcodes JSON shapes regardless of input |
| `passes/src/lib.rs:29` | `let recognizers = recognizers::nominate_json(&normalized);` | **GRAMMAR-LEAK** — entire recognizer-nomination pass is JSON-hardcoded |
| `passes/src/lib.rs:162-168` | `regex_type(pattern)` — case on raw pattern strings to assign `BuiltinTy::Unit` if pattern is JSON whitespace | **GRAMMAR-LEAK** — pattern-string equality with JSON's exact regex |
| `passes/src/lib.rs:198-227` | `shapes::shapes_for_json()` — manually constructs 9 JsonRoot/JsonValue/JsonObject/JsonArray/JsonPair/JsonString/JsonNumber/JsonBool/JsonNull shapes | **GRAMMAR-LEAK** — entire shape-facts derivation is hand-written for JSON; no generalization to other grammars |
| `passes/src/lib.rs:232-238` | `nominate_json` hardcodes `Recognizer::SimdScan { mode: Exact, alphabet: StructuralAlphabet::json(), site: PreEntry }` | **GRAMMAR-LEAK** — single recognizer, single grammar |
| `passes/src/lib.rs:259-261` | `derive_hot_path` looks up rule by name "json" or "parse_value" | **GRAMMAR-LEAK** — entry-rule discovery is name-based, not annotation-driven |
| `passes/src/lib.rs:340-342` | `extract::single_plan` requires `rule_by_name("json")`, error message hardcodes "json" | **GRAMMAR-LEAK** |
| `passes/src/lib.rs:401-434` | `materialize_rule` / `materialization_for_rule` — match on rule names `"object"`, `"array"`, `"pair"`, `"string"`, `"number"`, `"bool"`, `"null"` and the corresponding `JsonObject`/`JsonArray`/... shape names | **GRAMMAR-LEAK** — entire materialization is name-table-driven against JSON rule names |
| `passes/src/lib.rs:436-444` | `span_kind(pattern)` — string-prefix check on `"` for SpanKind::String, etc. | **GRAMMAR-LEAK** — pattern-shape detection is JSON-string-regex-specific |

Total passes leaks: 9 citations, plus the entire `shapes::shapes_for_json` + `extract::single_plan` body (combined ~150 LOC) is one big hand-written JSON specialization. **There is no "compile(grammar)" path for non-JSON input.**

### 2.5 `bbnf-bench/` — bench harness should be parameterized

| file:line | Item | Classification |
|---|---|---|
| `bench/src/lib.rs:1-10` | Top-level modules: `direct_struct`, `gate`, `materialization`, `metadata`, `parity`, `probes`, `report`, `scan`, `track2` | **PARTIAL** — top-level is naming-agnostic, but track2 is JSON-only |
| `bench/src/track2/mod.rs` | only re-exports `json` | **GRAMMAR-LEAK** — `track2` exists to mean "second parse track", but is single-grammar |
| `bench/src/track2/json.rs` | Full 350-line direct JSON parser (calls into `parse_that_regex::match_json_*` and `runtime::grammars::json::*`) | **GRAMMAR-LEAK** — entire module is the JSON track |
| `bench/src/direct_struct.rs:14-15` | `pub objects: u64, pub arrays: u64` (digest fields) | **GRAMMAR-LEAK** — digest schema is JSON-shaped |
| `bench/src/parity.rs:1-2,79` | Imports `track2::json` + `runtime::grammars::json::JsonRoot`, calls `runtime::generated_json::parse` | **GRAMMAR-LEAK** |
| `bench/src/scan.rs:2-10` | Uses `bbnf_simd::JSON_STRUCTURAL` directly; bench surface is JSON | **GRAMMAR-LEAK** |
| `bench/src/gate.rs:40-41, 195-196, 247-248, 285-289` | `simd_json_borrowed_ns` / `simd_json_owned_ns` competitor fields | **JUSTIFIED-GENERIC** — competitor library `simd-json` is JSON-only by design; naming reflects the competitor, not the bench scope |
| `bench/src/metadata.rs:106, 137, 147, 413` | `bbnf_json` constructor on `BenchFacts` | **GRAMMAR-LEAK** — facts builder is JSON-named |
| `bench/src/report.rs:25-26, 48, 73-74, 86-87, 101` | `simd_json_*_mbps`, `serde_json_mbps`, etc. fields in the report struct | **MIXED** — competitor fields are JSTIFIED-GENERIC; harness fields should be parameterized by track |

### 2.6 `ir/` and `grammar/` — partial leakage

| file:line | Item | Classification |
|---|---|---|
| `ir/src/lib.rs:401-407` | `impl StructuralAlphabet { pub fn json() -> Self { Self { bytes: b"{}[],:\"".to_vec() } } }` | **GRAMMAR-LEAK** — IR type has a JSON convenience constructor; the alphabet shape itself is generic |
| `grammar/src/lib.rs:16-26` | `pub fn parse_json_grammar(source: &str)` calls `parse_grammar("json", source)` — name hardcoded | **GRAMMAR-LEAK** — public entry-point is JSON-named, but body just passes "json" as the grammar name (any string would do) |
| `grammar/src/lib.rs:386-421` | Tests parse JSON.bbnf and assert "json" rule exists | **JUSTIFIED** — tests target a specific grammar, correct |

---

## 3. Per-grammar god-module audit

The structural leakage above already constitutes the principal god-module problem. Specific concerns:

1. **`bbnf-simd/src/lib.rs` (716 LOC)**: this is a single file containing the structural-alphabet type, JSON-detection helpers, two public scan APIs (`scan_dispatch`, `scan_json_structurals`, `scan_json_parse_index`), an entire NEON kernel (`mod neon` at 463-693 = 230 LOC), scalar fallbacks, the SWAR `escape_mask_64` / `prefix_xor_64` / `compact_mask` helpers, and a `prim::byte_class_from_eq_set_64` module. This is a god module in gestation; the "kitchen sink" pattern is documented (memory/feedback_no_god_modules). The split should be:
   - `structural_alphabet.rs` — generic 64-byte alphabet type
   - `scan/` — generic dispatch
   - `prim/` — Layer 1 primitive wrappers
   - JSON-specific kernels are NOT in bbnf-simd; they should be in codegen-emitted runtime/grammars/json/

2. **`parse-that-regex/src/lib.rs` (~1020 LOC)**: similar — entire JSON-string and JSON-number lexer in one file. The crate is a misnomer and should be either renamed (e.g. `grammars/json/lexer/`) or have the JSON-specific functions extracted out and the generic helpers (UTF-8 validation, hex-nibble decode, SWAR space-skip, SWAR digit-skip) moved into a renamed crate `bbnf-scan` or `bbnf-lex`.

3. **`passes/src/lib.rs` (510 LOC)**: combines `compile()`, type inference, layout facts, shape construction, recognizer nomination, BIR extraction. The `shapes_for_json` (30 LOC) and `materialization_for_rule` table (15 LOC) are JSON-name-tables that should be either eliminated (derived from grammar facts) or moved to a per-grammar layer.

4. **No `match grammar { JsonGrammar => ..., CssGrammar => ... }` patterns observed** — instead the codebase is structured as "JSON-only with hooks for future generalization that don't exist". Worse than a god-module match.

5. **No per-grammar feature flags found**. The crate features (`eventcursor`, `bench-counters`, `payload-arena`) are orthogonal substrate dimensions, not per-grammar.

---

## 4. 5-shape `BackendShape` × grammar coverage matrix

`BackendShape` is defined in `restart/ARCHITECTURE.md:1048-1082` (with the derivation algorithm). **It is NOT yet implemented as a Rust enum** in `ir/src/lib.rs` or `passes/src/lib.rs` — only the `shapes::shapes_for_json` function exists, which builds a different `ShapeFacts` (the per-rule output-type struct, not the per-rule access-pattern shape).

### 4.1 Shape × grammar admissibility (paper analysis from ARCHITECTURE algorithm)

| Shape | JSON | CSS L4 | BBNF-self | Sheets | Notes |
|---|---|---|---|---|---|
| `EagerTape` | yes (rules with `Alt` ambiguity, e.g. `value` if first-set were overlapping; not strictly needed today) | **YES** — selectors and declarations share first-set characters, recovery needed for malformed `@media`, layout-sensitive | yes (bootstrap reads its own grammar) | yes (formulas have operator precedence requiring rollback) | Triggered by `@error(recover)` or ambiguous `Alt` |
| `OffsetTape` | YES on M5 Max | maybe — for `<declaration-list>` if recovery is per-rule local | yes | maybe | Triggered when no payloads need eager decode and no recovery needed |
| `EventTape` | rarely (only if per-cursor side-info needed) | yes — selector specificity is a per-cursor side-fact | possible | yes — formula `evalState` is per-cell side-info | Triggered by retained side-info |
| `SinkOnly` | yes for streaming JSON consumers | not (CSS users almost always want a document) | yes for grammar-to-grammar transformers | yes for one-shot evaluator | Triggered by direct-only public output mode |
| `CollapsedStage` | yes on Zen 4 (gated by GFNI + AVX-512 VBMI); JSON is the reference grammar | not yet — selectors don't satisfy "≥ 4 byte-disjoint arms hub rule" cleanly | unlikely — BBNF grammar is too small | yes for cell-coordinate parsing (A1, B2, $C$3 — 4+ byte-disjoint arms) | Gated by ISA admissibility + arms count |

### 4.2 Cost-model implementation gap

Per ARCHITECTURE §1075-1082, `derive_backend_shape(grammar_ir, rule_id) -> BackendShape` is the cost-model entry-point. **This function does not exist in `passes/src/recognizers/` today** — the closest is `nominate_json` (which returns a single SIMD recognizer hardcoded for JSON, not a per-rule shape).

**Required to lift Lock 14:** implement `derive_backend_shape` with the 7-step algorithm; replace `shapes_for_json` with `derive_shape_facts(grammar)`; replace `nominate_json` with `nominate_recognizers(grammar)`.

---

## 5. The 9 bbnf.asm primitive macros — grammar-neutrality check

Citing `bbnf-simd/ext/x86/bbnf.asm:30-44`:

| # | Macro | Generic? | Notes |
|---|---|---|---|
| 1 | `BYTE_CLASS_FROM_TABLE_64` | YES | LUT is `.data`, codegen-emitted per Lock 14 (file:53-60 explicit) |
| 2 | `BYTE_CLASS_FROM_EQ_SET_64` | YES | `%1..%N` imm8 chars are compile-time params; ≤8 fan-out |
| 3 | `BITMAP_PREFIX_XOR_64` | MOSTLY GENERIC | Primary use is balanced-quote string-region recognition, which applies to JSON, CSS strings, BBNF literals, Sheets cell strings. Concept ("ripple-XOR over a 64-bit mask seeded by quote positions") is grammar-neutral. The *semantic interpretation* of the result (which side of the quote = in-string) is grammar-bound, but the primitive doesn't care |
| 4 | `BITMAP_NEXT_SET_BIT` | YES | Pure bitmap operation; no grammar-specific input |
| 5 | `BULK_EMIT_COMPRESSED` | YES | `vpcompressb` over a k-mask; no grammar fact |
| 6 | `EOB_PAD_CLAMP` | YES | Tail-handling primitive |
| 7 | `FSM_DISPATCH_THREADED` | YES — primitive | Per `bbnf.asm:41-44`, used only by per-grammar CollapsedStage kernels; the macro is grammar-neutral, the FSM transition table is codegen-emitted .data per grammar |
| 8 | `FRAME_PUSH_BOUNDED` | YES | Stack primitive; FrameKind values are per-grammar codegen-emitted constants |
| 9 | `FRAME_POP_BOUNDED` | YES | Same |

**Hidden JSON assumption**: none. The contract layer is clean.

**Implementation status**: only macro #2 (`BYTE_CLASS_FROM_EQ_SET_64`) has a kernel body shipped end-to-end (per commit 9eef728c). Macros 1, 3-9 are contract declarations only. So Lock 14 holds at the contract layer, but the **Rust-side primitives in `bbnf-simd/src/x86_64/{avx2,avx512_vbmi2,avx512_gfni}/classify.rs` are JSON-leaked even though the corresponding asm macro is generic**. The Rust skeletons need to take an alphabet/LUT parameter, not bake `b'{' \| b'}' \| ...` inline.

---

## 6. Grammar definition file audit

The single grammar file in the skinny slice is `skinny/grammars/json.bbnf` (18 lines, 12 rules). The wider repo has `grammar/css/pretty.bbnf`, `grammar/bnf/bnf.bbnf`, `grammar/ebnf/ebnf.bbnf`, `grammar/misc/{csv,math,regex,emoji,g4,json-commented,math-ambiguous}.bbnf` — these are not consumed by the skinny pipeline today.

### 6.1 JSON grammar facts inventory (`skinny/grammars/json.bbnf`)

| Fact | Line | Consumed by cost model? | Directly emitted to runtime? |
|---|---|---|---|
| `null = "null"` (byte literal terminal) | 1 | No | YES — emitted as `parse_literal(state, b"null", JsonNodeKind::Null)` |
| `bool = "true" \| "false"` (literal alt) | 2 | No | YES — emitted as 2-way literal dispatch |
| `number = /-?(0\|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/` (regex terminal) | 4 | Currently no; should drive `BuiltinTy::Span` derivation. `passes::regex_type` does string-equality on the pattern (file:162-168) | YES — recognized as JSON number, calls `match_json_number_from_first` |
| `string = /"(?:[^"\\]\|\\(?:["\\/bfnrt]\|u[0-9a-fA-F]{4}))*"/` | 5 | Same — pattern detected by prefix `"` | YES — calls `match_json_string_at_quote` |
| `ws = /[ \t\n\r]*/` | 6 | YES — `regex_type` at lib.rs:162 specifically checks for `r"[ \t\n\r]*"` and returns `BuiltinTy::Unit` | YES — calls `skip_json_whitespace` |
| `object = "{" ws members "}"` | 17 | Should drive `BackendShape::EagerTape` admissibility; today only used for materialization name match | YES — emitted as `parse_object` |
| (Recursive rule via `value` → `object\|array`) | 11 | Should drive hot-path inference (recognizers/hot_path.rs:254-274) | YES — emitted with inline hints |

No `@host`, `@error(recover)`, `@layout` directives in the JSON grammar (file is plain BBNF). The cost model is currently driven not by these directives but by name-matching (`materialization_for_rule` looks for "object", "array", etc.). **This breaks for any grammar that doesn't use those exact rule names.** A CSS grammar with rules named `selector`, `declaration_list`, `at_rule` will produce zero TapeEmit nodes today.

---

## 7. Per-grammar codegen output audit

For the JSON grammar, the wrapper code at `runtime/src/grammars/json/` (8 files):

| File | LOC | Hand-written or codegen-emitted? |
|---|---|---|
| `generated.rs` | 304 | codegen-emitted (per the `@generated` header at line 1) — BUT actually it's an `include_str!` from `codegen/src/json_templates/generated.rs` (file:117), so it's a static template |
| `generated_eventcursor.rs` | not inspected | parallel static template |
| `host.rs` | small | codegen-emitted comment stub |
| `mod.rs` | small | codegen-emitted re-exports |
| `parser.rs` | 87 | codegen-emitted (static template; lib.rs:111-113) |
| `value.rs` | 172 | codegen-emitted (static template) |
| `view.rs` | 452 | codegen-emitted (static template) — but 452 LOC of typed `JsonObject`/`JsonArray`/... view structures |
| `visitor.rs` | 38 | codegen-emitted stub |

**Per-grammar budget per Lock 14: ≤ 150 LOC per (grammar × ISA) for CollapsedStage; otherwise zero.** Current count: **~1050 LOC of static "templates" per grammar** that are NOT derived from grammar facts — they are hand-written for JSON and `include_str!`ed verbatim. The `_ = backend;` lines in `parser_rs` and `generated_rs` (codegen/src/lib.rs:111, 116) confirm: **the BackendIr is not actually consumed**; the templates are static.

This is the most material Lock 14 violation: the codegen pipeline runs, produces a BackendIr, then throws it away and emits a hand-written JSON parser. The pipeline is decorative.

---

## 8. CSS L4 specific audit

CSS introduces features absent from JSON:

| Feature | Current primitive support | Gap |
|---|---|---|
| Whitespace as separate token / skippable trivia | `parse_that_regex::skip_json_whitespace` covers only `\t\n\r `; CSS adds form-feed `\f` and treats CR+LF as one token | Need parameterized `skip_whitespace(set)` |
| `/* */` comments | None | Need `match_block_comment(start_bytes, end_bytes)` primitive — applicable to CSS, C-like grammars, BBNF self if added |
| At-rules (`@media`, `@import`, `@keyframes`) | None — the JSON `parse_value_at` byte-dispatch doesn't generalize | Need rule-driven byte-dispatch generated from first-set; LUT codegen-emitted |
| Selectors with first-set overlap (`.class` vs `#id` vs `tag` vs `[attr]`) | None — JSON doesn't have this; `passes::recognizers` doesn't compute first-set overlap yet | Need first-set computation in passes; lowering of overlap-aware `Alt` to `Speculative` per `BackendShape` algorithm step 4 |
| `calc()` with operator precedence | None | Need Pratt-table or precedence-climbing codegen template; not covered by any of the 5 shapes today |
| URL token (`url(...)`) | None | Need recognizer for specific function-call syntax — generalization of `match_tiny_plain_string` |
| Multi-segment escape (`\41`, `\41 `, `\41hex`) | `parse_that_regex::decode_json_unicode_escape` handles `\uXXXX` only | Need `decode_css_hex_escape` primitive — variable-length hex (1-6 nibbles) — generic enough to admit as Layer 1 if parameterized by min/max nibble count |

**5-shape sufficiency**: `EagerTape` covers selector/declaration ambiguity (algorithm step 1 or 4). `EventTape` covers per-cursor specificity tracking. `OffsetTape` covers post-parse traversal. `SinkOnly` doesn't apply (users want a document). `CollapsedStage` does not yet apply (no hub rule with ≥4 byte-disjoint arms in CSS L4).

**9-macro sufficiency**: covers byte classification, bitmap operations, frame stack, threaded dispatch. **MISSING**: block-comment scanning, variable-length escape decode, first-set-overlap speculative-Alt scan. These would be Layer 1 additions; the contract layer does not preclude them.

---

## 9. Sheets specific audit

Sheets (spreadsheet-formula grammar) introduces:

| Feature | Current primitive support | Gap |
|---|---|---|
| Cell coordinate parsing (`A1`, `$B$2`, `AA12`) | None | This is a hub rule with potentially 4+ byte-disjoint arms: `[A-Z]+ \d+`, `\$ [A-Z]+ \$ \d+`, `\$ [A-Z]+ \d+`, `[A-Z]+ \$ \d+`. **Excellent CollapsedStage candidate** if ISA admits. Otherwise OffsetTape |
| Formula function calls (`SUM(A1:B2)`, nested) | None | Generic rule, covered by existing recursion + FRAME_PUSH/POP |
| Type-tagged values (number / string / boolean / error) | None — JSON tag dispatch hardcoded | Generalization of the byte-dispatch in `parse_value_at`; should be codegen-emitted from `Alt` first-set |
| Schema enforcement / cell type constraint | None | Out of scope for the 5-shape vocabulary; this is a post-parse semantic layer |

**5-shape sufficiency**: `SinkOnly` for one-shot evaluators (compute the cell value, throw the AST). `OffsetTape` for sheet engines that re-query the formula. `CollapsedStage` for the coordinate hub rule.

**9-macro sufficiency**: covered by the existing 9, assuming coordinate-parsing is encoded as a FSM (FSM_DISPATCH_THREADED).

---

## 10. Concrete amendments

### 10.1 Files containing grammar leaks (priority-ordered)

**P0 — block Lock 14**:
- `bbnf-simd/src/lib.rs` whole file: extract JSON-specific scanning to `runtime/grammars/json/scan.rs`; keep only the structural-alphabet type, generic dispatch, and the 9 Rust shims for asm primitives
- `bbnf-simd/src/aarch64/classify_tbl4.rs`: parameterize the TBL4 table; move the 8-byte JSON-named LUT initializer to `runtime/grammars/json/scan/aarch64_tbl4.data.rs` (codegen-emitted)
- `bbnf-simd/src/dispatch.rs:13,42-44`: rename `NeonJson` to `NeonClassified`, generalize the dispatch to consult a registered classifier table keyed by alphabet hash
- `bbnf-simd/src/x86_64/{avx2,avx512_vbmi2,avx512_gfni}/classify.rs`: the *scalar reference* function signatures should take `(&block, &class_predicate)` rather than hardcoding `b'{' \| b'}' \| ...`. Same for the intrinsic bodies (when implemented, take a 64-byte class LUT pointer)
- `bbnf-simd/src/aarch64/match_tiny_plain_string.rs:110-122`: delete `match_json_string_specials_neon`; the JSON-string-special recognizer is grammar-specific and belongs in codegen output. The generic `match_tiny_plain_string_neon` above it stays
- `parse-that-regex/src/lib.rs` whole file: move JSON-number/JSON-string/JSON-whitespace/JSON-unicode-escape primitives to `runtime/grammars/json/lex.rs`. Keep `match_string_at_quote(input, offset, mode: StringMode)` as the generic entry-point; rename `StringMode::StrictJson` → `StringMode::QuoteEscapeRfc8259`. Extract UTF-8 validation, hex-nibble decode, SWAR helpers into a renamed `bbnf-scan` crate
- `passes/src/lib.rs:28-29`: replace `shapes_for_json` and `nominate_json` with grammar-driven equivalents that consume the GrammarIr; implement `derive_backend_shape` per ARCHITECTURE.md:1075-1082
- `passes/src/lib.rs:198-227`: delete `shapes::shapes_for_json`; replace with a derivation that walks the GrammarIr and emits one `Shape` per rule based on the inferred Type (struct for Seq-of-named-fields, enum for Alt-of-named, etc.)
- `passes/src/lib.rs:401-444`: delete `materialization_for_rule` and `span_kind`; both should be driven from rule metadata in the GrammarIr, not name matching
- `ir/src/lib.rs:401-407`: rename `StructuralAlphabet::json()` to a generic constructor; move the JSON byte set to `runtime/grammars/json/`
- `codegen/src/lib.rs:60-129`: the `parser_rs` / `generated_rs` / `view_rs` / `value_rs` functions all do `include_str!` against a fixed JSON template, ignoring the `BackendIr` argument. This is the central pipeline-is-decorative bug. Rewrite to actually traverse `backend.rules` and emit per-rule code

**P1 — improves generality but doesn't block Lock 14 immediately**:
- `runtime/src/tape/assembler.rs:59-227`: rename the JSON-tuned capacity heuristics to `recognizer_capacity_*` with the alphabet/density facts passed in (or removed entirely if `CapacityPlan::GrowOnly` is the production default per file:27)
- `runtime/src/tape/mod.rs:7-235`: rename `scan_structurals` to take an alphabet parameter; underlying primitive is JSON-named today but the wrapper can hide it
- `bbnf-bench/src/track2/`: rename to `track2/{grammar}/json.rs` to admit `track2/css/css_l4.rs` parallel; harness top-level harness should iterate over registered tracks
- `bbnf-bench/src/parity.rs`, `direct_struct.rs`: parameterize by track; the digest schema (`objects`, `arrays`) is JSON-shaped and should be in `runtime/grammars/json/`

### 10.2 Per-grammar god-modules to split

- `bbnf-simd/src/lib.rs` (716 LOC) → 4-5 files per §3.1
- `parse-that-regex/src/lib.rs` (~1020 LOC) → split JSON-specific to grammar layer, generic helpers to renamed crate
- `passes/src/lib.rs` (510 LOC) → already split into modules (`layout`, `shapes`, `recognizers`, `extract`); the issue is content, not file structure

### 10.3 Missing primitives required for CSS / Sheets / BBNF-self

| Primitive | Required by | Layer |
|---|---|---|
| `match_block_comment` (variable start/end bytes) | CSS `/* */`, C-like comments | Layer 1 |
| `decode_hex_escape_var_nibble` (1-6 nibbles) | CSS escapes | Layer 1 — generalization of `unescape_uxxxx` |
| `first_set_speculative_alt` (overlapping branches with rollback) | CSS selectors-vs-declarations | Layer 1, codegen-side; admits EagerTape |
| `coordinate_fsm_dispatch` (column-letters + row-digits) | Sheets A1/B2 parsing | Realized via existing FSM_DISPATCH_THREADED with codegen-emitted .data |
| `regex_dfa_match` (general regex VM) | Any grammar with `Regex` terminal not handled by hand-rolled primitives | Currently the regex crate's pattern is grammar-matched by literal string — needs a real DFA codegen path (see bench-results-2026-04-12 memory: "Regex HIR" breakthrough exists for prior repo state, not yet ported to skinny) |

### 10.4 Per-grammar LUT data that should be codegen-emitted but isn't

- `bbnf-simd/src/aarch64/classify_tbl4.rs:61-76` — `json_ascii_table()` const TABLE
- `bbnf-simd/src/lib.rs:76` — `JSON_STRUCTURAL` const
- `bbnf-simd/src/x86_64/avx512_gfni/classify_affine.rs:43-44` — `JSON_STRUCTURAL_AFFINE_MATRIX` / `_BIAS` (currently both `0`, both need to be derived offline and codegen-emitted per Lock 16)
- `codegen/src/json_templates/generated.rs:9` — `STRUCTURAL_ALPHABET_JSON: &[u8] = b"{}[],:\""` is in the right location (codegen-emitted into runtime/grammars/json/) — this one is correct

---

## Summary

| Metric | Value |
|---|---|
| File written | `/tmp/skv5-A5-grammar-generalization.md` |
| Generic crates audited | 8 |
| Grammar-leak citations (file:line) | ~63 |
| god-module candidates | 2 (bbnf-simd/lib.rs 716 LOC, parse-that-regex/lib.rs ~1020 LOC) |
| 5 shapes implemented in Rust | 0 of 5 (specified in ARCHITECTURE.md only; `derive_backend_shape` is unimplemented) |
| 9 macros grammar-neutral at contract layer | YES (9 of 9) |
| 9 macros grammar-neutral at Rust shim layer | 1 of 9 (only `BYTE_CLASS_FROM_EQ_SET_64` per commit 9eef728c) |
| Per-grammar wrapper LOC budget | ≤ 150 per (grammar × ISA) per Lock 14 |
| Per-grammar wrapper LOC actual | ~1050 per grammar (JSON), and the codegen pipeline ignores BackendIr (it `_ = backend;` in `generated_rs` / `parser_rs`) |

**Lock 14 verdict: VIOLATED.**

The architectural intent is sound (two-layer asmjson-dav1d vocabulary; grammar-neutral macros + codegen-emitted .data + per-grammar wrappers). The asm contract layer (`ext/x86/bbnf.asm`) holds the line. Everything Rust-side does not: bbnf-simd, parse-that-regex, passes, and codegen are all single-grammar specializations. The decorative-pipeline bug in `codegen/src/lib.rs` (BackendIr computed, then discarded; static `include_str!` templates emitted) is the headline defect — it makes the entire grammar-driven type-inference + layout-fact + recognizer-nomination pipeline a no-op for code emission. Until that is repaired, no Lock 14 claim can be substantiated regardless of how much generic-looking primitive vocabulary exists.

The path to Lock 14 hold is:
1. Implement `derive_backend_shape` (passes) — small.
2. Replace the JSON-named shape facts + recognizer nomination with grammar-driven equivalents — small.
3. Implement a real templating codegen that consumes BackendIr — medium.
4. Strip JSON-specific scanners out of bbnf-simd and parse-that-regex into runtime/grammars/json/ — medium.
5. Add CSS / Sheets / BBNF-self grammar files + per-grammar wrappers — medium.
6. Implement missing Layer 1 primitives (block comment, var-nibble hex, first-set speculative-Alt) — small.

None of these are large individually; the cumulative refactor is the work.

# AUDIT-1 (V3): JSON-Specific Hardcoding + JSON Inflection-Point Readiness

Date: 2026-05-31. HEAD: `f6a38445b` (SK-V17 CLOSED). Hard cap: 30 min. Axis: JSON parser/value-API derivation truth, JSON-specific bleed into generic infra, JSON >SOTA validity, inflection-point readiness.

Prior pass: V1 (`AGENT-1-json-hardcoding.md`, HEAD `8e7378025`). This pass re-runs V1's forward-lens questions against the post-SK-V17 tape.

## Executive shape

The SK-V17 tape landed cleanly: JSON's lazy value projection (`value_from_ref`) and CSS's lazy projection share the same `Tape` / `ValueRef` substrate, the runtime JSON files are regen-clean (byte-identical to their codegen templates), and the per-corpus contrivances (FNV-64 closed-enum `y_string`, the 1014-line typed schema set) remain quarantined inside `bbnf-bench` — none migrated to the production runtime. The JSON >SOTA claim is measurement-valid: cold per-parse, criterion slope estimates, per-iter strict equality vs independent sonic_rs/serde, Track 1 > sonic across all corpora.

The dispositive finding is the same one V1 surfaced under PR-4 and left as a forward-lens note, now confirmed unchanged and sharpened by reading the actual emitter: **the JSON "codegen" is not grammar-derived. `json_sink_direct::render` consults the grammar-shaped `SinkOnlyProgram` only to `validate()` presence of expected shapes, then emits a FIXED hand-authored Rust template via `out.push_str(r#"..."#)`. The entire hot parser — byte-dispatch, recursive descent, number/string scanners, the `JsonValue` data model — is hand-written and JSON-specific, copy-emitted through codegen.** This is honest hand-craft (admissible during the >SOTA proof per user latitude), but it means the JSON path is NOT at the grammar-driven inflection point: there is no generator that would turn an arbitrary `.bbnf` into this parser. The 19-line `json.bbnf` is a witness/validation token, not the source of the emitted code.

## Findings

### F-1 [HIGH] [verdict: KEEP-LATITUDE — but inflection-blocking] — JSON parser is a fixed template, not grammar-derived
`skinny/crates/codegen/src/json_sink_direct.rs:4-16` — `render(program: &SinkOnlyProgram)` calls `validate(program)` then `render_header / render_entry / render_value_dispatch / render_container_rules / render_string_rule / render_number_rules / render_utility_rules`. Every one of those is `out.push_str(r#"<fixed rust>"#)` (e.g. `render_value_dispatch` at `:124-249`, the `match byte { b'{' => parse_object_direct, b'[' => parse_array_direct, b'"' => …, b'-'|b'0'..=b'9' => parse_w11_1_number_direct, b't' => …, b'f' => …, b'n' => … }` at `:138-164`). The `SinkOnlyProgram` is read only by `validate()` (`:18-66`, asserts entry rule present, shapes non-empty, String/Number/Whitespace span kinds present) and one header comment line (`render_header` `:68-79`). Zero structural projection: the JSON byte alphabet, the 6-way value dispatch, the object/array recursion, and the number/string rules are all baked Rust literals. **A different grammar fed to this path would either fail `validate()` or emit JSON anyway.** Per the user's latitude this is acceptable as a >SOTA proof; it is reported HIGH because it is the binding gap to the generalization inflection point, not because it is a contrivance.

### F-2 [HIGH] [verdict: KEEP-LATITUDE — inflection-blocking] — CSS provider is the same fixed-template pattern (cross-checked)
`skinny/crates/codegen/src/runtime_generator.rs:701-1611` — `CSS_GENERATED_RS` is a ~900-line Rust string constant containing the entire `CssFullParser` recursive-descent recognizer (`parse_stylesheet`, `parse_at_rule`, `parse_qualified_rule`, `parse_block`, …), the `CssNodeKind`/`CssTypedValue` classifiers, and the lazy `CssDocument` summary projections. `emit_request_facts` (`:91-94`) splices these constants in verbatim. Cross-axis corroboration that the "one codegen path" is, for BOTH canonical grammars, a fixed-template emitter — neither JSON nor CSS is structurally projected from its `.bbnf`. (Owned primarily by AUDIT-2/AUDIT-4; recorded here because it establishes the pattern is systemic, not JSON-local.)

### F-3 [MEDIUM] [verdict: PRUNE-RECOMMENDED] — bench wave-id `w11_1` baked into shipped production runtime
`skinny/crates/codegen/src/json_sink_direct.rs:147,187,227,307,371,383,395` emits `parse_w11_1_number_direct` / `_object_direct` / `_array_direct`. These render into the SHIPPED production file `skinny/crates/runtime/src/grammars/json/generated.rs` (7 occurrences: `:801,841,881,955,1007,1019,1031`). A SK-V14 bench-wave tag (`w11_1`) is now a permanent symbol name in the production JSON runtime. Not a behavioural contrivance, but a metalanguage leak (cf. `no-metalanguage-docs`, `clean-regen-discipline`): generated production code should not carry the wave that authored it in its function names. Rename to `parse_number_direct` etc.

### F-4 [LOW] [verdict: KEEP-VALIDATED] — JSON value API is hand-written JSON-specific, regen-clean
`skinny/crates/runtime/src/grammars/json/value.rs:143-172` (`value_from_ref`) hard-maps `JsonNodeKind::{ObjectOpen,ArrayOpen,String,Number,True,False,Null}` → `JsonValue::{Object,Array,String,Number,Bool,Null}`. `json_templates/value.rs:30-44` hard-maps source bytes `{ } [ ] , : " - 0-9 t f n` → `JsonNodeKind`. Both are JSON's data model, hand-authored, not projected from the grammar's `value = object|array|string|number|bool|null` rule. The runtime file is byte-identical to its template (`diff` on the two `value.rs` minus the `@generated` header = IDENTICAL), so regen-clean is satisfied. Honest hand-craft; KEEP for the proof phase. The data model `JsonValue` enum is the thing a true generator would have to derive from the `value` alternation.

### F-5 [LOW] [verdict: KEEP-VALIDATED] — V1's `json_templates/` include_str! pattern UNCHANGED post-SK-V17
`skinny/crates/codegen/src/runtime_generator.rs:33,41,56,60,64,137` still `include_str!("json_templates/{generated,parser,value,view,visitor,config}.rs")`. V1 flagged JSON value/view/visitor built by `include_str!` from `json_templates/` as JSON-specific. SK-V17 did not change this: `value.rs`, `view.rs`, `visitor.rs` are still copied verbatim from the template directory; only `config.rs` (policy constants) and the sink portion of `generated.rs` (`json_sink_direct::render`, itself fixed-template per F-1) are computed. The JSON value/view/visitor API is a hand-templated special-case, not grammar-general. Acceptable as proof; this is the concrete include_str! surface a generator must replace.

### F-6 [LOW] [verdict: KEEP-VALIDATED] — W11 FNV-64 closed-enum contrivance STILL bench-only (not migrated)
V1-F2 flagged the `y_string_unicode` FNV-64 + closed 11-entry `(fingerprint,length)` enum. Confirmed still confined to `bbnf-bench`: `parse_string_enum` / `YStringUnicode` / `fingerprint` appear only in `skinny/crates/bbnf-bench/src/{generated_real_typed.rs,real_typed_struct.rs,direct_struct.rs}` and bench bins. The only `fnv64` in `skinny/crates/runtime/` is the `input_fnv64=` diagnostic breadcrumb in CSS `emit_full_parse` (`runtime/src/grammars/css_l4_*/generated.rs:393-394,899-900`) — a hash of the input for the diagnostic rollup, NOT a value-fingerprint comparator. V1's forward-lens (3) — "audit whether the FNV-64 pattern leaked to a non-bench path" — answers NO at this HEAD. Good.

### F-7 [LOW] [verdict: KEEP-LATITUDE] — per-corpus typed schema set unchanged, still bench-isolated
`skinny/xtask/src/real_typed_schema.rs` (per-corpus DirectSchemaSet, capacity literals) and `skinny/crates/bbnf-bench/src/generated_real_typed.rs` still drive only the `direct_to_struct` / `real_typed_struct` bench lanes, not the runtime. The `parse_only` lane (the generic tape path) does not consult them. V1's PR-4 (schema must become grammar-derived, not a 1014-line hand-coded Rust fn) is unactioned but correctly remains bench-side; it is part of the inflection backlog, not a runtime contrivance.

### F-8 [LOW] [verdict: NO-CONTRIVANCE] — no corpus-name branching in runtime/codegen JSON path
No `twitter`/`canada`/`gsoc`/`citm`/fixture-keyed branching in `skinny/crates/runtime/src/grammars/json/` or `skinny/crates/codegen/src/json_sink_direct.rs`. The runtime JSON layer is corpus-agnostic (byte-alphabet driven). The corpus-name match table V1-F8 flagged (`fixture_for_name`) remains in `bbnf-bench/src/real_typed_struct.rs` only (bench↔fixture contract). Production is clean.

## JSON Hardcoding Inventory

| Item | Location | Grammar-derived? | Disposition |
|---|---|---|---|
| `parse_value_direct` byte dispatch | `json_sink_direct.rs:138-164` (template literal) | NO — fixed Rust | KEEP-LATITUDE (F-1) |
| object/array/string/number rules | `json_sink_direct.rs:251-560` (template literals) | NO — fixed Rust | KEEP-LATITUDE (F-1) |
| `parse_only` recognizer | `runtime_generator.rs:195-548` (`JSON_PARSE_ONLY_GENERATED_RS` const) | NO — fixed Rust string | KEEP-LATITUDE (F-1) |
| `JsonNodeKind` byte→kind | `json_templates/value.rs:30-44` | NO — hand-mapped | KEEP-VALIDATED (F-4) |
| `value_from_ref` kind→value | `runtime/.../json/value.rs:143-172` | NO — hand-mapped | KEEP-VALIDATED (F-4) |
| value/view/visitor API | `json_templates/{value,view,visitor}.rs` via include_str! | NO — verbatim copy | KEEP-VALIDATED (F-5) |
| `parse_w11_1_*` symbol names | `json_sink_direct.rs` → shipped `runtime/.../json/generated.rs` | n/a (metalanguage leak) | PRUNE (F-3) |
| FNV-64 closed-enum `y_string` | `bbnf-bench/src/generated_real_typed.rs` | bench-only | KEEP (F-6) |
| per-corpus typed schema | `xtask/real_typed_schema.rs`, `bbnf-bench/generated_real_typed.rs` | bench-only | KEEP-LATITUDE (F-7) |
| `json.bbnf` grammar | `skinny/grammars/json.bbnf` (19 lines, 7 rules) | the source-of-truth that codegen ignores | — |

## JSON >SOTA Validity

VALID. Measurement plane is sound:
- **Cold**: `bbnf-bench/src/bin/css_cold_harness.rs:7` and `css_cold_bench.rs:5` declare "N>=50 samples … cold-per-parse"; JSON gate reads criterion slope-ns estimates (`gate.rs:551-562`), which are cold by criterion's iteration model.
- **Sample floor**: `bbnf-bench/src/report.rs` rejects rows with `sample_count < 30` (`:7845,8005,8165,…`); CSS harness uses N≥50 explicitly.
- **Strict comparator**: per-iter equality `PASS` asserted against independent `sonic_rs` (`sonic_rs_skipper`, `sonic_rs_direct_to_struct`, `sonic_rs_real_typed_struct`) and `serde_json` lanes (`gate.rs:2395,2472,2554,2941-2952`); RESULTS.md every JSON row carries `strict` strictness + `per-iter equality PASS`.
- **Margins** (`skinny/RESULTS.md`): Track 1 > sonic Skipper on every corpus — twitter parse_only 8349 vs 4913 Mbps, citm direct_to_struct 33366 vs 21250, canada parse_only 16709 vs 12970 (+45.4% vs simdjson DOM), marine_ik parse_only 9505 vs 5338 (+78%), apache_builds the thinnest at +1.4%. No broadcast (each row is its own corpus measurement). No corpus-name short-circuit in the measured path.

One caveat, not a contrivance: the `direct_to_struct`/`real_typed_struct` >SOTA rows ride the per-corpus bench-only typed schema (F-7). They are a fair speed comparison (sonic deserializes into the same typed struct), but their >SOTA is conditional on a hand-tuned per-corpus schema that does not generalize. The `parse_only` rows (generic tape path) are the unconditional, generalizable >SOTA proof.

## Inflection-Point Assessment for JSON

**NOT READY to backtrack to fully grammar-driven. The parse + value API are PERFECTED and >SOTA, but they are hand-written, and the "codegen" is a fixed-template emitter, not a generator.**

| Gate | Status |
|---|---|
| JSON parse perfected | YES (regen-clean, all corpora admitted) |
| JSON value API perfected | YES (lazy `ValueRef` tape projection, rich) |
| JSON >SOTA (cold, strict, N≥30/50) | YES (parse_only unconditional; typed conditional on bench schema) |
| Substrate-neutral (shared tape w/ CSS) | YES (same `Tape`/`ValueRef`, no JSON-only substrate) |
| **Parser is grammar-DERIVED** | **NO — fixed template in `json_sink_direct.rs`** |
| **Value API is grammar-DERIVED** | **NO — hand-authored `JsonValue` + include_str! templates** |

What is hand-written vs grammar-derived:
- **Hand-written (everything load-bearing)**: the byte-dispatch recursive-descent parser (`json_sink_direct.rs` template literals), the `parse_only` recognizer (`JSON_PARSE_ONLY_GENERATED_RS` const), the `JsonValue`/`JsonNodeKind`/view/visitor data model (`json_templates/`), the `value_from_ref` projection.
- **Grammar-derived (nothing load-bearing)**: only `validate()` consumes the grammar — it checks the `SinkOnlyProgram` has the expected entry rule, non-empty shapes, and String/Number/Whitespace span kinds. The grammar gates emission; it does not shape it.

Concrete gap to fully-grammar-driven JSON (SK-V18 backlog): a generator that, from `json.bbnf`'s 7 rules, (1) derives the `JsonValue` enum from the `value` alternation, (2) derives the byte-dispatch from each alternand's FIRST-set, (3) derives object/array recursion from the `members`/`elements` rules, (4) derives the number/string scanners from the two `/regex/` leaves. The hand-written template is the reference output that generator must reproduce — which is the correct order (prove the target, then generate it), but the generator does not yet exist.

## Prune / Course-Correct Recommendations for SK-V18

- **PR-1 [MEDIUM]**: Rename `parse_w11_1_number_*` → `parse_number_*` in `json_sink_direct.rs` (F-3). Re-regen `runtime/.../json/generated.rs`. Removes the bench-wave-id metalanguage leak from shipped production code.
- **PR-2 [HIGH, the inflection wave]**: SK-V18 should be a REBUILD wave that makes `json_sink_direct::render` actually consume the `SinkOnlyProgram` structure — derive the byte-dispatch FIRST-sets, the `JsonValue` enum, and the container recursion FROM the grammar, with the current hand-written template as the byte-for-byte parity target. This is the user's stated inflection point ("backtrack and generalize at THAT inflection"). JSON has met parse+value-API+>SOTA; the only unmet gate is derivation.
- **PR-3 [MEDIUM, carry V1-PR4]**: the typed-schema bench path (`xtask/real_typed_schema.rs`) must become a JSON-Schema-ingested input, not a 1014-line hand-coded Rust fn with per-corpus capacity literals, before the typed `direct_to_struct`/`real_typed_struct` >SOTA rows can be claimed as grammar-general rather than per-corpus-tuned.
- **PR-4 [LOW, carry V1-PR2]**: the FNV-64 closed-enum bench pattern (still bench-only, F-6) must gain a non-enum arbiter before any future promotion attempt; flag a guard so it cannot land in `runtime/`.

## Forward-lens note for SK-V18 S-P0

1. The "one codegen path" memory (`one-codegen-path`) and the `@generated` headers create the IMPRESSION of grammar-driven codegen. S-P0 must read `json_sink_direct.rs` / `CSS_GENERATED_RS` and verify whether the emitter PROJECTS the grammar or merely VALIDATES-then-templates. At this HEAD it is the latter for both grammars. Do not let RESULTS.md's "generated Track 1" phrasing imply derivation.
2. Watch for the inverse risk on the inflection wave: a generator that special-cases JSON/CSS shapes internally (a `match grammar_name`) would re-introduce the pre-restart divergence in a new place. The generalization must be FIRST-set/alternation-driven, corpus-blind.
3. The `parse_w11_1` leak (F-3) is the canary: any wave-id, corpus-name, or commit-tag appearing in a SHIPPED `runtime/` symbol is a regen-discipline failure. S-P0 should add a grep gate: shipped runtime files contain no `w[0-9]+`, no corpus names, no `sk_v` tags.
4. The typed-schema >SOTA rows are the soft spot: their margins (citm +57%, canada +74%) are the headline numbers but rest on per-corpus hand-tuning. S-P0 should require the consolidated narrative to distinguish the unconditional `parse_only` >SOTA from the schema-conditional typed >SOTA.

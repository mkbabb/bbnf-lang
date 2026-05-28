# AUDIT-6: Cross-Cutting Backend / Substrate Specialisation

Date: 2026-05-26. HEAD: `8e7378025`. Hard cap: 30 min.

Concern (verbatim): "Ensure we're not overfitting... once we perfect our parsing + value API for both CSS and JSON and >SOTA for each, we can backtrack and then generalize to be fully grammar driven. This should be done at that exactly inflection point."

Axis: verify substrate / backend / value-API neutrality. Distinguish legitimate per-grammar shape *selection* from grammar-aware shape *implementation* — the latter is dispositive.

## Findings

### F-1 CRITICAL — GRAMMAR-CREEP-PRESENT — Codegen runtime pipeline is hardcoded to one of two grammar-named modes; non-JSON / non-CSS-L4 grammars are rejected at compile time

`skinny/crates/codegen/src/grammar_profile.rs:89-100` enumerates exactly 8 hardcoded grammar profiles, and dispatch (`runtime_generator.rs:19-30`) routes on `RuntimeGenerationMode::{PassCompiled, FrontendFacts}`. `PassCompiled` is taken **only** by `"json"` (`grammar_profile.rs:102-115`); `FrontendFacts` is taken **only** by the seven `css_l4_*` profiles (`grammar_profile.rs:117-199`). `select_runtime_profile_for_name` returns `CodegenError::Lowering("runtime emission currently supports grammar profiles [...] found ...")` for anything else (`grammar_profile.rs:60-68`). This is the "8 enum variants" of generic-crate-leak the user calls out.

### F-2 CRITICAL — GRAMMAR-CREEP-PRESENT — Codegen "JSON" path is a static-template emit, not grammar-driven

`runtime_generator.rs:32-79` (`emit_compiled`) loads `json_templates/{generated,parser,value,view,visitor,config}.rs` via `include_str!`, plus hand-written constants `JSON_PARSE_ONLY_GENERATED_RS`, `JSON_PARSE_ONLY_PARSER_RS`, `JSON_HOST_RS`, `JSON_MOD_RS` (`runtime_generator.rs:253,608,652,630`). The only grammar-derived contribution is `json_sink_direct::render(sink_only)` appended to `generated.rs`. The "json" grammar is not so much compiled as templated — substituted into ~1.1 KLOC of pre-baked Rust. `crates/codegen/src/json_templates/` is 1149 LOC of pre-baked JSON-API source (value.rs 172, view.rs 460, generated.rs 391, visitor.rs 38, parser.rs 69, config.rs 19).

### F-3 CRITICAL — GRAMMAR-CREEP-PRESENT — Codegen "CSS L4" path emits a static fact-stream stub; the seven L4 "generated.rs" outputs are byte-identical

`runtime_generator.rs:81-105` (`emit_frontend_facts`) outputs `CSS_GENERATED_RS`, `CSS_MOD_RS`, `CSS_PARSER_RS`, `CSS_SINK_RS` (constants at lines 713, 656, 666, 695) — all hand-written templates. `runtime_generator.rs:114-153` (`css_profile_config`) hard-codes seven CSS-profile id literals, each with three hand-tailored string constants (`fact_schema`, `row_id`, `output_plane`). The seven `runtime/src/grammars/css_l4_*/generated.rs` files are byte-identical 646-line copies of the same template (`diff -q` confirms; `wc -l` confirms). That template embeds a 290-line hand-written CSS parser (`CssFullParser`, `runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:103-395`) — the BBNF grammar input is never consulted; only its source hash and fact-schema label are surfaced.

### F-4 HIGH — GRAMMAR-CREEP-PRESENT — `BackendShape` lower-implementations are stubs except for `SinkOnly`; only the JSON path is real

`codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs` are each 17-line files whose `lower_rule` body is `format!("rule {} -> shape_name", rule.name)` (e.g. `eager_tape.rs:15-17`). Only `lower/sink_only.rs` (270 lines) is real and only it is consumed (`lib.rs:175-181` requires `sink_only_program` to exist, otherwise codegen errors). The 5-shape canon is *advertised* (`ir/src/lib.rs:339-346`, `cost::all_backend_shapes() = [EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage]` at `ir/src/cost.rs:333-341`) and the priority table touches 8 `PriorityStep` rungs (`ir/src/cost.rs:262-285`), but the lowering pipeline is single-shape monoculture in disguise.

### F-5 HIGH — GRAMMAR-CREEP-PRESENT — `DecisionCspFacts` carries grammar-named status fields and a self-declared no-op block

`ir/src/cost.rs:242-243`: `static_css_provider_status: String, json_sink_only_status: String` — both grammar-named scalars on the generic CSP facts struct. `decision_csp.rs:151` sets `same_wave_consumer_class: "gate_json_decision_csp_cascade_contract"`; `decision_csp.rs:162-163` records `"static-template-blocker"` and `"sink-only-static-blocker"` (literal acknowledgement that the static-template codegen path defeats the CSP cascade); `decision_csp.rs:166` sets `block_id: Some("JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT")` — an in-tree confession that the CSP machinery executes but produces no runtime row movement (i.e. the decision layer does not drive what gets emitted).

### F-6 HIGH — GRAMMAR-CREEP-PRESENT — Active-cost e-graph is degenerate (zero rewrites)

`backend_egraph.rs:66`: `let rules: [&dyn RewriteFn<DecisionNode, NoAnalysis>; 0] = []; scheduler.run(&mut graph, &rules);`. The "e-graph" runs with an empty rewrite set, which collapses to a sort by `DecisionCost`. CSP `decision_csp.rs:54-83` adds 5 constraints whose substrate predicate `matches!(candidate.shape, EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage)` is tautological (`decision_csp.rs:67-77`). The "Decision Engine" in the SPEC sense (`same_substrate_union` enforcement + candidate generation + eqsat + CSP + active cost) is wired but degenerate — its outputs are never read by the codegen runtime templates, only stamped into `config.rs` strings for verification.

### F-7 HIGH — GRAMMAR-CREEP-PRESENT — Value API exists for JSON only; CSS has no Value API

`runtime/src/grammars/json/` has `value.rs` (`JsonValue`, `JsonNodeKind`), `view.rs`, `visitor.rs`, `host.rs`. `JsonValue` is a concrete JSON enum (`runtime/src/grammars/json/value.rs:68-76`: `Object|Array|String|Number|Bool|Null`). No `Value` trait, no `Document` trait — value API is a per-grammar enum. The seven `runtime/src/grammars/css_l4_*/` directories ship only `config.rs`, `generated.rs`, `mod.rs`, `parser.rs`, `sink.rs` (`grammar_profile.rs:119-127`). The CSS "parser" emits a tab-separated string of fact rows, not a typed value. The user's request — "perfect our parsing + value API for both CSS and JSON" — is materially unmet on the CSS side: there is no CSS value API.

### F-8 MEDIUM — GRAMMAR-CREEP-PRESENT — `TapeKind`, `SpanKind`, `JsonNodeKind` are JSON-leaning

`ir/src/lib.rs:429-440`: `TapeKind::{Container, Sequence, KeyValuePair, StringValue, NumberValue, BoolValue, NullValue, Member, Element}` — `Member`, `Element`, `KeyValuePair`, `Bool`/`Null` are JSON object/array vocabulary. `SpanKind::{String, Number, Whitespace}` (`ir/src/lib.rs:416-421`) reflects JSON's three scalar surface kinds. These propagate into shape facts; tests at `passes/src/lib.rs:1766-1779` curate JSON shape names (`JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`) as the canonical TapeKind→shape mapping. The vocabulary is not catastrophic (a CSS grammar can map declarations to `KeyValuePair`), but it shows JSON-first design pressure.

### F-9 MEDIUM — GRAMMAR-CREEP-PRESENT — `grammar::parse_json_grammar` is a JSON-named alias on the parse-grammar entrypoint

`grammar/src/lib.rs:16-27`: `parse_json_grammar(source)` and `load_json_grammar(path)` are JSON-named conveniences that delegate to `parse_grammar("json", source)`. They are still used in passes tests (`passes/src/lib.rs:1611,1649,1705,1739,1751,1759,1863`). No grammar-specific parsing is done — but the named entrypoint betrays a one-grammar-first design and is the only loader with an `fs` helper.

### F-10 NEUTRAL — Lock 1 substrate-union (no cross-call retained state) holds for classifier primitives

Sampled `bbnf-simd/src/{classifier.rs, scalar/*, aarch64/*, dispatch.rs}`. The only `OnceLock`/`thread_local!`/`static mut`/`RefCell` in the whole crate is `bbnf-simd/src/dispatch.rs:59` (`static KERNELS: OnceLock<PrimitiveKernels>`), which is architecture detection (CPU feature → kernel table) and grammar-agnostic. The transient classifier-state surface is clean: no per-grammar retention crept in. LAC-2F-V5-02 holds at the primitive layer.

### F-11 NEUTRAL — `BackendShape` enum honoured at 5 variants; `FactStream` is not a 6th variant

`ir/src/lib.rs:339-346` declares exactly five `BackendShape` variants. `FactStream` exists only as a `SubstrateTarget::AdmittedFactOutput` enum value (`ir/src/cost.rs:61,70`) plus the `Lock1PolicyTriad::fact_stream()` constructor (`ir/src/cost.rs:139-146`). There is **no `FactStream` struct, no row schema, no `PrimitiveFacts`/`CostFacts`-style record type for fact-stream rows** — Lock-1 substrate-manifest taxonomy is *named* but not *typed*. The CSS L4 runtime templates emit `\t`-separated strings ad-hoc; no shared schema.

## 5-Shape BackendShape Implementations — Grammar-Name Audit

| shape          | impl path                                            | grammar-named fields/methods                                                          | verdict          |
|----------------|------------------------------------------------------|---------------------------------------------------------------------------------------|------------------|
| EagerTape      | `codegen/src/lower/eager_tape.rs:15-17`              | none — 17-line stub returning `format!("rule {} -> eager_tape", rule.name)`           | SCAFFOLD (stub)  |
| OffsetTape     | `codegen/src/lower/offset_tape.rs:15-17`             | none — 17-line stub returning `format!("rule {} -> offset_tape", rule.name)`          | SCAFFOLD (stub)  |
| EventTape      | `codegen/src/lower/event_tape.rs:15-17`              | none — 17-line stub returning `format!("rule {} -> event_tape", rule.name)`           | SCAFFOLD (stub)  |
| SinkOnly       | `codegen/src/lower/sink_only.rs:1-270`               | **load-bearing**, consumed by `json_sink_direct::render` (`runtime_generator.rs:46`); see `json_typed_direct` 1245 LOC | LOAD-BEARING; JSON-routed downstream |
| CollapsedStage | `codegen/src/lower/collapsed_stage.rs:15-17`         | none — 17-line stub returning `format!("rule {} -> collapsed_stage", rule.name)`      | SCAFFOLD (stub)  |

**Conclusion:** four of five lowering files are 17-LOC strings; one is real but feeds a JSON-named renderer (`json_sink_direct`, `json_typed_direct`). The grammar-aware logic is downstream of `SinkOnly`, not inside it — but the JSON-only consumer means the substrate is JSON-coupled in effect.

## FactStream Schema Audit

**No FactStream struct, schema, or row type exists.** The substrate-manifest 5th category named in LAC-1E-14 is reduced in code to:

- `SubstrateTarget::AdmittedFactOutput` (`ir/src/cost.rs:61`) — a string-tagged enum variant.
- `Lock1PolicyTriad::fact_stream()` (`ir/src/cost.rs:139-146`) — a constructor that returns `{AdmittedFactOutput, OutputRow, GeneratedGrammar}`.
- Per-CSS-profile string constants (`runtime_generator.rs:114-153`): `fact_schema`, `row_id`, `output_plane` — three labels per CSS L4 profile.
- Runtime emitters (`runtime/src/grammars/css_l4_*/generated.rs`) writing tab-separated key=value strings ad-hoc; no shared row schema; no validation.

No grammar-specific fields, because *no fields exist at all*. This is a definitional gap, not a leak — but it means the FactStream "substrate" is unimplemented past its policy-triad label. The user-visible product is a string blob produced by hand-written, profile-keyed printers.

## Value API Isomorphism

| grammar                  | document.rs / value.rs / view.rs API surface                                                                                                                                                                       | matches others?                                                  |
|--------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------|
| json                     | `value.rs` (172 LOC: `JsonValue`, `JsonNodeKind`, span/canonical-string helpers) + `view.rs` (460 LOC: `JsonObject/Array/String/Number/Bool/Null`, `JsonPair`, `JsonRoot`, `JsonDocument`) + `visitor.rs` + `host.rs` | N/A baseline                                                     |
| css_l4_declaration_values        | only `config.rs/generated.rs/mod.rs/parser.rs/sink.rs`; no value, view, visitor                                                                                                                            | **NO** — no value API                                            |
| css_l4_declaration_values_extended | same five files; no value API                                                                                                                                                                          | **NO**                                                           |
| css_l4_stylesheet_selectors      | same five files; no value API                                                                                                                                                                            | **NO**                                                           |
| css_l4_visual_functions          | same five files; no value API                                                                                                                                                                            | **NO**                                                           |
| css_l4_at_rules_and_media        | same five files; no value API                                                                                                                                                                            | **NO**                                                           |
| css_l4_vendor_and_custom_atrules | same five files; no value API                                                                                                                                                                            | **NO**                                                           |
| css_l4_nested_layout             | same five files; no value API                                                                                                                                                                            | **NO**                                                           |

There is no `Value` trait, `Document` trait, or shared visitor abstraction. `JsonValue` is a concrete enum (`json/value.rs:68-76`). The seven CSS L4 grammars do not expose typed values; their `parse()` returns `Result<String, CssFactError>` of a fact-stream string (`runtime/src/grammars/css_l4_stylesheet_selectors/parser.rs:6`). The user's gating condition — "perfect our parsing + value API for both CSS and JSON" — is materially unmet on the CSS axis.

## Decision Engine Status

**SCAFFOLD with degenerate kernels.** Components exist:

- `passes::backend_egraph::select` (`backend_egraph.rs:36-134`) — e-graph that runs `scheduler.run(&mut graph, &rules)` with `rules = [&dyn RewriteFn; 0] = []` (line 66). With zero rewrites, e-class merging only union-finds the candidate root, so extraction reduces to a sort by `DecisionCost`. Functional but vestigial.
- `passes::decision_csp::finalize_rule` (`decision_csp.rs:16-27`) — CSP with 5 lambda constraints, of which `substrate` admits all 5 shapes (`decision_csp.rs:67-77`, tautological), `parity` admits all (`|_| true`, line 54), `recognizer` accepts non-empty ids (line 60), `simd` filters out `CollapsedStage`, `capacity` accepts `capacity_cost ≤ 1`. CSP "passes" but the constraint set does not prune meaningfully against the priority-table output.
- `Csp<CostFiniteDomain>` solver (`csp_solver` crate, vendored from csc411 per `project_csp_solver.md`) — present and called; produces `selected_candidate_id` (`decision_csp.rs:109-114`), but the same value is re-extracted with `unwrap_or(active.facts.selected_candidate_id.clone())` if solver fails to converge.
- **No W7 commits**: `git log --oneline --grep='sk-v14-w7'` returns only `672b927d5 feat(sk-v14-waveW7): wire policy union facts into generated runtimes` and `283b66f1a test(sk-v14-waveW8): reject css l4 production readmit on fact-stream track1` — W7 wired *policy facts*, not the decision engine itself. PRUNE-5 wire-up the SPEC promised is not present.
- The decision-engine output (`DecisionCspFacts`) is recorded into per-rule `CostFacts` and validated structurally in `codegen::lower::rust::lower_to_rust` (`rust.rs:52-75`), but the validation is `csp_status == "sat" && csp_budget_status == "pass" && ...` — a check that the strings were stamped, not that the chosen shape changed anything. `block_id` literally says `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT` (`decision_csp.rs:166`).

**Verdict: SCAFFOLD. Load-bearing only as a gate ("must have CSP facts attached"), not as a chooser.** The static templates are the truth; the engine is ceremonial.

## Lock 1 Substrate-Union Verification (sample)

3 sampled transient classifier-state primitives in `bbnf-simd/src/`:

- `bbnf-simd/src/scalar/byte_class_from_table_64.rs` — pure function, no static state, takes `&[u8]` table + `&[u8]` input. Clean.
- `bbnf-simd/src/scalar/bitmap_next_set_bit.rs` — pure function, no static state, operates on `u64` bitmap. Clean.
- `bbnf-simd/src/scalar/bulk_emit_positions_64.rs` — pure function, no static state. Clean.
- `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs` — pure function, NEON intrinsics, no static state. Clean.
- `bbnf-simd/src/dispatch.rs` — has `static KERNELS: OnceLock<PrimitiveKernels>` at line 59. **This is CPU-feature kernel dispatch, not grammar-classifier state.** The kernel table is filled once per process from CPU detection and is grammar-agnostic.

**LAC-2F-V5-02 (no cross-call retained classifier state) holds at the primitive layer.** No per-grammar cache, no per-grammar arena state, no per-grammar lookup table. This is one of the few areas where the substrate-neutrality contract is genuinely honoured.

## Verdict

**MIXED — primitive substrate layer is neutral; codegen + runtime layer is GRAMMAR-CREEP-PRESENT to the point of being two single-purpose pipelines wearing 8-profile clothing.**

The decision-engine machinery (e-graph + CSP + priority table + cost facts + policy triad + same-substrate-union facts) is wired top-to-bottom, but emits into a codegen pipeline whose actual outputs are (a) JSON: 1149 LOC of pre-baked static templates + a `SinkOnly` lowering pass, and (b) CSS L4: a single 646-LOC hand-written CSS parser cloned seven times across profile directories. The substrate enum, the priority table, the BackendShape canon, and the lowering trait all exist as advertised, but four of the five `ShapeLowering` impls are 17-LOC stubs, and the codegen entrypoint hard-rejects every grammar except the 8 enumerated profiles (`grammar_profile.rs:60-68`). The in-tree `block_id = "JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT"` (`decision_csp.rs:166`) is a self-confession that the decision layer's facts are consumed but do not change emitted runtime code.

This is the inflection point the user named — and the answer is: **we are not yet at it**. The JSON path is templated, not generated. The CSS path is hand-written, not parsed. There is no shared value API; CSS has no value API at all. SOTA-vs-CSS comparisons are accordingly meaningless until CSS produces a typed value.

## Prune Recommendations (if any)

1. **PRUNE-S6.1 [hard]:** Delete the four 17-LOC `lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs` stubs. They give the illusion of 5-shape coverage. Either land real lowerings (matching the SinkOnly footprint) or collapse the trait to one variant and rename to its current truth (`SinkOnlyLowering`).
2. **PRUNE-S6.2 [hard]:** Delete `static_css_provider_status` and `json_sink_only_status` from `DecisionCspFacts` (`ir/src/cost.rs:242-243`). Grammar-named status fields on a generic CSP record are leaks; replace with a single `substrate_blocker_status` neutral enum if the signal is needed.
3. **PRUNE-S6.3 [hard]:** Delete the `block_id` field from `DecisionCspFacts` if "JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT" is permanent; or fix the underlying no-op and remove the marker. Currently it is a TODO carrying a permanent-record name.
4. **PRUNE-S6.4 [medium]:** Delete `parse_json_grammar` / `load_json_grammar` from `grammar/src/lib.rs:16-27`. They are JSON-named aliases on `parse_grammar`. Migrate callers in `passes/src/lib.rs:1611-1863` to `parse_grammar("json", ...)`.
5. **PRUNE-S6.5 [medium]:** Replace `BackendExpr::TapeEmit { kind: TapeKind }` with grammar-neutral tape vocabulary — `TapeKind::{KeyValuePair, Member, Element, BoolValue, NullValue}` are JSON nouns. Rename to `Container/Sequence/Pair/Leaf{String,Number,Bool,Null}/Item/Field` or similar substrate-neutral terms (`ir/src/lib.rs:429-440`).
6. **PRUNE-S6.6 [soft]:** Delete the empty rewrite-rule array at `backend_egraph.rs:66` and the egraph machinery if it never gains rewrites; replace with a direct `extractor::best_node` over a `Vec<DecisionCost>::min` — the e-graph is currently a no-op wrapper.
7. **PRUNE-S6.7 [hard]:** The seven byte-identical `runtime/src/grammars/css_l4_*/generated.rs` files (646 LOC × 7 = 4522 LOC duplicated) should be one file shared between the CSS L4 profiles, or — preferably — deleted in favour of a real grammar-driven emitter once it exists. The current duplication masks a single hand-written CSS parser as "seven generated runtimes".

## Inflection-point assessment

The user's quoted gate: *"once we perfect our parsing + value API for both CSS and JSON and >SOTA for each, we can backtrack and then generalize to be fully grammar driven."*

Where we actually are:

- **JSON parsing:** real, sink-only-routed, perf-credible (per other audit axes).
- **JSON value API:** real (`JsonValue` enum + view/visitor surfaces) but pre-baked, not generated from the grammar — it ships as static templates in `crates/codegen/src/json_templates/`. So the value API exists but it is not the artefact of the BBNF compiler; it is a hand-written library that the compiler stitches to a small generated `sink_direct` blob.
- **CSS L4 parsing:** hand-written 290-LOC `CssFullParser` embedded in a 646-LOC template, cloned seven times. Real parsing happens, but the BBNF grammar is not consulted past hashing.
- **CSS L4 value API:** does not exist. `parse()` returns `Result<String, CssFactError>` — a tab-separated fact-stream. No types, no enums, no traversal.
- **>SOTA on each:** not assessable on CSS — comparing a hand-rolled bracket counter to lightningcss / cssparser is apples-to-fact-strings.

**The inflection point has not been reached.** The premise for generalization (two complete, parity-verified grammar pipelines) is half-met (JSON only). Generalizing now would be premature: there is no second worked example whose pattern the engine could abstract over, and the "second" example (CSS L4) is the wrong artefact shape entirely (string emit, not typed value). The honest path is to (a) build a real CSS value API to parity with the JSON one, (b) drive both from the BBNF source rather than static templates, (c) then look back and identify the shared substrate. Doing the generalize-and-then-fit dance before CSS has a value type will entrench more JSON-coupled abstractions.

## Forward-lens note for the next S-P0

Three things the next S-P0 should treat as binding inputs:

1. **The `8 enum variants` leak is here.** It is `grammar_profile.rs::runtime_profiles()` returning a `[&'static GrammarProfile; 8]` of literal-named profiles. Any candidate that proposes "generalize the dispatcher" must show the array shrinking to zero or to a single grammar-neutral entry, not to a different fixed N. (`skinny/crates/codegen/src/grammar_profile.rs:89-100`)
2. **The `block_id: JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT` is the self-evidence.** Treat it as a binding artefact of the audit: the codebase has already admitted (in its own typed cost facts) that the decision cascade does not move rows. Any plan that proposes "extend the decision engine" must first either delete this block_id and explain how the cascade now drives emission, or accept that the cascade is bookkeeping and act accordingly. (`skinny/crates/passes/src/decision_csp.rs:166`)
3. **The CSS-value-API hole is the real gate.** SOTA comparisons on CSS L4 today are not meaningful — the runtime emits tab-separated facts, not parsed values. The user's quoted inflection-point gate cannot be evaluated for CSS until a typed CSS value tree exists; until then any "we beat lightningcss" claim is comparing fact-stream emit cost to parse cost. The next S-P0's first question should be: what is the CSS Value type? — and the answer should not be "doesn't exist yet".

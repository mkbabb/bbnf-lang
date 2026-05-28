---
agent: 2A
pass: T-P2-research
cycle: V2
generated_at: 2026-05-28T02:35:46-04:00
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
primary_sources_cited: 18
techniques_grounded: 8
techniques_refuted: 7
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - "CH3-V1-01: LAC-04 now fences retained cursor/list/class-column/sidecar routes as REDRESS-refuted for SK-V15."
    - "CH4-V1-05: transferred parser primitive candidates now carry LOC/risk/wave-owner/hard-cap cost gates."
    - "CH6-V1-02: grounded SOTA rows now carry inline transfer, admission, verification, and close-status gates."
  first_cycle_additions: [T2A-V1-SOTA-JSON-001, T2A-V1-SOTA-JSON-002, T2A-V1-SOTA-JSON-003, T2A-V1-SOTA-JSON-004, T2A-V1-SOTA-JSON-005, T2A-V1-SOTA-CSS-001, T2A-V1-SOTA-CSS-002, T2A-V1-SOTA-CSS-003, T2A-V1-REFUTE-CSS-001, T2A-V1-REFUTE-CSS-002, T2A-V1-REFUTE-CSS-003, T2A-V1-REFUTE-CSS-004, T2A-V1-REFUTE-JSON-001]
locks_amendment_candidates: 5
sk_cycle: SK-V15
t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z
implementation_floor: PASS-IMPL-V1-CSS-CONTRIVANCE-JSON-HONEST
host_close_route: Apple-M5-Max-aarch64
stale_sk_v14_material_reused: none
---

## Executive Summary

The SK-V15 parsing landscape has one defensible JSON story and one refuted CSS
story. JSON claims can remain live only at the workload plane actually measured:
parse-only, direct-to-struct, and real typed product rows in
`skinny/RESULTS.md:3`-`31`, with same-run strict Rust comparators and Apple M5
Max/aarch64 as the close route. simdjson grounds DOM/tape and On-Demand lazy
iterator architectures; sonic-rs grounds direct typed deserialisation and
targeted SIMD leaves; yyjson grounds a non-SIMD DOM/value baseline that can be
SOTA on selected corpora. None of those sources justify a retained sidecar or
cross-workload comparison.

CSS is different. Current CSS evidence is a 24-row broadcast from one aggregate
measurement, a four-counter `CssFullParseSummary`, and fact-stream/string
outputs. Primary cssparser and lightningcss sources show richer declaration,
rule, typed-property, stylesheet, and CSSOM-like workloads. Therefore SK-V15
must treat current CSS >SOTA claims as refuted until W5 emits typed CSS
value/document/view/visitor output and W6 retimes against same-run cssparser on
that same workload. lightningcss is a diagnostic CSSOM/value comparator until
Track 1 emits comparable CSSOM/value output.

## Technique Grounding Table

| spec claim or T-P1 divergence id | published source cited | state | bbnf-specific note |
|---|---|---|---|
| T2A-V1-SOTA-JSON-001: simdjson separates stage 1 structural/UTF-8 discovery from stage 2 tape construction. | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, arXiv:1902.08318; simdjson `doc/parse_many.md:54`-`57` @ `79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5`, `https://github.com/simdjson/simdjson/blob/79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5/doc/parse_many.md#L54-L57`. | grounded | transfer_reason=transient structural projection can inform same-loop masks consumed by one DOM/tape builder; comparator_plane=DOM/tape architecture only; admission_gate=row-local equality/timing on Apple M5 Max/aarch64 with no retained cursor/list/class-column/sidecar and no public substrate expansion; verification_action=prove generated consumer consumes masks into the existing substrate in the same loop and compare against scalar reference; close_status=diagnostic-only; primitive_cost=none in 2A unless split into a 2B/2E leaf. |
| T2A-V1-SOTA-JSON-002: simdjson On-Demand is a lazy forward iterator over source text, parsing values as used and skipping unused values. | simdjson `doc/basics.md:344`-`350` @ `79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5`, `https://github.com/simdjson/simdjson/blob/79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5/doc/basics.md#L344-L350`. | grounded | transfer_reason=lazy access is useful only as a workload-plane comparator; comparator_plane=lazy/value access, not DOM or typed direct; admission_gate=typed skip must read the single consumed substrate and pass row-local equality/timing on Apple M5 Max/aarch64; verification_action=measure same input with strict product equality and reject any retained cursor sidecar; close_status=diagnostic-only; primitive_cost=none in 2A. |
| T2A-V1-SOTA-JSON-003: sonic-rs rejects simdjson-style two-stage SIMD and uses targeted SIMD at long strings, float fraction parsing, field lookup, and whitespace skip. | sonic-rs `README.md:60`-`66` @ `03545a9530346fe279b674dd496e037d94204bc5`, `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L60-L66`. | grounded | transfer_reason=targeted leaves may transfer as grammar-neutral byte-set/string/number/lookup/trivia primitives; comparator_plane=primitive row plus same-wave consumer, not whole-parser SOTA; admission_gate=scalar_reference=existing generated scalar path or yyjson-shape scalar baseline, parity_or_checkasm=Lock 16 same-run parity, hardware_gate=Apple M5 Max/aarch64, same_wave_consumer=W2 Lock 16 primitive lane or W5 typed CSS provider, row_movement_target=JSON direct/typed or CSS typed row-local movement; verification_action=wire one leaf at a time, compare equality and timing against scalar on the same row, and reject JSON-named generic branches; close_status=source-present-unwired; loc_estimate=150-350 per leaf; risk_class=medium; wave_owner=W2 primitive owner; hard_cap_fit=yes only for one-leaf slices. |
| T2A-V1-SOTA-JSON-004: sonic-rs direct struct deserialisation is a same-workload typed comparator, distinct from untyped Value/DOM. | sonic-rs `README.md:78`-`90` @ `03545a9530346fe279b674dd496e037d94204bc5`, `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L78-L90`. | grounded | transfer_reason=direct struct deserialisation matches bbnf direct/typed product rows; comparator_plane=typed_direct only; admission_gate=same-run strict product equality and timing on Apple M5 Max/aarch64, never `sonic_rs::Value` for direct-to-struct rows; verification_action=recapture row-local typed-direct comparator with current telemetry fields and route non-typed Value comparisons to diagnostics; close_status=admissible-after-gate; primitive_cost=none unless a targeted leaf is extracted under T2A-V1-SOTA-JSON-003. |
| T2A-V1-SOTA-JSON-005: yyjson is an ANSI C, no-explicit-SIMD strict DOM/value parser with ILP/branch-predictor performance posture. | yyjson `README.md:10`-`18`, `README.md:73`-`78`, and `src/yyjson.h:736`-`744` @ `d60852703c0fab67d488a692c50ed67d18b467ef`, `https://github.com/ibireme/yyjson/blob/d60852703c0fab67d488a692c50ed67d18b467ef/README.md#L10-L18`, `https://github.com/ibireme/yyjson/blob/d60852703c0fab67d488a692c50ed67d18b467ef/README.md#L73-L78`, `https://github.com/ibireme/yyjson/blob/d60852703c0fab67d488a692c50ed67d18b467ef/src/yyjson.h#L736-L744`. | grounded | transfer_reason=scalar DOM/value baseline refutes SIMD-required SOTA and supplies a yyjson-shape scalar reference; comparator_plane=DOM/value, not typed_direct; admission_gate=same-run C sidecar on Apple M5 Max/aarch64 plus row-local equality/timing for the same materialized product; verification_action=build/run the sidecar in the bench harness before using it as an admission anchor; close_status=diagnostic-only until sidecar is present; primitive_cost=scalar-delegated baseline, no new bbnf LOC. |
| T2A-V1-SOTA-CSS-001: cssparser is a CSS Syntax parser foundation over borrowed `&str`, tokens, declaration values, rule bodies, and stylesheet iteration. | cssparser `src/lib.rs:12`-`28`, `src/rules_and_declarations.rs:20`-`56`, `:196`-`234`, `:321`-`358`, `:404`-`453` @ `4c49486494fb24dc01390e3baca9698ef1744c71`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/lib.rs#L12-L28`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/rules_and_declarations.rs#L20-L56`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/rules_and_declarations.rs#L196-L234`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/rules_and_declarations.rs#L321-L358`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/rules_and_declarations.rs#L404-L453`. | grounded | transfer_reason=near-term CSS comparator for declaration/rule/stylesheet typed products; comparator_plane=CSS_typed_document; admission_gate=W5 must emit typed value/document/view/visitor output and W6 must run row-local equality/timing against cssparser on Apple M5 Max/aarch64; verification_action=record `css_comparator_workload` and reject fact-stream/four-counter substitutions; close_status=source-present-unwired; primitive_cost=provider cost lives in 2C/2F, no parser leaf adopted here. |
| T2A-V1-SOTA-CSS-002: cssparser supports full consumption and nested-block/value parsing, so "brace count" is below the comparator workload. | cssparser `src/parser.rs:256`-`264`, `:695`-`701`, `:780`-`788`, `:1122`-`1151` @ `4c49486494fb24dc01390e3baca9698ef1744c71`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/parser.rs#L256-L264`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/parser.rs#L695-L701`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/parser.rs#L780-L788`, `https://github.com/servo/rust-cssparser/blob/4c49486494fb24dc01390e3baca9698ef1744c71/src/parser.rs#L1122-L1151`. | grounded | transfer_reason=full consumption and nested-block parsing define the minimum CSS comparator workload; comparator_plane=CSS_typed_document/nested_value; admission_gate=row-local equality/timing on Apple M5 Max/aarch64 after typed Track 1 output exists, with brace counters excluded; verification_action=compare nested values/rules, not declarations or marker counts alone; close_status=source-present-unwired; primitive_cost=provider cost lives in 2C/2F. |
| T2A-V1-SOTA-CSS-003: lightningcss parses a stylesheet into a rule list and typed property/value structures, then serializes/minifies from that model. | lightningcss `README.md:10`-`12`, `src/stylesheet.rs:74`-`91`, `:122`-`207`, `src/properties/mod.rs:1`-`18`, `:81`-`89` @ `ec165294750bb02903e7f845b66533b0465debcc`, `https://github.com/parcel-bundler/lightningcss/blob/ec165294750bb02903e7f845b66533b0465debcc/README.md#L10-L12`, `https://github.com/parcel-bundler/lightningcss/blob/ec165294750bb02903e7f845b66533b0465debcc/src/stylesheet.rs#L74-L91`, `https://github.com/parcel-bundler/lightningcss/blob/ec165294750bb02903e7f845b66533b0465debcc/src/stylesheet.rs#L122-L207`, `https://github.com/parcel-bundler/lightningcss/blob/ec165294750bb02903e7f845b66533b0465debcc/src/properties/mod.rs#L1-L18`, `https://github.com/parcel-bundler/lightningcss/blob/ec165294750bb02903e7f845b66533b0465debcc/src/properties/mod.rs#L81-L89`. | grounded | transfer_reason=CSSOM/value pressure source for future richer typed output; comparator_plane=CSSOM_value, not fact_stream or four-counter summary; admission_gate=Track 1 must first emit comparable CSSOM/value output, then row-local equality/timing on Apple M5 Max/aarch64; verification_action=keep lightningcss diagnostic until the product plane matches; close_status=source-present-unwired; primitive_cost=provider cost lives in 2C/2F. |
| T2A-V1-REFUTE-CSS-001: current CSS 24-row admit is one measurement broadcast. | PASS-IMPL audit `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`29`; W8 bench source `skinny/crates/bbnf-bench/src/css_l4_w8.rs:16`-`18`, `:139`-`:144`, `:206`-`259`; `skinny/RESULTS.md:112`-`135`. | refuted | transfer_reason=none; comparator_plane=broadcast aggregate; admission_gate=each row needs independent command, input, equality, and timing on Apple M5 Max/aarch64; verification_action=demote shared tuple to diagnostic aggregate unless row-local typed measurements exist; close_status=refuted. |
| T2A-V1-REFUTE-CSS-002: current Track 1 CSS "full_parse" output is a four-counter summary, not typed CSS value/document/CSSOM. | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:50`-`59`, `:61`-`100`; generator source `skinny/crates/codegen/src/runtime_generator.rs:713`-`750`, `:766`-`810`. | refuted | transfer_reason=none; comparator_plane=four-counter summary below CSS_typed_document/CSSOM_value; admission_gate=typed CSS value/document/view/visitor output plus row-local equality/timing required before any close; verification_action=reject `CssFullParseSummary` as a comparator product; close_status=refuted. |
| T2A-V1-REFUTE-CSS-003: current fact-stream `parse()` is not a CSS Value API. | PASS-IMPL audit `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56`-`58`; SK-V15 spec `restart/skinny/tranches/sk-v15/SPEC.md:54`-`63`, `:336`-`349`. | refuted | transfer_reason=none; comparator_plane=fact_stream only; admission_gate=grammar-derived typed CSS API with row-local equality/timing on Apple M5 Max/aarch64; verification_action=keep fact streams as planning/provenance artifacts until W5 rebuilds typed surfaces; close_status=refuted. |
| T2A-V1-REFUTE-CSS-004: lightningcss cannot be an admitting comparator for current Track 1 CSS. | SK-V15 spec `restart/skinny/tranches/sk-v15/SPEC.md:61`-`63`, `:86`-`98`; lightningcss `src/stylesheet.rs:183`-`207` and `src/properties/mod.rs:1`-`18`. | refuted | transfer_reason=none for current Track 1 output; comparator_plane=CSSOM_value vs summary/fact_stream mismatch; admission_gate=comparable CSSOM/value output plus row-local equality/timing required before use; verification_action=retain lightningcss as diagnostic pressure only; close_status=refuted. |
| T2A-V1-REFUTE-JSON-001: simdjson DOM, simdjson On-Demand, yyjson DOM, sonic typed direct, and bbnf parse-only/direct/typed are distinct workloads. | simdjson, sonic-rs, yyjson sources above; `skinny/RESULTS.md:3`-`31`, `:139`-`152`. | partial | transfer_reason=plane separation protects current JSON claims; comparator_plane=parse_only, DOM, value, typed_direct, and lazy remain distinct; admission_gate=row-local equality/timing on Apple M5 Max/aarch64 for the same materialized product, with absent C++ sidecars disallowed as anchors; verification_action=record plane in RESULTS/BENCH and demote cross-plane comparisons to diagnostics; close_status=partial-blocked. |

## Architectural Assertions Defended

1. **JSON same-plane comparator discipline is defensible.** `skinny/RESULTS.md:3`-`31` separates parse-only, direct strict product, and real typed product. The notes at `skinny/RESULTS.md:139`-`152` state that direct rows are strict product rows and absent C++ sidecars are not strict anchors. This matches the primary-source split: sonic-rs typed struct at `README.md:78`-`90`, simdjson DOM/tape at `parse_many.md:54`-`57`, simdjson lazy iterator at `basics.md:344`-`350`, and yyjson DOM/value strict parse at `README.md:10`-`18` plus `yyjson.h:736`-`744`.

2. **aarch64-first close route is required.** SK-V15 SPEC binds native Apple M5 Max/aarch64 for all 51 JSON rows at `restart/skinny/tranches/sk-v15/SPEC.md:51`-`53` and rejects native-platform mismatch at `:119`-`122`. x86/AVX-512 and absent C++ sidecars can inform diagnostics, but they cannot close SK-V15.

3. **Targeted parser primitives are admissible only with grammar policy and same-wave consumer.** sonic-rs grounds SIMD at four leaves, not a universal two-stage parser. For bbnf this defends byte-set classify, long-string scan, numeric fragment scan, field/property lookup, and whitespace/trivia skip as primitive candidates, but only under generated grammar facts and Lock 16 parity.

4. **cssparser is the correct near-term CSS retime anchor after typed Track 1 exists.** cssparser exposes declaration value parsing, rule-body iteration, stylesheet iteration, single declaration/rule parsing, nested blocks, and full-consumption checks. That is close to the W5/W6 target in `restart/skinny/tranches/sk-v15/SPEC.md:336`-`376`.

5. **lightningcss remains valuable, but on the CSSOM/value plane.** lightningcss builds a `StyleSheet` with rule list and typed property values. It should be retained as a diagnostic/full-CSSOM pressure source until Track 1 emits comparable CSSOM/value structures.

## Architectural Assertions Refuted

1. **Refuted: current CSS L4 has 24 independent >SOTA admits.** The audit records one broadcast tuple at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`29`. The W8 source hardcodes `W8_SELECTED_CSS_ROWS = 24` and turns one aggregate `measure_full_parse_profiles` result into `admitted_rows` at `skinny/crates/bbnf-bench/src/css_l4_w8.rs:16`-`18`, `:139`-`144`, `:206`-`259`. SK-V15 must demote these to one diagnostic aggregate or rebuild row-local typed measurements.

2. **Refuted: brace-counter/full-parse summary is same-workload CSS parsing.** The generated summary has four counters at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:50`-`59` and prints only those counters at `:91`-`99`. cssparser and lightningcss sources prove richer declaration/rule/value workloads. A four-counter summary cannot be compared to CSSOM/value output.

3. **Refuted: fact-stream output is a CSS Value API.** The PASS-IMPL audit states CSS `parse()` returns a tab-separated fact stream, not a value API, at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:56`-`58`. SK-V15 close conditions require typed value, document, view, and visitor surfaces at `restart/skinny/tranches/sk-v15/SPEC.md:54`-`63`.

4. **Refuted: lightningcss can close current CSS >SOTA.** lightningcss's workload is stylesheet/rule-list and typed property parsing (`src/stylesheet.rs:183`-`207`, `src/properties/mod.rs:1`-`18`). SK-V15 SPEC already says lightningcss counts only after comparable CSSOM/value output exists (`SPEC.md:61`-`63`). Current Track 1 lacks that output.

5. **Refuted: simdjson stage 1 justifies retained class columns or sidecars.** simdjson stage 2 builds the tape from stage 1 indexes (`doc/parse_many.md:54`-`57`). The published architecture supports transient projection consumed by one parser substrate, not retained parallel class/cursor streams.

6. **Refuted: SOTA JSON requires SIMD.** yyjson explicitly advertises ANSI C and no explicit SIMD (`README.md:10`-`18`) and reports DOM parse benchmarks on EC2 and iPhone A14 (`README.md:33`-`55`). SIMD can be useful, but scalar ILP remains a required baseline.

7. **Refuted: a DOM/value win transfers to direct typed or lazy/fact-stream rows.** The primary sources expose different materialization contracts. SK-V15 must keep DOM, value, typed, lazy, fact-stream, CSSOM, and parse-only comparisons separate.

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| UNKNOWN-1: What exact CSS typed output should W5 expose first: cssparser-style declaration/rule products, lightningcss-style typed property values, or both behind separate planes? | T-P3 should split planes explicitly: `cssparser_typed_document` for W6 admission, `lightningcss_cssom_value` for optional diagnostic parity. W5 must not collapse both into a four-counter summary. |
| UNKNOWN-2: Can bbnf direct/typed JSON rows keep their SK-V15-open margins after telemetry additions and CSS gate restoration? | W0 must recapture SK-V15-open on Apple M5 Max/aarch64 and W11 must maintain the >=98% JSON guard if behavior changes, per `SPEC.md:51`-`53` and `SPEC.md:368`-`372`. |
| UNKNOWN-3: Which sonic-rs primitive leaves transfer cleanly to CSS without JSON leaks? | Require per-primitive generated facts: quote/escape policy for CSS strings, number/unit terminators, identifier/property lookup alphabet, whitespace/comment policy. Any hardcoded JSON role in generic code is Lock 14 failure. |
| UNKNOWN-4: Does cssparser comparison need full stylesheet iteration or row-scoped declaration/value parsing for W6? | W6 should record `css_comparator_workload` and `value_plane`. Per-feature rows need row-local measurement and equality; aggregate rows must be diagnostic aggregate only. |
| UNKNOWN-5: What same-run sidecar support is needed for simdjson/yyjson on Apple M5 Max? | Treat current C++/C sidecars as absent unless built and run in the same bench harness. Until then, simdjson/yyjson are architectural baselines, not SK-V15 admission anchors. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | lock(s) | proposed amendment candidate | supporting evidence | disposition for T-P3 |
|---|---|---|---|---|
| T2A-V1-LAC-01 | Lock 8 / bench comparator plane | Add a workload-plane gate: every SOTA claim must declare one of `parse_only`, `DOM`, `value`, `typed_direct`, `lazy`, `fact_stream`, `CSS_typed_document`, or `CSSOM_value`. Cross-plane comparisons are diagnostic only. | simdjson DOM/tape and On-Demand sources; sonic-rs typed direct source; yyjson DOM/value source; cssparser/lightningcss CSS workload sources. | Promote into BENCH/RESULTS schema via `value_plane` and `css_comparator_workload`. |
| T2A-V1-LAC-02 | Lock 8 / SK-V15 telemetry | Add broadcast rejection: repeated throughput tuples across conceptual row IDs require `broadcast_group_id` and non-admit status unless each row has independent command, input, equality, and timing. | `skinny/crates/bbnf-bench/src/css_l4_w8.rs:139`-`144`, `:206`-`259`; audit `CONSOLIDATED-AUDIT.md:21`-`29`; SK-V15 SPEC telemetry fields `:100`-`:122`. | Bind W0/W1 gate. |
| T2A-V1-LAC-03 | Lock 14 / CSS provider honesty | CSS parser parity cannot close unless Track 1 emits typed CSS value/document/view/visitor output from grammar-derived generation; fact streams and four-counter summaries are diagnostic. | SK-V15 SPEC `:54`-`:63`, `:336`-`:376`; generated summary `generated.rs:50`-`100`; codegen string literal `runtime_generator.rs:713`-`750`. | Bind W5/W6; prohibit old CSS proof retirement before rebuild provider lands. |
| T2A-V1-LAC-04 | Lock 1 / substrate union | Retained cursor/list/class-column/sidecar routes are REDRESS-refuted for SK-V15. The only allowed shape is transient same-loop masks consumed into the existing substrate, or generated single-substrate consumption; any retained sidecar-like route requires a new Alpha/P1/SPEC contract, not row-local measurement alone. | simdjson `parse_many.md:54`-`57`; REDRESS retained-shape failures; SK-V15 SPEC forbidden sidecar vocabulary `restart/skinny/tranches/sk-v15/SPEC.md:136`-`143`; CH3-V1-01 fold requirement. | Promote as a pre-block before any SIMD parser primitive wave. |
| T2A-V1-LAC-05 | Lock 16 / scalar-first admission | No SIMD/ASM primitive reaches admission from citation alone; it needs scalar reference, same-run parity/checkasm where relevant, hardware gate, same-wave consumer, and row movement against a yyjson-shape scalar baseline. | sonic-rs targeted leaves; yyjson no-SIMD source; SK-V15 SPEC non-negotiable `restart/skinny/tranches/sk-v15/SPEC.md:136`-`137`; dispatch context deep-SIMD process rule. | Route to 2B/2E primitive manifest and W2 Lock 16 gate restoration. |

## Source Register

| id | source | use |
|---|---|---|
| SRC-SIMDJSON-PAPER | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, arXiv:1902.08318. | Two-stage structural-index/tape architecture. |
| SRC-SIMDJSON-PARSE-MANY | simdjson `doc/parse_many.md:54`-`57` @ `79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5`. | Stage 1/stage 2 grounding. |
| SRC-SIMDJSON-ONDEMAND | simdjson `doc/basics.md:344`-`350` @ `79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5`. | Lazy On-Demand workload grounding. |
| SRC-SONIC-RS | sonic-rs `README.md:60`-`90` @ `03545a9530346fe279b674dd496e037d94204bc5`. | Targeted SIMD leaves and direct typed workload. |
| SRC-YYJSON | yyjson `README.md:10`-`18`, `:33`-`:55`, `:73`-`:78`; `src/yyjson.h:736`-`744` @ `d60852703c0fab67d488a692c50ed67d18b467ef`. | No-explicit-SIMD strict DOM/value baseline. |
| SRC-CSSPARSER | cssparser `src/lib.rs`, `src/parser.rs`, `src/rules_and_declarations.rs` @ `4c49486494fb24dc01390e3baca9698ef1744c71`. | CSS Syntax, declaration/rule/stylesheet, nested-block workload. |
| SRC-LIGHTNINGCSS | lightningcss `README.md`, `src/stylesheet.rs`, `src/properties/mod.rs` @ `ec165294750bb02903e7f845b66533b0465debcc`. | CSSOM/value workload and typed property grounding. |
| SRC-RESULTS | `skinny/RESULTS.md:3`-`31`, `:112`-`:152`. | JSON same-plane rows and CSS broadcast ledger. |
| SRC-PASS-IMPL-AUDIT | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31`, `:56`-`:58`, `:81`-`:94`, `:100`-`:106`. | CSS contrivance and workload mismatch. |
| SRC-SK-V15-SPEC | `restart/skinny/tranches/sk-v15/SPEC.md:51`-`63`, `:86`-`:122`, `:336`-`:376`. | SK-V15 close route, comparator classes, telemetry, W5/W6 gates. |
| SRC-CSS-W8-SOURCE | `skinny/crates/bbnf-bench/src/css_l4_w8.rs:16`-`18`, `:139`-`:144`, `:206`-`:259`. | Broadcast measurement mechanism. |
| SRC-CSS-GENERATED | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:50`-`100`. | Four-counter CSS summary evidence. |
| SRC-CSS-GENERATOR-STRING | `skinny/crates/codegen/src/runtime_generator.rs:713`-`750`, `:766`-`:810`. | Hand-written CSS parser string literal and summary generation. |

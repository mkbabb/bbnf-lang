---
agent: 2A
pass: T-P2-research
cycle: SK-V18-T-P2
generated_at: 2026-06-01T19:10:00-04:00
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
primary_sources_cited: 24
techniques_grounded: 14
techniques_refuted: 8
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - "CH3-V1-01: LAC-04 fences retained cursor/list/class-column/sidecar routes as REDRESS-refuted (V2; carried)."
    - "CH4-V1-05: transferred parser primitive candidates carry LOC/risk/wave-owner/hard-cap cost gates (V2; carried)."
    - "CH6-V1-02: grounded SOTA rows carry inline transfer, admission, verification, and close-status gates (V2; carried)."
    - "CH1-V3-SK18-01: checkasm harness count corrected to 13 (12 single-kernel + 1 aggregate `checkasm_parity.rs`) + the `checkasm_common.rs` helper module (V3)."
    - "CH3-V3-S1: T2A-V1-SOTA-JSON-001 admission_gate now cross-references the admissible-side ledger prior REDRESS 50/51/53 by id (V3)."
  first_cycle_additions: [T2A-V1-SOTA-JSON-001, T2A-V1-SOTA-JSON-002, T2A-V1-SOTA-JSON-003, T2A-V1-SOTA-JSON-004, T2A-V1-SOTA-JSON-005, T2A-V1-SOTA-CSS-001, T2A-V1-SOTA-CSS-002, T2A-V1-SOTA-CSS-003, T2A-V1-REFUTE-CSS-001, T2A-V1-REFUTE-CSS-002, T2A-V1-REFUTE-CSS-003, T2A-V1-REFUTE-CSS-004, T2A-V1-REFUTE-JSON-001]
  skv18_additions: [T2A-V18-DAV1D-001, T2A-V18-DAV1D-002, T2A-V18-CSS-LAZY-001, T2A-V18-JSON-SONIC-001, T2A-V18-SONIC-LAZY-002, T2A-V18-ASMJSON-001, T2A-V18-REFUTE-001, T2A-V18-REFUTE-002]
locks_amendment_candidates: 5
skv18_locks_amendment_candidates: 0
sk_cycle: SK-V18
prior_sk_cycle: SK-V15
t_p1_entry_state: CONVERGED-V1-V5-HARDENED-NEAR-CONVERGED
implementation_floor: SK-V18-GENERALIZATION-CSS-LAZY-RICH-HONEST-JSON-STRICT-HONEST
host_close_route: Apple-M5-Max-aarch64-ONLY
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
| T2A-V1-SOTA-JSON-001: simdjson separates stage 1 structural/UTF-8 discovery from stage 2 tape construction. | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", The VLDB Journal 28(6), 2019, arXiv:1902.08318; simdjson `doc/parse_many.md:54`-`57` @ `79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5`, `https://github.com/simdjson/simdjson/blob/79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5/doc/parse_many.md#L54-L57`. | grounded | transfer_reason=transient structural projection can inform same-loop masks consumed by one DOM/tape builder; comparator_plane=DOM/tape architecture only; admission_gate=row-local equality/timing on Apple M5 Max/aarch64 with no retained cursor/list/class-column/sidecar and no public substrate expansion — this is the ADMISSIBLE-transient side of REDRESS 50/51/53 (`skinny/REDRESS.md:807`-`813`, the ledger entry that defines the admissible same-loop-mask line; the inadmissible retained-cursor/sidecar side is REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE`); verification_action=prove generated consumer consumes masks into the existing substrate in the same loop and compare against scalar reference; close_status=diagnostic-only; primitive_cost=none in 2A unless split into a 2B/2E leaf. |
| T2A-V1-SOTA-JSON-002: simdjson On-Demand is a lazy forward iterator over source text, parsing values as used and skipping unused values. | simdjson `doc/basics.md:344`-`350` @ `79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5`, `https://github.com/simdjson/simdjson/blob/79bbba3e3e7ef7c817e399ba3bccbd65238b8ce5/doc/basics.md#L344-L350`. | grounded | transfer_reason=lazy access is useful only as a workload-plane comparator; comparator_plane=lazy/value access, not DOM or typed direct; admission_gate=typed skip must read the single consumed substrate and pass row-local equality/timing on Apple M5 Max/aarch64; verification_action=measure same input with strict product equality and reject any retained cursor sidecar; close_status=diagnostic-only; primitive_cost=none in 2A. |
| T2A-V1-SOTA-JSON-003: sonic-rs rejects simdjson-style two-stage SIMD and uses targeted SIMD at long strings, float fraction parsing, field lookup, and whitespace skip. | sonic-rs `README.md:60`-`66` @ `03545a9530346fe279b674dd496e037d94204bc5`, `https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L60-L66`. | grounded | transfer_reason=targeted leaves may transfer as grammar-neutral byte-set/string/number/lookup/trivia primitives; comparator_plane=primitive row plus same-wave consumer, not whole-parser SOTA; admission_gate=scalar_reference=existing generated scalar path or yyjson-shape scalar baseline, parity_or_checkasm=Lock 16 same-run parity, hardware_gate=Apple M5 Max/aarch64, same_wave_consumer=the G5/G6 `runtime_simd` scan-retarget consumer (the only LIVE SK-V18 §8 primitive lane — the JSON direct product path is SCAN-FREE per S-P1, so a sonic-rs leaf has no SK-V18 JSON consumer; its only viable same-wave consumer is the G5/G6 CSS scan or a SK-V19 receiver), row_movement_target=CSS typed row-local movement (NOT a JSON row — JSON direct is scan-free); verification_action=wire one leaf at a time, compare equality and timing against scalar on the same row, and reject JSON-named generic branches; close_status=blocked (no SK-V18 consumer — the 94.1% CSS hot leaf per `SYNTHESIS-PROFILE.md:96-98` needs the eq-set/balanced scan family, NOT a sonic-rs float/string/lookup leaf; these leaves are SK-V19 receivers, NOT a present-but-unwired SK-V18 consumer); loc_estimate=150-350 per leaf, CONDITIONAL-on-a-profiled-CSS-string/number-leaf-hot (not a committed SK-V18 budget); risk_class=medium; wave_owner=SK-V19-receiver (no LIVE SK-V18 consumer; the SK-V15 `W2` primitive lane is retired and there is no SK-V18 G5/G6 hot leaf that needs a float/string/lookup primitive); rollback path=leave the leaf unwired (no shipped output depends on it); abrogate threshold=DEFER to SK-V19 rather than author an orphan kernel — no SK-V18 grammar consumer exercises the leaf; hard_cap_fit=n/a in SK-V18 (no SK-V18 consumer; a one-leaf slice would fit a ≤450 LOC band only if a CSS hot leaf ever needed it). |
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

5. **Refuted: simdjson stage 1 justifies retained class columns or sidecars.** simdjson stage 2 builds the tape from stage 1 indexes (`doc/parse_many.md:54`-`57`). The published architecture supports transient projection consumed by one parser substrate, not retained parallel class/cursor streams. This is the same retired thesis T2A-V1-LAC-04 names: REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` (`skinny/REDRESS.md:2795`-`2940`, finding `:2928`-`2933`) RETIRED the retained-cursor/sidecar route on the M5 Max, and REDRESS 50/51/53 (`:807`-`813`) draws the admissible-transient vs inadmissible-retained line — the in-body refutation here and the LAC-04 lock candidate name the same retired prior.

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
| T2A-V1-LAC-04 | Lock 1 / substrate union | Retained cursor/list/class-column/sidecar routes are REDRESS-refuted. The only allowed shape is transient same-loop masks consumed into the existing substrate, or generated single-substrate consumption; any retained sidecar-like route requires a new Alpha/P1/SPEC contract, not row-local measurement alone. The named retired thesis is REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` (`skinny/REDRESS.md:2795`-`2940`): two correctness-green SIMD-structural-cursor-into-the-retained-parse-loop implementations UNIFORMLY regressed every must-improve row on the M5 Max (the `:2928`-`2933` finding: the wide-issue scalar `consume_structural`/delimiter path is cheaper than materializing or streaming a SIMD cursor through retained parsing). REDRESS 50/51/53 (`:715`-`882`) draw the same line — the admissible side (`:807`-`813`) is the transient single-substrate consumption, the inadmissible side is a retained side-table/event-cursor/structural-mask cursor. The G6 transient-same-loop-skip is the ADMISSIBLE side of exactly that REDRESS-53 line. | simdjson `parse_many.md:54`-`57`; REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` retirement (`skinny/REDRESS.md:2795`-`2940`,`:2928`-`2933`), REDRESS 50/51/53 (`:715`-`882`,`:807`-`813`); SK-V18 SPEC Lock-1 one-substrate + forbidden relocated-seam clause `restart/skinny/tranches/sk-v18/SPEC.md:397`-`402`; CH3-V1-S3 fold requirement. | Promote as a pre-block before any SIMD parser primitive wave. |
| T2A-V1-LAC-05 | Lock 16 / scalar-first admission | No SIMD/ASM primitive reaches admission from citation alone; it needs scalar reference, same-run parity/checkasm where relevant, hardware gate, same-wave consumer, and row movement against a yyjson-shape scalar baseline. | sonic-rs targeted leaves; yyjson no-SIMD source; SK-V15 SPEC non-negotiable `restart/skinny/tranches/sk-v15/SPEC.md:136`-`137`; dispatch context deep-SIMD process rule. | Route to 2B/2E primitive manifest and W2 Lock 16 gate restoration. |

## Source Register

| id | source | use |
|---|---|---|
| SRC-SIMDJSON-PAPER | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", The VLDB Journal 28(6), 2019, arXiv:1902.08318. | Two-stage structural-index/tape architecture. |
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

---

# SK-V18 EXTENSION (cycle SK-V18-T-P2)

The converged V2 dossier above (SK-V15 lineage) is NOT re-derived. This extension grounds the
SK-V18-specific SOTA assertions the certified SK-V18 wave plan depends upon: the dav1d/FFmpeg/VLC
PROCESS discipline behind the G6 NEON retarget of `find_component_delim`, the lazy-rich-vs-eager-cssom
CSS >SOTA framing vs lightningcss, and the JSON strict-vs-sonic-rs-strict bar. SK-V18 absorbs the
GENERALIZATION (ONE grammar-driven generator emitting JSON+CSS+Sheets from `.bbnf`). SCOPING (per 2C
LAC-2C-SK18-02): SK-V18 grounds a measured >SOTA plane on JSON+CSS ONLY — those are the only two grammars
with a same-run comparator (sonic-rs-strict, lightningcss-eager-CSSOM) and a witnessed row in
`skinny/RESULTS.md`. Sheets is a GENERALITY proof (the un-fork carries a third grammar through the same
generator), NOT a SOTA plane in SK-V18 — Sheets/CSV/Math/BNF/EBNF/CssPretty have no grounded SOTA
comparator here; the 9-grammar fleet SOTA is SK-V19. aarch64-PRIMARY (Apple M5 Max; x86 DELETED in
skinny — P1). Lock 16 admissibility binds every primitive
to: published citation + abstract-primitive name + scalar reference + checkasm-parity plan + same-wave
consumer. Refutation is first-class.

## SK-V18 Executive Summary

The SK-V18 SOTA framing is defensible on its two honest planes and the process discipline behind the G6
NEON retarget is grounded in primary literature that the bbnf-simd crate already replicates byte-for-byte
in structure. dav1d/FFmpeg's checkasm — originated in x264, now shared by FFmpeg, dav1d, and rav1d —
validates every SIMD/assembly kernel against a C reference on bit-identical input, fuzzed over seeds,
and is the published "scalar oracle + differential + same-wave consumer" process the SK-V18 §6 primitive
discipline transcribes (`bbnf-simd/tests/checkasm_parity.rs:3` states verbatim "Modelled on FFmpeg's
`tests/checkasm/checkasm.h`"). The CSS >SOTA framing (`track1_rich` lazy-rich projection vs lightningcss
eager full-CSSOM) is the honest equal-depth comparator, grounded by lightningcss building an eager typed
`StyleSheet`/property model and simdjson/sonic-rs establishing that lazy-vs-eager materialisation is a
real published axis. The JSON bar (strict-vs-sonic-rs-strict, same-plane) is grounded by sonic-rs's
direct-to-struct (no tape) architecture. REFUTED: the dav1d *pixel kernels* are NOT transferable (only
the process is); a checkasm PASS is a correctness gate, NEVER a speedup claim; x86/AVX-512 esoterica
cannot close any M5 Max row. No new amendment candidates: 1E LAC-1E-V5-01..07 already bind the §6 (a)-(d)
gate, the dav1d-discipline, the neutrality-proof, and the aarch64-ONLY standing.

## SK-V18 Technique Grounding Table

| spec claim or T-P1 divergence id | published source cited | state | bbnf-specific note |
|---|---|---|---|
| T2A-V18-DAV1D-001: the dav1d/FFmpeg/VLC PROCESS discipline — a scalar/C reference oracle, a checkasm differential that runs each SIMD/assembly kernel against that reference on bit-identical input (fuzzed over seeds), and a requirement that no kernel ships without a consumer — is the published model the SK-V18 §6 primitive gate transcribes (the PROCESS, not the pixel kernels). | FFmpeg checkasm: `tests/checkasm/checkasm.c` File Reference, `https://www.ffmpeg.org/doxygen/trunk/checkasm_8c.html`; checkasm canonical intro `https://checkasm.videolan.me/` (originated x264, shared by FFmpeg + dav1d); dav1d `tests/checkasm` tree, VideoLAN GitLab `https://code.videolan.org/videolan/dav1d/-/tree/master/tests/checkasm`. In-tree replica: `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:3`-`4` ("Modelled on FFmpeg's `tests/checkasm/checkasm.h`. Each primitive is run twice against bit-identical inputs (one buffer for the scalar reference, one for the candidate)"), `classify_reference`/`scan_scalar` at `:129`-`:130`, `:206`. | grounded | transfer_reason=the SK-V18 G6 retarget of `find_component_delim` (94.1% scalar hot leaf, S-P1 §3) admits a NEON kernel ONLY under this process — scalar reference FIRST, checkasm differential over the real corpora, same-wave generated caller; comparator_plane=correctness-parity (NOT a speed plane); admission_gate has scalar_reference=`scan_scalar`/`find_component_delim` shell, parity_or_checkasm=`checkasm_parity.rs` differential extended to the recursive shell + `neon_significant_skip_matches_scalar` guard over real 71KB-495KB corpora, hardware_gate=Apple M5 Max/aarch64 ONLY, same_wave_consumer=the P3-collapsed single CSS scan call site (G2∧G6 one seam), row_movement_target=`track1_rich/lightningcss` ratio held; verification_action=run the differential RED-falsifier (inject a wrong byte → checkasm flags it) then revert; close_status=source-present-unwired; loc_estimate=≤450 hand source/test (SPEC §8 G5/G6 row); risk_class=MED-HIGH; wave_owner=G5/G6; hard_cap_fit=≤90min wall, 45min/redress. |
| T2A-V18-DAV1D-002: a checkasm differential PASS is a CORRECTNESS gate only and never establishes a speedup; any Mbps/speedup figure must come from a separate corpus-in-timer symmetric harness. | FFmpeg checkasm (above) separates the correctness check from the optional `--bench` benchmark pass; SK-V18 SPEC `:194`-`:209` ("the checkasm differential is a CORRECTNESS gate only; G6 may report only its PASS/FAIL pre-H1"), `:482` H1 row. | grounded | transfer_reason=prevents the paper-close where a kernel that merely PASSES parity is then narrated as ">SOTA"; comparator_plane=correctness vs timed-plane SPLIT; admission_gate=any speedup CLAIM defers to the H1 symmetric `css_canon_bench` corpus-in-timer harness under the quiet (`host_loadavg < 1.0`) re-capture, AND the G6 wave MUST carry the machine-checkable exit gate `g6_speedup_claim_emitted == false` (not prose): the S-P1 directional ratios (2.190/3.375/1.658/2.101) are NON-CITABLE as a G6 close figure until the H1 quiet re-capture lands in the SAME tranche — any Mbps/speedup string in a G6 artifact pre-H1 is a REJECT (the named falsifier); verification_action=grep any G6 artifact for a Mbps/× string pre-H1 → RED; reject any G6 row that reports Mbps from the checkasm pass; close_status=diagnostic-only (pre-H1, `g6_speedup_claim_emitted==false`); primitive_cost=none (gate discipline). |
| T2A-V18-CSS-LAZY-001: the SK-V18 CSS >SOTA bar — `track1_rich` (lazy-rich 9-field typed projection re-derived from `(source, offset)` spans, nothing written to the arena) vs lightningcss eager full-CSSOM — is an equal-DEPTH typed-value comparison, NOT a count-only structural probe; the lazy-vs-eager materialisation distinction is the disclosed honest framing. | lightningcss eager typed model: `README.md:10`-`12`, `src/stylesheet.rs:74`-`91`, `src/properties/mod.rs:1`-`18` @ `ec165294750bb02903e7f845b66533b0465debcc` (SRC-LIGHTNINGCSS); the lazy axis grounded by simdjson On-Demand (SRC-SIMDJSON-ONDEMAND) and sonic-rs LazyValue (T2A-V18-SONIC-LAZY-002). In-tree truth: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:304`-`:305`,`:307` (`rich_summary`/`nodes()`: "preserve-rich-ast: rich, lazy, not eager, not flattened"); S-P1 `SYNTHESIS-PROFILE.md:28`-`34`; SK-V18 SPEC `:181`,`:188`-`192`,`:256`. | grounded | transfer_reason=defends the >SOTA claim as honest equal-work, not a four-counter probe (the V2-refuted shape); comparator_plane=`css_comparator_plane==full-cssom` for the lightningcss bar (the plane-match column the G2/H1 gate REJECTs on); admission_gate=`materialization_framing == lazy-rich-vs-eager-cssom` disclosed at H1 (enum CLOSED to two values), 9-field cssparser oracle EXACT parity gate-BEFORE-speed, then same-run `track1_rich/lightningcss > 1.0×` per corpus with no same-run regression vs the G2-entry pre-G2 baseline; verification_action=record the framing enum and reject any unqualified "beats CSSOM"; close_status=admissible-after-gate; primitive_cost=none in 2A (projection lives in 2C/G1-G2). |
| T2A-V18-JSON-SONIC-001: the SK-V18 JSON >SOTA bar is strict-vs-sonic-rs-strict, same-plane — direct-to-struct with no intermediate tape, mirroring sonic-rs's published architecture (parses JSON directly into the Rust struct, no temporary tape, unlike simd-json's tape-then-struct two-step). | sonic-rs `README.md:78`-`90` @ `03545a9530346fe279b674dd496e037d94204bc5` (SRC-SONIC-RS); sonic-rs performance doc `docs/performance.md` (`https://github.com/cloudwego/sonic-rs/blob/main/docs/performance.md`) confirms direct-to-struct vs simd-json tape; aarch64 bench evidence `docs/benchmark_aarch64.md`. In-tree: `skinny/RESULTS.md` JSON strict rows; SK-V18 SPEC `:184`,`:197`-`:198`,`:267`. | grounded | transfer_reason=the bbnf SinkOnly `track1_digest` direct-to-struct path (S-P1 §2, 91.5% hot leaf) is on the SAME plane as sonic-rs direct-to-struct; comparator_plane=`typed_direct` (strict-vs-strict, same-plane); admission_gate=per-iter oracle equality + the 51/51 cold strict guard carried from the W0 lock, Apple M5 Max/aarch64, no `sonic_rs::Value` for direct rows; verification_action=maintain `json_strict_rows_admitted==51/51` ∧ `json_sonic_rs_strict_delta` through G1∧G3; close_status=admissible-after-gate; primitive_cost=none (projection in G1). |
| T2A-V18-SONIC-LAZY-002: sonic-rs LazyValue + `get_from` is a published lazy-access architecture — wraps unparsed borrowed JSON text, uses SIMD to compute the string bitmap and bracket counting to skip whole containers — grounding lazy-vs-eager as a REAL published materialisation axis (the analog of bbnf CSS lazy-rich span re-derivation). | sonic-rs `README.md:60`-`66` (targeted SIMD: long-string/float/field-lookup/whitespace-skip) and LazyValue/`get_from` doc `https://docs.rs/sonic-rs` @ `03545a9530346fe279b674dd496e037d94204bc5`; SIMD-skip mechanism (string bitmap + bracket count) confirmed by sonic-rs docs. | grounded | transfer_reason=supplies the published precedent that "lazy access over borrowed source, materialise on use" is a legitimate SOTA architecture — the CSS `nodes()` lazy span re-walk is the same class; comparator_plane=lazy/value access (distinct from typed_direct or full-CSSOM — a DIAGNOSTIC analog, not the bbnf close plane); admission_gate=lazy access cannot be admitted as a speed claim against an eager comparator unless equal-depth materialisation is proven (the lazy-rich-vs-eager-cssom disclosure); verification_action=keep lazy as a framing precedent, not a cross-plane comparator; close_status=diagnostic-only; primitive_cost=targeted leaves route to 2B (none adopted in 2A). |
| T2A-V18-ASMJSON-001: asmjson / SIMD-assembly JSON parsers establish that hand-written vector kernels for JSON structural discovery are a known SOTA technique — but on the M5 Max aarch64 close route the S-P1 profile shows the bbnf JSON direct path is SCAN-FREE (zero `scan_structurals` samples), so no JSON classifier kernel is authored (G5 neutralizes `json/scan.rs`). | simdjson stage1 SIMD structural discovery (SRC-SIMDJSON-PAPER, Langdale & Lemire, The VLDB Journal 28(6), 2019, arXiv:1902.08318; SRC-SIMDJSON-PARSE-MANY); sonic-rs targeted SIMD (SRC-SONIC-RS). In-tree refutation of a JSON kernel: S-P1 `SYNTHESIS-PROFILE.md:75`-`79` (`json/scan.rs` ZERO samples, "CHEAP-TO-NEUTRALIZE, not a G5 target"). | partial | transfer_reason=grounds WHY the SK-V18 plan authors NO JSON SIMD kernel (the published technique exists, but the measured product path does not exercise it); comparator_plane=parse_only/structural (off the typed_direct product plane); admission_gate=profile-first — no orphan kernel may be authored without a profile-anchored hot leaf (S-P1 §5); verification_action=G5 neutralizes/retires `json/scan.rs` with no JSON classifier authored; close_status=partial-blocked (technique real, deliberately not transferred); primitive_cost=none (G5 is a deletion/neutralize); rollback path=re-author the JSON SIMD classifier ONLY if a future profile shows `json/scan.rs` hot (i.e. the G5 neutralize is reverted to a real kernel build under the §6 primitive gate — scalar reference + checkasm + same-wave consumer — never a bare intrinsic loop); abrogate threshold=if the S-P1 scan-free finding falsifies under PROVE/Sheets (a measured non-zero `scan_structurals` hot leaf appears on the product path), RE-OPEN the JSON-kernel question through the profile-first gate rather than ship a neutralized `json/scan.rs` that silently regresses the product path. |
| T2A-V18-REFUTE-001: the dav1d PIXEL kernels (itx/loopfilter/mc DSP functions) are NOT a transferable architecture; only the checkasm PROCESS transfers. | dav1d `src/` DSP kernels are video-pixel-specific (deblock/film-grain/inverse-transform), domain-orthogonal to byte-stream parsing; SK-V18 SPEC `:410`-`:412` binds "dav1d discipline on every primitive" to the PROCESS (scalar reference + checkasm + same-wave consumer), never the kernel shapes. | refuted | transfer_reason=none for the kernels; transfer is PROCESS-only; comparator_plane=N/A; admission_gate=any "we ported a dav1d kernel" claim is a category error and a REJECT; verification_action=ensure every SK-V18 primitive cites the PROCESS (checkasm differential) not a pixel-DSP shape; close_status=refuted. |
| T2A-V18-REFUTE-002: x86/AVX-512 and any non-aarch64 SIMD esoterica cannot close an M5 Max row; x86 is DELETED in skinny (P1), architecture-pressure only. | SK-V18 SPEC `:42`,`:130`,`:422`-`:425` (aarch64 ONLY; both x86 surfaces DELETED — `src/x86_64/` 24 files + `ext/x86/`); 1E LAC-1E-V5-04 (aarch64-ONLY refinement of the SK-V17 `LOCKS.md:622` aarch64-primary clause); `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.asm` is a P1 deletion target. | refuted | transfer_reason=none on the close route; comparator_plane=N/A (different ISA); admission_gate=x86/AVX-512 literature is diagnostic architecture-pressure, never an admission anchor; verification_action=P1 deletes the x86 surface and decouples `checkasm_parity.rs` x86_64 references, retaining only the aarch64 differential; close_status=refuted. |

## SK-V18 Architectural Assertions Defended

1. **The dav1d/FFmpeg checkasm PROCESS is the grounded model for the G6 NEON retarget, and the
   codebase already replicates it.** checkasm — originated in x264, shared by FFmpeg, dav1d, and rav1d —
   runs each SIMD/assembly kernel against a C reference on bit-identical input, fuzzed over seeds, and
   requires every new assembly to ship with a checkasm test (FFmpeg developer doc). The skinny
   `bbnf-simd/tests/checkasm_parity.rs:3`-`4` is explicitly "Modelled on FFmpeg's
   `tests/checkasm/checkasm.h`", runs each primitive twice on bit-identical buffers (scalar reference vs
   candidate), and carries 13 differential harnesses (12 single-kernel `checkasm_<primitive>.rs` + 1
   aggregate `checkasm_parity.rs`, SPEC §3.1) plus the `checkasm_common.rs` shared-helper module (an
   `#![allow(dead_code)]` `Xorshift64` RNG, NOT a harness). The G6
   retarget of `find_component_delim` (94.1% scalar hot leaf) is admissible ONLY under this process:
   scalar shell FIRST, checkasm differential over the real corpora, same-wave generated caller. This is a
   grounded transfer, not an invented discipline.

2. **The CSS lazy-rich-vs-eager-cssom framing is the honest equal-depth comparator.** `track1_rich`
   materialises 9 typed fields by re-deriving every field from `(source, offset)` spans, writing nothing
   to the arena (`generated.rs:304`-`:307`, "rich, lazy, not eager, not flattened"). lightningcss builds
   an eager typed `StyleSheet`/property model (`src/stylesheet.rs:74`-`91`, `src/properties/mod.rs:1`-`18`).
   The lazy-vs-eager axis is a published distinction (simdjson On-Demand; sonic-rs LazyValue). SK-V18
   discloses it explicitly via `materialization_framing == lazy-rich-vs-eager-cssom` (a CLOSED two-value
   enum that the H1 gate REJECTs any other value of). This defends the >SOTA claim as honest equal work,
   not the V2-refuted four-counter probe.

3. **The JSON strict-vs-sonic-rs-strict bar is same-plane and grounded.** sonic-rs parses JSON directly
   into the Rust struct with no intermediate tape (`README.md:78`-`90`, `docs/performance.md`), the exact
   plane of the bbnf SinkOnly `track1_digest` direct-to-struct path (S-P1 §2, 91.5% hot leaf). The 51/51
   cold strict rows are carried from the W0 lock, per-iter oracle equality, Apple M5 Max/aarch64. No
   framing asymmetry — the existing W0-locked proof carried forward.

4. **Profile-first refutes authoring a JSON SIMD classifier.** simdjson stage1 and asmjson establish that
   SIMD structural discovery is a known JSON SOTA technique, but the S-P1 profile shows the bbnf JSON
   direct product path is scan-free (`json/scan.rs` ZERO samples). The published technique is real; the
   measured product does not exercise it; G5 neutralizes `json/scan.rs` with no JSON classifier authored.
   The two opposite dispositions (CSS WIRE at 94.1%, JSON neutralize at 0%) are both grounded in measured
   share, not in a uniform "SIMD everywhere" prior.

## SK-V18 Architectural Assertions Refuted

1. **Refuted: the dav1d pixel kernels transfer.** dav1d's DSP kernels (inverse-transform, loopfilter,
   film-grain, mc) are video-pixel-domain-specific. Only the checkasm PROCESS (scalar oracle + differential
   + consumer) transfers; a "we ported a dav1d kernel" claim is a category error (T2A-V18-REFUTE-001).

2. **Refuted: a checkasm PASS is a speedup.** A differential PASS is a correctness gate; it says nothing
   about throughput. Any Mbps figure must come from the separate corpus-in-timer symmetric harness under
   the quiet re-capture, and the speedup claim defers to H1 (T2A-V18-DAV1D-002, SPEC `:194`-`:209`).

3. **Refuted: x86/AVX-512 esoterica close an M5 Max row.** aarch64 is the ONLY close route; the whole x86
   surface is DELETED in skinny (P1). x86 literature is diagnostic architecture-pressure, never an
   admission anchor (T2A-V18-REFUTE-002, 1E LAC-1E-V5-04).

4. **Refuted (carried from V2): a four-counter CSS summary or fact-stream is a CSSOM-plane comparator.**
   The lazy-rich projection (9 typed fields) supersedes the four-counter probe; the V2 refutations
   (T2A-V1-REFUTE-CSS-001..004) stand. SK-V18 admits CSS >SOTA ONLY on the `css_comparator_plane==full-cssom`
   plane with the framing disclosure.

## SK-V18 Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| SKV18-UNKNOWN-1: Can the G6 NEON retarget of the recursive `find_component_delim`+`consume_balanced_at` shell pass the checkasm differential when the existing dead kernels (`find_css_significant`, `find_comment_close`) target a FLATTER function (stop-at-delimiter, no nested `()[]{}` recursion / string-skip)? | G5/G6 wave must RETARGET (not wire-as-is) the NEON onto the recursive shell, extend `checkasm_parity.rs` + the `neon_significant_skip_matches_scalar` guard to the recursive shell over the real 71KB-495KB corpora, and prove `acceleration_at_admission==admission` by a non-`#[cfg(test)]` caller census in the generated `generated.rs`. RED-falsifier: inject a divergent byte → checkasm flags. |
| SKV18-UNKNOWN-2: Is the `balanced_component_scan` SHELL genuinely grammar-neutral, or must it demote to `css_balanced_component_scan`? | The neutrality-proof obligation (1E LAC-1E-V5-03): at least one NON-CSS invocation (JSON `{}`/`[]` balanced nesting OR Sheets `paren_expr`) must invoke the SAME primitive in-campaign, ELSE forced demotion to a CSS-scoped name. A neutrally-named CSS-only primitive is an overfit-in-waiting. |
| SKV18-UNKNOWN-3: Does the quiet (`host_loadavg < 1.0`) re-capture confirm the S-P1 directional `track1_rich/lightningcss` ratios (2.190/3.375/1.658/2.101) as the H1 absolute floor? | H1 mandates ONE quiet re-capture (the deferred G6 figure + the >SOTA directional re-confirmation); the S-P1 absolute Mbps is DIRECTIONAL (loadavg 4.35), NOT a re-locked baseline. The binding floor is the SAME-RUN ratio captured at G2 entry, re-confirmed directionally at H1. |
| SKV18-UNKNOWN-4: Can the SinkOnly JSON string/number leaf micro-opts (`b'-'|b'0'..=b'9'` fast-path, `match_tiny_plain_string_direct`) be reproduced byte-exact by the G1 AST-walk, or do they require the §6 (a)-(d) named-primitive escape? | G1 byte-equivalence gate against the `json_templates/` oracle BEFORE oracle deletion; each hot leaf kernel that cannot be walk-reproduced byte-exact is admitted ONLY under the §6 (a)-(b)-(c)-(d) machine gate (the byte-set/numeric-class mutation falsifier distinguishes a derived leaf from a relabeled courier). |

## SK-V18 LOCKS-AMENDMENTS-CANDIDATE

**No new 2A amendment candidates for SK-V18.** The SK-V18 SOTA/process discipline is already fully bound
by the 1E LOCKS-AMENDMENTS-CANDIDATE table (`restart/audit/totality/p1/1E-locks-evidence.md:139`-`162`),
which this dossier grounds rather than extends:

- **LAC-1E-V5-01** (the §6 named-primitive (a)-(d) gate) — grounded here by the checkasm PROCESS
  (T2A-V18-DAV1D-001) supplying the (a)-grammar-invoked + scalar-reference + parity components.
- **LAC-1E-V5-03** (the neutrality-proof / `balanced_component_scan` → `css_balanced_component_scan`
  forced demotion) — surfaced as SKV18-UNKNOWN-2.
- **LAC-1E-V5-04** (aarch64-ONLY; x86 surface DELETED) — grounded here by T2A-V18-REFUTE-002.

The 2A-scope axes scanned for a NEW candidate and found NONE: (i) no new comparator plane beyond the
V2-bound eight (`parse_only|DOM|value|typed_direct|lazy|fact_stream|CSS_typed_document|CSSOM_value`); the
SK-V18 `full-cssom`/`lazy-rich` framing is a NAMING of the existing CSSOM_value plane, not a new plane.
(ii) no new SOTA library admitted as an anchor (simdjson/sonic-rs/yyjson/cssparser/lightningcss/dav1d-checkasm
all already in the V2 + 1E register). (iii) the dav1d-discipline is bound by SPEC §8 + 1E, needs no 2A
lock. The V2 LAC-01..05 carry forward unchanged.

## SK-V18 Source Register (additions)

| id | source | use |
|---|---|---|
| SRC-FFMPEG-CHECKASM | FFmpeg `tests/checkasm/checkasm.c` doxygen, `https://www.ffmpeg.org/doxygen/trunk/checkasm_8c.html`; FFmpeg developer doc ("all new assembly should come with checkasm tests"). | The differential-against-C-reference PROCESS, fuzzed over seeds, multi-ISA. |
| SRC-CHECKASM-INTRO | checkasm canonical intro, `https://checkasm.videolan.me/` (originated x264; shared by FFmpeg + dav1d). | checkasm provenance + portability + scope. |
| SRC-DAV1D-CHECKASM | dav1d `tests/checkasm` tree, VideoLAN GitLab `https://code.videolan.org/videolan/dav1d/-/tree/master/tests/checkasm`. | dav1d's use of the checkasm differential for its NEON/SSE kernels. |
| SRC-BBNF-SIMD-CHECKASM | `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:3`-`4`,`:129`-`130`,`:206`; 14 `checkasm_*.rs` files = 12 single-kernel `checkasm_<primitive>.rs` harnesses + 1 aggregate `checkasm_parity.rs` + the `checkasm_common.rs` shared-helper module (13 differential harnesses, not 14 kernels). | In-tree replica of the dav1d/FFmpeg discipline (scalar reference vs candidate, bit-identical buffers). |
| SRC-SONIC-RS-PERF | sonic-rs `docs/performance.md` + `docs/benchmark_aarch64.md` @ `main`. | Direct-to-struct (no tape) vs simd-json tape-then-struct; aarch64 bench plane. |
| SRC-SONIC-RS-LAZY | sonic-rs LazyValue/`get_from` doc, `https://docs.rs/sonic-rs`. | Lazy borrowed-source access + SIMD bitmap/bracket-count container skip (the lazy-vs-eager axis). |
| SRC-CSS-LAZY-RICH | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:304`-`:307`. | `rich_summary`/`nodes()` lazy span re-derivation ("rich, lazy, not eager, not flattened"). |
| SRC-SKV18-SPEC | `restart/skinny/tranches/sk-v18/SPEC.md:104`-`:130`,`:177`-`:209`,`:256`-`:295`,`:410`-`:425`,§3.1. | SK-V18 close route, two >SOTA bars, framing enum, dav1d-discipline, aarch64-ONLY, x86 delete. |
| SRC-SKV18-PROFILE | `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md:36`-`:48`,`:75`-`:120`. | S-P1 directional >SOTA ratios + the 94.1% CSS / 0% JSON hot-leaf dispositions. |
| SRC-SKV18-1E | `restart/audit/totality/p1/1E-locks-evidence.md:139`-`162`. | The 7-candidate 1E LOCKS-AMENDMENTS-CANDIDATE table this dossier grounds. |

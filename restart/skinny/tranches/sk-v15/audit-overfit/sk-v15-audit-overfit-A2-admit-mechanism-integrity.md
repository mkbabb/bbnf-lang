# SK-V15 S-P0 A2 Admit-Mechanism Integrity

Date: 2026-05-27
Axis: A2 Admit-mechanism integrity
Scope: skinny SK-V15 entry state, SK-V14 admits, current dirty source state

## Verdict

Overall A2 verdict: CRITICAL.

JSON guard rows remain admissible as the validated baseline, subject to the
bench-only FNV/closed-enum quarantine below. CSS L4 remains audit-demoted:
the current 24 CSS rows still admit through one broadcast measurement, a
hand-written `CSS_GENERATED_RS` runtime template, a mismatched comparator
binding, and a marker-only equality oracle. No CSS row should feed forward as
an independent admit until PRUNE-WAVE-A and REBUILD-WAVE-E close.

## Commands Run

Read-only commands used for this audit:

- `git status --short`
- `git diff --stat`
- `git diff --name-only -- skinny/crates/codegen/src/runtime_generator.rs skinny/crates/codegen/src/grammar_profile.rs skinny/xtask/src/main.rs skinny/crates/bbnf-bench/benches/json_parity.rs skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/runtime/src/grammars/json/generated.rs skinny/crates/bbnf-bench/src/direct_struct.rs`
- `git diff --name-only -- skinny/crates/runtime/src/grammars/css_l4_*/generated.rs skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `git diff --word-diff=porcelain -- skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `git diff --word-diff=porcelain -- skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs`
- `shasum skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`
- `wc -l skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`
- `rg -n "CSS_GENERATED_RS|CssFullParseSummary|parse_full|strict_equality|SKV13_CSS_FEATURES|per-iter|per-iteration" ...`
- `nl -ba ... | sed -n ...` on cited source and authority files

Current relevant dirty-state summary: JSON admit harness/codegen paths checked
above returned no dirty files. CSS W8 and the seven CSS `generated.rs` files
are dirty, but the inspected word diffs are formatting-only around existing
logic, not new parser/codegen/SIMD mechanisms. The seven CSS generated files
still hash identically and are all 646 lines.

## Verdict Table

| ID | Severity | Surface | Verdict | Evidence | Receiver |
|---|---|---|---|---|---|
| A2-CRIT-1 | CRITICAL | CSS L4 24 rows | The 24 CSS admits are still one aggregate measurement broadcast across 24 conceptual rows. `W8_SELECTED_CSS_ROWS` remains hardcoded at 24, `TRACK1_PROFILES` has only seven runtime parsers, `measure_full_parse_profiles` times the combined corpus/profile loop once, and `admitted_rows` is set to the 24-row constant if that aggregate passes. | A2 criterion: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:37`, `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:78`; prior blocker: `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`; code: `skinny/crates/bbnf-bench/src/css_l4_w8.rs:17`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:60`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:136`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:139`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:144`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:217`; row projection: `skinny/xtask/src/main.rs:1272`, `skinny/xtask/src/main.rs:1333`; results: `skinny/RESULTS.md:112`, `skinny/RESULTS.md:135`; repeated TSV values: `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2`, `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:25`. | PRUNE-WAVE-A |
| A2-CRIT-2 | CRITICAL | CSS runtime/codegen | CSS admits do not land via grammar-derived parser/codegen/SIMD source changes. `RuntimeGenerationMode::FrontendFacts` routes all seven CSS profiles to `emit_frontend_facts`, which emits `normalize(CSS_GENERATED_RS)` as `generated.rs`. `CSS_GENERATED_RS` is a hand-written parser string literal, not derived from `grammar/css/l4/*.bbnf`; the live generated module still contains the same summary/parser shape. | CH7 grammar-derived rule: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:72`; prior blocker: `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:31`; SK-V15 close gate: `restart/skinny/tranches/sk-v15/SYNTHESIS.md:40`; codegen mode split: `skinny/crates/codegen/src/grammar_profile.rs:12`, `skinny/crates/codegen/src/grammar_profile.rs:117`; emitter: `skinny/crates/codegen/src/runtime_generator.rs:81`, `skinny/crates/codegen/src/runtime_generator.rs:92`, `skinny/crates/codegen/src/runtime_generator.rs:97`; template: `skinny/crates/codegen/src/runtime_generator.rs:713`, `skinny/crates/codegen/src/runtime_generator.rs:773`, `skinny/crates/codegen/src/runtime_generator.rs:815`; live output: `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:1`, `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:61`, `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:103`. | PRUNE-WAVE-A, PRUNE-WAVE-C, REBUILD-WAVE-E |
| A2-HIGH-1 | HIGH | CSS comparator binding | CSS admission is not strict-vs-strict same-plane in the load-bearing sense. Rows say Track 1 is `css_l4_full_parse` with `full_parse_summary`, but the live Track 1 output is only four counters while lightningcss parses a CSSOM. The same evidence also records cssparser at 2362.037 Mbps, faster than Track 1 at 2319.041 Mbps, but admission thresholds use lightningcss at 929.281 Mbps. | Requirement: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:79`; SK-V15 comparator gate: `restart/skinny/tranches/sk-v15/SYNTHESIS.md:42`; prior finding: `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29`; summary struct: `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:50`, `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:53`, `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:91`; benchmark comparators: `skinny/crates/bbnf-bench/src/css_l4_w8.rs:112`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:116`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:140`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:229`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:243`; results: `skinny/RESULTS.md:112`; retained evidence: `restart/skinny/tranches/sk-v14/research/skv14-W11-close.md:293`. | PRUNE-WAVE-A, REBUILD-WAVE-E |
| A2-HIGH-2 | HIGH | CSS equality oracle | CSS "per-iteration equality" is marker acceptance, not equality of equivalent values. `generated_full_parse_marker` accepts schema/row/status and rejects fact-stream leakage; it does not compare a Track 1 CSS value to cssparser/lightningcss output. The gate only requires strings such as `strict_equality[status=pass` and `wrong_plane_outputs=0`, so it can bless shape markers. | Requirement: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:80`; marker code: `skinny/crates/bbnf-bench/src/css_l4_w8.rs:181`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:188`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:192`; test asserts counts and admit constant: `skinny/crates/bbnf-bench/src/css_l4_w8.rs:449`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs:474`; gate checks telemetry strings: `skinny/xtask/src/main.rs:1004`, `skinny/xtask/src/main.rs:1027`; row evidence string: `skinny/RESULTS.md:112`. | PRUNE-WAVE-A, REBUILD-WAVE-E |
| A2-MED-1 | MEDIUM | JSON W11L/W11N/W11O bench-only token products | These admits land through real bench source and strict comparators, but the strict-product oracle remains weak against closed-enum sidecar coupling because Track 1 and sonic/serde sidecars deserialize into the same finite token domains. This is quarantined to bench-only code, not production runtime, but it must not become generalized admission machinery. | Prior audit: `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:60`, `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:62`, `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:141`; closed enum: `skinny/crates/bbnf-bench/src/real_typed_struct.rs:910`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:942`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:960`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:978`; affected rows: `skinny/RESULTS.md:54`, `skinny/RESULTS.md:55`, `skinny/RESULTS.md:98`, `skinny/RESULTS.md:99`, `skinny/RESULTS.md:86`, `skinny/RESULTS.md:87`. | REBUILD-WAVE-G |
| A2-CLEAN-1 | CLEAN | JSON parse_only/direct/typed guard rows, excluding A2-MED-1 caveat | The JSON guard has real parser/source surfaces and timed equality in the benchmark path. Parse-only rows call the generated validator; direct rows assert Track 1 strict product equals Track 2, serde, and sonic products; typed rows assert typed checksum equality in the timing loop. Comparator metadata binds parse_only to `parse_only/sonic_rs::Skipper` and direct/typed to corpus-specific strict struct planes. No current dirty diff touches the checked JSON admit harness/codegen paths. | SK-V15 guard baseline: `restart/skinny/tranches/sk-v15/SYNTHESIS.md:38`, `restart/skinny/tranches/sk-v15/SYNTHESIS.md:59`; JSON validation note: `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:83`; parse-only code: `skinny/crates/runtime/src/grammars/json/generated.rs:412`, `skinny/crates/runtime/src/grammars/json/generated.rs:556`; parse-only bench: `skinny/crates/bbnf-bench/benches/json_parity.rs:52`, `skinny/crates/bbnf-bench/benches/json_parity.rs:97`; direct per-iter oracle: `skinny/crates/bbnf-bench/benches/json_parity.rs:197`, `skinny/crates/bbnf-bench/benches/json_parity.rs:207`; typed per-iter oracle: `skinny/crates/bbnf-bench/benches/json_parity.rs:336`, `skinny/crates/bbnf-bench/benches/json_parity.rs:347`; strict product four-way parity: `skinny/crates/bbnf-bench/src/direct_struct.rs:485`, `skinny/crates/bbnf-bench/src/direct_struct.rs:490`, `skinny/crates/bbnf-bench/src/direct_struct.rs:493`; comparator metadata: `skinny/crates/bbnf-bench/src/bin/gate.rs:2803`, `skinny/crates/bbnf-bench/src/bin/gate.rs:2898`, `skinny/crates/bbnf-bench/src/bin/gate.rs:2907`; results note: `skinny/RESULTS.md:139`, `skinny/RESULTS.md:142`, `skinny/RESULTS.md:147`. | No prune receiver for JSON guard; REBUILD-WAVE-G only for A2-MED-1 |

## Admit Matrix

| Admit group | A2 status | Parser/codegen/SIMD source change | Comparator binding | Per-iteration equality oracle |
|---|---|---|---|---|
| JSON parse_only, 17 rows | CLEAN | Real generated parse-only validator and parse/string hot path are present in runtime/codegen. | Strict same-run `parse_only/sonic_rs::Skipper`; serde DOM is companion, not strict anchor. | Present in benchmark/report contract and profile-direct rows. |
| JSON direct_to_struct, 17 rows | CLEAN with A2-MED-1 caveat for W11L/N/O-style rows | Real direct/typed product parser code and bench source are present. | Strict product per corpus against sonic/serde strict struct deser. | Present: timing loop asserts Track 1 product equals Track 2 expected product; direct parity function checks Track 1, Track 2, serde, sonic. |
| JSON real_typed_struct, 17 rows | CLEAN with A2-MED-1 caveat for W11L/N/O-style rows | Real typed parser code is present in bench-only generated module and source history. | Typed strict struct deser per corpus. | Present: timing loop asserts typed checksum against Track 2 expected checksum. |
| CSS L4, 24 rows | CRITICAL | Fails. The live generator emits a static hand-written `CSS_GENERATED_RS` string to all seven CSS profile modules; current CSS dirty diffs are formatting-only. | Fails. Admission threshold is lightningcss CSSOM-ish full parse while Track 1 outputs a four-counter summary; cssparser same-run evidence is faster than Track 1. | Fails. Marker/status checks are not semantic equality. |

## Prune Receiver Routing

| Receiver | Routed A2 work |
|---|---|
| PRUNE-WAVE-A | Remove the 24-row CSS broadcast admit. Either demote CSS to one diagnostic aggregate row or require 24 distinct typed CSS rows with distinct measurements, source diffs, comparators, and equality oracles. Remove `W8_SELECTED_CSS_ROWS` as an admission multiplier. |
| PRUNE-WAVE-C | Remove the CSS runtime generation mode leak and the `CSS_GENERATED_RS` hand-written template path from live admission. A CSS admit cannot cite `generated_grammar` while emission is `normalize(CSS_GENERATED_RS)`. |
| REBUILD-WAVE-E | Build the CSS typed value/document/view path and rerun CSS against a same-workload comparator. cssparser can be the near-term comparator; lightningcss should only be an admission comparator after Track 1 emits comparable CSSOM/value output. |
| REBUILD-WAVE-G | Keep W11L/W11N/W11O FNV/closed-enum products bench-only, add a guard against migration into `crates/core/src/runtime/`, and harden strict-product differential against sidecars that share the same closed enum table. |

No A2-specific route is assigned to PRUNE-WAVE-B, PRUNE-WAVE-D, or
REBUILD-WAVE-F in this artifact; those receivers are covered by other S-P0
axes unless their fixes become prerequisites for one of the routes above.

## Notes For Synthesis

- CSS should be marked `AUDIT-DEMOTED` until PRUNE-WAVE-A retires the
  broadcast and REBUILD-WAVE-E proves typed CSS equality on a real value plane.
- A row whose comparator evidence says `strict_equality[status=pass]` is not
  enough. A2 requires the oracle to compare equivalent values or products
  inside the timing loop.
- The current worktree contains many unrelated dirty files. This audit did not
  edit or revert them and does not treat them as A2 admission evidence.

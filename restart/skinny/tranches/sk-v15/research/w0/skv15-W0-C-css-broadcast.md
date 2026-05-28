# SK-V15 W0-C CSS Broadcast Row Provenance

Date: 2026-05-28.
Worker: SK-V15 W0 research worker C.
Repository HEAD observed: `16d26a84b` (`git rev-parse --short=9 HEAD`).
Scope: research-only; no source, generated output, RESULTS, REDRESS, gate, or
non-assigned doc edits.

## Authority Read

- Current top-level authority says G-Omega V9 is authorized, current
  implementation authority is SK-V15 W0-W11, and the locked contract is
  `restart/skinny/tranches/sk-v15/SPEC.md` plus
  `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`
  (`restart/HANDOFF.md:5-9`). The next implementation dispatch is SK-V15 W0
  baseline/telemetry lock (`restart/HANDOFF.md:11-17`).
- The same handoff lists the PASS-IMPL V1 blocker map and current RESULTS /
  REDRESS as read-order authority (`restart/HANDOFF.md:41-65`), blocks
  source/generated/gate/RESULTS/REDRESS edits until W0 dispatch
  (`restart/HANDOFF.md:79-84`), and forbids stale CSS broadcast evidence as
  close proof (`restart/HANDOFF.md:88-102`).
- SK-V15 SPEC requires W0 to create the checked baseline and telemetry lock
  (`restart/skinny/tranches/sk-v15/SPEC.md:49-55`), classifies stale W8R CSS
  tuples as planning-only diagnostics (`restart/skinny/tranches/sk-v15/SPEC.md:86-93`),
  and says CSS rows carrying W8R broadcast evidence are diagnostic or NO-GO,
  not admits (`restart/skinny/tranches/sk-v15/SPEC.md:94-99`).

## Current 24-Row Representation

The 24 CSS rows are still represented in the live schema-v3 RESULTS ledger as
`SK-V14-W8R` CSS L4 `direct_to_struct` rows, with the same run id
`SK-V14-W8R:css-full-parse-profile-cold-8`, track1 generated
`parser::parse_full`, track2 `cssparser::StyleSheetParser full-parse probe`,
comparator plane `lightningcss full-parse`, validation `generated_grammar`, and
`REDRESS-215-superseded-by-W8R` / `admitted:SK-V14-W8R-full-parse` markers
(`skinny/RESULTS.md:112-135`). The rolling delta also still shows these 24 as
`ADMITTED` with identical `css_l4_full_parse` numbers
(`restart/skinny/ROLLING-SOTA-DELTA.md:66-93`).

| row | RESULTS | rolling delta | retained TSV |
|---|---:|---:|---:|
| `css_l4/declaration_values/direct_to_struct/main` | `skinny/RESULTS.md:112` | `restart/skinny/ROLLING-SOTA-DELTA.md:70` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2` |
| `css_l4/declarations/direct_to_struct/main` | `skinny/RESULTS.md:113` | `restart/skinny/ROLLING-SOTA-DELTA.md:71` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:3` |
| `css_l4/stylesheet_root/direct_to_struct/main` | `skinny/RESULTS.md:114` | `restart/skinny/ROLLING-SOTA-DELTA.md:72` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:4` |
| `css_l4/selectors/direct_to_struct/main` | `skinny/RESULTS.md:115` | `restart/skinny/ROLLING-SOTA-DELTA.md:73` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:5` |
| `css_l4/at_rules_keyframes/direct_to_struct/main` | `skinny/RESULTS.md:116` | `restart/skinny/ROLLING-SOTA-DELTA.md:74` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:6` |
| `css_l4/nested_rules/direct_to_struct/main` | `skinny/RESULTS.md:117` | `restart/skinny/ROLLING-SOTA-DELTA.md:75` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:7` |
| `css_l4/css_variables/direct_to_struct/main` | `skinny/RESULTS.md:118` | `restart/skinny/ROLLING-SOTA-DELTA.md:76` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:8` |
| `css_l4/calc_expressions/direct_to_struct/main` | `skinny/RESULTS.md:119` | `restart/skinny/ROLLING-SOTA-DELTA.md:77` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:9` |
| `css_l4/var_url_functions/direct_to_struct/main` | `skinny/RESULTS.md:120` | `restart/skinny/ROLLING-SOTA-DELTA.md:78` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:10` |
| `css_l4/color_functions/direct_to_struct/main` | `skinny/RESULTS.md:121` | `restart/skinny/ROLLING-SOTA-DELTA.md:79` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:11` |
| `css_l4/gradients/direct_to_struct/main` | `skinny/RESULTS.md:122` | `restart/skinny/ROLLING-SOTA-DELTA.md:80` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:12` |
| `css_l4/transforms/direct_to_struct/main` | `skinny/RESULTS.md:123` | `restart/skinny/ROLLING-SOTA-DELTA.md:81` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:13` |
| `css_l4/filters/direct_to_struct/main` | `skinny/RESULTS.md:124` | `restart/skinny/ROLLING-SOTA-DELTA.md:82` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:14` |
| `css_l4/easing_functions/direct_to_struct/main` | `skinny/RESULTS.md:125` | `restart/skinny/ROLLING-SOTA-DELTA.md:83` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:15` |
| `css_l4/media_queries/direct_to_struct/main` | `skinny/RESULTS.md:126` | `restart/skinny/ROLLING-SOTA-DELTA.md:84` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:16` |
| `css_l4/vendor_prefixes/direct_to_struct/main` | `skinny/RESULTS.md:127` | `restart/skinny/ROLLING-SOTA-DELTA.md:85` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:17` |
| `css_l4/custom_at_rules/direct_to_struct/main` | `skinny/RESULTS.md:128` | `restart/skinny/ROLLING-SOTA-DELTA.md:86` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:18` |
| `css_l4/pseudo_classes/direct_to_struct/main` | `skinny/RESULTS.md:129` | `restart/skinny/ROLLING-SOTA-DELTA.md:87` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:19` |
| `css_l4/pseudo_elements/direct_to_struct/main` | `skinny/RESULTS.md:130` | `restart/skinny/ROLLING-SOTA-DELTA.md:88` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:20` |
| `css_l4/attribute_selectors/direct_to_struct/main` | `skinny/RESULTS.md:131` | `restart/skinny/ROLLING-SOTA-DELTA.md:89` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:21` |
| `css_l4/logical_properties/direct_to_struct/main` | `skinny/RESULTS.md:132` | `restart/skinny/ROLLING-SOTA-DELTA.md:90` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:22` |
| `css_l4/grid/direct_to_struct/main` | `skinny/RESULTS.md:133` | `restart/skinny/ROLLING-SOTA-DELTA.md:91` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:23` |
| `css_l4/flexbox/direct_to_struct/main` | `skinny/RESULTS.md:134` | `restart/skinny/ROLLING-SOTA-DELTA.md:92` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:24` |
| `css_l4/typed_property_groups/direct_to_struct/main` | `skinny/RESULTS.md:135` | `restart/skinny/ROLLING-SOTA-DELTA.md:93` | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:25` |

## Repeated Measurement Tuple Evidence

The retained profile TSV header is
`row, track1_mbps, lightningcss_mbps, cssparser_mbps, floor_mbps, margin_mbps, admit`
(`restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:1`).
Every data row repeats the same tuple:

```text
track1_mbps=2319.041
lightningcss_mbps=929.281
cssparser_mbps=2362.037
floor_mbps=930.281
margin_mbps=1388.760
admit=yes
```

That tuple appears on all 24 TSV rows
(`restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2-25`).
`skinny/REDRESS.md` retained the same W8R evidence tuple and states that all 24
CSS L4 rows moved to `AUDIT-SUSTAINED` / `ADMITTED` on the
`css_l4_full_parse` plane (`skinny/REDRESS.md:5328-5342`).

The implementation explains why the tuple is broadcast. `W8_SELECTED_CSS_ROWS`
is a constant set to 24 (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:16-18`),
but the harness has only seven `TRACK1_PROFILES`
(`skinny/crates/bbnf-bench/src/css_l4_w8.rs:60-89`). It validates all seven
profiles across the combined four-file corpus (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:108-134`),
then calls one aggregate measurement function
(`skinny/crates/bbnf-bench/src/css_l4_w8.rs:139-144`). The measurement loop
computes `profiled_bytes = total_bytes(corpora) * TRACK1_PROFILES.len() *
W8_PROFILE_ITERS` and times the same aggregate profile/source loop for Track 1,
lightningcss, and cssparser (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:206-259`).
If that one aggregate beats `lightningcss + 1.0 Mbps`, the report assigns
`admitted_rows = W8_SELECTED_CSS_ROWS` (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:139-144`);
the test asserts that bulk count, not row-distinct measurements
(`skinny/crates/bbnf-bench/src/css_l4_w8.rs:435-480`).

The report/gate side also stamps this as a structural CSS W8R row, not a
measurement-diversity check: `is_skv14_w8r_css_row` accepts any `css_l4/<feature>/direct_to_struct/main`
feature in the 24-string list (`skinny/xtask/src/main.rs:1272-1277`,
`skinny/xtask/src/main.rs:1333-1358`), and the validation predicate checks W8R
strings, comparator strings, sidecar freshness, retention lifetime, redress
entry, and marker text (`skinny/xtask/src/main.rs:1004-1036`) without requiring
24 distinct measurement tuples.

PASS-IMPL V1 reached the same conclusion: all 24 CSS L4 row admits are one
measurement broadcast 24 times (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:19-29`),
and its CSS audit records the repeated RESULTS tuple and the single
measurement-to-24-admits mechanism (`restart/audit/skinny-impl-overfit/V1/AGENT-2-css-l4-hardcoding.md:53-71`).

## Comparator Workload Mismatch

The load-bearing admission plane says `css_l4_full_parse`, Track 2 entry says
`cssparser::StyleSheetParser full-parse probe`, and the comparator plane says
`lightningcss full-parse` on every CSS row (`skinny/RESULTS.md:112-135`).
The comparator evidence column repeats that Track 1 is `2319.041 Mbps`,
cssparser is `2362.037 Mbps`, and lightningcss is `929.281 Mbps`
(`skinny/RESULTS.md:112`, `skinny/RESULTS.md:135`).

The Track 1 output is not a CSSOM/value/document. The generated runtime's
`CssFullParseSummary` has only `rules`, `at_rules`, `qualified_rules`, and
`declarations` counters (`skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:50-59`);
`emit_full_parse` serializes only those counters as a `full_parse` accepted
marker (`skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:61-100`).
The source template that emits all seven CSS runtimes is the same
`CSS_GENERATED_RS` string literal (`skinny/crates/codegen/src/runtime_generator.rs:81-104`,
`skinny/crates/codegen/src/runtime_generator.rs:713-830`) and contains the same
four-counter `CssFullParseSummary`
(`skinny/crates/codegen/src/runtime_generator.rs:762-812`).

By contrast, the benchmark validates and times lightningcss through
`StyleSheet::parse(source, ParserOptions::default())`
(`skinny/crates/bbnf-bench/src/css_l4_w8.rs:112-114`,
`skinny/crates/bbnf-bench/src/css_l4_w8.rs:229-237`) while separately timing a
cssparser stylesheet probe (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:116-118`,
`skinny/crates/bbnf-bench/src/css_l4_w8.rs:243-248`). PASS-IMPL V1 therefore
calls the lightningcss comparison a workload mismatch and notes cssparser beats
Track 1 in the same evidence (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29-31`,
`restart/audit/skinny-impl-overfit/V1/AGENT-2-css-l4-hardcoding.md:73-91`,
`restart/audit/skinny-impl-overfit/V1/AGENT-2-css-l4-hardcoding.md:243-255`).

## Required W0 Diagnostic / Non-Admit Markers

W0 must preserve current CSS rows as diagnostic broadcast evidence unless W1 has
already demoted them, add/validate SK-V15 telemetry fields, and prove gate
consumption (`restart/skinny/tranches/sk-v15/SPEC.md:246-255`). W0 exit
requires CSS rows with W8R broadcast to carry `broadcast_group_id` and
non-admit status, and requires `gate-json` to reject missing SK-V15 fields and
hidden broadcast (`restart/skinny/tranches/sk-v15/SPEC.md:256-263`).

The telemetry fields that matter for this worker are explicit:
`measurement_row_id`, `measurement_origin`, `value_plane`,
`css_comparator_workload`, `generator_source`, `lock14_scan_scope`,
`lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and
`broadcast_group_id` (`restart/skinny/tranches/sk-v15/SPEC.md:100-122`).
The dependency row for this defect requires a gate over
`measurement_row_id`, `measurement_origin`, `broadcast_group_id`, `value_plane`,
CSS comparator fields, and a duplicate signature scan; its conditional status
is `diagnostic-demotion-only` and its consuming status is `diagnostic-only`
(`restart/skinny/tranches/sk-v15/SPEC.md:187-205`).

The per-wave dispatch contract agrees: W0 research scopes include RESULTS
schema, telemetry carrier, gate-json parser, CSS broadcast rows, and JSON 51
guard rows; W0 redress closes only when all SK-V15 telemetry fields are
gate-consumed and CSS broadcast evidence is diagnostic
(`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:119-129`). W1 then owns the
live CSS admission honesty action and must ensure no CSS live admit can be
produced from the W8R 24-row broadcast
(`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:130-142`).

Minimum W0 markers for the current CSS representation:

| marker | required value / disposition | evidence source |
|---|---|---|
| `broadcast_group_id` | one shared group for the 24 W8R rows; must be present | `restart/skinny/tranches/sk-v15/SPEC.md:256-263` |
| `measurement_row_id` | same aggregate W8R profile row / tuple, not 24 independent row measurements | `restart/skinny/tranches/sk-v15/SPEC.md:194`; TSV evidence `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2-25` |
| `measurement_origin` | `SK-V14-W8R:css-full-parse-profile-cold-8` / REDRESS-215 W8R retained evidence | `skinny/RESULTS.md:112-135`; `skinny/REDRESS.md:5328-5342` |
| `value_plane` | `css_l4_full_parse`, flagged diagnostic because it is a counter summary, not typed CSS value/document output | `skinny/RESULTS.md:112-135`; `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:50-100` |
| `css_comparator_workload` | mismatch marker: Track 1 counter summary vs `lightningcss::StyleSheet::parse`; cssparser tuple retained as non-admit diagnostic | `skinny/crates/bbnf-bench/src/css_l4_w8.rs:112-118`; `skinny/crates/bbnf-bench/src/css_l4_w8.rs:229-248` |
| row status | non-admit / diagnostic, not `ADMITTED`, until W1 demotion or W5/W6 typed retime proof | `restart/skinny/tranches/sk-v15/SPEC.md:94-99`; `restart/skinny/tranches/sk-v15/SPEC.md:256-263` |

## No-Deletion Constraints

- W0 is not a CSS provider deletion wave. Its manifest exit gate is telemetry
  consumption, CSS broadcast diagnostic posture, and "no provider deletion"
  (`restart/skinny/tranches/sk-v15/SPEC.md:172-185`).
- SK-V15 non-negotiables forbid any delete or retirement before rebuild proof
  per `NEW-CH3-V5-01` (`restart/skinny/tranches/sk-v15/SPEC.md:133-145`).
- W1 may demote/collapse the 24 CSS broadcast admits, but it must keep live CSS
  providers until W5 unless typed replacement proof lands in the same wave
  (`restart/skinny/tranches/sk-v15/SPEC.md:264-281`).
- Dispatch explicitly says provider deletion is forbidden unless W5/W6-grade
  typed proof lands in the same wave (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:130-142`).
- Dependency rows block premature retirement/deletion of `CSS_GENERATED_RS`,
  `CssFullParseSummary`, fact-stream-only `parse()`, brace-counter proof,
  provider/template/runtime family fanout, and CSS legacy runtime shims until
  the named W5/W6 typed provider/proof or W4 replacement proof exists
  (`restart/skinny/tranches/sk-v15/SPEC.md:192-205`).

Conclusion: W0-C should treat the 24 CSS rows as a single W8R broadcast group
with one repeated timing tuple, mark them diagnostic/non-admit through
gate-consumed telemetry, and preserve all CSS providers/proof artifacts for W1
demotion and W5/W6 rebuild/retime work.

# SK-V12 W5 Research B: RESULTS Row Shape

## Placement

The least invasive close movement is to append one CSS row to both existing
`skinny/RESULTS.md` tables without adding a new outcome variant, telemetry
column, or JSON comparator claim.

1. Add one summary row after `y_string_unicode | direct_to_struct` and before
   `## SK-V9 W0 Telemetry Manifest`.
2. Add one telemetry-manifest row after
   `json/y_string_unicode/direct_to_struct/main` and before `## Notes`.
3. Replace the existing note `Overall outcome N-direct / NoGo` with
   `Overall outcome A / Go`.
4. Relabel the existing Track 1 / Track 2 explanatory notes as JSON-specific,
   then add a CSS-specific note that points to the W1b-2b companion report.

## Summary Row

| Field | Value |
|---|---|
| Corpus | `css_l4/declaration_values` |
| Workload | `direct_to_struct` |
| Outcome | `A` |
| Verdict | `GO` |
| Strictness | `strict` |
| parse_utf8 | `measured-row` |
| escape_complete | `yes` |
| flaw_probe | `generated Track 1 vs independent cssparser Track 2/oracle plus lightningcss same-plane strict comparator; JSON guards held` |
| Output plane | `css_l4_declaration_value_fact_stream` |
| Track 1 Mbps | `429` |
| Track 2 Mbps | `217` |
| sonic-rs strict/lossy Mbps | `n/a` |
| simdjson DOM/On Demand Mbps | `n/a` |
| yyjson/default, asmjson, RapidJSON, serde_json Mbps | `n/a` |
| Delta fields | `n/a`, with SK-V6 noted as `n/a (no SK-V6 non-JSON baseline)` |
| Hot leaf | `criterion:target/criterion/nonjson_css_l4/track1_generated_css_l4_decl_values/new/estimates.json;hot-leaf=n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion;row=css_l4/declaration_values/direct_to_struct/main` |
| Signal | `PASS W5 promotes REDRESS-125 CSS ADMIT candidate; Track 1 429.34420791225705 > lightningcss threshold 169.92962215656692; strict three-way equality; W4 orphans zero; JSON guards held` |

## Manifest Row

Use a single table row with these values:

- Row id: `css_l4/declaration_values/direct_to_struct/main`
- Grammar: `css_l4`
- Domain: `non_json_generated:css_l4:declaration_values`
- Wave: `SK-V12-W1b-2b`
- Run id: `sk-v12-w1b-2b:criterion-fnv64-27240148e5780a54`
- Validation: `../restart/skinny/tranches/sk-v12/research/w1b/artifacts/strict-equality.txt`
- Profile artifact: `n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion`
- Sample cost:
  `track1_mean_ns=3484.383794;cssparser_mean_ns=6880.481226;lightningcss_mean_ns=8855.758871;bytes=187`
- Sample count: `30`
- Build flags: `profile=bench;rustflags=-C target-cpu=native`
- Host triple: `arch=aarch64;cpu=apple-m5-max`
- Feature mask: `arch=aarch64;os=macos;simd=neon;target_cpu=native`
- CostFacts:
  `schema=sk-v12-css-l4-sota-v1;outcome=A;verdict=GO;gate=pass;admission=PASS-ADMIT-CANDIDATE;source=crates/codegen/src/css_l4_declaration_values_templates/generated.rs;runtime=runtime::generated_css_l4_declaration_values::parser::parse;grammar_checksum=a86687a263f75b77ebc7c585651456b803045bea3b866488d68f587644964b42;input_checksum=cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374;input_bytes=187;generated_loc=287;generated_module_bytes=9243;grammar_size_guard=pass:generated_loc<=360;lock14=pass:lock14_baseline::validate;lock16=n/a:no_simd_or_asm_claim`
- Redress: `REDRESS-125`
- SK-V9-open delta: `new-nonjson-row:sk-v12-css-admit`
- Substrate: `generated_css_l4_declaration_values`
- Structural projection: `css_l4_declaration_value_fact_stream`
- Cardinality: `one`
- Consumer: `companion_gate_css_l4_lightningcss_sota`
- Track2:
  `independent_verified:cssparser-0.34:StyleSheetParser+RuleBodyParser:bench/nonjson_css_l4.rs`
- Diagnostic nonproducer:
  `scalar_reference=pass:cssparser_oracle;checkasm_or_parity=pass:three_way_fact_stream;json_guard_state=refreshed:skv12-w1a-json-guard-criterion:guards-pass`
- Comparator evidence:
  `track1_generated[plane=css_l4_declaration_value_fact_stream,strictness=strict,freshness=same-run-native,sidecar=n/a,mbps=429.34420791225705,source=../restart/skinny/tranches/sk-v12/research/w1b/artifacts/track1-facts.txt]; cssparser_oracle[plane=css_l4_declaration_value_fact_stream,strictness=strict,freshness=same-run-independent-oracle,sidecar=n/a,mbps=217.42665242186035,source=../restart/skinny/tranches/sk-v12/research/w1b/artifacts/oracle-facts.txt]; lightningcss_strict[plane=css_l4_declaration_value_fact_stream,strictness=strict,freshness=same-run-native,sidecar=same-plane-source-sidecar,mbps=168.92962215656692,threshold_mbps=169.92962215656692,margin_mbps=259.41458575569015,source=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar,artifact=../restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt,facts=../restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt]; strict_equality[status=pass,three_way=pass:track1=cssparser=lightningcss,sha256=caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c]`

## Gate Boundary

The legacy `gate --check-results` renderer is JSON-row shaped. W5 should not
pretend it can round-trip the new CSS row. The CSS row's gate consumption
remains the dedicated W1b-2b companion report gate; W5 should re-run that gate
and the JSON guard AWK proof, then record that division explicitly.

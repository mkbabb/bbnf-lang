# SK-V12 W1b-2 A3 - Report And Gate

Date: 2026-05-20.
Phase: W1b-2 research.
Scope: companion report/gate surface for the lightningcss comparator.

## Finding

Create a W1b-2-specific companion report schema instead of overloading
`sk-v12-nonjson-generated-v1`. The existing SK-V12 non-JSON intervention logic
assumes a baseline-relative threshold; the user pin requires
`track1_mbps > lightningcss_mbps + 1`.

Recommended schema id:

```text
sk-v12-css-l4-sota-v1
```

Report path:

```text
restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json
```

## Required Row Fields

The row should preserve W1b-1 identity/generated-size fields and add explicit
comparator/admission fields:

- `row_id = css_l4/declaration_values/direct_to_struct/main`
- `grammar_id = css_l4`
- `output_plane = css_l4_declaration_value_fact_stream`
- `strictness = strict`
- `track1_mbps`
- `cssparser_mbps`
- `lightningcss_mbps`
- `threshold_mbps = lightningcss_mbps + 1`
- `admission_margin_mbps = track1_mbps - threshold_mbps`
- `lightningcss_version`
- `lightningcss_build_hash`
- `lightningcss_artifact_path`
- `lightningcss_fact_artifact_path`
- `strict_output_equality = pass`
- `three_way_equality = pass:track1=cssparser=lightningcss`
- `sample_count >= 30`
- `sample_cost`
- `host_triple`, `feature_mask`, `build_flags`
- `generated_loc`, `generated_module_bytes`, `grammar_size_guard`
- `lock14_status`, `lock16_status`, `json_guard_state`
- `redress_entry = REDRESS-124`
- `same_wave_consumer_class = companion_gate_css_l4_lightningcss_sota`
- `admission_status`
- `gate_status`

## Gate Rules

The gate must derive, not trust, admission status:

- `PASS-ADMIT-CANDIDATE` when `track1_mbps > lightningcss_mbps + 1`.
- `PASS-MEASURED-BASELINE` when equality and measurement pass but
  `track1_mbps <= lightningcss_mbps + 1`.
- Validation failure for missing comparator, stale identity, failed equality,
  missing generated-size fields, or unconsumed gate context.

Code touch points:

- `skinny/crates/bbnf-bench/src/report.rs`: add
  `SKV12_CSS_L4_SOTA_REPORT_SCHEMA`, `SkV12CssL4SotaReport`,
  `SkV12CssL4SotaRow`, `from_json_str()`, and `validate_gate()`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`: add
  `--skv12-css-l4-sota-report <path>`, include it in companion flag
  exclusivity, and print either
  `G-W1b-2-CSS-L4-LIGHTNINGCSS PASS-ADMIT-CANDIDATE ...` or
  `G-W1b-2-CSS-L4-LIGHTNINGCSS PASS-MEASURED-BASELINE ...`.

`skinny/RESULTS.md` remains JSON authority unless W1b-2 intentionally records
a CSS ADMIT surface or a measured JSON guard demotion.

# SK-V12 W1b-2b Plan V2 - Narrow CSS L4 SOTA Gate

Date: 2026-05-20.
Phase: revised plan after CHALLENGE REVISE.
Risk class: high.
Hard cap: 30 min.
LOC budget: <=220 report/gate/test lines.

## Disposition

This plan supersedes `PLAN.md` and stale research text in A1/A6. W1b-2b uses
only `REDRESS-125` and gate `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`.

Keep W1b-2b inside the Section 7.2 cap by narrowing redress to a companion
report/gate validator plus one measured report artifact. Do not widen
`sk-v12-nonjson-generated-v1`; do not add a CSS row to `skinny/RESULTS.md`
unless the gate returns `PASS-ADMIT-CANDIDATE`.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`
- `skinny/REDRESS.md`
- `skinny/RESULTS.md` only for CSS `PASS-ADMIT-CANDIDATE` or accepted JSON
  guard demotion

## Source Budget

- report schema and pure validation: <=90 lines.
- gate flag and shared companion parser extension: <=35 lines.
- compact Criterion verifier in `gate.rs`: <=45 lines.
- focused unit tests: <=50 lines.

Defer broad integration shell checks and large negative matrices to REDRESS
command evidence. Do not add a report writer helper in source; the redress
agent writes the JSON artifact from the measured Criterion values.

## Schema

Add `SKV12_CSS_L4_SOTA_REPORT_SCHEMA = "sk-v12-css-l4-sota-v1"` and
`SkV12CssL4SotaReport { schema_id, wave_id, run_id, rows }` with exactly one
row. The row keeps only fields needed by Section 7.2:

- identity: `row_id`, `grammar_id`, `domain`, `corpus_or_workload`, `workload`,
  `output_plane`, `strictness`, `outcome_id`, `verdict`, `gate_status`;
- generated/source proof: `generated_track1_source_path`,
  `generated_runtime_path`, `generated_input_provenance`, `grammar_checksum`,
  `input_checksum`, `input_bytes`, `generated_loc`, `generated_module_bytes`,
  `grammar_size_guard`;
- comparator proof: `track1_mbps`, `track2_or_oracle_mbps`,
  `lightningcss_mbps`, `threshold_mbps`, `admission_margin_mbps`,
  `admission_status`, `track1_artifact`, `cssparser_artifact_path`,
  `lightningcss_artifact`, `lightningcss_fact_artifact_path`,
  `strict_output_equality`, `three_way_equality`,
  `lightningcss_sequence_status`, `track2_independence_status`;
- gate context: `benchmark_artifact_path`, `sample_count`, `sample_cost`,
  `host_triple`, `feature_mask`, `build_flags`, `lock14_status`,
  `lock16_status`, `scalar_reference_status`, `checkasm_or_parity_status`,
  `json_guard_state`, `same_wave_consumer_class`, `redress_entry`.

Pure validation rejects unknown fields and requires:

- exact row `css_l4/declaration_values/direct_to_struct/main`;
- exact plane `css_l4_declaration_value_fact_stream`;
- `wave_id == SK-V12-W1b-2b`;
- `redress_entry == REDRESS-125`;
- `same_wave_consumer_class == companion_gate_css_l4_lightningcss_sota`;
- fixture SHA-256
  `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`;
- `input_bytes == 187`;
- `lightningcss_fact_artifact_path` names the retained W1b-2a
  `lightningcss-facts.txt`;
- `strict_output_equality == pass`;
- `three_way_equality == pass:track1=cssparser=lightningcss`;
- `lightningcss_sequence_status == pass:ast_projection_matches_source_sidecar`;
- `track2_independence_status == independent_verified`;
- `lock14_status == pass:lock14_baseline::validate`;
- `lock16_status == n/a:no_simd_or_asm_claim`;
- `redress_entry == REDRESS-125`.

The validator derives `threshold_mbps = lightningcss_mbps + 1` and
`admission_margin_mbps = track1_mbps - threshold_mbps` with a small float
tolerance. Equality at the threshold is `PASS-MEASURED-BASELINE`, not admit.

## Executable Criterion Authority

The gate, not the report, is throughput authority. After pure report
validation, `gate.rs` reads only the three `new/` lanes under
`criterion_root()/nonjson_css_l4/`:

- `track1_generated_css_l4_decl_values`;
- `track2_cssparser_oracle`;
- `lightningcss_same_plane_fact_stream`.

For each lane, require:

- `new/benchmark.json` has `throughput.Bytes == 187`;
- `new/estimates.json` has finite positive `mean.point_estimate`;
- `new/sample.json.iters.len() >= 30`.

Compute `mbps = 187 * 8000 / mean_ns` and compare the report's Track 1,
cssparser, lightningcss, threshold, margin, and sample count to the computed
values. No fallback to `base/`, `change/`, report-only Mbps, or hand-entered
values is allowed.

Artifact freshness checks are bounded to strings:

- all report artifact paths must contain `nonjson_css_l4`;
- Track 1/csparser/lightningcss artifact paths must name their lane;
- equality/fact artifact paths must name the retained W1b artifacts;
- report `run_id` must begin `sk-v12-w1b-2b:criterion-fnv64-`.

The report path remains the SPEC owner path
`restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`;
this is intentional because W1b owns the CSS L4 fixture/artifact directory.

## Gate CLI

Add `--skv12-css-l4-sota-report <path>` to the shared companion-report parser.
It must reject missing paths, flag-as-path, duplicate/mixed companion reports,
write/update flags, volatile probes, and unrelated extra args. Allowed
co-flags are only `--advisory`, `--check-results`, and `--with-cost-facts`.

On success print:

```text
G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA <admission_status> <path>
```

If no JSON guard flag is present, return after the CSS gate. If
`--check-results` or `--with-cost-facts` is present, validate the CSS gate
first and then continue into the existing JSON gate path. CSS-only Criterion
roots are rejected by that JSON path because the required JSON groups and SIMD
rows are absent.

## Outcome Routing

- `PASS-ADMIT-CANDIDATE`: strict equality, independent oracle, live Criterion
  verification, JSON guard state, and `track1_mbps > lightningcss_mbps + 1`.
  May move `skinny/RESULTS.md` for the CSS row and records REDRESS 125.
- `PASS-MEASURED-BASELINE`: same evidence, but
  `track1_mbps <= lightningcss_mbps + 1`. Records REDRESS 125 and does not
  move `skinny/RESULTS.md`.
- `BLOCKED/FAIL`: missing report, stale/invalid Criterion lane, equality
  failure, bad JSON guard root, no-write matrix failure, or source/gate failure.
  Save `/tmp/skv12-waveW1b-2b-rejected.patch`.

## Tests And Commands

Focused tests:

- `cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture`;
- `cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture`;
- `cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture`;
- `cargo test -p bbnf-bench lock14 -- --nocapture`.

Redress evidence commands:

```sh
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30

CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p bbnf-bench --bin gate -- \
  --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory --check-results
```

Before and after the no-write guard command, compare `skinny/RESULTS.md`
bytes. For `PASS-MEASURED-BASELINE`, the file must remain byte-identical.

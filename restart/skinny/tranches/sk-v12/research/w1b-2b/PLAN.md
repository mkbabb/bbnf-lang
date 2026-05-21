# SK-V12 W1b-2b Plan - CSS L4 Lightningcss SOTA Report/Gate

Agent: PLAN P1.
Risk class: high.
Hard cap: 30 min.
LOC budget: <=220 hand/test lines.
Source edit status for this plan: none.

## Selected Intervention

Implement the W1b-2b companion report/gate surface for
`G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`. This is a report/gate-only intervention:
add a dedicated `sk-v12-css-l4-sota-v1` validator and
`--skv12-css-l4-sota-report <path>` gate flag that consume already-landed
W1b-2a CSS L4 Criterion and equality artifacts. Do not widen
`sk-v12-nonjson-generated-v1`; keep W1b-1 baseline behavior isolated.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`: schema structs, validation,
  threshold/margin derivation checks, and report unit tests.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`: companion flag extraction,
  no-write/probe rejection, JSON guard continuation, PASS line, and CLI tests.
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`:
  W1b-2b report artifact consumed by the gate.
- `skinny/REDRESS.md`: REDRESS 125 outcome evidence.
- `skinny/RESULTS.md`: move only for `PASS-ADMIT-CANDIDATE` or a measured JSON
  guard demotion accepted by the existing JSON gate.

## Report Schema

Add `SKV12_CSS_L4_SOTA_REPORT_SCHEMA = "sk-v12-css-l4-sota-v1"` with
`SkV12CssL4SotaReport { schema_id, wave_id, run_id, rows }`. Validation must
require `schema_id == sk-v12-css-l4-sota-v1`, `wave_id == SK-V12-W1b-2b`,
`is_skv12_run_id(run_id)`, and exactly one row.

Required row fields:

- Identity: `row_id`, `grammar_id`, `domain`, `corpus_or_workload`,
  `workload`, `output_plane`, `strictness`, `outcome_id`, `verdict`,
  `gate_status`.
- Generated provenance: `generated_track1_source_path`,
  `generated_runtime_path`, `generated_input_provenance`, `grammar_checksum`,
  `input_checksum`, `input_bytes`, `generated_loc`,
  `generated_module_bytes`, `grammar_size_guard`.
- Measurements/artifacts: `track1_mbps`, `track1_artifact`,
  `track2_or_oracle_mbps`, `track2_or_oracle_source_path`,
  `track2_independence_status`, `cssparser_artifact_path`,
  `lightningcss_mbps`, `lightningcss_version`, `lightningcss_command`,
  `lightningcss_artifact`, `lightningcss_fact_artifact_path`,
  `benchmark_artifact_path`, `measured_validation_path`, `profile_artifact`,
  `sample_count`, `sample_cost`.
- Admission math: `threshold_mbps`, `admission_margin_mbps`,
  `admission_status`.
- Guards/context: `strict_output_equality`, `three_way_equality`,
  `lightningcss_sequence_status`, `host_triple`, `feature_mask`,
  `build_flags`, `lock14_status`, `lock16_status`,
  `scalar_reference_status`, `checkasm_or_parity_status`,
  `json_guard_state`, `same_wave_consumer_class`, `redress_entry`.

Hard validation constants:

- `row_id == css_l4/declaration_values/direct_to_struct/main`.
- `grammar_id == css_l4`.
- `domain` includes `non_json_generated`, `css_l4`, and
  `declaration_values`.
- `corpus_or_workload == declaration_values`.
- `workload == direct_to_struct`; reject `parse_only`.
- `output_plane == css_l4_declaration_value_fact_stream`.
- `strictness == strict`.
- `same_wave_consumer_class == companion_gate_css_l4_lightningcss_sota`.
- `redress_entry == REDRESS-125`.
- `input_checksum ==
  cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`.
- `input_bytes == 187`.
- `grammar_size_guard == pass:generated_loc<=360`.
- `lightningcss_version == 1.0.0-alpha.71`.
- `track2_independence_status == independent_verified`.
- `strict_output_equality == pass`.
- `three_way_equality == pass:track1=cssparser=lightningcss`.
- `lock14_status == pass:lock14_baseline::validate`.
- `json_guard_state == not_refreshed:no_behavior_drift` or
  `json_guard_state` starts with `refreshed:` and contains `guards-pass`.

Derive and verify, do not trust: `threshold_mbps = lightningcss_mbps + 1` and
`admission_margin_mbps = track1_mbps - threshold_mbps`.

## Criterion Consumption

Consume only `new/` Criterion artifacts under
`skinny/target/criterion/nonjson_css_l4/`:

- `track1_generated_css_l4_decl_values/new/{benchmark.json,estimates.json,sample.json}`.
- `track2_cssparser_oracle/new/{benchmark.json,estimates.json,sample.json}`.
- `lightningcss_same_plane_fact_stream/new/{benchmark.json,estimates.json,sample.json}`.

Use `benchmark.json.throughput.Bytes == 187`, `estimates.json.mean.point_estimate`
as ns/iter, and `sample.json.iters.len() >= 30`. Compute Mbps as
`bytes * 8000 / mean_ns`. Do not read `base/` for gate decisions and treat
`change/` as advisory only. Missing, malformed, stale, non-finite, or
sample-deficient lanes fail closed.

Current W1b-2a means establish the expected shape:

- Track 1 generated: `429.344208 Mbps`.
- cssparser oracle: `217.426652 Mbps`.
- lightningcss same-plane: `168.929622 Mbps`.
- Threshold: `169.929622 Mbps`.
- Margin: `259.414586 Mbps`.

These numbers are evidence, not baked constants; the implementation must
recompute them from the consumed Criterion files and bind artifact paths to
the W1b-2b `run_id`.

## Gate Flag And CLI

Add `skv12_css_l4_sota_report_path(args)` beside the existing companion
helpers and include `--skv12-css-l4-sota-report` in the shared companion flag
count. On success, print:

```text
G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA PASS <path>
```

Allowed companion combinations are only `--advisory`, `--check-results`, and
`--with-cost-facts`. Reject missing path, flag-as-path, duplicate companion
flags, mixed companion reports, unrelated extra args, `--update-results`,
`--write-results`, and `--include-volatile-probes`. If the companion report
passes and no JSON guard flag is present, return immediately. If
`--check-results` or `--with-cost-facts` is present, validate the CSS report
first and then continue through the existing JSON gate path.

No-write JSON guard command:

```sh
CRITERION_HOME=/path/to/populated-json-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json \
  --skv12-css-l4-sota-report restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory --check-results
```

The guard root must contain accepted JSON `json_<fixture>/...` and SIMD scan
groups. A CSS-only Criterion root is invalid for JSON guard proof.

## Outcome Routing

REDRESS 125 is the W1b-2b slot; REDRESS 124 belongs to W1b-2a.

- `PASS-ADMIT-CANDIDATE`: all invariants pass,
  `track1_mbps > lightningcss_mbps + 1`, positive margin, strict three-way
  equality, independent cssparser oracle, telemetry consumed, JSON guards held
  or demoted. Move `skinny/RESULTS.md` for the CSS ADMIT candidate row and
  record REDRESS 125.
- `PASS-MEASURED-BASELINE`: invariants pass but
  `track1_mbps <= lightningcss_mbps + 1`. Record REDRESS 125 as measured CSS
  miss/FIXPOINT evidence and do not move `skinny/RESULTS.md` unless a measured
  JSON guard demotion is also accepted.
- `BLOCKED/FAIL`: comparator, equality, oracle independence, generated-size,
  throughput extraction, report validation, gate consumption, no-write matrix,
  JSON guard root, or stale-results guidance fails. Record REDRESS 125 and do
  not move `skinny/RESULTS.md` except for an accepted JSON guard demotion.

W3/W4 may proceed after a measured CSS row; W5 owns final ADMIT/FIXPOINT
reconciliation.

## Tests

Report tests in `skinny/crates/bbnf-bench/src/report.rs`:

- `skv12_css_l4_sota_report_accepts_admit_candidate`.
- `skv12_css_l4_sota_report_accepts_measured_baseline`.
- `skv12_css_l4_sota_report_derives_threshold_and_margin`.
- `skv12_css_l4_sota_report_rejects_required_failure_classes`.
- `skv12_css_l4_sota_report_rejects_unknown_producer_fields`.

CLI tests in `skinny/crates/bbnf-bench/src/bin/gate.rs`:

- `skv12_css_l4_sota_report_arg_extracts_single_path`.
- `skv12_css_l4_sota_report_arg_allows_no_write_json_check_flags`.
- `skv12_css_l4_sota_report_arg_rejects_update_results_combination`.
- `skv12_css_l4_sota_report_arg_rejects_probe_combination`.
- `skv12_css_l4_sota_report_arg_rejects_mixed_companion_reports`.
- `skv12_css_l4_sota_report_arg_rejects_missing_or_flag_path`.

Minimum command set:

```sh
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture
```

Add one no-write command check that snapshots `skinny/RESULTS.md`, runs the
companion report with `--advisory --check-results` against a populated JSON
guard root, and verifies byte-identical `RESULTS.md`. Add one negative command
check with an empty CSS-only Criterion directory and require failure.

## Revert Protocol

If rejected, revert the W1b-2b gate/report/result slice only, preserve unrelated
work, and save the rejected diff to:

```text
/tmp/skv12-waveW1b-2b-rejected.patch
```

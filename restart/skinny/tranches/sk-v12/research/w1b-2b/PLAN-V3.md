# SK-V12 W1b-2b Plan V3 - CSS L4 SOTA Gate With Split Evidence Roots

Date: 2026-05-20.
Phase: revised plan after CHALLENGE V2 REVISE.
Risk class: high.
Hard cap: 30 min redress.
Source budget: <=330 report/gate/test LOC, replacing the stale SPEC Section 2
`<=220` estimate for W1b-2b only. This is a budget correction, not an owner
surface expansion.

## Disposition

This plan supersedes `PLAN.md`, `PLAN-V2.md`, and stale research text in A1/A6.
W1b-2b uses gate `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` and `REDRESS-125` only.

W1b-2b is a measured companion-gate row disposition, not a RESULTS-renderer
wave. It records a CSS SOTA report and REDRESS-125. `skinny/RESULTS.md`
reconciliation is routed to W5 close even when the gate returns
`PASS-ADMIT-CANDIDATE`, so the existing JSON stale-results check stays bounded.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v12/SPEC.md` for the W1b-2b LOC-budget
  correction only

## Budget Correction

CHALLENGE V2 CH4 showed that a gate which consumes three Criterion lanes plus
retained fact/equality artifacts cannot honestly fit the original
`<=220 report/gate/test` estimate. Revise the W1b-2b budget to
`<=330 report/gate/test` while preserving the 30-minute redress cap. At 0.9x
cap the redress agent commits or records the blocking state; at cap it halts
and saves `/tmp/skv12-waveW1b-2b-rejected.patch` if source was attempted.

The budget remains bounded by forbidding renderer changes, broad RESULTS
stale-check changes, fresh benchmark harness work, or source outside the owner
paths above.

## Report Schema

Add `SKV12_CSS_L4_SOTA_REPORT_SCHEMA = "sk-v12-css-l4-sota-v1"` and a
dedicated report validator with exactly one row. The row must gate-consume all
SPEC Section 0.4 CSS telemetry required for this disposition:

- identity: `schema_id`, `wave_id`, `run_id`, `row_id`, `grammar_id`, `domain`,
  `corpus_or_workload`, `workload`, `output_plane`, `strictness`,
  `outcome_id`, `verdict`, `gate_status`;
- generated/source proof: `generated_track1_source_path`,
  `generated_runtime_path`, `generated_input_provenance`, `grammar_checksum`,
  `input_checksum`, `input_bytes`, `generated_loc`, `generated_module_bytes`,
  `grammar_size_guard`;
- comparator proof: `track1_mbps`, `track2_or_oracle_mbps`,
  `lightningcss_mbps`, `threshold_mbps`, `admission_margin_mbps`,
  `admission_status`, `track1_artifact`, `cssparser_artifact_path`,
  `track2_or_oracle_source_path`, `lightningcss_command`,
  `lightningcss_artifact`, `lightningcss_fact_artifact_path`,
  `fact_stream_sha256`, `strict_output_equality`, `three_way_equality`,
  `lightningcss_sequence_status`, `track2_independence_status`;
- gate context: `measured_validation_path`, `benchmark_artifact_path`,
  `profile_artifact`, `sample_count`, `sample_cost`, `host_triple`,
  `feature_mask`, `build_flags`, `lock14_status`, `lock16_status`,
  `scalar_reference_status`, `checkasm_or_parity_status`, `json_guard_state`,
  `same_wave_consumer_class`, `redress_entry`.

Pure validation rejects unknown fields and requires:

- exact row `css_l4/declaration_values/direct_to_struct/main`;
- exact plane `css_l4_declaration_value_fact_stream`;
- `wave_id == SK-V12-W1b-2b`;
- `redress_entry == REDRESS-125`;
- `same_wave_consumer_class == companion_gate_css_l4_lightningcss_sota`;
- fixture SHA-256
  `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`;
- `input_bytes == 187`;
- `track2_or_oracle_source_path` names the cssparser oracle source and cannot
  name generated Track 1;
- `lightningcss_command` names `lightningcss-1.0.0-alpha.71` and same-plane
  source-sidecar projection;
- `measured_validation_path` names the strict three-way equality artifact or
  gate artifact consumed below;
- `profile_artifact` is non-empty and may be
  `n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion` because this wave consumes
  already-landed W1b-2a Criterion artifacts rather than profiling a new kernel;
- `strict_output_equality == pass`;
- `three_way_equality == pass:track1=cssparser=lightningcss`;
- `lightningcss_sequence_status == pass:ast_projection_matches_source_sidecar`;
- `track2_independence_status == independent_verified`;
- `lock14_status == pass:lock14_baseline::validate`;
- `lock16_status == n/a:no_simd_or_asm_claim`;
- `scalar_reference_status == pass:cssparser_oracle`;
- `checkasm_or_parity_status == pass:three_way_fact_stream`.

The validator derives `threshold_mbps = lightningcss_mbps + 1` and
`admission_margin_mbps = track1_mbps - threshold_mbps` with a small float
tolerance. Equality at the threshold is `PASS-MEASURED-BASELINE`, not admit.

## Executable Evidence Protocol

Use two separate commands; never one shared `CRITERION_HOME` for CSS and JSON.

1. CSS SOTA gate:

   - Uses default `target/criterion` unless `CARGO_TARGET_DIR` routes it.
   - Runs `--skv12-css-l4-sota-report <path>` without `--check-results`.
   - Reads only
     `criterion_root()/nonjson_css_l4/{track1_generated_css_l4_decl_values,track2_cssparser_oracle,lightningcss_same_plane_fact_stream}/new/`.

2. JSON guard/stale check:

   - Runs the existing JSON gate separately with
     `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion`.
   - Uses `--advisory --check-results`.
   - The CSS report flag is absent in this command.

For each CSS lane, require `new/benchmark.json` has `throughput.Bytes == 187`,
`new/estimates.json` has finite positive `mean.point_estimate`, and
`new/sample.json.iters.len() >= 30`. Compute Mbps as
`187 * 8000 / mean_ns` and compare the report's Track 1, cssparser,
lightningcss, threshold, margin, and sample count to computed values. No
fallback to `base/`, `change/`, report-only Mbps, or hand-entered values is
allowed.

## Artifact Freshness And Comparator Isolation

The gate reads the retained W1b files named by the report:

- `track1-facts.txt`;
- `oracle-facts.txt`;
- `lightningcss-facts.txt`;
- `strict-equality.txt`;
- `lightningcss-strict-equality.txt`.

It verifies the three fact streams are byte-identical and their SHA-256 equals
`fact_stream_sha256`. It verifies each fact stream contains the exact row id,
plane, `input_fnv64=27240148e5780a54`, `input_bytes=187`, and
`stream_fnv64=285dd62f19dea4a8`. It verifies equality artifacts have
`status=pass`, the exact row id, and the retained W1b run id
`sk-v12-w1b-1:fixture-fnv64-27240148e5780a54`; this retained run id is accepted
because Section 7.2 consumes W1b-2a/W1b-1 artifacts rather than regenerating
the fact streams.

Redress also records a focused source audit:
`lightningcss_facts` may call lightningcss parse/projection and fixture-sidecar
span emission, but must not call `oracle_facts`, `ParserInput`, `Parser`, or
cssparser parser APIs. A failure routes W1b-2b to BLOCKED/FAIL.

## Gate CLI

Add `--skv12-css-l4-sota-report <path>` to the shared companion-report parser.
It must reject missing paths, flag-as-path, duplicate/mixed companion reports,
write/update flags, volatile probes, and unrelated extra args. Allowed co-flags
are only `--advisory`, `--check-results`, and `--with-cost-facts`, but the
redress command for W1b-2b uses CSS-only mode without JSON co-flags.

On success print:

```text
G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA <admission_status> <path>
```

If JSON co-flags are present despite the preferred two-command protocol, the
CSS gate still validates first and then continues into the existing JSON gate
path using the current process `criterion_root()`. Redress does not rely on
that mixed-root shape.

## Outcome Routing

- `PASS-ADMIT-CANDIDATE`: strict equality, independent oracle, consumed
  artifact freshness, live Criterion verification, JSON guard command passes,
  and `track1_mbps > lightningcss_mbps + 1`. Records REDRESS-125 and the CSS
  report. `skinny/RESULTS.md` is not moved until W5 close.
- `PASS-MEASURED-BASELINE`: same evidence, but
  `track1_mbps <= lightningcss_mbps + 1`. Records REDRESS-125 and leaves
  `skinny/RESULTS.md` byte-identical.
- `BLOCKED/FAIL`: missing report, stale/invalid Criterion lane, stale/missing
  fact/equality artifact, comparator isolation failure, bad JSON guard root,
  no-write failure, or source/gate failure. Save
  `/tmp/skv12-waveW1b-2b-rejected.patch`.

## Tests And Commands

Focused tests:

- `cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture`;
- `cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture`;
- `cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture`;
- `cargo test -p bbnf-bench lock14 -- --nocapture`.

Redress evidence commands:

```sh
RUSTFLAGS="-C target-cpu=native" \
cargo run -p bbnf-bench --bin gate -- \
  --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory

CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p bbnf-bench --bin gate -- --advisory --check-results
```

If the existing W1b-2a CSS Criterion lanes are missing or stale, rerun:

```sh
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30
```

Before and after the JSON guard command, compare `skinny/RESULTS.md` bytes.
W1b-2b must not move RESULTS in any outcome; W5 owns close reconciliation.

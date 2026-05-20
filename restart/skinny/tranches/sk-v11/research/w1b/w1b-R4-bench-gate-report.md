# SK-V11 W1b R4: Bench/Gate/Report Integration

Scope: research-only integration plan for `G-W1b-NONJSON-BASELINE`, focused on
how W1b produces exactly one generated non-JSON baseline report and has it
consumed by the W1a-established non-JSON gate lane, without moving
`skinny/RESULTS.md` or JSON rows.

## Finding

W1b should not render a non-JSON row into `skinny/RESULTS.md`. The safe shape is
one companion generated baseline report under the W1b research tree, validated
by `bbnf-bench --bin gate` through the non-JSON gate/report lane that W1a
created.

The report must be a real W1b baseline report, not a reused W1a schema fixture.
The current W1a implementation accepts only:

- `schema_version = "sk-v11-w1a-nonjson-v1"`
- `wave_id = "SK-V11-W1a"`
- `run_id = "sk-v11-w1a:fixture-fnv64-<16 hex>"`
- `outcome_id = "S"` and `verdict = "NO-GO"`
- `measured_validation_path = "schema-only"`
- `profile_artifact` beginning `fixture:w1a:`
- `same_wave_consumer_class = "non_json_gate_schema_only"`
- a single `internal_oracle` with source sentinel
  `oracle:w1a:<grammar>:<corpus>:<workload>:<plane>`

That is correct for W1a, but it cannot establish W1b baseline authority. W1b
therefore needs a sibling W1b baseline validator/report mode in the same
`bbnf-bench` gate/report surface, while preserving the existing W1a validator
and fixtures unchanged.

## Required Report Shape

Write exactly one generated report:

`restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-declaration-values.json`

Preferred selected row:

`css_l4/declaration_values/direct/main`

Required report facts:

- `schema_version = "sk-v11-w1b-nonjson-baseline-v1"`
- `wave_id = "SK-V11-W1b"`
- one report row only; no JSON rows and no second non-JSON row
- `run_id = "sk-v11-w1b:criterion-fnv64-<16 lowercase hex>"`
- `grammar_id = "css_l4"` and `domain = "css_l4_bench"`
- `corpus = "declaration_values"` and `workload = "direct"`
- `outcome_id = "S"` and `verdict = "NO-GO"` so the baseline is non-admitting
- `strictness = "strict"`, `parse_utf8 = "measured-row"`,
  `escape_complete = "yes"`, `output_plane = "digest"`
- finite positive generated Track 1 Mbps and independent Track 2/oracle Mbps
- real Criterion profile/source artifacts, not `fixture:w1a:` sentinels
- run id, host, build flags, feature mask, sample count, and sample cost from
  the same W1b Criterion capture
- `same_wave_consumer_class` naming the generated non-JSON direct consumer,
  e.g. `generated_non_json_direct_baseline`
- `track2_independence_status = "independent_verified"`
- oracle source path proving it does not call generated Track 1, generated
  SinkOnly helpers, generated typed helpers, hidden benchmark-private parser
  code, JSON providers, or old hand-only non-JSON runtimes

This report may be JSON as a file format. "Non-JSON" here means the benchmarked
grammar row is non-JSON. Do not create a Markdown baseline table and do not add
the row to `skinny/RESULTS.md`.

## Gate Integration

Use the W1a-established non-JSON gate lane in `bbnf-bench --bin gate`, but do
not loosen `validate_w1a_non_json_gate`.

Recommended implementation split:

- keep `--w1a-non-json-report <path>` and all W1a fixtures/tests unchanged;
- add `W1B_NON_JSON_BASELINE_REPORT_SCHEMA` and
  `validate_w1b_non_json_baseline_gate` in
  `skinny/crates/bbnf-bench/src/report.rs`;
- add a sibling CLI hook in `skinny/crates/bbnf-bench/src/bin/gate.rs`, e.g.
  `--w1b-non-json-baseline-report <path>`;
- optionally add a write mode only for the one generated report, e.g.
  `--write-w1b-non-json-baseline-report <path>`, sourced from Criterion data;
- make the W1b validator reject W1a fixture sentinels, `gate_only`,
  `non_json_gate_schema_only`, `A / GO`, `SK-V11-W1a`, JSON grammar/domain,
  extra rows, extra producer-only fields, mixed run ids, and any unconsumed
  telemetry.

The important contract is that W1b's generated report is consumed by the same
gate binary and report module that W1a established. `xtask gate-json` remains a
JSON preservation command only.

## Bench Integration

Add one Criterion bench target for the selected row, not a broad non-JSON suite.

Required owner path:

`skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`

Required Criterion group names should be stable and non-JSON-specific, for
example:

- group: `nonjson/css_l4/declaration_values`
- generated Track 1 bench: `track1_generated_direct`
- independent oracle bench: `track2_oracle_direct`

The gate/report code can then map those artifacts into:

- `profile_artifact =
  "criterion-slope-profile:nonjson_css_l4_declaration_values/track1_generated_direct/new/estimates.json"`
- oracle source artifact naming the independent oracle bench/source, not a
  W1a sentinel and not a generated Track 1 path

Do not reuse the JSON `json_<fixture>` Criterion group convention, because the
current W0 gate fingerprints and metadata readers treat those as JSON
baseline rows.

## Required Fixtures And Tests

File fixtures should stay minimal. The W1b pass evidence should be the one
generated report above. Negative cases can be unit-test builders to avoid
creating extra generated-looking report files.

Required report tests in `skinny/crates/bbnf-bench/src/report.rs`:

- `w1b_non_json_baseline_accepts_exact_generated_css_l4_report`
- `w1b_non_json_baseline_rejects_multiple_rows`
- `w1b_non_json_baseline_rejects_json_grammar_or_domain`
- `w1b_non_json_baseline_rejects_w1a_schema_fixture_sentinels`
- `w1b_non_json_baseline_rejects_gate_only_or_schema_only_consumer`
- `w1b_non_json_baseline_rejects_admission_claim`
- `w1b_non_json_baseline_rejects_oracle_plane_source_or_coupling`
- `w1b_non_json_baseline_rejects_unknown_producer_fields`

Required CLI tests in `skinny/crates/bbnf-bench/src/bin/gate.rs`:

- `w1b_non_json_baseline_report_arg_extracts_single_path`
- `w1b_non_json_baseline_report_arg_rejects_json_update_flags`
- `w1b_non_json_baseline_write_arg_rejects_multiple_output_paths`

Required preservation tests:

- existing W1a `report::tests::w1a` tests remain green
- existing W1a fixture command still passes
- existing W1a negative fixture commands still fail
- existing JSON `gate-json --with-cost-facts --check-results` remains green
- `git diff --exit-code -- skinny/RESULTS.md` remains clean

## Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Focused tests:

```sh
cargo test -p bbnf-bench report::tests::w1a -- --nocapture
cargo test -p bbnf-bench report::tests::w1b -- --nocapture
cargo test -p bbnf-bench --bin gate w1a -- --nocapture
cargo test -p bbnf-bench --bin gate w1b -- --nocapture
```

Generate the one W1b Criterion capture:

```sh
CRITERION_HOME=/tmp/skv11-w1b-nonjson-css-l4 RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_baseline -- css_l4_declaration_values
```

Write the one generated companion report:

```sh
CRITERION_HOME=/tmp/skv11-w1b-nonjson-css-l4 RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --write-w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-declaration-values.json
```

Consume the generated report through the gate:

```sh
cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-declaration-values.json
```

Preserve W1a and JSON gates:

```sh
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git -C .. diff --exit-code -- skinny/RESULTS.md
```

## Owner Paths

W1b owner paths needed for this R4 slice:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs` only if non-JSON bench metadata
  needs a helper; avoid changing `TrackTag` unless required
- `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
- `restart/skinny/tranches/sk-v11/research/w1b/reports/`
- selected generated parser/codegen owner paths from SPEC Section 5:
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/lower/`,
  `skinny/crates/codegen/src/direct_schema.rs`,
  `skinny/crates/runtime/src/grammars/`, and `grammar/css/l4/`

Explicitly out of scope for this R4 integration:

- `skinny/RESULTS.md`
- JSON `json_parity` row movement
- JSON `simd_scan` row movement
- `skinny/xtask/src/main.rs` unless a later plan explicitly chooses an xtask
  wrapper; it is not needed for W1b report consumption

## Rejection Conditions

Return REVISE before redress if the plan requires any of these:

- moving a non-JSON row into `skinny/RESULTS.md`;
- regenerating or rewriting JSON rows;
- relaxing W1a's schema-only validator so W1a fixtures become baseline
  authority;
- accepting a W1b report with zero rows or more than one row;
- accepting a W1b report with `A / GO` or any behavior admission claim;
- accepting a W1b report whose oracle source can call generated Track 1 or any
  JSON provider path;
- treating profile, PMU, cycles, or benchmark-private fields as evidence unless
  the W1b validator consumes them.

Self-verdict: research-only. No source files edited; no `RESULTS.md` movement;
the only written artifact is this R4 research note.

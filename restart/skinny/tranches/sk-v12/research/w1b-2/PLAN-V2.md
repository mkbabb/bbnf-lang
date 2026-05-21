# SK-V12 W1b-2 Plan V2 - Fixture-Limited Lightningcss Comparator

Date: 2026-05-20.
Phase: W1b-2 revised plan after CHALLENGE V1.
Scope: SPEC Section 7, `G-W1b-2-CSS-L4-LIGHTNINGCSS`.

## Disposition Of V1 CHALLENGE

V1 was rejected by CH1/CH2/CH3/CH4/CH5. V2 applies the required revisions:

- lightningcss is no longer claimed to emit or verify raw token facts by itself;
- `skinny/Cargo.lock` and W1b comparator artifacts are added to the Section 7
  owner table as dependency/evidence outputs;
- commands run from the nested `skinny/` workspace and use `bbnf-bench --bin
  gate` directly, avoiding an xtask passthrough change;
- REDRESS must state the frozen fixture limits;
- the admission threshold remains exactly `track1_mbps > lightningcss_mbps + 1`.

## Selected Intervention

Add a fixture-limited lightningcss comparator for:

```text
css_l4/declaration_values/direct_to_struct/main
```

The output plane remains:

```text
css_l4_declaration_value_fact_stream
```

The W1b-2 comparator is a source-sidecar fact emitter gated by lightningcss:

1. Track 1 emits the existing generated fact stream.
2. Track 2 emits the existing independent cssparser oracle fact stream.
3. The lightningcss comparator parses with lightningcss, validates the frozen
   fixture's declaration/property/importance/depth projection, then emits the
   same source-sidecar fact stream from the original input bytes.

This is not a claim that lightningcss public APIs expose raw token facts or
source byte spans. REDRESS 124 records that the claim is limited to the frozen
fixture shape unless a later wave adds adversarial CSS fixture coverage.

## Fixture Limits

W1b-2 may claim only the current fixture shape:

- seven declarations;
- thirteen value tokens;
- one nested `@media` block;
- no declaration after an important declaration in the same block;
- no comments inside declaration values;
- no strings, URLs, custom properties, or duplicate-property cascade cases.

If these limits are exceeded, the comparator fails closed. A broader CSS SOTA
claim requires a later S-P3/wave revision with adversarial fixture coverage.

## Owner Paths

Only these Section 7 paths are editable in redress:

- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/Cargo.lock`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/track1-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/oracle-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/strict-equality.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

`skinny/RESULTS.md` is touched only for CSS ADMIT or measured JSON guard
demotion. A measured CSS miss leaves RESULTS unchanged.

## Dependency

Add a direct bench dependency:

```toml
lightningcss = { version = "=1.0.0-alpha.71", default-features = false }
```

The direct dependency keeps lightningcss comparator-local. The lockfile delta is
generated dependency evidence and is intentionally in the owner table.

## Report And Gate

Create and validate:

```text
schema_id = sk-v12-css-l4-sota-v1
report = restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json
gate flag = --skv12-css-l4-sota-report <path>
```

The gate derives:

- `PASS-ADMIT-CANDIDATE` when `track1_mbps > lightningcss_mbps + 1`;
- `PASS-MEASURED-BASELINE` when equality and measurement pass but Track 1
  misses that threshold;
- `BLOCKED/FAIL` for comparator, equality, oracle independence, generated-size,
  throughput, JSON guard, or gate consumption failure.

The report must consume the fixture checksum, generated module size, fact
artifact paths, Track 1 Mbps, cssparser Mbps, lightningcss Mbps, threshold,
margin, lightningcss version, lockfile checksum, host/build fields, Lock 14/16
status, JSON guard state, REDRESS entry, and gate status.

## Criterion Evidence

The benchmark group sets `sample_size(30)`. The redress report may use the
existing quick timer only as a pre-Criterion smoke value. Final REDRESS evidence
must cite the Criterion run and artifact paths for:

- `track1_generated_css_l4_decl_values`;
- `track2_cssparser_oracle`;
- `lightningcss_same_plane_fact_stream`.

If Criterion ingestion is not implemented in this wave, REDRESS must label the
quick-timer values as smoke values and withhold CSS ADMIT until Criterion
artifact ingestion lands in a subsequent accepted plan.

## Commands

Run from `skinny/`:

```sh
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture

RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30

CRITERION_HOME=/tmp/skv12-w1b-2-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p bbnf-bench --bin gate -- \
  --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory --check-results
```

No xtask change is part of V2.

## REDRESS 124

REDRESS 124 records:

- status: `PASS-ADMIT-CANDIDATE`, `PASS-MEASURED-BASELINE`, or
  `BLOCKED/FAIL`;
- the frozen fixture limits above;
- fixture SHA-256, input bytes, run id, host/build fields;
- Track 1/cssparser/lightningcss Mbps, threshold, and margin;
- fact artifact paths and equality status;
- lightningcss version and lockfile checksum;
- Criterion command and artifact root;
- gate command result;
- routed remainder if Criterion ingestion or broader fixture coverage is
  withheld.

Rejected patch path:

```text
/tmp/skv12-waveW1b-2-rejected.patch
```

# SK-V12 W1b-2 Plan - Lightningcss Comparator

Date: 2026-05-20.
Phase: W1b-2 plan.
Scope: SPEC Section 7, `G-W1b-2-CSS-L4-LIGHTNINGCSS`.

## Selected Intervention

Add a same-plane lightningcss comparator for the already-landed W1b-1 CSS L4
row:

```text
css_l4/declaration_values/direct_to_struct/main
```

The output plane stays:

```text
css_l4_declaration_value_fact_stream
```

The comparator is hybrid by necessity: lightningcss parses and validates the
declaration sequence, while an independent source scanner emits the same raw
fact stream shape as W1b-1. The source scanner's facts are accepted only when
the lightningcss AST sequence matches declaration depth, property name, value
span, token count, and `!important` status. Direct `cssparser` calls are
forbidden inside the lightningcss comparator; the existing cssparser path
remains Track 2.

## Owner Paths

Only these SPEC Section 7 paths are editable in redress:

- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

`skinny/RESULTS.md` is touched only for a CSS ADMIT surface or measured JSON
guard demotion. A measured CSS miss leaves RESULTS unchanged and records the
disposition in REDRESS plus the W1b-2 companion report.

## Dependency And Comparator Identity

Add a direct bench dependency:

```toml
lightningcss = { version = "=1.0.0-alpha.71", default-features = false }
```

The report records:

- `lightningcss_version = 1.0.0-alpha.71`
- `lightningcss_build_hash = Cargo.lock package checksum for lightningcss`
- `lightningcss_command = cargo bench -p bbnf-bench --bench nonjson_css_l4`
- `lightningcss_artifact_path = criterion:{run_id}:target/criterion/nonjson_css_l4/lightningcss_same_plane_fact_stream`
- `lightningcss_fact_artifact_path = restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt`

## Report And Gate

Create a W1b-2 companion schema:

```text
sk-v12-css-l4-sota-v1
```

Report path:

```text
restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json
```

Gate flag:

```text
--skv12-css-l4-sota-report <path>
```

The gate derives the result:

- `PASS-ADMIT-CANDIDATE` when `track1_mbps > lightningcss_mbps + 1`.
- `PASS-MEASURED-BASELINE` when strict equality and measurement pass but
  Track 1 misses the lightningcss bar.
- `BLOCKED/FAIL` for missing comparator identity, failed equality, missing
  generated-size fields, invalid JSON guard state, or unconsumed telemetry.

The gate must consume Track 1, cssparser Track 2, lightningcss Mbps,
threshold, margin, fixture checksum, fact artifact paths, generated LOC/module
bytes, Lock 14/16 status, JSON guard state, run id, host/build/profile fields,
and REDRESS entry.

## Bench And Equality Commands

Redress runs:

```sh
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture
```

Native measurement:

```sh
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30
```

No-write JSON guard plus CSS report consumption:

```sh
CRITERION_HOME=/tmp/skv12-w1b-2-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p bbnf-bench --bin gate -- \
  --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory --check-results
```

## REDRESS Outcomes

REDRESS 124 records one of:

- `PASS-ADMIT-CANDIDATE`: CSS row is strict-equal, Track 2 independent,
  Track 1 beats `lightningcss_mbps + 1`, gate consumes report and JSON guard.
- `PASS-MEASURED-BASELINE`: CSS row is strict-equal and measurable but Track 1
  does not beat lightningcss; route W3/W4/fixpoint evidence.
- `BLOCKED/FAIL`: comparator, equality, oracle independence, generated-size,
  throughput, or gate consumption fails.

Rejected source patch path:

```text
/tmp/skv12-waveW1b-2-rejected.patch
```

## Pre-Blocked Routes

- No Sheets or BBNF-self fallback inside this wave.
- No new directive, BIR variant, `BackendShape` variant, or public substrate
  API.
- No CSS admission based on baseline-relative math; the only admission bar is
  `track1_mbps > lightningcss_mbps + 1`.
- No AST-only lightningcss equality claim; raw fact equality must be
  byte-identical across generated Track 1, cssparser Track 2, and lightningcss
  same-plane facts.

## CHALLENGE Target

W1b-2 is high-risk and CHALLENGE-mandatory. The six lenses should focus on:

- CH1: whether the hybrid comparator is same-plane and byte-identical.
- CH2: whether Lock 14 stays clean and no JSON semantics leak into CSS.
- CH3: whether JSON guard handling is no-write unless demotion is measured.
- CH4: whether lightningcss dependency and scan overhead stay inside the
  <=300 hand/gate budget.
- CH5: whether the source scanner silently becomes a second oracle or a
  cssparser clone.
- CH6: whether `PASS-MEASURED-BASELINE` is recorded honestly instead of
  paper-closing CSS SOTA.

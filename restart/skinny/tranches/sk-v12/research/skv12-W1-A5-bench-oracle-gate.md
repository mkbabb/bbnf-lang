# SK-V12 W1 A5 - Bench, Oracle, And Gate Surface

Scope: read-only audit of the benchmark, oracle, and companion gate surface
for `G-W1-GENERATED-NONJSON-BASELINE`.

## Conclusion

The W0 SK-V12 companion gate is available and can consume a W1 non-JSON row,
but W1 still needs a real generated Track 1, an independent same-plane oracle
or Track 2, and a Criterion artifact with sample count at least 30. The
existing fixtures and oracles are JSON-only.

## Executable Report Shape

The current companion schema is `sk-v12-nonjson-generated-v1`. The accepted
row fields include generated source/runtime/provenance paths, grammar id,
domain, workload, output plane, Track 1 Mbps, oracle Mbps, sample count,
strict equality, gate status, host/build fields, same-wave consumer class, and
JSON guard state.

The gate currently accepts grammar ids `css_l4`, `sheets`, and `bbnf_self`;
rejects `json`; requires `domain` to start with `non_json_generated:`; rejects
`parse_only`; maps `direct_to_struct` to `direct_sink`; maps
`real_typed_struct` to `typed_direct`; and requires:

- Track 1 Mbps >= 1.
- oracle/Track 2 Mbps >= 1.
- sample count >= 30.
- strict output equality `pass`.
- gate status `pass`.
- verdict `GO`.

## W1 Report Recommendation

For a Sheets selection, use:

- `schema_id`: `sk-v12-nonjson-generated-v1`
- `wave_id`: `SK-V12-W1`
- `row_id`: `sheets/formula/direct_to_struct/main`
- `grammar_id`: `sheets`
- `domain`: `non_json_generated:sheets`
- `corpus_or_workload`: `formula`
- `workload`: `direct_to_struct`
- `workload_class`: `baseline`
- `output_plane`: `direct_sink`
- `strict_output_equality`: `pass`
- `oracle_status`: `same-plane:strict:independent:fresh`
- `same_wave_consumer_class`: `companion_gate_generated_baseline`
- `json_guard_state`: `not_refreshed:no_behavior_drift`

## Commands

Expected post-redress commands:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1-target CRITERION_HOME=/tmp/skv12-w1-nonjson-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo bench -p bbnf-bench --bench nonjson_baseline -- nonjson/sheets/formula

RUSTFLAGS="-C target-cpu=native" \
  cargo run -p xtask -- gate-json --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1/skv12-W1-nonjson-baseline.json

cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" \
  cargo run -p xtask -- gate-json --advisory --check-results
git diff --exit-code -- RESULTS.md
```

## Risk

The companion gate currently string-checks some provenance/oracle fields. W1
should make the report point to real bench and equality artifacts to avoid
producer-only telemetry.

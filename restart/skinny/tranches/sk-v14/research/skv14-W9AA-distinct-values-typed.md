# SK-V14 W9AA distinct_values Typed Product Probe

Date: 2026-05-27.

Disposition: ADMIT. The generated `distinct_values/real_typed_struct` product
surface moves from missing product surface to measured sustained row.

## Candidate

W9AA adds a generated typed root for `distinct_values` through the existing
`regen-real-typed` path. The source slice is:

- `skinny/crates/codegen/src/direct_schema.rs`: add
  `UnknownFieldPolicy::CaptureStringEntries` so struct products can retain
  unknown string-key/string-value entries as typed vectors.
- `skinny/crates/codegen/src/json_typed_direct.rs`: emit vector initialization,
  unknown-field string parsing, and final construction for capture fields.
- `skinny/xtask/src/real_typed_schema.rs`: add
  `parse_distinct_values -> Vec<DistinctValue<'i>>` with fixed
  `timestamp`/`seq`/`status` fields plus dynamic entry capture.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`: route Track 1 through
  regenerated `parse_distinct_values` and sidecars through the same typed
  product shape.

The admitted product is not a partial fixed-field projection. Every object
retains the full dynamic `key_*` payload as `DistinctField { key, value }`,
and the checksum folds each dynamic key/value pair.

## Correctness

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p bbnf-bench distinct_values_typed -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen unknown_string_capture -- --nocapture`

The focused generated route tests passed before measurement.

## Cold Profile

Build:

```sh
CARGO_TARGET_DIR=/tmp/skv14-distinct-typed-target RUSTFLAGS="-C target-cpu=native" cargo build --manifest-path skinny/Cargo.toml --release -p bbnf-bench --bin profile_direct
```

Run:

```sh
/tmp/skv14-distinct-typed-target/release/profile_direct 400 distinct_values real_typed_track1 0
/tmp/skv14-distinct-typed-target/release/profile_direct 400 distinct_values real_typed_track2 0
/tmp/skv14-distinct-typed-target/release/profile_direct 400 distinct_values real_typed_sonic 0
/tmp/skv14-distinct-typed-target/release/profile_direct 400 distinct_values real_typed_serde 0
```

Evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W9AA-distinct-values-typed.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W9AA-distinct-values-typed.raw.log`

| mode | Mbps | threshold role |
|---|---:|---|
| real_typed_track1 | 8827.520 | candidate |
| real_typed_track2 | 3245.184 | independent sidecar |
| real_typed_sonic | 3895.064 | strict comparator |
| real_typed_serde | 3334.552 | reference sidecar |

Admission threshold: Track 1 must exceed `sonic + 1.0` Mbps. The threshold is
3896.064 Mbps; generated Track 1 reached 8827.520 Mbps, a +4931.456 Mbps
margin. Verdict: ADMIT.

## Ledger Impact

- JSON real_typed_struct moves to 12 / 17 ADMITTED and 5 MISSING.
- `distinct_values` leaves the missing typed product queue.
- REDRESS-227 records this generated-route admit.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` carry the
  measured sustained row and cold profile evidence.

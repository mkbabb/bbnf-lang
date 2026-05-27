# SK-V14 W9AB canada Typed Product Probe

Date: 2026-05-27.

Disposition: ADMIT. The generated `canada/real_typed_struct` product surface
moves from missing product surface to measured sustained row.

## Candidate

W9AB adds a generated typed root for `canada` through the existing
`regen-real-typed` path. The source slice is:

- `skinny/crates/codegen/src/direct_schema.rs`: add `NumberString` as a direct
  scalar for exact JSON numeric lexeme retention.
- `skinny/crates/codegen/src/json_typed_direct.rs`: emit
  `Cow<'i, str>` products by slicing the already-validated JSON number span.
- `skinny/xtask/src/real_typed_schema.rs`: add
  `parse_canada -> CanadaFeatureCollection<'i>` and route GeoJSON coordinate
  arrays through the `NumberString` scalar.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`: route Track 1 through
  regenerated `parse_canada`; sidecars use `serde_json::value::RawValue` and
  `sonic_rs::RawNumber` so equality is over exact numeric lexemes, not f64
  rounding.

The admitted product is a real typed GeoJSON projection: collection type,
feature type, properties name, geometry type, and every coordinate lexeme are
materialized into the product checksum.

## Correctness

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p bbnf-bench canada_typed -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen emits_typed_direct_number_string_capture -- --nocapture`

The initial f64 route was rejected before admission because serde and sonic do
not preserve the same rounded f64 checksum for long coordinate literals. W9AB
therefore admits the exact numeric lexeme product, which is the stable JSON
semantic surface required for cross-parser parity.

## Cold Profile

Build:

```sh
RUSTC_WRAPPER= RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Run from `skinny/`:

```sh
target/release/profile_direct 400 canada real_typed_track1 0
target/release/profile_direct 400 canada real_typed_track2 0
target/release/profile_direct 400 canada real_typed_sonic 0
target/release/profile_direct 400 canada real_typed_serde 0
```

Evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W9AB-canada-typed.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W9AB-canada-typed.raw.log`

| mode | Mbps | threshold role |
|---|---:|---|
| real_typed_track1 | 4761.909 | candidate |
| real_typed_track2 | 3397.878 | independent sidecar |
| real_typed_sonic | 2736.418 | strict comparator |
| real_typed_serde | 3383.986 | reference sidecar |

Admission threshold: Track 1 must exceed `sonic + 1.0` Mbps. The threshold is
2737.418 Mbps; generated Track 1 reached 4761.909 Mbps, a +2024.491 Mbps
margin. Verdict: ADMIT.

## Ledger Impact

- JSON real_typed_struct moves to 13 / 17 ADMITTED and 4 MISSING.
- `canada` leaves the missing typed product queue.
- REDRESS-228 records this generated-route admit.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` carry the
  measured sustained row and cold profile evidence.

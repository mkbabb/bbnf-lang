# SK-V13 W13.9 Redress - Canada Typed Product Rejected

Wave: W13.9.
Disposition: REJECT.
Rejected patch: `/tmp/skv13-waveW13.9-rejected.patch`.

## Correctness

The generated Canada GeoJSON typed product patch passed schema generation and
the small Canada fixture, but failed the full-corpus strict equality gate:

```text
cargo test -p bbnf-bench canada_typed -- --nocapture
```

The failing full-fixture checksum was:

| lane | checksum |
|---|---:|
| Track 1 generated typed | 7,760,849,640,330,549,600 |
| Track 2 serde oracle | 17,574,774,450,138,172,291 |

The first isolated mismatch was a one-ULP f64 rounding difference in ring `0`,
point `4`, coordinate `1`: Track 1 materialized `43.47470900000013`
(`0x4045bcc343b70f08`), while serde/sonic materialized
`43.474709000000125` (`0x4045bcc343b70f07`).

Native Criterion was not run because `G-W13.9-TYPED-CANADA` requires strict
Track 1 / Track 2 / serde_json / sonic equality before throughput measurement.

## Material Differential

W13.9 was not a direct digest, parse-only row, count-only coordinate checksum,
or REDRESS 80 mantissa replay. The rejected patch added a generated
`CanadaDocument` product surface covering the full GeoJSON document:
top-level type, feature list, feature type, `properties.name`, geometry type,
and all nested polygon coordinate f64 values in source order. The attempt
therefore materially differs from REDRESS 119/120 direct fixpoint closures and
from the W13.1 numeric array typed product admission, but it exposed a stricter
numeric-correctness issue in the generated typed f64 materializer.

## Verification

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench --bin gate w13_canada -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_9_parent_diff_under_w13_9_scope -- --nocapture`
- `cargo test -p bbnf-bench canada_typed -- --nocapture` failed on full fixture parity.

## Routed Remainder

`json/canada/real_typed_struct/main` remains `MISSING`. A second in-tranche
Canada typed reopen requires a fresh material differential: exact f64
materialization for generated typed products, a coordinate-specific product
shape that preserves serde/sonic f64 bits without weakening strict equality, or
an architectural block proving the current numeric materializer cannot match
the strict comparator on long coordinate decimals.

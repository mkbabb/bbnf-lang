# SK-V13 W13.8 Redress - Unicode Escapes Typed Product Rejected

Wave: W13.8.
Disposition: REJECT.
Rejected patch: `/tmp/skv13-waveW13.8-rejected.patch`.

## Measurement

Native command:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/unicode_escapes/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

Measured means:

| lane | mean ns | Mbps |
|---|---:|---:|
| Track 1 generated typed | 2,055,866.299 | 511.121 |
| Track 2 serde oracle | 2,051,804.982 | 512.133 |
| sonic-rs strict typed | 1,052,917.352 | 997.986 |
| serde_json typed | 2,048,557.482 | 512.945 |

Gate threshold was `sonic + 1 Mbps = 998.986 Mbps`; Track 1 missed by
`487.865 Mbps`. Full-fixture parity passed before measurement, so the
rejection is throughput, not correctness.

## Material Differential

W13.8 was not a direct digest row, parse-only row, unicode codec proof, or
partial fixture. The rejected patch added a generated typed product root for
`unicode_escapes` covering corpus metadata plus every escaped record `id` and
`v` value, routed it through the `real_typed_struct` Track 1 consumer, and
compared it against Track 2, sonic-rs strict, and serde_json typed outputs.
This materially differs from REDRESS 70-72/103-110 and REDRESS 150/151 by
measuring the dedicated escaped-object corpus rather than the mixed unicode
object corpus or the top-level unicode string vector, but it does not meet the
pinned sonic+1 throughput bar.

## Verification

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench unicode_escapes_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_unicode_escapes -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_8_parent_diff_under_w13_8_scope -- --nocapture`
- Native Criterion command above

## Routed Remainder

`json/unicode_escapes/real_typed_struct/main` remains `MISSING` in the
rolling table. A second in-tranche `unicode_escapes` reopen triggers the
round-trip rule unless it names a fresh material differential, such as SIMD
unicode escape decode consumption, row-specific escape-allocation deletion, or
a typed product shape that avoids per-string decode overhead without weakening
strict equality.

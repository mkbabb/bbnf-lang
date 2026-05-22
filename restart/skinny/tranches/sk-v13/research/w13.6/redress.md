# SK-V13 W13.6 Redress - Unicode Mixed Typed Product Rejected

Wave: W13.6.
Disposition: REJECT.
Rejected patch: `/tmp/skv13-waveW13.6-rejected.patch`.

## Measurement

Native command:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/unicode_mixed/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

Measured means:

| lane | mean ns | Mbps |
|---|---:|---:|
| Track 1 generated typed | 2,546,648.897 | 413.518 |
| Track 2 serde oracle | 2,384,038.949 | 441.723 |
| sonic-rs strict typed | 1,496,750.608 | 703.581 |
| serde_json typed | 2,393,741.428 | 439.933 |

Gate threshold was `sonic + 1 Mbps = 704.581 Mbps`; Track 1 missed by
`291.063 Mbps`. Full-fixture parity passed before measurement, so the
rejection is throughput, not correctness.

## Material Differential

W13.6 was not a direct digest row, parse-only row, unicode codec proof, or
partial string-only surface. The rejected patch added an actual generated
typed product root for `UnicodeMixed`, including metadata and all 4,185
records with `id`, `type`, `value`, and `n`, then routed it through the
`real_typed_struct` Track 1 consumer. This materially differs from REDRESS
70-72/103-110, but it does not meet the pinned sonic+1 throughput bar.

## Verification

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench unicode_mixed_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_unicode_mixed -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_6_parent_diff_under_w13_6_scope -- --nocapture`
- Native Criterion command above

## Routed Remainder

`json/unicode_mixed/real_typed_struct/main` remains `MISSING` in the rolling
table. A second in-tranche `unicode_mixed` reopen triggers the round-trip rule
unless it names a fresh material differential, such as a decode-allocation
deletion, row-specific string borrowing policy, or SIMD unicode string decode
consumer that changes the measured hot leaf.

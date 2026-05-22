# SK-V13 W13.7 Redress - Y String Unicode Typed Product Rejected

Wave: W13.7.
Disposition: REJECT.
Rejected patch: `/tmp/skv13-waveW13.7-rejected.patch`.

## Measurement

Native command:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/y_string_unicode/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

Measured means:

| lane | mean ns | Mbps |
|---|---:|---:|
| Track 1 generated typed | 55,647.522 | 639.759 |
| Track 2 serde oracle | 49,521.276 | 718.903 |
| sonic-rs strict typed | 43,218.241 | 823.749 |
| serde_json typed | 49,419.253 | 720.387 |

Gate threshold was `sonic + 1 Mbps = 824.749 Mbps`; Track 1 missed by
`184.990 Mbps`. Full-fixture parity passed before measurement, so the
rejection is throughput, not correctness.

## Material Differential

W13.7 was not a direct digest row, parse-only row, unicode codec proof, or
partial fixture. The rejected patch added a generated typed product root for
`y_string_unicode` that parses the corpus as `Vec<Cow<'input, str>>`, routes
it through the `real_typed_struct` Track 1 consumer, and compares it against
Track 2, sonic-rs strict, and serde_json typed outputs. This materially
differs from REDRESS 70-72/103-110 and REDRESS 150 because it isolates the
row to the borrowed-string vector product surface rather than the mixed-object
unicode row, but it does not meet the pinned sonic+1 throughput bar.

## Verification

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench y_string_unicode_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_y_string_unicode -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_7_parent_diff_under_w13_7_scope -- --nocapture`
- Native Criterion command above

## Routed Remainder

`json/y_string_unicode/real_typed_struct/main` remains `MISSING` in the
rolling table. A second in-tranche `y_string_unicode` reopen triggers the
round-trip rule unless it names a fresh material differential, such as
row-specific escape decode deletion, SIMD unicode decode consumption, or a
typed product shape that avoids per-string allocation pressure without
weakening strict equality.

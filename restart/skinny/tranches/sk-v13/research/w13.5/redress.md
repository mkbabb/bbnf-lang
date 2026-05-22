# SK-V13 W13.5 Redress - GSOC Typed Product Rejected

Wave: W13.5.
Disposition: REJECT.
Rejected patch: `/tmp/skv13-waveW13.5-rejected.patch`.

## Measurement

Native command:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/gsoc-2018/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

Measured means:

| lane | mean ns | Mbps |
|---|---:|---:|
| Track 1 generated typed | 4,398,147.576 | 6,053.150 |
| Track 2 serde oracle | 4,187,369.829 | 6,357.845 |
| sonic-rs strict typed | 3,905,113.364 | 6,817.382 |
| serde_json typed | 4,151,274.132 | 6,413.127 |

Gate threshold was `sonic + 1 Mbps = 6818.382 Mbps`; Track 1 missed by
`765.232 Mbps`. Full-fixture parity passed before measurement, so the
rejection is throughput, not correctness.

## Material Differential

W13.5 was not a proof-only row, direct digest row, or root key collector. The
patch added a generated map-entry typed product root for all 1,264 GSOC
proposal records, including nested sponsor and author objects, then routed it
through the `real_typed_struct` Track 1 consumer. This materially differs from
REDRESS 70/103/105/110, but it does not meet the pinned sonic+1 throughput bar.

## Verification

- `cargo xtask regen-real-typed && cargo xtask check-real-typed`
- `cargo test -p bbnf-bench gsoc_2018_typed -- --nocapture`
- Native Criterion command above

## Routed Remainder

`json/gsoc-2018/real_typed_struct/main` remains `MISSING` in the rolling
table. A second in-tranche GSOC reopen would trigger the round-trip rule; the
next attempt must name a fresh material differential, such as schema
specialization that avoids generic map-entry string matching or a row-specific
string-copy deletion.

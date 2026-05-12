# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | Track 1 / sonic | Track 2 / sonic |
|---|---:|---|---:|---:|---:|---:|---:|
| twitter | G | NO-GO | 12470 | 10063 | 18440 | 67.6% | 54.6% |
| citm_catalog | G | NO-GO | 12246 | 11547 | 23075 | 53.1% | 50.0% |
| canada | G | NO-GO | 8895 | 8177 | 12021 | 74.0% | 68.0% |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.73 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 7187 | 702939.50 | 57.6% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6028 | 838146.34 | 48.3% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 10759 | 469564.75 | 86.3% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 9452 | 1461841.86 | 77.2% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 7571 | 1825126.72 | 61.8% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 12086 | 1143272.28 | 98.7% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.72 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 7282 | 2472921.17 | 81.9% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4259 | 4227822.65 | 47.9% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 8613 | 2090844.99 | 96.8% | PASS <=2.00x T1 |

## Notes

- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter tape materialization: 40605 tokens, 649680 logical tape bytes (1.03x input), 1064272 allocated tape bytes (1.69x input), 0 payload bytes; pairs 13345, opens 2314, closes 0, scalars 24945, sibling-skips 15660.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog tape materialization: 89517 tokens, 1432272 logical tape bytes (0.83x input), 2351040 allocated tape bytes (1.36x input), 0 payload bytes; pairs 25869, opens 21388, closes 0, scalars 42259, sibling-skips 47258.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada tape materialization: 167196 tokens, 2675136 logical tape bytes (1.19x input), 3572160 allocated tape bytes (1.59x input), 0 payload bytes; pairs 8, opens 56049, closes 0, scalars 111138, sibling-skips 56058.
- canada structural scan: 65689 Mbps; floor is 40000 Mbps.
- Overall outcome G / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.

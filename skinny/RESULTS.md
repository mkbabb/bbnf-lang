# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | Track 1 / sonic | Track 2 / sonic |
|---|---:|---|---:|---:|---:|---:|---:|
| twitter | G | NO-GO | 11780 | 10770 | 18552 | 63.5% | 58.1% |
| citm_catalog | G | NO-GO | 9286 | 10277 | 21285 | 43.6% | 48.3% |
| canada | G | NO-GO | 7334 | 7701 | 11624 | 63.1% | 66.2% |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.83 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 3969 | 1272818.63 | 33.7% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 3539 | 1427688.09 | 30.0% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 9660 | 522988.47 | 82.0% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.80 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 5378 | 2569385.88 | 57.9% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 4920 | 2808228.33 | 53.0% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 8289 | 1667007.28 | 89.3% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 3776 | 4769478.80 | 51.5% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4233 | 4254150.73 | 57.7% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 8645 | 2082990.80 | 117.9% | PASS <=2.00x T1 |

## Notes

- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 47672 offsets, 190688 logical offset bytes (0.30x input), 190688 allocated offset bytes (0.30x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 36198, numbers 2109, literals 4737, separators 0.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 111639 offsets, 446556 logical offset bytes (0.26x input), 446556 allocated offset bytes (0.26x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 53208, numbers 14392, literals 1263, separators 0.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223248 offsets, 892992 logical offset bytes (0.40x input), 892992 allocated offset bytes (0.40x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 24, numbers 111126, literals 0, separators 0.
- canada structural scan: 48362 Mbps; floor is 40000 Mbps.
- Overall outcome G / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.

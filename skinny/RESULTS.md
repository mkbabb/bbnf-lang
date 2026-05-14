# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 16294 | 16068 | 20810 | 14352 | 12042 | sonic-rs | 20810 | 78.3% | 77.2% |
| citm_catalog | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 29185 | 29401 | 24910 | 16149 | 14498 | sonic-rs | 24910 | 117.2% | 118.0% |
| canada | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 16975 | 16675 | 12658 | 5546 | 5750 | sonic-rs | 12658 | 134.1% | 131.7% |
| apache_builds | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 17734 | 18012 | 16206 | 14783 | 11199 | sonic-rs | 16206 | 109.4% | 111.1% |
| github_events | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 25332 | 25794 | 22182 | 16580 | 12838 | sonic-rs | 22182 | 114.2% | 116.3% |
| update_center | C | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18204 | 17491 | 18019 | 9457 | 7254 | sonic-rs | 18019 | 101.0% | 97.1% |
| mesh | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13308 | 13351 | 11837 | 7209 | 7059 | sonic-rs | 11837 | 112.4% | 112.8% |
| random | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7770 | 7677 | 15370 | 9142 | 5631 | sonic-rs | 15370 | 50.6% | 49.9% |
| gsoc-2018 | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 47481 | 45829 | 43207 | 21038 | 18671 | sonic-rs | 43207 | 109.9% | 106.1% |
| marine_ik | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13240 | 12551 | 10064 | 7020 | 6843 | sonic-rs | 10064 | 131.6% | 124.7% |
| instruments | C | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 19946 | 19989 | 19737 | 12643 | 10731 | sonic-rs | 19737 | 101.1% | 101.3% |
| numbers | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 19195 | 19050 | 13567 | 8977 | 9133 | sonic-rs | 13567 | 141.5% | 140.4% |
| unicode_mixed | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7384 | 7300 | 15892 | 7685 | 6505 | sonic-rs | 15892 | 46.5% | 45.9% |
| unicode_escapes | C | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13945 | 15580 | 16048 | 4128 | 4292 | sonic-rs | 16048 | 86.9% | 97.1% |
| unicode_basic | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6561 | 6889 | 13304 | 7257 | 5729 | sonic-rs | 13304 | 49.3% | 51.8% |
| distinct_values | C | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 16241 | 16634 | 16259 | 12099 | 9196 | sonic-rs | 16259 | 99.9% | 102.3% |
| y_string_unicode | C | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13109 | 13871 | 13673 | 6319 | 5277 | sonic-rs | 13673 | 95.9% | 101.5% |

## Workloads

| Corpus | Workload | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9849 | 9814 | 11604 | 8506 | 84.9% | 84.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9849, Track 2 9814, sonic 11604 Mbps |
| citm_catalog | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 25291 | 25345 | 21615 | 14067 | 117.0% | 117.3% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| canada | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5105 | 5099 | 12512 | 8012 | 40.8% | 40.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5105, Track 2 5099, sonic 12512 Mbps |
| apache_builds | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11083 | 11141 | 10051 | 7617 | 110.3% | 110.8% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| github_events | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10595 | 10625 | 10825 | 8427 | 97.9% | 98.1% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| update_center | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9140 | 9187 | 9179 | 6718 | 99.6% | 100.1% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| mesh | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4987 | 5003 | 9606 | 7909 | 51.9% | 52.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4987, Track 2 5003, sonic 9606 Mbps |
| random | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6676 | 6667 | 9157 | 6212 | 72.9% | 72.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6676, Track 2 6667, sonic 9157 Mbps |
| gsoc-2018 | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5724 | 5744 | 8516 | 7757 | 67.2% | 67.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5724, Track 2 5744, sonic 8516 Mbps |
| marine_ik | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6458 | 6429 | 8799 | 7606 | 73.4% | 73.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6458, Track 2 6429, sonic 8799 Mbps |
| instruments | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 15877 | 16054 | 12974 | 9809 | 122.4% | 123.7% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| numbers | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4301 | 4317 | 12974 | 8759 | 33.1% | 33.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4301, Track 2 4317, sonic 12974 Mbps |
| unicode_mixed | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 3197 | 3199 | 6406 | 3747 | 49.9% | 49.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 3197, Track 2 3199, sonic 6406 Mbps |
| unicode_escapes | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4574 | 4576 | 9072 | 4336 | 50.4% | 50.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4574, Track 2 4576, sonic 9072 Mbps |
| unicode_basic | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4856 | 4859 | 7092 | 4947 | 68.5% | 68.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4856, Track 2 4859, sonic 7092 Mbps |
| distinct_values | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12370 | 12461 | 11677 | 7552 | 105.9% | 106.7% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| y_string_unicode | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5620 | 5671 | 8547 | 7172 | 65.8% | 66.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5620, Track 2 5671, sonic 8547 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4445 | 1136560.36 | 27.3% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6685 | 755784.28 | 41.0% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 13680 | 369298.68 | 84.0% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7669 | 1801700.42 | 26.3% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 7428 | 1860236.09 | 25.5% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 26944 | 512831.82 | 92.3% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.76 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 3979 | 4526041.97 | 23.4% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4422 | 4072752.66 | 26.0% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 14460 | 1245359.64 | 85.2% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 6563 | 155142.72 | 37.0% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6136 | 165935.23 | 34.6% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 18086 | 56298.03 | 102.0% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.72 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 7113 | 73252.66 | 28.1% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 7736 | 67350.67 | 30.5% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 24288 | 21452.97 | 95.9% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 4370 | 976010.30 | 24.0% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4491 | 949874.99 | 24.7% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 18434 | 231389.55 | 101.3% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5276 | 1097192.32 | 39.6% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 4403 | 1314636.96 | 33.1% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 12916 | 448171.68 | 97.1% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2821 | 1447900.56 | 36.3% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3670 | 1112864.84 | 47.2% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 6945 | 587988.99 | 89.4% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8515 | 3126694.04 | 17.9% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18545 | 1435550.72 | 39.1% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 49308 | 539930.19 | 103.8% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2539 | 9401881.73 | 19.2% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 3993 | 5977914.31 | 30.2% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 12764 | 1869949.79 | 96.4% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 6389 | 275920.28 | 32.0% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5233 | 336834.64 | 26.2% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 19210 | 91761.90 | 96.3% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 8975 | 133810.28 | 46.8% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 5924 | 202724.58 | 30.9% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 17757 | 67636.06 | 92.5% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 2062 | 4085686.57 | 27.9% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 4026 | 2092781.75 | 54.5% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 4933 | 1707859.15 | 66.8% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.72 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 3736 | 2250173.07 | 26.8% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 4587 | 1832478.95 | 32.9% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 14574 | 576823.49 | 104.5% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2616 | 3206930.18 | 39.9% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4253 | 1972482.28 | 64.8% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 5641 | 1487007.06 | 86.0% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.59 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 5162 | 238086.91 | 31.8% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4340 | 283203.57 | 26.7% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 16028 | 76682.70 | 98.7% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 2597 | 109655.40 | 19.8% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 6074 | 46888.54 | 46.3% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 11543 | 24673.76 | 88.1% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 9849 Mbps, Track 2 9814 Mbps, sonic-rs 11604 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3522560 bytes, S anchor sonic-rs=4816896 bytes.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4751360 bytes, S anchor sonic-rs=7667712 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 5105 Mbps, Track 2 5099 Mbps, sonic-rs 12512 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5799936 bytes, S anchor sonic-rs=11272192 bytes.
- canada structural scan: 69075 Mbps; floor is 40000 Mbps.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2719744 bytes, S anchor sonic-rs=2998272 bytes.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2703360 bytes, S anchor sonic-rs=2981888 bytes.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3342336 bytes, S anchor sonic-rs=4423680 bytes.
- mesh direct-to-struct gate: NO-GO. Track 1 4987 Mbps, Track 2 5003 Mbps, sonic-rs 9606 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3620864 bytes, S anchor sonic-rs=6078464 bytes.
- random direct-to-struct gate: NO-GO. Track 1 6676 Mbps, Track 2 6667 Mbps, sonic-rs 9157 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3342336 bytes, S anchor sonic-rs=4653056 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 5724 Mbps, Track 2 5744 Mbps, sonic-rs 8516 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6176768 bytes, S anchor sonic-rs=10108928 bytes.
- marine_ik direct-to-struct gate: NO-GO. Track 1 6458 Mbps, Track 2 6429 Mbps, sonic-rs 8799 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7192576 bytes, S anchor sonic-rs=16138240 bytes.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2916352 bytes, S anchor sonic-rs=3506176 bytes.
- numbers direct-to-struct gate: NO-GO. Track 1 4301 Mbps, Track 2 4317 Mbps, sonic-rs 12974 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2670592 bytes, S anchor sonic-rs=3145728 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 3197 Mbps, Track 2 3199 Mbps, sonic-rs 6406 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3866624 bytes, S anchor sonic-rs=5570560 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 4574 Mbps, Track 2 4576 Mbps, sonic-rs 9072 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3768320 bytes, S anchor sonic-rs=5079040 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 4856 Mbps, Track 2 4859 Mbps, sonic-rs 7092 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=3997696 bytes, S anchor sonic-rs=6389760 bytes.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2736128 bytes, S anchor sonic-rs=3129344 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 5620 Mbps, Track 2 5671 Mbps, sonic-rs 8547 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2621440 bytes, S anchor sonic-rs=2670592 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

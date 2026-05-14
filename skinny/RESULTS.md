# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12455 | 12514 | 21361 | 14731 | 12313 | sonic-rs | 21361 | 58.3% | 58.6% |
| citm_catalog | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21363 | 21483 | 25141 | 16718 | 15024 | sonic-rs | 25141 | 85.0% | 85.4% |
| canada | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 17244 | 17304 | 14079 | 6272 | 6296 | sonic-rs | 14079 | 122.5% | 122.9% |
| apache_builds | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11869 | 11862 | 17333 | 15780 | 11838 | sonic-rs | 17333 | 68.5% | 68.4% |
| github_events | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12230 | 12279 | 23090 | 18113 | 14608 | sonic-rs | 23090 | 53.0% | 53.2% |
| update_center | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9580 | 9542 | 19757 | 12120 | 8860 | sonic-rs | 19757 | 48.5% | 48.3% |
| mesh | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13855 | 13839 | 11854 | 7390 | 7336 | sonic-rs | 11854 | 116.9% | 116.8% |
| random | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7858 | 7826 | 15447 | 9776 | 7465 | sonic-rs | 15447 | 50.9% | 50.7% |
| gsoc-2018 | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21982 | 22010 | 49474 | 23698 | 19319 | sonic-rs | 49474 | 44.4% | 44.5% |
| marine_ik | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12849 | 13027 | 9987 | 7037 | 6774 | sonic-rs | 9987 | 128.7% | 130.4% |
| instruments | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11646 | 11701 | 19586 | 12557 | 10756 | sonic-rs | 19586 | 59.5% | 59.7% |
| numbers | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18119 | 18176 | 13453 | 8831 | 8872 | sonic-rs | 13453 | 134.7% | 135.1% |
| unicode_mixed | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 8878 | 8736 | 18196 | 8642 | 7715 | sonic-rs | 18196 | 48.8% | 48.0% |
| unicode_escapes | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12604 | 12744 | 19035 | 4703 | 4611 | sonic-rs | 19035 | 66.2% | 66.9% |
| unicode_basic | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 10946 | 11007 | 15857 | 9631 | 7203 | sonic-rs | 15857 | 69.0% | 69.4% |
| distinct_values | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6077 | 6062 | 17765 | 12118 | 9091 | sonic-rs | 17765 | 34.2% | 34.1% |
| y_string_unicode | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 5741 | 5705 | 13687 | 6423 | 5639 | sonic-rs | 13687 | 41.9% | 41.7% |

## Workloads

| Corpus | Workload | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7985 | 8154 | 11896 | 8643 | 67.1% | 68.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7985, Track 2 8154, sonic 11896 Mbps |
| citm_catalog | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 13532 | 14102 | 21601 | 14162 | 62.6% | 65.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 13532, Track 2 14102, sonic 21601 Mbps |
| canada | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7189 | 7786 | 12172 | 8031 | 59.1% | 64.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7189, Track 2 7786, sonic 12172 Mbps |
| apache_builds | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6077 | 6152 | 10148 | 7621 | 59.9% | 60.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6077, Track 2 6152, sonic 10148 Mbps |
| github_events | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7194 | 7296 | 10959 | 8545 | 65.6% | 66.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7194, Track 2 7296, sonic 10959 Mbps |
| update_center | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5281 | 5286 | 9259 | 6659 | 57.0% | 57.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5281, Track 2 5286, sonic 9259 Mbps |
| mesh | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6303 | 6672 | 9729 | 7902 | 64.8% | 68.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6303, Track 2 6672, sonic 9729 Mbps |
| random | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4511 | 4553 | 9140 | 6197 | 49.4% | 49.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4511, Track 2 4553, sonic 9140 Mbps |
| gsoc-2018 | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6640 | 6668 | 8492 | 7751 | 78.2% | 78.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6640, Track 2 6668, sonic 8492 Mbps |
| marine_ik | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6806 | 7187 | 8769 | 7526 | 77.6% | 82.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6806, Track 2 7187, sonic 8769 Mbps |
| instruments | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8577 | 8656 | 13185 | 9905 | 65.0% | 65.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8577, Track 2 8656, sonic 13185 Mbps |
| numbers | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7966 | 8360 | 12297 | 8712 | 64.8% | 68.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7966, Track 2 8360, sonic 12297 Mbps |
| unicode_mixed | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 3238 | 3238 | 6372 | 3894 | 50.8% | 50.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 3238, Track 2 3238, sonic 6372 Mbps |
| unicode_escapes | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4226 | 4199 | 9063 | 4385 | 46.6% | 46.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4226, Track 2 4199, sonic 9063 Mbps |
| unicode_basic | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4609 | 4537 | 7094 | 4915 | 65.0% | 64.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4609, Track 2 4537, sonic 7094 Mbps |
| distinct_values | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5433 | 5377 | 11904 | 7624 | 45.6% | 45.2% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5433, Track 2 5377, sonic 11904 Mbps |
| y_string_unicode | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4391 | 4326 | 8242 | 7001 | 53.3% | 52.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4391, Track 2 4326, sonic 8242 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4202 | 1202325.01 | 33.7% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6977 | 724063.65 | 56.0% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 8065 | 626452.52 | 64.7% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 6679 | 2068875.62 | 31.3% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 8017 | 1723537.88 | 37.5% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 13488 | 1024473.40 | 63.1% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 3981 | 4524134.29 | 23.1% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4962 | 3629263.45 | 28.8% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 16342 | 1101967.60 | 94.8% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.60 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 4604 | 221161.53 | 38.8% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6696 | 152068.83 | 56.4% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 7077 | 143867.67 | 59.6% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.56 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5262 | 99014.98 | 43.0% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 8238 | 63248.80 | 67.4% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 8576 | 60756.59 | 70.1% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3083 | 1383630.00 | 32.2% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4623 | 922706.30 | 48.3% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 5528 | 771570.10 | 57.7% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 4991 | 1159912.25 | 36.0% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 4870 | 1188740.65 | 35.1% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 13132 | 440817.72 | 94.8% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2679 | 1524530.04 | 34.1% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3912 | 1043998.68 | 49.8% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 4451 | 917433.65 | 56.7% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 7979 | 3336571.78 | 36.3% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18421 | 1445198.52 | 83.8% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 16209 | 1642441.75 | 73.7% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2299 | 10380968.33 | 17.9% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 3977 | 6001943.20 | 30.9% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 10866 | 2196621.08 | 84.6% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.60 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 4767 | 369754.34 | 40.9% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5250 | 335778.73 | 45.1% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 8004 | 220234.16 | 68.7% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.58 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 8738 | 137438.16 | 48.2% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6067 | 197951.51 | 33.5% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 17441 | 68861.45 | 96.3% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 1865 | 4517881.21 | 21.0% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 5039 | 1672029.57 | 56.8% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 5237 | 1608540.41 | 59.0% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2216 | 3794167.89 | 17.6% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5443 | 1544454.17 | 43.2% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 11328 | 742089.79 | 89.9% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2793 | 3003436.42 | 25.5% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4279 | 1960317.61 | 39.1% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 4881 | 1718468.01 | 44.6% | reported cold-sensitive |
| distinct_values | host_call_dispatch_overhead | n/a | 0.61 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3142 | 391154.91 | 51.7% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4344 | 282943.03 | 71.5% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 4003 | 307040.30 | 65.9% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.56 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 1874 | 151977.98 | 32.6% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 6053 | 47049.63 | 105.4% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 3751 | 75924.51 | 65.3% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 7985 Mbps, Track 2 8154 Mbps, sonic-rs 11896 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 422715 allocated tape bytes (0.67x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3473408 bytes, S anchor sonic-rs=4915200 bytes.
- citm_catalog direct-to-struct gate: NO-GO. Track 1 13532 Mbps, Track 2 14102 Mbps, sonic-rs 21601 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 1151524 allocated tape bytes (0.67x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4718592 bytes, S anchor sonic-rs=7749632 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 7189 Mbps, Track 2 7786 Mbps, sonic-rs 12172 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1500732 allocated tape bytes (0.67x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5750784 bytes, S anchor sonic-rs=11354112 bytes.
- canada structural scan: 70282 Mbps; floor is 40000 Mbps.
- apache_builds direct-to-struct gate: NO-GO. Track 1 6077 Mbps, Track 2 6152 Mbps, sonic-rs 10148 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 85420 allocated tape bytes (0.67x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2736128 bytes, S anchor sonic-rs=3096576 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 7194 Mbps, Track 2 7296 Mbps, sonic-rs 10959 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 43497 allocated tape bytes (0.67x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2719744 bytes, S anchor sonic-rs=3063808 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 5281 Mbps, Track 2 5286 Mbps, sonic-rs 9259 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 357404 allocated tape bytes (0.67x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3325952 bytes, S anchor sonic-rs=4505600 bytes.
- mesh direct-to-struct gate: NO-GO. Track 1 6303 Mbps, Track 2 6672 Mbps, sonic-rs 9729 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 482428 allocated tape bytes (0.67x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3571712 bytes, S anchor sonic-rs=6144000 bytes.
- random direct-to-struct gate: NO-GO. Track 1 4511 Mbps, Track 2 4553 Mbps, sonic-rs 9140 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 340348 allocated tape bytes (0.67x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3293184 bytes, S anchor sonic-rs=4734976 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 6640 Mbps, Track 2 6668 Mbps, sonic-rs 8492 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 345870 allocated tape bytes (0.10x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6111232 bytes, S anchor sonic-rs=10174464 bytes.
- marine_ik direct-to-struct gate: NO-GO. Track 1 6806 Mbps, Track 2 7187 Mbps, sonic-rs 8769 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 1989008 allocated tape bytes (0.67x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7127040 bytes, S anchor sonic-rs=16203776 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 8577 Mbps, Track 2 8656 Mbps, sonic-rs 13185 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 146928 allocated tape bytes (0.67x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2883584 bytes, S anchor sonic-rs=3588096 bytes.
- numbers direct-to-struct gate: NO-GO. Track 1 7966 Mbps, Track 2 8360 Mbps, sonic-rs 12297 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 100112 allocated tape bytes (0.67x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2637824 bytes, S anchor sonic-rs=3211264 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 3238 Mbps, Track 2 3238 Mbps, sonic-rs 6372 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 1579698 allocated tape bytes (1.50x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3833856 bytes, S anchor sonic-rs=5652480 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 4226 Mbps, Track 2 4199 Mbps, sonic-rs 9063 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 207864 allocated tape bytes (0.20x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3751936 bytes, S anchor sonic-rs=5144576 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 4609 Mbps, Track 2 4537 Mbps, sonic-rs 7094 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 699088 allocated tape bytes (0.67x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=3932160 bytes, S anchor sonic-rs=6471680 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 5433 Mbps, Track 2 5377 Mbps, sonic-rs 11904 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 102452 allocated tape bytes (0.67x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2686976 bytes, S anchor sonic-rs=3211264 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 4391 Mbps, Track 2 4326 Mbps, sonic-rs 8242 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 35404 allocated tape bytes (0.99x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2670592 bytes, S anchor sonic-rs=2719744 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

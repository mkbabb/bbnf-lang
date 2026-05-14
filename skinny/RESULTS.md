# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6048 | 6065 | 21028 | 14476 | 12048 | sonic-rs | 21028 | 28.8% | 28.8% |
| citm_catalog | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 22323 | 22450 | 25354 | 16429 | 14756 | sonic-rs | 25354 | 88.0% | 88.5% |
| canada | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 17829 | 17355 | 13981 | 6379 | 6234 | sonic-rs | 13981 | 127.5% | 124.1% |
| apache_builds | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12784 | 12848 | 17383 | 15679 | 11710 | sonic-rs | 17383 | 73.5% | 73.9% |
| github_events | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13755 | 13763 | 23066 | 17920 | 14434 | sonic-rs | 23066 | 59.6% | 59.7% |
| update_center | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 10333 | 10265 | 19754 | 12019 | 8786 | sonic-rs | 19754 | 52.3% | 52.0% |
| mesh | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13155 | 12989 | 11832 | 7321 | 7335 | sonic-rs | 11832 | 111.2% | 109.8% |
| random | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 3253 | 3258 | 15521 | 9675 | 7383 | sonic-rs | 15521 | 21.0% | 21.0% |
| gsoc-2018 | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21252 | 21260 | 49752 | 24233 | 19557 | sonic-rs | 49752 | 42.7% | 42.7% |
| marine_ik | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12983 | 12856 | 10023 | 7076 | 6837 | sonic-rs | 10023 | 129.5% | 128.3% |
| instruments | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13025 | 13174 | 19820 | 12553 | 10634 | sonic-rs | 19820 | 65.7% | 66.5% |
| numbers | A | GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18811 | 18984 | 13728 | 9024 | 9145 | sonic-rs | 13728 | 137.0% | 138.3% |
| unicode_mixed | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 2377 | 2407 | 18281 | 8652 | 7687 | sonic-rs | 18281 | 13.0% | 13.2% |
| unicode_escapes | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13070 | 13265 | 18913 | 4780 | 4707 | sonic-rs | 18913 | 69.1% | 70.1% |
| unicode_basic | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 2559 | 2559 | 15946 | 9661 | 7182 | sonic-rs | 15946 | 16.0% | 16.0% |
| distinct_values | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7379 | 7386 | 18052 | 12129 | 9118 | sonic-rs | 18052 | 40.9% | 40.9% |
| y_string_unicode | G | NO-GO | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7183 | 7218 | 13718 | 6428 | 5647 | sonic-rs | 13718 | 52.4% | 52.6% |

## Workloads

| Corpus | Workload | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4765 | 4758 | 11630 | 8426 | 41.0% | 40.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4765, Track 2 4758, sonic 11630 Mbps |
| citm_catalog | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 14889 | 15236 | 21475 | 14073 | 69.3% | 70.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 14889, Track 2 15236, sonic 21475 Mbps |
| canada | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7249 | 7656 | 12182 | 7946 | 59.5% | 62.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7249, Track 2 7656, sonic 12182 Mbps |
| apache_builds | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6831 | 6864 | 10113 | 7616 | 67.6% | 67.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6831, Track 2 6864, sonic 10113 Mbps |
| github_events | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7197 | 7261 | 10821 | 8426 | 66.5% | 67.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7197, Track 2 7261, sonic 10821 Mbps |
| update_center | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5750 | 5734 | 9207 | 6741 | 62.4% | 62.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5750, Track 2 5734, sonic 9207 Mbps |
| mesh | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6308 | 6501 | 9568 | 7739 | 65.9% | 67.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6308, Track 2 6501, sonic 9568 Mbps |
| random | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 2630 | 2632 | 9115 | 6199 | 28.9% | 28.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 2630, Track 2 2632, sonic 9115 Mbps |
| gsoc-2018 | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4976 | 4746 | 8474 | 7723 | 58.7% | 56.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4976, Track 2 4746, sonic 8474 Mbps |
| marine_ik | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7276 | 7621 | 8853 | 7664 | 82.2% | 86.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7276, Track 2 7621, sonic 8853 Mbps |
| instruments | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9945 | 10087 | 13195 | 9898 | 75.4% | 76.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9945, Track 2 10087, sonic 13195 Mbps |
| numbers | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8073 | 8601 | 12918 | 8754 | 62.5% | 66.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8073, Track 2 8601, sonic 12918 Mbps |
| unicode_mixed | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 1648 | 1654 | 6421 | 4043 | 25.7% | 25.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 1648, Track 2 1654, sonic 6421 Mbps |
| unicode_escapes | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4274 | 4274 | 9134 | 4392 | 46.8% | 46.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4274, Track 2 4274, sonic 9134 Mbps |
| unicode_basic | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 1991 | 1980 | 7184 | 4997 | 27.7% | 27.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 1991, Track 2 1980, sonic 7184 Mbps |
| distinct_values | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6456 | 6399 | 11712 | 7615 | 55.1% | 54.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6456, Track 2 6399, sonic 11712 Mbps |
| y_string_unicode | direct_to_struct | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 3956 | 3962 | 8537 | 7203 | 46.3% | 46.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 3956, Track 2 3962, sonic 8537 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 2489 | 2029389.37 | 41.2% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6857 | 736777.84 | 113.4% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 5683 | 889048.74 | 94.0% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7261 | 1903114.72 | 32.5% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 8017 | 1723468.66 | 35.9% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 21476 | 643389.14 | 96.2% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 4455 | 4042017.93 | 25.0% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4928 | 3653935.32 | 27.6% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 16961 | 1061723.56 | 95.1% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 5047 | 201752.79 | 39.5% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6674 | 152566.83 | 52.2% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 12465 | 81682.72 | 97.5% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5600 | 93050.32 | 40.7% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 8158 | 63871.29 | 59.3% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 13314 | 39135.97 | 96.8% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3344 | 1275619.20 | 32.4% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4614 | 924491.35 | 44.6% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 10137 | 420792.34 | 98.1% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5368 | 1078380.21 | 40.8% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 4948 | 1170004.26 | 37.6% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 12845 | 450654.09 | 97.6% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 1416 | 2884355.42 | 43.5% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3616 | 1129340.61 | 111.2% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 3113 | 1312044.23 | 95.7% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 5964 | 4463591.84 | 28.1% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18448 | 1443117.30 | 86.8% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 20379 | 1306396.00 | 95.9% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2526 | 9448679.16 | 19.5% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 4035 | 5914931.66 | 31.1% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 12626 | 1890391.43 | 97.3% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 5305 | 332293.19 | 40.7% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5225 | 337403.67 | 40.1% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 12796 | 137759.39 | 98.2% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 9549 | 125773.49 | 50.8% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6292 | 190879.00 | 33.4% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 18241 | 65839.24 | 97.0% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 960 | 8776259.38 | 40.4% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 5154 | 1634471.85 | 216.8% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 2180 | 3865238.08 | 91.7% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 3405 | 2469091.45 | 26.0% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5494 | 1530032.05 | 42.0% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 12747 | 659457.90 | 97.5% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 1119 | 7493542.04 | 43.8% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4326 | 1938955.88 | 169.1% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 2343 | 3579847.89 | 91.6% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3634 | 338161.81 | 49.3% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4362 | 281768.73 | 59.1% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 7276 | 168908.57 | 98.6% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 2112 | 134826.81 | 29.4% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 6065 | 46956.42 | 84.4% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 6651 | 42824.80 | 92.6% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 4765 Mbps, Track 2 4758 Mbps, sonic-rs 11630 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3637248 bytes, S anchor sonic-rs=4931584 bytes.
- citm_catalog direct-to-struct gate: NO-GO. Track 1 14889 Mbps, Track 2 15236 Mbps, sonic-rs 21475 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4882432 bytes, S anchor sonic-rs=7733248 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 7249 Mbps, Track 2 7656 Mbps, sonic-rs 12182 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5947392 bytes, S anchor sonic-rs=11354112 bytes.
- canada structural scan: 69976 Mbps; floor is 40000 Mbps.
- apache_builds direct-to-struct gate: NO-GO. Track 1 6831 Mbps, Track 2 6864 Mbps, sonic-rs 10113 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 33308 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2834432 bytes, S anchor sonic-rs=3096576 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 7197 Mbps, Track 2 7261 Mbps, sonic-rs 10821 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16429 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2801664 bytes, S anchor sonic-rs=3080192 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 5750 Mbps, Track 2 5734 Mbps, sonic-rs 9207 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 264064 allocated tape bytes (0.50x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3440640 bytes, S anchor sonic-rs=4505600 bytes.
- mesh direct-to-struct gate: NO-GO. Track 1 6308 Mbps, Track 2 6501 Mbps, sonic-rs 9568 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3751936 bytes, S anchor sonic-rs=6160384 bytes.
- random direct-to-struct gate: NO-GO. Track 1 2630 Mbps, Track 2 2632 Mbps, sonic-rs 9115 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3440640 bytes, S anchor sonic-rs=4751360 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 4976 Mbps, Track 2 4746 Mbps, sonic-rs 8474 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6275072 bytes, S anchor sonic-rs=10174464 bytes.
- marine_ik direct-to-struct gate: NO-GO. Track 1 7276 Mbps, Track 2 7621 Mbps, sonic-rs 8853 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7340032 bytes, S anchor sonic-rs=16220160 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 9945 Mbps, Track 2 10087 Mbps, sonic-rs 13195 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=3047424 bytes, S anchor sonic-rs=3588096 bytes.
- numbers direct-to-struct gate: NO-GO. Track 1 8073 Mbps, Track 2 8601 Mbps, sonic-rs 12918 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2785280 bytes, S anchor sonic-rs=3227648 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 1648 Mbps, Track 2 1654 Mbps, sonic-rs 6421 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3981312 bytes, S anchor sonic-rs=5652480 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 4274 Mbps, Track 2 4274 Mbps, sonic-rs 9134 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3883008 bytes, S anchor sonic-rs=5144576 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 1991 Mbps, Track 2 1980 Mbps, sonic-rs 7184 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=4096000 bytes, S anchor sonic-rs=6471680 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 6456 Mbps, Track 2 6399 Mbps, sonic-rs 11712 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2834432 bytes, S anchor sonic-rs=3211264 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 3956 Mbps, Track 2 3962 Mbps, sonic-rs 8537 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2686976 bytes, S anchor sonic-rs=2736128 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

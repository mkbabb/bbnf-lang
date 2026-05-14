# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12318 | 12183 | 21148 | 14655 | 12187 | sonic-rs | 21148 | 58.2% | 57.6% |
| citm_catalog | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21811 | 20714 | 25380 | 16441 | 14721 | sonic-rs | 25380 | 85.9% | 81.6% |
| canada | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18036 | 16605 | 13529 | 6292 | 6277 | sonic-rs | 13529 | 133.3% | 122.7% |
| apache_builds | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12511 | 12206 | 17374 | 15128 | 11403 | sonic-rs | 17374 | 72.0% | 70.3% |
| github_events | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13184 | 13002 | 22895 | 18072 | 14443 | sonic-rs | 22895 | 57.6% | 56.8% |
| update_center | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9259 | 9103 | 19242 | 11835 | 8597 | sonic-rs | 19242 | 48.1% | 47.3% |
| mesh | C | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13129 | 11881 | 11696 | 6494 | 6129 | sonic-rs | 11696 | 112.3% | 101.6% |
| random | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7639 | 7532 | 15398 | 9725 | 7349 | sonic-rs | 15398 | 49.6% | 48.9% |
| gsoc-2018 | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21928 | 21838 | 49366 | 22021 | 17833 | sonic-rs | 49366 | 44.4% | 44.2% |
| marine_ik | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13265 | 12233 | 10041 | 6988 | 6663 | sonic-rs | 10041 | 132.1% | 121.8% |
| instruments | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12532 | 11994 | 19789 | 12292 | 10491 | sonic-rs | 19789 | 63.3% | 60.6% |
| numbers | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 19853 | 18370 | 13526 | 8929 | 8864 | sonic-rs | 13526 | 146.8% | 135.8% |
| unicode_mixed | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 8107 | 8692 | 11981 | 8600 | 7591 | sonic-rs | 11981 | 67.7% | 72.5% |
| unicode_escapes | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9908 | 12575 | 18692 | 4690 | 4577 | sonic-rs | 18692 | 53.0% | 67.3% |
| unicode_basic | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11092 | 10859 | 15802 | 9522 | 7139 | sonic-rs | 15802 | 70.2% | 68.7% |
| distinct_values | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6144 | 6099 | 17728 | 11642 | 8814 | sonic-rs | 17728 | 34.7% | 34.4% |
| y_string_unicode | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6272 | 5901 | 13644 | 6422 | 5203 | sonic-rs | 13644 | 46.0% | 43.2% |

## Workloads

| Corpus | Workload | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11873 | 11015 | 15648 | 11194 | 75.9% | 70.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11873, Track 2 11015, sonic 15648 Mbps |
| citm_catalog | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 21388 | 20446 | 21428 | 14475 | 99.8% | 95.4% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| canada | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10563 | 10453 | 12508 | 7940 | 84.4% | 83.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10563, Track 2 10453, sonic 12508 Mbps |
| apache_builds | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11330 | 10335 | 11675 | 10478 | 97.0% | 88.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11330, Track 2 10335, sonic 11675 Mbps |
| github_events | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12275 | 11259 | 17062 | 13663 | 71.9% | 66.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 12275, Track 2 11259, sonic 17062 Mbps |
| update_center | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8308 | 7579 | 12581 | 8765 | 66.0% | 60.2% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8308, Track 2 7579, sonic 12581 Mbps |
| mesh | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8273 | 8442 | 9537 | 7755 | 86.8% | 88.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8273, Track 2 8442, sonic 9537 Mbps |
| random | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7785 | 7086 | 10190 | 7078 | 76.4% | 69.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7785, Track 2 7086, sonic 10190 Mbps |
| gsoc-2018 | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 15013 | 14458 | 24163 | 19856 | 62.1% | 59.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 15013, Track 2 14458, sonic 24163 Mbps |
| marine_ik | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9065 | 9280 | 8839 | 7618 | 102.6% | 105.0% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| instruments | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12071 | 11134 | 13459 | 10580 | 89.7% | 82.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 12071, Track 2 11134, sonic 13459 Mbps |
| numbers | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12616 | 12012 | 12474 | 8708 | 101.1% | 96.3% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| unicode_mixed | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 3881 | 4137 | 10142 | 5215 | 38.3% | 40.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 3881, Track 2 4137, sonic 10142 Mbps |
| unicode_escapes | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5143 | 5030 | 14485 | 5273 | 35.5% | 34.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5143, Track 2 5030, sonic 14485 Mbps |
| unicode_basic | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9095 | 8316 | 9803 | 6076 | 92.8% | 84.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9095, Track 2 8316, sonic 9803 Mbps |
| distinct_values | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6072 | 5563 | 13185 | 8837 | 46.1% | 42.2% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6072, Track 2 5563, sonic 13185 Mbps |
| y_string_unicode | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 3674 | 3679 | 8676 | 5886 | 42.3% | 42.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 3674, Track 2 3679, sonic 8676 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4199 | 1203307.97 | 34.1% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6860 | 736503.30 | 55.7% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 10672 | 473398.28 | 86.6% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7125 | 1939318.55 | 32.7% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 7939 | 1740413.41 | 36.4% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 21049 | 656445.92 | 96.5% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 4523 | 3981538.83 | 25.1% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4990 | 3609055.76 | 27.7% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 18319 | 983028.58 | 101.6% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.59 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 4819 | 211287.59 | 38.5% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6601 | 154248.36 | 52.8% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 12149 | 83810.92 | 97.1% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.64 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5456 | 95496.82 | 41.4% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 8064 | 64611.43 | 61.2% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 12700 | 41028.58 | 96.3% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 2567 | 1661858.13 | 27.7% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 3515 | 1213581.35 | 38.0% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 7595 | 561639.94 | 82.0% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5422 | 1067697.45 | 41.3% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 4908 | 1179433.77 | 37.4% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 13429 | 431060.18 | 102.3% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2809 | 1453709.34 | 36.8% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3866 | 1056396.84 | 50.6% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 7209 | 566486.02 | 94.4% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.72 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8045 | 3309363.80 | 36.7% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18096 | 1471202.32 | 82.5% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 21004 | 1267509.49 | 95.8% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2533 | 9423538.88 | 19.1% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 4018 | 5940273.05 | 30.3% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 13107 | 1821043.80 | 98.8% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.60 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 5088 | 346433.72 | 40.6% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5169 | 341014.35 | 41.2% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 12170 | 144842.21 | 97.1% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.63 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 9788 | 122697.14 | 49.3% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6194 | 193890.53 | 31.2% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 19078 | 62950.45 | 96.1% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 1828 | 4609327.42 | 22.5% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 5048 | 1668956.44 | 62.3% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 5829 | 1445190.06 | 71.9% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2192 | 3834367.37 | 22.1% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5325 | 1578552.30 | 53.7% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 11719 | 717352.59 | 118.3% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2834 | 2960474.56 | 25.5% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4298 | 1951763.89 | 38.7% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 7658 | 1095385.45 | 69.0% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.60 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3194 | 384771.44 | 52.0% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 3736 | 328992.03 | 60.8% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 6039 | 203532.10 | 98.3% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 1944 | 146478.07 | 31.0% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 5772 | 49343.32 | 92.0% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 5842 | 48751.60 | 93.1% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 11873 Mbps, Track 2 11015 Mbps, sonic-rs 15648 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3489792 bytes, S anchor sonic-rs=4866048 bytes.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4734976 bytes, S anchor sonic-rs=7700480 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 10563 Mbps, Track 2 10453 Mbps, sonic-rs 12508 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5783552 bytes, S anchor sonic-rs=11304960 bytes.
- canada structural scan: 41551 Mbps; floor is 40000 Mbps.
- apache_builds direct-to-struct gate: NO-GO. Track 1 11330 Mbps, Track 2 10335 Mbps, sonic-rs 11675 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2736128 bytes, S anchor sonic-rs=3063808 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 12275 Mbps, Track 2 11259 Mbps, sonic-rs 17062 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2703360 bytes, S anchor sonic-rs=3014656 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 8308 Mbps, Track 2 7579 Mbps, sonic-rs 12581 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3342336 bytes, S anchor sonic-rs=4456448 bytes.
- mesh direct-to-struct gate: NO-GO. Track 1 8273 Mbps, Track 2 8442 Mbps, sonic-rs 9537 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3604480 bytes, S anchor sonic-rs=6111232 bytes.
- random direct-to-struct gate: NO-GO. Track 1 7785 Mbps, Track 2 7086 Mbps, sonic-rs 10190 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3325952 bytes, S anchor sonic-rs=4685824 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 15013 Mbps, Track 2 14458 Mbps, sonic-rs 24163 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6160384 bytes, S anchor sonic-rs=10125312 bytes.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7176192 bytes, S anchor sonic-rs=16154624 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 12071 Mbps, Track 2 11134 Mbps, sonic-rs 13459 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2916352 bytes, S anchor sonic-rs=3555328 bytes.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2654208 bytes, S anchor sonic-rs=3178496 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 3881 Mbps, Track 2 4137 Mbps, sonic-rs 10142 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3899392 bytes, S anchor sonic-rs=5619712 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 5143 Mbps, Track 2 5030 Mbps, sonic-rs 14485 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3784704 bytes, S anchor sonic-rs=5128192 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 9095 Mbps, Track 2 8316 Mbps, sonic-rs 9803 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=3981312 bytes, S anchor sonic-rs=6438912 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 6072 Mbps, Track 2 5563 Mbps, sonic-rs 13185 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2752512 bytes, S anchor sonic-rs=3178496 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 3674 Mbps, Track 2 3679 Mbps, sonic-rs 8676 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2637824 bytes, S anchor sonic-rs=2703360 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

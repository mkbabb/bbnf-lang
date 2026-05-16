# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 15462 | 11473 | 18972 | 14386 | 11990 | sonic-rs | 18972 | 81.5% | 60.5% |
| citm_catalog | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 31487 | 13999 | 24821 | 15526 | 14335 | sonic-rs | 24821 | 126.9% | 56.4% |
| canada | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18859 | 17123 | 13782 | 6182 | 6189 | sonic-rs | 13782 | 136.8% | 124.2% |
| apache_builds | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12732 | 12147 | 17324 | 15739 | 11774 | sonic-rs | 17324 | 73.5% | 70.1% |
| github_events | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 15358 | 13001 | 22881 | 17951 | 14423 | sonic-rs | 22881 | 67.1% | 56.8% |
| update_center | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11778 | 9166 | 19649 | 12099 | 8674 | sonic-rs | 19649 | 59.9% | 46.7% |
| mesh | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13659 | 11793 | 10107 | 6968 | 7369 | sonic-rs | 10107 | 135.1% | 116.7% |
| random | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9534 | 7012 | 14711 | 8031 | 5815 | sonic-rs | 14711 | 64.8% | 47.7% |
| gsoc-2018 | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 23220 | 21950 | 49213 | 23509 | 18902 | sonic-rs | 49213 | 47.2% | 44.6% |
| marine_ik | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13648 | 12649 | 9803 | 7037 | 6775 | sonic-rs | 9803 | 139.2% | 129.0% |
| instruments | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 17919 | 11829 | 19572 | 12749 | 10725 | sonic-rs | 19572 | 91.6% | 60.4% |
| numbers | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 20340 | 18741 | 13625 | 8990 | 8967 | sonic-rs | 13625 | 149.3% | 137.6% |
| unicode_mixed | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7978 | 8979 | 16722 | 8662 | 7709 | sonic-rs | 16722 | 47.7% | 53.7% |
| unicode_escapes | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11185 | 11987 | 18734 | 4688 | 4565 | sonic-rs | 18734 | 59.7% | 64.0% |
| unicode_basic | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12016 | 10634 | 15765 | 9476 | 7162 | sonic-rs | 15765 | 76.2% | 67.5% |
| distinct_values | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9001 | 6082 | 17737 | 11891 | 8852 | sonic-rs | 17737 | 50.7% | 34.3% |
| y_string_unicode | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6258 | 5879 | 13020 | 6088 | 5234 | sonic-rs | 13020 | 48.1% | 45.2% |

## Workloads

| Corpus | Workload | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11774 | 10888 | 15051 | 10280 | 78.2% | 72.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11774, Track 2 10888, sonic 15051 Mbps |
| twitter | real_typed_struct | deferred | generated typed DirectBuild owned struct vs structurally independent typed oracle vs sonic-rs typed serde | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | 18013 | 14033 | 15259 | 16174 | 118.1% | 92.0% | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 14033 Mbps |
| citm_catalog | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 21257 | 19871 | 19883 | 13073 | 106.9% | 99.9% | PASS correctness green; sonic shape parity; throughput within gate |
| canada | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10644 | 10508 | 12117 | 7287 | 87.8% | 86.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10644, Track 2 10508, sonic 12117 Mbps |
| apache_builds | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11326 | 10197 | 10940 | 9364 | 103.5% | 93.2% | PASS correctness green; sonic shape parity; throughput within gate |
| github_events | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11266 | 10649 | 15071 | 12201 | 74.8% | 70.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11266, Track 2 10649, sonic 15071 Mbps |
| update_center | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8445 | 7589 | 11045 | 8182 | 76.5% | 68.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8445, Track 2 7589, sonic 11045 Mbps |
| update_center | real_typed_struct | deferred | generated typed DirectBuild owned struct vs structurally independent typed oracle vs sonic-rs typed serde | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | 12265 | 10590 | 12744 | 10601 | 96.2% | 83.1% | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 10590 Mbps |
| mesh | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8706 | 8559 | 8595 | 6673 | 101.3% | 99.6% | PASS correctness green; sonic shape parity; throughput within gate |
| random | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7141 | 6913 | 7753 | 5949 | 92.1% | 89.2% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7141, Track 2 6913, sonic 7753 Mbps |
| gsoc-2018 | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 15054 | 14240 | 22223 | 18779 | 67.7% | 64.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 15054, Track 2 14240, sonic 22223 Mbps |
| marine_ik | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9306 | 9491 | 8450 | 7002 | 110.1% | 112.3% | PASS correctness green; sonic shape parity; throughput within gate |
| instruments | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12046 | 11087 | 12675 | 9403 | 95.0% | 87.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 12046, Track 2 11087, sonic 12675 Mbps |
| numbers | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12647 | 12344 | 12689 | 8015 | 99.7% | 97.3% | PASS correctness green; sonic shape parity; throughput within gate |
| unicode_mixed | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4750 | 4758 | 10445 | 5295 | 45.5% | 45.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4750, Track 2 4758, sonic 10445 Mbps |
| unicode_escapes | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5239 | 5094 | 13998 | 5204 | 37.4% | 36.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5239, Track 2 5094, sonic 13998 Mbps |
| unicode_basic | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8906 | 8245 | 8831 | 5769 | 100.8% | 93.4% | PASS correctness green; sonic shape parity; throughput within gate |
| distinct_values | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6085 | 5142 | 10621 | 7587 | 57.3% | 48.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6085, Track 2 5142, sonic 10621 Mbps |
| y_string_unicode | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4682 | 3495 | 8243 | 6808 | 56.8% | 42.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4682, Track 2 3495, sonic 8243 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4431 | 1140243.74 | 28.7% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6679 | 756461.70 | 43.2% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 13170 | 383599.96 | 85.2% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7908 | 1747258.72 | 25.1% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 7851 | 1759933.34 | 24.9% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 29894 | 462223.50 | 94.9% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 4485 | 4015620.62 | 23.8% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4857 | 3707886.07 | 25.8% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 17900 | 1006055.01 | 94.9% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 4651 | 218929.47 | 36.5% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6588 | 154558.05 | 51.7% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 12412 | 82034.89 | 97.5% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5785 | 90066.05 | 37.7% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 7849 | 66382.70 | 51.1% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 14905 | 34957.41 | 97.1% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3364 | 1268017.37 | 28.6% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4593 | 928677.12 | 39.0% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 11298 | 377553.85 | 95.9% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.77 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 4758 | 1216715.83 | 34.8% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 3959 | 1462024.29 | 29.0% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 12367 | 468070.65 | 90.5% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.73 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2947 | 1385610.32 | 30.9% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3903 | 1046228.37 | 40.9% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 8682 | 470394.21 | 91.1% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8080 | 3294732.81 | 34.8% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 17943 | 1483731.52 | 77.3% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 22006 | 1209797.64 | 94.8% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2539 | 9401570.62 | 18.6% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 3977 | 6001337.18 | 29.1% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 13174 | 1811695.04 | 96.5% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 5824 | 302648.23 | 32.5% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5204 | 338719.59 | 29.0% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 17152 | 102775.36 | 95.7% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 9125 | 131609.59 | 44.9% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6000 | 200177.08 | 29.5% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 19780 | 60716.27 | 97.2% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 1873 | 4498328.90 | 23.5% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 4958 | 1699245.59 | 62.1% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 5161 | 1632243.53 | 64.7% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2208 | 3806527.09 | 19.7% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5402 | 1556105.79 | 48.3% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 11581 | 725896.83 | 103.5% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2849 | 2944790.92 | 23.7% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 3878 | 2163167.28 | 32.3% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 7643 | 1097548.71 | 63.6% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3924 | 313250.26 | 43.6% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4336 | 283446.12 | 48.2% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 9556 | 128612.97 | 106.2% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 1732 | 164466.13 | 27.7% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 5540 | 51411.42 | 88.5% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 5886 | 48391.43 | 94.0% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 11774 Mbps, Track 2 10888 Mbps, sonic-rs 15051 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=2719744 bytes, S anchor sonic-rs=3686400 bytes.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4030464 bytes, S anchor sonic-rs=6635520 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 10644 Mbps, Track 2 10508 Mbps, sonic-rs 12117 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5128192 bytes, S anchor sonic-rs=10305536 bytes.
- canada structural scan: 69075 Mbps; floor is 40000 Mbps.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2080768 bytes, S anchor sonic-rs=2260992 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 11266 Mbps, Track 2 10649 Mbps, sonic-rs 15071 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=1966080 bytes, S anchor sonic-rs=2080768 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 8445 Mbps, Track 2 7589 Mbps, sonic-rs 11045 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=2637824 bytes, S anchor sonic-rs=3588096 bytes.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=2981888 bytes, S anchor sonic-rs=5242880 bytes.
- random direct-to-struct gate: NO-GO. Track 1 7141 Mbps, Track 2 6913 Mbps, sonic-rs 7753 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=2637824 bytes, S anchor sonic-rs=3784704 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 15054 Mbps, Track 2 14240 Mbps, sonic-rs 22223 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=5537792 bytes, S anchor sonic-rs=9289728 bytes.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=6406144 bytes, S anchor sonic-rs=14942208 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 12046 Mbps, Track 2 11087 Mbps, sonic-rs 12675 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2228224 bytes, S anchor sonic-rs=2605056 bytes.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2113536 bytes, S anchor sonic-rs=2523136 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 4750 Mbps, Track 2 4758 Mbps, sonic-rs 10445 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3211264 bytes, S anchor sonic-rs=4784128 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 5239 Mbps, Track 2 5094 Mbps, sonic-rs 13998 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3096576 bytes, S anchor sonic-rs=4276224 bytes.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=3375104 bytes, S anchor sonic-rs=5586944 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 6085 Mbps, Track 2 5142 Mbps, sonic-rs 10621 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2129920 bytes, S anchor sonic-rs=2408448 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 4682 Mbps, Track 2 3495 Mbps, sonic-rs 8243 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=1998848 bytes, S anchor sonic-rs=2048000 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

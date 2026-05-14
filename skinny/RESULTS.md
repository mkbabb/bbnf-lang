# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12240 | 8442 | 21238 | 14631 | 12029 | sonic-rs | 21238 | 57.6% | 39.7% |
| citm_catalog | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 20862 | 13976 | 25229 | 16637 | 14929 | sonic-rs | 25229 | 82.7% | 55.4% |
| canada | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 17679 | 17081 | 14083 | 6214 | 6276 | sonic-rs | 14083 | 125.5% | 121.3% |
| apache_builds | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12344 | 7469 | 17494 | 15725 | 11943 | sonic-rs | 17494 | 70.6% | 42.7% |
| github_events | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13158 | 8919 | 23291 | 18061 | 14533 | sonic-rs | 23291 | 56.5% | 38.3% |
| update_center | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9379 | 5727 | 19840 | 12099 | 8857 | sonic-rs | 19840 | 47.3% | 28.9% |
| mesh | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13442 | 12850 | 11973 | 7275 | 7429 | sonic-rs | 11973 | 112.3% | 107.3% |
| random | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7882 | 4847 | 15566 | 9768 | 7457 | sonic-rs | 15566 | 50.6% | 31.1% |
| gsoc-2018 | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21750 | 16891 | 49651 | 24275 | 19490 | sonic-rs | 49651 | 43.8% | 34.0% |
| marine_ik | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12807 | 11118 | 10172 | 6931 | 6746 | sonic-rs | 10172 | 125.9% | 109.3% |
| instruments | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11890 | 8206 | 19703 | 12606 | 10797 | sonic-rs | 19703 | 60.3% | 41.6% |
| numbers | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18532 | 18848 | 13596 | 8750 | 8865 | sonic-rs | 13596 | 136.3% | 138.6% |
| unicode_mixed | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 8932 | 7254 | 18319 | 8627 | 7698 | sonic-rs | 18319 | 48.8% | 39.6% |
| unicode_escapes | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12783 | 11559 | 18719 | 4696 | 4578 | sonic-rs | 18719 | 68.3% | 61.7% |
| unicode_basic | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 10889 | 6069 | 15734 | 9169 | 6874 | sonic-rs | 15734 | 69.2% | 38.6% |
| distinct_values | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6117 | 4070 | 17818 | 12138 | 9256 | sonic-rs | 17818 | 34.3% | 22.8% |
| y_string_unicode | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6049 | 3982 | 13668 | 6385 | 5628 | sonic-rs | 13668 | 44.3% | 29.1% |

## Workloads

| Corpus | Workload | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9873 | 9735 | 15600 | 11512 | 63.3% | 62.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9873, Track 2 9735, sonic 15600 Mbps |
| citm_catalog | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 16052 | 16536 | 22182 | 14674 | 72.4% | 74.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 16052, Track 2 16536, sonic 22182 Mbps |
| canada | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10368 | 10472 | 12561 | 7914 | 82.5% | 83.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10368, Track 2 10472, sonic 12561 Mbps |
| apache_builds | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6977 | 6634 | 11759 | 10604 | 59.3% | 56.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6977, Track 2 6634, sonic 11759 Mbps |
| github_events | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9886 | 9605 | 17108 | 14142 | 57.8% | 56.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9886, Track 2 9605, sonic 17108 Mbps |
| update_center | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6392 | 6175 | 12775 | 9148 | 50.0% | 48.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6392, Track 2 6175, sonic 12775 Mbps |
| mesh | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8620 | 9034 | 9827 | 7906 | 87.7% | 91.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8620, Track 2 9034, sonic 9827 Mbps |
| random | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4916 | 4718 | 10036 | 7197 | 49.0% | 47.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4916, Track 2 4718, sonic 10036 Mbps |
| gsoc-2018 | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 13508 | 13322 | 24516 | 20398 | 55.1% | 54.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 13508, Track 2 13322, sonic 24516 Mbps |
| marine_ik | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7821 | 8047 | 8767 | 7687 | 89.2% | 91.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7821, Track 2 8047, sonic 8767 Mbps |
| instruments | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9303 | 9190 | 13457 | 10716 | 69.1% | 68.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9303, Track 2 9190, sonic 13457 Mbps |
| numbers | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12504 | 12447 | 12955 | 8747 | 96.5% | 96.1% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| unicode_mixed | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4241 | 4105 | 10988 | 5302 | 38.6% | 37.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4241, Track 2 4105, sonic 10988 Mbps |
| unicode_escapes | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4971 | 4961 | 14236 | 5258 | 34.9% | 34.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4971, Track 2 4961, sonic 14236 Mbps |
| unicode_basic | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5546 | 5188 | 9179 | 6146 | 60.4% | 56.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5546, Track 2 5188, sonic 9179 Mbps |
| distinct_values | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5826 | 5596 | 13271 | 8905 | 43.9% | 42.2% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5826, Track 2 5596, sonic 13271 Mbps |
| y_string_unicode | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4550 | 4462 | 8600 | 7393 | 52.9% | 51.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4550, Track 2 4462, sonic 8600 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4195 | 1204462.54 | 34.3% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6961 | 725807.22 | 56.9% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 10764 | 469354.52 | 87.9% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7079 | 1951998.66 | 33.9% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 8088 | 1708492.54 | 38.8% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 20045 | 689346.81 | 96.1% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 4455 | 4041896.76 | 25.2% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4911 | 3666930.81 | 27.8% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 17004 | 1059092.47 | 96.2% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 4779 | 213062.11 | 38.7% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6752 | 150789.48 | 54.7% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 12048 | 84510.46 | 97.6% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.61 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5510 | 94569.16 | 41.9% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 8268 | 63019.56 | 62.8% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 12748 | 40874.70 | 96.9% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3203 | 1331510.68 | 34.2% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4648 | 917697.09 | 49.6% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 9166 | 465338.19 | 97.7% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5392 | 1073500.98 | 40.1% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 5029 | 1151000.12 | 37.4% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 13123 | 441127.56 | 97.6% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2777 | 1470555.91 | 35.2% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3942 | 1036085.14 | 50.0% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 6987 | 584495.41 | 88.6% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8143 | 3269451.67 | 37.4% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18554 | 1434900.36 | 85.3% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 21023 | 1266352.27 | 96.7% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2521 | 9468561.74 | 19.7% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 3991 | 5979959.90 | 31.2% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 12486 | 1911491.17 | 97.5% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.61 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 4992 | 353099.91 | 42.0% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5257 | 335329.58 | 44.2% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 11634 | 151523.56 | 97.8% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 9536 | 125949.20 | 51.5% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6185 | 194189.02 | 33.4% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 18029 | 66613.53 | 97.3% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 1865 | 4516818.16 | 20.9% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 5019 | 1678428.73 | 56.2% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 6192 | 1360598.09 | 69.3% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2214 | 3796362.18 | 17.3% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5342 | 1573517.57 | 41.8% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 12343 | 681043.62 | 96.6% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2796 | 3000065.05 | 25.7% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4292 | 1954560.29 | 39.4% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 7632 | 1099120.88 | 70.1% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.61 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3196 | 384551.91 | 52.2% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4364 | 281620.11 | 71.3% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 6033 | 203722.56 | 98.6% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 1934 | 147284.51 | 32.0% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 5740 | 49616.30 | 94.9% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 5648 | 50422.61 | 93.4% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 9873 Mbps, Track 2 9735 Mbps, sonic-rs 15600 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3555328 bytes, S anchor sonic-rs=4898816 bytes.
- citm_catalog direct-to-struct gate: NO-GO. Track 1 16052 Mbps, Track 2 16536 Mbps, sonic-rs 22182 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4800512 bytes, S anchor sonic-rs=7733248 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 10368 Mbps, Track 2 10472 Mbps, sonic-rs 12561 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5849088 bytes, S anchor sonic-rs=11337728 bytes.
- canada structural scan: 40637 Mbps; floor is 40000 Mbps.
- apache_builds direct-to-struct gate: NO-GO. Track 1 6977 Mbps, Track 2 6634 Mbps, sonic-rs 11759 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2768896 bytes, S anchor sonic-rs=3063808 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 9886 Mbps, Track 2 9605 Mbps, sonic-rs 17108 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2752512 bytes, S anchor sonic-rs=3047424 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 6392 Mbps, Track 2 6175 Mbps, sonic-rs 12775 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3391488 bytes, S anchor sonic-rs=4489216 bytes.
- mesh direct-to-struct gate: NO-GO. Track 1 8620 Mbps, Track 2 9034 Mbps, sonic-rs 9827 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3670016 bytes, S anchor sonic-rs=6144000 bytes.
- random direct-to-struct gate: NO-GO. Track 1 4916 Mbps, Track 2 4718 Mbps, sonic-rs 10036 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3391488 bytes, S anchor sonic-rs=4718592 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 13508 Mbps, Track 2 13322 Mbps, sonic-rs 24516 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6209536 bytes, S anchor sonic-rs=10158080 bytes.
- marine_ik direct-to-struct gate: NO-GO. Track 1 7821 Mbps, Track 2 8047 Mbps, sonic-rs 8767 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7225344 bytes, S anchor sonic-rs=16220160 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 9303 Mbps, Track 2 9190 Mbps, sonic-rs 13457 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2965504 bytes, S anchor sonic-rs=3571712 bytes.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2686976 bytes, S anchor sonic-rs=3211264 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 4241 Mbps, Track 2 4105 Mbps, sonic-rs 10988 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3915776 bytes, S anchor sonic-rs=5636096 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 4971 Mbps, Track 2 4961 Mbps, sonic-rs 14236 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3801088 bytes, S anchor sonic-rs=5128192 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 5546 Mbps, Track 2 5188 Mbps, sonic-rs 9179 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=4046848 bytes, S anchor sonic-rs=6455296 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 5826 Mbps, Track 2 5596 Mbps, sonic-rs 13271 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2785280 bytes, S anchor sonic-rs=3194880 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 4550 Mbps, Track 2 4462 Mbps, sonic-rs 8600 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2637824 bytes, S anchor sonic-rs=2719744 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

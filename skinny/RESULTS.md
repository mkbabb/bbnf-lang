# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12398 | 12441 | 20727 | 14780 | 12319 | sonic-rs | 20727 | 59.8% | 60.0% |
| citm_catalog | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21110 | 21249 | 25590 | 16728 | 14907 | sonic-rs | 25590 | 82.5% | 83.0% |
| canada | L | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 17321 | 17206 | 13777 | 6315 | 6309 | sonic-rs | 13777 | 125.7% | 124.9% |
| apache_builds | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12526 | 12560 | 17689 | 15831 | 11847 | sonic-rs | 17689 | 70.8% | 71.0% |
| github_events | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13509 | 13601 | 23752 | 18228 | 14610 | sonic-rs | 23752 | 56.9% | 57.3% |
| update_center | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9685 | 9523 | 19295 | 12185 | 8819 | sonic-rs | 19295 | 50.2% | 49.4% |
| mesh | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13582 | 13426 | 12040 | 7534 | 7489 | sonic-rs | 12040 | 112.8% | 111.5% |
| random | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7965 | 8028 | 15716 | 9853 | 7459 | sonic-rs | 15716 | 50.7% | 51.1% |
| gsoc-2018 | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 22249 | 22260 | 50506 | 24361 | 19498 | sonic-rs | 50506 | 44.1% | 44.1% |
| marine_ik | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12794 | 12854 | 10178 | 7138 | 6851 | sonic-rs | 10178 | 125.7% | 126.3% |
| instruments | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11829 | 11829 | 19657 | 12646 | 10742 | sonic-rs | 19657 | 60.2% | 60.2% |
| numbers | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18556 | 18599 | 13581 | 9010 | 8930 | sonic-rs | 13581 | 136.6% | 136.9% |
| unicode_mixed | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 8578 | 8606 | 17751 | 8607 | 7628 | sonic-rs | 17751 | 48.3% | 48.5% |
| unicode_escapes | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12801 | 12855 | 18946 | 4719 | 4636 | sonic-rs | 18946 | 67.6% | 67.9% |
| unicode_basic | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 10800 | 10890 | 15873 | 9618 | 7160 | sonic-rs | 15873 | 68.0% | 68.6% |
| distinct_values | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6127 | 6095 | 17748 | 12044 | 8930 | sonic-rs | 17748 | 34.5% | 34.3% |
| y_string_unicode | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6022 | 6020 | 13686 | 6422 | 5613 | sonic-rs | 13686 | 44.0% | 44.0% |

## Workloads

| Corpus | Workload | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10128 | 9815 | 15678 | 11348 | 64.6% | 62.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10128, Track 2 9815, sonic 15678 Mbps |
| citm_catalog | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 16127 | 16519 | 22022 | 14487 | 73.2% | 75.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 16127, Track 2 16519, sonic 22022 Mbps |
| canada | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10316 | 10480 | 12536 | 7812 | 82.3% | 83.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10316, Track 2 10480, sonic 12536 Mbps |
| apache_builds | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7068 | 6740 | 11668 | 10598 | 60.6% | 57.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7068, Track 2 6740, sonic 11668 Mbps |
| github_events | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10080 | 9715 | 17230 | 14016 | 58.5% | 56.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10080, Track 2 9715, sonic 17230 Mbps |
| update_center | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6526 | 6200 | 12735 | 9117 | 51.2% | 48.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6526, Track 2 6200, sonic 12735 Mbps |
| mesh | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8777 | 8841 | 9768 | 7876 | 89.9% | 90.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8777, Track 2 8841, sonic 9768 Mbps |
| random | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5008 | 4829 | 10134 | 7153 | 49.4% | 47.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5008, Track 2 4829, sonic 10134 Mbps |
| gsoc-2018 | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 13782 | 13624 | 24506 | 20627 | 56.2% | 55.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 13782, Track 2 13624, sonic 24506 Mbps |
| marine_ik | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7805 | 8004 | 8846 | 7620 | 88.2% | 90.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7805, Track 2 8004, sonic 8846 Mbps |
| instruments | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9246 | 9106 | 13363 | 10512 | 69.2% | 68.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9246, Track 2 9106, sonic 13363 Mbps |
| numbers | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12182 | 12069 | 12748 | 8529 | 95.6% | 94.7% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| unicode_mixed | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4178 | 4022 | 11143 | 5255 | 37.5% | 36.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4178, Track 2 4022, sonic 11143 Mbps |
| unicode_escapes | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5018 | 4986 | 14746 | 5317 | 34.0% | 33.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5018, Track 2 4986, sonic 14746 Mbps |
| unicode_basic | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5520 | 5163 | 9653 | 6076 | 57.2% | 53.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5520, Track 2 5163, sonic 9653 Mbps |
| distinct_values | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5815 | 5559 | 13136 | 8681 | 44.3% | 42.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5815, Track 2 5559, sonic 13136 Mbps |
| y_string_unicode | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4518 | 4323 | 8691 | 7285 | 52.0% | 49.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4518, Track 2 4323, sonic 8691 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4264 | 1184936.98 | 34.4% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6948 | 727086.38 | 56.0% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 10912 | 462970.53 | 88.0% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7107 | 1944201.28 | 33.7% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 8047 | 1717133.30 | 38.1% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 20274 | 681543.72 | 96.0% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 4469 | 4029708.49 | 25.8% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4981 | 3615647.08 | 28.8% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 16565 | 1087162.41 | 95.6% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 4859 | 209535.10 | 38.8% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6755 | 150741.19 | 53.9% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 12231 | 83249.96 | 97.6% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5587 | 93261.02 | 41.4% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 8251 | 63151.90 | 61.1% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 13145 | 39638.54 | 97.3% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3226 | 1322065.73 | 33.3% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4662 | 915008.39 | 48.1% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 9451 | 451300.51 | 97.6% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5418 | 1068386.65 | 39.9% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 5035 | 1149699.07 | 37.1% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 13094 | 442100.60 | 96.4% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2840 | 1438024.59 | 35.7% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3925 | 1040381.07 | 49.3% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 7149 | 571260.64 | 89.8% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8241 | 3230647.94 | 37.0% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18437 | 1444011.56 | 82.9% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 21266 | 1251872.71 | 95.6% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2544 | 9380466.39 | 19.9% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 4020 | 5937561.07 | 31.4% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 12220 | 1953199.06 | 95.5% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 4981 | 353893.92 | 42.1% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5215 | 337998.23 | 44.1% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 11538 | 152785.74 | 97.5% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 9373 | 128129.96 | 50.5% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6137 | 195686.01 | 33.1% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 17648 | 68053.59 | 95.1% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 1890 | 4458046.26 | 22.0% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 5137 | 1640014.94 | 59.9% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 6157 | 1368241.19 | 71.8% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2192 | 3835797.71 | 17.1% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5416 | 1552142.92 | 42.3% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 12536 | 670565.53 | 97.9% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2839 | 2954369.29 | 26.3% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4264 | 1967312.62 | 39.5% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 7575 | 1107447.74 | 70.1% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3183 | 386168.77 | 51.9% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4350 | 282513.58 | 71.0% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 6050 | 203131.11 | 98.7% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 1921 | 148292.02 | 31.9% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 5710 | 49879.65 | 94.8% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 5658 | 50340.84 | 93.9% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 10128 Mbps, Track 2 9815 Mbps, sonic-rs 15678 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3506176 bytes, S anchor sonic-rs=4866048 bytes.
- citm_catalog direct-to-struct gate: NO-GO. Track 1 16127 Mbps, Track 2 16519 Mbps, sonic-rs 22022 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4751360 bytes, S anchor sonic-rs=7700480 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 10316 Mbps, Track 2 10480 Mbps, sonic-rs 12536 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5783552 bytes, S anchor sonic-rs=11304960 bytes.
- canada structural scan: 22136 Mbps; floor is 40000 Mbps.
- apache_builds direct-to-struct gate: NO-GO. Track 1 7068 Mbps, Track 2 6740 Mbps, sonic-rs 11668 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2719744 bytes, S anchor sonic-rs=3047424 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 10080 Mbps, Track 2 9715 Mbps, sonic-rs 17230 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2703360 bytes, S anchor sonic-rs=3014656 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 6526 Mbps, Track 2 6200 Mbps, sonic-rs 12735 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3342336 bytes, S anchor sonic-rs=4456448 bytes.
- mesh direct-to-struct gate: NO-GO. Track 1 8777 Mbps, Track 2 8841 Mbps, sonic-rs 9768 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3604480 bytes, S anchor sonic-rs=6111232 bytes.
- random direct-to-struct gate: NO-GO. Track 1 5008 Mbps, Track 2 4829 Mbps, sonic-rs 10134 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3325952 bytes, S anchor sonic-rs=4685824 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 13782 Mbps, Track 2 13624 Mbps, sonic-rs 24506 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6160384 bytes, S anchor sonic-rs=10141696 bytes.
- marine_ik direct-to-struct gate: NO-GO. Track 1 7805 Mbps, Track 2 8004 Mbps, sonic-rs 8846 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7159808 bytes, S anchor sonic-rs=16171008 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 9246 Mbps, Track 2 9106 Mbps, sonic-rs 13363 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2899968 bytes, S anchor sonic-rs=3538944 bytes.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2637824 bytes, S anchor sonic-rs=3178496 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 4178 Mbps, Track 2 4022 Mbps, sonic-rs 11143 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3866624 bytes, S anchor sonic-rs=5603328 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 5018 Mbps, Track 2 4986 Mbps, sonic-rs 14746 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3751936 bytes, S anchor sonic-rs=5111808 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 5520 Mbps, Track 2 5163 Mbps, sonic-rs 9653 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=3981312 bytes, S anchor sonic-rs=6422528 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 5815 Mbps, Track 2 5559 Mbps, sonic-rs 13136 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2719744 bytes, S anchor sonic-rs=3162112 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 4518 Mbps, Track 2 4323 Mbps, sonic-rs 8691 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2605056 bytes, S anchor sonic-rs=2703360 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

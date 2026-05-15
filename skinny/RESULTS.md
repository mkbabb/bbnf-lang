# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 15597 | 12128 | 21184 | 14744 | 12182 | sonic-rs | 21184 | 73.6% | 57.2% |
| citm_catalog | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 32459 | 20792 | 24910 | 16149 | 14498 | sonic-rs | 24910 | 130.3% | 83.5% |
| canada | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18775 | 17133 | 12658 | 5546 | 5750 | sonic-rs | 12658 | 148.3% | 135.4% |
| apache_builds | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12638 | 12227 | 16206 | 14783 | 11199 | sonic-rs | 16206 | 78.0% | 75.4% |
| github_events | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 15268 | 13034 | 22182 | 16580 | 12838 | sonic-rs | 22182 | 68.8% | 58.8% |
| update_center | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11912 | 9226 | 19983 | 12171 | 8451 | sonic-rs | 19983 | 59.6% | 46.2% |
| mesh | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 14330 | 13173 | 11837 | 7209 | 7059 | sonic-rs | 11837 | 121.1% | 111.3% |
| random | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 10071 | 7800 | 15370 | 9142 | 5631 | sonic-rs | 15370 | 65.5% | 50.7% |
| gsoc-2018 | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 23161 | 21870 | 43207 | 21038 | 18671 | sonic-rs | 43207 | 53.6% | 50.6% |
| marine_ik | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13688 | 12801 | 10064 | 7020 | 6843 | sonic-rs | 10064 | 136.0% | 127.2% |
| instruments | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18163 | 11826 | 19737 | 12643 | 10731 | sonic-rs | 19737 | 92.0% | 59.9% |
| numbers | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 20085 | 18671 | 13567 | 8977 | 9133 | sonic-rs | 13567 | 148.0% | 137.6% |
| unicode_mixed | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 8914 | 8940 | 15892 | 7685 | 6505 | sonic-rs | 15892 | 56.1% | 56.3% |
| unicode_escapes | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12905 | 12931 | 16048 | 4128 | 4292 | sonic-rs | 16048 | 80.4% | 80.6% |
| unicode_basic | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12193 | 10782 | 13304 | 7257 | 5729 | sonic-rs | 13304 | 91.7% | 81.0% |
| distinct_values | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9783 | 6100 | 16259 | 12099 | 9196 | sonic-rs | 16259 | 60.2% | 37.5% |
| y_string_unicode | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6290 | 6034 | 13673 | 6319 | 5277 | sonic-rs | 13673 | 46.0% | 44.1% |

## Workloads

| Corpus | Workload | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11899 | 11041 | 15173 | 10581 | 78.4% | 72.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11899, Track 2 11041, sonic 15173 Mbps |
| twitter | real_typed_struct | deferred | generated typed DirectBuild owned struct vs structurally independent typed oracle vs sonic-rs typed serde | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | 18129 | 16028 | 11969 | 16304 | 151.5% | 133.9% | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 16028 Mbps |
| citm_catalog | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 21460 | 25345 | 21615 | 14067 | 99.3% | 117.3% | PASS correctness green; sonic shape parity; throughput within gate |
| canada | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10463 | 5099 | 12512 | 8012 | 83.6% | 40.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10463, Track 2 5099, sonic 12512 Mbps |
| apache_builds | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11314 | 11141 | 10051 | 7617 | 112.6% | 110.8% | PASS correctness green; sonic shape parity; throughput within gate |
| github_events | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12377 | 10625 | 10825 | 8427 | 114.3% | 98.1% | PASS correctness green; sonic shape parity; throughput within gate |
| update_center | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8497 | 7131 | 9520 | 7255 | 89.3% | 74.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8497, Track 2 7131, sonic 9520 Mbps |
| update_center | real_typed_struct | deferred | generated typed DirectBuild owned struct vs structurally independent typed oracle vs sonic-rs typed serde | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | 12044 | 9490 | 12144 | 10437 | 99.2% | 78.1% | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 9490 Mbps |
| mesh | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8818 | 5003 | 9606 | 7909 | 91.8% | 52.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8818, Track 2 5003, sonic 9606 Mbps |
| random | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7858 | 6667 | 9157 | 6212 | 85.8% | 72.8% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7858, Track 2 6667, sonic 9157 Mbps |
| gsoc-2018 | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 15123 | 5744 | 8516 | 7757 | 177.6% | 67.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 15123, Track 2 5744, sonic 8516 Mbps |
| marine_ik | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9400 | 6429 | 8799 | 7606 | 106.8% | 73.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9400, Track 2 6429, sonic 8799 Mbps |
| instruments | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12131 | 16054 | 12974 | 9809 | 93.5% | 123.7% | PASS correctness green; sonic shape parity; throughput within gate |
| numbers | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12625 | 4317 | 12974 | 8759 | 97.3% | 33.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 12625, Track 2 4317, sonic 12974 Mbps |
| unicode_mixed | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4782 | 3199 | 6406 | 3747 | 74.6% | 49.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4782, Track 2 3199, sonic 6406 Mbps |
| unicode_escapes | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5303 | 4576 | 9072 | 4336 | 58.5% | 50.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5303, Track 2 4576, sonic 9072 Mbps |
| unicode_basic | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9180 | 4859 | 7092 | 4947 | 129.4% | 68.5% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9180, Track 2 4859, sonic 7092 Mbps |
| distinct_values | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6269 | 12461 | 11677 | 7552 | 53.7% | 106.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6269, Track 2 12461, sonic 11677 Mbps |
| y_string_unicode | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5070 | 5671 | 8547 | 7172 | 59.3% | 66.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5070, Track 2 5671, sonic 8547 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4445 | 1136560.36 | 28.5% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6685 | 755784.28 | 42.9% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 13680 | 369298.68 | 87.7% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7669 | 1801700.42 | 23.6% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 7428 | 1860236.09 | 22.9% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 26944 | 512831.82 | 83.0% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.76 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 3979 | 4526041.97 | 21.2% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4422 | 4072752.66 | 23.6% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 14460 | 1245359.64 | 77.0% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 6563 | 155142.72 | 51.9% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6136 | 165935.23 | 48.6% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 18086 | 56298.03 | 143.1% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.72 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 7113 | 73252.66 | 46.6% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 7736 | 67350.67 | 50.7% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 24288 | 21452.97 | 159.1% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 4370 | 976010.30 | 36.7% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4491 | 949874.99 | 37.7% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 18434 | 231389.55 | 154.7% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5276 | 1097192.32 | 36.8% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 4403 | 1314636.96 | 30.7% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 12916 | 448171.68 | 90.1% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2821 | 1447900.56 | 28.0% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3670 | 1112864.84 | 36.4% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 6945 | 587988.99 | 69.0% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8515 | 3126694.04 | 36.8% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18545 | 1435550.72 | 80.1% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 49308 | 539930.19 | 212.9% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2539 | 9401881.73 | 18.5% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 3993 | 5977914.31 | 29.2% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 12764 | 1869949.79 | 93.3% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 6389 | 275920.28 | 35.2% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5233 | 336834.64 | 28.8% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 19210 | 91761.90 | 105.8% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 8975 | 133810.28 | 44.7% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 5924 | 202724.58 | 29.5% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 17757 | 67636.06 | 88.4% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 2062 | 4085686.57 | 23.1% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 4026 | 2092781.75 | 45.2% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 4933 | 1707859.15 | 55.3% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.72 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 3736 | 2250173.07 | 28.9% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 4587 | 1832478.95 | 35.5% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 14574 | 576823.49 | 112.9% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2616 | 3206930.18 | 21.5% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4253 | 1972482.28 | 34.9% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 5641 | 1487007.06 | 46.3% | reported cold-sensitive |
| distinct_values | host_call_dispatch_overhead | n/a | 0.59 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 5162 | 238086.91 | 52.8% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4340 | 283203.57 | 44.4% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 16028 | 76682.70 | 163.8% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 2597 | 109655.40 | 41.3% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 6074 | 46888.54 | 96.6% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 11543 | 24673.76 | 183.5% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 11899 Mbps, Track 2 11041 Mbps, sonic-rs 15173 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3588096 bytes, S anchor sonic-rs=4915200 bytes.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4816896 bytes, S anchor sonic-rs=7749632 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 10463 Mbps, Track 2 5099 Mbps, sonic-rs 12512 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5865472 bytes, S anchor sonic-rs=11354112 bytes.
- canada structural scan: 69075 Mbps; floor is 40000 Mbps.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2785280 bytes, S anchor sonic-rs=3096576 bytes.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2768896 bytes, S anchor sonic-rs=3063808 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 8497 Mbps, Track 2 7131 Mbps, sonic-rs 9520 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3407872 bytes, S anchor sonic-rs=4505600 bytes.
- mesh direct-to-struct gate: NO-GO. Track 1 8818 Mbps, Track 2 5003 Mbps, sonic-rs 9606 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3670016 bytes, S anchor sonic-rs=6160384 bytes.
- random direct-to-struct gate: NO-GO. Track 1 7858 Mbps, Track 2 6667 Mbps, sonic-rs 9157 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3407872 bytes, S anchor sonic-rs=4718592 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 15123 Mbps, Track 2 5744 Mbps, sonic-rs 8516 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6225920 bytes, S anchor sonic-rs=10174464 bytes.
- marine_ik direct-to-struct gate: NO-GO. Track 1 9400 Mbps, Track 2 6429 Mbps, sonic-rs 8799 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7258112 bytes, S anchor sonic-rs=16220160 bytes.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2981888 bytes, S anchor sonic-rs=3588096 bytes.
- numbers direct-to-struct gate: NO-GO. Track 1 12625 Mbps, Track 2 4317 Mbps, sonic-rs 12974 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2719744 bytes, S anchor sonic-rs=3227648 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 4782 Mbps, Track 2 3199 Mbps, sonic-rs 6406 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3932160 bytes, S anchor sonic-rs=5652480 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 5303 Mbps, Track 2 4576 Mbps, sonic-rs 9072 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3817472 bytes, S anchor sonic-rs=5144576 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 9180 Mbps, Track 2 4859 Mbps, sonic-rs 7092 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=4063232 bytes, S anchor sonic-rs=6471680 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 6269 Mbps, Track 2 12461 Mbps, sonic-rs 11677 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2801664 bytes, S anchor sonic-rs=3211264 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 5070 Mbps, Track 2 5671 Mbps, sonic-rs 8547 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2670592 bytes, S anchor sonic-rs=2736128 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

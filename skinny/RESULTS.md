# Skinny JSON Bench Results

| Corpus | Outcome | Verdict | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12303 | 12308 | 21176 | 14658 | 12231 | sonic-rs | 21176 | 58.1% | 58.1% |
| citm_catalog | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 20775 | 20856 | 25413 | 16532 | 14945 | sonic-rs | 25413 | 81.8% | 82.1% |
| canada | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 17738 | 17277 | 13719 | 6275 | 6331 | sonic-rs | 13719 | 129.3% | 125.9% |
| apache_builds | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12341 | 12205 | 17453 | 15628 | 11987 | sonic-rs | 17453 | 70.7% | 69.9% |
| github_events | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13161 | 13179 | 23219 | 17915 | 14591 | sonic-rs | 23219 | 56.7% | 56.8% |
| update_center | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 9430 | 9311 | 19835 | 12074 | 8890 | sonic-rs | 19835 | 47.5% | 46.9% |
| mesh | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 13411 | 13278 | 11871 | 7311 | 7426 | sonic-rs | 11871 | 113.0% | 111.8% |
| random | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 7794 | 7763 | 15451 | 9631 | 7431 | sonic-rs | 15451 | 50.4% | 50.2% |
| gsoc-2018 | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 21907 | 21860 | 48816 | 24039 | 19236 | sonic-rs | 48816 | 44.9% | 44.8% |
| marine_ik | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12818 | 12803 | 9977 | 7028 | 6823 | sonic-rs | 9977 | 128.5% | 128.3% |
| instruments | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 11887 | 11950 | 19714 | 12576 | 10721 | sonic-rs | 19714 | 60.3% | 60.6% |
| numbers | A | GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 18740 | 18777 | 13523 | 8810 | 9101 | sonic-rs | 13523 | 138.6% | 138.9% |
| unicode_mixed | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 8720 | 8623 | 15681 | 8570 | 7658 | sonic-rs | 15681 | 55.6% | 55.0% |
| unicode_escapes | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 12848 | 13039 | 19090 | 4697 | 4606 | sonic-rs | 19090 | 67.3% | 68.3% |
| unicode_basic | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 10898 | 10581 | 15753 | 9373 | 7107 | sonic-rs | 15753 | 69.2% | 67.2% |
| distinct_values | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6097 | 6093 | 17828 | 11903 | 9058 | sonic-rs | 17828 | 34.2% | 34.2% |
| y_string_unicode | G | NO-GO | deferred | typed_root_over_offset_tape vs competitor DOM | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | 6084 | 6051 | 13633 | 6385 | 5662 | sonic-rs | 13633 | 44.6% | 44.4% |

## Workloads

| Corpus | Workload | Strictness | Output plane | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |
|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|
| twitter | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11932 | 10986 | 15614 | 11546 | 76.4% | 70.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11932, Track 2 10986, sonic 15614 Mbps |
| citm_catalog | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 21546 | 20204 | 21874 | 14594 | 98.5% | 92.4% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| canada | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 10529 | 10455 | 12606 | 7806 | 83.5% | 82.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10529, Track 2 10455, sonic 12606 Mbps |
| apache_builds | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 11348 | 10250 | 11791 | 10568 | 96.2% | 86.9% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11348, Track 2 10250, sonic 11791 Mbps |
| github_events | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12411 | 11379 | 17217 | 13930 | 72.1% | 66.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 12411, Track 2 11379, sonic 17217 Mbps |
| update_center | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8534 | 7716 | 12620 | 9107 | 67.6% | 61.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8534, Track 2 7716, sonic 12620 Mbps |
| mesh | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 8942 | 9142 | 9691 | 7927 | 92.3% | 94.3% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| random | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 7831 | 7028 | 10021 | 7129 | 78.1% | 70.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7831, Track 2 7028, sonic 10021 Mbps |
| gsoc-2018 | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 15115 | 14557 | 24392 | 20179 | 62.0% | 59.7% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 15115, Track 2 14557, sonic 24392 Mbps |
| marine_ik | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9500 | 9337 | 8809 | 7664 | 107.8% | 106.0% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| instruments | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12028 | 11107 | 13358 | 10704 | 90.0% | 83.1% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 12028, Track 2 11107, sonic 13358 Mbps |
| numbers | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 12633 | 12153 | 12583 | 8623 | 100.4% | 96.6% | PASS sink_only track1=track2=serde; sonic shape parity; throughput within gate |
| unicode_mixed | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 4633 | 4593 | 11117 | 5299 | 41.7% | 41.3% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4633, Track 2 4593, sonic 11117 Mbps |
| unicode_escapes | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5262 | 5129 | 14427 | 5264 | 36.5% | 35.6% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5262, Track 2 5129, sonic 14427 Mbps |
| unicode_basic | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 9187 | 8335 | 9647 | 6112 | 95.2% | 86.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 9187, Track 2 8335, sonic 9647 Mbps |
| distinct_values | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 6212 | 5609 | 13214 | 8825 | 47.0% | 42.4% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6212, Track 2 5609, sonic 13214 Mbps |
| y_string_unicode | direct_to_struct | deferred | generated SinkOnly digest vs independent hand SinkOnly digest vs sonic-rs typed serde | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | 5006 | 3732 | 8877 | 7634 | 56.4% | 42.0% | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5006, Track 2 3732, sonic 8877 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4232 | 1193840.52 | 34.4% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6937 | 728270.42 | 56.4% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 7764 | 650710.94 | 63.1% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.66 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7016 | 1969433.67 | 33.8% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 8070 | 1712326.92 | 38.8% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 13525 | 1021629.03 | 65.1% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 4453 | 4043754.44 | 25.1% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4975 | 3620037.63 | 28.0% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 16721 | 1077007.24 | 94.3% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 4782 | 212906.91 | 38.8% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6728 | 151326.52 | 54.5% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 7336 | 138792.64 | 59.4% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.61 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5535 | 94132.19 | 42.1% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 8232 | 63294.38 | 62.6% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 8740 | 59616.03 | 66.4% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3176 | 1343090.34 | 33.7% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4650 | 917255.56 | 49.3% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 5636 | 756881.82 | 59.8% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5400 | 1071983.59 | 40.3% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 5025 | 1152079.46 | 37.5% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 12654 | 457468.15 | 94.4% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2769 | 1474689.07 | 35.5% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3882 | 1052067.77 | 49.8% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 4428 | 922196.55 | 56.8% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8011 | 3323400.86 | 36.6% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18123 | 1469006.06 | 82.7% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 16232 | 1640092.16 | 74.1% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2523 | 9458306.05 | 19.7% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 4006 | 5958310.74 | 31.3% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 10891 | 2191554.95 | 85.0% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 4971 | 354620.31 | 41.8% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5207 | 338539.22 | 43.8% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 8003 | 220261.41 | 67.3% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.63 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 9508 | 126319.99 | 50.7% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6197 | 193798.04 | 33.1% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 18049 | 66540.68 | 96.3% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 1883 | 4474030.66 | 21.6% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 4855 | 1735187.65 | 55.7% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 5360 | 1571842.21 | 61.5% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2214 | 3796292.42 | 17.2% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5287 | 1590029.55 | 41.2% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 11304 | 743667.06 | 88.0% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.67 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2829 | 2965275.78 | 26.0% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 4246 | 1975471.60 | 39.0% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 4905 | 1710295.56 | 45.0% | reported cold-sensitive |
| distinct_values | host_call_dispatch_overhead | n/a | 0.65 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3200 | 384069.26 | 52.5% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4371 | 281167.62 | 71.7% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 4042 | 304041.07 | 66.3% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.63 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 1909 | 149199.83 | 31.4% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 5928 | 48043.71 | 97.4% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 3840 | 74178.03 | 63.1% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 11932 Mbps, Track 2 10986 Mbps, sonic-rs 15614 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=3538944 bytes, S anchor sonic-rs=4915200 bytes.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4784128 bytes, S anchor sonic-rs=7733248 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 10529 Mbps, Track 2 10455 Mbps, sonic-rs 12606 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5816320 bytes, S anchor sonic-rs=11337728 bytes.
- canada structural scan: 41495 Mbps; floor is 40000 Mbps.
- apache_builds direct-to-struct gate: NO-GO. Track 1 11348 Mbps, Track 2 10250 Mbps, sonic-rs 11791 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2768896 bytes, S anchor sonic-rs=3063808 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 12411 Mbps, Track 2 11379 Mbps, sonic-rs 17217 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=2736128 bytes, S anchor sonic-rs=3047424 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 8534 Mbps, Track 2 7716 Mbps, sonic-rs 12620 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=3375104 bytes, S anchor sonic-rs=4489216 bytes.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=3620864 bytes, S anchor sonic-rs=6127616 bytes.
- random direct-to-struct gate: NO-GO. Track 1 7831 Mbps, Track 2 7028 Mbps, sonic-rs 10021 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=3358720 bytes, S anchor sonic-rs=4734976 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 15115 Mbps, Track 2 14557 Mbps, sonic-rs 24392 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=6209536 bytes, S anchor sonic-rs=10158080 bytes.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=7192576 bytes, S anchor sonic-rs=16171008 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 12028 Mbps, Track 2 11107 Mbps, sonic-rs 13358 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2932736 bytes, S anchor sonic-rs=3571712 bytes.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2686976 bytes, S anchor sonic-rs=3194880 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 4633 Mbps, Track 2 4593 Mbps, sonic-rs 11117 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3915776 bytes, S anchor sonic-rs=5636096 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 5262 Mbps, Track 2 5129 Mbps, sonic-rs 14427 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3801088 bytes, S anchor sonic-rs=5111808 bytes.
- unicode_basic direct-to-struct gate: NO-GO. Track 1 9187 Mbps, Track 2 8335 Mbps, sonic-rs 9647 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=4014080 bytes, S anchor sonic-rs=6455296 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 6212 Mbps, Track 2 5609 Mbps, sonic-rs 13214 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2752512 bytes, S anchor sonic-rs=3194880 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 5006 Mbps, Track 2 3732 Mbps, sonic-rs 8877 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=2654208 bytes, S anchor sonic-rs=2703360 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named.

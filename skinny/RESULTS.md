# Skinny JSON Bench Results

| Corpus | Workload | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Output plane | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | sonic-rs lossy Mbps | simdjson DOM Mbps | simdjson On Demand Mbps | yyjson default Mbps | asmjson SWAR Mbps | asmjson AVX-512 Mbps | RapidJSON default Mbps | serde_json Mbps | Δ vs SK-V6 | Δ vs sonic-strict | Δ vs simdjson DOM | Δ vs yyjson | Hot leaf | Signal |
|---|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---:|---:|---:|---|---|
| twitter | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 15752 | 12285 | 21020 | 20919 | 24522 | n/a | 30931 | n/a | n/a | 4020 | 5974 | n/a (no machine-readable SK-V6 baseline in W0b) | -25.1% | -35.8% | -49.1% | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| twitter | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 11832 | 10986 | 14885 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 10465 | n/a (no machine-readable SK-V6 baseline in W0b) | -20.5% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11832, Track 2 10986, sonic 14885 Mbps |
| twitter | real_typed_struct | A | GO | deferred | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | typed direct | 18513 | 16193 | 15486 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 16332 | n/a (no machine-readable SK-V6 baseline in W0b) | +19.5% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 16193 Mbps |
| citm_catalog | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 31784 | 20817 | 25509 | 23834 | 35822 | n/a | 20956 | n/a | n/a | 6760 | 7541 | n/a (no machine-readable SK-V6 baseline in W0b) | +24.6% | -11.3% | +51.7% | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| citm_catalog | direct_to_struct | A | GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 21438 | 20280 | 19966 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 13065 | n/a (no machine-readable SK-V6 baseline in W0b) | +7.4% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS correctness green; sonic shape parity; throughput within gate |
| canada | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 17765 | 17070 | 13885 | 13792 | 11493 | n/a | 13003 | n/a | n/a | 5187 | 5215 | n/a (no machine-readable SK-V6 baseline in W0b) | +27.9% | +54.6% | +36.6% | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| canada | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 10773 | 10296 | 12421 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 7469 | n/a (no machine-readable SK-V6 baseline in W0b) | -13.3% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 10773, Track 2 10296, sonic 12421 Mbps |
| apache_builds | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 12482 | 12151 | 17381 | 17397 | 36014 | n/a | 16275 | n/a | n/a | 3945 | 6051 | n/a (no machine-readable SK-V6 baseline in W0b) | -28.2% | -65.3% | -23.3% | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| apache_builds | direct_to_struct | A | GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 11116 | 10187 | 11122 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 9886 | n/a (no machine-readable SK-V6 baseline in W0b) | -0.1% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS correctness green; sonic shape parity; throughput within gate |
| github_events | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 15198 | 13046 | 23034 | 23023 | 39642 | n/a | 21426 | n/a | n/a | n/a | 7686 | n/a (no machine-readable SK-V6 baseline in W0b) | -34.0% | -61.7% | -29.1% | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| github_events | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 12270 | 11366 | 16041 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 12799 | n/a (no machine-readable SK-V6 baseline in W0b) | -23.5% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 12270, Track 2 11366, sonic 16041 Mbps |
| update_center | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 11193 | 9227 | 19684 | 19660 | 30593 | n/a | 18540 | n/a | n/a | n/a | 4244 | n/a (no machine-readable SK-V6 baseline in W0b) | -43.1% | -63.4% | -39.6% | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| update_center | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 8401 | 7667 | 11081 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 8193 | n/a (no machine-readable SK-V6 baseline in W0b) | -24.2% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 8401, Track 2 7667, sonic 11081 Mbps |
| update_center | real_typed_struct | A | GO | deferred | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | typed direct | 11879 | 10451 | 12627 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 10602 | n/a (no machine-readable SK-V6 baseline in W0b) | -5.9% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 10451 Mbps |
| mesh | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 14265 | 13287 | 11754 | 11782 | 9414 | n/a | n/a | n/a | n/a | n/a | 4890 | n/a (no machine-readable SK-V6 baseline in W0b) | +21.4% | +51.5% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| mesh | direct_to_struct | A | GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 8259 | 8483 | 8789 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 7165 | n/a (no machine-readable SK-V6 baseline in W0b) | -6.0% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS correctness green; sonic shape parity; throughput within gate |
| mesh | real_typed_struct | A | GO | deferred | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | typed direct | 9466 | 8089 | 8696 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 6769 | n/a (no machine-readable SK-V6 baseline in W0b) | +8.9% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 8089 Mbps |
| random | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 9838 | 7804 | 15457 | 15471 | 20638 | n/a | n/a | n/a | n/a | 3526 | 3579 | n/a (no machine-readable SK-V6 baseline in W0b) | -36.4% | -52.3% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| random | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 7727 | 7123 | 8936 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 6536 | n/a (no machine-readable SK-V6 baseline in W0b) | -13.5% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 7727, Track 2 7123, sonic 8936 Mbps |
| gsoc-2018 | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 23026 | 21881 | 49292 | 49278 | n/a | n/a | n/a | n/a | n/a | n/a | 16349 | n/a (no machine-readable SK-V6 baseline in W0b) | -53.3% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| gsoc-2018 | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 15097 | 14306 | 23407 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 19567 | n/a (no machine-readable SK-V6 baseline in W0b) | -35.5% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 15097, Track 2 14306, sonic 23407 Mbps |
| marine_ik | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 13797 | 12384 | 10070 | 10100 | n/a | n/a | n/a | n/a | n/a | n/a | 4044 | n/a (no machine-readable SK-V6 baseline in W0b) | +37.0% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| marine_ik | direct_to_struct | A | GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 8943 | 9151 | 8147 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 6966 | n/a (no machine-readable SK-V6 baseline in W0b) | +9.8% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS correctness green; sonic shape parity; throughput within gate |
| marine_ik | real_typed_struct | A | GO | deferred | view-boundary | yes | generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary | typed direct | 12020 | 9630 | 8750 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 9269 | n/a (no machine-readable SK-V6 baseline in W0b) | +37.4% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS generated typed output within sonic-rs * 1.10 ns slack; correctness PASS; Track 2 oracle structurally different at 9630 Mbps |
| instruments | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 18038 | 11678 | 16312 | 18747 | n/a | n/a | n/a | n/a | n/a | 7477 | 4426 | n/a (no machine-readable SK-V6 baseline in W0b) | +10.6% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| instruments | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 11972 | 11086 | 12673 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 9350 | n/a (no machine-readable SK-V6 baseline in W0b) | -5.5% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 11972, Track 2 11086, sonic 12673 Mbps |
| numbers | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 20609 | 18514 | 13626 | 13578 | n/a | n/a | n/a | n/a | n/a | n/a | 6330 | n/a (no machine-readable SK-V6 baseline in W0b) | +51.2% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| numbers | direct_to_struct | A | GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 12615 | 12362 | 12838 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 8081 | n/a (no machine-readable SK-V6 baseline in W0b) | -1.7% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS correctness green; sonic shape parity; throughput within gate |
| unicode_mixed | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 8035 | 7698 | 16180 | 16659 | 13150 | n/a | n/a | n/a | n/a | n/a | 3887 | n/a (no machine-readable SK-V6 baseline in W0b) | -50.3% | -38.9% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| unicode_mixed | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 4579 | 4431 | 9679 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 4956 | n/a (no machine-readable SK-V6 baseline in W0b) | -52.7% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4579, Track 2 4431, sonic 9679 Mbps |
| unicode_escapes | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 12042 | 11146 | 18415 | 18828 | 5637 | n/a | n/a | n/a | n/a | n/a | 4810 | n/a (no machine-readable SK-V6 baseline in W0b) | -34.6% | +113.6% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| unicode_escapes | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 4866 | 4973 | 14028 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 5168 | n/a (no machine-readable SK-V6 baseline in W0b) | -65.3% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4866, Track 2 4973, sonic 14028 Mbps |
| unicode_basic | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 11416 | 10653 | 15596 | 15625 | 16276 | n/a | n/a | n/a | n/a | n/a | 3336 | n/a (no machine-readable SK-V6 baseline in W0b) | -26.8% | -29.9% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| unicode_basic | direct_to_struct | A | GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 8576 | 8059 | 8502 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 5482 | n/a (no machine-readable SK-V6 baseline in W0b) | +0.9% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | PASS correctness green; sonic shape parity; throughput within gate |
| distinct_values | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 6655 | 5633 | 17148 | 17166 | 22825 | n/a | n/a | n/a | n/a | n/a | 3881 | n/a (no machine-readable SK-V6 baseline in W0b) | -61.2% | -70.8% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| distinct_values | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 6105 | 5362 | 11344 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 8221 | n/a (no machine-readable SK-V6 baseline in W0b) | -46.2% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 6105, Track 2 5362, sonic 11344 Mbps |
| y_string_unicode | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 6216 | 6038 | 13537 | 13551 | 13627 | n/a | n/a | n/a | n/a | n/a | 5704 | n/a (no machine-readable SK-V6 baseline in W0b) | -54.1% | -54.4% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| y_string_unicode | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 5029 | 3766 | 9019 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 7604 | n/a (no machine-readable SK-V6 baseline in W0b) | -44.2% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5029, Track 2 3766, sonic 9019 Mbps |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4541 | 1112452.14 | 28.8% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6888 | 733464.39 | 43.7% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 13198 | 382783.06 | 83.8% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.72 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 7898 | 1749529.20 | 24.8% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 7163 | 1929032.48 | 22.5% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 28465 | 485424.77 | 89.6% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.68 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 4494 | 4006950.93 | 25.3% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 4853 | 3711004.72 | 27.3% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 18040 | 998270.28 | 101.5% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.61 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 4809 | 211735.56 | 38.5% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 6689 | 152218.33 | 53.6% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 12112 | 84065.54 | 97.0% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.63 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5801 | 89827.18 | 38.2% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 8171 | 63770.63 | 53.8% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 14551 | 35808.90 | 95.7% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3337 | 1278335.29 | 29.8% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 4596 | 928047.07 | 41.1% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 10760 | 396425.27 | 96.1% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 5526 | 1047565.66 | 38.7% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 4954 | 1168514.49 | 34.7% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 13854 | 417838.68 | 97.1% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2991 | 1365366.44 | 30.4% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 3886 | 1051012.62 | 39.5% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 8444 | 483625.84 | 85.8% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 8232 | 3233952.67 | 35.8% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 18335 | 1452027.75 | 79.6% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 22000 | 1210097.60 | 95.5% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 2514 | 9495758.06 | 18.2% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 3825 | 6240283.70 | 27.7% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 13279 | 1797430.90 | 96.2% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.61 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 5861 | 300766.92 | 32.5% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5194 | 339410.88 | 28.8% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 17667 | 99776.47 | 97.9% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.62 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 10033 | 119700.21 | 48.7% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 6176 | 194467.39 | 30.0% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 19751 | 60807.22 | 95.8% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 1841 | 4576479.85 | 22.9% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 4961 | 1698018.46 | 61.7% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 5748 | 1465635.79 | 71.5% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2180 | 3856998.37 | 18.1% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5225 | 1608774.64 | 43.4% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 12166 | 690956.25 | 101.0% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 2757 | 3042362.94 | 24.2% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 3967 | 2114376.94 | 34.8% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 7520 | 1115556.74 | 65.9% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.60 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 3890 | 315956.76 | 58.5% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4339 | 283244.79 | 65.2% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 9188 | 133771.34 | 138.1% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.59 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 1984 | 143528.23 | 31.9% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 6077 | 46866.76 | 97.8% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 6140 | 46386.97 | 98.8% | PASS <=2.00x T1 |

## Notes

- twitter direct-to-struct gate: NO-GO. Track 1 11832 Mbps, Track 2 10986 Mbps, sonic-rs 14885 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 29573 offsets, 118292 logical offset bytes + 1560 sparse flag bytes (0.19x input), 133632 allocated tape bytes (0.21x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 18099, numbers 2109, literals 4737, separators 0.
- twitter peak RSS subprocess probes: bbnf=2703360 bytes, S anchor sonic-rs=3686400 bytes.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 85035 offsets, 340140 logical offset bytes + 5 sparse flag bytes (0.20x input), 524312 allocated tape bytes (0.30x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 26604, numbers 14392, literals 1263, separators 0.
- citm_catalog peak RSS subprocess probes: bbnf=4014080 bytes, S anchor sonic-rs=6619136 bytes.
- canada direct-to-struct gate: NO-GO. Track 1 10773 Mbps, Track 2 10296 Mbps, sonic-rs 12421 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 223236 offsets, 892944 logical offset bytes + 0 sparse flag bytes (0.40x input), 1048576 allocated tape bytes (0.47x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 12, numbers 111126, literals 0, separators 0.
- canada peak RSS subprocess probes: bbnf=5111808 bytes, S anchor sonic-rs=10289152 bytes.
- canada structural scan: 69075 Mbps; floor is 40000 Mbps.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 7068 offsets, 28272 logical offset bytes + 5 sparse flag bytes (0.22x input), 32792 allocated tape bytes (0.26x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 5289, numbers 2, literals 3, separators 0.
- apache_builds peak RSS subprocess probes: bbnf=2064384 bytes, S anchor sonic-rs=2244608 bytes.
- github_events direct-to-struct gate: NO-GO. Track 1 12270 Mbps, Track 2 11366 Mbps, sonic-rs 16041 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 2526 offsets, 10104 logical offset bytes + 25 sparse flag bytes (0.16x input), 16424 allocated tape bytes (0.25x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 1891, numbers 149, literals 88, separators 0.
- github_events peak RSS subprocess probes: bbnf=1966080 bytes, S anchor sonic-rs=2064384 bytes.
- update_center direct-to-struct gate: NO-GO. Track 1 8401 Mbps, Track 2 7667 Mbps, sonic-rs 11081 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 35281 offsets, 141124 logical offset bytes + 1045 sparse flag bytes (0.27x input), 263424 allocated tape bytes (0.49x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 27229, numbers 0, literals 386, separators 0.
- update_center peak RSS subprocess probes: bbnf=2621440 bytes, S anchor sonic-rs=3588096 bytes.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 80250 offsets, 321000 logical offset bytes + 0 sparse flag bytes (0.44x input), 524288 allocated tape bytes (0.72x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 11, numbers 73013, literals 0, separators 0.
- mesh peak RSS subprocess probes: bbnf=2981888 bytes, S anchor sonic-rs=5226496 bytes.
- random direct-to-struct gate: NO-GO. Track 1 7727 Mbps, Track 2 7123 Mbps, sonic-rs 8936 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 49011 offsets, 196044 logical offset bytes + 0 sparse flag bytes (0.38x input), 262144 allocated tape bytes (0.51x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 33005, numbers 5002, literals 1000, separators 0.
- random peak RSS subprocess probes: bbnf=2637824 bytes, S anchor sonic-rs=3784704 bytes.
- gsoc-2018 direct-to-struct gate: NO-GO. Track 1 15097 Mbps, Track 2 14306 Mbps, sonic-rs 23407 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 41714 offsets, 166856 logical offset bytes + 8545 sparse flag bytes (0.05x input), 272384 allocated tape bytes (0.08x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 34128, numbers 0, literals 0, separators 0.
- gsoc-2018 peak RSS subprocess probes: bbnf=5488640 bytes, S anchor sonic-rs=9322496 bytes.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 359563 offsets, 1438252 logical offset bytes + 0 sparse flag bytes (0.48x input), 2097152 allocated tape bytes (0.70x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 38268, numbers 245175, literals 6, separators 0.
- marine_ik peak RSS subprocess probes: bbnf=6389760 bytes, S anchor sonic-rs=14925824 bytes.
- instruments direct-to-struct gate: NO-GO. Track 1 11972 Mbps, Track 2 11086 Mbps, sonic-rs 12673 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 14793 offsets, 59172 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.30x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 6889, numbers 4935, literals 557, separators 0.
- instruments peak RSS subprocess probes: bbnf=2211840 bytes, S anchor sonic-rs=2588672 bytes.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 10003 offsets, 40012 logical offset bytes + 0 sparse flag bytes (0.27x input), 65536 allocated tape bytes (0.44x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 0.
- numbers peak RSS subprocess probes: bbnf=2113536 bytes, S anchor sonic-rs=2490368 bytes.
- unicode_mixed direct-to-struct gate: NO-GO. Track 1 4579 Mbps, Track 2 4431 Mbps, sonic-rs 9679 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 41870 offsets, 167480 logical offset bytes + 9795 sparse flag bytes (0.17x input), 272384 allocated tape bytes (0.26x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 25121, numbers 8371, literals 0, separators 0.
- unicode_mixed peak RSS subprocess probes: bbnf=3194880 bytes, S anchor sonic-rs=4784128 bytes.
- unicode_escapes direct-to-struct gate: NO-GO. Track 1 4866 Mbps, Track 2 4973 Mbps, sonic-rs 14028 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 11274 offsets, 45096 logical offset bytes + 9385 sparse flag bytes (0.05x input), 75776 allocated tape bytes (0.07x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 5636, numbers 1877, literals 1, separators 0.
- unicode_escapes peak RSS subprocess probes: bbnf=3063808 bytes, S anchor sonic-rs=4259840 bytes.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 92146 offsets, 368584 logical offset bytes + 0 sparse flag bytes (0.35x input), 524288 allocated tape bytes (0.50x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 57590, numbers 11518, literals 0, separators 0.
- unicode_basic peak RSS subprocess probes: bbnf=3358720 bytes, S anchor sonic-rs=5586944 bytes.
- distinct_values direct-to-struct gate: NO-GO. Track 1 6105 Mbps, Track 2 5362 Mbps, sonic-rs 11344 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 11118 offsets, 44472 logical offset bytes + 0 sparse flag bytes (0.29x input), 65536 allocated tape bytes (0.43x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 9796, numbers 440, literals 0, separators 0.
- distinct_values peak RSS subprocess probes: bbnf=2129920 bytes, S anchor sonic-rs=2375680 bytes.
- y_string_unicode direct-to-struct gate: NO-GO. Track 1 5029 Mbps, Track 2 3766 Mbps, sonic-rs 9019 Mbps; Track 1 and Track 2 must be within 1.10x sonic-rs time.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 2202 offsets, 8808 logical offset bytes + 9000 sparse flag bytes (0.50x input), 26624 allocated tape bytes (0.75x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 2200, numbers 0, literals 0, separators 0.
- y_string_unicode peak RSS subprocess probes: bbnf=1949696 bytes, S anchor sonic-rs=2031616 bytes.
- Overall outcome N-direct / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.
- Schema v3 sidecar provenance: sonic-rs strict/lossy and serde_json rows are same-run; C++ simdjson, yyjson, RapidJSON, and asmjson columns come only from documented sidecar profile artefacts when populated and do not count as same-run strict anchors.

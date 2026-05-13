# Skinny JSON Bench Results

Authority note, 2026-05-12: this file is the current measured gate. The
original twitter / citm_catalog / canada skinny triad passed after the lazy
offset tape and local hot-path work, but the expanded corpus below is binding
for SOTA-BEAT dispatch. Its current aggregate verdict is **G / NoGo** because
`github_events`, `update_center`, `random`, `unicode_escapes`, and
`y_string_unicode` miss the S anchor. Treat the triad pass as substrate
validation; treat this expanded result as the current implementation target.

SK-V3 reprofile note, 2026-05-12: fresh samply rows for three expanded
blockers live under `skinny/profile/reprofile-2026-05-12/`. `random` and
`unicode_escapes` are dominated by `runtime::generated_json::generated::parse_value_at`;
`update-center` spreads across parse entry, sparse-flag capacity, and allocation
growth. The next redress target is typed event cursor consumption plus capacity
policy, not another eager-token or tape-width perturbation.

| Corpus | Outcome | Verdict | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |
|---|---:|---|---:|---:|---:|---:|---:|---|---:|---:|---:|
| twitter | A | GO | 22071 | 22058 | 20251 | 13708 | 11557 | sonic-rs | 20251 | 109.0% | 108.9% |
| citm_catalog | A | GO | 29959 | 26018 | 13388 | 11869 | 11626 | sonic-rs | 13388 | 223.8% | 194.3% |
| canada | A | GO | 14051 | 13960 | 12107 | 5127 | 4761 | sonic-rs | 12107 | 116.1% | 115.3% |
| apache_builds | A | GO | 15515 | 13441 | 11765 | 12056 | 7416 | simd-json borrowed | 12056 | 128.7% | 111.5% |
| github_events | G | NO-GO | 19017 | 16515 | 19678 | 13954 | 10550 | sonic-rs | 19678 | 96.6% | 83.9% |
| update_center | G | NO-GO | 14789 | 13270 | 16299 | 8753 | 5628 | sonic-rs | 16299 | 90.7% | 81.4% |
| mesh | A | GO | 10019 | 10731 | 9447 | 5429 | 5159 | sonic-rs | 9447 | 106.1% | 113.6% |
| random | G | NO-GO | 9370 | 8821 | 11586 | 6054 | 4480 | sonic-rs | 11586 | 80.9% | 76.1% |
| gsoc-2018 | C | GO | 29535 | 29365 | 29771 | 14476 | 10851 | sonic-rs | 29771 | 99.2% | 98.6% |
| marine_ik | A | GO | 9026 | 9102 | 7490 | 4700 | 4562 | sonic-rs | 7490 | 120.5% | 121.5% |
| instruments | A | GO | 16109 | 15931 | 15134 | 8263 | 9279 | sonic-rs | 15134 | 106.4% | 105.3% |
| numbers | A | GO | 16292 | 14778 | 12673 | 8555 | 8306 | sonic-rs | 12673 | 128.6% | 116.6% |
| unicode_mixed | C | GO | 14422 | 13687 | 14107 | 8185 | 7344 | sonic-rs | 14107 | 102.2% | 97.0% |
| unicode_escapes | G | NO-GO | 4928 | 4883 | 17854 | 4440 | 4253 | sonic-rs | 17854 | 27.6% | 27.3% |
| unicode_basic | C | GO | 14520 | 14398 | 14850 | 7893 | 6573 | sonic-rs | 14850 | 97.8% | 97.0% |
| distinct_values | C | GO | 16165 | 16985 | 16642 | 11327 | 8086 | sonic-rs | 16642 | 97.1% | 102.1% |
| y_string_unicode | G | NO-GO | 7257 | 7451 | 13343 | 6044 | 5061 | sonic-rs | 13343 | 54.4% | 55.8% |

## Masking Probes

| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |
|---|---|---:|---:|---:|---|
| twitter | host_call_dispatch_overhead | n/a | 0.74 | n/a | PASS <=50ns |
| twitter | host_call_eager_decode | 4960 | 1018493.71 | 22.5% | MASKING >1.15x T1 |
| twitter | alternate_scalar_plan | 6849 | 737609.39 | 31.0% | reported |
| twitter | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| twitter | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| twitter | cold_first_parse | 18354 | 275255.31 | 83.2% | PASS <=2.00x T1 |
| citm_catalog | host_call_dispatch_overhead | n/a | 0.77 | n/a | PASS <=50ns |
| citm_catalog | host_call_eager_decode | 6504 | 2124479.91 | 21.7% | MASKING >1.08x T1 |
| citm_catalog | alternate_scalar_plan | 6906 | 2000814.84 | 23.1% | reported |
| citm_catalog | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| citm_catalog | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| citm_catalog | cold_first_parse | 25525 | 541342.24 | 85.2% | PASS <=2.00x T1 |
| canada | host_call_dispatch_overhead | n/a | 0.76 | n/a | PASS <=50ns |
| canada | host_call_eager_decode | 2586 | 6964477.14 | 18.4% | MASKING >1.02x T1 |
| canada | alternate_scalar_plan | 3508 | 5133496.03 | 25.0% | reported |
| canada | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| canada | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| canada | cold_first_parse | 12291 | 1465113.55 | 87.5% | PASS <=2.00x T1 |
| apache_builds | host_call_dispatch_overhead | n/a | 0.96 | n/a | PASS <=50ns |
| apache_builds | host_call_eager_decode | 5219 | 195109.21 | 33.6% | MASKING >1.10x T1 |
| apache_builds | alternate_scalar_plan | 5425 | 187681.76 | 35.0% | reported |
| apache_builds | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| apache_builds | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| apache_builds | cold_first_parse | 14329 | 71056.90 | 92.4% | PASS <=2.00x T1 |
| github_events | host_call_dispatch_overhead | n/a | 0.73 | n/a | PASS <=50ns |
| github_events | host_call_eager_decode | 5650 | 92218.06 | 29.7% | MASKING >1.10x T1 |
| github_events | alternate_scalar_plan | 5867 | 88810.49 | 30.9% | reported |
| github_events | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| github_events | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| github_events | cold_first_parse | 17058 | 30546.17 | 89.7% | PASS <=2.00x T1 |
| update_center | host_call_dispatch_overhead | n/a | 0.81 | n/a | PASS <=50ns |
| update_center | host_call_eager_decode | 3217 | 1325866.31 | 21.8% | MASKING >1.10x T1 |
| update_center | alternate_scalar_plan | 3478 | 1226359.39 | 23.5% | reported |
| update_center | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| update_center | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| update_center | cold_first_parse | 13634 | 312847.61 | 92.2% | PASS <=2.00x T1 |
| mesh | host_call_dispatch_overhead | n/a | 0.87 | n/a | PASS <=50ns |
| mesh | host_call_eager_decode | 3762 | 1538847.16 | 37.5% | MASKING >1.10x T1 |
| mesh | alternate_scalar_plan | 3796 | 1525045.48 | 37.9% | reported |
| mesh | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| mesh | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| mesh | cold_first_parse | 9091 | 636745.51 | 90.7% | PASS <=2.00x T1 |
| random | host_call_dispatch_overhead | n/a | 0.85 | n/a | PASS <=50ns |
| random | host_call_eager_decode | 2448 | 1668381.49 | 26.1% | MASKING >1.10x T1 |
| random | alternate_scalar_plan | 2606 | 1566860.19 | 27.8% | reported |
| random | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| random | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| random | cold_first_parse | 6893 | 592468.55 | 73.6% | PASS <=2.00x T1 |
| gsoc-2018 | host_call_dispatch_overhead | n/a | 0.85 | n/a | PASS <=50ns |
| gsoc-2018 | host_call_eager_decode | 5885 | 4523492.84 | 19.9% | MASKING >1.10x T1 |
| gsoc-2018 | alternate_scalar_plan | 10713 | 2485121.25 | 36.3% | reported |
| gsoc-2018 | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| gsoc-2018 | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| gsoc-2018 | cold_first_parse | 27372 | 972634.55 | 92.7% | PASS <=2.00x T1 |
| marine_ik | host_call_dispatch_overhead | n/a | 0.80 | n/a | PASS <=50ns |
| marine_ik | host_call_eager_decode | 1599 | 14923572.39 | 17.7% | MASKING >1.10x T1 |
| marine_ik | alternate_scalar_plan | 2725 | 8757968.33 | 30.2% | reported |
| marine_ik | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| marine_ik | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| marine_ik | cold_first_parse | 9084 | 2627357.33 | 100.6% | PASS <=2.00x T1 |
| instruments | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| instruments | host_call_eager_decode | 5684 | 310128.95 | 35.3% | MASKING >1.10x T1 |
| instruments | alternate_scalar_plan | 5059 | 348428.18 | 31.4% | reported |
| instruments | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| instruments | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| instruments | cold_first_parse | 18796 | 93786.62 | 116.7% | PASS <=2.00x T1 |
| numbers | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| numbers | host_call_eager_decode | 8548 | 140502.41 | 52.5% | MASKING >1.10x T1 |
| numbers | alternate_scalar_plan | 5845 | 205475.24 | 35.9% | reported |
| numbers | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| numbers | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| numbers | cold_first_parse | 15736 | 76319.85 | 96.6% | PASS <=2.00x T1 |
| unicode_mixed | host_call_dispatch_overhead | n/a | 0.73 | n/a | PASS <=50ns |
| unicode_mixed | host_call_eager_decode | 3420 | 2463498.08 | 23.7% | MASKING >1.10x T1 |
| unicode_mixed | alternate_scalar_plan | 4444 | 1895534.59 | 30.8% | reported |
| unicode_mixed | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_mixed | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_mixed | cold_first_parse | 9408 | 895503.41 | 65.2% | PASS <=2.00x T1 |
| unicode_escapes | host_call_dispatch_overhead | n/a | 0.70 | n/a | PASS <=50ns |
| unicode_escapes | host_call_eager_decode | 2388 | 3519771.87 | 48.5% | MASKING >1.10x T1 |
| unicode_escapes | alternate_scalar_plan | 5194 | 1618527.36 | 105.4% | reported |
| unicode_escapes | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_escapes | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_escapes | cold_first_parse | 4942 | 1700905.12 | 100.3% | PASS <=2.00x T1 |
| unicode_basic | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| unicode_basic | host_call_eager_decode | 4019 | 2087282.62 | 27.7% | MASKING >1.10x T1 |
| unicode_basic | alternate_scalar_plan | 3733 | 2247435.23 | 25.7% | reported |
| unicode_basic | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| unicode_basic | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| unicode_basic | cold_first_parse | 8905 | 941980.44 | 61.3% | PASS <=2.00x T1 |
| distinct_values | host_call_dispatch_overhead | n/a | 0.69 | n/a | PASS <=50ns |
| distinct_values | host_call_eager_decode | 5740 | 214127.15 | 35.5% | MASKING >1.10x T1 |
| distinct_values | alternate_scalar_plan | 4158 | 295598.61 | 25.7% | reported |
| distinct_values | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| distinct_values | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| distinct_values | cold_first_parse | 15939 | 77110.71 | 98.6% | PASS <=2.00x T1 |
| y_string_unicode | host_call_dispatch_overhead | n/a | 0.71 | n/a | PASS <=50ns |
| y_string_unicode | host_call_eager_decode | 2651 | 107420.43 | 36.5% | MASKING >1.10x T1 |
| y_string_unicode | alternate_scalar_plan | 4932 | 57748.43 | 68.0% | reported |
| y_string_unicode | alternate_dispatch_table_plan | n/a | n/a | n/a | INVALID duplicate-probe disabled; real function-pointer table regressed |
| y_string_unicode | alternate_pext_mask_plan | n/a | n/a | n/a | missing |
| y_string_unicode | cold_first_parse | 6487 | 43906.69 | 89.4% | PASS <=2.00x T1 |

## Notes

- twitter payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- twitter lazy tape materialization: 73362 offsets, 293448 logical offset bytes + 1560 sparse flag bytes (0.47x input), 295008 allocated tape bytes (0.47x input), 0 payload bytes; object opens 1264, array opens 1050, closes 2314, string quotes 36198, numbers 2109, literals 4737, separators 25690.
- twitter peak RSS subprocess probes: bbnf=3244032 bytes, S anchor sonic-rs=4800512 bytes.
- citm_catalog payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- citm_catalog lazy tape materialization: 162594 offsets, 650376 logical offset bytes + 5 sparse flag bytes (0.38x input), 650381 allocated tape bytes (0.38x input), 0 payload bytes; object opens 10937, array opens 10451, closes 21388, string quotes 53208, numbers 14392, literals 1263, separators 50955.
- citm_catalog peak RSS subprocess probes: bbnf=4702208 bytes, S anchor sonic-rs=7618560 bytes.
- canada payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- canada lazy tape materialization: 334385 offsets, 1337540 logical offset bytes + 0 sparse flag bytes (0.59x input), 1337540 allocated tape bytes (0.59x input), 0 payload bytes; object opens 4, array opens 56045, closes 56049, string quotes 24, numbers 111126, literals 0, separators 111137.
- canada peak RSS subprocess probes: bbnf=5914624 bytes, S anchor sonic-rs=11206656 bytes.
- canada structural scan: 66957 Mbps; floor is 40000 Mbps.
- apache_builds payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- apache_builds lazy tape materialization: 17653 offsets, 70612 logical offset bytes + 5 sparse flag bytes (0.55x input), 70617 allocated tape bytes (0.55x input), 0 payload bytes; object opens 884, array opens 3, closes 887, string quotes 10578, numbers 2, literals 3, separators 5296.
- apache_builds peak RSS subprocess probes: bbnf=2473984 bytes, S anchor simd-json borrowed=3080192 bytes.
- github_events payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- github_events lazy tape materialization: 6547 offsets, 26188 logical offset bytes + 25 sparse flag bytes (0.40x input), 26213 allocated tape bytes (0.40x input), 0 payload bytes; object opens 180, array opens 19, closes 199, string quotes 3782, numbers 149, literals 88, separators 2130.
- github_events peak RSS subprocess probes: bbnf=2375680 bytes, S anchor sonic-rs=2916352 bytes.
- update_center payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- update_center lazy tape materialization: 90648 offsets, 362592 logical offset bytes + 1045 sparse flag bytes (0.68x input), 363637 allocated tape bytes (0.68x input), 0 payload bytes; object opens 1896, array opens 1937, closes 3833, string quotes 54458, numbers 0, literals 386, separators 28138.
- update_center peak RSS subprocess probes: bbnf=3194880 bytes, S anchor sonic-rs=4358144 bytes.
- mesh payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- mesh lazy tape materialization: 153285 offsets, 613140 logical offset bytes + 0 sparse flag bytes (0.85x input), 613140 allocated tape bytes (0.85x input), 0 payload bytes; object opens 3, array opens 3610, closes 3613, string quotes 22, numbers 73013, literals 0, separators 73024.
- mesh peak RSS subprocess probes: bbnf=3620864 bytes, S anchor sonic-rs=6012928 bytes.
- random payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- random lazy tape materialization: 121022 offsets, 484088 logical offset bytes + 0 sparse flag bytes (0.95x input), 484088 allocated tape bytes (0.95x input), 0 payload bytes; object opens 4001, array opens 1001, closes 5002, string quotes 66010, numbers 5002, literals 1000, separators 39006.
- random peak RSS subprocess probes: bbnf=3293184 bytes, S anchor sonic-rs=4587520 bytes.
- gsoc-2018 payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- gsoc-2018 lazy tape materialization: 109969 offsets, 439876 logical offset bytes + 8545 sparse flag bytes (0.13x input), 448421 allocated tape bytes (0.13x input), 0 payload bytes; object opens 3793, array opens 0, closes 3793, string quotes 68256, numbers 0, literals 0, separators 34127.
- gsoc-2018 peak RSS subprocess probes: bbnf=6537216 bytes, S anchor sonic-rs=10027008 bytes.
- marine_ik payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- marine_ik lazy tape materialization: 681280 offsets, 2725120 logical offset bytes + 0 sparse flag bytes (0.91x input), 2725120 allocated tape bytes (0.91x input), 0 payload bytes; object opens 9680, array opens 28377, closes 38057, string quotes 76536, numbers 245175, literals 6, separators 283449.
- marine_ik peak RSS subprocess probes: bbnf=10076160 bytes, S anchor sonic-rs=16089088 bytes.
- instruments payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- instruments lazy tape materialization: 34062 offsets, 136248 logical offset bytes + 0 sparse flag bytes (0.62x input), 136248 allocated tape bytes (0.62x input), 0 payload bytes; object opens 1012, array opens 194, closes 1206, string quotes 13778, numbers 4935, literals 557, separators 12380.
- instruments peak RSS subprocess probes: bbnf=2654208 bytes, S anchor sonic-rs=3473408 bytes.
- numbers payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- numbers lazy tape materialization: 20003 offsets, 80012 logical offset bytes + 0 sparse flag bytes (0.53x input), 80012 allocated tape bytes (0.53x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 0, numbers 10001, literals 0, separators 10000.
- numbers peak RSS subprocess probes: bbnf=2506752 bytes, S anchor sonic-rs=3080192 bytes.
- unicode_mixed payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_mixed lazy tape materialization: 100482 offsets, 401928 logical offset bytes + 9795 sparse flag bytes (0.39x input), 411723 allocated tape bytes (0.39x input), 0 payload bytes; object opens 4187, array opens 2, closes 4189, string quotes 50242, numbers 8371, literals 0, separators 33491.
- unicode_mixed peak RSS subprocess probes: bbnf=3751936 bytes, S anchor sonic-rs=5505024 bytes.
- unicode_escapes payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_escapes lazy tape materialization: 24423 offsets, 97692 logical offset bytes + 9385 sparse flag bytes (0.10x input), 107077 allocated tape bytes (0.10x input), 0 payload bytes; object opens 1879, array opens 1, closes 1880, string quotes 11272, numbers 1877, literals 1, separators 7513.
- unicode_escapes peak RSS subprocess probes: bbnf=3440640 bytes, S anchor sonic-rs=4997120 bytes.
- unicode_basic payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- unicode_basic lazy tape materialization: 218843 offsets, 875372 logical offset bytes + 0 sparse flag bytes (0.83x input), 875372 allocated tape bytes (0.83x input), 0 payload bytes; object opens 5759, array opens 5760, closes 11519, string quotes 115180, numbers 11518, literals 0, separators 69107.
- unicode_basic peak RSS subprocess probes: bbnf=4227072 bytes, S anchor sonic-rs=6307840 bytes.
- distinct_values payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- distinct_values lazy tape materialization: 31149 offsets, 124596 logical offset bytes + 0 sparse flag bytes (0.81x input), 124596 allocated tape bytes (0.81x input), 0 payload bytes; object opens 440, array opens 1, closes 441, string quotes 19592, numbers 440, literals 0, separators 10235.
- distinct_values peak RSS subprocess probes: bbnf=2555904 bytes, S anchor sonic-rs=3063808 bytes.
- y_string_unicode payload arena counters: Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations.
- y_string_unicode lazy tape materialization: 6601 offsets, 26404 logical offset bytes + 9000 sparse flag bytes (0.99x input), 35404 allocated tape bytes (0.99x input), 0 payload bytes; object opens 0, array opens 1, closes 1, string quotes 4400, numbers 0, literals 0, separators 2199.
- y_string_unicode peak RSS subprocess probes: bbnf=2375680 bytes, S anchor sonic-rs=2588672 bytes.
- Overall outcome G / NoGo.
- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape.
- Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse.

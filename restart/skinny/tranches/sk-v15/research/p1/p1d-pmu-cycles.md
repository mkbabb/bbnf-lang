# SK-V15 P1-D: PMU cycles-per-byte ledger

Pass: S-P1 Profile. Cycle: V15.
Scope: cycles, instructions, c/B, CPI, Mbps for 17 corpora x 12 `profile_direct` modes.
Verdict: CURRENT-PMU for cycles/instructions; branch/L1/LLC unavailable on this macOS source and explicitly absent.

## Section 1 - Method
Baseline: SK-V15 S-P0 head `279a60646` (`279a606466c60172932629dd9788cd80d6bc82b0`).
Clean source worktree: `/Users/mkbabb/Programming/bbnf-lang-skv15-profile-279a60646`; `git status --short` was empty after removing extractor pycache. The dirty main checkout was not profiled.
Build flags: `CARGO_TARGET_DIR=/tmp/skv15-p1-target RUSTFLAGS='-C target-cpu=native' cargo build --release`; `skinny/Cargo.toml` release and bench profiles carry `debug = true`.
Profile tool: `samply 0.13.1`, interactive `samply record --no-open --rate 4000 --unstable-presymbolicate`; the wrapper terminated only after samply printed the local-server marker and had written `.json.gz` plus `.json.syms.json`.
PMU source: `proc_pid_rusage(RUSAGE_INFO_V5)` via `profile_direct`; cycles and instructions are real. Branch, L1, and LLC counters are not exposed by this macOS source and are recorded absent rather than fabricated.
Command shape: `/tmp/skv15-p1-target/release/profile_direct ${iters} ${corpus} ${mode}`. The Track 2 / sonic / serde comparator sweep reused the Track 1 per-corpus iteration counts captured in P1-A/P1-B.

## Section 2 - Findings
| Corpus | Plane | Track1 c/B | Track2 c/B | sonic c/B | serde c/B | Track1 Mbps | Track1 c/B / best strict comparator |
|---|---|---:|---:|---:|---:|---:|---:|
| twitter | parse_only | 1.354 | 2.632 | 2.241 | 10.835 | 19467.189 | 0.604 |
| twitter | direct_strict | 1.666 | 2.238 | 2.566 | 2.564 | 13102.278 | 0.650 |
| twitter | real_typed | 1.735 | 2.201 | 2.531 | 2.516 | 12553.593 | 0.690 |
| citm_catalog | parse_only | 1.010 | 2.200 | 1.701 | 7.786 | 23838.822 | 0.594 |
| citm_catalog | direct_strict | 0.986 | 1.881 | 1.789 | 2.198 | 22037.219 | 0.551 |
| citm_catalog | real_typed | 0.991 | 1.914 | 1.758 | 2.209 | 22010.962 | 0.564 |
| canada | parse_only | 1.777 | 4.336 | 2.711 | 7.047 | 13532.685 | 0.655 |
| canada | direct_strict | 7.418 | 11.630 | 12.425 | 9.537 | 2917.407 | 0.778 |
| canada | real_typed | 7.505 | 10.237 | 12.484 | 9.496 | 2809.262 | 0.790 |
| apache_builds | parse_only | 1.730 | 2.802 | 2.633 | 8.434 | 12554.853 | 0.657 |
| apache_builds | direct_strict | 3.691 | 6.784 | 6.124 | 7.805 | 4872.487 | 0.603 |
| apache_builds | real_typed | 3.821 | 6.469 | 6.672 | 7.793 | 5539.654 | 0.573 |
| github_events | parse_only | 1.226 | 2.278 | 2.050 | 9.164 | 14243.175 | 0.598 |
| github_events | direct_strict | 2.678 | 3.189 | 3.586 | 3.650 | 5500.915 | 0.747 |
| github_events | real_typed | 2.480 | 3.169 | 3.346 | 3.397 | 7904.561 | 0.741 |
| update_center | parse_only | 2.117 | 3.876 | 2.383 | 14.997 | 6465.318 | 0.888 |
| update_center | direct_strict | 3.248 | 4.005 | 3.445 | 4.637 | 3410.422 | 0.943 |
| update_center | real_typed | 2.417 | 3.945 | 3.489 | 4.732 | 8446.744 | 0.693 |
| mesh | parse_only | 2.824 | 4.966 | 4.705 | 8.023 | 3410.056 | 0.600 |
| mesh | direct_strict | 5.089 | 4.756 | 4.655 | 4.842 | 2172.739 | 1.093 |
| mesh | real_typed | 3.818 | 4.773 | 4.104 | 4.728 | 5108.527 | 0.930 |
| random | parse_only | 3.041 | 4.591 | 3.125 | 16.951 | 2580.259 | 0.973 |
| random | direct_strict | 5.426 | 8.203 | 7.495 | 9.601 | 2783.385 | 0.724 |
| random | real_typed | 4.256 | 9.432 | 7.433 | 9.568 | 4792.921 | 0.573 |
| gsoc-2018 | parse_only | 0.939 | 1.261 | 1.064 | 3.927 | 7448.408 | 0.883 |
| gsoc-2018 | direct_strict | 4.828 | 5.633 | 5.434 | 6.055 | 4752.896 | 0.888 |
| gsoc-2018 | real_typed | 4.795 | 6.355 | 5.499 | 6.040 | 5577.610 | 0.872 |
| marine_ik | parse_only | 2.744 | 5.087 | 4.672 | 10.451 | 2402.829 | 0.587 |
| marine_ik | direct_strict | 2.874 | 3.577 | 3.829 | 3.466 | 10025.424 | 0.829 |
| marine_ik | real_typed | 2.923 | 4.165 | 3.826 | 3.451 | 10822.175 | 0.847 |
| instruments | parse_only | 1.819 | 3.055 | 2.250 | 9.774 | 4481.035 | 0.808 |
| instruments | direct_strict | 1.811 | 3.039 | 2.297 | 3.013 | 16551.591 | 0.788 |
| instruments | real_typed | 1.743 | 3.840 | 2.289 | 3.055 | 18644.844 | 0.762 |
| numbers | parse_only | 2.044 | 2.996 | 3.012 | 4.909 | 3847.094 | 0.679 |
| numbers | direct_strict | 2.682 | 3.670 | 2.950 | 3.910 | 11015.826 | 0.909 |
| numbers | real_typed | 2.680 | 5.248 | 3.375 | 4.873 | 12172.369 | 0.794 |
| unicode_mixed | parse_only | 3.552 | 4.278 | 4.332 | 13.424 | 2667.973 | 0.820 |
| unicode_mixed | direct_strict | 5.416 | 10.386 | 7.153 | 11.307 | 5575.199 | 0.757 |
| unicode_mixed | real_typed | 5.419 | 11.994 | 7.131 | 11.249 | 5816.825 | 0.760 |
| unicode_escapes | parse_only | 3.265 | 2.963 | 9.758 | 8.863 | 4289.831 | 0.368 |
| unicode_escapes | direct_strict | 12.169 | 8.113 | 16.115 | 8.682 | 2337.701 | 1.402 |
| unicode_escapes | real_typed | 12.095 | 8.288 | 16.003 | 8.674 | 2594.865 | 1.394 |
| unicode_basic | parse_only | 2.154 | 4.319 | 3.449 | 14.924 | 8409.267 | 0.624 |
| unicode_basic | direct_strict | 5.959 | 10.203 | 7.094 | 9.778 | 2783.946 | 0.840 |
| unicode_basic | real_typed | 5.330 | 10.291 | 7.106 | 9.771 | 6147.251 | 0.750 |
| distinct_values | parse_only | 2.105 | 3.840 | 2.792 | 14.185 | 10080.549 | 0.754 |
| distinct_values | direct_strict | 5.828 | 10.878 | 8.646 | 10.233 | 1468.197 | 0.674 |
| distinct_values | real_typed | 3.623 | 10.606 | 8.588 | 10.061 | 8885.646 | 0.422 |
| y_string_unicode | parse_only | 3.085 | 5.041 | 8.740 | 8.953 | 10413.856 | 0.353 |
| y_string_unicode | direct_strict | 2.941 | 4.620 | 4.039 | 4.498 | 5357.453 | 0.728 |
| y_string_unicode | real_typed | 2.953 | 4.597 | 4.029 | 4.476 | 11006.470 | 0.733 |

Summary: parse_only Track 1 beats the best strict comparator on 17/17 c/B rows. direct_strict Track 1 misses on `mesh` and `unicode_escapes`; real_typed Track 1 misses on `unicode_escapes`.

## Section 3 - Delta
The PMU ledger is new SK-V15 S-P1 evidence. It does not mutate admission rows; it supplies S-P2 with an empirical floor and exposes c/B misses not visible from `skinny/RESULTS.md` Mbps alone.

## Section 4 - Anomalies
- `xcrun xctrace list instruments` exposes CPU Profiler and Time Profiler but no branch/L1/LLC counter instrument in this environment.
- `proc_pid_rusage(RUSAGE_INFO_V5)` exposes cycles and instructions only; branch-misses, L1 misses, and LLC misses remain `absent:macos-counter-source` in this pass.
- `skinny/RESULTS.md` still has `hot-leaf=not-collected` cells; P1-D supplies PMU rows but does not patch RESULTS.

## Section 5 - Sources
- `restart/skinny/tranches/sk-v15/research/p1/evidence/pmu-probe-results.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/pmu-cpb-summary.tsv`.

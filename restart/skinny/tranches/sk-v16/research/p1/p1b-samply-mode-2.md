# SK-V16 P1-B: Samply Mode II Product Plane

Pass: S-P1 Profile. Cycle: V16.
Date: 2026-05-28.
Scope: cold per-parse `direct_strict_track1` and `real_typed_track1`.
Output: this file.
Baseline: SK-V16-open (`5ed43f8e1` profiling-tool anchor; baseline docs at `dafe288dd`).
Host triple: `aarch64-apple-darwin`.
Build flags: release profile with debuginfo; `warmup_iters=0`.
Profile tool: `/Users/mkbabb/.cargo/bin/samply`; offline symbols via `atos -inlineFrames`.
Corpus coverage: 17/17 for each mode.

## Section 1 - Method

```sh
cd /Users/mkbabb/Programming/bbnf-skv16-p1/skinny
cargo build --release -p bbnf-bench --bin profile_direct
./target/release/profile_direct 500 <corpus> direct_strict_track1 0
./target/release/profile_direct 500 <corpus> real_typed_track1 0
samply record --no-open --duration 3 -o /tmp/skv16-p1/profiles/<corpus>-direct_strict_track1.json.gz -- \
  ./target/release/profile_direct <iters> <corpus> direct_strict_track1 0
samply record --no-open --duration 3 -o /tmp/skv16-p1/profiles/<corpus>-real_typed_track1.json.gz -- \
  ./target/release/profile_direct <iters> <corpus> real_typed_track1 0
```

Run ledgers:

- `/tmp/skv16-p1/probe-results.tsv`
- `/tmp/skv16-p1/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1/probe-deltas.tsv`

## Section 2 - Findings

Direct and typed profiles have the same shape. Typed is effectively parity
with direct: mean typed delta `+0.69%`, median `+0.45%`, typed faster on
10/17 and slower on 7/17. That rules out a broad typed-vs-direct split as a
first SK-V16 primitive.

| Corpus | Direct Mbps | Typed Mbps | Typed vs direct | Direct c/B | Typed c/B |
|---|---:|---:|---:|---:|---:|
| apache_builds | 9898.787 | 9943.125 | +0.45% | 3.476145 | 3.459651 |
| canada | 4979.517 | 4930.202 | -0.99% | 6.900380 | 6.964183 |
| citm_catalog | 36024.578 | 35917.373 | -0.30% | 0.954977 | 0.958241 |
| distinct_values | 7170.135 | 7429.695 | +3.62% | 4.340925 | 4.213186 |
| github_events | 14289.928 | 14688.633 | +2.79% | 2.405495 | 2.338974 |
| gsoc-2018 | 7314.950 | 7302.806 | -0.17% | 4.697051 | 4.699367 |
| instruments | 19810.296 | 19766.645 | -0.22% | 1.736449 | 1.741356 |
| marine_ik | 12036.764 | 12094.172 | +0.48% | 2.855974 | 2.827112 |
| mesh | 9034.150 | 9289.901 | +2.83% | 3.798336 | 3.696734 |
| numbers | 12766.653 | 12565.460 | -1.58% | 2.693404 | 2.737288 |
| random | 8511.100 | 8456.738 | -0.64% | 4.041160 | 4.059692 |
| twitter | 20978.256 | 21038.892 | +0.29% | 1.631427 | 1.629382 |
| unicode_basic | 6154.297 | 6464.676 | +5.04% | 5.302382 | 5.269717 |
| unicode_escapes | 2746.357 | 2818.360 | +2.62% | 11.611600 | 11.597315 |
| unicode_mixed | 6047.748 | 5795.830 | -4.17% | 5.675189 | 5.816435 |
| update_center | 14560.262 | 14678.681 | +0.81% | 2.353729 | 2.340071 |
| y_string_unicode | 10652.205 | 10748.181 | +0.90% | 2.867622 | 2.946133 |

Representative hot leaves:

- `unicode_escapes`: generated typed Unicode document parsing and raw string hashing dominate.
- `gsoc-2018`: generated string-enum folding and proposal parsing dominate.
- `unicode_mixed`: generated decoded string facts and mixed document parsing dominate.
- `canada`, `apache_builds`, `unicode_basic`, `numbers`: top leaves include `typed_checksum`; this is harness validation cost, not parser work.

Full per-corpus top-20 symbol tables are in `/tmp/skv16-p1/samply-profile-top20-inline.tsv`.

## Section 3 - Delta Vs SK-V15

All 34 product-plane JSON rows remain `AUDIT-SUSTAINED`; no admission delta is
claimed. The worst product-plane c/B rows are:

| Corpus | Mode | c/B | Mbps |
|---|---|---:|---:|
| unicode_escapes | direct_strict_track1 | 11.611600 | 2746.357 |
| unicode_escapes | real_typed_track1 | 11.597315 | 2818.360 |
| canada | real_typed_track1 | 6.964183 | 4930.202 |
| canada | direct_strict_track1 | 6.900380 | 4979.517 |
| unicode_mixed | real_typed_track1 | 5.816435 | 5795.830 |
| unicode_mixed | direct_strict_track1 | 5.675189 | 6047.748 |

## Section 4 - Anomalies And Masking Signals

Checksum and product-validation leaves are real cost in the measured product
workload, but they are not parser primitives. S-P2 must not scope runtime
primitives from `typed_checksum` alone. It may scope parser work from generated
field dispatch, string decode, string enum fold, skip paths, vector fill, and
raw string validation where those leaves appear under generated parse frames.

## Section 5 - Sources

- `/tmp/skv16-p1/probe-results.tsv`
- `/tmp/skv16-p1/probe-deltas.tsv`
- `/tmp/skv16-p1/samply-artifacts.tsv`
- `/tmp/skv16-p1/samply-profile-top20-inline.tsv`
- `/tmp/skv16-p1/samply-mode-top20-inline.tsv`

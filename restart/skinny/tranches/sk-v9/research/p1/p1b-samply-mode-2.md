# SK-V9 P1-B: Samply Mode II Direct And Real-Typed Profile

Pass: S-P1 Profile. Cycle: V2 post-W0 rerun.
Date: 2026-05-18.
Scope: `direct_to_struct` Track 1 for all 17 corpora plus W0-admitted
`real_typed_struct` Track 1 rows.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, `debug=true`,
`strip=false`, `split-debuginfo=packed`.
Profile tool: `samply 0.13.1`, interactive `record --no-open`,
`--unstable-presymbolicate`, 4000 Hz, main-thread-only.
Corpus coverage: `direct_to_struct` 17/17; measured `real_typed_struct` 4/4.

## §1 - Method

Commands:

```bash
export OUT=/tmp/skv9-p1-rerun
export CARGO_TARGET_DIR="$OUT/target"
export RUSTFLAGS="-C target-cpu=native"
cargo build --release -p bbnf-bench --bin profile_direct
samply record --no-open --rate 4000 --main-thread-only \
  --unstable-presymbolicate \
  -o "$OUT/profiles/p1b/<corpus>.direct_to_struct.track1.profile.json.gz" \
  "$OUT/target/release/profile_direct" <iters> <corpus-or-test_data-path> track1
samply record --no-open --rate 4000 --main-thread-only \
  --unstable-presymbolicate \
  -o "$OUT/profiles/p1b/<corpus>.real_typed_struct.track1.profile.json.gz" \
  "$OUT/target/release/profile_direct" <iters> <corpus> real_typed_track1
```

`update_center/direct_to_struct` was rerun with `test_data/update-center.json`
because the direct profiling binary does not map that underscore name in
non-real-typed mode. The four real-typed rows are the only W0 measured typed
rows: `twitter`, `update_center`, `mesh`, and `marine_ik`.

## §2 - Findings

### Direct Rows

| Corpus | Samples | Top self-time symbol |
|---|---:|---|
| `twitter` | 6402 | 72.4% `parse_object_value_at_direct` at `generated.rs:468` |
| `citm_catalog` | 6238 | 55.5% `parse_array_element_at_direct` at `generated.rs:508` |
| `canada` | 6066 | 87.5% `parse_array_element_at_direct` at `generated.rs:508` |
| `apache_builds` | 6117 | 38.1% `parse_object_value_at_direct` at `generated.rs:468`; 32.7% `JsonDigestSink::array_string` at `direct_struct.rs:124` |
| `github_events` | 6256 | 72.8% `parse_object_value_at_direct` at `generated.rs:468` |
| `update_center` | 24038 | 68.3% `parse_object_value_at_direct` at `generated.rs:468` |
| `mesh` | 6769 | 75.9% `parse_array_element_at_direct` at `generated.rs:508` |
| `random` | 6493 | 39.5% `parse_object_value_at_direct`; 37.2% `parse_array_element_at_direct` |
| `gsoc-2018` | 6545 | 61.7% `parse_object_value_at_direct` at `generated.rs:468` |
| `marine_ik` | 6259 | 73.6% `parse_array_element_at_direct` at `generated.rs:508` |
| `instruments` | 6250 | 59.3% `parse_array_element_at_direct` at `generated.rs:508` |
| `numbers` | 6412 | 77.7% `parse_array_element_at_direct` at `generated.rs:508` |
| `unicode_mixed` | 6873 | 54.4% `parse_object_value_at_direct` at `generated.rs:468` |
| `unicode_escapes` | 6631 | 47.5% `parse_that_regex::unescape_string` at `parse-that-regex/src/lib.rs:718` |
| `unicode_basic` | 6095 | 44.6% `parse_object_value_at_direct` at `generated.rs:468` |
| `distinct_values` | 6084 | 50.1% `parse_array_element_at_direct` at `generated.rs:508` |
| `y_string_unicode` | 9170 | no single parser leaf dominates; `mach_absolute_time` is the top sampled leaf at 23.3% |

### Real-Typed Rows

| Corpus | Samples | Top self-time symbol |
|---|---:|---|
| `twitter` | 5775 | 74.0% `DirectParser::skip_value` at `generated_real_typed.rs:1273` |
| `update_center` | 6284 | 47.5% `parse_type_plugin` at `generated_real_typed.rs:439` |
| `mesh` | 7138 | 44.6% `parse_type_mesh`; 27.7% `parse_vec_cap_10800_scalar_f64` |
| `marine_ik` | 6800 | 41.0% `DirectParser::skip_value`; 40.1% `parse_type_marine_geometry_data` |

Full top-20 self-time data is in `/tmp/skv9-p1-rerun/profile-summary.json`.

## §3 - Delta vs SK-V8

P1-F owns the row delta ledger. P1-B does not admit new direct or typed rows:
Apache/CITM/Canada measured typed rows remain absent, and direct rows clamped by
W0 remain `N-direct / NO-GO`.

## §4 - Anomalies + Masking Signals

- `unicode_escapes/direct_to_struct` is the clearest string/unescape direct-row
  hotspot.
- Number-heavy direct rows concentrate in `parse_array_element_at_direct` plus
  `materialize_f64`/`materialize_u64`.
- Real-typed measured rows reveal generated typed skip/projection costs, but
  those four rows do not authorize Apache/CITM/Canada typed admissions.
- P1-D PMU absence blocks S-P1 convergence.

## §5 - Sources

- `/tmp/skv9-p1-rerun/profiles/p1b/*.profile.json.gz`
- `/tmp/skv9-p1-rerun/profiles/p1b/*.profile.json.syms.json`
- `/tmp/skv9-p1-rerun/logs/p1b.*.log`
- `/tmp/skv9-p1-rerun/profile-summary.json`

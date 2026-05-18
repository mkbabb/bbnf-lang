# SK-V9 P1-A: Samply Mode I Parse-Only Profile

Pass: S-P1 Profile. Cycle: V2 post-W0 rerun.
Date: 2026-05-18.
Scope: `parse_only` Track 1 generated-runtime profile over all 17 JSON corpora.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, `debug=true`,
`strip=false`, `split-debuginfo=packed`.
Profile tool: `samply 0.13.1`, interactive `record --no-open`,
`--unstable-presymbolicate`, 4000 Hz, main-thread-only.
Corpus coverage: 17/17.

## §1 - Method

Commands:

```bash
export OUT=/tmp/skv9-p1-rerun
export CARGO_TARGET_DIR="$OUT/target"
export RUSTFLAGS="-C target-cpu=native"
cargo build --release -p xtask --bin profile-lazy
samply record --no-open --rate 4000 --main-thread-only \
  --unstable-presymbolicate \
  -o "$OUT/profiles/p1a/<corpus>.parse_only.track1_generated.profile.json.gz" \
  "$OUT/target/release/profile-lazy" <iters> <corpus-or-test_data-path>
python3 <profile-summary extractor>
```

Name-resolution correction: `profile-lazy` resolves only the three
`crates/test-fixtures/corpus/json` fixtures by corpus name. The other 14 rows
were rerun with explicit `test_data/*.json` paths; panic profiles were discarded.

All 17 final profiles have both `.profile.json.gz` and `.profile.json.syms.json`.
Sample counts range from 5,999 to 45,504.

## §2 - Findings

Every parse-only row resolves to the same fused generated parser leaf:
`runtime::generated_json::generated::dispatch_value` at
`skinny/crates/runtime/src/grammars/json/generated.rs:47`. This is a measured
hot leaf, not a Criterion placeholder.

| Corpus | Samples | Top self-time symbol | Artifact |
|---|---:|---|---|
| `twitter` | 5999 | 98.8% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/twitter.parse_only.track1_generated.profile.json.gz` |
| `citm_catalog` | 6641 | 98.9% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/citm_catalog.parse_only.track1_generated.profile.json.gz` |
| `canada` | 6199 | 99.6% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/canada.parse_only.track1_generated.profile.json.gz` |
| `apache_builds` | 21803 | 98.6% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/apache_builds.parse_only.track1_generated.profile.json.gz` |
| `github_events` | 9200 | 98.2% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/github_events.parse_only.track1_generated.profile.json.gz` |
| `update_center` | 20179 | 99.0% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/update_center.parse_only.track1_generated.profile.json.gz` |
| `mesh` | 17460 | 99.3% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/mesh.parse_only.track1_generated.profile.json.gz` |
| `random` | 24384 | 99.3% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/random.parse_only.track1_generated.profile.json.gz` |
| `gsoc-2018` | 16235 | 99.1% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/gsoc-2018.parse_only.track1_generated.profile.json.gz` |
| `marine_ik` | 25951 | 99.4% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/marine_ik.parse_only.track1_generated.profile.json.gz` |
| `instruments` | 21333 | 98.0% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/instruments.parse_only.track1_generated.profile.json.gz` |
| `numbers` | 21229 | 96.7% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/numbers.parse_only.track1_generated.profile.json.gz` |
| `unicode_mixed` | 45504 | 99.2% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/unicode_mixed.parse_only.track1_generated.profile.json.gz` |
| `unicode_escapes` | 32047 | 99.0% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/unicode_escapes.parse_only.track1_generated.profile.json.gz` |
| `unicode_basic` | 37858 | 99.0% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/unicode_basic.parse_only.track1_generated.profile.json.gz` |
| `distinct_values` | 35234 | 98.3% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/distinct_values.parse_only.track1_generated.profile.json.gz` |
| `y_string_unicode` | 26675 | 95.6% `dispatch_value` at `generated.rs:47` | `/tmp/skv9-p1-rerun/profiles/p1a/y_string_unicode.parse_only.track1_generated.profile.json.gz` |

Full top-20 self-time data for every profile is in
`/tmp/skv9-p1-rerun/profile-summary.json`; compact top-5 rows are in
`/tmp/skv9-p1-rerun/profile-summary-top5.md`.

## §3 - Delta vs SK-V8

Delta rows are extracted in `p1f-results-delta.md`. P1-A itself does not convert
`ns/B` to cycles-per-byte because P1-D could not collect real PMU counters.

## §4 - Anomalies + Masking Signals

- The parse-only hot leaf is grammar-neutral generated dispatch, not a
  JSON-policy proposal.
- The fused symbol does not by itself authorize a primitive. S-P2 must not infer
  scan/string/number sub-leaves without a no-inline or PC-level split.
- P1-D PMU absence blocks S-P1 convergence even though P1-A coverage is complete.

## §5 - Sources

- `/tmp/skv9-p1-rerun/profiles/p1a/*.profile.json.gz`
- `/tmp/skv9-p1-rerun/profiles/p1a/*.profile.json.syms.json`
- `/tmp/skv9-p1-rerun/logs/p1a.*.log`
- `/tmp/skv9-p1-rerun/profile-summary.json`
- `skinny/RESULTS.md` run id `sk-v9-open:criterion-fnv64-cd1673844eeea12f`

# SK-V15 P1-A: samply profile - mode I (parse_only)

Pass: S-P1 Profile. Cycle: V15.
Scope: cold per-parse `parse_only_track1` profiling for all 17 JSON corpora.
Verdict: CURRENT-PROFILED. The earlier stale draft is superseded by interactive clean-worktree captures.

## Section 1 - Method
Baseline: SK-V15 S-P0 head `279a60646` (`279a606466c60172932629dd9788cd80d6bc82b0`).
Clean source worktree: `/Users/mkbabb/Programming/bbnf-lang-skv15-profile-279a60646`; `git status --short` was empty after removing extractor pycache. The dirty main checkout was not profiled.
Build flags: `CARGO_TARGET_DIR=/tmp/skv15-p1-target RUSTFLAGS='-C target-cpu=native' cargo build --release`; `skinny/Cargo.toml` release and bench profiles carry `debug = true`.
Profile tool: `samply 0.13.1`, interactive `samply record --no-open --rate 4000 --unstable-presymbolicate`; the wrapper terminated only after samply printed the local-server marker and had written `.json.gz` plus `.json.syms.json`.
PMU source: `proc_pid_rusage(RUSAGE_INFO_V5)` via `profile_direct`; cycles and instructions are real. Branch, L1, and LLC counters are not exposed by this macOS source and are recorded absent rather than fabricated.
Command shape: `samply record --no-open --rate 4000 --unstable-presymbolicate -o /tmp/skv15-p1/profiles-interactive/p1a/parse_only__${corpus}__track1.json.gz /tmp/skv15-p1-target/release/profile_direct ${iters} ${corpus} parse_only_track1`.
Full top-20 self-time rows: `restart/skinny/tranches/sk-v15/research/p1/evidence/p1ab-interactive-hotleaf-top20.tsv`. Full PMU rows: `restart/skinny/tranches/sk-v15/research/p1/evidence/pmu-probe-results.tsv`. Artifact existence is recorded in `restart/skinny/tranches/sk-v15/research/p1/evidence/artifact-manifest.tsv`.

## Section 2 - Findings
| Corpus | Profile artifact | Track1 Mbps | Track1 c/B | sonic c/B | serde c/B | first line-resolved hot leaf |
|---|---|---:|---:|---:|---:|---|
| twitter | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__twitter__track1.json.gz` | 19467.189 | 1.354 | 2.241 | 10.835 | rank 2 0.07% `profile_direct::run_once` `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:154` |
| citm_catalog | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__citm_catalog__track1.json.gz` | 23838.822 | 1.010 | 1.701 | 7.786 | rank 1 99.71% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1041` |
| canada | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__canada__track1.json.gz` | 13532.685 | 1.777 | 2.711 | 7.047 | rank 1 99.87% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1041` |
| apache_builds | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__apache_builds__track1.json.gz` | 12554.853 | 1.730 | 2.633 | 8.434 | rank 1 99.73% `parse_that_regex::skip_ascii_whitespace` `skinny/crates/parse-that-regex/src/lib.rs:121` |
| github_events | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__github_events__track1.json.gz` | 14243.175 | 1.226 | 2.050 | 9.164 | rank 1 99.43% `profile_direct::parse_only_checksum` `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:213` |
| update_center | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__update_center__track1.json.gz` | 6465.318 | 2.117 | 2.383 | 14.997 | rank 1 99.82% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1840` |
| mesh | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__mesh__track1.json.gz` | 3410.056 | 2.824 | 4.705 | 8.023 | rank 1 99.84% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1041` |
| random | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__random__track1.json.gz` | 2580.259 | 3.041 | 3.125 | 16.951 | rank 1 99.85% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1041` |
| gsoc-2018 | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__gsoc-2018__track1.json.gz` | 7448.408 | 0.939 | 1.064 | 3.927 | rank 1 99.81% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1840` |
| marine_ik | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__marine_ik__track1.json.gz` | 2402.829 | 2.744 | 4.672 | 10.451 | rank 1 99.83% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1041` |
| instruments | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__instruments__track1.json.gz` | 4481.035 | 1.819 | 2.250 | 9.774 | rank 1 99.92% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1840` |
| numbers | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__numbers__track1.json.gz` | 3847.094 | 2.044 | 3.012 | 4.909 | rank 1 99.66% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1041` |
| unicode_mixed | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__unicode_mixed__track1.json.gz` | 2667.973 | 3.552 | 4.332 | 13.424 | rank 2 0.03% `<usize>::wrapping_sub` `/rustc/02c7f9bec0fd583160f8bcccb830216023b07bee/library/core/src/num/uint_macros.rs:2548` |
| unicode_escapes | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__unicode_escapes__track1.json.gz` | 4289.831 | 3.265 | 9.758 | 8.863 | rank 6 0.02% `<alloc::raw_vec::RawVec<runtime::generated_json::generated::ParseOnly...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/raw_vec/mod.rs:186` |
| unicode_basic | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__unicode_basic__track1.json.gz` | 8409.267 | 2.154 | 3.449 | 14.924 | rank 1 99.78% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1840` |
| distinct_values | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__distinct_values__track1.json.gz` | 10080.549 | 2.105 | 2.792 | 14.185 | rank 1 99.95% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1840` |
| y_string_unicode | `/tmp/skv15-p1/profiles-interactive/p1a/parse_only__y_string_unicode__track1.json.gz` | 10413.856 | 3.085 | 8.740 | 8.953 | rank 1 99.81% `<alloc::vec::Vec<runtime::generated_json::generated::ParseOnlyFrame>>...` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec/mod.rs:1840` |

All 17 parse_only Track 1 rows beat the best strict comparator by cycles/byte in this fresh PMU sweep. Several samply top leaves are dominated by `profile_direct` checksum/frame-vector activity; P1-E classifies that as harness/tape attribution rather than a new parser primitive.

## Section 3 - Delta
`skinny/RESULTS.md` is byte-identical to SK-V14 close `8e7378025` and rolling source `bae430dcf`; this pass does not update admission rows. The fresh profile replaces stale hot-leaf absence only.

## Section 4 - Anomalies
- Branch/L1/LLC counters are absent on the available macOS PMU path; cycles and instructions are present for every row.
- Some top-ranked sidecar frames lack a line number. The table uses the first line-resolved row per corpus and the TSV preserves the raw rank order.
- Main checkout source remains dirty and was not profiled.

## Section 5 - Sources
- `restart/prompts/skinny/PASS-1-PROFILE.md`.
- `/tmp/skv15-p1/logs-interactive/p1a/*.log`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1ab-interactive-hotleaf-top20.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/artifact-manifest.tsv`.

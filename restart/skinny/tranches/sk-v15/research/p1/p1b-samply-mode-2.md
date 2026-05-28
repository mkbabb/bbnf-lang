# SK-V15 P1-B: samply profile - mode II (direct_to_struct + real_typed_struct)

Pass: S-P1 Profile. Cycle: V15.
Scope: cold per-parse `direct_strict_track{1,2}` and `real_typed_track{1,2}` profiling for all 17 JSON corpora.
Verdict: CURRENT-PROFILED with product-plane c/B gaps recorded.

## Section 1 - Method
Baseline: SK-V15 S-P0 head `279a60646` (`279a606466c60172932629dd9788cd80d6bc82b0`).
Clean source worktree: `/Users/mkbabb/Programming/bbnf-lang-skv15-profile-279a60646`; `git status --short` was empty after removing extractor pycache. The dirty main checkout was not profiled.
Build flags: `CARGO_TARGET_DIR=/tmp/skv15-p1-target RUSTFLAGS='-C target-cpu=native' cargo build --release`; `skinny/Cargo.toml` release and bench profiles carry `debug = true`.
Profile tool: `samply 0.13.1`, interactive `samply record --no-open --rate 4000 --unstable-presymbolicate`; the wrapper terminated only after samply printed the local-server marker and had written `.json.gz` plus `.json.syms.json`.
PMU source: `proc_pid_rusage(RUSAGE_INFO_V5)` via `profile_direct`; cycles and instructions are real. Branch, L1, and LLC counters are not exposed by this macOS source and are recorded absent rather than fabricated.
Command shape: `samply record --no-open --rate 4000 --unstable-presymbolicate -o /tmp/skv15-p1/profiles-interactive/p1b/${mode}__${corpus}.json.gz /tmp/skv15-p1-target/release/profile_direct ${iters} ${corpus} ${mode}`.
Full top-20 rows are in `evidence/p1ab-interactive-hotleaf-top20.tsv`; all 68 mode-II profile artifacts have matching sidecars. Artifact existence is recorded in `evidence/artifact-manifest.tsv`.

## Section 2 - Findings
| Corpus | Direct T1 c/B vs best strict comparator | Typed T1 c/B vs best typed comparator | Direct first line-resolved hot leaf | Typed first line-resolved hot leaf |
|---|---:|---:|---|---|
| twitter | 0.650 | 0.690 | rank 1 75.19% `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` | rank 1 74.41% `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` |
| citm_catalog | 0.551 | 0.564 | rank 1 74.74% `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` | rank 1 70.78% `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` |
| canada | 0.778 | 0.790 | rank 3 10.37% `core::str::validations::run_utf8_validation` `/rustc/02c7f9bec0fd583160f8bcccb830216023b07bee/library/core/src/str/validations.rs:133` | rank 2 32.61% `<bbnf_bench::generated_real_typed::DirectParser>::tiny_plain_string_end` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4249` |
| apache_builds | 0.603 | 0.573 | rank 1 43.90% `<u64>::wrapping_add` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:2511` | rank 1 44.96% `<u64>::wrapping_add` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:2511` |
| github_events | 0.747 | 0.741 | rank 1 37.94% `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` | rank 1 42.13% `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` |
| update_center | 0.943 | 0.693 | rank 1 33.01% `bbnf_bench::generated_real_typed::parse_type_plugin_ordered` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:837` | rank 1 30.39% `bbnf_bench::generated_real_typed::parse_type_plugin_ordered` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:837` |
| mesh | 1.093 | 0.930 | rank 1 42.79% `bbnf_bench::generated_real_typed::parse_type_mesh` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1886` | rank 1 43.39% `bbnf_bench::generated_real_typed::parse_type_mesh` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1886` |
| random | 0.724 | 0.573 | rank 1 37.09% `<bbnf_bench::generated_real_typed::DirectParser>::ws` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:3763` | rank 1 30.39% `alloc::alloc::alloc` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/alloc.rs:101` |
| gsoc-2018 | 0.888 | 0.872 | rank 1 99.03% `<bbnf_bench::generated_real_typed::DirectParser>::ws` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:3763` | rank 1 98.72% `<bbnf_bench::generated_real_typed::DirectParser>::ws` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:3763` |
| marine_ik | 0.829 | 0.847 | rank 1 41.55% `bbnf_bench::generated_real_typed::parse_type_marine_geometry_data` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2078` | rank 1 40.83% `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:4174` |
| instruments | 0.788 | 0.762 | rank 1 31.24% `bbnf_bench::generated_real_typed::parse_type_instrument` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2154` | rank 1 30.28% `bbnf_bench::generated_real_typed::parse_type_instrument` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2154` |
| numbers | 0.909 | 0.794 | rank 1 73.56% `alloc::alloc::alloc` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/alloc.rs:101` | rank 1 72.89% `<core::option::Option<&u8>>::copied` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/option.rs:2141` |
| unicode_mixed | 0.757 | 0.760 | rank 1 95.24% `bbnf_bench::generated_real_typed::parse_type_unicode_mixed_document` `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1362` | rank 1 94.74% `alloc::alloc::alloc` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/alloc.rs:101` |
| unicode_escapes | 1.402 | 1.394 | rank 1 99.56% `parse_that_regex::skip_ascii_whitespace` `skinny/crates/parse-that-regex/src/lib.rs:121` | rank 1 99.55% `parse_that_regex::skip_ascii_whitespace` `skinny/crates/parse-that-regex/src/lib.rs:121` |
| unicode_basic | 0.840 | 0.750 | rank 2 24.84% `parse_that_regex::skip_ascii_whitespace` `skinny/crates/parse-that-regex/src/lib.rs:121` | rank 2 25.91% `alloc::alloc::alloc` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/alloc.rs:101` |
| distinct_values | 0.674 | 0.422 | rank 1 44.46% `alloc::alloc::alloc` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/alloc.rs:101` | rank 1 44.59% `alloc::alloc::alloc` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/alloc.rs:101` |
| y_string_unicode | 0.728 | 0.733 | rank 1 88.86% `alloc::alloc::alloc` `nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/alloc.rs:101` | rank 1 89.66% `bbnf_bench::real_typed_struct::track1_typed` `skinny/crates/bbnf-bench/src/real_typed_struct.rs:1521` |

Track 1 direct_strict is slower than the best strict comparator by c/B on `mesh` and `unicode_escapes`; Track 1 real_typed is slower on `unicode_escapes`. These are not admission reversals here because S-P1 only measures. They are measurement-only S-P2 inputs and do not reopen REDRESS-50-55, REDRESS-60-72, REDRESS-80, REDRESS-82-84, REDRESS-88, or REDRESS-89.

## Section 3 - Delta
No `skinny/RESULTS.md` row is changed by this pass. Fresh P1-B profiles replace the SK-V14 stale profile citations and expose the c/B misses above.

## Section 4 - Anomalies
- Product-plane top leaves frequently land in generated strict-product code (`generated_real_typed.rs`) or checksum/allocation code, not in a generic substrate primitive. `evidence/p1e-normalized-attribution.tsv` maps those schema-shaped names to primitive boundaries or blocks them as parser antecedents.
- The FNV closed-enum bench concern remains outside P1-B measurement scope and is still routed to SK-V15 REBUILD-WAVE-G.
- Branch/L1/LLC counters are absent for the same macOS PMU reason recorded in P1-A.

## Section 5 - Sources
- `/tmp/skv15-p1/logs-interactive/p1b/*.log`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1ab-interactive-hotleaf-top20.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/pmu-cpb-summary.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/artifact-manifest.tsv`.

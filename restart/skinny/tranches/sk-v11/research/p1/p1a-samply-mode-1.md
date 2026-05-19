# SK-V11 P1-A: Parse-Only Diagnostic Profile

Pass: S-P1 Profile. Cycle: V2 fold.
Date: 2026-05-19.
Scope: parse-only diagnostic profile for generated Track 1 and independent
Track 2 across all 17 JSON corpora.
Output: this file.
Baseline: SK-V11-open, commit `3ce75df4`, run id
`sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile; debug symbols
were available to the xctrace exports.
Profile tool: `samply` JSON flame profiles, `xcrun xctrace` Time Profiler
exports, and `xctrace_probe` PMU rows; saved logs do not embed the samply
version, and the V2 hardening host reports `samply 0.13.1`.
Corpus coverage: parse-only Track 1/Track 2 17/17.

Shared capture provenance:

- Run id: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Capture root: `/tmp/skv11-p1`; W0 Criterion root:
  `/tmp/skv11-open-criterion-3ce75df`.
- Host/toolchain: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2.
- Source SHA for `xctrace_probe` and `profile_direct`: `3ce75df4`, the last
  behavior/probe source commit before profiling. Documentation/results freeze:
  `9c8da194`. This V2 fold edits docs only.
- Build profile: release with debug symbols, `RUSTFLAGS="-C target-cpu=native"`,
  target directory `/tmp/skv11-profile-target-9c8da194`.
- Binary paths:
  `/tmp/skv11-profile-target-9c8da194/release/xctrace_probe` and
  `/tmp/skv11-profile-target-9c8da194/release/profile_direct`.

Exact build command:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv11-profile-target-9c8da194 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

## Section 1 - Method

This worker consumed the authoritative fresh capture rooted at
`/tmp/skv11-p1`; it did not rerun or mutate the benchmark source. The capture
contains all 34 parse-only samply profiles and all 34 parse-only xctrace Time
Profiler exports.

Commands used to inspect the capture:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
jq '.source, (.traces | length)' /tmp/skv11-p1/parse-xctrace/exports/summary.json
awk -F '\t' '$1=="samply-parse" {rc=$5; sub(/^rc=/,"",rc); c[rc]++} END {for (k in c) print k, c[k]}' /tmp/skv11-p1/pmu/capture_status.tsv
awk -F '\t' '$1=="xctrace-time-profiler" && $2=="parse" {rc=$5; sub(/^rc=/,"",rc); c[rc]++} END {for (k in c) print k, c[k]}' /tmp/skv11-p1/pmu/capture_status.tsv
sed -n '1,80p' /tmp/skv11-p1/pmu/parse_pmu_rows.tsv
```

Capture command shape, as represented by the xctrace logs and artifact paths.
The exact per-row samply shell transcript is not embedded in the saved samply
logs. Therefore the saved samply profiles are artifact-only flame-profile
evidence and xctrace remains the self-time percentage authority. The rerunnable
loop below is the exact parameterization used by the retained artifacts:

```bash
xcrun xctrace record \
  --template "Time Profiler" \
  --time-limit 1000ms \
  --output /tmp/skv11-p1/parse-xctrace/time-profiler/<corpus>__<track>.trace \
  --launch -- \
  /tmp/skv11-profile-target-9c8da194/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> 100000

samply record --save-only --unstable-presymbolicate \
  -o /tmp/skv11-p1/samply/parse/<corpus>__<track>.json.gz \
  -- \
  /tmp/skv11-profile-target-9c8da194/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> 400
```

Status facts from `/tmp/skv11-p1/pmu/capture_status.tsv`:

| Capture family | Parse artifacts | Return codes |
|---|---:|---|
| `samply-parse` | 34/34 | all `rc=0` |
| `pmu-parse` | 34/34 | all `rc=0` |
| `xctrace-time-profiler parse` | 34/34 exported | 33 `rc=54`, 1 `rc=0` (`apache_builds/track1`) |
| `xctrace-cpu-counters parse` | 34/34 traces | all `rc=54` |

The `rc=54` xctrace rows are the 1000 ms time-limit behavior, not missing
profiles: the matching logs say "Reached specified time limit, ending
recording..." and "Output file saved as: <name>.trace". The weighted self-time
table below therefore uses `/tmp/skv11-p1/parse-xctrace/exports/summary.json`
and the per-trace `*.symbols.json` exports. The samply `.json.gz` profiles are
the complete flame artifacts; their Firefox profile metadata reports
`symbolicated=false`, while companion `.json.syms.json` files carry symbol maps,
so this file does not invent samply-only self-time percentages.

Parse-only is diagnostic only. `skinny/RESULTS.md` records the parse plane as
16 `S / NO-GO` rows and 1 `L / NO-GO` row (`canada`); none of those rows is an
SK-V11 SOTA target.

## Section 2 - Findings

Weighted symbol source:
`/tmp/skv11-p1/parse-xctrace/exports/summary.json`.

PMU source:
`/tmp/skv11-p1/pmu/parse_pmu_rows.tsv`.

| Corpus | Track 1 PMU | Track 1 top parse leaves | Track 2 PMU | Track 2 top parse leaves |
|---|---:|---|---:|---|
| `twitter` | 2.74 c/B, 3842 Mbps | 47.2% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 11.6% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 7.3% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47) | 3.21 c/B, 3188 Mbps | 38.3% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 12.2% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 10.7% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4) |
| `citm_catalog` | 1.33 c/B, 7739 Mbps | 25.2% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 22.7% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 9.6% `memcpy` `copy_nonoverlapping::<u8>` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr/mod.rs:531) | 1.84 c/B, 5624 Mbps | 26.3% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 19.6% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 12.2% `structural_rediscovery` `track2::Parser::consume_container_next` (skinny/crates/bbnf-bench/src/track2/json.rs:271) |
| `canada` | 1.97 c/B, 9583 Mbps | 30.0% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 15.2% `memcpy` `copy_nonoverlapping::<u8>` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr/mod.rs:531); 13.9% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47) | 2.06 c/B, 9976 Mbps | 26.8% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 20.2% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53); 13.9% `number_scan` `match_number_span_from_first` (skinny/crates/parse-that-regex/src/number/mod.rs:38) |
| `apache_builds` | 3.27 c/B, 3117 Mbps | 33.3% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 21.6% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 11.1% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4) | 3.32 c/B, 3119 Mbps | 27.8% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 20.5% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 12.9% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4) |
| `github_events` | 2.43 c/B, 6397 Mbps | 42.5% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 10.5% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4); 9.8% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113) | 2.73 c/B, 5721 Mbps | 41.8% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 14.2% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4); 9.3% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113) |
| `update_center` | 3.00 c/B, 5291 Mbps | 48.1% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 12.8% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4); 6.1% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47) | 3.88 c/B, 4270 Mbps | 40.0% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 14.3% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4); 7.6% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53) |
| `mesh` | 3.24 c/B, 3185 Mbps | 20.6% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 19.0% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47); 14.4% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113) | 3.30 c/B, 3141 Mbps | 23.1% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53); 22.7% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 11.7% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113) |
| `random` | 4.09 c/B, 2470 Mbps | 37.6% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 15.1% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 9.3% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4) | 4.83 c/B, 2114 Mbps | 35.1% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 17.6% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 8.7% `other` `track2::Parser::parse_key_colon` (skinny/crates/bbnf-bench/src/track2/json.rs:97) |
| `gsoc-2018` | 1.92 c/B, 6201 Mbps | 33.1% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4); 19.2% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 9.7% `other` `<u16>::trailing_zeros` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:177) | 1.96 c/B, 6533 Mbps | 33.3% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4); 17.9% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 12.2% `other` `<u16>::trailing_zeros` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:177) |
| `marine_ik` | 3.14 c/B, 3310 Mbps | 18.4% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47); 16.7% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 12.5% `memcpy` `copy_nonoverlapping::<u8>` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr/mod.rs:531) | 3.25 c/B, 3156 Mbps | 25.6% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53); 17.3% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 12.0% `number_scan` `match_number_span_from_first` (skinny/crates/parse-that-regex/src/number/mod.rs:38) |
| `instruments` | 2.48 c/B, 4280 Mbps | 29.6% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 24.3% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 7.5% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47) | 3.13 c/B, 3386 Mbps | 28.2% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 17.5% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 6.8% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4) |
| `numbers` | 2.27 c/B, 5440 Mbps | 34.5% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 19.6% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47); 8.5% `sequence_dispatch` `consume_array_next` (skinny/crates/runtime/src/grammars/json/generated.rs:348) | 2.32 c/B, 4625 Mbps | 36.0% `number_digit_scan` `scan_digit_run` (skinny/crates/parse-that-regex/src/number/mod.rs:106); 23.1% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53); 9.3% `number_scan` `match_number_span_from_first` (skinny/crates/parse-that-regex/src/number/mod.rs:38) |
| `unicode_mixed` | 5.28 c/B, 2449 Mbps | 22.7% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47); 18.0% `string_escape` `validate_string_escape` (skinny/crates/parse-that-regex/src/lib.rs:284); 14.1% `string_full_scan` `match_string_at_quote_trusted_utf8` (skinny/crates/parse-that-regex/src/lib.rs:162) | 5.23 c/B, 2807 Mbps | 20.5% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53); 19.5% `string_escape` `validate_string_escape` (skinny/crates/parse-that-regex/src/lib.rs:284); 15.9% `simd_movemask` `movemask_u8x16` (skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4) |
| `unicode_escapes` | 3.19 c/B, 10453 Mbps | 22.9% `unicode_escape_hex` `read_hex_unit_scalar` (skinny/crates/parse-that-regex/src/lib.rs:945); 19.4% `string_full_scan` `match_string_at_quote_trusted_utf8` (skinny/crates/parse-that-regex/src/lib.rs:162); 18.1% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47) | 2.88 c/B, 11743 Mbps | 24.5% `unicode_escape_hex` `read_hex_unit_scalar` (skinny/crates/parse-that-regex/src/lib.rs:945); 14.1% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53); 14.0% `string_full_scan` `match_string_at_quote_trusted_utf8` (skinny/crates/parse-that-regex/src/lib.rs:162) |
| `unicode_basic` | 2.91 c/B, 11567 Mbps | 34.8% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 14.0% `other` `<u16>::trailing_zeros` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:177); 10.1% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47) | 3.31 c/B, 9566 Mbps | 33.0% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 16.1% `other` `<u16>::trailing_zeros` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:177); 11.0% `dispatch_walk` `track2::Parser::parse_value_at` (skinny/crates/bbnf-bench/src/track2/json.rs:53) |
| `distinct_values` | 3.60 c/B, 9314 Mbps | 58.7% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171); 13.1% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113); 7.2% `dispatch_walk` `dispatch_value` (skinny/crates/runtime/src/grammars/json/generated.rs:47) | 5.71 c/B, 5910 Mbps | 65.1% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314); 6.6% `other` `<u16>::trailing_zeros` (/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num/uint_macros.rs:177); 5.4% `whitespace_skip` `skip_ascii_whitespace` (skinny/crates/parse-that-regex/src/lib.rs:113) |
| `y_string_unicode` | 6.02 c/B, 5640 Mbps | 20.8% `unicode_escape_hex` `read_hex_unit_scalar` (skinny/crates/parse-that-regex/src/lib.rs:945); 20.7% `unicode_escape_hex` `hex_nibble` (skinny/crates/parse-that-regex/src/lib.rs:959); 7.1% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (skinny/crates/runtime/src/grammars/json/generated.rs:171) | 5.96 c/B, 5621 Mbps | 26.0% `unicode_escape_hex` `read_hex_unit_scalar` (skinny/crates/parse-that-regex/src/lib.rs:945); 17.8% `unicode_escape_hex` `hex_nibble` (skinny/crates/parse-that-regex/src/lib.rs:959); 7.6% `string_tiny_scan` `track2::match_tiny_plain_string` (skinny/crates/bbnf-bench/src/track2/json.rs:314) |

## Section 3 - Delta vs SK-V10

This is a profile delta, not an admission delta. P1-F owns row-level Mbps
deltas. P1-A admits no parse-only row because SK-V11 inherited the SK-V10 close
rule that parse-only is not a SOTA target.

The fresh SK-V11 profile preserves the SK-V10 diagnostic shape:

- String-heavy parse rows remain dominated by tiny-string scan and whitespace
  leaves on both tracks: `distinct_values`, `update_center`, `twitter`,
  `github_events`, `random`, `apache_builds`, and `instruments`.
- Numeric rows remain split across digit scan, dispatch walk, and memcpy or
  number-span helpers: `canada`, `mesh`, `marine_ik`, and `numbers`.
- Unicode rows remain scalar escape/hex or full-string-scan bound:
  `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.
- `simd_movemask` is visible on string-heavy and unicode rows, but the top
  leaves do not collapse into a single retained structural substrate.

## Section 4 - Anomalies and Masking Signals

- Parse-only masking remains explicit. `canada`, `numbers`,
  `unicode_escapes`, `unicode_basic`, `distinct_values`, and
  `y_string_unicode` have high parse PMU Mbps, but `parse_only` is diagnostic
  and cannot count toward SK-V11 close.
- W3 union/substrate remains pre-blocked. REDRESS 50, 51, 53, 96, and 97 are
  falsification/rejection evidence for sidecar, cursor, and class-column
  substrate routes; REDRESS 98 records `G-W3-UNION-SUBSTRATE` retirement, and
  REDRESS 102 is the parse-only firewall. This profile surfaces no new
  single-substrate ceiling that would reopen that family.
- `distinct_values/track2` is the strongest tiny-string concentration at 65.1%
  self time, but it is a parse-only leaf. Treat it as primitive evidence for
  S-P2 only if a direct or non-JSON consumer is named later.
- `y_string_unicode` is the worst PMU cycles-per-byte row in this capture
  (Track 1 6.02 c/B, Track 2 5.96 c/B) and is dominated by
  `read_hex_unit_scalar` plus `hex_nibble`. That is a scalar unicode escape
  signal, not permission to add a sidecar scanner; REDRESS 54, 55, 60-62, 64,
  66-69, 72, 82, and 83 keep decoded stats, quote-source streaming hash,
  retained boundary collapse, direct materialization, cap-policy, single
  quartet, and StringBlock16 routes pre-blocked unless a later pass proves a
  material differential.
- `unicode_mixed` carries high c/B on both tracks (5.28 and 5.23) with
  `validate_string_escape`, full-string scan, and movemask in the top leaves.
  The row is also W0-clamped on direct output; parse evidence is planning
  input only.
- Track 2 sometimes wins in the PMU wrapper (`canada`, `gsoc-2018`,
  `unicode_mixed`, `unicode_escapes`) while `skinny/RESULTS.md` still treats
  parse-only as `NO-GO`. The mismatch is a masking signal from profiler wrapper
  shape and workload isolation, not an admission signal.
- Most xctrace rows show `rc=54`; this is the expected time-limit exit after
  trace save. The saved trace directories and JSON exports exist, so these are
  not missing captures.

## Section 5 - Sources

- `/tmp/skv11-p1/samply/parse/*.json.gz`
- `/tmp/skv11-p1/samply/parse/*.json.syms.json`
- `/tmp/skv11-p1/samply/parse/*.log`
- `/tmp/skv11-p1/parse-xctrace/time-profiler/*.trace`
- `/tmp/skv11-p1/parse-xctrace/exports/*.symbols.json`
- `/tmp/skv11-p1/parse-xctrace/exports/summary.json`
- `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv11-p1/pmu/capture_status.tsv`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1a-samply-mode-1.md`

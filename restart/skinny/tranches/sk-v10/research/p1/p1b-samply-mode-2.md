# SK-V10 P1-B: Direct And Real-Typed Product-Plane Profile

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: fresh Time Profiler attribution for `direct_to_struct` Track 1/Track 2
over all 17 JSON corpora and `real_typed_struct` Track 1/Track 2 over the six
live typed rows.
Output: this file.
Baseline: SK-V10 Alpha inherits W1-rendered `SK-V9-open`, run
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`; no separate W0 behavior reset
exists because REDRESS 98 retired W3 and Alpha authorized S-P1 only.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, debug symbols.
Profile tool: `xcrun xctrace record --template "Time Profiler"` plus
`xcrun xctrace export`; `rustfilt` for symbol demangling.
Corpus coverage: `direct_to_struct` 17/17 Track 1 + Track 2;
`real_typed_struct` 6/6 Track 1 + Track 2.

## Section 1 - Method

Commands:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin profile_direct --bin xctrace_probe

xcrun xctrace record \
  --template "Time Profiler" \
  --output /tmp/skv10-p1/direct-xctrace/traces/<corpus>__<mode>.trace \
  --no-prompt \
  --time-limit <2500ms-or-3500ms> \
  --launch -- \
  /Users/mkbabb/Programming/bbnf-lang/skinny/target/release/profile_direct \
  <iters> <corpus-or-absolute-fixture-path> <mode>

xcrun xctrace export \
  --input /tmp/skv10-p1/direct-xctrace/traces/<corpus>__<mode>.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]'

python3 /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v10/research/p1/tools/summarize_xctrace_time_profile.py \
  --trace-dir /tmp/skv10-p1/direct-xctrace/traces \
  --output-dir /tmp/skv10-p1/direct-xctrace/exports \
  --process-binary profile_direct
```

Modes:

- `track1`, `track2` for `direct_to_struct`.
- `real_typed_track1`, `real_typed_track2` for measured typed rows.

`update_center/direct_to_struct` was recaptured with the absolute fixture path
`/Users/mkbabb/Programming/bbnf-lang/skinny/test_data/update-center.json`
because the direct profiling binary does not map `update_center` to the
hyphenated fixture name. The first capture with the logical row name failed at
fixture load and is excluded from the exported summary.

Important harness caveat: `profile_direct` performs 16 sanity/warm-up parses
before the timed loop. The Time Profiler attribution below is therefore
accepted as hot-leaf evidence for the product plane, but not as a strict
cold-per-parse Criterion replacement. P1-C owns Criterion masking probes; P1-D
owns parse-lane PMU/cycles evidence and records the direct/typed PMU gap
honestly.

`xcrun xctrace record` exits with code 54 when a `--time-limit` capture reaches
the requested window. Those captures are accepted only when the corresponding
trace bundle and per-row log exist under `/tmp/skv10-p1/direct-xctrace/` and
the export contains process samples. The accepted `rc=54` rows are visible in
`/tmp/skv10-p1/direct-xctrace/capture.log`.

## Section 2 - Findings

Full export summary: `/tmp/skv10-p1/direct-xctrace/exports/summary.json`.

### Direct Track 1

| Corpus | Profiled ms | Top self-time leaves |
|---|---:|---|
| `twitter` | 2005 | 30.5% `string_tiny_scan` `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<8>` (`skinny/crates/runtime/src/grammars/json/generated.rs:171`); 12.2% `whitespace_skip` `parse_that_regex::skip_ascii_whitespace` (`skinny/crates/parse-that-regex/src/lib.rs:113`); 8.2% `simd_movemask` `bbnf_simd::aarch64::movemask::movemask_u8x16` (`skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`) |
| `citm_catalog` | 1306 | 27.4% `whitespace_skip` `parse_that_regex::skip_ascii_whitespace` (`skinny/crates/parse-that-regex/src/lib.rs:113`); 13.7% `string_tiny_scan` `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 8.7% `object_walk` `runtime::generated_json::generated::parse_object_value_at_direct` (`generated.rs:468`) |
| `canada` | 1998 | 23.2% `number_digit_scan` `parse_that_regex::number::scan_digit_run` (`skinny/crates/parse-that-regex/src/number/mod.rs:106`); 16.6% `array_walk` `runtime::generated_json::generated::parse_array_element_at_direct` (`generated.rs:508`); 10.5% `memcpy` `core::ptr::copy_nonoverlapping` |
| `apache_builds` | 2003 | 20.4% `other` `<u64>::wrapping_add`; 14.8% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 11.7% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`) |
| `github_events` | 2001 | 32.1% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 9.5% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`); 9.5% `simd_movemask` `movemask_u8x16` (`bbnf-simd/src/aarch64/movemask.rs:4`) |
| `update_center` | 3004 | 20.4% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 8.7% `other` `Option<&u8>::copied`; 8.1% `simd_movemask` `movemask_u8x16` (`bbnf-simd/src/aarch64/movemask.rs:4`) |
| `mesh` | 1979 | 18.5% `number_digit_scan` `scan_digit_run` (`number/mod.rs:106`); 17.6% `array_walk` `parse_array_element_at_direct` (`generated.rs:508`); 12.4% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`) |
| `random` | 2001 | 17.8% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`); 17.4% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 9.8% `other` `Option<&u8>::copied` |
| `gsoc-2018` | 1995 | 18.4% `simd_movemask` `movemask_u8x16` (`bbnf-simd/src/aarch64/movemask.rs:4`); 12.4% `other` `<[u8]>::split_at_checked`; 8.7% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`) |
| `marine_ik` | 2001 | 19.9% `array_walk` `parse_array_element_at_direct` (`generated.rs:508`); 14.8% `number_digit_scan` `scan_digit_run` (`number/mod.rs:106`); 13.4% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`) |
| `instruments` | 2004 | 31.6% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 16.5% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`); 8.9% `object_walk` `parse_object_value_at_direct` (`generated.rs:468`) |
| `numbers` | 2007 | 25.7% `number_digit_scan` `scan_digit_run` (`number/mod.rs:106`); 16.5% `array_walk` `parse_array_element_at_direct` (`generated.rs:508`); 9.3% `memcpy` `core::ptr::copy_nonoverlapping` |
| `unicode_mixed` | 2002 | 23.8% `string_full_scan` `match_string_at_quote_trusted_utf8` (`parse-that-regex/src/lib.rs:162`); 17.5% `string_escape` `unescape_string` (`parse-that-regex/src/lib.rs:718`); 12.7% `string_escape` `validate_string_escape` (`parse-that-regex/src/lib.rs:284`) |
| `unicode_escapes` | 2006 | 23.4% `string_escape` `unescape_string` (`parse-that-regex/src/lib.rs:718`); 18.2% `string_full_scan` `match_string_at_quote_trusted_utf8` (`parse-that-regex/src/lib.rs:162`); 11.2% `unicode_escape_hex` `read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:945`) |
| `unicode_basic` | 1998 | 15.5% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 11.8% `other` `<u16>::trailing_zeros`; 10.9% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`) |
| `distinct_values` | 1994 | 19.8% `string_tiny_scan` `match_tiny_plain_string_with_cap::<8>` (`generated.rs:171`); 15.7% `whitespace_skip` `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`); 12.5% `direct_struct` `JsonDirectDigest::fold_string_scalar` (`skinny/crates/bbnf-bench/src/direct_struct.rs:123`) |
| `y_string_unicode` | 1998 | 39.1% `alloc` `alloc::alloc::dealloc_nonnull`; 10.5% `alloc` `alloc::alloc::alloc`; 6.2% `unicode_escape_hex` `read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:945`) |

### Real-Typed Track 1

| Corpus | Profiled ms | Top self-time leaves |
|---|---:|---|
| `twitter` | 1686 | 37.6% `string_tiny_scan` `DirectParser::skip_plain_string_end` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1359`; generator template `skinny/crates/codegen/src/typed_direct.rs:649`); 15.0% `whitespace_skip`; 10.8% pointer equality |
| `citm_catalog` | 800 | 31.0% `whitespace_skip`; 17.9% `DirectParser::skip_plain_string_end` (`generated_real_typed.rs:1359`); 10.4% `DirectParser::skip_value` (`generated_real_typed.rs:1273`) |
| `apache_builds` | 1999 | 30.4% `DirectParser::tiny_plain_string_end` (`generated_real_typed.rs:1345`; generator template `typed_direct.rs:635`); 25.0% pointer equality; 7.9% `whitespace_skip` |
| `update_center` | 1994 | 30.0% `DirectParser::skip_plain_string_end` (`generated_real_typed.rs:1359`); 15.4% `DirectParser::tiny_plain_string_end` (`generated_real_typed.rs:1345`); 12.6% pointer equality |
| `mesh` | 1996 | 16.5% `number_digit_scan`; 13.8% `number_scan` `match_number_span_from_first` (`parse-that-regex/src/number/mod.rs:38`); 9.8% `whitespace_skip` |
| `marine_ik` | 1997 | 16.7% `whitespace_skip`; 15.9% `number_digit_scan`; 13.4% `number_scan` `match_number_span_from_first` (`number/mod.rs:38`) |

Track 2 profiles are captured for independence checking. The full Track 2
tables live in `/tmp/skv10-p1/direct-xctrace/exports/summary.json`. The
high-order shape matches Track 1: string-heavy rows are tiny-string/string scan
bound, numeric rows are number scan bound, and typed Track 2 spends most of its
time in serde_json read/whitespace/decimal routines.

## Section 3 - Delta vs SK-V9

P1-F owns the full row delta ledger. P1-B adds no row admission. The profile
confirms SK-V10 Alpha's frontier split:

- Direct digest rows are live behavior candidates: 14 of 17 remain
  `N-direct / NO-GO`, with losses clustering in tiny-string scans, unicode
  escape decode, and number scan/copy paths.
- The six typed rows are still `A / GO`; no new typed row exists in this
  artifact.
- Parse-only rows stay diagnostic and are not SOTA admissions.

## Section 4 - Anomalies + Masking Signals

- `y_string_unicode/direct_to_struct` is allocator-dominated despite a digest
  plane that should be SinkOnly. S-P2 must determine whether the allocation is
  from `Cow` unescape materialization and whether a bounded decoded-scratch
  route is admissible without reopening REDRESS 66-69.
- `unicode_escapes` and `unicode_mixed` still point to the existing-substrate
  unicode/string kernel candidate, but SK-V10 requires micro-prove-first before
  S-P3 scopes it.
- `twitter`, `github_events`, `instruments`, and `update_center` point to
  tiny-string/direct string scanner work rather than a substrate rewrite.
- `citm_catalog` is whitespace dominated while already admitted on direct and
  typed planes; it is a maintain row, not a primary intervention target.

## Section 5 - Sources

- `/tmp/skv10-p1/direct-xctrace/traces/*.trace`
- `/tmp/skv10-p1/direct-xctrace/logs/*.log`
- `/tmp/skv10-p1/direct-xctrace/exports/*.symbols.json`
- `/tmp/skv10-p1/direct-xctrace/exports/summary.json`
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`
- `skinny/RESULTS.md`

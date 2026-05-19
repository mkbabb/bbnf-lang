# SK-V10 P1-A: Parse-Only Diagnostic Profile

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: fresh diagnostic parse-only Time Profiler attribution for Track 1
generated parse and Track 2 hand-coded parse across all 17 JSON corpora.
Output: this file.
Baseline: SK-V10 Alpha inherits W1-rendered `SK-V9-open`, run
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, debug symbols.
Profile tool: `xcrun xctrace record --template "Time Profiler"` wrapped around
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`; `rustfilt` for demangling.
Corpus coverage: parse-only Track 1/Track 2 17/17.

## Section 1 - Method

Commands:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin profile_direct --bin xctrace_probe

xcrun xctrace record \
  --template "Time Profiler" \
  --no-prompt \
  --output /tmp/skv10-p1/parse-xctrace/time-profiler/<corpus>__<track>.trace \
  --launch -- \
  /Users/mkbabb/Programming/bbnf-lang/skinny/target/release/xctrace_probe \
  /Users/mkbabb/Programming/bbnf-lang/skinny/test_data/<corpus>.json \
  <track1-or-track2> <iters>

xcrun xctrace export \
  --input /tmp/skv10-p1/parse-xctrace/time-profiler/<corpus>__<track>.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]'
```

The capture is diagnostic only. SK-V10 Alpha retired parse-only as a SOTA close
target; this file keeps the parse plane legible so S-P2 does not smuggle the
retired W3 hypothesis back into a live wave.

## Section 2 - Findings

Full exported symbol tables:
`/tmp/skv10-p1/parse-xctrace/exports/summary.json`.

| Corpus | Track 1 top leaves | Track 2 top leaves |
|---|---|---|
| `twitter` | 49.0% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (`skinny/crates/runtime/src/grammars/json/generated.rs:171`); 9.5% `whitespace_skip` `skip_ascii_whitespace` (`skinny/crates/parse-that-regex/src/lib.rs:113`); 8.6% `dispatch_walk` `dispatch_value` (`generated.rs:51`) | 33.1% `string_tiny_scan` `track2::json::match_tiny_plain_string` (`skinny/crates/bbnf-bench/src/track2/json.rs:314`); 12.3% `whitespace_skip`; 11.2% `simd_movemask` `movemask_u8x16` (`skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`) |
| `citm_catalog` | 25.4% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (`generated.rs:171`); 23.2% `whitespace_skip`; 9.0% `dispatch_walk` `dispatch_value` (`generated.rs:51`) | 22.8% `whitespace_skip`; 19.1% `track2::json::match_tiny_plain_string` (`track2/json.rs:314`); 11.6% `dispatch_walk` `Parser::parse_value_at` (`track2/json.rs:53`) |
| `canada` | 20.2% `dispatch_walk` `dispatch_value` (`generated.rs:51`); 19.5% `number_digit_scan` `scan_digit_run` (`skinny/crates/parse-that-regex/src/number/mod.rs:106`); 16.2% `memcpy` `core::ptr::copy_nonoverlapping` | 23.2% `dispatch_walk` `Parser::parse_value_at` (`track2/json.rs:53`); 22.7% `number_digit_scan`; 15.4% `memcpy` |
| `apache_builds` | 53.1% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (`generated.rs:171`); 12.4% `whitespace_skip`; 8.2% `simd_movemask` | 43.0% `track2::json::match_tiny_plain_string` (`track2/json.rs:314`); 14.4% `whitespace_skip`; 8.7% `Parser::parse_pair` (`track2/json.rs:91`) |
| `github_events` | 43.6% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (`generated.rs:171`); 13.1% `whitespace_skip`; 8.5% `simd_movemask` | 29.1% `track2::json::match_tiny_plain_string` (`track2/json.rs:314`); 13.0% `simd_movemask`; 11.8% `whitespace_skip` |
| `update_center` | 57.5% `string_tiny_scan` `match_tiny_plain_string_with_cap::<16>` (`generated.rs:171`); 8.1% `simd_movemask`; 7.3% `parse_value_at` (`generated.rs:42`) | 44.6% `track2::json::match_tiny_plain_string` (`track2/json.rs:314`); 8.7% `Parser::parse_value_at`; 8.2% `simd_movemask` |
| `mesh` | 20.6% `number_digit_scan` `scan_digit_run` (`number/mod.rs:106`); 20.0% `dispatch_value`; 13.7% `number_scan` `match_number_span_from_first` (`number/mod.rs:127`) | 28.0% `Parser::parse_value_at`; 18.2% `scan_digit_run`; 11.2% `match_number_span_from_first` |
| `random` | 46.5% `string_tiny_scan`; 12.6% `whitespace_skip`; 8.0% `dispatch_value` | 41.6% `track2::json::match_tiny_plain_string`; 12.1% `whitespace_skip`; 11.5% `structural_rediscovery` `Parser::consume_container_next` (`track2/json.rs:271`) |
| `gsoc-2018` | 26.8% `simd_movemask`; 26.3% `string_tiny_scan`; 16.9% `parse_value_at` | 27.9% `simd_movemask`; 23.6% `track2::json::match_tiny_plain_string`; 18.1% `Parser::parse_pair` |
| `marine_ik` | 21.6% `dispatch_value`; 14.5% `scan_digit_run`; 12.2% `memcpy` | 24.7% `Parser::parse_value_at`; 15.2% `match_number_span_from_first`; 14.7% `scan_digit_run` |
| `instruments` | 36.8% `string_tiny_scan`; 23.1% `whitespace_skip`; 8.5% `dispatch_value` | 31.5% `track2::json::match_tiny_plain_string`; 18.0% `whitespace_skip`; 8.7% `Parser::parse_value_at` |
| `numbers` | 39.8% `scan_digit_run`; 15.4% `dispatch_value`; 9.4% `consume_array_next` (`generated.rs:315`) | 37.9% `scan_digit_run`; 21.2% `Parser::parse_value_at`; 9.3% `match_number_span_from_first` |
| `unicode_mixed` | 22.3% `dispatch_value`; 19.2% `validate_string_escape` (`parse-that-regex/src/lib.rs:310`); 16.3% `match_string_at_quote_trusted_utf8` (`parse-that-regex/src/lib.rs:162`) | 15.8% `match_string_at_quote_trusted_utf8`; 15.0% `validate_string_escape`; 13.5% `simd_movemask` |
| `unicode_escapes` | 23.9% `read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:945`); 18.0% `dispatch_value`; 16.6% `match_string_at_quote_trusted_utf8` | 25.4% `read_hex_unit_scalar`; 13.9% `Option<&u8>::copied`; 11.6% `hex_nibble` (`parse-that-regex/src/lib.rs:961`) |
| `unicode_basic` | 34.4% `string_tiny_scan`; 16.8% `parse_value_at`; 10.2% `whitespace_skip` | 34.7% `track2::json::match_tiny_plain_string`; 18.0% `Parser::parse_pair`; 9.8% `Parser::parse_value_at` |
| `distinct_values` | 61.3% `string_tiny_scan`; 9.0% `whitespace_skip`; 5.9% `simd_movemask` | 63.5% `track2::json::match_tiny_plain_string`; 7.6% `whitespace_skip`; 6.0% `simd_movemask` |
| `y_string_unicode` | 22.6% `hex_nibble`; 15.9% `read_hex_unit_scalar`; 8.9% `string_tiny_scan` | 29.7% `read_hex_unit_scalar`; 10.2% `track2::json::match_tiny_plain_string`; 9.8% `hex_nibble` |

## Section 3 - Delta vs SK-V9

The fresh parse profile matches the empirical conclusion that retired W3 should
not be reopened:

- Parse rows are still dominated by local string, unicode, number, whitespace,
  and dispatch leaves.
- `consume_structural` is not a universal top leaf in this fresh capture; it is
  visible only as a subordinate structural-rediscovery class.
- The hot leaves do not form one substrate-ceiling intervention. SK-V10 must
  plan direct/typed work from product-plane profiles instead.

P1-F owns row-level Mbps deltas. P1-A admits no parse row.

## Section 4 - Anomalies + Masking Signals

- `distinct_values`, `update_center`, `twitter`, `apache_builds`, and
  `random` are strongly tiny-string dominated on parse Track 1. That is useful
  profile context but cannot authorize parse-only SOTA closure.
- `unicode_escapes` and `y_string_unicode` expose the scalar hex/escape path
  on parse. Any successor kernel must be existing-substrate and micro-proved
  before S-P3 scopes it.
- Numeric rows (`numbers`, `mesh`, `canada`, `marine_ik`) remain number-scan
  and dispatch/memcpy bound, not W3-substrate bound.

## Section 5 - Sources

- `/tmp/skv10-p1/parse-xctrace/time-profiler/*.trace`
- `/tmp/skv10-p1/parse-xctrace/exports/*.symbols.json`
- `/tmp/skv10-p1/parse-xctrace/exports/summary.json`
- `/tmp/skv10-p1/parse-xctrace/pmu_rows.tsv`
- `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`

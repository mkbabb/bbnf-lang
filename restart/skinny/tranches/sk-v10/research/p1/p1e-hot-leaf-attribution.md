# SK-V10 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: synthesize per-row hot leaves from the fresh SK-V10 product-plane
capture and the fresh parse-plane PMU/Time Profiler lane.
Output: this file.
Baseline: SK-V10 Alpha inherits W1-rendered `SK-V9-open`, run
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile, debug symbols.
Profile tool: `xcrun xctrace` Time Profiler export plus
`proc_pid_rusage(RUSAGE_INFO_V5)` PMU rows where available.
Corpus coverage: direct product-plane 17/17 complete; typed product-plane 6/6
complete; parse-only Track 1/Track 2 17/17 complete.

## Section 1 - Method

Inputs:

- P1-B direct/typed Time Profiler summary:
  `/tmp/skv10-p1/direct-xctrace/exports/summary.json`.
- P1-C Mode III Criterion masking probes:
  `/tmp/skv10-p1/mode3-criterion`.
- P1-A/P1-D parse PMU/Time Profiler capture root:
  `/tmp/skv10-p1/parse-xctrace/`.
- `skinny/RESULTS.md` for row identity and current outcomes.

Symbol classes are grammar-neutral when possible: `string_tiny_scan`,
`string_full_scan`, `string_escape`, `unicode_escape_hex`, `number_digit_scan`,
`whitespace_skip`, `array_walk`, `object_walk`, `direct_struct`,
`simd_movemask`, `alloc`, and `memcpy`.

## Section 2 - Findings

### Product-Plane Class Map

| Class | Rows where it is load-bearing | Source anchors |
|---|---|---|
| `string_tiny_scan` | `twitter`, `github_events`, `instruments`, `update_center`, `apache_builds`, `distinct_values`, `unicode_basic`; typed `twitter`, `apache_builds`, `update_center` | `runtime/src/grammars/json/generated.rs:171`; `bbnf-bench/src/generated_real_typed.rs:1345`; generator `codegen/src/typed_direct.rs:635` |
| `string_full_scan` | `unicode_mixed`, `unicode_escapes`, Track 2 `unicode_basic` | `parse-that-regex/src/lib.rs:162`; `direct_struct.rs:541` caller |
| `string_escape` / `unicode_escape_hex` | `unicode_escapes`, `unicode_mixed`, `y_string_unicode` | `parse-that-regex/src/lib.rs:718`, `:945`, `:961` |
| `number_digit_scan` / `number_scan` | `canada`, `mesh`, `numbers`, `marine_ik`, typed `mesh`, typed `marine_ik` | `parse-that-regex/src/number/mod.rs:106`, `:127` |
| `whitespace_skip` | `citm_catalog`, `random`, `mesh`, `marine_ik`, typed `citm_catalog` | `parse-that-regex/src/lib.rs:113` |
| `array_walk` / `object_walk` | numeric and mixed rows where generated direct dispatch remains visible after primitive leaves | `runtime/src/grammars/json/generated.rs:468`, `:508` |
| `simd_movemask` | `gsoc-2018`, secondary on `twitter`, `github_events`, `update_center` | `bbnf-simd/src/aarch64/movemask.rs:4` |
| `alloc` | `y_string_unicode` direct Track 1 and Track 2 | Rust `alloc`; likely `Cow` escape materialization, to be verified before S-P2 candidate scoping |

### Direct Track 1 Per-Row Attribution

| Corpus | Dominant class | Top evidence | S-P2 implication |
|---|---|---|---|
| `twitter` | `string_tiny_scan` | 30.5% `match_tiny_plain_string_with_cap::<8>` | direct tiny-string scanner is a candidate only if micro-proved at the call site |
| `citm_catalog` | `whitespace_skip` | 27.4% `skip_ascii_whitespace` | maintain row; no W10-style regression tolerated |
| `canada` | `number_digit_scan` | 23.2% `scan_digit_run` | numeric primitive possible, but Canada typed remains pre-blocked |
| `apache_builds` | mixed digest arithmetic/string | 20.4% `wrapping_add`; 14.8% tiny string | typed row is already admitted; direct row is still W0-clamped |
| `github_events` | `string_tiny_scan` | 32.1% tiny string; 9.5% movemask | root-type typed generalization plus direct scanner evidence |
| `update_center` | `string_tiny_scan` | 20.4% tiny string | direct and typed both string-bound; typed row is below sonic but within gate |
| `mesh` | `number_digit_scan` | 18.5% digit scan; 17.6% array walk | numeric row; protect existing typed `A / GO` |
| `random` | whitespace/tiny string | 17.8% whitespace; 17.4% tiny string | mixed frontier, not a substrate candidate |
| `gsoc-2018` | `simd_movemask` / string split | 18.4% movemask; 12.4% slice split | root-type typed generalization likely precedes kernels |
| `marine_ik` | array/number/whitespace | 19.9% array walk; 14.8% digit scan | already direct and typed `A / GO`; maintain gate |
| `instruments` | `string_tiny_scan` | 31.6% tiny string; 16.5% whitespace | first typed-admission candidate from Alpha-E |
| `numbers` | `number_digit_scan` | 25.7% digit scan | direct row numerically close but W0-clamped |
| `unicode_mixed` | `string_full_scan` + escape | 23.8% full scan; 17.5% unescape | existing-substrate unicode/string micro-proof candidate |
| `unicode_escapes` | `string_escape` + hex | 23.4% unescape; 11.2% hex scalar | existing-substrate codec candidate, no union substrate |
| `unicode_basic` | tiny string + bit ops | 15.5% tiny string; 11.8% trailing_zeros | already direct `A / GO`; maintain row |
| `distinct_values` | tiny string + digest fold | 19.8% tiny string; 12.5% digest fold | direct scanner or digest fold only with product contract |
| `y_string_unicode` | `alloc` | 39.1% dealloc; 10.5% alloc | investigate allocation source before any codec/kernel wave |

## Section 3 - Delta vs SK-V9

SK-V10 did not perform behavior changes after SK-V9 W1/W2/W3 retirement.
P1-E therefore records a frontier shift rather than a throughput delta:

- The old parse-plane substrate hypothesis is no longer an intervention
  ancestor.
- The live direct-plane losses are primitive/call-site specific, not one
  uniform substrate ceiling.
- The typed-plane wins are preserved, but S-P1 does not admit new typed rows.

## Section 4 - Anomalies + Masking Signals

- The `y_string_unicode` allocator leaf is the highest single self-time leaf in
  the product-plane capture. It is not enough to authorize a decoded scratch
  route; P1-C's eager-decode probe is 3.02x slower than cold parse on the same
  row, and REDRESS 66-69 still pre-block direct
  receiver/scratch/semantic-fact classes without a material differential.
- `simd_movemask` being visible on `gsoc-2018` is not proof of a new SIMD
  kernel opportunity. It is a measured hot leaf; P1-C's structural scanner
  table is isolated primitive evidence only, so S-P2 must micro-prove the
  caller before S-P3 scopes a wave.
- `parse_only` remains diagnostic. P1-E must not use parse-only hot leaves as
  SOTA-close evidence.

## Section 5 - Sources

- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `/tmp/skv10-p1/direct-xctrace/exports/summary.json`
- `/tmp/skv10-p1/mode3-criterion`
- `/tmp/skv10-p1/parse-xctrace/pmu_rows.tsv`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 94-98

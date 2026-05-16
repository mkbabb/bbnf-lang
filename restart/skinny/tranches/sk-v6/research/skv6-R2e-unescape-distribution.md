# SK-V6 Wave 3 R2e: Unescape Distribution Scout

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only repo inspection; wrote only this `/tmp` report.

## Method

I used a byte-level JSON string lexer over:

- `skinny/test_data/unicode_escapes.json`
- `skinny/test_data/unicode_mixed.json`
- `skinny/test_data/y_string_unicode.json`

Counts below distinguish **escape sequences** from raw `\` bytes. Raw `\` byte counts include the second byte of escaped-backslash sequences; escape-sequence counts count only the escape introducer.

Current `unescape_json_string` shape in `skinny/crates/parse-that-regex/src/lib.rs`:

1. If raw content has no `\`, validate/classify and return `Cow::Borrowed`.
2. Otherwise allocate `String::with_capacity(raw_content.len())`.
3. Scan with `find_next_escape_or_control`, an 8-byte SWAR escape/control finder.
4. Copy literal segments with `push_str`.
5. Decode simple escapes with one `String::push(char)` each.
6. On AArch64, try `unescape_four_unicode_escapes` for four adjacent `\uXXXX` units, then fall back to scalar `decode_json_unicode_escape`.

## Distribution Counts

| row | file bytes | strings | escaped strings | escaped % | raw `\` bytes | escape seqs | simple seqs | `\u` units | simple-only strings | unicode-only strings | mixed simple+unicode |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `unicode_escapes` | 1,050,797 | 5,636 | 1,877 | 33.30% | 230,134 | 222,874 | 86,192 | 136,682 | 626 | 1,251 | 0 |
| `unicode_mixed` | 1,053,086 | 25,121 | 1,959 | 7.80% | 65,489 | 53,644 | 53,644 | 0 | 1,959 | 0 | 0 |
| `y_string_unicode` | 35,601 | 2,200 | 1,800 | 81.82% | 4,600 | 4,600 | 200 | 4,400 | 200 | 1,600 | 0 |

Escape type breakdown:

| row | `\u` | `\n` | `\t` | `\r` | `\b` | `\f` | `\"` | `\\` |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `unicode_escapes` | 136,682 | 25,589 | 20,546 | 8,212 | 10,288 | 7,079 | 7,218 | 7,260 |
| `unicode_mixed` | 0 | 14,989 | 14,995 | 0 | 0 | 0 | 11,815 | 11,845 |
| `y_string_unicode` | 4,400 | 0 | 0 | 0 | 0 | 0 | 200 | 0 |

Unicode unit classes:

| row | high surrogate | low surrogate | BMP 3-byte units | BMP 2-byte units | BMP ASCII units |
|---|---:|---:|---:|---:|---:|
| `unicode_escapes` | 30,455 | 30,455 | 75,772 | 0 | 0 |
| `unicode_mixed` | 0 | 0 | 0 | 0 | 0 |
| `y_string_unicode` | 1,600 | 1,600 | 1,200 | 0 | 0 |

## Current Batch Fit

| row | `\u` runs | run min / p50 / p90 / max | x4 attempts | x4 successes | units via x4 | units via scalar | miss reason |
|---|---:|---:|---:|---:|---:|---:|---|
| `unicode_escapes` | 1,251 | 40 / 109 / 158 / 180 | 35,022 | 33,787 | 135,148 | 1,534 | 1,235 end-of-run scalar remainders |
| `unicode_mixed` | 0 | 0 / 0 / 0 / 0 | 0 | 0 | 0 | 0 | no Unicode escapes |
| `y_string_unicode` | 1,600 | 1 / 2 / 11 / 11 | 2,400 | 200 | 800 | 3,600 | 1,800 short runs, 400 high-surrogate-at-fourth-unit boundaries |

Interpretation:

- `unicode_escapes` is already a near-perfect fit for the current four-unit Unicode batch: 98.88% of its `\u` units are consumed by the existing AArch64 x4 helper.
- `y_string_unicode` is not a four-unit-run row. Most runs are single BMP units or surrogate pairs, and the long family-emoji run repeatedly places a high surrogate at the fourth unit boundary, which intentionally falls back to scalar.
- `unicode_mixed` cannot benefit from a Unicode batch at all.

## Materialization Shape

Escaped-string decoded byte sources:

| row | copied literal decoded bytes | simple escape decoded bytes | Unicode escape decoded bytes | total decoded bytes from escaped strings |
|---|---:|---:|---:|---:|
| `unicode_escapes` | 16,196 | 86,192 | 349,136 | 451,524 |
| `unicode_mixed` | 228,543 | 53,644 | 0 | 282,187 |
| `y_string_unicode` | 0 | 200 | 10,000 | 10,200 |

Approximate current write operations in `unescape_json_string`:

| row | nonempty `push_str` segments | simple `push(char)` | Unicode `push(char)` | total char pushes |
|---|---:|---:|---:|---:|
| `unicode_escapes` | 13,596 | 86,192 | 106,227 | 192,419 |
| `unicode_mixed` | 28,994 | 53,644 | 0 | 53,644 |
| `y_string_unicode` | 0 | 200 | 2,800 | 3,000 |

Capacity from `String::with_capacity(raw_content.len())`:

| row | escaped raw bytes | decoded bytes | overreserved bytes | overreserved % of raw | raw/decoded ratio | avg waste per escaped string |
|---|---:|---:|---:|---:|---:|---:|
| `unicode_escapes` | 1,008,672 | 451,524 | 557,148 | 55.24% | 2.234x | 296.83 |
| `unicode_mixed` | 335,831 | 282,187 | 53,644 | 15.97% | 1.190x | 27.38 |
| `y_string_unicode` | 26,800 | 10,200 | 16,600 | 61.94% | 2.627x | 9.22 |

Escaped string size buckets by raw content bytes:

| row | 1-7 | 8-15 | 32-63 | 64-127 | 128-255 | 256+ |
|---|---:|---:|---:|---:|---:|---:|
| `unicode_escapes` | 0 | 0 | 0 | 0 | 244 | 1,633 |
| `unicode_mixed` | 0 | 0 | 92 | 549 | 973 | 345 |
| `y_string_unicode` | 800 | 800 | 0 | 200 | 0 | 0 |

## Candidate Choice

Best next candidate: **byte-Vec materialization inside `unescape_json_string`**, preserving the existing scanner, existing scalar semantics, and existing AArch64 x4 Unicode decoder.

Why this is the best of the four:

- **Unicode batch is not the next primary move.** `unicode_escapes` already batches 135,148 / 136,682 Unicode units. A pair-aware batch could help `y_string_unicode`, but it has no `unicode_mixed` surface and would mostly chase a 35 KB small-corpus row.
- **ASCII simple-escape fast path is useful but too narrow alone.** It covers all escaped `unicode_mixed` strings and 626 simple-only strings in `unicode_escapes`, but it misses the 349,136 decoded bytes from Unicode escapes in `unicode_escapes` and nearly all of `y_string_unicode`. It should be folded into a byte writer as a table-driven subpath, not pursued as a standalone candidate.
- **Capacity strategy is not the performance lever.** Raw-capacity allocation overreserves heavily for Unicode escapes, but `String::with_capacity` does not write those bytes. Shrinking capacity does not remove per-string allocation count, the second pass, hex decode, or `push(char)` work. It is more likely a memory/RSS cleanup than a throughput close.
- **Byte-Vec materialization covers every focus row.** Across the three rows, the current path performs 42,590 nonempty segment copies, 140,036 simple `push(char)` operations, and 109,027 Unicode `push(char)` operations. A `Vec<u8>` writer can copy literal bytes, emit simple escapes as single bytes, and encode Unicode scalars directly to UTF-8 bytes before one final `String::from_utf8_unchecked`/checked-debug conversion. That attacks the shared residual after the current x4 Unicode batch has already done its job.

Boundary for the candidate:

- Keep `unescape_json_string(raw_content: &str) -> Result<Cow<'_, str>, RegexError>` as the public behavior.
- Keep the no-backslash borrowed path unchanged.
- Keep `find_next_escape_or_control`, `decode_json_unicode_escape`, and `unescape_four_unicode_escapes` semantics/error offsets.
- Do not add parser-owned scratch, sink hooks, decoded stats, quote-source streaming hash, counters, or a second parser path.
- Implement only the escaped materializer body with byte output; the simple-escape table can be part of this writer.

## Gate

Correctness gate:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv6-R2e-bytevec-target cargo test -p parse-that-regex --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv6-R2e-bytevec-target cargo test -p runtime --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv6-R2e-bytevec-target cargo test -p bbnf-bench --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv6-R2e-bytevec-target cargo run -p xtask --release -- check-json
CARGO_TARGET_DIR=/tmp/skv6-R2e-bytevec-target cargo run -p xtask --release -- check-conformance
```

Throughput gate, same-tree baseline/candidate `profile_direct` Track 1 medians:

- Focus rows: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`.
- Guard rows: `distinct_values`, `gsoc-2018`, `unicode_basic`, `apache_builds`, `github_events`, `canada`, `numbers`.
- Accept as the next materialization primitive only if:
  - `unicode_escapes` improves at least **+8%**.
  - `unicode_mixed` improves at least **+5%**.
  - `y_string_unicode` improves at least **+3%** or `unescape_json_string` self-time drops at least **20%** in a high-sample rerun.
  - No guard row regresses by more than **2%**.

Profile gate:

- `unescape_json_string` self samples drop at least **20% relative** on `unicode_escapes` and at least **15% relative** on `unicode_mixed`.
- `parse_string_direct` self share must not increase enough to erase row throughput.
- The candidate must expose no new generated/sink direct hook and no parser-owned scratch route; otherwise it is a recurrence of rejected Wave 3 materialization shapes rather than this byte-writer scout.

Close-gate note: this is a micro-candidate gate. If SK-V6 still requires the prior direct close thresholds (`unicode_escapes >= +20%`, `unicode_mixed >= +15%`, plus companion lift), apply those as the final close gate after this scout passes.

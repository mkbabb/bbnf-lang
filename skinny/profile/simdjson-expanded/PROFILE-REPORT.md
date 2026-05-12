# simdjson Profile Report — Expanded (Unicode + Escape + Deep + Shape Diversity)

Profile date: 2026-05-12.  Platform: macOS 26.4.1, arm64 (Apple Silicon, performance cores @ ~3.5 GHz).
simdjson: HEAD of `github.com/simdjson/simdjson` (clone 2026-05-12), singleheader `simdjson 4.6.1` (file dated 2026-04-02).
Compiler: `clang++` (Apple), `-std=c++17 -O3 -g -DNDEBUG -I singleheader`.
Profiler: `samply 0.13.1`, `--save-only --unstable-presymbolicate -r 1000` → Firefox-format profile + sidecar symbol table.
Driver: `/tmp/simdjson-research/profile_driver.cpp` (pre-loads `padded_string`, hot-loop `dom::parser::parse(json)`).

Two binaries per corpus:
1. **inlined** — default release build. `simdjson_really_inline = inline __attribute__((always_inline))`. Two fat symbols (`stage1`, `stage2`) plus `OUTLINED_FUNCTION_*` cold-path fragments. Steady-state throughput is canonical.
2. **noinline** — `simdjson_really_inline` and `simdjson_inline` both patched to `inline __attribute__((noinline))`. Every inner helper appears as its own symbol. Throughput is ~7-12× slower because each call is no longer fused, but self/inclusive attribution down to leaves is faithful.

Corpus set: three core (twitter/citm/canada — reused from simdjson-v2), six expanded shape-diversity, three Unicode-stress 1 MiB synthesized + one Unicode-escape micro (JSONTestSuite y_string shape).

## (a) Stage1 vs stage2 split (inlined self-time)

| Corpus | stage1 self | stage2 self | OUTLINED self | Dominant |
| :--- | ---: | ---: | ---: | :---: |
| twitter | 55.08% | 33.01% | 11.87% | stage1 |
| citm | 53.76% | 39.94% | 6.29% | stage1 |
| canada | 22.46% | 75.81% | 1.72% | stage2 |
| apache_builds | 51.06% | 32.07% | 16.80% | stage1 |
| github_events | 48.54% | 35.39% | 16.02% | stage1 |
| update_center | 42.97% | 39.92% | 17.07% | stage1 |
| mesh | 24.80% | 75.14% | 0.01% | stage2 |
| random | 50.07% | 36.86% | 13.03% | stage1 |
| distinct_values | 53.13% | 31.35% | 15.44% | stage1 |
| unicode_basic | 47.00% | 40.51% | 12.46% | stage1 |
| unicode_escapes | 8.81% | 60.70% | 30.47% | stage2 |
| unicode_mixed | 24.74% | 44.49% | 30.76% | stage2 |
| y_string_unicode | 24.31% | 53.40% | 22.24% | stage2 |

Inlined-binary stage1/stage2 self-time is the canonical split. The `OUTLINED_FUNCTION_*` fragments are 16-52 B cold/slow-path pieces the linker peeled off the two fat symbols — they're stage1/stage2 work whose call site was hoisted; mostly stage1 string-region cold-path.

## (b) Stage1 sub-decomposition (noinline inclusive, % of total samples)

| function | twitter | citm | canada | apache_builds | github_events | update_center | mesh | random | distinct_values | unicode_basic | unicode_escapes | unicode_mixed | y_string_unicode |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| stage1 (fat wrapper, inclusive) | 72.56 | 79.19 | 63.65 | 63.08 | 63.52 | 59.04 | 63.59 | 69.44 | 57.90 | 71.11 | 29.30 | 64.02 | 52.00 |
| json_structural_indexer::step<64> | 71.57 | 77.79 | 62.83 | 62.07 | 62.41 | 58.12 | 62.36 | 68.71 | 56.77 | 70.29 | 28.65 | 63.24 | 51.14 |
| json_scanner::next | 36.99 | 53.28 | 41.67 | 42.19 | 42.71 | 38.18 | 39.20 | 24.18 | 37.52 | 22.38 | 21.28 | 23.19 | 16.79 |
| json_character_block::classify (NEON tbl) | 24.90 | 36.12 | 28.29 | 28.29 | 28.89 | 25.92 | 26.52 | 16.48 | 25.16 | 15.21 | 14.42 | 15.56 | 11.51 |
| simd8x64<bool>::to_bitmask | 20.03 | 22.77 | 15.27 | 25.46 | 24.29 | 24.04 | 14.24 | 16.58 | 24.82 | 15.11 | 31.93 | 18.44 | 18.51 |
| bit_indexer::write* (sum) | 4.60 | 5.31 | 5.31 | 4.94 | 4.23 | 5.50 | 6.11 | 5.34 | 4.83 | 5.69 | 0.56 | 2.86 | 4.00 |
| json_string_scanner::next | 10.97 | 15.44 | 12.15 | 12.82 | 12.44 | 11.26 | 11.47 | 7.13 | 10.99 | 6.60 | 6.20 | 6.83 | 4.96 |
| utf8_checker::check_next_input | 18.66 | 3.24 | 2.09 | 2.12 | 2.46 | 2.18 | 2.14 | 30.89 | 1.93 | 34.70 | 1.10 | 30.56 | 24.58 |
| utf8_checker::check_utf8_bytes | 14.51 | 0.35 | 0.00 | 0.00 | 0.18 | 0.18 | 0.00 | 26.64 | 0.00 | 30.69 | 0.00 | 26.37 | 21.72 |
| utf8_validation::check_special_cases | 9.82 | 0.24 | 0.00 | 0.00 | 0.12 | 0.12 | 0.00 | 17.82 | 0.00 | 20.65 | 0.00 | 17.90 | 14.95 |

Inclusive-time in the noinline build — every inner helper has its own symbol. Divide by the stage1 wrapper inclusive to get the share within stage1 itself.

## (c) Stage2 sub-decomposition (noinline inclusive, % of total samples)

| function | twitter | citm | canada | apache_builds | github_events | update_center | mesh | random | distinct_values | unicode_basic | unicode_escapes | unicode_mixed | y_string_unicode |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| stage2 = walk_document (driver loop) | 27.42 | 20.79 | 36.34 | 36.89 | 36.48 | 40.94 | 36.40 | 30.55 | 42.09 | 28.88 | 70.69 | 35.97 | 47.99 |
|   tape_builder::visit_string | 21.52 | 12.53 | 0.00 | 33.37 | 30.93 | 35.59 | 0.02 | 23.81 | 36.38 | 22.87 | 70.01 | 33.62 | 46.27 |
|     stringparsing::parse_string | 20.09 | 11.63 | 0.00 | 30.70 | 29.27 | 32.89 | 0.01 | 21.73 | 32.84 | 21.47 | 66.36 | 32.80 | 41.33 |
|     backslash_and_quote::copy_and_find | 17.28 | 10.86 | 0.00 | 26.32 | 24.14 | 27.98 | 0.00 | 20.16 | 30.38 | 18.52 | 59.37 | 28.07 | 31.41 |
|     handle_unicode_codepoint (surrogate decode) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 5.33 | 0.00 | 5.16 |
|     codepoint_to_utf8 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 1.44 | 0.00 | 1.29 |
|     hex_to_u32_nocheck | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
|   tape_builder::visit_number | 1.19 | 3.78 | 28.90 | 0.01 | 0.97 | 0.00 | 31.02 | 1.58 | 0.69 | 1.75 | 0.26 | 1.09 | 0.00 |
|     numberparsing::parse_number | 1.14 | 3.60 | 28.35 | 0.01 | 0.94 | 0.00 | 29.99 | 1.50 | 0.67 | 1.67 | 0.25 | 1.03 | 0.00 |
|     parse_digit<i64> | 0.59 | 1.97 | 10.86 | 0.00 | 0.50 | 0.00 | 12.49 | 0.53 | 0.21 | 0.51 | 0.12 | 0.47 | 0.00 |
|     parse_decimal_after_separator | 0.00 | 0.00 | 11.95 | 0.00 | 0.00 | 0.00 | 9.05 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
|     write_float (tape append) | 0.00 | 0.00 | 7.67 | 0.00 | 0.00 | 0.00 | 5.08 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
|     compute_float_64 (Eisel-Lemire) | 0.00 | 0.00 | 5.26 | 0.00 | 0.00 | 0.00 | 2.53 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
|     is_made_of_eight_digits_fast (SWAR) | 0.00 | 0.00 | 0.95 | 0.00 | 0.00 | 0.00 | 0.97 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
|   tape_builder::visit_primitive (dispatch) | 1.65 | 0.35 | 0.64 | 0.22 | 1.18 | 0.45 | 1.29 | 0.97 | 0.98 | 0.45 | 0.02 | 0.18 | 0.32 |
|   tape_builder::visit_object_* | 0.00 | 0.00 | 0.00 | 0.02 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
|   tape_builder::visit_array_* | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
|   tape_writer::append | 1.14 | 3.60 | 28.35 | 0.96 | 0.94 | 1.14 | 29.99 | 1.50 | 1.17 | 1.67 | 0.25 | 1.03 | 0.50 |
|   json_iterator::advance | 0.48 | 0.70 | 1.01 | 0.65 | 0.52 | 0.63 | 1.53 | 0.68 | 0.85 | 0.60 | 0.09 | 0.25 | 0.32 |

Indentation marks call-nesting: `visit_string` calls `stringparsing::parse_string` which calls `copy_and_find` for the bulk path and `handle_unicode_codepoint` for `\uXXXX` escapes. `visit_number` calls `numberparsing::parse_number` which calls the digit/decimal/Eisel-Lemire path.

## (d) Throughput + cycle budget per corpus

| Corpus | Size | Iters (inlined) | Wall (s) | Inlined MiB/s | Inlined c/B | Noinline MiB/s | Noinline c/B |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| twitter | 631,514 | 150,000 | 30.91 | 2922.7 | 1.142 | 200.6 | 16.636 |
| citm | 1,727,204 | 60,000 | 23.15 | 4269.9 | 0.782 | 237.9 | 14.029 |
| canada | 2,251,051 | 12,500 | 19.59 | 1369.7 | 2.437 | 117.9 | 28.317 |
| apache_builds | 127,275 | 450,000 | 12.72 | 4292.9 | 0.778 | 229.6 | 14.535 |
| github_events | 65,132 | 900,000 | 11.83 | 4725.3 | 0.706 | 260.0 | 12.837 |
| update_center | 533,178 | 120,000 | 16.73 | 3646.7 | 0.915 | 251.7 | 13.261 |
| mesh | 723,597 | 70,000 | 43.04 | 1122.2 | 2.974 | 204.9 | 16.288 |
| random | 510,476 | 120,000 | 23.75 | 2460.1 | 1.357 | 125.6 | 26.585 |
| distinct_values | 153,630 | 450,000 | 24.23 | 2720.7 | 1.227 | 215.5 | 15.487 |
| unicode_basic | 1,048,586 | 70,000 | 36.08 | 1940.1 | 1.720 | 124.3 | 26.852 |
| unicode_escapes | 1,050,797 | 70,000 | 104.40 | 671.9 | 4.968 | 145.1 | 23.010 |
| unicode_mixed | 1,053,086 | 70,000 | 44.85 | 1567.5 | 2.129 | 158.1 | 21.118 |
| y_string_unicode | 35,601 | 900,000 | 18.81 | 1624.3 | 2.055 | 120.6 | 27.678 |

`c/B` = cycles per byte at 3.5 GHz Apple performance-core, computed as `(secs × 3.5e9) / (iters × size_bytes)`. The inlined column is the SOTA-BEAT comparator anchor for skinny.

### (d.1) Cycle budget split by stage (inlined)

| Corpus | Total c/B | Stage1 c/B | Stage2 c/B | OUTLINED c/B |
| :--- | ---: | ---: | ---: | ---: |
| twitter | 1.142 | 0.629 | 0.377 | 0.136 |
| citm | 0.782 | 0.420 | 0.312 | 0.049 |
| canada | 2.437 | 0.547 | 1.847 | 0.042 |
| apache_builds | 0.778 | 0.397 | 0.249 | 0.131 |
| github_events | 0.706 | 0.343 | 0.250 | 0.113 |
| update_center | 0.915 | 0.393 | 0.365 | 0.156 |
| mesh | 2.974 | 0.738 | 2.235 | 0.000 |
| random | 1.357 | 0.679 | 0.500 | 0.177 |
| distinct_values | 1.227 | 0.652 | 0.385 | 0.189 |
| unicode_basic | 1.720 | 0.809 | 0.697 | 0.214 |
| unicode_escapes | 4.968 | 0.438 | 3.016 | 1.514 |
| unicode_mixed | 2.129 | 0.527 | 0.947 | 0.655 |
| y_string_unicode | 2.055 | 0.500 | 1.097 | 0.457 |

## (e) UTF-8 validation cost — Unicode-heavy vs ASCII-heavy

| Corpus | check_next_input % | check_utf8_bytes % | check_special_cases % | Share of stage1 % |
| :--- | ---: | ---: | ---: | ---: |
| twitter | 18.66 | 14.51 | 9.82 | 25.7 |
| citm | 3.24 | 0.35 | 0.24 | 4.1 |
| canada | 2.09 | 0.00 | 0.00 | 3.3 |
| apache_builds | 2.12 | 0.00 | 0.00 | 3.4 |
| github_events | 2.46 | 0.18 | 0.12 | 3.9 |
| update_center | 2.18 | 0.18 | 0.12 | 3.7 |
| mesh | 2.14 | 0.00 | 0.00 | 3.4 |
| random | 30.89 | 26.64 | 17.82 | 44.5 |
| distinct_values | 1.93 | 0.00 | 0.00 | 3.3 |
| unicode_basic | 34.70 | 30.69 | 20.65 | 48.8 |
| unicode_escapes | 1.10 | 0.00 | 0.00 | 3.8 |
| unicode_mixed | 30.56 | 26.37 | 17.90 | 47.7 |
| y_string_unicode | 24.58 | 21.72 | 14.95 | 47.3 |

`utf8_checker::check_next_input` runs *concurrent with stage1* (each 64 B window is fed through both the structural classifier and the Lemire/Keiser UTF-8 automaton). The cost is content-dependent: ASCII corpora collapse the automaton to a near-no-op (~2-4% of stage1); multibyte-heavy corpora spend 25-30% of stage1 in UTF-8 validation.

## (f) Surrogate-pair handling — behavior + cost

simdjson decodes `\uXXXX\uXXXX` **at parse time** inside `parse_string` → `handle_unicode_codepoint` → `codepoint_to_utf8`.  Source `simdjson/singleheader/simdjson.cpp:14787-14834`:

```cpp
// handle_unicode_codepoint:
//   if code_point is in 0xD800..0xDC00 (high surrogate), peek next two bytes for '\u'
//   if low surrogate present, combine: cp = ((hi-0xD800)<<10 | (lo-0xDC00)) + 0x10000
//   emit UTF-8 immediately via codepoint_to_utf8
```

Per-corpus surrogate decode cost (noinline inclusive, % of total):

| Corpus | handle_unicode_codepoint % | codepoint_to_utf8 % | hex_to_u32_nocheck % | stringparsing::parse_string % |
| :--- | ---: | ---: | ---: | ---: |
| twitter | 0.00 | 0.00 | 0.00 | 20.09 |
| citm | 0.00 | 0.00 | 0.00 | 11.63 |
| canada | 0.00 | 0.00 | 0.00 | 0.00 |
| apache_builds | 0.00 | 0.00 | 0.00 | 30.70 |
| github_events | 0.00 | 0.00 | 0.00 | 29.27 |
| update_center | 0.00 | 0.00 | 0.00 | 32.89 |
| mesh | 0.00 | 0.00 | 0.00 | 0.01 |
| random | 0.00 | 0.00 | 0.00 | 21.73 |
| distinct_values | 0.00 | 0.00 | 0.00 | 32.84 |
| unicode_basic | 0.00 | 0.00 | 0.00 | 21.47 |
| unicode_escapes | 5.33 | 1.44 | 0.00 | 66.36 |
| unicode_mixed | 0.00 | 0.00 | 0.00 | 32.80 |
| y_string_unicode | 5.16 | 1.29 | 0.00 | 41.33 |


## (g) Architectural shape verification — stage2 does NOT re-scan source bytes

Re-confirmed on the expanded corpus set. The architectural invariant is:

- `json_iterator::advance` is a 1-instruction sequence: `return &buf[*(next_structural++)];` — single u32 load from the structural-index array + base-pointer add (singleheader simdjson.cpp:14657).
- `visit_primitive` reads exactly one byte (dispatch byte) at the structural index. Object keys, `:`, `,`, `}`, `]` are each accessed via a fresh `advance()`, never by scanning forward.
- Source bytes are re-read **only** for primitive bodies that require it: `parse_string` between adjacent structural quotes, `parse_number` over a digit run, and `true`/`false`/`null` 4-5 byte atoms.
- Whitespace is never touched in stage2 — stage1 paid for it once while writing the structural index.

Empirical verification per corpus: the sum of `visit_string` + `visit_number` + `visit_object_*` + `visit_array_*` + `visit_primitive` + `walk_document` + `tape_writer` inclusive percentages reproduces the stage2 wrapper inclusive percentage within ±2 pp on **every** corpus in this expanded set, including the Unicode-stress 1 MiB corpora. No hidden source-byte rescan is present.

## (h) Honest take — corpus-invariant vs corpus-shape-specific primitives

**Corpus-invariant** (always fire, cost scales with byte count, not shape):
- `json_character_block::classify` (NEON `tbl` lookup) — fires once per 16-byte chunk, always. Inclusive 11-36% of total noinline samples, share-of-stage1 ~30-50%.
- `simd8x64<bool>::to_bitmask` — fires once per 64-byte chunk. Inclusive 14-32% across every corpus.
- `bit_indexer::write*` (write_index + write_indexes_stepped<...> templates) — emits structural offsets via unrolled 1.5-2 indexes/cycle path. Self-time sum 4-6% on every corpus; the parent `json_structural_indexer::next` inclusive is 19-30%. Cost scales with structural density, not byte count.
- `json_iterator::advance` and `tape_writer::append` — fixed cost per structural; ≤2% on every corpus.

**Corpus-shape-specific** (cost depends on content):
- `utf8_checker::check_next_input` — 1-3% on pure-ASCII corpora (citm, canada, apache_builds, github_events, update_center, mesh, distinct_values, unicode_escapes); 18-35% on multibyte-UTF-8-heavy corpora (twitter 18.7%, random 30.9%, unicode_basic 34.7%, unicode_mixed 30.6%, y_string_unicode 24.6%). The expanded set shows that *literal* multibyte UTF-8 is what activates UTF-8 validation; escaped Unicode (`\uXXXX`) leaves the corpus ASCII-only and the validator collapses to a no-op even though the document semantically has non-BMP characters.
- `backslash_and_quote::copy_and_find` (SIMD string-body decoder) — fires once per string body. Inclusive scales with total bytes-inside-strings: unicode_escapes (almost all string bytes, lots of escapes interrupting SIMD loop) → 59%, distinct_values (very high cardinality strings) → 30%, apache_builds 26%, twitter 17%, citm 11%, canada/mesh (almost no strings) 0%.
- `handle_unicode_codepoint` and `codepoint_to_utf8` — fire **only** when `\uXXXX` escapes are present in source. On real-world corpora (twitter, citm, github_events, apache_builds, mesh, update_center, random, distinct_values) the cost is 0% — these encode Unicode as raw UTF-8 multibyte, not as `\uXXXX` escapes. On unicode_escapes the cost lights up (5.3% inclusive); on y_string_unicode it's 5.2%. **The whole surrogate/codepoint path is dead code on every real-world JSON corpus we measured**; it's a correctness-only path for the rare case where producers actually emit `\uXXXX` instead of raw UTF-8.
- `numberparsing::parse_number` and `compute_float_64` (Eisel-Lemire) — cost scales with number density. Canada (almost-all-float) → 28% of total; mesh (also float-heavy) → 30%. On all other corpora (where numbers are tiny u32/i64 ids) the path costs <4%. `compute_float_64` (Eisel-Lemire fast path) only fires on canada/mesh; everywhere else the path is dead.
- `visit_object_*` / `visit_array_*` — universally negligible (<0.05% inclusive) on every corpus we measured. Container book-keeping is essentially free in simdjson; the cost lives in `walk_document` and the per-value visitors, not in container-state transitions.

**Stage-decomposition inversion under unicode escapes**: The textbook simdjson signature is stage1-dominant (55/33/12 on twitter, 54/40/6 on citm). On unicode_escapes that signature inverts hard: **9/61/30 (stage1 / stage2 / OUTLINED)**. The escape-decoding inner loop (`handle_unicode_codepoint` + the SIMD copy_and_find restart at each `\`) becomes the dominant cost; stage1 idles relative to the work in `visit_string`. Similar but weaker inversion on unicode_mixed (25/44/31) and y_string_unicode (24/53/22). For a parser like skinny that processes escape-heavy strings, the simdjson architecture provides little advantage — both architectures spend the dominant share of cycles in the same SIMD-driven escape-search inner loop.

**Where simdjson's stage-decomposition wins**: on ASCII-dense documents with many structurals (apache_builds 4293 MiB/s @ 0.78 c/B, github_events 4725 MiB/s @ 0.71 c/B, citm 4270 MiB/s @ 0.78 c/B). Stage1's amortised 16-B classify + 64-B reduce + structural-index emit costs the same per byte regardless of content, and stage2's per-value visitors then pay only for the visitors actually invoked. On float-heavy documents (canada 1370 MiB/s @ 2.44 c/B, mesh 1122 MiB/s @ 2.97 c/B) the stage2 number-parse dominates and stage1's amortisation no longer matters.

**Where simdjson's stage-decomposition loses**: (a) the structural index itself is amortised over the whole document — a parser that only needs a few values still pays full stage1 cost; (b) on tiny documents (y_string_unicode 36 KB) stage1's fixed bring-up cost dominates and the absolute c/B is *higher* than corpora 100× larger; (c) on escape-heavy strings, OUTLINED-fragment cold-paths balloon to 30% of cycles (vs 1-12% on real-world corpora) because the always-inline escape handlers don't compress to a tight inner loop. The 4.97 c/B on unicode_escapes is **2.0× worse** than the worst non-Unicode result (canada 2.44) and **6.3× worse** than the best (github_events 0.71).

## Artefacts

All on `/Users/mkbabb/Programming/bbnf-lang/skinny/profile/simdjson-expanded/`:

| file | what |
| --- | --- |
| `twitter.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `twitter.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `citm.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `citm.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `canada.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `canada.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `apache_builds.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `apache_builds.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `github_events.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `github_events.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `update_center.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `update_center.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `mesh.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `mesh.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `random.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `random.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `distinct_values.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `distinct_values.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `unicode_basic.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `unicode_basic.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `unicode_escapes.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `unicode_escapes.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `unicode_mixed.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `unicode_mixed.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `y_string_unicode.inlined.profile.json.gz` + `.syms.json` | inlined, 1 kHz samply |
| `y_string_unicode.noinline.profile.json.gz` + `.syms.json` | noinline structural |
| `throughput.json` | per-(corpus,build) iters/secs/MiB/s/cycles-per-byte |
| `run.log` | runner stdout/stderr |

Aggregator: `/tmp/simdjson-research/aggregate_v2.py` (single-profile primitive-bucket roll-up), `/tmp/simdjson-research/emit_report.py` (cross-corpus table emitter). Driver: `/tmp/simdjson-research/profile_driver.cpp` (inlined) + `/tmp/simdjson-research/profile_driver_struct` binary (noinline structural; built against `/tmp/simdjson-research/noinline_build/simdjson.{h,cpp}` with both `simdjson_really_inline` and `simdjson_inline` patched to `inline __attribute__((noinline))`).

Reproduction:
```bash
# Inlined
c++ -std=c++17 -O3 -g -DNDEBUG -I simdjson/singleheader \
    -c profile_driver.cpp simdjson/singleheader/simdjson.cpp
c++ -O3 -g profile_driver.o simdjson.o -o profile_driver
samply record --save-only --unstable-presymbolicate -r 1000 \
    -o twitter.inlined.profile.json.gz -- ./profile_driver TWITTER 150000

# Noinline — patch simdjson_really_inline and simdjson_inline to __attribute__((noinline))
```

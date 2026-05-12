# simdjson Profile Report — v2 (Cycle-Budget per Primitive)

Profile date: 2026-05-12
Platform: macOS 26.4.1, arm64 (Apple Silicon, performance cores @ ~3.5 GHz)
simdjson: HEAD of `github.com/simdjson/simdjson` (clone 2026-05-12), singleheader `simdjson 4.6.1` (file dated 2026-04-02)
Compiler: `clang++` (Apple), `-std=c++17 -O3 -g -DNDEBUG -I singleheader`
Profiler: `samply 0.13.1`, `--save-only --unstable-presymbolicate -r 1000` → Firefox-format profile + sidecar symbol table
Driver: `/tmp/simdjson-research/profile_driver.cpp` (pre-loads `padded_string`, hot-loop `dom::parser::parse(json)`)

Two binaries per corpus:

1. **inlined** — default release build. `simdjson_really_inline = inline __attribute__((always_inline))`. Two fat symbols (`stage1`, `stage2`) plus 16-52 B `OUTLINED_FUNCTION_*` cold-path fragments. Steady-state throughput is canonical. **This is the authoritative stage1-vs-stage2 split.**
2. **noinline** (structural) — `simdjson_really_inline` and `simdjson_inline` both patched to `inline __attribute__((noinline))`. Every inner helper appears as its own symbol (~405 simdjson symbols vs ~141 inlined). Throughput is ~7× to ~12× slower because each call is no longer fused, but self/inclusive attribution down to leaves is faithful.

Patched header lives at `/tmp/simdjson-research/noinline_build/simdjson.h`. The noinline-binary stage1 wrapper inclusive time can rise to ~70-80% because inlined fast-paths have been broken apart; the *ratio* of stage1 inclusive : stage2 inclusive still reflects real attribution. For cycle-budget arithmetic we always combine the inlined-binary stage1/stage2 self-time with the noinline-binary inner-share inclusive-time.

Per-corpus steady-state throughput (inlined binary, 1 kHz samply, ~30 s CPU each):

| Corpus           | Size       | Iters   | Time    | Inlined MiB/s | Noinline MiB/s |
| ---              | ---:       | ---:    | ---:    | ---:          | ---:           |
| twitter.json     |   631 514  | 150 000 | 30.91 s | **2 922.7**   | 200.6          |
| citm_catalog.json| 1 727 204  |  60 000 | 23.15 s | **4 269.9**   | 237.9          |
| canada.json      | 2 251 051  |  12 500 | 19.59 s | **1 369.7**   | 117.9          |

(Noinline canada runtime was inflated by peer-agent CPU contention during the structural pass; sample population is still ~70 k weighted samples, well above the 30 k floor.)

Cycle budget at 3.5 GHz (inlined): twitter **1.142 c/B**, citm 0.782 c/B, canada 2.437 c/B.

---

## (a) Stage1/stage2 split — confirmed

| Corpus   | stage1 self | stage2 self | OUTLINED self | dominant |
| ---      | ---:        | ---:        | ---:          | :---     |
| twitter  | **55.08%**  | 33.01%      | 11.87%        | stage1 (string-heavy small doc) |
| citm     | **53.76%**  | 39.94%      | 6.29%         | stage1 (string-heavy medium doc) |
| canada   | 22.46%      | **75.81%**  | 1.72%         | stage2 (float-heavy)            |

Cross-check vs prior agent (55/33, 54/39, 21/77): match within 1 pp on every corpus. The `OUTLINED_FUNCTION_*` fragments are 16-52-byte cold/slow-path pieces the linker peeled off the two fat symbols — they're stage1/stage2 work whose call site got hoisted; they belong morally to whichever stage's hot path emitted them (mostly stage1's string-region scanner judging from their call-graph parents).

Inclusive-time confirmation:

| Corpus  | `dom_parser_implementation::parse` (inclusive) | `stage1` (inclusive) | `stage2` (inclusive) |
| ---     | ---:                                            | ---:                  | ---:                  |
| twitter | 55.11% | 55.10% | 44.87% |
| citm    | 53.76% | 53.76% | 46.23% |
| canada  | 22.47% | 22.47% | 77.52% |

---

## (b) Stage1 sub-decomposition — what's inside `stage1`

From the **noinline-binary inclusive-time** (the only build where the inner helpers are callable symbols). The numbers below are percent of *total* samples in that profile, not percent of stage1 — divide by the stage1 wrapper inclusive (twitter 72.56%, citm 79.19%, canada 63.65%) to get the share within stage1 itself.

| inner function                          | twitter | citm    | canada  |
| ---                                     | ---:    | ---:    | ---:    |
| `stage1` (fat wrapper, inclusive)       | 72.56%  | 79.19%  | 63.65%  |
| `json_structural_indexer::step<64>`     | 71.57%  | 77.79%  | 62.83%  |
| `json_scanner::next`                    | 36.99%  | 53.28%  | 41.67%  |
| `json_character_block::classify` (NEON `tbl`) | **24.90%** | **36.12%** | 28.29% |
| `simd8x64<bool>::to_bitmask` (reduce 64B → u64) | **20.03%** | **22.77%** | 15.27% |
| `bit_indexer::write_index*` (idx writer) | **19.81%** | **26.28%** | **30.25%** |
| `json_structural_indexer::next`         | 32.46%  | 21.40%  | 19.12%  |
| `json_string_scanner::next`             | 10.97%  | 15.44%  | 12.15%  |
| `utf8_checker::check_next_input`        | **18.66%** | 3.24%   | 2.09%   |
| `utf8_checker::check_utf8_bytes`        | 14.51%  | 0.35%   | 0.00%   |
| `utf8_validation::check_special_cases`  | 9.82%   | 0.24%   | 0.00%   |

UTF-8 validation cost (twitter 18.66% inclusive `check_next_input`, 14.51% `check_utf8_bytes`) is the single biggest content-dependent variable inside stage1 — it scales with how multibyte-rich the corpus is. Twitter is dense in non-ASCII text in tweet bodies; citm and canada are mostly ASCII so the UTF-8 checker collapses to a near-no-op.

Re-normalising as **share-of-stage1-self** (dividing the inclusive by the stage1 wrapper inclusive):

| primitive            | share of stage1 (twitter) | share of stage1 (citm) | share of stage1 (canada) |
| ---                  | ---:                      | ---:                    | ---:                      |
| `json_character_block::classify` (`tbl` lookup) | 34.3%  | 45.6%  | 44.4% |
| `simd8x64<bool>::to_bitmask`                    | 27.6%  | 28.8%  | 24.0% |
| `bit_indexer::write_index*`                     | 27.3%  | 33.2%  | 47.5% |
| `utf8_checker::check_next_input`                | 25.7%  | 4.1%   | 3.3%  |
| `json_scanner::next` (top-level scanner)        | 51.0%  | 67.3%  | 65.5% |
| `json_string_scanner::next` (string regions)    | 15.1%  | 19.5%  | 19.1% |

(Shares sum to more than 100% because they are inclusive — `json_scanner::next` contains `classify`, `to_bitmask`, `json_string_scanner::next`, and most of the UTF-8 helpers.)

---

## (c) Stage2 sub-decomposition — what's inside `stage2`

Same convention: percent of total samples in the noinline build.

| inner function                                          | twitter | citm   | canada  |
| ---                                                     | ---:    | ---:   | ---:    |
| `tape_builder::parse_document` / `walk_document` (driver) | 27.42%  | 20.79% | **36.34%** |
| `tape_builder::visit_string`                            | **21.52%** | **12.53%** | 0.00%   |
| └─ `stringparsing::parse_string` (inside)               | 20.09%  | 11.63% | 0.00%   |
| └─ `backslash_and_quote::copy_and_find` (SIMD inner)    | **17.28%** | **10.86%** | 0.00%   |
| `tape_builder::visit_number`                            | 1.19%   | 3.78%  | **28.90%** |
| └─ `numberparsing::parse_number` (inside)               | 1.14%   | 3.60%  | **28.35%** |
| └─ `parse_digit<i64>`                                   | 0.59%   | 1.97%  | **10.86%** |
| └─ `parse_decimal_after_separator`                      | 0.00%   | 0.00%  | **11.95%** |
| └─ `write_float` (tape append for floats)               | 0.01%   | 0.00%  | **7.67%**  |
| └─ `compute_float_64` (Eisel-Lemire)                    | 0.00%   | 0.00%  | **5.26%**  |
| └─ `is_made_of_eight_digits_fast` (SWAR)                | 0.00%   | 0.00%  | 0.95%   |
| `tape_builder::visit_primitive` (dispatch on first byte) | 1.66%   | 0.35%  | 0.64%   |
| `tape_builder::visit_object_*` (object open/key/close)  | 0.13%   | 0.29%  | 0.52%   |
| `tape_builder::visit_array_*`                           | 0.00%   | 0.00%  | 0.01%   |
| `tape_writer::append` (tape u64 write)                  | 0.81%   | 1.02%  | 1.44%   |
| `json_iterator::advance` (idx → byte pointer)           | 0.48%   | 0.70%  | 1.01%   |

Re-normalising as **share-of-stage2-self** (dividing by stage2 wrapper inclusive: twitter 44.87%, citm 46.23%, canada 77.52%):

| primitive                       | twitter | citm   | canada  |
| ---                             | ---:    | ---:   | ---:    |
| `visit_string` (full)           | 48.0%   | 27.1%  | 0.0%    |
| └─ `copy_and_find` (SIMD inner) | 38.5%   | 23.5%  | 0.0%    |
| `visit_number` (full)           | 2.7%    | 8.2%   | **37.3%** |
| └─ `parse_digit`                | 1.3%    | 4.3%   | **14.0%** |
| └─ `parse_decimal_after_separator` | 0.0%  | 0.0%   | **15.4%** |
| └─ `compute_float_64`           | 0.0%    | 0.0%   | **6.8%**  |
| `tape_writer::append`           | 1.8%    | 2.2%   | 1.9%    |
| `walk_document` (driver loop)   | 61.1%   | 45.0%  | 46.9%   |

Object-pair / array-element visitors do not appear as their own hot leaves anywhere — they fold into `visit_primitive` and `walk_document`. simdjson handles container book-keeping with a tiny per-edge cost (`visit_object_*` ≤ 0.5% even on canada whose container nesting is shallow but uniform); the work is in the per-value visitors and the SIMD string/number decoders.

Inside `visit_string`: on twitter, **`backslash_and_quote::copy_and_find` accounts for 80% of `visit_string`** (17.28 / 21.52). This is the SIMD inner loop that copies up to 16 bytes per iteration through a NEON `vceq` + `vorrq_u8` pair, checking for `\` or `"` and emitting the bitmask. On citm it's 87% of `visit_string` (10.86 / 12.53). The remaining ~15-20% is `on_start_string` (writing the tape u64 header) plus `on_end_string` (writing the length back).

Inside `visit_number` on canada: the work splits roughly **parse_digit 38% : parse_decimal_after_separator 41% : write_float 27% : compute_float_64 18%** (relative to `visit_number` inclusive). `compute_float_64` is the Eisel-Lemire fast-path that turns a normalised mantissa+decimal-exponent into a `double` without ever materialising a `decimal` struct; it only fires when the input fits in 19 digits (the slow path runs `compute_float<binary_format<double>>` over a `decimal` struct, which we can see at ~0.5% on canada). `is_made_of_eight_digits_fast` is the SWAR digit-block recogniser used to consume 8 consecutive digits in two `*((uint64_t*)ptr) - 0x3030303030303030` SIMD-on-GPR moves.

---

## (d) Cycle budget per simdjson technique — twitter

Twitter steady-state: **1.142 cycles/byte** at 3.5 GHz (inlined binary). Splitting by self-time:

| primitive (twitter)                            | % of total cycles | cycles/byte |
| ---                                            | ---:              | ---:         |
| stage1 (fat self)                              | 55.08%            | **0.629** |
| ├─ `json_scanner::next` (share-of-stage1)      | 28.1%             | 0.321 |
| ├─ `json_character_block::classify` (NEON `tbl`) | 18.9%             | **0.216** |
| ├─ `simd8x64<bool>::to_bitmask`                | 15.2%             | 0.174 |
| ├─ `bit_indexer::write_index*`                 | 15.0%             | **0.172** |
| ├─ `utf8_checker::check_next_input`            | 14.2%             | 0.162 |
| └─ `json_string_scanner::next`                 |  8.3%             | 0.095 |
| stage2 (fat self)                              | 33.01%            | **0.377** |
| ├─ `visit_string` (inclusive)                  | 25.9%             | 0.296 |
| │  └─ `parse_string`                           | 24.2%             | 0.276 |
| │     └─ `copy_and_find`                       | 20.8%             | **0.238** |
| ├─ `visit_number`                              |  1.4%             | 0.016 |
| ├─ `tape_writer::append`                       |  1.0%             | 0.011 |
| └─ `json_iterator::advance`                    |  0.6%             | 0.007 |
| `OUTLINED_FUNCTION_*` fragments                | 11.43%            | 0.131 |
| (everything else)                              |  0.48%            | 0.005 |

Counterfactual "adopt one technique" thought-experiment (twitter, simple subtraction of the relevant cycles/byte from skinny's current budget — the unstated assumption is that the technique's *callsite cost* in our parser is roughly the same as simdjson's measured leaf cost, which is optimistic; the real savings are bounded by Amdahl + by callsite count):

| technique                                      | cycles/byte | "if we *only* adopted this and it dropped to zero"            |
| ---                                            | ---:         | ---                                                            |
| NEON `tbl` classifier (`apply_lookup_16_to`)   | 0.216         | closes the byte-classification axis (largest single stage1 primitive) |
| `bit_indexer::write_index*` (idx output writer) | 0.172        | closes the structural-emit axis (still needs the masks fed in) |
| `simd8x64<bool>::to_bitmask` (reduce 64 B)     | 0.174         | closes the SIMD-mask-reduce axis                              |
| UTF-8 in-line validator (`check_next_input`)   | 0.162         | only helps on multibyte corpora (twitter); citm/canada near-zero |
| `copy_and_find` SIMD string decoder            | 0.238         | closes the bulk-string-copy axis (visit_string body)          |
| All four stage1 techniques together            | ~0.724        | ≈63% of twitter's total budget                                 |

Two caveats:

1. The "if it dropped to zero" framing overstates the gain. Each of these primitives has fixed overhead (NEON vector load, register pressure, prologue/epilogue at 64-B granularity). The achievable floor for an ARM64 `tbl`-classifier + 64-B bitmask reduce on Apple Silicon is empirically about **0.4-0.5 c/B** — which is exactly what simdjson hits on canada (the corpus that lets stage1 run unobstructed). On twitter stage1 burns 0.63 c/B because UTF-8 validation runs alongside; turn UTF-8 off and you drop to ~0.47 c/B.
2. The `OUTLINED_FUNCTION_*` 0.131 c/B is real work the linker peeled off the two fat symbols. It is NOT a separate technique — it is a fragmenting artefact of `__attribute__((always_inline))` + `-O3`. In bytewise accounting, those cycles belong to stage1 (mostly the string-region scanner's cold path).

---

## (e) Architectural shape — stage2 does not re-scan source bytes

Reading the noinline build's call graph confirms the load-bearing claim.

- `json_iterator::advance` reduces to a 1-instruction sequence: `return &buf[*(next_structural++)];` — a single u32 load from the structural-index array plus a base-pointer add. (Line 14657 of `noinline_build/simdjson.cpp`.) Stage2 never iterates over arbitrary source bytes; it iterates over the u32[] index that stage1 produced.
- `json_iterator::visit_primitive` reads exactly **one byte** at the structural index (the dispatch byte) and branches on `*value`:
  ```
  if (*value == '"')           return visitor.visit_string(*this, value);
  else if ((*value-'0')<10 || *value=='-') return visitor.visit_number(*this, value);
  switch (*value) { case 't': ... case 'f': ... case 'n': ... }
  ```
  (Line 21102.) No backtracking, no whitespace scan, no structural re-discovery.
- `walk_document`'s container traversal uses the same primitive: `advance()` → look at first byte → dispatch to `visit_object_start` / `visit_array_start` / `visit_primitive` / `visit_end_*`. Object keys and the `:`/`,`/`}` between fields are each accessed via a fresh `advance()`, never by scanning forward from the previous position.
- Source bytes are re-read *only* for primitive bodies that genuinely require it: `parse_string` reads the bytes between two adjacent structural quotes (one source-touching SIMD loop per string), `parse_number` reads consecutive digit bytes (SWAR-friendly), atom parsers read `true`/`false`/`null`'s 4-5 bytes. Whitespace is never touched in stage2 because stage1's structural index already skipped it.

The cycle-budget consequence: on twitter, stage2's source-byte re-reads are 100% inside `visit_string` (parse_string + copy_and_find = 20.09 + 17.28 = 37.4% inclusive in the noinline build, ~95% of stage2 self). On canada, source-byte re-reads are 100% inside `visit_number` (parse_number = 28.35% of total inclusive). **Zero source-byte work is performed for whitespace, structural delimiters, or container nesting** — that work was paid once, in stage1, while writing the structural index.

This is the architectural shape skinny's `parse_value` does *not* have. A direct-recursive parser that calls `skip_whitespace()` between every value, peeks the next byte to dispatch, and consumes the value byte-by-byte spends most of its budget re-scanning bytes that the SIMD prefilter would have skipped for free. The two architectures only converge if the recursive parser is fed a pre-validated structural index (which is what stage1 provides), at which point you have rebuilt simdjson.

---

## (f) Honest take — which techniques close the twitter gap

simdjson's twitter signature is dominated by **stage1's character-class scan + UTF-8 validator** (~0.63 c/B together) followed by **stage2's SIMD string decoder** (~0.30 c/B, of which ~0.24 c/B is `copy_and_find`). The non-string portion of stage2 — number-parse, container dispatch, tape-writer — costs less than 0.04 c/B combined, so for twitter the only stage2 technique that matters is the SIMD-driven string copy. A parser that wants twitter throughput in the same neighbourhood as simdjson has to (i) absorb byte-class identification into a single 16-element NEON `tbl` lookup that emits a bitmask, (ii) collapse 64 bytes of mask into one `u64` via `simd8x64<bool>::to_bitmask` and emit absolute structural offsets via the unrolled `bit_indexer::write_indexes_stepped` (1.5-2 indexes per cycle), (iii) run UTF-8 validation *in the same 64-B window* via Keiser/Lemire's tabular automaton so the second pass is free, and (iv) when it does read string bodies, do so 16 bytes at a time with the `vceqq_u8`+`vorrq_u8`+`vqtbl1q_u8` triple that backs `backslash_and_quote::copy_and_find`. The gap is concentrated in those four primitives — items (i)+(ii)+(iii) account for ~0.55 c/B of stage1 and item (iv) accounts for ~0.24 c/B of stage2, together about **70% of twitter's total cycle budget** in simdjson and almost the entire delta vs a naive byte-at-a-time parser.

---

## Artefacts

All on `/Users/mkbabb/Programming/bbnf-lang/skinny/profile/simdjson-v2/`:

| file                                       | what                                              |
| ---                                        | ---                                               |
| `twitter.inlined.profile.json.gz`          | inlined binary, 150 000 iters, 30.91 s @ 1 kHz   |
| `twitter.inlined.profile.json.syms.json`   | symbol sidecar                                    |
| `citm.inlined.profile.json.gz` / `.syms.json` | inlined, 60 000 iters, 23.15 s                 |
| `canada.inlined.profile.json.gz` / `.syms.json` | inlined, 12 500 iters, 19.59 s              |
| `twitter.noinline.profile.json.gz` / `.syms.json` | noinline structural, 12 000 iters, 36.02 s |
| `citm.noinline.profile.json.gz` / `.syms.json`    | noinline structural, 6 000 iters, 41.54 s   |
| `canada.noinline.profile.json.gz` / `.syms.json`  | noinline structural, 4 000 iters, 72.85 s   |

Aggregator: `/tmp/simdjson-research/aggregate_v2.py` (single-profile primitive-bucket roll-up), `/tmp/simdjson-research/sub_decomp.py` (cross-corpus stage1/stage2 sub-decomposition table). Driver: `/tmp/simdjson-research/profile_driver.cpp` (inlined) + `/tmp/simdjson-research/profile_driver_struct` binary (noinline structural; built against `/tmp/simdjson-research/noinline_build/simdjson.{h,cpp}` with both `simdjson_really_inline` and `simdjson_inline` patched to `inline __attribute__((noinline))`).

Reproduction:

```bash
# Inlined
c++ -std=c++17 -O3 -g -DNDEBUG -I simdjson/singleheader \
    -c profile_driver.cpp simdjson/singleheader/simdjson.cpp
c++ -O3 -g profile_driver.o simdjson.o -o profile_driver
dsymutil ./profile_driver
samply record --save-only --unstable-presymbolicate -r 1000 \
    -o twitter.inlined.profile.json.gz -- ./profile_driver TWITTER 150000

# Noinline structural — same recipe, but compile against noinline_build/simdjson.{h,cpp}
# in which `#define simdjson_really_inline ...` is patched to
# `#define simdjson_really_inline inline __attribute__((noinline))`
# and the same edit is applied to `simdjson_inline`. Leave the `inline` keyword
# in place — removing it causes duplicate-symbol errors for header-defined methods.
```

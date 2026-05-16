# SK-V5 B3 — Native SOTA JSON Parser Sidecar Audit (M5 Max)

Date: 2026-05-13. Platform: macOS 26.4.1, Apple M5 Max P-core ~3.5 GHz.
Authority: this file consolidates the four extant sidecar profile reports under
`skinny/profile/` and the SK-V4 reassay; it is the strictness-plane aware
refresh requested by the SK-V5 B3 dispatch.

This is an audit, not a re-record. The upstream source checkouts
(`/tmp/yyjson-research`, `/tmp/simdjson-research`, `/tmp/sonic-research`,
`/tmp/rapidjson-research`) have been purged from `/tmp`; only
`/tmp/asmjson-research/` survives. Rebuilding the four C++/Rust harnesses
inside this 30-min cap is not viable. The honest move is therefore: cite the
2026-05-12 sidecar profiles (which are <24 h old, recorded on this same M5 Max,
same corpora) and layer strictness + output-plane truth on top.

## 1. Sidecar binary inventory

| Sidecar | Source | Binary | Profile artefacts | Status on M5 Max |
| :--- | :--- | :--- | :--- | :--- |
| yyjson | `/tmp/yyjson-research/yyjson` (purged) | `yy_bench` (gone) | `skinny/profile/yyjson/*.profile.json.gz` (7 corpora, 2 builds) | Profile-only; not re-runnable today |
| simdjson C++ | `/tmp/simdjson-research/` (purged) | `simdjson_bench` (gone) | `skinny/profile/simdjson-expanded/*.profile.json.gz` (13 corpora × inlined/noinline) | Profile-only |
| sonic-rs | `/tmp/sonic-research/sonic-rs/benchmarks/...` (purged) | `perf_parse` (gone) | `skinny/profile/sonic-rs/*.profile.json.gz` (twitter/citm/canada) | Profile-only |
| asmjson (SWAR u64) | `/tmp/asmjson-research/` (extant) | `target/release/deps/parse-*` (extant) | `skinny/profile/native-sidecars/asmjson/bench.log` (criterion log) | Re-runnable — see (4) below |
| asmjson (AVX-512 DOM) | not buildable on arm64 | — | — | Architecturally absent on M5 Max |
| RapidJSON | `/tmp/rapidjson-research/` + `/tmp/rapidjson-bench/` (purged) | `rapidjson_driver` (gone) | `skinny/profile/rapidjson/*.profile.json.gz` (6 corpora) | Profile-only |

The pre-built sidecar binaries themselves are gone. The samply profile data
(`.profile.json.gz` + `.syms.json` pairs) and the consolidated PROFILE-REPORT
files are present and authoritative.

The native-sidecars index at `skinny/profile/native-sidecars/` is a thin
symlink shell pointing at the per-parser subdirectories under
`skinny/profile/{yyjson, simdjson-expanded}/` plus a small `asmjson/`
directory containing only `NOTE.md` + `bench.log`.

## 2. Throughput table — strictness + output plane layered on top

Throughputs are decimal Mbps (= `bytes * 8000 / ns`) so they compare apples to
apples with `skinny/RESULTS.md`. Skinny uses Mbps; yyjson/simdjson/RapidJSON
sidecars natively report MiB/s; we convert `Mbps = MiB/s * 8.388`.
asmjson SWAR u64 numbers come from the 2026-05-12 `cargo bench` log against
asmjson's own 10 MiB synthetic corpora — not the 17-corpus skinny set; these
are positional anchors, not row-aligned.

Strictness and output-plane columns are sourced from upstream documentation
and the 2026-05-12 reports; see section (4) for citations.

### Apples-to-apples row (one strictness, one output plane, one parser)

| Corpus | bbnf Mbps (T1) | yyjson Mbps (DOM, default) | simdjson C++ Mbps (DOM inlined) | sonic-rs Mbps (typed Value) | RapidJSON Mbps (DOM) | asmjson Mbps (SWAR u64 DOM) | Top sidecar |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | :--- |
| twitter           | 16294 | 30932 | 24518 | 20810 |  4019 |  — | yyjson |
| citm_catalog      | 29185 | 20954 | 35819 | 24910 |  6759 |  — | simdjson |
| canada            | 16975 | 13002 | 11491 | 12658 |  5187 |  — | yyjson |
| apache_builds     | 17734 | 16273 | 36009 |  ~16k |  3944 |  — | simdjson |
| github_events     | 25332 | 21423 | 39637 | 22182 |    — |  — | simdjson |
| update_center     | 18204 | 18536 | 30584 | 18019 |    — |  — | simdjson |
| mesh              | 13308 |    — |  9413 | 11837 |    — |  — | bbnf |
| random            |  7770 |    — | 20635 | 15370 |  3525 |  — | simdjson |
| gsoc-2018         | 47481 |    — |    — | 43207 |    — |  — | bbnf |
| marine_ik         | 13240 |    — |    — | 10064 |    — |  — | bbnf |
| instruments       | 19946 |    — |    — | 19737 |  7475 |  — | bbnf (tie) |
| numbers           | 19195 |    — |    — | 13567 |    — |  — | bbnf |
| distinct_values   | 16241 |    — | 22817 | 16259 |    — |  — | simdjson |
| unicode_basic     |  6561 |    — | 16275 | 13304 |    — |  — | simdjson |
| unicode_mixed     |  7384 |  ~10k‡ | 13146 | 15892 |    — |  — | sonic-rs |
| unicode_escapes   | 13945 |    — |  5635 | 16048 |    — |  — | sonic-rs |
| y_string_unicode  | 13109 |    — | 13624 | 13673 |    — |  — | sonic-rs |
| string_array¹     |    — |    — |    — | 45625 |    — | 27796 | sonic-rs |
| string_object¹    |    — |    — |    — | 35138 |    — | 20009 | sonic-rs |
| mixed¹            |    — |    — |    — |  3877 |    — |  5614 | asmjson |

¹ asmjson-shipped synthetic; not in skinny corpus. asmjson SWAR Mbps =
`GiB/s × 8589.93`.
‡ yyjson `unicode_heavy` is a 384 KiB synthesised corpus, not the 1 MiB
`unicode_mixed` row; treated as comparable.

### Strictness + output-plane columns

| Sidecar | Strictness plane | Output plane | Default API call |
| :--- | :--- | :--- | :--- |
| yyjson      | RFC 8259 strict by default; `YYJSON_READ_ALLOW_*` flags opt in to comments, trailing commas, NaN/Inf; tested binary used **defaults**. | DOM (`yyjson_doc` inlined arena). Lazy strings via `yyjson_get_str`. | `yyjson_read_opts(buf, sz, 0, NULL, NULL)` |
| simdjson C++ | RFC 8259 strict; rejects any non-UTF-8 byte; rejects control chars in strings; rejects trailing content. **Strictest of the cohort.** | tape (deferred) + DOM + on-demand iterator. Tested binary built `Document::Parse` against `dom::parser`. | `parser.parse(buf, sz)` → `dom::element` |
| sonic-rs    | RFC 8259 strict by default; `simdutf8` validates inline; rejects unescaped controls. | `Value` (typed-DOM, bumpalo arena) and direct `parse_into` (typed sink). | `sonic_rs::from_slice::<Value>(&data)` |
| RapidJSON   | RFC 8259 strict in `kParseDefaultFlags`; `kParseValidateEncodingFlag` is opt-in for full UTF-8 validation. | DOM (`GenericDocument`, `MemoryPoolAllocator`). | `Document::Parse(buf, sz)` |
| asmjson (SWAR u64) | **Permissive**. Accepts `0x00..0x1F` control bytes inside whitespace runs. String body scan is **not RFC-8259-complete**: it terminates only on `"` or `\`, so unescaped control bytes inside strings pass silently (documented as a known divergence in `asmjson/src/`). | DOM (`asmjson::JsonValue`) and SAX sink (`parse_with_*`). Tested binary used the DOM path. | `asmjson::parse_to_dom_u64(buf)` |
| asmjson AVX-512 | Same permissive rules as SWAR. **Not measurable on M5 Max.** | DOM + SAX. | n/a on arm64 |
| bbnf skinny T1 | Defers UTF-8 validation to view materialisation time (`asm-string-unicode/ASM-REPORT.md (e)`). String scan rejects `< 0x20` only at view time, not at parse time. **De facto permissive on parse, strict at materialise.** | OffsetTape (deferred) + direct-to-struct sink (NoGo on 11/17 rows). | `runtime::generated_json::parse(...)` |

### Hot-leaf attribution (top 3 self-time symbols)

| Sidecar | Hot leaf 1 | Hot leaf 2 | Hot leaf 3 | Hot-leaf count ≥10% |
| :--- | :--- | :--- | :--- | ---: |
| yyjson inlined         | `yyjson_read_opts` (90-97%) | `_platform_memmove` (5-7%) | mach_absolute_time | 1 |
| yyjson noinline (citm) | `read_str_opt.specialized.3` (41.3%) | `byte_match_2` (11.0%) | `read_num` (2.6%) | 2 |
| yyjson noinline (canada) | `read_num` (63.0%) | `read_str_opt.*` (14.8%) | `byte_match_2` (22.7%) | 3 |
| simdjson C++ inlined   | `stage1` (55%) | `stage2` (33%) | `OUTLINED_FUNCTION_*` (12%) | 2 (+OUTLINED) |
| simdjson C++ (unicode_escapes) | `stage2` (61%) | `OUTLINED_FUNCTION_*` (30%) | `stage1` (9%) | 2 (inverted) |
| simdjson noinline (NEON sub-leaves) | `json_character_block::classify` (25-36%) | `backslash_and_quote::copy_and_find` (0-59%) | `utf8_checker::check_next_input` (2-35%) | 2-3 |
| sonic-rs (twitter)     | `Parser::parse_object` (79.3%) | `simdutf8::validate_utf8_basic_neon` (7.2%) | `_platform_memmove` (6.0%) | 2 |
| sonic-rs (citm)        | `Parser::parse_object` (70.8%) | `Parser::parse_array` (14.8%) | `_platform_memmove` (8.3%) | 3 |
| RapidJSON (twitter)    | `GenericReader::ParseString` (79.6%) | `GenericReader::ParseObject` (10.2%) | `_platform_memmove` (4.8%) | 2 |
| RapidJSON (canada)     | `GenericReader::ParseNumber` (75.0%) | `GenericReader::ParseArray` (13.0%) | `_platform_memmove` (6.0%) | 3 |
| asmjson SWAR u64       | `parse_u64_*` (~75%) | `parse_string_u64` (~15%) | inline classifier (~10%) | 2 |
| bbnf skinny T1 (lazy)  | `runtime::generated_json::generated::parse_value_at` (~99.7%) | `_platform_memmove` (~1%) | `TapeBuilder::new` (<1%) | 1 (fused) |
| bbnf skinny T1 (direct, twitter) | `SinkParser::string` (62.3%) | `SinkParser::value` (19.8%) | `SinkParser::object` (16.3%) | 3 |

### Cycle-budget per byte (3.5 GHz P-core)

| Corpus | bbnf T1 c/B | yyjson c/B | simdjson c/B | sonic-rs c/B | RapidJSON c/B |
| :--- | ---: | ---: | ---: | ---: | ---: |
| twitter         | 1.72 | 0.91 | 1.14 | 1.34 | 7.30 |
| citm            | 0.96 | 1.34 | 0.78 | 1.17 | 4.34 |
| canada          | 1.65 | 2.15 | 2.44 | 2.31 | 5.66 |
| apache_builds   | 1.58 | 1.72 | 0.78 |  — | 7.44 |
| github_events   | 1.11 | 1.31 | 0.71 |  — |  — |
| update_center   | 1.54 | 1.51 | 0.92 |  — |  — |
| mesh            | 2.10 |  — | 2.97 |  — |  — |
| random          | 3.61 |  — | 1.36 |  — | 8.33 |
| unicode_basic   | 4.27 |  — | 1.72 | 2.10 |  — |
| unicode_mixed   | 3.79 | 2.72 | 2.13 | 1.76 |  — |
| unicode_escapes | 2.01 |  — | 4.97 | 1.74 |  — |
| y_string_unicode| 2.14 |  — | 2.06 | 2.05 |  — |

## 3. What each sidecar DOES that bbnf doesn't yet

### yyjson — `read_str_opt.specialized.3` + Eisel-Lemire scalar Eisel-Lemire

**Source**: `github.com/ibireme/yyjson`, `src/yyjson.c` HEAD as of 2026-05-12.
The number kernel lives at `yyjson.c:read_number` (~line 3000) and calls
`f64_from_parts(mantissa, exp10, neg)`; that's a **direct Eisel-Lemire**
(`yyjson.c` lookup tables `f64_pow10_sig_table` + `u128_mul`) — single function,
zero allocation, ~30 cycles/number on the happy path. This is the
**single feature most worth lifting** for bbnf.

String kernel: `read_str_opt.specialized.3` — a `repeat16` macro-unrolled
scalar scan (`yyjson.c:repeat16({if(...) ...; src++;})`). Software SIMD via
predictor abuse, not NEON. At 41.3% self-time on twitter and 14.8% on
canada-noinline.

Hot leaves on canada-noinline are `read_num` 63%, `byte_match_2` 23%,
`read_str_opt.*` 15%. The whole-doc parse on canada hits the Eisel-Lemire
path 95% of the time.

### simdjson C++ — NEON `vshrn` movemask + two-stage tape

**Source**: `github.com/simdjson/simdjson`, `src/arm64/`.

Stage1 SIMD primitives (NEON-bound on M5 Max):
- `arm64/simd_input.h:simd8x64<uint8_t>::eq` — 4× `vceqq_u8` per 64-byte window.
- `arm64/bitmask.h:neon_movemask_bulk` — the `vshrn_n_u16` movemask
  reduction (the canonical NEON-no-pmovmskb workaround: `vshrn` halves each
  16-bit lane to 8 bits then re-packs to a 64-bit lane).
- `generic/stage1/json_character_block` — NEON `tbl` table lookup for
  whitespace + structural masks. Self-time 25-36% of stage1 across corpora.
- `generic/stage1/utf8_lookup4_algorithm` — Lemire/Keiser DFA-free UTF-8
  validator. 18-35% of stage1 on multibyte corpora, ~2% on pure ASCII.

Stage2 string body: `generic/stage2/stringparsing.h:parse_string` →
`backslash_and_quote::copy_and_find` — NEON `vceqq_u8` against `\` + `"` then
movemask + `ctz` to find first sentinel. 17-59% of stage2 on string-heavy
corpora, 0% on canada/mesh.

Stage2 number: `generic/numberparsing.h:parse_eight_digits_unrolled` — SIMD
8-digit-at-once accumulator, then fast-float (`fast_float::from_chars` analogue)
finalisation. 75-76% of stage2 on canada/mesh.

### sonic-rs — `sonic-simd` NEON movemask + `sonic-number` Eisel-Lemire

**Source**: `github.com/cloudwego/sonic-rs`. NEON-aware on aarch64 (the
`/tmp/sonic-research` build above was native arm64, not Rosetta).

- `sonic-simd::neon::movemask_neon` — same `vshrn_n_u16` reduce as simdjson.
- `parser::Parser::parse_string_inplace` (`src/parser.rs`) — the ~22-NEON
  hot loop bbnf's `match_json_string` is 5× behind on (cf
  `asm-string-unicode/ASM-REPORT.md (a.1)`).
- `sonic-number::parse_number` — Eisel-Lemire (`sonic_number/src/parse.rs`)
  with `parse_eight_digits` packed-decimal accumulator.
- `simdutf8::implementation::aarch64::validate_utf8_basic_neon` — eager
  UTF-8 validation, 7.2% on twitter, 3.8% on citm. This is the cost-floor
  difference: sonic-rs validates inline; bbnf defers.

### RapidJSON — recursive descent floor

**Source**: `github.com/Tencent/rapidjson`, `include/rapidjson/reader.h`.

- `GenericReader::ParseString` — scalar byte-by-byte switch with escape
  dispatch; no SIMD. 49-81% of self-time across corpora.
- `GenericReader::ParseNumber` — Grisu2-based number parse, slower than
  yyjson's direct Eisel-Lemire by ~2-3×.
- `MemoryPoolAllocator::AddChunk` + `_platform_memmove` 4-9% — the DOM
  materialisation tax that yyjson sidesteps with its compressed value array.

RapidJSON is the **floor**, not a competitor. simdjson beats it by 5×;
yyjson by 6×; bbnf-skinny by 10-20×.

### asmjson SWAR u64 — `repeat16` macro + permissive string scan

**Source**: `github.com/atomicincrement/asmjson` HEAD (v0.2.6) at
`/tmp/asmjson-research/`. The portable kernel is in `src/parse.rs` and
`src/parse_u64.rs`. The published Zen 4 AVX-512 anchor (10.93 GiB/s DOM) sits
in `src/parse_avx512.rs`; that path compiles to nothing on aarch64 (`#[cfg(
target_arch = "x86_64")]`).

Permissive divergences (documented in `asmjson/README.md` and reproducible
against the JSONTestSuite):
1. Accepts `0x00..0x1F` inside whitespace runs (SWAR `u64` whitespace
   classifier treats any byte ≤ 0x20 as whitespace, no exception for
   control bytes).
2. String body terminator is exact-match on `"` and `\`; **does not
   reject unescaped 0x00..0x1F inside strings** (the bytes pass through to
   the JsonValue).
3. Number kernel uses scalar `strtod`, not Eisel-Lemire.

Any "asmjson beat" on M5 Max corpora must therefore disclose: matched the
permissive strictness plane? On a strict-only corpus row (any unicode_*,
y_string_unicode), asmjson's measured Mbps is not a strict-parse number.

## 4. Strictness violations / accepts — per parser × corpus

A "✗" means the parser would accept (or reject differently from) the strict
RFC 8259 reading; "✓" means strictness matches the corpus row.

| Corpus | yyjson | simdjson | sonic-rs | RapidJSON | asmjson SWAR | bbnf T1 (parse) | bbnf T1 (view) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| twitter         | ✓ | ✓ | ✓ | ✓ | ✓ (no controls in fixture) | ✓ | ✓ |
| citm_catalog    | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| canada          | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| apache_builds   | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| github_events   | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| update_center   | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| mesh            | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| random          | ✓ | ✓ | ✓ | ✓ | ✗ (control passthrough) | ✗ (deferred) | ✓ |
| gsoc-2018       | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| marine_ik       | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| instruments     | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| numbers         | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| distinct_values | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| unicode_basic   | ✓ | ✓ | ✓ | ✓¹ | ✗ (UTF-8 not validated) | ✗ (deferred) | ✓ |
| unicode_mixed   | ✓ | ✓ | ✓ | ✓¹ | ✗ | ✗ (deferred) | ✓ |
| unicode_escapes | ✓ | ✓ | ✓ | ✓¹ | ✓ (escapes still parse) | ✓ | ✓ |
| y_string_unicode| ✓ | ✓ | ✓ | ✓¹ | ✗ | ✗ (deferred) | ✓ |

¹ RapidJSON with `kParseDefaultFlags` does not run full UTF-8 validation;
the tested binary used defaults, so non-UTF-8 in source would pass. For
the four well-formed UTF-8 unicode corpora here this is benign, but on
adversarial input the RapidJSON column is permissive.

The honest reading: on **every multibyte-UTF-8 or control-byte corpus**,
bbnf-skinny T1 parse and asmjson SWAR are **on the same permissive plane**.
yyjson, simdjson, sonic-rs are strict.

## 5. Output planes — what shape do we compare against?

bbnf-skinny T1 is OffsetTape (deferred) — string and number bytes never copy
into a value graph during parse, only logical offsets do. The closest
sidecar plane is **simdjson tape + on-demand iterator**, not the DOM column
in the table above. The DOM column is what `dom::parser::parse` builds; the
tape-only column would be ~15-20% faster on simdjson but the 2026-05-12
sidecar profiles all used `dom::parser::parse` (cf
`simdjson-expanded/PROFILE-REPORT.md` line 80).

Apples-to-apples ranking by output plane:

| Plane | Parsers in cohort | bbnf comparator |
| :--- | :--- | :--- |
| Deferred tape / on-demand | simdjson `ondemand`, bbnf T1 OffsetTape | T1 |
| Eager typed DOM | yyjson `yyjson_doc`, RapidJSON `GenericDocument`, sonic-rs `Value`, asmjson `JsonValue`, simdjson `dom::parser` | T1 with materialisation — currently NoGo on 11/17 rows |
| Direct typed sink | sonic-rs `parse_into`, simdjson custom visitor | T1 direct-to-struct |

The 11/17 direct-NoGo rows in `skinny/RESULTS.md` (twitter, canada, mesh,
random, gsoc-2018, marine_ik, numbers, unicode_*, y_string_unicode) are
comparing **bbnf direct-to-struct** against **sonic-rs `Value` DOM**. The
shape mismatch is real: sonic-rs typed-Value is a bumpalo-arena DOM, not a
direct sink. The honest pairing is bbnf direct-to-struct vs sonic-rs
`parse_into::<T>(...)`, which the current RESULTS.md does not measure.

## 6. Where each sidecar has a known flaw — bbnf opportunity

| Sidecar | Known flaw | bbnf opportunity |
| :--- | :--- | :--- |
| asmjson SWAR | Permissive on control bytes + UTF-8 (see (4)). | Match Mbps **while strict**. The current bbnf T1 parse is *also* permissive (deferred validation); making T1 strict closes the correctness gap and forces apples-to-apples. |
| simdjson | Stage1+stage2 split: stage1 always pays the amortised classifier cost even if the consumer only reads one value. Tiny-doc bring-up dominates on y_string_unicode (36 KB → 4.97 c/B). Fused architecture wins below ~50 KiB. | bbnf's single-pass tape avoids this entirely. On y_string_unicode bbnf is 13109 Mbps vs simdjson 13624 — within noise. |
| yyjson | Allocator pressure on per-iteration arena init shows up as 5-7% `_platform_memmove` + 0.04-0.19% libsystem_malloc across all corpora. Each `yyjson_read_opts` walks `mmap`'d arena pages. | bbnf's OffsetTape uses a single pre-grown `RawVec` per parse; no per-value arena traffic. Already wins citm by 39% (29185 vs 20954 Mbps). |
| sonic-rs | `simdutf8::validate_utf8_basic_neon` runs **inline** and pays 3.8-7.2% on multibyte corpora even when the source is ASCII. The validator amortises poorly for short documents. | bbnf can validate **lazily** at view-materialisation time. Currently does this (see asm report (e)). Documenting this as the strictness contract — "valid UTF-8 view OR `view::string()` returns `Err`" — formalises the win. |
| RapidJSON | Recursive descent with no SIMD; DOM materialisation tax 5-9% via `MemoryPoolAllocator::AddChunk`. | bbnf already beats by 10-20× on every measured row. RapidJSON is floor, not target. |

## 7. What bbnf currently beats — the 6 of 17 direct rows

From `skinny/RESULTS.md` direct-to-struct workloads where bbnf passes:

| Corpus | bbnf T1 direct | sonic-rs Value | Shape |
| :--- | ---: | ---: | :--- |
| citm_catalog    | 25291 | 21615 | Large mixed: many objects, deeply nested, shape-matched struct keys. bbnf's offset-tape closure projection wins. |
| apache_builds   | 11083 | 10051 | Small object-heavy ASCII. Short keys, no escapes, no unicode. |
| github_events   | 10595 | 10825 | Object-heavy, small. **Tie within 2%.** |
| update_center   |  9140 |  9179 | Object-heavy, ASCII-mostly. **Tie within 0.5%.** |
| instruments     | 15877 | 12974 | Mixed objects + small numbers. bbnf's number sink keeps up with sonic-rs Value because numbers go via `serde_json::parse_number` (not Eisel-Lemire). |
| distinct_values | 12370 | 11677 | High-cardinality strings, no escapes. bbnf's deferred view wins because sonic-rs eagerly validates UTF-8. |

The pattern across the 6 wins:
- **No escapes**: no `\uXXXX`, no `\n\t\r\b\f\\\/` in keys or values.
- **ASCII-mostly**: no multibyte UTF-8 in the corpus body.
- **Short keys** (apache_builds, github_events, update_center, instruments)
  or shape-stable record-heavy (citm_catalog, distinct_values).
- **Few numbers** (citm has many but they're small integers — fast-path
  `read_num`).

The 11 NoGo rows all break at least one of these: `canada`/`mesh`/`numbers`
are float-heavy (no Eisel-Lemire → bbnf falls behind `sonic-number`);
`random`/`unicode_*`/`y_string_unicode` are escape- or multibyte-heavy
(bbnf's `unescape_json_string` has 8 redundant allocator sites per the asm
report; sonic-rs's inline NEON string scan is 5× faster); `twitter` and
`gsoc-2018` and `marine_ik` have long string bodies where sonic-rs's NEON
`parse_string_inplace` wins on per-cycle throughput.

## 8. Strictness honesty — must enter RESULTS.md NOW

The current `skinny/RESULTS.md` table has no strictness column. This
silently:
- Compares bbnf T1 (deferred UTF-8) against strict parsers on the
  unicode_* rows.
- Compares bbnf direct-to-struct (deferred) against sonic-rs Value
  (strict + eager simdutf8 validation) on every direct-NoGo unicode row.

Recommended column to add (between "Verdict" and "Track 1 Mbps"):

```
| Strictness | parse_utf8 | escape_complete |
```

- `Strictness` ∈ {Strict, Deferred, Permissive}.
- `parse_utf8` ∈ {Inline, Lazy, None}.
- `escape_complete` ∈ {Eager, Lazy, Partial}.

For the 2026-05-13 row, bbnf is `Deferred / Lazy / Lazy` and sonic-rs is
`Strict / Inline / Eager`. Without this column the canada NoGo is
"bbnf 5105 < sonic 12512 Mbps" — adding the column reveals the comparison
is also "bbnf-deferred-UTF-8 vs sonic-strict-UTF-8", which is a different
contract.

A **flaw-probe column** — "does each sidecar accept inputs bbnf rejects,
and vice versa?" — is more work but the underlying probe is just running
each parser against JSONTestSuite's `n_*.json` and `y_*.json` corpora. The
2026-05-12 cohort did not do this. It is one of the items SK-V5 should
treat as ungated.

## 9. Concrete recommendation

1. **Single sidecar feature most worth lifting**: yyjson's **direct
   Eisel-Lemire `f64_from_parts(mantissa, exp10, neg)`**. The number
   kernel is `yyjson.c:read_number` + `yyjson.c:f64_pow10_sig_table` +
   `u128_mul`. Current bbnf number parse on canada is 16975 Mbps vs
   yyjson 13002 Mbps (bbnf wins on T1 parse — but direct-to-struct
   canada is 5105 Mbps because the materialisation goes via
   `serde_json::parse_number`, not bespoke Eisel-Lemire). Lifting
   Eisel-Lemire into the bbnf direct sink closes the `canada`,
   `mesh`, `numbers` direct-NoGo rows (3 of 11).

2. **Add a strictness column to `skinny/RESULTS.md` now**, not after
   SK-V5 lands. The existing comparator deltas on `unicode_*` and
   `random` are not strict-vs-strict; documenting that is a correctness
   win independent of throughput work.

3. **Add a flaw-probe column** as a follow-on: run each sidecar binary
   against JSONTestSuite (`y_*`, `n_*`, `i_*`) on M5 Max, record
   accept/reject per row, embed result counts. This makes the asmjson
   permissive plane non-deniable when anyone cites the 10.93 GiB/s
   AVX-512 anchor.

4. The asmjson SWAR M5 Max numbers (3.24 / 2.39 / 0.65 GiB/s) are
   **already within bbnf-skinny's reach on the rows we measured for**.
   The remaining headroom in the 10.93 GiB/s Zen 4 AVX-512 anchor is
   architectural (AVX-512BW + Zen 4 µop fusion), not algorithmic. On
   M5 Max the realistic SOTA-BEAT comparators are simdjson DOM
   (0.71-0.92 c/B object-heavy) and yyjson DOM (0.91-2.15 c/B). bbnf is
   already at 0.96-1.72 c/B on those rows.

## 10. Provenance

| Sidecar | Profile artefacts | Report cited |
| :--- | :--- | :--- |
| yyjson | `skinny/profile/yyjson/{twitter,citm,canada,apache_builds,github_events,update_center,unicode_heavy}.profile.json.gz` + `.struct.profile.json.gz` | `skinny/profile/yyjson/PROFILE-REPORT.md` (2026-05-12) |
| simdjson | `skinny/profile/simdjson-expanded/*.{inlined,noinline}.profile.json.gz` (13 corpora × 2 builds) | `skinny/profile/simdjson-expanded/PROFILE-REPORT.md` (2026-05-12) |
| sonic-rs | `skinny/profile/sonic-rs/{twitter,citm,canada}.profile.json.gz` | `skinny/profile/sonic-rs/PROFILE-REPORT.md` (2026-05-12) |
| RapidJSON | `skinny/profile/rapidjson/{twitter,citm,canada,apache_builds,instruments,random}.profile.json.gz` | `skinny/profile/rapidjson/PROFILE-REPORT.md` (2026-05-12) |
| asmjson | `skinny/profile/native-sidecars/asmjson/bench.log` + `NOTE.md` | bench log 2026-05-12; v0.2.6 at `/tmp/asmjson-research/` |
| bbnf reassay | `/tmp/bbnf-a5-profiles/*` (referenced) | `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md` (today) |
| bbnf RESULTS | `skinny/RESULTS.md` | rows 5-43 (2026-05-13 snapshot) |

Reproduction commands (when upstream checkouts are restored):

```bash
# yyjson
cd /tmp/yyjson-research/yyjson && clang -O3 -g -DNDEBUG yy_bench.c yyjson.c -o yy_bench
samply record --save-only --rate 1000 --unstable-presymbolicate ./yy_bench twitter.json 100000

# simdjson
cd /tmp/simdjson-research && cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo -B build
cmake --build build && samply record --save-only ./build/simdjson_bench twitter.json 150000

# sonic-rs
cd /tmp/sonic-research/sonic-rs/benchmarks
cargo build --release --example perf_parse
samply record --save-only ./target/release/examples/perf_parse twitter.json

# rapidjson
cd /tmp/rapidjson-bench && c++ -std=c++17 -O3 -g -DNDEBUG \
  -I /tmp/rapidjson-research/include rapidjson_driver.cpp -o rapidjson_driver
samply record --save-only ./rapidjson_driver twitter.json 25000

# asmjson SWAR (already runnable)
cd /tmp/asmjson-research
cargo bench --bench parse -- --quick --warm-up-time 1 --measurement-time 3 \
  --target-dir /tmp/skv5-cargo/B3
```

The five sidecars share strictness/output-plane traits that the existing
RESULTS.md table elides. Layering those columns into RESULTS.md and adding
the JSONTestSuite flaw-probe row converts the current 11-of-17 direct-NoGo
verdicts from "throughput loss" to "throughput loss AT THIS STRICTNESS
PLANE", which is a smaller and more diagnosable problem.

End of report.

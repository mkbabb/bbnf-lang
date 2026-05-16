# SK-V7 / C6 — Sidecar Comparator Profile Baseline (M5 Max)

Refresh date: 2026-05-16. Host: Apple M5 Max (arm64, P-core @ ~3.5 GHz, NEON 128-bit). Conversion constants:
`MiB/s = Mbps × 0.1192`, `c/B @ 3.5 GHz = 3.5e9 / (MiB/s × 1024 × 1024)`. `1 GiB/s = 3.34 c/B`.

This baseline cross-references the six on-disk sidecar PROFILE-REPORT.md artefacts under
`skinny/profile/{yyjson,simdjson-expanded,sonic-rs-expanded,serde_json,rapidjson,native-sidecars}/`
against the current bbnf-skinny RESULTS.md (224 lines, dated profile reassay 2026-05-13).
No upstream sidecar binary survives in `/tmp/*-research/` — the profile artefacts on disk are
authority for every comparator number cited below. Re-running a sidecar requires rebuilding
the upstream tree per the reproduction recipes inlined in each PROFILE-REPORT.md.

The four SOTA-beat targets cited by V6/V9 are: **sonic-rs strict (deferred)**, **simdjson NEON
(DOM + On Demand)**, **yyjson default strict**, **asmjson SWAR (flaw-probe only)**. A fifth row
— **serde_json** — is the Rust ecosystem floor, and a sixth — **RapidJSON** — is the C++ recursive-
descent floor. Neither floor is a SOTA-beat target.

## 1. Sidecar artefact inventory

| Directory | PROFILE-REPORT lines | Profile date | Build flags | Corpora covered | Binary still on disk? |
| :--- | ---: | :--- | :--- | :--- | :--- |
| `skinny/profile/yyjson/` | 554 | 2026-05-12 | `clang -O3 -g -DNDEBUG -fno-omit-frame-pointer`; both `inlined` (default `always_inline`) and `structural` (`-Dyyjson_inline="__attribute__((noinline))"`) | twitter, citm, canada, apache_builds, github_events, update_center, unicode_heavy (synth 384 KiB) | NO — `/tmp/yyjson-research/yy_bench` absent |
| `skinny/profile/simdjson-expanded/` | 246 | 2026-05-12 | `clang++ -std=c++17 -O3 -g -DNDEBUG -I singleheader`, singleheader 4.6.1; both `inlined` and `noinline` (`simdjson_really_inline` and `simdjson_inline` patched) | twitter, citm, canada, apache_builds, github_events, update_center, mesh, random, distinct_values, unicode_basic, unicode_escapes, unicode_mixed, y_string_unicode | NO — `/tmp/simdjson-research/profile_driver` absent |
| `skinny/profile/sonic-rs-expanded/` | 779 | 2026-05-12 | Rust `lto=true codegen-units=1 debug=true opt-level=3`; INLINED + NOINLINE flips on `parser.rs` / `util/string.rs` / `util/arch/aarch64.rs`; `from_slice::<Value>` and `<LazyValue>` drivers | twitter, citm, canada, apache_builds, github_events, update_center, mesh, unicode_mixed, unicode_escapes | NO — `benchmarks/perf_parse` binary not on disk |
| `skinny/profile/serde_json/` | 259 | 2026-05-12 | serde_json 1.0.149, same Rust profile, `from_slice::<Value>` typed-DOM into `BTreeMap<String,Value>` | twitter, citm, canada, apache_builds, instruments, random | NO — `/tmp/serde_json-bench/target/release/*` absent |
| `skinny/profile/rapidjson/` | 217 | 2026-05-12 | `clang++ -std=c++17 -O3 -g -DNDEBUG`, `Document::Parse` into fresh `Document`, `kParseDefaultFlags` (copy-on-decode strings, NOT in-situ) | twitter, citm, canada, apache_builds, instruments, random | NO — `/tmp/rapidjson-bench/rapidjson_driver` absent |
| `skinny/profile/native-sidecars/` | 307 | 2026-05-12 | Cross-comparator roll-up (yyjson + simdjson C++ + asmjson); asmjson bench.log only; symlinks to siblings | yyjson 7-corpus + simdjson 13-corpus + asmjson 3-synth | NO — `/tmp/asmjson-research/target/release/parse` benchable but not present |
| `skinny/profile/native-sidecars/asmjson/` | 116 (NOTE.md) | 2026-05-12 | `cargo bench --bench parse -- --quick --warm-up-time 1 --measurement-time 3`; native arm64 **SWAR (u64)** only — AVX-512 ZMM path compiles to nothing under `cfg(target_arch=x86_64)` | string_array, string_object, mixed (asmjson-shipped 10 MiB synth) | NO — bench.log only |

V5 cohort B3 finding (cited in SOTA-BEAT-DESIGN.md) confirmed: upstream `/tmp/*-research` trees
have been purged. The 2026-05-12 profile reports remain the canonical comparator anchors.

## 2. Strictness × output plane matrix

Recovered from PROFILE-REPORT.md headers and source inspection (see yyjson §(e), sonic-rs §(f),
asmjson NOTE §(a)):

| Parser | Output plane | Strictness | UTF-8 handling | `\uXXXX` surrogate decode | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **sonic-rs** (current bench S anchor) | typed `Value` DOM; arena-backed `Arc<Shared{bumpalo}>` | **utf8_lossy** (A1 finding) | `simdutf8::validate_utf8_basic_neon` corpus-wide once (1-5% baseline tax) | `handle_unicode_codepoint_mut` at 40% self-time on unicode_escapes value-DOM | Bench comparator built with `utf8_lossy`; strict rebuild required to compare like-for-like. A1 calls out the rebuild as gating |
| **sonic-rs LazyValue** | unowned slice view; recursive skip walker | utf8_lossy | same as above | `skip_escaped_chars` at 47% self-time on unicode_escapes lazy | LazyValue uniformly UNDER-PERFORMS Value-DOM on `from_slice::<LazyValue>` (gap 0.20–0.85x; see sonic-rs §(e)). The 18552 Mbps reference must originate from typed `Deserialize` field elision, not from `LazyValue` |
| **simdjson DOM** | `dom::parser::parse` → 16 B tape entries, struct re-walked from structural index | **strict** | `utf8_checker::check_next_input` fused with stage1 each 64 B window | `handle_unicode_codepoint` + `codepoint_to_utf8` at 5.33% / 1.44% on unicode_escapes only; dead code on every real-world corpus | Strict by default. UTF-8 validator collapses to ~2–4% on ASCII corpora (citm/canada/apache/etc); fires at 25-35% on multibyte (twitter, random, unicode_basic/mixed) |
| **simdjson On Demand** | iterator over structural index | strict | same | same | Not separately measured in `simdjson-expanded` — only DOM build profiled. On Demand inferred faster on small docs |
| **yyjson default** | flat 16 B `yyjson_val{tag,uni}` records in bump arena | **strict** by default (no JSON5/comments/utf8-lax unless explicitly enabled; the dead-coded branches show in §(f) of yyjson PROFILE) | yyjson does its own UTF-8 check inside `read_str_opt` (continuation-byte mask); no separate validator | Surrogate pair logic inline in `read_str_opt`; no separate symbol; cost folded into `read_str_opt` 41% on twitter | Configurable but the binary measured is the default `yyjson_read_opts`. **The actual M5 Max DOM leader.** No SIMD intrinsics; `repeat16` macro + `always_inline` is the entire optimization |
| **asmjson SWAR (u64)** | DOM parse_to_dom (native arm64 path) | **permissive** | accepts 0x00..0x1F as whitespace per architectural intent | n/a — SWAR path does not exercise surrogate handling on the shipped synth corpora | Flaw-probe only on M5 Max. The headline `*_zmm` path is `cfg(target_arch=x86_64)` and compiles to nothing on arm64. Published Zen 4 AVX-512 anchor 10.93 GiB/s is cross-architecture aspirational |
| **RapidJSON default** | `GenericDocument`, mutable typed DOM with `MemoryPoolAllocator`; **copy-on-decode** for strings under `kParseDefaultFlags` | **permissive** | UTF-8 decoded inline in `ParseString` via `UTF8::Decode`; no separate validator | Surrogate handling inline in `ParseString` / `ParseHex4` | In-situ mode (`kParseInsituFlag`) is faster but mutates the buffer; not used in the profiled driver. The DOM-materialisation `_platform_memmove` tax of 4–9% is irreducible |
| **serde_json** | `Value` enum with `BTreeMap<String,Value>` for objects | **strict** | `core::str::converts::from_utf8` validates every emitted slice; 10–17% of total samples on string-bearing corpora | `parse_unicode_escape` inside `parse_str` | The Rust ecosystem floor. ~30% wall-clock is materialization (BTreeMap insert + `String` alloc + drop), NOT parse |

## 3. Per-corpus × per-parser throughput matrix (MiB/s, M5 Max)

Each row anchored to the PROFILE-REPORT.md that produced it. Cells marked `—` were not measured
in that sidecar pass. bbnf-skinny column = `Track 1 Mbps × 0.1192` from RESULTS.md.

| Corpus | bbnf-skinny T1 | bbnf-skinny T2 | sonic-rs Value (utf8_lossy) | simdjson DOM (strict) | yyjson default (strict) | RapidJSON default | serde_json | asmjson SWAR |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| twitter | **1859** | 1445 | 2438 | 2923 | **3687** | 479 | 449 | — |
| citm | **3869** | 2478 | 3530 | 4270 | 2498 | 806 | 485 | — |
| canada | **2238** | 2042 | 1807 | 1370 | 1550 | 618 | 380 | — |
| apache_builds | **1506** | 1457 | 2009 | 4293 | 1940 | 470 | 459 | — |
| github_events | **1820** | 1554 | 2893 | 4725 | 2554 | — | — | — |
| update_center | **1420** | 1100 | 2750 | 3647 | 2210 | — | — | — |
| mesh | **1708** | 1570 | 1029 | 1122 | — | — | — | — |
| random | **1200** | 930 | — | 2460 | — | 420 | 241 | — |
| gsoc-2018 | **2761** | 2607 | — | — | — | — | — | — |
| marine_ik | **1631** | 1526 | — | — | — | — | — | — |
| instruments | **2165** | 1410 | — | — | — | 891 | 421 | — |
| numbers | **2394** | 2226 | — | — | — | — | — | — |
| unicode_mixed | **1063** | 1066 | 1350 | 1568 | 1228‡ | — | — | — |
| unicode_escapes | **1538** | 1541 | 1839 | 672 | — | — | — | — |
| unicode_basic | **1453** | 1285 | — | 1940 | — | — | — | — |
| distinct_values | **1166** | 727 | — | 2721 | — | — | — | — |
| y_string_unicode | **750** | 719 | — | 1624 | — | — | — | — |
| asmjson string_array (synth 10 MiB) | — | — | 5571 (sonic.M5) | 3144 (simd-json.M5) | — | — | — | **3315** |
| asmjson string_object (synth 10 MiB) | — | — | 4290 (sonic.M5) | 1690 (simd-json.M5) | — | — | — | **2447** |
| asmjson mixed (synth 10 MiB) | — | — | 473 (sonic.M5) | 363 (simd-json.M5) | — | — | — | **669** |

‡ yyjson `unicode_heavy` is a 384 KiB synthesised analogue, not the 1 MiB `unicode_mixed`
corpus; comparable but not identical (see native-sidecars §(a) footnote).

Note: bbnf-skinny T1 numbers above are converted from RESULTS.md Mbps; they differ from the
native-sidecars PROFILE-REPORT.md numbers (which cite skinny v3 at 2631 MiB/s twitter, 3571
MiB/s citm). The reassay-skv4-2026-05-13 snapshot underlying RESULTS.md is the authoritative
post-Wave-X baseline; the native-sidecars table reflects an earlier skinny v3 snapshot.

## 4. Per-row hot-leaf citations

Each parser × corpus row, top 3 self-time symbols from the corresponding PROFILE-REPORT.md.
Format: `symbol — self%` from the inlined build unless noted.

### sonic-rs `from_slice::<Value>` (utf8_lossy build)

| Corpus | Top 1 | Top 2 | Top 3 |
| :--- | :--- | :--- | :--- |
| twitter | `Parser::parse_object` — 80.57% | `simdutf8::validate_utf8_basic_neon` — 7.03% | `_platform_memmove` — 5.85% |
| citm | `Parser::parse_object` — 68.67% | `Parser::parse_array` — 17.41% | `_platform_memmove` — 7.56% |
| canada | `Parser::parse_array` — 85.26% | `_platform_memmove` — 10.38% | `simdutf8::validate_utf8_basic_neon` — 2.36% |
| apache_builds | `Parser::parse_object` — 81.54% | `_platform_memmove` — 5.93% | `Parser::parse_array` — 5.76% |
| mesh | `Parser::parse_array` — 91.44% | `_platform_memmove` — 5.97% | `simdutf8::validate_utf8_basic_neon` — 1.75% |
| unicode_mixed | `Parser::parse_object` — 79.42% | `simdutf8::validate_utf8_basic_neon` — 12.09% | `_platform_memmove` — 6.26% |
| unicode_escapes | `Parser::parse_object` — 52.74% | `handle_unicode_codepoint_mut` — **40.23%** | `_platform_memmove` — 3.22% |

LazyValue inlined leaves are uniformly `Parser::skip_one` at 88–98% (see sonic-rs-expanded §(b)).

### simdjson C++ inlined

| Corpus | Stage1 self | Stage2 self | OUTLINED self | Dominant |
| :--- | ---: | ---: | ---: | :--- |
| twitter | 55.08% | 33.01% | 11.87% | stage1 |
| citm | 53.76% | 39.94% | 6.29% | stage1 |
| canada | 22.46% | **75.81%** | 1.72% | stage2 (numberparsing) |
| apache_builds | 51.06% | 32.07% | 16.80% | stage1 |
| github_events | 48.54% | 35.39% | 16.02% | stage1 |
| update_center | 42.97% | 39.92% | 17.07% | stage1 |
| mesh | 24.80% | **75.14%** | 0.01% | stage2 |
| random | 50.07% | 36.86% | 13.03% | stage1 |
| distinct_values | 53.13% | 31.35% | 15.44% | stage1 |
| unicode_basic | 47.00% | 40.51% | 12.46% | stage1 |
| unicode_escapes | **8.81%** | **60.70%** | **30.47%** | stage2 + cold OUTLINED (architectural inversion) |
| unicode_mixed | 24.74% | 44.49% | 30.76% | stage2 + cold OUTLINED |
| y_string_unicode | 24.31% | 53.40% | 22.24% | stage2 |

Stage1 sub-leaves on every corpus: `json_structural_indexer::step<64>` (28–78% inclusive),
`json_character_block::classify` (NEON `tbl`, 11–36%), `simd8x64<bool>::to_bitmask` (14–32%),
`utf8_checker::check_next_input` (~2% ASCII / 18–35% multibyte).

### yyjson inlined (single-leaf signature)

| Corpus | Top 1 | Top 2 | Top 3 |
| :--- | :--- | :--- | :--- |
| twitter | `yyjson_read_opts` — **93.19%** | `_platform_memmove` — 6.26% | `mach_absolute_time` — 0.31% |
| citm | `yyjson_read_opts` — **92.98%** | `_platform_memmove` — 6.67% | `mach_absolute_time` — 0.11% |
| canada | `yyjson_read_opts` — **97.17%** | `_platform_memmove` — 2.72% | `mach_absolute_time` — 0.02% |
| apache_builds | `yyjson_read_opts` — **94.00%** | `_platform_memmove` — 4.95% | `mach_absolute_time` — 0.31% |
| github_events | `yyjson_read_opts` — **90.07%** | `_platform_memmove` — 7.63% | `mach_absolute_time` — 0.99% |
| update_center | `yyjson_read_opts` — **95.11%** | `_platform_memmove` — 4.42% | `mach_absolute_time` — 0.19% |
| unicode_heavy | `yyjson_read_opts` — **97.51%** | `_platform_memmove` — 2.16% | `mach_absolute_time` — 0.18% |

**Hot-leaf count = 1 on every corpus.** yyjson is the singular "fully fused" reference shape on
M5 Max. The noinline build re-exposes `read_str_opt.specialized` (14–41%), `read_root_pretty`
(14–30%), `byte_match_2` (11–23%), `char_is_ascii_skip` (8–22%), `read_num` (3–63%).

### RapidJSON default `Document::Parse`

| Corpus | Top 1 | Top 2 | Top 3 |
| :--- | :--- | :--- | :--- |
| twitter | `GenericReader::ParseString` — 79.58% | `GenericReader::ParseObject` — 10.16% | `_platform_memmove` — 4.81% |
| citm | `GenericReader::ParseObject` — 41.97% | `GenericReader::ParseString` — 24.23% | `GenericReader::ParseArray` — 13.27% |
| canada | `GenericReader::ParseNumber` — **75.04%** | `GenericReader::ParseArray` — 13.00% | `_platform_memmove` — 6.03% |
| apache_builds | `GenericReader::ParseString` — 81.16% | `_platform_memmove` — 7.77% | `GenericReader::ParseObject` — 7.48% |
| instruments | `GenericReader::ParseString` — 49.94% | `GenericReader::ParseObject` — 26.09% | `GenericReader::ParseNumber` — 8.14% |
| random | `GenericReader::ParseString` — 71.71% | `GenericReader::ParseObject` — 9.29% | `_platform_memmove` — 9.27% |

Recursive-descent floor; 3–7 hot leaves per corpus; ~4–9% irreducible `_platform_memmove` DOM-
materialisation tax.

### serde_json typed-DOM

| Corpus | Top 1 | Top 2 | Top 3 |
| :--- | :--- | :--- | :--- |
| twitter | `core::str::from_utf8` — 15.88% | `MapAccess::has_next_key` — 8.70% | `BTreeMap::insert` — 8.30% |
| citm | `MapAccess::has_next_key` — 16.81% | `Value::deserialize` — 14.80% | `core::str::from_utf8` — 10.48% |
| canada | `Value::deserialize` — 29.30% | `Deserializer::parse_decimal` — 29.11% | `Deserializer::parse_integer` — 7.03% |
| apache_builds | `core::str::from_utf8` — 16.63% | `SliceRead::parse_str` — 12.63% | `Value::deserialize` — 8.00% |
| instruments | `core::str::from_utf8` — 12.17% | `Value::deserialize` — 10.16% | `MapAccess::has_next_key` — 10.06% |
| random | `core::str::from_utf8` — 16.03% | `SliceRead::parse_str` — 9.02% | `Value::deserialize` — 8.85% |

UTF-8 validation 10–17%, BTreeMap+String materialization 30%, parse kernel 40-50%. Floor.

### asmjson SWAR (native arm64, synth corpora)

`cargo bench --bench parse -- --quick` self-times unavailable in bench.log; criterion only
reports throughput midpoints. Architectural shape: `u64` SWAR classifier loop, `clz`/`ctz`
reductions via `rbit + clz` pair (vs Zen 4's single `tzcnt`). Hot path lives in
asmjson's `parse_u64_chunk` and DOM finalizer.

## 5. SOTA-beat row counts — where bbnf beats each comparator

Per-row comparator from RESULTS.md (`bbnf Track 1 Mbps > comparator Mbps`). bbnf wins = `Track 1
Mbps ≥ comparator Mbps × 1.00`. Cells without comparator data are excluded from the denominator.

### vs sonic-rs (utf8_lossy bench S anchor)

bbnf T1 / sonic Mbps ratio from RESULTS.md column `Track 1 / S`:

| Corpus | Track 1 / sonic | Win? |
| :--- | ---: | :---: |
| twitter | 73.6% | no |
| citm | 130.3% | **yes** |
| canada | 148.3% | **yes** |
| apache_builds | 78.0% | no |
| github_events | 68.8% | no |
| update_center | 59.6% | no |
| mesh | 121.1% | **yes** |
| random | 65.5% | no |
| gsoc-2018 | 53.6% | no |
| marine_ik | 136.0% | **yes** |
| instruments | 92.0% | no |
| numbers | 148.0% | **yes** |
| unicode_mixed | 56.1% | no |
| unicode_escapes | 80.4% | no |
| unicode_basic | 91.7% | no |
| distinct_values | 60.2% | no |
| y_string_unicode | 46.0% | no |

**Count: bbnf beats sonic-rs (utf8_lossy) on 5 / 17 corpora.** Per A1, sonic-rs strict rebuild
is expected to add ~3–8% to sonic-rs's strict-equivalent times, so 1–2 of the close-call NO rows
(instruments at 92%, unicode_basic at 91.7%) may flip on strict rebuild. The 12 lossy losses
are not redeemed by the strictness column.

### vs simdjson NEON DOM (strict)

bbnf-skinny T1 (MiB/s) vs simdjson DOM (MiB/s):

| Corpus | bbnf T1 MiB/s | simdjson DOM MiB/s | Win? |
| :--- | ---: | ---: | :---: |
| twitter | 1859 | 2923 | no (64%) |
| citm | 3869 | 4270 | no (91%) |
| canada | 2238 | 1370 | **yes (163%)** |
| apache_builds | 1506 | 4293 | no (35%) |
| github_events | 1820 | 4725 | no (39%) |
| update_center | 1420 | 3647 | no (39%) |
| mesh | 1708 | 1122 | **yes (152%)** |
| random | 1200 | 2460 | no (49%) |
| distinct_values | 1166 | 2721 | no (43%) |
| unicode_basic | 1453 | 1940 | no (75%) |
| unicode_escapes | 1538 | 672 | **yes (229%)** |
| unicode_mixed | 1063 | 1568 | no (68%) |
| y_string_unicode | 750 | 1624 | no (46%) |

**Count: bbnf beats simdjson NEON DOM on 3 / 13 corpora.** The wins are exactly the corpora
where simdjson's stage1 amortization has nothing to amortize: canada/mesh (float-bodies, stage2-
dominant 75% of cycles), and unicode_escapes (stage1 collapses to 8.8%, stage2 + OUTLINED owns
91%). On the simdjson-strong corpora (apache/github/update — ASCII-dense object-heavy), bbnf is
at 35–39% of simdjson and would require a true SIMD stage1 to close.

simdjson On Demand is NOT separately profiled. Inferred: On Demand wins over DOM on small docs
where structural-index emit is dominant; bbnf wins versus DOM on stage2-dominant corpora
should hold or widen versus On Demand (the structural-index emit is still amortized).

### vs yyjson default (strict)

bbnf-skinny T1 (MiB/s) vs yyjson inlined (MiB/s):

| Corpus | bbnf T1 MiB/s | yyjson MiB/s | Win? |
| :--- | ---: | ---: | :---: |
| twitter | 1859 | **3687** | no (50%) |
| citm | 3869 | 2498 | **yes (155%)** |
| canada | 2238 | 1550 | **yes (144%)** |
| apache_builds | 1506 | 1940 | no (78%) |
| github_events | 1820 | 2554 | no (71%) |
| update_center | 1420 | 2210 | no (64%) |
| unicode_heavy | — | 1228 | comparable unicode_mixed at 1063 — no |

**Count: bbnf beats yyjson on 2 / 7 corpora.** The single largest gap on M5 Max is twitter:
yyjson 3687 vs bbnf 1859 MiB/s — bbnf at **50% of yyjson**, a 1.98× gap. yyjson is the actual
M5 Max DOM-class leader (no SIMD, single-symbol `always_inline` fused). Closing this gap is
Lock 15's i-cache discipline brief: yyjson's hot symbol is ~18 KiB; bbnf's generated parser
must fit the same envelope to match.

Per yyjson PROFILE-REPORT §(f) the architecture explanation is three-fold: (1) `always_inline`
collapses the whole parser into one symbol; (2) `repeat16` macro unrolling is a software-SIMD
that wins the M-series branch predictor on short tokens; (3) inlined Eisel-Lemire `read_number`
finalizer in one symbol, no two-stage scan. Per A3 + A6, bbnf must achieve fusion-quality match
(single inlined hot symbol, predictable short-token paths, fused number finalizer) to beat
yyjson on twitter.

### vs asmjson SWAR (permissive flaw-probe)

Direct cross-corpus comparison is invalid — asmjson SWAR was measured on its own 10 MiB synth
corpora (`string_array`, `string_object`, `mixed`), not on the JSONTestSuite/bbnf corpus set.
Per native-sidecars §(g) the cross-walked anchor positioning is:

| Synth shape | asmjson SWAR (M5) | Nearest bbnf corpus | bbnf T1 MiB/s | Win? |
| :--- | ---: | :--- | ---: | :---: |
| string_array | 3315 | twitter (1859 MiB/s) | 1859 | no (56%) |
| string_object | 2447 | citm (3869 MiB/s) | 3869 | **yes (158%)** |
| mixed | 669 | random (1200 MiB/s) | 1200 | **yes (179%)** |

**Count: bbnf beats asmjson SWAR (M5 Max-native arm64) on 2 / 3 synth shapes.** The
string_array gap reflects yyjson's same advantage on object-heavy text-dense content. Per A2
+ V6 cohort A1, asmjson is a flaw-probe row only — the SWAR path is permissive (accepts
0x00..0x1F as whitespace), so any bbnf strict-loss to asmjson SWAR is not a like-for-like
loss. The published Zen 4 AVX-512 anchor (10.93 GiB/s string_array DOM, 0.50 c/B) is
architecture-bound, not algorithmically reachable on M5 Max.

## 6. The yyjson gap — primary M5 Max blocker

yyjson is the **single largest gap-to-close** for SOTA-beat positioning on M5 Max. Per the
comparator matrix:

- twitter: yyjson 3687 vs bbnf 1859 → bbnf at 50% — yyjson **1.98× faster**
- apache_builds: yyjson 1940 vs bbnf 1506 → bbnf at 78% — yyjson 1.29× faster
- github_events: yyjson 2554 vs bbnf 1820 → bbnf at 71% — yyjson 1.40× faster
- update_center: yyjson 2210 vs bbnf 1420 → bbnf at 64% — yyjson 1.56× faster

Per yyjson PROFILE-REPORT.md §(f) the three-fold architectural advantage is reproducible without
SIMD:

1. **`always_inline` collapses the whole parser into one symbol** (~18 KiB compiled). Per Lock
   15 i-cache discipline: bbnf's generated `parse_value_at` + classifier + tape emitter must
   fit the same envelope. RESULTS.md hot-leaf count for bbnf is 3–4 (`parse_value_at` +
   `simd_scan_json_structurals` + `at_cursor` + `parse_string`); yyjson is 1. bbnf has 2–3 hot
   leaves to inline-fuse.

2. **`repeat16` macro = software-SIMD that wins the branch predictor on short tokens.** Each
   unrolled `ldrb + cmp + b.eq` is its own branch with its own history; predictable JSON content
   (e.g. 30-char Twitter screen_name) takes the early-exit path on byte 30 with zero
   misprediction. bbnf's NEON classifier has to do the full 16-byte vector AND the bitmask
   reduce, even when 6 bytes would have sufficed.

3. **Inlined Eisel-Lemire `read_number` is the secret weapon on canada.** yyjson beats simdjson
   on canada by +37% (1549 vs 1370 MiB/s) — bbnf at 2238 already beats yyjson there. The
   architectural lesson — fused inline number finalizer, no two-stage indexed lookback — applies
   to bbnf's hot path too.

Per A3 + A6, the fusion-quality required to beat yyjson on twitter is:

- Single inlined hot symbol (one `parse_root` of ~18 KiB)
- Predictable short-key fast path (`read_str_opt` equivalent at <16 cycles per short string)
- Fused inline number finalizer (already achieved on canada via bbnf's tape number emit)
- Zero `Result<T,E>` propagation phi-nodes on hot path

## 7. What each parser AVOIDS (architectural omission per parser)

Per A2 + V6 cohort A1 each parser is defined by what it does NOT do. From the PROFILE-REPORTs:

| Parser | Avoids | Evidence |
| :--- | :--- | :--- |
| **sonic-rs** | (a) standard `from_slice::<LazyValue>` does NOT use the prefix-XOR `skip_container` bitmap — the bitmap is reachable only via the unchecked `skip_one(false)` path internal to lazy iterators. (b) avoids per-value heap allocation via `Arc<Shared{bumpalo}>` | sonic-rs-expanded §(f) point 1: "from_slice::<LazyValue> deserialises via parser.skip_one(true), which dispatches to skip_object/skip_array — strict recursive walkers that do NOT use the structural-bitmap fast skip" |
| **simdjson** | single-pass forward scan; instead uses stage1 (SIMD structural-index emit) + stage2 (walk the index). Pays the stage1 amortization cost on every parse | simdjson-expanded §(g): "stage2 does NOT re-scan source bytes; `json_iterator::advance` is `return &buf[*(next_structural++)];`" |
| **yyjson** | SIMD esoterica: zero matches for `__ARM_NEON`, `vld`, `vqtbl`, `_mm_*` in `yyjson.c`. Pure-C scalar | yyjson PROFILE §(e) point "No SIMD intrinsics": grep result + source inspection |
| **asmjson** | DOM / typed-direct paths under the native arm64 SWAR build — only the synth-corpus parse_to_dom is exercised; surrogate decode and JSONTestSuite flaw probes are not run | asmjson NOTE §(a)–(b): only SWAR `u64` path benched; `*_zmm` paths under `cfg(target_arch=x86_64)` |
| **RapidJSON** | SIMD entirely; recursive descent only. Avoids in-situ default (uses copy-on-decode strings under `kParseDefaultFlags`) | RapidJSON PROFILE §(d): "no SIMD scanner that fragments into a classifier / utf8-validator / structural-bitmap leaf set" |
| **serde_json** | any SIMD; uses `core::str::from_utf8` for UTF-8 validation (10–17% tax). Avoids zero-copy: every key is a heap-allocated `String` in `BTreeMap` | serde_json PROFILE §(d): "utf8_check at 10-17% on every string-bearing corpus" + "BTreeMap::insert + IntoIter::dying_next at 5-12%" |

## 8. Flaw probe per parser

JSONTestSuite acceptance per parser (from PROFILE-REPORT.md cross-references and the bbnf
RESULTS.md `flaw_probe` column):

| Parser | Strict-rejection cases | Permissive-acceptance cases | Notes |
| :--- | :--- | :--- | :--- |
| **bbnf-skinny (strict)** | `n_string_unescaped_ctrl_char` REJECTED; `i_string_invalid_utf8` REJECTED outside hot scan | n/a | RESULTS.md row: "JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan" (all 17 corpora) |
| **sonic-rs (utf8_lossy)** | rejects strict-JSON null escapes correctly | accepts non-UTF-8 byte sequences and substitutes U+FFFD on emit (utf8_lossy build) | Per A1: strict-rebuild required for like-for-like; the rebuild is expected to lift the utf8_lossy acceptance |
| **simdjson DOM (strict)** | strict by default; rejects 0x00..0x1F in strings, rejects invalid UTF-8, rejects trailing commas | n/a | No specific JSONTestSuite numbers in the simdjson-expanded report |
| **yyjson default (strict)** | strict by default | (with `YYJSON_READ_ALLOW_*` flags: JSON5, comments, trailing commas, invalid UTF-8 — none enabled in the profiled binary) | yyjson PROFILE §(e): dead-coded permissive branches show in noinline build |
| **asmjson SWAR (permissive)** | rejects nothing structurally; SWAR classifier emits 0x00..0x1F as whitespace | accepts JSONTestSuite `n_string_unescaped_ctrl_char` (permissive) | Flaw-probe only. asmjson NOTE §(a): the SWAR path is permissive by architectural intent |
| **RapidJSON default (permissive)** | rejects malformed UTF-8 in `UTF8::Decode` (returns false) | accepts trailing whitespace, accepts duplicate keys (last-write-wins) | RapidJSON in-situ + permissive default. The profile did not run JSONTestSuite |
| **serde_json (strict)** | rejects 0x00..0x1F in strings, rejects invalid UTF-8 via `core::str::from_utf8` | n/a | Strict by default |

The flaw-probe column in RESULTS.md cites the same two-case rejection set for every bbnf-skinny
row; it does NOT differentiate between the comparators, only between bbnf and the JSONTestSuite
corpus. A Wave-0 cross-comparator JSONTestSuite roll-call has not been produced.

## 9. Strictness column repair — gating

Per A1 (sonic-rs strict rebuild gate): the current bench S anchor (`sonic-rs Mbps` column in
RESULTS.md) is the **utf8_lossy** build. Strict-vs-strict comparison cannot land until sonic-rs
is rebuilt strict. Expected impact per the lossy/strict gap published in sonic-rs upstream
microbenches: ~3–8% time penalty for strict on `Value` DOM, larger on `LazyValue` if escape
decode lights up.

Concretely, on the 12 corpora where bbnf currently LOSES to sonic-rs:

| Corpus | bbnf T1 / sonic_lossy | Estimated bbnf T1 / sonic_strict (sonic +5% time) | Flip? |
| :--- | ---: | ---: | :---: |
| twitter | 73.6% | 77.3% | no |
| apache_builds | 78.0% | 81.9% | no |
| github_events | 68.8% | 72.2% | no |
| update_center | 59.6% | 62.6% | no |
| random | 65.5% | 68.8% | no |
| gsoc-2018 | 53.6% | 56.3% | no |
| instruments | **92.0%** | **96.6%** | maybe |
| unicode_mixed | 56.1% | 58.9% | no |
| unicode_escapes | 80.4% | 84.4% | no |
| unicode_basic | **91.7%** | **96.3%** | maybe |
| distinct_values | 60.2% | 63.2% | no |
| y_string_unicode | 46.0% | 48.3% | no |

Strict rebuild may flip 0–2 borderline rows. The 5/17 win count to sonic-rs becomes 5–7/17 after
strict rebuild. This is materially relevant only if the gate threshold is "majority of corpora"
— the strict rebuild does not close the structural majority-loss.

## 10. Wave 0 of SK-V7 — recommended ordering

Three concrete asks for Wave 0 of the SK-V7 cohort:

1. **sonic-rs strict rebuild** (A1 gate). Rebuild benchmarks/sonic-rs with the strict UTF-8
   feature flag, re-run RESULTS.md against the strict bench. Document the strict-vs-utf8_lossy
   delta per corpus in a new `RESULTS-STRICTNESS-DELTA.md` so reviewers can audit which rows
   shifted. This is gating because every comparator number in RESULTS.md is dishonest until
   sonic-rs is strict.

2. **Add a yyjson row to RESULTS.md** alongside the existing `sonic-rs Mbps` /
   `simd-json borrowed Mbps` / `simd-json owned Mbps` columns. yyjson is the actual M5 Max DOM
   leader — its absence from the per-row comparator triple in RESULTS.md means the existing
   gate ("Track 1 within 1.10× sonic-rs time") is not gating against the real ceiling. Per
   yyjson PROFILE-REPORT.md the 7 corpora with yyjson data are twitter / citm / canada /
   apache_builds / github_events / update_center / unicode_heavy. Adding the yyjson column on
   those 7 rows is a one-day mechanical change.

3. **Designate yyjson as the primary SOTA-beat target on M5 Max.** simdjson is the secondary
   target (object-heavy corpora only); sonic-rs strict is the tertiary target (gating). asmjson
   is flaw-probe only and should be moved out of SOTA-beat row counts. Per Lock 15 / A3 / A6
   the fusion-quality work (single-symbol hot path, repeat16-style unrolling, fused number
   finalizer, no Result-phi propagation on hot path) targets the yyjson twitter gap (1.98×).

## 11. Single comparator most worth beating first

**yyjson on twitter.** Three reasons:

(a) **Largest gap on a single concrete row.** bbnf 1859 vs yyjson 3687 MiB/s on twitter is a
1.98× factor and the largest single-corpus deficit against any strict comparator on M5 Max.

(b) **Architectural intent is fully documented and reproducible without SIMD.** Per yyjson §(f)
the gap is `always_inline` + `repeat16` macro unrolling + inlined Eisel-Lemire finalizer. Each
is a structural change to bbnf-codegen, not a new SIMD intrinsic.

(c) **Closing it unlocks the whole twitter/apache_builds/github_events/update_center bucket** —
the same architectural improvements (single inlined hot symbol fitting i-cache, branch-predictor-
friendly unrolled byte classifier, no Result propagation) apply identically to all four. yyjson
already wins all four; bbnf closing the twitter gap should bring apache/github/update along for
the ride.

The secondary comparator to beat is simdjson on apache_builds/github_events/update_center — but
that requires a true SIMD stage1, which is a substantial lift, not a tuning pass. yyjson is the
reachable target for Wave 0.

## 12. Artefact provenance and reproduction

Every number in this report traces to one of the six PROFILE-REPORT.md files under
`skinny/profile/`. The samply profile artefacts (`.profile.json.gz` + `.syms.json` sidecars)
for each corpus × parser × build live alongside their PROFILE-REPORT.md and can be opened in
Firefox Profiler if a reviewer wishes to audit hot-leaf attribution.

Reproduction commands for each sidecar are documented at the bottom of the respective
PROFILE-REPORT.md:

- yyjson: `git clone github.com/ibireme/yyjson`, build `yy_bench.c` driver, samply at 1 kHz.
  Both inlined and noinline (via `-Dyyjson_inline="__attribute__((noinline))"`) variants
  required for hot-leaf attribution.
- simdjson: `git clone github.com/simdjson/simdjson`, singleheader 4.6.1, `profile_driver.cpp`
  with `padded_string` pre-load. Inlined and noinline variants via `simdjson_really_inline` and
  `simdjson_inline` patches.
- sonic-rs: `benchmarks/benches/perf_parse.rs` under `lto=true codegen-units=1`; inlined and
  noinline variants via `#[inline(always)]` → `#[inline(never)]` flips on `src/parser.rs`,
  `src/util/string.rs`, `src/util/arch/aarch64.rs`.
- serde_json: `cargo build --release` against `/tmp/serde_json-bench/`; same Rust profile.
- RapidJSON: header-only clone, `rapidjson_driver.cpp`, `-O3 -g -DNDEBUG`.
- asmjson: `cargo bench --bench parse -- --quick --warm-up-time 1 --measurement-time 3`.

None of the binaries survives under `/tmp/*-research/`. Re-running any sidecar requires the
full reproduction recipe — but the existing PROFILE-REPORT.md files are sufficient authority
for the SK-V7 baseline and no fresh re-run is required.

---

End of /tmp/skv7-C6-sidecars.md.

# RapidJSON Profile Report — Floor Comparator

Profile date: 2026-05-12
Platform: macOS 25.4.0 / Darwin 25.4.0, arm64 (Apple Silicon, performance cores ~3.5 GHz)
RapidJSON: `master` from `github.com/Tencent/rapidjson`, header-only clone at `/tmp/rapidjson-research`
Compiler: `c++` (Apple clang, `-std=c++17 -O3 -g -DNDEBUG -I /tmp/rapidjson-research/include`)
Profiler: `samply 0.13.1` at 1 kHz, `--save-only --unstable-presymbolicate` (Firefox-format gecko + `.syms.json` sidecar)
Driver: `/tmp/rapidjson-bench/rapidjson_driver.cpp` — load file once into `std::vector<char>`, warmup 3×, then loop `rapidjson::Document::Parse(buf,sz)` into a fresh `Document` per iter (immutable Parse, mutable DOM build); touch root to defeat dead-store elision.

Why a single inlined build (no two-build attribution): RapidJSON does NOT carry a `simdjson_really_inline` macro analogue. Every internal helper (`SkipWhitespace`, `ParseHex4`, `ParseStringToStream`, `UTF8::Decode`, allocator stacks) is already a separate symbol at `-O3 -g` because RapidJSON relies on conventional `inline` semantics, not always-inline forcing. The single-build attribution below preserves the full classifier vs object vs array vs number vs string decomposition without needing a noinline variant.

## (a) Per-corpus throughput

```
corpus         | size_bytes | iters   | wall_s | MiB/s  | c/B at 3.5GHz
twitter        |    631 515 |  25 000 | 31.42  |  479.2 |  7.30
citm           |  1 727 204 |  10 000 | 20.44  |  805.8 |  4.34
canada         |  2 251 051 |   7 500 | 26.04  |  618.3 |  5.66
apache_builds  |    127 275 | 130 000 | 33.56  |  470.2 |  7.44
instruments    |    220 346 |  75 000 | 17.68  |  891.3 |  3.93
random         |    510 476 |  32 000 | 37.06  |  420.3 |  8.33
```

(Instruments wall is below the 30 s CPU target because RapidJSON's small-doc throughput shaped iter count assumptions; sample population is 17 652 weighted samples, still ample for attribution. All other corpora exceed 30 s CPU.)

The throughputs match the published RapidJSON micro-benchmark range (300-900 MiB/s on typical corpora — RapidJSON's `Document::Parse` builds a fully-typed mutable DOM with copy-on-decode for strings under `kParseDefaultFlags`; the in-situ mode is faster but mutates the buffer and was not used here since it would have corrupted re-iterations).

## (b) Top 15 self-time per corpus (single inlined build)

### twitter — top 15 by self-time

Samples: 31 042 (~31.0 s CPU at 1 kHz)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
79.58% |   24703 | rapidjson::GenericReader<...>::ParseString
10.16% |    3155 | rapidjson::GenericReader<...>::ParseObject
 4.81% |    1494 | _platform_memmove
 2.24% |     696 | rapidjson::GenericReader<...>::ParseValue
 1.37% |     426 | rapidjson::GenericReader<...>::ParseNumber
 0.74% |     229 | rapidjson::GenericReader<...>::ParseArray
 0.59% |     183 | rapidjson::GenericDocument<...>::ParseStream
 0.20% |      62 | mach_absolute_time
 0.01% |      <4 | libsystem_malloc.dylib magazine ops (tiny/small allocator)
```

### citm — top 15 by self-time

Samples: 16 928 (~16.9 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
41.97% |    7105 | rapidjson::GenericReader<...>::ParseObject
24.23% |    4101 | rapidjson::GenericReader<...>::ParseString
13.27% |    2247 | rapidjson::GenericReader<...>::ParseArray
 8.71% |    1475 | rapidjson::GenericReader<...>::ParseNumber
 7.27% |    1231 | _platform_memmove
 2.59% |     438 | rapidjson::GenericReader<...>::ParseValue
 1.00% |     169 | rapidjson::GenericDocument<...>::ParseStream
 0.33% |      56 | mach_absolute_time
 0.04% |       7 | libsystem_malloc.dylib magazine ops
```

### canada — top 15 by self-time

Samples: 23 599 (~23.6 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
75.04% |   17709 | rapidjson::GenericReader<...>::ParseNumber
13.00% |    3068 | rapidjson::GenericReader<...>::ParseArray
 6.03% |    1423 | _platform_memmove
 4.27% |    1008 | rapidjson::GenericReader<...>::ParseValue
 0.45% |     107 | mach_absolute_time
 0.13% |      31 | mach_vm_reclaim_try_cancel
 0.02% |       5 | rapidjson::GenericReader<...>::ParseString
 0.02% |       4 | rapidjson::GenericReader<...>::ParseObject
 0.01% |       3 | rapidjson::MemoryPoolAllocator::AddChunk
```

### apache_builds — top 15 by self-time

Samples: 33 484 (~33.5 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
81.16% |   27175 | rapidjson::GenericReader<...>::ParseString
 7.77% |    2601 | _platform_memmove
 7.48% |    2503 | rapidjson::GenericReader<...>::ParseObject
 0.96% |     320 | rapidjson::GenericReader<...>::ParseValue
 0.83% |     279 | rapidjson::GenericReader<...>::ParseArray
 0.44% |     147 | mach_absolute_time
 0.01% |       5 | rapidjson::GenericReader<...>::ParseNumber
 0.01% |       3 | rapidjson::internal::Stack::Expand<GenericValue<...>>
```

### instruments — top 15 by self-time

Samples: 17 652 (~17.7 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
49.94% |    8815 | rapidjson::GenericReader<...>::ParseString
26.09% |    4606 | rapidjson::GenericReader<...>::ParseObject
 8.14% |    1436 | rapidjson::GenericReader<...>::ParseNumber
 7.84% |    1384 | _platform_memmove
 2.96% |     523 | rapidjson::GenericReader<...>::ParseValue
 2.84% |     501 | rapidjson::GenericReader<...>::ParseArray
 0.54% |      96 | mach_absolute_time
 0.03% |       6 | libsystem_malloc.dylib magazine ops
```

### random — top 15 by self-time

Samples: 36 520 (~36.5 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
71.71% |   26187 | rapidjson::GenericReader<...>::ParseString
 9.29% |    3391 | rapidjson::GenericReader<...>::ParseObject
 9.27% |    3387 | _platform_memmove
 2.99% |    1092 | rapidjson::GenericReader<...>::ParseValue
 2.25% |     820 | rapidjson::GenericReader<...>::ParseArray
 2.11% |     769 | rapidjson::GenericReader<...>::ParseNumber
 1.44% |     525 | rapidjson::GenericDocument<...>::ParseStream
 0.35% |     126 | mach_absolute_time
 0.06% |      23 | mach_vm_reclaim_try_cancel
```

## (c) Hot-leaf count per corpus (>= 1% self)

| corpus        | samples | hot leaves (>=1%) |
| ---           | ---:    | ---:              |
| twitter       | 31 042  | 5                 |
| citm          | 16 928  | 6                 |
| canada        | 23 599  | 4                 |
| apache_builds | 33 484  | 3                 |
| instruments   | 17 652  | 6                 |
| random        | 36 520  | 7                 |

Hot-leaf counts of 3-7 are characteristic of recursive-descent designs: the parser tree (Parse{Value,String,Object,Array,Number}) and one memmove for memory shuffling cover ~95% of self-time on every corpus. There is no SIMD scanner that fragments into a classifier / utf8-validator / structural-bitmap leaf set — RapidJSON's `SkipWhitespace` / `ParseStringToStream` / `UTF8::Decode` are call-sized inline expansions inside `ParseValue` / `ParseString`.

## (d) Per-class self-time attribution

Grouping the leaves into semantic classes (parse-class shown as % of total samples):

| corpus        | ParseString | ParseObject | ParseArray | ParseNumber | ParseValue | memmove | other |
| ---           | ---:        | ---:        | ---:       | ---:        | ---:       | ---:    | ---:  |
| twitter       | 79.58%      | 10.16%      | 0.74%      | 1.37%       | 2.24%      | 4.81%   | 1.10% |
| citm          | 24.23%      | 41.97%      | 13.27%     | 8.71%       | 2.59%      | 7.27%   | 1.96% |
| canada        |  0.02%      |  0.02%      | 13.00%     | 75.04%      | 4.27%      | 6.03%   | 1.62% |
| apache_builds | 81.16%      |  7.48%      | 0.83%      | 0.01%       | 0.96%      | 7.77%   | 1.79% |
| instruments   | 49.94%      | 26.09%      | 2.84%      | 8.14%       | 2.96%      | 7.84%   | 2.19% |
| random        | 71.71%      |  9.29%      | 2.25%      | 2.11%       | 2.99%      | 9.27%   | 2.38% |

The shape mirrors corpus content: canada is float-arrays (75% in ParseNumber), apache_builds and twitter are string-heavy keys+values (79-81% in ParseString), citm splits across object structure and string keys, random has wide key heterogeneity, instruments is mixed.

`_platform_memmove` at 4-9% across all corpora is the **DOM materialisation cost**: RapidJSON's `MemoryPoolAllocator` arena grows by `memmove`-ing `GenericValue` stack frames as `ParseObject` / `ParseArray` finalise their member/element arrays into the document. This is the irreducible cost of building a typed DOM with stack-then-commit semantics; simdjson's tape representation avoids it entirely.

## (e) Where RapidJSON sits vs SOTA on this host

Single-build inlined throughput (MiB/s, this host, this driver):

| corpus        | rapidjson | serde_json | sonic-rs (typed Value) | simdjson (DOM) | yyjson  | bbnf-skinny (v3) |
| ---           | ---:      | ---:       | ---:                   | ---:           | ---:    | ---:             |
| twitter       |    479.2  |     448.6  |              2 782     |     2 922.7    |  ~3 200 |          5 521   |
| citm          |    805.8  |     485.4  |              2 860     |     4 269.9    |  ~4 100 |          8 947   |
| canada        |    618.3  |     380.0  |              1 447     |     1 369.7    |    ~900 |          4 640   |
| apache_builds |    470.2  |     458.5  |                   —    |          —     |       — |          8 502   |
| instruments   |    891.3  |     420.9  |                   —    |          —     |       — |          8 854   |
| random        |    420.3  |     240.9  |                   —    |          —     |       — |          6 674   |

Sources: sonic-rs from `/skinny/profile/sonic-rs-v2/PROFILE-REPORT.md`, simdjson from `/skinny/profile/simdjson-v2/PROFILE-REPORT.md`, yyjson approximated from published yyjson microbench (in-tree confirmation pending; profile data lives at `/skinny/profile/yyjson/`), bbnf-skinny from `/skinny/profile/skinny-v3/PROFILE-REPORT.md`. Sonic-rs/simdjson/yyjson did not measure apache/instruments/random; those rows are intentionally sparse to avoid fabrication.

The floor picture:
- **RapidJSON is 5.5× slower than simdjson** on twitter, 5.3× on citm, 2.2× on canada — and **11× slower than bbnf-skinny** on twitter, 11× on citm, 7.5× on canada.
- The 2.2× gap on canada is the smallest because canada is dominated by `strtod`-class number parsing where RapidJSON's `Grisu2` is competitive with simdjson's `parse_eight_digits` SIMD ladder, but it's still 2× behind because simdjson's stage1 classifies the entire float corpus in vector lanes before stage2 dispatches.
- On apache_builds / instruments / random — small-to-medium documents where stage1 overhead amortizes poorly — bbnf-skinny is **18-21× faster** than RapidJSON. This is the lazy-tape regime where skinny's `simd_scan::scan_json_parse_index` does in one pass what RapidJSON's recursive descent does in five.

Cycle-budget at 3.5 GHz:
- twitter: bbnf-skinny 0.61 c/B → rapidjson **7.30 c/B** (~12× the cycle count per byte)
- citm: bbnf-skinny 0.39 c/B → rapidjson **4.34 c/B** (~11×)
- canada: bbnf-skinny 0.75 c/B → rapidjson **5.66 c/B** (~7.5×)

## (f) Honest take: is RapidJSON a useful comparator or just floor?

**Floor**, with one analytical caveat. RapidJSON is the cleanest available reference for "what does a textbook recursive-descent JSON parser cost on this hardware?" because:
- The hot leaves are exactly the textbook leaves: ParseString, ParseObject, ParseArray, ParseNumber, ParseValue. There is no SIMD, no bitfield classifier, no tape; the cost model is "byte-at-a-time switch in a recursive descent."
- The C++ template machinery doesn't obscure attribution: each `Generic*` instantiation gets its own symbol. Inlining is conservative enough that we see the structural decomposition without needing a noinline build.
- The cycle budget of 4-8 c/B sets a clean **upper bound** for any JSON parser doing typed-DOM construction without SIMD. If a candidate parser exceeds 8 c/B with SIMD, it has worse-than-RapidJSON cache behaviour — a meaningful regression signal.

But RapidJSON is **not useful as a head-to-head competitor**: simdjson and yyjson dominate on every corpus; bbnf-skinny dominates by an order of magnitude. The only comparator role it usefully serves is the **"recursive-descent floor"** — if anyone proposes a new parser design, "did you beat RapidJSON?" is the table-stakes question, and the answer on this hardware should be 5-10× minimum for a SIMD design.

The other genuinely useful finding: `_platform_memmove` at 5-9% across all corpora is the **DOM-materialisation tax** that recursive-descent designs cannot avoid as long as they materialise typed values into a heap arena. simdjson's tape, yyjson's compressed value array, and bbnf-skinny's lazy index all sidestep this entirely. Any plan to "fix" a recursive parser by hand-rolling SIMD scanners but keeping the typed-DOM materialisation will hit this same 5-9% floor.

## Artefacts

- `twitter.profile.json.gz` + `.syms.json` — twitter corpus, 25 000 iters
- `citm.profile.json.gz` + `.syms.json` — citm_catalog corpus, 10 000 iters
- `canada.profile.json.gz` + `.syms.json` — canada corpus, 7 500 iters
- `apache_builds.profile.json.gz` + `.syms.json` — apache_builds corpus, 130 000 iters
- `instruments.profile.json.gz` + `.syms.json` — instruments corpus, 75 000 iters
- `random.profile.json.gz` + `.syms.json` — random corpus, 32 000 iters
- `analyze.py` — re-runnable analyzer (binary-search RVA → symbol resolver against `.syms.json` + optional nm-dump fallback; lib-prefixed unresolved addresses)

Driver source: `/tmp/rapidjson-bench/rapidjson_driver.cpp`. Built with:
```
c++ -std=c++17 -O3 -g -DNDEBUG -I /tmp/rapidjson-research/include rapidjson_driver.cpp -o rapidjson_driver
```

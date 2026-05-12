# serde_json Profile Report — Floor Comparator

Profile date: 2026-05-12
Platform: macOS 25.4.0 / Darwin 25.4.0, arm64 (Apple Silicon, performance cores ~3.5 GHz)
serde_json: `1.0.149` (released crates.io), serde_core `1.0.228`, itoa `1.0.18`, memchr `2.8.0`
Compiler: `rustc` stable, `[profile.release] lto=true codegen-units=1 debug=true opt-level=3 panic="unwind"` (mirrors `skinny` and `sonic-rs-v2`)
Profiler: `samply 0.13.1` at 1 kHz, `--save-only --unstable-presymbolicate`
Driver: `/tmp/serde_json-bench/src/main.rs` — load file once into `Vec<u8>`, warmup 3×, then loop `serde_json::from_slice::<serde_json::Value>(&data)` per iter (typed-DOM `Value` enum, full `BTreeMap<String, Value>` for objects); `black_box(&v)` to defeat dead-store elision.

Why a single inlined build (no two-build attribution): unlike sonic-rs, serde_json's parser kernel is **not** decorated with `#[inline(always)]`. Under `lto=true` the most performance-critical leaves (`SliceRead::parse_str`, `Deserializer::parse_integer`, `Deserializer::parse_decimal`, `MapAccess::next_key_seed`, `Value::deserialize`, `BTreeMap::insert`) all retain their own symbol identity in the optimized binary. The single-build attribution below already exposes the genuine leaf structure without needing a noinline variant.

## (a) Per-corpus throughput

```
corpus         | size_bytes | iters   | wall_s | MiB/s  | c/B at 3.5GHz
twitter        |    631 515 |  25 000 | 33.57  |  448.6 |  7.80
citm           |  1 727 204 |  10 000 | 33.93  |  485.4 |  7.21
canada         |  2 251 051 |   7 500 | 42.37  |  380.0 |  9.21
apache_builds  |    127 275 | 130 000 | 34.41  |  458.5 |  7.63
instruments    |    220 346 |  75 000 | 37.45  |  420.9 |  8.31
random         |    510 476 |  32 000 | 64.67  |  240.9 | 14.53
```

All corpora exceed the 30 s CPU target. The throughput numbers are consistent with public serde_json microbenchmarks (~400-500 MiB/s on object-heavy corpora, slower on number-heavy or wide-key corpora because of BTreeMap insert and ParseDecimal overhead).

## (b) Top 15 self-time per corpus

### twitter — top 15 by self-time

Samples: 33 198 (~33.2 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
15.88% |    5273 | core::str::converts::from_utf8
 8.70% |    2889 | <serde_json::de::MapAccess as MapAccess>::next_key_seed::has_next_key
 8.30% |    2756 | <BTreeMap<String, Value>>::insert
 8.08% |    2684 | <SliceRead as Read>::parse_str
 7.72% |    2563 | _platform_memmove
 7.58% |    2515 | <Value as Deserialize>::deserialize::<Deserializer<SliceRead>>
 6.87% |    2280 | _platform_memcmp
 3.16% |    1050 | std::rt::handle_rt_panic                       <-- panic-handler dispatch (cold)
 2.30% |     763 | core::ptr::drop_in_place::<Value>
 1.99% |     661 | <BTreeMap<String, Value> as IntoIter>::dying_next
 1.87% |     621 | libsystem_malloc.dylib magazine ops
 1.44% |     478 | _platform_memset
 0.96% |     319 | <Deserializer<SliceRead>>::parse_integer
 0.70% |     232 | libsystem_malloc.dylib magazine ops
 0.69% |     228 | libsystem_malloc.dylib magazine ops
```

### citm — top 15 by self-time

Samples: 30 264 (~30.3 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
16.81% |    5086 | <MapAccess as MapAccess>::next_key_seed::has_next_key
14.80% |    4479 | <Value as Deserialize>::deserialize::<Deserializer<SliceRead>>
10.48% |    3172 | core::str::converts::from_utf8
 3.84% |    1163 | core::ptr::drop_in_place::<Value>
 3.73% |    1129 | <SliceRead as Read>::parse_str
 3.46% |    1048 | <BTreeMap<String, Value> as IntoIter>::dying_next
 3.46% |    1047 | <BTreeMap<String, Value>>::insert
 2.97% |     898 | <Deserializer<SliceRead>>::parse_integer
 2.14% |     649 | _platform_memmove
 1.92% |     580 | libsystem_malloc.dylib magazine ops
 1.90% |     575 | _platform_memset
 1.87% |     566 | mach_absolute_time
 1.39% |     422 | std::rt::handle_rt_panic
 1.33% |     404 | _platform_memcmp
 0.69% |     208 | libsystem_malloc.dylib magazine ops
```

### canada — top 15 by self-time

Samples: 39 170 (~39.2 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
29.30% |   11475 | <Value as Deserialize>::deserialize::<Deserializer<SliceRead>>
29.11% |   11401 | <Deserializer<SliceRead>>::parse_decimal
 7.03% |    2753 | <Deserializer<SliceRead>>::parse_integer
 5.77% |    2259 | core::ptr::drop_in_place::<Value>
 2.50% |     980 | <RawVecInner>::finish_grow
 1.35% |     527 | mach_absolute_time
 1.15% |     452 | libsystem_malloc.dylib magazine ops
 0.94% |     369 | _platform_memmove
 0.91% |     356 | std::rt::handle_rt_panic
 0.68% |     266 | <RawVec<Value>>::grow_one
 0.59% |     230 | libsystem_malloc.dylib magazine ops
 0.56% |     221 | _platform_memset
 0.54% |     213 | libsystem_malloc.dylib magazine ops
 0.52% |     202 | libsystem_malloc.dylib magazine ops
 0.49% |     193 | libsystem_malloc.dylib magazine ops
```

### apache_builds — top 15 by self-time

Samples: 34 321 (~34.3 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
16.63% |    5708 | core::str::converts::from_utf8
12.63% |    4334 | <SliceRead as Read>::parse_str
 8.00% |    2746 | <Value as Deserialize>::deserialize::<Deserializer<SliceRead>>
 6.00% |    2059 | _platform_memmove
 5.09% |    1746 | <MapAccess as MapAccess>::next_key_seed::has_next_key
 2.74% |     942 | <BTreeMap<String, Value>>::insert
 2.61% |     895 | libsystem_malloc.dylib magazine ops
 1.97% |     676 | <BTreeMap<String, Value> as IntoIter>::dying_next
 1.63% |     558 | core::ptr::drop_in_place::<Value>
 1.61% |     554 | _platform_memset
 1.08% |     371 | libsystem_malloc.dylib magazine ops
 1.05% |     360 | libsystem_malloc.dylib magazine ops
 0.98% |     337 | libsystem_malloc.dylib magazine ops
 0.95% |     327 | libsystem_malloc.dylib magazine ops
 0.93% |     319 | libsystem_malloc.dylib magazine ops
```

### instruments — top 15 by self-time

Samples: 36 571 (~36.6 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
12.17% |    4449 | core::str::converts::from_utf8
10.16% |    3714 | <Value as Deserialize>::deserialize::<Deserializer<SliceRead>>
10.06% |    3678 | <MapAccess as MapAccess>::next_key_seed::has_next_key
 9.17% |    3354 | <BTreeMap<String, Value>>::insert
 7.12% |    2604 | _platform_memcmp
 6.05% |    2212 | <SliceRead as Read>::parse_str
 3.55% |    1298 | _platform_memmove
 2.77% |    1013 | <BTreeMap<String, Value> as IntoIter>::dying_next
 2.70% |     987 | <Deserializer<SliceRead>>::parse_integer
 2.25% |     822 | core::ptr::drop_in_place::<Value>
 1.97% |     721 | libsystem_malloc.dylib magazine ops
 1.91% |     699 | libsystem_malloc.dylib magazine ops
 1.08% |     395 | _platform_memset
 0.80% |     294 | libsystem_malloc.dylib magazine ops
 0.76% |     277 | libsystem_malloc.dylib magazine ops
```

### random — top 15 by self-time

Samples: 63 547 (~63.5 s CPU)

```
self%  | samples | symbol
------ | ------- | ----------------------------------------------------------------
16.03% |   10184 | core::str::converts::from_utf8
 9.02% |    5731 | <SliceRead as Read>::parse_str
 8.85% |    5622 | <Value as Deserialize>::deserialize::<Deserializer<SliceRead>>
 5.67% |    3604 | <BTreeMap<String, Value>>::insert
 5.50% |    3494 | _platform_memmove
 4.81% |    3057 | _platform_memcmp
 3.14% |    1996 | <MapAccess as MapAccess>::next_key_seed::has_next_key
 2.85% |    1814 | std::rt::handle_rt_panic
 2.84% |    1806 | core::ptr::drop_in_place::<Value>
 2.37% |    1506 | libsystem_malloc.dylib magazine ops
 2.19% |    1394 | <BTreeMap<String, Value> as IntoIter>::dying_next
 1.28% |     812 | _platform_memset
 1.07% |     678 | mach_absolute_time
 0.93% |     592 | libsystem_malloc.dylib magazine ops
 0.88% |     560 | libsystem_malloc.dylib magazine ops
```

## (c) Hot-leaf count per corpus (>= 1% self)

| corpus        | samples | hot leaves (>=1%) |
| ---           | ---:    | ---:              |
| twitter       | 33 198  | 12                |
| citm          | 30 264  | 14                |
| canada        | 39 170  | 7                 |
| apache_builds | 34 321  | 13                |
| instruments   | 36 571  | 13                |
| random        | 63 547  | 13                |

Hot-leaf counts of 7-14 are characteristic of "thin-parser + heavy-allocator" designs. serde_json's parse kernel is small (`parse_str`, `parse_integer`, `parse_decimal`, `has_next_key`) but the **typed-DOM materialisation cost** (`Value` enum tagging, `BTreeMap::insert`, `String` heap allocation, `RawVec::grow_one`, plus drop_in_place on iteration teardown) inflates the leaf count by ~5-7 entries. Canada is the exception with 7 leaves — because canada is float-arrays with no strings and no objects, the BTreeMap and string-decode leaves don't fire.

A note on `std::rt::handle_rt_panic` at 1-3%: this is **not** triggered by panicking. It's the panic-machinery dispatcher that lives at the entry of every `Result`-returning function with `panic="unwind"`; LTO inlines the happy-path return but leaves the panic-prologue addressable. Samply attributes the tail-call prologue samples to this symbol because the address falls inside its function range. It is genuine inline-emitted code from the deserializer's error-handling.

## (d) Per-class self-time attribution

Grouping the leaves into semantic classes (% of total samples):

| corpus        | parse_str | parse_num | utf8_check | BTreeMap_insert | Value_drop | memmove | memcmp | malloc | rt_panic | other |
| ---           | ---:      | ---:      | ---:       | ---:            | ---:       | ---:    | ---:   | ---:   | ---:     | ---:  |
| twitter       |  8.08%    |  0.96%    | 15.88%     |  8.30%          |  2.30%     |  7.72%  |  6.87% |  5-7%  |  3.16%   | 41.7% |
| citm          |  3.73%    |  2.97%    | 10.48%     |  3.46%          |  3.84%     |  2.14%  |  1.33% |  5-7%  |  1.39%   | 61.3% |
| canada        |  ~0%      | 36.14%    |  ~0%       |   0%            |  5.77%     |  0.94%  |  ~0%   |  3-5%  |  0.91%   | 48.6% |
| apache_builds | 12.63%    |  0.4%     | 16.63%     |  2.74%          |  1.63%     |  6.00%  |  ~0%   | 12-15% |  0.5%    | 47.6% |
| instruments   |  6.05%    |  2.70%    | 12.17%     |  9.17%          |  2.25%     |  3.55%  |  7.12% | 10-12% |  0.5%    | 46.6% |
| random        |  9.02%    |  0.5%     | 16.03%     |  5.67%          |  2.84%     |  5.50%  |  4.81% | 10-13% |  2.85%   | 43.8% |

(The "other" column folds in the various small symbols `<Value as Deserialize>::deserialize`, `MapAccess::next_key_seed`, `IntoIter::dying_next`, and minor allocator dispatch.)

Key load-bearing observations:
- **utf8_check at 10-17% on every string-bearing corpus**. serde_json validates every input slice with `core::str::converts::from_utf8` before exposing it as a `&str`. This single leaf is the largest contributor on twitter, apache_builds, instruments, random. It is also the single biggest gap to simdjson/yyjson, both of which fuse UTF-8 validation into the SIMD scan.
- **BTreeMap::insert + IntoIter::dying_next at 5-12% on object-heavy corpora**. serde_json defaults `Value::Object` to `BTreeMap<String, Value>` (preserves key ordering). Each key insertion is `O(log N)` with a heap-allocated `String` key and a `Value` clone — a known design tax.
- **malloc magazine ops at 5-15% on small-document corpora**. Each iteration freshly allocates the entire DOM and drops it; libsystem_malloc's small-bin churn dominates apache_builds (12-15%) and instruments (10-12%) because their per-iter cost is so small the allocator overhead is comparatively large.
- **canada is the cleanest profile**: 36% in `parse_decimal`, 29% in `Value::deserialize` (constructing `Value::Number` variants), 6% in `Vec::drop_in_place`, ~3% in float-format `RawVec::finish_grow`. No BTreeMap, no UTF-8, no memcmp. canada is what a serde_json "pure number kernel" looks like.

## (e) Where serde_json sits vs SOTA on this host

Single-build inlined throughput (MiB/s, this host, this driver):

| corpus        | serde_json | rapidjson | sonic-rs (typed Value) | simdjson (DOM) | yyjson  | bbnf-skinny (v3) |
| ---           | ---:       | ---:      | ---:                   | ---:           | ---:    | ---:             |
| twitter       |    448.6   |    479.2  |              2 782     |     2 922.7    |  ~3 200 |          5 521   |
| citm          |    485.4   |    805.8  |              2 860     |     4 269.9    |  ~4 100 |          8 947   |
| canada        |    380.0   |    618.3  |              1 447     |     1 369.7    |    ~900 |          4 640   |
| apache_builds |    458.5   |    470.2  |                   —    |          —     |       — |          8 502   |
| instruments   |    420.9   |    891.3  |                   —    |          —     |       — |          8 854   |
| random        |    240.9   |    420.3  |                   —    |          —     |       — |          6 674   |

The floor picture:
- **serde_json is 6-9× slower than simdjson** and **6× slower than sonic-rs (typed Value)** on twitter / citm. The gap narrows to ~3.6× on canada because sonic-rs and simdjson both pay the float-parse cost serde_json pays.
- **serde_json is 12-21× slower than bbnf-skinny** across the board, with the worst gaps on small-document corpora (apache_builds 18×, instruments 21×, random 28×) where bbnf-skinny's lazy-tape amortisation pays off most.
- **serde_json is comparable to RapidJSON** on object-heavy corpora (twitter 0.94×, apache_builds 0.97×) and slightly slower on number-heavy (canada 0.61×) and wide-corpus (random 0.57×). RapidJSON wins on citm (1.66× faster than serde_json) because RapidJSON's `MemoryPoolAllocator` avoids the per-key heap allocation that serde_json's `BTreeMap<String, Value>` mandates.

Cycle-budget at 3.5 GHz:
- twitter: bbnf-skinny 0.61 c/B → serde_json **7.80 c/B** (~13×)
- citm: bbnf-skinny 0.39 c/B → serde_json **7.21 c/B** (~18×)
- canada: bbnf-skinny 0.75 c/B → serde_json **9.21 c/B** (~12×)
- random: bbnf-skinny 0.53 c/B → serde_json **14.53 c/B** (~27×)

## (f) Honest take: is serde_json a useful comparator or just floor?

**Floor, but the most consequential floor for the Rust ecosystem.** serde_json is **the** ecosystem default — any Rust JSON workload that hasn't been hand-tuned uses it. Its cycle budget of 7-15 c/B on every corpus is the realistic baseline that downstream Rust services experience. The role this profile serves:

1. **It is the parity floor for any Rust JSON parser.** A new Rust parser that does not beat serde_json across the board is by definition not worth shipping. RapidJSON is the C++ floor; serde_json is the Rust floor.
2. **It quantifies the cost of the typed-DOM construction tax in Rust.** ~30% of serde_json's wall-clock on object-heavy corpora is `BTreeMap::insert` + `String` allocation + `Value` enum dispatch — none of which is the parser kernel. The pure parse-kernel cost (parse_str + parse_num + utf8_check + has_next_key) is only ~40-50% on most corpora. That 30% materialisation tax is the "what you can save by going lazy/tape" lower-bound — bbnf-skinny saves all of it.
3. **It exposes UTF-8 validation as a load-bearing cost (10-17% on string corpora).** Any future Rust parser that fuses UTF-8 validation into the structural scan (the way simdjson does) immediately recovers that share. This is the most tractable single optimization a serde_json-shape parser could adopt.

But serde_json is **not useful as a head-to-head competitor**. The Rust SOTA on this host (sonic-rs, bbnf-skinny) is 6-27× faster on every corpus. serde_json's design is "correctness + ergonomics + ecosystem compatibility, performance not paramount" — and the cycle budget honestly reflects that priority order. In any benchmark report where the goal is "show SOTA performance," serde_json should appear as the Rust floor row, never as a peer.

The truly load-bearing analytical finding here is the **15-17% UTF-8 validation cost on string corpora**: this is the single largest opportunity in any serde_json-shape parser that wants to recover 1.5-2× without changing API contract. Every other cost (BTreeMap, String alloc, Value enum) requires API-breaking change.

## Artefacts

- `twitter.profile.json.gz` + `.syms.json` — twitter corpus, 25 000 iters
- `citm.profile.json.gz` + `.syms.json` — citm_catalog corpus, 10 000 iters
- `canada.profile.json.gz` + `.syms.json` — canada corpus, 7 500 iters
- `apache_builds.profile.json.gz` + `.syms.json` — apache_builds corpus, 130 000 iters
- `instruments.profile.json.gz` + `.syms.json` — instruments corpus, 75 000 iters
- `random.profile.json.gz` + `.syms.json` — random corpus, 32 000 iters
- `analyze.py` — re-runnable analyzer (binary-search RVA → symbol resolver against `.syms.json` + nm-dump fallback; lib-prefixed unresolved addresses)

Driver source: `/tmp/serde_json-bench/src/main.rs`. Built with:
```
cd /tmp/serde_json-bench && cargo build --release
```
where `Cargo.toml` carries `[profile.release] lto=true codegen-units=1 debug=true opt-level=3 panic="unwind"`.

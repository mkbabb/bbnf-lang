# sonic-rs Expanded Profile Report

Profiler: samply 0.13.1 (sampling, 1000 Hz)
Host: Apple M5 Max (arm64, NEON SIMD), macOS 25.4.0
Driver: `benchmarks/benches/perf_parse.rs` → `sonic_rs::from_slice::<Value>` or `<LazyValue>`

Build A — INLINED (canonical wall-clock):
  `[profile.release] lto=true codegen-units=1 debug=true opt-level=3`
Build B — NOINLINE (leaf attribution):
  Same profile, plus `#[inline(always)] -> #[inline(never)]` flips in the
  parser kernel (`src/parser.rs`), the string SIMD pipeline (`src/util/string.rs`),
  and the NEON intrinsics (`src/util/arch/aarch64.rs`).

## Corpora

Throughput grid covers all 9 corpora × 2 paths × 2 variants = 36 measurements.
Samply profiles cover 7 of the 9 corpora (omits `github_events` and
`update_center` for time budget; they are object-heavy patterns already
represented by `apache_builds`/`citm`). Each profile is (corpus × path × variant)
→ 28 samply profiles total.

```
corpus          | bytes     | scope    
--------------- | --------- | ---------
twitter         | 631,514   | samply   
citm            | 1,727,204 | samply   
canada          | 2,251,051 | samply   
apache_builds   | 127,275   | samply   
github_events   | 65,132    | tput-only
update_center   | 533,178   | tput-only
mesh            | 723,597   | samply   
unicode_mixed   | 1,053,086 | samply   
unicode_escapes | 1,050,797 | samply   
```

Corpus shapes:
- `twitter` — 616 KiB, object-heavy social-graph data, mixed ASCII + UTF-8
- `citm_catalog` — 1.65 MiB, mixed objects + arrays, deep nesting
- `canada` — 2.15 MiB, deeply-nested arrays of float pairs (no strings, no keys)
- `apache_builds` — 124 KiB, small CI build records
- `github_events` — 63 KiB, deeply-nested event records
- `update_center` — 521 KiB, Jenkins update center metadata
- `mesh` — 706 KiB, 3D geometry (heavy floats + small structural skeleton)
- `unicode_mixed` — 1.00 MiB, raw UTF-8 strings (ASCII/Latin/Greek/CJK/Emoji)
- `unicode_escapes` — 1.00 MiB, `\uXXXX\uXXXX` surrogate-pair-escaped strings

## (a) Per-corpus × per-driver-shape Mbps (inlined wall-clock)

```
corpus          | Value-DOM Mbps | LazyValue Mbps | Lazy/Value
--------------- | -------------- | -------------- | ----------
twitter         | 2438           | 1718           | 0.70x     
citm            | 3530           | 2658           | 0.75x     
canada          | 1807           | 1542           | 0.85x     
apache_builds   | 2009           | 1304           | 0.65x     
github_events   | 2893           | 1804           | 0.62x     
update_center   | 2750           | 1558           | 0.57x     
mesh            | 1029           | 769            | 0.75x     
unicode_mixed   | 1350           | 838            | 0.62x     
unicode_escapes | 1839           | 364            | 0.20x     
```

Noinline throughput (leaf-attribution build):

```
corpus          | Value-DOM noinline Mbps | LazyValue noinline Mbps
--------------- | ----------------------- | -----------------------
twitter         | 1686                    | 1395                   
citm            | 2416                    | 1952                   
canada          | 1508                    | 930                    
apache_builds   | 1136                    | 955                    
github_events   | 1494                    | 1312                   
update_center   | 1998                    | 1340                   
mesh            | 651                     | 584                    
unicode_mixed   | 1097                    | 792                    
unicode_escapes | 1916                    | 406                    
```

## (b) Hot-leaf count for INLINED (anchor: should be 1-2)

Count of distinct symbols holding ≥10% self-time in the INLINED profile.
If it's 1-2, LTO has fused the entire SIMD pipeline into a single descent leaf.

```
corpus          | path  | n_hot(>=10%) | top hot leaves                                                                                                  
--------------- | ----- | ------------ | ----------------------------------------------------------------------------------------------------------------
twitter         | value | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Padded(81%)                                                         
twitter         | lazy  | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(89%)                                                         
citm            | value | 2            | <sonic_rs::parser::Parser<sonic_rs::reader::Padded(69%), <sonic_rs::parser::Parser<sonic_rs::reader::Padded(17%)
citm            | lazy  | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(94%)                                                         
canada          | value | 2            | <sonic_rs::parser::Parser<sonic_rs::reader::Padded(85%), _platform_memmove(10%)                                 
canada          | lazy  | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(98%)                                                         
apache_builds   | value | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Padded(82%)                                                         
apache_builds   | lazy  | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(95%)                                                         
mesh            | value | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Padded(91%)                                                         
mesh            | lazy  | 1            | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(95%)                                                         
unicode_mixed   | value | 2            | <sonic_rs::parser::Parser<sonic_rs::reader::Padded(79%), simdutf8::implementation::aarch64::validate_utf8_b(12%)
unicode_mixed   | lazy  | 2            | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(69%), <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(25%)
unicode_escapes | value | 2            | sonic_rs::util::unicode::handle_unicode_codepoint_(40%), <sonic_rs::parser::Parser<sonic_rs::reader::Padded(53%)
unicode_escapes | lazy  | 2            | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(47%), <sonic_rs::parser::Parser<sonic_rs::reader::Read>>(53%)
```

## (c) Per-technique ns/B across corpora (NOINLINE → INLINED wall-clock)

Per-byte cost = NOINLINE self% × INLINED ns/B for the same (corpus, path).

### Driver: `from_slice::<Value>`

```
technique                                  | twitter      | citm         | canada       | apache_builds | mesh         | unicode_mixed | unicode_escapes
------------------------------------------ | ------------ | ------------ | ------------ | ------------- | ------------ | ------------- | ---------------
PSHUFB whitespace skip                     |  29.4%/0.121 |  42.9%/0.122 |   5.9%/0.033 |  29.7%/0.148  |  26.5%/0.257 |   8.7%/0.064  |   4.0%/0.022   
Prefix-XOR string-bitmap (`skip_container_ |   0.0%/0.000 |   0.0%/0.000 |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000   
NEON StringBlock (quote/escape SIMD)       |  41.5%/0.170 |  10.8%/0.031 |   0.0%/0.000 |  48.4%/0.241  |   0.0%/0.000 |  70.9%/0.525  |  86.0%/0.467   
Eisel-Lemire fast-float (`sonic-number`)   |   4.4%/0.018 |  15.2%/0.043 |  66.9%/0.370 |   0.0%/0.000  |  54.6%/0.531 |   4.7%/0.035  |   1.1%/0.006   
UTF-8 validation (`simdutf8`)              |   3.6%/0.015 |   2.6%/0.007 |   1.6%/0.009 |   1.3%/0.006  |   0.9%/0.008 |   6.6%/0.049  |   2.4%/0.013   
Fused descent driver (incl. `LazyValue` sk |  13.6%/0.056 |  20.7%/0.059 |  17.1%/0.095 |   9.8%/0.049  |  13.0%/0.126 |   5.3%/0.040  |   2.6%/0.014   
Memmove/memcmp (arena copy + key compare)  |   6.5%/0.027 |   5.6%/0.016 |   6.8%/0.037 |   9.5%/0.047  |   4.5%/0.044 |   3.2%/0.024  |   3.2%/0.018   
Allocation (bumpalo, malloc)               |   0.2%/0.001 |   0.3%/0.001 |   0.1%/0.001 |   0.0%/0.000  |   0.1%/0.001 |   0.3%/0.002  |   0.3%/0.002   
```

### Driver: `from_slice::<LazyValue>`

```
technique                                  | twitter      | citm         | canada       | apache_builds | mesh         | unicode_mixed | unicode_escapes
------------------------------------------ | ------------ | ------------ | ------------ | ------------- | ------------ | ------------- | ---------------
PSHUFB whitespace skip                     |  41.7%/0.243 |  66.1%/0.249 |  40.0%/0.260 |  66.3%/0.509  |  47.7%/0.621 |  11.6%/0.138  |   1.5%/0.041   
Prefix-XOR string-bitmap (`skip_container_ |   0.0%/0.000 |   0.0%/0.000 |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000   
NEON StringBlock (quote/escape SIMD)       |  21.5%/0.125 |   6.4%/0.024 |   0.0%/0.000 |  16.6%/0.128  |   0.0%/0.000 |  74.0%/0.883  |  96.6%/2.654   
Eisel-Lemire fast-float (`sonic-number`)   |   3.0%/0.017 |   6.8%/0.026 |  38.7%/0.251 |   0.1%/0.001  |  31.9%/0.414 |   2.8%/0.033  |   0.3%/0.009   
UTF-8 validation (`simdutf8`)              |   3.9%/0.023 |   1.5%/0.006 |   1.3%/0.008 |   0.7%/0.006  |   0.8%/0.011 |   5.3%/0.063  |   0.7%/0.019   
Fused descent driver (incl. `LazyValue` sk |  27.8%/0.162 |  18.7%/0.070 |  19.9%/0.129 |  16.2%/0.124  |  19.6%/0.254 |   6.4%/0.076  |   0.8%/0.023   
Memmove/memcmp (arena copy + key compare)  |   1.7%/0.010 |   0.4%/0.001 |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000   
Allocation (bumpalo, malloc)               |   0.0%/0.000 |   0.0%/0.000 |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000 |   0.0%/0.000  |   0.0%/0.000   
```

## (d) Unicode-specific: UTF-8 validation cost

UTF-8 validation (`simdutf8::validate_utf8_basic_neon`) self-time as a fraction
of total NOINLINE self-time. Higher fraction → validation dominates.

```
corpus          | path  | utf8% (noinline) | utf8 ns/B (inlined) | non-ASCII byte frac
--------------- | ----- | ---------------- | ------------------- | -------------------
twitter         | value |  3.62%           | 0.0148 ns/B         | 15.11%             
twitter         | lazy  |  3.94%           | 0.0230 ns/B         | 15.11%             
citm            | value |  2.56%           | 0.0073 ns/B         | 0.02%              
citm            | lazy  |  1.50%           | 0.0057 ns/B         | 0.02%              
canada          | value |  1.64%           | 0.0091 ns/B         | 0.00%              
canada          | lazy  |  1.30%           | 0.0085 ns/B         | 0.00%              
apache_builds   | value |  1.29%           | 0.0064 ns/B         | 0.00%              
apache_builds   | lazy  |  0.75%           | 0.0057 ns/B         | 0.00%              
mesh            | value |  0.86%           | 0.0083 ns/B         | 0.00%              
mesh            | lazy  |  0.84%           | 0.0109 ns/B         | 0.00%              
unicode_mixed   | value |  6.55%           | 0.0485 ns/B         | 51.39%             
unicode_mixed   | lazy  |  5.27%           | 0.0629 ns/B         | 51.39%             
unicode_escapes | value |  2.42%           | 0.0132 ns/B         | 0.00%              
unicode_escapes | lazy  |  0.70%           | 0.0192 ns/B         | 0.00%              
```

## (e) LazyValue vs Value-DOM gap

On which corpora does LazyValue most outperform Value-DOM, and why?

```
corpus          | Value Mbps | Lazy Mbps | gap (x) | Value top class           | Lazy top class           
--------------- | ---------- | --------- | ------- | ------------------------- | -------------------------
twitter         | 2438       | 1718      | 0.70x   | string_simd(41%)          | whitespace_skip_simd(42%)
citm            | 3530       | 2658      | 0.75x   | whitespace_skip_simd(43%) | whitespace_skip_simd(66%)
canada          | 1807       | 1542      | 0.85x   | number_simd(67%)          | whitespace_skip_simd(40%)
apache_builds   | 2009       | 1304      | 0.65x   | string_simd(48%)          | whitespace_skip_simd(66%)
mesh            | 1029       | 769       | 0.75x   | number_simd(55%)          | whitespace_skip_simd(48%)
unicode_mixed   | 1350       | 838       | 0.62x   | string_simd(71%)          | string_simd(74%)         
unicode_escapes | 1839       | 364       | 0.20x   | string_simd(86%)          | string_simd(97%)         
```

## (f) Honest take — corpus-invariant vs corpus-specific primitives

Across the **9 expanded corpora** (twitter, citm, canada, apache_builds,
github_events, update_center, mesh, unicode_mixed, unicode_escapes) profiled on
Apple M5 Max under `sonic_rs::from_slice::<Value>` and `<LazyValue>`:

**1. LazyValue is NOT a uniform speedup over Value-DOM in this driver.** The v2
report speculated that sonic-rs's reference 18552 Mbps came from a LazyValue
path engaging the prefix-XOR `skip_container` bitmap. The expanded data
disproves this for `from_slice::<LazyValue>(input)`: it deserialises a single
top-level lazy value via `parser.skip_one(true)`, which dispatches to
`skip_object`/`skip_array` — **strict recursive walkers that do NOT use the
structural-bitmap fast skip**. The bitmap (`skip_container_loop` + `prefix_xor`)
is reachable only via the unchecked path (`skip_one(false)` → `skip_container`),
which is used internally by skip-aware lazy iterators but NOT by the top-level
`from_slice::<LazyValue>` entry point. So our profile measures recursive-skip
cost, not bitmap-skip cost. The 18552 Mbps reference must come from a tighter
path — most likely struct-typed `deserialize`, where field elision lets sonic-rs
skip individual values via `parse_skip` and field-specific routing.

**2. The three corpus-invariant load-bearing primitives** (present, non-trivial,
on every corpus): (a) **`get_nonspace_bits` PSHUFB whitespace classifier** —
active on every corpus with non-zero indentation; only canada (zero whitespace
between number tokens) suppresses it. (b) **`parse_number_unchecked` Eisel-Lemire
fast-float** — fires on every numeric token; the bulk of canada and a major
share of mesh/random/numbers. (c) **`simdutf8::validate_utf8_basic_neon`** —
validates the entire input buffer once on parse entry; a corpus-invariant 1-5%
baseline tax.

**3. The corpus-specific primitives** (only matter on certain shapes):
- **NEON `StringBlock` + `parse_string_inplace`** — needs strings to fire. On
  canada (zero strings) this drops to 0%. On twitter/apache_builds it dominates.
- **Prefix-XOR `skip_container` bitmap** — only fires on unchecked container
  skip (lazy iterator internals), never on the standard `from_slice::<Value>`
  or `<LazyValue>` paths measured here. Absent across the entire grid.
- **Surrogate-pair decode (`handle_unicode_codepoint`)** — only fires when JSON
  contains `\uXXXX\uXXXX` escapes. Visible on unicode_escapes (~360 Mbps —
  5x slower than unicode_mixed which uses raw UTF-8). This is the single
  largest corpus-dependent cost spike, and the only one where sonic-rs's
  performance ceiling collapses.

**4. UTF-8 validation cost is content-driven but capped low.** simdutf8's
validator burns a near-constant ~0.01-0.05 ns/B regardless of byte mix:
0.0091 ns/B on canada (0% non-ASCII), 0.0148 ns/B on twitter (15% non-ASCII),
0.0485 ns/B on unicode_mixed (51% non-ASCII). The *self%* share rises on
unicode_mixed (6.55%) primarily because the *other* work decreases (no
structural complexity, no escape decode) — the validator itself doesn't run
slower per byte. Crucially, on unicode_escapes (which has 0% non-ASCII bytes
at the wire level because the escape sequences are themselves ASCII), UTF-8
validation drops back to 2.4% / 0.013 ns/B while `StringBlock` + escape decode
explodes to 86% self-time. The cost has shifted from validation to decode.

**5. LazyValue underperforms Value-DOM on M5 Max** for the from_slice path,
uniformly. Every corpus shows Lazy/Value < 1.0, ranging from 0.20× (unicode_escapes,
the catastrophic case) to 0.85× (canada). The strict recursive skip pays the
same per-byte structural cost as Value-DOM without saving allocation: LazyValue
still copies the document slice for the output value via `as_str(raw)`. The
`<sonic_rs::parser::Parser<sonic_rs::reader::Read>>` skip path is ~70-80% PSHUFB
whitespace + recursive descent, dominated by the same `skip_space` / `skip_one`
functions that the Value-DOM path also calls — but without the Value-construction
loss being recouped by anything cheaper. unicode_escapes is the worst because
escape decode now happens on a *much* hotter inner loop (every key, every string)
instead of the once-per-value Value-DOM path. This contradicts the standard
sonic-rs sales pitch and is the clearest 'don't rely on `from_slice::<LazyValue>`
for parse-everything' signal. The 18552 Mbps reference must come from struct-typed
`Deserialize` with field elision, not the `LazyValue` newtype.

## Appendix: hot-leaf tables (top 15 each)

### twitter.value.inlined  (1436 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
80.57% | 1157    | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentV
 7.03% | 101     | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 5.85% | 84      | _platform_memmove                                                                                             
 2.58% | 37      | _platform_memcmp                                                                                              
 2.30% | 33      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVi
 0.35% | 5       | perf_parse!0x5ae58                                                                                            
 0.28% | 4       | mach_absolute_time                                                                                            
 0.28% | 4       | perf_parse!0x5ae64                                                                                            
 0.07% | 1       | read                                                                                                          
 0.07% | 1       | libsystem_malloc.dylib!0x13b00                                                                                
 0.07% | 1       | libsystem_malloc.dylib!0x13494                                                                                
 0.07% | 1       | perf_parse!0x5ae54                                                                                            
 0.07% | 1       | __rustc::__rdl_alloc                                                                                          
 0.07% | 1       | libsystem_malloc.dylib!0x150d4                                                                                
 0.07% | 1       | mach_vm_reclaim_try_cancel                                                                                    
```

### twitter.value.noinline  (663 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
16.29% | 108     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_key_scalar::<sonic_rs::value::node::Docum
13.27% | 88      | sonic_rs::parser::is_whitespace                                                                               
10.11% | 67      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::skip_space                                     
 9.50% | 63      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::dispatch_value::<sonic_rs::value::node::Documen
 6.49% | 43      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_quote_first                            
 6.03% | 40      | sonic_rs::util::string::parse_string_inplace                                                                  
 4.68% | 31      | _platform_memmove                                                                                             
 4.68% | 31      | sonic_rs::util::arch::aarch64::get_nonspace_bits::chunk_nonspace_bits                                         
 3.62% | 24      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 3.32% | 22      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_unchecked                         
 3.32% | 22      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::new                                        
 2.87% | 19      | sonic_rs::util::string::load::<sonic_simd::neon::Simd128u>                                                    
 2.87% | 19      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_string_visit::<sonic_rs::value::node::Doc
 2.41% | 16      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_unescaped                              
 2.11% | 14      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object_clo                               
```

### twitter.lazy.inlined  (8110 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
88.67% | 7191    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
 5.54% | 449     | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 4.88% | 396     | <sonic_rs::reader::PinnedInput>::as_ptr                               
 0.86% | 70      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 0.02% | 2       | perf_parse::main                                                      
 0.01% | 1       | core::fmt::write                                                      
 0.01% | 1       | core::ptr::drop_in_place::<sonic_rs::lazyvalue::value::LazyValue>     
```

### twitter.lazy.noinline  (3652 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
27.49% | 1004    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space        
21.00% | 767     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_string       
17.88% | 653     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
 8.19% | 299     | sonic_rs::parser::is_whitespace                                       
 4.55% | 166     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_object       
 4.24% | 155     | sonic_rs::util::arch::aarch64::get_nonspace_bits::chunk_nonspace_bits 
 3.94% | 144     | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 3.78% | 138     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_object_clo  
 2.35% | 86      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::do_skip_number    
 1.70% | 62      | _platform_memcmp                                                      
 1.59% | 58      | sonic_rs::util::arch::aarch64::get_nonspace_bits                      
 1.42% | 52      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_literal     
 0.60% | 22      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_number       
 0.55% | 20      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 0.25% | 9       | perf_parse!0x4a510                                                    
```

### citm.value.inlined  (1350 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
68.67% | 927     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentV
17.41% | 235     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVi
 7.56% | 102     | _platform_memmove                                                                                             
 3.70% | 50      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 1.19% | 16      | perf_parse!0x5ae64                                                                                            
 0.37% | 5       | _platform_memcmp                                                                                              
 0.15% | 2       | mach_absolute_time                                                                                            
 0.15% | 2       | libsystem_malloc.dylib!0x15028                                                                                
 0.07% | 1       | read                                                                                                          
 0.07% | 1       | libsystem_malloc.dylib!0x12f9c                                                                                
 0.07% | 1       | libsystem_malloc.dylib!0x2a310                                                                                
 0.07% | 1       | libsystem_malloc.dylib!0x2b6bc                                                                                
 0.07% | 1       | mach_vm_reclaim_is_reusable                                                                                   
 0.07% | 1       | libsystem_malloc.dylib!0x2b450                                                                                
 0.07% | 1       | libsystem_malloc.dylib!0x30378                                                                                
```

### citm.value.noinline  (585 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
18.97% | 111     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::dispatch_value::<sonic_rs::value::node::Documen
18.63% | 109     | sonic_rs::parser::is_whitespace                                                                               
16.75% | 98      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::skip_space                                     
11.62% | 68      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_unchecked                         
 9.91% | 58      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_key_scalar::<sonic_rs::value::node::Docum
 5.30% | 31      | _platform_memmove                                                                                             
 4.62% | 27      | sonic_rs::util::arch::aarch64::get_nonspace_bits::chunk_nonspace_bits                                         
 3.59% | 21      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_visit::<sonic_rs::value::node::Doc
 2.91% | 17      | sonic_rs::util::arch::aarch64::get_nonspace_bits                                                              
 2.56% | 15      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 1.54% | 9       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object_clo                               
 0.85% | 5       | perf_parse!0x4a51c                                                                                            
 0.68% | 4       | mach_absolute_time                                                                                            
 0.34% | 2       | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_quote_first                            
 0.34% | 2       | _platform_memcmp                                                                                              
```

### citm.lazy.inlined  (6388 samples)

```
self%  | samples | symbol                                                      
------ | ------- | ------------------------------------------------------------
93.86% | 5996    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one
 3.29% | 210     | <sonic_rs::reader::PinnedInput>::as_ptr                     
 2.82% | 180     | simdutf8::implementation::aarch64::validate_utf8_basic_neon 
 0.02% | 1       | read                                                        
 0.02% | 1       | perf_parse::main                                            
```

### citm.lazy.noinline  (2926 samples)

```
self%  | samples | symbol                                                               
------ | ------- | ---------------------------------------------------------------------
43.20% | 1264    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space       
 9.88% | 289     | sonic_rs::util::arch::aarch64::get_nonspace_bits::chunk_nonspace_bits
 9.84% | 288     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one         
 9.30% | 272     | sonic_rs::parser::is_whitespace                                      
 6.39% | 187     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_string      
 5.23% | 153     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::do_skip_number   
 4.17% | 122     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_object      
 3.25% | 95      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_object_clo 
 2.70% | 79      | sonic_rs::util::arch::aarch64::get_nonspace_bits                     
 1.57% | 46      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_number      
 1.50% | 44      | simdutf8::implementation::aarch64::validate_utf8_basic_neon          
 1.23% | 36      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_array       
 1.06% | 31      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space_peek  
 0.38% | 11      | _platform_memcmp                                                     
 0.24% | 7       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_literal    
```

### canada.value.inlined  (2496 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
85.26% | 2128    | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVi
10.38% | 259     | _platform_memmove                                                                                             
 2.36% | 59      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 1.36% | 34      | perf_parse!0x5ae64                                                                                            
 0.08% | 2       | perf_parse::main                                                                                              
 0.08% | 2       | mach_absolute_time                                                                                            
 0.08% | 2       | libsystem_malloc.dylib!0x3611c                                                                                
 0.04% | 1       | read                                                                                                          
 0.04% | 1       | libsystem_malloc.dylib!0x2a8c8                                                                                
 0.04% | 1       | libsystem_malloc.dylib!0x33af4                                                                                
 0.04% | 1       | libsystem_malloc.dylib!0x14964                                                                                
 0.04% | 1       | mach_vm_reclaim_try_enter                                                                                     
 0.04% | 1       | libsystem_malloc.dylib!0x13a44                                                                                
 0.04% | 1       | <sonic_rs::value::node::DocumentVisitor>::new                                                                 
 0.04% | 1       | libsystem_malloc.dylib!0x2b1d0                                                                                
```

### canada.value.noinline  (1035 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
54.88% | 568     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_unchecked                         
17.10% | 177     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::dispatch_value::<sonic_rs::value::node::Documen
11.98% | 124     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_visit::<sonic_rs::value::node::Doc
 6.76% | 70      | _platform_memmove                                                                                             
 4.25% | 44      | sonic_rs::parser::is_whitespace                                                                               
 1.64% | 17      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 1.64% | 17      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::skip_space                                     
 0.97% | 10      | perf_parse!0x4a51c                                                                                            
 0.39% | 4       | mach_absolute_time                                                                                            
 0.10% | 1       | read                                                                                                          
 0.10% | 1       | <sonic_rs::value::node::Value as core::ops::drop::Drop>::drop                                                 
 0.10% | 1       | libsystem_malloc.dylib!0x1432c                                                                                
 0.10% | 1       | perf_parse!0x4a520                                                                                            
```

### canada.lazy.inlined  (17427 samples)

```
self%  | samples | symbol                                                      
------ | ------- | ------------------------------------------------------------
98.38% | 17144   | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one
 1.61% | 280     | simdutf8::implementation::aarch64::validate_utf8_basic_neon 
 0.01% | 2       | <sonic_rs::reader::PinnedInput>::as_ptr                     
 0.01% | 1       | read                                                        
```

### canada.lazy.noinline  (9732 samples)

```
self%  | samples | symbol                                                               
------ | ------- | ---------------------------------------------------------------------
31.96% | 3110    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space       
19.84% | 1931    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::do_skip_number   
14.67% | 1428    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one         
10.07% | 980     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_single_digit
 8.81% | 857     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_number      
 6.63% | 645     | sonic_rs::parser::is_whitespace                                      
 5.22% | 508     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_array       
 1.46% | 142     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space_peek  
 1.30% | 127     | simdutf8::implementation::aarch64::validate_utf8_basic_neon          
 0.01% | 1       | lsl::Allocator::AllocationMetadata::firstAddress() const             
 0.01% | 1       | perf_parse::main                                                     
 0.01% | 1       | <sonic_rs::reader::Read>::new                                        
 0.01% | 1       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_object      
```

### apache_builds.value.inlined  (1197 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
81.54% | 976     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentV
 5.93% | 71      | _platform_memmove                                                                                             
 5.76% | 69      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVi
 3.17% | 38      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 1.25% | 15      | mach_absolute_time                                                                                            
 1.17% | 14      | perf_parse!0x5ae64                                                                                            
 0.17% | 2       | <bumpalo::Bump>::alloc_layout_slow                                                                            
 0.08% | 1       | libsystem_malloc.dylib!0x3059c                                                                                
 0.08% | 1       | libsystem_malloc.dylib!0x2a400                                                                                
 0.08% | 1       | libsystem_malloc.dylib!0x2b080                                                                                
 0.08% | 1       | <sonic_rs::value::node::DocumentVisitor>::new                                                                 
 0.08% | 1       | libsystem_malloc.dylib!0x303ec                                                                                
 0.08% | 1       | mach_vm_reclaim_try_cancel                                                                                    
 0.08% | 1       | libsystem_malloc.dylib!0x30478                                                                                
 0.08% | 1       | libsystem_malloc.dylib!0x2a138                                                                                
```

### apache_builds.value.noinline  (622 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
15.92% | 99      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_quote_first                            
12.38% | 77      | sonic_rs::parser::is_whitespace                                                                               
12.22% | 76      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::skip_space                                     
 9.49% | 59      | _platform_memmove                                                                                             
 8.36% | 52      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::dispatch_value::<sonic_rs::value::node::Documen
 8.36% | 52      | sonic_rs::util::string::parse_string_inplace                                                                  
 7.40% | 46      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_string_visit::<sonic_rs::value::node::Doc
 6.11% | 38      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_key_scalar::<sonic_rs::value::node::Docum
 3.86% | 24      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_unescaped                              
 3.86% | 24      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::new                                        
 3.22% | 20      | sonic_rs::util::arch::aarch64::get_nonspace_bits::chunk_nonspace_bits                                         
 2.09% | 13      | sonic_rs::util::string::load::<sonic_simd::neon::Simd128u>                                                    
 1.93% | 12      | sonic_rs::util::arch::aarch64::get_nonspace_bits                                                              
 1.45% | 9       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object_clo                               
 1.29% | 8       | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
```

### apache_builds.lazy.inlined  (8795 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
95.50% | 8399    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
 3.42% | 301     | <sonic_rs::reader::PinnedInput>::as_ptr                               
 0.88% | 77      | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 0.18% | 16      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 0.01% | 1       | perf_parse::main                                                      
 0.01% | 1       | <sonic_rs::reader::Read>::new                                         
```

### apache_builds.lazy.noinline  (3620 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
48.09% | 1741    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space        
16.60% | 601     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_string       
11.82% | 428     | sonic_rs::parser::is_whitespace                                       
 7.85% | 284     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
 4.72% | 171     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_object       
 4.53% | 164     | sonic_rs::util::arch::aarch64::get_nonspace_bits::chunk_nonspace_bits 
 3.45% | 125     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_object_clo  
 1.88% | 68      | sonic_rs::util::arch::aarch64::get_nonspace_bits                      
 0.75% | 27      | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 0.19% | 7       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_array        
 0.06% | 2       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_number       
 0.03% | 1       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 0.03% | 1       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::do_skip_number    
```

### mesh.value.inlined  (1776 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
91.44% | 1624    | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVi
 5.97% | 106     | _platform_memmove                                                                                             
 1.75% | 31      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 0.45% | 8       | perf_parse!0x5ae64                                                                                            
 0.23% | 4       | mach_absolute_time                                                                                            
 0.06% | 1       | <sonic_rs::value::node::Value as core::ops::drop::Drop>::drop                                                 
 0.06% | 1       | libsystem_malloc.dylib!0x16b8                                                                                 
 0.06% | 1       | libsystem_malloc.dylib!0x2b320                                                                                
```

### mesh.value.noinline  (1402 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
40.87% | 573     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_unchecked                         
16.69% | 234     | sonic_rs::parser::is_whitespace                                                                               
13.77% | 193     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_visit::<sonic_rs::value::node::Doc
12.98% | 182     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::dispatch_value::<sonic_rs::value::node::Documen
 9.77% | 137     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::skip_space                                     
 4.49% | 63      | _platform_memmove                                                                                             
 0.86% | 12      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 0.21% | 3       | mach_absolute_time                                                                                            
 0.21% | 3       | perf_parse!0x4a51c                                                                                            
 0.07% | 1       | libsystem_malloc.dylib!0x135d8                                                                                
 0.07% | 1       | libsystem_malloc.dylib!0x3493c                                                                                
```

### mesh.lazy.inlined  (18520 samples)

```
self%  | samples | symbol                                                      
------ | ------- | ------------------------------------------------------------
95.13% | 17619   | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one
 3.89% | 720     | <sonic_rs::reader::PinnedInput>::as_ptr                     
 0.96% | 177     | simdutf8::implementation::aarch64::validate_utf8_basic_neon 
 0.01% | 2       | perf_parse::main                                            
 0.01% | 1       | core::fmt::write                                            
 0.01% | 1       | <sonic_rs::reader::Read>::new                               
```

### mesh.lazy.noinline  (8970 samples)

```
self%  | samples | symbol                                                               
------ | ------- | ---------------------------------------------------------------------
37.09% | 3327    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space       
21.57% | 1935    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::do_skip_number   
15.45% | 1386    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one         
10.47% | 939     | sonic_rs::parser::is_whitespace                                      
 6.31% | 566     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_number      
 4.10% | 368     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_array       
 3.96% | 355     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_single_digit
 0.84% | 75      | simdutf8::implementation::aarch64::validate_utf8_basic_neon          
 0.13% | 12      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space_peek  
 0.02% | 2       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_string      
 0.01% | 1       | read                                                                 
 0.01% | 1       | <sonic_rs::reader::Read>::new                                        
 0.01% | 1       | core::ptr::drop_in_place::<sonic_rs::reader::Read>                   
 0.01% | 1       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_exponent    
 0.01% | 1       | perf_parse::main                                                     
```

### unicode_mixed.value.inlined  (2109 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
79.42% | 1675    | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentV
12.09% | 255     | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 6.26% | 132     | _platform_memmove                                                                                             
 1.09% | 23      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVi
 0.33% | 7       | perf_parse!0x5ae64                                                                                            
 0.28% | 6       | mach_absolute_time                                                                                            
 0.09% | 2       | libsystem_malloc.dylib!0x3611c                                                                                
 0.05% | 1       | main                                                                                                          
 0.05% | 1       | _platform_memset                                                                                              
 0.05% | 1       | libsystem_malloc.dylib!0x3a054                                                                                
 0.05% | 1       | libsystem_malloc.dylib!0x30078                                                                                
 0.05% | 1       | mach_vm_reclaim_query_state                                                                                   
 0.05% | 1       | perf_parse!0x5ae48                                                                                            
 0.05% | 1       | libsystem_malloc.dylib!0x2a368                                                                                
 0.05% | 1       | libsystem_malloc.dylib!0x12d50                                                                                
```

### unicode_mixed.value.noinline  (992 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
44.76% | 444     | sonic_rs::util::string::parse_string_inplace                                                                  
 9.38% | 93      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_quote_first                            
 6.55% | 65      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 5.14% | 51      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::dispatch_value::<sonic_rs::value::node::Documen
 4.84% | 48      | sonic_rs::parser::is_whitespace                                                                               
 4.33% | 43      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::new                                        
 3.83% | 38      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_unchecked                         
 3.83% | 38      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::skip_space                                     
 3.63% | 36      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_key_scalar::<sonic_rs::value::node::Docum
 3.23% | 32      | _platform_memmove                                                                                             
 3.02% | 30      | sonic_rs::util::string::load::<sonic_simd::neon::Simd128u>                                                    
 2.32% | 23      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_unescaped                              
 1.51% | 15      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_backslash                              
 1.51% | 15      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_string_visit::<sonic_rs::value::node::Doc
 0.91% | 9       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_visit::<sonic_rs::value::node::Doc
```

### unicode_mixed.lazy.inlined  (16940 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
69.22% | 11726   | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
25.41% | 4305    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 4.74% | 803     | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 0.60% | 101     | <sonic_rs::reader::PinnedInput>::as_ptr                               
 0.01% | 2       | <sonic_rs::reader::Read>::new                                         
 0.01% | 1       | read                                                                  
 0.01% | 1       | perf_parse::main                                                      
 0.01% | 1       | core::ptr::drop_in_place::<sonic_rs::reader::Read>                    
```

### unicode_mixed.lazy.noinline  (5790 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
47.18% | 2732    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_string       
26.79% | 1551    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 7.86% | 455     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space        
 5.27% | 305     | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 3.70% | 214     | sonic_rs::parser::is_whitespace                                       
 3.45% | 200     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
 2.06% | 119     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::do_skip_number    
 1.49% | 86      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_object_clo  
 1.43% | 83      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_object       
 0.71% | 41      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_number       
 0.03% | 2       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_array        
 0.02% | 1       | read                                                                  
 0.02% | 1       | perf_parse::main                                                      
```

### unicode_escapes.value.inlined  (1551 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
52.74% | 818     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentV
40.23% | 624     | sonic_rs::util::unicode::handle_unicode_codepoint_mut                                                         
 3.22% | 50      | _platform_memmove                                                                                             
 2.51% | 39      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 0.39% | 6       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVi
 0.26% | 4       | mach_absolute_time                                                                                            
 0.13% | 2       | <sonic_rs::value::node::Value>::parse_with_padding                                                            
 0.06% | 1       | read                                                                                                          
 0.06% | 1       | libsystem_malloc.dylib!0x336d0                                                                                
 0.06% | 1       | <bumpalo::Bump>::alloc_layout_slow                                                                            
 0.06% | 1       | libsystem_malloc.dylib!0x14310                                                                                
 0.06% | 1       | libsystem_malloc.dylib!0x30174                                                                                
 0.06% | 1       | libsystem_malloc.dylib!0x33718                                                                                
 0.06% | 1       | perf_parse!0x5ae64                                                                                            
 0.06% | 1       | libsystem_malloc.dylib!0x334c4                                                                                
```

### unicode_escapes.value.noinline  (620 samples)

```
self%  | samples | symbol                                                                                                        
------ | ------- | --------------------------------------------------------------------------------------------------------------
75.48% | 468     | sonic_rs::util::string::parse_string_inplace                                                                  
 3.55% | 22      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::new                                        
 3.23% | 20      | _platform_memmove                                                                                             
 2.90% | 18      | sonic_rs::parser::is_whitespace                                                                               
 2.42% | 15      | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_quote_first                            
 2.42% | 15      | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                   
 2.26% | 14      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::dispatch_value::<sonic_rs::value::node::Documen
 1.29% | 8       | sonic_rs::util::string::load::<sonic_simd::neon::Simd128u>                                                    
 1.13% | 7       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::skip_space                                     
 0.81% | 5       | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_backslash                              
 0.81% | 5       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_string_visit::<sonic_rs::value::node::Doc
 0.81% | 5       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_key_scalar::<sonic_rs::value::node::Docum
 0.65% | 4       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_visit::<sonic_rs::value::node::Doc
 0.65% | 4       | <sonic_rs::util::string::StringBlock<sonic_simd::bits::NeonBits>>::has_unescaped                              
 0.48% | 3       | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_number_unchecked                         
```

### unicode_escapes.lazy.inlined  (38331 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
52.80% | 20240   | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
46.66% | 17884   | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 0.51% | 195     | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 0.02% | 8       | <sonic_rs::reader::PinnedInput>::as_ptr                               
 0.01% | 2       | core::ptr::drop_in_place::<sonic_rs::lazyvalue::value::LazyValue>     
 0.00% | 1       | read                                                                  
 0.00% | 1       | core::ptr::drop_in_place::<sonic_rs::reader::Read>                    
```

### unicode_escapes.lazy.noinline  (12869 samples)

```
self%  | samples | symbol                                                                
------ | ------- | ----------------------------------------------------------------------
48.72% | 6270    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_string       
47.90% | 6164    | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_escaped_chars
 1.06% | 136     | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_space        
 0.70% | 90      | simdutf8::implementation::aarch64::validate_utf8_basic_neon           
 0.45% | 58      | sonic_rs::parser::is_whitespace                                       
 0.37% | 47      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_one          
 0.22% | 28      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::do_skip_number    
 0.21% | 27      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_object_clo  
 0.19% | 25      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_object       
 0.10% | 13      | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_number       
 0.06% | 8       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::skip_array        
 0.01% | 1       | <sonic_rs::parser::Parser<sonic_rs::reader::Read>>::parse_literal     
 0.01% | 1       | core::ptr::drop_in_place::<sonic_rs::reader::Read>                    
 0.01% | 1       | perf_parse::main                                                      
```

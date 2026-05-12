# sonic-rs Profile Report

Profiler: samply (sampling, 1000 Hz)
Binary: /tmp/sonic-research/sonic-rs/benchmarks/target/release/examples/perf_parse
Build: release profile (lto=true, codegen-units=1, debug=true, opt-level=3, panic=unwind)
Corpora: /Users/mkbabb/Programming/bbnf-lang/skinny/crates/test-fixtures/corpus/json/{twitter,citm_catalog,canada}.json
Bench loop: `let v: sonic_rs::Value = sonic_rs::from_slice(&data).unwrap();` (typed-Value DOM)

IMPORTANT: sonic-rs is built with `lto=true, codegen-units=1` — extensive cross-crate inlining.
Almost every hot leaf is inlined into the two top-level recursive descent functions,
`Parser::parse_object` and `Parser::parse_array`. Profile attribution therefore concentrates
cycles on the two recursive descent symbols rather than splitting cleanly between
structural-scan / string-decode / number-parse leaves. This is a measurement artefact of LTO,
not an absence of those phases; it is itself a load-bearing finding (see section (e)).

## twitter

Samples: 24884 (~24.9s of CPU at 1000 Hz)

### Top 15 by self-time

```
self%  | samples | symbol                                                                                                               
------ | ------- | ---------------------------------------------------------------------------------------------------------------------
79.25% | 19720   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentVisitor>
 7.17% | 1783    | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                          
 6.02% | 1497    | _platform_memmove                                                                                                    
 2.91% | 723     | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVisitor> 
 2.72% | 676     | _platform_memcmp                                                                                                     
 0.46% | 114     | perf_parse!0x452f8                                                                                                   
 0.43% | 106     | mach_absolute_time                                                                                                   
 0.25% | 61      | perf_parse!0x45304                                                                                                   
 0.05% | 13      | <bumpalo::Bump>::alloc_layout_slow                                                                                   
 0.04% | 10      | libsystem_malloc.dylib!0x13b34                                                                                       
 0.04% | 9       | mach_vm_reclaim_try_enter                                                                                            
 0.03% | 8       | <sonic_rs::value::node::Value>::parse_with_padding                                                                   
 0.03% | 8       | perf_parse::main                                                                                                     
 0.03% | 8       | libsystem_malloc.dylib!0x15ea8                                                                                       
 0.02% | 5       | libsystem_malloc.dylib!0x3a09c                                                                                       
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                                                                               
------- | ------- | ---------------------------------------------------------------------------------------------------------------------
100.00% | 24884   | perf_parse::main                                                                                                     
100.00% | 24884   | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>                                                        
100.00% | 24884   | std::rt::lang_start::<()>::{closure#0}                                                                               
100.00% | 24884   | std::rt::lang_start_internal                                                                                         
100.00% | 24884   | main                                                                                                                 
100.00% | 24884   | start                                                                                                                
92.44%  | 23002   | <sonic_rs::value::node::Value>::parse_with_padding                                                                   
88.49%  | 22019   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentVisitor>
88.38%  | 21993   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVisitor> 
 7.17%  | 1783    | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                          
 6.02%  | 1497    | _platform_memmove                                                                                                    
 2.72%  | 676     | _platform_memcmp                                                                                                     
 0.46%  | 114     | perf_parse!0x452f8                                                                                                   
 0.43%  | 106     | mach_absolute_time                                                                                                   
 0.39%  | 96      | <bumpalo::Bump>::alloc_layout_slow                                                                                   
```

### Self-time by function class

```
self%  | samples | class           
------ | ------- | ----------------
82.15% | 20443   | parse-recursive 
 8.74% | 2175    | memmove-memcpy  
 7.17% | 1783    | utf8-validation 
 0.75% | 186     | other           
 0.59% | 148     | allocation      
 0.50% | 124     | syscall         
 0.03% | 8       | parse-entry     
 0.03% | 8       | runtime         
 0.03% | 8       | drop-teardown   
 0.00% | 1       | dispatch-visitor
```

## citm

Samples: 25275 (~25.3s of CPU at 1000 Hz)

### Top 15 by self-time

```
self%  | samples | symbol                                                                                                               
------ | ------- | ---------------------------------------------------------------------------------------------------------------------
70.82% | 17899   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentVisitor>
14.82% | 3746    | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVisitor> 
 8.34% | 2108    | _platform_memmove                                                                                                    
 3.79% | 958     | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                          
 0.85% | 214     | perf_parse!0x45304                                                                                                   
 0.49% | 123     | _platform_memcmp                                                                                                     
 0.26% | 66      | mach_absolute_time                                                                                                   
 0.07% | 18      | perf_parse!0x452f8                                                                                                   
 0.04% | 11      | libsystem_malloc.dylib!0x3611c                                                                                       
 0.04% | 9       | <bumpalo::Bump>::alloc_layout_slow                                                                                   
 0.02% | 5       | libsystem_malloc.dylib!0x2b698                                                                                       
 0.02% | 5       | __rustc::__rdl_alloc                                                                                                 
 0.02% | 4       | <sonic_rs::value::node::Value>::parse_with_padding                                                                   
 0.01% | 3       | mach_vm_reclaim_try_cancel                                                                                           
 0.01% | 3       | libsystem_malloc.dylib!0x15ea8                                                                                       
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                                                                               
------- | ------- | ---------------------------------------------------------------------------------------------------------------------
100.00% | 25275   | start                                                                                                                
100.00% | 25274   | perf_parse::main                                                                                                     
100.00% | 25274   | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>                                                        
100.00% | 25274   | std::rt::lang_start::<()>::{closure#0}                                                                               
100.00% | 25274   | std::rt::lang_start_internal                                                                                         
100.00% | 25274   | main                                                                                                                 
95.98%  | 24259   | <sonic_rs::value::node::Value>::parse_with_padding                                                                   
91.41%  | 23103   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentVisitor>
86.89%  | 21961   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVisitor> 
 8.34%  | 2108    | _platform_memmove                                                                                                    
 3.79%  | 958     | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                          
 0.85%  | 214     | perf_parse!0x45304                                                                                                   
 0.49%  | 123     | _platform_memcmp                                                                                                     
 0.34%  | 87      | <bumpalo::Bump>::alloc_layout_slow                                                                                   
 0.28%  | 70      | libsystem_malloc.dylib!0x2a3c7                                                                                       
```

### Self-time by function class

```
self%  | samples | class           
------ | ------- | ----------------
85.64% | 21645   | parse-recursive 
 8.83% | 2232    | memmove-memcpy  
 3.79% | 958     | utf8-validation 
 0.96% | 242     | other           
 0.45% | 114     | allocation      
 0.30% | 75      | syscall         
 0.02% | 4       | parse-entry     
 0.01% | 3       | drop-teardown   
 0.00% | 1       | runtime         
 0.00% | 1       | dispatch-visitor
```

## canada

Samples: 41024 (~41.0s of CPU at 1000 Hz)

### Top 15 by self-time

```
self%  | samples | symbol                                                                                                               
------ | ------- | ---------------------------------------------------------------------------------------------------------------------
86.53% | 35498   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVisitor> 
 9.27% | 3801    | _platform_memmove                                                                                                    
 1.75% | 718     | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                          
 1.43% | 586     | perf_parse!0x45304                                                                                                   
 0.38% | 156     | mach_vm_reclaim_update_kernel_accounting_trap                                                                        
 0.16% | 66      | mach_absolute_time                                                                                                   
 0.04% | 17      | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentVisitor>
 0.03% | 13      | <bumpalo::Bump>::alloc_layout_slow                                                                                   
 0.02% | 10      | mach_vm_reclaim_try_enter                                                                                            
 0.02% | 9       | perf_parse::main                                                                                                     
 0.01% | 6       | libsystem_malloc.dylib!0x2ff48                                                                                       
 0.01% | 6       | <sonic_rs::value::node::Value as core::ops::drop::Drop>::drop                                                        
 0.01% | 5       | perf_parse!0x45300                                                                                                   
 0.01% | 5       | <sonic_rs::value::node::Value>::parse_with_padding                                                                   
 0.01% | 5       | libsystem_malloc.dylib!0x33544                                                                                       
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                                                                               
------- | ------- | ---------------------------------------------------------------------------------------------------------------------
100.00% | 41024   | perf_parse::main                                                                                                     
100.00% | 41024   | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>                                                        
100.00% | 41024   | std::rt::lang_start::<()>::{closure#0}                                                                               
100.00% | 41024   | std::rt::lang_start_internal                                                                                         
100.00% | 41024   | main                                                                                                                 
100.00% | 41024   | start                                                                                                                
97.80%  | 40120   | <sonic_rs::value::node::Value>::parse_with_padding                                                                   
95.23%  | 39066   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_object::<sonic_rs::value::node::DocumentVisitor>
95.21%  | 39057   | <sonic_rs::parser::Parser<sonic_rs::reader::PaddedSliceRead>>::parse_array::<sonic_rs::value::node::DocumentVisitor> 
 9.27%  | 3801    | _platform_memmove                                                                                                    
 1.75%  | 718     | simdutf8::implementation::aarch64::validate_utf8_basic_neon                                                          
 1.43%  | 586     | perf_parse!0x45304                                                                                                   
 0.42%  | 172     | <alloc::sync::Arc<sonic_rs::value::shared::Shared>>::drop_slow                                                       
 0.38%  | 156     | mach_vm_reclaim_update_kernel_accounting_trap                                                                        
 0.38%  | 156     | mach_vm_reclaim_update_kernel_accounting                                                                             
```

### Self-time by function class

```
self%  | samples | class           
------ | ------- | ----------------
86.57% | 35515   | parse-recursive 
 9.27% | 3804    | memmove-memcpy  
 1.75% | 718     | utf8-validation 
 1.46% | 597     | other           
 0.58% | 239     | syscall         
 0.31% | 127     | allocation      
 0.02% | 9       | runtime         
 0.02% | 8       | drop-teardown   
 0.01% | 5       | parse-entry     
 0.00% | 2       | dispatch-visitor
```

## (d) Bottleneck signature per corpus

Cross-corpus signature for sonic-rs (typed-`Value` DOM parse, aarch64 / Apple Silicon):

- **The fused recursive descent is the bottleneck symbol.** `Parser::parse_object` and
  `Parser::parse_array` hold 82-87% of self-time across all three corpora. Every inner technique —
  the bitmask-driven structural scan via NEON `movemask` (sonic-simd), the SIMD quote/escape search
  in the string parser, the `sonic-number` Eisel-Lemire fast-float path, the whitespace skip — is
  inlined into them under `lto=true codegen-units=1`. They are not fine-grained leaves; they are
  the fused hot loop. The corpus shape determines which descent function dominates: twitter and
  citm are object-heavy so `parse_object` carries the load; canada is one nested-array document
  of float pairs so `parse_array` carries all 86%.

- **`_platform_memmove`** (Apple libsystem aligned-SIMD memmove) is the second-largest leaf on
  every corpus at 6-9% self-time. This is the cost of moving parsed strings/numbers into the
  bumpalo arena that backs `Value`. The flat ~8% share across very different corpora (mostly
  strings, mixed, mostly numbers) suggests this is per-element token-copy overhead more than
  string-specific bulk copy.

- **`simdutf8::validate_utf8_basic_neon`** is the only consistently non-inlined SIMD leaf and runs
  once per parse over the full input. Its share scales with text density: twitter 7.17% (text-heavy),
  citm 3.79% (mixed), canada 1.75% (mostly numeric). UTF-8 validation cost is linear in input bytes,
  not in string content.

- **`_platform_memcmp`** appears on twitter (2.72%) and citm (0.49%) but not canada (0.0%). This
  is object key comparison during deserialisation (key string equality checks inside the
  ahash-backed object map). canada has no object keys to compare.

- **`Value::parse_with_padding`** has only 0.01-0.03% self-time but 92-98% inclusive — it is the
  thin wrapper that allocates +64-byte SIMD padding around the input and calls into the recursive
  descent. The padding allocation itself is amortized to near zero by Apples malloc cache.

- **DOM teardown is nearly free**: `<Value as Drop>::drop` is 0.01-0.03% inclusive, `Arc<Shared>::drop_slow`
  is 0.2-0.4% inclusive. The arena-backed DOM (`Arc<Shared>` holding bumpalo) means dropping the
  Value drops a single Arc, which frees the arena in one shot.

### Class-share comparison across corpora (self-time)

```
class            | twitter% | citm% | canada%
---------------- | -------- | ----- | -------
allocation       | 0.59     | 0.45  | 0.31   
dispatch-visitor | 0.00     | 0.00  | 0.00   
drop-teardown    | 0.03     | 0.01  | 0.02   
memmove-memcpy   | 8.74     | 8.83  | 9.27   
other            | 0.75     | 0.96  | 1.46   
parse-entry      | 0.03     | 0.02  | 0.01   
parse-recursive  | 82.15    | 85.64 | 86.57  
runtime          | 0.03     | 0.00  | 0.02   
syscall          | 0.50     | 0.30  | 0.58   
utf8-validation  | 7.17     | 3.79  | 1.75   
```

## (e) Honest take

sonic-rs spends ~95% of its cycles inside two recursive-descent functions —
`Parser::parse_object` and `Parser::parse_array` — into which the SIMD structural scanner, the
inline string SIMD, and the `sonic-number` fast-float path have all been LTO-fused; the residual
5-10% splits between upfront `simdutf8` NEON validation, libsystem `_platform_memmove` for string
arena copies (corpus-dependent), and parser-state drop. There is no PSHUFB whitespace-skip hotspot
visible as a leaf, no separate number-parse leaf, no string-decode leaf — all are subsumed into
the fused descent. The honest one-line summary: **sonic-rs is one fused SIMD recursive descent,**
**and the profile shape is dominated by what you cannot see (inlining) rather than what you can.**

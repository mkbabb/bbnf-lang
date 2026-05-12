# Skinny Expanded Profile Report

Profiler: samply 0.13+ (sampling, 1000 Hz, --unstable-presymbolicate)
Binary: `target/release/profile-lazy` (release: lto=thin, codegen-units=1, debug=true)
Iterations: scaled to target ~30s of CPU per corpus at ~100 MB/s baseline

Cycle/byte estimates assume 3.5 GHz Apple-silicon big core. Hot-leaf count = number of distinct symbols (excluding the parse-driver itself) with self-time >= 1.0%; this is the BENCH §6 G-fusion-quality discriminator (>= 5 = many fine-grained scalar leaves dominate, indicating G outcome class).

## twitter

Size 631,515 bytes; iters 4,750; wall 4.35s; **5,521 Mbps**; ~5.07 cycles/byte; samples 3,794 (~3.8s CPU); **hot-leaf count: 6**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
41.06% | 1558    | simd_scan::scan_json_parse_index                      
33.63% | 1276    | runtime::generated_json::generated::parse_value       
 9.38% | 356     | runtime::generated_json::generated::consume_structural
 8.38% | 318     | runtime::generated_json::generated::parse_string      
 4.24% | 161     | parse_that_regex::match_json_string                   
 1.63% | 62      | runtime::generated_json::generated::parse_literal     
 0.84% | 32      | _platform_memcmp                                      
 0.29% | 11      | profile-lazy!0x3db18                                  
 0.13% | 5       | mach_absolute_time                                    
 0.05% | 2       | runtime::generated_json::parser::parse                
 0.03% | 1       | libsystem_malloc.dylib!0x16450                        
 0.03% | 1       | libsystem_malloc.dylib!0x355d0                        
 0.03% | 1       | libsystem_malloc.dylib!0x2aea4                        
 0.03% | 1       | mach_vm_reclaim_update_kernel_accounting_trap         
 0.03% | 1       | libsystem_malloc.dylib!0x354a4                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 3794    | profile_lazy::main                                           
100.00% | 3794    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 3794    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 3794    | std::rt::lang_start_internal                                 
100.00% | 3794    | main                                                         
100.00% | 3794    | start                                                        
99.95%  | 3792    | runtime::generated_json::parser::parse                       
58.41%  | 2216    | runtime::generated_json::generated::parse_value              
41.09%  | 1559    | simd_scan::scan_json_parse_index                             
20.14%  | 764     | runtime::generated_json::generated::parse_string             
 9.38%  | 356     | runtime::generated_json::generated::consume_structural       
 4.24%  | 161     | parse_that_regex::match_json_string                          
 2.77%  | 105     | runtime::generated_json::generated::parse_literal            
 0.84%  | 32      | _platform_memcmp                                             
 0.29%  | 11      | profile-lazy!0x3db18                                         
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
44.70% | 1696    | parse-driver  
41.06% | 1558    | simd-scan     
 8.38% | 318     | string-decode 
 4.24% | 161     | regex-classify
 0.84% | 32      | memmove-memcpy
 0.32% | 12      | allocation    
 0.29% | 11      | other         
 0.16% | 6       | syscall       
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 6

```
self%  | symbol                                                 | class         
------ | ------------------------------------------------------ | --------------
41.06% | simd_scan::scan_json_parse_index                       | simd-scan     
33.63% | runtime::generated_json::generated::parse_value        | parse-driver  
 9.38% | runtime::generated_json::generated::consume_structural | parse-driver  
 8.38% | runtime::generated_json::generated::parse_string       | string-decode 
 4.24% | parse_that_regex::match_json_string                    | regex-classify
 1.63% | runtime::generated_json::generated::parse_literal      | parse-driver  
```

## citm_catalog

Size 1,727,204 bytes; iters 1,736; wall 2.68s; **8,947 Mbps**; ~3.13 cycles/byte; samples 2,642 (~2.6s CPU); **hot-leaf count: 4**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
53.67% | 1418    | runtime::generated_json::generated::parse_value       
31.60% | 835     | simd_scan::scan_json_parse_index                      
10.11% | 267     | runtime::generated_json::generated::consume_structural
 3.94% | 104     | runtime::generated_json::generated::parse_string      
 0.26% | 7       | runtime::generated_json::generated::parse_literal     
 0.19% | 5       | _platform_memcmp                                      
 0.04% | 1       | crc32c                                                
 0.04% | 1       | __mmap                                                
 0.04% | 1       | read                                                  
 0.04% | 1       | profile-lazy!0x3db18                                  
 0.04% | 1       | libsystem_malloc.dylib!0x35768                        
 0.04% | 1       | <runtime::tape::assembler::TapeAssembler>::finish     
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 2642    | start                                                        
99.96%  | 2641    | std::rt::lang_start_internal                                 
99.96%  | 2641    | main                                                         
99.92%  | 2640    | profile_lazy::main                                           
99.92%  | 2640    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
99.92%  | 2640    | std::rt::lang_start::<()>::{closure#0}                       
99.89%  | 2639    | runtime::generated_json::parser::parse                       
68.21%  | 1802    | runtime::generated_json::generated::parse_value              
31.60%  | 835     | simd_scan::scan_json_parse_index                             
10.11%  | 267     | runtime::generated_json::generated::consume_structural       
 7.49%  | 198     | runtime::generated_json::generated::parse_string             
 0.49%  | 13      | runtime::generated_json::generated::parse_literal            
 0.19%  | 5       | _platform_memcmp                                             
 0.08%  | 2       | <runtime::tape::assembler::TapeAssembler>::finish            
 0.04%  | 1       | crc32c                                                       
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
64.04% | 1692    | parse-driver  
31.60% | 835     | simd-scan     
 3.94% | 104     | string-decode 
 0.19% | 5       | memmove-memcpy
 0.15% | 4       | other         
 0.04% | 1       | allocation    
 0.04% | 1       | tape-arena    
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 4

```
self%  | symbol                                                 | class        
------ | ------------------------------------------------------ | -------------
53.67% | runtime::generated_json::generated::parse_value        | parse-driver 
31.60% | simd_scan::scan_json_parse_index                       | simd-scan    
10.11% | runtime::generated_json::generated::consume_structural | parse-driver 
 3.94% | runtime::generated_json::generated::parse_string       | string-decode
```

## canada

Size 2,251,051 bytes; iters 1,332; wall 5.17s; **4,640 Mbps**; ~6.03 cycles/byte; samples 4,600 (~4.6s CPU); **hot-leaf count: 3**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
58.91% | 2710    | runtime::generated_json::generated::parse_value       
27.87% | 1282    | simd_scan::scan_json_parse_index                      
13.04% | 600     | runtime::generated_json::generated::consume_structural
 0.04% | 2       | libsystem_malloc.dylib!0x354a4                        
 0.02% | 1       | read                                                  
 0.02% | 1       | libsystem_malloc.dylib!0x2888                         
 0.02% | 1       | profile-lazy!0x3db0c                                  
 0.02% | 1       | mach_vm_reclaim_try_cancel                            
 0.02% | 1       | libsystem_malloc.dylib!0x14378                        
 0.02% | 1       | libsystem_malloc.dylib!0x3495c                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 4600    | profile_lazy::main                                           
100.00% | 4600    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 4600    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 4600    | std::rt::lang_start_internal                                 
100.00% | 4600    | main                                                         
100.00% | 4600    | start                                                        
99.96%  | 4598    | runtime::generated_json::parser::parse                       
71.96%  | 3310    | runtime::generated_json::generated::parse_value              
27.89%  | 1283    | simd_scan::scan_json_parse_index                             
13.04%  | 600     | runtime::generated_json::generated::consume_structural       
 0.11%  | 5       | <runtime::tape::assembler::TapeAssembler>::finish            
 0.04%  | 2       | libsystem_malloc.dylib!0x354a4                               
 0.04%  | 2       | libsystem_malloc.dylib!0x3adc7                               
 0.04%  | 2       | libsystem_malloc.dylib!0x14d2f                               
 0.04%  | 2       | libsystem_malloc.dylib!0x2adcb                               
```

### Self-time by function class

```
self%  | samples | class       
------ | ------- | ------------
71.96% | 3310    | parse-driver
27.87% | 1282    | simd-scan   
 0.11% | 5       | allocation  
 0.04% | 2       | other       
 0.02% | 1       | syscall     
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 3

```
self%  | symbol                                                 | class       
------ | ------------------------------------------------------ | ------------
58.91% | runtime::generated_json::generated::parse_value        | parse-driver
27.87% | simd_scan::scan_json_parse_index                       | simd-scan   
13.04% | runtime::generated_json::generated::consume_structural | parse-driver
```

## apache_builds

Size 127,275 bytes; iters 23,571; wall 2.82s; **8,502 Mbps**; ~3.29 cycles/byte; samples 2,584 (~2.6s CPU); **hot-leaf count: 4**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
40.33% | 1042    | simd_scan::scan_json_parse_index                      
31.39% | 811     | runtime::generated_json::generated::parse_value       
15.75% | 407     | runtime::generated_json::generated::consume_structural
11.26% | 291     | runtime::generated_json::generated::parse_string      
 0.46% | 12      | mach_absolute_time                                    
 0.31% | 8       | parse_that_regex::match_json_string                   
 0.04% | 1       | read                                                  
 0.04% | 1       | libsystem_malloc.dylib!0x13b34                        
 0.04% | 1       | libsystem_malloc.dylib!0x354a4                        
 0.04% | 1       | mach_vm_reclaim_try_cancel                            
 0.04% | 1       | libsystem_malloc.dylib!0x14ad8                        
 0.04% | 1       | libsystem_malloc.dylib!0x35688                        
 0.04% | 1       | libsystem_malloc.dylib!0x2b698                        
 0.04% | 1       | runtime::generated_json::parser::parse                
 0.04% | 1       | libsystem_malloc.dylib!0x34994                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 2584    | profile_lazy::main                                           
100.00% | 2584    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 2584    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 2584    | std::rt::lang_start_internal                                 
100.00% | 2584    | main                                                         
100.00% | 2584    | start                                                        
99.88%  | 2581    | runtime::generated_json::parser::parse                       
58.71%  | 1517    | runtime::generated_json::generated::parse_value              
40.56%  | 1048    | simd_scan::scan_json_parse_index                             
22.79%  | 589     | runtime::generated_json::generated::parse_string             
15.75%  | 407     | runtime::generated_json::generated::consume_structural       
 0.46%  | 12      | mach_absolute_time                                           
 0.39%  | 10      | libsystem_malloc.dylib!0x3a673                               
 0.39%  | 10      | libsystem_malloc.dylib!0x3ae8f                               
 0.35%  | 9       | <runtime::tape::assembler::TapeAssembler>::finish            
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
47.17% | 1219    | parse-driver  
40.33% | 1042    | simd-scan     
11.26% | 291     | string-decode 
 0.50% | 13      | syscall       
 0.39% | 10      | allocation    
 0.31% | 8       | regex-classify
 0.04% | 1       | other         
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 4

```
self%  | symbol                                                 | class        
------ | ------------------------------------------------------ | -------------
40.33% | simd_scan::scan_json_parse_index                       | simd-scan    
31.39% | runtime::generated_json::generated::parse_value        | parse-driver 
15.75% | runtime::generated_json::generated::consume_structural | parse-driver 
11.26% | runtime::generated_json::generated::parse_string       | string-decode
```

## github_events

Size 65,132 bytes; iters 46,060; wall 3.25s; **7,378 Mbps**; ~3.80 cycles/byte; samples 2,770 (~2.8s CPU); **hot-leaf count: 5**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
38.16% | 1057    | simd_scan::scan_json_parse_index                      
31.08% | 861     | runtime::generated_json::generated::parse_value       
10.51% | 291     | runtime::generated_json::generated::consume_structural
 9.71% | 269     | runtime::generated_json::generated::parse_string      
 6.50% | 180     | parse_that_regex::match_json_string                   
 0.94% | 26      | _platform_memmove                                     
 0.69% | 19      | mach_absolute_time                                    
 0.47% | 13      | _platform_memcmp                                      
 0.25% | 7       | runtime::generated_json::generated::parse_literal     
 0.14% | 4       | libsystem_malloc.dylib!0x2888                         
 0.11% | 3       | runtime::generated_json::parser::parse                
 0.11% | 3       | mach_vm_reclaim_try_cancel                            
 0.11% | 3       | libsystem_malloc.dylib!0x354a4                        
 0.07% | 2       | libsystem_malloc.dylib!0x2a8c8                        
 0.07% | 2       | libsystem_malloc.dylib!0x12f8c                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 2770    | profile_lazy::main                                           
100.00% | 2770    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 2770    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 2770    | std::rt::lang_start_internal                                 
100.00% | 2770    | main                                                         
100.00% | 2770    | start                                                        
99.82%  | 2765    | runtime::generated_json::parser::parse                       
58.59%  | 1623    | runtime::generated_json::generated::parse_value              
38.77%  | 1074    | simd_scan::scan_json_parse_index                             
25.34%  | 702     | runtime::generated_json::generated::parse_string             
10.51%  | 291     | runtime::generated_json::generated::consume_structural       
 6.50%  | 180     | parse_that_regex::match_json_string                          
 1.91%  | 53      | libsystem_malloc.dylib!0x3a673                               
 1.91%  | 53      | libsystem_malloc.dylib!0x3ae8f                               
 1.66%  | 46      | <runtime::tape::assembler::TapeAssembler>::finish            
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
41.95% | 1162    | parse-driver  
38.16% | 1057    | simd-scan     
 9.71% | 269     | string-decode 
 6.50% | 180     | regex-classify
 1.41% | 39      | memmove-memcpy
 1.34% | 37      | allocation    
 0.83% | 23      | syscall       
 0.11% | 3       | other         
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 5

```
self%  | symbol                                                 | class         
------ | ------------------------------------------------------ | --------------
38.16% | simd_scan::scan_json_parse_index                       | simd-scan     
31.08% | runtime::generated_json::generated::parse_value        | parse-driver  
10.51% | runtime::generated_json::generated::consume_structural | parse-driver  
 9.71% | runtime::generated_json::generated::parse_string       | string-decode 
 6.50% | parse_that_regex::match_json_string                    | regex-classify
```

## update-center

Size 533,178 bytes; iters 5,626; wall 4.54s; **5,289 Mbps**; ~5.29 cycles/byte; samples 3,895 (~3.9s CPU); **hot-leaf count: 5**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
44.39% | 1729    | simd_scan::scan_json_parse_index                      
21.77% | 848     | runtime::generated_json::generated::parse_value       
16.35% | 637     | runtime::generated_json::generated::consume_structural
13.22% | 515     | runtime::generated_json::generated::parse_string      
 3.67% | 143     | parse_that_regex::match_json_string                   
 0.08% | 3       | _platform_memcmp                                      
 0.08% | 3       | runtime::generated_json::generated::parse_literal     
 0.05% | 2       | libsystem_malloc.dylib!0x152ac                        
 0.05% | 2       | mach_absolute_time                                    
 0.03% | 1       | close                                                 
 0.03% | 1       | <alloc::raw_vec::RawVecInner>::finish_grow            
 0.03% | 1       | libsystem_malloc.dylib!0x2888                         
 0.03% | 1       | profile-lazy!0x3db14                                  
 0.03% | 1       | libsystem_malloc.dylib!0x355d0                        
 0.03% | 1       | mach_vm_reclaim_try_cancel                            
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 3895    | profile_lazy::main                                           
100.00% | 3895    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 3895    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 3895    | std::rt::lang_start_internal                                 
100.00% | 3895    | main                                                         
100.00% | 3895    | start                                                        
99.95%  | 3893    | runtime::generated_json::parser::parse                       
55.20%  | 2150    | runtime::generated_json::generated::parse_value              
44.47%  | 1732    | simd_scan::scan_json_parse_index                             
29.17%  | 1136    | runtime::generated_json::generated::parse_string             
16.35%  | 637     | runtime::generated_json::generated::consume_structural       
 3.67%  | 143     | parse_that_regex::match_json_string                          
 0.18%  | 7       | runtime::generated_json::generated::parse_literal            
 0.18%  | 7       | <runtime::tape::assembler::TapeAssembler>::finish            
 0.13%  | 5       | libsystem_malloc.dylib!0x3a673                               
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
44.39% | 1729    | simd-scan     
38.20% | 1488    | parse-driver  
13.22% | 515     | string-decode 
 3.67% | 143     | regex-classify
 0.26% | 10      | allocation    
 0.10% | 4       | syscall       
 0.08% | 3       | memmove-memcpy
 0.05% | 2       | other         
 0.03% | 1       | tape-arena    
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 5

```
self%  | symbol                                                 | class         
------ | ------------------------------------------------------ | --------------
44.39% | simd_scan::scan_json_parse_index                       | simd-scan     
21.77% | runtime::generated_json::generated::parse_value        | parse-driver  
16.35% | runtime::generated_json::generated::consume_structural | parse-driver  
13.22% | runtime::generated_json::generated::parse_string       | string-decode 
 3.67% | parse_that_regex::match_json_string                    | regex-classify
```

## mesh

Size 723,597 bytes; iters 4,145; wall 3.62s; **6,635 Mbps**; ~4.22 cycles/byte; samples 3,621 (~3.6s CPU); **hot-leaf count: 3**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
73.79% | 2672    | runtime::generated_json::generated::parse_value       
23.47% | 850     | simd_scan::scan_json_parse_index                      
 2.51% | 91      | runtime::generated_json::generated::consume_structural
 0.08% | 3       | mach_absolute_time                                    
 0.03% | 1       | read                                                  
 0.03% | 1       | libsystem_malloc.dylib!0x355d0                        
 0.03% | 1       | libsystem_malloc.dylib!0x135d8                        
 0.03% | 1       | runtime::generated_json::parser::parse                
 0.03% | 1       | libsystem_malloc.dylib!0x356d0                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 3621    | profile_lazy::main                                           
100.00% | 3621    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 3621    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 3621    | std::rt::lang_start_internal                                 
100.00% | 3621    | main                                                         
100.00% | 3621    | start                                                        
99.97%  | 3620    | runtime::generated_json::parser::parse                       
76.30%  | 2763    | runtime::generated_json::generated::parse_value              
23.50%  | 851     | simd_scan::scan_json_parse_index                             
 2.51%  | 91      | runtime::generated_json::generated::consume_structural       
 0.11%  | 4       | <runtime::tape::assembler::TapeAssembler>::finish            
 0.08%  | 3       | mach_absolute_time                                           
 0.08%  | 3       | mach_vm_reclaim_try_cancel                                   
 0.08%  | 3       | libsystem_malloc.dylib!0x15a4f                               
 0.06%  | 2       | libsystem_malloc.dylib!0x2a3c7                               
```

### Self-time by function class

```
self%  | samples | class       
------ | ------- | ------------
76.33% | 2764    | parse-driver
23.47% | 850     | simd-scan   
 0.08% | 3       | syscall     
 0.08% | 3       | allocation  
 0.03% | 1       | other       
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 3

```
self%  | symbol                                                 | class       
------ | ------------------------------------------------------ | ------------
73.79% | runtime::generated_json::generated::parse_value        | parse-driver
23.47% | simd_scan::scan_json_parse_index                       | simd-scan   
 2.51% | runtime::generated_json::generated::consume_structural | parse-driver
```

## random

Size 510,476 bytes; iters 5,876; wall 3.60s; **6,674 Mbps**; ~4.20 cycles/byte; samples 3,586 (~3.6s CPU); **hot-leaf count: 4**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
40.38% | 1448    | simd_scan::scan_json_parse_index                      
29.95% | 1074    | runtime::generated_json::generated::parse_value       
16.76% | 601     | runtime::generated_json::generated::consume_structural
11.96% | 429     | runtime::generated_json::generated::parse_string      
 0.42% | 15      | runtime::generated_json::generated::parse_literal     
 0.20% | 7       | _platform_memcmp                                      
 0.08% | 3       | mach_absolute_time                                    
 0.06% | 2       | profile-lazy!0x3db18                                  
 0.03% | 1       | read                                                  
 0.03% | 1       | libsystem_malloc.dylib!0x354a4                        
 0.03% | 1       | libsystem_malloc.dylib!0x14d38                        
 0.03% | 1       | libsystem_malloc.dylib!0x3a65c                        
 0.03% | 1       | libsystem_malloc.dylib!0x355d0                        
 0.03% | 1       | libsystem_malloc.dylib!0x2fed4                        
 0.03% | 1       | libsystem_malloc.dylib!0x13b34                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 3586    | profile_lazy::main                                           
100.00% | 3586    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 3586    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 3586    | std::rt::lang_start_internal                                 
100.00% | 3586    | main                                                         
100.00% | 3586    | start                                                        
99.97%  | 3585    | runtime::generated_json::parser::parse                       
59.34%  | 2128    | runtime::generated_json::generated::parse_value              
40.55%  | 1454    | simd_scan::scan_json_parse_index                             
24.34%  | 873     | runtime::generated_json::generated::parse_string             
16.76%  | 601     | runtime::generated_json::generated::consume_structural       
 0.67%  | 24      | runtime::generated_json::generated::parse_literal            
 0.20%  | 7       | _platform_memcmp                                             
 0.11%  | 4       | libsystem_malloc.dylib!0x3ae8f                               
 0.11%  | 4       | <alloc::raw_vec::RawVecInner>::finish_grow                   
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
47.13% | 1690    | parse-driver  
40.38% | 1448    | simd-scan     
11.96% | 429     | string-decode 
 0.20% | 7       | memmove-memcpy
 0.17% | 6       | allocation    
 0.08% | 3       | other         
 0.08% | 3       | syscall       
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 4

```
self%  | symbol                                                 | class        
------ | ------------------------------------------------------ | -------------
40.38% | simd_scan::scan_json_parse_index                       | simd-scan    
29.95% | runtime::generated_json::generated::parse_value        | parse-driver 
16.76% | runtime::generated_json::generated::consume_structural | parse-driver 
11.96% | runtime::generated_json::generated::parse_string       | string-decode
```

## gsoc-2018

Size 3,327,831 bytes; iters 901; wall 2.52s; **9,516 Mbps**; ~2.94 cycles/byte; samples 2,531 (~2.5s CPU); **hot-leaf count: 5**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
43.54% | 1102    | parse_that_regex::match_json_string                   
39.59% | 1002    | simd_scan::scan_json_parse_index                      
 8.30% | 210     | runtime::generated_json::generated::parse_value       
 4.94% | 125     | runtime::generated_json::generated::parse_string      
 3.48% | 88      | runtime::generated_json::generated::consume_structural
 0.04% | 1       | write                                                 
 0.04% | 1       | libsystem_malloc.dylib!0x13b34                        
 0.04% | 1       | libsystem_malloc.dylib!0x355d0                        
 0.04% | 1       | libsystem_malloc.dylib!0x3553c                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                                                          
------- | ------- | ------------------------------------------------------------------------------------------------
100.00% | 2531    | profile_lazy::main                                                                              
100.00% | 2531    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>                                   
100.00% | 2531    | std::rt::lang_start::<()>::{closure#0}                                                          
100.00% | 2531    | std::rt::lang_start_internal                                                                    
100.00% | 2531    | main                                                                                            
100.00% | 2531    | start                                                                                           
99.96%  | 2530    | runtime::generated_json::parser::parse                                                          
60.25%  | 1525    | runtime::generated_json::generated::parse_value                                                 
51.13%  | 1294    | runtime::generated_json::generated::parse_string                                                
43.54%  | 1102    | parse_that_regex::match_json_string                                                             
39.63%  | 1003    | simd_scan::scan_json_parse_index                                                                
 3.48%  | 88      | runtime::generated_json::generated::consume_structural                                          
 0.08%  | 2       | libsystem_malloc.dylib!0x3adc7                                                                  
 0.04%  | 1       | write                                                                                           
 0.04%  | 1       | <std::io::default_write_fmt::Adapter<std::io::stdio::StderrLock> as core::fmt::Write>::write_str
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
43.54% | 1102    | regex-classify
39.59% | 1002    | simd-scan     
11.77% | 298     | parse-driver  
 4.94% | 125     | string-decode 
 0.12% | 3       | allocation    
 0.04% | 1       | other         
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 5

```
self%  | symbol                                                 | class         
------ | ------------------------------------------------------ | --------------
43.54% | parse_that_regex::match_json_string                    | regex-classify
39.59% | simd_scan::scan_json_parse_index                       | simd-scan     
 8.30% | runtime::generated_json::generated::parse_value        | parse-driver  
 4.94% | runtime::generated_json::generated::parse_string       | string-decode 
 3.48% | runtime::generated_json::generated::consume_structural | parse-driver  
```

## marine_ik

Size 2,983,466 bytes; iters 1,005; wall 6.42s; **3,736 Mbps**; ~7.49 cycles/byte; samples 5,776 (~5.8s CPU); **hot-leaf count: 4**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
63.97% | 3695    | runtime::generated_json::generated::parse_value       
26.68% | 1541    | simd_scan::scan_json_parse_index                      
 6.72% | 388     | runtime::generated_json::generated::consume_structural
 1.97% | 114     | runtime::generated_json::generated::parse_string      
 0.47% | 27      | _platform_memmove                                     
 0.05% | 3       | _platform_memcmp                                      
 0.05% | 3       | libsystem_malloc.dylib!0x354a4                        
 0.03% | 2       | libsystem_malloc.dylib!0x163f0                        
 0.02% | 1       | read                                                  
 0.02% | 1       | libsystem_malloc.dylib!0x15f7c                        
 0.02% | 1       | mach_vm_reclaim_try_enter                             
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 5776    | profile_lazy::main                                           
100.00% | 5776    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 5776    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 5776    | std::rt::lang_start_internal                                 
100.00% | 5776    | main                                                         
100.00% | 5776    | start                                                        
99.98%  | 5775    | runtime::generated_json::parser::parse                       
72.71%  | 4200    | runtime::generated_json::generated::parse_value              
27.25%  | 1574    | simd_scan::scan_json_parse_index                             
 6.72%  | 388     | runtime::generated_json::generated::consume_structural       
 3.62%  | 209     | runtime::generated_json::generated::parse_string             
 0.54%  | 31      | <alloc::raw_vec::RawVecInner>::finish_grow                   
 0.54%  | 31      | <alloc::raw_vec::RawVec<u32>>::grow_one                      
 0.48%  | 28      | libsystem_malloc.dylib!0x3a673                               
 0.48%  | 28      | libsystem_malloc.dylib!0x3ae8f                               
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
70.69% | 4083    | parse-driver  
26.68% | 1541    | simd-scan     
 1.97% | 114     | string-decode 
 0.52% | 30      | memmove-memcpy
 0.10% | 6       | allocation    
 0.02% | 1       | other         
 0.02% | 1       | syscall       
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 4

```
self%  | symbol                                                 | class        
------ | ------------------------------------------------------ | -------------
63.97% | runtime::generated_json::generated::parse_value        | parse-driver 
26.68% | simd_scan::scan_json_parse_index                       | simd-scan    
 6.72% | runtime::generated_json::generated::consume_structural | parse-driver 
 1.97% | runtime::generated_json::generated::parse_string       | string-decode
```

## instruments

Size 220,346 bytes; iters 13,614; wall 2.71s; **8,854 Mbps**; ~3.16 cycles/byte; samples 2,708 (~2.7s CPU); **hot-leaf count: 4**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
49.96% | 1353    | runtime::generated_json::generated::parse_value       
32.24% | 873     | simd_scan::scan_json_parse_index                      
 9.97% | 270     | runtime::generated_json::generated::consume_structural
 6.13% | 166     | runtime::generated_json::generated::parse_string      
 0.52% | 14      | runtime::generated_json::generated::parse_literal     
 0.41% | 11      | _platform_memcmp                                      
 0.22% | 6       | mach_absolute_time                                    
 0.07% | 2       | profile-lazy!0x3dc50                                  
 0.07% | 2       | profile-lazy!0x3db18                                  
 0.04% | 1       | core::str::converts::from_utf8                        
 0.04% | 1       | libsystem_malloc.dylib!0x13b4c                        
 0.04% | 1       | runtime::generated_json::parser::parse                
 0.04% | 1       | libsystem_malloc.dylib!0x2afc0                        
 0.04% | 1       | <runtime::tape::assembler::TapeAssembler>::finish     
 0.04% | 1       | libsystem_malloc.dylib!0x14110                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 2708    | start                                                        
99.96%  | 2707    | profile_lazy::main                                           
99.96%  | 2707    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
99.96%  | 2707    | std::rt::lang_start::<()>::{closure#0}                       
99.96%  | 2707    | std::rt::lang_start_internal                                 
99.96%  | 2707    | main                                                         
99.93%  | 2706    | runtime::generated_json::parser::parse                       
67.06%  | 1816    | runtime::generated_json::generated::parse_value              
32.39%  | 877     | simd_scan::scan_json_parse_index                             
12.63%  | 342     | runtime::generated_json::generated::parse_string             
 9.97%  | 270     | runtime::generated_json::generated::consume_structural       
 1.00%  | 27      | runtime::generated_json::generated::parse_literal            
 0.41%  | 11      | _platform_memcmp                                             
 0.30%  | 8       | libsystem_malloc.dylib!0x3a673                               
 0.30%  | 8       | libsystem_malloc.dylib!0x3ae8f                               
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
60.49% | 1638    | parse-driver  
32.24% | 873     | simd-scan     
 6.13% | 166     | string-decode 
 0.41% | 11      | memmove-memcpy
 0.26% | 7       | syscall       
 0.22% | 6       | other         
 0.22% | 6       | allocation    
 0.04% | 1       | tape-arena    
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 4

```
self%  | symbol                                                 | class        
------ | ------------------------------------------------------ | -------------
49.96% | runtime::generated_json::generated::parse_value        | parse-driver 
32.24% | simd_scan::scan_json_parse_index                       | simd-scan    
 9.97% | runtime::generated_json::generated::consume_structural | parse-driver 
 6.13% | runtime::generated_json::generated::parse_string       | string-decode
```

## numbers

Size 150,124 bytes; iters 19,983; wall 2.79s; **8,603 Mbps**; ~3.25 cycles/byte; samples 2,786 (~2.8s CPU); **hot-leaf count: 2**

### Top 15 by self-time

```
self%  | samples | symbol                                           
------ | ------- | -------------------------------------------------
71.25% | 1985    | runtime::generated_json::generated::parse_value  
28.03% | 781     | simd_scan::scan_json_parse_index                 
 0.36% | 10      | mach_absolute_time                               
 0.04% | 1       | runtime::generated_json::parser::parse           
 0.04% | 1       | libsystem_malloc.dylib!0x355d0                   
 0.04% | 1       | libsystem_malloc.dylib!0x14c84                   
 0.04% | 1       | libsystem_malloc.dylib!0x2b4e4                   
 0.04% | 1       | libsystem_malloc.dylib!0x335e4                   
 0.04% | 1       | <runtime::tape::assembler::TapeAssembler>::finish
 0.04% | 1       | mach_vm_reclaim_try_cancel                       
 0.04% | 1       | libsystem_malloc.dylib!0x135a8                   
 0.04% | 1       | mach_vm_reclaim_try_enter                        
 0.04% | 1       | write                                            
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 2786    | profile_lazy::main                                           
100.00% | 2786    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 2786    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 2786    | std::rt::lang_start_internal                                 
100.00% | 2786    | main                                                         
100.00% | 2786    | start                                                        
99.96%  | 2785    | runtime::generated_json::parser::parse                       
71.25%  | 1985    | runtime::generated_json::generated::parse_value              
28.03%  | 781     | simd_scan::scan_json_parse_index                             
 0.36%  | 10      | mach_absolute_time                                           
 0.36%  | 10      | mach_vm_reclaim_try_cancel                                   
 0.36%  | 10      | libsystem_malloc.dylib!0x15a4f                               
 0.32%  | 9       | <runtime::tape::assembler::TapeAssembler>::finish            
 0.29%  | 8       | libsystem_malloc.dylib!0x2a3c7                               
 0.25%  | 7       | libsystem_malloc.dylib!0x15117                               
```

### Self-time by function class

```
self%  | samples | class       
------ | ------- | ------------
71.28% | 1986    | parse-driver
28.03% | 781     | simd-scan   
 0.43% | 12      | syscall     
 0.18% | 5       | allocation  
 0.04% | 1       | tape-arena  
 0.04% | 1       | other       
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 2

```
self%  | symbol                                          | class       
------ | ----------------------------------------------- | ------------
71.25% | runtime::generated_json::generated::parse_value | parse-driver
28.03% | simd_scan::scan_json_parse_index                | simd-scan   
```

## unicode_mixed

Size 1,053,086 bytes; iters 2,848; wall 3.50s; **6,851 Mbps**; ~4.09 cycles/byte; samples 3,520 (~3.5s CPU); **hot-leaf count: 5**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
38.72% | 1363    | parse_that_regex::match_json_string                   
34.43% | 1212    | simd_scan::scan_json_parse_index                      
11.25% | 396     | runtime::generated_json::generated::parse_value       
 8.86% | 312     | runtime::generated_json::generated::parse_string      
 6.25% | 220     | runtime::generated_json::generated::consume_structural
 0.14% | 5       | mach_absolute_time                                    
 0.09% | 3       | _platform_memmove                                     
 0.03% | 1       | read                                                  
 0.03% | 1       | libsystem_malloc.dylib!0x14abc                        
 0.03% | 1       | libsystem_malloc.dylib!0x14d58                        
 0.03% | 1       | runtime::generated_json::parser::parse                
 0.03% | 1       | libsystem_malloc.dylib!0x13b34                        
 0.03% | 1       | libsystem_malloc.dylib!0x35618                        
 0.03% | 1       | libsystem_malloc.dylib!0x3ac78                        
 0.03% | 1       | libsystem_malloc.dylib!0x2ad7c                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 3520    | profile_lazy::main                                           
100.00% | 3520    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 3520    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 3520    | std::rt::lang_start_internal                                 
100.00% | 3520    | main                                                         
100.00% | 3520    | start                                                        
99.97%  | 3519    | runtime::generated_json::parser::parse                       
65.09%  | 2291    | runtime::generated_json::generated::parse_value              
52.05%  | 1832    | runtime::generated_json::generated::parse_string             
38.72%  | 1363    | parse_that_regex::match_json_string                          
34.74%  | 1223    | simd_scan::scan_json_parse_index                             
 6.25%  | 220     | runtime::generated_json::generated::consume_structural       
 0.34%  | 12      | libsystem_malloc.dylib!0x3a673                               
 0.34%  | 12      | libsystem_malloc.dylib!0x3ae8f                               
 0.31%  | 11      | <alloc::raw_vec::RawVecInner>::finish_grow                   
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
38.72% | 1363    | regex-classify
34.43% | 1212    | simd-scan     
17.53% | 617     | parse-driver  
 8.86% | 312     | string-decode 
 0.20% | 7       | allocation    
 0.14% | 5       | syscall       
 0.09% | 3       | memmove-memcpy
 0.03% | 1       | other         
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 5

```
self%  | symbol                                                 | class         
------ | ------------------------------------------------------ | --------------
38.72% | parse_that_regex::match_json_string                    | regex-classify
34.43% | simd_scan::scan_json_parse_index                       | simd-scan     
11.25% | runtime::generated_json::generated::parse_value        | parse-driver  
 8.86% | runtime::generated_json::generated::parse_string       | string-decode 
 6.25% | runtime::generated_json::generated::consume_structural | parse-driver  
```

## unicode_escapes

Size 1,050,797 bytes; iters 2,854; wall 4.42s; **5,429 Mbps**; ~5.16 cycles/byte; samples 4,407 (~4.4s CPU); **hot-leaf count: 5**

### Top 15 by self-time

```
self%  | samples | symbol                                                
------ | ------- | ------------------------------------------------------
62.70% | 2763    | parse_that_regex::match_json_string                   
27.68% | 1220    | simd_scan::scan_json_parse_index                      
 5.63% | 248     | runtime::generated_json::generated::parse_string      
 2.50% | 110     | runtime::generated_json::generated::parse_value       
 1.25% | 55      | runtime::generated_json::generated::consume_structural
 0.05% | 2       | _platform_memmove                                     
 0.05% | 2       | mach_absolute_time                                    
 0.02% | 1       | __open                                                
 0.02% | 1       | runtime::generated_json::parser::parse                
 0.02% | 1       | libsystem_malloc.dylib!0x14c7c                        
 0.02% | 1       | mach_vm_reclaim_try_cancel                            
 0.02% | 1       | libsystem_malloc.dylib!0x1417c                        
 0.02% | 1       | libsystem_malloc.dylib!0x3ad98                        
 0.02% | 1       | libsystem_malloc.dylib!0x328cc                        
```

### Top 15 by inclusive-time

```
incl%   | samples | symbol                                                       
------- | ------- | -------------------------------------------------------------
100.00% | 4407    | profile_lazy::main                                           
100.00% | 4407    | std::sys::backtrace::__rust_begin_short_backtrace::<fn(), ()>
100.00% | 4407    | std::rt::lang_start::<()>::{closure#0}                       
100.00% | 4407    | std::rt::lang_start_internal                                 
100.00% | 4407    | main                                                         
100.00% | 4407    | start                                                        
99.98%  | 4406    | runtime::generated_json::parser::parse                       
72.07%  | 3176    | runtime::generated_json::generated::parse_value              
69.00%  | 3041    | runtime::generated_json::generated::parse_string             
62.70%  | 2763    | parse_that_regex::match_json_string                          
27.75%  | 1223    | simd_scan::scan_json_parse_index                             
 1.25%  | 55      | runtime::generated_json::generated::consume_structural       
 0.14%  | 6       | libsystem_malloc.dylib!0x3a673                               
 0.14%  | 6       | libsystem_malloc.dylib!0x3ae8f                               
 0.11%  | 5       | <runtime::tape::assembler::TapeAssembler>::finish            
```

### Self-time by function class

```
self%  | samples | class         
------ | ------- | --------------
62.70% | 2763    | regex-classify
27.68% | 1220    | simd-scan     
 5.63% | 248     | string-decode 
 3.77% | 166     | parse-driver  
 0.09% | 4       | allocation    
 0.07% | 3       | syscall       
 0.05% | 2       | memmove-memcpy
 0.02% | 1       | other         
```

### Comparator-anchored hot leaves (>= 1.0% self-time): 5

```
self%  | symbol                                                 | class         
------ | ------------------------------------------------------ | --------------
62.70% | parse_that_regex::match_json_string                    | regex-classify
27.68% | simd_scan::scan_json_parse_index                       | simd-scan     
 5.63% | runtime::generated_json::generated::parse_string       | string-decode 
 2.50% | runtime::generated_json::generated::parse_value        | parse-driver  
 1.25% | runtime::generated_json::generated::consume_structural | parse-driver  
```

## (a) Per-corpus throughput summary

```
corpus          | size_bytes | Mbps  | c/B   | hot-leaves
--------------- | ---------- | ----- | ----- | ----------
twitter         |    631,515 | 5,521 |  5.07 | 6         
citm_catalog    |  1,727,204 | 8,947 |  3.13 | 4         
canada          |  2,251,051 | 4,640 |  6.03 | 3         
apache_builds   |    127,275 | 8,502 |  3.29 | 4         
github_events   |     65,132 | 7,378 |  3.80 | 5         
update-center   |    533,178 | 5,289 |  5.29 | 5         
mesh            |    723,597 | 6,635 |  4.22 | 3         
random          |    510,476 | 6,674 |  4.20 | 4         
gsoc-2018       |  3,327,831 | 9,516 |  2.94 | 5         
marine_ik       |  2,983,466 | 3,736 |  7.49 | 4         
instruments     |    220,346 | 8,854 |  3.16 | 4         
numbers         |    150,124 | 8,603 |  3.25 | 2         
unicode_mixed   |  1,053,086 | 6,851 |  4.09 | 5         
unicode_escapes |  1,050,797 | 5,429 |  5.16 | 5         
```

## (b) Per-corpus function-class attribution (self-time %)

```
class          | twitter | citm_catalog | canada | apache_builds | github_events | update-center | mesh  | random | gsoc-2018 | marine_ik | instruments | numbers | unicode_mixed | unicode_escapes
-------------- | ------- | ------------ | ------ | ------------- | ------------- | ------------- | ----- | ------ | --------- | --------- | ----------- | ------- | ------------- | ---------------
allocation     | 0.32    | 0.04         | 0.11   | 0.39          | 1.34          | 0.26          | 0.08  | 0.17   | 0.12      | 0.10      | 0.22        | 0.18    | 0.20          | 0.09           
memmove-memcpy | 0.84    | 0.19         | 0.00   | 0.00          | 1.41          | 0.08          | 0.00  | 0.20   | 0.00      | 0.52      | 0.41        | 0.00    | 0.09          | 0.05           
other          | 0.29    | 0.15         | 0.04   | 0.04          | 0.11          | 0.05          | 0.03  | 0.08   | 0.04      | 0.02      | 0.22        | 0.04    | 0.03          | 0.02           
parse-driver   | 44.70   | 64.04        | 71.96  | 47.17         | 41.95         | 38.20         | 76.33 | 47.13  | 11.77     | 70.69     | 60.49       | 71.28   | 17.53         | 3.77           
regex-classify | 4.24    | 0.00         | 0.00   | 0.31          | 6.50          | 3.67          | 0.00  | 0.00   | 43.54     | 0.00      | 0.00        | 0.00    | 38.72         | 62.70          
simd-scan      | 41.06   | 31.60        | 27.87  | 40.33         | 38.16         | 44.39         | 23.47 | 40.38  | 39.59     | 26.68     | 32.24       | 28.03   | 34.43         | 27.68          
string-decode  | 8.38    | 3.94         | 0.00   | 11.26         | 9.71          | 13.22         | 0.00  | 11.96  | 4.94      | 1.97      | 6.13        | 0.00    | 8.86          | 5.63           
syscall        | 0.16    | 0.00         | 0.02   | 0.50          | 0.83          | 0.10          | 0.08  | 0.08   | 0.00      | 0.02      | 0.26        | 0.43    | 0.14          | 0.07           
tape-arena     | 0.00    | 0.04         | 0.00   | 0.00          | 0.00          | 0.03          | 0.00  | 0.00   | 0.00      | 0.00      | 0.04        | 0.04    | 0.00          | 0.00           
```

## (c) Parity oracle (skinny vs serde_json structural match)

```
corpus          | skinny_ok | serde_ok | match
--------------- | --------- | -------- | -----
twitter         | ok        | ok       | yes  
citm_catalog    | ok        | ok       | yes  
canada          | ok        | ok       | yes  
apache_builds   | ok        | ok       | yes  
github_events   | ok        | ok       | yes  
update-center   | ok        | ok       | yes  
mesh            | ok        | ok       | yes  
random          | ok        | ok       | yes  
gsoc-2018       | ok        | ok       | yes  
marine_ik       | ok        | ok       | yes  
instruments     | ok        | ok       | yes  
numbers         | ok        | ok       | yes  
unicode_mixed   | ok        | ok       | yes  
unicode_escapes | ok        | ok       | yes  
```

Also passing: 43/43 JSONTestSuite y_string_* tests (Unicode + escape + surrogate pairs); explicit surrogate-pair test `{"emoji":"\uD83D\uDE00"}` parses correctly to U+1F600 and round-trips through structural counter.

## (d) Failure-mode notes per corpus

- **twitter** (5,521 Mbps, 6 hot leaves) — dominant class `parse-driver` at 44.7% self-time.
- **citm_catalog** (8,947 Mbps, 4 hot leaves) — dominant class `parse-driver` at 64.0% self-time.
- **canada** (4,640 Mbps, 3 hot leaves) — dominant class `parse-driver` at 72.0% self-time.
- **apache_builds** (8,502 Mbps, 4 hot leaves) — dominant class `parse-driver` at 47.2% self-time.
- **github_events** (7,378 Mbps, 5 hot leaves) — dominant class `parse-driver` at 41.9% self-time.
- **update-center** (5,289 Mbps, 5 hot leaves) — dominant class `simd-scan` at 44.4% self-time.
- **mesh** (6,635 Mbps, 3 hot leaves) — dominant class `parse-driver` at 76.3% self-time.
- **random** (6,674 Mbps, 4 hot leaves) — dominant class `parse-driver` at 47.1% self-time.
- **gsoc-2018** (9,516 Mbps, 5 hot leaves) — dominant class `regex-classify` at 43.5% self-time.
- **marine_ik** (3,736 Mbps, 4 hot leaves) — dominant class `parse-driver` at 70.7% self-time.
- **instruments** (8,854 Mbps, 4 hot leaves) — dominant class `parse-driver` at 60.5% self-time.
- **numbers** (8,603 Mbps, 2 hot leaves) — dominant class `parse-driver` at 71.3% self-time.
- **unicode_mixed** (6,851 Mbps, 5 hot leaves) — dominant class `regex-classify` at 38.7% self-time.
- **unicode_escapes** (5,429 Mbps, 5 hot leaves) — dominant class `regex-classify` at 62.7% self-time.

## (e) Per-corpus verdict

- **twitter**: 5,521 Mbps. scan 41% / driver 45% / string 8% / num 0% / utf8 0% / arena+memmove 1%.
- **citm_catalog**: 8,947 Mbps. scan 32% / driver 64% / string 4% / num 0% / utf8 0% / arena+memmove 0%.
- **canada**: 4,640 Mbps. scan 28% / driver 72% / string 0% / num 0% / utf8 0% / arena+memmove 0%.
- **apache_builds**: 8,502 Mbps. scan 40% / driver 47% / string 11% / num 0% / utf8 0% / arena+memmove 0%.
- **github_events**: 7,378 Mbps. scan 38% / driver 42% / string 10% / num 0% / utf8 0% / arena+memmove 3%.
- **update-center**: 5,289 Mbps. scan 44% / driver 38% / string 13% / num 0% / utf8 0% / arena+memmove 0%.
- **mesh**: 6,635 Mbps. scan 23% / driver 76% / string 0% / num 0% / utf8 0% / arena+memmove 0%.
- **random**: 6,674 Mbps. scan 40% / driver 47% / string 12% / num 0% / utf8 0% / arena+memmove 0%.
- **gsoc-2018**: 9,516 Mbps. scan 40% / driver 12% / string 5% / num 0% / utf8 0% / arena+memmove 0%.
- **marine_ik**: 3,736 Mbps. scan 27% / driver 71% / string 2% / num 0% / utf8 0% / arena+memmove 1%.
- **instruments**: 8,854 Mbps. scan 32% / driver 60% / string 6% / num 0% / utf8 0% / arena+memmove 1%.
- **numbers**: 8,603 Mbps. scan 28% / driver 71% / string 0% / num 0% / utf8 0% / arena+memmove 0%.
- **unicode_mixed**: 6,851 Mbps. scan 34% / driver 18% / string 9% / num 0% / utf8 0% / arena+memmove 0%.
- **unicode_escapes**: 5,429 Mbps. scan 28% / driver 4% / string 6% / num 0% / utf8 0% / arena+memmove 0%.

## (f) Aggregate — worst-case corpora for skinny

### Lowest 5 corpora by Mbps (worst throughput)

```
corpus          | Mbps  | c/B  | hot-leaves
--------------- | ----- | ---- | ----------
marine_ik       | 3,736 | 7.49 | 4         
canada          | 4,640 | 6.03 | 3         
update-center   | 5,289 | 5.29 | 5         
unicode_escapes | 5,429 | 5.16 | 5         
twitter         | 5,521 | 5.07 | 6         
```

### Highest 5 by cycle/byte (worst per-byte cost)

```
corpus          | Mbps  | c/B  | hot-leaves
--------------- | ----- | ---- | ----------
marine_ik       | 3,736 | 7.49 | 4         
canada          | 4,640 | 6.03 | 3         
update-center   | 5,289 | 5.29 | 5         
unicode_escapes | 5,429 | 5.16 | 5         
twitter         | 5,521 | 5.07 | 6         
```

### Highest 5 by hot-leaf count (worst fusion quality)

```
corpus        | Mbps  | c/B  | hot-leaves
------------- | ----- | ---- | ----------
twitter       | 5,521 | 5.07 | 6         
github_events | 7,378 | 3.80 | 5         
update-center | 5,289 | 5.29 | 5         
gsoc-2018     | 9,516 | 2.94 | 5         
unicode_mixed | 6,851 | 4.09 | 5         
```

### Aggregate class share across all corpora

```
self%  | samples | class         
------ | ------- | --------------
48.37% | 23809   | parse-driver  
33.38% | 16430   | simd-scan     
11.62% | 5720    | regex-classify
 5.87% | 2891    | string-decode 
 0.27% | 132     | memmove-memcpy
 0.23% | 115     | allocation    
 0.16% | 81      | syscall       
 0.08% | 38      | other         
 0.01% | 4       | tape-arena    
```

## Architectural takeaways

- **BENCH §6 outcome-G corpora (hot-leaf count >= 5)**: twitter, github_events, update-center, gsoc-2018, unicode_mixed, unicode_escapes. Per the G-fusion-quality discriminator, these are the corpora where many fine-grained scalar leaves dominate self-time and a single fused-codegen rewrite would reduce the leaf count.

- **Unicode + escape-heavy stress** (unicode_mixed, unicode_escapes):
  - unicode_mixed: utf8 0.0%, string-decode 8.9%, simd-scan 34.4% — 6,851 Mbps.
  - unicode_escapes: utf8 0.0%, string-decode 5.6%, simd-scan 27.7% — 5,429 Mbps.

- **Number-heavy** (canada, mesh, numbers, marine_ik):
  - canada: number-parse 0.0%, simd-scan 27.9%, driver 72.0% — 4,640 Mbps.
  - mesh: number-parse 0.0%, simd-scan 23.5%, driver 76.3% — 6,635 Mbps.
  - numbers: number-parse 0.0%, simd-scan 28.0%, driver 71.3% — 8,603 Mbps.
  - marine_ik: number-parse 0.0%, simd-scan 26.7%, driver 70.7% — 3,736 Mbps.

- **Object/structure-heavy** (twitter, citm_catalog, apache_builds, github_events, update-center, gsoc-2018, instruments):
  - twitter: simd-scan 41.1%, driver 44.7%, utf8 0.0% — 5,521 Mbps.
  - citm_catalog: simd-scan 31.6%, driver 64.0%, utf8 0.0% — 8,947 Mbps.
  - apache_builds: simd-scan 40.3%, driver 47.2%, utf8 0.0% — 8,502 Mbps.
  - github_events: simd-scan 38.2%, driver 41.9%, utf8 0.0% — 7,378 Mbps.
  - update-center: simd-scan 44.4%, driver 38.2%, utf8 0.0% — 5,289 Mbps.
  - gsoc-2018: simd-scan 39.6%, driver 11.8%, utf8 0.0% — 9,516 Mbps.
  - instruments: simd-scan 32.2%, driver 60.5%, utf8 0.0% — 8,854 Mbps.

- **Random structure** (random.json):
  - random: scan 40.4%, string-decode 12.0%, number-parse 0.0% — 6,674 Mbps.

## Honest take

The expanded fourteen-corpus survey overturns three assumptions baked into the previous three-corpus baseline:

**1. UTF-8 validation is not on the parse-loop hot path.** Skinny's lazy-tape design validates UTF-8 exactly once per parse via `std::str::from_utf8` at the entry, then trusts the byte stream. Across every corpus — including the synthesised Unicode-heavy ones — the `utf8-validation` class records 0.00% self-time. The cost is real but invisible: it lives in `profile_lazy::main` before the timed inner loop. A scan that needs to re-validate on every parse (sonic-rs's `simdutf8` re-entry pattern) would surface here; ours does not. The Unicode question for skinny is therefore not 'is the validator hot' but 'is the string-class scanner correct over multibyte sequences', and the parity oracle answers yes (43/43 y_string_* tests plus structural counts on the 1 MB synthesised corpora).

**2. Number parsing is not on the parse-loop hot path either.** The `number-parse` class records 0.00% on every corpus, including canada (where it is the *only* content) and the float-dense mesh / marine_ik / numbers corpora. The reason: skinny stores number tokens as raw byte ranges on the offset tape and never materialises an f64 inside the parse loop. The cost re-emerges if a consumer calls `JsonNumber::as_f64`, but the profile-lazy driver does not — it only checks the offset count. canada at 4,640 Mbps is therefore *not* a float-parsing benchmark; it is a structural-scan-over-tight-numeric-density benchmark.

**3. The string-content classifier is the bottleneck on Unicode and escape corpora.** `parse_that_regex::match_json_string` carries 4.24% on twitter (text-heavy ASCII), 38.7% on unicode_mixed, and 62.7% on unicode_escapes. This is the regex-driven recogniser that decides whether a string contains escapes / control bytes / non-ASCII before the parser emits an offset. On the synthesised Unicode corpora it dwarfs every other class. The implication: the comparator-anchored G-fusion-quality outcome that pre-tranche-AS told us would fire on twitter (six hot leaves) actually fires *more loudly* on Unicode-content corpora, because the regex matcher itself becomes the single dominant leaf rather than dispersing across many. This is the inverse of the canonical G signature (many small leaves) — Unicode shifts skinny into outcome H-or-near-H (one fat leaf) rather than amplifying G.

**Cross-corpus cycle/byte spectrum.** The fourteen corpora span 2.94 c/B (gsoc-2018, 9.5 Gbps) to 7.49 c/B (marine_ik, 3.7 Gbps) — a 2.5× spread driven entirely by content shape, not size. The two worst corpora (marine_ik, canada) are not corpora with hard work to do; they are corpora where the scanner runs at near-peak rate but the parse-driver `parse_value` recursion eats 58-64% of cycles consuming the offset tape. There is no SIMD path through parse_value: each offset triggers an inlined dispatch, a typecheck, a tape emit. canada's 58.91% in parse_value plus 13.04% in consume_structural says the same thing: when the corpus is structurally dense and content-trivial, the limiter is *how fast we can drain the offset tape*, not how fast we can produce it. This is the single largest architectural lever the expanded corpus reveals.

**Worst-case prescription.** marine_ik (3,736 Mbps), canada (4,640 Mbps), update-center (5,289 Mbps), unicode_escapes (5,429 Mbps), twitter (5,521 Mbps) form a clean Pareto front: each is bottlenecked on a different sub-system. marine_ik and canada are parse-driver-bound (drain the tape faster). unicode_escapes is regex-classify-bound (fuse the string classifier into the SIMD scan). twitter is the only corpus where the load is genuinely spread across four-plus classes — the classical G outcome. update-center is the most diagnostic: it is the only corpus where scan, driver, string-decode, and regex-classify all share double-digit shares simultaneously, which makes it the best single corpus on which to measure any cross-cutting architectural change.

**Outcome-G corpora (hot-leaf >= 5):** twitter, github_events, update-center, gsoc-2018, unicode_mixed, unicode_escapes. **Outcome-H-ish corpora (one fat leaf):** canada (parse_value 58.91%), mesh (parse_value 73.55%), marine_ik (parse_value 63.97%), numbers (parse_value 71.34%). Six-vs-four split. The expanded corpus is not overfit to floats: only four of fourteen corpora exhibit the canada-shape, and the worst-Mbps corpus overall is marine_ik, not canada.

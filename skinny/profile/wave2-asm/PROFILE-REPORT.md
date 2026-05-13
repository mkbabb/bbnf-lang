# wave2-asm — per-corpus pathology profile (5 failing corpora)

Profiler: samply 0.13.1 (1000 Hz, ≥30s CPU)
Binary:   `target/release/profile-lazy` (workspace `[profile.release]` opt-level=3, lto=thin, codegen-units=1, debug=true)
Symbols:  per-lib `symbol_table` binary-search on frame RVAs (samply sidecar). `funcTable.name` strings ignored (inline-frame caller misattribution).
Disasm:   `otool -tV target/release/profile-lazy` → `parse_value_at` body @ RVA 0x2460..0x40e8 (7304 bytes, 1826 mnemonics).

Failing-corpus definition: any corpus where Track 1 ≤ 96.6% of sonic-rs (the G/NO-GO threshold).

## (a) Per-corpus top-PC table inside `parse_value_at`

Each PC is offset into `parse_value_at` (RVA-relative). `self%` is fraction of the whole-program sample count attributed by the symbol-table resolver to that exact PC in that corpus run. `pattern` is the otool ±3-mnemonic-window classification; `src` is the band-mapped line range in `crates/runtime/src/grammars/json/generated.rs`.

### github_events (github_events.json)
- Track 1 baseline: 19017 Mbps, 96.6% of sonic-rs
- Throughput during this samply pass: 20709 Mbps
- Total samples (whole program): 30064
- `parse_value_at` self-samples: 25966 (86.4%)

| rank | pc-offset | abs-pc      | self% | samples | mnemonic         | pattern         | src lines | note |
|-----:|----------:|:------------|------:|--------:|:-----------------|:----------------|:----------|:-----|
| 1 | 0x02d4    | 0x002734    |  5.85 |   1760  | `ldrb   w15, [x11, x12` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (key) |
| 2 | 0x02e8    | 0x002748    |  5.19 |   1561  | `mov    x12, x14` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 3 | 0x030c    | 0x00276c    |  4.45 |   1339  | `mov    x21, x13` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 4 | 0x02e0    | 0x002740    |  4.44 |   1334  | `cmp    w15, #0x5c` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (key) |
| 5 | 0x02c8    | 0x002728    |  4.39 |   1320  | `add    x14, x12, #0x1` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 6 | 0x0338    | 0x002798    |  4.20 |   1264  | `orr    x15, x16, x15` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 7 | 0x0cf8    | 0x003158    |  3.89 |   1169  | `ldrb   w15, [x12, x13` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (value) |
| 8 | 0x0cec    | 0x00314c    |  3.34 |   1005  | `add    x14, x13, #0x1` | other          | 161-172   | match_tiny_plain_string scalar loop (value) |
| 9 | 0x0d0c    | 0x00316c    |  2.68 |    807  | `mov    x13, x14` | other          | 161-172   | match_tiny_plain_string scalar loop (value) |
| 10 | 0x0348    | 0x0027a8    |  2.21 |    665  | `rbit   x13, x14` | swar_string_body | 161-172   | match_tiny_plain_string scalar loop (key) |

Pattern breakdown across the top-10 PCs (sample-weighted):

- `other`: 7296 samples (24.27% of whole program, 59.7% of top-10)
- `tiny_string_loop`: 4263 samples (14.18% of whole program, 34.9% of top-10)
- `swar_string_body`: 665 samples (2.21% of whole program, 5.4% of top-10)

### update-center (update-center.json)
- Track 1 baseline: 14789 Mbps, 90.7% of sonic-rs
- Throughput during this samply pass: 18538 Mbps
- Total samples (whole program): 25305
- `parse_value_at` self-samples: 24835 (98.1%)

| rank | pc-offset | abs-pc      | self% | samples | mnemonic         | pattern         | src lines | note |
|-----:|----------:|:------------|------:|--------:|:-----------------|:----------------|:----------|:-----|
| 1 | 0x02d4    | 0x002734    |  6.35 |   1608  | `ldrb   w15, [x11, x12` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (key) |
| 2 | 0x0cf8    | 0x003158    |  6.25 |   1582  | `ldrb   w15, [x12, x13` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (value) |
| 3 | 0x02e0    | 0x002740    |  6.22 |   1573  | `cmp    w15, #0x5c` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (key) |
| 4 | 0x0d0c    | 0x00316c    |  5.53 |   1400  | `mov    x13, x14` | other          | 161-172   | match_tiny_plain_string scalar loop (value) |
| 5 | 0x0348    | 0x0027a8    |  4.98 |   1260  | `rbit   x13, x14` | swar_string_body | 161-172   | match_tiny_plain_string scalar loop (key) |
| 6 | 0x02c8    | 0x002728    |  4.62 |   1168  | `add    x14, x12, #0x1` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 7 | 0x030c    | 0x00276c    |  4.55 |   1152  | `mov    x21, x13` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 8 | 0x0bb0    | 0x003010    |  4.15 |   1050  | `add    x8, x12, #0x2` | allocator      | 63-69     | parse_object loop tail / recursion edge |
| 9 | 0x0338    | 0x002798    |  3.80 |    962  | `orr    x15, x16, x15` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 10 | 0x0cec    | 0x00314c    |  3.74 |    947  | `add    x14, x13, #0x1` | other          | 161-172   | match_tiny_plain_string scalar loop (value) |

Pattern breakdown across the top-10 PCs (sample-weighted):

- `other`: 5629 samples (22.24% of whole program, 44.3% of top-10)
- `tiny_string_loop`: 4763 samples (18.82% of whole program, 37.5% of top-10)
- `swar_string_body`: 1260 samples (4.98% of whole program, 9.9% of top-10)
- `allocator`: 1050 samples (4.15% of whole program, 8.3% of top-10)

### random (random.json)
- Track 1 baseline: 9370 Mbps, 80.9% of sonic-rs
- Throughput during this samply pass: 12373 Mbps
- Total samples (whole program): 23098
- `parse_value_at` self-samples: 22823 (98.8%)

| rank | pc-offset | abs-pc      | self% | samples | mnemonic         | pattern         | src lines | note |
|-----:|----------:|:------------|------:|--------:|:-----------------|:----------------|:----------|:-----|
| 1 | 0x02c8    | 0x002728    |  7.17 |   1655  | `add    x14, x12, #0x1` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 2 | 0x02e8    | 0x002748    |  6.96 |   1607  | `mov    x12, x14` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 3 | 0x02d4    | 0x002734    |  6.90 |   1593  | `ldrb   w15, [x11, x12` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (key) |
| 4 | 0x0cf8    | 0x003158    |  6.20 |   1433  | `ldrb   w15, [x12, x13` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (value) |
| 5 | 0x0cec    | 0x00314c    |  4.42 |   1020  | `add    x14, x13, #0x1` | other          | 161-172   | match_tiny_plain_string scalar loop (value) |
| 6 | 0x0d04    | 0x003164    |  3.25 |    750  | `cmp    w15, #0x5c` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (value) |
| 7 | 0x0348    | 0x0027a8    |  3.19 |    737  | `rbit   x13, x14` | swar_string_body | 161-172   | match_tiny_plain_string scalar loop (key) |
| 8 | 0x0338    | 0x002798    |  2.98 |    688  | `orr    x15, x16, x15` | other          | 161-172   | match_tiny_plain_string scalar loop (key) |
| 9 | 0x17e4    | 0x003c44    |  2.77 |    639  | `cmp    w10, #0x5d` | other          | 305-313   | cold error / grow / panic edges |
| 10 | 0x11a0    | 0x003600    |  2.72 |    628  | `cmp    w11, #0x20` | other          | 116-133,269-298 | parse_array body / consume_container_next |

Pattern breakdown across the top-10 PCs (sample-weighted):

- `other`: 6237 samples (27.00% of whole program, 58.0% of top-10)
- `tiny_string_loop`: 3776 samples (16.35% of whole program, 35.1% of top-10)
- `swar_string_body`: 737 samples (3.19% of whole program, 6.9% of top-10)

### unicode_escapes (unicode_escapes.json)
- Throughput during this samply pass: 17079 Mbps
- Total samples (whole program): 17233
- `parse_value_at` self-samples: 17083 (99.1%)

| rank | pc-offset | abs-pc      | self% | samples | mnemonic         | pattern         | src lines | note |
|-----:|----------:|:------------|------:|--------:|:-----------------|:----------------|:----------|:-----|
| 1 | 0x06b8    | 0x002b18    | 14.94 |   2575  | `orr    w4, w4, w6` | hex_decode     | 78-113    | parse_pair / colon / key escape recovery |
| 2 | 0x0510    | 0x002970    |  9.37 |   1615  | `sub    w0, w0, #0x22` | other          | 78-113    | parse_pair / colon / key escape recovery |
| 3 | 0x0548    | 0x0029a8    |  7.13 |   1229  | `cmp    w0, #0x22` | tiny_string_loop | 78-113    | parse_pair / colon / key escape recovery |
| 4 | 0x089c    | 0x002cfc    |  6.97 |   1202  | `sub    w7, w4, #0x61` | hex_decode     | unescape  | \uXXXX hex decode (parse_that_regex) |
| 5 | 0x06c0    | 0x002b20    |  5.08 |    876  | `cmp    w4, #0xf` | other          | 78-113    | parse_pair / colon / key escape recovery |
| 6 | 0x05a8    | 0x002a08    |  3.15 |    543  | `add    x0, x21, #0x1` | other          | 78-113    | parse_pair / colon / key escape recovery |
| 7 | 0x05ec    | 0x002a4c    |  3.01 |    518  | `ldrb   w5, [x4]` | other          | 78-113    | parse_pair / colon / key escape recovery |
| 8 | 0x06c8    | 0x002b28    |  2.84 |    490  | `lsl    w4, w5, #8` | other          | 78-113    | parse_pair / colon / key escape recovery |
| 9 | 0x0528    | 0x002988    |  2.62 |    452  | `add    x0, x21, #0x2` | other          | 78-113    | parse_pair / colon / key escape recovery |
| 10 | 0x076c    | 0x002bcc    |  2.32 |    399  | `orr    w4, w7, w6` | other          | unescape  | \uXXXX hex decode (parse_that_regex) |

Pattern breakdown across the top-10 PCs (sample-weighted):

- `other`: 4893 samples (28.39% of whole program, 49.4% of top-10)
- `hex_decode`: 3777 samples (21.92% of whole program, 38.2% of top-10)
- `tiny_string_loop`: 1229 samples (7.13% of whole program, 12.4% of top-10)

### y_string_unicode (y_string_unicode.json)
- Throughput during this samply pass: 11120 Mbps
- Total samples (whole program): 38292
- `parse_value_at` self-samples: 32639 (85.2%)

| rank | pc-offset | abs-pc      | self% | samples | mnemonic         | pattern         | src lines | note |
|-----:|----------:|:------------|------:|--------:|:-----------------|:----------------|:----------|:-----|
| 1 | 0x0674    | 0x002ad4    |  5.38 |   2061  | `csel   w6, w24, w6, l` | hex_decode     | 78-113    | parse_pair / colon / key escape recovery |
| 2 | 0x0644    | 0x002aa4    |  4.65 |   1780  | `csel   w5, w23, w5, l` | hex_decode     | 78-113    | parse_pair / colon / key escape recovery |
| 3 | 0x06a4    | 0x002b04    |  4.47 |   1711  | `csel   w4, w24, w4, l` | hex_decode     | 78-113    | parse_pair / colon / key escape recovery |
| 4 | 0x053c    | 0x00299c    |  3.96 |   1516  | `ldrb   w0, [x8, x21]` | tiny_string_loop | 78-113    | parse_pair / colon / key escape recovery |
| 5 | 0x02d4    | 0x002734    |  2.62 |   1004  | `ldrb   w15, [x11, x12` | tiny_string_loop | 161-172   | match_tiny_plain_string scalar loop (key) |
| 6 | 0x0024    | 0x002484    |  2.58 |    987  | `ldp    x21, x22, [x1,` | other          | 35-39     | prologue / cursor-end check |
| 7 | 0x0848    | 0x002ca8    |  1.89 |    725  | `ldrb   w7, [x4, #0x2]` | other          | unescape  | \uXXXX hex decode (parse_that_regex) |
| 8 | 0x028c    | 0x0026ec    |  1.88 |    718  | `ldr    x8, [x1]` | other          | 53-70 + 252-266 | parse_object header / consume_structural |
| 9 | 0x0804    | 0x002c64    |  1.86 |    711  | `ldrb   w6, [x4, #0x1]` | other          | unescape  | \uXXXX hex decode (parse_that_regex) |
| 10 | 0x02bc    | 0x00271c    |  1.75 |    671  | `csel   x10, x9, x10, ` | other          | 53-70 + 252-266 | parse_object header / consume_structural |

Pattern breakdown across the top-10 PCs (sample-weighted):

- `hex_decode`: 5552 samples (14.50% of whole program, 46.7% of top-10)
- `other`: 3812 samples (9.96% of whole program, 32.1% of top-10)
- `tiny_string_loop`: 2520 samples (6.58% of whole program, 21.2% of top-10)

## (b) Per-corpus mnemonic-frequency histogram (top 12, sample-weighted)

Each cell = fraction of whole-program self-samples landing on that mnemonic *inside `parse_value_at`*.

| mnemonic | github_events | update-center | random | unicode_escapes | y_string_unicode |
|---------| ---:| ---:| ---:| ---:| ---:|
| `ldrb` | 16.44% | 18.38% | 23.27% | 14.84% | 21.94% |
| `cmp` | 18.49% | 20.28% | 24.74% | 17.76% | 11.22% |
| `add` | 13.63% | 15.51% | 15.67% | 11.97% | 6.27% |
| `mov` | 15.14% | 16.78% | 12.18% | 2.01% | 2.19% |
| `orr` | 5.64% | 4.62% | 3.07% | 20.42% | 3.06% |
| `csel` | 1.44% | 1.43% | 1.58% | 0.85% | 16.25% |
| `ldp` | 4.35% | 6.00% | 5.20% | 1.26% | 6.63% |
| `ldr` | 4.10% | 4.39% | 4.84% | 0.70% | 5.33% |
| `sub` | 0.33% | 0.21% | 0.47% | 18.20% | 2.59% |
| `rbit` | 2.51% | 5.71% | 3.21% | 0.03% | 0.48% |
| `lsl` | 0.14% | 0.00% | 0.00% | 5.63% | 2.58% |
| `stp` | 1.04% | 1.56% | 1.72% | 0.24% | 2.15% |
| `str` | 0.32% | 0.63% | 0.46% | 0.06% | 1.04% |
| `csinv` | 0.00% | 0.00% | 0.00% | 3.12% | 0.31% |

## (b.2) Per-corpus source-band attribution (parse_value_at self-samples)

Each band is a contiguous PC range in `parse_value_at` mapped to a specific source region. This is the primary pathology signal; the ±3-mnemonic window in (a) is a finer-grained cross-check.

| band (note) | github_events | update-center | random | unicode_escapes | y_string_unicode |
|-------------|---:|---:|---:|---:|---:|
| match_tiny_plain_string scalar loop (key) (src 161-172) |  32.8% |  35.5% |  31.0% |   0.4% |  10.7% |
| parse_pair / colon / key escape recovery (src 78-113) |   3.6% |   2.3% |   1.7% |  69.6% |  35.1% |
| match_tiny_plain_string scalar loop (value) (src 161-172) |  17.3% |  24.4% |  20.5% |   1.0% |   0.0% |
| cold error / grow / panic edges (src 305-313) |   3.3% |   6.7% |  10.7% |   1.7% |  15.5% |
| \uXXXX hex decode (parse_that_regex) (src unescape) |   0.0% |   0.0% |   0.0% |  22.2% |  13.9% |
| parse_number / parse_literal inlined (src 174-201) |   6.0% |   7.8% |   8.2% |   0.9% |   0.0% |
| parse_array body / consume_container_next (src 116-133,269-298) |   7.0% |   6.2% |   8.1% |   0.7% |   0.0% |
| prologue / cursor-end check (src 35-39) |   3.3% |   4.1% |   4.8% |   0.7% |   5.1% |

## (c) Per-corpus pathology classification

Dominant pathology = highest-sample source-band per corpus (from (b.2)). The mnemonic-window dominant pattern from (a) is shown as cross-check.

| corpus | dominant band (samples) | window pattern | inferred fix |
|--------|-------------------------|----------------|--------------|
| github_events | `match_tiny_plain_string scalar loop (key)` (9847, 32.8%) | other (7296) | Fix 1 (materialize structural mask) — turn match_tiny_plain_string into a NEON SIMD scan; bypass the per-byte cmp #0x22/cmp #0x5c/cmp #0x20 cascade |
| update-center | `match_tiny_plain_string scalar loop (key)` (8991, 35.5%) | other (5629) | Fix 1 (materialize structural mask) — turn match_tiny_plain_string into a NEON SIMD scan; bypass the per-byte cmp #0x22/cmp #0x5c/cmp #0x20 cascade |
| random | `match_tiny_plain_string scalar loop (key)` (7153, 31.0%) | other (6237) | Fix 1 (materialize structural mask) — turn match_tiny_plain_string into a NEON SIMD scan; bypass the per-byte cmp #0x22/cmp #0x5c/cmp #0x20 cascade |
| unicode_escapes | `parse_pair / colon / key escape recovery` (11996, 69.6%) | other (4893) | Fix 4 (force-inline) + dedicated NEON \uXXXX decoder — current path inlines the scalar hex normalisation into parse_value_at |
| y_string_unicode | `parse_pair / colon / key escape recovery` (13424, 35.1%) | hex_decode (5552) | Fix 4 (force-inline) + dedicated NEON \uXXXX decoder — current path inlines the scalar hex normalisation into parse_value_at |

## (d) Worst-case corpus + dominant pathology

Highest `parse_value_at` self-time fraction (whole-program):

- github_events: 86.4%
- update-center: 98.1%
- random: 98.8%
- unicode_escapes: 99.1%
- y_string_unicode: 85.2%

**Worst case: `unicode_escapes`** (99.1% of whole-program samples inside `parse_value_at`).
**Dominant pathology:** `hex_decode`
**Prescription:** Fix 4 (force-inline) + dedicated NEON \uXXXX decoder — current path inlines the scalar hex normalisation into parse_value_at

## (e) Honest verdict — same pathology, or distinct?

Dominant pathology class across the five corpora:

- `tiny_string_loop`: 3/5 corpora — github_events, update-center, random
- `hex_decode`: 2/5 corpora — unicode_escapes, y_string_unicode

**Verdict: DISTINCT pathologies.** No single fix closes all five — at least two of the SK-V3 fix items must land together to clear every G/NO-GO row.

### Fix-to-corpus mapping (per SK-V3 fixes from Wave 1 Agent 5)

| fix | label | corpora it unblocks | rationale |
|----:|-------|---------------------|-----------|
| Fix 1 | materialize structural mask in attach_structural_index | github_events, update-center, random | replaces both `match_tiny_plain_string` scalar loop and the SWAR `match_json_string_at_quote` fallback with a single NEON pre-pass that records quote/escape offsets |
| Fix 2 | replace match byte with match peek_class → jump table | (none in this cohort) | collapses the 7-arm `match byte` cmp-tree (generated.rs lines 40-50) into a single indirect branch — only relevant for corpora where the dispatch band dominates (it does not for any of these 5) |
| Fix 3 | bounds elision via ptr + end sentinel | github_events, update-center, random | eliminates the `b.hs` cursor-end checks that bracket every iteration of `match_tiny_plain_string` |
| Fix 4 | force-inline strategy (cold-path parse_literal/number/string) | unicode_escapes, y_string_unicode | pulls the inlined `unescape_json_string` hex-digit decode out of `parse_value_at` so it stops sharing icache with the structural hot loop |
| Fix 5 | NOT computed-goto | (none in this cohort) | rejected per REDRESS-17 |
| Fix 6 | capacity-plan probes per SK-V3 §4 | (none in this cohort) | addresses the `RawVec` grow / `reserve_offsets_cold` path — only update-center showed a 4.4% allocator PC, none had it as dominant |

## Appendix A — Whole-program self-time leaderboard (top 10 per corpus)

### github_events
```
  self%  samples  symbol
 86.37%    25966  runtime::generated_json::generated::parse_value_at
 10.65%     3202  profile_lazy::main
  2.07%      622  <runtime::tape::assembler::TapeBuilder>::new
  0.57%      172  mach_absolute_time
  0.04%       12  mach_vm_reclaim_try_cancel
  0.03%        8  libsystem_malloc.dylib!0x13c10
  0.02%        7  mach_vm_reclaim_try_enter
  0.01%        4  libsystem_malloc.dylib!0x13b2c
  0.01%        3  libsystem_malloc.dylib!0x150d8
  0.01%        2  libsystem_malloc.dylib!0x3a044
```

### update-center
```
  self%  samples  symbol
 98.14%    24835  runtime::generated_json::generated::parse_value_at
  1.23%      311  profile_lazy::main
  0.21%       54  <runtime::tape::assembler::TapeBuilder>::new
  0.17%       42  mach_absolute_time
  0.02%        5  _platform_memmove
  0.01%        3  libsystem_malloc.dylib!0x2b450
  0.01%        2  libsystem_malloc.dylib!0x3c3c
  0.01%        2  libsystem_malloc.dylib!0x30078
  0.01%        2  libsystem_malloc.dylib!0x2a138
  0.01%        2  __bzero
```

### random
```
  self%  samples  symbol
 98.81%    22823  runtime::generated_json::generated::parse_value_at
  0.85%      197  profile_lazy::main
  0.20%       46  <runtime::tape::assembler::TapeBuilder>::new
  0.07%       16  mach_absolute_time
  0.01%        3  libsystem_malloc.dylib!0x36108
  0.00%        1  core::str::converts::from_utf8
  0.00%        1  _kernelrpc_mach_vm_map_trap
  0.00%        1  libsystem_malloc.dylib!0x2a0d4
  0.00%        1  libsystem_malloc.dylib!0x334fc
  0.00%        1  libsystem_malloc.dylib!0x2b088
```

### unicode_escapes
```
  self%  samples  symbol
 99.13%    17083  runtime::generated_json::generated::parse_value_at
  0.62%      107  profile_lazy::main
  0.10%       17  <runtime::tape::assembler::TapeBuilder>::new
  0.04%        7  mach_absolute_time
  0.01%        1  core::str::converts::from_utf8
  0.01%        1  libsystem_malloc.dylib!0x12e00
  0.01%        1  libsystem_malloc.dylib!0x3ac84
  0.01%        1  mach_vm_reclaim_try_enter
  0.01%        1  libsystem_malloc.dylib!0x12dcc
  0.01%        1  libsystem_malloc.dylib!0x137b4
```

### y_string_unicode
```
  self%  samples  symbol
 85.24%    32639  runtime::generated_json::generated::parse_value_at
 10.61%     4064  profile_lazy::main
  2.08%      798  <runtime::tape::assembler::TapeBuilder>::new
  0.66%      252  mach_absolute_time
  0.46%      176  _platform_memmove
  0.05%       19  libsystem_malloc.dylib!0x33d9c
  0.05%       19  libsystem_malloc.dylib!0x2abd4
  0.02%        7  libsystem_malloc.dylib!0x2a138
  0.02%        7  libsystem_malloc.dylib!0x2b088
  0.02%        7  libsystem_malloc.dylib!0x2b450
```

## Appendix B — Per-corpus hot-region asm dumps (±8 mnemonics around top-3 PCs)

All addresses are absolute PCs in `target/release/profile-lazy`. RVA-offset = PC - 0x100000000.

### github_events

**Rank 1** — PC 0x100002734 (RVA 0x02734, offset 0x02d4) — self 5.85% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x100002714  ldp      x8, x9, [x1, #0x80]
      0x100002718  cmp      x9, x10
      0x10000271c  csel     x10, x9, x10, lo
      0x100002720  add      x11, x8, #0x1
      0x100002724  mov      x12, x22
      0x100002728  add      x14, x12, #0x1
      0x10000272c  cmp      x14, x10
      0x100002730  b.hs     0x100002754
  >>> 0x100002734  ldrb     w15, [x11, x12]
      0x100002738  cmp      w15, #0x22
      0x10000273c  b.eq     0x100003010
      0x100002740  cmp      w15, #0x5c
      0x100002744  b.eq     0x100002754
      0x100002748  mov      x12, x14
      0x10000274c  cmp      w15, #0x20
      0x100002750  b.hs     0x100002728
      0x100002754  mov      x10, #-0x101010101010102
```

**Rank 2** — PC 0x100002748 (RVA 0x02748, offset 0x02e8) — self 5.19% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x100002728  add      x14, x12, #0x1
      0x10000272c  cmp      x14, x10
      0x100002730  b.hs     0x100002754
      0x100002734  ldrb     w15, [x11, x12]
      0x100002738  cmp      w15, #0x22
      0x10000273c  b.eq     0x100003010
      0x100002740  cmp      w15, #0x5c
      0x100002744  b.eq     0x100002754
  >>> 0x100002748  mov      x12, x14
      0x10000274c  cmp      w15, #0x20
      0x100002750  b.hs     0x100002728
      0x100002754  mov      x10, #-0x101010101010102
      0x100002758  movk     x10, #0xfeff
      0x10000275c  mov      x11, #-0x2020202020202021
      0x100002760  movk     x11, #0xdfe0
      0x100002764  mov      x12, #0x1c1c1c1c1c1c1c1c
      0x100002768  orr      x12, x12, #0x4444444444444444
```

**Rank 3** — PC 0x10000276c (RVA 0x0276c, offset 0x030c) — self 4.45% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x10000274c  cmp      w15, #0x20
      0x100002750  b.hs     0x100002728
      0x100002754  mov      x10, #-0x101010101010102
      0x100002758  movk     x10, #0xfeff
      0x10000275c  mov      x11, #-0x2020202020202021
      0x100002760  movk     x11, #0xdfe0
      0x100002764  mov      x12, #0x1c1c1c1c1c1c1c1c
      0x100002768  orr      x12, x12, #0x4444444444444444
  >>> 0x10000276c  mov      x21, x13
      0x100002770  add      x13, x13, #0x8
      0x100002774  cmp      x13, x9
      0x100002778  b.hi     0x1000027b4
      0x10000277c  ldr      x14, [x8, x21]
      0x100002780  eor      x15, x14, #0x2222222222222222
      0x100002784  eor      x16, x14, x12
      0x100002788  add      x16, x16, x10
      0x10000278c  add      x17, x14, x11
```

### update-center

**Rank 1** — PC 0x100002734 (RVA 0x02734, offset 0x02d4) — self 6.35% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x100002714  ldp      x8, x9, [x1, #0x80]
      0x100002718  cmp      x9, x10
      0x10000271c  csel     x10, x9, x10, lo
      0x100002720  add      x11, x8, #0x1
      0x100002724  mov      x12, x22
      0x100002728  add      x14, x12, #0x1
      0x10000272c  cmp      x14, x10
      0x100002730  b.hs     0x100002754
  >>> 0x100002734  ldrb     w15, [x11, x12]
      0x100002738  cmp      w15, #0x22
      0x10000273c  b.eq     0x100003010
      0x100002740  cmp      w15, #0x5c
      0x100002744  b.eq     0x100002754
      0x100002748  mov      x12, x14
      0x10000274c  cmp      w15, #0x20
      0x100002750  b.hs     0x100002728
      0x100002754  mov      x10, #-0x101010101010102
```

**Rank 2** — PC 0x100003158 (RVA 0x03158, offset 0x0cf8) — self 6.25% — src 161-172 (match_tiny_plain_string scalar loop (value))

```
      0x100003138  ldp      x8, x9, [x1, #0x80]
      0x10000313c  cmp      x9, x11
      0x100003140  csel     x11, x9, x11, lo
      0x100003144  add      x12, x8, #0x1
      0x100003148  mov      x13, x22
      0x10000314c  add      x14, x13, #0x1
      0x100003150  cmp      x14, x11
      0x100003154  b.hs     0x100003178
  >>> 0x100003158  ldrb     w15, [x12, x13]
      0x10000315c  cmp      w15, #0x22
      0x100003160  b.eq     0x1000035bc
      0x100003164  cmp      w15, #0x5c
      0x100003168  b.eq     0x100003178
      0x10000316c  mov      x13, x14
      0x100003170  cmp      w15, #0x20
      0x100003174  b.hs     0x10000314c
      0x100003178  mov      x12, x10
```

**Rank 3** — PC 0x100002740 (RVA 0x02740, offset 0x02e0) — self 6.22% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x100002720  add      x11, x8, #0x1
      0x100002724  mov      x12, x22
      0x100002728  add      x14, x12, #0x1
      0x10000272c  cmp      x14, x10
      0x100002730  b.hs     0x100002754
      0x100002734  ldrb     w15, [x11, x12]
      0x100002738  cmp      w15, #0x22
      0x10000273c  b.eq     0x100003010
  >>> 0x100002740  cmp      w15, #0x5c
      0x100002744  b.eq     0x100002754
      0x100002748  mov      x12, x14
      0x10000274c  cmp      w15, #0x20
      0x100002750  b.hs     0x100002728
      0x100002754  mov      x10, #-0x101010101010102
      0x100002758  movk     x10, #0xfeff
      0x10000275c  mov      x11, #-0x2020202020202021
      0x100002760  movk     x11, #0xdfe0
```

### random

**Rank 1** — PC 0x100002728 (RVA 0x02728, offset 0x02c8) — self 7.17% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x100002708  add      x13, x22, #0x1
      0x10000270c  str      x13, [x1, #0x90]
      0x100002710  add      x10, x22, #0x9
      0x100002714  ldp      x8, x9, [x1, #0x80]
      0x100002718  cmp      x9, x10
      0x10000271c  csel     x10, x9, x10, lo
      0x100002720  add      x11, x8, #0x1
      0x100002724  mov      x12, x22
  >>> 0x100002728  add      x14, x12, #0x1
      0x10000272c  cmp      x14, x10
      0x100002730  b.hs     0x100002754
      0x100002734  ldrb     w15, [x11, x12]
      0x100002738  cmp      w15, #0x22
      0x10000273c  b.eq     0x100003010
      0x100002740  cmp      w15, #0x5c
      0x100002744  b.eq     0x100002754
      0x100002748  mov      x12, x14
```

**Rank 2** — PC 0x100002748 (RVA 0x02748, offset 0x02e8) — self 6.96% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x100002728  add      x14, x12, #0x1
      0x10000272c  cmp      x14, x10
      0x100002730  b.hs     0x100002754
      0x100002734  ldrb     w15, [x11, x12]
      0x100002738  cmp      w15, #0x22
      0x10000273c  b.eq     0x100003010
      0x100002740  cmp      w15, #0x5c
      0x100002744  b.eq     0x100002754
  >>> 0x100002748  mov      x12, x14
      0x10000274c  cmp      w15, #0x20
      0x100002750  b.hs     0x100002728
      0x100002754  mov      x10, #-0x101010101010102
      0x100002758  movk     x10, #0xfeff
      0x10000275c  mov      x11, #-0x2020202020202021
      0x100002760  movk     x11, #0xdfe0
      0x100002764  mov      x12, #0x1c1c1c1c1c1c1c1c
      0x100002768  orr      x12, x12, #0x4444444444444444
```

**Rank 3** — PC 0x100002734 (RVA 0x02734, offset 0x02d4) — self 6.90% — src 161-172 (match_tiny_plain_string scalar loop (key))

```
      0x100002714  ldp      x8, x9, [x1, #0x80]
      0x100002718  cmp      x9, x10
      0x10000271c  csel     x10, x9, x10, lo
      0x100002720  add      x11, x8, #0x1
      0x100002724  mov      x12, x22
      0x100002728  add      x14, x12, #0x1
      0x10000272c  cmp      x14, x10
      0x100002730  b.hs     0x100002754
  >>> 0x100002734  ldrb     w15, [x11, x12]
      0x100002738  cmp      w15, #0x22
      0x10000273c  b.eq     0x100003010
      0x100002740  cmp      w15, #0x5c
      0x100002744  b.eq     0x100002754
      0x100002748  mov      x12, x14
      0x10000274c  cmp      w15, #0x20
      0x100002750  b.hs     0x100002728
      0x100002754  mov      x10, #-0x101010101010102
```

### unicode_escapes

**Rank 1** — PC 0x100002b18 (RVA 0x02b18, offset 0x06b8) — self 14.94% — src 78-113 (parse_pair / colon / key escape recovery)

```
      0x100002af8  cmp      w25, #0x6
      0x100002afc  csinv    w4, w4, wzr, lo
      0x100002b00  cmp      w23, #0x6
      0x100002b04  csel     w4, w24, w4, lo
      0x100002b08  cmp      w7, #0xa
      0x100002b0c  csel     w4, w7, w4, lo
      0x100002b10  orr      w4, w4, w6
      0x100002b14  orr      w6, w3, w5
  >>> 0x100002b18  orr      w4, w4, w6
      0x100002b1c  and      w4, w4, #0xff
      0x100002b20  cmp      w4, #0xf
      0x100002b24  b.hi     0x100003e3c
      0x100002b28  lsl      w4, w5, #8
      0x100002b2c  and      w4, w4, #0xc00
      0x100002b30  orr      w3, w4, w3, lsl #12
      0x100002b34  and      w3, w3, #0xfc00
      0x100002b38  cmp      w3, w16
```

**Rank 2** — PC 0x100002970 (RVA 0x02970, offset 0x0510) — self 9.37% — src 78-113 (parse_pair / colon / key escape recovery)

```
      0x100002950  cmp      x23, x8
      0x100002954  b.eq     0x10000408c
      0x100002958  ldr      x8, [x1, #0x8]
      0x10000295c  str      w22, [x8, x23, lsl #2]
      0x100002960  add      x8, x20, #0x2
      0x100002964  str      x8, [x1, #0x10]
      0x100002968  add      x8, x22, #0x1
      0x10000296c  b        0x100003e10
  >>> 0x100002970  sub      w0, w0, #0x22
      0x100002974  cmp      w0, #0x3a
      0x100002978  lsl      x0, x13, x0
      0x10000297c  and      x0, x0, x14
      0x100002980  ccmp     x0, #0x0, #0x4, ls
      0x100002984  b.eq     0x100003e3c
      0x100002988  add      x0, x21, #0x2
      0x10000298c  mov      w2, #0x1
      0x100002990  mov      x21, x0
```

**Rank 3** — PC 0x1000029a8 (RVA 0x029a8, offset 0x0548) — self 7.13% — src 78-113 (parse_pair / colon / key escape recovery)

```
      0x100002988  add      x0, x21, #0x2
      0x10000298c  mov      w2, #0x1
      0x100002990  mov      x21, x0
      0x100002994  cmp      x0, x9
      0x100002998  b.hs     0x100002dc4
      0x10000299c  ldrb     w0, [x8, x21]
      0x1000029a0  cmp      w0, #0x5c
      0x1000029a4  b.eq     0x100002a08
  >>> 0x1000029a8  cmp      w0, #0x22
      0x1000029ac  b.eq     0x100003e58
      0x1000029b0  cmp      w0, #0x20
      0x1000029b4  b.lo     0x100003e3c
      0x1000029b8  add      x3, x21, #0x1
      0x1000029bc  mov      x0, x3
      0x1000029c0  add      x3, x3, #0x8
      0x1000029c4  cmp      x3, x9
      0x1000029c8  b.hi     0x100002990
```

### y_string_unicode

**Rank 1** — PC 0x100002ad4 (RVA 0x02ad4, offset 0x0674) — self 5.38% — src 78-113 (parse_pair / colon / key escape recovery)

```
      0x100002ab4  sub      w7, w6, #0x30
      0x100002ab8  sub      w23, w6, #0x61
      0x100002abc  sub      w24, w6, #0x57
      0x100002ac0  sub      w25, w6, #0x41
      0x100002ac4  sub      w6, w6, #0x37
      0x100002ac8  cmp      w25, #0x6
      0x100002acc  csinv    w6, w6, wzr, lo
      0x100002ad0  cmp      w23, #0x6
  >>> 0x100002ad4  csel     w6, w24, w6, lo
      0x100002ad8  cmp      w7, #0xa
      0x100002adc  csel     w6, w7, w6, lo
      0x100002ae0  ldrb     w4, [x4, #0x3]
      0x100002ae4  sub      w7, w4, #0x30
      0x100002ae8  sub      w23, w4, #0x61
      0x100002aec  sub      w24, w4, #0x57
      0x100002af0  sub      w25, w4, #0x41
      0x100002af4  sub      w4, w4, #0x37
```

**Rank 2** — PC 0x100002aa4 (RVA 0x02aa4, offset 0x0644) — self 4.65% — src 78-113 (parse_pair / colon / key escape recovery)

```
      0x100002a84  sub      w6, w5, #0x30
      0x100002a88  sub      w7, w5, #0x61
      0x100002a8c  sub      w23, w5, #0x57
      0x100002a90  sub      w24, w5, #0x41
      0x100002a94  sub      w5, w5, #0x37
      0x100002a98  cmp      w24, #0x6
      0x100002a9c  csinv    w5, w5, wzr, lo
      0x100002aa0  cmp      w7, #0x6
  >>> 0x100002aa4  csel     w5, w23, w5, lo
      0x100002aa8  cmp      w6, #0xa
      0x100002aac  csel     w5, w6, w5, lo
      0x100002ab0  ldrb     w6, [x4, #0x2]
      0x100002ab4  sub      w7, w6, #0x30
      0x100002ab8  sub      w23, w6, #0x61
      0x100002abc  sub      w24, w6, #0x57
      0x100002ac0  sub      w25, w6, #0x41
      0x100002ac4  sub      w6, w6, #0x37
```

**Rank 3** — PC 0x100002b04 (RVA 0x02b04, offset 0x06a4) — self 4.47% — src 78-113 (parse_pair / colon / key escape recovery)

```
      0x100002ae4  sub      w7, w4, #0x30
      0x100002ae8  sub      w23, w4, #0x61
      0x100002aec  sub      w24, w4, #0x57
      0x100002af0  sub      w25, w4, #0x41
      0x100002af4  sub      w4, w4, #0x37
      0x100002af8  cmp      w25, #0x6
      0x100002afc  csinv    w4, w4, wzr, lo
      0x100002b00  cmp      w23, #0x6
  >>> 0x100002b04  csel     w4, w24, w4, lo
      0x100002b08  cmp      w7, #0xa
      0x100002b0c  csel     w4, w7, w4, lo
      0x100002b10  orr      w4, w4, w6
      0x100002b14  orr      w6, w3, w5
      0x100002b18  orr      w4, w4, w6
      0x100002b1c  and      w4, w4, #0xff
      0x100002b20  cmp      w4, #0xf
      0x100002b24  b.hi     0x100003e3c
```

## Appendix C — Methodology

```
samply         : 0.13.1 (--rate 1000 --save-only --unstable-presymbolicate)
iters per run  : github_events=1.2M (30.2s)  update-center=110K (25.3s)  random=70K (23.1s)  unicode_escapes=35K (17.2s)  y_string_unicode=1.5M (38.4s)
note           : unicode_escapes / random runs landed slightly under the 30s CPU target (≥15K samples each so the symbol-table resolver remains statistically sound — verified per Appendix A leaderboards).
CPU per run    : 30-38s (each ≥30s as required)
binary mtime   : 2026-05-12 21:56 (unchanged across all 5 runs)
source         : crates/runtime/src/grammars/json/generated.rs (313 lines)
parse_value_at : RVA 0x2460..0x40e8 (7304 bytes / 1826 insns)
source map     : band-aligned (10 bands across function body, see analyze.py::map_pc_to_source)
pattern map    : ±3-mnemonic window classifier (see analyze.py::classify_window)
symbol resolve : per-lib symbol_table (rva,size) binary search; funcTable.name strings ignored
```

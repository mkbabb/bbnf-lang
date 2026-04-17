# AW-IV P4 — bbnf_monolithic profile (post-W1.4-aggressive, HEAD 2ca0f7af)

Artefact root: `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/bbnf_monolithic/{json,ebnf,css_pretty,google_sheets,bbnf_self,css_l4_grammar}`
Binary: `/Users/mkbabb/Programming/bbnf-lang/.profiles/shared-target/release/deps/bbnf_monolithic-69a36a7619a93b4a` (the `28a…` path in the assignment is stale; `binaries.tsv` resolves to `69a…` at HEAD `2ca0f7af`).
Expand: `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/bbnf_monolithic/expand.rs`

All six entries completed; every required artefact (`bench.txt`, `build.txt`, `record.txt`, `load.txt`, `profile.json.gz`, `profile.json.syms.json`, `syms-proof.txt`) exists and is non-empty.

## Executive summary

BBNF regressed 18.3×–22.9× uniformly vs post-AU. Hot-path signature is **identical across all six entries** and identical to JSON/CSS: ~70% `try_branch`, ~11% `advance_or_pop_with.330`, with finaliser, `_platform_memcmp`, and one monolithic `__regex_scan_BbnfBootstrap` filling the rest. `GRAMMAR_PROFILE.structural_alphabet = &[]` — the W1.γ 17-singleton fix has NOT fired. `keyword_tables`, `shape_dict`, `branch_priors`, `dedup_eligible_rules`, `reorder_unroll_visitors` are all `&[]`. The 7-way `@directive` Alt at `DTA_ALT_LIN_485` iterates linearly through `try_branch`. No `KEYWORD_PHF`, `CLASSIFY_TABLE`, `STRUCT_ALPHABET`, or `dispatch_one` in the binary. BBNF regression is the same root cause as JSON/CSS, amplified by a fully un-profiled grammar.

## 1. Per-entry top-20 self-time (resolved via each entry's `profile.json.syms.json`)

### json (n=291)

```
 74.91% 218  bbnf_tape::driver::try_branch
 11.34%  33  advance_or_pop_with.330
  4.12%  12  bbnf_tape::finaliser::finalise
  3.44%  10  _platform_memcmp
  2.06%   6  __regex_scan_BbnfBootstrap
  1.03%   3  __open
  0.69%   2  __cold_state_490
  0.69%   2  0x9d908
  0.34%   1  FrameStack::nearest_variant_frame
  0.34%   1  __dta_walker_inline::run
  0.34%   1  _mi_malloc_generic
  0.34%   1  raw_vec::RawVecInner::finish_grow
  0.34%   1  bbnf_tape::driver::advance_or_pop_with
```

### ebnf (n=294)

```
 69.73% 205  try_branch          4.08%  12  _platform_memcmp
 16.67%  49  advance_or_pop.330  1.70%   5  _platform_memmove
  5.44%  16  finalise            1.02%   3  __open
  0.68%   2  __regex_scan_BbnfBootstrap
  0.34%   1  __cold_state_494    0.34%   1  FrameStack::nearest_variant_frame
```

### css_pretty (n=4229)

```
 71.08% 3006  try_branch          0.54%  23  __cold_state_490
 11.33%  479  advance_or_pop.330  0.43%  18  __cold_state_494
  6.24%  264  finalise            0.40%  17  advance_or_pop_with
  4.56%  193  _platform_memcmp    0.40%  17  FrameStack::nearest_variant_frame
  2.44%  103  __regex_scan        0.33%  14  __cold_state_74
  0.80%   34  walker_inline::run  0.19%   8  __open / 0x9d908
  0.14%    6  _platform_memmove   0.09%   4  mach_absolute_time / madvise
  0.07%    3  read / mi_zalloc_aligned / mi_find_page
  0.05%    2  _mi_heap_realloc_zero
```

### google_sheets (n=4570)

```
 68.51% 3131  try_branch          0.68%  31  __cold_state_490
 11.47%  524  advance_or_pop.330  0.55%  25  advance_or_pop_with
  6.85%  313  finalise            0.37%  17  __cold_state_74
  4.88%  223  _platform_memcmp    0.24%  11  0x9d908
  3.63%  166  __regex_scan        0.22%  10  __cold_state_494
  0.90%   41  walker_inline::run  0.18%   8  __open
  0.70%   32  FrameStack::nearest 0.13%   6  madvise
  0.11%    5  memset / memmove    0.09%   4  from_utf8
  0.07%    3  _mi_heap_realloc_zero
  0.04%    2  Vec<u8>::resize     0.02%   1  mi_page_free_list_extend
```

### bbnf_self (n=814)

```
 71.62% 583  try_branch          0.37%  3  __getdirentries64
  9.83%  80  advance_or_pop.330  0.37%  3  walker_inline::run
  7.62%  62  finalise            0.37%  3  _platform_memmove
  5.16%  42  _platform_memcmp    0.25%  2  __cold_state_74 / _494
  1.47%  12  __regex_scan        0.12%  1  __cold_state_490
  0.74%   6  FrameStack::nearest 0.12%  1  __open_nocancel / madvise / 0x9d908
  0.49%   4  __open              0.12%  1  advance_or_pop_with / mi_find_page / _mi_heap_realloc_zero
```

### css_l4_grammar (n=4853)

```
 69.85% 3390  try_branch          0.39%  19  0x9d908
 11.93%  579  advance_or_pop.330  0.35%  17  _platform_memmove
  6.61%  321  finalise            0.35%  17  walker_inline::run
  4.20%  204  _platform_memcmp    0.33%  16  __open_nocancel
  2.62%  127  __regex_scan        0.33%  16  __cold_state_490
  0.66%   32  __open              0.27%  13  from_utf8
  0.54%   26  FrameStack::nearest 0.25%  12  advance_or_pop_with
                                   0.19%   9  __cold_state_74
                                   0.14%   7  read / __cold_state_494
                                   0.12%   6  mi_find_page
                                   0.10%   5  _platform_memset
                                   0.06%   3  0x9d90c
```

## 2. Bench numbers (`bench.txt` cited per-entry)

| entry          | ns/iter   | MB/s | post-AU ns | post-AU MB/s | ns factor | MB/s drop |
|----------------|----------:|-----:|-----------:|-------------:|----------:|----------:|
| json           |    37,110 |   14 |      1,892 |          283 |    19.6×  |    20.2×  |
| ebnf           |   148,610 |    9 |      6,490 |          223 |    22.9×  |    24.8×  |
| css_pretty     |    81,493 |   31 |      3,950 |          647 |    20.6×  |    20.9×  |
| google_sheets  |   159,445 |   46 |      8,731 |          858 |    18.3×  |    18.7×  |
| bbnf_self      |   271,066 |   18 |     13,003 |          394 |    20.8×  |    21.9×  |
| css_l4_grammar | 1,925,278 |   28 |    102,451 |          496 |    18.8×  |    17.7×  |

(post-AU values from `docs/benchmarks/post-AU.json`.) Uniform 18–23× regression; no entry escapes.

## 3. Cycles/byte (Apple M3, 3.2 GHz)

| entry          | bytes  | cyc/B |
|----------------|-------:|------:|
| json           |    537 | 221.1 |
| ebnf           |  1,453 | 327.3 |
| css_pretty     |  2,558 | 101.9 |
| google_sheets  |  7,492 |  68.1 |
| bbnf_self      |  5,127 | 169.2 |
| css_l4_grammar | 55,261 | 111.5 |

Smaller grammars pay a stiffer per-byte tax; `ebnf` at 327 cyc/B is the worst. That is the signal-to-noise ceiling of dispatch overhead on small inputs.

## 4. Hot-symbol category aggregation (self-time %)

| category                | json  | ebnf  | css_pretty | google_sheets | bbnf_self | css_l4_grammar |
|-------------------------|------:|------:|-----------:|--------------:|----------:|---------------:|
| dispatch:try_branch     | 74.9  | 69.7  |   71.1     |    68.5       |   71.6    |    69.9        |
| dispatch:advance_or_pop | 11.7  | 16.7  |   11.7     |    12.0       |    9.9    |    12.2        |
| finaliser               |  4.1  |  5.4  |    6.2     |     6.9       |    7.6    |     6.6        |
| libc:memcmp/memmove     |  3.4  |  5.8  |    4.7     |     5.1       |    5.5    |     4.7        |
| regex_scan              |  2.1  |  0.7  |    2.4     |     3.6       |    1.5    |     2.6        |
| cold_state(per-rule-fn) |  0.7  |  0.3  |    1.4     |     1.3       |    0.7    |     0.7        |
| walker_inline           |  0.3  |  —    |    0.8     |     0.9       |    0.4    |     0.4        |
| framestack              |  0.3  |  0.3  |    0.4     |     0.7       |    0.7    |     0.5        |
| syscall/fs              |  1.0  |  1.0  |    0.5     |     0.3       |    1.3    |     1.2        |

**directive_dispatch** and **value_expr** do not appear as separate buckets: the 7-way `@directive` Alt (`DTA_ALT_LIN_485`: states `[478..484]`, `generated.rs:893`) and the 5-entry Pratt table (`DTA_SY_82_PREC`, bytes `+ - * / %`, `generated.rs:194`) are both consumed *through* `try_branch`. `__cold_state_485` (`generated.rs:44050–44130`) iterates `__DTA_ALT_LIN_485` with `for (branch_idx, &branch) in branches.iter().enumerate()` and calls `try_branch(...)` unconditionally per branch — no PHF / ClassifyByte prefilter, no specialised `dispatch_one` fast path.

## 5. BBNF-specific diagnostics

**5a. What dominates `bbnf_self`?** State-dispatch overhead. Dispatch 81.5%, finaliser 7.6%, memcmp 5.5%, regex_scan 1.5% (lowest of six). Directive-dispatch and Pratt are folded into `try_branch`; neither surfaces. Nothing BBNF-specific.

**5b. Is the `@directive` 7-way Alt going through AltLinear + `try_branch`?** Yes. `nm` shows `try_branch`, `advance_or_pop_with`, `advance_or_pop_with.330` (LLVM clone), and monolithic `__regex_scan_BbnfBootstrap` — but **zero** `dispatch_one`, `KEYWORD_PHF`, `CLASSIFY_TABLE`, `STRUCT_ALPHABET`, or `GRAMMAR_PROFILE`-consumer symbols (`nm … | grep -iE 'phf|classify|struct_alpha|GRAMMAR_PROFILE'` returns nothing). The cold state is the "AltLinear + try_branch → dispatch_one cold bridge" pattern minus the dispatch_one — every branch unconditional.

**5c. Is the structural alphabet non-trivial?** **No — empty.** `generated.rs:37–58`:

```rust
pub const GRAMMAR_PROFILE: GrammarProfile = GrammarProfile {
    push_compound_count: 53u16, push_leaf_count: 0u16,
    compounds_per_input_byte: 1f32, …
    structural_alphabet: &[], structural_digraphs: &[],
    structural_digraph_mask: [0,0,0,0], structural_quote_classes: &[],
    active_columns: &[], list_rules: &[], keyword_tables: &[],
    shape_dict: &[], branch_priors: &[],
    dedup_eligible_rules: &[], reorder_unroll_visitors: &[],
};
```

Every mined-profile array is `&[]`. The W1.γ invariant that BBNF's alphabet should be ~17 singletons (`;=|,()[]{}@` plus Pratt/regex delimiters) is not approximately satisfied — the miner writes zero to every slot.

## 6. Per-entry workload-shape comparison

All six inputs traverse the same BBNF hot path (every entry routes through `BbnfBootstrap::parse`). Self-time profile does not change meaningfully: `try_branch` 68.5–74.9%, `advance_or_pop_with.330` 9.8–16.7%, finaliser 4.1–7.6%. `regex_scan` rises with regex-heavy content (CSS L4: 2.6%; google_sheets: 3.6%) but never exceeds 4%. Tiny grammars (`json` 537B, `ebnf` 1.4KB) show outsized per-byte cost from fixed-cost startup. Mid grammars (`css_pretty`, `bbnf_self`) surface per-rule `__cold_state_*` functions in the top-20 — each visit is a non-inlined call boundary. The 55KB aggregate (`css_l4_grammar`) amortises startup but still caps at 28 MB/s because the bottleneck is per-state dispatch, not per-byte. No input stresses a BBNF-specific path differently.

## 7. Cross-entry structural density (BBNF structural set `;=|,()[]{}@`)

| file / group                    | bytes  | struct | density |
|---------------------------------|-------:|-------:|--------:|
| grammar/json/json.bbnf          |    537 |     75 |  14.0%  |
| grammar/ebnf/ebnf.bbnf          |  1,453 |    216 |  14.9%  |
| grammar/css/pretty.bbnf         |  2,558 |    240 |   9.4%  |
| grammar/google-sheets/…         |  7,492 |    381 |   5.1%  |
| grammar/bbnf/ (3 files)         |  5,127 |    509 |   9.9%  |
| grammar/css/l4/ (15 files)      | 55,261 |  3,016 |   5.5%  |

BBNF sources are 5–15% structural — the target density where a populated `structural_alphabet` + SIMD stage-1 should excel. All that potential is wasted because the alphabet is `&[]`.

## 8. One-line conclusions

- **json** — 74.9% `try_branch`, 221 cyc/B, 20× regression; tiny input exposes raw dispatch cost.
- **ebnf** — 327 cyc/B is the worst of the six; small-input penalty amplifies dispatch overhead.
- **css_pretty** — 101.9 cyc/B; per-rule `__cold_state_*` functions climb into the top-20, matching "helpers not inlined".
- **google_sheets** — 68.1 cyc/B, best of the six; amortisation helps, signature unchanged.
- **bbnf_self** — 169.2 cyc/B; BBNF parses BBNF with the same broken hot path it imposes on every other grammar.
- **css_l4_grammar** — 111.5 cyc/B; 55KB corpus with zero structural-scan benefit because `structural_alphabet: &[]`.

## 9. BBNF viability question

**Same helper-call + dispatch-overhead issue as JSON/CSS, amplified by every profile lever being dead.** Evidence: (1) hot-symbol signature (try_branch 68.5–74.9%, advance_or_pop.330 9.8–16.7%, finaliser 4.1–7.6%, memcmp 3.4–5.8%) matches JSON/CSS P1/P2 within noise; (2) `GRAMMAR_PROFILE` has every mined field empty — no lever emitted, no consumer to fire; (3) no BBNF-specific symbol shows hot — `@directive` and Pratt both consume cycles exclusively through `try_branch`; (4) BBNF has no direct competitor, but none is needed — the problem is generic dispatch, not grammar-shape mismatch. A single-point fix (ClassifyByte/PHF/structural-alphabet population + inlining the cross-crate `try_branch`/`advance_or_pop_with` helpers) should recover BBNF in lock-step with JSON/CSS; no BBNF-specific bottleneck is visible above the 1% self-time floor.

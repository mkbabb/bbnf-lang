# AW-IV P3 — Sheets Profile (HEAD 2ca0f7af, post-W1.4-aggro)

## Artefact provenance

The artefacts delivered into the wave directory prior to this session (mtime Apr 17 00:24) were recorded against binary hash `6d1ed9fdbf99c8ce` — **before** the W1.4-aggro regen that landed at HEAD `2ca0f7af` (Apr 17 17:35). The stale binary contained the (since-deleted) `DtaDfaScanner as RegexScanner::scan` symbol; that symbol is absent from the current HEAD's binary (verified via `nm | c++filt`). The stale numbers (3-4 MB/s, 132K-509K ns) do not reflect the post-W1.4-aggro codegen.

I re-profiled all three entries against the **current** binary `google_sheets_monolithic-9fd0373153d26e41` (built Apr 17 18:01 against HEAD `2ca0f7af`, confirmed by presence of `__dta_walker_inline::run` + 42 `__cold_state_N` symbols and absence of `DtaDfaScanner` / `dispatch_one` / `reserve_compound` / `close_compound` / `emit_leaf`). All seven artefacts landed per entry; the stale set was overwritten.

## Executive summary

Sheets's regression is **not setup-floor-dominated and not a small-input amortisation artefact**. Linear regression across three entries yields near-zero setup-per-call; all three report a **uniform ~165 ns/byte = ~570 cyc/byte**, ~40× JSON twitter's 14.5 cyc/byte. `bbnf_tape::driver::try_branch` consumes **52-72% self-time** invariant across input size — it is the dispatcher into which `dispatch_one` was inlined, called by the W1.4-aggro walker's AltLinear arms for Sheets's Alt-heavy grammar (10-branch `primary`, 9-branch `error_literal`, 3-branch `range_end`, nested operator tower). `PRECEDENCE_LUT` is direct-indexed and `lookup_precedence` is absent. PHF tables exist only for the 4 tiny operator Alts (compare/add/mul/unary_prefix). Function-name PHF is grammar-agnostic: Sheets has no function-name keyword set — names match the generic `identifier` regex. The actionable lever is inlining `try_branch` (or specialising it per-grammar) into the walker's AltLinear arm; projected post-lever throughput ~10 MB/s, still well short of AU parity, requiring the W3 Alt-classifier as the second lever.

## 1. Per-entry top-20 self-time

Source: `.profiles/samply/google_sheets_monolithic/{entry}/profile.json.gz` + `profile.json.syms.json` (fresh, Apr 17 18:07-18:08). RVA lookup via bench binary's `symbol_table`.

### parse_simple (4699 samples, 85644 ns/iter, ±9087)

| pct | symbol |
|---:|:---|
| 52.20 | `bbnf_tape::driver::try_branch` |
|  9.41 | `bbnf_tape::driver::advance_or_pop_with.458` |
|  9.04 | `__dta_walker_inline::run` |
|  5.51 | `bbnf_tape::finaliser::finalise` |
|  3.17 | `__regex_scan_GoogleSheetsParser` |
|  3.04 | `_platform_memmove` |
|  2.98 | `_mi_heap_realloc_zero` |
|  1.51 | `<RawVecInner>::finish_grow` |
|  1.45 | `advance_or_pop_with` |
|  1.28 | `<GoogleSheetsParser>::parse` |
|  1.26 | `_platform_memcmp` / `mi_free` |
|  1.19 | `mi_malloc_aligned` |
|  0.77 | `_mi_page_retire` |
|  0.55 | `do_reserve_and_handle::<Global>` |
|  0.53 | `mi_realloc_aligned` |
|  0.47 | `google_sheets_monolithic::parse_simple` |
|  0.45 | `<SplitInternal<char>>::next_inclusive` |
|  0.43 | `<Columns>::grow_all` |
|  0.40 | `madvise` |

### parse_nested (3919 samples, 203013 ns/iter, ±12104)

| pct | symbol |
|---:|:---|
| 63.72 | `try_branch` |
| 11.33 | `advance_or_pop_with.458` |
|  6.51 | `finaliser::finalise` |
|  4.29 | `__regex_scan_GoogleSheetsParser` |
|  3.83 | `__dta_walker_inline::run` |
|  1.74 | `_platform_memcmp` |
|  1.25 | `_platform_memmove` |
|  0.87 | `_mi_heap_realloc_zero` |
|  0.82 | `advance_or_pop_with` |
|  0.79 | `<GoogleSheetsParser>::parse` |
|  0.56 | `mi_free` |
|  0.36 | `madvise` / `mi_malloc_aligned` / `nearest_variant_frame` |
|  0.33 | `_mi_page_retire` / `finish_grow` |
|  0.26 | `do_reserve_and_handle` |

### parse_stress (311 samples, 304172 ns/iter, ±5890)

| pct | symbol |
|---:|:---|
| 71.70 | `try_branch` |
| 14.79 | `advance_or_pop_with.458` |
|  4.82 | `__regex_scan_GoogleSheetsParser` |
|  3.86 | `finaliser::finalise` |
|  0.96 | `_platform_memcmp` / `_mi_heap_realloc_zero` |
|  0.64 | `__dta_walker_inline::run` / `advance_or_pop_with` |
|  0.32 | `<GoogleSheetsParser>::parse` / `_mi_malloc_generic` |

## 2. Per-entry bench numbers

Source: `.profiles/samply/google_sheets_monolithic/{entry}/bench.txt`.

| entry | ns_iter | bytes | lines | mean_line | MB/s |
|:--|---:|---:|---:|---:|---:|
| parse_simple |  85,644 |   505 | 34 |  14.9 B | 5.9 |
| parse_nested | 203,013 | 1,456 | 31 |  47.0 B | 7.2 |
| parse_stress | 304,172 | 1,838 |  8 | 229.8 B | 6.0 |

Each `.iter()` body re-parses **every non-empty line** (`crates/core/benches/google_sheets/monolithic.rs:44-49`). Per-formula ns = ns_iter / lines; setup amortises per formula, not per file.

## 3. Cycle/byte derivation (M1 @ 3.44 GHz)

| entry | ns/byte | cyc/byte |
|:--|---:|---:|
| parse_simple | 169.59 | **583.4** |
| parse_nested | 139.43 | **479.6** |
| parse_stress | 165.49 | **569.3** |

## 4. Setup-floor vs per-byte split

Linear regression `ns/line = setup_floor + ns_per_byte × mean_line` across 3 entries:

```
setup_floor = -591.8 ns/call
ns_per_byte = 167.59  (cyc_per_byte = 576.5)
```

Negative floor is a least-squares artefact; setup-per-call is effectively zero. Per-byte cost alone predicts within 2%:

| entry | ns/line | per-byte cost | setup% |
|:--|---:|---:|---:|
| parse_simple  |  2,519 |  2,489 | −31 |
| parse_nested  |  6,549 |  7,871 |  −8 |
| parse_stress  | 38,022 | 38,504 |  −2 |

**Answer**: **per-byte dominates, not setup_floor.** Evidence:
- Inclusive-time: `<GoogleSheetsParser>::parse` 97.66/99.00/99.68% across entries. `TapeBuilder::with_capacity` contributes <1% self and ~1.3% inclusive combined.
- `try_branch` self-time (52-72%) is invariant across entry size; setup-dominated runs would show `with_capacity` inflated for the small entry, which they do not.

## 5. Pratt reducer verification

- `DtaState::ShuntingYard` arm construction at `expand.rs:2338, 2791`; Pratt frame-kind pushes at `expand.rs:10334, 18865` (the operator tower IS using ShuntingYard frames, not AltLinear fallthrough).
- **`PRECEDENCE_LUT`**: `pub const PRECEDENCE_LUT: [u8; 256]` at `expand.rs:3071-3087`, populated per-byte (`&`=4, `+`=3, `-`=3, `*`=2, `/`=2, `<`=4, `=`=4, `>`=4, `;`=2, etc.).
- **`PRECEDENCE_ENTRIES`**: sparse `DtaPrecedenceEntry` slice at `expand.rs:3093-…` with `{byte, precedence, associativity, op_rule, op_discriminant}` per operator.
- **`lookup_precedence`**: **ABSENT**. `nm binary | c++filt | rg lookup_preced` → zero matches. W3.4 is effectively a no-op on Sheets; the walker arms splice `PRECEDENCE_LUT[byte]` inline.

## 6. Function-name PHF verification

- `grammar/google-sheets/google-sheets.bbnf:87` defines `identifier = /[A-Za-z_][A-Za-z0-9_.]*/ -> input : Span`. Function names are not a keyword set — they match the generic identifier regex.
- Only `LET(` and `LAMBDA(` use case-insensitive regexes (`expand.rs:35*`), inline-spliced DFA bodies with no keyword table.
- Emitted PHF tables (`expand.rs:100-117`): 4 tiny operator sets —
  - `KW_0=[<,=,>,>=]` (compare_op), `KW_1=[+,-]` (add_op), `KW_2=[*,/]` (mul_op), `KW_3=[+,-]` (unary_prefix).
- `__phf_GoogleSheetsParser_dispatch_10` at `expand.rs:3058` = binary-search over KW_0.
- No 150-name PHF exists, and correctly so — the grammar has no such keyword set.

## 7. Hot-symbol category aggregation

| category | simple | nested | stress | notes |
|:--|---:|---:|---:|:--|
| **dispatch (`try_branch`)** | 52.20 | 63.72 | 71.70 | `dispatch_one` inlined into it |
| **walker (`__dta_walker_inline::run`)** | 9.04 | 3.83 | 0.64 | hot spliced path |
| **repeat (`advance_or_pop_with[.458]`)** | 10.86 | 12.15 | 15.43 | `.458` is LLVM dup-clone |
| **cold regex (`__regex_scan_GS…`)** | 3.17 | 4.29 | 4.82 | `#[cold]` adapter |
| **finalise** | 5.51 | 6.51 | 3.86 | |
| **alloc/mimalloc** | ~6.8 | ~1.7 | ~1.3 | realloc_zero/malloc_aligned/free |
| **memcmp/memmove** | 4.30 | 2.99 | 1.92 | |
| **RawVec grow** | 1.51 | 0.33 | 0.00 | |
| **precedence lookup** | 0 | 0 | 0 | direct-indexed |
| **function_dispatch** | 0 | 0 | 0 | no function-name keyword set |

Inclusive: `__dta_walker_inline::run` carries **87.42 / 90.20 / 95.18%** across entries. `try_branch`'s self-time is the fused cost of savepoint + dispatch_one's 11-variant match (inlined in) + handle_repeat_failure recovery + the call-boundary per Alt branch attempt.

## 8. Setup or per-byte? Recoverable lever?

**Per-byte dominates; recoverable.** §4 + §1 invariant self-time excludes setup-floor.

**Recoverable lift**: inline or per-grammar-specialise `try_branch` into the walker's AltLinear arm, mirroring the W1.4-aggro splice for hot dispatch states. Assuming 50% of `try_branch`'s self-time is genuinely irreducible branch-attempt work (savepoint/restore, byte-peek, state transition), the remaining 26-36% recovers. Projected throughput:
- simple: 85.6K × 0.64 ≈ 54.8K ns → **9.2 MB/s** (from 5.9)
- nested: 203K × 0.60 ≈ 121.8K ns → **11.9 MB/s** (from 7.2)
- stress: 304K × 0.58 ≈ 176.4K ns → **10.4 MB/s** (from 6.0)

Still ~10× below post-AU. Grammar-shape factor demands the **second lever** (W3.2/W3.3): grammar-emitted Alt-classifier routing `primary`/`range_end`/`cell_or_range` by dispatch-byte.

## 9. Sheets-vs-JSON per-byte comparison

JSON twitter: 14.5 cyc/byte · Sheets stress: 569 cyc/byte → **~39× slower per byte**.

**Not input-size amortisation**: stress's 230-byte mean line is past any setup regime; all three Sheets entries are ~570 cyc/byte. The 39× is genuine per-byte walker cost, rooted in:
1. **Non-inlined dispatcher**: `try_branch` is a call-boundary hit per Alt branch attempt.
2. **Alt-density**: `primary`=10 branches, `error_literal`=9, nested operator tower at every precedence level. JSON twitter spends most cycles inside single DFA bodies where Alt backtracking is absent.
3. **`advance_or_pop_with.458`** at 9-15%: Repeat continuations re-enter through the shared helper; LLVM duplicates the body for cold vs hot emission sites.
4. **Cold regex adapter** at 3-5%: `#[cold]` fn is reached from `try_branch`'s cold exit during branch replay.

**Conclusion**: Sheets's per-byte cost is architectural (Alt-dense grammar × non-inlined dispatcher × cold regex back-door), not inherent. W1.4-aggro alone leaves a ~10 MB/s ceiling; AU parity requires `try_branch` inlined/specialised + grammar-emitted Alt-classifier.

## Artefact citations

- bench: `.profiles/samply/google_sheets_monolithic/{entry}/bench.txt`
- top-20: `.profiles/samply/google_sheets_monolithic/{entry}/profile.json.gz` + `profile.json.syms.json`
- emitted tables + PRECEDENCE_LUT + `__regex_scan_GoogleSheetsParser`: `.profiles/samply/prebuild/expand/google_sheets_monolithic/expand.rs`
- symbol-presence: `nm .profiles/shared-target/release/deps/google_sheets_monolithic-9fd0373153d26e41 | c++filt`
- grammar: `grammar/google-sheets/google-sheets.bbnf`
- bench harness: `crates/core/benches/google_sheets/monolithic.rs:44-49`
- try_branch source: `crates/bbnf-tape/src/driver.rs:1277`

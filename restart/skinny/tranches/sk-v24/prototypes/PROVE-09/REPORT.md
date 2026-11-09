# PROVE-09 — parse-that DE-NIGHTLY + arena (F5)

Host aarch64-apple-darwin (Apple M), Darwin 25.4.0. Scratch copy of
`/Users/mkbabb/Programming/parse-that` per R3 §8. `CARGO_TARGET_DIR` inside the
copy. Baseline captured on the pristine copy FIRST. NO x86.

**VERDICT: PARTIAL.** The `portable_simd` + `smallbox` de-nightly of the
hand-written combinator core is measured, complete, and **not merely
cyc/B-neutral but 1.34–1.56× FASTER**; every file I touched compiles on stable
1.96.1 with zero errors. Two gate clauses remain unmet, each with its exact
bounded fix: (1) full `cargo +stable` build has ONE residual `#![feature]`
forced NOT by crate source but by an `@generated` file; (2) no alloc-count
*reduction* — the de-nightly is alloc-*neutral*, and the bump-slab arena for the
vtable is blocked by the `'a` input-lifetime over 86 constructor sites (carried
honestly, not stubbed).

---

## Built (patch.diff — 12 files, +358/−14)

| File | Change |
|---|---|
| `src/simd128.rs` (NEW 276 L) | Stable 128-bit byte-lane SIMD shim. aarch64 → `core::arch::aarch64` NEON; other arches → `[u8;16]` scalar parity oracle. Exposes the exact `std::simd` vocabulary the scanners use (`splat/from_slice/from_array/to_array`, `& | >>`, `simd_eq/simd_ne`, `to_bitmask/any/all`, `swizzle_dyn`). `to_bitmask` == `Mask::to_bitmask` (lane i → bit i) via `vand`(bit-positions)+`vaddv` halves. |
| 7 scanner files | one-line `use std::simd::prelude::*` → `use crate::simd128::*`; ZERO call-site churn. |
| `parse_that/Cargo.toml` | `smallbox {"0.8",nightly}` → `smallbox = "0.8"`. |
| `src/parse.rs` | `SmallBox::new` → `smallbox!` — stable unsizing, identical S32 inline/heap behavior. |
| `src/lib.rs` | Excised `portable_simd` + `stmt_expr_attributes`; retained `fmt_helpers_for_derive` (the @generated residual). |
| `examples/alloc_count.rs` (NEW) | counting-`GlobalAlloc` probe. |

**Nightly surface (R3 §2), itemized:** `portable_simd` (8 files/~88 hits) →
EXCISED via the shim (every hit was one uniform `u8x16` vocabulary; `state.rs`
"hits" were comments; `swizzle`==`swizzle_dyn`→`vqtbl1q_u8`). `smallbox[nightly]`
→ EXCISED. `stmt_expr_attributes` → EXCISED (dead). `fmt_helpers_for_derive` →
**RESIDUAL**, forced by `src/regex/generated.rs` (`@generated` from
`bootstrap/regex.bbnf`) which emits the *expanded* `#[derive(Debug)]` — an
`#[automatically_derived] impl` that calls the perma-unstable
`core::fmt::Formatter::debug_tuple_field1_finish`. Per the clean-regen edict this
is a GENERATOR fix (emit `#[derive(Debug)]`, not its expansion) +
`scripts/bootstrap-regex.sh`, never a hand-patch. Proof it is the SOLE residual:
on stable 1.96.1 all 20–21 errors are in `generated.rs`; my 12 files: zero.

---

## Measured — cold divan, N=100/corpus

Both runs on pinned **nightly-2026-04-11** (same toolchain, same `.cargo` `-Z`
flags, `[profile.bench]` set to `lto=false/cgu=16/O3` IDENTICALLY per A3 D1),
`#[global_allocator]=mimalloc`. Comparison isolates the `portable_simd`→NEON
port (smallbox/gate edits are construction/compile-time). Ratio = baseline/modified
median (>1 = modified faster). loadavg 10–20 the whole window (busy host).

| corpus | lane | N | loadavg | baseline med | modified med | ratio | min-col corroboration |
|---|---|---|---|---|---|---|---|
| apache-builds | combinator | 100 | ~14–16 | 136.7 µs | 102.2 µs | **1.34×** | 132.8→100 µs |
| canada | combinator | 100 | ~14–16 | 3.962 ms | 2.531 ms | **1.57×** | 3.679→2.382 ms |
| citm_catalog | combinator | 100 | ~14–16 | 1.629 ms | 1.057 ms | **1.54×** | 1.363→1.000 ms |
| data | combinator | 100 | ~14–16 | 41.12 µs | 27.31 µs | **1.51×** | 40.2→26.5 µs |
| data_xl | combinator | 100 | ~14–16 | 40.17 ms | 28.30 ms | **1.42×** | 34.5→26.3 ms |
| twitter | combinator | 100 | ~14–16 | 735.1 µs | 470.2 µs | **1.56×** | 579→441 µs |

Metric: parse-that's harness is divan wall-time + BytesCount (not the skinny PMU
bin); at fixed clock/one machine the throughput ratio is the cyc/B ratio.
Absolutes directional, ratios certified. The min-of-N (least-preempted) column
corroborates the median gap, so it is not load noise. **The magnitude (>1.3×)
most plausibly reflects `portable_simd`'s suboptimal aarch64 `to_bitmask`/
`swizzle_dyn` lowering on this pinned OLD nightly vs the tight hand-NEON**
(`addv.8b`+`and.16b` movemask, verified below); a re-verify fleet should confirm.
The GATE — "cyc/B not worse than nightly baseline" — is met with wide margin.

**Alloc profile (counting allocator):**
- construct `json_parser()`: **6 allocs / 5 960 B** (yyjson-tier; vtable boxing is trivial + construction-time).
- parse twitter.json (631 514 B): **5 390 allocs** (Vec growth in many/sep_by + string arena — the real alloc mass, unaffected by this change).

The de-nightly is **alloc-neutral** (`smallbox!`≡`SmallBox::new`: same inline≤S32
/ heap>S32). The probe proves the vtable boxes are a construction-time cost paid
once outside the timed loop — which is why the port is cyc/B-safe and why an
arena would move construction allocs only (6), never the measured per-parse
number (5 390). No alloc REDUCTION delivered → that gate sub-clause UNMET.

**Tests:** `cargo test -p parse_that` (nightly) EXIT 0, 0 failed suites (~400 per R3 §7).

---

## Visual verification (hot NEON path)

`otool -tv` of the built combinator bench binary, hot classify functions:
```
100051f8  addv.8b  b0, v0     ; simd128 to_bitmask: vaddv_u8 of a movemask half
10005234  addv.8b  b0, v0     ; (9× across classify_stripe / find_next_structural / ws scan)
...
10012a34  and.16b  v2, v3, v2 ; vandq_u8 — nibble-LUT AND / bit-position mask
```
The shim lowers to straight NEON (`addv.8b`, `and.16b`, plus `cmeq/tbl/ushl`) —
no `std::simd` runtime artifacts, no panics on the hot path.

---

## Why the bump-slab arena was NOT the vtable substitute (carried, not faked)

`Parser<'a,O>` OWNS its `parser_fn` (SmallBox moves+drops with the Parser), and
`ParserFn<'a>` is parameterized by the INPUT lifetime `'a`. A shared bump-slab
(the tree already ships `BumpSlab`, used by the regex generated parser at
`regex/generated.rs:174`) would own every closure and return `&'arena dyn
ParserFn`; that arena must be `'a`-scoped and threaded through ALL 86
`Parser::new` sites plus the whole `Parser` type — a large, lifetime-invasive
rewrite that cannot be made sound under the cap (a `'static`/thread-local arena
holding `'a`-parameterized closures is unsound). `smallbox!` reaches the arena's
GOAL (drop the nightly allocator dep, keep inline storage, zero perf change).
The alloc reduction an arena promises targets construction heap boxes only (just
6 on JSON) — it would not move the per-parse cyc/B. The arena stays the right F5
move as a tranche-scoped Parser-ownership refactor, not a prover-cap edit.

## Integration cost

`simd128.rs` is one colocated module; 7 scanners import it by one `use` line —
directly upstreamable. Stable cutover needs ONE further change outside
parse-that scope: regenerate `regex/generated.rs` from a codegen emitting
`#[derive(Debug)]` instead of its expansion (bounded, mechanical). No new deps,
no x86. NB the scratch dir was shared/clobbered by a concurrent PROVE-10 session
(its `regex/prefilter.rs` WIP + git-repo takeover); patch.diff is scoped to my
12 files only; `src-snapshot/` backs up the new files.

# LOCK-9 · Family 9B — NATIVE-REBIRTH: parse-that keeps its own kernels

parse-that retains its own scan/kernel layer, rewritten in idiomatic stable `core::arch::aarch64`
NEON, each kernel bound to a scalar parity oracle and a checkasm-liveness test; no cross-repo
coupling to `bbnf-simd`; the gate is measured no-regression under paired interleaving, honoring the
refutation that the de-nightly buys STABILITY, not speed. This report makes that mechanism concrete,
names its substrate at `path:line`, and states exactly what a prototype must prove.

## 1. The mechanism — the movemask contract-split is the whole family

The scanners share one narrow `u8x16` vocabulary (`splat`/`from_slice`/`simd_eq`/`simd_ne`/`&`/`|`/
`>>`/`swizzle_dyn`/`to_bitmask`). Every hot loop terminates in one reduction: pack 16 comparison
lanes into a bitmask. That producer is the ALU-bound critical path (SPEC §0 refutation 04: "classify
is ALU-bound on the movemask bit-stitch… the F1 lever is the MOVEMASK NARROW + vectorized compaction,
not block scheduling"). 9B's rebirth is therefore, first and last, authoring the CANONICAL producer
in two forms and purging the refuted idiom from every site parse-that owns.

**The refuted idiom (double-addv) — present at four live sites, all must die:**
- `parse_that/src/simd128.rs:322` `mask8x16::to_bitmask` — `vand(BITS)` + `vaddv_u8(vget_low)` +
  `vaddv_u8(vget_high) << 8`. This is the exact form the verify fleet refuted: paired rebuild emitted
  **2271 `addv.8b` vs baseline 509** for the same masks — the shim does *more* reduction work, which
  is why the claimed 1.34–1.57× is a two-arm window-asymmetry mirage, not a property of the port.
- `parse_that/src/parsers/scan/number_simd.rs:53-57` `count_leading_digits_neon` — `lo_sum`/`hi_sum`
  double-`vaddv`. Additionally a **digit-scan SIMD kernel**, which SPEC §0 refutation 02 makes NO_GO
  (−37/−48% ins but CPI 0.13→0.30 latency inversion). This kernel is abrogated, not ported.
- `bbnf-simd/src/aarch64/structural_classify_64.rs:147` and `comment_body_mask_64.rs:68` carry local
  double-`vaddv` `movemask_u8x16` copies — evidence the refuted idiom already fragmented across homes;
  9B does not consume them (no coupling) but they document the drift hazard §5 names.

**The canonical LEAN producer — `vshrn_n_u16::<4>` narrow.** For a single 16-byte membership stripe,
compare→`vshrn_n_u16::<4>` narrows each 16-bit lane-pair to a nibble, giving a 64-bit register whose
bit `4i` carries lane `i`'s high bit; one `fmov` + `and #0x8888888888888888` extracts the strided
mask. This is SPEC §0 feature 03 **CONFIRMED** ("Fused NEON string scan (vshrn narrow)": twitter
parse_tape 0.898–0.931× cyc/B, ins/B −9/−12% deterministic, won in both interleave slots) and is the
textbook lowering `rust`'s own `core::simd::Mask::to_bitmask` emits on aarch64 (`shrn.8b v0,v0,#4` —
visible in the regex-automata memchr NEON hot loop that PROVE-10 disassembled: `cmeq.16b; shrn.8b
v0,v0,#4; fmov x8,d0; ands x8,#0x8888…`). The canonical `bbnf-simd/src/aarch64/movemask.rs`
`movemask_u8x16` reaches for `vshrn_n_u16::<4>` but then does a scalar-loop reassembly (lines 18-24)
to restore lane-`i`=bit-`i` ordering — a smell 9B avoids by keeping the strided convention through the
CTZ iterator (see §5).

**The canonical DENSE-64 producer — the `vpaddq_u8` tree.** For a 64-byte structural block (four
`u8x16` stripes ORed into a `u64`), the amortized-cheapest pack is simdjson's `neonmovemask_bulk`:
`vandq_u8(pi, bitmask)` on all four, then `vpaddq_u8(t0,t1)`, `vpaddq_u8(t2,t3)`, `vpaddq_u8(sum0,
sum1)`, `vpaddq_u8(sum,sum)` → `vgetq_lane_u64` — **4 AND + 4 pairwise-add for the whole 64 bytes**,
versus the refuted `4 × (vand + 2 vaddv)` = 8 reduction ops. This is the "DENSE dense-64 vpaddq tree".

## 2. Substrate — the eight scanner files and the one shim

parse-that owns (`/Users/mkbabb/Programming/parse-that/rust/parse_that/src/`, READ-ONLY):
`scanners.rs` (22 SIMD hits), `parsers/scan/decode.rs` (17, JSON string decode-to-arena),
`parsers/scan/ws_comment.rs` (16), `parsers/scan/structural_bitmap.rs` (13, the simdjson bitmap),
`parsers/scan/quoted_simd.rs` (7, `odd_parity_backslashes`), `span_parser/scan.rs` (7),
`parsers/scan/quote_parity.rs` (4, `classify_stripe_64`), `state.rs` (2, `PaddedView`). The
de-nightly shim `simd128.rs` (276 LOC) already exposes the exact `std::simd` vocabulary via
`core::arch::aarch64`, so each scanner ports by one `use std::simd::prelude::*` → `use
crate::simd128::*` line with zero call-site churn (7 files, one line each). 9B keeps this
substrate and corrects only its `to_bitmask` producer. The PaddedView invariant
(`state.rs:57 INPUT_PAD_BYTES=64`; `:122 PaddedView`, backing `len+64` trailing-NUL) lets every stripe
load skip a tail guard and is the parse-that-native padding convention (contrast §4).

**Scanner-file census (kernel-tier | vector-tier | no-go):** `structural_bitmap.rs`,
`quote_parity.rs`, `quoted_simd.rs`, `scanners.rs` (nibble-LUT find) are **kernel-tier** (own a
movemask producer, must adopt the LEAN/DENSE split). `decode.rs`, `ws_comment.rs`, `span_parser/
scan.rs` are **vector-tier** (consume `u8x16` compares + `store_chunk`, no independent movemask —
inherit the corrected producer transitively). `number_simd.rs` is **no-go** (digit-scan, refutation 02
→ abrogate to the scalar SWAR `parse_eight_digits` path at `:78`, which is already the accumulator).

## 3. Integration seams — where 9B code attaches

- `parsers/scan/structural_bitmap.rs:36` `classify_16` → `.to_bitmask() as u16` — the **LEAN site**;
  swap to `vshrn_n_u16::<4>` narrow. Downstream `:51` `classify_stripe` stitches `4 × (m << k*16)` —
  the **DENSE-64 site**; replace the 4 independent `to_bitmask` calls with one `vpaddq_u8` tree over
  the 4 stripe masks. `find_next_structural_from:~120` CTZ-iterates the `u64`; if the LEAN producer
  returns a nibble-strided mask, the CTZ stride/`&0x8888…` mask must be threaded to it (§5 risk).
- `src/simd128.rs:322` `to_bitmask` — the refuted producer; the single point of correction that
  fixes every vector-tier consumer at once (decode/ws_comment/span_parser inherit it).
- `parsers/scan/quote_parity.rs:41` `prefix_xor` — **x86 purge site**: the `pclmulqdq` arm (`:42-49`,
  `_mm_clmulepi64_si128`) is deleted (constitution iv, x86 dead); the aarch64 `vmull_p64`
  (`:53-55`, `target_feature="aes"`) and 5-shift-xor scalar fallback (`:58-68`) are retained.
- `parsers/scan/number_simd.rs:29` `count_leading_digits_neon` — abrogate (§2 no-go).
- `parse.rs:74` `smallbox!(parser_fn)` — the smallbox nightly→stable cut: blast radius is exactly
  **2 lines** (`Cargo.toml` feature drop `smallbox="0.8"` + `SmallBox::new`→`smallbox!` macro),
  alloc-neutral (`smallbox!` ≡ `SmallBox::new`, same S32 inline/heap threshold) and codegen-neutral.

## 4. F1 — two-home vs one-home residual divergence (9B is the two-home answer)

Authoring the canonical LEAN+DENSE producers once, the residual substrate divergence between keeping
them in parse-that (two homes) vs sharing `bbnf-simd` (one home) is small and concretely enumerable:

| Axis | parse-that (own) | bbnf-simd | divergence cost |
|---|---|---|---|
| padded view | `PaddedView`, `INPUT_PAD_BYTES=64` trailing-NUL (`state.rs:57`) | `eob_pad_clamp`/`EobBlock` clamp (`scalar/eob_pad_clamp.rs`, `aarch64/eob_pad_clamp.rs`) | one clamp convention each; reconciling threads the `PaddedView` witness through bbnf-simd's `&[u8;64]` block API |
| shuffle | 2-table nibble `swizzle_dyn` (`vqtbl1q_u8`), lo/hi LUT (`structural_bitmap.rs:36`) | 4-register `vqtbl4q_u8` over 64-entry low-6-bit table (`aarch64/classify_tbl4.rs:31`) | multi-table (dense, distinct-class) vs 2-table (membership); genuinely different fit |
| capacity | `STRIPE=64`, `CHUNK=16`, `MASK16=0xFFFF` | 64-byte block, `[u8;64]` fixed | constant-only |
| movemask | (post-9B) LEAN `vshrn`/DENSE `vpaddq` in `simd128.rs` | canonical `movemask.rs::movemask_u8x16` (+ 2 divergent local copies) | ~30 LOC × 2 producers duplicated across homes |
| oracle | (owed) native checkasm harness | `tests/checkasm_*.rs` (17 files) | one harness dir duplicated |

**Two-home basis (9B):** the divergence is deliberate — parse-that's `u8x16` LEAN membership stripe
serves combinator restore/CTZ semantics; bbnf-simd's `vqtbl4q_u8` DENSE serves the 64-byte structural
*index* (`StructuralIndex`, positions Vec). Sharing forces a lowest-common-denominator block API and
imports bbnf-simd's `blake3` parity-hash dep (`bbnf-simd/Cargo.toml`) into parse-that's dep graph.
Maintenance basis: two ~30-LOC producers + one checkasm dir is the drift surface; it is bounded and
each side's oracle pins its own contract, so drift is *detectable* rather than *silent* (the two
existing bbnf-simd local double-addv copies show that even one home already drifts without a lint).

**Where `#[inline]` fails to cross the seam (F1):** across a `codegen-units>1` non-LTO rlib boundary,
a `#[inline]` (not `#[inline(always)]`) NEON kernel *may* not be imported into the caller's CGU — a
non-inlinable boundary appears as a `bl` to the kernel symbol in the caller's disasm. `bbnf-simd`
compiles as its own rlib; parse-that's `[profile.bench]` is fat-LTO/cgu=1 (`rust/Cargo.toml:10`) which
dissolves the seam, but `[profile.dev]`/`release` (thin/none LTO) would expose it. This is precisely
why 9B keeps kernels *in-crate* (`simd128.rs`, same CGU as the scanners) — the boundary never forms.

## 5. F2 — stable toolchain + the differential oracle

**Build on stable.** Every scanner + the shim compiles on stable 1.96.1 (`core::arch::aarch64`,
`smallbox` de-featured) with zero errors in the 12 touched files. The SOLE residual `#![feature]` is
`fmt_helpers_for_derive`, forced NOT by hand-written source but by the `@generated`
`regex/generated.rs`, which emits the *expanded* `#[derive(Debug)]` (an `#[automatically_derived]`
impl calling perma-unstable `core::fmt::Formatter::debug_tuple_field1_finish`). Per the clean-regen
edict this is a GENERATOR fix — emit `#[derive(Debug)]`, not its expansion, via
`scripts/bootstrap-regex.sh` — never a hand-patch. The `smallbox` cut blast radius is 2 lines (§3).

**The differential oracle (template exists in bbnf-simd, replicated natively for 9B).** Per kernel:
a scalar `reference(&[u8;64]) -> u64` beside the NEON `candidate`, asserted equal over (a) an
**alignment sweep** `for align in 0..64` of a 128-byte backing (`checkasm_structural_terminator_64.rs:
40-48`), and (b) **4096 random JSON-ish blocks** from a fixed `Xorshift64` (`checkasm_common.rs:4`,
seed-pinned; pool `b"{}[],:\"\\… a-z0-9"`). Liveness/OOB is caught by `guarded_call`
(`checkasm_common.rs:33`) — a 1 KB stack canary xor-folded pre/post that panics on any clobber (a bad
`vst1q`/`tbl` OOB store trips it), the checkasm/dav1d/FFmpeg differential-asm discipline. INJECT_BUG:
perturb one output bit; the alignment sweep + random corpus must catch it (a single flipped lane
appears in ≥1 of 4096 random blocks with overwhelming probability). `black_box` both inputs and the
canary so the compiler cannot fold the reference into the candidate.

**"Does a naive nibble-stride on the dense-64 corrupt parity?" — YES, concretely.** `vshrn_n_u16::<4>`
yields a **nibble-strided** mask: lane `i`'s bit lands at position `4i`, not `i`. `classify_16`
(`structural_bitmap.rs:36`) contractually returns lane-`i`=bit-`i` `u16`, and `classify_stripe` packs
`m << (k*16)` assuming that. Feeding a raw `vshrn` result into that `<< k*16` stitch permutes every
position; `find_next_structural_from`'s CTZ then emits offsets at `4×` the true stride → the emitted
structural index is garbage and the parse diverges. The correction is either (a) apply `& 0x8888…` +
`vshrn`-aware compaction to restore lane-`i`=bit-`i` before the stitch, or (b) migrate the CTZ
iterator to consume the strided form directly (mask `0x8888…`, step by 4). The oracle in §5(a-b) is
exactly what surfaces this — the canonical `movemask.rs` needing a scalar reassembly loop is the same
hazard already visible in-tree.

## 6. Prior art (citations)

- Langdale & Lemire, *Parsing Gigabytes of JSON per Second* (VLDB Journal 28, 2019) — the DENSE-64
  `neonmovemask_bulk` `vpaddq_u8` tree and the structural-bitmap model `structural_bitmap.rs` mirrors.
- Geoff Langdale, "Bits to indices" / "Vector'd… movemask" (branchfree.org) — NEON has no
  `pmovmskb`; `vshrn #4` narrow vs `vpaddq` tree are the two canonical emulations, split by run length.
- Lemire, "Iterating over set bits quickly" — `rbit`+`clz` first-set extraction for the CTZ iterator.
- `regex-automata` / `memchr` aarch64 backend — `cmeq;shrn.8b #4;fmov;ands #0x8888…` is the shipped
  LEAN movemask (PROVE-10 disassembled it inside `Dfa::find`; feature 10 CONFIRMED 8.8–29.5×).
- `rust` `core::simd::Mask::<i8,16>::to_bitmask` — LLVM lowers to `shrn.8b #4`; the stable
  `core::arch` port must reproduce that stream, not the double-`vaddv` the shim wrote.
- checkasm (FFmpeg/dav1d differential-asm harness) — the `guarded_call` stack-canary + reference/
  candidate pattern is that discipline; the name origin.

## 7. Named risks / counterexamples

1. **The speed mirage.** The 1.34–1.57× is refuted (two-arm window asymmetry; shim emits 2× the
   movemask reduces). 9B must claim STABILITY only; the gate is paired-interleaved cyc/B ≥ parity.
2. **Nibble-stride parity corruption** (§5) — the dominant correctness trap; the whole family stands
   on the oracle catching it.
3. **Digit-scan relapse** — porting `number_simd.rs` instead of abrogating it re-introduces
   refutation 02; keep scalar SWAR.
4. **`#[inline]` seam** (§4) — only in-crate kernels are safe under non-LTO profiles; a shared home
   would need `#[inline(always)]` audited on every kernel and disasm-checked for `bl` to the symbol.
5. **`vmull_p64` gate** — quote_parity's prefix-XOR needs `target_feature="aes"`; on a base M-class
   `target-cpu` this must be enabled or the scalar 5-shift fallback silently taken (measure which).

## 8. What a prototype must test to move 9B

1. **LEAN producer swap:** replace `simd128.rs:322 to_bitmask` (double-addv) with `vshrn_n_u16::<4>`
   narrow, thread the `&0x8888…`/stride convention to the `structural_bitmap` CTZ iterator; measure
   paired-interleaved cold cyc/B on twitter/citm/canada/data_xl; GATE = ≥ parity (no-regression);
   `otool -tv` shows `shrn.8b #4`, not `2× addv.8b`; reduction-op count strictly down.
2. **DENSE-64 producer:** replace `classify_stripe`'s 4× `to_bitmask` with the `vpaddq_u8` tree; the
   `u64` mask must be bit-identical to the scalar reference over the alignment sweep + 4096 random
   blocks; reduction ops ≤ hand baseline.
3. **Full stable build:** regen `regex/generated.rs` (emit `derive(Debug)`), then `cargo +stable
   build -p parse_that` with zero `#![feature]`; ~400 tests green on stable.
4. **Oracle strength:** INJECT_BUG one bit per kernel; confirm the sweep+random corpus catches 100%;
   confirm `guarded_call` trips on a deliberately OOB `vst1q`.
5. **Abrogation confirm:** delete `count_leading_digits_neon`; `number_fastpath_test`/`number_simd_
   test` (33 cases) green on the scalar SWAR path; measure no number-lane regression.

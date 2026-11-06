# LOCK-9 · Family 9B — NATIVE-REBIRTH (parse-that keeps its own kernels)

*The scanner & kernel plane, reborn in idiomatic stable NEON — each kernel a scalar
parity oracle plus a checkasm-liveness proof, no cross-repo coupling, gated on measured
no-regression per the refutation law.*

---

## 1. The thesis, stated against its neighbours

Family 9B holds that parse-that's scan layer is best served by **native intrinsic
kernels written directly in `core::arch::aarch64`**, one scalar twin per kernel as the
correctness ground truth, and a differential/liveness harness that proves each NEON body
is both *reached* and *bit-parity* with its scalar oracle — with **no physical code shared
with `bbnf-simd`**. The isomorphism to bbnf-lang's kernel home is *behavioral and by
discipline* (same movemask idiom, same checkasm protocol, same scalar-oracle law), never a
vendored crate under a sync gate. This is the deliberate contrast to the one-kernel-library
route: 9B's central bet is that the *reasonable point* of rust↔rust kernel isomorphism
(Constitution clause iii) is design-level, and that the drift risk of duplicated small
kernels is cheaper to carry than the lowest-common-denominator API + sync-gate tax a shared
crate imposes. That bet is a **null hypothesis to defeat by measurement**, named in §6.

The distinction from the refuted shim (`simd128.rs`) is the crux: that shim reproduced the
`std::simd` *vocabulary* one-for-one (`u8x16`, `to_bitmask`, `swizzle_dyn`) so scanners
needed zero call-site churn — and precisely because it had to honour the "dense
1-bit-per-lane" semantics of `to_bitmask`, it was *forced* into the double-`vaddv` movemask
that made it emit 2271 `addv.8b` vs a 509 baseline. **Native rebirth rewrites the inner
loops**, so each site chooses the *correct* movemask idiom for how it actually consumes the
mask — accepting real, bounded call-site churn as the price of optimal codegen the
zero-churn shim traded away.

---

## 2. The mechanism made concrete — the movemask taxonomy IS the family

The refutation law reads "stable NEON must be measured no-regression; vshrn narrow, never
the double-addv movemask." Made concrete, there are three movemask conventions and a
scanner picks by its **consumption pattern**, not by convenience:

**(a) Nibble-stride, 4-bit-per-lane — the cheap position-scan idiom.** A single
`vshrn_n_u16::<4>(vreinterpretq_u16_u8(cmp))` narrows a 16-lane compare to a `u64` where
each input byte occupies one nibble (0x0 / 0xF). One `shrn.8b` + one `fmov` — no reduction
at all. The consumer reads position via `trailing_zeros() >> 2` and tests presence via
`& 0x8888…` / `& 0x1111…`. This is exactly the lowering the confirmed PROVE-10 memchr hot
path shows (`shrn.8b v0,v0,#4; fmov x8,d0; ands x8,x8,#0x8888…`). It fits every consumer
whose only need is *the index of the first/next matching byte* — i.e. all the
`trailing_zeros()` sites.

**(b) Dense-64, 1-bit-per-byte, over four blocks — the vpaddq tree.** Where a scanner
assembles a **64-bit mask across four 16-byte blocks** and feeds it to a whole-word
operation (quote-parity's `quote |= q << (k*16)` → `prefix_xor` carry-less multiply), the
nibble stride is unusable (4 bits × 64 bytes = 256 bits ≠ `u64`). The lawful producer is
the simdjson `neon_movemask_bulk` **pairwise-add tree**: `vandq_u8` each block with the
bit-position constant `{1,2,4,8,16,32,64,128, …}`, then three `vpaddq_u8` (pairwise, NOT
`vaddv`) to fold four blocks into one `u64`. `vpaddq` walks all 64 bytes in a balanced tree;
it is **not** the per-half `vaddv` full-reduction the law forbids.

**(c) FORBIDDEN — double-`vaddv` dense-16.** `vand`(bit-positions) on the low/high halves
then two `vaddv_u8` reductions to build a dense `u16`. This is the shim's defect — and it
is **already live in parse-that's own hand-NEON**: `number_simd.rs`
`count_leading_digits_neon` does `let lo_sum = vaddv_u8(lo_masked); let hi_sum =
vaddv_u8(hi_masked); mask = lo_sum | (hi_sum<<8); … mask.trailing_zeros()`
(`parse-that/rust/parse_that/src/parsers/scan/number_simd.rs:56-60`). The consumer wants
only `trailing_zeros()` — so this dense-16 double-`vaddv` is strictly dominated by idiom (a):
a lone `vshrn_n_u16::<4>` + `trailing_zeros() >> 2`. This is the family's cleanest concrete
win and its first prototype target. (It is a *digit-count position scan*, not the SPEC-02
NO_GO Mula/Lemire *digit-value* accumulation — replacing its movemask is lawful.)

The engineered form of the taxonomy: a thin `simd/movemask.rs` exposing exactly two lawful
producers — `movemask_nibble(uint8x16_t) -> u64` (idiom a) and `movemask_bulk64(x4) -> u64`
(idiom b) — plus `first_match(uint8x16_t) -> Option<usize>` sugar over (a). The bbnf-simd
crate's `aarch64/movemask.rs` (`vshrn_n_u16::<4>` then a compaction tail to a dense `u16`)
demonstrates the narrow but keeps a costly repack; 9B's `first_match` **elides the repack
entirely** by consuming the nibble stride in place. That elision — not sharing the file — is
the point of a native rebirth.

---

## 3. The substrate

**Module shape (colocation edict; productionizes the flat shim into a directory-module):**

```
rust/parse_that/src/simd/
  mod.rs        — the cfg pivot + public re-exports (aarch64 → neon, else → scalar)
  movemask.rs   — the TWO lawful producers (nibble + bulk64) + first_match sugar
  neon.rs       — #[cfg(target_arch="aarch64")] load/compare/tbl primitives
  scalar.rs     — #[cfg(not aarch64)] [u8;16] parity oracle, no target intrinsics
```

Native rebirth is *not* a single `u8x16` vocabulary type that the scanners import
unchanged — that is the shim again. It is: (i) a minimal shared primitive set
(`movemask.rs`, the `vqtbl1q_u8` swizzle, `vld1q_u8`/`vst1q_u8` load-store, `vmaxvq_u8`
"any") in `simd/`, and (ii) **each scanner's inner loop rewritten in `core::arch::aarch64`
directly**, choosing its movemask producer per §2. The scalar twin lives beside each NEON
body under `cfg(not(target_arch="aarch64"))` and is the differential oracle, never a
fallback that ships on the M-series target.

**Why stable, at zero cost:** aarch64 NEON intrinsics (`vld1q_u8`, `vshrn_n_u16`,
`vqtbl1q_u8`, `vpaddq_u8`, `vmaxvq_u8`, `vsubq_u8`, `vcgtq_u8`, `vmull_p64`) are **stable in
`core::arch::aarch64` since Rust 1.59** — the port needs no nightly feature. NEON is
*baseline-mandatory* on aarch64, so unlike the x86 arms these kernels need **no
`is_*_feature_detected!` runtime guard and no `#[target_feature]` gate** (the one exception,
`vmull_p64`, is discussed in §6). Purging x86 therefore also deletes the entire runtime
feature-detection machinery as dead weight on the sole target — a rebirth simplification, not
merely a removal.

**The excision co-travelers (required for the stable cutover, not kernel concerns):**
`smallbox[nightly]` → stable `smallbox!` at `parse.rs:74` (alloc-neutral, `Cargo.toml:97`);
the `portable_simd`/`stmt_expr_attributes` `lib.rs` feature attrs deleted once the last
`std::simd` site is gone; the `fmt_helpers_for_derive` residual is a *generator* fix
(emit `#[derive(Debug)]`, regen `generated.rs`), out of the kernel plane. 9B owns the
kernels; it inherits these as the price of leaving the ICE-pinned nightly.

---

## 4. Integration seams — where extant code serves (path:line)

**The eight `portable_simd` scanners to rebirth** (hit counts per the recon census):
- `parse_that/src/scanners.rs` (22) — ws-skip + shufti `find_first_of`; movemask consumers
  at `:163` (`to_bitmask() as u64` ws mask), `:208` (`(!mask).to_bitmask().trailing_zeros()`
  → **idiom a**), `:286` (`is_match.to_bitmask().trailing_zeros()` shufti hit → **idiom a**);
  `swizzle_dyn` LUT at `:281-282` → `vqtbl1q_u8` (codegen-equivalent, confirmed `tbl`
  516=516).
- `parse_that/src/parsers/scan/structural_bitmap.rs` (13) — simdjson structural scan;
  `:43` `to_bitmask() as u16` and `trailing_zeros()` consumers at `:128,148,188,216,233` →
  **idiom a** throughout (all are `pos = i + mask.trailing_zeros()`).
- `parse_that/src/parsers/scan/quote_parity.rs` (4 portable + x86/aarch64 arms) —
  `classify_stripe_64` at `:71` builds `quote |= q << (k*16)` over 4 blocks → the **one
  dense-64 site, idiom b** (`vpaddq` tree); its output feeds `prefix_xor` at `:41`.
- `parse_that/src/parsers/scan/decode.rs` (17) — JSON string decode-to-arena;
  `store_chunk` at `:227` is `write_unaligned([u8;16])` → native `vst1q_u8`; the escape scan
  is a position scan → **idiom a**.
- `parse_that/src/parsers/scan/ws_comment.rs` (16), `span_parser/scan.rs` (7),
  `parsers/scan/quoted_simd.rs` (7, `odd_parity_backslashes`), `state.rs` (2, comments only).

**The two x86 arms to purge** (the abrogation reaches parse-that):
- `parsers/scan/number_simd.rs:145-235` — SSE4.2 `_mm_*` + `is_x86_feature_detected!`; keep
  and rebirth the NEON `:29-136` (fixing the double-`vaddv` at `:56-60` per §2c), scalar
  `:239` stays as oracle.
- `parsers/scan/quote_parity.rs:42-49` — `pclmulqdq` `prefix_xor`; keep the aarch64
  `vmull_p64` `:51-56` + the 5-shift scalar oracle `:58-68`.

**The invariant every kernel rides:** `state.rs:57` `INPUT_PAD_BYTES = 64`; `state.rs:122`
`PaddedView` guarantees `bytes.len() == len + 64` trailing NUL — every stripe load skips a
tail guard. This is parse-that-specific (bbnf-simd blocks are unpadded 64-byte) and is a
concrete reason the two kernel homes are *not* trivially unifiable — a substrate argument for
9B's no-coupling stance.

**The bench/oracle protocol seam:** divan `harness=false`, `#[global_allocator] mimalloc`,
`[profile.bench]` fat-LTO/cgu=1/O3; targets `parse_that_combinator` (6 JSON corpora),
`parse_that_css`, `micro_parse_that`. The paired no-regression gate builds a baseline and a
patched bench binary and runs them back-to-back in two windows, opposite arm order.

---

## 5. Prior art (cited)

- **simdjson aarch64 `neon_movemask_bulk`** (Langdale & Lemire, *Parsing Gigabytes of JSON
  per Second*, VLDB Journal 28(6), 2019) — the four-block `vandq`+`vpaddq` pairwise-add tree,
  idiom (b). The canonical dense-64 movemask that avoids `vaddv`.
- **Danila Kutenin / Daniel Lemire, "the `vshrn` narrowing movemask" (2022 blog + sse2neon
  PR history)** — `vshrn_n_u16::<4>` as the one-instruction replacement for the naive
  `vand`+`vaddv` `_mm_movemask_epi8` emulation; idiom (a). This is the literal source of the
  "vshrn narrow, never double-addv" law.
- **sse2neon `_mm_movemask_epi8`** — the documented before/after (double-`vaddv` → `shrn`)
  is the counterexample-and-fix pair the family's insn-count gate encodes.
- **FFmpeg `checkasm.h`** — the differential-test + micro-bench harness for hand-vectorized
  kernels: randomized identical src buffers, `call_ref`/`call_new` dst compare, `memcmp`
  clobber detection, alignment sweep, stack-clobber canary, signal handler, outlier filter.
  A working native port of this pattern already exists in the campaign at
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` (Xorshift64 PRNG, `check_parity_at`
  alignment sweep 0..64, `stack_clobber_then` 1 KiB `0xDE` canary, `signal_guard::arm`
  SIGSEGV/SIGBUS/SIGILL, `BBNF_SIMD_INJECT_BUG` injection, `robust_mean_ns`). 9B **re-derives
  this harness natively in `parse_that/tests/`** — same pattern, no shared code.
- **Rust `core::arch::aarch64` stabilization** — aarch64 NEON stable since 1.59.0 (2022);
  the port carries no nightly SIMD dependency.
- **regex-automata memchr/Teddy prefilter ladder** (BurntSushi) — the confirmed PROVE-10
  `Dfa::find` tier; lives in `bbnf-regex`, consumed by scanners, *not* duplicated — the seam
  where 9B's hand kernels and the DFA prefilter meet without merging.
- **`wide` / `simdeez` crates — considered and rejected**: both reintroduce a portable-SIMD
  *vocabulary* abstraction, i.e. the shim trap that forced the dense-`to_bitmask` semantics;
  native `core::arch` is the whole point of a rebirth.
- **tree-sitter external scanners** — production parser-generators ship hand-written scanner
  kernels (C) rather than generating them; a data point that a native hand-kernel plane is a
  legitimate, not anachronistic, architecture.

---

## 6. Named risks & counterexamples

- **R1 — the double-`vaddv` recurs.** The forbidden idiom is *already present* in
  `number_simd.rs:56-60`; a careless rebirth could preserve it or re-introduce it when a
  consumer looks like it "wants a dense u16." *Guard:* the `otool -tv` insn-count clause —
  reduction ops (`addv.8b` + `vaddv`) on the hot classify/movemask functions must be **≤ the
  baseline** (baseline `addv.8b` ≈ 509; the refuted shim ballooned to 2271). Any `vaddv` in a
  position-scan path is a defect.
- **R2 — the dense-64 quote-parity site is mis-idiomed (the sharpest 9B counterexample).**
  `classify_stripe_64` genuinely needs a **dense** 64-bit mask (`<<(k*16)` → `prefix_xor`
  clmul); naively applying the nibble-stride `vshrn` there yields 256 bits and corrupts the
  parity. This is the one site that must use the `vpaddq` tree (idiom b), *not* (a).
  Conflating the two idioms is the family's most plausible silent regression. *Guard:* the
  prototype ports this exact site and disasm-proves `vpaddq` (not `vaddv`, not a shrn-then-
  repack).
- **R3 — parity ≠ liveness.** A scalar oracle and a NEON body agree *by construction*, so a
  passing parity test does **not** prove the NEON path executed — the kernel could be
  dead-stripped, mis-`cfg`'d, or silently scalar. *Guard:* liveness is proven by the `otool`
  disasm showing `shrn`/`tbl`/`vpaddq` present on the hot path (as PROVE-10 did), **plus** a
  build-time `cfg!(target_arch="aarch64")` assertion in the harness — the checkasm parity
  sweep proves *correctness across alignments*, the disasm proves *the fast path is live*.
  The two are distinct obligations; the family name binds both.
- **R4 — `vmull_p64` feature gate.** quote-parity's aarch64 `prefix_xor` is gated
  `target_feature="aes"`; a build without it silently takes the 5-shift scalar oracle. On the
  M-series target AES/PMULL is always present, but the gate means the checkasm harness must
  **exercise both arms** (feature-on and the scalar fallback) so the fallback cannot rot. Keep
  the scalar 5-shift as the parity oracle; do not hard-`panic` the non-AES path.
- **R5 — no-coupling drift (the family's null hypothesis to defeat).** Duplicating ~8 small
  kernels across parse-that and bbnf-simd risks a fix landing in one and not the other (a
  movemask bug, an off-by-one in a tail). *This is where the one-kernel-library route wins if
  9B cannot answer it.* 9B's answer is three-part and must be *measured*, not asserted:
  (i) the shared **design** — one movemask idiom doc, one checkasm protocol, one scalar-oracle
  law — as the anti-drift mechanism; (ii) a **shared differential conformance corpus as DATA**
  (a fixture file, not linked code) both harnesses run; (iii) the measured LOC-duplication and
  substrate-divergence (PaddedView-64-pad vs unpadded blocks, `vqtbl1q_u8` single-table shufti
  vs bbnf-simd's `vqtbl4q_u8` four-table classify) showing the "shared" API would be a
  lowest-common-denominator that helps neither. If (iii) shows the kernels are near-identical
  after rebirth, 9B *loses to 9A on merit* — that is the honest close.
- **R6 — no-regression is inside the noise floor.** The refuted shim's own "neutral" claim
  straddled 1.0 with the two windows disagreeing in *sign* — a neutral result and a
  masked-regression are indistinguishable without discipline. *Guard:* two windows, opposite
  arm order, N≥200, per-corpus median **and** best-min; a sign disagreement on any corpus
  means "re-instrument," never "declare neutral."

---

## 7. What a prototype must test to move this family

1. **Native rebirth of the two hardest, idiomatically-opposite kernels**, in
   `core::arch::aarch64` with no `std::simd` vocabulary: (a) `structural_bitmap.rs` /
   `scanners.rs` position scan → `movemask_nibble` + `trailing_zeros()>>2`; (b)
   `quote_parity.rs::classify_stripe_64` dense-64 → `movemask_bulk64` `vpaddq` tree. Plus the
   `number_simd.rs:56-60` double-`vaddv` → nibble-stride replacement as the clean-win warm-up.
2. **Disasm liveness + insn-count no-regression.** `otool -tv` (or `cargo asm`) on the hot
   functions must show `shrn`/`vpaddq`/`tbl` present (liveness) and reduction-op count
   **≤ baseline** (the 2271-vs-509 counterexample must not reproduce). This is born-RED.
3. **checkasm-liveness harness ported natively into `parse_that/tests/`** (the bbnf-simd
   pattern re-derived, not linked): alignment sweep 0..63, 10k random 16-byte vectors + a
   JSON-alphabet distribution, stack-clobber canary, SIGSEGV/SIGBUS/SIGILL guard, bug-injection
   mode, and a `cfg!(target_arch="aarch64")` liveness assertion. Byte-parity NEON-vs-scalar on
   every alignment, both `vmull_p64`-on and scalar-fallback arms.
4. **Paired divan no-regression** — baseline vs patched bench binaries, **two windows,
   opposite arm order, N≥200**, all six JSON corpora + CSS: patched median **≥ 0.98× on every
   corpus in both windows**. This is precisely the gate the shim FAILED (window A 0.81–0.98×);
   passing it on native rebirth is the family's load-bearing evidence.
5. **The no-coupling cost, measured** — LOC duplicated vs bbnf-simd, the substrate-divergence
   table (PaddedView-64 vs unpadded; `vqtbl1q_u8` vs `vqtbl4q_u8`), and a shared differential
   conformance corpus (as data) proving anti-drift without shared code. This is the datum that
   defeats — or concedes to — the one-kernel-library null hypothesis.
6. **Full stable build** — `cargo +stable build` of the rebirthed scanners on
   `core::arch::aarch64`, `portable_simd` feature deleted, proving the kernels need no nightly.

The value framing is **stability + optimal-idiom codegen, never a speedup claim** — the word
"faster" appears in no gate. The family succeeds if the native kernels are stable, live,
bit-parity, no-regression, and the no-coupling cost is measured to be less than a shared
crate's sync-gate tax; it honestly concedes to the one-kernel-library route if that last
measurement inverts.

# LOCK-9 · Family 9B — NATIVE-REBIRTH (pass-3 research)

*parse-that keeps its own aarch64 kernels, rewritten in idiomatic stable NEON; each kernel
carries a scalar parity oracle and checkasm-liveness; no cross-repo coupling; the gate is
measured no-regression per the PROVE-09 refutation law.*

Host oracle base: aarch64-apple-darwin (Apple M, 18 CPU), Darwin 25.4.0. Substrate =
parse-that@`ef10d5b` scratch copy (R3 §8 recipe), toolchain pin `nightly-2026-04-11`; the
family's terminal claim is a `cargo +stable` build on that copy. bbnf-simd@HEAD
(`skinny/crates/bbnf-simd`) is READ as prior-art reference only — 9B does NOT vendor it.

---

## 1. The load-bearing pass-3 refinement — the movemask producer is CONTRACT-SPLIT

The extant PT-2.W1 plan (`docs/tranches/sk-v24/PT-2.md:173-184`) commits a single error: it fixes
the movemask "at the `vshrn` narrow idiom" **uniformly** and migrates ~5 consumers to a
`tz >> 2` nibble stride. That is correct for one class of site and **corrupts** the other. The
producer surface is two contracts, and 9B must author both, each with its own oracle.

**LEAN producer — single 16-byte vector → true 16-bit movemask, CTZ-iterated.** Every site that
builds a mask from one `u8x16` compare and consumes it only by `trailing_zeros()` (never
OR-stitching into a wider word, never feeding a carryless multiply):
- `structural_bitmap.rs:43` `classify_16` → `to_bitmask() as u16`, iterated at `:128`,`:188`,`:216`.
- `quoted_simd.rs:45-46` `to_bitmask() as u32`; `ws_comment.rs:81`,`:150`,`:184-211` (CTZ).
- `scanners.rs::find_first_of_nibble_lut` + `span_parser/scan.rs:123` (CTZ).

Here the reference technique is bbnf-simd's `aarch64/movemask.rs:4` `movemask_u8x16`:
`vshrn_n_u16::<4>(vreinterpretq_u16_u8(cmp))` narrows the 16×`0x00/0xFF` compare-mask to 8
nibble-pairs, then reconstitutes a **true 1-bit-per-lane** `u16` (`vand 0x11` / `vsri` /
`vzip1q` / scalar pack, `:6-24`). The nibble *narrow* is only an intermediate; the final
contract is still one bit per byte. A LEAN consumer MAY instead skip the reconstitution and
iterate the raw nibble stride at `tz >> 2` — legal *only* because CTZ never crosses a word
boundary and never enters `prefix_xor`.

**DENSE-64 producer — four 16-byte vectors → one true 64-bit mask, OR-stitched, fed to
`prefix_xor` / structural bitmap.** Two sites:
- `quote_parity.rs:72-87` `classify_stripe_64`: 4× `to_bitmask() as u64` OR-stitched
  `q |= q_k << (k*16)`, then `:139` `prefix_xor(real_quotes)` — a carryless-multiply
  (`vmull_p64`, `:54`) mod-2 prefix sum. **Every bit MUST map to exactly one byte in order** or
  the in-string bitmap is silently wrong.
- `structural_bitmap.rs:49-60` `classify_stripe`: 4× `to_bitmask() as u64` OR-stitched
  `mask |= m << (k*16)`, CTZ-iterated over 64 bits.

A nibble-stride form is **forbidden** at both dense sites: (i) a 4-bit-per-lane result from one
16-byte vector cannot be `<< (k*16)` OR-stitched into 64 bits — four of them overlap; (ii)
`prefix_xor` is bit-exact — a 4-bits-per-byte packing makes the carryless-multiply parity
meaningless. **This is the concrete counterexample the differential oracle must exhibit (§4).**

The two lawful dense strategies (the prototype fork):
- **(a) 4× LEAN-narrow + shift-OR** — what bbnf-simd actually ships:
  `byte_class_from_eq_set_64.rs:66-69` calls `movemask_u8x16(m0..m3) as u64` then
  `lo0 | lo1<<16 | lo2<<32 | lo3<<48`. Four vshrn narrows + a scalar stitch. No `addv` at all.
- **(b) single `vpaddq` reduction tree** (simdjson `neonmovemask_bulk`, Lemire/Langdale): AND
  the 4 compare-vectors with a bit-position constant `[0x01,0x02,…,0x80]×2`, then a
  `vpaddq_u8` fold 64→32→16→8 bytes = 64 bits in one reduction tree. No per-16 narrow, no
  scalar loop.

Both avoid the PROVE-09 refutation's forbidden idiom — the shim's **double-`vaddv` per
`to_bitmask`** (VERIFY attack #2: 2271 `addv.8b` vs baseline 509). The gate is which of (a)/(b)
emits fewer reduction ops on the dense site with bit-parity intact — measured, not asserted.

---

## 2. F2 — stable-toolchain build & the exact blast radius

**Kernel surface builds on stable `core::arch::aarch64`** once portable_simd is replaced by the
`simd/` directory-module (PT-2.W1: `mod.rs` cfg-pivot + `neon.rs` + `scalar.rs`) exposing the
u8x16/mask8x16 vocabulary the 8 scanners use (`splat/from_slice/from_array/to_array`,
`& | >>`, `simd_eq/simd_ne`, `swizzle_dyn` = `vqtbl1q_u8`) **plus the two movemask entry
points** — NOT one. PROVE-09 proved every scanner file compiles on stable `1.96.1` with a
276-LOC flat shim and zero call-site churn (`patch.diff`); the only defect was that its single
`to_bitmask` used the double-`vaddv` reduction. 9B's `simd/` keeps the zero-churn `use`-swap
for the vocabulary but splits `to_bitmask` into `movemask_16` (LEAN) and `movemask_dense_64`
(DENSE, folds the `<< k*16` OR-stitch into the producer).

**smallbox nightly→stable cut — blast radius = 3 edits, alloc-neutral, zero perf surface**
(PROVE-09 §Alloc, `patch.diff:5-10,100-122`):
- `parse_that/Cargo.toml:97` `smallbox {"0.8",features=["nightly"]}` → `smallbox = "0.8"`.
- `parse.rs:1` `use smallbox::{SmallBox,space::S32}` gains `smallbox` (the macro).
- `parse.rs:74` `SmallBox::new(parser_fn)` → `smallbox!(parser_fn)`. The nightly feature only
  provided the `CoerceUnsized`/`coerce` `dyn`-unsizing; `smallbox!` does it on stable with the
  identical S32 inline / heap-spill threshold. Counting-allocator probe: construct
  `json_parser()` = 6 allocs/5960 B, parse twitter = 5390 allocs, **byte-identical** across the
  cut (the vtable boxes are construction-time, never per-parse — why the cut is cyc/B-neutral).

**Residual nightly gates:** `stmt_expr_attributes` is dead (delete). `portable_simd` falls when
W1 lands. `fmt_helpers_for_derive` is the SOLE residual and is **not a kernel concern** —
`regex/generated.rs` emits the *expanded* `#[derive(Debug)]` (an `automatically_derived impl`
calling perma-unstable `Formatter::debug_tuple_field1_finish`); the fix is a cross-repo
generator change (emit the attribute, regen via `scripts/bootstrap-regex.sh`), named here, owned
by LOCK-7/GN-1, out of 9B's own-kernel scope. Blast radius of the kernel rebirth proper is
therefore: 8 scanner `use` lines + `simd/` (3 files ~380 LOC) + 3 smallbox edits + 2 x86-arm
excisions (§3) — nothing touches `Parser`/lifetime surface.

---

## 3. Scanner-file mapping census (F1, owed) — kernel-tier / vector-tier / no-go

| File | portable_simd hits | tier | producer contract |
|---|---|---|---|
| `parsers/scan/structural_bitmap.rs` | 13 | **kernel — LEAN** | `classify_16`→`u16`, CTZ; the LEAN producer home |
| `parsers/scan/quote_parity.rs` | 4 (+clmul) | **kernel — DENSE** | `classify_stripe_64`→OR-stitch 64→`prefix_xor` (`vmull_p64`) |
| `parsers/scan/number_simd.rs` | (x86+NEON) | **kernel — reduction** | `count_leading_digits_neon` (`vcgtq`+`vaddv`); NOT Mula/Lemire |
| `parsers/scan/decode.rs` | 17 | vector-tier | `store_chunk` u8x16 unaligned store to arena; no movemask |
| `parsers/scan/ws_comment.rs` | 16 | vector-tier (LEAN consumer) | consumes `classify_stripe`; CTZ |
| `parsers/scan/quoted_simd.rs` | 7 | vector-tier | `odd_parity_backslashes` 16-bit chunk parity |
| `scanners.rs` | 22 | vector-tier (LEAN) | inline ws scan + `find_first_of_nibble_lut` CTZ |
| `span_parser/scan.rs` | 7 | vector-tier (LEAN) | `find_first_of_nibble_lut` mirror |
| `state.rs` | 2 | **no-go** | comments only; `PaddedView`/`INPUT_PAD_BYTES:57` (feeds all) |

Split: **3 kernel-tier** (2 movemask producers + 1 reduction kernel), **5 vector-tier**, **1
no-go**. The x86 arms to purge sit inside two kernel files and are single-strategy excisions,
not rewrites: `number_simd.rs:145-235` (SSE4.2 `_mm_*` + `is_x86_feature_detected!`) — keep NEON
`:29-136` + scalar `:239`; SPEC §0/02 **NO_GO** forbids pursuing in-vector digit accumulation
(CPI inversion 0.13→0.30 regresses the lane) — the NEON path stays a plain
`vcgtq`+`vaddv` count. `quote_parity.rs:42-49` (`pclmulqdq prefix_xor`) — keep aarch64
`vmull_p64` `:51-56` + the 5-shift scalar fallback `:58-68`.

---

## 4. F2 — the differential correctness oracle (the checkasm technique, own-homed)

9B re-authors bbnf-simd's checkasm harness inside parse-that (`tests/simd_parity.rs`,
`tests/checkasm_common.rs`) — the same technique, no shared crate. The reference machinery
(`bbnf-simd/tests/checkasm_common.rs`):
- `Xorshift64` deterministic PRNG (`:3-31`) fills random byte vectors — reproducible sweep.
- `guarded_call` / `with_stack_canary_xor_fold` (`:33-72`): a 1024-byte canary, xor-folded
  pre/post the candidate, panics on first clobbered byte — catches hand-NEON stack corruption.
- `callee_saved_register_then` (`:83-112`): writes sentinels into x19–x28, runs the candidate,
  asserts they survive — an ABI-liveness probe for the `unsafe` intrinsic bodies.
- Test shape (`checkasm_bitmap_prefix_xor_64.rs`): boundary cases
  `{0, u64::MAX, 0x5555…, 0xAAAA…, 1, 1<<63}` × `carry∈{false,true}`, then a 4096-iteration
  random sweep, each asserting `candidate == scalar_reference`.

9B's `simd_parity.rs` extends this to the movemask contract split:
1. **Per-primitive parity:** `movemask_16`, `movemask_dense_64` (both strategies (a)/(b)),
   `classify_16`, `classify_stripe_64`, `count_leading_digits_neon`, `swizzle_dyn`, `simd_ne`,
   `>>` — each NEON result `== scalar` oracle over 10k random 16/64-byte vectors + the boundary
   set, all `black_box`'d.
2. **Alignment sweep:** run every kernel at input offset `0..64` inside a `PaddedView` (trailing
   64 NUL, `state.rs:57`) — proves the padded-load contract and no OOB.
3. **INJECT_BUG rate:** a `cfg(inject_bug)` mutant perturbs one lane of each producer; the
   harness must fail — measuring the oracle's detection power (target 100% single-lane).
4. **The nibble-stride corruption proof (the §1 counterexample, made concrete):** build a
   deliberately-wrong dense-64 producer using the LEAN `tz>>2` nibble stride, feed its output
   through `prefix_xor`, and assert the resulting in-string bitmap **diverges** from the scalar
   `compute_in_string_bitmap` reference on inputs with `"`/`\\` past byte 16. This is the test
   that ratifies the contract split — a naive uniform-vshrn migration fails it.

Evidence to produce: `cargo +stable build` log (aarch64, portable_simd + smallbox[nightly]
removed, only the generated.rs residual remaining); the parity-harness pass table (boundary +
4096-sweep + alignment 0..64 + INJECT_BUG 100%); the nibble-stride divergence witness.

---

## 5. Own-kernel vs shared — the residual divergence that justifies 9B

Even were vendoring adopted (9A), parse-that would still author kernels bbnf-simd does not
have, because the two repos' dense-64 sites are **semantically different consumers**:
- bbnf-simd's dense producer (`byte_class_from_eq_set_64.rs:61-76`) is 4× `movemask_u8x16` OR-
  stitch coupled to its **grammar-classifier** surface (`alphabet:&[u8;64]`, `StructuralAlphabet`,
  `select_classifier`). Its `bitmap_prefix_xor_64` is a *separate* kernel taking a
  pre-built mask — there is **no quote-parity / `vmull_p64` prefix_xor producer** in bbnf-simd.
- parse-that's dense-64 (`classify_stripe_64`→`prefix_xor`) is coupled to `PaddedView` /
  `ParserState` and the simdjson quote-parity algorithm. No bbnf-simd analog exists.

So the **residual LOC/substrate divergence** (F1) between one-home and two-home is bounded and
concrete: 9B's own home re-authors ~2 movemask producers + the checkasm harness (~1 test module)
— duplication of *technique*, not of *kernel*, since the dense producers, the padded-view clamp
convention (`INPUT_PAD_BYTES=64` vs bbnf-simd's `eob_pad_clamp`), and the single-table
`find_first_of_nibble_lut` shuffle (vs bbnf-simd's `classify_tbl4` multi-table) do not coincide.
The maintenance/drift basis: 9B trades a sync-gate (and the capacity-constant/clamp reconciliation
a shared home would force) for local, per-kernel enforcement of the PROVE-09 no-regression law —
each kernel's gate lives beside it, no cross-repo cadence. `#[inline]` across the rlib seam is
measured-live (0 in-loop indirect); a non-inlinable boundary would only appear under `cgu>1` +
non-LTO with the kernel body in a *different* crate — which 9B avoids by construction (kernels
are in-crate).

---

## 6. Prior art (citations)

- **simdjson NEON `neonmovemask_bulk`** (Langdale & Lemire, *Parsing Gigabytes of JSON per
  Second*, VLDBJ 2019; simdjson `arm64/simd` `to_bitmask`): the `vpaddq` reduction tree
  = dense strategy (b); the canonical "NEON has no `pmovmskb`" workaround.
- **Lemire, "Bit-shuffling / movemask on ARM NEON"** (2022) & Mula's writeup — the
  vshrn-narrow (strategy (a)) vs shift-add-tree tradeoff; bbnf-simd cites Mula at
  `byte_class_from_eq_set_64.rs:64`.
- **`std::arch` `vshrn_n_u16` / `vqtbl1q_u8` / `vmull_p64`** — stable since Rust 1.61 aarch64
  NEON stabilization (`stdarch`); the entire vocabulary PROVE-09 needs is stable.
- **simdjson quote-parity / prefix-XOR via carryless multiply** (`_mm_clmulepi64` / `vmull_p64`)
  — the algorithm `quote_parity.rs` implements; parity correctness depends on 1-bit-per-byte.
- **dav1d / FFmpeg `checkasm`** — the stack-canary + callee-saved-register liveness harness
  bbnf-simd's `checkasm_common.rs` mirrors; the model for 9B's own oracle.
- **SPEC §0 laws**: 03 (vshrn narrow **CONFIRMED GO**, +35% mask-narrow), 04 (movemask
  bit-stitch is the ALU-bound floor — narrow+compaction, NOT block scheduling), 02
  (Mula/Lemire digit **NO_GO**), 09 (shim perf **REFUTED** — the no-regression gate is law).

---

## 7. Named risks & counterexamples

- **R-split — uniform-vshrn corrupts the dense site.** The PT-2.W1 "one movemask idiom" is
  wrong. *Counterexample:* nibble-stride into `prefix_xor` → wrong in-string bitmap (§4.4).
  *Guard:* the contract split + the divergence witness test.
- **R-double-vaddv — the movemask regresses again.** *Counterexample:* PROVE-09's 2271 `addv.8b`
  vs 509 (VERIFY attack #2). *Guard:* `otool -tv` reduction-insn census on both producers ≤
  baseline, base-pinned.
- **R-noise — a claimed neutral that is window asymmetry.** *Counterexample:* PROVE-09's phantom
  1.34–1.57× (two-arm asymmetry, refuted). *Guard:* paired binaries, opposite arm order, two
  windows, N≥200, min+median+best-min, per-block loadavg; ≥0.98× on EVERY corpus in BOTH windows.
- **R-x86-residue — a `pclmulqdq`/SSE4.2 arm survives behind a `cfg`.** *Guard:* structural
  token grep `rg -E '_mm_|x86_64::|is_x86_feature' rust/*/src` → 0.
- **R-unsound-ABI — a hand-NEON body clobbers a callee-saved register.** *Guard:* the
  `callee_saved_register_then` probe on every producer.

---

## 8. What a prototype must test to move 9B

1. Author `simd/` (`mod.rs`/`neon.rs`/`scalar.rs`) with the u8x16/mask8x16 vocabulary + **two**
   producers: `movemask_16` (LEAN, vshrn-narrow→1-bit/lane `u16`) and `movemask_dense_64`
   (DENSE) implemented **both** ways — (a) 4×narrow+shift-OR, (b) `vpaddq` reduction tree.
2. `cargo +stable build` (aarch64) with portable_simd + smallbox[nightly] removed → capture the
   log; confirm the sole residual is the cross-repo generated.rs Debug expansion.
3. `tests/simd_parity.rs`: NEON==scalar over boundary + 10k random + alignment-sweep(0..64) +
   black_box for every kernel; INJECT_BUG mutant → 100% caught; the nibble-stride→prefix_xor
   divergence witness (ratifies the split).
4. `otool -tv` reduction-insn census: LEAN `movemask_16` ≤ baseline `to_bitmask` (509);
   DENSE (a) vs (b) head-to-head reduction-op count — pick the winner by insn count with parity.
5. Paired cold divan no-regression (two windows, opposite arm order, N≥200, per corpus, ≥0.98×)
   — the exact gate the PROVE-09 shim failed; base-pinned to `nightly-2026-04-11`, both HEADs named.
6. `tests/no_x86.rs` structural token gate → 0; `callee_saved_register_then` ABI probe on each
   producer.

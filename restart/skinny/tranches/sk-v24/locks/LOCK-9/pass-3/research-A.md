# LOCK-9 · Family 9A — ONE-KERNEL-LIBRARY (research, pass 3)

One shared aarch64 kernel home serves both repos; parse-that scanners become thin consumers;
the isomorphism is physical (one movemask definition, both callers, zero in-loop indirect).
Base-pin: bbnf-simd@HEAD + parse-that@`ef10d5b`; host aarch64-apple-darwin M-series.

## 1 — The mechanism: two canonical movemask producers, authored once

The whole plane reduces a `uint8x16_t` (lanes 0x00/0xFF) to bits. Two contracts, not one, and
they are **not substitutable** — swapping them is a correctness fault, not a perf choice:

- **LEAN nibble-`vshrn`** — for consumers that only CTZ to the *first* set lane
  (`find_ascii_set_member64` lib.rs:208, memchr-style skip, ws first-non-ws `scanners.rs:208`).
  Idiom: `shrn.8b v,v,#4; fmov x,d` → a 64-bit value with a **nibble (4 bits) per input byte**;
  test with `& 0x8888…8` and `trailing_zeros()/4`. Exactly memchr's lowering, disassembled live
  in PROVE-10 REPORT §2c (`shrn.8b v0,v0,#4; fmov x8,d0; ands x8,x8,#0x8888…`). Cost: **zero
  cross-lane reduction ops** (no `addv`, no `vpaddq`).
- **DENSE dense-64 `vpaddq` tree** — for consumers needing a true **1-bit-per-byte** 64-bit
  mask over a 64-byte block feeding prefix-xor / structural CTZ-iteration / position compaction
  (`byte_class_from_table_64`, `structural_classify_64`, `byte_class_from_eq_set_64`, the
  `compact_mask`→`bulk_emit_positions_64` chain lib.rs:228). Four 16-lane compare vectors pack
  into 64 adjacent bits; the preferred reducer is a paired-add (`vpaddq`) tree across the four
  stripes — **not** four independent `vaddv_u8` double-addv reductions.

**Counterexample that makes the split load-bearing (F2/F3 warning):** a naive nibble-stride
(LEAN) form on a DENSE site yields a 256-bit nibble-strided result (4 bits × 64 bytes), so
`bit i ↔ byte i` is destroyed and every downstream `prefix_xor`/CTZ consumer misreads the
structural set. The dense-64 site therefore *cannot* be served by the LEAN producer; it needs
its own packer. This is why "one producer" is wrong and "two producers, authored once" is the
charter.

## 2 — The substrate already exists — and already drifts (the "author once" target)

bbnf-simd is stable-clean (`core::arch::aarch64`, **no `#![feature]`**, sole dep `blake3` for
parity hashing, Cargo.toml:10). It already carries the producers — but as **three divergent
copies plus a replicated dense-pack**, which is the drift 9A collapses:

| site | form | verdict |
|---|---|---|
| `aarch64/movemask.rs:5` | `vshrn_n_u16::<4>` **then a scalar `for pair in 0..8` loop + `vst1q` spill** (lines 17–24) | LEAN intent, **un-canonical** — spills to memory instead of `fmov x,d` |
| `byte_class_from_eq_set_64.rs:79-87` | bit-position `vandq` + **`vaddv_u8` of low/high halves** | **REFUTED double-addv** (PROVE-09 law) — must be purged |
| `structural_classify_64.rs:147` | third private `movemask_u8x16` copy + `lo0\|lo1<<16\|lo2<<32\|lo3<<48` (:138) | dense-pack, hand-replicated |
| `classify_tbl4.rs:59-63` | imports `movemask.rs` form, then per-lane `<<shift` OR | dense-pack, hand-replicated |

The DENSE `vpaddq`-tree producer is **absent** — every dense site hand-rolls `movemask×4 +
shift-OR`. Authoring it once (a `fn movemask_dense_64(uint8x16x4_t)->u64`) into
`aarch64/movemask.rs`, canonicalizing the LEAN form to the clean `shrn+fmov` shape, and
**deleting the eq_set double-addv copy (lines 79-87)** is the concrete "purge the refuted
reduction idiom there" the F1 directive names. Parity is gated by the extant checkasm harness
(CHECKASM-REPORT §d: `INJECT_BUG` +1-shift, alignment sweep 0..64, signal guard) — authoring is
admission-gated, not asserted.

## 3 — F1: residual one-home vs two-home divergence, enumerated

After authoring both producers once, the divergence between a single shared home and two
per-repo homes is **not the reducer** (it unifies to two functions) but three adapters:

1. **Padded-view / clamp convention (stays per-repo; ~0 kernel divergence).** parse-that pads
   the *whole buffer* once: `PaddedView`, `INPUT_PAD_BYTES=64`, trailing 64 NUL
   (`state.rs:57,122`) so every stripe load skips a tail guard. bbnf-simd clamps the *final
   partial block* into a zero-padded `[u8;64]` (`eob_pad_clamp`, `EobBlock`, lib.rs:276). Both
   hand the kernel a valid `&[u8;64]`; the kernel core is **agnostic to which fed it**. The
   producer is shared; the two tail harnesses stay idiomatic per repo. Divergence residual =
   the tail adapter only, and it is *inherent* — collapsing it forces one repo onto the other's
   whole-buffer-vs-partial-block memory model.
2. **Single- vs multi-table shuffle (stays two fronts; ~40–80 LOC each).** parse-that classify
   is **two-table shufti**: `lo`/`hi` `u8x16` LUTs + `swizzle_dyn`×2 + `simd_ne(splat0)`
   (`structural_bitmap.rs:37-43`, `ws_comment.rs:72-81`). bbnf-simd is **four-table lo6**:
   `vqtbl4q_u8` over a `low6` index (`classify_tbl4.rs:31-32`), admissible only when the
   alphabet is 6-bit-collision-free (`dispatch.rs:101` `lo6_table_admissible`). The **movemask
   unifies; the classify front does not** — the honest residual: one home collapses the reducer
   beneath both, not the shufti-vs-lo6 front. Both fronts valid; drift cost lives in the front.
3. **Capacity constants (already convergent).** Both size the position sink at `len/8+8`
   (bbnf-simd lib.rs:107; parse-that mirrors). STRIPE=64 both. bbnf-simd's
   `bulk_emit_positions_64 + set_len` (lib.rs:236) is strictly tighter than parse-that's
   per-structural `Vec::push` — adopting it is a *win*, not a divergence.

**Basis:** one-home wins on the reducer (one definition ⇒ the eq_set-refuted / movemask.rs /
structural_classify triple-copy can never re-diverge, and the checkasm gate covers one body,
not three). Two-home wins nowhere structural — its only "advantage" is avoiding a cross-repo
patch edge, already paid (§4). **Verdict-neutral recommendation: one home**; the residual (tail
adapter + classify front) is inherent and stays per-repo *inside* the one-home layout.

### The non-inlinable boundary, named precisely (F1 sub-q)

Cross-crate `#[inline]` is measured-live 0 in-loop indirect — **but only for the cfg-direct
entry shape.** bbnf-simd exposes producers **two ways**, differing categorically:

- **Cfg-direct `#[inline]`** (`prim::byte_class_from_eq_set_64` lib.rs:283-289;
  `structural_classify_64`, `comment_body_mask_64`, `bracket_depth_mask_64`) — a
  `#[cfg(target_arch="aarch64")] return neon(...)` + `#[allow(unreachable_code)] scalar(...)`.
  `#[inline]` fns serialize MIR into rlib metadata, so they inline across the crate seam
  **regardless of LTO or cgu count**. This is the shape parse-that hot loops must consume.
- **`OnceLock` fn-pointer table** (`PrimitiveKernels`, `dispatch.rs:50-61`; `primitive_kernels()`
  gates `byte_class_from_table_64`, `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `eob_pad_clamp`). **Runtime indirect calls by construction** — a
  `fn(...)` loaded from a `OnceLock<struct>`. **Not inlinable at any LTO/cgu setting**; each
  block eats an indirect `blr`.

So the non-inlinable boundary is not a toolchain corner — it is **the fn-ptr dispatch table
itself**. A plain (non-`#[inline]`) `pub fn` *also* becomes a real `bl` under
`[profile.dev]`/`ax-iter` (lto=false, cgu>1) and inlines only under the bench profile's fat-LTO
(Cargo.toml:10 `lto="fat"`). **The fn-ptr table is unfixable by LTO; the non-`#[inline]`
boundary is fixable by `#[inline]`.** Because refutation-law iv kills x86, the runtime
scalar↔NEON swap the `PrimitiveKernels` table serves is **unnecessary on the aarch64-only
target** — deleting it in favor of the cfg-direct `#[inline]` shape is both faster (no `blr`)
and inlinable cross-crate. Concrete target: `dispatch.rs:50-87` → replicate the lib.rs:283-289
cfg-direct pattern for the five table-dispatched kernels.

### Scanner-file mapping census (F1 owed)

Nine SIMD scanner files (R3 §2a + digits):

| file | LOC | class | maps to |
|---|---|---|---|
| `structural_bitmap.rs` | 282 | **1:1-kernel** | `byte_class_from_table_64` / `structural_classify_64` (DENSE) |
| `quoted_simd.rs` | 189 | **1:1-kernel** | `escape_mask_64` (lib.rs:174) / odd-parity |
| `quote_parity.rs` | 210 | **1:1-kernel** (prefix-xor) | `bitmap_prefix_xor_64` (lib.rs:261) |
| `decode.rs` | 405 | **1:1-kernel** (find part) | LEAN find + `bulk_emit`; unescape stays repo-local |
| `scanners.rs` | 370 | vector-tier | LEAN producer + 2-table shufti (front repo-local) |
| `ws_comment.rs` | 306 | vector-tier | LEAN producer + shufti front |
| `span_parser/scan.rs` | 184 | vector-tier | LEAN producer (enum-dispatch twin of scanners.rs) |
| `state.rs` | 492 | **no-go** | harness (`PaddedView`), not a kernel |
| `number_simd.rs` | 267 | **no-go (REFUTED)** | refutation-law iv: no digit-scan SIMD — scalarize/delete |

**Split: 4 kernel-tier / 3 vector-tier / 2 no-go.** Kernel-tier sites *become calls* into the
shared home; vector-tier sites *import the LEAN producer* and keep their repo-local classify
front; no-go sites are excluded (harness) or deleted (refuted digit SIMD).

## 4 — Substrate & sync gate (concrete)

parse-that's `.cargo/config.toml` already patches four bbnf-lang siblings
(`bbnf`/`bbnf-ir`/`bbnf-ser`/`egraph` → `../../bbnf-lang/crates/*`, config.toml:8-17). Adding
the kernel home is **one line**: `bbnf-simd = { path =
"../../bbnf-lang/skinny/crates/bbnf-simd" }`. The vendoring mechanism (path-dep under the
gitignored patch table, R3 §1) exists and is load-bearing. bbnf-simd's only dep is `blake3`
(parity hash) — feature-gate it so the hot-kernel consumer pulls no hash surface. **Sync gate =
the checkasm parity harness** (CHECKASM-REPORT: `xtask primitive-checkasm`, `BBNF_SIMD_STRICT=1`)
run in *both* consumers' CI — one body, two callers, differential-gated.

## 5 — Convergence with F2 (de-nightly), and the untested lever

Adopting bbnf-simd producers **is** the parse-that de-nightly (F2): it removes `portable_simd`
(the PRIMARY nightly blocker, 7 files/~88 hits, R3 §2a) from every kernel/vector-tier scanner,
because bbnf-simd is already `core::arch::aarch64`. Left over: `smallbox` (LOCK-4 vtable,
droppable per PROVE-09) and the `@generated` `fmt_helpers_for_derive` residual (a LOCK-5 regen
fix, not a hand-patch). PROVE-09's `simd128.rs` was a **second 276-LOC home**; 9A supersedes it
— the producers already exist in bbnf-simd, so the shim is redundant.

**The lever PROVE-09 left on the table.** PROVE-09 measured its de-nightly **perf-neutral**
(VERIFY: parity ±15%) — *because its `simd128::to_bitmask` replicated the double-addv* (REPORT
line 23: `vand`(bit-positions)+`vaddv` halves), and otool showed **baseline portable_simd
`to_bitmask` = 509 `addv.8b`, shim = 2271 `addv.8b`** — the shim did *more* reduction work.
Neither arm used the LEAN `vshrn`. The **untested lever**: migrate the vector-tier `to_bitmask`
sites (double-addv, 509 `addv.8b`) to the bbnf-simd **LEAN `vshrn` producer** (`shrn.8b #4` +
`fmov`, **zero `addv`**). This is a reduction-op count the prototype can settle with `otool -tv`
alone, independent of the noisy wall-clock.

## 6 — What a prototype must test to move 9A

1. **Author `movemask_dense_64` (`vpaddq` tree) into `movemask.rs`, canonicalize the LEAN form
   to `shrn+fmov` (drop the `for pair in 0..8` spill, lines 17-24), delete the eq_set
   double-addv copy (lines 79-87).** Gate: `xtask primitive-checkasm` strict — the naive
   nibble-stride-on-dense counterexample (§1) must be *caught* by `INJECT_BUG` before it is
   fixed, proving the harness has teeth on the DENSE site.
2. **otool reduction-op census, base-pinned.** For each vector- + kernel-tier site, `otool -tv`
   the pre- (portable_simd `to_bitmask`) and post- (bbnf-simd producer) bench binaries; report
   `addv.8b` / `shrn.8b` / `blr` counts. Expected: LEAN sites 509→0 `addv`; kernel-tier sites
   lose the fn-ptr `blr` when consumed cfg-direct vs via `primitive_kernels()`.
3. **Cross-crate inline liveness.** Confirm the cfg-direct `#[inline]` producers show **0
   in-loop indirect** across the rlib seam under BOTH `[profile.bench]` (fat-LTO) and `ax-iter`
   (lto=false, cgu>1); confirm the `PrimitiveKernels` fn-ptr path shows a live `blr` under both
   — proving the boundary is the table, not the toolchain.
4. **Paired cold no-regression (PROVE-09 protocol, the law).** divan cold, N≥200, interleaved
   opposite-arm windows, load recorded; report base/patched ratio + best-min. The claim is
   **no-regression + reduction-op drop**, never a speedup (PROVE-09 refuted the speedup story);
   the win is isomorphism-physical + one checkasm body, not cyc/B.
5. **Classify-front residual measurement.** Quantify the LOC the shared movemask does *not*
   collapse (shufti two-table vs `vqtbl4q` lo6) to size the honest two-front residual §3.2.

## Prior art

- Lemire & Langdale, *Parsing Gigabytes of JSON per Second* / simdjson NEON stage-1 — the
  four-stripe classify + movemask spill; Langdale's `vshrn`-narrow 64-bit movemask is the LEAN
  idiom.
- `regex-automata` / `memchr` aarch64 backend — the `shrn.8b #4; fmov; & 0x8888…` LEAN form,
  disassembled live in PROVE-10 §2c; the memchr1/2/3 prefilter tier is the LOCK-5 sibling.
- dav1d `checkasm.h` — differential parity + alignment sweep + signal guard, mirrored in
  bbnf-simd `tests/checkasm_parity.rs` (CHECKASM-REPORT §b).
- Rust reference / `rustc` cross-crate inlining — `#[inline]` serializes MIR to rlib metadata
  (inlinable without LTO); non-`#[inline]` `pub fn` needs LTO; fn-pointers are never inlined.

## Named risks

- **DENSE producer is unbuilt** — the `vpaddq`-tree body is new; naive nibble substitution
  corrupts parity (§1). Mitigated by the checkasm strict gate as a *precondition*, not post-hoc.
- **fn-ptr table deletion touches runtime-dispatch semantics** — safe only because x86 is dead
  (law iv); on a hypothetical multi-µarch target the cfg-direct form loses runtime selection.
  Named, not hidden.
- **Two classify fronts persist** — the shared movemask does not collapse shufti-vs-lo6; a
  reviewer expecting "one home ⇒ one classifier" must be told the reducer unifies, the front
  does not (§3.2).
- **`blake3` dep bleed** — vendoring bbnf-simd whole pulls a hash crate into parse-that;
  feature-gate the parity/hash surface off the hot-kernel consumer.
- **smallbox + `@generated` residual are out of 9A scope** but block the *full* stable cutover
  (LOCK-4 / LOCK-5); 9A delivers the kernel-plane de-nightly only.

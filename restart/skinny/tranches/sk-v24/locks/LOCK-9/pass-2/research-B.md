# LOCK-9 Family 9B — NATIVE-REBIRTH (parse-that keeps its own kernels)

Thesis under development, alone: parse-that's scan/kernel layer is reborn *in-crate* as
idiomatic stable `core::arch::aarch64` NEON — no `bbnf-simd` dependency, no shared kernel home.
Each kernel gets a scalar reference oracle (executable spec) and a checkasm-liveness gate, and
the admission gate is *measured no-regression* under paired-interleaved cold divan (the refutation
law below). The distinctive mechanism is **movemask-by-downstream-contract**: the movemask
producer is chosen per kernel from its consumer's requirement, not from one uniform `to_bitmask`
vocabulary. This is what a vocabulary-preserving shim structurally cannot do, and it is where the
only honest value beyond stability lives.

## 1. The refutation that is law (the gate)

The proof-grade `std::simd`→NEON shim claimed 1.34–1.57× and was REFUTED on paired rebuild: it is
performance-neutral, and its `to_bitmask` emitted **2271 `addv.8b` vs baseline 509** — it did ~4×
the reduction work per mask because it repacked a `vand`(bit-position) + two `vaddv_u8` halves into
a dense u16 at *every* call site regardless of consumer (SPEC §0/09; the shim's `to_bitmask` is the
double-`vaddv` idiom (c)). The standing lessons that bind 9B:
- **Two-arm window asymmetry is the classic self-deception**; both arms must be built on one pinned
  toolchain/profile and run back-to-back in one window. The 9B gate makes NO "faster" claim; its
  value is *correct idiom + stable + x86-purged + measured no-regression*, with any small win
  (number scanner, §2) treated as a candidate the prototype must confirm paired, never asserted.
- The certified movemask is the **`vshrn` narrow idiom** (SPEC §0/03 CONFIRMED): a single
  `vshrn_n_u16::<4>(vreinterpretq_u16_u8(cmp))` yields a 64-bit value at **4-bit-per-lane stride**;
  its NEON lowering is disasm-verified in the prefilter hot path — `cmeq.16b` → `shrn.8b v0,v0,#4`
  → `fmov x8,d0` → `ands x8,#0x8888888888888888` (PROVE-10 REPORT §2c). That is 4 instructions to a
  usable mask; position = `trailing_zeros() >> 2`.

## 2. The heart: movemask-by-downstream-contract (canonical-producer question, answered)

There is **no single canonical producer**. The repack to dense 1-bit-per-lane is load-bearing for
exactly one downstream contract and dead weight for the other. Census of the parse-that scan layer
(`rust/parse_that/src/`, all sites verified) partitioned by consumer:

**LEAN contract** — the only consumer is one `trailing_zeros`/count; a 4-bit-stride mask suffices,
no repack:
- `parsers/scan/number_simd.rs:54-55` — `count_leading_digits_neon`: two `vaddv_u8` halves →
  `mask.trailing_zeros()`. This is idiom (c), refuted. Rebirth: `cmeq`→`vshrn_n_u16::<4>`→
  `vget_lane_u64`→`trailing_zeros() >> 2`. One narrow, zero repack, zero `vaddv`.
- `scanners.rs:208` `(!mask).to_bitmask().trailing_zeros()` (first non-ws), `scanners.rs:286`
  `is_match.to_bitmask().trailing_zeros()` (2-table swizzle find), `span_parser/scan.rs:152`
  `.to_bitmask().trailing_zeros()` (first excluded), `ws_comment.rs:150` single-`/` find.

**DENSE contract** — the mask is CTZ-iterated with bit-clear, shift-ORed into a u64 across four
16-byte chunks, or fed to bitmap algebra; 1-bit-per-lane is required:
- `parsers/scan/quote_parity.rs:81-82` `classify_stripe_64` → `prefix_xor` (`vmull_p64` carry-less
  multiply, `:54`) — the carry-less multiply *requires* a dense mask; the repack is load-bearing.
- `parsers/scan/structural_bitmap.rs:43` `classify_16` → `mask |= m << (k*16)` (`:57`), then
  `NibbleBitmapIter` CTZ-iterates + clears bits (`:187-220`) — dense required.
- `parsers/scan/decode.rs:82-83,189-190` and `quoted_simd.rs:45-46` (quote/backslash bits →
  `odd_parity_backslashes`/escape bitmap), `ws_comment.rs:81,247` (OR into u64) — dense required.

The instruction-count comparison the directive demands, at a real classify site (per-16-byte-chunk,
to a *usable* mask for the stated consumer):

| producer | ops to usable mask | consumer it fits |
|---|---|---|
| lean `vshrn` 4-bit-stride | `cmeq` + `shrn` + `fmov` + `ands`/`clz` ≈ **4** | LEAN (trailing_zeros/count) |
| refuted double-`vaddv` (c) | `vand`+`vand`+`vaddv`+`vaddv`+`orr` ≈ 5, **two full horizontal reductions** | none (idiom (c), refuted) |
| dense-64 `vpaddq` tree (b) | 4× `cmeq` + 4× `vand`(bit-weights) + `vpaddq` tree (2 levels) + `fmov` ≈ **~12 for all 64 bytes** | DENSE (prefix_xor / CTZ-iterate) |
| bbnf-simd `movemask_u8x16` repack | `vshrn`+`vand`+`vand`+`vsri`+`vorr`+`vcombine`+`vzip1q`+`vst1q` + 8-iter scalar loop ≈ **8 SIMD + ~24 scalar** | DENSE at 16-byte granularity (its `compact_mask` popcount) |

The lean producer eliminates the repack **only** for the LEAN contract; for the DENSE contract the
right producer is the simdjson dense-64 `vpaddq` bit-weight tree (idiom (b)) that assembles all four
16-byte compares into one dense u64 in a single reduction — never 4× (`to_bitmask` + shift + OR),
and never the 16-byte-granularity full-repack of `bbnf-simd/src/aarch64/movemask.rs:5-24` (whose
25-line `vand`/`vsri`/`vzip`/store + 8-iteration scalar loop is load-bearing only for *its own*
`compact_mask`/`bulk_emit_positions_64` popcount contract, `bbnf-simd/src/lib.rs:228-242`). Binding
each site to its contract is the mechanism; the call-site churn a uniform shim avoids is exactly
where the win is banked.

## 3. File-by-file census + x86 purge (mapping table)

| scanner file | movemask sites | contract | rebirth idiom | x86 arm to delete |
|---|---|---|---|---|
| `number_simd.rs` | `:54-55` | LEAN | `vshrn` 4-bit-stride | `:145-235` SSE4.2 + `is_x86_feature_detected` |
| `quote_parity.rs` | `:81-82` (`classify_stripe_64`) | DENSE | `vpaddq` tree; keep `vmull_p64:54` | `:42-49` pclmulqdq |
| `structural_bitmap.rs` | `:43` (`classify_16`) | DENSE | `vpaddq` tree (nibble-LUT via `vqtbl1q` pair) | none |
| `decode.rs` | `:82-83,:189-190` | DENSE | `vpaddq` tree | none (the 2 x86 tokens are comments `:168,:221`) |
| `quoted_simd.rs` | `:45-46` | DENSE | `vpaddq` tree | none |
| `ws_comment.rs` | `:81,:150,:247` | DENSE (81/247), LEAN (150) | mixed by site | none |
| `scanners.rs` | `:163,:208,:286` | DENSE (163), LEAN (208/286) | mixed by site | none |
| `span_parser/scan.rs` | `:152` | LEAN | `vshrn` 4-bit-stride | none |

Net: two files carry live x86 arms (`number_simd`, `quote_parity`); their excision is a pure
LOC reduction (~−90 / −30) and satisfies constitution iv (x86 dead) + SPEC §0. The `#[cfg(not(any(
aarch64,x86_64)))]` scalar `scan_digits_simd:239` becomes the *primary* scalar oracle, not a
dead-arch fallback. `state.rs:57` `INPUT_PAD_BYTES=64` / `PaddedView:122` (64-byte NUL tail witness)
is untouched — every rebirth kernel keeps the padded-load-no-tail-guard invariant.

## 4. Scalar-oracle + checkasm-liveness plane, native (no cross-repo coupling)

The oracles already exist as scalar arms and become the executable specs: `number_simd.rs:239`
(scalar digit scan), `quote_parity.rs:58-68` (5-shift-xor `prefix_xor`), `structural_bitmap.rs:76`
(`classify_tail`), `ws_comment`/`decode` scalar tails. 9B formalizes each NEON kernel as a
`_scalar` reference sibling + a differential sweep in `parse_that/tests/` (parse-that already ships
`quote_parity_test`(7), `number_simd_test`(12), `structural_bitmap_test`(6), `decode_test`(44),
`quoted_simd_test`(33), `ws_comment_test`(18) — R3 §7). The checkasm harness is reimplemented
natively (~200 LOC over parse-that's existing divan dev-dep): the FFmpeg `checkasm.h` / dav1d
shape — identical randomized src0/src1 clones, `call_ref`(scalar) vs `call_new`(NEON), post-call
`memcmp` clobber check, alignment sweep `0..64 × {1,16,32,64,128,1024,8192}`, a stack-clobber
canary, a `SIGSEGV/SIGBUS/SIGILL` guard, an outlier-robust mean, and a bug-injection env toggle that
proves the kernel is *live* (not DCE'd) — the exact set validated by the sibling
`bbnf-simd/tests/checkasm_parity.rs` (CHECKASM-REPORT §b), rebuilt independently.

Coupling ruling: the harness is dev/test-only, so sharing it would not violate 9B's *kernel*
no-coupling law — but the physical seam forbids even that. parse-that's `.cargo/config.toml`
`[patch.crates-io]` has **no `bbnf-simd` entry** (`bbnf-simd` is absent; R3 §1), parse-that is
READ-ONLY to the bbnf-lang campaign (non-clobber P23), and adding a path-dep on
`skinny/crates/bbnf-simd` is a parse-that mutation requiring a named cross-lock supersession — not
presumed granted. 9B therefore reimplements the harness natively; the ~200 LOC of test infra is the
price of zero cross-repo coupling, and it is cheap.

## 5. Cross-crate inline reality — 9B sidesteps it entirely

The kernel-entry lever (a `OnceLock` fn-pointer facade blocks `#[inline]` under fat-LTO; a direct
`#[cfg(target_arch="aarch64")]` entry ICF-folds inline) is a *cross-crate* hazard only when kernels
live in a separate crate reached through the `[patch]` path-dep. 9B keeps every kernel in the
`parse_that` crate, so `#[inline(always)]` is intra-crate and unconditional — no LTO dependency, no
facade. parse-that's scanners are **already** structured this way: `number_simd.rs:29-30`,
`quote_parity.rs:40-41`, `structural_bitmap.rs:36,48` are direct `#[cfg]`+`#[inline(always)]`
entries with zero fn-pointer indirection. The anti-pattern to avoid is exactly
`bbnf-simd/src/dispatch.rs:58-61` `primitive_kernels()` — a `OnceLock<PrimitiveKernels>` fn-pointer
table routing `byte_class_from_table_64`/`bitmap_prefix_xor_64`/`bulk_emit_positions_64` — which is
the facade the shared-facts note warns survives as an in-loop indirect branch. bbnf-simd is
*mixed*: `byte_class_from_eq_set_64`/`structural_classify_64`/`comment_body_mask_64` use direct
`#[cfg]` dispatch (`lib.rs:283-352`, ICF-folds) but the five `prim::` kernels go through the
`OnceLock` table. Native rebirth inherits none of that ambiguity: no runtime backend selection, no
table, one aarch64 body + one scalar oracle per kernel, `#[cfg]`-selected at compile time.

## 6. Divergence datum (9A ⟂ 9B substrate divergence, as data)

After a native rebirth of two idiomatically-opposite kernels — a position scan (idiom (a),
`number_simd` count) and the dense-64 stripe (idiom (b), `quote_parity classify_stripe_64`) — the
raw compare-and-narrow **core** is near-identical to bbnf-simd's (`cmeq.16b` + a narrow), ~2–4
intrinsic lines shareable. The **substrate** diverges large on three axes, and it is already
divergent at HEAD:

| axis | parse-that | bbnf-simd | divergence |
|---|---|---|---|
| tail model | `PaddedView` — one unconditional 64-byte NUL tail, type-witness threaded through `state.rs`, whole-buffer | `eob_pad_clamp`/`EobBlock` — per-tail-block copy into a padded `[u8;64]` (`lib.rs:276`, `dispatch.rs:2`) | opposite invariants; a shared kernel must abstract over both |
| table arity | `vqtbl1q` nibble-LUT **pair** (lo+hi `swizzle_dyn`, `structural_bitmap.rs:40-41`) | `vqtbl4q_u8` **4-table** classifier (`classify_tbl4.rs`) | single-table-pair vs quad-table |
| mask density | lean 4-bit-stride (LEAN sites) / dense-64 `vpaddq` (DENSE sites), **by contract** | dense-16 full repack (`movemask.rs`) for `compact_mask` popcount | producer chosen by consumer vs one dense repack |
| block granularity | 16-byte (number) and 64-byte (structural) | 64-byte uniform | mixed vs uniform |

Reading (both outcomes valid, no home verdict): the shareable core is ~4 instructions; the LOC and
the correctness surface all live in the substrate, which is already divergent (PaddedView vs
eob_pad_clamp is the sharpest — a shared home forces one repo's tail convention or an abstraction
seam that the direct-cfg inline entry must then punch back through). So 9B's *duplication* cost is
small (the ~4-instruction core), while a shared home's *reconciliation* cost is the whole substrate.
A differential-conformance corpus (parse-that's `data/json` + `data/css`, R3 §1) run against both
rebirths' scalar oracles is the datum a synthesis pass would weigh; 9B needs only its own half.

## 7. Extant-tranche consciousness — relation to PT-2 (named)

PT-2 (`docs/tranches/sk-v24/PT-2.md`) is the extant de-nightly tranche: W1 "The Stable Port"
(`portable_simd`→`core::arch::aarch64` across 8 scanners via a `simd/` directory-module; x86 arms
purged; movemask fixed at the narrow idiom; born-RED). 9B **re-cuts** PT-2 W1, it does not
contradict it. The named supersession: PT-2 W1 as written is a *vocabulary-preserving* port — "each
of the eight scanner sites swaps one `use` line `std::simd::prelude::*` → `crate::simd::*`" (PT-2
§W1) — which keeps the uniform `to_bitmask` dense semantics at every call site and then fixes the
movemask impl to `vshrn`. That still owes a repack wherever `to_bitmask` must return dense-u16 for a
LEAN consumer. 9B's amendment: **do not preserve the uniform vocabulary** — bind each site to its
contract (§2), so the LEAN sites (`number_simd`, `scanners.rs:208/286`, `span_parser/scan.rs:152`)
consume the 4-bit-stride mask directly (`trailing_zeros()>>2`, zero repack) and the DENSE sites get
the `vpaddq` tree. This is a strict superset of PT-2 W1's movemask fix, reached by embracing the
call-site churn PT-2 W1 avoids. PT-2 W2 (smallbox de-nightly, alloc-neutral) and W5 (guard rails:
stable toolchain, `[workspace.lints]`, aarch64-only CI, x86 structural-token gate) are inherited
unchanged.

## 8. Prior art (citations)

- **`vshrn` movemask trick** — D. Kutenin, "Bit Twiddling with ARM NEON: comparing pairs and
  packing" (2021): `vshrn_n_u16(x,4)` as the canonical ARM replacement for x86
  `_mm_movemask_epi8`, yielding a nibble-per-byte reduced mask; `trailing_zeros/4` for position,
  `count_ones/4` for popcount. Used verbatim by `regex-automata`/`memchr` aarch64 (BurntSushi) —
  the in-repo disasm proof is PROVE-10 REPORT §2c.
- **Dense-64 `vpaddq` bit-weight tree + quote-parity prefix-XOR** — Langdale & Lemire, "Parsing
  Gigabytes of JSON per Second" (VLDB J. 2019), simdjson stage-1: the bit-weight
  `{0x01,0x02,…,0x80}` AND + `vpaddq` reduction assembling four 16-byte compares into one dense
  u64, and the carry-less-multiply (`vmull_p64` on ARM) prefix-XOR quote parity that
  `quote_parity.rs` already implements.
- **checkasm differential harness** — FFmpeg `checkasm.h` and dav1d's `checkasm` (VideoLAN):
  randomized identical src buffers, ref-vs-new + memcmp clobber detection, alignment sweep, signal
  guard, outlier-robust timing — the shape `bbnf-simd/tests/checkasm_parity.rs` mirrors and 9B
  reimplements natively.
- **Instruction-count over reduction-count** — the refutation's own attack #2 (2271 vs 509
  `addv.8b`) is the governing negative datum; `vaddv`/`vpaddq` horizontal reductions are the
  expensive ops, so the count that matters is *instructions to a usable mask*, not reduction ops.

## 9. Risks & counterexamples

1. **Neutral, not faster.** The DENSE sites' `vpaddq` tree may be codegen-equivalent to the current
   4× (`to_bitmask` + shift + OR) once LLVM lowers `to_bitmask` well — the win there could be zero.
   Only the LEAN `number_simd` double-`vaddv`→`vshrn` change has a *mechanistic* reason to be
   cheaper (two horizontal reductions → one narrow); it may still be latency-bound and wash. The
   gate is no-regression, so a wash is a PASS, but claiming a win requires the paired measurement.
2. **`vshrn` 4-bit-stride edge semantics.** `trailing_zeros()>>2` assumes the compare wrote 0xFF/0x00
   lanes; a partial-lane compare (never happens for byte `cmeq`, but a future half-word compare
   would) would break the `>>2`. The scalar oracle + alignment sweep must cover the all-zero mask
   (`==0` → "no hit", currently `number_simd:59` returns 16) and the all-ones mask.
3. **Bit-injection liveness must actually fail.** Under fat-LTO the scalar oracle can be
   const-folded away if the harness feeds constant input; the injection toggle must use
   `read_volatile`/`black_box` inputs (bbnf-simd's `stack_clobber_then` pattern) or the liveness
   check is vacuous.
4. **`data/` corpus is gitignored, 124M** (R3 §1/§8) — the differential corpus must be sourced from
   the working tree copy, not a `git archive`, or the sweep runs on nothing.
5. **`fmt_helpers_for_derive` residual** (`regex/generated.rs`) is orthogonal to 9B's kernels but
   blocks the full `+stable` build; 9B's scope is the eight scanners, and it must state that the
   stable cutover is gated on a *generator* fix elsewhere (clean-regen), not a hand-patch.

## 10. What a prototype must test to move this family

1. **The LEAN win, paired.** Rebirth `number_simd.rs:29-61` to `vshrn` 4-bit-stride (delete
   `:145-235` x86). Build one binary; run `micro_parse_that` number lane + `parse_that_combinator`
   back-to-back against the pristine arm in one window, N≥200 cold, ratio + min-of-N. `otool -tv`:
   assert **zero `vaddv`** in the number scanner and the `shrn.8b …,#4` idiom present; no
   `addv.8b` regression vs baseline (the 2271-vs-509 counterexample must not reproduce).
2. **The DENSE producer, measured.** Rebirth `quote_parity classify_stripe_64` to the `vpaddq` tree;
   paired `quote_parity`/`structural_bitmap` micro-bench vs the current 4× `to_bitmask` assembly.
   Report whether the tree is a win, a wash, or a loss — a loss is a valid negative result that
   says "keep `to_bitmask` for DENSE, rebirth only LEAN."
3. **checkasm-liveness native.** Wire `number_simd` + `quote_parity` + `structural_bitmap` NEON
   kernels through a native `parse_that/tests/checkasm.rs` (scalar oracle + alignment sweep 0..64 +
   signal guard + injection toggle). Prove: default green, `INJECT_BUG` flags ≥90% of sweep cases,
   `STRICT` panics on any divergence.
4. **The divergence datum.** Rebirth exactly two idiomatically-opposite kernels; count LOC
   duplicated vs `bbnf-simd`'s equivalents and fill the §6 substrate table with measured LOC —
   feeding the 9A⟂9B home question with data, not a verdict.
5. **x86-zero + inline.** `rg -c 'target_arch *= *"x86|is_x86_feature|std::simd' parse_that/src`
   → 0 for the two rebirthed files; `otool -tv` shows the kernels inlined into their callers with
   no indirect branch (the intra-crate inline claim, §5).

# LOCK-9 · PASS-3 · FAMILY 9A — ONE-KERNEL-LIBRARY (research-A)

*One shared aarch64 kernel home serving BOTH repos; parse-that scanners become thin consumers;
the isomorphism is physical. Primary charter question: F1 (shared-vs-own kernel home & the
canonical producer surface). F2/F3/F4 touched only where they bear on the shared-home decision.*

Host substrate: `skinny/crates/bbnf-simd` 0.1.0 (aarch64 + scalar, blake3 dep only,
`crates/bbnf-simd/Cargo.toml`); parse-that scanners under `parse_that/src/{scanners.rs,
parsers/scan/*, span_parser/scan.rs}` (READ-ONLY). All line pointers verified live.

---

## 1. The mechanism made concrete — the canonical producer surface

The load-bearing artifact of 9A is not a build-system reshuffle; it is **one authored
contract-split movemask producer**, because that producer is *absent in correct form from both
homes* and is *triplicated in refuted form* in bbnf-simd. Author it once, purge the divergent
idioms, and the shared-vs-own question collapses to a small residual (§2).

### 1.1 The producer is absent-and-triplicated, empirically

**bbnf-simd carries THREE `movemask_u8x16` definitions, no two alike, none canonical:**

1. `src/aarch64/movemask.rs:4` — `vshrn_n_u16::<4>` narrow (the LEAN idiom's front half),
   then `vand`/`vorr`/`vsri` lane-shuffle, then `vst1q` to a `[u8;16]` and a **scalar
   `for pair in 0..8` reduction loop** (`:20-24`). Consumed by `classify_tbl4.rs:5,38-41,85`
   (the only *working* NEON classifier, `NeonTbl4` backend, `dispatch.rs:23-33`). The vshrn
   front is right; the scalar-loop tail throws the win away.
2. `src/aarch64/byte_class_from_eq_set_64.rs:79-86` — private duplicate: `vandq` bit-position
   `[1,2,4,…,128]×2` + **double `vaddv_u8`** (`:83-84`, lo/hi halves). This is *exactly* the
   idiom PROVE-09/VERIFY attack #2 refuted (2271 `addv.8b` vs a 509 baseline) and SPEC §0/03
   forbids ("vshrn narrow, never the double-addv movemask").
3. `src/aarch64/comment_body_mask_64.rs:68-74` — a THIRD private copy of the same double-`vaddv`
   idiom (`:72-73`).

Neither the LEAN 64-bit nibble-stride producer nor the DENSE 64-bit vpaddq-tree producer exists
in clean form. Worse, `src/aarch64/byte_class_from_table_64.rs:2-4` — the dense table classifier
NEON body — is a **scalar stub** that delegates to `crate::scalar::…::byte_class_from_table_64_scalar`;
likewise the 4-LOC "NEON" files `bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`,
`bulk_emit_positions_64.rs`, `eob_pad_clamp.rs` (`:4`) all delegate to scalar. bbnf-simd's
`primitive_kernels()` table (`dispatch.rs:66-74`) advertises "Neon" but four of five entries run
scalar bodies. This is a Y.13 hazard already present: advertised-but-dormant vector surface.

**parse-that produces the same masks ~10 sites, uniformly on portable_simd `to_bitmask`:**
`structural_bitmap.rs:43` (nibble-LUT `classify_16`, the producer feeding `find_next_structural_from`
+ `NibbleBitmapIter` + `ws_comment.rs:179`), `quote_parity.rs:81-82` (`classify_stripe_64`),
`quoted_simd.rs:45-46`, `decode.rs:82-83,189-190`, `scanners.rs:163,208,286`,
`ws_comment.rs:81,150,247`, `span_parser/scan.rs:152`. On the pinned nightly, portable
`to_bitmask` lowers to the **509-`addv.8b`** baseline (PROVE-09/VERIFY #2) — decent, but
nightly-locked and not the vshrn optimum.

### 1.2 The contract-split producer to author (once, into any home)

Two producers, TYPE-DISTINCT so a consumer cannot cross wires:

- **LEAN** `movemask_nibble_64(x0..x3: uint8x16_t) -> NibbleMask64`: per 16-byte lane,
  `vshrn_n_u16::<4>(vreinterpretq_u16_u8(cmp))` yields 8 bytes of 4-bit-per-lane stride;
  concatenate the four lanes → a 64-bit word where lane *i* occupies nibble *i*. Consumers that
  only need *first-match / presence* read `tz >> 2` for the index. Cost: **1 `vshrn` per 16
  lanes, 4 per 64-byte block, zero horizontal reduction.** This is simdjson's own aarch64
  `to_bitmask`/`neon_movemask` shape and the SPEC §0/03 CONFIRMED "+35% over scalar" spine.
- **DENSE** `movemask_dense_64(x0..x3) -> u64`: true 1-bit-per-byte mask for consumers that
  CTZ-*iterate* every hit (structural positions → `bulk_emit_positions_64`, `NibbleBitmapIter`).
  Build via `vandq` bit-position + a **`vpaddq_u8` log-reduction tree** (pairwise, `vpaddq`
  halving 16→8→…→1), NEVER a per-half `vaddv` scalar-widen. Reserve DENSE only for the ~2 true
  iterating consumers; everything else takes LEAN.

Purge all three bbnf-simd movemasks (`movemask.rs:19-24` scalar tail;
`byte_class_from_eq_set_64.rs:79-86`; `comment_body_mask_64.rs:68-74`) and re-point their four
call clusters at the LEAN/DENSE pair. Author real NEON bodies for the four scalar-stub primitives
in the same wave (they are the shared home's whole reason to exist).

---

## 2. Residual one-home vs two-home divergence (the F1 delta, enumerated)

After authoring the canonical producer into a **scratch shared home** and purging the refuted
idioms, the residual substrate divergence between one-home and two-home is small and concrete.
Both directions remain valid; the basis below is maintenance/drift cost.

| Axis | parse-that convention | bbnf-simd convention | one-home resolution | LOC/drift basis |
|---|---|---|---|---|
| **Padded view / clamp** | `PaddedView` zero-copy, `INPUT_PAD_BYTES=64` trailing NUL, `bytes.len()==len+64` (`state.rs:57,122`); stripes read *into* the pad, no tail copy | `eob_pad_clamp -> EobBlock` (a *copied* clamped 64-byte block; currently itself a scalar stub `eob_pad_clamp.rs:4`) | pick zero-copy padded-view (the hotter contract); express `EobBlock` as a *view* over the pad | ~30 LOC + one backing-length invariant threaded to both callers; two-home perpetuates the copy-vs-view seam |
| **Membership table** | nibble-LUT **pair** — `lo_lut[16]`+`hi_lut[16]`, 2× `swizzle_dyn`(`vqtbl1q`), arbitrary 256-subset (`structural_bitmap.rs:37-44`) | (a) lo6 **single** `vqtbl4q_u8` over `uint8x16x4_t`, ≤64 distinct low-6, admissibility-gated (`classify_tbl4.rs:31`, `dispatch.rs:101`); (b) `vceqq`-fan, **≤8** members (`byte_class_from_eq_set_64.rs:51-57`) | parameterize the classifier by separability (nibble-pair ∨ tbl4 ∨ eq-fan) — the same partition/selection F3 raises for classifier-emit | 3 membership representations with different alphabet caps; two-home freezes pt on nibble-pair, bbnf on tbl4/eq-fan forever |
| **Alphabet capacity** | arbitrary 256-subset | `set.len()≤8` (eq-set) / `≤64` low-6 (tbl4) | expose all three, dispatch by set shape at compile time | divergent expressible-alphabet ceilings; a shared kernel must admit the union |
| **Movemask** | portable `to_bitmask` (1-bit stride, 509-addv, nightly-locked) | 3 divergent hand idioms (2 refuted) | the LEAN/DENSE pair (§1.2) | **the single biggest convergence win**: ~120 LOC (LEAN+DENSE+parity) retires ~60 LOC of triplicated bbnf idiom + unforks ~10 pt `to_bitmask` sites |
| **Dispatch shape** | pure `#[cfg(target_arch)]` static + portable_simd | `OnceLock` fn-ptr table for 5 prims (`dispatch.rs:50-87`) + `#[cfg]`-static for 4 (`lib.rs:283-352`) | `#[cfg]`-static only (see §3) | the fn-ptr table is the ONE indirect layer; removable since parse-that is aarch64-only post-purge |

**Net:** the divergence that *survives* the canonical-producer authoring is (i) one padding
contract and (ii) one classifier-strategy parameterization. Everything else (movemask, the four
scalar stubs, the STRIPE=64 constant) collapses to shared code the moment the producer is written.
One-home saves the triplication and the ~10-site to_bitmask fork; two-home's only advantage is
zero cross-repo version coupling (§ risk R4). The basis favors **one-home** because the drift cost
is already *realized* (three divergent movemasks in ONE crate today) and would only compound across
two.

---

## 3. #[inline] across the crate seam — where a non-inlinable boundary actually appears

The cross-crate `#[inline]` across the parse-that↔bbnf-simd rlib boundary is measured-live at
0 in-loop indirect (given). The precise place a *non-inlinable* boundary DOES appear is the
`PrimitiveKernels` **`OnceLock` fn-pointer table** (`dispatch.rs:50-87`): `byte_class_from_table_64`,
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp` are each
reached through a `fn` pointer loaded from `KERNELS.get_or_init(...)` — a genuine indirect call the
optimizer cannot devirtualize regardless of cgu/LTO. This is *not* a crate-seam inline failure; it
is a deliberate runtime-select indirection. The `#[cfg]`-**static** kernels
(`byte_class_from_eq_set_64`, `structural_classify_64`, `comment_body_mask_64`,
`bracket_depth_mask_64`, `lib.rs:283-352`) are `#[inline]` + statically dispatched with no fn ptr
and DO cross the rlib seam at any cgu count / LTO mode.

Where would a *real* seam inline-failure appear? Only a producer that is (a) NOT `#[inline]`,
(b) NON-generic, (c) compiled in a distinct cgu with LTO `off`/`thin` — then the cross-rlib call
is a hard `bl`. Guard: the LEAN/DENSE producers are tiny; mark them `#[inline]` (as movemask
producers already are) → they always cross. **Consequence for physical isomorphism (9A):** since
parse-that is aarch64-only after the x86 purge (PT-2.W1/W5), the runtime fn-ptr table is
*unnecessary* — the shared home should be `#[cfg]`-static so it stays inline-clean into both
consumers. Removing the `OnceLock` table is a concrete one-home simplification (deletes
`dispatch.rs:58-87`), not merely a hygiene choice.

---

## 4. Scanner-file mapping census (F1 owed) — 4 kernel-tier / 3 vector-tier / 1 no-go

Of the eight portable_simd scanner files (R3 §2a):

**1:1-kernel (maps directly onto an existing/authored bbnf-simd primitive) — 4:**
- `parsers/scan/structural_bitmap.rs` (282 L) → `byte_class_from_table_64` / `byte_class_from_eq_set_64`
  producer + `bitmap_next_set_bit` for `NibbleBitmapIter`. The canonical producer's home file.
- `parsers/scan/ws_comment.rs` (306 L) → `byte_class_from_table_64` + `comment_body_mask_64`
  (bbnf-simd already has the latter, `aarch64/comment_body_mask_64.rs`).
- `parsers/scan/quoted_simd.rs` (189 L) → `bitmap_prefix_xor_64` + `escape_mask_64`
  (both already live in bbnf-simd `lib.rs:174-205,261-263`; `odd_parity_backslashes` ≡ `escape_mask_64`).
- `parsers/scan/quote_parity.rs` (210 L) → `bitmap_prefix_xor_64` (the `pclmulqdq`/`vmull_p64`
  prefix-XOR, `:41-68`); `classify_stripe_64` (`:72`) takes the LEAN producer.

**vector-tier (consumes the shared LEAN producer, keeps domain glue) — 3:**
- `scanners.rs` (370 L) → LEAN producer + `find_ascii_set_member64` (bbnf-simd `lib.rs:208`); ws/match scan.
- `parsers/scan/decode.rs` (405 L) → quote/bs LEAN producer shared; `store_chunk` (u8x16 unaligned
  arena write, `:227`) stays domain-local (JSON decode-to-arena, not a kernel).
- `span_parser/scan.rs` (184 L) → LEAN producer for the exclusion scan (`:152`).

**no-go — 1:**
- `parsers/scan/number_simd.rs` (267 L) → digit accumulation (`vaddv_u8` digit-sum `:54-55`, NOT a
  movemask) is SPEC §0/02 refuted digit-scan SIMD; keep scalar, PURGE the x86 arm
  (`_mm_movemask_epi8:163`, SSE4.2 `:145-235`). Never enters the shared kernel home.

`state.rs` is substrate (PaddedView), not a scanner — it governs the §2 padding axis.

---

## 5. Substrate & integration seams (path:line)

- **Shared home candidate**: `skinny/crates/bbnf-simd` — already consumed by `bbnf-bench`,
  `runtime`, and crucially **`parse-that-regex`** (`crates/parse-that-regex/Cargo.toml:10`),
  the vendored parse-that regex inside skinny. That dependency IS the physical-isomorphism seam:
  a shared kernel need not be a new crate — bbnf-simd already sits under a skinny→parse-that
  vendor edge. Extraction to a standalone repo under a sync gate is the alternative (9A "extracted
  or vendored"); the vendor path already exists, so the cheaper move is bbnf-simd-as-source with
  parse-that consuming via the vendor.
- **Producer authoring site**: new `bbnf-simd/src/aarch64/movemask.rs` (rewrite the 25-LOC file to
  the LEAN/DENSE pair); delete the two private duplicates
  (`byte_class_from_eq_set_64.rs:79-86`, `comment_body_mask_64.rs:68-74`).
- **Parity oracle substrate (already built)**: bbnf-simd `tests/checkasm_*.rs` — 14 checkasm parity
  tests + `classifier_parity.rs` + `corpus_parity.rs` + `aarch64_primitives.rs`, dav1d-style
  scalar-reference-is-spec (`lib.rs:244-248`). This is the ready-made differential-oracle home for
  the merged producer (F2's oracle need); parse-that has only scattered per-kernel tests
  (`quoted_simd_test.rs`, `number_simd_test.rs`) and no unified checkasm harness — a further
  one-home argument.
- **Classifier interface**: `bbnf-simd/src/classifier.rs` (`ClassifyResult{structural/quote/
  backslash/control _mask}`, `SimdClassifier` trait) is the natural typed face for the scanner
  consumers; `dispatch.rs:18-47` shows the `SelectedClassifier` wiring.

---

## 6. Prior art (citations)

- **Langdale & Lemire, "Parsing Gigabytes of JSON per Second" (simdjson, VLDB J. 2019)** —
  structural-bitmap stage-1 and the prefix-XOR quote-parity that `quote_parity.rs`/`escape_mask_64`
  implement. simdjson's **aarch64 backend `neon_movemask`** is the `vshrn_n_u16(#4)` paired-narrow
  over 4×16-byte lanes → 64-bit — i.e. the LEAN producer is SOTA-canonical, not novel.
- **Wojciech Muła, "SIMD-ized movemask on ARM NEON"** — the `vshrn` narrow vs multiply-add
  trade; the double-`vaddv`/multiply-add path (bbnf-simd's refuted idiom) is the one Muła shows is
  strictly dominated by `vshrn` for the mask-narrow case.
- **Geoff Langdale, "Bits to indices"** — the `vqtbl`-driven bulk position emit that
  `bulk_emit_positions_64` targets.
- **rust `std::simd` aarch64 `to_bitmask` lowering** — the 509-`addv` baseline; a reasonable but
  non-optimal path that the LEAN producer must not regress against (the no-regression law).
- **dav1d `checkasm`** — the scalar-reference-is-spec parity+liveness harness bbnf-simd already
  mirrors (`tests/checkasm_*`); the model for the merged producer's admission gate.
- **regex-automata byte-class equivalence** (`[u8;256]`→class) — the classifier the F3 census and
  the §2 membership-table axis touch; relevant if the shared home admits an emitted classifier.

---

## 7. Named risks & counterexamples

- **R1 — DENSE tree regresses to the refuted idiom.** A botched `vpaddq` reduction reintroduces
  the double-`vaddv` cost. *Counterexample present in-tree:* `byte_class_from_eq_set_64.rs:83-84`
  (2271-addv). *Test:* otool reduction-insn count per producer, base-pinned; DENSE must be a
  log2 `vpaddq` tree, LEAN must show `shrn`, neither may show a per-half `vaddv`.
- **R2 — nibble-stride index corruption.** A consumer CTZ-ing a LEAN mask without `>>2` reads
  4×-sparse positions. *Guard:* type the split (`NibbleMask64` vs `u64`) so a nibble mask cannot
  feed a 1-bit iterating consumer (`bulk_emit_positions_64`, `NibbleBitmapIter`).
- **R3 — padding-contract merge segfaults one side.** pt reads *into* a 64-NUL pad
  (`state.rs:57`); bbnf-simd `eob_pad_clamp` *copies*. A shared producer assuming zero-copy
  padded-view will over-read if a bbnf caller passes an unpadded slice. *Guard:* thread the
  `bytes.len()==len+64` invariant into both call graphs, or express `EobBlock` as a pad view.
- **R4 — name+version collision / source-of-truth.** Two crates already share `bbnf-regex 0.1.0`
  (F3); a shared `bbnf-simd 0.1.0` adds a second. The sync gate must name ONE source (bbnf-simd in
  skinny, consumed by parse-that via the existing vendor edge, is the least-coupling answer).
- **R5 — consumer-less substrate (Y.13).** bbnf-simd already ships advertised-but-scalar "NEON"
  kernels (`byte_class_from_table_64.rs:2-4` + the four 4-LOC stubs). Authoring more dormant
  producers repeats the violation. *Guard:* every producer lands with a live parse-that scanner
  call site in-wave; the four stubs get real bodies or are deleted.

---

## 8. What a prototype must test to move 9A

1. **Author the LEAN (`vshrn_n_u16::<4>`) + DENSE (`vpaddq_u8` tree) producer pair** into ONE
   scratch home; `otool -tv` reduction-insn count per producer, base-pinned — LEAN ≤ the 509-addv
   portable baseline; DENSE a proper log2 tree, zero double-`vaddv`.
2. **Rewire ONE 1:1-kernel file (`structural_bitmap.rs`) + ONE vector-tier file (`scanners.rs`)**
   to consume the shared producer; paired cold divan (2 windows, N≥200, opposite arm order) vs the
   portable_simd baseline — gate **≥0.98× every corpus, both windows** (the PROVE-09 no-regression
   law; name N, both windows, HEAD).
3. **Differential parity**: byte-equality oracle of the merged producer vs pt portable `to_bitmask`
   AND the bbnf scalar reference over 10k random 16-/64-byte vectors + INJECT_BUG + an alignment
   sweep (F2); confirm the LEAN/DENSE typed contract catches a nibble-stride miswire.
4. **Measure the one-home vs two-home LOC delta empirically**: producer LOC authored vs triplicated
   movemask + fork LOC purged; confirm the `OnceLock` fn-ptr table (`dispatch.rs:58-87`) is
   removable under aarch64-only so the shared home is `#[cfg]`-static.
5. **Cross-crate inline probe**: build skinny with bbnf-simd as an rlib into `parse-that-regex`;
   `otool` the hot loop under {cgu=16, thin-LTO} and {cgu=1, fat-LTO}; assert 0 in-loop `bl` to the
   `#[inline]` producer (the physical-isomorphism inline-cleanliness claim).

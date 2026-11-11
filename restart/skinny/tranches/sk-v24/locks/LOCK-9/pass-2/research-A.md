# LOCK-9 Family 9A — ONE-KERNEL-LIBRARY (pass-2 research)

One shared aarch64 kernel home serving BOTH repos; parse-that scanners become thin
consumers; the isomorphism is physical. This report develops that family alone: the
mechanism made concrete, the substrate that already exists, the integration seams as
path:line, the divergence data, the canonical-movemask adjudication, named risks, and the
exact prototype that moves the family. No home verdict is drawn — both outcomes are left
open per the constitution.

---

## 1. The mechanism, made concrete

The shared home already exists and is nearly a superset of parse-that's scan/kernel surface.
`skinny/crates/bbnf-simd` (1226 aarch64 LOC + a parallel scalar reference tree + a checkasm
parity suite) carries fourteen distinct kernel responsibilities; parse-that's eight scanner
files (2705 LOC incl. `state.rs`) express roughly fourteen kernel responsibilities of which
~13 already have a bbnf-simd counterpart. 9A's mechanism is therefore not "build a shared
library" but "collapse parse-that's hand scanners onto the extant checkasm-gated one, made
physical through the patch table."

Concrete steps:
1. **Physical seam.** parse-that's `rust/.cargo/config.toml [patch.crates-io]` already
   path-deps `bbnf`, `bbnf-regex`, `csp-solver` at the sibling repo (R3 §1). `bbnf-simd` is
   ABSENT. 9A adds one line: `bbnf-simd = { path = "../../bbnf-lang/skinny/crates/bbnf-simd" }`.
   **This is a parse-that mutation (P23 non-clobber; parse-that is READ-ONLY to the campaign)
   — it requires a named cross-lock supersession and is executed owner-side or in a scratch
   copy (R3 §8). Reported, never presumed granted.**
2. **De-facade the primitive entries.** bbnf-simd's `dispatch.rs` today ships BOTH dispatch
   idioms; only one inlines (see §3). The `OnceLock<PrimitiveKernels>` fn-pointer table
   (`dispatch.rs:50-61`) fronts five kernels (`byte_class_from_table_64`,
   `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp`)
   through an indirect call `(primitive_kernels().byte_class_from_table_64)(src, table)`
   (`lib.rs:257`). The other four (`byte_class_from_eq_set_64`, `structural_classify_64`,
   `comment_body_mask_64`, `bracket_depth_mask_64`) dispatch by direct
   `#[cfg(target_arch="aarch64")] return crate::aarch64::…` (`lib.rs:285-352`). For a physical
   thin-consumer to pay zero cost the five faced kernels must move to the direct-cfg form —
   the exact lever the shared-facts note names.
3. **Scanner-thinning.** Each parse-that scanner's `use std::simd::prelude::*` + hand body is
   replaced by a `bbnf_simd::prim::*` call; the scanner keeps only its orchestration
   (cursor, carry, arena append), delegating classification/movemask/compaction to the home.

---

## 2. Mapping census — the 8 hot scanner files

Classification: **K** = 1:1-to-existing-kernel; **V** = vector-tier (SIMD but arena/state
coupled, not a pure classify→mask kernel); **NO_GO** = refutation-barred or pure orchestration.
Split ≈ **K-tier dominant in 5 files, V-tier in 2, one NO_GO core.** Deciding NEON body cited.

| # | file (LOC) | tier | bbnf-simd counterpart | deciding body |
|---|---|---|---|---|
| 1 | `scanners.rs` (370) | K + NO_GO | `byte_class_from_eq_set_64` (≤8 set) / `classify_tbl4`; WS→`byte_class_from_table_64` | `find_first_of_nibble_lut:277-291` (nibble-LUT `swizzle_dyn`+`to_bitmask`+CTZ). memchr1/2/3 dispatch + AhoCorasick/DFA caches (`:35-80`) = NO_GO orchestration |
| 2 | `decode.rs` (405) | K + V | quote/bs classify→`string_block::scan_string_special_block`; `\uXXXX`→`unescape_uxxxx`; store→`digit_mac`-adjacent V | `simd_scan_or_decode:80-90` (`simd_eq`→`to_bitmask` quote/bs). `store_chunk` (u8x16 unaligned store→`&mut Vec<u8>` arena) + `owned_decode` = V, arena-coupled |
| 3 | `ws_comment.rs` (306) | K | classify→`classify_stripe`/`byte_class_from_eq_set_64`; `/`-scan→`comment_body_mask_64` | `:81` `matched.simd_ne(splat0).to_bitmask() as u16`; `:150,:247` `simd_eq('/')→to_bitmask` |
| 4 | `structural_bitmap.rs` (282) | K (near-perfect 1:1) | `scan_dispatch`+`byte_class_from_table_64`+`compact_mask`+`bulk_emit_positions_64` | `classify_stripe:49-59` (4×nibble-LUT chunk→`u64`); `find_next_structural_from` CTZ loop — this IS simdjson stage-1, maps directly |
| 5 | `quoted_simd.rs` (189) | K | `odd_parity`→`bitmap_prefix_xor_64`; classify→`string_block`; escape→`escape_mask_64` (`lib.rs:174`) | `:45-46` `simd_eq→to_bitmask` quote/bs; `odd_parity_backslashes` prefix-XOR |
| 6 | `span_parser/scan.rs` (184) | V | inverted set-exclusion≈`byte_class_from_eq_set_64` | `:152` `is_excluded.to_bitmask().trailing_zeros()` — coupled to enum-dispatch span machinery |
| 7 | `quote_parity.rs` (210) | K (idiom b) + NO_GO | dense-64 classify→`structural_classify_64`; `prefix_xor`→`bitmap_prefix_xor_64` | `classify_stripe_64:72-87` (dense `simd_eq`→`to_bitmask`, idiom (b)); `pclmul/vmull_p64 prefix_xor:41-56`; `filter_quote_parity` `vec![0u64;nwords]` alloc = NO_GO |
| 8 | `number_simd.rs` (267) | **NO_GO** (SPEC §0/02) | — (digit-scan SIMD is refuted) | `count_leading_digits_neon:54-55` — **the double-`vaddv` movemask, the refuted idiom, LIVE**; `accumulate_digits_neon` Mula/Lemire = NO_GO. bbnf-simd `digit_mac.rs` exists but is likewise SPEC-barred for digit accumulation |

(`state.rs`, 492 LOC — no live SIMD, comments only; it is the `PaddedView`/`INPUT_PAD_BYTES=64`
SUBSTRATE, §4.)

**K-tier / V-tier split:** the classify→mask→CTZ→emit spine (files 3,4,5, half of 2, core of
7) is fully K-tier and already homed. The arena-append and enum-dispatch surfaces (store_chunk,
owned_decode, span scan) are V-tier — SIMD but fused to parse-that-local state, the honest
residue that stays hand-written even under 9A. number_simd is NO_GO by refutation law.

---

## 3. Cross-crate inline reality

**The lever, in bbnf-simd itself.** The shared-facts kernel-entry lever is not hypothetical —
`dispatch.rs` instantiates both sides. The `prim::byte_class_from_table_64` path
(`lib.rs:256-258`) loads a fn pointer from `OnceLock<PrimitiveKernels>` and calls it
indirectly; under fat-LTO this indirect branch survives in-loop (blocks `#[inline]`). The
`prim::byte_class_from_eq_set_64` path (`lib.rs:283-289`) is a direct
`#[cfg] return crate::aarch64::…_neon(...)` that ICF-folds to the inline body. Any 9A physical
consumption of the five faced kernels inherits the indirect branch unless they are de-faced.

**What is measurable today, and what is not.** The directive asks for `otool -tv` proof that
fat-LTO carries `#[inline]` across the `[patch]` path-dep on a LIVE hot scanner with
zero-indirect-in-loop. **This cannot be measured yet:** bbnf-simd is absent from parse-that's
patch table, and parse-that is READ-ONLY, so no live cross-crate consumption exists. The honest
measurable surface is two-fold: (a) bbnf-simd-internal — the OnceLock `prim::` path vs the
direct-cfg path, disassembled at a monomorphized call site; (b) an isolated probe crate
path-dep'ing bbnf-simd under `[profile].lto="fat"`. Building the true datum REQUIRES first
materialising the physical seam in a scratch parse-that (R3 §8) — which is itself the family's
first prototype move. This is a named limitation, not a gap in the mechanism.

**Corroborating asm datum (this session, `cargo rustc --release -p bbnf-simd --emit asm`,
nightly-2026-04-11).** The non-monomorphised lib residual carries `shrn.8b ×4`, `addv.8b ×2`,
`tbl.16b ×1`, `cmeq.16b ×4`, `fmov ×4` — i.e. BOTH the lean `shrn`-narrow idiom AND the
double-`vaddv` idiom are live in the same crate's codegen simultaneously (§5). The faced
kernels emit no standalone symbol here precisely because no in-crate consumer monomorphises
them — the very absence a physical consumer would fill.

---

## 4. Divergence datum — two idiomatically-opposite kernels reborn

Idiom (a) position scan: `byte_class_from_eq_set_64_neon` (parse-that `find_first_of` /
`structural_bitmap::classify_stripe`). Idiom (b) dense-64 stripe: `structural_classify_64` /
parse-that `quote_parity::classify_stripe_64`. Substrate divergence, measured:

| axis | parse-that form | bbnf-simd form | divergence |
|---|---|---|---|
| tail/padding | `PaddedView` bakes `INPUT_PAD_BYTES=64` NUL tail into the view TYPE (`state.rs:57,122`); final stripe reads into pad, mask-clamped | kernels take `&[u8; 64]` fixed blocks; tail handled EXTERNALLY by a separate `eob_pad_clamp` kernel → `EobBlock` | REAL but reconcilable (both express "read full 64 past end safely"); homes differ (type-level vs factored kernel) |
| table strategy | nibble-LUT: `swizzle_dyn`=`vqtbl1q_u8`, single 16-byte table, TWO lookups (lo/hi nibble), ≤8 targets | `classify_tbl4`: `vqtbl4q_u8`, 64-byte lo6 table (`vld1q_u8_x4`), ONE lookup, any alphabet with distinct low-6-bits | genuinely different; bbnf-simd's is a SUPERSET (richer admission via `lo6_table_admissible`, `dispatch.rs:101`) |
| mask producer | `to_bitmask` (portable_simd) | `movemask_u8x16` vshrn+repack (`movemask.rs`) — divergent from the eq_set-local double-`vaddv` (§5) | producer is itself unsettled inside the home |
| parity apparatus | none (bare hand kernel) | scalar reference = executable spec + checkasm diff (`tests/checkasm_*.rs`, `CHECKASM-REPORT.md`) | bbnf-simd carries the whole dav1d-style oracle; parse-that carries none |

**LOC-duplicated.** parse-that's two idioms: `classify_16`+`classify_stripe` ≈ 25 LOC (a),
`classify_stripe_64` ≈ 16 LOC (b) = ~41 LOC. bbnf-simd's: `classify_tbl4` 104 LOC (a, richer:
four output masks) + `structural_classify_64` 155 LOC (b) = 259 LOC. The divergence is LARGE in
raw LOC — but the delta is almost entirely the parity/scalar-reference apparatus and
grammar-parameterisation, NOT the NEON body (both (a) bodies are ~10 intrinsics; both (b) bodies
are a `vceqq` fan). A shared differential-conformance corpus already exists as the substrate for
the datum: bbnf-simd `tests/corpus_parity.rs` + `classifier_parity.rs` + the checkasm suite.
Both outcomes stay valid: a native rebirth in ONE home costs ~260 LOC (with oracle) vs ~41 (bare)
— the "large divergence" reading is true by LOC, the "near-identical" reading is true at the
intrinsic-body level. No home verdict.

---

## 5. Canonical movemask producer — the crux (newly opened)

Three producers narrow a 16-lane compare to a mask:

1. **bbnf-simd `movemask_u8x16` (`aarch64/movemask.rs:3-25`) — dense-u16 via `vshrn`+repack.**
   `vshrn_n_u16::<4>` narrows to a nibble-per-lane, THEN repacks to dense 1-bit-per-lane:
   `vand(0x11)`, `vand(0x01)`, `vsri_n_u8::<3>`, `vorr`, `vcombine`, `vzip1q`, a 16-byte
   `vst1q_u8` store, and an **8-iteration scalar `for` loop** reading the stored array. Heavy.
2. **The lean `shrn.8b→fmov→4-bit-stride` no-repack form** (PROVE-10 REPORT disasm, memchr NEON
   hot path): `cmeq.16b; shrn.8b v0,v0,#4; fmov x8,d0; ands x8,#0x8888888888888888`. Produces a
   64-bit value at 4-bit-per-lane stride, consumed by `tz>>2` index arithmetic. NO repack.
3. **parse-that `to_bitmask` sites** (17, censused: `scanners.rs:163,208,286`;
   `structural_bitmap.rs:43`; `decode.rs:82,83,189,190`; `quoted_simd.rs:45,46`;
   `quote_parity.rs:81,82`; `ws_comment.rs:81,150,247`; `span_parser/scan.rs:152`). portable_simd
   `Mask::to_bitmask`; PROVE-09 VERIFY attack #2 measured its shim variant at **2271 `addv.8b`
   vs 509 baseline** — the double-`vaddv` reduction tree, refuted.

**Is the repack load-bearing?** Yes, under the current architecture — and that is the whole
finding. Downstream, `classify_tbl4::classify_block_from_table:59-63` packs FOUR dense-16 masks
into a contiguous dense-64 (`result.structural_mask |= u64::from(structural) << (lane*16)`), and
`compact_mask`/`bulk_emit_positions_64`/`bitmap_next_set_bit` CTZ-iterate assuming **bit i ↔ byte
i** (dense 1-bit-per-lane), so `mask.trailing_zeros()` yields a byte offset directly. A lean
nibble-stride producer (form 2) CANNOT compose 4×16 chunks into one u64 (4-bit stride × 64 lanes
= 256 bits) and forces per-16 `tz>>2` consumption — a different consumer contract. So dense-16 IS
required by the four-chunk composition; the repack is not gratuitous.

**But the producer is over-costed and the home is internally inconsistent.** The alternative that
obviates BOTH the per-16 repack AND the shift-OR is idiom (b): the **dense-64 `vpaddq` reduction
tree** (simdjson `neonmovemask_bulk`) narrows all 64 bytes to a dense-64 mask in one tree, no
per-16 repack. Critically, bbnf-simd already contains a THIRD, REFUTED producer inside the same
crate: `byte_class_from_eq_set_64.rs:79-87` hand-rolls a LOCAL `movemask_u8x16` using the
bit-position `vand` + double `vaddv_u8` halves — byte-for-byte the PROVE-09-refuted idiom the
directive flags at `number_simd.rs:54-55`, now living in the SHARED KERNEL HOME. So the canonical
producer is unsettled: `movemask.rs` (vshrn+scalar-loop repack) and `eq_set:79` (double-vaddv)
disagree, and the lean shrn-4bit and the dense-64 vpaddq tree are both unadopted. My asm census
(`shrn.8b ×4` AND `addv.8b ×2` in one lib) confirms the coexistence.

**Instruction-count comparison — what must be produced.** The `movemask.rs` repack's real
lowering is UNCERTAIN: the source shows store+8-iter-scalar-loop, yet the residual asm shows no
store/loop (LLVM may fold it, or the fn wasn't monomorphised here). The prototype MUST
disassemble all three producers at a real classify site (e.g. `structural_classify_64`) and
report INSTRUCTION counts (not reduction-op counts): repack-dense-16×4+shift-OR vs
dense-64-vpaddq-tree vs lean-shrn-4bit. Only that adjudicates the canonical producer, and it
simultaneously purges the refuted `eq_set:79` double-vaddv from the home.

---

## 6. Substrate + the sync gate

`bbnf-simd` is a workspace member of `skinny` (`skinny/Cargo.toml:11`), dep only on `blake3`
(parity hashing) — a clean, near-zero-dep home. Structure: `src/aarch64/` (NEON bodies),
`src/scalar/` (executable-spec references, one per kernel), `src/dispatch.rs` (backend select),
`tests/checkasm_*.rs` (dav1d-style differential parity: scalar reference is the spec, NEON is
parity-checked). The sync-gate mechanism 9A needs is already modelled by this repo's csp-solver
discipline (git log: "re-vendor src/ from csc411@… under the sync gate", canonical-diff commits):
bbnf-simd is authored once in skinny, consumed read-only by parse-that via the patch table, and a
canonical-diff gate keeps any vendored copy byte-identical. The checkasm suite is the isomorphism
proof — run in BOTH repos against the SAME scalar reference, the physical isomorphism is a passing
differential corpus, not a code-shape assertion.

---

## 7. Integration seams (path:line)

- Physical seam: `parse-that/rust/.cargo/config.toml` `[patch.crates-io]` (R3 §1) — add
  `bbnf-simd` line; P23 supersession required.
- De-facade target: `bbnf-simd/src/dispatch.rs:50-61` (the `OnceLock` table) → mirror the
  direct-cfg form of `bbnf-simd/src/lib.rs:285-352`.
- Thin-consumer swaps: the 8 files of R3 §2a, each `use std::simd::prelude::*` →
  `bbnf_simd::prim::*`; the 17 `to_bitmask` sites (§5) → `bbnf_simd::prim` mask producer.
- Near-1:1 landing: `structural_bitmap.rs:49` `classify_stripe` → `bbnf_simd::scan_dispatch` +
  `byte_class_from_table_64` + `compact_mask` (`lib.rs:105-242`).
- Padded-input reconcile: `parse_that/src/state.rs:57,122` (`PaddedView`) ↔
  `bbnf-simd/src/aarch64/eob_pad_clamp.rs` + `scalar/eob_pad_clamp.rs`.
- Canonical producer loci: `bbnf-simd/src/aarch64/movemask.rs:3`,
  `byte_class_from_eq_set_64.rs:79` (refuted double-vaddv to purge), the lean form from
  PROVE-10 REPORT disasm.

---

## 8. Prior art (citations)

- **simdjson** (Langdale & Lemire, *Parsing Gigabytes of JSON per Second*, VLDBJ 2019):
  stage-1 structural classification, `neonmovemask_bulk` dense-64 `vpaddq` tree (idiom b),
  prefix-XOR quote-parity — the direct ancestors of files 4/7. The 64-byte-block dense-mask
  architecture is theirs.
- **dav1d** checkasm harness: scalar reference as executable spec, SIMD parity-diffed — the
  exact model bbnf-simd's `tests/checkasm_*.rs` adopt (the admission gate).
- **Lemire, `vshrn` movemask** (Software: Practice & Experience 2022, "arm neon movemask"):
  the `vshrn_n_u16::<4>` narrow is the SOTA aarch64 movemask, superseding the `vand`+`vaddv`
  bit-position form — the empirical basis for preferring `movemask.rs` over `eq_set:79`.
- **memchr 2.5** aarch64 NEON backend (BurntSushi): the lean `shrn.8b→fmov→4-bit-stride`
  no-repack consumption (PROVE-10 disasm) — the form-2 reference.
- **rustc/LLVM ICF + fat-LTO**: identical-code-folding across crate boundaries is the
  mechanism by which a direct-cfg entry inlines and an `OnceLock` fn-pointer does not
  (the shared-facts lever).

---

## 9. Named risks & counterexamples

- **R-A1 — the P23 wall.** The physical seam mutates read-only parse-that. Counterexample: any
  9A landing that edits `parse-that/rust/.cargo/config.toml` without a named supersession
  violates the constitution. Guard: scratch-copy prototype only; the real edit is owner-side.
- **R-A2 — the facade tax survives sharing.** If the five faced kernels ship as-is, the physical
  consumer pays an indirect branch per hot-loop kernel call — negating "zero-cost thin consumer."
  Counterexample: `otool` of the shared `byte_class_from_table_64` shows `blr` in-loop. Guard:
  de-facade to direct-cfg BEFORE the consumer measures.
- **R-A3 — the refuted producer rides along.** `eq_set:79`'s double-`vaddv` is already in the
  home; sharing propagates the refuted idiom into parse-that's hot path. Counterexample: PROVE-09
  VERIFY's 2271-vs-509. Guard: §5's instruction-count adjudication purges it as a precondition.
- **R-A4 — divergence is understated by intrinsic count, overstated by LOC.** A "kernels are
  near-identical" claim from body-level similarity ignores the PaddedView-vs-eob_pad_clamp and
  vqtbl1q-vs-vqtbl4q substrate splits; a "kernels diverge hugely" claim from LOC ignores that the
  delta is oracle apparatus. Guard: report BOTH axes as data (§4), draw no verdict.
- **R-A5 — number_simd cannot be shared.** SPEC §0/02 bars digit-scan SIMD; `digit_mac.rs` in the
  home is itself refutation-adjacent. Counterexample: a 9A census that lists number_simd as
  K-tier. Guard: it is NO_GO — the shared home must NOT absorb it as a live kernel.

---

## 10. What a prototype must test to move this family

1. **Materialise the physical seam** in a scratch parse-that (R3 §8): add the `bbnf-simd`
   patch-table line, thin ONE near-1:1 scanner (`structural_bitmap.rs`) onto
   `bbnf_simd::{scan_dispatch, byte_class_from_table_64, compact_mask}`.
2. **Cross-crate inline datum:** `otool -tv` the thinned scanner's hot loop under
   `[profile.bench] lto="fat"` — prove zero indirect-branch-in-loop with the de-faced entry, and
   demonstrate the `OnceLock`-faced entry regressing to a `blr` (the lever, measured live).
3. **Canonical movemask instruction-count** at `structural_classify_64`: disassemble
   repack-dense-16×4+shift-OR (`movemask.rs`) vs dense-64 `vpaddq` tree vs lean shrn-4bit vs the
   refuted double-`vaddv`; report instruction counts, adopt the winner, purge `eq_set:79`.
4. **Differential-conformance run:** the thinned parse-that scanner vs its hand original over the
   bbnf-simd checkasm corpus + parse-that's ~400 tests — byte-identical structural positions
   (parity hash `lib.rs:158`), the physical-isomorphism proof.
5. **No-regression gate** (PROVE-09 law): paired cold divan, two windows opposite arm order,
   N≥200, patched ≥0.98× on every JSON+CSS corpus — the shared kernel must not cost cycles vs the
   hand scanner it replaces.

# LOCK-9 · Family 9B — NATIVE-REBIRTH (parse-that keeps its own kernels)

*parse-that's scan/kernel layer reborn in idiomatic stable `core::arch::aarch64` — its own
kernels, no cross-repo coupling — each carrying a scalar parity oracle and a
checkasm-liveness harness; the gate is measured no-regression per the PROVE-09 refutation
law. The family's earned value is not the de-nightly port (measured perf-neutral) but the
re-derivation of the movemask reduction to the narrow form the refutation demands, proven
bit-parity-clean and paired-interleaved no-worse.*

---

## The existence proof: a stable NEON kernel surface already builds (F2 Q1)

`skinny/crates/bbnf-simd/` demonstrates that the reborn kernel *form* compiles and passes
on the stable toolchain today: zero `#![feature]` in `src/` (grep clean), pure
`core::arch::aarch64` bodies (`src/aarch64/*.rs`), a scalar reference beside each
(`src/scalar/*.rs`), fn-pointer dispatch (`src/dispatch.rs`), and a Cargo.toml declaring
only `edition.workspace`/`rust-version.workspace` — no nightly channel, no unstable deps.
It is a `[u8;64]`-block kernel library with a runtime-selected backend (`dispatch.rs:42`
`select_classifier`, `dispatch.rs:58` `primitive_kernels` behind `OnceLock`).
Native-rebirth need not invent a stable substrate; it must **adopt this form** inside
parse-that without importing the crate.

**parse-that's stable blast radius, itemized (measured, not estimated):** three nightly
blockers, each with a bounded, already-exercised fix.

1. `#![feature(portable_simd)]` (`rust/parse_that/src/lib.rs:1`) — 8 scanner files,
   ~88 `std::simd` hits, all one uniform `u8x16` vocabulary
   (`splat/simd_eq/simd_ne/to_bitmask/swizzle_dyn`). De-nightly replaces each
   `use std::simd::prelude::*` with a colocated `core::arch::aarch64` module; **zero
   call-site churn** was demonstrated (one `use`-line swap per file).
2. `#![feature(fmt_helpers_for_derive)]` + `#![feature(stmt_expr_attributes)]`
   (`lib.rs:2-3`) — `stmt_expr_attributes` is dead (excisable); `fmt_helpers_for_derive`
   is the **sole residual** and is NOT hand code — it is forced by
   `rust/parse_that/src/regex/generated.rs` (`@generated` from `bootstrap/regex.bbnf`),
   which emits the *expanded* `#[derive(Debug)]` calling the perma-unstable
   `core::fmt::Formatter::debug_tuple_field1_finish`. Per the clean-regen edict this is a
   **generator fix** (emit `#[derive(Debug)]`, not its expansion) + a
   `scripts/bootstrap-regex.sh` regen — never a hand-patch, and it lands in LOCK-5/7
   territory, not 9B's own files.
3. `smallbox = {"0.8", features=["nightly"]}` (`Cargo.toml:97`) — **exact blast radius:
   one dep-feature-flag drop + `SmallBox::new`→`smallbox!` at three sites**
   (`rust/parse_that/src/parse.rs:1,62,74`). **Alloc-neutral**: `smallbox!` ≡
   `SmallBox::new`, identical S32 inline / heap-spill behavior, vtable-box profile
   unchanged (construct `json_parser()` = 6 allocs / ~5 960 B, all construction-time,
   none per-parse). No lifetime churn, no `Parser<'a,O>` rewrite. The bump-slab arena
   that would move those 6 boxes is a separate Parser-ownership refactor (blocked by the
   `'a` input lifetime threaded through 86 `Parser::new` sites) — **out of 9B scope**,
   carry it, do not stub it.

Net: parse-that builds on stable after (1)+(3) in-family + (2) as an out-of-family regen.
No `wide`/`simdeez` dependency is warranted — the `std::simd` vocabulary maps 1:1 onto
hand NEON, so a third-party stable-SIMD crate would only re-introduce a lowering layer we
control better by hand.

---

## The mechanism the rebirth must actually deliver: the narrow movemask (F1 producers)

The refutation makes the perf story unambiguous. **PROVE-09 is law:** a mechanical
`portable_simd`→NEON *shim* (1:1 vocabulary wrap) is perf-**neutral** — the verify fleet's
paired rebuild showed parity-to-slower and disproved the "portable_simd `to_bitmask` is
suboptimal" mechanism at the instruction level (the shim emitted **2× the `addv`**
reductions: 2271 vs 509). **Refutation-04 is law:** classify is ALU-bound on the *movemask
bit-stitch*; block scheduling is dead; the only lever is **MOVEMASK NARROW + vectorized
compaction**. Therefore native-rebirth earns nothing by porting — it earns its keep only
by re-deriving the reduction to the narrow form, and it must *measure* the win paired.

**The canonical producers are absent and must be authored once (F1).** Even inside
bbnf-simd there is **live idiom drift** — exactly the "purge any refuted reduction idiom"
the directive names:

- The shared LEAN producer `src/aarch64/movemask.rs:5` uses the correct
  `vshrn_n_u16::<4>` narrow (128→64, 4 bits/lane) + `0x11`-mask + `vsri` stitch — **but
  its tail is a scalar `for pair in 0..8` loop** (`movemask.rs:20-24`), itself a
  bit-stitch that can erase the narrow advantage.
- `src/aarch64/structural_classify_64.rs:145-155` carries a **duplicate, private**
  `movemask_u8x16` using the **double-`vaddv_u8` halves** form (`vandq` bit-pattern + two
  `vaddv_u8`) — the exact refuted idiom; its `pack4` (`:132`) issues **8 `vaddv` per 64
  bytes**.
- parse-that's own `number_simd.rs:54-55` uses the same double-`vaddv` movemask; the six
  `to_bitmask()` sites (`decode.rs:82,189`; `quote_parity.rs:81`; `structural_bitmap.rs:43`;
  `ws_comment.rs:81,150,247`; `scanners.rs:163,208,286`) lower to whatever `std::simd`
  picks on the pinned old nightly.

The rebirth authors **two** producers into parse-that's own scan home and routes every
site through them:

- **LEAN nibble-`vshrn`** (16-lane membership): `vshrn_n_u16::<4>` narrow, stitched
  *vectorized* — no scalar `0..8` tail — for the stripe scanners (`ws_comment`,
  `structural_bitmap`, `quote_parity` masks).
- **DENSE dense-64** (64-byte block): four stripes narrowed and combined via a
  **`vpaddq`/`vsli` reduction tree** into one u64 — **never** the 4×double-`vaddv` `pack4`
  the current structural path uses, and never a per-stripe scalar `<<16` shift chain
  (`structural_classify_64.rs:138`).

This is the concrete mechanism 9B contributes that the shim did not.

---

## The correctness oracle, transplanted (F2 Q2)

The differential oracle 9B needs exists as a reusable harness:
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs` (modeled on FFmpeg
`tests/checkasm/checkasm.h`) + `tests/checkasm_common.rs`. Its facilities, each directly
transplantable to parse-that's `tests/`:

- **twin-buffer memcmp** — scalar reference and intrinsic candidate on bit-identical
  separate buffers; the framework `memcmp`s *both halves* so a kernel that scribbles its
  own source is caught.
- **alignment sweep 0..64** — `checkasm_structural_terminator_64.rs:42` walks every
  misalignment; where a naive nibble-stride emit dies (below).
- **stack canary xor-fold** — `checkasm_common.rs:50` drops a 1 KiB canary, xor-folds
  before/after, `black_box`es both, panics on clobber (uninitialised-stack-read guard).
- **callee-saved register sentinels** — `checkasm_common.rs:85` writes x19–x28 via inline
  `asm!`, asserts survival (ABI-clobber guard).
- **SIGSEGV/SIGBUS/SIGILL longjmp trampoline** — OOB candidate → test failure, not a
  killed binary.
- **INJECT_BUG teeth** — `BBNF_SIMD_INJECT_BUG=1` xors the first emitted mask byte so the
  candidate diverges; proves the harness actually fails (the "does the oracle have teeth"
  question answered by construction). `BBNF_SIMD_STRICT` promotes random-input divergences
  to failures; `corpus_parity.rs` always asserts strictly against real JSON/CSS fixtures.

**Does a naive nibble-stride form corrupt parity on the dense-64 site? Yes —
demonstrably.** The narrow producer packs *two* lanes per output byte at nibble stride
(`vshrn_n_u16::<4>`); a naive "one nibble per lane, no `0x11` mask, no `vsri` stitch" emit
(skipping `movemask.rs:6-12`) mis-assigns adjacent lanes whenever two neighboring bytes
are both members. The `structural_terminator_64_random_jsonish` sweep
(`checkasm_structural_terminator_64.rs:49`: 4096 blocks drawn from the full `{}[],:"\...`
pool) puts adjacent structural bytes in the same output byte with high probability and
flags it; the deterministic `_alignment_windows` sweep (`:37`) pins the failing offset. So
the oracle distinguishes the correct narrow stitch from the naive one.

**Inherited open item.** `CHECKASM-REPORT.md` records an unresolved NEON↔scalar handoff
divergence (which is *why* `BBNF_SIMD_STRICT` defaults off). A two-home 9B **owns its own
green oracle** and is not gated on bbnf-simd's open divergence — a point for native-rebirth.

---

## One-home vs two-home: the residual substrate divergence (F1)

Native-rebirth is the two-home position; the honest accounting of what stays divergent:

- **Granularity mismatch.** bbnf-simd kernels are **64-byte-block** shaped
  (`classify_chunk(&[u8;64])`, `dispatch.rs:19`; `structural_classify_64(&[u8;64])`).
  parse-that scanners are **16-byte-stripe** shaped (`u8x16` throughout), with a logical
  `STRIPE=64` only in `quote_parity.rs:34`. A one-home merge must reconcile
  stripe-vs-block iteration; two-home keeps each fit-to-purpose. Dominant delta, not a
  naming one.
- **Padding/clamp convention divergence.** parse-that's tail guard is `PaddedView` with
  `INPUT_PAD_BYTES = 64` (`state.rs:57`) — every stripe load skips a tail check because
  `bytes.len() == len + 64`. bbnf-simd's convention is `EobBlock`/`eob_pad_clamp`
  (`src/scalar/eob_pad_clamp.rs`, `aarch64/eob_pad_clamp.rs`). **Different clamp
  contracts**; one-home forces choosing one and rewriting the other side's ~8 scanners.
- **Table shape/strategy.** bbnf-simd's fast path is a single `vqtbl4q` lo6-table
  (`dispatch.rs:100` `lo6_table_admissible`), falling to scalar on lo6 collision; its
  structural path *rejects* the LUT for the 11-byte set (`;`=0x3B ≡ `{`=0x3B) and uses a
  `vceqq` **fan** (`structural_classify_64.rs:11-31`). parse-that's `structural_bitmap.rs`
  is a smaller multi-table membership. Even the classification *strategy* differs by which
  byte-set each repo scans.
- **Capacity constants.** 64-byte block vs 16-lane stripe; parse-that's LUT box is
  `[bool;256]` (`leaf.rs:182`) vs bbnf-simd's `[u8;256]` `byte_class_from_table`.

**LOC basis.** The *shared* asset is exactly the two movemask producers (~50 LOC) + the
checkasm harness (~200 LOC transplant). Everything else — iteration shape, padding, table
strategy — is genuinely per-repo. So the two-home residual is small **and** the one-home
saving is small; the maintenance/drift cost of a cross-repo sync gate (version, toolchain,
the R3 §1 patch-table coupling that already binds parse-that to 7 sibling repos) plausibly
exceeds the ~250 shared LOC it would dedupe. Concrete both-directions-valid finding:
**9B's two-home is cheap because the truly shared surface is tiny and the divergent
surface is intrinsic.**

**Where `#[inline]` fails the crate seam (F1 Q2).** Cross-crate `#[inline]` across the
rlib boundary is measured-live 0 in-loop indirect — but the non-inlinable boundary is
**real and named**: the fn-pointer dispatch struct. `dispatch.rs:50`
`PrimitiveKernels { byte_class_from_table_64: fn(...), ... }` behind `OnceLock`
(`dispatch.rs:58`) is a **runtime-selected function pointer** — under any cgu/LTO config a
genuine indirect call, because the callee is chosen at runtime, not because of a crate
seam. Contrast `movemask.rs:3` `#[inline(always)] pub unsafe fn movemask_u8x16`: a
monomorphized free function imported directly inlines across the seam even at
`codegen-units>1, lto=false`. **9B's rule:** import the hot kernels as `#[inline(always)]`
free functions (movemask, stripe classify), **never** route the hot loop through the
`PrimitiveKernels`/`OnceLock` fn-pointer table — that table is the one place a boundary
appears, one-home or two-home alike.

---

## Scanner-file mapping census (F1, owed) — the ~8 files, classified

| File | portable_simd hits | class | rationale |
|---|---|---|---|
| `parsers/scan/structural_bitmap.rs` | 13 | **1:1-kernel** | simdjson structural bitmap ≙ bbnf-simd `structural_classify_64`; the DENSE producer's home |
| `parsers/scan/quote_parity.rs` | 4 (+aes `vmull_p64`) | **1:1-kernel** | prefix-XOR ≙ bbnf-simd `bitmap_prefix_xor_64`; keep aarch64 `vmull_p64`, purge x86 pclmul (`:42-49`) |
| `parsers/scan/quoted_simd.rs` | 7 | **1:1-kernel** | `odd_parity_backslashes`/escape-mask ≙ bbnf-simd `escape_mask_64` |
| `parsers/scan/decode.rs` | 17 | **vector-tier** | `u8x16` decode-to-arena; `store_chunk` unaligned 16-byte store — stripe kernel, LEAN movemask |
| `parsers/scan/ws_comment.rs` | 16 | **vector-tier** | ws/comment stripe scan, LEAN movemask + ctz |
| `scanners.rs` | 22 | **vector-tier** | shared inline byte scanners (`to_bitmask`+`trailing_zeros`) |
| `span_parser/scan.rs` | 7 | **vector-tier** | span-parser inline scan |
| `parsers/scan/number_simd.rs` | arch-split | **NO-GO (digit)** | SIMD *digit accumulation* is refutation-02 dead; purge x86 SSE4.2 (`:145-235`); keep only terminator-find movemask as vector-tier |

**Split:** 3 kernel-tier (map onto bbnf-simd's structural/prefix-xor/escape kernels — the
one-home argument's only purchase), 4 vector-tier (own LEAN movemask, no bbnf-simd
analogue), 1 no-go (digit scan). The vector-tier + no-go are intrinsically parse-that's —
the two-home case.

---

## Prior art (citations)

- **FFmpeg checkasm** (`tests/checkasm/checkasm.h`) — the differential twin-buffer +
  robust-timing + register/canary model bbnf-simd's harness follows; the shipping standard
  for SIMD-vs-reference liveness.
- **simdjson** (Langdale & Lemire, *Parsing Gigabytes of JSON per Second*, VLDB Journal
  28(6), 2019) — structural bitmap, `pclmulqdq`/`vmull_p64` prefix-XOR quote parity, the
  lo×hi two-LUT `shufti` classifier (the upgrade path named in
  `structural_classify_64.rs:18-21` when a set widens past ~16).
- **Geoff Langdale, "shufti"/Hyperscan vectorized classification** — nibble-LUT
  membership; why the `vqtbl4q` lo6-table works only when lo6-collision-free
  (`dispatch.rs:101`).
- **ARM NEON movemask via `vshrn`** — NEON has no `pmovmskb`; `vshrn_n_u16(·,4)` narrows a
  128-bit compare to a 64-bit, 4-bit-per-lane mask in one instruction (Danila Kutenin,
  "Porting x86 `movemask` to ARM NEON", 2022; Dougall Johnson aarch64 latency notes) — the
  LEAN producer's basis and why double-`vaddv` is strictly worse.
- **regex-automata (BurntSushi)** — the memchr/rare-byte prefilter ladder productionised
  in the adjacent PROVE-10 (`Dfa::find` + `Prefilter::from_dfa`), relevant at the
  scanner↔DFA border.
- **rustc `portable_simd` aarch64 lowering** — LLVM lowers `Mask::to_bitmask` on aarch64
  through a comparable reduction, which is *why* the shim measured neutral rather than
  faster (the refutation's instruction-count evidence).

---

## Named risks & counterexamples

1. **The shim trap (PROVE-09, LAW).** A 1:1 port is refuted as a speedup. Any perf claim
   must be *paired, interleaved, opposite-arm order, min-of-N, loadavg-logged* — two-arm
   window asymmetry is the named self-deception. The gate is **no-regression**, not a
   headline multiple.
2. **Scalar-tail erasure.** `movemask.rs:20-24`'s scalar `0..8` stitch may negate the
   `vshrn` advantage; copying it reproduces the neutral result. The DENSE producer must
   reduce four stripes with a `vpaddq`/`vsli` tree — not a scalar shift chain, not
   double-`vaddv`.
3. **Refutation-02 (digit scan).** Do **not** rebirth `number_simd`'s SIMD digit
   accumulation as a perf play — NO-GO; the canada tax is FLOAT CONVERSION (Eisel-Lemire),
   not the digit scan. Keep the digit path scalar; the movemask there is only
   terminator-find.
4. **x86 purge (constitution iv).** `number_simd.rs:145-235` (SSE4.2, runtime
   `is_x86_feature_detected!`) and `quote_parity.rs:42-49` (`pclmulqdq`) are dead code to
   delete, not port. Keep aarch64 `vmull_p64` (feature `aes`).
5. **INJECT_BUG must be proven RED.** A parity harness that never fails is worthless; the
   prototype must show the INJECT_BUG toggle turning the suite red before any green claim.
6. **Inherited open divergence.** Sharing bbnf-simd inherits its unresolved NEON↔scalar
   handoff divergence (`STRICT` off by default); owning the kernels lets 9B ship a fully
   strict green oracle — a substantive two-home advantage.

---

## What a prototype must test to move 9B

1. **Author both producers** into a parse-that-owned scan module: LEAN `movemask_u8x16`
   (`vshrn_n_u16::<4>` + `0x11` + `vsri`, **vectorized** stitch, no scalar `0..8`) and
   DENSE `pack64` (4-stripe `vshrn` → `vpaddq` tree → u64, no double-`vaddv`, no scalar
   `<<16`). Route the six `to_bitmask()` sites + `number_simd` terminator movemask through
   them; delete the private double-`vaddv` duplicates.
2. **Transplant the checkasm harness** into `rust/parse_that/tests/` — per-scanner scalar
   reference, twin-buffer memcmp, 0..64 align sweep, canary, register sentinels, SIGSEGV
   trampoline, INJECT_BUG. **Demonstrate RED under INJECT_BUG**, then GREEN + strict corpus
   parity on the JSON+CSS fixtures.
3. **Measure the reduction delta at instruction level, base-pinned:** `otool -tv`
   `addv`/`vshrn`/`vpaddq` counts, baseline (portable/double-`vaddv`) vs narrow, per hot
   scanner — the refutation-04 lever quantified.
4. **Paired interleaved perf, the gate:** all 6 JSON corpora
   (apache/canada/citm/data/data_xl/twitter) + CSS, opposite-arm order, min-of-N, loadavg
   logged; report cyc/B or divan wall as **ratio ≥ parity** (no-regression), naming HEAD
   and both windows.
5. **Stable-build proof:** `cargo +stable` after portable_simd→own-NEON + smallbox
   de-feature; report the exact residual (expected: only `regex/generated.rs`'s
   `fmt_helpers_for_derive`, an out-of-family regen).
6. **The census, empirically:** confirm the 3 kernel-tier / 4 vector-tier / 1 no-go split
   by which files actually consume the two shared producers vs their own LEAN movemask.

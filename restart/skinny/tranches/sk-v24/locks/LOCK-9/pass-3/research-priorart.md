# LOCK-9 — THE SCANNER & KERNEL PLANE · pass-3 · PRIOR-ART SWEEP

Problem class: **portable byte-scan SIMD kernels on aarch64 NEON, shared across
crates, honest after a measured refutation, with the scanner layer's relation to a
DFA/prefilter engine and to a streaming surface all in question.** No family
allegiance — this maps what exists, what won and lost elsewhere for THIS class,
which constraints our own evidence base already fixes, and the named traps. REGISTRY
deliberately not read.

Scope anchor (R3 §2/§4, verified): the surface is `#![feature(portable_simd)]`
across 8 scanner files (`scanners.rs`, `parsers/scan/{decode,ws_comment,
structural_bitmap,quoted_simd,quote_parity,number_simd}.rs`, `span_parser/scan.rs`),
two files with real per-arch intrinsics (`quote_parity.rs` clmul/`vmull_p64`,
`number_simd.rs` SSE4.2+NEON digit accumulation), `smallbox[nightly]` in the
combinator vtable, and the kernel-sharing question against
`skinny/crates/bbnf-simd` (33 src files, scalar+aarch64 mirror, checkasm suite).

---

## 1. NEON movemask — the central kernel idiom (F1/F2)

**The problem is real and named in the literature.** ARM NEON has no `PMOVMSKB`
equivalent; the naive "port" of an x86 movemask is the single most-cited NEON
scanning trap. Two families of solution exist, and the winner is settled:

- **WINNER — `SHRN #4` narrow (the vshrn idiom).** Treat the 16-byte compare
  result as eight `u16`, `vshrn_n_u16(v, 4)` narrows each lane to 8 bits, yielding
  a 64-bit mask where each **nibble** encodes one input byte. One instruction, one
  `fmov` to GPR. This is the canonical technique in the ARM community blog "Porting
  x86 vector bitmask optimizations to Arm NEON" [ARM], adopted by **memchr's
  aarch64 backend** (BurntSushi/memchr PR #114 [memchr]), **facebook/zstd**
  `ZSTD_row_getMatchMask` (PR #3139) and Folly [zstd]. The mask is nibble-granular,
  not bit-granular — consumers mask with `0x8888…` or `0xF…` and divide indices by
  4. PROVE-10's own disasm confirms memchr emits exactly this: `cmeq.16b` →
  `shrn.8b v0,v0,#4` → `fmov x8,d0` → `ands x8,x8,#0x8888888888888888`.

- **LOSER — the `vand`(bit-positions)+`vaddv` "true movemask" reduction.** Builds a
  bit-per-lane mask by AND-ing a `[1,2,4,…,128]` position vector then horizontally
  summing each 8-byte half with `vaddv` (two `addv.8b`). This is what
  `std::simd::Mask::to_bitmask` lowers to on aarch64, and what **PROVE-09's
  `simd128.rs` shim reproduced by hand.** The PROVE-09 VERIFY fleet measured it:
  **2271 `addv.8b` vs baseline 509** — the shim does *more* reduction work per
  movemask, and the claimed 1.34–1.57× speedup **did not reproduce** (parity-to-
  slower on paired interleave). This is now SPEC §0 refutation **09 (LAW)** and the
  lock's binding contract line: *"vshrn narrow, never the double-addv movemask."*
  Prior art and our own evidence agree — the two families are not
  interchangeable, and the narrow form is the only one that pays.

**Constraint this fixes for LOCK-9.** Any authored canonical producer (the
directive's LEAN nibble-`vshrn` / DENSE dense-64 `vpaddq`-tree contract split) must
emit `vshrn`/`shrn` narrow at the membership tier; the `vaddv`-pair form is a
banked negative result, not a candidate. A "naive nibble-only" form on the dense-64
site is separately flagged (F2/F3): nibble-granular masks lose per-byte identity
where the structural set needs full resolution — the DENSE tier legitimately needs
the wider `vpaddq` tree, which is why the contract is *split*, not unified.

---

## 2. Stable-toolchain rebirth — portable_simd is not coming (F2, 9B)

**`std::simd` remains nightly-only "for the foreseeable future"** — no stabilization
date, blocked on mask element types, swizzle API, and trait design (rust-lang
portable-simd #364; RFC 2977/2948 unresolved) [pssimd, shnatsel]. Shnatsel's "State
of SIMD in Rust 2025" and pythonspeed both conclude the production path today is
`core::arch::aarch64` intrinsics (stable) or the `wide`/`simdeez` crates — exactly
the migration R3 §2a scoped and PROVE-09's `simd128.rs` executed. **The de-nightly
PORT is validated prior art; only its perf *claim* was refuted.** The honest value
is stability (stable toolchain, `smallbox[nightly]` excised) at cyc/B parity —
which is precisely how SPEC §0/09 and PT-N3 already re-frame it.

**bbnf-simd is already stable-clean and is the reference shape.** It carries a
scalar + `core::arch::aarch64` mirror (`src/scalar/*` ↔ `src/aarch64/*`), a
`movemask.rs` primitive, and a `checkasm_*` differential parity suite (12 kernels).
This is the mature form of what parse-that's scanners must become — the F2
correctness-oracle question (bit-parity emitted-mask vs scalar, INJECT_BUG rate,
alignment sweep) is *already solved once* in bbnf-simd's checkasm harness. Prior art
here is in-tree, not external.

**Trap named:** the `smallbox[nightly]→stable` cut (R3 §2c) is orthogonal to the
SIMD cut and cheap (`smallbox!` macro ≡ `SmallBox::new`, alloc-neutral, PROVE-09
confirmed). The residual `fmt_helpers_for_derive` blocker is NOT a hand-patch target
— it is forced by `@generated regex/generated.rs` emitting *expanded* `#[derive(
Debug)]`; the clean-regen edict makes it a GENERATOR fix (emit the derive, not its
expansion). PROVE-09 VERIFY flagged that the retained feature-attr is itself an
`E0554` on stable — so "zero stable errors" is nuanced; the true stable gate is the
regen fix, not the shim.

---

## 3. The kernel-sharing question — what won elsewhere (F1, 9A vs 9B)

**simdjson / simdutf: on aarch64 there is exactly ONE implementation and no runtime
dispatch.** simdjson's implementation-selection doc [simdjson] states plainly: x86
compiles 4 impls (icelake/haswell/westmere/fallback) behind runtime CPU detection,
but **64-bit ARM includes just one — "dispatching is unnecessary."** This is
independent prior-art validation of the SPEC's x86 abrogation (Constitution iv, §0):
the multi-arch dispatch machinery that dominates simdjson's architecture is *pure
overhead we correctly do not carry*. M-series is NEON-only (no SVE/SVE2/SME for byte
work — SPEC §0 carried law), so the single-impl world is ours by construction.

**Kernel-sharing across a crate boundary is the live design axis, and prior art
leans toward ONE home.** simdjson keeps all kernels in a single library consumed by
Node, ClickHouse, Velox, etc.; **simdutf is the *extraction* of simdjson's UTF
kernels into a standalone shared library** precisely so multiple consumers stop
duplicating them. The pattern that won: extract the kernel core once, make callers
thin consumers — that is LOCK-9's 9A (one aarch64 kernel home serving both repos).
The directive's pre-settled facts sharpen this: cross-crate `#[inline]` across the
rlib boundary is measured-live (0 in-loop indirect), and the canonical movemask
producers are *absent from bbnf-simd and must be authored once regardless of
ownership* — so the one-home/two-home choice is a maintenance/drift-cost question,
not a perf question. Prior art (simdutf) says the drift cost of duplication is the
thing that eventually forces extraction.

**Trap named (the `#[inline]` seam):** cross-crate inlining silently fails at the
rlib boundary under `codegen-units>1` without LTO on non-`#[inline]`-annotated
functions; the seam appears only where a kernel is *not* marked `#[inline]` and the
consumer crate is compiled separately (thin-LTO off). This is why every shared
kernel must carry `#[inline]` explicitly — the isomorphism is "physical" only if the
attribute is present at every producer.

---

## 4. Scanner-as-DFA — the prefilter ladder is proven, the classifier is not (F3, 9C)

**The prefilter/acceleration ladder is mature, external, and won.** BurntSushi's
regex-automata composes a tiered prefilter: **memchr/memchr2/memchr3** for 1–3 rare
entry bytes, **Teddy** (ported from Hyperscan — SIMD packed multi-substring, self-
confirming), and **shufti** (Hyperscan nibble-classifier), with **DFA start-state /
`.*`-state acceleration** that runs memchr to skip to the next candidate [regex-int,
rebar]. Rare-byte selection uses a background frequency distribution, not first/last.
**PROVE-10 productionised the bottom rung of exactly this ladder** into
`bbnf-regex Dfa::find` (start-state entry-byte set → memchr tier) and it is SPEC §0
**10 (CONFIRMED)**: 8.8–29.5× on prefilterable patterns, 0.97–1.01× controls,
compile-neutral, 44/44. The Teddy/shufti multi-byte tiers are the named remaining
rungs (PROVE-10 §5 out-of-scope).

**Trap named — small-multistate serial `vqtbl1q` chains lose on short runs.** The
directive (F3) records this as a *measured* negative: a `[u8;256]`→SIMD classifier
built as a serial table-lookup chain is a loss on the short spans these scanners see;
prior art agrees — Hyperscan's shufti/truffle win because they are *single* shuffle
membership tests, not multi-state chains. So the DFA-derived scanner must weight the
**membership + structural tiers** (single-`vqtbl1q`/shufti and the prefilter skip),
not attempt a general small-DFA `tbl` walk. The 255-class / 4096-state ceilings
(F3) are the denominators: coverage is high only where the leaf collapses to a
membership class; a rich CSS `url()` or wide-unicode leaf will exceed them and must
fall to the scalar/DFA path, not a rigged full-SIMD claim.

**Name+version collision (F3, settled cross-lock XL-C):** two crates ship
`bbnf-regex 0.1.0` — the *vendored parse-that-regex inside skinny* is the DFA
source-of-truth; top-level `crates/bbnf-regex` is legacy and archives at GN-0. The
grammar tier keeps `Regex(StringId)` opaque; engine tier keeps rich `Hir`, joined by
a string/format bridge — no single-HIR dependency. So scanner-as-DFA (9C) consumes
LOCK-5's engine through a bridge; it does not fuse HIRs.

---

## 5. Streaming surface — prior art says whole-doc streaming buys nothing here (F4, 9D)

**simdjson's `document_stream` / on-demand and nom's streaming exist, but the class
lesson is negative for RSS.** Whole-document-granularity streaming (feed complete
docs, parse each) does not reduce peak tree RSS — it reduces *aggregate* memory
across many docs, not the single-parse footprint. If parse-that's grammars each build
one whole owned tree (R3 §5: `many`/`sep_by` push into `Vec`, string arena), then a
streaming surface must be *sub-document* (chunked byte windows with bounded rewind)
to matter — and that requires bounded backtracking.

**Backtracking-bound prior art (packrat/PEG).** Packrat guarantees linear time via a
full memo table but at O(input) memory; the streaming-relevant advances are the
**elastic sliding window** (IPSJ) and **incremental packrat with interval-tree memo**
(Dubroy/Warth; Yedidia gpeg) [packrat] — all of which presuppose the parser's rewind
distance is *bounded* so a ring buffer / window suffices. The empirical question F4
poses (rewind histogram `furthest_offset − offset`, P50/P99/max over JSON+CSS) is
exactly the discriminator: bounded ⇒ a fixed ring buffer streams; unbounded ⇒ no
streaming surface is sound without full retention. **Prior art does not answer this
for parse-that — it must be measured**, and R3's `PaddedView`
(`INPUT_PAD_BYTES=64`, trailing-NUL invariant) is a *whole-buffer* precondition every
SIMD kernel currently assumes, which a windowed surface would have to reproduce per
chunk.

---

## 6. Constraints our own evidence base already imposes (binding)

| Source | Constraint on LOCK-9 |
|---|---|
| SPEC §0 / **09 LAW** | movemask = `vshrn` narrow; `vaddv`-pair shim is a banked NO. Paired interleave mandatory even for "obvious" wins (two-arm window asymmetry is the classic self-deception). |
| SPEC §0 / **10 CONFIRMED** | prefilter tier (memchr entry-byte) is proven; generator inherits by emitting `dfa.find`. Teddy/shufti tiers unbuilt. |
| SPEC §0 / **01+03 CONFIRMED** | NEON shufti/truffle prefilter (GATE=16) and fused string scan (`vshrn`) are the *validated* kernel wins — dense-neutral, skip-heavy 4.7–7.5×. |
| SPEC §0 refutation **02** | no digit-scan SIMD (latency-bound CPI inversion); `number_simd.rs`'s SIMD digit path is a banked loss — redress targets float *conversion* (Eisel-Lemire), never the scan. |
| SPEC §0 refutation **04** | no block-scheduling; the lever is movemask-narrow + vectorized compaction. |
| Constitution iv / §4 | x86 dead (4-layer purge). `quote_parity.rs`/`number_simd.rs` x86 arms are purged, not ported; NEON `vmull_p64` clmul path survives. |
| Constitution v | kernels cost zero at the type plane; no runtime dispatch (aarch64 single-impl, simdjson-validated). |
| Bench law (§4) | divan cold paired, N≥200, both windows, PMU cyc/B where macOS allows (it mostly does not — wall-clock paired ratios are the certified metric per PROVE-09/10). |

---

## 7. Named traps (comparable systems + our own)

1. **Two-arm window asymmetry** (PROVE-09 self-deception): measuring "modified" in a
   quieter window than "baseline" manufactures a phantom 1.3–1.5×. Only same-binary
   paired lanes (PROVE-10's method) or interleaved opposite-order windows are safe.
2. **The `vaddv`-pair movemask** looks like the "faithful" port and is the slow one
   (does 2× reduction). The nibble-`vshrn` looks lossy (nibble-granular) and is the
   fast, correct one.
3. **Serial `vqtbl1q` small-DFA walk** — loses on short scanner runs; only single-
   shuffle membership (shufti/truffle) and prefilter-skip win at scanner scale.
4. **Whole-doc streaming ≠ RSS reduction** — a streaming *claim* that doesn't cut
   single-parse peak memory is a non-win; the surface must be sub-document +
   bounded-rewind or it is theatre.
5. **`@generated` nightly residual masquerading as a source blocker** — the stable
   cut is gated on a regen fix (emit `#[derive(Debug)]`), not on hand-editing lib.rs;
   a hand-patch violates the clean-regen edict and leaves an `E0554`.
6. **Cross-crate `#[inline]` omission** — the "physical isomorphism" of one kernel
   home evaporates at the rlib seam unless every producer is `#[inline]`-annotated
   under the shipped cgu/LTO config.

---

## Sources

- **[ARM]** ARM Community — "Porting x86 vector bitmask optimizations to Arm NEON"
  (SHRN #4 movemask idiom): https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
- **[memchr]** BurntSushi/memchr PR #114 (aarch64 NEON, `vshrn` movemask):
  https://github.com/BurntSushi/memchr/pull/114
- **[zstd]** facebook/zstd PR #3139 (`ZSTD_row_getMatchMask`, NEON movemask via SHRN):
  https://github.com/facebook/zstd/pull/3139
- **[pssimd]** "Using portable SIMD in stable Rust" — pythonspeed:
  https://pythonspeed.com/articles/simd-stable-rust/ ; portable-simd #364 (critical
  pre-stabilization issues): https://github.com/rust-lang/portable-simd/issues/364
- **[shnatsel]** "The state of SIMD in Rust in 2025" — S. Davidoff:
  https://shnatsel.medium.com/the-state-of-simd-in-rust-in-2025-32c263e5f53d
- **[simdjson]** simdjson implementation-selection (aarch64 = single impl, no
  dispatch): https://github.com/simdjson/simdjson/blob/master/doc/implementation-selection.md
- **[regex-int]** "Regex engine internals as a library" — A. Gallant (prefilter,
  memchr acceleration, Teddy/shufti): https://burntsushi.net/regex-internals/ ;
  **[rebar]** https://github.com/BurntSushi/rebar
- **[packrat]** Dubroy & Warth, "Incremental Packrat Parsing" (SLE 2017):
  https://ohmjs.org/pubs/sle2017/incremental-packrat-parsing.pdf ; Yedidia, "Fast
  Incremental PEG Parsing" (SLE 2021):
  https://people.seas.harvard.edu/~chong/pubs/gpeg_sle21.pdf ; "Packrat Parsing with
  Elastic Sliding Window" (IPSJ):
  https://www.jstage.jst.go.jp/article/ipsjjip/23/4/23_505/_pdf
- **In-tree evidence:** SPEC §0 (features 01/03/10, refutations 02/04/09),
  PROVE-09 REPORT+VERIFY (movemask refutation), PROVE-10 REPORT+VERIFY (prefilter
  confirmed), R3 §2/§4/§5 (nightly surface + scanner census),
  skinny/crates/bbnf-simd (stable aarch64 mirror + checkasm oracle).
</content>

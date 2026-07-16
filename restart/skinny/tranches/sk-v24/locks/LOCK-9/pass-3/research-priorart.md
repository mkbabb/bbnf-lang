# LOCK-9 pass-3 — PRIOR-ART RESEARCH: the scanner & kernel plane (parse-that rust)

*No family allegiance. What exists, what won and lost elsewhere for THIS problem class, the
constraints our own evidence already certifies, the named traps. Citations inline; internal
artifacts cited by path+line, external by URL. REGISTRY deliberately not read.*

---

## 0. The problem class, stated

parse-that's Rust scan/kernel layer is a **byte-level structural-classification engine** of the
simdjson stage-1 lineage: eight `#![feature(portable_simd)]` scanner files (`scanners.rs` 22
hits, `parsers/scan/decode.rs` 17, `ws_comment.rs` 16, `structural_bitmap.rs` 13,
`quoted_simd.rs` 7, `span_parser/scan.rs` 7, `quote_parity.rs` 4, `state.rs` 2 — R3 §2a) plus
two *mixed x86/NEON* kernels (`number_simd.rs`, `quote_parity.rs` — R3 §4). Every hot loop
reduces to the same three motifs the whole SIMD-parsing literature revolves around: **(a) a
16/64-byte compare-to-mask (movemask)**, **(b) quote/escape parity via prefix-XOR**, **(c) a
skip/prefilter to find where a match can begin**. This is exactly simdjson's problem, and the
prior art is deep and settled. bbnf-lang's own `crates/bbnf-simd` already implements all three
motifs as a scalar-oracle-gated aarch64 kernel home — so the live question is not "how" but
"**one kernel home or two**", and "**hand NEON or dissolve into the DFA**".

---

## 1. The central kernel decision — the ARM movemask problem (motif a)

This is THE decision of the plane, and the literature is unusually crisp.

- **NEON has no `PMOVMSKB`.** Geoff Langdale's canonical treatment names the two substitutes:
  (i) shift each high-bit lane to a power-of-two position + **two `vaddv_u8` halves** (the
  "horizontal-add movemask"), or (ii) a **`shrn`/`vshrn_n_u16` narrow** that packs a
  4-bit-per-lane mask into a 64-bit register with one instruction — "Fitting My Head Through The
  ARM Holes", branchfree.org (Langdale, 2019)
  <https://branchfree.org/2019/04/01/fitting-my-head-through-the-arm-holes-or-two-sequences-to-substitute-for-the-missing-pmovmskb-instruction-on-arm-neon/>.
- **The narrow (`shrn #4`) idiom WON and is the community standard.** ARM's own blog ("Bit
  twiddling with Arm Neon: beating SSE movemasks", developer.arm.com) documents the
  `shrn`/`vshrn` 4-bit-nibble mask as the technique that *beats* the x86 movemask idiom when the
  mask is consumed at nibble stride (`index = tz >> 2`)
  <https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon>.
  simdjson independently hit the double-`vaddv` trap — "neon_movemask_bulk losing half the mask"
  (simdjson#309) <https://github.com/simdjson/simdjson/issues/309> — the failure mode of the
  horizontal-add path.
- **This is measured LAW in our own evidence, not a borrowed opinion.** PROVE-09/VERIFY attack
  #2: the portable_simd `to_bitmask` shim emitted **2271 `addv.8b` vs baseline 509** (~4× the
  reduction work) — that is what `std::simd::Mask::to_bitmask` lowers to on the pin — and it
  *regressed the product lane* (window A 0.81–0.98×). SPEC §0/03 CONFIRMED the inverse: the
  **fused NEON string scan with `vshrn` narrow** won in both interleave slots (twitter
  0.898–0.931× cyc/B, ins/B −9/−12% deterministic). PROVE-10's `otool` shows the memchr NEON
  path already lowering `shrn.8b v0, v0, #4` (REPORT §2c). **Verdict for the plane: `vshrn`
  narrow is mandatory; double-`vaddv` is a named counterexample.**
- bbnf-lang already encodes this: `crates/bbnf-simd/src/aarch64/movemask.rs` = "`vshrn_n_u16<4>`
  compressed movemask" (CHECKASM-REPORT §a). The correct idiom is *already sitting in the shared
  kernel home* — a strong signal for the family-9A kernel-sharing thesis.

---

## 2. Quote/escape parity — prefix-XOR + carryless-multiply (motif b)

- The structural-bitmap core is **prefix-XOR of the quote mask** to fill "inside-string"
  regions, plus odd/even backslash-run analysis for escapes. Langdale's "Finding quote pairs
  with carry-less multiply (PCLMULQDQ)" is canonical
  <https://branchfree.org/2019/03/06/code-fragment-finding-quote-pairs-with-carry-less-multiply-pclmulqdq/>.
- **On ARM the PCLMULQDQ equivalent is `vmull_p64`** (emits `pmull`), gated on
  `target_feature="aes"`. parse-that's `quote_parity.rs:51-56` already uses `vmull_p64`; its x86
  arm (`:42-49`, `pclmulqdq`) is the abrogation target. The scalar fallback is the 5/6 shift-XOR
  chain the literature documents a `vmull_p64` obviates — Wunkolo, "pclmulqdq Tricks"
  <https://wunkolo.github.io/post/2020/05/pclmulqdq-tricks/>.
- bbnf-simd already ships this: `bitmap_prefix_xor_64` + `escape_mask_64` (even/odd carry
  analysis, `lib.rs:169-205`) with a scalar reference. **Second motif already homed in the shared
  kernel.** The trap the literature warns of (and lib.rs handles): the **inter-block carry** —
  parity must thread across 64-byte blocks (`carry_in` bool), or a string spanning a boundary
  flips inside/outside. Any streaming/lazy (9D) rework inherits this.

---

## 3. The prefilter/skip motif (c) — Teddy, shufti, memchr ladders

- The "where can a match begin" skip is the highest-leverage lever and the most-studied.
  **Hyperscan** contributed the vocabulary the field reuses: **shufti/truffle** (SIMD byte-class
  membership via `vqtbl` nibble-LUT) and **Teddy** (multi-literal SIMD fingerprint) — Wang et
  al., NSDI'19 <https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf>; jneem/teddy Rust port
  <https://github.com/jneem/teddy>. SPEC §0/01 CONFIRMED the NEON shufti/truffle prefilter
  (skip-heavy ~7.5×/4.7×, dense-neutral 1.03–1.15×); §0/05 the CSS shufti token kernel (tailwind
  lightningcss 3.25→**4.16×**).
- **The memchr ladder** (1/2/3-byte entry sets → `memchr`/`memchr2`/`memchr3`, else fall
  through) is regex-automata's prefilter tier — Gallant, "Regex engine internals as a library"
  <https://burntsushi.net/regex-internals/>. PROVE-10 productionized *exactly* this against the
  bbnf-regex DFA start-state (`Dfa::find`, 9.6–29× on prefilterable, 0.99× controls, compile-
  neutral, 44/44 tests); memchr's aarch64 backend is itself the `shrn #4` NEON loop (REPORT §2c).
  This is the concrete substrate for family **9C** (scanner-as-DFA): the prefilter derivation
  lives once in the regex continuum, and quote/number/ws scanners *could* become compiled
  prefilter products rather than eight hand kernels. **Open cost the literature flags**: DFA
  prefilters win on *rare* start bytes and go parity-or-worse on dense ones (PROVE-10's `h` case,
  §2a) — a hand kernel's unconditional 64-byte stride has no such cliff. 9C must prove it does
  not regress the dense/`None` path the eight flat scanners currently cover.

---

## 4. The kernel-sharing question (9A vs 9B) — differential-testing prior art

- **dav1d's `checkasm`** is the reference design for "one kernel, N implementations, proven
  bit-identical": every SIMD kernel is differential-tested against a scalar reference on random
  inputs, with a liveness assertion the SIMD path was actually taken. bbnf-simd adopts it
  verbatim — `tests/checkasm_parity.rs`, "the binding dav1d correctness anchor"
  (CHECKASM-REPORT; lib.rs §prim: "the scalar reference is the executable specification per the
  checkasm admission gate"). Every `prim::*` kernel dispatches `#[cfg(aarch64)]` NEON with a
  scalar-oracle fallback — the exact 9B "scalar parity oracle + checkasm-liveness" shape, but
  already *shared* (9A) rather than parse-that-local.
- **The 9A case is strong and pre-built.** bbnf-simd's kernel set (`structural_classify_64`,
  `byte_class_from_table_64`, `byte_class_from_eq_set_64`, `comment_body_mask_64`,
  `bracket_depth_mask_64`, `movemask`, `bitmap_prefix_xor_64`, `bulk_emit_positions_64`,
  `unescape_uxxxx`, `match_tiny_plain_string`) is a near-superset of what parse-that's eight
  scanners hand-roll. SPEC §1 already commits bbnf-simd's kernels to port into the greenfield
  "under the colocation edict". The danger the Rust-ecosystem prior art warns of (`pulp`/`wide`
  design notes): a shared crate exposing a **portable vocabulary** (`u8x16`, `to_bitmask`)
  re-invites the movemask trap at the seam — bbnf-simd correctly exposes *named semantic kernels*
  (`byte_class_from_eq_set_64 → u64`), not a portable-SIMD façade, sidestepping it.
- **The 9B counter-case** (parse-that keeps its own kernels): PT-2 takes it — a
  `src/simd/{mod,neon,scalar}.rs` directory-module, no cross-repo coupling, gate = measured
  no-regression, `vshrn` fix as the load-bearing production delta over the refuted shim. Cost:
  duplicates motifs proven once in bbnf-simd. Benefit: zero cross-repo build/version coupling
  (R3 §1: the 7-external-patch-table is already load-bearing and fragile — a broken repoint
  yields unresolved-dependency errors, not fallbacks).

---

## 5. The stability constraint our evidence imposes (portable_simd is a dead end)

- **`std::simd` is NOT stable as of 2026** and requires a pinned nightly — Rust std docs;
  Davidoff, "The state of SIMD in Rust in 2025"
  <https://shnatsel.medium.com/the-state-of-simd-in-rust-in-2025-32c263e5f53d>; portable-simd#364
  (open pre-stabilization blockers: mask element types, swizzle API). Stable alternatives are
  **`wide`** (portable abstraction) and **`pulp`** (built-in multiversioning, powers faer).
  parse-that's pin (`nightly-2026-04-11`) exists *only* to dodge 93 recorded `on_disk_cache` ICEs
  (R3 §1) — the nightly is a liability, not a feature.
- **The certified path is `core::arch::aarch64` directly, not a portable crate.** PROVE-09 proved
  the shim de-nightly is faithful and alloc-neutral but its *portable* movemask regressed;
  PT-2.W1 productionizes into hand `core::arch::aarch64` with the `vshrn` idiom fixed. Prior art
  agrees: for a *single-target* engine (aarch64-only, M-series; SPEC §0 "x86 dead", "no
  SVE/SVE2/SME for byte work") a portable-SIMD layer buys nothing and costs the optimal idiom.
  Firm constraint on any 9A/9B design: **the shared vocabulary must be semantic kernels over
  `core::arch`, never a `std::simd` re-export.**

---

## 6. Named traps (comparable systems + our own refutations)

1. **The double-`vaddv` movemask** (simdjson#309, PROVE-09 attack #2) — the horizontal-add
   substitute does ~4× the work of `shrn` narrow and regresses. `std::simd::to_bitmask` lowers to
   it. NAMED COUNTEREXAMPLE for every kernel touching a mask.
2. **The shim / portable-vocabulary self-deception** (PROVE-09/VERIFY, SPEC §0/09 REFUTED) — a
   "de-nightly port" claimed 1.34–1.57×; fresh paired windows showed parity-to-slower. Any
   kernel-plane perf claim is disinformation without paired interleaving, opposite arm order,
   N≥200, two windows. Two-arm window asymmetry is THE classic trap.
3. **Digit-scan SIMD** (SPEC §0/02 NO_GO) — NEON Mula/Lemire in-vector digit accumulation is
   −37/−48% instructions but latency-bound CPI inversion (0.13→0.30) regresses the product.
   `number_simd.rs`'s NEON arm stays a *byte-preserving port*, never extended toward in-vector
   accumulation; the float-conversion (Eisel-Lemire) tax is the real target, not the scan.
4. **Block-scheduling / multi-block pipelining** (SPEC §0/04 refuted) — 1.05–1.06× only; the
   M-class OoO core already pipelines the single-block loop; classify is ALU-bound on the
   **movemask bit-stitch**. The lever is narrow-movemask + vectorized compaction, NOT unrolling.
5. **Inter-block parity carry** (§2) — prefix-XOR and escape parity MUST thread the carry across
   64-byte blocks or a boundary-spanning string corrupts; a hazard specifically for any
   streaming/lazy (9D) reformulation, which trades the flat-buffer PaddedView invariant
   (`INPUT_PAD_BYTES=64`, trailing NUL — R3 §4) for windowed input and must re-establish it.
6. **DFA-prefilter dense cliff** (§3, PROVE-10 §2a) — prefilter wins on rare start bytes, parity
   on dense; a hand 64-byte-stride scanner has no cliff. 9C (scanner-as-DFA) must not regress the
   dense/`None` path the eight flat scanners cover.
7. **Warm/cached benches** (SPEC §0 standing law) — disinformation; cold per-parse only.

---

## 7. Synthesis — what the prior art tells each family (no advocacy)

- **9A (one kernel library)**: strongest structural case — bbnf-simd *already is* the shared home,
  with `vshrn` movemask, prefix-XOR, structural-classify, and dav1d-checkasm parity all built and
  gated; parse-that's eight scanners are a near-subset. Risk: cross-repo build coupling (R3 §1
  fragile patch table) and keeping the seam semantic (not a portable façade).
- **9B (native rebirth)**: the *measured-safe* path (PT-2 productionizes PROVE-09); duplicates
  motifs proven once elsewhere; zero coupling. The `vshrn` fix is the load-bearing delta over the
  refuted shim.
- **9C (scanner-as-DFA)**: has a live substrate (PROVE-10 `Dfa::find` + Hyperscan shufti/Teddy
  vocabulary); must clear the dense-cliff and prove hand-scanner parity on `None` patterns.
- **9D (lazy-surface)**: least prior art in our evidence base; the inter-block carry + the
  PaddedView flat-buffer invariant are the concrete hazards a streaming reformulation inherits.

**Constitution touchpoints**: motif (a)/(b)/(c) kernels are lawful hand-written primitives
(clause i exempts primitives/combinators); the checkasm scalar oracle IS the zero-runtime-cost
discipline for kernels (clause v); the shared-home question is clause iii (rust/rust isomorphism
to a measured point) — bbnf-simd already demonstrates the physical isomorphism 9A asserts.

---

## Sources

Internal: SPEC §0/§1 (`sk-v24/SPEC.md`); PROVE-09 REPORT+VERIFY, PROVE-10 REPORT
(`sk-v24/prototypes/`); PT-2.md (`docs/tranches/sk-v24/`); R3-parse-that-rust.md
(`sk-v24/research/`); `crates/bbnf-simd/{src/lib.rs, src/aarch64/, CHECKASM-REPORT.md}`.

External:
- Langdale, "Fitting My Head Through The ARM Holes" (movemask substitutes) — branchfree.org
- ARM, "Bit twiddling with Arm Neon: beating SSE movemasks" (shrn narrow wins) — developer.arm.com
- simdjson#309 (neon_movemask_bulk half-mask loss) — github.com/simdjson/simdjson
- Langdale, "Finding quote pairs with carry-less multiply (PCLMULQDQ)" — branchfree.org
- Wunkolo, "pclmulqdq Tricks" (vmull_p64 equivalence) — wunkolo.github.io
- Wang et al., "Hyperscan" NSDI'19 (shufti/truffle/Teddy) — usenix.org
- Gallant, "Regex engine internals as a library" (memchr prefilter ladder, lazy DFA) — burntsushi.net
- jneem/teddy (Teddy Rust port) — github.com
- Davidoff, "The state of SIMD in Rust in 2025"; Rust std::simd docs; portable-simd#364
  (portable_simd not stable; wide/pulp as stable alternatives)
</content>

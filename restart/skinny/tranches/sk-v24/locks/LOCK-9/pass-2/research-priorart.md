# LOCK-9 pass-2 — PRIOR-ART RESEARCH (Scanner & Kernel Plane, parse-that rust)

No family allegiance. Each item reports what exists, what won/lost elsewhere for THIS problem
class, what our evidence base already fixes as law, and the named traps. Verdicts belong to
synthesis; this is the settled-fact substrate.

Host reality (binding on all of it): Apple M-series, aarch64-apple-darwin, NEON-only — no
SVE/SVE2/SME/VBMI for byte work (SPEC §0 carried law); PMU cyc/B unavailable in macOS
userspace (PROVE-09/10 VERIFY §6 — wall-clock paired ratios are the only certified metric).
All NEON intrinsics named below are stable since Rust 1.59; nothing here needs nightly.

---

## 1. The problem class and what our own evidence already fixes as LAW

The plane is: byte-parallel scanning kernels (structural classify, quote/string, digit, ws,
prefix-XOR) feeding a backtracking-PEG combinator core, on M-series NEON, honest after the
PROVE-09 refutation. Six constraints are already MEASURED and non-negotiable:

1. **No shim perf claim (SPEC §0/09 REFUTED; PROVE-09 VERIFY).** The `portable_simd`→NEON port
   is neutral-to-slower on fresh paired windows (0.81–0.98× window A). The de-nightly's honest
   value is STABILITY, not speed. Any LOCK-9 gate that says "faster" is born-defective.
2. **The double-`vaddv` dense-16 movemask is the named cost defect.** PROVE-09 VERIFY attack #2:
   the shim emitted **2271 `addv.8b` vs baseline 509** — ~4× the reduction work per mask. It is
   *live in parse-that at `number_simd.rs:54-55`* (`vaddv_u8(lo)` + `vaddv_u8(hi)`, weight-vector
   form) — confirmed by direct read this pass. This is idiom (c) of the movemask taxonomy and
   the single most-cited counterexample the plane must not reproduce.
3. **No digit-scan SIMD (SPEC §0/02 NO_GO).** Mula/Lemire in-vector accumulation is −37/−48%
   instructions but latency-bound CPI inversion 0.13→0.30 regresses the product lane.
   `number_simd.rs` ports byte-for-byte; it does NOT pursue in-vector digit parse.
4. **No block-scheduling (SPEC §0/04 refuted as a lever).** 1.05–1.06× only; the M-class OoO
   core already pipelines the single-block loop; classify is ALU-bound on the movemask bit-stitch.
   The lever is the movemask NARROW + vectorized compaction, not multi-block software pipelining.
5. **x86 dead (SPEC §0, constitution iv).** `number_simd.rs:145-235` (SSE4.2 `_mm_movemask_epi8`
   + `is_x86_feature_detected!`) and `quote_parity.rs:42-49` (`pclmulqdq prefix_xor`) are the two
   live x86 arms — abrogation-purged, keep NEON+scalar.
6. **Warm benches are disinformation; PEG semantics are backtracking.** Cold per-parse, paired,
   N≥200, two windows, opposite arm order (the exact protocol that refuted PROVE-09's 1.34–1.57×).

---

## 2. Movemask taxonomy — the canonical-producer question (newly opened)

The directive's three producers, grounded in prior art and code:

**(a) Nibble-stride `vshrn_n_u16::<4>` (the lean form).** Danila Kutenin, *Porting x86 vector
bitmask optimizations to Arm NEON* (Arm Community blog, "Bit twiddling with Arm Neon: beating
SSE movemasks"): given lanes that are all-0/all-0xFF, `vshrn_n_u16(reinterpret_u16(cmp), 4)`
narrows 128→64 bits, putting **4 bits per lane** in a 64-bit scalar — one `shrn` + one `fmov`.
This does **not** produce a dense 1-bit-per-lane mask; consumers that only need the *position* of
the next set lane iterate via `index = trailing_zeros(mask) >> 2` (nibble stride). This is the
form SPEC §0/03 certified ("vshrn narrow", CONFIRMED, ins/B −9/−12% deterministic) and is
already the primitive in **bbnf-simd** (`aarch64/movemask.rs:5`, `vshrn_n_u16::<4>`).

**(b) Dense-64 `vpaddq`/`LD4` tree (the classic PMOVMSKB substitute).** Branchfree, *Fitting My
Head Through the ARM Holes* (2019): a genuine dense 1-bit-per-lane mask over 4×128-bit vectors
costs a **3-stage `vpaddq` reduction (7 ops simple / 6 ops LD4-interleaved)** — strictly cheaper
only when four vectors are processed together. This is idiom (b); it is what assembles the
single 64-bit stripe in a simdjson-style `classify_stripe_64`.

**(c) Double-`vaddv` dense-16 (REFUTED).** `number_simd.rs:54-55` — two `vaddv_u8` halves of a
weight-masked vector. The 2271-vs-509 defect (§1.2). Never for a per-16-byte scan.

**Is the repack load-bearing?** Prior art gives the clean answer, matched to code:
- **bbnf-simd's `movemask_u8x16` fully repacks** the nibble-stride result back to a dense u16 via
  `vand`/`vsri`/`vzip1q`/`vst1q` + an 8-iteration scalar loop (`movemask.rs:14-24`). That repack
  is pure overhead **unless** a downstream consumer requires dense 1-bit semantics.
- **Dense 1-bit is load-bearing ONLY where the mask is arithmetically manipulated**: quote-parity
  prefix-XOR (simdjson string region, `quote_parity.rs` — needs clmul/carry over a dense mask),
  popcount, or cross-mask bit-interleave. The Arm "beating SSE movemasks" blog and simdjson's
  arm64 stage1 both keep a dense 64-bit bitset precisely because stage2 does bit arithmetic on it.
- **The ~5 pure position-scan sites in parse-that** (`scanners.rs:208,286`, `span_parser/scan.rs:152`,
  `structural_bitmap.rs:43`, `ws_comment.rs`) do `.to_bitmask().trailing_zeros()` — dense semantics
  are NOT required; a lean nibble-stride producer with `tz>>2` eliminates the repack.
- The honest experiment the directive asks for is an **instruction-count** (not reduction-op count)
  comparison of repack-`movemask_u8x16` vs lean-`shrn→fmov→tz>>2` at one real classify site. Prior
  art predicts lean wins the scan sites and dense is mandatory at the prefix-XOR site — a
  producer *split by consumer need*, not one canonical producer. (`sse2neon` issue #712 documents
  the same peephole tension: SSE-idiom normalization of `_mm_movemask_epi8` regresses when the
  downstream only needs a scan.)

---

## 3. Kernel-home axis (9A one shared crate ⟂ 9B parse-that keeps its own)

**Cross-crate inline reality (the deciding datum).** Directive-settled + prior art: rustc does
cross-crate inlining only for `#[inline]`-annotated (or LTO-visible) functions (rust-lang forum,
alexcrichton; rustc-book codegen options). CRITICAL trap confirmed live: **bbnf-simd's
`dispatch.rs` routes primitives through `OnceLock<PrimitiveKernels>` holding raw `fn` pointers**
(`byte_class_from_table_64: fn(&[u8;64],&[u8;256])->u64`, etc., `dispatch.rs:52-66`). An `fn`-ptr
behind `OnceLock` is an **indirect call that survives in-loop even under fat-LTO** — it is not a
candidate for `#[inline]` at all (the pointer's type erases the body). This is the "fn-pointer
facade blocks inline" lever. Its sibling `SelectedClassifier` uses **enum dispatch**
(`Scalar | NeonTbl4`, `dispatch.rs:11-15`) which *does* ICF-fold to the inline body. So the
kernel-home question is downstream of a dispatch-shape question: a shared crate that exposes
kernels behind fn-ptrs regresses; one that exposes `#[inline] #[cfg(target_arch="aarch64")]`
bodies (or generic monomorphized entries) inlines across the `[patch]` path-dep. The measurement
owed: `otool -tv` on a LIVE hot scanner reached through the path-dep, asserting zero
indirect-branch-in-loop — NOT an isolated probe crate (isolated ICF proves nothing about the hot
loop).

**Substrate-divergence datum (both outcomes valid; no home verdict).** The two repos' classifiers
are idiomatically opposite by *deliberate design*, per direct read:
- **bbnf-simd** is a mature 64-byte-block engine: `classify_tbl4.rs` uses `vqtbl4q_u8` over a
  64-entry lo6 table (idiom b substrate); `structural_classify_64.rs` **deliberately rejects the
  table classifier** for the 11-byte structural set `' " / ( ) [ ] { } ; :` because it has
  lo-nibble collisions (`; [ {`≡0xB) AND a lo-6-bit collision (`;`=`{`=0x3B), using a `vceqq_u8`
  equality fan (scheme α) instead — with the simdjson lo×hi two-LUT AND-fold named as the upgrade
  path only if the set widens past ~16. Padded to 64-byte blocks.
- **parse-that** is a lighter 16-byte per-terminal scanner: `simd_eq(splat).to_bitmask()` compares
  (`quoted_simd.rs`, `decode.rs`, `structural_bitmap.rs`), `PaddedView` with `INPUT_PAD_BYTES=64`
  (`state.rs:57`) but 16-byte stripe loads.
- Divergence axes are real and large: `vqtbl4q_u8` single-4-table vs `vceqq` fan vs `simd_eq`
  compare; 64-byte block vs 16-byte stripe; dense-repack movemask vs `to_bitmask`. A native
  rebirth of "a position scan" (idiom a) and "the dense-64 stripe" (idiom b) would duplicate the
  *narrow primitive* (both need vshrn) but NOT the classify substrate — the LOC-duplicated count
  is the vshrn/quad-load/next-set-bit tier, ~5-8 primitives; the classify tier diverges.

**The shared-corpus precedent (already built).** bbnf-simd ships a **checkasm parity harness**
(`tests/checkasm_parity.rs`, CHECKASM-REPORT) modeled 1:1 on FFmpeg/dav1d `checkasm.h`:
randomized identical src0/src1, `call_ref`(scalar)/`call_new`(NEON) dst compare, alignment sweep
0..64 × len {1,16,32,64,128,1024,8192}, stack-clobber canary, SIGSEGV/BUS/ILL guard, robust-mean
outlier filter, opt-in bug injection (`BBNF_SIMD_INJECT_BUG` shifts first position +1, must be
caught). This is the mature differential-conformance substrate; parse-that has per-file tests
(`quoted_simd_test.rs`(33), `number_simd_test.rs`(12), `structural_bitmap_test.rs`(6) — R3 §7) but
no unified checkasm. A shared home would inherit checkasm; twin homes each need their own scalar
parity oracle + injection gate (the PROVE-09-law "scalar parity oracle and checkasm-liveness").

---

## 4. Scanner-as-DFA (9C) — what won and lost elsewhere

**Sheng / Hyperscan is the SOTA precedent, and it names the two blocking constraints.** Geoff
Langdale, *Say Hello To My Little Friend: Sheng* (Branchfree 2018) + Hyperscan NSDI'19: a
shuffle-based DFA does `state' = shuffle(transition_table, class(byte))` per byte.
- **The 16-state ceiling on NEON.** Sheng's basic form caps at **16 states** because one SIMD
  shuffle indexes a 16-lane table. Sheng32/Sheng64 lift this ONLY via **AVX-512 VBMI**
  (`hyperscan` CHANGELOG) — which M-series lacks (SPEC §0 NEON-only law). On `vqtbl1q_u8` the hard
  cap is **16 states per single-table transition**; `vqtbl4q_u8` reaches 64 lanes but the index is
  still one byte → still a 16-state-effective transition per shuffle unless the class alphabet is
  packed. This is the register-budget wall the directive says is "measured closed for the ASCII
  archetype" — Sheng confirms it generalizes poorly past 16 states on NEON.
- **The per-byte serial-dependency stall (the directive's core 9C question).** Every Sheng-class
  DFA carries a **loop-carried dependency**: `state[i]` depends on `state[i-1]` via the shuffle,
  so the transition cannot pipeline across bytes — Hyperflex (arXiv 2512.07123, 2025) and the
  Mcclellan-DFA memory-latency analysis both cite this as the fundamental DFA-throughput ceiling
  vs the *branch-free every-byte classify* (simdjson stage1), which has NO loop-carried dep and
  saturates the vector units. So `state' = tbl(row[class], state)` **does** incur a serial stall;
  the branch-free classify does not. The measured head-to-head owed: DFA-lowered dense classify vs
  hand simdjson `vceqq`-fan on the dominant every-byte pass — prior art predicts the hand classify
  wins the dense case and DFA wins only *skip-heavy* / prefilterable patterns (which is exactly
  what PROVE-10's `Dfa::find` prefilter already captures at 8.8–29.5×).
- **The DFA source-of-truth is a two-crate reconciliation risk.** R3 §1: parse-that has an in-repo
  `bbnf-regex = { path = "regex" }` (6742 LOC, HIR→NFA→DFA→Hopcroft, `automata/dfa.rs`); the
  bbnf-lang side vendors its own. LOCK-5 owns whether the byte-fixpoint-gated grammar→DFA pipeline
  exists at scanner fidelity. Report status, not verdict: **absent LOCK-5 delivering that
  substrate, 9C has no source** — it is a consumer of a cross-lock predecessor that may not land.
  simdjson itself does NOT lower its scanner to a DFA — its stage1 is hand-written branch-free
  classify precisely because the DFA serial-dep loses on dense input.

---

## 5. Lazy / windowed input-surface (9D) — the consumer question

**simdjson's streaming precedent refutes the byte-partial premise.** `document_stream` /
`parse_many` (simdjson `doc/parse_many.md`): simdjson streams at **whole-document granularity**
(NDJSON / concatenated docs separated by JSON whitespace), never a byte-partial single document;
`batch_size` is a window over *document boundaries*, and `truncated_bytes()` handles only the
tail. There is **no prior art for streaming a partial single document** in a high-throughput SIMD
parser — the padded-block invariant (parse-that `INPUT_PAD_BYTES=64`, simdjson's 64-byte block)
is fundamentally incompatible with an incremental byte-window, because every stripe load assumes
64 trailing bytes exist. A straddle guard at window boundaries is **expected neutral-to-slower**
(directive constraint) — it re-reads the boundary stripe.

**PEG backtracking gives no bounded-backtrack guarantee.** parse-that is a backtracking PEG (R3
§5: `ParserResult=Option<Output>`, `many`/`sep_by` with rewind); with `diagnostics` recovery the
`furthest_offset` tracks arbitrarily far. There is no PEG theorem bounding rewind distance to a
window — packrat memoization bounds *time* not *rewind span*. The cheap decider the directive
asks for (instrument `furthest_offset − offset` distribution across JSON+CSS ± diagnostics) is the
only honest way to know if a bounded window is even sound; prior art says PEG rewind is unbounded
in the worst case.

**Consumer-less-substrate is the named defect (constitution viii, Y.13).** No parse-that grammar
(JSON/CSS/CSV) has a chunked-feed streaming consumer. A windowed scanner plane with no in-wave
streaming consumer is a design defect, not a feature. The only *candidate* value is latency /
time-to-first-structural + peak-RSS (never throughput), and it must be MEASURED — simdjson's own
streaming value is bulk-load memory, not TTFS, and it is measured, not asserted.

---

## 6. Named traps (from comparable systems)

- **Two-arm window asymmetry** (PROVE-09 VERIFY): the classic self-deception — measure baseline
  and patched under different load and cherry-pick the quiet "modified" window. Paired
  interleaving with opposite arm order is mandatory even for "obvious" wins.
- **Reduction-op count masquerading as instruction count** (PROVE-09 attack #2): counting
  `addv.8b` alone missed nothing there, but the general trap is that a lean-looking narrow can
  hide a repack; count *total* instructions at the real site.
- **fn-pointer / OnceLock facade** (bbnf-simd `dispatch.rs`): erases the body, defeats `#[inline]`,
  survives fat-LTO as an in-loop indirect branch. A shared kernel crate that exposes kernels this
  way regresses the hot loop.
- **Sheng 16-state ceiling + serial dependency** (Hyperscan/Hyperflex): shuffle-DFA loses to
  branch-free classify on dense every-byte input on NEON, and cannot exceed 16 states without VBMI
  the M-series lacks.
- **lo6 / lo-nibble collision** (bbnf-simd `structural_classify_64` scheme α): a table classifier
  silently mis-buckets structural bytes with colliding low bits; the `vceqq` fan is exact — a
  DFA/table lowering of the structural set must prove collision-freedom or it is wrong, not slow.
- **Padded-block vs streaming incompatibility** (simdjson, parse-that `PaddedView`): a windowed
  surface breaks the 64-byte trailing-pad invariant every SIMD stripe load depends on.
- **PROVE-09's scratch-tree clobber** (REPORT §Integration): PROVE-09 and PROVE-10 raced on one
  scratch copy (`regex/prefilter.rs` WIP takeover). parse-that is READ-ONLY (P23); any mutation
  needs a named cross-lock supersession and file-territory sequencing (PT-2 W0/G2 precedent).

---

## Citations

- SPEC §0 (certified swath + refutation laws), SK-V24; PROVE-09 REPORT+VERIFY, PROVE-10
  REPORT+VERIFY (prototypes/PROVE-09,10); PT-2.md (docs/tranches/sk-v24); R3-parse-that-rust.md.
- Code read this pass: `bbnf-simd/src/aarch64/movemask.rs`, `dispatch.rs`, `classify_tbl4.rs`,
  `structural_classify_64.rs`, `CHECKASM-REPORT.md`; parse-that `number_simd.rs:40-70`,
  `quoted_simd.rs`, `decode.rs`, `ws_comment.rs`, `quote_parity.rs`, `scanners.rs`,
  `span_parser/scan.rs`.
- Kutenin, *Porting x86 vector bitmask optimizations to Arm NEON* / "Bit twiddling with Arm Neon:
  beating SSE movemasks", developer.arm.com community blog.
- Langdale, *Fitting My Head Through the ARM Holes* (PMOVMSKB substitutes, 2019) and *Say Hello To
  My Little Friend: Sheng* (2018), branchfree.org.
- Hyperscan NSDI'19 (usenix.org/system/files/nsdi19-wang-xiang.pdf); intel/hyperscan CHANGELOG
  (Sheng32/64 need AVX-512 VBMI); Hyperflex, arXiv 2512.07123 (SIMD-DFA serial-dep analysis).
- sse2neon issue #712 (SSE-idiom normalization / movemask peephole tension), github.com/DLTcollab.
- simdjson `doc/parse_many.md` + `document_stream` API (whole-document streaming granularity).
- Rust cross-crate inlining: rust-lang users forum #55004; rustc-book codegen-options.
</content>
</invoke>

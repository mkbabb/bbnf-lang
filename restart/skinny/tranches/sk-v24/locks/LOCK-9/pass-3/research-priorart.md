# LOCK-9 pass-3 — PRIOR-ART RESEARCH: the scanner & kernel plane (parse-that rust)

Scope: parse-that's scan/kernel layer — the eight `portable_simd` scanners
(`scanners.rs`, `parsers/scan/{decode,ws_comment,structural_bitmap,quoted_simd,quote_parity,number_simd}.rs`,
`span_parser/scan.rs`, `state.rs`), the two x86-mixed files (`number_simd.rs`,
`quote_parity.rs`), the kernel-sharing question with `skinny/crates/bbnf-simd/`, and the
divan/oracle bench protocol. No family allegiance; this maps external prior art onto the
problem class and names the constraints our own evidence base already fixes. REGISTRY not read.

---

## 1. The movemask problem class — the load-bearing lever (families 9A/9B; PT-2.W1; GN-2.W4)

parse-that has **no PMOVMSKB**: `to_bitmask` on aarch64 must be synthesized. This is the single
most-studied kernel in the whole plane, and the SOTA is settled.

- **Langdale, "Fitting My Head Through The ARM Holes" (branchfree.org, 2019)** — the canonical
  catalogue of PMOVMSKB substitutes for NEON. Key finding directly binding on us: simdjson does
  *not* need a single movemask, it needs **4 in a row into one 64-bit register**, and the
  amortized ARM sequence is cheap *only when batched* — a naive per-vector movemask is the wrong
  shape. [1]
- **Kutenin (Google), "Porting x86 vector bitmask optimizations to Arm NEON" (ARM community
  blog)** — the `shrn v.8b, v.8h, #4` idiom: treat the 128-bit compare result as eight `u16`,
  shift-right-narrow by 4, yielding a **64-bit mask at 4-bit-per-lane (nibble) stride** in ONE
  instruction, then `fmov` to GPR. Applied to zstd/simdjson with large speedups. [2]
- **Highway / Lemire, "Scanning HTML at Tens of GB/s on ARM" (Software: Practice & Experience
  55(7), 2025; arXiv 2503.01662)** — production confirmation: WebKit and Blink both ship
  `vshrn_n_u16(cmp, 4)` as their movemask; measured **~20× over prior scalar HTML scanning**. This
  is the exact intrinsic PROVE-10's disasm shows (`shrn.8b v0,v0,#4` + `ands #0x8888…`) and that
  `bbnf-simd/src/aarch64/movemask.rs` already implements (`vshrn_n_u16<4>`). [3][4]

**What LOST — and it is our own refutation as law.** PROVE-09's de-nightly shim built `to_bitmask`
as a bit-position `vand` + **two `vaddv_u8` halves**; the verify fleet's `otool -tv` counted
**2271 `addv.8b` vs 509 baseline** — ~4× the reduction work per movemask, the mechanism behind the
measured regression (VERIFY attack #2). The external literature *predicts* this: `addv`
horizontal-reduce is the slow substitute; `shrn #4` is the fast one. Prior art and our evidence
agree — **the double-addv movemask is a named trap; the vshrn-narrow nibble-stride is the only
lawful `to_bitmask`.** Consumers that *iterate* the mask index at `tz >> 2` (nibble stride);
presence-only consumers stay on `any()`=`vmaxvq_u8`. [1][2][3] + PROVE-09 VERIFY.

**The classify-floor lever (GN-2.W4).** SPEC §0/04 refuted multi-block software pipelining
(1.05–1.06× only; the M-class OoO core already pipelines the single block; classify is ALU-bound
on the movemask bit-stitch). The literature names the real lever identically: the cost is the
**movemask narrow + vectorized compaction**, not block scheduling — Langdale's "batch the
movemasks" and Lemire's compaction discussion. Our F1 lever is externally corroborated. [1][3]

---

## 2. portable_simd vs core::arch on aarch64 — the stability-not-speed law (family 9B; PT-2)

- **rust-lang/stdarch #1659** ("aarch64: use portable SIMD intrinsics where possible") and
  **rust-lang/portable-simd #312** ("weird codegen with masks > 8 lanes") document that
  `std::simd` mask reductions lower to suboptimal aarch64 sequences — the very artifact PROVE-09's
  prover *mistook* for a speedup and the verify fleet *disproved* at the instruction level
  (`tbl`/swizzle identical 516=516; the delta was entirely the movemask reduction tree). [5][6]
- **"Rust SIMD Benchmark: std::simd vs NEON on Apple M4" (dev.to)** — independent measurement that
  hand `core::arch::aarch64` NEON and `std::simd` are *close* on M-series when the reduction idiom
  matches; the gaps appear precisely at mask/bitmask boundaries. [7]

**Binding conclusion.** The prior art supports PT-2's charter exactly: the port's honest value is
**stability, not speed** (SPEC §0/09 REFUTED). It breaks even *iff* the hand movemask uses
`vshrn #4`; if it regresses to `addv`, it loses. The literature makes the shim's zero-churn
`to_bitmask` a liability, not a convenience — call-site churn to the nibble stride is the price of
parity, not an optimization.

---

## 3. Quote-parity / structural bitmap — carryless-multiply prefix-XOR (family 9C; `quote_parity.rs`)

- **Langdale, "Finding quote pairs with carry-less multiply (PCLMULQDQ)" (branchfree.org, 2019)** —
  the simdjson prefix-XOR-of-quotes-against-all-ones trick: fill every bit *between* matched
  quotes, carrying one boolean (odd-quote-count) across 64-bit chunks. [8]
- The ARM equivalent is `vmull_p64` (polynomial multiply), gated `target_feature="aes"` — exactly
  `quote_parity.rs:51-56` (`prefix_xor`). A single `vmull_p64` replaces the 6-shift scalar chain;
  the x86 `pclmulqdq` arm (`quote_parity.rs:42-49`) is the abrogation target; the aarch64
  `vmull_p64` + 5-shift scalar fallback survive. `bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs` is
  the shared-kernel candidate. [8]

**Trap named elsewhere and by us:** the cross-chunk carry-in (odd-parity boolean) is the classic
escape/quote state-machine bug — `bbnf-simd`'s CHECKASM report flags `escape_mask_64` carry handoff
as "the proven source of the open divergence." Any kernel-share must carry the exhaustive
`(bs_mask, bs_carry_in)` sweep with it.

---

## 4. Character classification / the prefilter kernel — and the SVE2 TRAP (families 9A/9C)

parse-that's structural classifier and the PROVE-01/05 shufti/truffle prefilter (`classify_tbl4.rs`
= `vqtbl4q_u8` 4-table) sit on the most active 2025-26 research front.

- **Langdale & Lemire (2019) table-driven branch-free classifier** — nibble→bitmask `vqtbl`
  lookup, one table pass instead of N comparisons; the basis of shufti/truffle and of
  `classify_tbl4.rs`. [3][9]
- **Lemire, "The fastest way to match characters on ARM processors" (2026-04)** — freshest
  head-to-head on Graviton 4 (Neoverse V2): traditional **NEON table-lookup 15.5 GB/s @ 0.75
  ins/B** vs **SVE2 `match` 16.0 GB/s @ 0.55 ins/B** (−25% instructions). SVE2 `match`
  set-membership is the new SOTA. [9]

  **THIS IS A CONSTRAINT, NOT A ROUTE.** SPEC §0 law: *"M-series is NEON-only (no SVE/SVE2/SME for
  byte work)."* Apple M-class — our sole certified host — has **no SVE2**. The freshest winning
  technique in the literature is **barred by our evidence base**; NEON `vqtbl` table-lookup is the
  ceiling on our hardware. A design reaching for `svmatch`/`match` is dead on arrival here. (Cost-
  model note for 9A/9D: were a Graviton lane ever certified, SVE2 would be a pluggable kernel — but
  it is out of scope under the current NEON-only certification.) [9]

- PROVE-01 (shufti/truffle prefilter, fused GATE=16) CONFIRMED skip-heavy ~7.5×/4.7×, dense ~1.0×
  (dense-neutral) — consistent with the literature: table classifiers win big on skip-heavy input,
  go neutral on dense structural alphabets.

---

## 5. Prefilter tier / memchr / Teddy — the DFA-scan seam (family 9C; PROVE-10, PT-N1)

- **BurntSushi, `regex-automata` prefilter ladder + `memchr`** — the rare-byte/`memchr`/`memchr2/3`
  entry-byte prefilter that PROVE-10 productionized into `Dfa::find` (9.6–29×, 0.99× controls,
  44/44). memchr's aarch64 backend is NEON (`vshrn` movemask again). Direct prior art for the 9C
  "scanner-as-DFA" thesis: quote/number/ws scanning become compiled DFA+prefilter products. [10]
- **aho-corasick Teddy (BurntSushi)** — packed multi-substring SIMD; **works on aarch64 via NEON**,
  best with <100 patterns, self-confirming matches. The named future tier for entry-sets larger
  than 3 bytes (PROVE-10 §5 leaves it out of scope; the Teddy/shufti multi-byte tier is the
  documented next step). [11][12]

**Design implication for 9C/9D.** The external ladder is: `memchr` (1–3 bytes) → Teddy (≤~100
short patterns) → shufti/truffle (class membership) → full DFA. A grammar-derived cost model (9D)
selecting among these tiers at combinator-compile time is exactly regex-automata's architecture;
the prior art says the *tiers* are real and the *selection* is the win, not any single kernel. [10][11]

---

## 6. Kernel-sharing & differential testing — one-kernel-library (family 9A)

- **FFmpeg/dav1d `checkasm`** — the reference framework for the kernel-sharing gate: every
  hand-SIMD variant validated *identical* to a C scalar reference and benchmarked across ISA
  variants; "a massive element of dav1d's success is handwritten SIMD" made safe by checkasm's
  scalar-parity differential. [13][14]
- **`bbnf-simd/CHECKASM-REPORT.md`** already mirrors this: `tests/checkasm_parity.rs` maps each
  checkasm.h feature (randomized src0/src1, `call_ref`/`call_new`, clobber memcmp, alignment sweep,
  stack canary, SIG{SEGV,BUS,ILL} guard, outlier filter) to a Rust construct, with a scalar spec
  per aarch64 primitive and `BBNF_SIMD_INJECT_BUG` self-test. This is the physical isomorphism 9A
  wants: **one aarch64 kernel home (`bbnf-simd`) with a scalar parity oracle per kernel**, and
  parse-that scanners as thin consumers.

**The kernel-share is already half-built.** `bbnf-simd/src/aarch64/` carries the exact primitives
parse-that hand-rolls: `movemask.rs` (`vshrn_n_u16<4>`), `bitmap_prefix_xor_64.rs` (quote parity),
`structural_classify_64.rs`, `classify_tbl4.rs` (`vqtbl4q_u8`), `string_block.rs`
(quote/backslash/control), `digit_mac.rs`, plus a full `scalar/` mirror. 9A's "extract or vendor
under a sync gate" is a de-duplication of *already-parallel* implementations, not a green-field
build. The trap (CHECKASM §d): the NEON↔scalar handoff at stripe boundaries (`escape_mask_64`,
`vextq_u8` context) is the historically-divergent seam — any share must carry its exhaustive
carry-in sweep.

---

## 7. Constraints our evidence base already imposes (binding on every family)

1. **NEON-only, no SVE2/SME** (§0) — bars the freshest SOTA char-match (§4 [9]); `vqtbl`+`vshrn` is
   the hardware ceiling on M-class.
2. **vshrn-narrow movemask, never double-addv** (§0/09 + PROVE-09 VERIFY, corroborated [1][2][3]) —
   the one lawful `to_bitmask`; iterating consumers index at `tz>>2`.
3. **No digit-scan SIMD** (§0/02 NO_GO) — `number_simd.rs` NEON path is ported byte-preserving and
   x86-purged, never advanced to Mula/Lemire in-vector accumulation (CPI 0.13→0.30 latency
   inversion). The canada redress targets float *conversion* (Eisel-Lemire tax), not the digit scan.
4. **No block-scheduling** (§0/04) — the classify lever is movemask-narrow + compaction ([1][3]),
   not multi-block pipelining.
5. **Warm benches are disinformation; cold, paired, N≥200, two windows, opposite arm order** (§0 +
   PT-2.W1 gate) — macOS restricts userspace PMU, so divan wall-clock *ratios* of paired
   same-binary lanes are the certified metric (PROVE-10 §5); the two-window opposite-order protocol
   is mandatory precisely because window asymmetry fabricated PROVE-09's speedup.
6. **x86 abrogation reaches parse-that** — the two x86-mixed files (`number_simd.rs` SSE4.2 arm,
   `quote_parity.rs` `pclmulqdq` arm) lose their x86 halves; `vmull_p64`/NEON + scalar survive;
   structural token gate + `tests/no_x86.rs` enforce.
7. **Scalar parity oracle per kernel** (checkasm law [13][14] + bbnf-simd CHECKASM) — no NEON
   primitive lands without its `[u8;N]` scalar reference and differential sweep.

---

## Sources

[1] Langdale, "Fitting My Head Through The ARM Holes" — https://branchfree.org/2019/04/01/fitting-my-head-through-the-arm-holes-or-two-sequences-to-substitute-for-the-missing-pmovmskb-instruction-on-arm-neon/
[2] Kutenin, "Porting x86 vector bitmask optimizations to Arm NEON" — https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
[3] Lemire, "Scanning HTML at Tens of Gigabytes per Second on ARM Processors" (SPE 55(7), 2025) — https://arxiv.org/abs/2503.01662
[4] Lemire, "Checking whether an ARM NEON register is zero" — https://lemire.me/blog/2025/01/20/checking-whether-an-arm-neon-register-is-zero/
[5] rust-lang/stdarch #1659, "aarch64: use portable SIMD intrinsics where possible" — https://github.com/rust-lang/stdarch/issues/1659
[6] rust-lang/portable-simd #312, "Weird codegen with masks > 8 lanes" — https://github.com/rust-lang/portable-simd/issues/312
[7] "Rust SIMD Benchmark: std::simd vs NEON on Apple M4" (dev.to) — https://dev.to/erioharrison/rust-simd-benchmark-stdsimd-vs-neon-on-apple-m4-47jd
[8] Langdale, "Finding quote pairs with carry-less multiply (PCLMULQDQ)" — https://branchfree.org/2019/03/06/code-fragment-finding-quote-pairs-with-carry-less-multiply-pclmulqdq/
[9] Lemire, "The fastest way to match characters on ARM processors" (2026-04) — https://lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/
[10] BurntSushi, aho-corasick DESIGN.md (memchr/prefilter ladder) — https://github.com/BurntSushi/aho-corasick/blob/master/DESIGN.md
[11] BurntSushi, aho-corasick Teddy README (aarch64 NEON, <100 patterns) — https://github.com/BurntSushi/aho-corasick/blob/master/src/packed/teddy/README.md
[12] BurntSushi, aho-corasick repo — https://github.com/BurntSushi/aho-corasick
[13] FFmpeg checkasm — https://deepwiki.com/FFmpeg/FFmpeg/10.1-assembly-testing-with-checkasm
[14] dav1d tests/checkasm — https://code.videolan.org/videolan/dav1d/-/tree/master/tests/checkasm

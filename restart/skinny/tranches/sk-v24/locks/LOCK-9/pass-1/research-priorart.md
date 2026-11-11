# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) — pass-1 prior-art research

Researcher pass 1. No family allegiance; registry/favored-approach withheld by protocol.
Scope: parse-that's own scan/kernel layer reborn — the eight `portable_simd` scanner files,
`quote_parity`/`number_simd`/`decode`/`structural_bitmap`, honest after the PROVE-09
refutation, the x86 purge, the kernel-sharing question with `bbnf-simd`, and the bench/oracle
protocol. Evidence base (verified this session): SPEC §0, PROVE-09 REPORT+VERIFY, PROVE-10
REPORT+VERIFY, R3, PT-2.md, `skinny/crates/bbnf-simd/` (lib.rs, dispatch.rs, movemask.rs,
CHECKASM-REPORT.md).

---

## 1. The problem class, stated

parse-that carries hand-written 16-byte-lane byte scanners in `std::simd` (nightly
`portable_simd`) across **8 files** (R3 §2a: `scanners.rs` 22 hits, `parsers/scan/decode.rs`
17, `ws_comment.rs` 16, `structural_bitmap.rs` 13, `quoted_simd.rs` 7, `span_parser/scan.rs`
7, `quote_parity.rs` 4, `state.rs` 2/comments), plus **two mixed x86+aarch64** scanners with
live x86 arms (R3 §4: `number_simd.rs:145-235` SSE4.2 + `is_x86_feature_detected!`;
`quote_parity.rs:42-49` `pclmulqdq` `prefix_xor`). The reborn plane must (a) de-nightly to
stable `core::arch::aarch64`, (b) purge x86, (c) resolve the kernel-sharing question against
the sibling `bbnf-simd` crate, (d) carry a scalar-parity oracle + bench protocol. This is a
**settled problem class** in the literature — the answers exist and are cited below.

---

## 2. What won and lost elsewhere (web prior art)

### 2a. NEON movemask — the vshrn narrow idiom is SETTLED canonical art (and it is LAW here)

The x86 `PMOVMSKB` (compare→bitmask) has **no NEON equivalent**; Geoff Langdale's
["Fitting My Head Through The ARM Holes"](https://branchfree.org/2019/04/01/fitting-my-head-through-the-arm-holes-or-two-sequences-to-substitute-for-the-missing-pmovmskb-instruction-on-arm-neon/)
(2019, the simdjson author) gives the two canonical substitute sequences; the Arm community
blog ["Porting x86 vector bitmask optimizations to Arm NEON"](https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon)
crowns the winner: **`vshrn_n_u16::<4>`** (narrowing shift by 4) collapses a 128-bit
compare result to a **64-bit mask at 4-bit-per-lane stride** in ONE instruction — the idiom
"common in JSON parsers like simdjson" and used by s2n/sonic-rs. Iterate the mask with
`tz >> 2`; for presence-only use `vmaxvq_u8` (cheap), never a reduction tree.

**This is not just prior art — it is our §0 law.** SPEC §0/03 (CONFIRMED): the ONE measured
scanner win, "fused NEON string scan (vshrn narrow)," twitter `parse_tape` 0.898-0.931×
cyc/B. PROVE-10's memchr disasm shows the exact textbook form: `cmeq.16b; shrn.8b v0,v0,#4;
fmov x8,d0; ands x8,#0x8888...`. The **named counterexample is now binding**: PROVE-09's
refuted shim used a bit-position `vand` + **two `vaddv_u8` halves** for `to_bitmask`;
`otool` showed **2271 `addv.8b` vs baseline 509** (VERIFY attack #2) — ~4× the reduction
work, the mechanism behind window-A's regression. The
[sse2neon peephole issue #712](https://github.com/DLTcollab/sse2neon/issues/712) names the
same trap: naive per-instruction PMOVMSKB translation must be recognized as the FUSED
compare+movemask idiom and lowered to `shrn`, not literally emulated. **Refutation law:
vshrn narrow, never double-addv.**

`bbnf-simd/src/aarch64/movemask.rs` ALREADY uses `vshrn_n_u16::<4>` — but its downstream
reconstruction (`vand`/`vsri_n_u8::<3>`/`vzip1q_u8` + an 8-iter scalar loop packing a `u16`)
is **more elaborate than the canonical `shrn#4 → fmov → 4-bit-stride` form** PROVE-10's
memchr lowering exhibits. A pass-1 open question: is that reconstruction (widen back to
u8x16 lane-bits) a measured win or a bbnf-simd over-engineering that a parse-that native
scanner should NOT inherit?

### 2b. portable_simd is STILL nightly; core::arch NEON stable since 1.59 → port target is clear

Per [rust-lang/stdarch](https://github.com/rust-lang/stdarch/pull/1266) and
[std::simd docs](https://doc.rust-lang.org/std/simd/index.html): aarch64 NEON intrinsics in
`core::arch::aarch64` have been **stable since Rust 1.59**; `std::simd` (`portable_simd`)
has **no stabilization date** and "maps to a reasonable implementation… not necessarily a
single instruction" — i.e. **no guaranteed optimal lowering**. The de-nightly port therefore
targets `core::arch::aarch64` (PT-2.W1's `simd/{neon,scalar}.rs`), and cannot wait on
portable_simd. Crucially the ["aarch64 has completely standard float semantics"] note means
NEON needs no arch-specific float divergence — supports the scalar oracle being byte-exact.

### 2c. simdjson / sonic-rs NEON stage1 — the bar is REAL, not a free win

[sonic-rs](https://github.com/cloudwego/sonic-rs) "rewrote many SIMD algorithms from
simdjson"; its own [performance docs](https://github.com/cloudwego/sonic-rs/blob/main/docs/performance.md)
concede **aarch64 perf is "lower and needs optimization."** The structural bitmap is built
by the branchless simdjson algorithm: compute the in-string bitmap via **prefix-XOR of the
quote mask** (`pclmulqdq` on x86 / `vmull_p64` on aarch64), then XOR against bracket bitmaps
(sonic-rs docs). This is precisely parse-that's `quote_parity.rs::prefix_xor` and
`structural_bitmap.rs`. **Lesson**: a NEON stage1 port is NOT automatically at x86 parity —
which is exactly why SPEC §0/09 REFUTED the shim's 1.34-1.57× and why PT-2 makes **zero
perf claim**; the honest value is STABILITY, expected outcome cyc/B parity, gate can fail.

### 2d. checkasm — the canonical parity+bench oracle (FFmpeg/dav1d), already adopted in-tree

[FFmpeg/dav1d checkasm](https://code.videolan.org/videolan/dav1d/-/tree/master/tests/checkasm)
([DeepWiki](https://deepwiki.com/FFmpeg/FFmpeg/10.1-assembly-testing-with-checkasm)) is the
settled discipline: **every asm variant has a C scalar fallback used BOTH as the
generic-platform impl AND as the differential correctness oracle** — randomized identical
`src0/src1` buffers, clobber/`memcmp` detection, alignment sweep, register-preservation,
signal guard, benchmark mode. `bbnf-simd/CHECKASM-REPORT.md` shows this **already ported**
(`tests/checkasm_parity.rs` maps all five checkasm.h features 1:1; `Xorshift64` PRNG,
alignment 0..64 × len sweep, `BBNF_SIMD_INJECT_BUG` proving zero false-negatives,
`BBNF_SIMD_STRICT` gate, `xtask primitive-checkasm`). This is the model for parse-that's
scanner oracle (PT-2.W1's `simd_test.rs` cross-checks `neon.rs` vs `scalar.rs` on 10k random
vectors) — but bbnf-simd's is far richer. The **"no-orphan rule"** (CHECKASM-REPORT §SK-V5:
no primitive body admitted without a real codegen/runtime consumer) is the constitution's
Y.13 consumer-less-substrate law made physical.

---

## 3. Constraints the evidence base already imposes (SPEC §0 laws — binding)

| Law | Source | What it forbids/mandates for LOCK-9 |
|---|---|---|
| **No stable-port perf claim** | §0/09 REFUTED, VERIFY | De-nightly = STABILITY only; expected cyc/B parity; no-regression gate can FAIL (shim failed window A 0.81-0.98×). PT-2 §1.1: "faster" appears in no gate. |
| **vshrn narrow, never double-addv** | §0/03 + 09 VERIFY #2 | The movemask packs via one `vshrn_n_u16::<4>`; the 2271-vs-509 `addv.8b` is the named counterexample. |
| **No digit-scan SIMD** | §0/02 NO_GO | `number_simd.rs` ports its NEON path + purges x86, but does NOT pursue Mula/Lemire in-vector digit accumulation (CPI 0.13→0.30 latency-bound inversion). Redress targets FLOAT conversion (Eisel-Lemire), never the digit scan. |
| **No block-scheduling pursuit** | §0/04 refuted | No multi-block software pipelining (M-class OoO already pipelines single-block; classify is ALU-bound on the movemask bit-stitch). The lever is movemask-narrow + vectorized compaction, NOT unrolling classify over blocks. |
| **x86 dead (4-layer abrogation)** | §0, §4, PT-2.W5 | Purge `number_simd.rs:145-235` SSE4.2 + `quote_parity.rs:42-49` pclmulqdq; structural token gate `rg -E '_mm_|x86_64::|is_x86_feature' → 0`; aarch64-only CI. |
| **M-series is NEON-only** | §0 carried | No SVE/SVE2/SME for byte work (measured); kernels are 128-bit NEON. |
| **Warm benches disinformation** | §0, P43-49 | Cold per-parse, paired interleaving, N≥200, multi-window, opposite arm order, per-block loadavg. **PMU cyc/B unavailable on macOS userspace** → divan wall-clock paired ratios are the certified metric (PROVE-10 §5). |
| **Two-arm window asymmetry** | §0/09 VERIFY | "The classic self-deception"; paired interleaving mandatory even for "obvious" wins. The refutation's whole lesson. |

---

## 4. The kernel-sharing question (9A vs 9B) — the live adjudication

**Extant baseline (PT-2, the frozen codex) chose 9B-native-rebirth explicitly**: a
`rust/parse_that/src/simd/` directory-module (`neon.rs` + `scalar.rs` parity oracle),
parse-that keeps its own kernels, gate = measured no-regression. Rationale in the record:
parse-that is **READ-ONLY** to the bbnf-lang campaign (non-clobber, P23); the port's value is
stability, not coupling; the flat `simd128.rs` shim productionizes into a colocated module.

**Evidence FOR 9A-one-kernel-library** (physical isomorphism, LOCK-6):
- `bbnf-simd` **already implements the exact primitives parse-that re-implements**:
  movemask (vshrn, `movemask.rs`), `bitmap_prefix_xor_64` (the quote-parity/clmul kernel,
  `lib.rs:169`), `byte_class_from_table_64` / `byte_class_from_eq_set_64` (shufti-style
  membership, `lib.rs:256/283`), `structural_classify_64` (simdjson stage1, `lib.rs:301`),
  `comment_body_mask_64` / `bracket_depth_mask_64` (CSS, `lib.rs:316/336`),
  `bulk_emit_positions_64` (compaction). parse-that's `quote_parity`/`structural_bitmap`/
  `decode`/`ws_comment` are the **same kernels re-derived** — literal duplication.
- The vendoring-under-sync-gate mechanism is **proven in-tree**: R3 §1 shows parse-that
  already consumes `bbnf`/`bbnf-ir`/`bbnf-regex` via the `.cargo/config.toml` patch table;
  the repo's own git log shows the `csp-solver` "re-vendor src/ from csc411@… under a sync
  gate" pattern (canonical-diff commits) — the exact 9A mechanic already runs for another
  crate.
- PROVE-09 proved the consumption is **mechanically cheap**: the 8 scanners each swapped ONE
  `use` line (`std::simd::prelude::*` → the shim) with **zero call-site churn**. A vendored
  `bbnf-simd` vocabulary would be the same one-line swap per scanner.

**Evidence FOR 9B-native-rebirth**:
- Non-clobber: a shared crate needs BOTH repos depending on one home; parse-that is
  read-only, so 9A means either extracting `bbnf-simd` to a neutral home or vendoring it INTO
  parse-that under a gate (adds a sync obligation the two-homed drift risk trades against).
- bbnf-simd's `movemask.rs` reconstruction (§2a) may be over-built for parse-that's needs;
  a native scanner wants the lean `shrn#4→fmov→4-bit-stride` form directly.
- PT-2 already specifies 9B and the LOCK-6 verdict on "reasonable isomorphism" is not yet in.

**The measured question the design loop must answer** (per constitution iii/vii — measured,
not asserted): does the **maintenance/drift cost of two movemask homes** (bbnf-simd +
parse-that/simd, which could silently diverge on the double-addv-vs-vshrn choice — the exact
refuted defect) exceed the **coupling cost of a vendored sync gate**? The null hypothesis
(9B) must be defeated by measured drift cost or it stands (mirrors LOCK-6D's framing).

---

## 5. Traps named in comparable systems (carry as guards)

1. **Literal PMOVMSKB translation** ([sse2neon #712](https://github.com/DLTcollab/sse2neon/issues/712)):
   translating x86 movemask instruction-by-instruction yields the double-reduction the §0/09
   refutation caught. Recognize the FUSED compare+movemask idiom → `shrn`.
2. **"portable_simd is the problem" misattribution** (§0/09 VERIFY #2): the shim's slowness
   was NOT the abstraction — `tbl` (swizzle_dyn) was codegen-identical (516=516); the defect
   was the hand `to_bitmask`. Blaming the framework hides the real fix (the right idiom).
3. **NEON stage1 = free x86-parity win** (sonic-rs' own aarch64 caveat): NO. The bar is real;
   a port that assumes a win reproduces §0/09's refuted 1.34-1.57×.
4. **Digit-scan SIMD** (§0/02): the seductive Mula/Lemire in-vector accumulator regresses the
   product lane (latency-bound). number_simd stays a behaviour-preserving byte port.
5. **Block scheduling** (§0/04): multi-block pipelining is a 1.05× mirage on OoO cores.
6. **Shared-scratch race** (PROVE-09 §Integration, PT-2 R6): the PROVE-09 and PROVE-10
   sessions clobbered one scratch tree (`simd128.rs` + `prefilter.rs` co-write, a foreign
   `lto=false` profile edit). LOCK-9 work partitions file territory + banks one baseline
   (PT-2.W0 = PT-1.W0 joint handshake) before any parallel W1.
7. **Warm/single-window benches** (§0): the mandatory oracle is cold, paired, two-window,
   opposite arm order; PMU where the OS allows, divan wall-clock ratio otherwise.

---

## 6. Seam to the regex continuum (9C boundary) + the memchr prefilter fact

PROVE-10 (CONFIRMED, §0/10): `Dfa::find` start-state memchr prefilter, **8.8-29.5×** on
prefilterable patterns, 0.99× controls, compile-neutral, 44/44 tests, 245-LOC additive. Its
memchr aarch64 backend lowers to the **same vshrn#4 movemask** (PROVE-10 §2c disasm) — the
kernel vocabulary is shared with the scanner plane. The GN-2.W1⇄PT-1.W5 dispatch law
(INDEX): one generator lowering per scan site — self-loop → `PrefilterSet::find_member`
(bbnf-simd), regex-leaf → `dfa.find` (bbnf-regex), **never both**. LOCK-9's 9C route
(scanner-as-DFA, dissolve into LOCK-5) must reconcile with this seam: `bbnf-simd`'s
`find_ascii_set_member64` (`lib.rs:208`) and bbnf-regex's `Dfa::find` are two entry points
to the same prefilter idea — a candidate double-home for the gestalt to collapse.

---

## 7. Pass-1 open questions for synthesis

- **Movemask form**: is bbnf-simd `movemask.rs`'s widen-back reconstruction measured-optimal,
  or should the native/shared kernel use the lean `shrn#4→fmov→4-bit-stride` PROVE-10 form?
- **9A vs 9B**: the drift-cost-vs-coupling-cost measurement (§4) — the null (9B/PT-2) stands
  until 9A's shared home is shown to reduce measured maintenance below two-home drift.
- **Oracle**: adopt bbnf-simd's full checkasm harness (alignment sweep + clobber + signal +
  inject-bug + strict gate) for parse-that scanners, or the leaner `simd_test.rs`? The
  no-orphan rule (Y.13) is binding either way.
- **number_simd / quote_parity**: x86 arms purged; NEON path retained byte-for-byte; the
  float redress (§0/02, Eisel-Lemire) is GN-2.W5 territory, not the scanner port.

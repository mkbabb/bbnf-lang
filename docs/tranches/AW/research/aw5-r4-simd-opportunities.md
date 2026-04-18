# AW-V.R4 — SIMD facilities the codebase is leaving on the table

## 1. Angle headline

Apple M4 Max has ISA-level facilities (SME/SME2, PMULL, TBL multi-register, I8MM, `prfm`) the current NEON kernels do not touch. The shift-XOR ladder dominates because PMULL is `#[cfg]`-gated but never enabled; the `vshrn_n_u16 #4 + vaddv_u8` movemask is inferior to sonic-simd's `vpaddq_u8` cascade; v32 fused store is a tautology load-store; "17-digit NEON" does not exist — canada was won by *removing* SIMD from the integer scan. Concrete per-site audit follows.

## 2. ISA primitive inventory — Apple M4 Max (verified)

From `sysctl hw.optional.arm.*` on the host:

| Feature | Present | Usable from `core::arch::aarch64`? |
|---|---|---|
| NEON (FEAT_ASIMD) | baseline | yes |
| `FEAT_PMULL` | 1 | yes — `vmull_p64` (requires `target_feature="aes"` in Rust) |
| `FEAT_SHA3` | 1 | `veor3q_u8` ternary XOR — unused |
| `FEAT_DotProd` | 1 | `vdotq_s32` — unused |
| `FEAT_I8MM` | 1 | `vmmlaq_s32`, `vusmmlaq_s32` 2×8×2 int matmul — unused |
| `FEAT_BF16` | 1 | `vbfmmlaq_f32` — unused |
| `FEAT_SME`, `FEAT_SME2` | 1 | **SVL=512 bits**. `core::arch::aarch64` has no SME intrinsics as of nightly 2026-04; need inline asm (`.arch armv9-a+sme2`, ZA tile spill). |
| `FEAT_SVE*` | **absent** (no `FEAT_SVE` flag on M4; SVE only under SSVE streaming mode via SME) | Rust nightly SVE intrinsics inapplicable outside SSVE. |
| `FEAT_DPB2` | 1 | `vst1q_u8` pointed cache-line ops; prefetch via inline asm `prfm`. |

Firestorm/Everest core (dougallj `firestorm-simd.html`): 4 × 128-bit ASIMD pipelines, 3 loads/cyc, `vceqq_u8` L=2 rtp=0.25, `vpaddq_u8` L=3 rtp=0.5, `vqtbl1q_u8` L=3 rtp=0.5, `vqtbl4q_u8` L=6 rtp=1.0, `vmull_p64` L=3 rtp=1.0, `vextq_u8` L=2 rtp=0.25. M4 Everest is the same μ-arch generation with widened ASIMD issue; treat as a lower bound on throughput.

## 3. Per-primitive audit

**PMULL (`vmull_p64`).** `parity.rs:177-188` has the implementation. Gated behind `target_feature = "aes"`. Cold-path `cargo rustc --print cfg -- -C target-cpu=apple-m4` emits `target_feature="aes"` — so the CLMUL path *is* reachable. sonic-simd's comment (`sonic-rs-0.5.8/src/util/arch/aarch64.rs:20`) "*Not use PMULL instructions, but it is apparently slow*" is **stale** — Firestorm PMULL is L=3 rtp=1.0, matching shift-XOR's 6-shift 6-xor = 12 ops / 6 cyc at rtp=1. PMULL wins at 3 c vs 6 c on the critical path. Site: every quote-stripe invocation in `neon.rs:540`. **Gap: confirm the `aes` cfg actually gates true at codegen; if so shift-XOR is dead code on M4; if no, an unconditional PMULL arm with `is_aarch64_feature_detected!` gating at the per-parse prelude is one line.**

**TBL multi-register (`vqtbl2/3/4q_u8`).** **Zero uses in the codebase.** `neon.rs:212-215` uses two `vqtbl1q_u8` on the same input (low/high nibble). The AW-V wide-LUT path (`neon.rs:332-335`) uses **four** `vqtbl1q_u8`s for the nibble×byte cross-product. Firestorm data: `vqtbl2q_u8` L=3 rtp=0.5, `vqtbl4q_u8` L=6 rtp=1.0. Two `vqtbl1q_u8` on the same nibble-input cost 2×3=6 cyc at rtp=0.5 — identical to one `vqtbl4q_u8`. **Not a win on M4 Firestorm**, but a 64-entry LUT (four-register) lets you classify structural-kind not just presence in one pass — see §6.

**vshrn_n_u16 + vaddv movemask vs vpaddq_u8 cascade.** `neon.rs:671-693` uses `vandq_u8(bits, pat)` + two `vaddv_u8` calls. `vaddv_u8` is a horizontal-add with L=3 rtp=1.0. sonic-simd-0.1.4/src/neon.rs:151-165 uses a four-stage `vpaddq_u8` cascade (L=3 rtp=0.5) reducing 4 stripes to 8 bytes, then `vgetq_lane_u64` — 4 paddq ops = 6 cyc critical path for an entire 64-byte stripe, vs our 2 `vaddv_u8` × 4 chunks = 24 cyc in the same path. Site: `neon.rs:186-232` (stripe classifier) and `simd.rs:151-199` (nospace). **3–4× critical-path cost on movemask**; concrete regression to port.

**`simd_str2int` (packadd cascade).** `number.rs:239-366` is sonic-number's 16-byte packadd. Commit `0dcf9743` (2026-04-17) removed it from the *integer* path because the 16-byte load didn't amortise on 2-3 digit integers. Still used for the *fraction* path where canada has 15-digit precision. The macro cascade uses `vuzp1/2q_u8`, `vmull_u8(10)`, `vaddw_u8` — none of these leverage `FEAT_DotProd` or `FEAT_I8MM`. **`vdotq_s32` with a `[1, 10, 100, 1000]` pattern in a 4-lane accumulator is one `vdotq_s32` per 4 digits = 4 ops for 16 digits vs the current 6-op packadd cascade.** ~1.5× speedup on fraction path for canada-scale inputs.

**`vst1q_u8` pair-store in `push_compound_fused_v32`.** See §5 below — the current implementation is a self-aliased load/store tautology. LLVM MemorySSA elides it. `b3cf555e` doc-comment acknowledges this.

**Prefetch (`prfm`).** Zero uses across crates + sonic-rs. `core::arch::aarch64` exposes no `prfm` intrinsic; `core::intrinsics::prefetch_read_data` (unstable) or inline asm works. Apple Firestorm has an aggressive hardware stream prefetcher (6 parallel streams per core per Apple Silicon CPU Optimization Guide §3.4); for sequential stripe walks it saturates without software hints. **Not a gap.** Prefetch *is* useful for random-access patterns — `jump_to_next_structural` in the driver; not a stripe-scan concern.

## 4. The "17-digit NEON lever" — not found, mechanism inventable

`AW-V.md:684,692` projects canada +1.15× from an "NEON 17-digit lever". There is no 17-digit kernel in-tree. `simd_str2int` handles up to 16 digits per stripe. The projection is aspirational — it presumably refers to a two-stripe fraction accumulator for the 17-digit (1 + 16) total mantissa width that f64 needs, but the `parse_number_body` stops absorbing at 19 total digits (`number.rs:115`) and falls through to `str::parse` on overflow. The literal "17-digit" value is architecturally arbitrary — f64 mantissa is 52 bits ≈ 15.95 decimal digits, so 16 digits is the true SIMD boundary, not 17. **Recommendation: strike the 17-digit projection from the AW-V ledger as an overfit artefact; the real canada lever is `vdotq_s32` for the packadd cascade (4 ops vs 6) and keeping the integer path scalar per `0dcf9743`.**

## 5. `push_compound_fused_v32` pathology — structural hazard

`columns.rs:974-985` loads `vld1q_u8(packed.0.as_ptr())` then stores `vst1q_u8(packed.0.as_mut_ptr(), ...)` on the same address. `b3cf555e` acknowledges LLVM MemorySSA elides the round-trip. The seven subsequent scatter reads (`columns.rs:1005-1034`) read `packed.0[..]` as individual bytes — LLVM keeps the original scalar stores live because the reads observe them; the vector load/store is dead weight.

**W3.2 projection wrong.** The ledger claims "the elision pathology will be exercised in W3.2 when emitter wires v32 from runtime-populated SIMD scanner data, where the load/store are no longer self-aliased." This is incorrect. The load/store is still self-aliased in W3.2 — what changes is the *source* of the packed bytes. The scalar writes `packed.0[0] = kind_meta; packed.0[1] = flags_byte; …` populate `packed`; the vector op round-trips `packed → packed`; the scatter reads `packed`. **Nothing in W3.2 breaks the self-alias.** The only way to make the store observable is to change the SoA layout so the *columns themselves* are written as one 32-byte store — but they are separate allocations, so the store becomes seven disjoint narrower stores anyway.

**Alternative design.** Pack all seven narrow columns into a single **interleaved** column per compound record (AoS inside a bounded window), then `vst1q_u8` the packed record directly into the interleaved buffer. Forfeits the SoA `reduce_column<C, R>` consumer that AV.2.5 shipped. **The v32 lever is architecturally incompatible with the SoA consumer.** Drop Lever 4 from AW-V; replace with merged-column pair-store — see §6.

## 6. Novel kernel design — paired-column `stp` + TBL-4 kinded bitmap

**Design A: paired-column `stp q0, q1` for aligned (span_lo, span_hi).** `span_lo` and `span_hi` are both `Vec<u32>`. When the compound length is known-1 at codegen time (every shape-classified compound), emit a single paired store across both column tails. Concrete ISA: `stp w0, w1, [x_lo]; stp w2, w3, [x_hi]` — four 32-bit stores, or a single `stp d0, d1, [x_span]` packing both into a `f64x2`-shaped buffer that the columns alias. **No self-alias, writes land in distinct memory.** 2 cyc latency (Firestorm stp L=1 rtp=0.5). Saves 4 scalar stores vs 8 on the current path.

**Design B: TBL-4 kinded-bitmap classifier.** Today `classify_stripe_nibble` produces a 1-bit-per-byte "is structural" mask. A `vqtbl4q_u8` with a 64-entry kinds-LUT (one per valid byte value in the structural alphabet) produces a **per-byte kind code (0..=15)** in the same instruction — no second classification pass needed in the walker. Per-stripe: 4 × `vqtbl4q_u8` (one per 16-byte chunk, L=6 rtp=1.0 = 6 cyc critical); AND against a `<< (kind_bit)` pattern; OR-reduce with `vaddvq_u8`. Instruction count: 4 `vqtbl4q_u8` + 4 `vandq_u8` + 4 `vaddvq_u8` = 12 ops / 64 B = 0.19 c/B at 4-wide issue (Firestorm sustains 4 ASIMD ops/cyc). **Gain: eliminates the walker's post-scan "what kind is this?" byte-load at `compaction::compact_stripe_tzcnt:41` (`*bytes.get_unchecked(pos)`).** Walker becomes a pure bitmap walk; the `bytes` slice is only re-read for string/number payload extraction, not for structural kind resolution. At ~8% structural density this saves ~0.08 c/B on the random-access kind load.

## 7. Cross-grammar applicability

| Primitive | JSON | CSS | Sheets | BBNF |
|---|---|---|---|---|
| PMULL prefix-XOR | Yes — quote class | Yes — two quote classes (multi-class serial walker in `neon.rs:608-656` is the hot path here; PMULL subsumes the 6-shift ladder per class but the multi-class walker is byte-serial regardless) | minimal | minimal |
| TBL-4 kinded bitmap | High — 8 structural kinds fit in 4 LUT registers | Very high — CSS has 15-20 structural bytes | High — function-name PHF + structural | Medium |
| `vdotq_s32` packadd | Yes — fraction path for numeric-heavy corpora (canada) | `<number>` tokens | numerics | n/a |
| Paired-column stp | All shapes using fused compound push | All | All | All |
| Multi-key SIMD `vceqq_u8 × 4` (Lever 3) | Yes — ObjectVisitor known keys | Selector tag matching | error literals | directive kinds |

Only **TBL-4 kinded** and **paired-column stp** are primitives the codebase does not use today; the rest either exists or was measured non-performant.

## 8. Risks

- **PMULL cfg gate.** Verify `target_feature="aes"` at the enclosing fn; if the fn is in a crate without `#[target_feature]`, the CLMUL arm compiles out silently. Belt-and-suspenders: `is_aarch64_feature_detected!("aes")` runtime check at prelude, then call an `#[inline(always)]` arm that LLVM monomorphises per-feature.
- **SME/SME2 unreachable.** `core::arch::aarch64` has no SME intrinsics on stable or nightly as of 2026-04. Apple's Accelerate framework wraps SME via opaque C APIs. Inline asm entry is possible (`smstart za; smstop za`) but requires kernel thread-state cooperation — the thread's SME state is saved/restored across syscalls via `task_restartable_ranges`. **Defer SME to AW-VI or later.**
- **TBL-4 LUT density.** `vqtbl4q_u8` takes a `uint8x16x4_t` — 64 bytes of LUT held live across the stripe loop. Four-register state is aggressive register pressure; on Firestorm 32 ASIMD registers this is fine; on x86 AVX2 `_mm256_shuffle_epi8` is 16-entry-per-lane and does not have a direct 64-entry analog without two AVX-512 `vpermi2b` ops.
- **Portability.** NEON-only designs need x86 AVX2 mirrors per the cfg-gated fragment shape already in `bbnf-simd-scan::emit`. AVX-512 VBMI2 `_mm512_permutexvar_epi8` is the four-register-TBL analog; Ice Lake Xeon + Zen 4 expose it. **Graviton / AWS ARM baseline lacks SME** — the TBL-4 + PMULL primitives work; SME-gated designs do not.
- **LLVM intrinsic stability.** All intrinsics cited are stable since 1.59 (aarch64) / 1.27 (x86); `vmull_p64` requires nightly or stable ≥1.62 with `target_feature="aes"`. No regressions known on 1.85+.

## 9. Recommended W3.2 actions

1. Verify PMULL path fires on M4 cold bench (`cargo asm --target aarch64-apple-darwin` on `quote_stripe_masked`); if shift-XOR symbol present, swap cfg gate to `target_feature="pmull"` or use the runtime-detect + static-dispatch pattern.
2. Replace `movemask_u8x16` with sonic-simd's `vpaddq_u8` cascade (`to_bitmask64`). Port verbatim; same output semantics.
3. Strike Lever 4 `push_compound_fused_v32` from AW-V.W1 enablers; replace with paired-column `stp` for `(span_lo, span_hi)`.
4. Strike "17-digit NEON lever" canada projection; replace with `vdotq_s32` packadd refinement on the fraction path.
5. TBL-4 kinded bitmap as a W3.2 shape-emitter codegen option for alphabets where `|singletons| ≤ 16` and kinds are codegen-numbered.

Paths cited: `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-simd-scan/src/{neon.rs,parity.rs,compaction.rs}`, `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-json-prototype/src/{simd.rs,number.rs}`, `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/columns.rs`, `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/{AW-V.md,PROGRESS.md}`, `/Users/mkbabb/.cargo/registry/src/index.crates.io-*/sonic-simd-0.1.4/src/neon.rs:151-165`.

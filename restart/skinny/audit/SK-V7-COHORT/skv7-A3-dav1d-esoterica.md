# SK-V7 A3 — DAV1D-Style SIMD Esoterica and Arbitrary-Grammar Generalization

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: research-only. No repository files were edited. Output written only to `/tmp/skv7-A3-dav1d-esoterica.md`.

Prior cohort reads:

- `restart/skinny/audit/SK-V6-COHORT/skv6-A2-dav1d-asm-process.md` (FFmpeg/dav1d/VLC process)
- `restart/skinny/audit/SK-V6-COHORT/skv6-A5-general-grammar-abstraction.md` (BackendShape + grammar-neutrality)
- `restart/skinny/audit/SK-V6-COHORT/skv6-A6-host-asm-instruction-map.md` (M5 Max ISA inventory)
- `restart/skinny/audit/SK-V6-COHORT/skv6-B2-checkasm-hardening-plan.md` (admission discipline)
- `restart/skinny/audit/SK-V6-COHORT/skv6-B5-primitive-gap-inventory.md` (primitive vocabulary)
- `restart/skinny/audit/SK-V6-COHORT/skv6-C5-parse-that-gaps.md` (parse-that primitive gaps)
- `restart/skinny/audit/SK-V6-COHORT/skv6-C6-generality-costfacts.md` (Lock 14 leaks)
- `restart/skinny/audit/SK-V5-COHORT/skv5-A2-dav1d-process.md` (x86inc/x86util audit-grade walk)
- `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1-486` (the 9-macro vocabulary)
- `restart/MASTER-PLAN.md:491-534` (H-tranche wave routing)

## 1. DAV1D / FFmpeg / VLC reusable patterns — the discipline, not just primitives

The reusable surface partitions into a 2-layer macro vocabulary, an ABI-checked differential harness, and a per-instance data-vs-code split. None of the three is grammar-specific; all three transfer verbatim to bbnf-simd. The five process invariants are:

1. **Scalar oracle is executable truth, not documentation.** Every admitted primitive in dav1d/FFmpeg has a portable C reference at the call site of the asm body. checkasm's `call_ref` enters the C reference under armed signal handlers, then `call_new` enters the candidate; the buffers and return values are compared byte-by-byte. bbnf-simd already has scalar refs at `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs` (per skv6-A6:264) and `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs` (per skv6-A6:96) — they must remain the oracle even when the asm body diverges in speed.

2. **Forced feature masks are non-optional.** dav1d's `dav1d_set_cpu_flags_mask` (per skv6-A2:67) lets a host with AVX-512ICL exercise its SSSE3 and AVX2 candidates under the same matrix. Without forced masks a passing fast path silently rots the lower tiers it falls back to. The local proposal at skv6-B2:241-258 names the tiers (`scalar`, `swar`, `aarch64_neon`, `aarch64_dotprod`, `aarch64_i8mm`, `aarch64_sve`, `aarch64_sve2`, `x86_avx2`, `x86_avx512icl`) and the env controls (`BBNF_SIMD_FORCE`, `BBNF_SIMD_MASK`). SK-V7 must land this before any new esoterica.

3. **ABI checked-call shims at the real boundary.** FFmpeg's x86 `checkasm.asm` and AArch64 `checkasm.S` seed callee-saved GPRs and vector registers with sentinels, install stack canaries around stack-passed argument space, call the raw function pointer, then verify everything before returning (per skv6-A2:82-90). The current bbnf-simd state at `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` is not equivalent: the AArch64 `callee_saved_register_then` wraps a Rust closure (per skv6-A2:159-167), which proves nothing about the raw ASM ABI. skv6-B2:158-216 specifies the replacement: `tests/checkasm_ffi_x86_64.asm` for SysV (verify `rbx`, `rbp`, `r12-r15`, stack canary) and `tests/checkasm_ffi_aarch64.S` for AAPCS64 (verify `x19-x28`, `d8-d15`, 16-byte alignment, stack canary).

4. **Recoverable fault handling, not panic-from-handler.** dav1d/FFmpeg `checkasm_save_context` uses `sigsetjmp`/`siglongjmp` to report a faulting primitive as a failed test row, not a process abort (per skv6-A2:86). The current `tests/checkasm_parity.rs:743` signal path panics from the handler — not async-signal-safe, not recoverable. The replacement at skv6-B2:217-239 uses `sigaction` for SIGSEGV/SIGBUS/SIGILL/SIGFPE plus a `sigsetjmp` trampoline.

5. **Cycle counters carry their source.** dav1d's bench loop uses `rdtscp`/`mach_absolute_time` per-OS and applies the `count*4 <= sum` outlier rejection (per skv5-A2:237-240). bbnf-simd currently converts `Instant`-based timings into bytes/cycle using an assumed 3.5 GHz constant — not admissible. skv6-B2:295-323 binds every cycle reading to one of `x86_rdtsc`, `aarch64_cntvct`, `instant_ns`, or `external_perf`, and forbids ns→cycles conversion under `instant_ns`.

The data-vs-code split is the dav1d primitive-lift discipline in full force: shared primitives + per-instance LUT data + shared dispatch spine. dav1d's film-grain classifier (per skv5-A2:171-205 and bbnf.asm:94) ships one macro body and N per-codec `.data` tables. bbnf.asm at `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:46-60` rejects per-grammar tables inside the macro library — they live in codegen-emitted per-grammar `.asm` files. The 9 macros are grammar-neutral by construction; per-grammar specialization lives in `.data`.

The same-wave consumer rule is the load-bearing admission gate. skv6-A2:154 and skv6-B2:426-459 forbid orphan primitives: `BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, `FSM_DISPATCH_THREADED` are all `blocked_no_consumer` until a real runtime path uses them. Microbench wins do not lift status. SK-V7 must keep these four `blocked` until the CollapsedStage codegen route lands a same-wave consumer.

The x86inc.asm/x86util.asm split (per skv5-A2:17-205) is the Layer 0 / Layer 1 cleavage:

- **Layer 0** (vendored, 1978 LOC at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`): ABI handling, INIT_XMM/YMM/ZMM width macros, `cglobal`/`cextern`/`cvisible` symbol declaration, `RET` with auto-rep-ret and vzeroupper, SECTION_RODATA/SECTION_TEXT, PIC handling. Reused verbatim.
- **Layer 1** (project-authored at `skinny/crates/bbnf-simd/ext/x86/bbnf.asm`, 485 LOC, contract-only): the 9 grammar-neutral macros (`BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`). 3 of 9 have admitted bodies (`BYTE_CLASS_FROM_EQ_SET_64` + the V6-admitted `bulk_emit_positions_64` and `structural_terminator_64`). The remaining 6 are contracts.

x86util.asm at `skinny/crates/bbnf-simd/ext/x86/x86util.asm` (1036 LOC, vendored) is video-codec-specific (TRANSPOSE, HADD, PALIGNR emulation, LOAD_DIFF). bbnf imports it for the PALIGNR emulation pattern and the broadcast suffix conventions; the video-specific patterns are dead code in the parser context but are kept to satisfy the upstream macro graph.

## 2. ARMv8.2+ esoterica on M5 Max — full inventory

Host detection (per skv6-A6:9-22): Darwin RELEASE_ARM64_T6050, Apple M5 Max, `hw.optional.arm.FEAT_PMULL=1`, `hw.optional.arm.FEAT_DotProd=1`, `hw.optional.arm.FEAT_CSSC=1`, `hw.optional.arm.FEAT_SME=1`, `hw.optional.arm.FEAT_SME2=1`, `hw.optional.arm.FEAT_SME2p1=1`. Ordinary non-streaming SVE/SVE2 returned unavailable. SHA3 and AES extensions are present in the Rust target_feature default cfg.

### Instruction inventory

| Extension | Instruction | C intrinsic | bbnf admissibility | Status in tree |
|---|---|---|---|---|
| NEON baseline | LD1/ST1/TBL/EOR/AND/ORR/CMEQ/CMHI | `vld1q_u8`/`vqtbl1q_u8`/`veorq_u8`/`vceqq_u8` | universal; all 9 macros legal | `aarch64/classify_tbl4.rs:1-104`, `aarch64/string_block.rs:1-72` |
| FEAT_PMULL | PMULL/PMULL2 (64×64→128 polynomial multiply) | `vmull_p64`, `vmull_high_p64` | `BITMAP_PREFIX_XOR_64`: carryless multiply by `u64::MAX` yields the prefix-XOR in a single instruction; folds the 6-stage scalar shift-XOR chain at `scalar/bitmap_prefix_xor_64.rs:1-?` | `aarch64/bitmap_prefix_xor_64.rs` currently calls scalar (skv6-A6:101-108) |
| FEAT_DotProd | UDOT/SDOT (4×byte→i32 in one op) | `vdotq_u32`, `vdotq_s32` | digit MAC: 4-digit accumulation in one instruction; extends to 8-digit via 2× UDOT + horizontal add. Used at `aarch64/digit_mac.rs:1-71` via inline asm | partially admitted; checkasm row exists |
| FEAT_CSSC | CTZ (replaces RBIT+CLZ for trailing-zero count) | exposed via Rust `trailing_zeros()` under `-C target-cpu=native` | `BITMAP_NEXT_SET_BIT`: shrx-cursor + ctz, two-instruction hot path; eliminates the 2-instruction RBIT+CLZ sequence | inspection only; no separate body |
| FEAT_SHA3 | EOR3 (3-way XOR), BCAX (ternary AND-NOT), RAX1 (rotate-AND-XOR), XAR (XOR-and-rotate) | `vbcaxq_u8`, `veor3q_u8`, `vrax1q_u64`, `vxarq_u64` | **NEW for SK-V7**: EOR3 collapses 2-stage XOR chains in BITMAP_PREFIX_XOR_64's interior; BCAX is the canonical "in-class AND not-in-other-class" predicate for byte classification fan-outs | not used in tree; SK-V7 admission candidate |
| FEAT_LSE | LDADD/SWP/CAS (atomics) | `__atomic_*` | not relevant to parser hot path | not used |
| FEAT_AES | AESE/AESMC/AESD/AESIMC | `vaeseq_u8`, etc. | repurposable for hash/mixing; not currently a primitive contract | not used |
| FEAT_I8MM | SMMLA/UMMLA (8×8 i8 matrix multiply, 32×32 result) | `vmmlaq_u32`, `vusmmlaq_s32` | matmul over byte LUT could replace TBL+sum chains, but bbnf classifier is naturally TBL-shaped, not matmul-shaped | not used; not currently admissible |
| FEAT_FRINTTS | rounding-toward-zero with saturation | unused | parser does not round floats | not used |
| FEAT_FCMA | complex multiply | unused | irrelevant to parsing | not used |
| FEAT_BF16/F16 | BF16/F16 dot products | unused | bbnf does not need narrow floats | not used |
| FEAT_SVE/SVE2 | scalable predicated vectors | `svld1`, `svtbl`, etc. | **unavailable on M5 Max** per skv6-A6:18-22; do not target | blocked by host |
| FEAT_SME/SME2 | streaming SVE matrix engine | `sme_*` intrinsics | streaming-mode ABI/state transition cost is research-only; not a parser candidate (skv6-A6:21-22) | blocked by ABI cost |

### Top 3 admissions for the bbnf.asm vocabulary extension

The cohort-A6 priority order (skv6-A6:331-340) plus the 9-macro contract surface yields three M5-Max admissions for SK-V7. Each lands as a body for an existing macro contract; no new macro added.

1. **PMULL prefix-XOR body for BITMAP_PREFIX_XOR_64** (`vmull_p64(mask, u64::MAX)`). Single-µop replacement for the 6-stage scalar `>>1/^/>>2/^/>>4/^/...` ladder. The current AArch64 wrapper at `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4` calls scalar; the admission gate is checkasm exhaustive single-bit rows × carry∈{0,1} (skv6-B2:317-328) plus a same-wave consumer in `runtime/src/grammars/json/scan.rs` quote-mask propagation. Row impact is conditional on attribution: SK-V6 evidence points more to matcher boundaries than raw prefix-XOR cost, so this is admitted only as a clean replacement and an ARM-NEON evidence for the macro-vs-scalar pattern. Scalar reference complexity: 6 shifts + 6 XORs; admitted complexity: 1 PMULL + 1 mask-fold.

2. **CSSC-aware BITMAP_NEXT_SET_BIT body** (cursor-shift + native CTZ). The hot loop at `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs` ends in `trailing_zeros()`; under `+cssc`, LLVM emits a single CTZ. Under baseline AArch64, the same Rust source emits RBIT+CLZ. The admission step is `cargo asm` proof that the intended CTZ sequence appears under target-cpu=native, plus a doc note that the primitive's perf claim depends on the CSSC target feature. Same-wave consumer is `compact_mask` in `runtime/src/grammars/json/scan.rs`. Row impact is modest (skv6-A6:155-167): rows `citm_catalog`, `canada`, `random`, `marine_ik`, `numbers`, `instruments`.

3. **DotProd 4-digit MAC extension for digit_block_accumulate** (`vdotq_u32(acc, digits, weights)` with weights `[1000,100,10,1]`). The current `aarch64/digit_mac.rs:1-71` uses inline asm `udot {acc:v}.4s, ...`; SK-V7 promotes this to a Rust `vdotq_u32` call once nightly stabilizes the intrinsic. Extension from 4 to 8 digits via two UDOTs + horizontal add is gated by a parsing-row attribution gate per skv6-A6:280-287. Scalar reference complexity: 4 multiplies + 3 adds; admitted complexity: 1 UDOT.

Rejected for M5 Max:

- **EOR3 (FEAT_SHA3)** — useful for collapsing 2-stage XOR chains in BITMAP_PREFIX_XOR_64's prefix tree, but the PMULL admission above subsumes the entire ladder in one instruction. EOR3 stays as a fallback option on hosts without PMULL.
- **BCAX (FEAT_SHA3)** — could fuse "in-class-A AND not-in-class-B" classifier predicates, but the current bbnf-simd classifier vocabulary at `aarch64/classify_tbl4.rs:1-104` is single-class via vqtbl4q_u8; BCAX is admissible only when multi-class predicates emerge (CSS L4 selector dispatch is a candidate).
- **SVE2/SME** — host blocks per skv6-A6:18-22 and ABI cost. Streaming-mode SME2 vector-length is dynamic; a parser primitive that needs fixed widths cannot pay the streaming-mode transition overhead.
- **I8MM (UMMLA)** — matmul shape does not match the TBL-driven classifier shape. Reconsider only if a future grammar's classifier becomes naturally matrix-shaped.

## 3. x86_64 esoterica for Wave 7 successor

asmjson uses zero esoterica (per bbnf.asm:122-133): the only AVX-512 instructions it touches are `vpcmpeqb`, `korq`, and `tzcnt`. It hits 10.93 GiB/s on plain AVX-512BW + BMI1. The esoterica inventory below is the Wave 7 successor menu, not the Wave 1-6 floor.

| Extension | Instruction | Intrinsic | bbnf macro mapping | Admission |
|---|---|---|---|---|
| AVX-512BW | vpcmpeqb / korq / kmovq | `_mm512_cmpeq_epi8_mask`, `_kor_mask64` | BYTE_CLASS_FROM_EQ_SET_64 (admitted, body at `src/x86_64/byte_class_from_eq_set_64.asm`) | floor |
| BMI1 | tzcnt / blsr | `_tzcnt_u64`, `_blsr_u64` | BITMAP_NEXT_SET_BIT, BULK_EMIT_COMPRESSED loop | floor |
| BMI2 | bzhi / shrx / pext | `_bzhi_u64`, `_shrx_u64`, `_pext_u64` | EOB_PAD_CLAMP (mask construction); PEXT used cautiously (slow on Zen<4) | floor |
| AVX-512F | vpcompressd | `_mm512_mask_compressstoreu_epi32` | BULK_EMIT_POSITIONS_64 fast path on x86 | candidate |
| AVX-512VBMI | vpermb | `_mm512_permutexvar_epi8` | BYTE_CLASS_FROM_TABLE_64 fast path | candidate; scaffold at `src/x86_64/avx2/` |
| AVX-512VBMI2 | vpcompressb / vpexpandb | `_mm512_mask_compressstoreu_epi8` | BULK_EMIT_COMPRESSED (admitted contract; scaffold at `src/x86_64/avx512_vbmi2/compress.rs`) | blocked_no_consumer |
| AVX-512GFNI | vgf2p8affineqb | `_mm512_gf2p8affine_epi64_epi8` | BYTE_CLASS_FROM_TABLE_64 affine-encodable predicates only; replaces 256B LUT with 8B affine constant | candidate; scaffold at `src/x86_64/avx512_gfni/classify_affine.rs` |
| AVX-512BITALG | vpshufbitqmb | `_mm512_bitshuffle_epi64_mask` | per-bit dispatch for compact classifiers; not used by asmjson | research; scaffold at `src/x86_64/avx512_bitalg/multiclass.rs` |
| AVX-512VPCLMUL | vpclmulqdq (512-bit form) | `_mm512_clmulepi64_epi128` | BITMAP_PREFIX_XOR_64 body (admitted contract per bbnf.asm:159-176) | candidate; scaffold at `src/x86_64/avx512_vpclmul/prefix_xor.rs` |
| AVX-512VNNI | vpdpbusd | `_mm512_dpbusd_epi32` | digit MAC parallel; same shape as ARM UDOT | candidate; scaffold at `src/x86_64/avx512_vnni/digit_mac.rs` |
| AVX-IFMA | vpmadd52luq / vpmadd52huq | `_mm512_madd52lo_epu64`, `_mm512_madd52hi_epu64` | Eisel-Lemire mantissa multiply over batched numbers | candidate; scaffold at `src/x86_64/avx_ifma/mantissa.rs` |
| k-mask family | kandn / kxor / kxnor / kshiftrq / ktestq | `_kandn_mask64`, `_kxor_mask64`, etc. | FSM_DISPATCH_THREADED state-mask carriers; CollapsedStage interior | candidate; scaffold at `src/x86_64/avx512_kmask/arithmetic.rs` |

### Mislabels caught by skv6-A6:343-348

The current in-tree x86 stubs need three label fixes before SK-V7 commits:

- `vpshufbitqmb` is **AVX512BITALG**, not VBMI2 (current bbnf.asm comment may need correction).
- `vpermb` is **AVX512VBMI**, not VBMI2.
- `vpcompressb` is **AVX512VBMI2**; `vpcompressd` is **AVX512F**.

GFNI is admissible only when the class predicate is encodable as an 8-byte affine transform over GF(2⁸). The current `src/x86_64/avx512_gfni/classify_affine.rs` carries a placeholder `STRUCTURAL_AFFINE_MATRIX = 0` which is **not admissible** (skv6-A6:79-80). The codegen layer at LayoutFacts derivation time (ARCH §7.3) must derive the actual affine matrix from the class predicate; SK-V7 cannot ship GFNI bodies until that derivation lands.

## 4. Grammar-neutral primitive vocabulary extension beyond the 9 bbnf.asm macros

The current 9 macros cover scan + classify + seek + emit + bound. For arbitrary grammars (CSS L4, Sheets, BBNF-self, future user grammars), the SK-V6 primitive gap inventory at skv6-B5:28-35 and the parse-that gap at skv6-C5:28-34 identify 5 missing primitives. Each is grammar-neutral by construction; per-grammar variation lives in generated `.data`.

| New primitive | Signature | ISA admissibility | Scalar reference complexity | Current consumer status |
|---|---|---|---|---|
| `skip_class_run_64` | `(input: &[u8], cursor: usize, class_lut: &[u8;256]) -> usize` — advance past every byte that matches the class | BYTE_CLASS_FROM_TABLE_64 + BITMAP_NEXT_SET_BIT (existing); CSSC ctz for tail. Universal across x86/arm. | one TBL + one prefix-XOR for first-non-class-bit | trivia/comment/whitespace skip; needed by CSS L4 and BBNF-self; same-wave consumer must land before admission |
| `delimited_region_scan` | `(input: &[u8], open: u8, close: u8, escape: u8) -> RegionFacts {body_start, body_end, needs_decode}` | BYTE_CLASS_FROM_EQ_SET_64 + BITMAP_PREFIX_XOR_64 + structural-terminator. Universal. | 64 cmp + prefix-XOR + ctz tail | strings, comments, heredocs, quoted regions; superset of current `quoted_span_match_trusted`. skv6-C5:30 candidate `trusted_quoted_span` |
| `hex_nibble_decode_block` | `(input: &[u8], len: usize, out_units: &mut [u16]) -> ValidityMask` — variable-length 1..6 nibble runs | NEON TBL on AArch64 (existing `unescape_uxxxx` x4); AVX-512VBMI vpermb on x86 | 6 nibble decodes + range checks | CSS L4 hex escapes (variable length 1-6 nibbles); needed by Sheets cell coordinate parser (A1..XFD1048576 row/col split) |
| `first_set_speculative_dispatch_64` | `(input: &[u8], cursor: usize, first_sets: &[FirstSet;N]) -> RuleId` — admits when grammar has byte-disjoint first sets at the dispatch point | BYTE_CLASS_FROM_TABLE_64 + BITMAP_NEXT_SET_BIT (existing); BITALG vpshufbitqmb on x86 for N≤8 rules; NEON TBL on arm | one TBL + log2(N) compares | CSS L4 selector dispatch; BBNF-self rule dispatch; Sheets formula/value dispatch |
| `multi_byte_terminator_scan` | `(input: &[u8], cursor: usize, terminators: &[&[u8]]) -> usize` — find first occurrence of any multi-byte delimiter | NEON TBL+EXT chain; AVX-512VBMI vpermb + cross-lane carry | per-byte loop with Boyer-Moore tail | CSS L4 `/* */` comment terminator; Sheets multi-char operators; BBNF-self `::=` and similar |

The 5 candidates above stay in the **candidate** column of the primitive admission manifest at skv6-B2:80-93 until a same-wave consumer lands. SK-V7 must not admit any of them on speculative benefit alone — the dav1d/FFmpeg orphan-rejection rule applies.

CSS L4 specific gaps (skv6-A5:104-115 + cohort C5):

- **calc() operator precedence**: not a primitive; the Pratt spine in IR already covers it. Cost-model lever, not a SIMD primitive.
- **selector first-set speculative dispatch**: maps onto `first_set_speculative_dispatch_64` above.
- **variable-length hex escape decoder** (`\E9 ` → `é`): maps onto `hex_nibble_decode_block`. Needs explicit run-length termination on whitespace/semicolon.

Sheets specific gaps (Lock 14 allows per-grammar `.data` only):

- **cell coordinate parser** (A1, $B$2, AA12, $AAA$1048576): scalar today; admissible as a primitive once `hex_nibble_decode_block` lands (the column-letter component is a base-26 decode with a similar shape).
- **formula syntax with function calls**: ordinary grammar; recursive descent in Rust suffices.
- **type-tagged value materialization**: `DirectBuild` payload refinement per skv6-A5:298-355; not a SIMD primitive.

BBNF-self bootstrap problem: BBNF must parse itself, and the bootstrap parser cannot depend on generated `.data` tables that the grammar itself would emit. The minimal primitive set for the bootstrap is `byte_class_from_eq_set_64` (for `::=`, `|`, `(`, `)`, `[`, `]`, `{`, `}` ≤ 8 chars), `delimited_region_scan` (for literals `"..."` and `'...'`), and `skip_class_run_64` (for whitespace/comments). All three are in the candidate list; admission must include a bootstrap-self-parse test as the same-wave consumer.

## 5. DPDA / CollapsedStage architecture generalization

asmjson's 9-state DPDA over r10-PC-as-state (per bbnf.asm:316-368) is the reference shape. The macro contract at `FSM_DISPATCH_THREADED` is the SOLE FSM macro in bbnf.asm; the other 8 are recursive-descent leaf primitives. Generalizing to arbitrary grammars requires:

1. **State count fits 8 bits (≤256 states).** LR(1) post-compaction typically yields <50 states for JSON, ~40 for BBNF-self (recursive grammar), ~30 for CSS L4 declaration parsing, ~25 for Sheets cell-value dispatch. All four fit in u8. Grammars with >256 post-compaction states must fall back to scalar dispatch — this is a cost-model condition, not a hard rejection.

2. **frames_buf[N] bounded stack, configurable per grammar.** asmjson uses N=64. JSON nesting beyond 64 levels is rare; BBNF-self rule nesting is bounded by author intent; CSS L4 selector nesting is unbounded in principle (Selectors L4 admits arbitrary depth via `:is()`, `:where()`), so CSS may force the scalar fallback for deep documents. Sheets has no nesting in the cell-grammar sense. The macro at `FRAME_PUSH_BOUNDED` bounds depth at the natural ZMM width (64 bytes); doubling to 128 requires a second ZMM and a second macro variant.

3. **Hand-written NASM is the only admissible CollapsedStage route.** Per the V9.5 PSI excavation, Rust codegen of automata fails because LLVM cannot fold the implicit dispatch automaton into a single computed jump — the compiler reverts to per-state branch trees. The MASTER-PLAN at restart/MASTER-PLAN.md:146 confirms: "asmjson strict/permissive rows split on the same x86_64 host... CollapsedStage may beat asmjson only on a strict same-plane row with generated grammar tables, admitted Layer 1 primitives, and a per-grammar wrapper."

4. **Codegen output**: per-grammar `.data` tables (classifier LUT + state-transition LUT + close-bracket map) emitted into per-grammar `.asm` files. The shared FSM_DISPATCH_THREADED spine is the macro body; the per-grammar wrapper composes BYTE_CLASS_FROM_TABLE_64 → BITMAP_NEXT_SET_BIT → FSM_DISPATCH_THREADED in a state-loop chain.

### Per-grammar admissibility matrix

| Grammar | 1-byte decidable transitions | No backtracking | LR(1) compactable | Target ISA supports indirect jump | Admissible? |
|---|---|---|---|---|---|
| JSON | yes (8 structural bytes) | yes | yes (~9 states) | x86_64 / aarch64 yes | **yes — primary Wave 7 target** |
| CSS L4 declaration body | mostly (selector first-sets require 2-byte lookahead at `::`, `:not(`, `:is(`) | within a declaration, yes; across declarations, requires recovery | yes for declaration body; no for `@`-rule envelope | yes | **partial — declaration-body only; envelope falls back to scalar** |
| BBNF-self | yes (rule names start with ident-class; operators are 1-byte except `::=`) | yes | yes (~40 states, includes optional/repeat/group) | yes | **yes — secondary Wave 7 target after JSON** |
| Sheets formula | yes for cell-coord/operator/function-name dispatch | yes within a formula | yes (~25 states) | yes | **yes — tertiary target** |
| YAML | no (significant whitespace; multi-line scalars) | no (recovery is intrinsic) | no (block scalars require lookbehind) | n/a | **no — scalar parser required** |
| CSV | yes (3 byte delimiters: comma, quote, newline) | yes | yes (~5 states) | yes | **yes — trivial; possibly not worth the complexity given asmjson-floor scalar speeds** |

The admissibility predicate (4 conditions, all required) drives the CostFacts decision per skv6-A5:144-160. Falsification of any one drops the grammar to `OffsetTape`/`EventTape` for the affected region. Lock 14 forbids per-grammar branches in generic crates — the CollapsedStage selection lives in side tables, the wrapper `.asm` lives under per-grammar generated output.

## 6. DAV1D arithmetic — abstract patterns from video that transfer to grammar parsing

Video and parser hot loops share a deeper shape than the SIMD-instruction surface. Four patterns transfer directly.

| Video pattern | Parser application | LOC budget | bbnf.asm mapping |
|---|---|---|---|
| Motion vector dispatch (per-block transform selection: 4×4, 8×8, 16×16, 32×32 each with N intra modes) | Per-byte grammar rule dispatch: classifier byte → state-target table → indirect jump. asmjson's r10-PC-as-state is the parser homolog. | 20-40 LOC per per-grammar dispatch wrapper; 1 LOC for `FSM_DISPATCH_THREADED` itself | `FSM_DISPATCH_THREADED` |
| DCT butterfly pattern (pairwise add/sub with rotation factors, recursive halving) | Bitmap prefix-XOR / Hamming weight reduction (recursive shift-XOR ladder; SUM via popcount). simdjson's quote-mask ripple is the direct parser homolog. | 2-stage scalar 6-shift / 1-instruction PMULL or VPCLMUL | `BITMAP_PREFIX_XOR_64` |
| Loop filter (cross-block dependencies; neighboring pixel reads spanning block boundaries) | Cross-chunk codepoint validation (UTF-8 sequences spanning 64-byte block boundaries); cross-chunk escape carry. simdjson's escape-mask carry-in is the direct homolog. | 5-10 LOC of carry-state plumbing per primitive | `EOB_PAD_CLAMP` + caller-supplied `carry_in` for prefix-XOR |
| Quantization tables (per-mode/per-bit-depth lookup arrays, swapped at frame init time) | Per-grammar classifier LUTs (one 256-byte table per class predicate); per-state transition LUT (8 bytes × #states). | 256 B per class × #classes; 8 B × #states; total < 8 KiB per grammar | `BYTE_CLASS_FROM_TABLE_64` data section + per-grammar `.data` |

Two patterns from dav1d do **not** transfer:

- **Inverse transform stages (IDCT8/IDCT16/IDCT32)** — video's 1D-then-2D transform structure has no parser homolog. Parsers do not transpose, do not do 2D operations, and do not need horizontal reductions in the hot loop.
- **Per-pixel sign/abs/clamp arithmetic** — parser values are byte-indices and bit-positions, not signed magnitudes.

The film-grain classifier (per skv5-A2:171-205 and bbnf.asm:94) is the closest dav1d homolog to a parser classifier: it consumes a per-pixel byte and emits a class-membership boolean via a 256-byte LUT. The macro pattern is identical to `BYTE_CLASS_FROM_TABLE_64`.

## 7. The skinny ⊂ greater arch feedback loop

skinny is JSON-focused (per `restart/skinny/INDEX.md`) but the primitives must generalize. The current Lock 14 leaks documented at skv6-C6:53-83 must close before SK-V7 can claim grammar neutrality. Re-stating the leak inventory at the level the SK-V7 wave needs to own:

- **passes** (`skinny/crates/passes/src/lib.rs:30, 31, 211-238, 245-249, 577-579, 658-660, 742-750, 755-807`): `shapes_for_json`, `nominate_json`, hardcoded `Json*` shape rosters, JSON-named rule lookups, hardcoded `TapeKind::{Object, Array, Pair, ...}` switches. **Owner: SK-V7 Wave 4** (grammar-neutral CostFacts).
- **codegen** (`skinny/crates/codegen/src/lib.rs:2-3, 68-97, 117, 131-154, 180-188, 201-226`): `json_sink_direct`, `json_typed_direct`, `emit_json_*`, JSON template includes from generic crate. **Owner: SK-V7 Wave 4**.
- **parse-that-regex** (`skinny/crates/parse-that-regex/src/lib.rs:34-45, 120-178, 260-265, 268-341, 416-514, 594-719, 766-854, 854-968`): `JsonStringMatch`, `StringMode::StrictJson`, `skip_json_whitespace`, JSON number/escape/unescape names. **Owner: SK-V7 Wave 4**.
- **runtime/tape**: clean (per skv6-C6:83). Keep as grammar-neutral.

The greater V1 spec at `restart/MASTER-PLAN.md:495-541` owns the H-tranche wave routing. SK-V7 folds into:

- H.W1 (typed event cursor) — landed for Rust-state substrate
- H.W2 (bbnf-simd kernel contract) — partially landed; SK-V7 lands PMULL/CSSC bodies
- H.W3 (parse-that primitive closure) — number landed; UTF-8 fusion refuted as close
- H.W4 (SinkOnly closure + 5-shape backend_shape) — partially landed; SK-V7 lands `CostFacts` evidence
- H.W4.LOCK14 (Lock 14 remediation) — SK-V7 Wave 4 closes the named JSON leaks above
- H.W5 (primitive bodies) — SK-V7 Wave 5 admits PMULL + CSSC + UDOT-extension bodies if same-wave consumers land
- H.W6 (strict matrix) — SK-V7 close gate after Wave 4/5 reduce parse-G and direct N-direct
- Optional H.W7 — x86 CollapsedStage successor, grammar-keyed (JSON first, then BBNF-self, then Sheets)

The SK-V7 mandate per the user prompt: clean up the JSON-name leaks in passes/codegen/parse-that-regex as Wave 4, then land the M5-Max admissions above as Wave 5 only with same-wave consumers.

## 8. Admission gates carried forward from SK-V6

Every SK-V7 primitive admission must pay the gates listed at skv6-A6:351-360, restated:

1. Scalar executable spec exists in `src/scalar/<primitive>.rs` and is the oracle.
2. Same-wave runtime/generated consumer exists; no orphan primitive.
3. Checkasm or equivalent parity covers alignment, tails, random data, and corpus shapes.
4. `cargo asm`/`llvm-objdump` proves the intended instruction sequence on the target feature.
5. Row gate is measured against same-HEAD baseline/candidate binaries, not only microbench throughput.
6. Existing SK-V6 REDRESS rejections stay binding: do not relabel rejected wide string scans, parser scratch, direct source hooks, byte-output unescape, sink-local decoded stats, or sidecar cursors as new SIMD work.

The four blocked-no-consumer primitives at skv6-B2:90-93 (`BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, `FSM_DISPATCH_THREADED`) remain blocked through SK-V7 until either:

- the CollapsedStage codegen route lands a per-grammar `.asm` consumer (BULK_EMIT_COMPRESSED, FRAME_PUSH_BOUNDED, FRAME_POP_BOUNDED, FSM_DISPATCH_THREADED — all four unlock together if the JSON CollapsedStage wrapper lands), or
- a non-CollapsedStage consumer is identified during SK-V7 profile work (none currently anticipated).

The dav1d/FFmpeg orphan-rejection rule is absolute: benchmark potential does not lift status. SK-V7 must publish a primitive admission manifest at `tests/checkasm_manifest.rs` or `checkasm_primitives.toml` (skv6-B2:57-78) before admitting any new body.

## 9. Bottom line for SK-V7

- The 9-macro bbnf.asm vocabulary at `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1-486` is the right Layer-1 contract; no new macros for SK-V7. Three bodies admit on M5 Max: PMULL for BITMAP_PREFIX_XOR_64, CSSC-CTZ for BITMAP_NEXT_SET_BIT, DotProd-UDOT-extension for digit MAC.
- The dav1d/FFmpeg/VLC process discipline lands as Wave 1 infrastructure: forced feature masks, ABI checked-call shims (x86 SysV and AAPCS64), recoverable fault handling, primitive admission manifest. Without this infrastructure the new bodies are not admissible.
- 5 new grammar-neutral primitives are gap-identified for CSS L4 / Sheets / BBNF-self: `skip_class_run_64`, `delimited_region_scan`, `hex_nibble_decode_block`, `first_set_speculative_dispatch_64`, `multi_byte_terminator_scan`. All five stay `candidate` until same-wave consumers land.
- The JSON-name leaks in passes/codegen/parse-that-regex are SK-V7 Wave 4 owner work per skv6-C6:140-146. `runtime/tape` is already clean.
- The x86 esoterica menu (GFNI, VPCLMULQDQ-512, VBMI/VBMI2, VPCOMPRESS, BITALG, VNNI, IFMA, k-mask family) is the optional Wave 7 successor menu; asmjson hits 10.93 GiB/s on AVX-512BW + BMI1 alone, so esoterica admission must be gated on a separate row-impact attribution.
- CollapsedStage admissibility per skv6-A5:144-160: only hand-written NASM admissible; JSON primary target; BBNF-self secondary; Sheets tertiary; CSS L4 declaration-body only (envelope falls back); YAML rejected.

### Top 3 ARMv8.2+ esoterica admissions for the bbnf.asm vocabulary extension (recap)

1. **PMULL (FEAT_PMULL)** for BITMAP_PREFIX_XOR_64 — replaces the 6-stage scalar shift-XOR ladder with one `vmull_p64(mask, u64::MAX)`. Same-wave consumer in `runtime/src/grammars/json/scan.rs` quote-mask propagation. Admission gates: checkasm exhaustive single-bit × carry; corpus parity under `BBNF_SIMD_STRICT=1`; row gate against `scan_structurals` self-time.
2. **CSSC CTZ (FEAT_CSSC)** for BITMAP_NEXT_SET_BIT — replaces the 2-instruction RBIT+CLZ sequence with a single CTZ under `-C target-cpu=native`. Same-wave consumer `compact_mask` in JSON scan. Admission gates: `cargo asm` proof of CTZ emission; microbench sparse/medium/dense masks; row attribution drop on `emit_plain_offset`.
3. **DotProd UDOT (FEAT_DotProd)** for digit_block_accumulate — extends the current 4-digit inline-asm UDOT at `aarch64/digit_mac.rs:1-71` to 8-digit and 16-digit via two/four UDOT + horizontal add. Same-wave consumer in `parse-that-regex/src/number/mod.rs` `match_number_at_digit`. Admission gates: checkasm parity over all digit lengths; row attribution drop on `canada`/`marine_ik` direct.

End of report.

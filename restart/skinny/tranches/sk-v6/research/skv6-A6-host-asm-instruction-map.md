# SK-V6 A6 host ASM/SIMD instruction opportunity map

Date: 2026-05-15
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Repo edits: none. This report was written under `/tmp`.

## Host architecture and feature conclusion

Local commands identify the host as:

- `uname -m`: `arm64`
- `uname -a`: `Darwin ... RELEASE_ARM64_T6050 arm64`
- `sysctl -n machdep.cpu.brand_string`: `Apple M5 Max`
- `rustc --version --verbose`: `host: aarch64-apple-darwin`, rustc `1.96.0-nightly`

Relevant local feature flags:

- Present: `hw.optional.neon=1`, `hw.optional.arm.AdvSIMD=1`, `hw.optional.arm.FEAT_PMULL=1`, `hw.optional.arm.FEAT_DotProd=1`, `hw.optional.arm.FEAT_CSSC=1`, `hw.optional.arm.FEAT_SME=1`, `hw.optional.arm.FEAT_SME2=1`, `hw.optional.arm.FEAT_SME2p1=1`, `hw.optional.arm.sme_max_svl_b=64`.
- Not exposed as normal SVE: `hw.optional.arm.FEAT_SVE` and `hw.optional.arm.FEAT_SVE2` returned unavailable.
- Rust default target cfg includes `target_feature="neon"`, `aes`, `dotprod`, and many Armv8.x features, but does not enable SVE/SVE2/SME by default.

Conclusion: the host-local implementation lane is AArch64 NEON/AdvSIMD plus PMULL, DotProd, and maybe CSSC-specific scalar bit operations. Treat ordinary non-streaming SVE/SVE2 as unavailable on this host. Treat SME/SME2 as research-only for this parser: Apple/Darwin exposes SME state through streaming SVE mode, and Arm ACLE documents that streaming mode changes vector length/state handling. That is a poor fit for branchy byte-parser helpers unless a future microbench proves the streaming-mode transition and ABI costs are hidden.

External docs used:

- Rust AArch64 feature names include `sme`, `sve`, `sve2`, `pmull`, etc.: https://doc.rust-lang.org/beta/std/arch/macro.is_aarch64_feature_detected.html
- Rust x86 feature names include `avx2`, `bmi1`, `bmi2`, `avx512bw`, `avx512vbmi2`, `gfni`, `vpclmulqdq`, etc.: https://doc.rust-lang.org/std/macro.is_x86_feature_detected.html
- Rust NEON `vqtbl1q_u8`: https://dev-doc.rust-lang.org/stable/core/arch/aarch64/fn.vqtbl1q_u8.html
- Rust NEON/PMULL `vmull_p64`: https://dev-doc.rust-lang.org/stable/core/arch/aarch64/fn.vmull_p64.html
- Rust DotProd `vdotq_u32`: https://doc.rust-lang.org/core/arch/aarch64/fn.vdotq_u32.html
- Rust x86 `_mm256_shuffle_epi8`, `_pext_u64`, `_mm_clmulepi64_si128`, `_mm512_mask_compressstoreu_epi32`: https://doc.rust-lang.org/stable/core/arch/x86_64/fn._mm256_shuffle_epi8.html, https://doc.rust-lang.org/stable/core/arch/x86_64/fn._pext_u64.html, https://doc.rust-lang.org/stable/core/arch/x86_64/fn._mm_clmulepi64_si128.html, https://doc.rust-lang.org/stable/core/arch/x86_64/fn._mm512_mask_compressstoreu_epi32.html
- Rust AVX-512 VNNI/BITALG/IFMA/GFNI intrinsics: `_mm512_dpbusd_epi32`, `_mm512_bitshuffle_epi64_mask`, `_mm512_madd52lo_epu64`, `_mm512_gf2p8affine_epi64_epi8`.
- Intel AVX-512 overview and feature families: https://www.intel.com/content/www/us/en/developer/articles/technical/intel-avx-512-instructions.html and https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html
- Arm ACLE SVE2/SME/CSSC semantics: https://arm-software.github.io/acle/main/acle.html
- Apple Hypervisor SME state exposes streaming SVE mode and ZA state: https://developer.apple.com/documentation/hypervisor/4359483-hv_vcpu_get_sme_state

## Local code surfaces read

- `skinny/crates/bbnf-simd`: primitive dispatch, scalar refs, AArch64 NEON bodies, x86 asm/stub bodies, checkasm reports.
- `skinny/crates/parse-that-regex`: JSON string/number primitives, UTF-8 block path, `unescape_json_string`, x4 AArch64 Unicode escape decode.
- `skinny/crates/runtime/src/grammars/json`: generated retained parser, structural scan, sink/direct string materialization boundary.
- SK-V6 reports under `restart/skinny/tranches/sk-v6/research/` and `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md`.
- Current `skinny/RESULTS.md`.

## Opportunity map by primitive class

### 1. Byte classification and string-special classification

Current local surfaces:

- `bbnf-simd/src/aarch64/classify_tbl4.rs`: `vld1q_u8`, `vandq_u8`, `vqtbl4q_u8`, `vceqq_u8`, `vcgtq_u8`, `vcltq_u8`, `movemask_u8x16`.
- `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`: four 16-byte stripes, `vceqq_u8` fanout, `vorrq_u8`, custom movemask.
- `bbnf-simd/src/aarch64/string_block.rs`: quote/backslash/control/non-ASCII block with `vceqq_u8`, `vcltq_u8`, `vcgeq_u8`.
- `parse-that-regex/src/lib.rs`: trusted string path already consumes `string_block::scan_string_special_block` on AArch64.

Host AArch64 candidates:

- Keep primary host lane as NEON TBL/equality, not SVE.
- Exact instructions/intrinsics:
  - Load/table: `ld1`, `tbl`; Rust `vld1q_u8`, `vld1q_u8_x4`, `vqtbl1q_u8`, `vqtbl4q_u8`.
  - Equality/range: `cmeq`, `cmhi/cmhs`, `cmlo`; Rust `vceqq_u8`, `vcgtq_u8`, `vcgeq_u8`, `vcltq_u8`.
  - Logical: `and`, `orr`; Rust `vandq_u8`, `vorrq_u8`.
  - Movemask: current `vshrn_n_u16::<4>` path, or weighted-vector `vandq_u8` + `vaddv_u8`/`vaddvq_u8` path. Unify and benchmark both, because there are two in-tree movemask idioms today.
  - First-hit extraction: scalar `ctz` if compiled with `+cssc`, otherwise `rbit; clz`; Rust `trailing_zeros()` plus asm inspection.

x86 candidates:

- AVX2 floor:
  - `vpshufb`: Rust `_mm256_shuffle_epi8`.
  - `vpcmpeqb`: Rust `_mm256_cmpeq_epi8`.
  - `vpmovmskb`: Rust `_mm256_movemask_epi8`.
- AVX-512 equality-set:
  - In-tree asm already uses `vmovdqu64`, `vpbroadcastb`, `vpcmpeqb`, `korq`, `kmovq`.
  - Rust equivalent: `_mm512_cmpeq_epi8_mask` plus mask ORs under `avx512bw`.
- AVX-512 table/bit classifiers:
  - `vpshufbitqmb` is AVX512BITALG, intrinsic `_mm512_bitshuffle_epi64_mask`.
  - `vpermb` is AVX512VBMI, not VBMI2. Do not label `vpshufbitqmb` as VBMI2 in new work.
  - GFNI `vgf2p8affineqb`, intrinsic `_mm512_gf2p8affine_epi64_epi8`, only if the class predicate is actually derivable as the required affine transform. The current `STRUCTURAL_AFFINE_MATRIX = 0` placeholder is not admissible.

Expected row impact:

- Structural classification alone is not the retained blocker: `canada` structural scan is green at 69075 Mbps against the 40000 Mbps NEON floor.
- If wired into retained trusted string scan, the target rows are `twitter`, `random`, `unicode_basic`, `apache_builds`, `github_events`, `update_center`, `distinct_values`, `instruments`, and long/plain parts of `gsoc-2018`.
- Prior always-wide and delayed-wide string scanners were rejected. A new classification kernel is only valuable if it is narrower than those rejected routes or tied to a new local fact.

Falsifiability gates:

- Checkasm: per-block parity over alignment 0..63, random bytes, and corpus strings.
- Attribution: `match_tiny_plain_string + match_string_at_quote` drops on the named string rows, not just standalone microbench ns/B.
- Throughput: no repeat of REDRESS 61/62. Guard rows `canada`, `instruments`, `marine_ik`, `citm_catalog`, and `numbers` must stay within 2%.

### 2. Prefix/parity and quote/escape carry

Current local surfaces:

- `bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`: six shift-XOR stages.
- `bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`: currently calls scalar.
- `runtime/src/grammars/json/scan.rs`: quote/backslash masks feed `escape_mask_64` and `prefix_xor_64`; open checkasm report also notes an old random-input NEON tail carry divergence.

Host AArch64 candidates:

- PMULL prefix XOR:
  - Instruction: `pmull`.
  - Rust intrinsic: `vmull_p64(mask, u64::MAX)` under `neon` + `aes`/PMULL.
  - Shape: carryless multiply by all-ones gives prefix parity; fold `carry_in` by conditional invert, then verify bit orientation against the scalar six-shift implementation.
- Keep scalar `escape_mask_64` arithmetic unless a profile proves this exact carry path is hot. It is branchless integer math already.
- SVE/SVE2 prefix operations are not host candidates because non-streaming SVE/SVE2 is not exposed locally.

x86 candidates:

- PCLMUL floor:
  - Instruction: `pclmulqdq`.
  - Rust intrinsic: `_mm_clmulepi64_si128`.
- AVX-512:
  - Instruction: `vpclmulqdq`.
  - Rust intrinsic: `_mm512_clmulepi64_epi128` under `vpclmulqdq,avx512f`.
  - K-mask carry/fuse candidates: `kshiftlq`, `korq`, `kandnq`, `kxorq`, `kortestq`.

Expected row impact:

- Helps quote/string-region propagation if scan time is visible in retained parser or a future stage-1/inline path.
- Likely rows: `twitter`, `random`, `unicode_basic`, `unicode_mixed`, `y_string_unicode`, but only if profiles attribute time to quote/body mask formation, not matcher control flow.
- Current SK-V6 evidence points more to generated matcher boundaries and escape validation than raw prefix-XOR cost.

Falsifiability gates:

- Exhaustive or high-volume parity over `(mask, carry_in)` against scalar.
- Corpus scan parity with `BBNF_SIMD_STRICT=1`, including adversarial backslash-at-stripe-boundary cases.
- Throughput gate must show `scan_structurals` or generated string matcher self-time drops; otherwise reject as a microbench-only win.

### 3. Bit iteration and next-set-bit

Current local surfaces:

- `bbnf-simd/src/scalar/bitmap_next_set_bit.rs`: shift then `trailing_zeros`.
- `bbnf-simd/src/scalar/bulk_emit_positions_64.rs`: loop over `trailing_zeros`, `mask &= mask - 1`.
- AArch64 wrappers currently call scalar.

Host AArch64 candidates:

- CSSC CTZ:
  - Instruction: `ctz` from FEAT_CSSC.
  - Rust surface: `trailing_zeros()` should be checked under `-C target-cpu=native` or `+cssc`; otherwise expect `rbit; clz`.
- Baseline AArch64 fallback:
  - Instructions: `rbit`, `clz`, `lsr`, `subs/ands`, `bic` or `and mask, mask-1`.
- Candidate is mostly compile/dispatch gating, not a new algorithm.

x86 candidates:

- BMI1/BMI2:
  - `tzcnt` for next set bit.
  - `blsr` or `and mask, mask-1` to clear.
  - `shrx` for cursor shift without flag side effects.
  - `popcnt` for reserve/length.

Expected row impact:

- Low to modest, but real on rows where offset/tape emission is visible.
- R2b measured `emit_plain_offset` at 6.7-11.0% on `random`, `citm_catalog`, and `canada`; R6 also shows number/container rows with bit/offset pressure.
- Rows: `citm_catalog`, `canada`, `random`, `marine_ik`, `numbers`, `instruments`.

Falsifiability gates:

- `cargo asm` or `llvm-objdump` must show `ctz` on this host when the CSSC path is claimed; otherwise report it as the existing `rbit; clz` scalar path.
- Microbench sparse, medium, and dense masks. Do not optimize for dense masks if corpus structural density is single-digit percent.
- Row gate: `emit_plain_offset` or `compact_mask` attributed cost must drop, and no string-heavy guard may regress by more than 2%.

### 4. UTF-8 validation

Current local surfaces:

- `parse-that-regex/src/unicode/utf8_block.rs` routes AArch64 to `bbnf-simd::aarch64::utf8::validate_block`.
- `bbnf-simd/src/aarch64/utf8/validate_block.rs` checks ASCII by NEON high-bit mask, then spills to scalar validation.
- Generated retained JSON accepts `&str`, then uses `match_json_string_at_quote_trusted_utf8`; SK-V6 reports repeatedly say raw UTF-8 validation is not the hot retained leaf.

Host AArch64 candidates:

- ASCII and special-byte fast path is already correct:
  - `ld1`, `cmhs`/`vcgeq_u8` for high-bit.
  - `cmlt`/`vcltq_u8` for controls.
  - movemask via existing helper.
- Full SIMD UTF-8 validation would use byte-class tables and cross-lane carries:
  - Candidate instructions: `tbl`/`vqtbl1q_u8`, `cmhs`/`cmlo`, `uqsub`, `orr`, `and`, `ext`/`vextq_u8`, horizontal `uminv`/`vminvq_u8`.
- Do not dispatch this for retained generated JSON until a fresh profile shows `validate_utf8_codepoint` or `validate_block` as a sampled hot leaf.

x86 candidates:

- AVX2: `_mm256_movemask_epi8`, `_mm256_cmpgt_epi8`, `_mm256_subs_epu8`, `_mm256_shuffle_epi8`.
- AVX-512BW: byte compares to k-masks, `kortestq` for fast all-ASCII/all-valid exits.

Expected row impact:

- Current expected impact is near zero for retained JSON. R1/R2/R4/R1c all reject raw UTF-8 fusion as the active diagnosis.
- Possible future rows: byte-mode grammars, non-`&str` JSON parse, or validation-heavy sidecar comparators.

Falsifiability gates:

- Admission gate before implementation: sampled profiles must show UTF-8 validation above 10% self-time on focus rows.
- Correctness: all invalid sequences, boundary-prefix continuation cases, and exact error offsets.
- Throughput: at least one UTF-8-heavy row moves without regressing trusted-UTF8 retained rows.

### 5. `\uXXXX` and escaped-code-unit validation/materialization

Current local surfaces:

- `bbnf-simd/src/aarch64/unescape_uxxxx.rs`: scalar anchor, single quartet NEON, x4 quartet NEON, surrogate join helper.
- `parse-that-regex/src/lib.rs`: `validate_json_unicode_escape_run`, `decode_json_unicode_escape`, and `unescape_json_string`; AArch64 already tries `unescape_four_unicode_escapes` before scalar fallback.
- R1c/R1e/R2e/R3e distinguish retained escape validation from direct decoded output. Direct materializer rewrites were later falsified; retained Unicode-escape validation remains a narrow possible primitive if a scalar split proves it.

Host AArch64 candidates:

- Existing decode:
  - Instructions/intrinsics: `ld1`, `vld1q_u8`, `vld1q_lane_u8`, `and`/`vandq_u8`, `tbl`/`vqtbl1q_u8`, `cmhs`/`vcgeq_u8`, `orr`/`vorrq_u8`, `add`/`vaddq_u8`, `uminv`/`vminvq_u8`, `st1`/`vst1q_u8`.
- Retained validator candidate:
  - Validate four fixed-width hex units without materializing decoded chars.
  - Use `vqtbl1q_u8` for low-nibble value, range masks for digit/upper/lower hex, `vminvq_u8` or movemask for invalid detection.
  - For surrogate policy, either keep scalar pair legality first or add vector `u16` compare pack with `vcgeq_u16`/`vcleq_u16` after nibble packing. Do not add vector surrogate logic before the scalar helper split proves row impact.
- Direct decoded-output SIMD is not a fresh candidate after REDRESS 68/69 unless the output contract changes.

x86 candidates:

- AVX2:
  - `vpshufb`/`_mm256_shuffle_epi8` nibble LUT.
  - `vpcmpeqb`/range compares or saturated subtract for hex class.
  - `vpmaddubsw`/`_mm256_maddubs_epi16` and `vpmaddwd`/`_mm256_madd_epi16` to pack nibbles into code units.
  - `vpmovmskb` for invalid-lane detection.
- AVX-512:
  - `vpermb` for larger tables where VBMI exists.
  - `vpcmpub`/`_mm512_cmple_epu8_mask`, `_mm512_cmpeq_epi8_mask`.
  - `kortestq` for invalid mask.
  - `vpcompressb` only if the workload is byte-output compaction, not retained validation.

Expected row impact:

- Retained primary: `unicode_escapes`, `y_string_unicode`, escape-heavy portion of `unicode_mixed`.
- Direct primary if output contract changes later: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`; however the current local materializer family is exhausted by REDRESS 66-69.
- R2e distribution: `unicode_escapes` has 136,682 Unicode units, 98.88% already consumed by the x4 helper; residual work is validation/materialization control, not missing quartet decode alone. `unicode_mixed` has zero Unicode units and needs simple-escape handling if targeted.

Falsifiability gates:

- First gate is scalar attribution, not SIMD: split escape-run validation into a named helper and prove it owns at least 20% of `match_string_at_quote` self-time on `unicode_escapes` and `y_string_unicode`.
- SIMD gate only after scalar proof: checkasm parity on valid/invalid hex, surrogate pairs, high-surrogate-at-boundary fallback, all alignments.
- Retained throughput: `unicode_escapes >= +12%`, `y_string_unicode >= +8%`, and one of `unicode_mixed` or `gsoc-2018 >= +5%`; guard rows no worse than -2%.
- Attribution: escape-region PCs in `match_string_at_quote` drop at least 25% on `unicode_escapes` and `y_string_unicode`.

### 6. Number scanning and number materialization

Current local surfaces:

- `parse-that-regex/src/number/mod.rs`: SWAR scan for 8/4/2 digit chunks, mantissa/exponent facts, Eisel-Lemire materializer.
- `parse-that-regex/src/number/eisel_lemire/algorithm.rs`: `u128` full multiply.
- `bbnf-simd/src/aarch64/digit_mac.rs`: DotProd `udot` inline asm for 4 digits.
- x86 stubs: AVX-IFMA mantissa and AVX-512 VNNI digit MAC.

Host AArch64 candidates:

- Digit chunk MAC:
  - Instruction: `udot`.
  - Rust options: current inline asm `udot {acc:v}.4s, ...`; nightly intrinsic `vdotq_u32`.
  - Extend from 4 digits to 8/16 digits only if parsing rows prove this is hot. Weights for 4 digits can be `[1000,100,10,1]` or two-stage pair/quads depending overflow.
- Eisel-Lemire full multiply:
  - Scalar instructions remain plausible: `mul`, `umulh`, `adds/adcs` for 128-bit product. On AArch64, LLVM often lowers `u128` multiply well; prove before hand asm.
- Avoid SME/I8MM for this parser unless a packed multi-number array kernel is generated; current parser is one number at a time.

x86 candidates:

- AVX2:
  - `vpmaddubsw`: Rust `_mm256_maddubs_epi16`.
  - `vpmaddwd`: Rust `_mm256_madd_epi16`.
  - `vpsubb`/`vpcmpgtb` for digit validation.
- AVX-512 VNNI:
  - `vpdpbusd`: Rust `_mm512_dpbusd_epi32`.
- AVX-512 IFMA:
  - `vpmadd52luq`: Rust `_mm512_madd52lo_epu64`.
  - Use only if batching several mantissas; scalar `mulx`/`mul` may win for single-number control flow.

Expected row impact:

- Retained rows: `canada`, `marine_ik`, `numbers`, and the number share of `instruments`.
- Direct rows: `canada` direct showed `parse_number_array_direct` 49.1%, `materialize_f64` 12.3%, `emit_number_array_direct` 11.2% in R3, but SK-V6 synthesis says number parsing is not the first retained parse close and `numbers` direct is already passing.
- Best expected impact is row-specific, not global.

Falsifiability gates:

- Checkasm/unit parity: every valid/invalid length, leading zero rules, overflow, exponent sign, Eisel-Lemire exact bit parity against `str::parse` where fallback would have been used.
- Attribution: `match_number_at_digit`/`parse_number_array_direct` self-time drops at least 20% on `canada` or `marine_ik`.
- Throughput: `canada` retained or direct improves at least 5% with no string rows regressing above 2%.

### 7. Bulk emit: positions and decoded bytes

Current local surfaces:

- `bbnf-simd/src/scalar/bulk_emit_positions_64.rs`: scalar ctz loop.
- `bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`: scalar wrapper.
- Runtime retained tape push and `emit_plain_offset` are visible in SK-V6 R2b/R6.
- `parse-that-regex::unescape_json_string` decoded output materializer remains public API; byte-output rewrite was already falsified for the current direct digest workload.

Host AArch64 candidates:

- Position emit:
  - No NEON compress-store exists. Prefer improved scalar bit loop with CSSC `ctz` where available.
  - Candidate instructions: `ctz` under FEAT_CSSC, or `rbit; clz`; `str wN, [dst], #4`; `and mask, mask-1`.
  - A NEON LUT emit can be tested for dense masks, but variable-count stores likely force scratch stores and scalar copy; do not assume it beats the ctz loop at JSON structural densities.
- Byte emit:
  - For escape materialization, candidates are not new NEON instructions but output policy: stack byte buffer, `st1`/`vst1q_u8` where already decoded as bytes, `extend_from_slice`.
  - Current direct materializer byte-output route failed; do not reopen without a new output contract.
- SME/SVE compact stores are not host-practical for this workload despite SME2 presence, because normal SVE/SVE2 is unavailable and streaming-mode function overhead/state rules must be paid.

x86 candidates:

- AVX-512:
  - Position emit: `vpcompressd`; Rust `_mm512_mask_compressstoreu_epi32` under `avx512f`.
  - Byte emit: `vpcompressb` under AVX512VBMI2 where available.
  - Mask skip: `kortestq`; count: `popcnt`.
- AVX2/BMI:
  - `tzcnt` loop remains baseline.
  - `_pext_u64` can gather selected bits into low positions, but for a list of u32 positions it still needs position lookup/expansion. Gate against plain `tzcnt`, especially on AMD families where PEXT can be slow.
  - `vpshufb` LUT by 4-bit/8-bit mask is a candidate for dense masks.

Expected row impact:

- Position emit: modest on `citm_catalog`, `canada`, `random`, `marine_ik`, `numbers`, where `emit_plain_offset` is 6.7-11.0% in R2b.
- Decoded byte emit: current direct digest rows `unicode_escapes`, `unicode_mixed`, `y_string_unicode` are tempting, but REDRESS 66-69 reject this local family under the current contract.

Falsifiability gates:

- Checkasm parity over all masks for small subsets and randomized 64-bit masks for full space; alignment and destination overrun canaries.
- Microbench sparse/dense masks separately; report density crossover.
- Row gate: `emit_plain_offset` or `bulk_emit_positions_64` cost drops and at least one of `citm_catalog`, `canada`, or `random` moves by 3-5%; no retained guard row regresses by more than 2%.

## Priority order for this host

1. Retained Unicode-escape run validator scalar split first; only then NEON `vqtbl1q_u8`/range-mask x4 validator. This is the freshest SK-V6 retained primitive candidate.
2. PMULL prefix-XOR for `bitmap_prefix_xor_64`, but only if scan/string-region attribution is visible after parity gates. Low implementation risk, uncertain row impact.
3. CSSC-aware bit iteration and bulk position emit. Cheap to test with asm inspection, likely modest row-specific gains.
4. DotProd digit chunks for `match_number_at_digit` / number arrays. Useful for `canada`/`marine_ik`, not the global close.
5. New generic plain-string classification is not first priority because the always/delayed wide scanner family was already falsified. Only revisit with a narrower local fact.
6. Raw UTF-8 SIMD fusion is not a candidate for current retained generated JSON.
7. SME/SVE2 is not a host-local parser target despite SME2 sysctl support. Ordinary SVE/SVE2 is not exposed; streaming SME has ABI/state complexity and needs separate proof.

## Cross-architecture cautions

- The in-tree x86 stubs are useful as a vocabulary, but some labels need tightening:
  - `vpshufbitqmb` belongs to AVX512BITALG, not VBMI2.
  - `vpermb` belongs to AVX512VBMI.
  - `vpcompressb` belongs to AVX512VBMI2; `vpcompressd` is available in AVX512F.
  - GFNI is not a magic arbitrary byte-set classifier unless the predicate is actually encoded/derived.
- x86 AVX-512 candidates should be dispatched independently by feature: `avx512bw`, `avx512bitalg`, `avx512vbmi`, `avx512vbmi2`, `vpclmulqdq`, `avx512vnni`, `avx512ifma`, `gfni`. Do not collapse them into one "AVX-512" switch.
- On this actual Apple host, the safest path is NEON intrinsics plus scalar asm inspection. New `.S`/inline-asm should only appear where Rust intrinsics cannot express the instruction (`udot` was one such case in the current tree) or where emitted asm misses the intended CSSC/PMULL shape.

## Minimum global admission rule

For any candidate above:

1. Scalar executable spec exists and is the oracle.
2. Same-wave runtime/generated consumer exists; no orphan primitive.
3. Checkasm or equivalent parity covers alignment, tails, random data, and corpus shapes.
4. `cargo asm`/`llvm-objdump` proves the intended instruction sequence on the target feature.
5. Row gate is measured against same-HEAD baseline/candidate binaries, not only microbench throughput.
6. Existing SK-V6 REDRESS rejections stay binding: do not relabel rejected wide string scans, parser scratch, direct source hooks, byte-output unescape, sink-local decoded stats, or sidecar cursors as new SIMD work.

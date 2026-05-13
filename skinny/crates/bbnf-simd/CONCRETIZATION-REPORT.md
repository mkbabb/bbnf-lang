# bbnf-simd — SK-V3 kernel concretization

Wave 1 + Wave 6 kernel-file scaffold per `restart/skinny/audit/IMPLEMENTATION-
PACKET-SK-V3-SOTA-BEAT.md § 5`.  All files compile against the existing
checkasm harness; intrinsic bodies are stubbed (`unimplemented!()`) so the
parity tests fail fast the moment they are exercised — that is the gate.

## Inventory

### Vendored

- `ext/x86/x86inc.asm`  — 1978 LOC, ISC (BSD-2-equivalent), x264 / FFmpeg.
- `ext/x86/x86util.asm` — 1036 LOC, LGPL-2.1+, FFmpeg (include-only use).
- `ext/x86/LICENSE-VENDOR` — NEW.  Full attribution + redistribution terms
  for both files, with rationale for why include-only use is consistent
  with FFmpeg's own header-include exception.

### Build infrastructure

- `Cargo.toml` — added `build = "build.rs"`, plus `[build-dependencies]`
  `cc = "1"` and `nasm-rs = "0.3"`.  Locked 2 packages: `nasm-rs v0.3.2`
  and `log v0.4.29`.
- `build.rs` — NEW.  Assembles authored `.asm` / `.S` sources via nasm-rs,
  archives via cc.  No-op on aarch64 / wasm32 / non-x86 hosts.  No-op when
  the authored-asm set is empty (steady state for early SK-V3).  Honors
  `BBNF_SIMD_DISABLE_ASM` kill switch.

### AArch64 kernel stubs

| File | Class | Status | Body |
|---|---|---|---|
| `src/aarch64/match_tiny_plain_string.rs` | A fix | NEW | scalar reference complete, NEON stub |
| `src/aarch64/unescape_uxxxx.rs`          | B fix | NEW | scalar reference complete, NEON stub |
| `src/aarch64/{classify_tbl4,movemask,quad_load,string_block,byte_context,digit_mac,cache_hints}.rs` | pre-existing | unchanged | already implemented |
| `src/aarch64/mod.rs` | wiring | UPDATED | re-exports the two new modules |

### x86_64 kernel stubs

| File | Tier | Status |
|---|---|---|
| `src/x86_64/mod.rs`                                 | root          | UPDATED — registers 8 sub-modules |
| `src/x86_64/avx2/{classify,bmi2_emit,prefix_xor}.rs`| AVX-2 floor   | NEW (3 files) |
| `src/x86_64/avx512_vbmi2/{classify,compress,mask_fuse,carry}.rs` | VBMI-2 | NEW (4 files) |
| `src/x86_64/avx512_gfni/classify_affine.rs`         | GFNI (NEW)    | NEW per Wave 1 Agent 3 |
| `src/x86_64/avx512_kmask/arithmetic.rs`             | k-mask (NEW)  | NEW per Wave 1 Agent 3 |
| `src/x86_64/avx512_vpclmul/prefix_xor.rs`           | VPCLMUL (NEW) | NEW per Wave 1 Agent 3 |
| `src/x86_64/avx_ifma/mantissa.rs`                   | IFMA (NEW)    | NEW per Wave 1 Agent 3 |
| `src/x86_64/avx512_vnni/digit_mac.rs`               | VNNI (NEW)    | NEW per Wave 1 Agent 3 |
| `src/x86_64/avx512_bitalg/multiclass.rs`            | BITALG (NEW)  | NEW per Wave 1 Agent 3 |

### Scalar fallback

- `src/scalar/swar_8byte.rs` — pre-existing, unchanged (already implements
  the 64-byte scalar classifier).

### Tests

- `tests/checkasm_parity.rs` — extended with:
  - `sk_v3_scalar_anchors_compile` (passes) — exercises every scalar
    reference function on real-shape inputs so the parity anchors stay
    typechecked and the cross-kernel invariants (e.g. VBMI-2 scalar mask
    == GFNI scalar mask == BITALG `structural_mask`) hold.
  - `sk_v3_intrinsic_parity_aarch64` (`#[ignore]`) — gate to flip when
    Wave 1 Agent 2 lands the NEON kernel bodies.
  - `sk_v3_intrinsic_parity_x86_64` (`#[ignore]`) — gate to flip when
    Wave 6 lands the AVX-512 kernel bodies.

## Docstring quality audit

Every kernel stub carries a module-level docstring with:

1. **Lock 16 citation** — explicit reference to the SK-V3
   SOTA-BEAT-DESIGN dav1d primitive-lift row that motivates the kernel.
2. **Academic + industry citations** — at minimum two of:
   - Lemire / Langdale "Parsing gigabytes of JSON per second" (VLDB 2019)
     + the 2022 AVX-512 follow-up.
   - Mula's SIMD blog series (URL parsing, BMI2 bitmask population, hex
     decode).
   - Validark's Sneller / asmjson posts (TBL-class probes, prefix-XOR,
     AVX-512 classifier).
   - Eisel + Lemire "Number Parsing at a Gigabyte per Second" (SP&E 2021).
   - Pohrt / Reinhart GFNI tutorials (for the GFNI rows).
   - Intel ISA Reference Vol. 2 (for VPMADD52LUQ / VPDPBUSD / VPCOMPRESSB /
     VPSHUFBITQMB / VGF2P8AFFINEQB cycle counts).
3. **"Replaces in asmjson" callout** — concrete identification of which
   symbol from the SK-V2 / asmjson reference the kernel supersedes, with
   the µop delta quantified.
4. **`#[cfg(target_arch = …, target_feature = …)]` gates** — every
   intrinsic body is feature-gated; scalar references are unconditional.

## Build verification

```
$ cargo build -p bbnf-simd --profile ax-iter
Finished `ax-iter` profile [unoptimized + debuginfo] target(s) in 1.44s

$ cargo build -p bbnf-simd --release
Finished `release` profile [optimized + debuginfo] target(s) in 0.67s

$ cargo test -p bbnf-simd --profile ax-iter --test checkasm_parity \
        sk_v3_scalar_anchors_compile
test sk_v3_scalar_anchors_compile ... ok
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 6 filtered out
```

Both `ax-iter` and `release` profiles build clean.  The single dead-code
warning (`ParityReport.label`) pre-dates this work and is unrelated.

## Follow-up — what the next agent owns

1. **Wave 1 Agent 2** — Fill in the NEON bodies for
   `match_tiny_plain_string_neon` and `unescape_uxxxx_neon`.  When the
   bodies land, drop `#[ignore]` from `sk_v3_intrinsic_parity_aarch64`
   and the alignment-sweep loop in checkasm will assert parity over the
   full 0..64-byte alignment ladder.
2. **Wave 6** — Fill in the AVX-2 / AVX-512 bodies in dispatch-priority
   order: GFNI affine classify → BITALG multi-class → VBMI-2 compress →
   VPCLMUL prefix-XOR → k-mask fuse → IFMA mantissa → VNNI digit-MAC →
   AVX-2 floor.  Each kernel can be activated independently because the
   stub files already compile; the dispatch table just gains a new live
   pointer.
3. **Authored .asm** — When the first `.asm` source lands under
   `src/x86_64/**/*.asm`, `build.rs` will pick it up automatically and
   archive it as `libbbnf_simd_asm.a`.  No further build-script edits
   needed; just drop the file in.
4. **GFNI matrix derivation** — `JSON_STRUCTURAL_AFFINE_MATRIX` /
   `JSON_STRUCTURAL_AFFINE_BIAS` are placeholder zeros today; the Wave 6
   supplementary tool that derives the 8×8 GF(2) affine transform for the
   JSON structural class still needs to be written.

## Time accounting

| Phase | Wall |
|---|---|
| Inventory + read existing kernels | ~6 min |
| LICENSE-VENDOR + build.rs + Cargo.toml | ~4 min |
| Class A + Class B fix kernels        | ~6 min |
| AVX-2 floor (3 files)                | ~5 min |
| AVX-512 VBMI-2 cluster (4 files)     | ~6 min |
| GFNI / k-mask / VPCLMUL / IFMA / VNNI / BITALG (7 files) | ~8 min |
| checkasm extension + cargo verify    | ~5 min |
| Concretization report                | ~3 min |
| **Total**                            | **~43 min** — under 45-min HARD CAP |

# SK-V16 P2-C: AArch64 Host Esoterica

Pass: S-P2 Research. Cycle: V16.
Date: 2026-05-28.
Scope: Apple M5 Max / aarch64 instruction inventory grounded in S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: scanner/string, scanner/whitespace, scanner/number, structural scan, tape/view.
Lock surface: both.

## Section 1 - Findings

Live target is aarch64 only. x86 is out for implementation and appears here
only as rejected context.

Existing local aarch64 surface:

- `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:21`-`65` uses
  `vqtbl4q_u8` plus movemask to classify structural, terminator, escape, and
  control masks.
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:57`-`72` uses NEON
  comparisons and movemask for string-special blocks.
- `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4` implements the project
  movemask convention.
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:25`-`49` has a DotProd
  4-digit body using `udot`.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74`-`166` contains
  TBL-backed hex quartet decode paths.
- `skinny/crates/bbnf-simd/src/dispatch.rs:63`-`74` selects aarch64 primitive
  kernels at compile time.

Official ISA/intrinsic anchors:

- Arm ACLE Neon reference version 2026Q1, released 2026-05-15:
  https://arm-software.github.io/acle/neon_intrinsics/advsimd.html.
- `vqtbl4q_u8` maps to A64 `TBL Vd.16B,{Vn.16B - Vn+3.16B},Vm.16B`.
- `vmull_p64` maps to `PMULL`.
- `vdotq_u32` maps to `UDOT`.
- `vclzq_u8` maps to `CLZ` and supports the existing RBIT+CLZ style first-set
  extraction when CTZ is unavailable.

## Section 2 - Candidate Primitives

| Candidate | Shape | Scalar-ref status | Arch detail | P1 antecedent |
|---|---|---|---|---|
| TBL class table | 64-byte low-6 byte classifier with per-grammar table | scalar exists; checkasm exists for table classifier | `vqtbl4q_u8` / TBL, already present | structural scan, whitespace, CSS delimiters |
| String-special mask | 16-byte terminator/escape/control/non-ASCII mask | scalar exists; `checkasm_parity.rs:626`-`630` covers scalar vs NEON, with dedicated test optional if S-P3 exposes it publicly | NEON compare + movemask | string scanner leaves |
| Backslash parity mask | prefix/escape carry over a 64-bit mask | scalar exists; checkasm exists | PMULL is only a candidate replacement for scalar prefix XOR, not proof | quote/escape scanner |
| First-set extraction | next set bit from mask and cursor | scalar exists; checkasm exists | current Rust `trailing_zeros`; CSSC CTZ is host-feature proof only if disassembly confirms it | structural/tape cursor loops |
| Digit MAC | 4/8/16 digit block accumulate | 4-digit scalar and DotProd body exist; widened scalar/checkasm needed | `UDOT` / DotProd | number scan and Canada/numbers rows |
| Hex quartet decode | four hex nibbles to codepoint or reject | scalar and TBL bodies exist; `checkasm_utf8_block.rs:59`-`65` covers x4 parity, with wider malformed/surrogate coverage required for semantic promotion | `vqtbl1q_u8` / TBL plus shift/OR | Unicode escape rows |
| Prefetch/store hints | tape write/read locality hints | no parser primitive yet | PRFM/STNP only after row-local tape-write profile | tape/view Mode III |

Rejected inventory:

- PMULL prefix XOR remains blocked unless S-P3 finds a fresh non-checksum
  prefix-XOR hot leaf. The aarch64 wrapper currently delegates to scalar, and
  prior PMULL promotion was REDRESS-rejected.
- CSSC CTZ and RBIT+CLZ next-bit extraction remain support context only. The
  current next-set-bit and bulk-emit wrappers delegate to scalar, and previous
  CSSC production promotion failed row movement.
- TBX is inventory-only for this pass. No current bbnf source uses `vqtbx*`,
  and no S-P1 hot leaf requires extended table lookup.
- SVE/SVE2/SME are out of scope for Apple M5 Max evidence unless a later host
  proof and dispatch plan establish support.

## Section 3 - Grammar-Neutrality

TBL class table, string-special mask, first-set extraction, digit MAC, and hex
decode are grammar-neutral if the grammar supplies data tables and semantic
validation remains above the primitive.

PMULL prefix XOR is grammar-neutral as a bit algebra primitive, but REDRESS
88 blocks promoting it merely because the instruction exists.

Prefetch/store hints are not parser semantics. They can be a tape layout
optimization only after S-P1/S-P3 name a tape write/read hot leaf and bind a
same-wave consumer.

## Section 4 - Risks

- SIMD candidates without scalar reference plus strict checkasm remain research
  only.
- `aarch64` does not mean every optional feature is present. DotProd/CSSC must
  be gated by target feature or disassembly; no `target-cpu=native` assumption
  is sufficient for a portable runtime claim.
- `SVE`/`SVE2`/`SME` are out unless the host and dispatch layer prove support.
- Any instruction route tied only to JSON token names fails Lock 14.
- Tape/view costs are not automatically SIMD costs. Cursor API and layout
  changes may be scalar substrate work.

## Section 5 - Sources

- Arm Neon Intrinsics Reference, 2026Q1: https://arm-software.github.io/acle/neon_intrinsics/advsimd.html
- Arm CTZ instruction reference: https://developer.arm.com/documentation/ddi0602/2022-09/Base-Instructions/CTZ--Count-Trailing-Zeros-
- `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/bbnf-simd/src/dispatch.rs`
- `restart/skinny/tranches/sk-v16/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v16/research/p1/p1c-samply-mode-3.md`

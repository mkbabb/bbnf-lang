# SK-V12 W2 A1 - `escape_mask_64` Scalar Contract

Scope: read-only research lens for SPEC Section 5.

## Finding

`escape_mask_64` is a public bbnf-simd primitive at
`skinny/crates/bbnf-simd/src/lib.rs:175`. It accepts a 64-bit backslash
mask and a carry-in bit, then returns the escape mask for the current
stripe and a carry-out bit for an odd trailing backslash run.

Load-bearing source points:

- `lib.rs:175` exposes `pub fn escape_mask_64(bs_mask, bs_carry_in)`.
- `lib.rs:179` computes starts of backslash runs with
  `bs_mask & !(bs_mask << 1)`.
- `lib.rs:180-196` adjusts the run-start parity when carry-in continues
  into bit 0 and marks bytes escaped after odd-length runs.
- `lib.rs:198-204` computes `new_carry`: false unless bit 63 is a
  backslash; all-backslash stripes preserve carry-in; otherwise trailing
  run parity controls carry-out.

The active scalar reference is the primitive itself. The AVX-512 carry
facade delegates to it at `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/carry.rs:17`;
the AVX-512 body is unimplemented and out of SK-V12 scope.

## Gap

No dedicated `escape_mask_64` checkasm cell exists. Existing tests mention
the falsifier only indirectly in
`tests/checkasm_byte_class_from_eq_set_64.rs:255`, and the checked-in
`CHECKASM-REPORT.md:188-192` explicitly calls for direct carry-state
fuzzing.

## Commands Inspected

```sh
rg -n "escape_mask_64|escape_mask" skinny/crates/bbnf-simd
rg -n "bs_carry|carry_in|new_carry|scan_json_tail|escaped|escape_mask_64" skinny/crates/bbnf-simd skinny/crates/runtime
```

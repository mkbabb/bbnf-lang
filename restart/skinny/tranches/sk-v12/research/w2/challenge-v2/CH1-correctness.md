# SK-V12 W2 CH1 V2 - Correctness

Disposition: ACCEPT.

V2 fixes the V1 blocker. SPEC Section 5 now owns
`skinny/crates/runtime/src/grammars/json/scan.rs` for caller-level adversarial
parity and minimal handoff repair if falsified, and PLAN-V2 makes that runtime
proof mandatory.

Primitive parity is sufficient: the plan requires
`checkasm_escape_mask_64.rs` with an independent byte-walk scalar reference
that must not call `bbnf_simd::escape_mask_64`, plus carry-in, bit-0
continuation, bit-63, `u64::MAX`, sparse/random masks, and 1..128 slash-run
stripe splits.

The caller proof is now in the right layer. The historical failure is the
aarch64 scanner handoff from `escape_mask_64` carry into scalar-tail `escaped`
state. Runtime parity should compare positions/hash and assert the NEON
backend on aarch64 to avoid scalar-vs-scalar vacuity.

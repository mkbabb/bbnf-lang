# SK-V12 W2 CH5 - Hidden Coupling

Disposition: REVISE.

The direct `checkasm_escape_mask_64` primitive test is in bounds. The hidden
coupling is the proposed caller-level JSON scanner test inside `bbnf-simd`.

`bbnf-simd` cannot import `runtime` without a reverse dependency: its
dev-dependencies are only `test-fixtures` and `libc`, and `runtime` already
depends on `bbnf-simd`. The runtime scanner is the real consumer of
`escape_mask_64`, so caller parity must be in a runtime-owned or bench-owned
test path.

Required revision: keep primitive parity under
`skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs`; move JSON scanner
adversarial parity to an explicitly owned runtime or bench test path. Do not
hide the runtime import inside a `bbnf-simd` test.

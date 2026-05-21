# SK-V12 W2 CH2 - Generality / Lock 14

Disposition: REVISE.

The proof-only W2 shape is grammar-neutral enough when it stays at
`escape_mask_64` mask/carry parity. It does not claim a CSS row, non-JSON row,
public substrate API, BIR variant, directive, or `BackendShape` expansion.

Blocking issue: the plan places JSON scanner adversarial parity in a possible
`bbnf-simd` sibling test. `bbnf-simd` is generic and has only `test-fixtures`
and `libc` dev-dependencies, while `runtime` already depends on `bbnf-simd`.
A `bbnf-simd -> runtime` test dependency would reverse the crate boundary and
carry JSON structural/string policy into the generic SIMD crate.

Required revision: keep `bbnf-simd` tests limited to primitive-level
`escape_mask_64` parity. Move JSON scanner adversarial parity to a JSON-owned
runtime or bench/gate test path and explicitly add that path to W2 ownership.

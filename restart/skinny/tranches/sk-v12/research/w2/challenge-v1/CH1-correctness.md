# SK-V12 W2 CH1 - Correctness

Disposition: REVISE.

The direct primitive proof is acceptable: W2 should add an independent
byte-walk scalar reference for `escape_mask_64(bs_mask, carry_in)` and must not
call the primitive from its own reference.

Blocking issue: caller-level parity is not executable as written. The plan
requires comparing `runtime::grammars::json::scan::scan_structurals` with
`scan_structurals_scalar`, but SPEC Section 5 owner paths only list
`bbnf-simd`, `CHECKASM-REPORT.md`, and `REDRESS.md`. The historical bug is the
runtime NEON handoff between `escape_mask_64` carry and scalar-tail `escaped`,
so the caller-level test must be a named required gate and must live in an
owned runtime/bench surface.

Required revision: name the caller-level parity test and command explicitly,
add it to the mandatory gate surface, and expand W2 ownership to that test path
or state the wave fails closed if runtime scanner source changes are needed.

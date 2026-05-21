# SK-V12 W2 CH5 V2 - Hidden Coupling

Disposition: ACCEPT.

Primitive proof stays in `bbnf-simd`. Caller-level parity moves legally to the
JSON runtime owner, avoiding the V1 reverse dependency from `bbnf-simd` into
`runtime`.

The SPEC owner amendment is sufficient for CH5. It names `scan.rs` with the
exact allowed scope, while PLAN-V2 keeps runtime behavior edits conditional and
minimal. No parallel substrate, sidecar, row admission, new primitive, or
orphan route is introduced.

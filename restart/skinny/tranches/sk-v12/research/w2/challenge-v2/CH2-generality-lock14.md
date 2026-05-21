# SK-V12 W2 CH2 V2 - Generality / Lock 14

Disposition: ACCEPT.

PLAN-V2 resolves the V1 generality blocker. Primitive mask/carry parity stays
in `bbnf-simd`, while JSON escape-window policy moves to JSON-owned
`runtime/src/grammars/json/scan.rs` tests.

No JSON policy leak into `bbnf-simd` is introduced. No public substrate API,
directive, BIR variant, or `BackendShape` expansion is planned. W2 claims no
CSS/non-JSON row and remains a correctness prerequisite before later SIMD/ASM
admission.

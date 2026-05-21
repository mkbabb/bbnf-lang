# SK-V12 W2 A3 - NEON Handoff Root Cause

Scope: read-only aarch64 JSON scanner audit for SPEC Section 5.

## Finding

The historical bug is a contract mismatch between two state meanings, not
a byte-classifier failure.

- `escape_mask_64` returns `new_carry`, meaning the current 64-byte stripe
  ended with an odd trailing backslash run
  (`skinny/crates/bbnf-simd/src/lib.rs:198-204`).
- `scan_tail` accepts `escaped`, meaning the current scalar byte is already
  escaped and should skip special handling once
  (`skinny/crates/runtime/src/grammars/json/scan.rs:107-147`).

Those values are equivalent only when the carry is still inside the active
string and no residual bytes have changed escape state. The archived
report records the same failure at `CHECKASM-REPORT.md:102-121` for seed
`0xCAFEF00DBAADF00D`, where NEON dropped a closing quote at position 126.

## Current HEAD State

HEAD already contains the mitigation shape W2 needs to prove:

- `scan.rs:241-245` computes strictness conditions for escaped quotes and
  carry state.
- `scan.rs:246-261` falls back to `resolve_string_masks_64` when the fast
  path is not strict.
- `scan.rs:262-273` gates outgoing `bs_carry` with `in_string` before
  passing state to the scalar tail.

Local check before research close:

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_parity -- --nocapture
```

Result: PASS at HEAD, including `classifier_corpus_parity`.

## Redress Implication

W2 likely needs proof and report repair more than a source fix. If a new
dedicated falsifier test fails, the minimal source fix is to route any
stripe with `bs_carry`, `new_bs_carry`, or `quotes & escaped != 0` through
`resolve_string_masks_64`, then keep `bs_carry = next_bs_carry && in_string`.

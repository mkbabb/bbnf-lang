# SK-V12 W2 A2 - Checkasm Coverage

Scope: read-only checkasm/parity coverage audit for SPEC Section 5.

## Finding

`escape_mask_64` is not directly covered by a current
`skinny/crates/bbnf-simd/tests` test. The existing strict classifier
parity harness uses the falsifier seed, but it exercises generic
byte-class dispatch, not the JSON string-state scanner or the
`escape_mask_64(bs_mask, carry_in)` primitive.

Existing adjacent coverage:

- `tests/checkasm_parity.rs:243` seeds the alignment sweep with
  `0xCAFEF00D_BAADF00D`.
- `tests/checkasm_parity.rs:237-256` sweeps lengths and alignments for
  structural classification.
- `tests/aarch64_primitives.rs:117` and `tests/checkasm_parity.rs:617`
  cover the 16-byte string-special block, but not cross-block escape carry.
- `tests/checkasm_bitmap_prefix_xor_64.rs` covers prefix-XOR carry-in;
  this is quote-prefix parity, not backslash escape-run parity.
- `CHECKASM-REPORT.md:102-126` records the historical open divergence and
  its falsifier.

## Coverage Gaps For W2

- Exact regression for xorshift seed `0xCAFEF00DBAADF00D`, iteration 0,
  128-byte JSON-pool buffer.
- Direct `escape_mask_64` matrix for carry-in true/false, bit-0
  continuation, bit-63 run termination, all-backslash masks, and sparse
  runs.
- Long backslash runs 1..128 crossing the 64-byte stripe boundary.
- Caller-level JSON scan parity for mixed ASCII/escape windows, especially
  boundary quotes after odd/even slash runs.
- Tail-state parity for residual lengths 0..63 with inherited carry.

## Candidate Commands

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity -- --nocapture
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bitmap_prefix_xor_64 -- --nocapture
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_utf8_block -- --nocapture
cargo run -p xtask --release -- primitive-checkasm
```

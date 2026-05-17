# SK-V7 Wave 10c Plan: B6 Stack-Canary Stage 1 Only

Inputs: `restart/skinny/tranches/sk-v7/SPEC.md` section 12;
`restart/skinny/tranches/sk-v7/HANDOFF.md` section 3;
`skinny/REDRESS.md` items 88 and 89;
`restart/skinny/tranches/sk-v7/research/wave-10c-r1-b6-only-viability.md`;
`restart/skinny/tranches/sk-v7/research/wave-10c-r2-closure-protocol.md`.

Intervention: admit only B6 Stage 1 checkasm stack-canary XOR-fold hardening.
Both bitmap asm body fills remain rejected for SK-V7 by measurement: PMULL
prefix-XOR by item 88, and CTZ next-bit plus bulk consumer by item 89.

Owner paths:
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/REDRESS.md`

Non-owner paths that must stay unchanged:
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/RESULTS.md`

Plan:
1. Add `checkasm_common::with_stack_canary_xor_fold(label, f)`. It fills a
   1 KiB stack canary with deterministic `Xorshift64` bytes, computes a
   pre-call XOR fold, runs the candidate closure, computes a post-call XOR
   fold, and panics with label/fold/first-byte evidence if either the fold or
   exact byte comparison changes.
2. Keep `guarded_call` and `stack_canary_then` as compatibility forwarders to
   `with_stack_canary_xor_fold`.
3. Migrate the private silent `stack_clobber_then` wrappers in
   `checkasm_byte_class_from_eq_set_64.rs` and `checkasm_parity.rs` to the
   shared helper with file-specific labels.
4. Do not edit production aarch64 primitive bodies, runtime scan consumers,
   generated files, or `RESULTS.md`.

Falsifiability gate:
- Correctness:
  - `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64`
  - `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity`
  - `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit`
  - `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64`
  - `cargo run -p xtask --release -- primitive-checkasm`
  - `cargo test --workspace`
- Static wiring:
  - wrapper audit shows the migrated call chains reach
    `with_stack_canary_xor_fold`.
  - no private fixed-canary volatile wrapper remains in
    `crates/bbnf-simd/tests/checkasm_*.rs`.
  - production bitmap source files remain scalar delegates; no PMULL text or
    CTZ/bulk source-level consumer is introduced.
  - `skinny/RESULTS.md` has zero diff.
- Canary reach:
  - Temporarily inject `canary[0] ^= 1` inside
    `with_stack_canary_xor_fold` after the candidate closure returns.
  - Under the injection, representative guarded targets must fail with the B6
    canary assertion: `checkasm_bitmap_next_set_bit`,
    `checkasm_bulk_emit_positions_64`, `checkasm_byte_class_from_eq_set_64`,
    and `checkasm_parity` using `classifier_parity_alignment_sweep`.
  - Remove the injection and rerun `primitive-checkasm`.
- Measurement:
  - W10c is harness-only and must not refresh `RESULTS.md`.
  - The measurement evidence for rejecting bitmap bodies is REDRESS items 88
    and 89. W10c admits only if `git diff --exit-code -- skinny/RESULTS.md`
    is empty after verification.

Hard cap: W10c is a narrowed sub-cycle; commit or reject at 0.9x and halt at
cap rather than extending production-body experiments.

Revert protocol: save any failed B6-only patch to
`/tmp/skv7-wave-10c-rejected.patch`, revert source/test edits, append a
same-row REDRESS entry with the failure mode and next candidate shape, and
commit `docs(sk-v7-wave10c-redress): reject B6 stack-canary Stage 1`.

Same-wave consumer declaration: B6 Stage 1 is a test-harness consumer only. It
hardens all existing guarded checkasm candidate calls reached through
`guarded_call`, `stack_canary_then`, or the two migrated private wrappers. It
does not claim a production runtime consumer.

Pre-blocked routes: HANDOFF section 3 remains binding. W10c must not reopen
REDRESS 28+33 Class A tiny-string wiring, REDRESS 50-55 UTF-8 fusion routes,
REDRESS 60-72 retained/direct-materialization routes, function-pointer
dispatch-table churn, generic SWAR whitespace, separator elision, capacity
prescan, EventCursor parallel prepass, PMULL default prefix-XOR from item 88,
or CTZ/bulk production consumption from item 89.

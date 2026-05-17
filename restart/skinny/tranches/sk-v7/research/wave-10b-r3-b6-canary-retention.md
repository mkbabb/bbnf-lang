# SK-V7 Wave 10b R3 - B6 Canary Retention

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: research only; no source edits. Confirm whether the B6 stack-canary
fold can be retained independently of the rejected PMULL prefix-XOR body, name
the exact files/tests, and propose the W10b negative canary-reach gate.

## Conclusion

The B6 canary fold is independent of PMULL and should be retained in W10b.
PMULL lives on the production primitive path in
`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`; the B6 fold
lives in integration-test harness code under
`skinny/crates/bbnf-simd/tests/`. The rejected W10 candidate was falsified by
JSON parse-row regressions attributed in REDRESS item 88 to PMULL as the
default hot `bitmap_prefix_xor_64` body, not to checkasm canary hardening.

W10b should therefore keep prefix-XOR scalar on the production hot path unless
a separately measured PMULL consumer proves non-regression, while still landing
the B6 Stage 1 stack-canary XOR-fold and the CSSC/`ctz` next-bit consumer.

## Evidence

SPEC §12 combines three separable workstreams: PMULL for
`BITMAP_PREFIX_XOR_64`, CSSC CTZ for `BITMAP_NEXT_SET_BIT`, and B6 Stage 1
checkasm hardening. The owner paths split the production primitive bodies from
`bbnf-simd/tests/`, and the task list names B6 as "stack canary XOR-fold
compare" after the primitive/checkasm parity requirements.

Current production AArch64 bitmap files are scalar delegates:

- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:2` exposes
  `bitmap_prefix_xor_64_neon`; line 3 delegates to the scalar reference.
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:2` exposes
  `bitmap_next_set_bit_neon`; line 3 delegates to the scalar reference.
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2` exposes
  `bulk_emit_positions_64_neon`; line 3 delegates to the scalar reference.

Current B6 harness state is test-only:

- `skinny/crates/bbnf-simd/tests/checkasm_common.rs:33` defines
  `guarded_call`; line 38 forwards to `stack_canary_then`.
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs:41` defines
  `stack_canary_then`; lines 46-51 allocate a fixed 1 KiB canary, snapshot it,
  run the closure, and assert full equality. This is not yet the requested
  XOR-fold API, but it is confined to the test harness.
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:127`
  defines a private `stack_clobber_then`; lines 131-137 only volatile-touch a
  fixed canary and do not compare it. It wraps candidate calls at lines 166 and
  359.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:176` defines the same
  private silent wrapper; lines 180-189 volatile-touch the canary without a
  compare. It wraps candidate calls at lines 213 and 712.

REDRESS item 88 rejects the first W10 candidate after correctness and negative
canary checks had already passed. The recorded failure mode is PMULL on the
default hot `bitmap_prefix_xor_64` path causing hard JSON parse-row
regressions; the next candidate shape explicitly says to "retain the B6 canary
fold and CSSC/`ctz` next-bit consumer" while keeping prefix-XOR scalar unless a
narrowly gated PMULL consumer proves same-row non-regression.

## Exact Files

B6 retention files:

- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`

Expected central helper shape:

- Add `checkasm_common::with_stack_canary_xor_fold(label: &'static str, f: F)
  -> R`.
- Keep `guarded_call(f)` and `stack_canary_then(f)` as compatibility
  forwarders to the new helper.
- Replace the two private silent `stack_clobber_then` implementations with
  forwarders to the shared helper, or remove them and call the shared helper
  directly.
- Retain exact byte comparison as a collision backstop after checking the
  pre/post XOR fold.

PMULL-sensitive production file to avoid in B6-only retention:

- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`

CSSC/consumer files are W10b implementation scope, but not required for the B6
fold itself:

- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs`

## Exact Tests

The explicit primitive gate in `skinny/xtask/src/main.rs:292` runs these nine
release integration-test targets under `BBNF_SIMD_STRICT=1`:

- `checkasm_byte_class_from_eq_set_64`
- `checkasm_byte_class_from_table_64`
- `checkasm_bulk_emit_positions_64`
- `checkasm_structural_terminator_64`
- `checkasm_bitmap_prefix_xor_64`
- `checkasm_bitmap_next_set_bit`
- `checkasm_eob_pad_clamp`
- `checkasm_parity`
- `checkasm_utf8_block`

B6 Stage 1 directly reaches the currently guarded targets:

- `checkasm_byte_class_from_eq_set_64`
- `checkasm_byte_class_from_table_64`
- `checkasm_bulk_emit_positions_64`
- `checkasm_structural_terminator_64`
- `checkasm_bitmap_prefix_xor_64`
- `checkasm_bitmap_next_set_bit`
- `checkasm_eob_pad_clamp`
- `checkasm_parity`

`checkasm_utf8_block` is in the xtask gate but is not currently on the
`guarded_call` or private `stack_clobber_then` paths. W10b should either wrap
it in the same helper and include it in negative reach expectations, or keep
the claim precise: B6 Stage 1 covers all guarded checkasm candidate calls, not
every target in `primitive-checkasm`.

Positive gate:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64
cargo test -p bbnf-simd --release --test checkasm_parity
cargo run -p xtask --release -- primitive-checkasm
```

Static audit gate after the source patch:

```sh
rg -n "let mut canary = \[0xDEu8; 1024\]|let canary = \[0xDEu8; 1024\]|read_volatile\(canary" crates/bbnf-simd/tests/checkasm_*.rs
rg -n "guarded_call\(|stack_canary_then\(|stack_clobber_then\(|with_stack_canary_xor_fold" crates/bbnf-simd/tests/checkasm_*.rs
```

Expected result: no private fixed-canary implementation remains outside
`checkasm_common`, and every remaining guarded call chain reaches
`with_stack_canary_xor_fold`.

## W10b Negative Canary-Reach Gate

Use a temporary source injection only for the gate, then revert the injection
before committing:

1. In `checkasm_common::with_stack_canary_xor_fold`, inject
   `canary[0] ^= 1;` immediately after the candidate closure returns and before
   computing the post-call fold/comparison.
2. Run each guarded target directly:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_byte_class_from_table_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_structural_terminator_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bitmap_prefix_xor_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_eob_pad_clamp
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
   ```

3. Expected result: every command above fails with the B6 canary assertion and
   reports the helper label/fold mismatch or first divergent canary byte.
4. If W10b also wraps `checkasm_utf8_block`, run it under the same injection
   and require the same failure:

   ```sh
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_utf8_block
   ```

5. Revert only the temporary `canary[0] ^= 1;` injection.
6. Re-run the positive gate:

   ```sh
   cargo run -p xtask --release -- primitive-checkasm
   ```

This gate proves that the retained B6 fold is live in the migrated wrappers. It
does not depend on PMULL being present, because the injected failure happens in
the test wrapper after the candidate returns and before the harness comparison.

## W10b Recommendation

Admit B6 retention as a harness-only slice with these boundaries:

- Do not edit `src/aarch64/bitmap_prefix_xor_64.rs` for the B6 slice.
- Do not require PMULL asm proof for the B6 slice.
- Do require static wrapper audit, direct negative canary-reach failures, and
  `primitive-checkasm` passing after the temporary injection is removed.
- Record the B6 evidence separately from any CSSC/consumer or PMULL evidence so
  a future PMULL rejection cannot invalidate the canary fold.


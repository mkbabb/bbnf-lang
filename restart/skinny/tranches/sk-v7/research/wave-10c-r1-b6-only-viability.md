# SK-V7 Wave 10c R1 - B6-Only Canary Viability

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: research only. Owns this artifact only:
`restart/skinny/tranches/sk-v7/research/wave-10c-r1-b6-only-viability.md`.

## Verdict

A B6-only canary-hardening candidate is viable for W10c.

The viable shape is harness-only: migrate existing checkasm stack-clobber and
canary wrappers to one shared randomized XOR-fold helper with an exact
byte-compare backstop, then prove the helper is live with negative canary
injection. It must not change production SIMD bodies, scan consumers, generated
runtime files, benchmarks, or gate output.

W10c should leave both bitmap asm body fills rejected for this tranche:

- PMULL prefix-XOR was rejected by REDRESS item 88 after hard JSON parse-row
  regressions, despite correct checkasm and visible `pmull.1q`.
- CTZ/bulk production consumption was rejected by REDRESS item 89 after the
  refreshed `RESULTS.md` comparison showed six Track 1/2 parse rows below the
  W10b maintain invariant, despite correct checkasm, static wiring, negative
  canary reach, explicit `ctz` asm proof, and stable `simd_scan`.

Because B6-only does not touch production code, `skinny/RESULTS.md` must have
zero diff. A non-empty `RESULTS.md` diff is a W10c failure, not an admission
artifact.

## Inputs Read

- `restart/skinny/tranches/sk-v7/SPEC.md` section 12.
- `skinny/REDRESS.md` items 88 and 89.
- `restart/skinny/tranches/sk-v7/research/wave-10b-plan.md`.
- `restart/skinny/tranches/sk-v7/research/wave-10b-r1-pmull-failure-envelope.md`.
- `restart/skinny/tranches/sk-v7/research/wave-10b-r2-cssc-ctz-consumer.md`.
- `restart/skinny/tranches/sk-v7/research/wave-10b-r3-b6-canary-retention.md`.
- Current `skinny/crates/bbnf-simd` source/test wrappers.

## Owner Paths

W10c B6-only owner paths:

- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`

Positive/negative test owner paths:

- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_structural_terminator_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_eob_pad_clamp.rs`

Non-owner production paths that must stay unchanged for B6-only:

- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/RESULTS.md`

Current production state to preserve:

- `bitmap_prefix_xor_64_neon` delegates to
  `crate::scalar::bitmap_prefix_xor_64::bitmap_prefix_xor_64_scalar`.
- `bitmap_next_set_bit_neon` delegates to
  `crate::scalar::bitmap_next_set_bit::bitmap_next_set_bit_scalar`.
- `bulk_emit_positions_64_neon` delegates to
  `crate::scalar::bulk_emit_positions_64::bulk_emit_positions_64_scalar`.

## Exact Implementation Shape

1. Add one shared helper in `checkasm_common.rs`:

   ```rust
   #[inline(never)]
   pub fn with_stack_canary_xor_fold<F, R>(label: &'static str, f: F) -> R
   where
       F: FnOnce() -> R,
   {
       // Allocate 1 KiB local canary.
       // Fill with deterministic randomized bytes from Xorshift64.
       // Compute pre-call XOR fold.
       // Run candidate closure.
       // Compute post-call XOR fold.
       // Assert fold equality with label context.
       // Assert exact byte equality as collision backstop and report the first
       // divergent byte when present.
       // Return candidate result.
   }
   ```

2. Keep compatibility forwarders in `checkasm_common.rs`:

   ```rust
   pub fn guarded_call<F, R>(f: F) -> R
   where
       F: FnOnce() -> R,
   {
       with_stack_canary_xor_fold("guarded_call", f)
   }

   pub fn stack_canary_then<F, R>(f: F) -> R
   where
       F: FnOnce() -> R,
   {
       with_stack_canary_xor_fold("stack_canary_then", f)
   }
   ```

3. Replace the private silent `stack_clobber_then` helpers in
   `checkasm_byte_class_from_eq_set_64.rs` and `checkasm_parity.rs` with calls
   to `checkasm_common::with_stack_canary_xor_fold`, or forward their local
   names to the shared helper with a file-specific label.

4. Preserve current checkasm call sites and semantics. This is a detection
   hardening change only. It must not alter candidate/reference order, fixture
   content, masks, classifier logic, primitive dispatch, or benchmarked code.

5. Do not add or retain any W10/W10b production primitive body changes. No
   PMULL, no CTZ/bulk source-level consumer, no runtime scan rewiring.

## Positive Tests

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` after the B6-only patch:

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64
cargo run -p xtask --release -- primitive-checkasm
cargo test --workspace
```

The W10b evidence says these classes passed before measurement, so B6-only is
not blocked by correctness. W10c still needs fresh local proof after the exact
B6-only patch.

No `bench-json`, `gate-json`, or `RESULTS.md` refresh is required for the
B6-only candidate. If a cautious implementer runs them, the only acceptable
artifact state is no checked-in `skinny/RESULTS.md` diff.

## Negative Canary Reach

Use a temporary source injection only for the reach gate, then remove it before
any commit or final diff:

1. In `checkasm_common::with_stack_canary_xor_fold`, inject:

   ```rust
   canary[0] ^= 1;
   ```

   Place it immediately after the candidate closure returns and before the
   post-call fold and exact byte comparison.

2. Run representative migrated wrappers:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
   ```

3. Expected result: each command fails closed with the B6 canary assertion. The
   failure must identify the shared helper label or exact byte backstop.

4. For complete Stage 1 reach, also run every currently guarded integration
   target under the same injection:

   ```sh
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_byte_class_from_table_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_structural_terminator_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_bitmap_prefix_xor_64
   BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_eob_pad_clamp
   ```

5. Remove only the temporary `canary[0] ^= 1` injection.

6. Re-run:

   ```sh
   cargo run -p xtask --release -- primitive-checkasm
   ```

`checkasm_utf8_block` is part of `primitive-checkasm` but was not on the
current `guarded_call` or private `stack_clobber_then` paths in the W10b
research. W10c can either wrap it and include it in negative reach, or keep the
claim precise: B6 Stage 1 covers all guarded checkasm candidate calls.

## Static Greps

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` after the patch:

```sh
rg -n 'with_stack_canary_xor_fold|guarded_call\(|stack_canary_then\(|stack_clobber_then\(' \
  crates/bbnf-simd/tests/checkasm_*.rs
rg -n 'let mut canary = \[0xDEu8; 1024\]|let canary = \[0xDEu8; 1024\]|read_volatile\(canary' \
  crates/bbnf-simd/tests/checkasm_*.rs
rg -n 'pmull|vmull_p64' \
  crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs
rg -n 'bitmap_next_set_bit_neon' \
  crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs
rg -n 'bitmap_prefix_xor_64_scalar' \
  crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs
rg -n 'bulk_emit_positions_64_scalar' \
  crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs
git diff --exit-code -- \
  skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs \
  skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs \
  skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs \
  skinny/crates/runtime/src/grammars/json/scan.rs \
  skinny/crates/runtime/src/grammars/json/generated.rs \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  skinny/RESULTS.md
```

Expected results:

- The wrapper grep shows all guarded call chains reaching
  `with_stack_canary_xor_fold`.
- The fixed `0xDE`/volatile canary grep is empty outside any intentional
  compatibility helper in `checkasm_common.rs`; preferred W10c shape removes it
  entirely.
- The PMULL grep is empty in `bitmap_prefix_xor_64.rs`.
- The bulk consumer grep does not prove a W10c requirement. If it shows a
  source-level call from `bulk_emit_positions_64_neon` to
  `bitmap_next_set_bit_neon`, W10b production changes leaked in and W10c is no
  longer B6-only.
- The scalar delegate greps remain present for prefix-XOR and bulk emit.
- The final `git diff --exit-code` command succeeds.

## RESULTS.md Requirement

`skinny/RESULTS.md` must have zero diff for W10c B6-only.

Rationale: W10c changes only test harness hardening. It does not modify the
parser, SIMD scan implementation, runtime consumer wiring, generated output,
bench harness, fixtures, comparator plane, or gate classifier. Refreshing
`RESULTS.md` would create measurement churn unrelated to the admitted slice and
would make it harder to prove the PMULL and CTZ/bulk production changes stayed
rejected.

Admission condition:

```sh
git diff --exit-code -- skinny/RESULTS.md
```

If this command fails, reject the W10c candidate or remove the unrelated
`RESULTS.md` refresh before admission.

## Final Recommendation

Proceed with a B6-only W10c implementation if it is limited to
`bbnf-simd/tests` harness hardening and the negative reach gate proves the
shared helper is live. Do not count PMULL or CTZ/bulk as admitted W10
primitives. Do not refresh benchmark authority. The expected final source
state is: checkasm canary hardening improved, production bitmap files unchanged,
and `skinny/RESULTS.md` byte-identical.

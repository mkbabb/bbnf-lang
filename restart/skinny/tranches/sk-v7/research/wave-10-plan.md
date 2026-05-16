# SK-V7 Wave 10 Plan: Consumed AArch64 Bitmap Bodies + B6 Canary Fold

Inputs: `restart/skinny/tranches/sk-v7/SPEC.md` §12;
`restart/skinny/tranches/sk-v7/HANDOFF.md` §3 and §5;
`restart/skinny/tranches/sk-v7/research/wave-10-r1-pmull-prefix-xor.md`;
`restart/skinny/tranches/sk-v7/research/wave-10-r2-cssc-next-bit.md`;
`restart/skinny/tranches/sk-v7/research/wave-10-r3-b6-canary.md`;
`restart/skinny/tranches/sk-v7/research/wave-10-r4-consumer-bench.md`.

Intervention: replace the AArch64 scalar-delegating bitmap wrappers with
consumed scan-path bodies, then land the B6 Stage 1 stack-canary XOR-fold in
the checkasm harness.

Owner paths:
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/REDRESS.md`

Plan:
1. PMULL body: implement `bitmap_prefix_xor_64_neon` as a safe wrapper with an
   `aes`/PMULL-enabled fast path and scalar fallback. The fast path computes
   the low 64 bits of `vmull_p64(mask, u64::MAX)` and XORs `u64::MAX` when
   `carry_in` is true.
2. CSSC next-bit body: make the AArch64 next-bit wrapper own the scalar
   cursor-shift/`trailing_zeros` body instead of delegating to the scalar
   module. CTZ is verified under explicit `+cssc`; ordinary builds keep the
   portable AArch64 lowering.
3. Same-wave consumer: wire `aarch64::bulk_emit_positions_64_neon` through
   `bitmap_next_set_bit_neon`, so JSON structural scan consumes the next-bit
   primitive through `scan_structurals -> compact_mask ->
   prim::bulk_emit_positions_64 -> bulk_emit_positions_64_neon`.
4. B6 Stage 1: add `with_stack_canary_xor_fold(label, f)` in
   `checkasm_common`, keep `guarded_call`/`stack_canary_then` as compatibility
   forwarders, and migrate the private silent canary wrappers in
   `checkasm_byte_class_from_eq_set_64.rs` and `checkasm_parity.rs` to the
   shared XOR-fold helper. Retain exact byte comparison as a collision
   backstop.

Falsifiability gate:
- `cargo test -p bbnf-simd --release --test checkasm_bitmap_prefix_xor_64`
- `cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit`
- `cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64`
- `cargo run -p xtask --release -- primitive-checkasm`
- `cargo test --workspace`
- Consumer grep must show `bitmap_next_set_bit_neon` or
  `prim::bitmap_next_set_bit` in the non-test bulk-emitter scan path.
- Assembly proof:
  `CARGO_TARGET_DIR=/tmp/skv7-w10-asm RUSTFLAGS="-C target-cpu=native -C target-feature=+cssc,+aes" cargo rustc -p bbnf-simd --release --lib -- --emit=asm`
  followed by `rg -n '\bpmull\b' /tmp/skv7-w10-asm/release/deps/*.s` and
  `rg -n '\bctz\b' /tmp/skv7-w10-asm/release/deps/*.s`.
- Measurement: capture a pre-edit `simd_scan` Criterion baseline, rerun
  `cargo bench -p bbnf-bench --bench simd_scan -- --baseline skv7-w10-pre`,
  run `cargo run -p xtask --release -- bench-json --advisory`, run
  `cargo run -p xtask --release -- gate-json --advisory`, and compare
  pre/post `skinny/RESULTS.md` rows. A W10 admit requires no verdict downgrade
  and no Track 1 or Track 2 Mbps drop greater than 2%.

Hard cap: 240 min W10 cap; redress implementation/measurement must stop and
reject if the scan consumer or no-regression gate cannot be proven inside the
remaining cap.

Revert protocol: if PMULL/checkasm fails, revert the prefix body and save the
patch to `/tmp/skv7-wave-10-rejected.patch`; if next-bit remains orphaned or
regresses `simd_scan`, revert the next-bit and bulk-emitter changes; if B6
hardening breaks primitive-checkasm, revert only the harness migration. Record
the failed candidate in `skinny/REDRESS.md` with measurement evidence and the
next candidate shape.

Same-wave consumer: PMULL is consumed by JSON scan quote-mask propagation
through `bbnf_simd::prefix_xor_64`; CSSC next-bit is consumed by JSON scan
structural offset emission through `compact_mask` and the AArch64 bulk emitter.

Pre-blocked routes: HANDOFF §3 remains binding. W10 must not reopen REDRESS
28+33 Class A tiny-string wiring, REDRESS 50-55 UTF-8 fusion routes, REDRESS
60-72 retained/direct materialization routes, function-pointer dispatch table
churn, generic SWAR whitespace, separator elision, capacity prescan,
EventCursor parallel prepass, or any parser default rewrite used only to
manufacture a consumer.

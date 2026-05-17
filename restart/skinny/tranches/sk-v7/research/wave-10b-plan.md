# SK-V7 Wave 10b Plan: CTZ Bulk Consumer + B6 Canary Fold

Inputs: `restart/skinny/tranches/sk-v7/SPEC.md` section 12;
`restart/skinny/tranches/sk-v7/HANDOFF.md` section 3;
`skinny/REDRESS.md` item 88;
`restart/skinny/tranches/sk-v7/research/wave-10b-r1-pmull-failure-envelope.md`;
`restart/skinny/tranches/sk-v7/research/wave-10b-r2-cssc-ctz-consumer.md`;
`restart/skinny/tranches/sk-v7/research/wave-10b-r3-b6-canary-retention.md`.

Intervention: admit only the independently viable W10 pieces after item 88:
keep prefix-XOR scalar on the production AArch64 hot path, wire the AArch64
bulk emitter through the next-bit body so explicit `+cssc` emits `ctz`, and
land B6 Stage 1 stack-canary XOR-fold hardening in the checkasm harness.

Owner paths:
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/REDRESS.md`

Plan:
1. Keep `bitmap_prefix_xor_64_neon` as the scalar forwarder. W10 item 88
   rejected default PMULL on the hot parse path; W10b must not reintroduce
   `vmull_p64`, `pmull`, or any broad target-feature PMULL dispatch.
2. Replace the AArch64 next-bit scalar delegation with the local executable
   body: `cursor == 64` returns 64, `mask >> cursor` zero returns 64, otherwise
   return `cursor + trailing_zeros`.
3. Rewire `bulk_emit_positions_64_neon` to consume
   `bitmap_next_set_bit_neon(mask, 0)` while emitting structural offsets and
   clearing the low set bit. This is the same-wave runtime consumer:
   `scan_structurals -> compact_mask -> prim::bulk_emit_positions_64 ->
   bulk_emit_positions_64_neon -> bitmap_next_set_bit_neon`.
4. Add `with_stack_canary_xor_fold(label, f)` in `checkasm_common`, retain
   `guarded_call` and `stack_canary_then` as compatibility forwarders, and
   migrate the private byte-class and parity canary wrappers to the shared
   helper. Keep exact byte comparison as the collision backstop.

Falsifiability gate:
- Correctness:
  - `cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit`
  - `cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64`
  - `cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64`
  - `cargo test -p bbnf-simd --release --test checkasm_parity`
  - `cargo run -p xtask --release -- primitive-checkasm`
  - `cargo test --workspace`
- Static wiring:
  - `rg -n 'bitmap_next_set_bit_neon' skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
  - `rg -n 'bitmap_prefix_xor_64_scalar' skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
  - `! rg -n 'pmull|vmull_p64' skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
  - no private fixed-canary volatile wrapper remains outside `checkasm_common`.
- Canary reach:
  - Temporarily inject `canary[0] ^= 1` inside
    `with_stack_canary_xor_fold` after the candidate closure returns.
  - Under the injection, representative guarded targets must fail with the B6
    canary assertion: `checkasm_bitmap_next_set_bit`,
    `checkasm_bulk_emit_positions_64`, `checkasm_byte_class_from_eq_set_64`,
    and `checkasm_parity`.
  - Remove the injection and rerun `primitive-checkasm`.
- Assembly proof:
  - `CARGO_TARGET_DIR=/tmp/skv7-w10b-asm RUSTFLAGS="-C target-cpu=native -C target-feature=+cssc" cargo rustc -p bbnf-simd --release --lib -- --emit=asm`
  - `rg -n '\bctz\b' /tmp/skv7-w10b-asm/release/deps/*.s`
  - `! rg -n '\bpmull\b' /tmp/skv7-w10b-asm/release/deps/*.s`
  - Record whether `rustc -C target-cpu=native --print cfg` exposes
    `target_feature="cssc"`; on this host/pinned nightly it is expected to be
    absent, so explicit `+cssc` is the instruction proof.
- Measurement:
  - Capture a pre-edit `simd_scan` baseline.
  - Rerun `cargo bench -p bbnf-bench --bench simd_scan -- --baseline skv7-w10b-pre`.
  - Run `cargo run -p xtask --release -- bench-json --advisory`.
  - Run `cargo run -p xtask --release -- gate-json --advisory`.
  - Compare pre/post `skinny/RESULTS.md`; W10b admits only if no verdict
    downgrades and no Track 1 or Track 2 Mbps row drops by more than 2%.

Hard cap: W10b inherits W10's remaining cap discipline. Commit or reject at
0.9x; halt at cap. If measurement time is exhausted, reject with the completed
row evidence rather than deferring.

Revert protocol: save any failed source/test patch to
`/tmp/skv7-wave-10b-rejected.patch`, revert source/test edits, append a
same-row REDRESS entry with the failure mode and the next candidate shape, and
commit `docs(sk-v7-wave10b-redress): reject ...`.

Same-wave consumer declaration: the only admitted production primitive body is
next-bit through the AArch64 bulk emitter used by JSON structural scan. B6 is
test-harness hardening. PMULL is not admitted in W10b.

Pre-blocked routes: HANDOFF section 3 remains binding. W10b must not reopen
REDRESS 28+33 Class A tiny-string wiring, REDRESS 50-55 UTF-8 fusion routes,
REDRESS 60-72 retained/direct-materialization routes, function-pointer
dispatch-table churn, generic SWAR whitespace, separator elision, capacity
prescan, EventCursor parallel prepass, or any parser default rewrite used only
to manufacture a consumer.

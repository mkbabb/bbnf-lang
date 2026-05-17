# SK-V7 W10b R2 - CSSC CTZ next-bit consumer with scalar prefix XOR

Date: 2026-05-16.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: research-only. Source files and other docs were not edited.

## Verdict

W10b is admissible as a narrowed candidate only if it keeps
`bitmap_prefix_xor_64_neon` scalar on the production hot path and admits just
the CSSC/`ctz` next-bit path plus a `bulk_emit_positions_64_neon` consumer.
The first W10 patch was rejected because PMULL prefix XOR regressed JSON parse
rows, not because the CTZ body, bulk emitter consumer, checkasm parity, or B6
canary fold failed.

This is not yet satisfied by the current tree. Current AArch64 source still has
`bitmap_next_set_bit_neon`, `bulk_emit_positions_64_neon`, and
`bitmap_prefix_xor_64_neon` all forwarding to scalar bodies. The current scan
path is:

`json::scan::neon::scan -> compact_mask -> prim::bulk_emit_positions_64 -> aarch64::bulk_emit_positions_64_neon`

and `bulk_emit_positions_64_neon` does not call
`bitmap_next_set_bit_neon` in source today. Therefore the current tree has
instruction-level CTZ potential under CSSC, but not the same-wave source-level
next-bit consumer that W10 requires.

## Evidence

- SPEC §12 requires W10 body fills for AArch64 prefix XOR and next-bit, checkasm
  tests, same-wave runtime consumer wiring, B6 stack-canary XOR-fold hardening,
  and no row regressions (`SPEC.md:366-384`).

- REDRESS item 88 rejects the mixed W10 candidate and identifies the next
  candidate shape as W10b: retain B6 canary fold and CSSC/`ctz` next-bit
  consumer, but keep prefix XOR scalar on the production hot path
  (`REDRESS.md:2510-2540`).

- The rejected patch's correctness and structural-scan evidence is useful for
  W10b: release checkasm passed for next-bit, bulk-emit, prefix-XOR,
  byte-class, and parity; `primitive-checkasm` and `cargo test --workspace`
  passed; explicit `-C target-feature=+cssc,+aes` asm proof showed `ctz`; and
  `simd_scan` against `skv7-w10-pre` was stable or improved with the largest
  SIMD midpoint drop about `-0.52%` on `update_center/simd`
  (`REDRESS.md:2517-2526`).

- The rejected patch's falsifying rows point at PMULL prefix XOR: repeated
  `bench-json --advisory` runs regressed `instruments`, `numbers`, and
  `unicode_escapes` parse rows by roughly `-4.19%` to `-15.52%`, and REDRESS
  names PMULL as the failure mode (`REDRESS.md:2527-2537`). That failure does
  not apply to a W10b patch that leaves `bitmap_prefix_xor_64_neon` as the
  existing scalar implementation.

- Local hardware supports CSSC (`sysctl -n hw.optional.arm.FEAT_CSSC` returned
  `1`; host brand is Apple M5 Max), but the pinned nightly
  `rustc 1.96.0-nightly (2026-04-10)` still does not expose
  `target_feature="cssc"` under `-C target-cpu=native`. A strict "native emits
  CTZ" gate therefore fails today even on this host.

- Current-source assembly probe:

```text
RUSTFLAGS='-C target-cpu=native' cargo asm -p bbnf-simd --lib bitmap_next
  bitmap_next_set_bit_neon: lsr; rbit; clz; csel

RUSTFLAGS='-C target-cpu=native -C target-feature=+cssc' \
  cargo asm -p bbnf-simd --lib bitmap_next
  bitmap_next_set_bit_neon: lsr; ctz; csel

RUSTFLAGS='-C target-cpu=native -C target-feature=+cssc' \
  cargo asm -p bbnf-simd --lib bulk_emit
  bulk_emit_positions_64_neon loop: ctz; str; mask &= mask - 1
```

The `+cssc` route emits the wanted instruction but currently produces Rust's
unstable target-feature warning. Treat it as an explicit asm-proof mode, not a
portable default test.

- Baseline primitive tests run during this research pass:

```text
cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
  2 passed

cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64
  2 passed
```

## Admission Shape

The W10b source delta should be narrower than the rejected patch:

1. Leave `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs` as a
   scalar forwarder. Do not introduce PMULL on the production JSON scan path.

2. Replace the AArch64 `bitmap_next_set_bit_neon` scalar forwarder with the
   same executable logic as the scalar oracle: guard `cursor == 64`, shift
   `mask >> cursor`, return `64` for zero shifted masks, otherwise return
   `cursor + trailing_zeros`.

3. Rewire `bulk_emit_positions_64_neon` to enumerate positions via
   `bitmap_next_set_bit_neon(mask, 0)`, write `base + bit`, clear the low bit
   with `mask &= mask - 1`, and return the write count. This creates the
   same-wave consumer chain:

   `scan_structurals -> compact_mask -> prim::bulk_emit_positions_64 -> bulk_emit_positions_64_neon -> bitmap_next_set_bit_neon`

4. Keep B6 Stage 1 if it is part of the implementation slice: shared randomized
   stack canary, XOR-fold compare, exact-byte backstop, and first-bad-byte
   diagnostic. Do not claim broader B6 hardening unless raw ABI/register/cycle
   sentinel work also lands.

This shape can be admitted if and only if the gates below pass on the final
patch. The archived mixed-candidate results justify the narrowed direction, but
they are not a substitute for fresh W10b measurements.

## Required Gates

Correctness and consumer wiring:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny

cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64
cargo run -p xtask --release -- primitive-checkasm

rg -n 'bitmap_next_set_bit_neon' \
  crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs
rg -n 'prefix_xor_64_scalar|bitmap_prefix_xor_64_scalar' \
  crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs
```

Assembly proof:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny

RUSTFLAGS='-C target-cpu=native -C target-feature=+cssc' \
  cargo asm -p bbnf-simd --lib bitmap_next | rg '\bctz\b'

RUSTFLAGS='-C target-cpu=native -C target-feature=+cssc' \
  cargo asm -p bbnf-simd --lib bulk_emit | rg '\bctz\b'

RUSTFLAGS='-C target-cpu=native' \
  cargo asm -p bbnf-simd --lib bitmap_prefix | rg -v '\bpmull\b'
```

If `rustc -C target-cpu=native --print cfg` still omits
`target_feature="cssc"`, the native-only CTZ proof must be recorded as blocked
by Rust CPU feature modelling. Do not fail portable CI on this; use the explicit
`+cssc` proof as a local W10b artifact until native CSSC appears in cfg.

Benchmark and no-regression gate:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv7-w10b-r2-bench
rm -rf "$CARGO_TARGET_DIR"

cargo bench -p bbnf-bench --bench simd_scan -- --save-baseline skv7-w10b-r2-pre

# after applying the W10b source patch
cargo bench -p bbnf-bench --bench simd_scan -- --baseline skv7-w10b-r2-pre
cargo run -p xtask --release -- bench-json --advisory
cargo run -p xtask --release -- gate-json --advisory
```

Final close should use the normal wave protocol, not advisory-only output:

```sh
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Admission is blocked by any JSON row regression, any SIMD/scalar structural
parity failure, loss of `ctz` in the explicit CSSC asm proof, any `pmull` in
the production prefix-XOR proof, or missing source-level evidence that
`bulk_emit_positions_64_neon` consumes `bitmap_next_set_bit_neon`.

## Risk Notes

- Do not claim strict SPEC task 2 literally until `-C target-cpu=native` emits
  `ctz` without `-C target-feature=+cssc`. On this Apple M5 Max with the pinned
  nightly, native currently emits `rbit; clz`.

- Do not make `+cssc` a required normal test path while Rust warns that CSSC is
  unstable. Keep the normal parity tests target-agnostic and make instruction
  proof host/feature-gated.

- Do not reopen PMULL in W10b. REDRESS item 88 already falsified PMULL as a
  default hot prefix-XOR body for this wave.

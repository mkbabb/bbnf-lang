# SK-V12 W2 A4 - Corpus Parity And Guard Commands

Scope: read-only corpus/gate command audit for SPEC Section 5.

## Finding

`escape_mask_64` is live in the aarch64 JSON structural scanner:

- `skinny/crates/runtime/src/grammars/json/scan.rs:203` imports it.
- `scan.rs:236-237` computes escaped-byte masks from the backslash mask.
- `scan.rs:255-260` uses the scalar resolver fallback for non-strict cases.

The generic `bbnf-simd` corpus parity test proves classifier parity only.
The JSON scanner proof is stronger and is consumed by the bench/gate layer:

- `skinny/crates/bbnf-bench/benches/simd_scan.rs:16` asserts scalar/SIMD
  structural-scan hash parity before measurement.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:91` recomputes scalar/SIMD
  hashes.
- `gate.rs:1768` validates SIMD metadata from the same Criterion capture.

Generated JSON parity is covered by `json_parity` before measurement:

- Parse parity: `skinny/crates/bbnf-bench/src/parity.rs:23`.
- Direct digest parity: `direct_struct.rs:420`.
- Typed parity: `real_typed_struct.rs:449`.

## Mandatory W2 Exit Commands

```sh
cargo run -p xtask -- check-json
cargo run -p xtask -- check-conformance
cargo run -p xtask -- primitive-checkasm
```

Native guard capture and exact result check:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w2-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CARGO_TARGET_DIR=/tmp/skv12-w2-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
```

Targeted diagnostics:

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity -- --nocapture
cargo test -p bbnf-simd --release --test corpus_parity
```

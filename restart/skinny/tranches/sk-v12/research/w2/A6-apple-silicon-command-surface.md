# SK-V12 W2 A6 - Apple Silicon Command Surface

Scope: read-only build/test topology audit for SPEC Section 5.

## Finding

`bbnf-simd` is a member of the skinny workspace, not the repo-root
workspace. Run W2 commands from `skinny/` or with
`--manifest-path skinny/Cargo.toml`.

The crate has no Cargo feature matrix; target selection is `cfg`-based:

- `build.rs` returns early for non-`x86_64`, so Apple Silicon does not build
  x86 assembly.
- `src/aarch64/mod.rs` gates each aarch64 module with
  `#[cfg(target_arch = "aarch64")]`.
- `src/dispatch.rs:63-74` selects aarch64 primitive kernels on Apple
  Silicon before scalar fallback.

W2 owns the `bbnf-simd` correctness gate only. It moves no row and should not
admit a new SIMD optimization.

## Focused Commands

```sh
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" \
  cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --release \
  --test checkasm_parity classifier_parity_alignment_sweep -- --nocapture

BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" \
  cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --release \
  --test checkasm_parity classifier_parity_random_full_alphabet -- --nocapture

BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" \
  cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --release \
  --test checkasm_parity classifier_corpus_parity -- --nocapture
```

Aarch64 support surface:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --release --test aarch64_primitives -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --release --test checkasm_structural_terminator_64 -- --nocapture
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --release --test checkasm_utf8_block -- --nocapture
```

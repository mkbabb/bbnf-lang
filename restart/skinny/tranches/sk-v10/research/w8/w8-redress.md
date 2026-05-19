# SK-V10 W8 Redress - Hex Escape Micro-Proof

Pass: Wave Redress.
Cycle: W8.
Date: 2026-05-19.
Gate: `G-W8-ESCAPE-SEGMENT-MICROPROOF`.
Disposition: PASS.

## Patch

W8 admits the proof-only `C6-hex-escape-proof` route:

- `escape-segment-proof/Cargo.toml` provides a standalone proof harness.
- `escape-segment-proof/w8_escape_microbench.rs` compares current
  `unescape_string` against a scalar-only mirror while preserving JSON escape
  and surrogate policy outside `bbnf-simd`.
- `escape-segment-proof/W8-ESCAPE-MICROPROOF.md` records the measured artifact.

No production caller, generated parser, SIMD primitive body, benchmark row,
gate, or `RESULTS.md` row changes in W8.

## Measurement

The proof artifact measured the current aarch64 x4 Unicode escape path:

| Slice | Strings | Bytes/sample | Production ns | Scalar ns | Speedup |
|---|---:|---:|---:|---:|---:|
| `unicode_escapes` | 1251 | 32803680 | 14113893 | 37208978 | 2.636x |
| `unicode_mixed` | 0 | 0 | 0 | 0 | 0.000x |
| `y_string_unicode` | 1600 | 33554400 | 59484198 | 56084246 | 0.943x |

Aggregate speedup over eligible fixed-width Unicode escape slices:
`1.268x`, above the `1.08x` threshold.

`unicode_mixed` is zero eligible for C6 because its apparent `\u` text is
escaped-backslash data, not valid JSON Unicode escape syntax. The proof records
that as a policy guard instead of synthesizing a fake mixed escape slice.

## Evidence

Microbench:

```text
CARGO_TARGET_DIR=/tmp/skv10-w8-escape-target \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/Cargo.toml --release -- /Users/mkbabb/Programming/bbnf-lang
```

Primitive x4 parity:

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd unescape_uxxxx_x4_matches_scalar -- --nocapture
```

Strict checkasm parity:

```text
BBNF_SIMD_STRICT=1 \
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd sk_v3_intrinsic_parity_aarch64 -- --nocapture
```

Caller policy tests:

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p parse-that-regex unescape -- --nocapture
```

## Gate Accounting

- Scalar oracle and differential harness pass.
- Caller microbench clears the predeclared threshold.
- JSON slash, `\u`, invalid hex, surrogate-pair, escaped-backslash, and
  materialized-output policy remain outside `bbnf-simd`.
- `RESULTS.md` is unchanged.
- W9 may consume W8 only for `unescape_uxxxx_x4_neon` in the current
  `unescape_string` caller, with production wiring and row gates measured in
  W9.

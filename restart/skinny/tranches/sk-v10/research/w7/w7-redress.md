# SK-V10 W7 Redress - Full String Primitive Micro-Proof

Pass: Wave Redress.
Cycle: W7.
Date: 2026-05-19.
Gate: `G-W7-STRING-MICROPROOF`.
Disposition: FAIL.

## Patch

W7 selected `C5-full-string-proof` and tested the current
`match_string_at_quote_trusted_utf8` caller through `skip_string_plain_trusted`.
The proof source added during redress was saved to
`/tmp/skv10-waveW7-rejected.patch` and reverted per the W7 revert protocol. No
production caller behavior, generated parser, `RESULTS.md` row, or SIMD
primitive body remains changed.

## Measurement

The redress microbench linked against the current `parse-that-regex` crate with
`RUSTFLAGS="-C target-cpu=native"` and compared:

- production caller: `match_string_at_quote_trusted_utf8` through
  `skip_string_plain_trusted`;
- scalar mirror: the current 8-byte SWAR `string_special_mask` caller
  semantics plus equivalent JSON escape advancement;
- representative slices: `unicode_mixed`, `unicode_escapes`, and
  `unicode_basic`;
- threshold: aggregate caller speedup `>= 1.08x`;
- sample count: 25 trimmed samples per slice;
- feature gate: `target_arch=aarch64`.

Measured result:

| Slice | Offsets | Bytes/sample | Production ns | Scalar ns | Speedup |
|---|---:|---:|---:|---:|---:|
| `unicode_mixed` | 25121 | 66207330 | 61537734 | 28982062 | 0.471x |
| `unicode_escapes` | 5636 | 66664585 | 43384764 | 57057633 | 1.315x |
| `unicode_basic` | 57590 | 66601626 | 28155968 | 17008470 | 0.604x |

Aggregate speedup: `0.774x`, below the required `1.08x` threshold.

## Evidence

Microbench command:

The command was run while the rejected proof patch was applied; that patch is
now saved at `/tmp/skv10-waveW7-rejected.patch`.

```text
CARGO_TARGET_DIR=/tmp/skv10-w7-string-target \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path restart/skinny/tranches/sk-v10/research/p3/string-primitive-proof/Cargo.toml --release -- /Users/mkbabb/Programming/bbnf-lang
```

Scalar/reference parity passed:

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd string_special_block_matches_scalar_reference -- --nocapture
```

Strict checkasm parity passed:

```text
BBNF_SIMD_STRICT=1 \
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd sk_v3_intrinsic_parity_aarch64 -- --nocapture
```

## Gate Accounting

- Primitive parity is green.
- The current caller did not clear the predeclared caller-level microbench
  threshold.
- `RESULTS.md` is unchanged.
- No W9 production route may consume W7.
- W8 may still dispatch only if it selects an escape/segment primitive whose
  entry gate does not depend on an accepted W7 string proof.

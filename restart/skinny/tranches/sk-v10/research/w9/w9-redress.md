# SK-V10 W9 Redress - Existing Escape Production Rejection

Pass: Wave Redress.
Cycle: W9.
Date: 2026-05-19.
Gate: `G-W9-KERNEL-PRODUCTION`.
Disposition: REJECT.

## Patch

No production patch was applied. The accepted W8 C6 proof names
`unescape_uxxxx_x4_neon` in the current `unescape_string` caller, and that
caller already consumed the primitive before W9. A wrapper, constant, or
feature re-gate would not be a real same-commit production delta, so W9 rejects
instead of paper-closing the integration clause.

The rejected source patch marker is `/tmp/skv10-waveW9-rejected.patch`; it is
empty because redress did not attempt a source edit.

## Parity Evidence

Primitive x4 parity:

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd \
  unescape_uxxxx_x4_matches_scalar -- --nocapture
```

Result: PASS.

Strict checkasm parity:

```text
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd \
  sk_v3_intrinsic_parity_aarch64 -- --nocapture
```

Result: PASS.

Caller policy tests:

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p parse-that-regex \
  unescape -- --nocapture
```

Result: PASS.

## Measurement

Targeted direct Criterion command:

```text
CARGO_TARGET_DIR=/tmp/skv10-w9-target \
CRITERION_HOME=/tmp/skv10-w9-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench \
  --bench json_parity -- \
  'json/(unicode_escapes|y_string_unicode)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
```

The filtered capture returned 0 and wrote the relevant
`/tmp/skv10-w9-criterion/.../new/estimates.json` files. Criterion also printed
missing `sample.json` warnings for skipped non-target benches in each filtered
group; those skipped benches are not W9 evidence.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | serde direct Mbps | Floor | Outcome |
|---|---:|---:|---:|---:|---:|---|
| `unicode_escapes` | 5207 | 5234 | 14315 | 5195 | 12527 | FAIL |
| `y_string_unicode` | 5096 | 3723 | 8851 | 7555 | 8027 | FAIL |

## Gate Accounting

- Scalar fallback and differential parity are green.
- The named production caller did not consume the primitive in the same commit;
  it already consumed the primitive before W9.
- Both candidate direct rows miss Section 0.2 floors on at least one track, and
  `unicode_escapes` misses on both tracks by a wide margin.
- `RESULTS.md` is unchanged.
- W8 remains accepted proof-only evidence and cannot be reinterpreted as W9
  production admission.

# SK-V14 W11P unicode_escapes Codepoint Product Reject

Date: 2026-05-27.

Status: REJECT. No source patch lands, no `skinny/RESULTS.md` row moves, and
`restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Candidate

W11P tested a `unicode_escapes` typed-direct product whose string payload is a
decoded Unicode-scalar fact product instead of a decoded UTF-8 byte product.
The transient source added `DirectScalar::DecodedJsonCodepoints`, generated a
`parse_unicode_escapes` typed root, and made both `direct_to_struct` and
`real_typed_struct` compare meta fields, record ids, decoded scalar
fingerprints, and decoded scalar counts.

This is materially distinct from REDRESS-242 / W11M: W11M folded decoded
strings by UTF-8 bytes and retained a borrowed/raw-source product boundary;
W11P decoded JSON escapes directly into scalar facts, validated surrogate
pairs, rejected malformed escapes, and avoided decoded string materialization
in Track 1.

The transient patch was reverted after measurement and retained at
`/tmp/skv14-W11P-unicode-escapes-codepoint-product-rejected.patch` with
SHA-256 `68e11bbad6c6708fb34b8ee83566707899c6e50325477afbd831bc10b913bfb1`.

## Correctness Gates

- `rustfmt --edition 2021 skinny/crates/codegen/src/direct_schema.rs skinny/crates/codegen/src/json_typed_direct.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/xtask/src/real_typed_schema.rs`
- `CARGO_TARGET_DIR=/tmp/skv14-w11p-unicode-regen-target cargo run --profile ax-iter -p xtask -- regen-real-typed`
- `CARGO_TARGET_DIR=/tmp/skv14-w11p-unicode-check-target cargo run --profile ax-iter -p xtask -- check-real-typed`
- `CARGO_TARGET_DIR=/tmp/skv14-w11p-codegen-test-target cargo test --profile ax-iter -p codegen typed_direct -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11p-unicode-test-target cargo test --profile ax-iter -p bbnf-bench unicode_escapes -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11p-direct-test-target cargo test --profile ax-iter -p bbnf-bench direct_strict_product -- --nocapture`

All correctness gates passed before measurement.

## Cold Evidence

Release-native no-warm profile command:

```sh
CARGO_TARGET_DIR=/tmp/skv14-w11p-profile-target RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
for mode in real_typed_track1 real_typed_track2 real_typed_sonic real_typed_serde direct_strict_track1 direct_strict_track2 direct_strict_sonic direct_strict_serde; do
  /tmp/skv14-w11p-profile-target/release/profile_direct 400 unicode_escapes "$mode" 0
done
```

Retained evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11P-unicode-escapes-codepoint-product.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11P-unicode-escapes-codepoint-product.raw.log`

Measured results:

| Row | Track 1 Mbps | Same-run sonic Mbps | Margin |
|---|---:|---:|---:|
| `unicode_escapes/real_typed_struct` | 4211.977 | 6908.358 | -2696.381 |
| `unicode_escapes/direct_to_struct` | 4186.323 | 7217.462 | -3031.139 |

## Disposition

W11P proves that decoded codepoint-fact products are not sufficient to admit
the remaining `unicode_escapes` direct or typed rows. The route is pre-blocked
without a fresh material differential. It does not pre-block a different
semantic product family or a parse_only residual route, but any retry must
beat the same-run strict sonic floor with cold per-parse evidence before
authority files move.

Current state remains:

- JSON direct_to_struct: 16 / 17 ADMITTED, 1 OPEN: `unicode_escapes`.
- JSON real_typed_struct: 16 / 17 ADMITTED, 1 MISSING: `unicode_escapes`.
- JSON parse_only: 11 / 17 ADMITTED, 6 OPEN.

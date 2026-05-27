# SK-V14 W11N unicode_mixed Decoded Token Product

## Verdict

ADMIT for:

- `json/unicode_mixed/direct_to_struct/main`
- `json/unicode_mixed/real_typed_struct/main`

W11N is a product-plane route, not `parse_only`, not `JsonDigestSink`, not
`JsonDirectDigest`, and not an aggregate checksum. The material differential
over W11B is that `records[*].value` is a per-field decoded JSON string product
scalar while `records[*].type` and `metadata.classes[*]` are closed token
products. Track 1 avoids decoded `String` allocation for escaped value strings
while preserving strict product equality against Track 2, sonic, and serde.

## Evidence

Cold release-native profile:
`restart/skinny/tranches/sk-v14/research/skv14-W11N-unicode-mixed-decoded-token-product.tsv`.

Raw log:
`restart/skinny/tranches/sk-v14/research/skv14-W11N-unicode-mixed-decoded-token-product.raw.log`.

| row | Track 1 Mbps | Track 2 Mbps | sonic Mbps | serde Mbps | margin vs sonic+1 |
|---|---:|---:|---:|---:|---:|
| `unicode_mixed/real_typed_struct` | 5837.942 | 3247.472 | 5309.589 | 3356.572 | 527.353 |
| `unicode_mixed/direct_to_struct` | 5903.562 | 3275.337 | 5340.219 | 3355.287 | 562.343 |

Admission checks:

- `cargo run --profile ax-iter -p xtask -- regen-real-typed`
- `cargo run --profile ax-iter -p xtask -- check-real-typed`
- `cargo test --profile ax-iter -p bbnf-bench unicode_mixed -- --nocapture`
- `cargo test --profile ax-iter -p codegen emits_typed_direct -- --nocapture`
- `cargo test --profile ax-iter -p bbnf-bench direct_strict_product -- --nocapture`

## Ledger Effects

- `skinny/RESULTS.md` moves the two `unicode_mixed` product rows to
  `AUDIT-SUSTAINED`.
- `restart/skinny/ROLLING-SOTA-DELTA.md` marks both rows `ADMITTED` under
  `SK-V14-W11N-current`.
- JSON `direct_to_struct` state becomes 15 / 17 admitted and 2 open:
  `gsoc-2018`, `unicode_escapes`.
- JSON `real_typed_struct` state becomes 15 / 17 admitted and 2 missing:
  `gsoc-2018`, `unicode_escapes`.

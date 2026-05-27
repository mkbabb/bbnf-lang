# SK-V14 W11O gsoc-2018 Decoded Token Product

## Verdict

ADMIT for:

- `json/gsoc-2018/direct_to_struct/main`
- `json/gsoc-2018/real_typed_struct/main`

W11O is a strict product route, not `parse_only`, not `JsonDigestSink`, and
not `JsonDirectDigest`. The material differential over W9AC/W11C is that root
numeric object keys are parsed directly as `u32`, fixed Schema.org strings are
validated as closed tokens, and long proposal/sponsor/author strings are
per-field decoded JSON string fact products. Track 1 avoids the second
long-string checksum pass while preserving independent strict-product equality
against Track 2, sonic, and serde.

## Evidence

Cold release-native profile:
`restart/skinny/tranches/sk-v14/research/skv14-W11O-gsoc-decoded-token-product.tsv`.

Raw log:
`restart/skinny/tranches/sk-v14/research/skv14-W11O-gsoc-decoded-token-product.raw.log`.

| row | Track 1 Mbps | Track 2 Mbps | sonic Mbps | serde Mbps | margin vs sonic+1 |
|---|---:|---:|---:|---:|---:|
| `gsoc-2018/real_typed_struct` | 7176.742 | 6233.927 | 6627.652 | 6101.119 | 548.090 |
| `gsoc-2018/direct_to_struct` | 7228.198 | 6036.352 | 6669.742 | 6283.482 | 557.456 |

Admission checks:

- `cargo run --profile ax-iter -p xtask -- regen-real-typed`
- `cargo run --profile ax-iter -p xtask -- check-real-typed`
- `cargo test --profile ax-iter -p codegen typed_direct -- --nocapture`
- `cargo test --profile ax-iter -p bbnf-bench gsoc_2018 -- --nocapture`
- `cargo test --profile ax-iter -p bbnf-bench direct_strict_product -- --nocapture`

## Ledger Effects

- `skinny/RESULTS.md` moves the two `gsoc-2018` product rows to
  `AUDIT-SUSTAINED`.
- `restart/skinny/ROLLING-SOTA-DELTA.md` marks both rows `ADMITTED` under
  `SK-V14-W11O-current`.
- JSON `direct_to_struct` state becomes 16 / 17 admitted and 1 open:
  `unicode_escapes`.
- JSON `real_typed_struct` state becomes 16 / 17 admitted and 1 missing:
  `unicode_escapes`.

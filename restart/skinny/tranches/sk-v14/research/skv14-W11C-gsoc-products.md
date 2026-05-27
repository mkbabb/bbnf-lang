# SK-V14 W11C GSoC Product Probes

Date: 2026-05-27.

Disposition: REJECT. No source patch lands, no `RESULTS.md` row moves, and
`restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Scope

W11C tested generated strict product surfaces for `gsoc-2018` after W11A
admitted the thirteen direct rows that already had products. The transient
patch added a numeric-key generated root for the `gsoc-2018` top-level object,
ordered object-member emission for its fixed Schema.org records, and both
typed and W11A-style direct-strict consumers over the generated product.

Four material variants were measured:

1. Numeric-key root plus full product.
2. Numeric-key root plus identity product.
3. Ordered identity product.
4. Ordered full product and ordered required full product.

The transient source patch was reverted after measurement and retained only as
`/tmp/skv14-W11C-gsoc-products-rejected.patch`:

```text
sha256 258bdb69a286b0e60b57543f127be7c57ca0561a5657454d0ce5d7639a74faa9
```

## Evidence

Commands:

```bash
cargo run --profile ax-iter -p xtask -- regen-real-typed
cargo run --profile ax-iter -p xtask -- check-real-typed
cargo test --profile ax-iter -p codegen typed_direct_ -- --nocapture
cargo test --profile ax-iter -p bbnf-bench gsoc_2018_typed -- --nocapture
cargo test --profile ax-iter -p bbnf-bench direct_strict_product -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Retained cold native evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-u32.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-u32.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-identity-product.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-identity-product.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-identity.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-identity.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-full.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-full.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-required.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11C-gsoc-ordered-required.raw.log`

## Result

Best observed full-product row:

| plane | Track 1 Mbps | sonic Mbps | floor | margin | disposition |
|---|---:|---:|---:|---:|---|
| real_typed_struct | 5789.034 | 6482.407 | 6483.407 | -694.373 | REJECT |
| direct_to_struct strict product | 5834.269 | 6111.175 | 6112.175 | -277.906 | REJECT |

Best observed identity-product row:

| plane | Track 1 Mbps | sonic Mbps | floor | margin | disposition |
|---|---:|---:|---:|---:|---|
| real_typed_struct | 19909.635 | 24783.657 | 24784.657 | -4875.022 | REJECT |
| direct_to_struct strict product | 19938.076 | 24927.218 | 24928.218 | -4990.142 | REJECT |

The products were correct but not competitive. The closest route was the full
ordered product, which still missed the `sonic + 1.0` floor. The identity
product increased absolute throughput but also made the strict sonic sidecar
much faster, so it widened the admission gap.

## Close Effect

Current row state remains unchanged:

- JSON `direct_to_struct`: 13 / 17 ADMITTED, 4 OPEN.
- JSON `real_typed_struct`: 13 / 17 ADMITTED, 4 MISSING product surfaces.
- Open direct/product rows remain `gsoc-2018`, `unicode_mixed`,
  `unicode_escapes`, and `y_string_unicode`.

The next `gsoc-2018` attempt needs a fresh material differential beyond
product-surface generation, numeric-key root parsing, ordered member emission,
or required-field product shape. The residual appears to sit in string/object
substrate cost rather than missing schema shape.

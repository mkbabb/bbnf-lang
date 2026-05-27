# SK-V14 W11B Unicode Product Probe

Date: 2026-05-27.

Disposition: REJECT. No source patch lands, no `RESULTS.md` row moves, and
`restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Scope

W11B tested generated strict product surfaces for `unicode_mixed` and
`unicode_escapes` after W11A admitted the thirteen direct rows that already had
strict products. The transient patch added generated `real_typed_struct`
products, routed `direct_strict_*` over those products, and measured both
typed and direct-strict modes against same-run strict sonic/serde sidecars.

The transient source patch was reverted after measurement and retained only as
`/tmp/skv14-W11B-unicode-products-rejected.patch`:

```text
sha256 6fa6aa72ee5afd1fc701a17aa3871ed003b5ba9d3a46e2ce456167bba8b72aa5
```

## Evidence

Commands:

```bash
cargo run --profile ax-iter -p xtask -- regen-real-typed
cargo run --profile ax-iter -p xtask -- check-real-typed
cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p bbnf-bench unicode_ -- --nocapture
cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p bbnf-bench direct_strict_product -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo build --manifest-path skinny/Cargo.toml --release -p bbnf-bench --bin profile_direct
```

Retained cold native evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11B-unicode-products.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11B-unicode-products.raw.log`

## Result

| corpus | plane | Track 1 Mbps | sonic Mbps | floor | margin | disposition |
|---|---|---:|---:|---:|---:|---|
| `unicode_mixed` | real_typed_struct | 3074.922 | 5166.402 | 5167.402 | -2092.480 | REJECT |
| `unicode_mixed` | direct_to_struct strict product | 3130.925 | 5144.127 | 5145.127 | -2014.202 | REJECT |
| `unicode_escapes` | real_typed_struct | 3870.109 | 7649.956 | 7650.956 | -3780.847 | REJECT |
| `unicode_escapes` | direct_to_struct strict product | 3829.754 | 7762.353 | 7763.353 | -3933.599 | REJECT |

The products were correct but not competitive. The direct-strict route
preserves W11A's evidence discipline but exposes the same unicode string
decode/materialization cost that made the W13.6/W13.8 typed products miss.

## Close Effect

Current row state remains unchanged:

- JSON `direct_to_struct`: 13 / 17 ADMITTED, 4 OPEN.
- JSON `real_typed_struct`: 13 / 17 ADMITTED, 4 MISSING product surfaces.
- Open direct/product rows remain `gsoc-2018`, `unicode_mixed`,
  `unicode_escapes`, and `y_string_unicode`.

The next unicode attempt needs a fresh material differential, not another
product-surface-only route. Viable routes must reduce decoded string
materialization cost or change the consumer shape while preserving the strict
equality plane.

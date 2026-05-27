# SK-V14 W11K JSON y_string_unicode Fused Materializer Reject

Date: 2026-05-27.

## Disposition

W11K tested a generated `y_string_unicode` strict product route with a fused
trusted-UTF-8 string materializer. The candidate added a
`parse_that_regex::materialize_string_at_quote_after_plain_prefix_trusted_utf8`
helper that scanned and decoded escaped JSON strings in one pass, kept the
existing tiny plain-string borrowed fast path, generated a
`parse_y_string_unicode` typed root, and routed both `real_typed_struct` and
`direct_to_struct` strict products through the generated product surface.

The route is rejected. Correctness held, but cold same-binary native
`profile_direct` evidence missed strict sonic for both affected rows. The
transient source patch was reverted and retained at
`/tmp/skv14-W11K-y-string-fused-materializer-rejected.patch` with SHA-256
`f12d67fea15eaff2fbfcc212cb78b37fc8db674e79dbd769e7ad4f2365fadb4d`.

## Evidence

| row | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| `y_string_unicode/real_typed_struct` | 1797.903 | 3775.346 | 3776.346 | -1978.443 |
| `y_string_unicode/direct_to_struct` | 1727.175 | 4078.430 | 4079.430 | -2352.255 |

All rows used 1000 cold iterations with `warmup_iters=0`.

## Verification

- `cargo test --profile ax-iter -p parse-that-regex trusted_string_materializer -- --nocapture`
- `cargo test --profile ax-iter -p codegen emits_typed_direct -- --nocapture`
- `cargo run --profile ax-iter -p xtask -- regen-real-typed`
- `cargo run --profile ax-iter -p xtask -- check-real-typed`
- `cargo test --profile ax-iter -p bbnf-bench y_string_unicode_typed -- --nocapture`
- `cargo test --profile ax-iter -p bbnf-bench direct_strict_product_reuses_generated_products_without_digest_plane -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-ystr-fused-target RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct`
- `/tmp/skv14-ystr-fused-target/release/profile_direct 1000 y_string_unicode {real_typed_track1,real_typed_sonic,direct_strict_track1,direct_strict_sonic} 0`

Retained raw evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11K-y-string-fused-materializer.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-W11K-y-string-fused-materializer.tsv`

## Residual State

JSON direct_to_struct remains 13 / 17 ADMITTED and 4 OPEN:
`gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.
JSON real_typed_struct remains 13 / 17 ADMITTED and 4 MISSING product
surfaces for the same four corpora. Future `y_string_unicode` routes must add a
fresh material differential beyond generated product wiring and fused escaped
string materialization.

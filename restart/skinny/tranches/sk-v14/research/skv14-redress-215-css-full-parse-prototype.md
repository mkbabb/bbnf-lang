# SK-V14 REDRESS-215 CSS Full-Parse Re-Admit

Date: 2026-05-27.

Disposition: ADMITTED. W8R supersedes the initial REDRESS-215 W8 rejection by
moving generated CSS Track 1 output from the fact-stream plane to a generated
full-parse plane and by beating the same-run lightningcss full-parse floor in
release-native cold measurement.

## Route

The CSS generated runtime now exposes an additive `parser::parse_full` route
that calls `generated::emit_full_parse`. Existing `parser::parse` remains the
fact-stream route for prior consumers.

`emit_full_parse` validates stylesheet/block structure, comments, strings,
balanced component blocks, at-rules, qualified rules, nested rules, and
declarations, then emits:

- schema: `css-l4-full-parse-v1`
- output plane: `css_l4_full_parse`
- status row: `full_parse	status=accepted`

The W8 probe is rebound to `parse_full` and rejects any Track 1 output that
contains `_fact_stream` or `policy	backend_shape=admitted_fact_output`.

## Evidence

Commands run:

```sh
cargo run --profile ax-iter -p xtask -- regen-css
cargo run --profile ax-iter -p xtask -- check-css-l4-declaration-values
cargo run --profile ax-iter -p xtask -- check-css-l4-declaration-values-extended
cargo run --profile ax-iter -p xtask -- check-css-l4-stylesheet-selectors
cargo run --profile ax-iter -p xtask -- check-css-l4-at-rules-and-media
cargo run --profile ax-iter -p xtask -- check-css-l4-nested-layout
cargo run --profile ax-iter -p xtask -- check-css-l4-visual-functions
cargo run --profile ax-iter -p xtask -- check-css-l4-vendor-and-custom-atrules
cargo test --profile ax-iter -p codegen css_l4_generated_runtimes_reproducible_from_request -- --nocapture
cargo test --profile ax-iter -p codegen css_l4_frontend_profiles_are_request_generated -- --nocapture
cargo test --profile ax-iter -p runtime generated_css_l4 -- --nocapture
cargo test --profile ax-iter -p bbnf-bench css_l4_w8 -- --nocapture
CARGO_TARGET_DIR=/tmp/skv14-css-w8-target RUSTFLAGS="-C target-cpu=native" cargo test --release -p bbnf-bench css_l4_w8 -- --nocapture
```

All passed.

Release-native retained evidence:

```text
profile_iters=8 profiled_bytes=54859728 track1_mbps=2319.041 lightningcss_mbps=929.281 cssparser_mbps=2362.037 margin_mbps=1388.760
```

Retained files:

- `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv`

Rows under the W8R gate:

```text
selected_rows=24 admitted_rows=24 disposition=ADMITTED
track1_profile_runs=28 track1_full_parse_runs=28 track1_wrong_plane_outputs=0 track1_errors=0
```

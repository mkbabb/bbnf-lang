# bbnf Skinny Prototype

Standalone JSON skinny workspace for the restart prior-validation slice.

Useful commands:

```sh
cargo check --workspace --all-targets
cargo test --workspace
cargo run -p xtask -- regen-json
cargo run -p xtask -- check-json
cargo run -p xtask -- lint-loc
cargo run -p xtask -- bench-json
cargo run -p xtask -- gate-json
```

The workspace is intentionally nested under `skinny/` and is not a member of
the repository root Cargo workspace.

`bench-json` runs the Track 1 generated parser, Track 2 hand-coded parser,
sonic-rs, simd-json, serde_json, and the structural scan bench against
`twitter.json`, `citm_catalog.json`, and `canada.json`. The gate writes the
latest Mbps verdict to `RESULTS.md`. `REDRESS.md` records the spec deltas and
the current tape-materialization gap against sonic-rs.

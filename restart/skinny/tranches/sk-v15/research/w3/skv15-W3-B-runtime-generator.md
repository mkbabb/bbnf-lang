# SK-V15 W3-B Research: Runtime Generator Branches

Scope: `skinny/crates/codegen/src/runtime_generator.rs`.

Status: read-only research.

## Findings

`emit_from_request()` branches on `profile.mode()` at
`runtime_generator.rs:14-29`. The `PassCompiled` arm calls JSON source
compilation, while the `FrontendFacts` arm calls `emit_frontend_facts()`.
`emit_compiled()` at `runtime_generator.rs:32-79` also checks
`RuntimeGenerationMode::PassCompiled` directly before rendering JSON template
files.

`emit_frontend_facts()` at `runtime_generator.rs:81-105` then calls
`css_profile_config(profile.id())`, and `css_profile_config()` at
`runtime_generator.rs:114-153` maps seven `css_l4_*` profile IDs to
`fact_schema`, `row_id`, and `output_plane`. This is the second half of the W3
leak family: CSS row/profile facts are hardcoded in generic runtime generation.

The CSS old proof boundary starts later in the same file. `CSS_MOD_RS`,
`CSS_PARSER_RS`, `CSS_SINK_RS`, and `CSS_GENERATED_RS` begin around
`runtime_generator.rs:656`, `:666`, `:695`, and `:713`. `CSS_GENERATED_RS`
contains fact-stream and full-parse proof internals, including
`CssFullParseSummary`. W3 must not retire or delete these before W6.

## Grep Terms

```sh
rg -n "RuntimeGenerationMode|PassCompiled|FrontendFacts|css_profile_config|CssProfileConfig|CSS_GENERATED_RS|CssFullParseSummary|emit_fact_stream|emit_full_parse|css_l4_.*fact_stream" skinny/crates/codegen/src/runtime_generator.rs
```

## W3 Boundary

Safe W3 work is to move `fact_schema`, `row_id`, and `output_plane` out of a
CSS-name match and into request/profile metadata. Generated CSS files should
remain byte-equivalent if possible. Any change to `CSS_GENERATED_RS`,
`CssFullParseSummary`, `parse_full`, or fact-stream output is W5/W6 scope.

## Consumer Commands

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_frontend_profiles_are_request_generated -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_generated_runtimes_reproducible_from_request -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5c_gen_css_runtime_output_depends_on_frontend_source_hash -- --exact
```

If JSON branch logic changes, `cargo run --manifest-path skinny/Cargo.toml -p
xtask -- check-json` and `gate-json --check-results` become mandatory.

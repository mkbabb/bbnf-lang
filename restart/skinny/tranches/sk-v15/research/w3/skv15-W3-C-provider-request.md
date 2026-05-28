# SK-V15 W3-C Research: Provider Request Metadata

Scope: `skinny/crates/codegen/src/grammar_provider.rs`,
`skinny/crates/codegen/src/lib.rs`, and xtask request construction.

Status: read-only research.

## Findings

`RuntimeGenerationRequest` at `grammar_provider.rs:3-13` already carries
`grammar_name`, `profile_id`, `entry_rule`, `source_roots`, `sources`,
workspace metadata, `output_dir`, and `expected_files`. This is the right load
bearing carrier for W3 metadata.

`emit_runtime_from_request()` validates request shape and frontend closure at
`grammar_provider.rs:31-74`, but it still selects a static profile through
`select_runtime_profile_for_name()` at `grammar_provider.rs:43`. It also runs
`validate_non_json_frontend_materiality()` only when
`profile.mode() == RuntimeGenerationMode::FrontendFacts` at
`grammar_provider.rs:70-72`. That converts emitter family into behavior.

`validate_non_json_frontend_materiality()` at `grammar_provider.rs:210-249`
is semantically useful but named and triggered as a family branch. It should
become metadata-driven requirements: imports, whitespace directives/modifiers,
discard operators, pretty directives, host captures, projections, typed
projections, token directives, and comma facts.

`lib.rs:127-134` exposes `emit_runtime_profile()` and
`runtime_profile_expected_files()` as static profile helpers. Tests at
`lib.rs:283-340` and `lib.rs:501-556` hardcode the seven CSS profile IDs.

## Grep Terms

```sh
rg -n "RuntimeGenerationMode|validate_non_json_frontend_materiality|select_runtime_profile|runtime_profile_expected_files|W5C_CSS_PROFILES|grammar_name: \"css_l4\"|profile_id: \"css_l4_|grammar_name: \"json\"|profile_id: \"json\"" skinny/crates/codegen/src skinny/xtask/src
```

## W3 Boundary

The request can consume a profile contract with expected files, emitter kind,
frontend fact requirements, and optional output labels. That keeps the
generator honest without inventing CSS typed value output before W5.

W3 should add at least one regression proving a synthetic non-CSS frontend
profile can be validated from metadata without adding a `css_l4` or `json`
match arm. If that cannot land within budget, the missing non-CSS receiver must
be an intrinsic block, not papered over.

## Verification

Required after implementation:

```sh
cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_frontend_profiles_are_request_generated -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_generated_runtimes_reproducible_from_request -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen w5b_frontend_request_rejects_missing_closure_materiality -- --exact
```

# SK-V14 W5B.2 Plan: Layout And Discard Facts

Inputs:

- `restart/skinny/tranches/sk-v14/SPEC.md` §8B W5B.2 requires `@ws`, `?w`,
  `>>`, and `<<` to lower into request-local facts while public syntax remains
  retired.
- W5B.1 closed at `6777465aa`; `RuntimeFrontendClosure` is the frontend fact
  carrier.
- `skv14-W5B2-A-layout-surface.md` identifies the closure extension point.
- `skv14-W5B2-B-consumer-carry.md` identifies W5A/W5B.1 carry checks.

Intervention: extend `RuntimeFrontendClosure` with layout facts for whitespace
directives, whitespace modifiers, and discard operators. The runtime source
scanner records typed layout facts while preserving raw construct counts. The
public parser must also reject `?w`-prefixed compatibility syntax so `ident ?w`
cannot slip through as optional `ident` plus ref `w`.

Owner paths:

- Redress may edit `skinny/crates/grammar/src/lib.rs`.
- Redress may write dedicated proof logs under `/tmp/skv14-w5b-<test-name>.log`.
- Redress must not edit codegen behavior, xtask, provider/template, generated
  runtime, or rolling-delta files in W5B.2.

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_layout_contract_lowers_to_request_facts --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_public_ws_remains_retired --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_malformed_whitespace_modifier_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_discard_operators_lower_to_request_facts --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_malformed_discard_operator_fails_closed --profile ax-iter -- --exact`

Carry checks:

- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_import_graph_resolves_request_sources --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5a_css_l4_constructs_parse_as_source_facts --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_runtime_contract_consumes_source_and_metadata --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_json_request_matches_emit_from_source --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract --profile ax-iter -- --exact`

Each command tees to its matching `/tmp/skv14-w5b-<test-name>.log`, and each log
is paired with a dedicated
`rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5b-<test-name>.log`.

Same-wave consumer: `parse_runtime_source_facts()` consumes the layout scanner by
returning layout facts inside `RuntimeFrontendClosure`. W5B.4 remains
responsible for codegen generation behavior that consumes those facts.

Pre-blocked routes:

- No public parser acceptance for `@ws`.
- No public parser acceptance for `?w` as optional-plus-ref syntax.
- No provider/template change.
- No codegen consumer claim before W5B.4.
- No generated-runtime edits.

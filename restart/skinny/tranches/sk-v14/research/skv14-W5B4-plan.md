# SK-V14 W5B.4 Plan: Request Consumer Closure

Inputs:

- `restart/skinny/tranches/sk-v14/SPEC.md` §8B W5B.4 requires
  `emit_runtime_from_request` to consume the frontend closure while preserving
  JSON, Sheets/BBNF-self, `regen-css`, CSS companions, and provider/template
  topology.
- W5B.1 through W5B.3 have admitted the frontend closure facts.
- `skv14-W5B4-A-request-consumer-surface.md` identifies the codegen consumer
  gap.
- `skv14-W5B4-B-proof-carry.md` identifies the W5B close evidence.

Intervention: in `skinny/crates/codegen/src/grammar_provider.rs`, make
non-JSON runtime requests validate materiality through `facts.frontend` before
provider rendering. Keep JSON unchanged-output behavior and keep unsupported
diagnostics stable for unsupported profiles.

Owner paths:

- Redress may edit `skinny/crates/codegen/src/grammar_provider.rs`.
- Redress may edit W5A/W5B tests in `skinny/crates/codegen/src/lib.rs`.
- Redress may write dedicated proof logs under `/tmp/skv14-w5b-<test-name>.log`.
- Redress must not delete or rename provider modules, template directories, or
  generated runtime output.

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5b_frontend_request_consumes_lowered_ir_before_provider_rendering --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5b_frontend_request_rejects_missing_closure_materiality --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_runtime_contract_consumes_source_and_metadata --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_json_request_matches_emit_from_source --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_sheets_bbnf_fail_closed_through_runtime_contract --profile ax-iter -- --exact`

Carry checks:

- W5B.1 import graph exact test.
- W5B.2 layout exact tests.
- W5B.3 pretty/span/projection exact tests.
- W5B.0 Lock 14 frontend owner-path exact test.
- `cargo xtask regen-css`.
- The seven exact `check-css-l4-*` companions.
- `cargo xtask check-json`.
- Provider/template topology count remains nonzero and unchanged from W5A.

Each exact W5B test command tees to its matching
`/tmp/skv14-w5b-<test-name>.log`, and each log is paired with a dedicated
`rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5b-<test-name>.log`.

Same-wave consumer: `emit_runtime_from_request()` rejects non-JSON requests
that lack frontend-closure facts before provider rendering and continues to
emit the existing provider-backed bytes for complete requests. W5C-GEN owns
provider-free generation.

Sidecar fold: `validate_frontend_closure(&request, &facts.frontend)` runs
immediately after parsing facts to consume closure source hashes, source roots,
and import arcs. Non-JSON materiality then consumes closure vectors for layout,
discard, pretty, host capture, projection, and typed projection facts, while
`@token` and comma materiality remain raw construct checks because they do not
yet have dedicated closure vectors.

Pre-blocked routes:

- No provider/template deletion.
- No provider-free generator body.
- No grammar-name branches in generic crates.
- No generated-runtime edits.
- No public parser expansion.

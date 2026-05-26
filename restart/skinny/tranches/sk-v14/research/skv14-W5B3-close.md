# SK-V14 W5B.3 Close: Pretty Span Projection Facts

Date: 2026-05-26.
Disposition: ADMIT.

## Commits

- Research: `2e4bc8a38`
- Plan: `724af75e2`
- Redress: `42f564f4b`

## Landed Surface

W5B.3 extends `RuntimeFrontendClosure` with request-local facts for `@pretty`,
`@{...}` host captures, untyped projections, and typed projections. The scanner
preserves raw `RuntimeConstruct` counts and the existing unsupported diagnostics
for codegen consumers until W5B.4 consumes the frontend closure.

Projection parsing is delimiter-aware and preserves raw target/type text without
misclassifying `crate::` paths. Pretty directives accept the live compatibility
hints (`block`, `group`, `indent`, `sep`, `compact`, `hardbreak`, `blankline`)
and fail closed on unknown hints. Host captures fail closed when unterminated.

## Evidence

- `cargo fmt --manifest-path skinny/Cargo.toml -p grammar --check`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_pretty_span_projection_lower_to_request_facts --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_unknown_pretty_payload_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_host_capture_unterminated_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_projection_malformed_target_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_typed_projection_malformed_type_fails_closed --profile ax-iter -- --exact`
- Carry checks: W5B.1 import graph, W5B.2 layout, W5A source facts, W5A
  unsupported projection, W5A runtime request, JSON unchanged-output, and
  Sheets/BBNF-self fail-closed exact tests.
- Dedicated nonzero proof greps over the twelve matching `/tmp/skv14-w5b-*.log`
  files.

## Routed Remainder

W5B.4 owns codegen request-consumer behavior for the full frontend closure.
W5C-GEN still owns provider-free generator-body replacement. W5D-DELETE still
owns provider/template deletion.

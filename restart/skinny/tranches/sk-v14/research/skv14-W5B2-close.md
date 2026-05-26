# SK-V14 W5B.2 Close: Layout And Discard Facts

Date: 2026-05-26.
Disposition: ADMIT.

## Commits

- Research: `f8ea8a96c`
- Plan: `6610df970`
- Challenge fold: `15aa5c38a`
- Redress: `d3f92a437`

## Landed Surface

W5B.2 extends `RuntimeFrontendClosure` with request-local layout facts for
`@ws`, `?w`, `>>`, and `<<`. The scanner preserves raw `RuntimeConstruct`
counts for W5A materiality validation, records request path/source-hash/span
identity for layout facts, and fails closed on malformed whitespace modifiers
and discard operators.

Public syntax remains retired: `parse_grammar()` rejects `@ws` and rejects
`?w`-prefixed compatibility syntax before it can be interpreted as optional
syntax plus a rule reference.

## Evidence

- `cargo fmt --manifest-path skinny/Cargo.toml -p grammar --check`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_layout_contract_lowers_to_request_facts --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_public_ws_remains_retired --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_malformed_whitespace_modifier_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_discard_operators_lower_to_request_facts --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_malformed_discard_operator_fails_closed --profile ax-iter -- --exact`
- Carry checks: W5B.1 import graph, W5A source facts, W5A runtime request,
  JSON unchanged output, and Sheets/BBNF-self fail-closed exact tests.
- Dedicated nonzero proof greps over all ten `/tmp/skv14-w5b-*.log` files.

## Routed Remainder

W5B.3 owns `@pretty`, `@{...}` host capture, untyped projection, and typed
projection facts. W5B.4 owns codegen request-consumer behavior.

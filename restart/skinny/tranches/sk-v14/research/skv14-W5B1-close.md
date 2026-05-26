# SK-V14 W5B.1 Close: Request-Local Import Closure

Date: 2026-05-26.
Disposition: ADMIT.

## Commits

- Research: `6e4c4eeaf`
- Plan: `7fbda84f9`
- Challenge fold: `9ac89c071`
- Redress: `ea800aadc`

## Landed Surface

W5B.1 adds a grammar-crate frontend closure surface to
`RuntimeSourceFacts`. Runtime requests now resolve `@import` edges entirely
from the provided `RuntimeSource<'_>` slice, preserve per-source hashes, record
import specifiers and resolved target hashes, and fail closed for duplicate
source paths, missing imports, and import cycles.

Resolved imports remain countable as `RuntimeConstructKind::Import` materiality
facts but no longer surface as unsupported constructs. The only codegen change
is fixture-only: the existing W5A runtime request proof now supplies the
request-local imported source it already declared.

## Evidence

- `cargo fmt --manifest-path skinny/Cargo.toml -p grammar -p codegen --check`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_import_graph_resolves_request_sources --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_missing_import_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_import_cycle_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen w5a_runtime_contract_consumes_source_and_metadata --profile ax-iter -- --exact`
- Dedicated nonzero proof greps over `/tmp/skv14-w5b-w5b_frontend_import_graph_resolves_request_sources.log`, `/tmp/skv14-w5b-w5b_frontend_missing_import_fails_closed.log`, `/tmp/skv14-w5b-w5b_frontend_import_cycle_fails_closed.log`, and `/tmp/skv14-w5b-w5a_runtime_contract_consumes_source_and_metadata.log`
- Carry check: `cargo test --manifest-path skinny/Cargo.toml -p grammar w5a_css_l4_constructs_parse_as_source_facts --profile ax-iter -- --exact`

## Routed Remainder

W5B.2 owns layout-discard lowering for `@ws`, `?w`, `>>`, and `<<`.
W5B.3 owns pretty/span/projection lowering. W5B.4 owns request-consumer behavior
in codegen; W5B.1 does not claim generation behavior.

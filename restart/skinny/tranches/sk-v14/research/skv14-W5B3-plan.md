# SK-V14 W5B.3 Plan: Pretty Span Projection Facts

Inputs:

- `restart/skinny/tranches/sk-v14/SPEC.md` §8B W5B.3 requires `@pretty`,
  `@{...}`, `->`, and typed projections to lower into request-local facts.
- W5B.2 closed at `6d8b4cdf7`; `RuntimeFrontendClosure` is the additive fact
  carrier.
- `skv14-W5B3-A-pretty-span-projection-surface.md` identifies the closure
  extension point.
- `skv14-W5B3-B-proof-carry.md` identifies compatibility carries.

Intervention: extend `RuntimeFrontendClosure` with pretty directives, host
captures, and projection facts. Preserve raw `RuntimeConstruct` counts and leave
consumer-facing unsupported diagnostics stable until W5B.4 wires the request
consumer.

Owner paths:

- Redress may edit `skinny/crates/grammar/src/lib.rs`.
- Redress may write dedicated proof logs under `/tmp/skv14-w5b-<test-name>.log`.
- Redress must not edit codegen behavior, xtask, provider/template, generated
  runtime, or rolling-delta files in W5B.3.

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_pretty_span_projection_lower_to_request_facts --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_unknown_pretty_payload_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_host_capture_unterminated_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_projection_malformed_target_fails_closed --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p grammar w5b_frontend_typed_projection_malformed_type_fails_closed --profile ax-iter -- --exact`

Carry checks:

- W5B.1 import graph exact test.
- W5B.2 layout contract exact test.
- W5A source facts, runtime request, JSON unchanged-output, and
  Sheets/BBNF-self fail-closed exact tests.

Each command tees to its matching `/tmp/skv14-w5b-<test-name>.log`, and each log
is paired with a dedicated
`rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5b-<test-name>.log`.

Same-wave consumer: `parse_runtime_source_facts()` returns the W5B.3 facts
inside `RuntimeFrontendClosure`. W5B.4 remains responsible for codegen
generation behavior that consumes those facts.

Pre-blocked routes:

- No codegen consumer claim before W5B.4.
- No provider/template change.
- No generated-runtime edits.
- No public parser expansion for W5B.3 compatibility constructs.

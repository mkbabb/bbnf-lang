# SK-V14 W5B.2 Research B: Consumer Carry

Date: 2026-05-26.
Scope: W5B.2 downstream compatibility after W5B.1.
Output: proof carry list.

## Findings

`emit_runtime_from_request()` consumes `RuntimeSourceFacts` for materiality
counts and unsupported-construct diagnostics. W5B.2 can add frontend layout
fields without changing the request API or codegen behavior, provided existing
construct counts remain intact.

The W5A runtime contract remains the highest-value carry proof because it
exercises `@ws`, `?w`, `>>`, and `<<` through the current codegen request path.
The W5B.1 import graph proof should also remain green because W5B.2 extends the
closure rather than replacing it.

## Recommendations

- Keep W5B.2 source edits in `skinny/crates/grammar/src/lib.rs`.
- Run the exact W5B.2 grammar tests plus W5B.1 import graph and W5A runtime
  contract carry checks.
- Do not edit provider/template files, generated runtime files, xtask, or
  `grammar_provider.rs` in W5B.2.

## Risks

Removing unsupported diagnostics for `@ws`/`?w` can change invalid-profile error
ordering. That is acceptable for lowered layout facts, but W5B.2 must leave
`@pretty`, host capture, and projections for W5B.3.

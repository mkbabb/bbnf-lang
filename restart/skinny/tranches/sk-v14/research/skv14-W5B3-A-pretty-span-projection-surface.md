# SK-V14 W5B.3 Research A: Pretty Span Projection Surface

Date: 2026-05-26.
Scope: `skinny/crates/grammar/src/lib.rs` frontend closure after W5B.2 close.
Output: W5B.3 fact surface.

## Findings

`RuntimeFrontendClosure` now carries import and layout/discard facts. W5B.3 can
extend the same closure with pretty directives, host capture spans, untyped
projections, and typed projections without changing request APIs or codegen
behavior.

The scanner already counts `@pretty`, `@{...}`, `->`, and typed projections as
raw `RuntimeConstructKind`s. W5B.3 should preserve those counts while adding
typed request facts for W5B.4 to consume.

## Recommendations

- Add pretty, host-capture, and projection fact vectors under
  `RuntimeFrontendClosure`.
- Preserve request path, source hash, byte offset/end, and raw payload text.
- Keep unsupported diagnostics for these constructs until W5B.4 consumes the
  facts through codegen; W5B.3 is fact-lowering only.
- Fail closed on unknown pretty payloads, unterminated host captures, empty
  projection targets, and malformed typed projection suffixes.

## Risks

Projection parsing can overfit to the current sample if it ignores delimiters or
typed suffixes. The scanner should preserve raw target/type text and stop at
request-source delimiters without requiring codegen knowledge.

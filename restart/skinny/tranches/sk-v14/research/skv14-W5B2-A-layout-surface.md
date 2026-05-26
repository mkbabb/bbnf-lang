# SK-V14 W5B.2 Research A: Layout Surface

Date: 2026-05-26.
Scope: `skinny/crates/grammar/src/lib.rs` frontend facts after W5B.1 close.
Output: W5B.2 layout fact surface.

## Findings

W5B.1 established `RuntimeFrontendClosure` as the request-local fact carrier.
W5B.2 should extend that closure rather than adding a parallel API. `@ws`,
`?w`, `>>`, and `<<` are already scanned as runtime constructs, but only as raw
construct counts; they are not yet represented as typed frontend facts.

The existing skinny parser still rejects directives outside `@import` and
`@token`, so public `@ws` syntax remains retired on the parse path. W5B.2 only
needs to prove that request-source scanning can lower CSS compatibility layout
constructs into facts without making `parse_grammar()` accept them.

## Recommendations

- Add `RuntimeFrontendLayout` to `RuntimeFrontendClosure`.
- Record `whitespace_directives`, `whitespace_modifiers`, and
  `discard_operators` with request path, source hash, and byte offset.
- Preserve `RuntimeConstructKind` counts for W5A materiality validation.
- Treat lowered `@ws` and `?w` as no longer unsupported after successful
  frontend fact extraction.

## Risks

Malformed `?w` and discard operators are currently easy to count accidentally.
W5B.2 needs explicit fail-closed checks for `?w` without a word boundary and
for triple discard operators such as `>>>` / `<<<`.

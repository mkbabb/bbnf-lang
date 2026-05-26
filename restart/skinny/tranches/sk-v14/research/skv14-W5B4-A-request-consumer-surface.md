# SK-V14 W5B.4 Research A: Request Consumer Surface

Date: 2026-05-26.
Scope: `skinny/crates/codegen/src/grammar_provider.rs` after W5B.3 close.
Output: W5B.4 consumer surface.

## Findings

`emit_runtime_from_request()` already constructs `RuntimeSourceFacts` from the
request-local `RuntimeSource<'_>` slice before profile rendering. The remaining
W5B.4 gap is that non-JSON materiality is still mostly checked through raw
`RuntimeConstructKind` counts. That proves the scanner saw compatibility tokens,
but it does not prove codegen consumed the W5B frontend closure.

W5B.4 can close that gap without replacing provider-backed rendering. The
provider-free generator body is W5C-GEN, so W5B.4 should add a dedicated
frontend-closure materiality gate immediately before `render_runtime_profile()`
for non-JSON profiles.

## Recommendations

- Keep `emit_runtime_from_request()` as the single request entrypoint.
- Validate source metadata and expected file roster unchanged.
- For non-JSON profiles, validate `facts.frontend` directly:
  sources, imports, `layout.whitespace_directives`, `layout.whitespace_modifiers`,
  `layout.discard_operators`, `pretty_directives`, `host_captures`, untyped
  projections, and typed projections.
- Preserve raw-count checks only for facts that do not yet have a frontend
  closure vector (`@token` and comma materiality).
- Keep JSON on the existing unchanged-output route.

## Risks

Changing unsupported diagnostics for unsupported profiles would regress the
Sheets/BBNF-self fail-closed witness. W5B.4 should not remove
`facts.first_unsupported()` from the profile-lookup failure path.

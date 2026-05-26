# SK-V14 W5B.1 B: Request Source Map

Date: 2026-05-26.
Scope: W5B.1 request source map and codegen boundary.
Output: this file.

## §1 — Findings

`skinny/crates/codegen/src/grammar_provider.rs` maps
`RuntimeGenerationRequest.sources` into `grammar::RuntimeSource` values before
calling `parse_runtime_source_facts`. `validate_request_shape` only checks that
each source root is present in the request map; it does not check import
reachability.

W5B.1 can land import closure in the grammar crate without moving codegen to
consume it as a generation decision yet. W5B.4 owns the
`emit_runtime_from_request` consumer gate. This keeps W5B.1 focused and prevents
a hidden W5B.4/W5C coupling.

## §2 — Recommendations

The grammar API should accept the same `RuntimeSource<'_>` slice that codegen
already builds. The closure must not read the filesystem, committed generated
output, or global manifests. All resolution is request-local.

The missing-import and cycle tests should prove fail-closed behavior using only
in-memory request sources.

## §3 — Risks

Changing `grammar_provider.rs` in W5B.1 would expand the owner path beyond the
plan and risk conflating import closure with W5B.4 request-consumer work.

Weakening missing-import rejection to preserve old inline helpers would reopen
the provider/template smuggling route W5B.0 just closed.

## §4 — Sources

- `skinny/crates/codegen/src/grammar_provider.rs`
- `skinny/crates/grammar/src/lib.rs`
- `restart/skinny/tranches/sk-v14/SPEC.md` §8B W5B.1 and W5B.4.

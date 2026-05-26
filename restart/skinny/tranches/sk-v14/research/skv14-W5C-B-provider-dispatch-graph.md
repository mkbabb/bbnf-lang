# SK-V14 W5C-B: Provider Dispatch Graph

Date: 2026-05-26.
Scope: live runtime-generation dependency on providers and templates.
Output: this file.

## §1 — Findings

The live `regen-css` path is:

```text
xtask regen_css::TARGETS
  -> regen::runtime_request
  -> codegen::runtime_profile_expected_files
  -> codegen::emit_runtime_from_request
  -> grammar_provider frontend validation
  -> render_runtime_profile(profile, None)
  -> RuntimeProvider / css_l4_*_provider
  -> css_l4_*_templates
```

JSON runtime requests already enter through the W5A request boundary, but JSON
still reaches static `json_provider` code through `emit_from_source`.

## §2 — Recommendations

Cut the production route at `emit_runtime_from_request`: after W5B frontend
closure validation, non-compiled profiles must call one neutral generator body
that consumes the request, workspace metadata, and frontend facts. Provider and
template files remain on disk only as W5D residue.

Keep `runtime_profile_expected_files` as a profile roster lookup, but remove the
provider enum and provider-backed profile statics from the live production path.
`emit_runtime_profile` should fail closed for request-only profiles.

## §3 — Risks

Static centralization of provider output would not satisfy W5C. The replacement
must carry source-derived evidence, such as frontend source hash and request
source identities, into generated output and prove that profile-identical
requests with different frontend facts produce different bytes.

## §4 — Sources

- `skinny/xtask/src/regen.rs`.
- `skinny/xtask/src/regen_css.rs`.
- `skinny/crates/codegen/src/grammar_provider.rs`.
- `skinny/crates/codegen/src/lib.rs`.
- `skinny/crates/codegen/src/grammar_profile.rs`.

# SK-V14 W5B-GEN-A: Codegen Reachability

Date: 2026-05-26.
Scope: Read-only codegen call-graph audit for W5B-GEN provider-free runtime generation.
Output: this file.

## §1 — Findings

`cargo xtask regen-css` already reaches the W5A request boundary:
`skinny/xtask/src/main.rs:22` dispatches `regen-css`,
`skinny/xtask/src/regen_css.rs:99` calls `regen::write_targets`,
`skinny/xtask/src/regen.rs:17` builds each request, and
`skinny/xtask/src/regen.rs:22` calls `codegen::emit_runtime_from_request`.

The request path is still provider-backed below codegen. `emit_runtime_from_request`
selects a static profile at `skinny/crates/codegen/src/grammar_provider.rs:42`,
routes JSON to `emit_from_source` at `grammar_provider.rs:69`, and routes
non-JSON to `render_runtime_profile` at `grammar_provider.rs:77`.

The provider mesh is live in production codegen. `skinny/crates/codegen/src/lib.rs:1`
through `lib.rs:11` declares the CSS provider modules, `grammar_profile`,
and `json_provider`; `lib.rs:180` defines `render_runtime_profile`; `lib.rs:184`
matches `RuntimeProvider`; and `lib.rs:233` through `lib.rs:244` still builds JSON
runtime files from `json_provider`.

`skinny/crates/codegen/src/grammar_profile.rs:16` defines `RuntimeProvider`, and
`grammar_profile.rs:100` lists the provider-backed profile roster. Each CSS
provider is a thin `include_str!` copier, for example
`skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:20`.

## §2 — Recommendations

W5B-GEN cannot admit until the production path from
`emit_runtime_from_request` to generated runtime bytes no longer mentions
`render_runtime_profile`, `RuntimeProvider`, `GrammarProfile`, `json_provider`,
or `css_l4_*_provider` in `lib.rs` or `grammar_provider.rs`.

The falsifiability gate is the amended SPEC grep:

```sh
cd skinny && ! rg -n '\b(render_runtime_profile|RuntimeProvider|GrammarProfile|json_provider|css_l4_.*provider)\b' crates/codegen/src/{lib.rs,grammar_provider.rs}
```

## §3 — Risks

The obvious shortcut is static centralization: moving the current template
strings into a new neutral-looking module. That would preserve the same
provider dependency under a new name and re-open the W5B-GEN pre-blocked route
at `restart/skinny/tranches/sk-v14/SPEC.md:752`.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:706` through `SPEC.md:756`
- `skinny/xtask/src/main.rs:22`
- `skinny/xtask/src/regen_css.rs:99`
- `skinny/xtask/src/regen.rs:17`
- `skinny/xtask/src/regen.rs:22`
- `skinny/crates/codegen/src/grammar_provider.rs:42`
- `skinny/crates/codegen/src/grammar_provider.rs:77`
- `skinny/crates/codegen/src/lib.rs:1`
- `skinny/crates/codegen/src/lib.rs:180`
- `skinny/crates/codegen/src/lib.rs:233`
- `skinny/crates/codegen/src/grammar_profile.rs:16`
- `skinny/crates/codegen/src/grammar_profile.rs:100`

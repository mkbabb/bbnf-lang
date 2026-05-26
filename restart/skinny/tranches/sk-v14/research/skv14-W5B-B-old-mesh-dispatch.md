# SK-V14 W5B-B: Old Provider Mesh Dispatch

Date: 2026-05-26.
Scope: W5B research agent B, codegen dispatch surfaces.
Output: `restart/skinny/tranches/sk-v14/research/skv14-W5B-B-old-mesh-dispatch.md`.
HEAD: `286233fa2`.

## Findings

The old provider mesh is still wired in three places.

First, `skinny/crates/codegen/src/lib.rs` imports all seven CSS providers plus
`json_provider` and exposes `emit_runtime_profile(grammar_name)`. Its
`render_runtime_profile` body matches `RuntimeProvider::{CssL4..., Json}` and
calls the provider modules or `json_provider::*` helpers. This is the dispatch
surface W5B must retire before provider deletion can compile.

Second, `skinny/crates/codegen/src/grammar_profile.rs` imports the providers,
stores a `RuntimeProvider` enum in each `GrammarProfile`, and builds the
hard-coded eight-profile registry by calling each provider's `runtime_profile`.
That registry is not a declarative grammar-source/workspace-metadata surface.

Third, `skinny/crates/codegen/src/grammar_provider.rs` is the W5A request
boundary, but it still depends on the old mesh:

- `use crate::{grammar_profile, render_runtime_profile, ...};`
- `if profile.provider() == grammar_profile::RuntimeProvider::Json { ... }`
- `render_runtime_profile(profile, None)`

`regen.rs`, `regen_css.rs`, and `xtask/src/main.rs` already construct
`RuntimeGenerationRequest` values and can remain W5A consumers. The hidden
dependency is inside codegen, not at the xtask call boundary.

## Required Retirements

W5B cannot stop at file deletion. It must remove:

- `mod css_l4_*_provider;` and `mod json_provider;` imports.
- `emit_runtime_profile(grammar_name)` or its provider-backed semantics.
- `render_runtime_profile` as a provider-dispatch function.
- `RuntimeProvider` and the provider-backed `runtime_profiles()` roster.
- The JSON special case in `grammar_provider.rs`.

If those surfaces remain, provider count may be zero by path but Lock 14 remains
open by behavior.

## Lock 14 Scan Risk

The literal SPEC grep is weak: it can miss `RuntimeProvider::CssL4...` arms and
method-call matches such as `match profile.provider() { ... }`. W5B should add
a stronger supplemental gate:

```sh
cd skinny && ! rg -nU 'match\s+[^{]+\{[^}]*\b(Json|CssL4\w*|Bbnf\w*|GoogleSheets\w*)\b\s*=>' crates/{codegen,runtime,passes,bbnf,grammar}/src
cd skinny && ! rg -n '\b(RuntimeProvider|GrammarProfile|CssL4\w*|GoogleSheets\w*|Bbnf\w*)\b' crates/codegen/src
```

The second command will be noisy until W5B genuinely removes the provider mesh.
Tests may need narrower patterns, but production code must not keep the old
hard-coded provider roster.

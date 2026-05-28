# SK-V15 W3-A Research: Runtime Profile Roster

Scope: `skinny/crates/codegen/src/grammar_profile.rs`.

Status: read-only research.

## Findings

The active W3 leak family is the generic codegen runtime profile roster.
`GrammarProfile` carries both file roster and runtime mode at
`grammar_profile.rs:4-9`, and `RuntimeGenerationMode::{PassCompiled,
FrontendFacts}` at `grammar_profile.rs:11-15` is a grammar-family split.

`runtime_profiles()` at `grammar_profile.rs:89-100` hardcodes one JSON profile
plus seven CSS L4 profile rows. The CSS rows at `grammar_profile.rs:117-199`
carry concrete `css_l4_*` names and all select `FrontendFacts`. This is the
static profile roster W3 is allowed to neutralize under
`DEP-W3-W6-CSS-PROVIDER-TEMPLATE`.

Selection is exact-name based. `select_runtime_profile()` forwards
`backend.grammar_name`, and `select_runtime_profile_for_name()` loops over the
static table at `grammar_profile.rs:47-69`. That is generic-codegen admission
logic bound to grammar/profile names.

## Grep Terms

```sh
rg -n "RuntimeGenerationMode|FrontendFacts|PassCompiled|runtime_profiles|JSON_PROFILE|CSS_L4_|css_l4_|select_runtime_profile_for_name" skinny/crates/codegen/src
```

## W3 Boundary

W3 may replace the mode and profile/config roster with metadata carried by the
request/target path. W3 may not delete CSS generated parser proof, generated CSS
runtime bodies, or CSS provider templates before W6 typed proof.

Preferred intervention: make profile file rosters, emitter kind, and frontend
requirements data-driven profile metadata consumed by `RuntimeGenerationRequest`
and xtask targets, while keeping byte-equivalent generated output.

## Same-Wave Consumers

Minimum consumers:

```sh
cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_frontend_profiles_are_request_generated -- --exact
cargo test --manifest-path skinny/Cargo.toml -p codegen css_l4_generated_runtimes_reproducible_from_request -- --exact
```

CSS `check-css-l4-*` commands are the desired generated-output proof, but the
current dirty generated CSS files make them pre-existing blockers unless the W3
plan owns or isolates those generated diffs.

# SK-V14 W5-A: Provider Dispatch Inventory

Date: 2026-05-26.
Wave: W5.
Phase: research.
Agent: Newton.
Scope: read-only inspection of codegen provider dispatch.

## Question

Inventory the current runtime-provider dispatch surface and identify the
minimal honest replacement API for W5.

## Finding

The current W5 target is centered in `skinny/crates/codegen`, not
`skinny/crates/passes`. SPEC §8 says to replace a `RuntimeProvider` enum in
`passes/src/lib.rs`, but current HEAD has no such enum there. `passes` carries
grammar identity generically through `GrammarIr` and `BackendIr`; the provider
enum and eight-profile roster live in `skinny/crates/codegen/src/grammar_profile.rs`.

Current call graph:

```text
xtask regen-css/check-css
  -> regen::{write_targets, check_target}
  -> codegen::emit_runtime_profile(profile)
  -> grammar_profile::select_runtime_profile_for_name
  -> render_runtime_profile
  -> match RuntimeProvider::{7 CSS, Json}
  -> provider module / json_provider + json_sink_direct
  -> EmittedSource::{write_to_dir, check_dir}
```

Source-generation call graph:

```text
codegen::emit_from_source("json", source)
  -> grammar::parse_grammar
  -> passes::compile
  -> emit_with_layout
  -> grammar_profile::select_runtime_profile(BackendIr.grammar_name)
  -> lower::lower_to_rust
  -> JSON provider path
```

## Surfaces

- `skinny/crates/codegen/src/lib.rs` imports seven CSS provider modules at
  crate root and matches over `RuntimeProvider`.
- `skinny/crates/codegen/src/grammar_profile.rs` imports all eight provider
  modules, defines `RuntimeProvider`, and returns an eight-profile static
  roster.
- `skinny/crates/codegen/src/json_provider.rs` is distinct from the CSS
  providers: JSON mixes static template files, inline normalized source, and
  dynamic `json_sink_direct::render(sink_only)` output.
- The seven CSS provider modules are thin wrappers around per-target template
  directories.

## Replacement API Shape

An honest W5 replacement should keep provider dispatch inside `codegen` and
model runtime emission as data plus source input:

```rust
pub struct GrammarSourceFile<'a> {
    pub path: &'a str,
    pub source: &'a str,
}

pub struct RuntimeTargetMetadata<'a> {
    pub grammar_id: &'a str,
    pub target_id: &'a str,
    pub output_dir: &'a str,
    pub generated_files: &'a [&'a str],
    pub contract: RuntimeContract<'a>,
}

pub enum RuntimeContract<'a> {
    Parser,
    FactStream {
        schema: &'a str,
        row_id: &'a str,
        output_plane: &'a str,
    },
}

pub struct RuntimeEmitRequest<'a> {
    pub target: RuntimeTargetMetadata<'a>,
    pub sources: &'a [GrammarSourceFile<'a>],
    pub workspace_metadata_digest: &'a str,
}
```

The current system cannot fill this contract for CSS L4 yet because the CSS
grammar sources use syntax the skinny grammar parser does not accept, including
`->` value projections and `@{...}` span capture.

## Risk

Moving the seven CSS template bodies into `grammar_provider.rs` would remove
the forbidden file names but would not make the generator consume grammar
source and workspace metadata. That is a cosmetic file-count pass, not the
Lock 14 W5 replacement.

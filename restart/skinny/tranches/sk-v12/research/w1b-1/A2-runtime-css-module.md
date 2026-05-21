# SK-V12 W1b-1 A2 - Runtime CSS Module

Scope: read-only inspection of the post-W1a JSON runtime layout,
`GrammarConfig`/profile surfaces, runtime exports, and requirements for
`runtime/src/grammars/css_l4_declaration_values`. No source edits.

## Findings

Current runtime export shape is JSON-only. `runtime/src/lib.rs` exposes JSON
via `#[path = "grammars/json/mod.rs"] pub mod generated_json;` and re-exports
it as `runtime::grammars::json`. There is no `grammars/mod.rs`, so a CSS
runtime module is not importable unless `runtime/src/lib.rs` adds an equivalent
path alias/re-export.

Current codegen is still JSON-provider-bound: `grammar_profile` supports only
one profile, and `emit_with_layout` selects a profile but then unconditionally
calls `json_provider::*` and `json_sink_direct::render`. Adding a CSS profile
alone will still emit JSON-shaped files unless provider/rendering dispatch
becomes profile-driven.

## Minimal Runtime Module Shape

Smallest W1b-1 CSS runtime directory:

- `mod.rs` generated: exports the CSS direct entrypoint and fact-stream
  sink/types.
- `config.rs` generated: CSS grammar metadata, not generic JSON policy.
- `generated.rs` generated: Track 1 parser for the exact row
  `css_l4/declaration_values/direct_to_struct/main`; emits
  `css_l4_declaration_value_fact_stream`.
- `parser.rs` generated or folded into `generated.rs`: CSS `ParserState` and
  public entry wrapper if the JSON split is retained.
- `sink.rs` hand-owned or generated per grammar: CSS fact stream collector.
- `host.rs` CSS-owned: normalization helpers only if required for strict oracle
  parity; no hidden host schema.

Do not add `value.rs`, `view.rs`, or `visitor.rs` for W1b-1 unless the wave
intentionally adds a CSS DOM/read API. W1b-1’s output plane is direct fact
stream, so JSON-style traversal is unnecessary risk.

## Generated Vs Hand-Owned Split

Generated:

- CSS `mod.rs`, `config.rs`, parser body, generated parser entrypoints, rule
  and fact ids, and any generated profile roster.
- Codegen provider profile for `css_l4_declaration_values`.

Hand-owned per grammar:

- CSS fact stream sink/collector if the fact schema is deliberately stable
  across generated parser and oracle.
- CSS host normalization helpers.
- Bench/oracle/gate plumbing outside runtime.

Generic codegen may be changed only to select a provider from metadata. Avoid
grammar-name policy branches in generic roots.

## Lock 14 Risks

- `runtime/src/lib.rs` edit is required for crate-level importability but is
  not listed in W1b-1 owner paths. The plan should add it explicitly with
  CHALLENGE.
- Reusing `JsonSink`, `JsonNodeKind`, JSON view wrappers, JSON structural
  bytes, or JSON parse errors in generic code fails Lock 14.
- CSS must not reuse JSON number/string policy: CSS allows leading-dot numbers
  and CSS escapes.
- `scan.rs` is JSON-owned and outside generated roster. A CSS scanner must be
  CSS-owned or generated; replacing only `STRUCTURAL_BYTES` is not legal.

## No-New-Directive Constraints

W1b-1 should use existing grammar imports, regex/literal rules, sink-only
lowering, and runtime provider metadata. Do not add BBNF directives, BIR
variants, `BackendShape` variants, public substrate APIs, parser-owned
sidecars, or decoded-byte sidecars.

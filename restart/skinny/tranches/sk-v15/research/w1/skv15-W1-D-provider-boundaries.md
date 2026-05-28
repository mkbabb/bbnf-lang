# SK-V15 W1-D: Provider Retirement Boundaries

Date: 2026-05-28.
Scope: CSS provider, generated runtime, summary/fact-stream, and brace-counter retirement boundaries.
Output: this file.

## Findings

- W1 cannot delete or retire live CSS provider/template/generated surfaces.
  `DEP-W6-CSS-GENERATED-RS`, `DEP-W6-CSS-SUMMARY-FACT-STREAM`, and
  `DEP-W3-W6-CSS-PROVIDER-TEMPLATE` place `CSS_GENERATED_RS`, summary/fact
  stream proof, and provider/template deletion no earlier than W5/W6 proof
  (`restart/skinny/tranches/sk-v15/SPEC.md:194`,
  `restart/skinny/tranches/sk-v15/SPEC.md:195`,
  `restart/skinny/tranches/sk-v15/SPEC.md:196`,
  `restart/skinny/tranches/sk-v15/SPEC.md:197`).
- W1's own dispatch text says providers stay live until W5 unless W5/W6-grade
  typed proof lands in the same wave
  (`restart/skinny/tranches/sk-v15/SPEC.md:268`,
  `restart/skinny/tranches/sk-v15/SPEC.md:272`,
  `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:135`).
- `CSS_GENERATED_RS` is emitted by `skinny/crates/codegen/src/runtime_generator.rs:92`,
  materialized at `skinny/crates/codegen/src/runtime_generator.rs:97`, and
  embedded as a string literal at `skinny/crates/codegen/src/runtime_generator.rs:713`.
- Generated CSS files still expose `emit_fact_stream`, `CssFullParseSummary`,
  full-parse summary emission, and parser entry points returning strings, for
  example `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5`,
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:54`,
  and `skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs:17`.
- The W8 harness should remain a diagnostic fixture in W1, not a provider
  deletion target (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:16`,
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs:120`,
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs:143`,
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs:217`).

## Recommendations

- W1 may mark `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only
  `parse()`, and brace-counter/four-counter output diagnostic-only for W8R
  purposes.
- W1 must not remove generated CSS modules, runtime exports, codegen profile
  rosters, `regen_css`, or root typed CSS provider files.
- Any W1 edit to `css_l4_w8.rs` should demote its disposition to diagnostic
  output only, not delete the harness or provider paths.

## Risks

- Provider deletion in W1 would violate the dependency table and reopen the
  SK-V14 W2/W4 wave-graph failure class.
- Root `crates/core/src/runtime/css_l4/**` files are dirty and outside the
  W1 demotion lane.

## Sources

- `restart/skinny/tranches/sk-v15/SPEC.md:194`
- `restart/skinny/tranches/sk-v15/SPEC.md:195`
- `restart/skinny/tranches/sk-v15/SPEC.md:196`
- `restart/skinny/tranches/sk-v15/SPEC.md:197`
- `restart/skinny/tranches/sk-v15/SPEC.md:268`
- `restart/skinny/tranches/sk-v15/SPEC.md:272`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:135`
- `skinny/crates/codegen/src/runtime_generator.rs:92`
- `skinny/crates/codegen/src/runtime_generator.rs:97`
- `skinny/crates/codegen/src/runtime_generator.rs:713`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:54`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs:17`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:16`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:120`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:143`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:217`

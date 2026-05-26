# SK-V14 W5B-GEN-C: Request Facts And Grammar Coverage

Date: 2026-05-26.
Scope: Read-only audit of W5A request structures, runtime source facts, and CSS L4 grammar compileability.
Output: this file.

## §1 — Findings

W5A admitted a request boundary, not a provider-free generator body.
`restart/skinny/tranches/sk-v14/research/skv14-W5A-redress.md:10` states that
the source-consuming capability is closed while provider/template deletion
remains W5B-owned. The amended V6 packet moves that deletion after W5B-GEN.

The request structures are present in `skinny/crates/codegen/src/grammar_provider.rs:4`.
`RuntimeGenerationRequest` carries grammar name, profile id, entry rule, source
roots, source bytes, workspace metadata, output dir, and expected files.
`RuntimeGrammarSource` carries path plus source at `grammar_provider.rs:16`.
`RuntimeWorkspaceMetadata` carries manifest and registry fields at
`grammar_provider.rs:21`.

The W5A source-fact surface scans constructs but does not compile CSS L4 source
into IR. `skinny/crates/grammar/src/lib.rs:59` defines `RuntimeSourceFacts`,
`lib.rs:141` parses source facts, and `lib.rs:188` recognizes imports, token and
workspace directives, commas, whitespace modifiers, shifts, projections, typed
projections, and host captures.

The actual `parse_grammar` path remains narrower than the CSS L4 grammar files.
`skinny/crates/grammar/src/lib.rs:320` accepts only `@import` and `@token`
directives, while `grammar/css/l4/stylesheet.bbnf:12` uses `@ws`,
`stylesheet.bbnf:15` through `stylesheet.bbnf:16` use `?w`, `>>`, and `<<`,
and `stylesheet.bbnf:53` through `stylesheet.bbnf:60` use `@pretty`.
`grammar/css/l4/values.bbnf:67` uses span capture, and `grammar/css/l4/color.bbnf:190`
and `color.bbnf:228` use typed host projections.

Therefore W5B-GEN cannot honestly be implemented as "compile the CSS L4 sources
through the existing generic lowerer" at current HEAD. The parser/import surface
needed for that route does not exist.

## §2 — Recommendations

The only admissible W5B-GEN plan must name a concrete provider-free generator
body that consumes `RuntimeGenerationRequest` and the source facts without
pretending that current `parse_grammar` can compile CSS L4. If the plan cannot
do that inside the W5B-GEN owner paths and cap, the wave must reject and route a
SPEC correction rather than land a static sidecar.

## §3 — Risks

Adding a `match profile_id` or `match grammar_name` replacement for
`RuntimeProvider` would preserve the same hidden coupling with different names.
The second amended SPEC grep at `restart/skinny/tranches/sk-v14/SPEC.md:736`
is designed to catch this class of regression.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:725` through `SPEC.md:756`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-redress.md:10`
- `skinny/crates/codegen/src/grammar_provider.rs:4`
- `skinny/crates/codegen/src/grammar_provider.rs:16`
- `skinny/crates/codegen/src/grammar_provider.rs:21`
- `skinny/crates/grammar/src/lib.rs:59`
- `skinny/crates/grammar/src/lib.rs:141`
- `skinny/crates/grammar/src/lib.rs:188`
- `skinny/crates/grammar/src/lib.rs:320`
- `grammar/css/l4/stylesheet.bbnf:12`
- `grammar/css/l4/stylesheet.bbnf:15`
- `grammar/css/l4/stylesheet.bbnf:53`
- `grammar/css/l4/values.bbnf:67`
- `grammar/css/l4/color.bbnf:190`
- `grammar/css/l4/color.bbnf:228`

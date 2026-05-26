# SK-V14 W5B-GEN CHALLENGE V1 CH5 Hidden Coupling

Date: 2026-05-26.
Lens: CH5 Hidden Coupling.
Disposition: ACCEPT.

## Findings

The W5B-GEN rejection plan identifies the hidden coupling correctly: current
runtime emission still routes through `render_runtime_profile`,
`RuntimeProvider`, `GrammarProfile`, `json_provider`, and CSS providers, while
SPEC §8B requires provider-free source+metadata generation and blocks static
centralization, committed generated-output-as-source, compatibility provider
dispatch, grammar-name branches, and borrowed W5C/W6 budget.

The corrective packet does not authorize deletion before replacement. It splits
the missing source frontend, provider-free generator, and provider/template
deletion into distinct waves. That preserves NEW-CH5-V4-01 and NEW-CH5-V6-01:
deletion stays coupled to the same-wave consumer path, and live provider
reachability remains a blocking grep until the provider-free body is
production-reachable.

Live evidence matches the rejection: `grammar_provider.rs` still imports and
calls `render_runtime_profile`, `lib.rs` still declares provider modules and
dispatches over `RuntimeProvider`, and `parse_grammar` still rejects CSS L4-only
constructs beyond `@import` and `@token`.

## Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:31`
- `restart/skinny/tranches/sk-v14/SPEC.md:725`
- `restart/skinny/tranches/sk-v14/SPEC.md:752`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:42`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:181`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:184`
- `skinny/crates/codegen/src/grammar_provider.rs:1`
- `skinny/crates/codegen/src/lib.rs:180`
- `skinny/crates/grammar/src/lib.rs:320`

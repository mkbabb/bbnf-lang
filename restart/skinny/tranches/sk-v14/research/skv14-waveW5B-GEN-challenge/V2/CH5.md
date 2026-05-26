# SK-V14 W5B-GEN CHALLENGE V2 CH5 Hidden Coupling

Date: 2026-05-26.
Lens: CH5 Hidden Coupling.
Disposition: ACCEPT.

## Findings

The V2 fold preserves coupling honesty. It keeps the W5B-GEN rejection anchored
to the real hidden coupling: live runtime emission still goes through
`render_runtime_profile`, `RuntimeProvider`, CSS providers, and `json_provider`,
while the skinny parser still cannot compile the CSS L4 construct set into IR.

The fold does not introduce a sidecar provider or generator. The plan rejects
static centralization, generated-output mining, compatibility provider dispatch,
grammar-name branches, and W5C/W6 budget borrowing. The corrective packet
reframes the missing work as generic BBNF frontend plus provider-free generator
for every admitted grammar, with CSS L4 only as the strict witness.

Deletion remains blocked until the same-wave consumer path exists. SPEC §8B
requires provider-free source/metadata emission first and makes
provider/template deletion later scope, with `regen-css`, seven CSS companions,
`check-json`, and provider-reachability grep as consumers.

## Sources

- `skinny/crates/codegen/src/grammar_provider.rs:69`
- `skinny/crates/codegen/src/lib.rs:180`
- `skinny/crates/codegen/src/lib.rs:233`
- `skinny/crates/grammar/src/lib.rs:320`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:101`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:51`
- `restart/skinny/tranches/sk-v14/SPEC.md:725`
- `restart/skinny/tranches/sk-v14/SPEC.md:748`
- `restart/skinny/tranches/sk-v14/SPEC.md:752`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:178`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:182`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:184`

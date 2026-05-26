# SK-V14 W5B.0 CH5 V2: Hidden Coupling

Date: 2026-05-26.
Scope: W5B.0 hidden-coupling review after CH1 V2 plan fold.
Disposition: ACCEPT.

## Findings

The two V1 hidden couplings are blocked. The plan replaces the CSS-only template
census with an all-template census (`skv14-W5B0-plan.md:19`-`21`,
`skv14-W5B0-plan.md:53`) and requires modified protected providers/templates to
fail with the rest of the protected status set (`skv14-W5B0-plan.md:42`-`44`).
Those folds address the research risks at
`skv14-W5B0-B-provider-template-topology.md:47`-`53`.

The plan does not let W5B.0 smuggle W5B frontend implementation or W5C/W5D
generator/deletion work. It blocks provider/template deletion, provider-free
generator-body replacement, grammar/codegen/xtask frontend implementation edits,
public `@ws`, grammar-name branch leakage, and W5C/W5D unblocking
(`skv14-W5B0-plan.md:74`-`81`).

## Required Folds

None.

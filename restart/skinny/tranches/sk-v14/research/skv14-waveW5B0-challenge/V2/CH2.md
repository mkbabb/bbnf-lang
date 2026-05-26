# SK-V14 W5B.0 CH2 V2: Generality

Date: 2026-05-26.
Scope: W5B.0 Lock 14 generality review after CH1 V2 plan fold.
Disposition: ACCEPT.

## Findings

The plan generalises the gate beyond CSS-only topology. It names the W5B owner
roster in grammar-neutral paths (`skv14-W5B0-plan.md:31`-`36`), requires the
all-template census (`skv14-W5B0-plan.md:19`-`21`), and retains the exact
`w5b_lock14_frontend_all_templates_guard_counts_8` test
(`skv14-W5B0-plan.md:53`). This folds the V1 requirement to include
`json_templates` with the seven CSS template dirs (`CONSOLIDATED.md:38`-`39`;
`skv14-W5B0-B-provider-template-topology.md:18`-`21`).

The provider exception is narrow. The plan rejects protected provider/template
mutation statuses and names `crates/codegen/src/grammar_provider.rs` as the only
provider exception (`skv14-W5B0-plan.md:42`-`44`), matching the research packet
(`skv14-W5B0-B-provider-template-topology.md:32`-`38`).

## Required Folds

None.

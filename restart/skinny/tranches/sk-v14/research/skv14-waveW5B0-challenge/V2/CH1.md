# SK-V14 W5B.0 CH1 V2: Correctness

Date: 2026-05-26.
Scope: read-only correctness review of `skv14-W5B0-plan.md` against `SPEC.md`
Section 8B, the W5B.0 research packets, and V1 consolidated challenge.
Disposition: REVISE.

## Findings

The plan is directionally correct on the W5B.0 self-cycle. It treats W5B.0 as
the gate that must run before W5B.1 through W5B.4
(`skv14-W5B0-plan.md:5`-`7`) and says the aggregate roster applies after W5B.0
admits (`skv14-W5B0-plan.md:31`-`32`). That matches the V1 fold reading the
`SPEC.md:728` entry-gate sentence as W5B.0 exit / W5B.1+ precondition
(`CONSOLIDATED.md:31`-`32`).

The exact-test evidence shape is acceptable. The plan lists all eight `SPEC.md`
tests (`skv14-W5B0-plan.md:36`-`43`) and requires dedicated tee logs plus
dedicated nonzero `rg` proof (`skv14-W5B0-plan.md:45`-`48`), matching
`SPEC.md:736`-`744` and `SPEC.md:764`-`767`.

## Required Folds

- Name the exact `SK_V14_W5B_FRONTEND_OWNER_PATHS` roster in the plan. `SPEC.md`
  requires W5B-FRONTEND to name exact owner paths, including any neutral module
  path (`SPEC.md:713`-`721`, `SPEC.md:729`). The plan currently says only that
  the roster admits aggregate W5B source owner paths (`skv14-W5B0-plan.md:31`-`32`).
- Name the accepted W5B parent-diff subject forms. V1 requires routing to cover
  W5B.0 through W5B.4 subject forms or require the aggregate
  `sk-v14-waveW5B-FRONTEND` token, while W5C/W5D remain rejected
  (`CONSOLIDATED.md:33`-`35`). The plan requires parent-diff routing generally
  (`skv14-W5B0-plan.md:19`-`21`) and has W5C/W5D tests
  (`skv14-W5B0-plan.md:37`-`38`), but does not state which W5B subject forms are
  admitted.
- Make the provider/template status guard fully measurable. V1 requires rejection
  of modified, added, deleted, renamed, and untracked protected
  providers/templates (`CONSOLIDATED.md:36`-`37`), and the research packet repeats
  that full status set (`skv14-W5B0-B-provider-template-topology.md:32`-`38`).
  The plan names modified-provider/template tests (`skv14-W5B0-plan.md:39`-`40`)
  but does not explicitly require the A/D/R/`??` statuses to remain rejected
  after the W5B.0 tightening.

## Fold Status

The plan fold lands at `skv14-W5B0-plan.md:31`-`44`: exact roster, explicit
W5B.0..W5B.4 subject forms, W5C/W5D rejection, full protected-status rejection,
and the `grammar_provider.rs` exception.

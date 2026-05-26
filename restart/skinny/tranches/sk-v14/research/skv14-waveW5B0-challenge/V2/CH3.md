# SK-V14 W5B.0 CH3 V2: Regression

Date: 2026-05-26.
Scope: W5B.0 regression and wave-graph review after CH1 V2 plan fold.
Disposition: ACCEPT.

## Findings

The plan preserves the V8 sequencing correction. W5B.0 runs before W5B.1 through
W5B.4 (`skv14-W5B0-plan.md:5`-`7`), and W5C/W5D remain rejected until their own
Lock 14 gates land (`skv14-W5B0-plan.md:37`-`41`). This matches SPEC's aggregate
W5B close rule (`SPEC.md:723`-`732`) and W5C gate (`SPEC.md:817`-`823`).

The modified-provider regression is closed in the plan. V1 identified that W5A
allowed modified protected providers/templates (`CONSOLIDATED.md:36`-`37`;
`skv14-W5B0-B-provider-template-topology.md:23`-`28`). The folded plan requires
modified, added, deleted, renamed, and untracked protected paths to fail
(`skv14-W5B0-plan.md:42`-`44`) and keeps exact modified-provider/template tests
(`skv14-W5B0-plan.md:51`-`52`).

## Required Folds

None.

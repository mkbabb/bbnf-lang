# SK-V12 W4 CHALLENGE V3 - CH3 Regression And REDRESS

Verdict: REVISE.

PLAN-V3 fixes the REDRESS-125 reuse problem, but two REDRESS-accounting issues
remain before redress can dispatch.

## Findings

1. The default `MEASURED-REJECT` branch is a pre-production microbench-only
   branch, while A6 and parts of the gate text still require
   `same-wave consumer` and strict equality evidence. Those requirements are
   correct for a production ASM admission, but inconsistent with a branch that
   deliberately refuses to ship a production consumer when the microbench fails.
   PLAN-V4 must state the contract explicitly: either pre-production
   microbench-only rejection is valid evidence for the ASM-gen attempt, or the
   branch must include production consumer and equality proof. If the
   microbench-only branch remains selected, it must not claim strict
   fact-stream equality or a same-wave production consumer.

2. Orphan disposition still mixes evidence labels with final disposition labels.
   Rows such as `production_reachable_scalar_delegate` are useful evidence, but
   they are not final close vocabulary. PLAN-V4 must normalize every final
   orphan row to one of the SPEC close classes: consumed, removed, or inventory
   demoted with evidence. `production_reachable_scalar_delegate` may remain as
   an evidence field beneath `inventory_demoted_with_evidence`.

Accepted regression guardrails:

- JSON no-touch remains the right default guard if no JSON/report/gate source
  path moves.
- W5, not W4, owns final `RESULTS.md` campaign-close movement.
- PLAN-V3 no longer risks paper-reusing REDRESS-125 as the W4 gate.

PLAN-V4 is required before redress.

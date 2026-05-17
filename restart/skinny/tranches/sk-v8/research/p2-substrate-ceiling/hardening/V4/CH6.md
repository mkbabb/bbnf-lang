# CH6 - Anti-Paper-Close Review, S-P2 Substrate-Ceiling V4

Role: CH6 Anti-Paper-Close.

Verdict: REVISE.

Score: 92/100.

## Blocking Findings

1. **One SC-1 deferral phrase still leaves a carry-forward aperture.**

   The V4 fold repairs the specific V3 R2 failure: SC-1 now says a
   number-heavy maintain miss rejects the current Tier A candidate for this
   cycle, and later reconsideration needs fresh W0 evidence plus a newly
   accepted S-P3/W3 plan (`SC-1-offset-tape-teardown.md:394`). That closes the
   old "routed to a later independent S-P3 proof" wording required by the V3
   consolidated hardening (`HARDENING-S-P2-V3-CONSOLIDATED.md:210-214`).

   But SC-1's grammar-generalisation caveat still says that if Tier A cannot
   satisfy the invariant for a grammar class, "the union candidate is rejected or
   routed to a separate S-P3 proof" (`SC-1-offset-tape-teardown.md:366-377`).
   That is still a deferral-shaped escape hatch. The S-P2 prompt is explicit:
   a candidate deferred to "a future wave will detail" is a paper-close; the
   research grounds the candidate now or drops it
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:133-138`). The governing
   convergence loop also says hardening must fold before advancement and the
   next pass does not dispatch before convergence (`ORCHESTRATOR.md:110-123`).

   This matters because the rest of V4 is now strict: SC-2 says both tiers are
   unselected until S-P3/W3 supplies owner paths, thresholds, strict planes, and
   accepted challenge evidence (`SC-2-two-stage-sota.md:373-378`), SPEC says W3
   needs W0/W1 closure plus a fresh plan and challenge acceptance
   (`SPEC.md:447-461`), and HANDOFF says SC-1..SC-6 authorize no W3 plan by
   themselves (`HANDOFF.md:71-85`). SC-1 should not preserve a looser "separate
   S-P3 proof" phrase that a later planner can cite as a routed promise.

## Notes

- The two-consecutive-ACCEPT guard is now materially folded into the packet.
  ORCHESTRATOR requires `>=95% ACCEPT` for two consecutive cycles or a user pin
  (`ORCHESTRATOR.md:118-123`), and S-P2 repeats that rule
  (`PASS-2-RESEARCH.md:155-158`). SYNTHESIS and SPEC now state that V1, V2, and
  V3 did not converge and that a future V4 ACCEPT would be only the first ACCEPT
  after REVISE, with no automatic S-P3 (`SYNTHESIS.md:91-99`,
  `SYNTHESIS.md:188-191`, `SPEC.md:454-457`). HANDOFF carries the operational
  caveat, but should be tightened from "V1 and V2 did not converge" to
  "V1, V2, and V3 did not converge" for consistency (`HANDOFF.md:71-76`).

- G-Alpha/W0-only sequencing is strong. SYNTHESIS says G-Alpha sign-off
  authorizes only W0 unless later post-W0 plan augmentations are explicitly
  signed off (`SYNTHESIS.md:14-17`) and repeats that posture at G-Alpha
  (`SYNTHESIS.md:263-268`). SPEC says no implementation wave dispatches before
  G-Alpha and only W0 dispatches after it (`SPEC.md:180-193`,
  `SPEC.md:587-594`). HANDOFF and DISPATCH-PROMPT match that sequencing
  (`HANDOFF.md:5-7`, `HANDOFF.md:174-179`, `DISPATCH-PROMPT.md:6-9`,
  `DISPATCH-PROMPT.md:127-137`).

- Executable gates now mostly displace rhetoric. SPEC requires `gate-json` to
  reject strict admission unless comparator plane, strictness, freshness, and
  measured-row validation all hold (`SPEC.md:117-123`), requires W0 to validate
  SK-V8 telemetry and reject malformed sidecars (`SPEC.md:289-316`), and makes
  W3 conditional on owner paths, same-wave consumer, thresholds, pre-blocked
  routes, challenge acceptance, scalar/checkasm where relevant, and Lock 1
  proof (`SPEC.md:417-489`). SC-2 and SC-3 add concrete owner/cost/test tables
  for Tier A (`SC-2-two-stage-sota.md:337-349`,
  `SC-3-union-substrate-design.md:469-501`).

- The string knee is diagnostic-only in the V4 fold. SC-4 marks the quote-density
  table as diagnostic rather than an admission gate (`SC-4-string-plane-gap.md:190-193`)
  and requires an in-repo command, row set, numeric target, formula, maintain
  budget, and pass/fail rule before any plan may pass or fail on string-fraction
  displacement (`SC-4-string-plane-gap.md:322-327`). SPEC and HANDOFF preserve
  that posture (`SPEC.md:441-445`, `HANDOFF.md:62-69`).

- `tape_vs_tape` is no longer hidden W0/W1 work. SPEC routes it as residual, not
  default W0/W1 scope, and says it cannot satisfy W3's production same-wave
  consumer (`SPEC.md:125-131`, `SPEC.md:450-452`). SC-5 prices the possible
  future work with owner files, focused tests, and rerun budget while still
  routing it outside default V4 scope (`SC-5-k-classification-adjudication.md:326-346`).

## Required Folds

1. Replace SC-1's Section 4 phrase "rejected or routed to a separate S-P3 proof" with
   the same hard posture already used in SC-1 R2: if Tier A cannot satisfy the
   invariant for a grammar class, the current Tier A candidate is rejected for
   this S-P2 cycle. Later reconsideration requires fresh W0 evidence, a newly
   accepted S-P3/W3 plan, exact owner paths, tests, numeric thresholds, same-wave
   production consumer, and challenge acceptance; it is not a carry-forward
   promise from S-P2.
2. Tighten HANDOFF's governance sentence to name V1, V2, and V3 as
   non-converged cycles, preserving the "future V4 ACCEPT is only the first
   ACCEPT after REVISE; no automatic S-P3" caveat.
3. Preserve the accepted folds: strict-vs-strict only for strict admission,
   `parse_only`/string-knee evidence as diagnostic or guard telemetry only,
   `tape_vs_tape` as routed residual and never a W3 production consumer, no
   deferrals, no new directive/BIR/BackendShape/public substrate API, no
   parallel substrate, and G-Alpha authorizes W0 only.

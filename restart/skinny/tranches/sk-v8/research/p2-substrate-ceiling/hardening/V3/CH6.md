# CH6 — Anti-Paper-Close Review, S-P2 Substrate-Ceiling V3

Role: CH6 Anti-Paper-Close.

Verdict: REVISE.

Score: 84/100.

## Blocking Findings

1. **Strict-vs-strict comparator drift remains inside the folded cohort.**

   V3 correctly repairs the packet summary in places: `SYNTHESIS.md:138-140`
   says twitter is `-25.1%` versus same-run sonic strict and `-35.8%` versus
   simdjson DOM, and citm_catalog is `+24.6%` versus same-run sonic strict and
   `-11.3%` versus simdjson DOM. `SC-4-string-plane-gap.md:95-104` and
   `SC-5-k-classification-adjudication.md:41-61` also state the authoritative
   column order and explicitly mark canada `+54.6%` / mesh `+51.5%` as simdjson
   DOM sidecar values, not sonic-strict values.

   But SC-1 and SC-2 still carry the old shifted-column claims. SC-1 labels
   canada `+54.6%` and mesh `+51.5%` as "same-run sonic strict" and "strict win
   evidence" (`SC-1-offset-tape-teardown.md:101-104`), repeats them as "same-run
   strict `parse_only` wins" (`SC-1-offset-tape-teardown.md:196-197`), and also
   labels citm_catalog `-11.3%`, update_center `-63.4%`, apache_builds
   `-65.3%`, github_events `-61.7%`, and distinct_values `-70.8%` as same-run
   sonic-strict deltas (`SC-1-offset-tape-teardown.md:107-116`) even though those
   are simdjson DOM sidecar values where populated. SC-2 likewise says canada
   `+54.6% vs sonic strict` and mesh `+51.5%` in its evidence bullet
   (`SC-2-two-stage-sota.md:248-255`). The authoritative table has canada
   `+27.9%` versus sonic-strict and `+54.6%` versus simdjson DOM, and mesh
   `+21.4%` versus sonic-strict and `+51.5%` versus simdjson DOM
   (`skinny/RESULTS.md:10`, `skinny/RESULTS.md:19`); sidecar provenance is
   non-admission by policy (`skinny/RESULTS.md:219`).

   This is a paper-close blocker because a later planner can cite SC-1/SC-2's
   "strict win evidence" while silently using sidecar columns. The packet's own
   strict-admission rule requires matching plane, strictness, same-run freshness,
   and measured-row validation (`SPEC.md:117-123`).

2. **The two-consecutive-ACCEPT guard is not folded uniformly into the packet.**

   The governing rule is clear: a pass advances only after `>=95% ACCEPT` for
   two consecutive cycles with no unresolved REVISE, or an explicit user pin
   (`ORCHESTRATOR.md:118-123`; `PASS-2-RESEARCH.md:155-158`). V1 and V2 both
   failed (`HARDENING-S-P2-V1-CONSOLIDATED.md:5-7`;
   `HARDENING-S-P2-V2-CONSOLIDATED.md:6-14`), and the V2 consolidated fold
   required V3 to make a future single ACCEPT cycle count only as the first
   ACCEPT after REVISE (`HARDENING-S-P2-V2-CONSOLIDATED.md:285-290`).

   HANDOFF now states that correctly (`HANDOFF.md:71-83`). SYNTHESIS and SPEC do
   not. SYNTHESIS says the cohort "converged" on the finding
   (`SYNTHESIS.md:91-95`) and then presents the lead W3 hypothesis
   (`SYNTHESIS.md:169-179`) without the V1/V2-not-converged / single-ACCEPT
   caveat. SPEC nominates the S-P2 cohort's lead W3 hypothesis and says W3
   requires W0/W1 closure and a fresh plan (`SPEC.md:413-444`), but it also lacks
   the explicit S-P2 two-cycle guard. That leaves the user-facing packet with one
   file saying "future single ACCEPT is only first ACCEPT" and two packet files
   omitting that governance.

   This is a paper-close blocker because S-P2 V3 ACCEPT, if it happens, must not
   be treated as automatic S-P3 eligibility. The guard must appear wherever the
   V3-folded S-P2 cohort is summarized or nominated as W3 input.

3. **One residual deferral phrase still leaves an escape hatch after failure.**

   SC-1's R2 says that if Tier A misses the number-heavy maintain budget, the
   candidate "must be rejected or routed to a later independent S-P3 proof"
   (`SC-1-offset-tape-teardown.md:392-393`). The pass contract says S-P2
   candidates may not defer grounding to a future wave: a candidate deferred to
   "a future wave will detail" is a paper-close; the research grounds it now or
   drops it (`PASS-2-RESEARCH.md:133-138`). The rest of V3 is stricter than this
   line: SC-2 says both tiers remain unselected until S-P3/W3 challenge supplies
   owner paths, revert protocol, numeric thresholds, strict planes, and accepted
   challenge evidence (`SC-2-two-stage-sota.md:369-371`), and HANDOFF says
   SC-1..SC-6 authorize no W3 plan by themselves (`HANDOFF.md:79-83`).

   The SC-1 line should be folded to the stricter posture: a miss rejects the
   current Tier A candidate. Any later reconsideration requires a new
   independently dispatched, evidence-bearing pass/plan; it is not a routed
   promise attached to this S-P2 close.

## Non-Blocking Notes

- The G-Alpha/W0-only sequencing is otherwise strong. SYNTHESIS says G-Alpha
  closed authorizes W0 only (`SYNTHESIS.md:251-256`), SPEC says no
  implementation wave before G-Alpha and only W0 after G-Alpha (`SPEC.md:172-184`,
  `SPEC.md:574-580`), and HANDOFF repeats the same constraint
  (`HANDOFF.md:5-7`, `HANDOFF.md:172-179`).
- The diagnostic-only string-knee fold is materially improved. SC-4 says the
  quote-density table is diagnostic, not an admission gate
  (`SC-4-string-plane-gap.md:190-193`), and requires an in-repo command, row set,
  numeric target, formula, maintain budget, and pass/fail rule before any plan
  can use string-fraction movement (`SC-4-string-plane-gap.md:322-327`).
- Lock 1 / Lock 14 anti-sidecar posture is now mostly executable: SPEC rejects
  sidecar projections that run beside the offset tape (`SPEC.md:470-476`), and
  SC-6 removes the `UnionTape`/new-variant path in favor of representation
  replacement (`SC-6-lock1-amendment-generalisation.md:657-666`).

## Required Fold Actions

1. Correct SC-1 and SC-2 comparator deltas against the authoritative RESULTS
   column order. Canada is `+27.9%` sonic-strict and `+54.6%` simdjson DOM
   sidecar; mesh is `+21.4%` sonic-strict and `+51.5%` simdjson DOM sidecar.
   Apply the same correction to citm_catalog, update_center, apache_builds,
   github_events, distinct_values, and any other SC-1 table row currently using
   the simdjson DOM sidecar as the sonic-strict delta.
2. Add the HANDOFF convergence caveat to SYNTHESIS and SPEC: V1 and V2 did not
   converge; a V3 ACCEPT cycle is only the first ACCEPT cycle after REVISE unless
   followed by another qualifying ACCEPT cycle or explicitly pinned by the user;
   no automatic S-P3 follows one V3 ACCEPT.
3. Replace SC-1's "routed to a later independent S-P3 proof" phrase with a
   hard rejection/reroute rule: failed Tier A is rejected for this cycle, and any
   later reconsideration requires fresh W0 evidence plus a new accepted S-P3/W3
   plan rather than a carry-forward promise.
4. Preserve the already-correct folds: `tape_vs_tape` remains W0/W1 telemetry
   only, string-density remains diagnostic only, G-Alpha authorizes W0 only, and
   no new directive, BIR variant, `BackendShape`, public substrate type, or
   parallel substrate is introduced.

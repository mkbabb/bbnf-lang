# S-P2 V3 CH4 - Cost

Role: CH4 (Cost) adversarial review of the V3-folded S-P2
substrate-ceiling cohort and packet docs.

Verdict: REVISE

Score: 88/100

## Blocking Findings

1. **`tape_vs_tape` is still a W0/W1 gate obligation without a manifest slot.**
   V3 correctly says `tape_vs_tape` is telemetry only and cannot satisfy W3's
   production same-wave consumer (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:141-145`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:79-83`,
   `restart/skinny/tranches/sk-v8/SPEC.md:439-444`). SC-5 prices standing it
   up at about 120-180 LOC across `json_parity.rs`, `metadata.rs`,
   `report.rs`, `gate.rs`, and `src/bin/gate.rs`, plus workload-map,
   comparator-plane refusal, deferred/view-boundary refusal, stale-sidecar
   refusal, bbnf-only structural-scan rejection tests, and one gate refresh
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:326-342`).
   But the actual W0 tasks do not include a `tape_vs_tape` row or the SC-5
   focused tests (`restart/skinny/tranches/sk-v8/SPEC.md:279-308`), and W1 is
   scoped to CostFacts evidence binding, not comparator-harness workload
   insertion (`restart/skinny/tranches/sk-v8/SPEC.md:321-358`). This leaves the
   SC-5 work neither fully folded into W0/W1 nor explicitly residual. Under CH4,
   it is hidden gate work: priced in SC-5 prose, but not budgeted in the wave
   manifest that would own the 120-180 LOC and one gate refresh.

## Non-Blocking Notes

- The V2 W3 overpricing defect is materially fixed. SPEC now splits Tier A
  structural-class cursor migration from Tier B string-boundary /
  quote-backslash-parity / CostFacts-template work (`restart/skinny/tranches/sk-v8/SPEC.md:423-431`),
  and SC-3 bars Tier A from claiming string-boundary closure, parity masks,
  density policy, broad lowerer bodies, or non-JSON production migration
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:411-428`).
- Tier A now has an S-P3-ready owner/cost table with owner files, LOC, generated
  audit, row/plane targets, same-wave consumers, named commands, and revert
  slices (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:466-493`).
  Its touched/proven-untouched rows cover retained view, `path!`,
  direct/SinkOnly, Track 2, and Lock 14 proof (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:473-477`).
- Non-JSON proof is no longer an unpriced afterthought for Tier A. SPEC requires
  CSS L4, Sheets, and BBNF-self proof for generic edits
  (`restart/skinny/tranches/sk-v8/SPEC.md:257-261`), and SC-3 puts named
  no-op/diff tests and grep/API scans inside the Tier A table
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:477`).
- The no-dispatch posture is intact. SYNTHESIS and HANDOFF keep W3 blocked on
  W0/W1 closure, fresh S-P3/W3 planning, exact owners, same-wave production
  consumer, revert protocol, thresholds, measured-path proof, and challenge
  acceptance (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:169-179`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:71-83`).

## Required Fold Actions

1. Fold SC-5's `tape_vs_tape` path into the packet manifest one way or the
   other:
   - If it is W0 work, add it to W0 tasks/owner paths with the 120-180 LOC,
     focused tests, malformed/stale/plane-mismatch refusal tests, and one gate
     refresh budget from SC-5.
   - If it is W1 work, add it to W1 as an explicit comparator-plane gate-binding
     augmentation and show how it fits beside the 300 LOC CostFacts cap and one
     gate refresh.
   - If it is not in SK-V8 default scope, mark it as a routed residual and keep
     W0/W1 limited to the already-manifested schema/refusal/CostFacts work.
2. Preserve the current W3 guard: `tape_vs_tape` remains telemetry/gate-binding
   only and must not count as Tier A's production same-wave consumer.

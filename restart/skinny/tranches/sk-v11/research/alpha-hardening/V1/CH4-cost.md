# SK-V11 Pass Alpha CHALLENGE V1 - CH4 Cost/Feasibility

Date: 2026-05-19.
Lens: CH4 cost / feasibility.
Scope: Review SK-V11 Pass Alpha V1 candidate family, pass order, micro-prove-first discipline, and S-P3 planning freedom.
Output: `restart/skinny/tranches/sk-v11/research/alpha-hardening/V1/CH4-cost.md`.

## Disposition

ACCEPT-WITH-NITS.

No blocking cost defect. The Alpha packet does not over-authorize source work,
does not create a wave plan early, and keeps S-P3 responsible for exact owner
paths, gates, hard caps, revert protocol, same-wave consumer, and pre-blocked
routes as PASS-ALPHA requires (`restart/prompts/pass-contracts/PASS-ALPHA.md:112`).
The candidate set is feasible for skinny discipline if S-P3 treats the high-risk
SIMD candidates as proof-first, single-consumer waves rather than omnibus
primitive + all-row closure attempts.

## Findings

### CH4-1 - PASS: Alpha leaves the implementation packet to S-P3

PASS-ALPHA states that Pass Alpha produces `SYNTHESIS.md` and `HANDOFF.md`,
while `SPEC.md` is downstream S-P3 work (`restart/prompts/pass-contracts/PASS-ALPHA.md:3`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:53`). Alpha-F preserves that
boundary: it explicitly refuses to create `SPEC.md` or `DISPATCH-PROMPT.md`
before S-P1/S-P2 converge (`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:10`),
and HANDOFF says the immediate move is S-P1 with no source edits
(`restart/skinny/tranches/sk-v11/HANDOFF.md:82`,
`restart/skinny/tranches/sk-v11/HANDOFF.md:97`).

Cost impact: good. The packet does not hide redress cost by pretending Alpha can
settle implementation granularity. It gives S-P3 the correct freedom to drop,
split, or resequence candidates after profiling.

### CH4-2 - PASS: Micro-prove-first is a cost reducer, not a new over-scope

The global eligibility rules require fresh SK-V11 profiling before every
candidate, and require SIMD candidates to prove scalar oracle, checkasm/parity,
representative JSON and non-JSON slices, host flags, feature gates, and caller
speed before S-P3 wave scoping (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:57`).
SYNTHESIS carries the same gate into the close condition
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:60`) and into the S-P2/S-P3
candidate filter (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:165`).
Alpha-F also captures the W7/W8/W9 cost lesson: parity and isolated primitive
speed are insufficient without a real same-wave consumer
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:79`).

Cost impact: good. This is the discipline that prevents another W3/W7-style
large patch from reaching redress before the cheap evidence says it can pay.

### CH4-3 - NIT: C1 is a likely chokepoint and should be split if the first non-JSON adapter is not already runnable

C1 bundles report/gate metadata, SK-V11-open residual-floor refresh,
micro-proof manifest consumption, and the first non-JSON direct/typed benchmark
row (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:107`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:136`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:150`).
Its stated budget is 220-380 LOC plus 80-160 LOC for the first non-JSON adapter
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:161`).
That can fit a skinny redress only if an existing CSS/Sheets bench path is
close to runnable. It is tight if C1 must also design a new report schema and a
new grammar adapter.

Recommended fold: S-P3 should write C1 as two admissible waves if S-P1 finds no
runnable non-JSON harness: W0a freezes JSON telemetry and micro-proof manifest
consumption; W0b adds exactly one non-JSON benchmark/oracle row. This preserves
the 75-minute redress cap (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:71`)
without weakening the close condition.

### CH4-4 - NIT: C2-C5 should default to proof-first plus one production consumer, not primitive plus broad row sweep

The four row-moving candidates are individually plausible, but their top-end
budgets are above what a single redress wave should assume when paired with a
new non-JSON adapter:

- C2: 260-460 LOC plus 120-220 LOC for a non-JSON adapter, HIGH risk
  (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:235`,
  `restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:239`).
- C3: 220-420 LOC plus 100-240 LOC, MEDIUM-HIGH risk
  (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:310`,
  `restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:314`).
- C4: 300-560 LOC and explicitly says to split proof and production if the
  redress cap cannot hold both (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:383`).
- C5: 220-420 LOC plus 80-180 LOC, HIGH risk
  (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:452`,
  `restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:456`).

Recommended fold: S-P3 should default every high-risk kernel family to a
proof-first sub-wave followed by one same-wave production consumer over at most
one or two named rows. C2 already caps row movement at two rows
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:223`);
apply that discipline uniformly to C3-C5 unless S-P1/S-P2 produce unusually
strong evidence and an existing non-JSON adapter is already running. This is
consistent with the triumvirate rule that every redress wave has a single
implementation thread and 60+15 minutes for implementation plus measurement
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:65`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:69`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:71`).

### CH4-5 - NIT: Clarify where pre-S-P3 micro-proofs live

The contract correctly forbids source work before S-P3
(`restart/skinny/tranches/sk-v11/HANDOFF.md:115`), but Alpha-E also says S-P2
should micro-prove C2-C5 before S-P3 wave scoping
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:489`).
Those statements are compatible only if pre-S-P3 micro-proofs are read-only
research artifacts, throwaway `/tmp` benches, or existing bench invocations, and
durable source changes wait for the S-P3-authored wave plan.

Recommended fold: add one sentence to HANDOFF or SYNTHESIS: "Pre-S-P3
micro-proofs are research artifacts and do not authorize source edits; durable
micro-proof harness changes land only in the S-P3-authored wave packet." This
will prevent accidental role merger with the source-free S-P1/S-P2 pass
discipline.

## Feasibility Summary

The pass order is feasible:

1. G-Alpha presents the Alpha contract.
2. S-P1 refreshes profile, PMU, direct residual rows, guard rows, and non-JSON
   harness inventory.
3. S-P2 researches only candidates with fresh hot-leaf and micro-proof evidence.
4. S-P3 writes SPEC/DISPATCH with hard caps, exact owner paths, and sub-waves.
5. Redress admits or rejects one bounded intervention at a time.

The candidate family is feasible if S-P3 keeps the Alpha-E recommendation's
"one of C2-C5 selected by fresh hot-leaf evidence" posture
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:491`)
and does not try to close all 11 direct rows with a single broad SIMD wave.
The current packet gives S-P3 enough freedom to do that; the nits above are
precision folds, not blockers.

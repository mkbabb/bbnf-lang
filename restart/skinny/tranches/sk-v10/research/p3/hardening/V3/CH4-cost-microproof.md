# SK-V10 S-P3 V3 CH4 Cost / Micro-Proof

Verdict: ACCEPT.

Acceptance: 100%.

## Scope

Lens: CH4 cost and micro-proof confirmation for the S-P3 V3 contract. This
audit checks that the V2 acceptance has not regressed across five cost-bearing
surfaces: binding LOC budgets, redress cap semantics, W7/W8 threshold-bearing
proof exits, W9 one primitive/caller/plane/target set, and clarified proof-only
caller evidence versus W9 production wiring.

Primary inputs:

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH4-cost-microproof.md`

## Findings

### F1 - Binding LOC budgets still bind every wave. ACCEPT.

No regression from V2. The authoritative manifest still carries a budget column
for every wave, including W7 `90-260 proof LOC`, W8 `90-260 proof LOC`, and W9
`220-420 source/bench/gate LOC` (`SPEC.md:162-175`). The dispatch manifest
mirrors the same bands for W7, W8, and W9 (`DISPATCH-PROMPT.md:44-57`), so cost
control is present in both the governing spec and the execution prompt.

Plan discipline also remains cost-bearing: each plan must name exact owner paths,
entry gate, exit gate, falsifiability threshold, LOC budget, risk class, revert
protocol, same-wave consumer, scalar reference or oracle, and
checkasm/differential requirement (`DISPATCH-PROMPT.md:82-86`).

Required fix: none.

### F2 - Redress cap semantics remain normalized. ACCEPT.

No regression from V2. The SPEC still defines the manifest cap as the redress
execution cap and separates it from research, plan, and CHALLENGE timing
(`SPEC.md:197-202`). The dispatch prompt repeats the same interpretation:
research is capped at 30 minutes per agent, plan at 30 minutes per agent, and
CHALLENGE at 60-90 minutes when required; at 0.9x of the redress cap the agent
must commit or record REDRESS rejection, and at the cap it halts for an extension
decision (`DISPATCH-PROMPT.md:220-224`).

Required fix: none.

### F3 - W7/W8 still require threshold-bearing proof exits. ACCEPT.

No regression from V2. W7 entry remains limited to exactly one string primitive
family plus cap, output plane, caller, scalar oracle, representative slices,
feature gate, and failure threshold (`SPEC.md:544-546`). Its exit gate requires
passing scalar/differential proof and a caller microbench artifact that clears the
predeclared threshold while recording observed value, threshold, run id, host
triple, build flags, feature gate, representative corpus slices, sample count,
scalar oracle identity, and differential harness identity (`SPEC.md:557-566`).

W8 keeps the same threshold-bearing shape for escape/segment proof. CHALLENGE
must accept one primitive family, caller, scalar oracle, representative slices,
feature gate, and failure threshold (`SPEC.md:592-595`), and the exit gate
requires the same threshold-clearing caller microbench artifact and metadata
(`SPEC.md:607-616`). The dispatch prompt preserves this as a load-bearing fact
for W7 and W8 proof-only closure (`DISPATCH-PROMPT.md:158-162`).

Required fix: none.

### F4 - W9 remains narrowed to one production slice. ACCEPT.

No regression from V2. W9 is still limited to a relevant accepted W7 or W8
`C4`-`C7` primitive (`SPEC.md:627-630`). The manifest rules continue to say W9
consumes exactly one relevant accepted proof for the exact primitive and caller,
while `C8` digit/number and `C9` whitespace/class work cannot feed W9 without a
future SPEC/CHALLENGE amendment (`SPEC.md:188-191`).

The W9 task envelope is still narrow enough for dispatch: it wires only the
proven primitive into the named existing caller (`SPEC.md:648-651`) and limits
the dispatch to exactly one proven primitive, exactly one existing production
caller, exactly one consumer plane, and one row-moving target set
(`SPEC.md:654-657`). The dispatch prompt mirrors the same one-primitive,
one-caller, one-plane, one-target-set limit (`DISPATCH-PROMPT.md:163-166`).

Required fix: none.

### F5 - Proof-only caller evidence and W9 production wiring are now clarified. ACCEPT.

The V2 CH4 report accepted the contract with one required wording fix because the
dispatch prompt could be read as requiring W7/W8 production consumers. V3 closes
that ambiguity. The current dispatch prompt says W7 and W8 CHALLENGE must reject
plans combining multiple primitive families, missing scalar oracle, missing
checkasm/differential parity, lacking an identified existing caller, or lacking a
threshold-bearing caller microbench; only W9 must also reject a plan lacking
same-commit production consumer wiring (`DISPATCH-PROMPT.md:107-112`).

That matches the SPEC split. W7 identifies the current caller but does not
production-wire it and moves no `RESULTS.md` row (`SPEC.md:557-566`). W8 also
forbids production caller behavior and moves no `RESULTS.md` row
(`SPEC.md:597-616`). W9 is the production wave: its exit gate requires the named
production caller to consume the primitive in the same commit (`SPEC.md:659-662`).

Required fix: none.

## Required Fixes

None.

## Disposition

ACCEPT at 100%. V3 preserves all V2 CH4 acceptance surfaces and closes the only
V2 follow-up. The contract has binding LOC budgets, normalized redress cap
semantics, threshold-bearing W7/W8 proof exits, a one-slice W9 production
envelope, and unambiguous proof-only caller evidence versus W9 same-commit
production wiring.

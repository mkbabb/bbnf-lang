# SK-V10 S-P3 V1 CH4 Cost / Micro-Proof

Verdict: REVISE.

Acceptance: 78%.

## Scope

Lens: CH4 COST for S-P3 V1. This audit checks whether SK-V10 carries bounded
wave cost, plausible <=90-minute execution, micro-prove-first requirements,
proof-only versus production splits, and enough specificity for W7-W9.

Primary inputs:

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md`

## Findings

### F1 - Final SPEC drops per-wave LOC budgets required by CH4

`PASS-3-SYNTHESIS-PLAN.md:128-132` requires every wave to carry a LOC budget,
hard cap, phase breakdown, and same-wave-consumer requirement. The final SPEC
manifest carries only a hard cap column (`SPEC.md:160-175`). The richer LOC
budget existed in the P3-F draft (`p3f-spec-draft.md:175-188`) but was not
preserved in `SPEC.md`.

This is a CH4 contract miss because implementation agents bind to the final
SPEC and dispatch prompt, not to a draft. W7 and W8 therefore retain proof-only
task language, but no binding 90-260 proof-LOC band in the final SPEC
(`SPEC.md:479-558`). W9 carries no binding production LOC budget in the SPEC
even though P3-F scoped the total W7-W9 budget as 350-650 LOC
(`p3f-spec-draft.md:184-186`).

Required fix: copy the source/edit budget column from P3-F into the final SPEC
manifest, or add a per-wave "Budget" block under each SPEC wave section. W7 and
W8 must be `90-260 proof LOC; no production wiring`. W9 must be a production
slice budget, not a pooled W7-W9 budget.

### F2 - Phase cap semantics conflict across S-P3 artifacts

The synthesis-plan CH4 lens asks for a phase breakdown by research / plan /
redress (`PASS-3-SYNTHESIS-PLAN.md:128-130`). `DISPATCH-PROMPT.md:65-123`
provides the phase protocol and gives research and plan 30-minute caps
(`DISPATCH-PROMPT.md:70-87`). It also says every dispatch carries a minute cap
and halts at the cap (`DISPATCH-PROMPT.md:197-199`).

P3-B contradicts that by stating every wave has a hard cap of 90 minutes
research, 90 minutes plan, and 90 minutes redress (`p3b-wave-sequencing.md:34-36`).
The SPEC manifest says only `<=90 min` per wave (`SPEC.md:164-175`). P3-F says
some waves have `<=90 min / 75-min redress` (`p3f-spec-draft.md:184-187`).

This makes the 90-minute cap implausible as a binding operational rule: the
same wave can be read as 90 minutes total, 90 minutes per phase, or 75 minutes
redress inside a 90-minute total cap.

Required fix: normalize all artifacts to one interpretation. Recommended:
SPEC wave hard cap is <=90 minutes for redress execution, with pre-redress
research and plan capped by the dispatch protocol at 30 minutes each. If the
intended cap is <=90 minutes total across research/plan/redress, W7-W9 must be
split further or the cap must explicitly force REDRESS rejection at planning
when scalar oracle + checkasm + microbench cannot fit.

### F3 - W9 candidate envelope is broader than its proof producers

The SPEC makes W7 select exactly one of `C4` or `C5` (`SPEC.md:481-482`) and W8
select exactly one of `C6` or `C7` (`SPEC.md:520-521`). W9 then says it may
consume a proven `C4`-`C9` primitive (`SPEC.md:562-563`) and the manifest repeats
`proven C4-C9` (`SPEC.md:171-173`).

That admits `C8-digit-number-proof` and `C9-whitespace-class-skip` into W9 even
though no W7/W8 proof wave can produce them. P3-C still has C8/C9 in the generic
primitive proof set (`p3c-falsifiability-gates.md:265-294`), and P3-A shortlists
C8 as candidate 8 (`p3a-candidate-shortlist.md:386-425`), but the final SPEC
contains no W7/W8 slot for a digit or whitespace proof. This is under-specified
and risks a production wave consuming a primitive without a same-cycle proof
producer.

Required fix: either restrict W9 to `C4`-`C7` proven by W7/W8, or add explicit
proof-only W8b/W8c waves for `C8` and `C9` with the same scalar/checkasm/
microbench/caller requirements before any W9 production wiring.

### F4 - W9 remains too broad for a plausible <=90-minute production wave

P2G allows one primitive family, one call-site proof at 90-260 proof LOC before
production (`p2g-candidate-ledger.md:104-116`). P3-B correctly says W9 may wire
only a W7/W8-proven primitive into one named current caller
(`p3b-wave-sequencing.md:61-64`). The final SPEC entry gate gives examples of
current callers (`SPEC.md:576-578`), and the exit gate requires same-commit
production consumption plus row floors and W10b maintain floors
(`SPEC.md:587-594`).

The remaining gap is that W9's owner paths and candidate text still cover "the
exact W7/W8 primitive owner paths", multiple bench/report/gate surfaces, and
direct or typed row movement (`SPEC.md:565-574`). There is no binding "exactly
one primitive, one existing caller, one row plane, at most N rows" cap in the
W9 section. In a 90-minute slice, production wiring plus scalar fallback,
differential parity, fresh same-run Criterion, W10b maintain checks, gate/report
updates, `RESULTS.md`, and `REDRESS.md` is only plausible if W9 is narrowed to
one caller and one plane.

Required fix: add to W9: "exactly one proven primitive, exactly one existing
production caller, exactly one consumer plane, and at most one row-moving target
set; split if gate/report updates exceed the LOC budget or if both direct and
typed rows would move."

### F5 - Micro-prove-first discipline is mostly correct and should be retained

The P2G ledger establishes micro-prove-first for all kernel/SIMD rows with
scalar oracle, checkasm target, host feature gate, representative corpus slices,
caller microbench, failure threshold, and same-wave consumer
(`p2g-candidate-ledger.md:27-31`). The SPEC non-negotiables repeat that no
kernel ships without scalar reference, parity, feature gate, representative
windows, microbench, and same-wave hot-path caller (`SPEC.md:138-140`).

W7 and W8 correctly close proof-only with no `RESULTS.md` movement and no
production behavior wiring (`SPEC.md:497-509`, `SPEC.md:537-550`). W9 correctly
requires the named production caller to consume the primitive in the same commit
(`SPEC.md:587-590`). These are acceptable; the fixes above should preserve this
proof-only / production split rather than merge W7-W9.

## Required Fixes

1. Add binding LOC/edit budgets to `SPEC.md` for every wave, not only in P3-F.
2. Normalize phase caps across P3-B, SPEC, and DISPATCH. State whether <=90 min
   means total wave time, redress time, or per-phase time.
3. Restrict W9 to `C4`-`C7` unless new proof-only waves are added for `C8` and
   `C9`.
4. Narrow W9 to exactly one proven primitive, one existing caller, one consumer
   plane, and one row-moving target set per dispatch.
5. Preserve the existing W7/W8 proof-only rule and W9 same-commit production
   consumer rule.

## Disposition

REVISE. The plan has the right architecture for micro-prove-first and avoids the
largest paper-close risk by splitting W7/W8 proof from W9 production. It does
not yet satisfy CH4 because the final SPEC omits LOC budgets, cap semantics
conflict across artifacts, and W9 is wider than its proof-producing waves.

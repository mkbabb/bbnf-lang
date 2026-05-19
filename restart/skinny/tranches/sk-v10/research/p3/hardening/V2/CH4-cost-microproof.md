# SK-V10 S-P3 V2 CH4 Cost / Micro-Proof

Verdict: ACCEPT.

Acceptance: 96%.

## Scope

Lens: CH4 cost and micro-proof adequacy for the S-P3 V2 folded contract. This
audit checks whether the V2 `SPEC.md` and `DISPATCH-PROMPT.md` fixed the V1
cost blockers: binding LOC budgets, normalized cap semantics, W9 narrowing to
one relevant C4-C7 primitive/caller/plane/target set, and threshold-bearing W7
/ W8 micro-proof gates.

Primary inputs:

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/V1/CH4-cost-microproof.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`

## Findings

### F1 - Binding LOC budgets are now present in SPEC and DISPATCH. ACCEPT.

V1 failed because final `SPEC.md` omitted the binding per-wave budget column that
existed only in draft artifacts. V2 fixes this in the authoritative manifest:
`SPEC.md:162-175` now gives every wave an explicit budget and redress cap. The
relevant cost rows are precise enough for dispatch control: W7 and W8 are each
`90-260 proof LOC`, and W9 is `220-420 source/bench/gate LOC`
(`SPEC.md:171-173`).

The dispatch prompt mirrors those same budget bands rather than relying on a
stale draft: `DISPATCH-PROMPT.md:44-57` repeats W7, W8, and W9 with the same LOC
and cap values. Planning is also required to name a falsifiability threshold, LOC
budget, risk class, revert protocol, same-wave consumer, scalar oracle, and
differential/checkasm requirement (`DISPATCH-PROMPT.md:82-86`).

Required fix: none.

### F2 - Redress cap semantics are normalized. ACCEPT.

V1 allowed incompatible readings of the 90-minute cap. V2 resolves that by
stating that the manifest cap is the redress execution cap, while research and
plan are separate dispatch-protocol caps of 30 minutes per agent and CHALLENGE is
60-90 minutes when required (`SPEC.md:197-202`).

The dispatch prompt matches this interpretation: research is capped at 30
minutes per agent (`DISPATCH-PROMPT.md:73-77`), plan at 30 minutes
(`DISPATCH-PROMPT.md:82-87`), and the redress section states that every dispatch
carries the manifest redress cap, with a 0.9x checkpoint and cap halt
(`DISPATCH-PROMPT.md:218-222`).

Required fix: none.

### F3 - W9 is narrowed to the relevant C4-C7 proof and one production slice. ACCEPT.

V1 allowed W9 to consume a broader C4-C9 envelope even though W7/W8 only proved
C4-C7. V2 fixes the candidate source: the manifest limits W9 to proven `C4`-`C7`
(`SPEC.md:173`), the manifest rules say W9 consumes exactly one relevant accepted
W7 or W8 proof and explicitly exclude `C8` and `C9` absent a future
SPEC/CHALLENGE amendment (`SPEC.md:188-191`), and the W9 section repeats that
only a relevant accepted W7 or W8 `C4`-`C7` primitive is eligible
(`SPEC.md:627-630`).

V2 also fixes the production envelope. W9 must name the exact primitive and
caller (`SPEC.md:643-646`), wire only that proven primitive into the named
existing caller (`SPEC.md:648-651`), and limit dispatch to exactly one proven
primitive, one existing production caller, one consumer plane, and one
row-moving target set (`SPEC.md:654-657`). `DISPATCH-PROMPT.md:59-64` and
`DISPATCH-PROMPT.md:161-164` mirror the same one-proof / one-caller / one-plane
restriction.

Required fix: none.

### F4 - W7/W8 now have threshold-bearing micro-proof exits. ACCEPT.

V1 needed W7 and W8 to close only on measured, threshold-clearing caller
microbench artifacts. V2 makes that binding in both proof waves. W7 CHALLENGE
must accept one string primitive family, cap, output plane, caller, scalar
oracle, representative slices, feature gate, and failure threshold
(`SPEC.md:544-546`). Its exit gate requires scalar/differential pass and a caller
microbench artifact that clears the predeclared threshold while recording the
observed value, threshold, run id, host triple, build flags, feature gate,
representative slices, sample count, scalar oracle identity, and differential
harness identity (`SPEC.md:557-566`). A miss records observed value versus
threshold in REDRESS (`SPEC.md:568-570`).

W8 carries the same threshold-bearing shape for escape/segment proof:
CHALLENGE accepts one primitive family, caller, scalar oracle, representative
slices, feature gate, and failure threshold (`SPEC.md:592-595`), and the exit
gate requires the threshold-clearing caller microbench artifact with the same
metadata (`SPEC.md:607-616`). Misses likewise record observed value versus
threshold (`SPEC.md:618-620`). The dispatch prompt mirrors the rule at
`DISPATCH-PROMPT.md:156-160`.

Required fix: none.

### F5 - Minor dispatch wording ambiguity remains, but it is not a CH4 blocker. ACCEPT WITH FIX.

`DISPATCH-PROMPT.md:107-110` says W7-W9 CHALLENGE must reject absent production
consumer. Read literally, that phrase can conflict with W7/W8 proof-only rules:
W7 must identify the current caller but not production-wire it (`SPEC.md:564-566`),
and W8 must not wire production caller behavior (`SPEC.md:597-605`). W9, not W7
or W8, requires same-commit production consumption (`SPEC.md:661-662`).

The governing SPEC sections are clear enough to prevent production wiring in
proof-only waves, so this does not reopen the V1 cost blocker. However, dispatch
text should be clarified to avoid agents interpreting "production consumer" as a
W7/W8 wiring requirement.

Required fix: change `DISPATCH-PROMPT.md:107-110` to say W7/W8 require an
identified existing caller and caller microbench, while W9 requires an absent
production consumer to be rejected.

## Disposition

ACCEPT at 96%. V2 fixes the V1 CH4 cost blockers in the binding contract: LOC
budgets are present in SPEC and DISPATCH, cap semantics are normalized around the
redress execution cap, W9 is narrowed to one relevant accepted C4-C7 proof and
one production slice, and W7/W8 now have threshold-bearing measured micro-proof
exit gates. The only required follow-up is a dispatch wording clarification so
W7/W8 proof-only waves cannot be misread as requiring production wiring.

# SK-V10 S-P3 V2 CH1 - Correctness

Date: 2026-05-19.
Scope: CH1 correctness audit of the V2 folded `SPEC.md`, `DISPATCH-PROMPT.md`,
P3-A through P3-F support artifacts, and the V1 hardening consolidation.

## Verdict

Verdict: ACCEPT.

Acceptance: 96%.

Required fixes: none for CH1 acceptance.

Residual cleanup: correct stale "V1" labels in support and telemetry prose so
readers do not confuse V1 support artifacts with the V2 folded authority.

## Findings

### 1. V1 CH1 blocker fixed: P3-C and P3-E wave numbering no longer governs dispatch

Status: ACCEPT.

The V1 consolidation named P3-C/P3-E wave-number drift as a CH1 blocker
(`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:15`,
`:56`-`:58`). V2 fixes the dispatch authority in the top-level manifest:
`SPEC.md` defines W0-W10 plus Close with W4 typed admission, W5 root proof, W6
root row admission, W7/W8 proof-only primitive waves, W9 production, and W10
direct residual work (`restart/skinny/tranches/sk-v10/SPEC.md:160`-`:175`).
`DISPATCH-PROMPT.md` mirrors the same topology
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:42`-`:57`).

The support artifacts now explicitly defer to the final numbering:
P3-C maps each final wave to its gate family
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:128`-`:148`),
and P3-E states that older compressed aliases are not dispatch identifiers
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:62`-`:68`).
P3-F remains a support draft and tells the integrated SPEC to preserve the
P3-B W0-W10 topology
(`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:168`-`:173`).

### 2. V1 CH1 blocker fixed: C8/C9 cannot feed W9

Status: ACCEPT.

The V2 SPEC now limits W9 to a relevant accepted W7 or W8 proof for C4-C7 and
explicitly bars C8 digit/number and C9 whitespace/class work from W9 without a
future SPEC/CHALLENGE amendment
(`restart/skinny/tranches/sk-v10/SPEC.md:186`-`:191`,
`:627`-`:646`). The dispatch prompt repeats the same rule
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:59`-`:64`).

P3 support artifacts agree: P3-A routes C8 out of the W0-W10 executable plan and
keeps C9 unshortlisted/maintain-only
(`restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md:107`-`:111`,
`:392`-`:395`, `:481`-`:490`), and P3-C says both C8 and C9 cannot feed W9
without future amendment
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:328`-`:338`).

### 3. V1 CH1 blocker fixed: W9 dependency is relevant-proof bound

Status: ACCEPT.

W9 no longer has ambiguous dependency on broad primitive research. The SPEC says
W9 consumes exactly one relevant accepted W7 or W8 proof for the exact primitive
and caller (`restart/skinny/tranches/sk-v10/SPEC.md:188`-`:189`,
`:643`-`:646`) and limits the dispatch to one primitive, one production caller,
one consumer plane, and one row-moving target set (`restart/skinny/tranches/sk-v10/SPEC.md:654`-`:657`).
The dispatch prompt mirrors this and adds the same split rule
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:161`-`:164`).

### 4. Numeric floors are present and consistently routed

Status: ACCEPT.

The V2 SPEC binds direct floors, direct guard maintain floors, typed maintain
floors, and the W10b maintain block in Section 0.2
(`restart/skinny/tranches/sk-v10/SPEC.md:67`-`:121`). The relevant gates consume
those floors: W2 direct reclamation (`restart/skinny/tranches/sk-v10/SPEC.md:344`-`:351`),
W4 typed admission (`restart/skinny/tranches/sk-v10/SPEC.md:423`-`:432`),
W6 typed row admission (`restart/skinny/tranches/sk-v10/SPEC.md:510`-`:519`),
W9 kernel production (`restart/skinny/tranches/sk-v10/SPEC.md:659`-`:668`),
and W10 direct residual work (`restart/skinny/tranches/sk-v10/SPEC.md:701`-`:709`).
`DISPATCH-PROMPT.md` correctly points agents back to SPEC Section 0.2 instead of
creating a competing floor table (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:145`-`:155`).

### 5. Gate names are coherent across SPEC and DISPATCH

Status: ACCEPT.

The gate names listed in `DISPATCH-PROMPT.md`
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:128`-`:143`) match the exit
gate names in `SPEC.md`: W0 (`:257`), W1 (`:299`), W2 (`:344`), W3 (`:383`),
W4 (`:423`), W5 (`:465`), W6 (`:510`), W7 (`:557`), W8 (`:607`), W9 (`:659`),
W10 (`:701`), and Close (`:735`). I found no CH1 gate-name mismatch.

### 6. Run-id claims are acceptable, with one stale-label cleanup

Status: ACCEPT with cleanup.

The final SPEC is explicit that the current opening authority remains the
W1-rendered SK-V9-open report unless W0 refreshes it, including the current run
id (`restart/skinny/tranches/sk-v10/SPEC.md:21`-`:23`, `:55`-`:59`). W0 requires
the run-id grammar to be gate-accepted and uniform
(`restart/skinny/tranches/sk-v10/SPEC.md:257`-`:263`). P3-D adds the missing
detail that any new `sk-v10-*` prefix requires a same-wave gate validator and
fixture update (`restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md:142`-`:154`).

Cleanup: `SPEC.md` still says "No new column is authorized by SK-V10 S-P3 V1"
at `restart/skinny/tranches/sk-v10/SPEC.md:793`, while the file header says V2
at `restart/skinny/tranches/sk-v10/SPEC.md:3`-`:5`. P3-A, P3-D, and P3-F also
retain V1/support-draft headers
(`restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md:1`-`:4`,
`restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md:1`-`:4`,
`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:1`-`:4`). This is
not a CH1 blocker because the top-level SPEC/DISPATCH contract is already V2 and
the stale references do not alter gates, floors, dependencies, or row movement.
It should still be cleaned up before final sign-off to reduce reader ambiguity.

## Required Fixes

None.

## Recommended Fixes

1. Change `SK-V10 S-P3 V1` to `SK-V10 S-P3 V2` in
   `restart/skinny/tranches/sk-v10/SPEC.md:793`.
2. Add a one-line note to P3-A, P3-D, and P3-F headers that these are V1 support
   artifacts folded by the V2 SPEC/DISPATCH, or update their cycle labels if the
   project wants all support files to carry V2 labels.

# SK-V10 S-P3 V3 CH1 - Correctness

Date: 2026-05-19.
Scope: CH1 confirmation audit of the post-V2 hygiene-fold `SPEC.md`,
`DISPATCH-PROMPT.md`, P3-A through P3-F support artifacts, and V1/V2
hardening records.

## Verdict

Verdict: ACCEPT.

Acceptance: 97%.

Required fixes: none.

## Findings

### 1. SPEC and DISPATCH remain coherent after the hygiene fold

Status: ACCEPT.

The top-level SPEC declares the active cycle as V2 challenge-fold
(`restart/skinny/tranches/sk-v10/SPEC.md:3`-`:8`) and the dispatch prompt binds
implementation agents to that SPEC rather than to support drafts
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:3`-`:10`). The V2
consolidation required a V3 confirmation pass after the hygiene fold
(`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:37`-`:42`);
this V3 check finds no post-fold CH1 contradiction between SPEC, DISPATCH, and
the P3 support cohort.

### 2. V1/V2 wave-numbering risk is closed

Status: ACCEPT.

V1 CH1 blocked on P3-C/P3-E wave-number drift
(`restart/skinny/tranches/sk-v10/research/p3/hardening/V1/CH1-correctness.md:31`-`:77`),
and the V1 consolidation required P3-B, P3-C, P3-E, P3-F, and P3-A alignment to
the final W0-W10 topology
(`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:56`-`:58`).
The binding SPEC manifest now defines W0-W10 plus Close in the final order
(`restart/skinny/tranches/sk-v10/SPEC.md:160`-`:175`), and DISPATCH mirrors the
same sequence (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:42`-`:57`).

The supporting artifacts no longer present stale compressed numbers as dispatch
authority: P3-C states it now uses the final SPEC W0-W10 numbering and maps each
final wave to the gate family
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:128`-`:148`);
P3-E states that older compressed draft aliases are not dispatch identifiers
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:62`-`:68`);
P3-F preserves the P3-B W0-W10 plus Close topology
(`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:168`-`:188`).

### 3. Gate names match across SPEC and DISPATCH

Status: ACCEPT.

DISPATCH lists the gate set as `G-W0-TELEMETRY-FREEZE`,
`G-W1-DIRECT-CONTRACT`, `G-W2-DIRECT-RECLAMATION`,
`G-W3-PARSE-FIREWALL`, `G-W4-INSTRUMENTS-TYPED`,
`G-W5-ROOT-TYPED-PROOF`, `G-W6-ROOT-TYPED-ROW`,
`G-W7-STRING-MICROPROOF`, `G-W8-ESCAPE-SEGMENT-MICROPROOF`,
`G-W9-KERNEL-PRODUCTION`, `G-W10-DIRECT-RESIDUAL`, and
`G-CLOSE-SK-V10`
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:130`-`:145`). The SPEC uses
those same exit-gate names at the corresponding wave sections:
W0 (`restart/skinny/tranches/sk-v10/SPEC.md:257`), W1 (`:299`), W2 (`:344`),
W3 (`:383`), W4 (`:423`), W5 (`:465`), W6 (`:510`), W7 (`:557`), W8 (`:607`),
W9 (`:659`), W10 (`:701`), and Close (`:735`). I found no CH1 gate-name drift.

### 4. C8/C9 remain excluded from W9

Status: ACCEPT.

The V1 CH1 blocker was that W9 could consume C8/C9 without final proof-wave
coverage (`restart/skinny/tranches/sk-v10/research/p3/hardening/V1/CH1-correctness.md:79`-`:101`).
The current SPEC now limits W9 to relevant accepted W7/W8 `C4`-`C7` primitives
(`restart/skinny/tranches/sk-v10/SPEC.md:627`-`:646`) and states that C8
digit/number and C9 whitespace/class work cannot feed W9 without a future
SPEC/CHALLENGE amendment (`restart/skinny/tranches/sk-v10/SPEC.md:186`-`:191`).
DISPATCH repeats the same exclusion
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:59`-`:64`).

P3 support agrees: P3-A routes C8 out of the executable W0-W10 plan and keeps
C9 unshortlisted/maintain-only
(`restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md:107`-`:111`,
`:392`-`:395`, `:481`-`:490`), and P3-C says both C8 and C9 cannot feed final
W9 without future amendment
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:328`-`:338`).

### 5. W9 dependency is relevant-proof bound

Status: ACCEPT.

V1 CH1 required replacing ambiguous `W7/W8` or `W7 + W8` dependency wording with
"the relevant accepted W7 or W8 proof for the exact primitive and caller"
(`restart/skinny/tranches/sk-v10/research/p3/hardening/V1/CH1-correctness.md:103`-`:119`).
The current SPEC manifest and rules use that relevant-proof dependency
(`restart/skinny/tranches/sk-v10/SPEC.md:172`-`:173`,
`:186`-`:191`), and the W9 entry gate requires the relevant accepted proof for
the exact primitive and caller
(`restart/skinny/tranches/sk-v10/SPEC.md:643`-`:646`). DISPATCH mirrors this and
limits W9 to exactly one proven primitive, existing production caller, consumer
plane, and target set
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:59`-`:64`,
`:163`-`:166`).

### 6. Numeric floors are present and consistently routed

Status: ACCEPT.

The SPEC binds direct row floors, direct guard maintain floors, typed maintain
floors, and the W10b maintain block in Section 0.2
(`restart/skinny/tranches/sk-v10/SPEC.md:67`-`:121`). The row-moving or
production gates consume those floors: W2 direct reclamation
(`restart/skinny/tranches/sk-v10/SPEC.md:334`-`:351`), W4 typed admission
(`restart/skinny/tranches/sk-v10/SPEC.md:411`-`:432`), W6 root typed row
admission (`restart/skinny/tranches/sk-v10/SPEC.md:501`-`:519`), W9 kernel
production (`restart/skinny/tranches/sk-v10/SPEC.md:659`-`:668`), and W10
direct residual work (`restart/skinny/tranches/sk-v10/SPEC.md:701`-`:709`).
DISPATCH intentionally points agents back to SPEC Section 0.2 for floor
authority instead of restating a competing table
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:147`-`:157`).

### 7. Run-id language is honest and gate-bound

Status: ACCEPT.

The SPEC says the opening authority remains the W1-rendered SK-V9-open report
unless W0 refreshes it, and records the current run id
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`
(`restart/skinny/tranches/sk-v10/SPEC.md:21`-`:23`, `:55`-`:59`). W0 requires a
gate-accepted uniform run-id grammar (`restart/skinny/tranches/sk-v10/SPEC.md:257`-`:263`).
P3-D supplies the matching run-id rule: any `sk-v10-*` prefix requires a
same-wave gate validator and fixture update, and mixed run ids reject
(`restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md:142`-`:154`).
DISPATCH also requires `gate-json` rejection of non-uniform run ids and stale or
mismatched strict evidence
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:203`-`:211`).

### 8. Hygiene-fold label cleanup is complete enough for CH1

Status: ACCEPT.

The V2 CH1 residual cleanup asked for stale authority labels to be corrected or
marked as support so readers do not confuse V1 support artifacts with V2 folded
authority (`restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH1-correctness.md:97`-`:118`).
The SPEC telemetry line now says no new column is authorized by SK-V10 S-P3 V2
(`restart/skinny/tranches/sk-v10/SPEC.md:793`). P3-A, P3-C, P3-D, and P3-F
headers now explicitly say they are V1 support folded by V2
(`restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md:1`-`:4`,
`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:1`-`:5`,
`restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md:1`-`:4`,
`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:1`-`:4`). P3-E's
header remains a P3-E cycle label rather than a V2 label
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:1`-`:7`),
but its per-wave section explicitly records the V2 CHALLENGE fold and final
SPEC numbering (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:62`-`:68`).
This is not a CH1 defect.

## Required Fixes

None.

## Acceptance Rationale

The post-V2 hygiene-fold contract preserves the V2 accepting posture and closes
the CH1-critical V1 defects: wave numbering is aligned, gate names are coherent,
C8/C9 cannot feed W9, W9 is relevant-proof bound, numeric floors are routed into
the row-moving gates, and run-id language is explicit and gate-bound. The
remaining risk is ordinary implementation discipline in later waves, not an
S-P3 CH1 correctness blocker.

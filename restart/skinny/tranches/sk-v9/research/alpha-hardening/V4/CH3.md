# SK-V9 Alpha Hardening V4 CH3 Regression Challenge

Date: 2026-05-18.
Lens: CH3 Regression.
Scope: unchanged SK-V9 Pass Alpha packet at commit `795bbbec`, plus V3 consolidated.

## Verdict

Overall disposition: ACCEPT.
Confidence: 97%.

The unchanged packet preserves the V3 CH3 regression accept state. I found no
drift in REDRESS 91/92/93 carry-forward, no REDRESS 73 helper-shape reopening,
no demotion of Alpha-C's historical pre-block ledger, and no SK-V9 implementation
dispatch before G-Alpha. This clears the >=95% confidence requirement for CH3
acceptance.

## Sources Read

- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CH3.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md`
- `skinny/REDRESS.md`

## Drift Checks

- `HEAD` is `795bbbecb6cdcbb73b0b55945a8999e4091f5c6f`.
- `git diff --name-only 795bbbec -- restart/skinny/tranches/sk-v9` returned no
  paths, so the packet under review is unchanged from the requested commit.
- `find restart/skinny/tranches/sk-v9 -name SPEC.md -print` returned no paths.
- `find restart/skinny/tranches/sk-v9 -name DISPATCH-PROMPT.md -print` returned
  no paths.

## Findings

### CH3-F1: V3 accept state carries forward unchanged

Disposition: ACCEPT.

V3 consolidated records a six-lens ACCEPT, minimum confidence 96%, no open
critical defects, and no orphan REVISE dispositions
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:10-22`).
The V3 CH3 row specifically accepted REDRESS 91/92/93, REDRESS 73, and Alpha-C
historical pre-block carry-forward with no fold required
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:16`).
V3 also states that V4 is the unchanged re-challenge needed for two clean cycles
before G-Alpha presentation
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:34-38`).

### CH3-F2: REDRESS 91 remains source/product-only, not measured-row admission

Disposition: ACCEPT.

The REDRESS ledger keeps REDRESS 91 partial, with Apache/CITM source/product
parity admitted, `canada/real_typed_struct` rejected on checksum mismatch,
Apache/CITM absent from measured `RESULTS.md` rows, and `skinny/RESULTS.md`
unchanged (`skinny/REDRESS.md:2620-2659`). Alpha-C carries the same boundary:
Apache/CITM are not measured rows, and the W0 run-id validator remains intact
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:59-93`).
The final synthesis repeats that Apache/CITM may become measured rows only with
fresh run-id/metadata evidence and that Canada is pre-blocked until full-fixture
checksum proof exists
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:81-85`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`).
Alpha-E also blocks source-only admission and validator weakening
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:52-116`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:128-135`).

### CH3-F3: REDRESS 92 stays proof-first and pre-implementation

Disposition: ACCEPT.

The REDRESS ledger rejects/routes REDRESS 92 before source redress, records the
scanner/tape event-model mismatch, and blocks sidecars, parser-owned structural
cursors/facts, `tape_vs_tape` production use, `UnionTape`, new `BackendShape`,
new BIR/directive, public substrate APIs, and Tier B work under the Tier A name
(`skinny/REDRESS.md:2661-2690`). Alpha-C preserves that disposition
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:95-131`).
Alpha-E frames the route as retained class/event grammar plus `ValueRef` proof,
with no row claim unless a same-wave generated retained Track 1 consumer lands
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:137-211`).
The synthesis repeats that no structural parse implementation starts before the
grammar and cursor proof are proven and accepted
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:86-87`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-156`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:315-316`).

### CH3-F4: REDRESS 93 and REDRESS 73 are not reopened under renamed direct work

Disposition: ACCEPT.

The REDRESS ledger keeps REDRESS 93 rejected/routed after Criterion falsified
the selected W4 rows and blocks scalar-parent folding under another name without
a W4/V9-aware checked gate, full-table maintain measurement, and independent
Track 2 digest-arithmetic backstop (`skinny/REDRESS.md:2692-2729`). Alpha-C
pre-blocks scalar-parent folding, digest-only product proof, and Track 2
admission without those conditions
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:133-164`).
Alpha-C's prior ledger also keeps REDRESS 73 blocked as generated retained array
helper-shape transfer to hand Track 2
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:221-228`).
Alpha-E repeats both restrictions for direct/control work and rejects scalar
parent folding plus REDRESS 73 helper-shape transfer as shortlist routes
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:315-325`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:497-503`).
The synthesis and handoff keep direct digest rows guard/control-only and require
direct hand-parser profiling before any REDRESS 73-shaped route can reopen
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-181`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:317-323`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:90-94`).

### CH3-F5: Alpha-C historical pre-blocks remain G-Alpha-facing

Disposition: ACCEPT.

Alpha-C carries the historical pre-block clusters still active for SK-V9:
REDRESS 16/17/18/25, 28+33 and 72/83, 50-55, 60-72, 73, 80, 82, 84/65,
88/89/90, 36-38 and 85-86 plus W5, SC-6-L1-R1/substrate ceiling, and
strictness/telemetry
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:215-234`).
Alpha-C makes that ledger binding and additive: renamed rejected routes stay
rejected unless a later plan proves a materially different shape with fresh
evidence before redress
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:266-276`).
The synthesis and handoff bind the full Alpha-C pre-block ledger by reference,
so it is visible on G-Alpha-facing surfaces rather than confined to research
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-312`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:101-105`).
Alpha-F also names pre-block alignment to REDRESS 91/92/93, REDRESS 73, and the
Alpha-C historical ledger as part of the materialized contract
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:48-62`).

### CH3-F6: No admitted SK-V8 row is silently regressed

Disposition: ACCEPT.

The synthesis records the current measured state as 16 `parse_only` `S / NO-GO`,
one `parse_only` `L / NO-GO`, three `direct_to_struct A / GO`, 14
`direct_to_struct N-direct / NO-GO`, and four measured `real_typed_struct A / GO`
rows
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:21-35`).
It then requires all current GO rows to maintain GO unless a selected-row gate
sets stricter floors, and names the maintain targets for the seven current GO
rows
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:200-210`).
Alpha-E adds current real-typed GO maintain gates and blocks checked-report
failure from becoming a `RESULTS.md` edit
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:104-116`).

### CH3-F7: No SK-V9 implementation dispatch exists before G-Alpha

Disposition: ACCEPT.

The synthesis states that V9 implementation is not dispatched, no `SPEC.md` or
`DISPATCH-PROMPT.md` is created, and downstream S-P3 owns future wave planning
only after `G-Alpha closed`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-335`).
The handoff repeats that no SK-V9 `SPEC.md` or `DISPATCH-PROMPT.md` exists and
that no implementation wave dispatches before downstream planning converges
(`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`).
Alpha-F gives the same boundary: after challenge convergence, present G-Alpha;
only after `G-Alpha closed` can skinny passes begin
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:11-13`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:103-105`).

## Required Folds

None from CH3.

## Blockers To G-Alpha

None from the CH3 regression lane. G-Alpha remains procedurally dependent on the
full V4 unchanged re-challenge converging cleanly and the user-controlled
`G-Alpha closed` boundary; CH3 does not add a required fold or blocker.

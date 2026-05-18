# SK-V9 Alpha Hardening V3 CH3 Regression Challenge

Date: 2026-05-18.
Lens: CH3 Regression.
Scope: corrected SK-V9 Pass Alpha packet at commit `32369fe8`.

## Verdict

Overall disposition: ACCEPT.
Confidence: 97%.

The corrected packet preserves the V2 CH3 accept state. The only V2 fold was the
complete-table citation repair from `skinny/RESULTS.md:3-40` to
`skinny/RESULTS.md:3-42`; that correction does not reopen REDRESS routes, demote
Alpha-C's pre-block ledger, or change the SK-V9 pre-dispatch boundary
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:24-42`).
I found no proposal that reopens REDRESS 91, REDRESS 92, REDRESS 93, REDRESS 73,
or the historical Alpha-C prior pre-block clusters, and no admitted SK-V8 row is
silently regressed by the SK-V9 Alpha goalset.

## Sources Read

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Findings

### CH3-F1: V2 citation hardening did not regress the V1 CH3 folds

Disposition: ACCEPT.

V1 required three regression folds: expose Alpha-C on G-Alpha-facing surfaces,
make Alpha-C's historical pre-block ledger binding by reference, and carry
REDRESS 73 into Alpha-C, Alpha-E, `SYNTHESIS.md`, `HANDOFF.md`, and Alpha-F
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md:62-68`).
V2 accepted those folds and required only the complete-table citation repair
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:14-27`).
The corrected packet still lists Alpha-C in the authority/read-first surfaces
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:11-19`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:10-20`), keeps Alpha-C's full
prior-preblock ledger binding by reference
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-312`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:101-105`), and preserves REDRESS 73
across the retained/control boundaries
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:221-228`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:225-234`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:315-325`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:320-323`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:93-94`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:61-62`).

### CH3-F2: REDRESS 91 remains a source/product boundary, not measured-row admission

Disposition: ACCEPT.

REDRESS 91 admits only the Apache/CITM typed source slice; it states those rows
are not measured W0 `skinny/RESULTS.md` rows, rejects
`canada/real_typed_struct` on full-fixture checksum mismatch, and leaves
`skinny/RESULTS.md` unchanged (`skinny/REDRESS.md:2620-2659`). The Alpha packet
keeps that boundary: Alpha-A records 38 measured rows and four measured
`real_typed_struct A / GO` rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:58-73`),
Alpha-E requires fresh run-id/metadata, checksum parity, selected comparator
evidence, current typed GO maintain floors, and no source-only admission
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:52-116`),
and `SYNTHESIS.md` keeps Apache/CITM source parity separate from fresh measured
row admission while pre-blocking Canada until full-fixture parity exists
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`).

### CH3-F3: REDRESS 92 is not reopened as structural implementation

Disposition: ACCEPT.

REDRESS 92 rejects W3 before source redress because scanner structural positions
and retained tape events are not isomorphic, and it blocks sidecars,
parser-owned cursors/facts, `tape_vs_tape` as production consumer, `UnionTape`,
new `BackendShape`, new BIR/directive, public substrate APIs, and Tier B work
under the Tier A name (`skinny/REDRESS.md:2661-2690`). Alpha-C preserves the
route as rejected/routed and names retained class/event grammar plus `ValueRef`
proof as the precursor
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:95-131`).
Alpha-E frames the route as proof-first and forbids row claims without same-wave
generated retained Track 1 consumption
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:137-211`).
`SYNTHESIS.md` repeats that structural parse implementation cannot start before
the grammar and cursor proof are accepted
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:86-87`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-156`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:315-316`).

### CH3-F4: REDRESS 93 scalar-parent folding remains rejected

Disposition: ACCEPT.

REDRESS 93 rejects the W4 hand Track 2 scalar-parent fold after Criterion
falsified the selected rows: Apache cleared, random missed, and numbers
regressed by +6.3287% Track 2 time. It requires any future route to supply a
W4/V9-aware checked gate, full-table maintain measurement, and an independent
Track 2 digest-arithmetic backstop (`skinny/REDRESS.md:2692-2729`). Alpha-C
carries scalar-parent folding, digest-only local arithmetic, and Track 2
admission without full-table maintain as pre-blocked
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:133-164`).
Alpha-E requires selected direct rows to clear floors without scalar-parent
folding and explicitly blocks REDRESS 93 under another name
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:236-325`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:497-503`).
The final contract keeps direct digest rows guard/control-only until a direct
output contract or control-path tranche exists
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-181`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:183-198`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:317-319`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:90-92`).

### CH3-F5: Historical Alpha-C prior pre-blocks are G-Alpha-facing

Disposition: ACCEPT.

PASS-ALPHA requires CH3 to cross-check the shortlist against REDRESS entries
and verify that Alpha-C identified the pre-block list
(`restart/prompts/pass-contracts/PASS-ALPHA.md:33-49`). Alpha-C carries the
historical clusters that matter for SK-V9: REDRESS 16/17/18/25, 28+33 and
72/83, 50-55, 60-72, 73, 80, 82, 84/65, 88/89/90, 36-38 and 85-86 plus W5,
SC-6-L1-R1/substrate ceiling, and strictness/telemetry
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:215-234`).
Alpha-C makes the ledger additive and says renamed rejected routes remain
rejected unless the new plan explains why the shape is materially different and
supplies fresh evidence before redress
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:266-276`).
Because `SYNTHESIS.md` and `HANDOFF.md` make that ledger binding
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-312`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:101-105`), the pre-blocks are visible
at G-Alpha rather than confined to research.

### CH3-F6: Current admitted rows retain no-regression guards

Disposition: ACCEPT.

CH3 must ensure no admitted route is silently regressed
(`restart/prompts/ORCHESTRATOR.md:81-88`). The current measured table has seven
`A / GO` rows, all under `Strictness=deferred` with view-boundary validation
(`skinny/RESULTS.md:3-42`). `SYNTHESIS.md` requires current GO rows to maintain
GO unless a selected-row gate sets stricter floors, then names maintain floors
for `twitter/real_typed_struct`, `citm_catalog/direct_to_struct`,
`update_center/real_typed_struct`, `mesh/real_typed_struct`,
`marine_ik/direct_to_struct`, `marine_ik/real_typed_struct`, and
`unicode_basic/direct_to_struct` (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:200-210`).
Alpha-E adds the same typed-maintain guard for current real-typed rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:104-108`).

### CH3-F7: No SK-V9 dispatch surface reopens a blocked route before G-Alpha

Disposition: ACCEPT.

The packet remains pre-dispatch: `SYNTHESIS.md` says V9 implementation is not
dispatched, no `SPEC.md` or `DISPATCH-PROMPT.md` is created, and Section 4.4
wave planning is downstream after G-Alpha
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-335`). `HANDOFF.md` repeats that
no `SPEC.md` or `DISPATCH-PROMPT.md` exists and no implementation wave dispatches
before downstream planning converges
(`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`). That boundary matters for
CH3 because the accepted state is a regression-safe Alpha contract, not an
implementation authorization.

## Required Folds

None from CH3.

## Blockers To G-Alpha

None from the CH3 regression lane. G-Alpha still depends on full V3 Alpha
challenge convergence and the user-controlled sign-off boundary
(`restart/prompts/ORCHESTRATOR.md:118-123`,
`restart/prompts/ORCHESTRATOR.md:167-172`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182`).

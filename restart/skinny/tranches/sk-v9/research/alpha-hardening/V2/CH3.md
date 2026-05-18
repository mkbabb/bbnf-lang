# SK-V9 Alpha Hardening V2 CH3 Regression Challenge

Date: 2026-05-18.
Lens: CH3 Regression.
Scope: folded SK-V9 Pass Alpha packet at commit `e3ebe0b4`.

## Verdict

Overall disposition: ACCEPT.
Confidence: 96%.

The folded packet no longer carries the V1 CH3 regression defect. Alpha-C is now
a first-class authority/read-first source, its historical pre-block ledger is
binding in the G-Alpha-facing contract surfaces, and REDRESS 73 is explicitly
carried into the retained/control-path boundaries. I found no proposal that
reopens REDRESS 91, 92, 93, REDRESS 73, or the historical Alpha-C prior
pre-block clusters, and no admitted SK-V8 row is silently regressed by the
SK-V9 Alpha goalset.

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
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Findings

### CH3-F1: V1 regression folds are present in the folded packet

Disposition: ACCEPT.

V1 required Alpha-C to become visible on the final contract surfaces, required
Alpha-C's prior pre-block ledger to become binding by reference, and required
REDRESS 73 to be carried through Alpha-C, Alpha-E, `SYNTHESIS.md`, `HANDOFF.md`,
and Alpha-F (`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md:62-68`).
The folded packet at `e3ebe0b4` does that:

- `SYNTHESIS.md` lists Alpha-C in the authority set
  (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:11-19`), and `HANDOFF.md` lists
  Alpha-C in the read-first order
  (`restart/skinny/tranches/sk-v9/HANDOFF.md:10-20`).
- `SYNTHESIS.md` makes the full Alpha-C prior pre-block ledger binding by
  reference and requires REDRESS citation plus changed-shape proof before
  implementation planning
  (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-312`).
  `HANDOFF.md` carries the same binding
  (`restart/skinny/tranches/sk-v9/HANDOFF.md:101-105`).
- REDRESS 73 is present in Alpha-C's prior-preblock table
  (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:221-228`),
  retained/control Alpha-E notes
  (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:225-234`,
  `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:315-325`),
  rejected-as-shortlist notes
  (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:495-503`),
  `SYNTHESIS.md`
  (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:320-323`), `HANDOFF.md`
  (`restart/skinny/tranches/sk-v9/HANDOFF.md:93-94`), and Alpha-F
  (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:61-62`,
  `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:78-84`,
  `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:87-101`).

### CH3-F2: REDRESS 91 is preserved as a typed-row boundary

Disposition: ACCEPT.

REDRESS 91 admits only the W2 source/product slice for
`apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct`; those
rows are not measured W0 `RESULTS.md` rows, and `canada/real_typed_struct`
remains rejected on full-fixture DirectBuild-vs-serde checksum mismatch
(`skinny/REDRESS.md:2620-2659`). The Alpha packet preserves this boundary:
Alpha-A records 38 measured rows and only four measured real-typed `A / GO`
rows (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:58-73`),
Alpha-E requires fresh run-id/metadata, full checksum parity, same-run comparator
evidence, and no source-only admission
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:52-116`),
and the final contract keeps Apache/CITM source parity separate from measured
row admission while pre-blocking Canada until fresh full-fixture parity exists
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:45-49`).

### CH3-F3: REDRESS 92 is not reopened as structural implementation

Disposition: ACCEPT.

REDRESS 92 rejects/routes W3 before source redress because scanner structural
positions and retained tape events are not isomorphic, and it blocks sidecars,
parser-owned cursors/facts, `tape_vs_tape` as production consumer, `UnionTape`,
new `BackendShape`, new BIR/directive, public substrate APIs, and Tier B work
under the Tier A name (`skinny/REDRESS.md:2661-2690`). Alpha-C keeps the route
as rejected/routed and names the retained class/event grammar plus `ValueRef`
proof as the only possible precursor
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:95-131`).
Alpha-E frames the candidate as proof-first, forbids second tapes/sidecars/new
substrate surfaces, and bars row claims without same-wave generated retained
Track 1 consumption
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:137-211`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:225-234`).
`SYNTHESIS.md` repeats that no structural parse implementation can start before
the grammar and cursor proof are accepted
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:86-87`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-156`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:315-316`).

### CH3-F4: REDRESS 93 scalar-parent folding remains rejected

Disposition: ACCEPT.

REDRESS 93 rejected the W4 hand Track 2 scalar-parent fold after binding native
Criterion falsified selected rows: Apache cleared, random missed, and numbers
regressed by +6.3287% Track 2 time; any future route needs a W4/V9 checked gate,
full-table maintain, and independent Track 2 digest-arithmetic backstop
(`skinny/REDRESS.md:2692-2729`). Alpha-C carries scalar parent folding,
digest-only local arithmetic, and Track 2 admission without full-table maintain
as pre-blocked
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:133-164`).
Alpha-E requires selected direct rows to clear floors without scalar-parent
folding and explicitly blocks REDRESS 93 under another name
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:236-325`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:497-503`).
The final contract preserves the direct plane as guard/control until a direct
output contract or control-path tranche exists
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-181`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:183-198`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:317-319`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:90-92`).

### CH3-F5: Historical Alpha-C prior pre-blocks are carried forward

Disposition: ACCEPT.

PASS-ALPHA requires CH3 to cross-check the shortlist against REDRESS entries
1-N and verify that Alpha-C identified the pre-block list
(`restart/prompts/pass-contracts/PASS-ALPHA.md:33-49`). Alpha-C now carries the
historical clusters that matter for SK-V9: REDRESS 16/17/18/25, 28+33 and
72/83, 50-55, 60-72, 73, 80, 82, 84/65, 88/89/90, 36-38 and 85-86 plus W5,
SC-6-L1-R1/substrate ceiling, and strictness/telemetry
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:215-234`).
Its guidance makes the ledger additive and says renamed rejected routes remain
rejected unless the new plan explains why the shape is materially different and
supplies fresh evidence before redress
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:266-276`).
Because `SYNTHESIS.md` and `HANDOFF.md` make that ledger binding
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-312`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:101-105`), the historical
pre-blocks are now G-Alpha-facing rather than buried in research.

### CH3-F6: Current admitted rows are protected from silent regression

Disposition: ACCEPT.

CH3 must also ensure no admitted row is silently regressed
(`restart/prompts/ORCHESTRATOR.md:81-88`). The current W0-rendered result table
has seven `A / GO` rows and all rows remain `Strictness=deferred` with
view-boundary validation (`skinny/RESULTS.md:3-42`). `SYNTHESIS.md` requires
current GO rows to maintain GO unless a selected-row gate sets stricter floors,
then names maintain floors for `twitter/real_typed_struct`,
`citm_catalog/direct_to_struct`, `update_center/real_typed_struct`,
`mesh/real_typed_struct`, `marine_ik/direct_to_struct`,
`marine_ik/real_typed_struct`, and `unicode_basic/direct_to_struct`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:200-210`). Alpha-E adds the same
typed maintain guard for current real-typed rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:104-108`).

## Required Folds

None from CH3.

## Blockers To G-Alpha

None from the CH3 regression lane. G-Alpha still depends on the full V2 Alpha
challenge satisfying the orchestrator/pass convergence requirements and the
user-controlled G-Alpha boundary
(`restart/prompts/ORCHESTRATOR.md:104-123`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182`).

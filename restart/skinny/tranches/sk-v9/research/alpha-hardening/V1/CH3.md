# SK-V9 Alpha Hardening V1 CH3 Regression Challenge

Date: 2026-05-18.
Lens: CH3 Regression.
Scope: SK-V9 Pass Alpha draft artifacts.

## Verdict

Overall disposition: REVISE.
Confidence: 92%.

The SK-V9 Alpha packet is disciplined on the three new SK-V8 residual routes.
Apache/CITM typed rows are fenced behind fresh measured row-table evidence,
REDRESS 91 keeps `canada/real_typed_struct` blocked until full-fixture parity is
repaired, REDRESS 92 is reframed as a retained class/event grammar plus
`ValueRef` proof before implementation, and REDRESS 93 scalar-parent folding is
not shortlisted as a behavior route. The regression blocker is narrower:
the G-Alpha-facing contract surfaces do not yet carry the full historical
pre-block ledger that Alpha-C already assembled, and that gap leaves room for
renamed control/string/sidecar routes to re-enter during S-P3.

## Sources Read

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`

## Findings

### CH3-F1: Final contract surfaces do not carry Alpha-C's full pre-block ledger

Disposition: REVISE.

PASS-ALPHA requires Alpha-C to identify routes that should pre-block SK-V9 and
Alpha-F to include the pre-blocked routes from Alpha-C in the contract draft
(`restart/prompts/pass-contracts/PASS-ALPHA.md:24-27`). The CH3 lens explicitly
checks whether Alpha-C identified the pre-block list and whether the shortlist
re-opens REDRESS routes (`restart/prompts/pass-contracts/PASS-ALPHA.md:41-42`).

Alpha-C does assemble the historical ledger: REDRESS 16/17/18/25, 28+33 and
72/83, 50-55, 60-72, 80, 82, 84/65, 88/89/90, 36-38 plus 85-86 and W5, the
SC-6-L1-R1 substrate ceiling, and strictness/telemetry blocks are all named as
active for SK-V9 (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:213-231`).
It also states that renamed rejected routes remain rejected unless the new plan
explains the difference and supplies fresh evidence before redress
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:263-273`).

The final G-Alpha-facing surfaces compress that ledger too far. SYNTHESIS names
REDRESS 91, 92, and 93 plus several global classes
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-259`), and HANDOFF repeats the
same reduced list (`restart/skinny/tranches/sk-v9/HANDOFF.md:64-82`). Neither
surface directly points the next-pass reader to Alpha-C's prior-preblock table.
HANDOFF's read-first list also names Alpha-F but not Alpha-C
(`restart/skinny/tranches/sk-v9/HANDOFF.md:10-19`). That is a regression risk
because downstream S-P3 is explicitly responsible for producing the future
pre-blocked-route ledger before waves dispatch
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:58-63`).

Required fold:

- Add `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
  to `SYNTHESIS.md` authority and `HANDOFF.md` read-first surfaces.
- In `SYNTHESIS.md` and `HANDOFF.md`, make Alpha-C's prior-preblock table
  binding by reference, or copy its REDRESS cluster list into the Pre-Blocked
  Routes section.
- State that any candidate touching a rejected ownership boundary must cite the
  REDRESS item and pass a changed-shape proof before implementation planning.

### CH3-F2: REDRESS 73 is missing from the SK-V9 pre-block carry-forward

Disposition: REVISE.

REDRESS 73 rejected the retained Track 2 array next-byte dispatch parity repair:
it proved that generated retained helper shape does not transfer monotonically to
the hand comparator, and future Track 2 work must profile the hand parser's code
layout directly (`skinny/REDRESS.md:2061-2088`). This matters for SK-V9 because
the retained class/event grammar candidate and the direct output/control-path
candidate both operate near parser continuation/control boundaries
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:116-187`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:209-272`).

Alpha-C's historical table covers REDRESS 60-72 and REDRESS 84/65, but it does
not explicitly include REDRESS 73
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:224-227`).
Alpha-E blocks many direct/materializer routes, including REDRESS 54, 55, 66-69,
72, 80, 84, and 93, but it likewise omits REDRESS 73 from the direct/control
candidate's pre-block notes
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:284-291`).

Required fold:

- Add REDRESS 73 to Alpha-C's historical pre-block ledger.
- Add REDRESS 73 to the retained/control candidate notes where helper-shape
  transfer from generated retained parsing to Track 2 or control-path work could
  otherwise re-enter under a neutral name.
- Include the same REDRESS 73 block in the final SYNTHESIS/HANDOFF pre-block
  surface carried to G-Alpha.

### CH3-F3: REDRESS 91 boundaries are preserved

Disposition: ACCEPT.

The measured authority remains the W0-rendered 38-row table, with only four
measured `real_typed_struct A / GO` rows; W2 Apache/CITM are source/product
parity only (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:58-73`).
REDRESS 91 itself says Apache/CITM are not measured rows, rejects
`canada/real_typed_struct` after a full-fixture checksum mismatch, and leaves
`skinny/RESULTS.md` unchanged (`skinny/REDRESS.md:2620-2659`).

Alpha-E's typed row-table candidate requires fresh same-run metadata, checksum
parity, selected comparator evidence, current typed GO maintain floors, and no
source-only admission (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:75-95`).
SYNTHESIS and HANDOFF both preserve the same boundary
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:161-168`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:44-46`). No revised route is needed
for REDRESS 91.

### CH3-F4: REDRESS 92 is not reintroduced as a storage-swap implementation

Disposition: ACCEPT.

REDRESS 92 rejects SK-V8 W3 before source redress because scanner structural
positions and retained tape events are not isomorphic; it forbids sidecar
producers, parser-owned cursors/facts, `tape_vs_tape` as production consumer,
`UnionTape`, new `BackendShape`, BIR variants, directives, public substrate APIs,
and Tier B string-boundary work under the Tier A name (`skinny/REDRESS.md:2661-2690`).

Alpha-E reframes the route as a proof candidate: define the retained event
grammar, prove the `ValueRef` contract, forbid second tapes/sidecars/new
substrate surfaces, and make row movement impossible without a generated retained
Track 1 consumer (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:156-187`).
SYNTHESIS also blocks structural-heavy implementation before the grammar/cursor
proof (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:76-84`). No REDRESS 92 route
is reopened under a new name in the Alpha packet.

### CH3-F5: REDRESS 93 scalar-parent folding remains rejected

Disposition: ACCEPT.

REDRESS 93 rejects the hand Track 2 scalar-parent fold after Criterion
falsified the selected rows: Apache cleared, random still missed, and numbers
regressed by +6.3287% Track 2 time (`skinny/REDRESS.md:2692-2729`). Alpha-C
classifies the route as rejected/routed and names scalar parent folding,
digest-only local arithmetic, and Track 2 admission without full-table maintain
as pre-blocked (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:131-162`).

Alpha-E's direct candidate explicitly requires selected rows to clear direct
floors without scalar-parent folding and says W4 scalar-parent folding is not
shortlisted (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:260-271`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:445-450`).
SYNTHESIS and HANDOFF preserve the renamed parent-digest block
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:251-253`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:75-77`). No REDRESS 93 route is
reintroduced.

### CH3-F6: Comparator sidecar manifest is not a parser sidecar, but the wording must stay narrow

Disposition: ACCEPT with carry-forward guard.

Alpha-B correctly demotes historical C++ sidecars to planning signals and
requires same-run, matching-plane telemetry before strict admission
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:116-143`).
Alpha-E's comparator/sidecar manifest candidate is gate/report telemetry only:
`gate-json` consumes the manifest, DOM sidecars cannot admit digest or typed
rows, permissive comparators remain flaw probes, and parser/generated throughput
must not drift without a separate behavior wave
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:327-354`).

This does not reopen the parser-sidecar/substrate routes forbidden by REDRESS 92
and Alpha-C (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:109-129`).
The carry-forward guard is terminology: the final fold should say "comparator
sidecar manifest" when discussing this candidate, so future agents do not confuse
telemetry sidecars with parser-sidecar substrate.

## Required Folds

1. Add Alpha-C to the SK-V9 SYNTHESIS authority list and HANDOFF read-first list.
2. Make the Alpha-C historical pre-block table binding in SYNTHESIS/HANDOFF, either
   by direct reference or by copying the REDRESS cluster list.
3. Add REDRESS 73 to Alpha-C, Alpha-E retained/control notes, SYNTHESIS, and
   HANDOFF as a standing pre-block against helper-shape transfer under a new name.
4. Preserve the existing REDRESS 91/92/93 boundary language; no fold is required
   for those three beyond the broader ledger carry-forward.
5. Keep comparator sidecar manifest wording explicitly telemetry-only and distinct
   from parser-sidecar substrate.

## Blockers To G-Alpha

G-Alpha should not be presented until CH3-F1 and CH3-F2 are folded. The packet can
then converge from CH3's lane without rejecting the SK-V9 Alpha goalset: the
91/92/93 residual routes are regression-safe once the historical pre-block ledger
is made visible on the final contract surfaces.

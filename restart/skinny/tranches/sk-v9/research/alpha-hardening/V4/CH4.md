# SK-V9 Alpha Hardening V4 CH4 - Cost

Verdict: ACCEPT.
Confidence: 96%.

## Scope

CH4 re-reviewed the unchanged SK-V9 Alpha packet at commit `795bbbec`
(`docs(sk-v9-alpha): record V3 accept convergence cycle`) plus the V3
consolidated disposition. Before this CH4 file was created, the worktree was
clean and `git diff --name-status 795bbbec -- restart/skinny/tranches/sk-v9`
returned no packet paths, so this is an unchanged re-challenge rather than a
fold review.

## Findings

### F1 - ACCEPT - V3 cost acceptance remains applicable

V3 consolidated recorded CH4 as ACCEPT at 96% confidence with no fold required
for candidate status, LOC budgets, `<=90 min` hard caps, same-wave consumers,
and proof-only retained routing
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:12-19`).
It also states that V3 was the first clean ACCEPT cycle and requires a V4
unchanged re-challenge before G-Alpha presentation
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:34-38`).
The unchanged packet does not reopen the CH4 lane.

### F2 - ACCEPT - Cost, LOC budgets, status, and hard caps are explicit

PASS-ALPHA requires Alpha-E to shortlist no more than five candidate
interventions and to carry file paths, scalar/checkasm status, same-wave
consumer, falsifiability gate, LOC budget, and risk classification
(`restart/prompts/pass-contracts/PASS-ALPHA.md:20-29`). The CH4 lens itself
checks LOC budget, risk class, wave alignment, hard cap, and same-wave consumer
(`restart/prompts/ORCHESTRATOR.md:81-88`).

The packet binds exactly five Alpha-E entries in the Alpha scope and cost
matrix: three behavior/proof routes plus two gate prerequisites, each with
G-Alpha status, LOC budget, risk, downstream alignment, same-wave consumer,
hard cap, and expected row effect
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:106-120`). `HANDOFF.md` mirrors the
same LOC budgets and `<=90 min implementation/redress` caps for later S-P3
planning (`restart/skinny/tranches/sk-v9/HANDOFF.md:57-65`). Alpha-F records
that the materialized contract carries the Alpha cost matrix and that the two
extra Alpha-E entries are gate-only prerequisites, not row movers
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:48-60`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:72-85`).

### F3 - ACCEPT - Same-wave consumers are present and gate-consumed

The matrix names the same-wave consumer for all five entries: `gate-json` for
typed row metadata, `ValueRef` cursor proof for retained grammar, the
gate/report row classifier for direct contract/control, `gate-json` for sidecar
manifest validation, and `gate-json` for the SK-V9-open manifest
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-117`). Alpha-E gives matching
consumer plans for typed row admission, retained proof, direct contract/control,
sidecar manifest, and telemetry refresh
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:83-90`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:177-184`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:270-276`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:363-372`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:447-453`).

The individual Alpha-E cost slices remain bounded: 300 LOC for Apache/CITM
typed row admission, 450 LOC for retained proof, 600 LOC for direct
contract/control, 500 LOC for sidecar manifest, and 450 LOC for SK-V9-open
telemetry refresh
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:117-120`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:213-217`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:304-307`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:396-398`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:477-479`).

### F4 - ACCEPT - Selected, proof-only, and gate-only statuses are not drifting

The packet separates three W6 behavior candidates from two non-behavior
gate-only prerequisites; the gate-only entries cannot produce parser data,
retained tape data, row output, substrate, strict admission, throughput
movement, or Apache/CITM measured rows by themselves
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:37-52`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:39-52`). Rejected-as-shortlist routes
remain excluded: storage-only W3 production, W4 scalar-parent folding, PMULL,
CTZ, bulk bitmap, tiny-string, Unicode escape, REDRESS 73 helper-shape transfer,
cap-16 direct, object-pair value-byte, and Pass Omega residuals
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:495-506`).

The retained class/event route remains proof-only at Alpha depth. `SYNTHESIS.md`
classifies it as a proof precursor and bars `RESULTS.md` row movement unless a
later capped generated retained Track 1 consumer lands in the same accepted wave
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-115`). The parse-row goalset
repeats that `parse_only` rows remain non-admission unless future S-P3 first
defines a capped implementation wave with same-wave generated retained Track 1
consumption, output-plane validation, strict validation posture, and challenge
acceptance (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-156`). Alpha-E's
retained proof lane likewise says the `ValueRef` proof consumes the grammar and
that proof-only artifacts do not create GO/SOTA rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:177-184`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:201-211`).

### F5 - ACCEPT - No deferral or SK-V9 implementation dispatch is introduced

The hard cap is fail-closed: a later S-P3 plan that exceeds either its LOC
budget or the `<=90 minute` implementation/redress cap returns REVISE before
dispatch (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:119-120`). This matches
the orchestrator's no-deferral and hard-cap discipline
(`restart/prompts/ORCHESTRATOR.md:197-227`).

The packet stays Alpha-depth only. `SYNTHESIS.md` says V9 implementation is not
dispatched, Alpha does not create `SPEC.md` or `DISPATCH-PROMPT.md`, G-Alpha
must be presented and closed before skinny passes begin, and downstream S-P3
owns any future wave plan
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`;
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`;
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-335`). `HANDOFF.md` repeats the
same pre-dispatch boundary and states that no implementation wave dispatches
before the downstream plan converges
(`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`). Alpha-E and Alpha-F also
state that they dispatch no SK-V9 implementation waves
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:5-7`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:11-13`).
The local SK-V9 tree contains no `SPEC.md` or `DISPATCH-PROMPT.md`.

### F6 - ACCEPT - G-Alpha posture is preserved

PASS-ALPHA requires G-Alpha presentation only after CHALLENGE convergence, with
the summary carrying rows targeted, interventions, LOC budget, hard caps, and
pre-blocked routes
(`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182`). The orchestrator
requires two consecutive >=95% ACCEPT cycles with zero open critical defects and
no orphan unresolved REVISE, and makes G-Alpha mandatory before SK-V9 dispatch
(`restart/prompts/ORCHESTRATOR.md:118-123`;
`restart/prompts/ORCHESTRATOR.md:159-172`). From CH4, the V4 unchanged
re-challenge supplies the second clean cost-lane acceptance after V3.

## Required Folds

None from CH4.

## Blockers To G-Alpha

No CH4 blocker remains. G-Alpha still depends on full V4 challenge consolidation
and the mandatory user `G-Alpha closed` sign-off before any SK-V9 skinny
dispatch, per the cited PASS-ALPHA and ORCHESTRATOR gates.

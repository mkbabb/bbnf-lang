# SK-V9 Alpha Hardening V3 - CH1 Correctness

Verdict: ACCEPT

Confidence: 97%

## Scope

Reviewed the corrected SK-V9 Pass Alpha packet at commit `32369fe8` against
CH1: citation correctness, row arithmetic, threshold floors, strict-vs-strict
comparator discipline, the V1/V2 folds, and the no-dispatch boundary. The V2
consolidation required only the citation fold from `skinny/RESULTS.md:3-40` to
`skinny/RESULTS.md:3-42` in Alpha-B through Alpha-F
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:24-35`).

## Findings

1. ACCEPT: The V2 citation fold is complete. The main table authority spans
   `skinny/RESULTS.md:3-42`, with measured rows at `skinny/RESULTS.md:5-42`;
   the prior `3-40` range omitted the final two `y_string_unicode` rows at
   `skinny/RESULTS.md:41-42`. Alpha-B now cites the full table for close-state
   authority, all-row strictness, complete native comparator coverage, lossy
   parse coverage, C++ sidecar coverage, and absent simdjson/asmjson families
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:22-39`).
   Alpha-C, Alpha-D, Alpha-E, and Alpha-F likewise use the complete `3-42`
   anchor for 38-row or all-current-row claims
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:25-30`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:26-43`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:21-28`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:29-33`).

2. ACCEPT: Row counts and row families match the source table. Alpha-A records
   38 main rows, 17 `parse_only`, 17 `direct_to_struct`, 4
   `real_typed_struct`, 7 `A / GO`, 31 `NO-GO`, 38 deferred-strictness rows,
   and the `N-direct / NoGo` overall outcome
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:33-56`;
   `skinny/RESULTS.md:5-42`; `skinny/RESULTS.md:138-141`). Mechanical recount
   of `skinny/RESULTS.md:5-42` matched those totals.

3. ACCEPT: Threshold arithmetic is closed. The parse-row formula is
   `max(ceil(SK-V8-open Track1 * 1.10), ceil(sonic_strict / 1.10))`, and every
   rendered parse threshold matches it
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-176`). The direct-row floor
   is `ceil(sonic_strict / 1.10)`, and every direct target matches
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-198`). The maintain floors
   match the 2% maintain rule in the rendered table
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:200-210`). Alpha-E's selected
   retained, typed, and direct floors agree with the master tables
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:104-108`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:203-207`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:287-291`).

4. ACCEPT: Competitor deltas and comparator strictness are CH1-safe. Alpha-B
   preserves already-rendered `sonic-rs strict`, `simdjson DOM`, and `yyjson`
   deltas from `skinny/RESULTS.md`, and its displayed integer recomputations for
   `sonic-rs lossy`, `RapidJSON default`, and `serde_json` are arithmetically
   consistent with `(bbnf Track 1 Mbps / comparator Mbps - 1) * 100`
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:12-18`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:80-119`).
   The packet treats lossy/permissive comparators, historical sidecars, absent
   sidecars, and parse DOM-vs-borrowed-view mismatches as planning evidence only,
   not strict admission evidence
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:40-60`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:121-149`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`).

5. ACCEPT: The packet does not dispatch SK-V9 implementation. The synthesis
   states that V9 implementation is not dispatched, no `SPEC.md` or
   `DISPATCH-PROMPT.md` is created by Alpha, and downstream S-P3 owns the future
   wave plan only after G-Alpha closes
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-335`). HANDOFF and Alpha-F
   preserve the same boundary
   (`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:11-13`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:103-105`).

## Required Folds

None for CH1.

## Blockers To G-Alpha

No CH1 blocker remains. G-Alpha may be presented only after the full V3
challenge wave and consolidation satisfy the Alpha convergence rule: >=95%
ACCEPT, zero open critical defects, no orphan REVISE, and then explicit user
G-Alpha sign-off (`restart/prompts/ORCHESTRATOR.md:118-121`;
`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182`).

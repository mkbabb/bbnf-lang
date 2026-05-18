# SK-V9 Alpha Hardening V2 - CH1 Correctness

Verdict: REVISE

Confidence: 96%

## Scope

Reviewed the folded Alpha packet at commit `e3ebe0b4` against CH1 correctness:
claim citations, row arithmetic, strict-vs-strict comparator handling, threshold
floors, V1 fold closure, and the G-Alpha dispatch boundary.

## Fold Verification

1. ACCEPT: V1 threshold arithmetic folds landed. The optional retained
   `apache_builds/parse_only` gate is now `>=15368 Mbps` in Alpha-E, matching the
   master row and the formula
   `max(ceil(SK-V8-open Track1 * 1.10), ceil(sonic_strict / 1.10))` over T1
   12694 and sonic strict 16904 (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:203-207`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-164`;
   `skinny/RESULTS.md:12`). Alpha-E also now uses the master maintain floors
   `twitter >=15027`, `update_center >=11719`, `mesh >=9431`, and
   `marine_ik >=11548` (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:104-108`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:204-210`;
   `skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`,
   `skinny/RESULTS.md:28`).

2. ACCEPT: V1 candidate-scope folds landed. SYNTHESIS, HANDOFF, and Alpha-F now
   distinguish the three behavior candidates from the two gate-only prerequisites
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:37-52`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:41-51`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:72-85`).

3. ACCEPT: V1 strict comparator and no-dispatch folds landed. The packet keeps
   lossy/permissive and historical sidecar comparators as flaw probes or planning
   evidence, requires strict same-run matching-plane evidence for admission, and
   keeps SK-V9 implementation behind G-Alpha and downstream S-P3
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:38-50`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:109-115`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:299-302`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`).

4. ACCEPT: V1 Alpha-D head wording fold landed. Alpha-D now says `SK-V8 close
   head` for `32870fea`, not repository HEAD for the Alpha packet
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:22-28`).

## Findings

1. REVISE: The V1 citation fold is incomplete because several folded artifacts
   still cite `skinny/RESULTS.md:3-40` as the complete final SK-V8 row authority.
   The main results table runs from the header at `skinny/RESULTS.md:3` through
   the final two `y_string_unicode` rows at `skinny/RESULTS.md:41` and
   `skinny/RESULTS.md:42`; Alpha-A already cites the complete authority as
   `skinny/RESULTS.md:3-42` and the measured row range as
   `skinny/RESULTS.md:5-42` (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:35-40`).
   Alpha-B uses the shorter `3-40` range for close-state authority, all-row
   strictness, complete native comparator columns, lossy parse coverage, C++
   sidecar coverage, and absent simdjson/asmjson families
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:22-39`).
   Alpha-C, Alpha-D, Alpha-E, and Alpha-F repeat the same incomplete row anchor
   for 38-row or all-current-row claims
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:25-30`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:26-43`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:21-28`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:29-33`).
   This fails the CH1 citation contract because the cited range omits two live
   measured rows while claiming complete 38-row authority
   (`restart/prompts/ORCHESTRATOR.md:81-84`;
   `restart/prompts/pass-contracts/PASS-ALPHA.md:35-38`).

## Required Folds

1. Replace every complete-table citation of `skinny/RESULTS.md:3-40` in
   Alpha-B through Alpha-F with a complete range, preferably
   `skinny/RESULTS.md:3-42` for table authority or `skinny/RESULTS.md:5-42` for
   measured-row claims.

2. Where a claim is specifically about the manifest rather than the main table,
   use `skinny/RESULTS.md:44-85` or cite Alpha-A's manifest extraction at
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:58-65`.

## Blockers To G-Alpha

G-Alpha is blocked on the citation-range fold above. I found no remaining CH1
blocker in threshold arithmetic, strict-vs-strict comparator discipline, V1
candidate scoping, or premature SK-V9 implementation dispatch.

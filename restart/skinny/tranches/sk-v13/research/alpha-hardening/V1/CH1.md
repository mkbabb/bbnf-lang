# CH1 Correctness - SK-V13 Pass Alpha V1

Disposition: REVISE

Scope: CH1 checked the Alpha V1 packet for resolvable numeric claims, row counts,
REDRESS references, comparator planes, and measurable goal/gate language under the
2026-05-21 addendum.

## Findings

### F1 - SOTA-margin gate is misstated as `>=1 Mbps` in Alpha-B

Alpha-B says the binding target is Track 1 beating sonic-rs strict by `>=1 Mbps`
and classifies direct, typed, and parse-only rows under that rule
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:3`,
`:38`, `:62`, `:78`). The addendum's JSON acceptance test is stricter: Track 1
must be greater than `sonic-rs strict Mbps + 1`, strict equality, strict-mode
comparator only, and no silent demotion
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:89-95`). CSS uses the
same `> comparator + 1` shape against lightningcss
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:78-87`).

The current row classifications do not appear to flip on this exact boundary:
for example direct `citm_catalog`, `apache_builds`, and `marine_ik` are the only
direct rows above sonic by more than 1 Mbps in `skinny/RESULTS.md:9`,
`:14`, and `:30`, while `numbers` and `unicode_basic` are A/GO by old gate but
below sonic at `skinny/RESULTS.md:35` and `:41`. The contract language is still
wrong and would admit an exact +1 Mbps row that the addendum rejects.

Required fix: replace every `>=1 Mbps` / "by at least 1 Mbps" SOTA-margin gate
with `Track 1 > strict comparator Mbps + 1` for JSON and CSS, including any
derived status text in Alpha-B and the SK-V13 contract surfaces.

### F2 - Alpha-A allows measured rejection for CSS gap rows where the addendum requires architectural-block proof

Alpha-A states that the remaining 23 CSS rows "must land or be
measured-rejected" (`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:90-93`).
That is not the close rule under the addendum. A CSS feature closes only as
`ADMITTED-PARITY` or with an architectural-level intrinsic-block proof; a
`PARTIAL` or implementation-limited miss is a reopen
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-31`,
`:151-170`).

Required fix: change Alpha-A's CSS gap disposition to "admit or record
architectural-level intrinsic-block proof"; measured implementation rejection is
REDRESS evidence for continued work, not a close state.

### F3 - Alpha-A does not extract every current row at the contract-required granularity

PASS-ALPHA requires Alpha-A to extract every `skinny/RESULTS.md` row with
per-corpus/per-workload Mbps, c/B, strictness, output plane, hot-leaf attribution,
and delta vs the prior SK cycle
(`restart/prompts/pass-contracts/PASS-ALPHA.md:20-27`,
`:77-108`). Alpha-A only provides the CSS close row, aggregate counts, missing
typed rows, and direct/parse row name lists
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:18-87`).

The aggregate counts themselves resolve: the current top table has 41 JSON rows
and 1 CSS row (`skinny/RESULTS.md:3-46`), with JSON split as 17 `parse_only`
`S/NO-GO`, 5 direct `A/GO`, 12 direct `N-direct/NO-GO`, and 7 typed `A/GO`
(`skinny/RESULTS.md:5-45`). The addendum target is 51 JSON rows, all 17 corpora
across 3 planes (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:33-46`).
The issue is not the aggregate count; it is that row-level extraction and c/B
coverage are missing, so CH1 cannot verify "every numeric claim" at row level.

Required fix: add a row-level Alpha-A table or companion section covering all 42
rendered current rows plus the 10 absent typed rows, with strictness, output
plane, comparator Mbps, delta-vs-SK baseline when available, hot leaf, and c/B or
explicit "not emitted by current RESULTS schema" debt.

### F4 - Source maps contain non-resolving or imprecise line anchors

CH1 requires every claim to cite resolving file:line, commit SHA, RESULTS row, or
REDRESS entry (`restart/prompts/ORCHESTRATOR.md:74-88`). Alpha-B's source map
anchors PASS-ALPHA comparator/schema authority to `PASS-ALPHA.md:31-35` and
`:76-123` (`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:9`).
The comparator extraction obligation and strict comparator gate actually live at
`restart/prompts/pass-contracts/PASS-ALPHA.md:23` and
`restart/prompts/pass-contracts/PASS-ALPHA.md:64-75`; the telemetry schema starts
at `restart/prompts/pass-contracts/PASS-ALPHA.md:77-110`.

Alpha-A also lists evidence paths without line anchors for the addendum, campaign
close, RESULTS, REDRESS, and scoping claims
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:115-146`).
Several of those claims are numerically material, such as the CSS close row and
parity counts, and should cite the resolving lines:
`restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:8-21`,
`skinny/RESULTS.md:46`, and
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:127-132`.

Required fix: correct Alpha-B's PASS-ALPHA anchors and add line-level citations
to Alpha-A's material evidence list.

## Checked Correct

- CSS close numbers resolve: Track 1 `429.34420791225705`, cssparser
  `217.42665242186035`, lightningcss `168.92962215656692`, threshold
  `169.92962215656692`, margin `259.41458575569015`, ratio
  `2.5415566697611705x`, strict equality, and SHA all match
  `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:8-21`,
  `skinny/RESULTS.md:46`, and `skinny/REDRESS.md:3822-3835`.
- CSS parity count resolves: 1 PARITY, 7 PARTIAL, 16 MISSING, 6 OUT_OF_SCOPE,
  total 30, so 24 non-OUT_OF_SCOPE and 23 remaining after the SK-V12 admission
  (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:96-132`;
  addendum target at
  `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-31`).
- REDRESS-119/120 are correctly treated as history under the addendum, not active
  row closures (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:58-75`;
  REDRESS-119/120 source at `skinny/REDRESS.md:3495-3553`).
- The SK-V13 SYNTHESIS/HANDOFF goalset correctly names G1-G7, G-Omega before W0,
  rolling SOTA delta, no demotion, and strict comparator anchors
  (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:30-129`,
  `restart/skinny/tranches/sk-v13/SYNTHESIS.md:152-184`,
  `restart/skinny/tranches/sk-v13/HANDOFF.md:42-58`).

## Required Fix Summary

1. Replace `>=1 Mbps` / "at least 1 Mbps" with `Track 1 > strict comparator + 1`
   everywhere the SOTA gate is stated.
2. Replace "measured-rejected" CSS close language with architectural-block proof
   language.
3. Expand Alpha-A to row-level extraction, including c/B or explicit schema debt.
4. Repair source maps so material claims carry resolving file:line anchors.

After those fixes, the packet is likely ACCEPT for CH1; current defects are
correctable and do not require discarding the SK-V13 contract.

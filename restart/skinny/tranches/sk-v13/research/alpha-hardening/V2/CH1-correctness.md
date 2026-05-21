# CH1 Correctness - SK-V13 Pass Alpha V2

Date: 2026-05-21.

Verdict: REVISE.

Scope: CH1 rechecked the V2 fold against the four prior CH1 blockers: strict
comparator-plus-one semantics, row-level B0 extraction with absent typed rows,
CSS close authority, and evidence citations.

## Fixed

### F1 - Strict comparator-plus-one semantics

Status: ACCEPT.

The V1 `>=1 Mbps` wording has been removed from the alpha packet. Alpha-B now
states the JSON close target as Track 1 greater than `sonic-rs strict Mbps + 1`
or architectural intrinsic-block proof
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:3`),
and applies the same rule to direct, typed, and `parse_only` classifications
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:38`,
`:62`, `:78`). Alpha-A defines B0 margin as
`Track 1 - (strict comparator + 1)` with CSS using lightningcss and JSON using
sonic-rs strict
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:88-93`).
The master contract also uses the strict form for CSS and parse-only JSON
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:45-54`, `:95-105`).

### F2 - Row-level B0 extraction, including absent typed rows

Status: ACCEPT.

Alpha-A now exposes a row-level B0 inventory for the current rendered rows with
state, strictness, plane, Track 1, Track 2, strict comparator, comparator-plus-one
margin, c/B schema debt, and hot-leaf evidence
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:88-138`).
It also explicitly lists the ten absent `real_typed_struct` rows as absent-row
debt (`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:139-148`).
This fixes the prior aggregate-only extraction issue and matches the 51-row G5
target in the addendum
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:33-46`).

### F3 - CSS close authority limited to ADMIT or architectural block

Status: ACCEPT.

Alpha-A no longer says the remaining 23 CSS rows may close by measured rejection.
It now says the 23 partial/missing rows must admit or carry architectural-level
intrinsic-block proof, and that measured implementation rejection is REDRESS
evidence, not close authority
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:150-157`).
Alpha-D independently states that no CSS feature may close as `PARTIAL` and no
implementation-limited block can close
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md:143-153`).
This matches the binding addendum's CSS close rule
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-31`,
`:151-170`) and the SK-V13 synthesis close rule
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:30-57`).

## Remaining CH1 Defect

### F4 - Evidence citations are still not fully resolving

Status: REVISE.

The V2 fold repaired the Alpha-B PASS-ALPHA anchors
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:5-14`)
and added line-level evidence for the material Alpha-A addendum, close,
RESULTS, REDRESS, and CSS gap claims
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:179-204`).

However, Alpha-A still ends its evidence list with three unanchored file-level
citations:

- `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:205-207`
  cites profile truth without line anchors for PMU staleness, 13 direct residual
  rows, 17-corpus capture scope, and fresh PMU requirements.
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:208-210`
  cites value/API union without line anchors for residual policy leaks.
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:211-213`
  cites SIMD/ASM/union without line anchors for W4 orphan disposition,
  `a64_ascii_set_run_skip`, and same-wave consumer requirements.

Alpha-F also remains a source map without resolving line anchors:
`restart/skinny/tranches/sk-v13/research/alpha/alpha-F-contract-draft.md:10-20`.
Because CH1 requires material claims to resolve to a RESULTS row, REDRESS entry,
commit SHA, file:line, or measurement artifact, this keeps V2 at REVISE.

## Required File/Line Fixes

1. In `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:205-213`,
   replace the three whole-file evidence bullets with line-resolving citations.
   Minimal sufficient anchors:
   - profile truth: `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md:11-25`,
     `:105-112`, `:182-192`;
   - value/API union: `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:57-72`,
     `:180-214`;
   - SIMD/ASM/union: `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:10-23`,
     `:52-73`, `:184-203`.
2. In `restart/skinny/tranches/sk-v13/research/alpha/alpha-F-contract-draft.md:10-20`,
   add line-level anchors for each source-map bullet. Use at least:
   `restart/prompts/pass-contracts/PASS-ALPHA.md:20-29`,
   `:33-49`, `:51-123`;
   `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-46`,
   `:76-95`, `:151-183`;
   `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:8-21`, `:41-60`;
   `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:13-18`,
   `:96-132`;
   and the scoping anchors listed in fix 1 for profile, value/API/union, and
   SIMD/ASM claims.

After those citation repairs, CH1 should converge to ACCEPT. No further numeric
or close-authority corrections are required by this CH1 pass.

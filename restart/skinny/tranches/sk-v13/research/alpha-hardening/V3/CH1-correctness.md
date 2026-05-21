# CH1 Correctness - SK-V13 Pass Alpha V3

Date: 2026-05-21.

Verdict: ACCEPT.

Scope: CH1 rechecked the V3 Alpha packet against the remaining V2 citation
blocker and spot-checked that the accepted V2 fixes did not regress:
comparator-plus-one semantics, row-level B0 extraction including absent typed
rows, and CSS close authority.

## V3 Citation Fix

Status: ACCEPT.

The V2 blocker was narrow: Alpha-A still had three whole-file evidence bullets,
and Alpha-F had a source map without resolving line anchors. V3 fixes both.

- Alpha-A now resolves the profile-truth evidence to
  `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md:11-25`,
  `:105-112`, and `:182-192`, covering PMU staleness, direct-row priority/risk,
  17-corpus capture scope, and fresh SK-V13 PMU requirements
  (`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:206-209`).
- Alpha-A now resolves value/API union evidence to
  `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:57-72`
  and `:180-214`, covering residual JSON policy leaks and CSS sufficiency
  requirements
  (`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:210-212`).
- Alpha-A now resolves SIMD/ASM/union evidence to
  `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:10-23`,
  `:52-73`, and `:184-203`, covering W4 orphan disposition,
  `a64_ascii_set_run_skip`, and same-wave consumer requirements
  (`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:213-216`).
- Alpha-F's source map now line-anchors PASS-ALPHA, the addendum, SK-V12 close,
  CSS parity gap, profile truth, decision engine, value/API union, and SIMD/ASM
  sources
  (`restart/skinny/tranches/sk-v13/research/alpha/alpha-F-contract-draft.md:10-33`).

Those ranges exist in the cited files and resolve to the claimed evidence:
PASS-ALPHA's Alpha-A/Alpha-F duties and CH1 comparator checks
(`restart/prompts/pass-contracts/PASS-ALPHA.md:20-29`, `:33-49`, `:51-123`);
the addendum's CSS, JSON, rolling-delta, and no-demotion rules
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-46`,
`:76-95`, `:151-183`); SK-V12 close facts
(`restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:8-21`, `:31-60`); CSS gap
surface
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:13-18`,
`:96-132`); profile truth
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md:11-25`,
`:105-112`, `:182-192`); value/API union leaks
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:57-72`,
`:180-214`); and SIMD/ASM scope
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:10-23`,
`:52-73`, `:184-203`).

## Regression Checks

### Comparator-Plus-One Semantics

Status: ACCEPT.

No regression found. Alpha-B still states the close target as every JSON row
greater than `sonic-rs strict Mbps + 1` or architectural intrinsic-block proof
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:3`)
and applies the same boundary to direct, typed, and `parse_only` rows
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:38`,
`:62`, `:78`). Alpha-A's B0 inventory still defines margin as
`Track 1 - (strict comparator + 1)` with CSS using lightningcss and JSON using
sonic-rs strict
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:88-93`).
The SK-V13 synthesis keeps the same `+ 1` threshold for CSS and JSON close
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:45-57`, `:95-105`).

### Row-Level B0 Extraction

Status: ACCEPT.

No regression found. Alpha-A still carries a row-level B0 inventory with state,
strictness, plane, Track 1, Track 2, strict comparator, comparator-plus-one
margin, c/B schema debt, and hot-leaf evidence for the rendered JSON/CSS rows
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:88-138`).
It still lists the ten absent `real_typed_struct` rows as row-level
absent-row debt
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:139-148`).

### CSS Close Authority

Status: ACCEPT.

No regression found. Alpha-A still limits the remaining 23 CSS rows to ADMIT or
architectural-level intrinsic-block proof and explicitly states that measured
implementation rejection is REDRESS evidence, not close authority
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:150-157`).
Alpha-D still invalidates `PARTIAL` CSS close and implementation-limited close
(`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md:143-153`).
The SK-V13 synthesis preserves the same close authority rule
(`restart/skinny/tranches/sk-v13/SYNTHESIS.md:30-57`).

## Disposition

CH1 has no remaining correctness blocker for the V3 Alpha packet. The V2
citation defect is repaired, and the previously accepted numeric, extraction,
and close-authority fixes still hold.

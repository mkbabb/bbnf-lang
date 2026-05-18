# CH2 Ledger Consistency Review

Date: 2026-05-18.

Verdict: ACCEPT.
Confidence: 97%.

Scope: adversarial review of W6 close consistency across
`skinny/RESULTS.md`, `skinny/REDRESS.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`, and the W6 close packet.

## Evidence

- W2 Apache/CITM measured-row overclaim: not found. The close packet says W2
  admitted source/product parity only, not measured row-table expansion
  (`skv8-W6-close-and-alpha-feedback.md:15`, `:29`, `:80-82`;
  `skv8-W6-close-reconciliation-research.md:31-33`). REDRESS 91 says the
  admitted source/product rows are `apache_builds/real_typed_struct` and
  `citm_catalog/real_typed_struct`, not present as measured rows in the W0
  manifest, and W2 does not claim six measured `real_typed_struct A / GO` rows
  (`skinny/REDRESS.md:2622-2657`). HANDOFF matches this
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:177-194`). The measured
  `RESULTS.md` real-typed rows remain only `twitter`, `update_center`, `mesh`,
  and `marine_ik` (`skinny/RESULTS.md:7`, `:18`, `:21`, `:28`); the manifest
  counter returned `manifest_rows=38` and `real_typed_rows=4`.
- W3/W4 status mismatch: not found. W3 is rejected/routed with no source patch,
  no rejected patch artifact, no `RESULTS.md` change, and no row-table
  admission in HANDOFF (`HANDOFF.md:199-208`) and REDRESS 92
  (`skinny/REDRESS.md:2663-2686`). W4 is rejected/routed after selected-row
  falsification, with the patch reverted and `RESULTS.md` unchanged in HANDOFF
  (`HANDOFF.md:214-229`) and REDRESS 93 (`skinny/REDRESS.md:2694-2729`). The
  close packet repeats the same status (`skv8-W6-close-and-alpha-feedback.md:30-31`,
  `:48-51`).
- W5 row/perf overclaim: not found. HANDOFF says W5 admits only the named Lock
  14 provider-boundary cleanup and makes no performance claim, row-table
  refresh, generated-output change, or `RESULTS.md` change
  (`HANDOFF.md:231-244`). The W6 plan and close packet use the same boundary
  (`skv8-W6-plan.md:19-20`, `:61-62`;
  `skv8-W6-close-and-alpha-feedback.md:20`, `:32`). The reconciliation
  correctly says no W5 REDRESS entry is required absent a close mismatch
  (`skv8-W6-close-reconciliation-research.md:52-55`).
- RESULTS/REDRESS/HANDOFF contradiction: not found. `RESULTS.md` still reports
  overall `N-direct / NoGo` and independent Track 2 authority
  (`skinny/RESULTS.md:138-141`), while HANDOFF records the same current measured
  state (`HANDOFF.md:35-42`). `git diff --exit-code HEAD -- skinny/RESULTS.md
  skinny/REDRESS.md restart/skinny/tranches/sk-v8/HANDOFF.md` returned clean.

## Required Fold

None. No CH2 blocker found for W6 close.

# SK-V10 Alpha CH4 Cost

Date: 2026-05-19.

Scope: LOC budgets, hard caps, wave sizing, and measurable closure cost.

## Disposition

REVISE -> ACCEPT after fold.

## Findings

1. Alpha-E carried risks but not the required LOC budgets, hard caps, same-wave
   consumers, or revert behavior.
   Fold: every candidate now carries a wave contract seed with LOC range, hard
   cap, same-wave consumer, and REDRESS/revert disposition.
2. Several gates lacked row floors or target matrices.
   Fold: Alpha-E now has a direct target matrix, typed target seed, root target
   matrix, unicode/string kernel target matrix, W10b maintain floors, and
   telemetry target matrix.
3. The direct candidate had an unbounded "future artifacts" owner entry.
   Fold: Alpha-E now names concrete `research/p1/direct-profile/`,
   `research/p2/direct-contract/`, and `research/p3/direct-wave-plan/`
   namespaces and requires S-P3 CHALLENGE for owner expansion.

## Result

S-P3 can tighten the candidate budgets and gates but may not loosen them
without CHALLENGE.

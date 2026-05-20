# SK-V12 S-P1 Hardening V2 Consolidation

Pass: S-P1 Profile. Cycle: V2 CHALLENGE -> V3 fold.
Date: 2026-05-20.
Scope: adversarial review of the V1-folded SK-V12 S-P1 profile packet at
commit `d1e6938a`.

## Lens Dispositions

| Lens | Disposition | Blocking point |
|---|---|---|
| CH1 correctness | REVISE | Fresh self-time evidence exists, but the derived xctrace leaf tables still contain line-zero source anchors in cited top-leaf rows. |
| CH2 generality / Lock 14 | ACCEPT | The V1 JSON-role vocabulary defect is folded and non-JSON limits are fenced as profile limits, not proof. |
| CH3 regression / REDRESS | ACCEPT | No row movement, RESULTS mutation, or REDRESS overclaim is introduced by the profile packet. |
| CH4 cost / replayability | REVISE | Replay remains placeholder-based; the packet needs one repo-tracked replay surface enumerating exact row commands and a clear samply artifact-only policy. |
| CH5 hidden coupling | ACCEPT | Artifact paths, cwd boundaries, and status ledgers are sufficient for coupling audit after the V1 fold. |
| CH6 anti-paper-close | ACCEPT | The packet no longer paper-closes missing self-time percentages or Mode III absence. |

Result: 4/6 ACCEPT, 2/6 REVISE. The S-P1 packet is not converged. No behavior
source edit is required.

## Required V3 Fold

1. Regenerate or post-process `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`
   and `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv` so every row claimed
   as exact self-time evidence has a concrete nonzero source anchor, or is
   explicitly marked unresolved and no longer used to satisfy the exact
   file:line contract.
2. Add a repo-tracked replay TSV or script that enumerates each parse, direct,
   and typed PMU, samply, Time Profiler, CPU Counter, and export row with cwd,
   full command, corpus, alias, mode, iteration count, binary path, expected
   return-code policy, and output artifact.
3. Label the samply `--save-only` lane as retained artifact-only evidence. The
   self-time authority for S-P1 remains exported xctrace Time Profiler XML and
   the derived TSVs.

The resolved V1 concerns remain resolved: PMU arithmetic is internally
consistent, Mode III is an explicit absence boundary, and the packet claims no
SK-V12 row movement.

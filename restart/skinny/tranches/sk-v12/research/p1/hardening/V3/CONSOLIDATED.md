# SK-V12 S-P1 Hardening V3 Consolidation

Pass: S-P1 Profile. Cycle: V3 CHALLENGE -> V4 fold.
Date: 2026-05-20.
Scope: adversarial review of the V2-folded SK-V12 S-P1 profile packet at
commit `ffe5553d`.

## Lens Dispositions

| Lens | Disposition | Blocking point |
|---|---|---|
| CH1 correctness | REVISE | Source columns are clean, but cited self-time TSV symbol fields still contain line-zero pseudo-symbols such as `direct_struct::direct_struct.rs:0`, `lib.rs:0`, and `serde_json::lib.rs:0`. |
| CH2 generality / Lock 14 | ACCEPT | The packet keeps JSON-only profile facts fenced and does not promote them as grammar-neutral proof. |
| CH3 regression / REDRESS | ACCEPT | The replay/source-line fold does not move rows, reopen W3, or mutate RESULTS/REDRESS status. |
| CH4 cost / replayability | ACCEPT | The replay TSV enumerates the required row commands, artifacts, cwd, aliases, and return-code policies; samply is labeled artifact-only. |
| CH5 hidden coupling | ACCEPT | The packet preserves artifact/cwd/alias boundaries without making profile-only evidence implementation authority. |
| CH6 anti-paper-close | ACCEPT | Required evidence is either present or explicitly absent; no gate closes on profile-only data. |

Result: 5/6 ACCEPT, 1/6 REVISE. The S-P1 packet is not converged. No behavior
source edit is required.

## Required V4 Fold

Regenerate or post-process `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`
and `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv` so the cited `top_leaf`
and `symbol` fields no longer contain line-zero pseudo-symbols. Existing
concrete source columns may remain authoritative, but the symbol fields must
name resolved function/symbol paths rather than `*:0` placeholders.

The resolved V2 concerns remain resolved: self-time source columns have no
line-zero anchors, the replay TSV is internally valid, PMU arithmetic is
consistent, Mode III is an absence boundary, and the packet claims no SK-V12 row
movement.

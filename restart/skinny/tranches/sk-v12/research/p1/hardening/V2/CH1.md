# SK-V12 S-P1 Hardening V2 - CH1 Correctness

Verdict: REVISE.

Lens: CH1 correctness after commit `d1e6938a`. Scope: the six SK-V12 S-P1
artifacts under `restart/skinny/tranches/sk-v12/research/p1/`, the V1 fold
ledger, `skv12-p1-capture-manifest.md`, `skinny/RESULTS.md`, and
`skinny/REDRESS.md`.

## Findings

1. BLOCKING - Fresh self-time evidence now exists, but not every top-leaf claim
   has an exact source file:line.

   The CH1 contract requires every hot-leaf claim to carry a symbol path, %
   self-time, and source file:line, with every `unprofiled` cell resolved
   (`restart/prompts/skinny/PASS-1-PROFILE.md:123`,
   `restart/prompts/skinny/PASS-1-PROFILE.md:127`). V2 materially improves the
   V1 state: the manifest says the V1 fold parsed xctrace Time Profiler XML into
   target-binary leaf self-time tables
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:118`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:133`),
   and P1-E reports parse/direct/typed selected-target coverage at 99.86%,
   99.92%, and 99.92%
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:124`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:130`).
   P1-E also routes the exact row-level symbol, percent, and file:line evidence
   to `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:202`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:225`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:252`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:287`).

   The referenced self-time artifacts still contain unresolved line-zero source
   references. In the top-leaf summary, 13/82 rows have `top_leaf_source`
   ending in `:0`; examples include parse `apache_builds/track2`,
   parse `distinct_values/track1`, direct `citm_catalog/track2`, typed
   `twitter/real_typed_track2`, direct `unicode_escapes/track1`, and typed
   `update_center/real_typed_track1`
   (`/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:3`,
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:8`,
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:45`,
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:69`,
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:74`,
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:78`). The details table has
   the same defect across 31/410 detail rows, including rank-1 rows such as
   parse `apache_builds/track2`, parse `distinct_values/track1`, direct
   `citm_catalog/track2`, typed `twitter/real_typed_track2`, direct
   `unicode_escapes/track1`, and typed `update_center/real_typed_track1`
   (`/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:7`,
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:32`,
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:217`,
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:337`,
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:362`,
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:382`).

   This is no longer the V1 failure mode where fresh self-time percentages were
   absent. It is still incomplete for CH1 because `:0` is not an exact source
   line and the artifact text claims exact file:line evidence.

2. PASS - PMU aggregate arithmetic is now internally consistent.

   V1 required P1-E's aggregate c/B block to match the PMU TSVs and P1-D
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH1.md:157`,
   `restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH1.md:159`). The
   V1 fold ledger says P1-E was corrected to parse `2.920217`, direct
   `4.290305`, and typed `3.123172`
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/FOLD-REVISIONS.md:37`,
   `restart/skinny/tranches/sk-v12/research/p1/hardening/V1/FOLD-REVISIONS.md:38`).
   Current P1-D, P1-E, and the manifest all agree on those weighted values
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:93`,
   `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:100`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:112`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:118`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:146`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:154`).
   Recomputing from `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv` and
   `/tmp/skv12-p1/pmu/product_pmu_rows.tsv` produced the same aggregate Mbps,
   c/B, and CPI for parse, direct, and typed guard rows.

3. PASS - Mode III call-stack handling is now an explicit absence boundary, not
   an unsupported fresh-symbol claim.

   V1 allowed the pass to resolve the Mode III issue by producing fresh 17/17
   call-stack captures or by folding an explicit non-converged/absent status
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH1.md:161`,
   `restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH1.md:164`). V2
   chooses the latter. P1-C states no fresh Mode III samply call-stack capture is
   claimed and records 0/17 fresh Mode III call-stack rows
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:13`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:19`).
   It also states there is no `/tmp/skv12-p1/samply/probes`, no fresh probe
   capture, and no fresh structural-scan capture, and fences W0 Mode III facts as
   throughput-only diagnostic evidence that S-P2/S-P3 may not use as fresh
   SK-V12 hot-leaf authority
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:63`). The
   absent-cell table preserves that boundary
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:145`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:157`), and
   the manifest repeats the same prohibition
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:160`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:168`).

   This does not create fresh Mode III call-stack evidence, but it resolves the
   CH1 correctness concern by preventing stale W0 probe extraction from being
   counted as fresh SK-V12 hot-leaf authority.

4. PASS - I found no unsupported row movement.

   P1-F records the live surface as 16 `S / NO-GO` parse diagnostics, 1
   `L / NO-GO` parse diagnostic, 4 direct `A / GO` guards, 13
   `N-direct / NO-GO` residual direct rows, 7 typed `A / GO` guards, no admitted
   generated non-JSON baseline, and overall `N-direct / NoGo`
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:83`).
   It states every SK-V12-open delta is zero because `skinny/RESULTS.md` is
   unchanged from SK-V11 close
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:91`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:93`), and
   its telemetry section reports no `skinny/RESULTS.md` or `skinny/REDRESS.md`
   diff from SK-V11 close plus no parser-source path changes
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:197`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:205`).
   P1-E repeats that no row moved between SK-V11 close and SK-V12-open
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:340`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:352`).
   REDRESS 120 remains the authority: SK-V11 closed as a measured fixpoint, made
   no behavior/gate/RESULTS change, and admitted no direct or W0-clamped row
   (`skinny/REDRESS.md:3531`, `skinny/REDRESS.md:3544`). The live RESULTS file
   still ends with overall `N-direct / NoGo` (`skinny/RESULTS.md:143`).

## Required Remediation

1. Regenerate or post-process
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` and
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv` so every row that P1-A,
   P1-B, and P1-E cite as exact self-time evidence has a concrete source line
   greater than zero. If xctrace cannot resolve a concrete line for an inlined or
   external frame, mark that row as unresolved and do not claim it satisfies the
   exact file:line contract.

2. Update P1-A/P1-B/P1-E after the regenerated tables so their "exact top leaf
   symbol, percent, and file:line" statements are true for all cited rows. No
   behavior source edit is required.

3. Keep the PMU aggregates, Mode III absence boundary, and row-movement posture
   as-is unless later evidence changes them. Those V1 CH1 blockers are resolved
   in the current packet.

## Evidence Commands

```bash
awk -F '\t' 'NR>1 {n++; if ($16 ~ /:0$/) z++} END {print n, z}' \
  /tmp/skv12-p1/time_profile_hot_leaf_summary.tsv

awk -F '\t' 'NR>1 {n++; if ($9 ~ /:0$/) z++} END {print n, z}' \
  /tmp/skv12-p1/time_profile_hot_leaf_details.tsv

awk -F '\t' 'NR>1 {
  bytes += $4 * $5; cycles += $9; instr += $10; elapsed += $6
} END {
  printf "parse rows=%d Mbps=%.3f c/B=%.6f CPI=%.6f\n",
    NR-1, bytes*8/elapsed/1000/1000, cycles/bytes, cycles/instr
}' /tmp/skv12-p1/pmu/parse_pmu_rows.tsv

awk -F '\t' 'NR>1 && $2 ~ /^track/ {
  rows++; bytes += $4 * $5; cycles += $9; instr += $10; elapsed += $6
} END {
  printf "direct rows=%d Mbps=%.3f c/B=%.6f CPI=%.6f\n",
    rows, bytes*8/elapsed/1000/1000, cycles/bytes, cycles/instr
}' /tmp/skv12-p1/pmu/product_pmu_rows.tsv

awk -F '\t' 'NR>1 && $2 ~ /^real_typed/ {
  rows++; bytes += $4 * $5; cycles += $9; instr += $10; elapsed += $6
} END {
  printf "typed rows=%d Mbps=%.3f c/B=%.6f CPI=%.6f\n",
    rows, bytes*8/elapsed/1000/1000, cycles/bytes, cycles/instr
}' /tmp/skv12-p1/pmu/product_pmu_rows.tsv
```

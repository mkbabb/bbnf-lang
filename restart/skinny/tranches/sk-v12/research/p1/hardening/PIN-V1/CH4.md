# SK-V12 S-P1 PIN-V1 CH4 - Cost / Replayability

Verdict: REVISE
Score: 82%

## Blocking Findings

1. The pin replay surface is not exact. `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:70-92` names `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` as the authoritative independent replay ledger, but that TSV still targets the pre-pin root and build (`/tmp/skv12-p1`, `/tmp/skv12-profile-target-50bd1648`) starting at `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv:2`. The real pin PMU and samply command ledgers exist only under `/tmp/skv12-pin-p1/pmu/pmu-commands.sh` and `/tmp/skv12-pin-p1/samply/samply-commands.sh`; there is no equivalent pin-root xctrace command ledger under `/tmp/skv12-pin-p1`. A third party can infer many commands by rewrite, but CH4 requires verbatim commands.

2. The run identity is incomplete for the pin fold. The manifest addendum records source head `cf7848b2`, capture root `/tmp/skv12-pin-p1`, and build root `/tmp/skv12-pin-profile-target-cf7848b2` at `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:11-20`, while the committed fold is `b1043383`. The docs do not name one unified pin run id tying `cf7848b2` source capture, the `b1043383` fold commit, target dir, capture root, and completion stamps (`/tmp/skv12-pin-p1/pmu/done.txt`, `/tmp/skv12-pin-p1/samply/done.txt`, `/tmp/skv12-pin-p1/xctrace/done.txt`). PASS-1 requires run id / host / build flags for replayability (`restart/prompts/skinny/PASS-1-PROFILE.md:143-146`).

3. The final docs contain stale contradictory missing-artifact ledgers. P1-A says the final fold has parse samply/xctrace and derived hot-leaf artifacts at `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:23-37`, but later says those paths do not exist at `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:80-89`, `:137-144`, and lists them as missing at `:204-356`. P1-B similarly declares final product hot-leaf authority at `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:18-30`, then says attribution is unavailable and lists final paths as missing at `:163-190`. P1-E declares final hot-leaf authority at `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:17-44`, then preserves an unavailable ledger at `:75-118` and `:165-193`. The artifacts now exist, so these sections make cost/replay state ambiguous.

4. Export status wording is not honest to the artifact. The manifest and lane folds call Time Profiler XML exports PASS (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:31`, `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:35`, `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:29`), but `/tmp/skv12-pin-p1/time_profile_export_status.tsv:2-83` records 82 rows as `SKIP`, not `PASS`. The exports are nonempty, so the likely intended meaning is "present/reused"; the fold must say that rather than relabeling SKIP as PASS.

## Validated Evidence

- PMU authority is present and complete: `/tmp/skv12-pin-p1/pmu/capture_status.tsv` has 82 data rows, all `PASS rc=0`; `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` has 34 data rows and `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv` has 48.
- Samply authority is present and complete: `/tmp/skv12-pin-p1/samply/capture_status.tsv` has 82 data rows, all `PASS rc=0`, with 82 nonempty `.json.gz` artifacts.
- xctrace authority is present and complete: `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` has 212 data rows, all `PASS`, with 82 primary Time Profiler rows, 82 CPU Counters rows, and 48 product-v2 Time Profiler rows. All 185 `rc=54` rows have logs containing an accepted stop condition plus "Output file saved as".
- Derived tables pass the requested checks: `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` has 82 rows split parse 34 / direct 34 / typed 14; `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` has 410 rows split parse 170 / direct 170 / typed 70; neither table contains unresolved `:0`, `unknown`, `none`, or `UNRESOLVED_LINE_ZERO` in the load-bearing source fields.
- CSS L4 absence is correctly treated as a boundary, not a fallback authorization: the user pin requires CSS L4 first (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`), and P1-C records that S-P2/S-P3 must first create a generated CSS L4 parser plus lightningcss comparator/equality path before CSS profiling or SOTA claims (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:106-110`). `/tmp/skv12-pin-p1` contains no CSS or lightningcss artifacts.

## Nonblocking Notes

- The xctrace rc-54 policy itself is acceptable: the artifact logs satisfy the manifest rule at `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:114-115`.
- Branch misses, L1 misses, and LLC misses are honestly not inferred from the PMU TSVs; P1-D records this at `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:65-74`.
- JSON guard/direct/typed measurements are not being used to populate the CSS L4 close bar; P1-F records the absent CSS row and stale RESULTS run id at `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:22-36` and `:171-193`.

## Exact Fold Edits Required

1. Add a pin-root replay ledger, either by updating `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` in place or adding a clearly named pin file, with all PMU, samply, xctrace primary, xctrace CPU Counters, product-v2, and export commands rewritten to `/tmp/skv12-pin-p1` and `/tmp/skv12-pin-profile-target-cf7848b2`. Include the xctrace commands verbatim; do not leave them reconstructable only by inference.
2. In `skv12-p1-capture-manifest.md`, add a single pin run identity block naming: source capture commit `cf7848b2`, fold commit `b1043383`, run id, capture root `/tmp/skv12-pin-p1`, target dir `/tmp/skv12-pin-profile-target-cf7848b2`, binary paths, host/tool versions, and PMU/samply/xctrace completion stamps.
3. Replace or delete the stale post-fold missing-artifact sections in P1-A, P1-B, and P1-E so the files contain one current truth: final artifacts present for JSON parse/direct/typed, Mode III absent, CSS L4 absent.
4. Change export wording from "PASS" to "present/reused SKIP" unless the export step is actually rerun and `/tmp/skv12-pin-p1/time_profile_export_status.tsv` records PASS rows.
5. Keep the CSS L4 absence as a hard S-P2 prerequisite: S-P2 must include generated CSS L4 runtime bring-up plus lightningcss same-plane comparator/equality before profiling or claiming that row.

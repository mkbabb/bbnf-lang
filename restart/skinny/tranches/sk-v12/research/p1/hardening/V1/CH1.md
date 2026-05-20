# SK-V12 S-P1 Hardening V1 - CH1 Correctness

Disposition: REVISE.

Lens: CH1 correctness. Scope: SK-V12 S-P1 artifacts
`restart/skinny/tranches/sk-v12/research/p1/p1*.md`, SK-V12
`SYNTHESIS.md` / `HANDOFF.md` / `G-ALPHA-SK-V12.md`, `skinny/RESULTS.md`,
`skinny/REDRESS.md` through REDRESS 120, `/tmp/skv12-p1`, and SK-V11 S-P1
hardening precedent.

CH1 requires every hot-leaf claim to cite a symbol path, percent self-time, and
source file:line; c/B must come from real PMU counters; corpus coverage must be
complete; and `unprofiled` cells must be resolved
(`restart/prompts/skinny/PASS-1-PROFILE.md:123`,
`restart/prompts/skinny/PASS-1-PROFILE.md:127`). P1-C is also scoped as a
Mode III samply profile, not just stale probe extraction
(`restart/prompts/skinny/PASS-1-PROFILE.md:54`), and every profiling agent is
supposed to cover all 17 JSON corpora
(`restart/prompts/skinny/PASS-1-PROFILE.md:69`,
`restart/prompts/skinny/PASS-1-PROFILE.md:70`).

## Findings

1. BLOCKING - Fresh SK-V12 hot-leaf attribution does not satisfy the CH1
   self-time contract.

   The packet is honest about this gap, but the gap remains. P1-A says no fresh
   parse xctrace summary or symbols export exists and that it therefore does not
   claim fresh top-leaf percentages
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:95`,
   `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:100`,
   `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:103`).
   P1-B makes the same product-plane caveat: samply profiles are
   `symbolicated=false`, no fresh `direct-xctrace/exports/summary.json` exists,
   and leaf percentages are not extracted
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:114`,
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:117`,
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:118`).
   P1-E then explicitly says the companion `.syms.json` files resolve symbol
   maps but not exact per-inlined-frame self-time percentages
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:73`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:80`).
   Its primitive map is source-level and behavior-equivalent, not fresh
   percentage evidence
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:184`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:186`),
   and its own hardening caveat asks whether to require regenerated summaries
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:336`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:342`).

   This is not a paper-close or false percentage claim; the artifacts avoid
   inventing percentages. It is still incomplete for the stated S-P1 CH1
   contract.

2. BLOCKING - P1-E aggregate c/B values do not match `/tmp/skv12-p1`.

   P1-D's aggregate PMU table matches the raw TSVs: parse 34 rows at
   2.920217 c/B, direct 34 rows at 4.290305 c/B, and typed guards 14 rows at
   3.123172 c/B
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:96`,
   `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:100`). P1-E
   instead records parse 2.938593 c/B and direct 4.331411 c/B, while typed is
   only rounding-equivalent
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:112`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:118`).

   Recomputing `sum(cycles) / sum(iters * corpus_bytes)` from
   `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv` and
   `/tmp/skv12-p1/pmu/product_pmu_rows.tsv` yields P1-D's numbers, not P1-E's.
   The per-row c/B tables in P1-E appear rounded from the raw rows, but the
   aggregate block needs correction or removal.

3. BLOCKING - P1-C is truthful about the Mode III gap, but the gap means the
   S-P1 coverage contract is not complete.

   P1-C declares 17/17 W0 masking and structural-scan facts plus 17/17 fresh
   parse PMU rows, but also declares 0/17 fresh Mode III samply call-stack probe
   rows (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:17`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:19`).
   The method confirms there is no `/tmp/skv12-p1/samply/probes`, no
   `json_probes_*` capture, and no fresh structural-scan capture under
   `/tmp/skv12-p1`
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:57`).
   It correctly keeps those cells absent rather than inferred
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:139`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:150`).

   That honesty is good, but the artifact cannot be counted as fresh 17/17
   Mode III profiling under the prompt's P1-C scope.

4. PASS - Main parse/direct/typed coverage and capture-status counts are
   supported.

   P1-A claims parse-only Track 1/Track 2 coverage across 17/17 corpora
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:14`).
   P1-B claims direct 17/17 x two tracks and typed guard 7/7 x two tracks
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:15`,
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:16`).
   P1-D claims the same PMU row surface
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:14`,
   `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:15`). The raw
   capture status has 328 data rows: 82 PMU captures, 82 samply captures, and
   164 xctrace captures, matching P1-F
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:24`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:30`).

5. PASS - Artifact counts match `/tmp/skv12-p1`.

   P1-E reports 82 samply `.json.gz`, 82 companion `.json.syms.json`, and 164
   retained xctrace trace bundles
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:26`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:28`).
   The filesystem matches those counts. The 164 xctrace bundles are `.trace`
   directories, not regular files, which matters for verification commands.
   P1-F's log inventory also matches the filesystem: 752 log files, split as
   260 `pmu-*`, 164 `samply-*`, and 328 `xctrace-*`
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:28`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:30`).

6. PASS - The source-delta claim is supported.

   P1-F states that SK-V12-open has no `skinny/RESULTS.md` or
   `skinny/REDRESS.md` diff from SK-V11 close, and that source delta since
   `3ce75df4` is limited to `skinny/RESULTS.md`, `skinny/REDRESS.md`,
   `bbnf-bench/src/bin/gate.rs`, and `bbnf-bench/src/report.rs`, with no
   parser-source filter changes
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:197`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:205`).
   My diff checks matched that shape.

7. PASS - No new row admission is claimed.

   The SK-V12 synthesis makes generated non-JSON baseline first, keeps parse-only
   diagnostic, and keeps JSON direct residual rows pre-blocked by REDRESS 119
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:49`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:62`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:69`). P1-F's result extraction
   keeps the live surface at 16 `S / NO-GO`, 1 `L / NO-GO`, 4 direct
   `A / GO`, 13 `N-direct / NO-GO`, 7 typed `A / GO`, and overall
   `N-direct / NoGo`
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`,
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:87`).
   REDRESS 120 agrees that SK-V11 closed as a measured fixpoint, not direct
   `GO`, and that W9 admitted no direct or W0-clamped row
   (`skinny/REDRESS.md:3531`, `skinny/REDRESS.md:3544`).

## Required Fixes

1. Regenerate or export fresh SK-V12 self-time summaries for parse/direct/typed
   rows, or explicitly fold S-P1 as non-converged for CH1. The fixed artifact
   must bind each hot-leaf row to symbol path, percent self-time, and source
   file:line. Source-family maps alone are useful S-P2 clues, but they do not
   satisfy `PASS-1-PROFILE.md` CH1.

2. Correct P1-E's aggregate c/B table to match the PMU TSVs and P1-D:
   parse 2.920217, direct 4.290305, typed 3.123172, or remove the aggregate
   block from P1-E and cite P1-D as the sole aggregate authority.

3. Resolve P1-C's Mode III coverage gap. Either produce fresh 17/17 Mode III
   samply/xctrace call-stack captures under `/tmp/skv12-p1`, or fold the pass
   with an explicit non-converged/absent status for Mode III rather than counting
   stale W0 Criterion extraction as complete fresh S-P1 profiling.

## Evidence Commands

```bash
# Main RESULTS row counts.
awk -F '|' '/^\| [^|]+ \| (parse_only|direct_to_struct|real_typed_struct) \|/{
  gsub(/^ +| +$/, "", $3); gsub(/^ +| +$/, "", $4); gsub(/^ +| +$/, "", $5);
  fam[$3]++; out[$3":"$4":"$5]++; total++
} END {
  print "total", total;
  for (k in fam) print "family", k, fam[k];
  for (k in out) print "outcome", k, out[k]
}' skinny/RESULTS.md | sort

# Capture status row counts and return-code splits.
awk -F '\t' 'NR>1 {
  fam[$1]++; stat[$1":"$4]++; rc[$1":"$5]++; total++
} END {
  print "total", total;
  for (k in fam) print k, fam[k];
  for (k in stat) print k, stat[k];
  for (k in rc) print k, rc[k]
}' /tmp/skv12-p1/pmu/capture_status.tsv | sort

# PMU row counts.
wc -l /tmp/skv12-p1/pmu/capture_status.tsv \
      /tmp/skv12-p1/pmu/parse_pmu_rows.tsv \
      /tmp/skv12-p1/pmu/product_pmu_rows.tsv

# Aggregate c/B recomputation from raw PMU counters.
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

# Artifact inventory. Count xctrace bundles as directories.
find /tmp/skv12-p1/samply -type f -name '*.json.gz' | wc -l
find /tmp/skv12-p1/samply -type f -name '*.json.syms.json' | wc -l
find /tmp/skv12-p1 -maxdepth 4 -type d -name '*.trace' | wc -l
find /tmp/skv12-p1/logs -type f | wc -l
find /tmp/skv12-p1/logs -type f -name 'pmu-*' | wc -l
find /tmp/skv12-p1/logs -type f -name 'samply-*' | wc -l
find /tmp/skv12-p1/logs -type f -name 'xctrace-*' | wc -l

# Source-delta checks.
git diff --name-only 3ce75df4..HEAD -- skinny/crates skinny/Cargo.toml skinny/Cargo.lock | sort
git diff --name-only 50bd1648..HEAD -- skinny/crates skinny/Cargo.toml skinny/Cargo.lock | sort
git diff --name-status db2c999b..HEAD -- skinny/RESULTS.md skinny/REDRESS.md
```

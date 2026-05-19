ACCEPT

# SK-V11 S-P1 V3 CH1 Correctness

Scope: CH1 correctness only, on the folded S-P1 packet at `2e988a6a`.
The checks are hot-leaf source resolution, PMU-derived c/B, complete profile
coverage, and absence of unresolved `unprofiled` result cells.

## Findings

1. PASS - Hot-leaf claims resolve to exact symbols/source loci or to explicit
   xctrace authority.

   The V2 fold that created the V3 entry changed only CH2 vocabulary wording;
   CH1 had no required fold
   (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:12`,
   `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:22`).
   Current P1-B and P1-E retain the V1/V2 source maps for generated runtime,
   Track 2, direct hand parser, typed parser, `parse-that-regex`, SIMD movemask,
   Rust core helper, and serde/oracle leaves
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:127`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:104`).
   Local spot-checks resolve the cited generated parser, Track 2 parser,
   string/number scanner, direct digest, typed parser, and movemask functions at
   the named loci. P1-A and P1-B also state that saved samply profiles are
   `symbolicated=false`, so xctrace summaries and `*.symbols.json` exports are
   the self-time authority
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:93`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:96`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:91`).

2. PASS - c/B is PMU-derived.

   P1-D names `xctrace_probe` and `profile_direct` `PROBE_RESULT` rows, reading
   `ri_cycles` and `ri_instructions`, as the cycles/B authority
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:76`).
   Recomputing aggregate c/B from `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv` and
   `/tmp/skv11-p1/pmu/product_pmu_rows.tsv` matches P1-D: parse 34 rows at
   2.777033 c/B, direct 34 rows at 4.428342 c/B, and typed guards 14 rows at
   3.190644 c/B
   (`restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:89`).
   P1-F correctly routes PMU/cycles absence from `skinny/RESULTS.md` to P1-D
   instead of fabricating row-surface c/B
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:204`).

3. PASS - Coverage is complete for CH1.

   W0 requires all 17 corpora, 13 direct residual rows, four direct guard rows,
   and seven typed guard rows
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:82`).
   P1-A covers 17/17 parse rows; P1-B covers 17/17 direct and 7/7 typed guard
   rows; P1-E reports xctrace summaries for 34 parse and 48 product rows; P1-D
   reports 34 parse PMU rows, 34 direct PMU rows, and 14 typed guard PMU rows,
   all PMU `rc=0`
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:16`,
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:16`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:76`,
   `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md:89`).

4. PASS - No unresolved `unprofiled` cells remain.

   A current text scan found no `unprofiled` cells in P1-A through P1-F, W0, or
   `skinny/RESULTS.md`. The remaining `n/a` and `absent:*` fields in
   `skinny/RESULTS.md` are comparator or diagnostic nonproducer facts; P1-F
   classifies PMU/cycles as absent from the result row surface but supplied by
   P1-D, and classifies sidecar absence as planning signal only
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:204`,
   `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:211`,
   `skinny/RESULTS.md:146`).

## Required Fold

None for CH1. The folded V3 packet satisfies the S-P1 correctness lens: hot
leaves resolve to symbols/source loci or explicit xctrace authority, c/B comes
from PMU rows, coverage is complete, and no `unprofiled` cells remain.

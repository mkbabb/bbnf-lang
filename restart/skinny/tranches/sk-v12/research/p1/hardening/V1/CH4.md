# SK-V12 S-P1 Hardening V1 CH4 Cost / Reproducibility

Disposition: REVISE.

CH4 does not reject the packet: the capture tree exists, the final status
ledger is complete, the product CWD failure is disclosed, `xctrace rc=54` is
treated as retained time-limit traces, and stale RESULTS run identity is stated
instead of hidden. V1 still cannot ACCEPT because the packet is not yet
rerunnable enough from the documents alone: several method blocks give command
shapes with placeholders, not a single exact capture manifest or replay script,
and the fresh `/tmp/skv12-p1` run identity is not uniformly bound to the stale
`skinny/RESULTS.md` run id.

## Read Scope

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `/tmp/skv12-p1/pmu/capture_status.tsv`
- `/tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv`
- `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`
- SK-V11 CH4 precedent:
  `restart/skinny/tranches/sk-v11/research/p1/hardening/V1/CH4.md` and
  `restart/skinny/tranches/sk-v11/research/p1/hardening/V4/CH4.md`

The controlling rule is PASS-1 CH4: every Section 1 method block must carry
verbatim commands a third party can rerun, and a profile with absent run id,
host triple, or build flags fails CH4
(`restart/prompts/skinny/PASS-1-PROFILE.md:143`-`146`). The same prompt says
hardening findings must fold before the pass advances
(`restart/prompts/skinny/PASS-1-PROFILE.md:171`-`174`).

## Acceptable Evidence

- Shared build provenance is mostly present. P1-A names baseline `50bd1648`,
  host, release/debug/native flags, target directory, tools, run locator,
  binary paths, and rustc/LLVM versions
  (`p1a-samply-mode-1.md:8`-`23`). P1-B and P1-D repeat the target root,
  binary paths, rustc/cargo versions, and exact build commands from `skinny/`
  (`p1b-samply-mode-2.md:19`-`42`; `p1d-pmu-cycles.md:17`-`43`).
- The capture status is complete in the final ledger. `/tmp/skv12-p1` records
  82 PMU rows, 82 samply rows, and 164 xctrace rows as `PASS`; PMU and samply
  rows are `rc=0`, Time Profiler rows are `rc=54`, CPU Counter rows are `rc=54`
  except two direct `rc=0` rows. P1-F records the same 328 PASS-row shape
  (`p1f-results-delta.md:21`-`30`), and P1-E records the same family summary
  (`p1e-hot-leaf-attribution.md:84`-`104`).
- `xctrace rc=54` is disclosed correctly as time-limit trace retention, not a
  missing capture. P1-A says the logs show "Reached specified time limit" and
  "Output file saved as" (`p1a-samply-mode-1.md:86`-`103`); P1-E records the
  same retained-trace interpretation (`p1e-hot-leaf-attribution.md:101`-`104`);
  P1-D confines xctrace to artifact evidence, not PMU numeric authority
  (`p1d-pmu-cycles.md:81`-`85`, `:275`-`279`).
- The initial product CWD failure is honestly disclosed and isolated. P1-D says
  the first product wrapper run from repo root failed fixture lookup, the
  failure ledger has parse 34/34 pass plus direct 34/34 and typed 14/14
  `rc=134`, and the final rerun from `skinny/` is clean
  (`p1d-pmu-cycles.md:69`-`79`, `:267`-`274`). P1-E carries the same caveat
  (`p1e-hot-leaf-attribution.md:106`-`110`, `:346`-`348`).
- Missing xctrace exports are disclosed rather than papered over. P1-A states
  no retained parse export or summary exists and therefore makes no fresh
  top-leaf percentage claim (`p1a-samply-mode-1.md:95`-`103`). P1-B says no
  fresh direct export summary exists and does not extract inline percentages
  (`p1b-samply-mode-2.md:114`-`125`, `:250`-`258`). P1-E repeats that retained
  traces exist but exact per-inlined-frame percentages are absent
  (`p1e-hot-leaf-attribution.md:73`-`80`, `:336`-`342`).
- The stale RESULTS identity is explicit. P1-F says the live RESULTS run id
  remains `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, no SK-V12-specific
  Criterion render is present, and the physical labels still say `SK-V9-open`
  (`p1f-results-delta.md:21`-`33`, `:70`-`75`, `:195`-`210`).
- Cost framing stays bounded. P1-D marks PMU rows as profile evidence only,
  not row movement (`p1d-pmu-cycles.md:87`-`89`), and records aggregate c/B
  plus high-cost rows for parse/direct/typed guard lanes
  (`p1d-pmu-cycles.md:96`-`125`, `:224`-`236`). P1-B and P1-F preserve the
  SK-V12 priority: JSON product profiling is guard/diagnostic evidence, while
  the generated non-JSON baseline remains the first material target
  (`p1b-samply-mode-2.md:161`-`175`, `:236`-`248`;
  `p1f-results-delta.md:167`-`193`).

## Findings Requiring Fold

1. Capture replay is still parameterized, not verbatim. P1-A labels its
   capture block "Retained capture command shape" and uses `<corpus>`,
   `<track>`, and `<iters>` placeholders (`p1a-samply-mode-1.md:57`-`84`).
   P1-B says it consumes already-present captures and gives "Reproducible
   command shape" with the same placeholders (`p1b-samply-mode-2.md:46`-`82`).
   P1-D's PMU commands are also "parameterized by corpus and mode" with
   `<iters>` placeholders (`p1d-pmu-cycles.md:45`-`67`). The PMU TSVs contain
   exact per-row `iters`, aliases, modes, and output rows, so this is foldable,
   but the replay recipe is not yet one exact manifest or script.

2. Tool versions are incomplete. P1-A and P1-E name `samply 0.13.1` and
   rustc/LLVM (`p1a-samply-mode-1.md:12`-`23`;
   `p1e-hot-leaf-attribution.md:23`-`28`), and P1-D names cargo/rustc/LLVM
   (`p1d-pmu-cycles.md:28`-`30`). P1-B names `samply` and `xcrun xctrace`
   without versions (`p1b-samply-mode-2.md:12`-`14`), and none of P1-A/B/D/E
   records an `xcrun xctrace version` or Xcode/Instruments version. V2 should
   pin those tool identities next to the build command.

3. Run identity is honest but not uniform enough for CH4 acceptance. P1-A has a
   fresh run locator `sk-v12-open:50bd1648:/tmp/skv12-p1`
   (`p1a-samply-mode-1.md:16`-`23`), while P1-C carries the old W0 Criterion run
   id (`p1c-samply-mode-3.md:21`-`31`) and P1-F says every live RESULTS row
   still renders the stale SK-V9 run id (`p1f-results-delta.md:21`-`23`,
   `:208`-`210`). V2 must name a single fresh capture run id for `/tmp/skv12-p1`
   and explicitly distinguish it from the stale RESULTS Criterion run id.

4. Missing xctrace exports are disclosed, but no export/replay policy is folded.
   The packet correctly avoids fresh percentage claims, yet V2 must either add
   exact `xctrace export` commands and retained export paths, or state in every
   P1-A/B/E source list that raw `.trace` bundles are artifact-only and not a
   self-time percentage authority. This matters because SK-V11 V4 CH4 accepted
   samply artifact parameterization only after xctrace summary authority and
   caveats were folded
   (`restart/skinny/tranches/sk-v11/research/p1/hardening/V4/CH4.md:63`-`75`,
   `:82`-`92`).

5. PMU aggregate math is internally inconsistent between P1-D and P1-E. P1-D
   records aggregate cycles/B as parse `2.920217`, direct `4.290305`, typed
   `3.123172` (`p1d-pmu-cycles.md:96`-`100`), while P1-E records parse
   `2.938593`, direct `4.331411`, typed `3.123173`
   (`p1e-hot-leaf-attribution.md:112`-`118`). The row tables appear to use the
   same TSVs, so V2 should either align the aggregation formula or cite why the
   planes differ.

## Required Fold Into V2

- Add a shared `capture_manifest.tsv` or replay script under the P1 research
  packet that enumerates every parse/direct/typed row, corpus alias, mode,
  iters, output paths, CWD, binary path, and command for PMU, samply, Time
  Profiler, and CPU Counters. The manifest may be generated from
  `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`,
  `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`, and
  `/tmp/skv12-p1/pmu/capture_status.tsv`, but it must be cited by P1-A/B/D.
- Add a single shared provenance block to P1-A through P1-F: fresh capture run
  id, capture root, source SHA, target directory, binary paths, exact build
  command, host triple, OS version, `RUSTFLAGS`, rustc/cargo versions,
  `samply --version`, and `xcrun xctrace` / Xcode/Instruments version.
- Preserve the initial product CWD failure as a provenance caveat, but make the
  rerun CWD rule explicit: product `profile_direct` rows must be run from
  `/Users/mkbabb/Programming/bbnf-lang/skinny`; parse `xctrace_probe` rows must
  pass absolute `skinny/test_data/<corpus>.json` paths.
- Preserve the `rc=54` rule: `rc=54` is acceptable only when the log says the
  time limit was reached and the trace bundle was saved; `rc=0` remains
  acceptable for target-exit rows.
- Either export xctrace summaries and cite the export paths, or keep every
  P1-A/B/E hot-leaf percentage claim source-level/PMU-only and explicitly mark
  raw `.trace` bundles as non-percentage authority.
- State the stale `skinny/RESULTS.md` run id in the consolidated P1 fold as a
  stale SK-V11/SK-V9 Criterion identity, separate from the fresh SK-V12
  `/tmp/skv12-p1` capture run id. Do not let downstream consumers cite
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c` as a fresh SK-V12 run.
- Reconcile P1-D and P1-E aggregate cycles/B numbers or cite distinct formulas.

No CH4 REJECT is issued because the artifacts and final status rows exist and
the packet discloses the material reproducibility caveats. The V1 packet should
revise before convergence because a third party still has to reconstruct too
much capture intent from placeholders, TSVs, and logs.

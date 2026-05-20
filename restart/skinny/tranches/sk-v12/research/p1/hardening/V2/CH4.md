# SK-V12 S-P1 Hardening V2 CH4 Cost / Replayability

Disposition: REVISE.

## Findings Requiring Fold

1. Replay is still not a verbatim third-party command set. The controlling CH4
   rule requires every Section 1 method block to carry commands a third party can
   rerun, and absent run id / host triple / build flags fail CH4
   (`restart/prompts/skinny/PASS-1-PROFILE.md:143`-`146`). V1 CH4 specifically
   asked for a shared manifest or replay script enumerating every row, alias,
   mode, iteration count, output path, CWD, binary path, and command
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH4.md:133`-`145`).
   The V2 packet added `skv12-p1-capture-manifest.md`, but the replay blocks
   remain command shapes: P1-A still uses `<corpus>`, `<track>`, and `<iters>`
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:57`-`84`);
   P1-B still uses `<corpus>`, `<iters>`, `<corpus-or-update-center-alias>`, and
   `<mode>` (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:46`-`82`);
   P1-D explicitly says its PMU commands are "parameterized by corpus and mode"
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:47`-`67`);
   and the manifest's product-v2 recipe also uses placeholders
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:90`-`105`).
   The `/tmp` TSVs contain the missing row parameters
   (`/tmp/skv12-p1/pmu/parse_pmu_rows.tsv:1`-`8`,
   `/tmp/skv12-p1/pmu/product_pmu_rows.tsv:1`-`8`), and the status ledger maps
   rows to artifacts (`/tmp/skv12-p1/pmu/capture_status.tsv:1`-`8`), but an
   independent replay still requires reconstructing commands from several files.

2. The samply replay lane remains ambiguous against the S-P1 samply discipline.
   The prompt says samply needs interactive `samply record`, not `--save-only`,
   because `--save-only` loses symbol resolution
   (`restart/prompts/skinny/PASS-1-PROFILE.md:251`-`254`). P1-A and P1-B still
   document `samply record --save-only --unstable-presymbolicate`
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:78`-`83`;
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:53`-`64`).
   This is not a REJECT because the fold correctly shifts self-time percentage
   authority to exported xctrace XML
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:95`-`103`;
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:114`-`128`;
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:72`-`80`).
   V2 still should label samply as retained artifact-only in the replay manifest
   or provide prompt-conforming samply commands.

## Accepted Checks

- Tool and build provenance is now packet-level sufficient. The manifest pins
  macOS, Darwin, rustc, cargo, samply, and xctrace versions
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:18`-`27`)
  and gives the release/native build command from `skinny/`
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:29`-`36`).

- The CWD failure is correctly interpreted and fenced. The final product PMU run
  is from `skinny/`, while the initial repository-root product run is preserved
  as a fixture lookup failure
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:58`-`68`;
  `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:69`-`79`).

- `xctrace rc=54` is handled as retained evidence only when the log records a
  time-limit or target-exit stop plus saved output. The manifest states that rule
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:51`-`59`),
  and representative retained logs show the expected lines
  (`/tmp/skv12-p1/logs/xctrace-time-parse-twitter-track1.log.out:3`,
  `/tmp/skv12-p1/logs/xctrace-time-parse-twitter-track1.log.out:5`,
  `/tmp/skv12-p1/direct-xctrace/time-profiler-v2/update_center__track1.log.out:3`,
  `/tmp/skv12-p1/direct-xctrace/time-profiler-v2/update_center__track1.log.out:5`).

- The product Time Profiler v2 recapture, export policy, and alias fix are now
  citable. The manifest records why product rows were recaptured, the 2s /
  20,000-iteration command shape, and export destinations
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:84`-`116`).
  The v2 status ledger records trace/XML paths and export pass status
  (`/tmp/skv12-p1/product_time_profile_v2_status.tsv:1`-`20`), and the
  `update_center` launch alias is explicit
  (`/tmp/skv12-p1/product_time_profile_v2_alias_fixes.tsv:1`-`3`).

- PMU aggregate arithmetic is reconciled. P1-D and P1-E now agree on weighted
  c/B and CPI for parse, direct, and typed guard lanes
  (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:93`-`100`;
  `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:112`-`122`).
  Recomputing `sum(cycles) / sum(iters * corpus_bytes)` and
  `sum(cycles) / sum(instructions)` from
  `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv` and
  `/tmp/skv12-p1/pmu/product_pmu_rows.tsv` yields parse `2.920217 / 0.204887`,
  direct `4.290305 / 0.183717`, and typed `3.123172 / 0.185056`.

- The artifact paths are sufficient to audit the current profile evidence but
  not sufficient for independent replay without the row-command fold above. The
  retained self-time authorities are named in the manifest
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:118`-`145`)
  and in P1-E sources
  (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:394`-`409`);
  the missing piece is a single replay surface that binds those paths back to
  exact commands.

## Required Fold

Add one repo-tracked replay TSV or script for the S-P1 packet. It should enumerate
each parse/direct/typed PMU, samply, Time Profiler, CPU Counter, and export row
with CWD, full command, corpus, alias, mode, iteration count, binary path,
expected rc policy, and output artifact path. Keep the existing `/tmp` evidence
and manifest; the new replay surface should remove placeholder substitution from
CH4 review.

# SK-V12 S-P1 PIN-V3 CH1 - Correctness

Verdict: REVISE

Score: 94%

## Blocking Findings

1. The tracked pin replay ledger still has two noncanonical `update-center`
   corpus keys.

   PIN-V2 claimed the `update_center` key was normalized in
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`, and
   the status authorities do use `update_center`. However, the tracked replay
   ledger still records `update-center` in the corpus column for the two PMU
   parse rows:

   - `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:66`
     has `pmu-parse / parse / update-center / track1`.
   - `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:67`
     has `pmu-parse / parse / update-center / track2`.

   The corresponding authority rows in
   `/tmp/skv12-pin-p1/pmu/capture_status.tsv:66-67` use
   `update_center`, and all samply/xctrace `update_center` rows are already
   normalized. The command operand `test_data/update-center.json` is fine as a
   file/launch alias; the defect is only the replay ledger's fourth-column
   corpus key. Because the manifest makes the tracked TSV part of replay
   authority, the replay-ledger schema is not fully normalized yet.

## Nonblocking Notes

- The specific samply-parse PIN-V2 defect is fixed: all 34 `samply-parse`
  rows now use canonical `track1`/`track2` modes, and the two samply
  `update_center` rows at replay lines 148-149 use the status-file corpus key.
- Replay mode validation passes across the whole tracked ledger: 458 data rows,
  0 noncanonical modes, with lane counts 82 PMU, 82 samply, 212 xctrace
  capture, and 82 xctrace export.
- PMU, samply, and xctrace status counts recheck: PMU 82/82 PASS
  (`pmu-parse` 34, `pmu-direct` 34, `pmu-typed` 14), samply 82/82 PASS
  (`samply-parse` 34, `samply-direct` 34, `samply-typed` 14), and xctrace
  212/212 PASS with 82 primary Time Profiler rows, 82 CPU Counter rows, and
  48 product-v2 Time Profiler rows.
- The xctrace `rc=54` policy now cites stdout correctly. All 185 `rc=54` rows
  have `Output file saved as` plus an accepted stop condition in the stdout
  path recorded by `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`.
- XML export semantics are correct: `/tmp/skv12-pin-p1/time_profile_export_status.tsv`
  has 82 `SKIP` rows because nonzero XML files already existed; I found 0
  missing or empty export paths.
- Hot-leaf anchors remain acceptable for JSON evidence: summary has 82 rows,
  details has 410 rows, and the load-bearing source fields have 0 `:0`,
  `unknown`, or `none` anchors.
- Capture-source wording is materially fixed. The P1-A/P1-B/P1-D/P1-F headers
  now identify `cf7848b2` as the capture source commit rather than the current
  review head, and the manifest records `d4ef80b2` as the PIN-V2 review base.
- CSS L4 absence is bounded correctly: the pin root has no CSS/lightningcss
  artifacts, and `skinny/crates/runtime/src/grammars/` still contains JSON and
  `sheets_witness` modules only, with no generated `css_l4` or
  `css_l4_declaration_values` runtime.

## Exact Fold Edits Required

1. Patch only the corpus column in
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:66-67`
   from `update-center` to `update_center`. Keep the command paths using
   `skinny/test_data/update-center.json`; that is the launch/file alias, not
   the ledger corpus key.

2. Extend the replay sanity check in
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
   to cover corpus-key normalization, not only mode normalization. For example:

   ```bash
   awk -F '\t' 'NR>1 && $4=="update-center" {bad++}
     END{print bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   # 0
   ```

3. After that patch, keep the existing PIN-V2 stdout-backed `rc=54`, capture
   source, export `SKIP`, hot-leaf, PMU/samply/xctrace count, and CSS L4
   absence wording unchanged; those checks passed in this CH1 review.

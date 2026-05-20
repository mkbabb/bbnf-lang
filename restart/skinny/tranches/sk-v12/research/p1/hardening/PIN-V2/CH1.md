# SK-V12 S-P1 PIN-V2 CH1 - Correctness

Verdict: REVISE

Score: 90%

## Blocking Findings

1. The tracked pin replay ledger is malformed for every samply parse row.

   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:68-71`
   makes `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`
   part of the pin replay authority, but rows 118-151 in that TSV put iteration
   counts in the `mode` column instead of `track1` or `track2`. Example:
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:118`
   records `apache_builds` mode `31429` while its artifact path and command are
   the Track 1 samply parse capture; line 119 repeats `31429` for the Track 2
   capture. The authoritative status file has the correct row shape at
   `/tmp/skv12-pin-p1/samply/capture_status.tsv:36-69`, so the captures are not
   missing, but the single tracked replay surface is not schema-correct. The
   same malformed block also uses `update-center` instead of the status-file
   corpus key `update_center` at replay lines 148-149.

2. The xctrace rc=54 acceptance rule points at the wrong log stream.

   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:107-110`
   says rc=54 is accepted only when xctrace `stderr` records the accepted stop
   condition and `Output file saved as`. In the actual pin artifact,
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv:2` has rc=54/PASS for
   `direct/apache_builds/track1`, but the stderr file is empty and the accepted
   stop/save messages are in
   `/tmp/skv12-pin-p1/logs/xctrace-time-profiler-primary-direct-apache_builds-track1.out:1-5`.
   The xctrace PASS rows are materially supported by stdout, not by the manifest
   wording as written.

## Nonblocking Notes

- JSON/profile coverage is otherwise artifact-backed: PMU 82/82 PASS, samply
  82/82 PASS, and xctrace 212/212 PASS. The row splits are parse 34, direct 34,
  and typed 14 for PMU/samply, with xctrace adding 82 CPU Counter rows and 48
  product-v2 Time Profiler rows.
- Product-plane coverage is present: direct covers 17 JSON corpora x Track 1/2,
  and typed covers the 7 real-typed guard corpora x Track 1/2.
- XML export status is now represented honestly as present/nonzero with
  `/tmp/skv12-pin-p1/time_profile_export_status.tsv` recording 82 `SKIP` rows;
  I found 0 missing or empty XML export paths.
- Hot-leaf attribution is acceptable for JSON lanes: summary has 82 rows,
  details has 410 rows, no load-bearing `:0`, `unknown`, or `none` anchors, and
  the referenced source file:line targets exist in the workspace.
- CSS L4 absence is stated as an unprofiled boundary, not as a substitute
  admission claim. P1-C records `unprofiled: no generated Track 1`, P1-D records
  CSS L4 target PMU missing, and P1-F records 0 admitted CSS L4 rows.
- Several lane headers still call `cf7848b2` "current HEAD" even though this
  challenge runs at `d4ef80b2`. The manifest correctly identifies `cf7848b2` as
  the capture source commit, and the intervening changes are docs/profile-fold
  changes, so this is cleanup rather than a blocker.

## Exact Fold Edits Required

1. Regenerate or patch `skv12-p1-pin-replay.tsv:118-151` so the `mode` column is
   `track1`/`track2` for all 34 `samply-parse` rows, matching
   `/tmp/skv12-pin-p1/samply/capture_status.tsv:36-69`. Also normalize the two
   parse `update_center` corpus cells to the status-file key.

2. Add a replay-ledger sanity check to the manifest fold, for example:
   `awk -F '\t' 'NR>1 && $5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ {bad++} END{print bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`
   must print `0`.

3. Change `skv12-p1-capture-manifest.md:107-110` from "xctrace stderr records"
   to "the linked xctrace stdout/stderr logs record" or, more exactly, "stdout
   records the accepted stop condition and `Output file saved as`." Keep the
   xctrace capture-status rows as PASS; the evidence supports them after the log
   stream wording is fixed.

4. Optional cleanup while folding: replace "current HEAD `cf7848b2`" wording in
   lane headers with "capture source/profile binary commit `cf7848b2`"; reserve
   `d4ef80b2` for the post-PIN-V1 documentation review head.

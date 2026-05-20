# SK-V12 S-P1 PIN-V4 CH6 Anti-Paper-Close Review

Verdict: ACCEPT

Score: 96%

## Blocking Findings

None.

Every live S-P1 profile claim I checked is backed by a measured or logged file,
and the remaining absences are explicit rather than paper-closed. The replay
surface is now mechanically clean at review base `1669c5512c0bf694a9591ba4178dc4a3113de16c`;
the prior PIN-V1/PIN-V2/PIN-V3 REVISE folds are reflected in the current
manifest, replay TSV, and P1 documents.

## Nonblocking Notes

- Artifact backing is present. The manifest names the pin capture root,
  build root, binaries, completion stamps, and result-authority boundary
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-29`),
  marks the pre-pin `/tmp/skv12-p1` surface historical only
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:51-53`),
  and enumerates the live replay lanes and tracked 458-row replay ledger
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:55-73`).
  Direct checks found 458 replay data rows, zero missing artifact/status
  fields, and zero missing or empty referenced artifact/status files.

- Status TSVs match the current claims. `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
  has 82 data rows, all `PASS`; `/tmp/skv12-pin-p1/samply/capture_status.tsv`
  has 82 data rows, all `PASS`; `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`
  has 212 data rows, all `PASS`; `/tmp/skv12-pin-p1/time_profile_export_status.tsv`
  has 82 data rows, all `SKIP` for already-existing exports. This matches the
  manifest coverage table
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:115-125`).
  The derived hot-leaf TSVs are also backed: summary 82 rows and details 410
  rows with zero bad source anchors, matching P1-E's validation surface
  (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:19-30`,
  `:97-116`).

- The prior fold blockers are reflected. PIN-V1 required a single pin authority
  surface, honest XML `SKIP` wording, stale missing-artifact cleanup, Track
  1/Track 2 split, and generated-size/O(N) routing; the fold is recorded in
  `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V1/CONSOLIDATED.md:21-35`
  and is now visible in the manifest and P1-A/P1-B/P1-E tables. PIN-V2 required
  canonical replay modes, stdout-backed `rc=54`, and capture-source wording; the
  fold is recorded in
  `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V2/CONSOLIDATED.md:21-35`
  and the current checks returned `bad_modes 0` plus `rc54 185 bad 0`. PIN-V3
  required normalizing the two `update_center` corpus-key cells; the fold is
  recorded in
  `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:21-34`
  and the current replay check returned `bad_update_center_keys 0`.

- CSS L4 is not admitted by JSON profile evidence. The user pin makes CSS L4
  authoritative and raises the close bar to generated CSS L4 Track 1 greater
  than `lightningcss_mbps + 1`
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`,
  `:80-89`). P1-A records no generated CSS L4 runtime and refuses root CSS
  snippets, report fixtures, or lightningcss-only runs as substitutes
  (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:145-158`,
  `:181-183`). P1-C records concrete CSS blockers and requires generated Track
  1 plus strict lightningcss comparator/equality before CSS hot-leaf, Mode III,
  or SOTA claims become measurable
  (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:96-110`,
  `:116-127`). P1-F records 0 admitted CSS L4 rows and says JSON rows cannot
  populate the lightningcss bar
  (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:19-33`,
  `:80-93`, `:205-210`).

- Missing CSS L4 is explicitly routed, not deferred as prose. `HANDOFF.md`
  requires S-P2/S-P3 re-derivation under the pin, exact CSS row selection,
  generated Track 1 path, oracle/comparator/equality/benchmark/gate commands,
  `GrammarConfig`, generated-size checks, and O(N) guard
  (`restart/skinny/tranches/sk-v12/HANDOFF.md:103-128`). The CSS telemetry
  binding consumes comparator evidence, strict equality, profile/benchmark
  artifacts, generated LOC/module size, O(N) status, JSON guard state, and gate
  status, and missing lightningcss evidence or stale run ids fail closed
  (`restart/skinny/tranches/sk-v12/HANDOFF.md:142-155`). That satisfies the
  anti-paper-close contract in `restart/prompts/ORCHESTRATOR.md:112-120` and
  `restart/prompts/skinny/PASS-1-PROFILE.md:155-160`.

- S-P1 does not substitute prose for profile TSVs. P1-D derives PMU aggregates
  from `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` and
  `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv`, refuses branch/L1/LLC inference,
  and states the values move no RESULTS row
  (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:17-36`,
  `:67-80`, `:228-231`). P1-E uses the xctrace-derived summary/detail TSVs as
  hot-leaf authority, preserves raw symbol/source anchors, and keeps family
  labels from replacing symbol evidence
  (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:118-147`,
  `:159-178`). P1-B preserves the Track 1/Track 2 product split and keeps JSON
  product rows guard/diagnostic only
  (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:39-50`,
  `:160-168`, `:187-195`).

- One caution remains nonblocking: the samply replay commands are
  `samply record --save-only --unstable-presymbolicate` in the tracked ledger
  (for example,
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:84-85`,
  `:148-149`), while the S-P1 prompt warns CH6 not to rely on `--save-only` for
  symbol resolution (`restart/prompts/skinny/PASS-1-PROFILE.md:155-160`,
  `:251-254`). I do not count this as a blocker because the current load-bearing
  self-time authority is the xctrace-derived TSV set, and samply is described as
  companion evidence (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:12-13`,
  `:86-88`). Any future fold that promotes samply to source-symbol authority
  should rerun or replace that lane with an interactive symbol-resolving capture.

- Commands run for this CH6 review included:

  ```bash
  git status --short && git rev-parse HEAD
  nl -ba restart/prompts/ORCHESTRATOR.md | sed -n '104,125p'
  nl -ba restart/prompts/skinny/PASS-1-PROFILE.md | sed -n '1,285p'
  nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md | sed -n '1,180p'
  nl -ba restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md
  nl -ba restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V1/CONSOLIDATED.md
  nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V2/CONSOLIDATED.md
  nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md
  for f in /tmp/skv12-pin-p1/pmu/capture_status.tsv /tmp/skv12-pin-p1/samply/capture_status.tsv /tmp/skv12-pin-p1/xctrace/capture_status.tsv /tmp/skv12-pin-p1/time_profile_export_status.tsv /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv; do if [ -f "$f" ]; then printf '%s\t' "$f"; wc -l < "$f"; else printf 'MISSING\t%s\n' "$f"; fi; done
  awk -F '\t' 'NR>1{total++; if($7=="" || $8=="") missing++; else {cmd="test -s \"" $7 "\""; if(system(cmd)!=0){bad_art[$7]++}; cmd2="test -s \"" $8 "\""; if(system(cmd2)!=0){bad_status[$8]++}}} END{print "rows",total,"missing_cols",missing+0,"bad_artifacts",length(bad_art),"bad_status_artifacts",length(bad_status)}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  awk -F '\t' 'NR>1 && $5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ {bad++} END{print "bad_modes",bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  awk -F '\t' 'NR>1 && $4=="update-center" {bad++} END{print "bad_update_center_keys",bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  awk -F '\t' 'NR>1{n++; if($7!="PASS") print "BAD",NR,$0} END{print "pmu_rows",n}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
  awk -F '\t' 'NR>1{n++; if($7!="PASS") print "BAD",NR,$0} END{print "samply_rows",n}' /tmp/skv12-pin-p1/samply/capture_status.tsv
  awk -F '\t' 'NR>1{n++; if($7!="PASS") print "BAD",NR,$0} END{print "xctrace_rows",n}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
  awk -F '\t' 'NR>1{n++; if($4!="SKIP") print "BAD",NR,$0} END{print "export_rows",n}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
  awk -F '\t' 'NR>1 && $6==54 {n++; f=$9; cmd="rg -q '\''Output file saved as'\'' \"" f "\" && rg -q '\''Reached specified time limit|Target app exited'\'' \"" f "\""; if(system(cmd)!=0){bad++; print "BAD",NR,f}} END{print "rc54",n,"bad",bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
  awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++} END{print "summary_rows",n,"bad",bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
  awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++} END{print "detail_rows",n,"bad",bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
  rg -n "CSS L4|lightningcss|admission|unprofiled|Mode III|route|profile TSV|/tmp/skv12-pin-p1" restart/skinny/tranches/sk-v12/research/p1/*.md restart/skinny/tranches/sk-v12/HANDOFF.md
  ```

## Exact Fold Edits If REVISE

N/A. Verdict is ACCEPT.

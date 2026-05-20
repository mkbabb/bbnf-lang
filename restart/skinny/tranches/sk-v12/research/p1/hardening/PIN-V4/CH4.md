# SK-V12 S-P1 PIN-V4 CH4 Cost / Replayability Review

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

- Review base and worktree: `git rev-parse HEAD` returned
  `1669c5512c0bf694a9591ba4178dc4a3113de16c`; `git status --short` was empty
  before this output write. The review followed the S-P1 convergence and CH4
  cost contract: CHALLENGE cycles fold until the section 3Z criterion is met
  (`restart/prompts/ORCHESTRATOR.md:104-121`), and CH4 fails only when run id,
  host triple, build flags, or rerunnable commands are absent
  (`restart/prompts/skinny/PASS-1-PROFILE.md:143-146`). The packet supplies
  capture identity, host/tool versions, and the build command in
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-49`.

- Replay row counts are coherent and match the manifest. I ran:

  ```bash
  awk -F '\t' 'NR==1{print "pin replay header:", $0; next} {total++; lane[$1]++; fam[$1"/"$2]++; mode[$5]++; if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) badmode++; if($4=="update-center") badcorpus++; if($7=="" || $8=="" || $9=="") missing++} END{print "total", total; print "badmode", badmode+0; print "badcorpus", badcorpus+0; print "missing_path_or_command", missing+0; for(k in lane) print "lane", k, lane[k]; for(k in fam) print "family", k, fam[k]; for(k in mode) print "mode", k, mode[k]}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv | sort
  ```

  Output showed `total 458`, `badmode 0`, `badcorpus 0`,
  `missing_path_or_command 0`; lane counts were PMU 82, samply 82,
  xctrace CPU Counters 82, xctrace primary Time Profiler 82, xctrace product-v2
  Time Profiler 48, and xctrace export 82. This matches the manifest's replay
  ledger claim (`skv12-p1-capture-manifest.md:60-73`). The PIN-V3 fold's
  remaining `update_center` issue is resolved: representative normalized rows
  are now `corpus=update_center` while the command operand still uses
  `test_data/update-center.json` where needed
  (`skv12-p1-pin-replay.tsv:66-67`, `:148-149`, `:230-231`, `:312-313`,
  `:408-409`, `:454-457`), matching the required PIN-V3 fold
  (`hardening/PIN-V3/CONSOLIDATED.md:21-34`).

- Profiler lane status coverage is complete for the admitted pin surface. I ran:

  ```bash
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++; by[$2"/"$7]++} END{print "pmu", total, bad+0; for(k in by) print k, by[k]}' /tmp/skv12-pin-p1/pmu/capture_status.tsv | sort
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++; by[$2"/"$7]++} END{print "samply", total, bad+0; for(k in by) print k, by[k]}' /tmp/skv12-pin-p1/samply/capture_status.tsv | sort
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++; by[$2"/"$7]++; rc[$6]++} END{print "xctrace", total, bad+0; for(k in by) print k, by[k]; for(k in rc) print "rc", k, rc[k]}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | sort
  ```

  Output showed PMU `82 0`, samply `82 0`, xctrace `212 0`, with rc counts
  `rc 0 27` and `rc 54 185`. The family split was 34 parse, 34 direct, and
  14 typed for PMU/samply; xctrace had the same 82 primary Time Profiler rows,
  the same 82 CPU Counters rows, plus 48 product-v2 Time Profiler rows. The
  manifest's coverage table records the same PASS surface
  (`skv12-p1-capture-manifest.md:117-125`). Mode III and CSS L4 remain explicit
  absence boundaries, not hidden replay assumptions
  (`skv12-p1-capture-manifest.md:172-177`).

- Command, log, and status paths are not paper references. I ran:

  ```bash
  find /tmp/skv12-pin-p1 -type f \( -name '*status.tsv' -o -name 'capture_status.tsv' -o -name 'done.txt' -o -name '*commands.sh' -o -name 'time_profile_hot_leaf_*.tsv' \) -print | sort
  wc -l /tmp/skv12-pin-p1/pmu/pmu-commands.sh /tmp/skv12-pin-p1/samply/samply-commands.sh
  awk -F '\t' 'NR>1{print $7; print $8}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv | sort -u | while IFS= read -r p; do [ -e "$p" ] || printf 'missing\t%s\n' "$p"; done | awk 'BEGIN{n=0} {n++; print} END{print "missing_replay_artifact_or_status_paths", n+0}'
  ```

  The status and command files were present; both command ledgers had 82 lines;
  and the replay ledger reported `missing_replay_artifact_or_status_paths 0`.
  PMU/samply/xctrace done stamps also exist and match the manifest timestamps
  (`skv12-p1-capture-manifest.md:21-27`). The manifest points to the command and
  status authority paths by lane (`skv12-p1-capture-manifest.md:62-68`), while
  P1-D records PMU provenance, target directory, binaries, toolchain, and host
  OS (`p1d-pmu-cycles.md:15-25`).

- The PIN-V2 `rc=54` issue is closed by stdout-backed acceptance. The manifest
  states that `rc=54` is accepted only when the stdout path in
  `capture_status.tsv` contains an accepted stop condition and `Output file
  saved as` (`skv12-p1-capture-manifest.md:109-113`), and PIN-V2 required that
  exact correction (`hardening/PIN-V2/CONSOLIDATED.md:29-35`). I reran:

  ```bash
  awk -F '\t' 'NR>1 && $6==54 {print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | while IFS= read -r f; do if rg -q 'Output file saved as' "$f" && rg -q 'Reached specified time limit|Target app exited' "$f"; then printf 'ok\n'; else printf 'bad\t%s\n' "$f"; fi; done | awk 'BEGIN{ok=0; bad=0} $1=="ok"{ok++} $1=="bad"{bad++} END{print "rc54_ok", ok+0, "rc54_bad", bad+0}'
  ```

  Output was `rc54_ok 185 rc54_bad 0`.

- XML export SKIP semantics are now replayable and correctly named. PIN-V1
  required replacing false `PASS` wording with present/nonzero XML plus `SKIP`
  status for already-existing exports (`hardening/PIN-V1/CONSOLIDATED.md:21-35`).
  The manifest now names that policy (`skv12-p1-capture-manifest.md:67`,
  `:124`, `:159-161`), and P1-E preserves it as valid self-time input
  (`p1e-hot-leaf-attribution.md:84-90`). I ran:

  ```bash
  awk -F '\t' 'NR>1{n++; if($4!="SKIP") bad++; if($3!=0) badrc++; if(system("test -s " q $2 q)!=0) missing_export++; if(system("test -e " q $1 q)!=0) missing_trace++} BEGIN{q=sprintf("%c",39)} END{print "export_rows", n+0, "non_skip", bad+0, "nonzero_rc", badrc+0, "missing_or_empty_xml", missing_export+0, "missing_trace", missing_trace+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
  ```

  Output was `export_rows 82 non_skip 0 nonzero_rc 0 missing_or_empty_xml 0
  missing_trace 0`.

- Hot-leaf source anchors are concrete for the admitted self-time tables. The
  packet records 82 summary rows and 410 detail rows with no `:0`, `unknown`, or
  `none` anchors (`skv12-p1-capture-manifest.md:163-169`;
  `p1e-hot-leaf-attribution.md:21-30`). I reran:

  ```bash
  awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none" || $16=="" || $15=="") bad++} END{print "summary_rows", n+0, "bad_source_anchor", bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
  awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none" || $9=="" || $8=="") bad++} END{print "detail_rows", n+0, "bad_source_anchor", bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
  ```

  Output was `summary_rows 82 bad_source_anchor 0` and `detail_rows 410
  bad_source_anchor 0`.

- The packet is replayable without hidden local assumptions for this workspace:
  it declares the capture source commit, `/tmp` capture root, `/tmp` build root,
  binaries, host/tool versions, build command, historical pre-pin boundary, and
  per-lane replay authorities (`skv12-p1-capture-manifest.md:8-73`). The commands
  are absolute to this checkout and `/tmp` root, but those paths are disclosed
  and backed by tracked replay rows plus live status TSVs; I found no remaining
  unstated dependency beyond the declared macOS/xctrace/samply/Rust host stack.

## Exact Fold Edits If REVISE

None. Verdict is ACCEPT.

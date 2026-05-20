# SK-V12 S-P1 PIN-V7 CH4 Cost / Replayability

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

1. Review base and CH4 contract are pinned. Before writing this assigned output,
   `git rev-parse HEAD` returned
   `d4a7e3e3f19482688fa42dd1be9cf584f6c3d19b`, and `git status --short` had no
   output. CH4 rejects missing reproducibility inputs: rerun commands, run id,
   host triple, or build flags (`restart/prompts/skinny/PASS-1-PROFILE.md:143-146`).
   S-P1 still converges only after two consecutive clean challenge cycles
   (`restart/prompts/ORCHESTRATOR.md:118-123`;
   `restart/prompts/skinny/PASS-1-PROFILE.md:177-180`). PIN-V6 was the first
   clean cycle after the PIN-V5 reset, and PIN-V7 is the potential second
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V6/CONSOLIDATED.md:19-20`,
   `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V6/CONSOLIDATED.md:40-43`).

2. Replay identity is explicit and pin-era. The capture manifest records
   capture source `cf7848b2`, committed S-P1 fold `b1043383`, capture root
   `/tmp/skv12-pin-p1`, build root `/tmp/skv12-pin-profile-target-cf7848b2`,
   the two binaries, host/tool versions, and the exact cargo build command
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-49`).
   The same manifest states that the pre-pin manifest/replay surface is
   historical only, not pin-era replay authority
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:51-52`).
   I ran:

   ```sh
   test -x /tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe &&
     test -x /tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct
   ```

   Result: `binaries executable`.

3. Replay row counts and lane coverage match the manifest. The manifest declares
   458 pin-era command rows: 82 PMU, 82 samply, 212 xctrace capture, and 82
   xctrace export rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:69-72`),
   and summarizes lane coverage/status at
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:114-124`.
   I ran:

   ```sh
   wc -l restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv \
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   awk -F '\t' 'NR==1{print "header_fields",NF; next}
     {rows++; lane[$1]++; family[$2]++; if(NF!=10) bad_nf++;
      if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) bad_mode++;
      if($4=="update-center") bad_corpus++}
     END{print "rows",rows; print "bad_nf",bad_nf+0;
       print "bad_mode",bad_mode+0; print "bad_corpus_update-center",bad_corpus+0;
       for(k in lane) print "lane",k,lane[k]; for(k in family) print "family",k,family[k]}' \
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   ```

   Results: old pre-pin replay TSV has 507 lines; pin replay TSV has 459 lines
   (458 data rows). The pin ledger has 10 header fields, `bad_nf=0`,
   `bad_mode=0`, `bad_corpus_update-center=0`, and lane counts of `pmu 82`,
   `samply 82`, `xctrace-time-profiler-primary 82`, `xctrace-cpu-counters 82`,
   `xctrace-time-profiler-product-v2 48`, and `xctrace-export 82`.

4. Command, log, artifact, and status paths are present. The tracked replay TSV
   header includes `artifact`, `status_artifact`, and `command`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:1`),
   and representative rows carry exact pin-root commands and status paths
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:2`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:459`).
   The PMU, samply, xctrace, and XML export status schemas expose the expected
   status/log/artifact fields (`/tmp/skv12-pin-p1/pmu/capture_status.tsv:1`,
   `/tmp/skv12-pin-p1/samply/capture_status.tsv:1`,
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv:1`,
   `/tmp/skv12-pin-p1/time_profile_export_status.tsv:1`). I ran:

   ```sh
   awk -F '\t' 'NR>1{n++; if(system("test -e " q $7 q)!=0) missing_artifact++;
     if(system("test -e " q $8 q)!=0) missing_status++}
     BEGIN{q="\047"} END{print "replay_rows",n,"missing_artifact",missing_artifact+0,
       "missing_status",missing_status+0}' \
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   awk -F '\t' 'NR>1{n++; if(system("test -e " q $8 q)!=0) missing_stdout++;
     if(system("test -e " q $9 q)!=0) missing_stderr++}
     BEGIN{q="\047"} END{print "pmu_rows",n,"missing_stdout",missing_stdout+0,
       "missing_stderr",missing_stderr+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
   awk -F '\t' 'NR>1{n++; if(system("test -e " q $8 q)!=0) missing_artifact++}
     BEGIN{q="\047"} END{print "samply_rows",n,"missing_artifact",missing_artifact+0}' \
     /tmp/skv12-pin-p1/samply/capture_status.tsv
   awk -F '\t' 'NR>1{n++; if(system("test -e " q $8 q)!=0) missing_artifact++;
     if(system("test -e " q $9 q)!=0) missing_stdout++;
     if(system("test -e " q $10 q)!=0) missing_stderr++}
     BEGIN{q="\047"} END{print "xctrace_rows",n,"missing_artifact",missing_artifact+0,
       "missing_stdout",missing_stdout+0,"missing_stderr",missing_stderr+0}' \
     /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   ```

   Results: replay `458 0 0`, PMU `82 0 0`, samply `82 0`, and xctrace
   `212 0 0 0`. The PMU and samply command ledgers each have 82 lines.

5. Profiler status rows are complete for the replayable pin surface. The capture
   manifest and P1 artifacts agree that PMU is 82/82 PASS, samply is 82/82 PASS,
   xctrace is 212/212 PASS, and Mode III/CSS L4 are disclosed absence boundaries
   rather than hidden local assumptions
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:116-124`;
   `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:16-21`;
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:15-17`;
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:39-62`;
   `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:54-76`). I ran:

   ```sh
   for f in /tmp/skv12-pin-p1/pmu/capture_status.tsv \
     /tmp/skv12-pin-p1/samply/capture_status.tsv \
     /tmp/skv12-pin-p1/xctrace/capture_status.tsv \
     /tmp/skv12-pin-p1/time_profile_export_status.tsv \
     /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv \
     /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv; do
       printf '%s\t' "$f"
       awk -F '\t' 'NR==1{h=NF; next} {n++; if(NF!=h) bad++}
         END{printf "rows=%d fields=%d bad_nf=%d\n", n,h,bad+0}' "$f"
     done
   awk -F '\t' 'NR>1{total++; by_status[$7]++; by_rc[$6]++; by_family[$1]++}
     END{print "total",total; for(k in by_status) print "status",k,by_status[k];
       for(k in by_rc) print "rc",k,by_rc[k]; for(k in by_family) print "lane",k,by_family[k]}' \
     /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   ```

   Results: PMU `rows=82 fields=9 bad_nf=0`; samply `rows=82 fields=8 bad_nf=0`;
   xctrace `rows=212 fields=10 bad_nf=0`; export `rows=82 fields=4 bad_nf=0`;
   hot-leaf summary `rows=82 fields=16 bad_nf=0`; hot-leaf details
   `rows=410 fields=9 bad_nf=0`. Xctrace is `212 PASS`, split as CPU Counters
   82, primary Time Profiler 82, product-v2 Time Profiler 48, with `rc 54 185`
   and `rc 0 27`.

6. `rc=54` acceptance is stdout-backed. The manifest allows `rc=54` only when
   the captured xctrace log stream records both an accepted stop condition and
   `Output file saved as`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:108-112`).
   A representative xctrace status row records stdout/stderr paths at
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv:2`. I ran:

   ```sh
   awk -F '\t' 'NR>1 && $6==54 {print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv |
     while IFS= read -r f; do
       if [ -s "$f" ] &&
          rg -q 'Output file saved as' "$f" &&
          rg -q 'Reached specified time limit|Target app exited' "$f"; then
         echo ok
       else
         echo bad:"$f"
       fi
     done | sort | uniq -c
   ```

   Result: `185 ok`.

7. XML export `SKIP` semantics are honest. The manifest says the XML export rows
   are already-present nonzero files and therefore stay `SKIP`, not relabeled as
   `PASS`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:66-67`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:158-160`).
   P1-E repeats the same boundary
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:83-90`).
   I ran:

   ```sh
   awk -F '\t' 'NR>1{total++; status[$4]++; if($4!="SKIP") bad_status++;
     if(system("test -s " q $2 q) != 0) bad_file++}
     BEGIN{q="\047"} END{print "total",total; for(k in status) print "status",k,status[k];
       print "bad_status",bad_status+0; print "missing_or_empty_export",bad_file+0}' \
     /tmp/skv12-pin-p1/time_profile_export_status.tsv
   ```

   Result: `total 82`, `status SKIP 82`, `bad_status 0`, `missing_or_empty_export 0`.

8. Hot-leaf source anchors are concrete. P1-E names the xctrace-derived summary
   and detail tables as authority and validates 82/82 summary rows plus 410/410
   detail rows with no `:0`, `unknown`, or `none` source anchors
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:21-30`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:96-111`).
   The manifest carries the same validation
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:162-168`).
   I ran:

   ```sh
   for spec in '/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv:16' \
     '/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv:9'; do
       f=${spec%:*}; col=${spec#*:}
       awk -F '\t' -v col="$col" 'NR>1{gsub(/\r/,"",$col); print $col}' "$f"
     done |
     awk -F ':' 'BEGIN{q="\047"} {line=$NF; path=$0; sub(":" line "$","",path); n++;
       if(path=="" || line !~ /^[0-9]+$/ || line==0){badfmt++; next}
       if(system("test -f " q path q)!=0){missing++; next}
       if(system("test $(wc -l < " q path q ") -ge " line)!=0){badline++}}
       END{print "anchors",n; print "bad_format",badfmt+0;
         print "missing_files",missing+0; print "line_past_eof",badline+0}'
   ```

   Result: `anchors 492`, `bad_format 0`, `missing_files 0`, `line_past_eof 0`.

9. The live packet is replayable without stale pre-pin authority. The current
   authority/status documents point to the pin replay TSV, pin capture root, pin
   build root, and pin self-time tables
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:27-48`;
   `restart/skinny/tranches/sk-v12/SPEC.md:16-23`;
   `restart/skinny/tranches/sk-v12/SPEC.md:350-354`). PIN-V5 folded the stale
   pre-pin authority blocker
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:22-37`),
   and PIN-V6 rechecked it as folded
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V6/CONSOLIDATED.md:22-35`).
   I ran:

   ```sh
   rg -n 'skv12-p1-replay\.tsv|/tmp/skv12-profile-target|/tmp/skv12-p1(/|$)|/tmp/skv12-p1-[^f]|50bd1648' \
     restart/skinny/tranches/sk-v12/SPEC.md \
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md \
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv \
     restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md
   ```

   Result: no matches. Broader P1 markdown still contains explicitly historical
   or diagnostic references, for example the W0 Criterion-only Mode III boundary
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:64-72`)
   and pre-pin comparison notes
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:217-222`),
   but those are not live replay authority. One minor cycle-age wording remains:
   `HARDENING-S-P1-CONVERGED.md` still says PIN-V5 is in review
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:19-21`).
   That is not a CH4 replay blocker because PIN-V6 consolidated state routes the
   current cycle to PIN-V7 and no replay path depends on that sentence.

## Exact Fold Edits If REVISE

None; ACCEPT.

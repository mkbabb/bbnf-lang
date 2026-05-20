# SK-V12 S-P1 PIN-V6 CH4 Cost / Replayability

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

1. Review base and CH4 contract are pinned. I reviewed `HEAD f3e68a43`
   (`docs(sk-v12-p1-hardening): fold PIN-V5 stale profile authority rejection`);
   `git status --short` was clean before this assigned output was written. CH4
   rejects missing reproducibility inputs: commands, run id, host triple, and
   build flags (`restart/prompts/skinny/PASS-1-PROFILE.md:143-146`). S-P1 still
   advances only under the two-clean-cycle rule in `ORCHESTRATOR.md` §3Z
   (`restart/prompts/ORCHESTRATOR.md:118-121`) and the fold rule
   (`restart/prompts/ORCHESTRATOR.md:112-117`).

2. The PIN-V5 stale-authority blocker is folded. `HARDENING-S-P1-CONVERGED.md`
   now marks pre-pin convergence historical only and not live authority
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:9-24`),
   then names the pin capture source `cf7848b2`, capture root
   `/tmp/skv12-pin-p1`, build root
   `/tmp/skv12-pin-profile-target-cf7848b2`, pin replay TSV, status TSVs, and
   xctrace self-time TSVs as the authority surface
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:27-48`).
   `SPEC.md` likewise points W0 at the pin source/root/build/replay/self-time
   surface, not the old profile root
   (`restart/skinny/tranches/sk-v12/SPEC.md:350-354`). I ran:

   ```sh
   rg -n "(/tmp/skv12-p1([^a-zA-Z0-9_-]|$)|/tmp/skv12-profile-target-50bd1648|skv12-p1-replay.tsv)" \
     restart/skinny/tranches/sk-v12/SPEC.md \
     restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md \
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md \
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   ```

   It returned no matches. Historic hardening files and the old pre-pin replay
   TSV still exist as history/context, but the live replay packet does not route
   through them.

3. Replay row counts and lane coverage match the manifest. The capture manifest
   declares 458 pin-era command rows: 82 PMU, 82 samply, 212 xctrace capture,
   and 82 xctrace export rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:69-72`),
   with lane coverage/status summarized at
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:114-124`.
   I ran:

   ```sh
   awk -F '\t' 'NR==1{h=NF; next} {rows++; if(NF!=h) bad_nf++; lane[$1]++; family[$2]++; mode[$5]++; if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) bad_mode++; if($4=="update-center") bad_corpus++; if($0 ~ /\/tmp\/skv12-p1([^a-zA-Z0-9_-]|$)|\/tmp\/skv12-profile-target-50bd1648|skv12-p1-replay.tsv/) stale++; if($7=="" || $8=="" || $9=="") missing_cols++} END{print h, rows, bad_nf+0, bad_mode+0, bad_corpus+0, stale+0, missing_cols+0; for(k in lane) print k, lane[k]; for(k in family) print k, family[k]; for(k in mode) print k, mode[k]}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   ```

   Result: 10 header fields, 458 rows, `bad_nf=0`, `bad_mode=0`,
   `bad_corpus=0`, `stale_refs=0`, `missing_cols=0`; lanes were `pmu 82`,
   `samply 82`, `xctrace-time-profiler-primary 82`,
   `xctrace-cpu-counters 82`, `xctrace-time-profiler-product-v2 48`, and
   `xctrace-export 82`.

4. Command, log, artifact, and status paths are present. The replay ledger
   header carries `artifact`, `status_artifact`, and `command`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:1`);
   representative rows show executable pin-root commands and status paths
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:2-10`).
   I ran:

   ```sh
   awk -F '\t' 'NR>1{n++; if(system("test -e \"" $7 "\"")!=0) missing_artifact++; if(system("test -e \"" $8 "\"")!=0) missing_status++} END{print n+0, missing_artifact+0, missing_status+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   awk -F '\t' 'NR>1{n++; if(system("test -e \"" $8 "\"")!=0) missing_stdout++; if(system("test -e \"" $9 "\"")!=0) missing_stderr++} END{print n+0, missing_stdout+0, missing_stderr+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
   awk -F '\t' 'NR>1{n++; if(system("test -e \"" $8 "\"")!=0) missing_artifact++} END{print n+0, missing_artifact+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
   awk -F '\t' 'NR>1{n++; if(system("test -e \"" $8 "\"")!=0) missing_artifact++; if(system("test -e \"" $9 "\"")!=0) missing_stdout++; if(system("test -e \"" $10 "\"")!=0) missing_stderr++} END{print n+0, missing_artifact+0, missing_stdout+0, missing_stderr+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   ```

   Results: replay `458 0 0`, PMU `82 0 0`, samply `82 0`, xctrace
   `212 0 0 0`. The two pin binaries are executable:
   `/tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe` and
   `/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct`, matching
   the manifest's binary/build-command lines
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:18-49`).

5. Profiler status rows are complete. PMU, samply, and xctrace status TSV
   schemas expose the expected status/log/artifact columns
   (`/tmp/skv12-pin-p1/pmu/capture_status.tsv:1`,
   `/tmp/skv12-pin-p1/samply/capture_status.tsv:1`,
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv:1`). I ran:

   ```sh
   awk -F '\t' 'NR>1{n++; status[$7]++; family[$2]++; rc[$6]++} END{print n+0; for(k in status) print k,status[k]; for(k in family) print k,family[k]; for(k in rc) print k,rc[k]}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
   awk -F '\t' 'NR>1{n++; status[$7]++; family[$2]++; rc[$6]++} END{print n+0; for(k in status) print k,status[k]; for(k in family) print k,family[k]; for(k in rc) print k,rc[k]}' /tmp/skv12-pin-p1/samply/capture_status.tsv
   awk -F '\t' 'NR>1{n++; status[$7]++; lane[$1]++; family[$2]++; rc[$6]++} END{print n+0; for(k in status) print k,status[k]; for(k in lane) print k,lane[k]; for(k in family) print k,family[k]; for(k in rc) print k,rc[k]}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   ```

   Results: PMU `82 PASS` with `rc 0 82`; samply `82 PASS` with `rc 0 82`;
   xctrace `212 PASS`, split as primary Time Profiler 82, CPU Counters 82,
   product-v2 Time Profiler 48, with `rc 54 185` and `rc 0 27`.

6. `rc=54` acceptance is stdout-backed. The manifest says `rc=54` is accepted
   only when the captured xctrace log stream records an accepted stop condition
   and `Output file saved as`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:108-112`).
   A representative status row points stdout to
   `/tmp/skv12-pin-p1/logs/xctrace-time-profiler-primary-direct-apache_builds-track1.out`
   (`/tmp/skv12-pin-p1/xctrace/capture_status.tsv:2`), whose stdout records both
   the time-limit stop and saved output
   (`/tmp/skv12-pin-p1/logs/xctrace-time-profiler-primary-direct-apache_builds-track1.out:3-5`).
   I ran:

   ```sh
   awk -F '\t' 'NR>1 && $6==54 {n++; f=$9; cmd="rg -q '\''Output file saved as'\'' \"" f "\""; cmd2="rg -q '\''Reached specified time limit|Target app exited'\'' \"" f "\""; if(system(cmd)!=0 || system(cmd2)!=0){bad++; print "BAD", NR, f}} END{print "rc54", n+0, "bad", bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   ```

   Result: `rc54 185 bad 0`.

7. XML export `SKIP` semantics are honest. The manifest states the export rows
   are `SKIP` because nonzero XML exports already existed
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:66-67`),
   and P1-E explicitly says not to relabel those as PASS
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:83-90`).
   The export status schema is `trace export rc status`
   (`/tmp/skv12-pin-p1/time_profile_export_status.tsv:1`). I ran:

   ```sh
   awk -F '\t' 'NR>1{n++; status[$4]++; if($3!=0) bad_rc++; if(system("test -e \"" $1 "\"")!=0) missing_trace++; if(system("test -s \"" $2 "\"")!=0) missing_xml++} END{print n+0, bad_rc+0, missing_trace+0, missing_xml+0; for(k in status) print k,status[k]}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
   ```

   Result: `82 0 0 0` and `SKIP 82`.

8. Hot-leaf source anchors are concrete and replayable from xctrace XML-derived
   tables. The manifest and P1-E both name the summary/detail row counts and
   source-anchor validation
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:162-168`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:21-30`).
   Representative summary/detail rows carry symbol plus file:line anchors
   (`/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv:2-8`,
   `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv:2-8`). I ran:

   ```sh
   awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none" || $15=="" || $16=="") bad++} END{print n+0, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
   awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none" || $8=="" || $9=="") bad++} END{print n+0, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
   awk -F '\t' 'NR>1{n++; field=$16; gsub(/\r/,"",field); source=field; sub(/:[0-9]+$/, "", source); line=field; sub(/^.*:/, "", line); if(source=="" || line !~ /^[1-9][0-9]*$/) bad++; else if(system("test -f \"" source "\"")!=0) missing++} END{print n+0, bad+0, missing+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
   awk -F '\t' 'NR>1{n++; field=$9; gsub(/\r/,"",field); source=field; sub(/:[0-9]+$/, "", source); line=field; sub(/^.*:/, "", line); if(source=="" || line !~ /^[1-9][0-9]*$/) bad++; else if(system("test -f \"" source "\"")!=0) missing++} END{print n+0, bad+0, missing+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
   ```

   Results: summary `82 0`, detail `410 0`; source-file checks returned
   `82 0 0` and `410 0 0`.

9. CSS L4 and Mode III absence boundaries remain explicit, not stale replay
   holes. The manifest records Mode III absent and CSS L4 unprofiled until a
   generated CSS L4 Track 1 runtime, lightningcss same-plane comparator, and
   strict equality oracle exist
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:171-176`).
   P1-C independently says the pin root has no fresh Mode III, structural-scan,
   CSS, non-JSON, or sheets command/path authority
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:39-62`,
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:96-110`).
   I ran `find /tmp/skv12-pin-p1 -iname '*css*' -o -iname '*lightning*' -o -iname '*nonjson*'`,
   which returned no paths, and `find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort`,
   which showed only generated JSON and `sheets_witness` grammar files.

## Exact Fold Edits If REVISE

N/A - ACCEPT.

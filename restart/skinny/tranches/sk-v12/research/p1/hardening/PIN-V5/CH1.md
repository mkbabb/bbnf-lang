# SK-V12 S-P1 PIN-V5 CH1 - Correctness

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

1. The governing CH1 and convergence contracts are satisfied for this lens. S-P1
   CH1 asks whether hot-leaf claims have source-backed profiler evidence, c/B
   comes from real PMU counters, JSON profiling coverage is complete, and
   unresolved `unprofiled` cells are not hidden
   (`restart/prompts/skinny/PASS-1-PROFILE.md:123-127`). Convergence requires
   `>=95% ACCEPT` for two consecutive cycles, with zero open critical defects
   and no orphan unresolved REVISE
   (`restart/prompts/ORCHESTRATOR.md:118-121`;
   `restart/prompts/skinny/PASS-1-PROFILE.md:177-180`). PIN-V4 was six ACCEPT,
   zero REVISE, zero REJECT and is recorded as the first consecutive all-ACCEPT
   pin cycle (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/CONSOLIDATED.md:12-20`).
   Therefore this PIN-V5 CH1 ACCEPT can serve as the correctness component of
   the second consecutive all-ACCEPT cycle if the five sibling PIN-V5 lenses are
   also clean and aggregation finds no open critical defect or unresolved REVISE
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/CONSOLIDATED.md:44-47`).

2. Replay schema, lane split, corpus keys, and canonical modes recheck clean.
   The tracked pin replay ledger declares the 10-field schema at
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:1`.
   The manifest says the tracked ledger contains 458 pin-era command rows: 82
   PMU, 82 samply, 212 xctrace capture, and 82 xctrace export rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:70-73`).
   I ran:

   ```bash
   awk -F '\t' '
   NR==1 {print "header", $0; print "header_fields", NF; next}
   {rows++; if(NF!=10) nf_bad++; lane[$1]++; family[$2]++; plane[$3]++;
    mode[$5]++; corpus[$4]++;
    if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) mode_bad[$5]++;
    if($4=="update-center") update_dash++;
    if($4 !~ /^(twitter|citm_catalog|canada|apache_builds|github_events|update_center|mesh|random|gsoc-2018|marine_ik|instruments|numbers|unicode_mixed|unicode_escapes|unicode_basic|distinct_values|y_string_unicode)$/) corpus_bad[$4]++}
   END {print "rows", rows; print "nf_bad", nf_bad+0;
        print "mode_bad_count", length(mode_bad); print "update_dash", update_dash+0;
        print "bad_corpus_keys", length(corpus_bad); print "corpora", length(corpus);
        for (k in lane) print k, lane[k]; for (k in mode) print k, mode[k]}'
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   ```

   Result: `header_fields 10`, `rows 458`, `nf_bad 0`,
   `mode_bad_count 0`, `update_dash 0`, `bad_corpus_keys 0`, `corpora 17`;
   lane counts were `pmu 82`, `samply 82`, `xctrace-time-profiler-primary 82`,
   `xctrace-cpu-counters 82`, `xctrace-time-profiler-product-v2 48`, and
   `xctrace-export 82`. Mode counts were `track1 187`, `track2 187`,
   `real_typed_track1 42`, and `real_typed_track2 42`. The PIN-V3 blocker is
   folded: the former PMU parse rows now use corpus key `update_center` while
   retaining `test_data/update-center.json` only as the file alias
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:66-67`;
   `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:23-34`).

3. Artifact counts and artifact paths match the pinned packet. The manifest
   records PMU 82, samply 82, primary Time Profiler 82, CPU Counters 82,
   product-v2 Time Profiler 48, XML exports 82, and derived hot-leaf tables
   82 summary / 410 detail rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:60-68`,
   `:117-125`). I ran:

   ```bash
   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++; family[$2]++}
     END{print "pmu_total", total, "bad", bad+0; for(k in family) print k, family[k]}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++; family[$2]++}
     END{print "samply_total", total, "bad", bad+0; for(k in family) print k, family[k]}' /tmp/skv12-pin-p1/samply/capture_status.tsv
   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++; family[$2]++; rc[$6]++}
     END{print "xctrace_total", total, "bad", bad+0; for(k in family) print k, family[k]; for(k in rc) print k, rc[k]}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   awk -F '\t' 'NR==1{next} {n++; if($4!="SKIP") bad_status++; if($3!=0) bad_rc++;
     if(system("test -s " q $2 q)!=0) missing++} BEGIN{q="'\''"}
     END{print "exports", n, "bad_status", bad_status+0, "bad_rc", bad_rc+0, "missing_or_empty", missing+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
   awk -F '\t' 'NR>1{n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++}
     END{print "summary", n, "bad", bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
   awk -F '\t' 'NR>1{n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++}
     END{print "details", n, "bad", bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
   awk -F '\t' 'NR>1{n++; if(system("test -e " q $7 q)!=0) missing_artifact++;
     if(system("test -e " q $8 q)!=0) missing_status++} BEGIN{q="'\''"}
     END{print "replay_rows", n, "missing_artifact", missing_artifact+0, "missing_status_artifact", missing_status+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   ```

   Results: PMU `82 bad 0` split 34 parse / 34 direct / 14 typed; samply
   `82 bad 0` split 34 parse / 34 direct / 14 typed; xctrace `212 bad 0`
   split 34 parse / 34 direct / 14 typed for Time Profiler, 34 parse / 34
   direct / 14 typed for CPU Counters, and 34 direct / 14 typed for product-v2;
   xctrace rc counts were `54 185` and `0 27`; exports were `82` with
   `bad_status 0`, `bad_rc 0`, and `missing_or_empty 0`; hot-leaf summary was
   `82 bad 0`; hot-leaf details was `410 bad 0`; replay artifact/status paths
   were `458 missing_artifact 0 missing_status_artifact 0`.

4. The xctrace `rc=54` stdout policy is artifact-true. The manifest states that
   `rc=54` is accepted only when the captured xctrace log stream records an
   accepted stop condition and `Output file saved as`, and that those strings
   are in the stdout path recorded by `capture_status.tsv`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:109-113`).
   The xctrace status schema puts stdout in field 9
   (`/tmp/skv12-pin-p1/xctrace/capture_status.tsv:1`); a representative
   `rc=54` row points to stdout at
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv:2`, and that stdout records
   the accepted time-limit stop and saved output
   (`/tmp/skv12-pin-p1/logs/xctrace-time-profiler-primary-direct-apache_builds-track1.out:3-5`).
   I ran:

   ```bash
   awk -F '\t' 'BEGIN{q="'\''"} NR>1 && $6==54 {n++; f=$9;
     if(!(system("rg -q " q "Output file saved as" q " " q f q)==0 &&
          system("rg -q " q "Reached specified time limit|Target app exited" q " " q f q)==0))
       {bad++; print "bad", NR, f}}
     END{print "rc54_checked", n, "bad", bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   ```

   Result: `rc54_checked 185 bad 0`.

5. Capture-source authority is separated from the current review base. I ran
   `git rev-parse --short HEAD`, which returned `ecda8b13`, matching the review
   base in the dispatch. The manifest pins the capture source to `cf7848b2`,
   the initial committed S-P1 fold to `b1043383`, the PIN-V2 review base to
   `d4ef80b2`, the capture root to `/tmp/skv12-pin-p1`, and the build root to
   `/tmp/skv12-pin-profile-target-cf7848b2`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-20`).
   It also states that `skinny/RESULTS.md` remains result authority and that the
   pre-pin `/tmp/skv12-p1`, `/tmp/skv12-profile-target-50bd1648`, and
   `skv12-p1-replay.tsv` surfaces are historical only
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:28-29`,
   `:51-53`). P1-A, P1-B, P1-D, and P1-F identify `cf7848b2` as the capture
   source/baseline rather than the review head
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:5-18`;
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:5-15`;
   `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:5-13`;
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:5-20`).
   P1-C's loose `documentation head cf7848b2` wording remains a non-authoritative
   boundary phrase; PIN-V3 CH4 already treated it as nonblocking because no
   replay command depends on it
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:5-17`;
   `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CH4.md:63-65`).

6. CSS L4 absence is bounded correctly and not substituted by JSON, Sheets,
   report fixtures, or lightningcss-only evidence. The user pin makes CSS L4
   authoritative, raises the close target to generated CSS L4 Track 1 beating
   lightningcss on the same corpus/output plane, and preserves `parse_only` as
   diagnostic-only (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`,
   `:80-99`). The manifest says CSS L4 remains unprofiled because the pin root
   has no generated CSS L4 Track 1 runtime, lightningcss same-plane comparator
   row, or strict equality oracle row
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:172-177`).
   P1-A/P1-C/P1-D/P1-E/P1-F preserve the same boundary
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:145-158`;
   `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:89-110`;
   `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:67-80`;
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:47-50`;
   `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80-93`).
   I ran:

   ```bash
   find /tmp/skv12-pin-p1 -iname '*css*' -o -iname '*lightning*' -o -iname '*nonjson*' | wc -l
   rg -n "lightningcss|css_l4|CSS L4" skinny/RESULTS.md /tmp/skv12-p1f-current-results.csv
   find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
   ```

   Results: the pin root search returned `0`; the RESULTS/current extraction
   search returned no matches; runtime grammars list only JSON files and
   `sheets_witness`, with no `css_l4` or `css_l4_declaration_values` module.

## Exact Fold Edits If REVISE

N/A - ACCEPT.

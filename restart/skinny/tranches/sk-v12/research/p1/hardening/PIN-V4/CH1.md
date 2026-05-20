# SK-V12 S-P1 PIN-V4 CH1 - Correctness

Verdict: ACCEPT

Score: 97%

## Blocking Findings

None.

## Nonblocking Notes

1. The PIN-V3 CH1 blocker is folded. PIN-V3 required normalizing the two PMU parse replay corpus cells from `update-center` to `update_center`, preserving `skinny/test_data/update-center.json` only as the file alias, and adding a corpus-key sanity check (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:12`, `:23-34`). The tracked replay now has `update_center` in the corpus column for the former blocker rows while the command operand remains `test_data/update-center.json` (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:66-67`). The samply parse rows also retain canonical `update_center` corpus keys and `track1`/`track2` modes (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:148-149`).

2. Replay schema, modes, and corpus keys validate. The tracked replay header is the expected 10-field schema (`lane family plane corpus mode cwd artifact status_artifact command notes`) at `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:1`, and `wc -l` reports 459 lines, i.e. 458 data rows. I ran:

   ```bash
   awk -F '\t' '
   NR==1 {print "header_fields", NF; next}
   {rows++; if(NF!=10) nf_bad++; lane[$1]++; mode[$5]++; corpus[$4]++;
    if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) mode_bad++;
    if($4=="update-center") update_dash++;
    if($4 !~ /^(twitter|citm_catalog|canada|apache_builds|github_events|update_center|mesh|random|gsoc-2018|marine_ik|instruments|numbers|unicode_mixed|unicode_escapes|unicode_basic|distinct_values|y_string_unicode)$/) corpus_bad[$4]++}
   END {print "rows", rows; print "nf_bad", nf_bad+0; print "mode_bad", mode_bad+0;
        print "update_dash", update_dash+0; print "bad_corpus_keys", length(corpus_bad);
        print "corpora", length(corpus); for (k in lane) print k, lane[k]}'
     restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   ```

   Result: `header_fields 10`, `rows 458`, `nf_bad 0`, `mode_bad 0`, `update_dash 0`, `bad_corpus_keys 0`, `corpora 17`, with lane counts `pmu 82`, `samply 82`, `xctrace-time-profiler-primary 82`, `xctrace-cpu-counters 82`, `xctrace-time-profiler-product-v2 48`, and `xctrace-export 82`. This matches the manifest's 458-row replay surface (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:70-73`) and the PIN-V2/PIN-V3 fold expectations (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V2/CONSOLIDATED.md:25-35`; `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:28-34`).

3. The xctrace `rc=54` stdout policy is now artifact-true. The manifest states that `rc=54` is accepted only when the captured xctrace log stream records an accepted stop condition and `Output file saved as`, and that those strings are in the stdout path recorded by `capture_status.tsv` (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:109-113`). I ran:

   ```bash
   awk -F '\t' 'NR>1 && $6==54 {n++; f=$9;
     if(!(system("rg -q \047Output file saved as\047 " q f q)==0 &&
          system("rg -q \047Reached specified time limit|Target app exited\047 " q f q)==0)) bad++}
     BEGIN{q="\047"} END{print "rc54_checked", n, "bad", bad+0}'
     /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   ```

   Result: `rc54_checked 185 bad 0`. The status schema confirms stdout is field 9 (`/tmp/skv12-pin-p1/xctrace/capture_status.tsv:1`); a representative `rc=54` row points to stdout at `/tmp/skv12-pin-p1/xctrace/capture_status.tsv:2`, and that stdout records the accepted stop and saved-output lines (`/tmp/skv12-pin-p1/logs/xctrace-time-profiler-primary-direct-apache_builds-track1.out:3-5`) while the paired stderr is empty.

4. Artifact counts match the pinned packet. The manifest records PMU 82, samply 82, xctrace primary 82, xctrace CPU Counters 82, xctrace product-v2 48, Time Profiler exports 82, and derived hot-leaf tables 82 summary / 410 detail rows (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:60-68`, `:117-125`). I reran the status checks:

   ```bash
   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "pmu", total, bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "samply", total, bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
   awk -F '\t' 'NR>1{total++; if($4!="SKIP") bad++} END{print "exports", total, bad+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
   awk -F '\t' 'NR>1{n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++} END{print "summary", n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
   awk -F '\t' 'NR>1{n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++} END{print "details", n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
   ```

   Result: `pmu 82 0`, `samply 82 0`, `exports 82 0`, `summary 82 0`, `details 410 0`. P1-A/P1-B/P1-E repeat the same final fold counts for parse/product/hot-leaf subsets (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:23-35`; `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:24-31`; `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:21-30`). One extra `/tmp/skv12-pin-p1/inspect/apache_builds__track1.time-profile.xml` exists outside the 82-row export-status ledger; it is not cited as replay authority.

5. Capture-source authority is separated from review head. Current review base is `1669c551` (`git rev-parse --short=8 HEAD`), while the manifest pins the capture source commit `cf7848b2`, initial committed S-P1 fold `b1043383`, PIN-V2 review base `d4ef80b2`, capture root `/tmp/skv12-pin-p1`, and build root `/tmp/skv12-pin-profile-target-cf7848b2` (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-20`). The manifest also states that `skinny/RESULTS.md` remains result authority and that the pre-pin `/tmp/skv12-p1` / `skv12-p1-replay.tsv` surface is historical only (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:28-29`, `:51-53`). The prior "current HEAD cf7848b2" defect is gone from the load-bearing lane headers; P1-A, P1-B, P1-D, and P1-F identify `cf7848b2` as capture source/baseline rather than current review head (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:5-8`; `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:5-9`; `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:5-9`; `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:5-7`). P1-C's looser "documentation head cf7848b2" wording remains only a boundary-baseline phrase already treated as nonblocking by the PIN-V3 cost lens; no replay command or authority depends on it (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:8-13`; `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CH4.md:63-65`).

6. CSS L4 absence is bounded correctly. The user pin makes CSS L4 authoritative and raises the close target to generated CSS L4 Track 1 `> lightningcss_mbps + 1`, while preserving `parse_only` as diagnostic-only (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`, `:90-99`). The pinned packet does not substitute JSON, Sheets, or report fixtures for CSS: the manifest states that CSS L4 remains unprofiled because no generated CSS L4 Track 1 runtime, lightningcss same-plane comparator row, or strict equality oracle row exists (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:172-177`); P1-A/P1-C/P1-D/P1-E/P1-F preserve the same boundary (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:145-158`; `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:89-110`; `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:67-80`; `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:47-50`; `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80-93`). I also ran `find /tmp/skv12-pin-p1 -iname '*css*' -o -iname '*lightning*' -o -iname '*nonjson*'`, which returned no pin-root artifacts; `rg -n "lightningcss|css_l4|CSS L4" skinny/RESULTS.md /tmp/skv12-p1f-current-results.csv`, which returned no RESULTS matches; and `find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort`, which showed generated JSON files and `sheets_witness` only, with no `css_l4` or `css_l4_declaration_values` module.

7. CH1's governing contract is satisfied for the pinned JSON profile surface. S-P1 CH1 asks whether hot-leaf claims cite source-backed profiler evidence, c/B comes from real PMU counters, coverage is complete for the profiled JSON lanes, and unresolved `unprofiled` cells are not hidden (`restart/prompts/skinny/PASS-1-PROFILE.md:123-127`). The profile remains non-converged until the challenge process records consecutive ACCEPT cycles or a user pin per the orchestrator, but this CH1 cycle has no remaining correctness blocker (`restart/prompts/ORCHESTRATOR.md:104-121`; `restart/prompts/skinny/PASS-1-PROFILE.md:166-180`).

## Exact Fold Edits If REVISE

N/A - ACCEPT.

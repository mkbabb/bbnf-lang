# SK-V12 S-P1 PIN-V6 CH1 - Correctness

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

1. Governance and reset state are correctly bounded. `ORCHESTRATOR.md` requires two consecutive cycles at >=95% ACCEPT with zero critical defects and no orphan REVISE before S-P1 can advance (`restart/prompts/ORCHESTRATOR.md:104-121`), and the S-P1 prompt repeats the same convergence gate (`restart/prompts/skinny/PASS-1-PROFILE.md:166-185`). PIN-V5 was five ACCEPT plus one REVISE and reset the clean-cycle count (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:8-20`, `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:39-43`). Therefore PIN-V6 can become the first clean cycle after the PIN-V5 reset, not the second.

2. The PIN-V5 stale-authority fold is applied on the live S-P1/SPEC authority surfaces. The PIN-V5 consolidation required demoting stale pre-pin S-P1 convergence and SPEC profile authority paths (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:22-37`). `HARDENING-S-P1-CONVERGED.md` now marks pre-pin convergence historical-only and names the pin capture source, `/tmp/skv12-pin-p1` root, `/tmp/skv12-pin-profile-target-cf7848b2` build root, pin replay TSV, status TSVs, and xctrace-derived self-time TSVs as the current authority (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:7-25`, `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:27-49`). `SPEC.md` now lists `skv12-p1-pin-replay.tsv` in the authority list and binds W0 to `cf7848b2`, `/tmp/skv12-pin-p1`, `/tmp/skv12-pin-profile-target-cf7848b2`, and the pin self-time TSVs (`restart/skinny/tranches/sk-v12/SPEC.md:16-23`, `restart/skinny/tranches/sk-v12/SPEC.md:331-368`). The remaining old replay references I found are in non-current or historical surfaces, for example `DISPATCH-PROMPT.md:25` and pre-pin research artifacts; `HANDOFF.md` keeps those out of live dispatch authority by requiring pin-aware S-P1 -> S-P2 -> S-P3 re-derivation and authorizing no source work until that happens (`restart/skinny/tranches/sk-v12/HANDOFF.md:103-128`, `restart/skinny/tranches/sk-v12/HANDOFF.md:172-177`).

3. Replay schema, corpus keys, and canonical modes recheck clean. The tracked pin replay schema is the 10-column header at `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:1`, and the manifest claims 458 command rows split as 82 PMU, 82 samply, 212 xctrace capture, and 82 xctrace export rows (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:69-72`). I ran:

```bash
awk -F '\t' 'NR==1 {print "header=" $0; next}
NF!=10 {bad_nf++; if (bad_nf<=5) print "bad_nf", NR, NF}
{rows++; lane[$1]++; family[$2]++; plane[$3]++; corpus[$4]++; mode[$5]++; artifact[$7]++}
END {print "rows=" rows " bad_nf=" bad_nf+0; for (k in lane) print k, lane[k]; for (k in mode) print k, mode[k]}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
```

Result: `rows=458 bad_nf=0`; lanes were `pmu 82`, `samply 82`, `xctrace-time-profiler-primary 82`, `xctrace-cpu-counters 82`, `xctrace-time-profiler-product-v2 48`, and `xctrace-export 82`; modes were only `track1 187`, `track2 187`, `real_typed_track1 42`, and `real_typed_track2 42`. I also ran corpus and stale-key checks:

```bash
awk -F '\t' 'BEGIN {split("twitter citm_catalog canada apache_builds github_events update_center mesh random gsoc-2018 marine_ik instruments numbers unicode_mixed unicode_escapes unicode_basic distinct_values y_string_unicode", allowed, " "); for (i in allowed) ok[allowed[i]]=1}
NR>1 {seen[$4]++; if (!ok[$4]) bad[$4]++}
END {print "corpora_seen=" length(seen); print "bad_corpora=" length(bad)}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv

awk -F '\t' 'NR>1 && $5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ {bad_mode++}
NR>1 && $4=="update-center" {bad_update++}
END {print "bad_mode=" bad_mode+0; print "bad_update=" bad_update+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
```

Results: `corpora_seen=17`, `bad_corpora=0`, `bad_mode=0`, and `bad_update=0`.

4. Artifact counts and capture status match the manifest. The manifest records PMU 82, samply 82, primary Time Profiler 82, CPU Counters 82, product-v2 Time Profiler 48, XML exports 82, and derived hot-leaf tables of 82 summary / 410 detail rows (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:56-68`, `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:114-125`). I ran:

```bash
awk -F '\t' 'NR==1 {next} {rows++; if($7!="PASS") bad++} END {print "pmu_rows=" rows " bad=" bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
awk -F '\t' 'NR==1 {next} {rows++; if($7!="PASS") bad++} END {print "samply_rows=" rows " bad=" bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
awk -F '\t' 'NR==1 {next} {rows++; if($7!="PASS") bad++; rc[$6]++} END {print "xctrace_rows=" rows " bad=" bad+0; for (k in rc) print "rc", k, rc[k]}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
awk -F '\t' 'NR>1 {n++; if($4!="SKIP") bad++} END {print "export_rows=" n " non_skip=" bad+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++} END {print "summary_rows=" n " bad_source_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++} END {print "detail_rows=" n " bad_source_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
awk -F '\t' 'NR>1 {n++; if(system("test -s \"" $7 "\"")!=0) bad++} END {print "replay_artifacts_checked=" n " missing_or_empty=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
```

Results: PMU `82 bad=0`; samply `82 bad=0`; xctrace `212 bad=0` with `rc 54 185` and `rc 0 27`; exports `82 non_skip=0`; hot-leaf summary `82 bad_source_anchors=0`; hot-leaf details `410 bad_source_anchors=0`; replay artifacts `458 missing_or_empty=0`.

5. The xctrace `rc=54` stdout policy is satisfied. The manifest permits `rc=54` only when the stdout log records an accepted stop condition plus `Output file saved as` (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:108-113`), and `capture_status.tsv` places stdout in field 9. I ran:

```bash
awk -F '\t' 'NR>1 && $6==54 {n++; print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | while IFS= read -r f; do if rg -q 'Output file saved as' "$f" && rg -q 'Reached specified time limit|Target app exited' "$f"; then ok=$((ok+1)); else bad=$((bad+1)); printf "bad_rc54_stdout %s\n" "$f"; fi; done; printf "rc54_ok=%s rc54_bad=%s\n" "${ok:-0}" "${bad:-0}"
```

Result: `rc54_ok=185 rc54_bad=0`.

6. Capture-source authority is not confused with the review base. `git rev-parse HEAD` returned `f3e68a43bb5c7765457c48907a6f0853d1f71bc5`, while the capture manifest correctly pins the measured profile source to `cf7848b2`, the initial committed S-P1 fold to `b1043383`, the capture root to `/tmp/skv12-pin-p1`, and the build root to `/tmp/skv12-pin-profile-target-cf7848b2` (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-20`). The manifest also states that `skinny/RESULTS.md` remains result authority and that the pre-pin manifest/replay surface is historical-only, not pin-era replay authority (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:28-29`, `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:51-53`).

7. CSS L4 absence is correctly bounded. The user pin makes CSS L4 the authoritative first W1 target and requires Sheets/BBNF-self fallbacks only after a CSS L4 redress attempt fails, not after preflight failure (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`, `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:80-103`). The status file records the same boundary and explicitly says the pin profile root has no generated CSS L4 Track 1 runtime, no same-plane lightningcss comparator row, and no strict equality oracle row (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:55-76`). The manifest preserves the absence boundary at the capture layer (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:171-176`). I ran:

```bash
awk -F '\t' 'NR>1 && tolower($2 FS $3 FS $4 FS $5) ~ /(css|lightningcss|sheets|bbnf)/ {hits++; print NR ":" $2 ":" $3 ":" $4 ":" $5} END{print "semantic_nonjson_key_hits=" hits+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
```

Result: `semantic_nonjson_key_hits=0`.

## Exact Fold Edits If REVISE

N/A - ACCEPT.

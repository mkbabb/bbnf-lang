# SK-V12 S-P1 PIN-V5 CH6 Anti-Paper-Close Review

Verdict: ACCEPT

Score: 97%

## Blocking Findings

None.

At review base `ecda8b131efca2fbf9a4acfe67efef2a3c13e8b4`, I found no remaining
CH6 paper-close blocker. Every current S-P1 artifact claim I checked is backed
by a measured/logged file or is explicitly recorded as absent. The CSS L4 gap is
not admitted by JSON evidence, and the prior PIN-V1/PIN-V2/PIN-V3 REVISE folds
are reflected in the manifest, replay ledger, and P1 documents.

PIN-V4 was the first all-ACCEPT user-pin S-P1 cycle
(`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/CONSOLIDATED.md:19-20`).
This CH6 result is clean enough for PIN-V5 to serve as the second consecutive
all-ACCEPT cycle if the other PIN-V5 lenses also return ACCEPT, satisfying
`ORCHESTRATOR.md` section 3Z's two-cycle criterion
(`restart/prompts/ORCHESTRATOR.md:118-121`).

## Nonblocking Notes

- The replay authority is concrete. The manifest pins the capture source, live
  root, build root, binaries, completion stamps, and result-authority boundary
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-29`),
  marks the pre-pin `/tmp/skv12-p1` surface historical only
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:51-53`),
  and enumerates the pin replay lanes plus the tracked 458-row command ledger
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:55-73`).
  My replay-ledger check returned `total 458`, `badmode 0`, `badcorpus 0`, and
  `missing_path_or_command 0`; the referenced artifact/status existence check
  returned no missing or empty paths.

- Status TSV evidence matches the manifest. `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
  has 82/82 `PASS`, `/tmp/skv12-pin-p1/samply/capture_status.tsv` has 82/82
  `PASS`, `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` has 212/212 `PASS`
  with `rc_54=185` and `rc_0=27`, and
  `/tmp/skv12-pin-p1/time_profile_export_status.tsv` has 82/82 `SKIP` for
  already-existing exports. This matches the manifest coverage and validation
  claims (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:115-169`).
  The `rc=54` stdout proof rechecked as `rc54 185 bad 0`.

- Hot-leaf claims are backed by TSVs, not prose. P1-E declares xctrace-derived
  tables as hot-leaf authority and records 82 summary rows plus 410 detail rows
  (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:19-30`);
  my checks returned `summary_rows 82 bad_anchors 0` and `detail_rows 410
  bad_anchors 0`. P1-D derives c/B only from
  `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` and
  `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv`, refuses branch/L1/LLC inference,
  and states the PMU values move no RESULTS row
  (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:17-36`,
  `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:67-80`,
  `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:228-231`).

- CSS L4 is not paper-admitted. The user pin makes generated CSS L4 authoritative
  and raises the close bar to same-plane generated Track 1 throughput greater
  than `lightningcss_mbps + 1`
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`,
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:80-89`). The
  manifest states CSS L4 remains unprofiled because there is no generated CSS L4
  Track 1 runtime, lightningcss same-plane comparator row, or strict equality
  oracle row (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:172-177`).
  P1-A refuses CSS snippets, report fixtures, or lightningcss-only runs as
  substitutes (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:145-158`),
  P1-C records the concrete source/profile blockers
  (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:96-110`),
  and P1-F records 0 admitted CSS L4 rows plus no JSON substitution for the
  lightningcss close bar (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:19-33`,
  `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80-93`,
  `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:205-210`).
  My checks found no `*css*`, `*lightning*`, or `*nonjson*` artifacts in
  `/tmp/skv12-pin-p1`, no `lightningcss|css_l4|CSS L4` matches in
  `skinny/RESULTS.md` or `/tmp/skv12-p1f-current-results.csv`, and no generated
  CSS grammar module under `skinny/crates/runtime/src/grammars`.

- Missing CSS L4 is routed rather than deferred. `HANDOFF.md` records CSS L4 as
  the authoritative first target with Sheets/BBNF-self fallback only after CSS
  redress (`restart/skinny/tranches/sk-v12/HANDOFF.md:32-39`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:49-68`), requires S-P2/S-P3 to
  name the exact CSS row, generated Track 1 path, oracle/comparator/equality,
  benchmark, gate, `GrammarConfig`, generated-size, and O(N) evidence
  (`restart/skinny/tranches/sk-v12/HANDOFF.md:103-128`), and makes missing
  lightningcss evidence or stale run ids fail closed
  (`restart/skinny/tranches/sk-v12/HANDOFF.md:142-155`). That matches the CH6
  anti-paper-close contract in `PASS-1-PROFILE.md`
  (`restart/prompts/skinny/PASS-1-PROFILE.md:155-160`).

- The previous REVISE folds are reflected. PIN-V1 required single pin authority,
  honest XML `SKIP` wording, stale missing-artifact cleanup, Track 1/Track 2
  split, and generated-size/O(N) routing
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V1/CONSOLIDATED.md:21-35`);
  the manifest, P1-B/P1-E mode splits, and HANDOFF routing now carry those fixes.
  PIN-V2 required canonical `samply-parse` modes, stdout-backed `rc=54`, and
  capture-source wording
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V2/CONSOLIDATED.md:21-35`);
  current checks show zero bad modes and `rc54 185 bad 0`. PIN-V3 required
  normalizing the two `update_center` corpus-key cells
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:21-34`);
  current checks show `badcorpus 0`.

- Nonblocking caution: the tracked samply commands still use `samply record
  --save-only --unstable-presymbolicate` (for example
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:84-91`),
  while the S-P1 prompt warns not to rely on `--save-only` for symbol resolution
  (`restart/prompts/skinny/PASS-1-PROFILE.md:155-160`,
  `restart/prompts/skinny/PASS-1-PROFILE.md:251-254`). I do not count this as a
  blocker because current source-symbol/self-time authority is the xctrace-derived
  TSV set and samply is explicitly companion evidence
  (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:12-13`,
  `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:86-88`).

Commands run for this CH6 review:

```bash
git status --short
git rev-parse HEAD
rg --files -g 'AGENTS.md' -g 'CH6.md' -g '*PIN-V3*' -g '*PIN-V4*' -g '*status*.tsv' -g '*.md' restart/prompts restart/skinny/tranches/sk-v12 /tmp
rg -n "3Z|PIN|CSS L4|S-P1|profile TSV|REVISE|ACCEPT|artifact|manifest|samply|xctrace|pmu|time_profile" restart/prompts/ORCHESTRATOR.md
nl -ba restart/prompts/ORCHESTRATOR.md | sed -n '100,130p'
nl -ba restart/prompts/skinny/PASS-1-PROFILE.md
nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md
nl -ba restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md
nl -ba restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
find /tmp/skv12-pin-p1 -maxdepth 3 -type f -name '*status*.tsv' -print | sort
awk -F '\t' 'NR>1{total++; status[$7]++} END{print total; for (s in status) print s,status[s]}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
awk -F '\t' 'NR>1{total++; status[$7]++} END{print total; for (s in status) print s,status[s]}' /tmp/skv12-pin-p1/samply/capture_status.tsv
awk -F '\t' 'NR>1{total++; status[$7]++; rc[$6]++} END{print total; for (s in status) print s,status[s]; for (r in rc) print r,rc[r]}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
awk -F '\t' 'NR>1{total++; status[$4]++} END{print total; for (s in status) print s,status[s]}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
awk -F '\t' 'NR==1{next} {total++; lane[$1]++; fam[$1 "/" $2]++; mode[$5]++; if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) badmode++; if($4=="update-center") badcorpus++; if($7=="" || $8=="" || $9=="") missing++} END{print "total", total; print "badmode", badmode+0; print "badcorpus", badcorpus+0; print "missing_path_or_command", missing+0; for(k in lane) print "lane", k, lane[k]; for(k in fam) print "family", k, fam[k]; for(k in mode) print "mode", k, mode[k]}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv | sort
awk -F '\t' 'NR>1{paths[$7]=1; paths[$8]=1} END{for(p in paths){cmd="test -s \"" p "\""; if(system(cmd)!=0) print "missing_or_empty", p}}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
awk -F '\t' 'NR>1 && $6==54 {n++; f=$9; cmd="rg -q '\''Output file saved as'\'' \"" f "\""; cmd2="rg -q '\''Reached specified time limit|Target app exited'\'' \"" f "\""; if(system(cmd)!=0 || system(cmd2)!=0){bad++; print "BAD", NR, f}} END{print "rc54", n+0, "bad", bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
awk -F '\t' 'NR>1{n++; if($4!="SKIP") non_skip++; if($3!=0) bad_rc++; cmd="test -s \"" $2 "\""; if(system(cmd)!=0){bad_xml++; print "BAD_XML", NR, $2}; cmd2="test -e \"" $1 "\""; if(system(cmd2)!=0){bad_trace++; print "BAD_TRACE", NR, $1}} END{print "export_rows",n+0,"non_skip",non_skip+0,"bad_rc",bad_rc+0,"bad_xml",bad_xml+0,"bad_trace",bad_trace+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++} END{print "summary_rows",n+0,"bad_anchors",bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++} END{print "detail_rows",n+0,"bad_anchors",bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
find /tmp/skv12-pin-p1 -iname '*css*' -o -iname '*lightning*' -o -iname '*nonjson*'
rg -n "lightningcss|css_l4|CSS L4" skinny/RESULTS.md /tmp/skv12-p1f-current-results.csv
find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V1/CONSOLIDATED.md
nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V2/CONSOLIDATED.md
nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md
nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/CONSOLIDATED.md
rg -n "samply record|--save-only|unstable-presymbolicate" restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv | head -n 8
```

## Exact Fold Edits If REVISE

N/A. Verdict is ACCEPT.

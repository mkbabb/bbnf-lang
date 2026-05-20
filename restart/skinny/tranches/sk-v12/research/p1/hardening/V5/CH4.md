# SK-V12 S-P1 Hardening V5 CH4 Cost / Replayability

Verdict: ACCEPT

## Scope

Audited current repo commit
`fe7ae2ab40d3ba205445f07bc4cd870d68cdb1cb` plus the retained
`/tmp/skv12-p1` artifact tree through the CH4 cost / replayability lens.

This is the confirmation cycle after V4 all-ACCEPT. The V4 archive commit only
added the V4 hardening verdict files and consolidation; it did not alter the P1
replay TSV, manifest, source, `RESULTS`, or `REDRESS`.

## Evidence

The replay TSV remains sufficient for third-party replay. It has 506 data rows
and 14 required fields on every row: lane, family, plane, corpus, mode, alias,
iters, cwd, binary, expected return-code policy, output artifact, status
artifact, full command, and notes. There are no empty required fields, no
non-numeric iteration counts, and no placeholder commands.

Coverage is complete for PMU, samply, xctrace CPU Counters, primary Time
Profiler traces, primary Time Profiler exports, product-v2 Time Profiler
recaptures, and product-v2 Time Profiler XML exports. The lane/plane counts
match the expected 34 parse, 34 direct, and 14 typed rows where applicable, with
48 direct+typed rows for product-v2 recaptures and exports.

All replay cwd, output artifact, and status artifact paths resolve in the repo
or `/tmp/skv12-p1`. Launch rows embed the absolute probe binary under
`/tmp/skv12-profile-target-50bd1648/release/`. Export rows are concrete
`xctrace export` transforms from absolute `.trace` inputs to `.time-profile.xml`
outputs using the Time Profiler table XPath.

The samply lane is correctly retained as artifact-only evidence: all 82 samply
rows use `samply record --save-only`, and their notes state that self-time
percentages are not sourced from samply save-only output. The self-time
authority is exported xctrace Time Profiler XML, including product-v2 exports
for direct and typed product self-time.

## Validation Commands

```bash
git rev-parse HEAD
# fe7ae2ab40d3ba205445f07bc4cd870d68cdb1cb

git show --name-only --format='%H %s' --no-renames fe7ae2ab
# fe7ae2ab40d3ba205445f07bc4cd870d68cdb1cb docs(sk-v12-p1-challenge): archive V4 all-accept hardening
# restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CH1.md
# restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CH2.md
# restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CH3.md
# restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CH4.md
# restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CH5.md
# restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CH6.md
# restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CONSOLIDATED.md

awk -F '\t' 'NR==1{header=NF; next}
  {total++; nf[NF]++; lane[$1]++; laneplane[$1 FS $3]++;
   if(NF!=14) badnf++;
   for(i=1;i<=14;i++) if($i=="") miss[i]++;
   if($7 !~ /^[0-9]+$/) bad_iters++;
   if($13 ~ /<[^>]+>/) placeholders++}
  END{
   print "header_fields=" header;
   print "rows=" total;
   print "NF_14=" nf[14];
   print "bad_nf=" badnf+0;
   print "bad_iters=" bad_iters+0;
   print "placeholder_commands=" placeholders+0;
   print "pmu=" lane["pmu"];
   print "pmu parse=" laneplane["pmu" FS "parse"];
   print "pmu direct=" laneplane["pmu" FS "direct"];
   print "pmu typed=" laneplane["pmu" FS "typed"];
   print "samply=" lane["samply"];
   print "samply parse=" laneplane["samply" FS "parse"];
   print "samply direct=" laneplane["samply" FS "direct"];
   print "samply typed=" laneplane["samply" FS "typed"];
   print "xctrace-cpu-counters=" lane["xctrace-cpu-counters"];
   print "xctrace-cpu-counters parse=" laneplane["xctrace-cpu-counters" FS "parse"];
   print "xctrace-cpu-counters direct=" laneplane["xctrace-cpu-counters" FS "direct"];
   print "xctrace-cpu-counters typed=" laneplane["xctrace-cpu-counters" FS "typed"];
   print "xctrace-time-profiler-primary=" lane["xctrace-time-profiler-primary"];
   print "xctrace-time-profiler-primary parse=" laneplane["xctrace-time-profiler-primary" FS "parse"];
   print "xctrace-time-profiler-primary direct=" laneplane["xctrace-time-profiler-primary" FS "direct"];
   print "xctrace-time-profiler-primary typed=" laneplane["xctrace-time-profiler-primary" FS "typed"];
   print "xctrace-time-profiler-export=" lane["xctrace-time-profiler-export"];
   print "xctrace-time-profiler-export parse=" laneplane["xctrace-time-profiler-export" FS "parse"];
   print "xctrace-time-profiler-export-primary=" lane["xctrace-time-profiler-export-primary"];
   print "xctrace-time-profiler-export-primary direct=" laneplane["xctrace-time-profiler-export-primary" FS "direct"];
   print "xctrace-time-profiler-export-primary typed=" laneplane["xctrace-time-profiler-export-primary" FS "typed"];
   print "xctrace-time-profiler-product-v2=" lane["xctrace-time-profiler-product-v2"];
   print "xctrace-time-profiler-product-v2 direct=" laneplane["xctrace-time-profiler-product-v2" FS "direct"];
   print "xctrace-time-profiler-product-v2 typed=" laneplane["xctrace-time-profiler-product-v2" FS "typed"];
   print "xctrace-time-profiler-product-v2-export=" lane["xctrace-time-profiler-product-v2-export"];
   print "xctrace-time-profiler-product-v2-export direct=" laneplane["xctrace-time-profiler-product-v2-export" FS "direct"];
   print "xctrace-time-profiler-product-v2-export typed=" laneplane["xctrace-time-profiler-product-v2-export" FS "typed"];
   for(i=1;i<=14;i++) if(miss[i]) print "missing_" i "=" miss[i];
  }' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# header_fields=14
# rows=506
# NF_14=506
# bad_nf=0
# bad_iters=0
# placeholder_commands=0
# pmu=82
# pmu direct=34
# pmu parse=34
# pmu typed=14
# samply=82
# samply direct=34
# samply parse=34
# samply typed=14
# xctrace-cpu-counters=82
# xctrace-cpu-counters direct=34
# xctrace-cpu-counters parse=34
# xctrace-cpu-counters typed=14
# xctrace-time-profiler-export=34
# xctrace-time-profiler-export parse=34
# xctrace-time-profiler-export-primary=48
# xctrace-time-profiler-export-primary direct=34
# xctrace-time-profiler-export-primary typed=14
# xctrace-time-profiler-primary=82
# xctrace-time-profiler-primary direct=34
# xctrace-time-profiler-primary parse=34
# xctrace-time-profiler-primary typed=14
# xctrace-time-profiler-product-v2=48
# xctrace-time-profiler-product-v2 direct=34
# xctrace-time-profiler-product-v2 typed=14
# xctrace-time-profiler-product-v2-export=48
# xctrace-time-profiler-product-v2-export direct=34
# xctrace-time-profiler-product-v2-export typed=14

while IFS=$'\t' read -r lane family plane corpus mode alias iters cwd binary \
    expected output status_artifact command notes; do
  [ "$lane" = lane ] && continue
  [ -d "$cwd" ] || echo "missing_cwd $lane $family $corpus $mode $cwd"
  [ -e "$output" ] || echo "missing_output $lane $family $corpus $mode $output"
  [ -e "$status_artifact" ] || echo "missing_status $lane $family $corpus $mode $status_artifact"
done < restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

awk -F '\t' 'NR>1 && $1=="samply" &&
  ($13 !~ /samply record/ || $13 !~ /--save-only/ ||
   $14 !~ /artifact-only/ ||
   $14 !~ /self-time percentages are not sourced/)
  {print NR,$0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

awk -F '\t' 'NR>1 && $1 ~ /export/ &&
  ($13 !~ /xctrace export --input \/tmp\/skv12-p1\/.*\.trace/ ||
   $11 !~ /^\/tmp\/skv12-p1\/.*\.time-profile\.xml$/ ||
   $13 !~ /schema=\\"time-profile\\"/)
  {print NR,$1,$11,$13}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

awk -F '\t' 'NR>1 && $1 ~ /xctrace-time-profiler/ {
   if($11 ~ /\.time-profile\.xml$/) xml++;
   if($13 ~ /xctrace export/) export_cmd++;
   if($13 ~ /schema=\\"time-profile\\"/) schema++;
  } END{
   print "time_profiler_xml_outputs=" xml+0;
   print "time_profiler_export_commands=" export_cmd+0;
   print "time_profile_schema_xpath=" schema+0;
  }' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# time_profiler_xml_outputs=130
# time_profiler_export_commands=130
# time_profile_schema_xpath=130

awk -F '\t' 'NR>1 &&
  ($1=="pmu" || $1=="samply" ||
   $1=="xctrace-time-profiler-primary" ||
   $1=="xctrace-cpu-counters" ||
   $1=="xctrace-time-profiler-product-v2") &&
  $13 !~ /\/tmp\/skv12-profile-target-50bd1648\/release\/(xctrace_probe|profile_direct)/
  {print NR,$1,$2,$3,$4,$5,$13}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

rg -n "artifact-only|self-time percentages.*exported xctrace Time Profiler XML|product-v2 exports are the self-time authority|self-time percentages come from exported xctrace Time Profiler XML|self-time percentages are sourced from" \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md \
  restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md \
  restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md \
  restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md
# Confirms samply artifact-only language and xctrace Time Profiler XML self-time authority.
```

## Required Fold

None.

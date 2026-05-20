# SK-V12 S-P1 Hardening V4 CH4 Cost / Replayability

Verdict: ACCEPT.

## Scope

Audited commit `6d19429f2c0afd25d8746658b4bdb458226402fe` plus the retained
`/tmp/skv12-p1` artifact tree through the CH4 replayability lens.

Inputs reviewed:

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/CH4.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V3/CH4.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md`
- `/tmp/skv12-p1`

PASS-1-PROFILE CH4 requires rerunnable method commands with run identity, host,
and build flags. The samply rule remains strict: `--save-only` is not a
self-time authority. V2 CH4 required one repo-tracked replay surface enumerating
every parse/direct/typed PMU, samply, Time Profiler, CPU Counter, and export row
with cwd, full command, corpus, alias, mode, iters, binary path, expected rc
policy, and output artifact.

## Evidence

The V2 CH4 blocker is folded and remains folded after commit `6d19429f`. The
commit under audit only added the V3 fold ledger and tightened the capture
manifest's line-zero invariant; it did not change source, `RESULTS`, `REDRESS`,
or the replay TSV.

The replay TSV has 506 rows and complete required columns. Coverage is complete:
PMU, samply, primary Time Profiler, and CPU Counter lanes each enumerate 34
parse rows, 34 direct rows, and 14 typed rows. Export coverage is also explicit:
34 parse primary Time Profiler exports, 34 direct plus 14 typed primary exports,
48 product-v2 Time Profiler recaptures, and 48 product-v2 exports.

Every row has cwd, corpus, alias, mode, numeric iters, expected rc policy, output
artifact, status artifact, and full command. All current cwd, output artifact,
and status artifact paths resolve. Capture commands are concrete and include the
absolute launched target binary under
`/tmp/skv12-profile-target-50bd1648/release/`; export rows are concrete
`xctrace export` transforms over absolute trace inputs and XML outputs. The TSV
uses the `binary` field as the command driver for `samply`/`xctrace` rows, but
the full command embeds the target binary path where a launched target exists,
which is sufficient for third-party replay.

The `update_center` launch alias is explicit: parse rows use
`update-center.json`; direct and typed product rows use `update-center`. The
samply `--save-only` lane is consistently labeled artifact-only, and the
self-time authority is exported xctrace Time Profiler XML, including the
product-v2 XML lane for product self-time.

## Validation Commands

```bash
git rev-parse HEAD
# 6d19429f2c0afd25d8746658b4bdb458226402fe

git show --name-only --format='%H %s' --no-renames 6d19429f
# 6d19429f2c0afd25d8746658b4bdb458226402fe docs(sk-v12-p1): fold V3 hardening symbol-label revisions
# restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md
# restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md

awk -F '\t' 'NR==1{print "fields=" NF; next} {c[$1]++; total++} END{
  print "rows=" total;
  print "pmu=" c["pmu"];
  print "samply=" c["samply"];
  print "xctrace-cpu-counters=" c["xctrace-cpu-counters"];
  print "xctrace-time-profiler-primary=" c["xctrace-time-profiler-primary"];
  print "xctrace-time-profiler-export=" c["xctrace-time-profiler-export"];
  print "xctrace-time-profiler-export-primary=" c["xctrace-time-profiler-export-primary"];
  print "xctrace-time-profiler-product-v2=" c["xctrace-time-profiler-product-v2"];
  print "xctrace-time-profiler-product-v2-export=" c["xctrace-time-profiler-product-v2-export"];
}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# fields=14
# rows=506
# pmu=82
# samply=82
# xctrace-cpu-counters=82
# xctrace-time-profiler-primary=82
# xctrace-time-profiler-export=34
# xctrace-time-profiler-export-primary=48
# xctrace-time-profiler-product-v2=48
# xctrace-time-profiler-product-v2-export=48

awk -F '\t' 'NR>1 {c[$1 FS $3]++}
  END{for (k in c) print k,c[k]}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv | sort
# pmu direct 34
# pmu parse 34
# pmu typed 14
# samply direct 34
# samply parse 34
# samply typed 14
# xctrace-cpu-counters direct 34
# xctrace-cpu-counters parse 34
# xctrace-cpu-counters typed 14
# xctrace-time-profiler-export parse 34
# xctrace-time-profiler-export-primary direct 34
# xctrace-time-profiler-export-primary typed 14
# xctrace-time-profiler-primary direct 34
# xctrace-time-profiler-primary parse 34
# xctrace-time-profiler-primary typed 14
# xctrace-time-profiler-product-v2 direct 34
# xctrace-time-profiler-product-v2 typed 14
# xctrace-time-profiler-product-v2-export direct 34
# xctrace-time-profiler-product-v2-export typed 14

awk -F '\t' 'NR>1{
  for(i=1;i<=14;i++) if($i=="") miss[i]++;
  if($7 !~ /^[0-9]+$/) bad_iters++;
} END{
  for(i=1;i<=14;i++) if(miss[i]) print "missing_col_" i, miss[i];
  print "bad_iters=" bad_iters+0;
}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# bad_iters=0

awk -F '\t' 'NR>1 && $13 ~ /<[^>]+>/ {print NR,$0}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

while IFS=$'\t' read -r lane family plane corpus mode alias iters cwd binary \
    expected output status_artifact command notes; do
  [ "$lane" = lane ] && continue
  [ -e "$output" ] || echo "missing_output $lane $family $corpus $mode $output"
  [ -e "$status_artifact" ] || echo "missing_status $lane $family $corpus $mode $status_artifact"
  [ -d "$cwd" ] || echo "missing_cwd $lane $family $corpus $mode $cwd"
done < restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

awk -F '\t' 'NR>1 &&
  ($1=="pmu" || $1=="samply" || $1=="xctrace-time-profiler-primary" ||
   $1=="xctrace-cpu-counters" || $1=="xctrace-time-profiler-product-v2") &&
  $13 !~ /\/tmp\/skv12-profile-target-50bd1648\/release\/(xctrace_probe|profile_direct)/
  {print NR,$1,$13}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

awk -F '\t' 'NR>1 && $1 ~ /export/ &&
  ($13 !~ /xctrace export --input \/tmp\/skv12-p1\/.*\.trace/ ||
   $11 !~ /^\/tmp\/skv12-p1\/.*\.time-profile\.xml$/)
  {print NR,$1,$11,$13}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

awk -F '\t' 'NR>1 && $1=="samply" &&
  ($14 !~ /artifact-only/ || $14 !~ /self-time percentages are not sourced/)
  {print NR,$0}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

rg -n "artifact-only|self-time percentages.*exported xctrace Time Profiler XML|product-v2 exports are the self-time authority|self-time percentages are sourced from" \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md \
  restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md \
  restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md \
  restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md
# Confirms samply artifact-only labels and exported xctrace Time Profiler XML self-time authority.
```

## Required Fold

None. CH4 replayability is acceptable for V4.

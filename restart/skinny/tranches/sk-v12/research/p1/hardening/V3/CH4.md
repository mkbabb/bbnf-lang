# SK-V12 S-P1 Hardening V3 CH4 Cost / Replayability

Verdict: ACCEPT.

## Scope

Audited current repo commit `ffe5553d` plus `/tmp/skv12-p1` for the V2 CH4
replay fold. Inputs reviewed:

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/CH4.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md`
- `/tmp/skv12-p1/pmu/capture_status.tsv`
- `/tmp/skv12-p1/time_profile_export_status.tsv`
- `/tmp/skv12-p1/product_time_profile_v2_status.tsv`
- `/tmp/skv12-p1/product_time_profile_v2_alias_fixes.tsv`

PASS-1-PROFILE CH4 requires verbatim rerunnable method commands with run id,
host, and build flags. Its samply rule also says `--save-only` is not a
prompt-conforming self-time authority. V2 CH4 required a single replay surface
covering parse/direct/typed PMU, samply, Time Profiler, CPU Counter, and export
rows with cwd, command, corpus, alias, mode, iteration count, binary path,
expected return-code policy, and output artifact.

## Evidence

The V2-to-V3 fold adds the requested replay surface:
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`. The ledger
has 506 replay rows and non-empty columns for lane, family, plane, corpus, mode,
alias, iters, cwd, binary, expected rc policy, output artifact, status artifact,
full command, and notes.

Replay row coverage is complete for the CH4 fold target:

| Lane | Rows |
|---|---:|
| `pmu` | 82 |
| `samply` | 82 |
| `xctrace-cpu-counters` | 82 |
| `xctrace-time-profiler-primary` | 82 |
| `xctrace-time-profiler-export` | 34 |
| `xctrace-time-profiler-export-primary` | 48 |
| `xctrace-time-profiler-product-v2` | 48 |
| `xctrace-time-profiler-product-v2-export` | 48 |

The family split is also complete: 34 parse, 34 direct, and 14 typed rows for
PMU, samply, CPU Counters, and primary Time Profiler; 34 parse and 48 product
primary export rows; and 48 product v2 record plus 48 product v2 export rows.

The replay commands are no longer parameterized placeholders. Every `command`
field is concrete, every `iters` field is numeric, every `cwd` resolves, and
every `output_artifact` and `status_artifact` path resolves under the current
`/tmp/skv12-p1` tree. The `update_center` product alias is explicit as
`update-center` where the launch binary needs it, including product-v2 rows.

The `samply` lane is now clearly non-authoritative for self-time. The manifest
states that samply rows are retained artifact-only evidence because they use
`--save-only`, and every samply replay row notes that self-time percentages are
not sourced from samply save-only output. P1-A and P1-B both route self-time
authority to exported xctrace Time Profiler XML instead.

The only minor shape caveat is non-blocking: for profiler and export lanes, the
`binary` column names the tool (`samply` or `xctrace`) rather than duplicating
the launched target binary path. The full replay command embeds the absolute
target path for every capture row, and export rows are tool-only transforms over
absolute trace paths. That still satisfies replayability because the command
field is the rerunnable authority and the manifest pins the tool versions.

## Validation Commands

```bash
git rev-parse HEAD
# ffe5553d6b38c629e7213fd3b67e2beb9785181c

awk -F '\t' 'NR==1{print "fields=" NF; next} {c[$1]++; total++}
  END{print "rows=" total; for (k in c) print k, c[k]}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# fields=14
# rows=506
# pmu 82
# samply 82
# xctrace-cpu-counters 82
# xctrace-time-profiler-export 34
# xctrace-time-profiler-export-primary 48
# xctrace-time-profiler-primary 82
# xctrace-time-profiler-product-v2 48
# xctrace-time-profiler-product-v2-export 48

awk -F '\t' 'NR>1 {c[$1 FS $2 FS $3]++}
  END{for (k in c) print c[k], k}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv | sort
# Confirms 34 parse, 34 direct, and 14 typed rows where applicable.

awk -F '\t' 'NR>1{for(i=1;i<=14;i++) if($i=="") miss[i]++}
  END{for(i=1;i<=14;i++) print i, miss[i]+0}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# All fields report 0 missing values.

awk -F '\t' 'NR>1 && $7 !~ /^[0-9]+$/ {print NR,$1,$7}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.

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

awk -F '\t' 'NR>1 && $1=="samply" &&
  ($14 !~ /artifact-only/ || $14 !~ /self-time percentages are not sourced/)
  {print NR,$0}' \
  restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv
# No output.
```

## Required Fold

None. The V2 CH4 replayability blocker is folded.

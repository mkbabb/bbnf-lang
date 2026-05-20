# SK-V12 S-P1 PIN-V6 CH5 - Hidden Coupling Review

Verdict: ACCEPT

Score: 97%

## Blocking Findings

None.

The PIN-V5 CH5 blocker is folded. The stale pre-pin roots
`/tmp/skv12-p1`, `/tmp/skv12-profile-target-50bd1648`, and
`skv12-p1-replay.tsv` no longer appear as live or hidden authority in the
required sweep over `HARDENING-S-P1-CONVERGED.md`, `SPEC.md`, and
`research/p1/*.md`. `HARDENING-S-P1-CONVERGED.md` now marks pre-pin
convergence superseded and binds the pin capture root, build root, replay TSV,
status TSVs, and self-time TSVs (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:5`,
`:33-47`). `SPEC.md` now lists the pin replay TSV and W0 binds
`/tmp/skv12-pin-p1`, `/tmp/skv12-pin-profile-target-cf7848b2`, and
`skv12-p1-pin-replay.tsv` (`restart/skinny/tranches/sk-v12/SPEC.md:22`,
`:351-353`).

Command run for the stale-root blocker check:

```bash
rg -n "(/tmp/skv12-p1([^a-zA-Z0-9_-]|$)|/tmp/skv12-profile-target-50bd1648|skv12-p1-replay\.tsv)" \
  restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md \
  restart/skinny/tranches/sk-v12/SPEC.md \
  restart/skinny/tranches/sk-v12/research/p1/*.md
# no output; rg exit 1
```

## Nonblocking Notes

1. Replay authority and replay cells are clean. The manifest identifies
`/tmp/skv12-pin-p1`, `/tmp/skv12-pin-profile-target-cf7848b2`, and 458
pin-era replay rows (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:16-17`,
`:70-71`). The tracked replay has 10 fields, 458 rows, no malformed rows, no
bad mode/update-center cells, no stale-root references, no artifact paths
outside `/tmp/skv12-pin-p1`, and no non-export command missing the pin build
target.

```bash
awk -F '\t' 'NR==1{h=NF; next} {rows++; if(NF!=h){bad++; print NR ":" NF ":" $0}} END{print "header_fields=" h, "rows=" rows, "bad_field_count=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# header_fields=10 rows=458 bad_field_count=0

awk -F '\t' 'NR>1 && ($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ || $4=="update-center") {bad++; print NR ":" $4 ":" $5} END{print "bad_replay_cells=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# bad_replay_cells=0

awk -F '\t' 'NR>1 && /\/tmp\/skv12-p1([^a-zA-Z0-9_-]|$)|\/tmp\/skv12-profile-target-50bd1648|skv12-p1-replay\.tsv/ {bad++; print NR ":" $0} END{print "stale_replay_root_refs=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# stale_replay_root_refs=0

awk -F '\t' 'NR>1 && ($7 !~ /^\/tmp\/skv12-pin-p1\// || $8 !~ /^\/tmp\/skv12-pin-p1\// || $9 !~ /\/tmp\/skv12-pin-p1\//) {bad++; print NR ":" $1 ":" $7 ":" $8} END{print "bad_pin_root_artifacts=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# bad_pin_root_artifacts=0

awk -F '\t' 'NR>1 && $1!="xctrace-export" && $9 !~ /\/tmp\/skv12-pin-profile-target-cf7848b2\/release\// {bad++; print NR ":" $1 ":" $9} END{print "non_export_rows_missing_pin_target=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# non_export_rows_missing_pin_target=0
```

2. The pin capture status and hot-leaf anchors are coherent. PMU is 82/82
PASS, samply is 82/82 PASS, xctrace is 212/212 PASS, `rc=54` logs are
185/185 accepted, summary anchors are 82/82 clean, and detail anchors are
410/410 clean. The manifest also states Mode III remains absent and CSS L4 is
unprofiled because no generated CSS L4 Track 1 runtime, lightningcss
same-plane comparator, or strict equality oracle row exists
(`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:171-175`).

```bash
awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "pmu_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
# pmu_rows=82 bad_status=0

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "samply_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
# samply_rows=82 bad_status=0

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "xctrace_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# xctrace_rows=212 bad_status=0

awk -F '\t' 'NR>1 && $6==54 {print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | while IFS= read -r f; do if rg -q 'Output file saved as' "$f" && rg -q 'Reached specified time limit|Target app exited' "$f"; then printf 'ok\n'; else printf 'bad\t%s\n' "$f"; fi; done | awk 'BEGIN{ok=0; bad=0} $1=="ok"{ok++} $1=="bad"{bad++} END{print "rc54_ok=" ok+0, "rc54_bad=" bad+0}'
# rc54_ok=185 rc54_bad=0

awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none" || $15=="" || $16=="") bad++} END{print "summary_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
# summary_rows=82 bad_anchors=0

awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none" || $8=="" || $9=="") bad++} END{print "detail_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
# detail_rows=410 bad_anchors=0
```

3. No CSS L4 substitution is present. The user pin makes CSS L4 authoritative
and Sheets/BBNF-self fallback-only after a CSS redress attempt
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-31`).
The pin S-P1 packet records CSS L4 as absent rather than substituting
Sheets, BBNF-self, JSON rows, report fixtures, root CSS snippets, or
lightningcss-only evidence: P1-A rejects snippets/report fixtures as
substitutes (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:147-157`),
P1-C records no CSS/non-JSON artifact under the pin root and says CSS must
first create generated Track 1 plus strict lightningcss equality
(`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:104-109`),
P1-E keeps CSS L4 unprofiled with no Track 1 parser or comparator
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:47-50`,
`:113-115`), and P1-F reports zero admitted CSS L4, Sheets, or BBNF-self rows
(`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:85-93`,
`:205-210`).

```bash
find /tmp/skv12-pin-p1 -maxdepth 3 \( -iname '*css*' -o -iname '*lightning*' -o -iname '*sheet*' -o -iname '*bbnf*' \) -print
# no output

awk -F '\t' 'NR>1 && tolower($2 FS $3 FS $4 FS $5) ~ /(css|lightningcss|sheets|bbnf)/ {hits++; print NR ":" $2 ":" $3 ":" $4 ":" $5} END{print "semantic_nonjson_key_hits=" hits+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# semantic_nonjson_key_hits=0
```

4. I found no hidden local-only profile authority beyond the documented pin
profile root. `rg -n "/tmp/skv12|profile-target|skv12-p1-.*replay\.tsv" ...`
shows live profile authority on `/tmp/skv12-pin-p1`,
`/tmp/skv12-pin-profile-target-cf7848b2`, and
`skv12-p1-pin-replay.tsv`. Non-pin `/tmp` mentions are bounded and not hidden
profile roots: `p1f-results-delta.md` uses `/tmp/skv12-p1f-current-results.csv`
as extraction scratch (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:57-61`,
`:223`), `p1c-samply-mode-3.md` fences `/tmp/skv11-open-criterion-3ce75df`
as W0 Criterion-only diagnostic evidence (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:65-87`),
and `SPEC.md`/`HANDOFF.md` use `/tmp/skv12-waveW*-rejected.patch` only as
failure-path patch locations (`restart/skinny/tranches/sk-v12/SPEC.md:453`,
`:525`, `:583`; `restart/skinny/tranches/sk-v12/HANDOFF.md:135-139`).

5. No orphan SIMD or generated-size route is promoted before S-P2. The pin
makes the five orphan primitives wave-eligible only with a same-commit
consumer and keeps Lock 16 plus the `escape_mask_64` correctness prerequisite
load-bearing (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-78`,
`:98-106`). `HANDOFF.md` requires generated CSS runtime LOC, module byte size,
regen/check command, and an O(N) grammar-size guard before W1b redress, and
requires Lock 16/O(N)/same-wave-consumer fields to be gate-consumed
(`restart/skinny/tranches/sk-v12/HANDOFF.md:121-125`, `:149-155`).
P1-A, P1-B, and P1-C keep route promotion out of S-P1
(`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:181-186`,
`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:206-213`,
`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:83-87`).

6. This is CH5's first possible clean cycle after the PIN-V5 reset. PIN-V5
recorded CH5 as REVISE and said S-P1 needs two new consecutive all-ACCEPT
cycles after the fold (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:16-20`,
`:41-42`). If the other PIN-V6 lenses also accept with zero open critical
defects and no orphan unresolved REVISE, PIN-V6 can count as the first clean
cycle after that reset under the convergence rule
(`restart/prompts/ORCHESTRATOR.md:120-123`;
`restart/prompts/skinny/PASS-1-PROFILE.md:177-179`).

## Exact Fold Edits If REVISE

None.

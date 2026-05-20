# SK-V12 S-P1 PIN-V5 CH5 - Hidden Coupling Review

Verdict: REVISE

Score: 88%

## Blocking Findings

1. Stale pre-pin S-P1 convergence authority is still live enough to be consumed downstream. The pin-era manifest makes `/tmp/skv12-pin-p1` the capture root, `/tmp/skv12-pin-profile-target-cf7848b2` the build root, and explicitly demotes `/tmp/skv12-p1`, `/tmp/skv12-profile-target-50bd1648`, and `skv12-p1-replay.tsv` to historical-only status (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:16-20`, `:51-53`). However `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md` still declares `Status: CONVERGED` and names the pre-pin source baseline `50bd1648`, capture root `/tmp/skv12-p1`, target directory `/tmp/skv12-profile-target-50bd1648`, replay authority `skv12-p1-replay.tsv` with 506 rows, and self-time authority under `/tmp/skv12-p1` (`HARDENING-S-P1-CONVERGED.md:5`, `:23-37`). That is a hidden coupling risk because the file is not fenced as obsolete-by-pin and has the exact name downstream agents look for when S-P1 is converged.

2. The stale convergence file is not isolated; `SPEC.md` still promotes it and the old replay TSV through an authority path. `SPEC.md` lists `HARDENING-S-P1-CONVERGED.md`, `skv12-p1-capture-manifest.md`, and `skv12-p1-replay.tsv` as authority (`restart/skinny/tranches/sk-v12/SPEC.md:14-21`), then instructs W0 to bind source baseline `50bd1648`, capture root `/tmp/skv12-p1`, replay TSV, and self-time TSVs into the gate/report surface (`SPEC.md:348-350`). `HANDOFF.md` does say the earlier SK-V12 packet is historical when it conflicts with the user pin (`restart/skinny/tranches/sk-v12/HANDOFF.md:5-10`, `:103-108`), but the stale convergence/SPEC pair still gives a downstream route an authoritative-looking pre-pin root. CH5 cannot count this cycle clean while those authority surfaces still point at pre-pin profile roots.

Commands run for the blocking check:

```bash
git rev-parse HEAD
# ecda8b131efca2fbf9a4acfe67efef2a3c13e8b4

git status --short
# no output before writing this CH5 file

rg -n "(/tmp/skv12-p1([^a-zA-Z0-9_-]|$)|/tmp/skv12-profile-target-50bd1648|skv12-p1-replay.tsv)" \
  restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md \
  restart/skinny/tranches/sk-v12/SPEC.md \
  restart/skinny/tranches/sk-v12/HANDOFF.md \
  restart/skinny/tranches/sk-v12/research/p1/*.md
# p1 manifest and P1-A/P1-D/P1-E use these only as historical/prior context.
# HARDENING-S-P1-CONVERGED.md:27-36 and SPEC.md:20,349 still use them as authority.
```

## Nonblocking Notes

1. The pin-era replay ledger itself is clean. It has 458 data rows, a stable 10-field TSV shape, no malformed mode cells, no stale replay-root references, and no `update-center` corpus-key regressions.

```bash
awk -F '\t' 'NR==1{h=NF; next} {rows++; if(NF!=h){bad++; print NR ":" NF ":" $0}} END{print "header_fields=" h, "rows=" rows, "bad_field_count=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# header_fields=10 rows=458 bad_field_count=0

awk -F '\t' 'NR>1 && $5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ {bad++; print NR ":" $5} END{print "bad_modes=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# bad_modes=0

awk -F '\t' 'NR>1 && $4=="update-center" {bad++; print NR ":" $4} END{print "bad_update_center_keys=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# bad_update_center_keys=0

awk -F '\t' 'NR>1 && /\/tmp\/skv12-p1([^a-zA-Z0-9_-]|$)|\/tmp\/skv12-profile-target-50bd1648|skv12-p1-replay.tsv/ {bad++; print NR ":" $0} END{print "stale_replay_root_refs=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# stale_replay_root_refs=0
```

2. Replay artifact paths and non-export commands stay inside the documented pin roots. This satisfies the local-artifact part of CH5 for the tracked pin replay ledger.

```bash
awk -F '\t' 'NR>1 && ($7 !~ /^\/tmp\/skv12-pin-p1\// || $8 !~ /^\/tmp\/skv12-pin-p1\// || $9 !~ /\/tmp\/skv12-pin-p1\//) {bad++; print NR ":" $1 ":" $7 ":" $8} END{print "bad_pin_root_artifacts=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# bad_pin_root_artifacts=0

awk -F '\t' 'NR>1 && $1!="xctrace-export" && $9 !~ /\/tmp\/skv12-pin-profile-target-cf7848b2\/release\// {bad++; print NR ":" $1 ":" $9} END{print "non_export_rows_missing_pin_target=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# non_export_rows_missing_pin_target=0
```

3. CSS L4 is not substituted by Sheets, BBNF-self, root CSS snippets, report fixtures, JSON rows, or lightningcss-only evidence in the pin-era S-P1 packet. The user pin makes CSS L4 authoritative and Sheets/BBNF-self fallback-only after a CSS redress attempt (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`, `:80-103`). P1-A rejects root snippets/report fixtures/lightningcss-only runs as substitutes (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:145-158`, `:181-183`), P1-C records no CSS/non-JSON/Sheets command in the pin root and requires generated CSS Track 1 plus strict lightningcss comparator/equality first (`p1c-samply-mode-3.md:59-62`, `:91-110`), P1-E keeps CSS L4 as the absent lane rather than a fallback authorization (`p1e-hot-leaf-attribution.md:47-50`, `:176-178`), and P1-F records zero admitted CSS L4, Sheets, or BBNF-self rows (`p1f-results-delta.md:80-93`, `:196-210`).

```bash
find /tmp/skv12-pin-p1 -maxdepth 3 \( -iname '*css*' -o -iname '*lightning*' -o -iname '*sheet*' -o -iname '*bbnf*' \) -print
# no output

awk -F '\t' 'NR>1 && tolower($2 FS $3 FS $4 FS $5) ~ /(css|lightningcss|sheets|bbnf)/ {hits++; print NR ":" $2 ":" $3 ":" $4 ":" $5} END{print "semantic_nonjson_key_hits=" hits+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# semantic_nonjson_key_hits=0
```

4. Tool-role separation and Track 1/Track 2 separation remain intact in the pin profile evidence. The manifest splits PMU, samply, xctrace capture, XML exports, and derived hot-leaf tables by lane (`skv12-p1-capture-manifest.md:57-68`); P1-D refuses to infer missing branch/L1/LLC counters or derive cycles/B from companion xctrace/samply captures (`p1d-pmu-cycles.md:67-80`); P1-B and P1-E keep Track 2/oracle-only families out of generated Track 1 antecedents (`p1b-samply-mode-2.md:33-50`, `p1e-hot-leaf-attribution.md:32-45`, `:143-147`).

```bash
awk -F '\t' 'NR>1{count[$1"/"$3]++} END{for(k in count) print k, count[k]}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv | sort
# direct/track1 17
# direct/track2 17
# parse/track1 17
# parse/track2 17
# typed/real_typed_track1 7
# typed/real_typed_track2 7
```

5. The pin capture status and source-anchor checks pass: PMU 82/82 PASS, samply 82/82 PASS, xctrace 212/212 PASS, `rc=54` accepted stdout logs 185/185, hot-leaf summary 82/82 with no bad anchors, and detail 410/410 with no bad anchors.

```bash
awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "pmu_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
# pmu_rows=82 bad_status=0

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "samply_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
# samply_rows=82 bad_status=0

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "xctrace_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# xctrace_rows=212 bad_status=0

awk -F '\t' 'NR>1 && $6==54 {n++; if($9!="") print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | while IFS= read -r f; do if rg -q 'Output file saved as' "$f" && rg -q 'Reached specified time limit|Target app exited' "$f"; then printf 'ok\n'; else printf 'bad\t%s\n' "$f"; fi; done | awk 'BEGIN{ok=0; bad=0} $1=="ok"{ok++} $1=="bad"{bad++} END{print "rc54_ok=" ok+0, "rc54_bad=" bad+0}'
# rc54_ok=185 rc54_bad=0

awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none" || $15=="" || $16=="") bad++} END{print "summary_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
# summary_rows=82 bad_anchors=0

awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none" || $8=="" || $9=="") bad++} END{print "detail_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
# detail_rows=410 bad_anchors=0
```

6. SIMD/orphan and generated-size/O(N) routing is correctly bounded in the pin-aware handoff and P1 packet. The user pin makes orphan SIMD wave-eligible only with a same-commit consumer and keeps `escape_mask_64` resolution mandatory before new SIMD admission (`USER-PIN-W1-CSS-L4-SOTA.md:71-78`, `:98-106`). P1-A, P1-B, and P1-C do not scope SIMD/union/ASM-gen routes from profile evidence alone (`p1a-samply-mode-1.md:181-186`, `p1b-samply-mode-2.md:206-213`, `p1c-samply-mode-3.md:83-87`). `HANDOFF.md` requires generated CSS runtime LOC, module byte size, regen/check command, and an O(N) grammar-size guard before W1b redress, and keeps Lock 16/generated-size/O(N)/same-wave consumer fields gate-consumed (`HANDOFF.md:119-125`, `:144-155`).

7. PIN-V4 is already documented as the first all-ACCEPT pin cycle (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/CONSOLIDATED.md:12-20`, `:44-47`). This PIN-V5 CH5 review cannot serve as part of the second consecutive all-ACCEPT cycle until the stale pre-pin convergence/SPEC authority paths above are folded and CH5 is rerun to ACCEPT. If that fold lands and all six PIN-V5 lenses are ACCEPT with zero open critical defects and no orphan unresolved REVISE, the cycle can satisfy `ORCHESTRATOR.md` Section 3Z (`restart/prompts/ORCHESTRATOR.md:104-121`).

## Exact Fold Edits If REVISE

1. Replace or obsolete the pre-pin convergence authority in `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md` before any pin-era S-P1 convergence claim is accepted. The folded file must no longer name `/tmp/skv12-p1`, `/tmp/skv12-profile-target-50bd1648`, or `skv12-p1-replay.tsv` as live authority. It should either be renamed/marked obsolete by the user pin or rewritten to the pin-era basis after PIN-V5 consolidation: capture source `cf7848b2`, capture root `/tmp/skv12-pin-p1`, build root `/tmp/skv12-pin-profile-target-cf7848b2`, tracked replay `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv` with 458 rows, PMU/samply/xctrace status roots under `/tmp/skv12-pin-p1`, and CSS L4 explicitly absent pending generated Track 1 plus lightningcss/equality evidence.

2. Update `restart/skinny/tranches/sk-v12/SPEC.md` before it is treated as dispatch authority under the user pin. Remove `skv12-p1-replay.tsv` from the authority list or replace it with `skv12-p1-pin-replay.tsv`; replace the W0 task text that binds source baseline `50bd1648`, `/tmp/skv12-p1`, replay TSV, and pre-pin self-time TSVs with the pin-era capture manifest/replay roots; and keep CSS L4/Sheets/BBNF-self ordering consistent with the user pin and `HANDOFF.md` if SPEC is no longer historical-only context.

3. After those folds, rerun this exact CH5 check set:

```bash
rg -n "(/tmp/skv12-p1([^a-zA-Z0-9_-]|$)|/tmp/skv12-profile-target-50bd1648|skv12-p1-replay.tsv)" \
  restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md \
  restart/skinny/tranches/sk-v12/SPEC.md \
  restart/skinny/tranches/sk-v12/research/p1/*.md

awk -F '\t' 'NR==1{h=NF; next} {rows++; if(NF!=h){bad++}} END{print "header_fields=" h, "rows=" rows, "bad_field_count=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv

awk -F '\t' 'NR>1 && ($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ || $4=="update-center") {bad++; print NR ":" $0} END{print "bad_replay_cells=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv

find /tmp/skv12-pin-p1 -maxdepth 3 \( -iname '*css*' -o -iname '*lightning*' -o -iname '*sheet*' -o -iname '*bbnf*' \) -print
```

Expected post-fold result: no stale-root authority hits outside explicitly historical prose, zero malformed replay cells, and no CSS/Sheets/BBNF/lightningcss artifacts inside the pin profile root unless a later generated CSS L4 profile capture has been deliberately added and documented.

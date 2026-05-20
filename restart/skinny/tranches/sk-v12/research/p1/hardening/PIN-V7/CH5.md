# SK-V12 S-P1 PIN-V7 CH5 - Hidden Coupling Review

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

1. The PIN-V5 stale-authority blocker remains folded after the PIN-V6 first clean
   cycle. The scoped stale-root regex over
   `HARDENING-S-P1-CONVERGED.md`, `SPEC.md`, and `research/p1/*.md` returns no
   hits for the pre-pin profile root, pre-pin build root, or pre-pin replay TSV.
   The live S-P1 status file now marks pre-pin convergence superseded and binds
   the pin capture/build/replay/status/self-time authorities
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:5`,
   `:13-24`, `:29-47`). `SPEC.md` also binds W0 to `cf7848b2`,
   `/tmp/skv12-pin-p1`, `/tmp/skv12-pin-profile-target-cf7848b2`, and
   `skv12-p1-pin-replay.tsv` (`restart/skinny/tranches/sk-v12/SPEC.md:20-22`,
   `:350-354`).

   ```bash
   git rev-parse HEAD
   # d4a7e3e3f19482688fa42dd1be9cf584f6c3d19b

   git status --short
   # no output before this CH5 file was written

   rg -n '(/tmp/skv12-p1([^a-zA-Z0-9_-]|$)|/tmp/skv12-profile-target-50bd1648|skv12-p1-replay\.tsv)' \
     restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md \
     restart/skinny/tranches/sk-v12/SPEC.md \
     restart/skinny/tranches/sk-v12/research/p1/*.md
   # no output; rg exit 1
   ```

2. The tracked pin replay ledger has no malformed replay cells and no hidden
   replay root. The manifest states the pin replay ledger has 458 command rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:69-72`);
   the rerun confirmed 10 fields, 458 data rows, no bad mode cells, no
   `update-center` corpus-key cells, no pre-pin root references, all replay
   artifact/status/command paths under `/tmp/skv12-pin-p1`, and every non-export
   command using `/tmp/skv12-pin-profile-target-cf7848b2`.

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

3. No hidden local-only profile authority is introduced outside the documented
   pin profile root. The non-pin `/tmp` mentions in scope are bounded:
   `p1f-results-delta.md` uses `/tmp/skv12-p1f-current-results.csv` only as
   extraction scratch from tracked `skinny/RESULTS.md`
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:56-62`,
   `:223`), and `p1c-samply-mode-3.md` fences
   `/tmp/skv11-open-criterion-3ce75df` as W0 Criterion-only diagnostic evidence,
   not wave authority (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:64-87`,
   `:140-142`). `SPEC.md` uses `/tmp/skv12-waveW*-rejected.patch` only as
   failure-path patch locations (`restart/skinny/tranches/sk-v12/SPEC.md:451-454`,
   `:523-525`).

4. CSS L4 is not substituted by Sheets, BBNF-self, JSON rows, report fixtures,
   root snippets, or lightningcss-only evidence. The user pin makes CSS L4 the
   authoritative first target and Sheets/BBNF-self fallback-only after measured
   CSS redress (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`);
   `HANDOFF.md` carries the same ordering and close bar
   (`restart/skinny/tranches/sk-v12/HANDOFF.md:51-69`, `:76-90`, `:112-126`).
   The S-P1 packet records CSS L4 as absent, not substituted:
   `HARDENING-S-P1-CONVERGED.md:55-76`, `p1a-samply-mode-1.md:145-158`,
   `p1c-samply-mode-3.md:91-110`, `p1e-hot-leaf-attribution.md:47-50`, and
   `p1f-results-delta.md:85-93`, `:157-169`, `:205-210`.

   ```bash
   find /tmp/skv12-pin-p1 -maxdepth 3 \( -iname '*css*' -o -iname '*lightning*' -o -iname '*sheet*' -o -iname '*bbnf*' \) -print
   # no output

   awk -F '\t' 'NR>1 && tolower($2 FS $3 FS $4 FS $5) ~ /(css|lightningcss|sheets|bbnf)/ {hits++; print NR ":" $2 ":" $3 ":" $4 ":" $5} END{print "semantic_nonjson_key_hits=" hits+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   # semantic_nonjson_key_hits=0
   ```

5. Orphan SIMD and generated-size/O(N) work remains routed to later pass/wave
   gates, not promoted by S-P1. The user pin makes the five orphan aarch64
   primitives wave-eligible only with a same-commit consumer and keeps
   `escape_mask_64` resolution mandatory before new SIMD admission
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-78`,
   `:98-106`). `HANDOFF.md` requires generated CSS runtime LOC, module byte
   size, regen/check command, O(N) grammar-size status, Lock 16 status, and
   same-wave consumer fields before admission (`restart/skinny/tranches/sk-v12/HANDOFF.md:119-125`,
   `:144-155`). P1-B/P1-C/P1-E/P1-F keep those as routed prerequisites rather
   than S-P1 promotions (`p1b-samply-mode-2.md:206-213`,
   `p1c-samply-mode-3.md:83-87`, `p1e-hot-leaf-attribution.md:187-191`,
   `p1f-results-delta.md:163-169`).

6. The pin capture status, export status, source anchors, and Track 1/Track 2
   split still match PIN-V6. PMU is 82/82 PASS, samply 82/82 PASS, xctrace
   212/212 PASS, `rc=54` logs are 185/185 stdout-backed, XML exports are 82/82
   `SKIP` for already-present nonzero exports, and hot-leaf anchors remain clean.
   Track 2/oracle families remain context rather than generated Track 1
   antecedents (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:171-176`,
   `:193-205`; `p1e-hot-leaf-attribution.md:32-50`, `:167-178`).

   ```bash
   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "pmu_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
   # pmu_rows=82 bad_status=0

   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "samply_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
   # samply_rows=82 bad_status=0

   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "xctrace_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   # xctrace_rows=212 bad_status=0

   awk -F '\t' 'NR>1 && $6==54 {print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | while IFS= read -r f; do if rg -q 'Output file saved as' "$f" && rg -q 'Reached specified time limit|Target app exited' "$f"; then printf 'ok\n'; else printf 'bad\t%s\n' "$f"; fi; done | awk 'BEGIN{ok=0; bad=0} $1=="ok"{ok++} $1=="bad"{bad++} END{print "rc54_ok=" ok+0, "rc54_bad=" bad+0}'
   # rc54_ok=185 rc54_bad=0

   awk -F '\t' 'NR>1{total++; if($4!="SKIP") bad++} END{print "export_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
   # export_rows=82 bad_status=0

   awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none" || $15=="" || $16=="") bad++} END{print "summary_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
   # summary_rows=82 bad_anchors=0

   awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none" || $8=="" || $9=="") bad++} END{print "detail_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
   # detail_rows=410 bad_anchors=0

   awk -F '\t' 'NR>1{count[$1"/"$3]++} END{for(k in count) print k, count[k]}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv | sort
   # direct/track1 17
   # direct/track2 17
   # parse/track1 17
   # parse/track2 17
   # typed/real_typed_track1 7
   # typed/real_typed_track2 7
   ```

7. PIN-V6 is recorded as the first all-ACCEPT cycle after the PIN-V5 reset
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V6/CONSOLIDATED.md:19-20`,
   `:40-43`). This CH5 PIN-V7 result finds no hidden-coupling blocker for the
   second clean CH5 cycle. Pass-level convergence still depends on all lenses
   returning ACCEPT with zero open critical defects and no orphan unresolved
   REVISE under `ORCHESTRATOR.md` Section 3Z
   (`restart/prompts/ORCHESTRATOR.md:104-123`;
   `restart/prompts/skinny/PASS-1-PROFILE.md:177-180`).

## Exact Fold Edits If REVISE

None.

# SK-V12 S-P1 PIN-V4 CH5 - Hidden Coupling Review

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

1. The PIN-V1/PIN-V2/PIN-V3 fold chain is present and rechecked. PIN-V1 removed
   stale pre-pin replay authority, split Track 1/Track 2 hot-family summaries,
   and routed generated-size/O(N) checks (`PIN-V1/CONSOLIDATED.md:25-35`).
   PIN-V2 repaired malformed `samply-parse` mode/corpus cells and added the
   replay-ledger sanity checks (`PIN-V2/CONSOLIDATED.md:25-35`). PIN-V3 then
   normalized the remaining PMU `update_center` corpus keys and reran the mode,
   corpus, and xctrace `rc=54` checks (`PIN-V3/CONSOLIDATED.md:25-34`).

2. Current pin authority is single-root and not stale pre-pin authority. The
   manifest pins capture root `/tmp/skv12-pin-p1`, build root
   `/tmp/skv12-pin-profile-target-cf7848b2`, and the two replay binaries
   (`skv12-p1-capture-manifest.md:16-20`). It explicitly marks `/tmp/skv12-p1`,
   `/tmp/skv12-profile-target-50bd1648`, and `skv12-p1-replay.tsv` historical
   only (`skv12-p1-capture-manifest.md:51-53`). Live P1 references to pre-pin
   files are likewise historical/format/delta context, not authority:
   P1-A says the pre-pin replay is historical only (`p1a-samply-mode-1.md:78-83`,
   `:190-195`), P1-D uses `/tmp/skv12-p1` only as prior-delta context
   (`p1d-pmu-cycles.md:217-226`), and P1-E says no pre-pin hot-leaf rows are
   carried as fresh claims (`p1e-hot-leaf-attribution.md:75-78`).

3. The replay ledger has no malformed cells left. Commands run:

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

   The tracked ledger also matches the manifest's row accounting: 82 PMU, 82
   samply, 82 primary xctrace Time Profiler, 82 xctrace CPU Counters, 48 product
   xctrace Time Profiler, and 82 xctrace export rows
   (`skv12-p1-capture-manifest.md:60-73`).

4. There is no hidden profile-artifact dependency outside the documented pin
   root/build root. I checked that every replay artifact and status path points
   under `/tmp/skv12-pin-p1`, and every non-export replay command uses the pin
   build root:

   ```bash
   awk -F '\t' 'NR>1 && ($7 !~ /^\/tmp\/skv12-pin-p1\// || $8 !~ /^\/tmp\/skv12-pin-p1\// || $9 !~ /\/tmp\/skv12-pin-p1\//) {bad++; print NR ":" $1 ":" $7 ":" $8} END{print "bad_pin_root_artifacts=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   # bad_pin_root_artifacts=0

   awk -F '\t' 'NR>1 && $1!="xctrace-export" && $9 !~ /\/tmp\/skv12-pin-profile-target-cf7848b2\/release\// {bad++; print NR ":" $1 ":" $9} END{print "non_export_rows_missing_pin_target=" bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
   # non_export_rows_missing_pin_target=0
   ```

   The one extra local temp named by P1-F, `/tmp/skv12-p1f-current-results.csv`,
   is generated from tracked `skinny/RESULTS.md` by the method block
   (`p1f-results-delta.md:56-62`) and is rendered back into the document's row
   tables; it is not replay/profile authority.

5. Tool roles remain decoupled. The manifest separates PMU command/status
   authority, samply command/status authority, xctrace capture status, XML export
   status, and derived hot-leaf TSVs (`skv12-p1-capture-manifest.md:57-68`).
   P1-D refuses to infer missing branch/L1/LLC counters or derive cycles/B from
   xctrace/samply companion captures (`p1d-pmu-cycles.md:67-80`). P1-E treats
   xctrace XML-derived tables as hot-leaf authority and PMU/samply as companion
   evidence (`p1e-hot-leaf-attribution.md:82-92`).

6. Track 2/oracle work is not promoted into generated Track 1 antecedents. The
   manifest states that Track 2/oracle-only families are guard/comparator
   context, not generated Track 1 optimization antecedents
   (`skv12-p1-capture-manifest.md:193-205`). P1-B makes the same split for
   product rows (`p1b-samply-mode-2.md:33-50`, `:160-180`), and P1-E preserves
   the split in both the family table and derivation requirements
   (`p1e-hot-leaf-attribution.md:32-45`, `:143-147`, `:167-178`). Direct hot-leaf
   split check:

   ```bash
   awk -F '\t' 'NR>1{count[$1"/"$3]++} END{for(k in count) print k, count[k]}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv | sort
   # direct/track1 17
   # direct/track2 17
   # parse/track1 17
   # parse/track2 17
   # typed/real_typed_track1 7
   # typed/real_typed_track2 7
   ```

7. CSS L4 is not substituted by Sheets, root-workspace snippets, report fixtures,
   JSON rows, or lightningcss-only evidence. The user pin makes CSS L4 first
   target and Sheets/BBNF-self fallback-only after a CSS redress attempt
   (`USER-PIN-W1-CSS-L4-SOTA.md:18-35`). P1-A rejects report fixtures and
   lightningcss-only/root snippets as substitutes for a skinny generated Track 1
   parser (`p1a-samply-mode-1.md:145-158`). P1-C records no CSS/non-JSON/Sheets
   command in the pin root and requires generated CSS Track 1 plus strict
   lightningcss comparator/equality before CSS hot-leaf or SOTA claims
   (`p1c-samply-mode-3.md:59-62`, `:91-110`). P1-F records zero admitted CSS L4
   rows, zero generated Sheets/BBNF-self rows, and no JSON row filling the
   `lightningcss_mbps + 1` close bar (`p1f-results-delta.md:22-36`, `:80-93`,
   `:157-169`, `:196-210`). I also ran:

   ```bash
   find /tmp/skv12-pin-p1 -maxdepth 3 -iname '*css*' -o -iname '*lightning*'
   # no output
   ```

8. SIMD/orphan and generated-size/O(N) work is routed, not promoted before S-P2.
   The user pin makes orphan SIMD wave-eligible only with a same-commit consumer
   and keeps `escape_mask_64` resolution mandatory before new SIMD admission
   (`USER-PIN-W1-CSS-L4-SOTA.md:71-78`, `:98-106`). P1-A and P1-B propose no
   SIMD/union/ASM-gen route from profile evidence alone
   (`p1a-samply-mode-1.md:181-186`, `p1b-samply-mode-2.md:206-213`). P1-C says
   any new union/ASM-gen candidate still needs material differential, fresh
   profile antecedent, micro-proof, scalar/parity or checkasm coverage,
   same-wave consumer, and CHALLENGE acceptance (`p1c-samply-mode-3.md:83-87`).
   The handoff requires generated CSS runtime LOC, module byte size,
   regen/check command, and O(N) grammar-size guard before W1b redress, then
   routes `escape_mask_64` before SIMD admission (`HANDOFF.md:119-125`), and the
   telemetry binding keeps Lock 16, generated LOC, module byte size, O(N)
   status, same-wave consumer, JSON guard state, and REDRESS id in the same gate
   field set (`HANDOFF.md:142-155`).

9. Source-anchor and status checks still pass. Commands run:

   ```bash
   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "pmu_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
   # pmu_rows=82 bad_status=0

   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "samply_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
   # samply_rows=82 bad_status=0

   awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "xctrace_rows=" total, "bad_status=" bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
   # xctrace_rows=212 bad_status=0

   awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++} END{print "summary_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
   # summary_rows=82 bad_anchors=0

   awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++} END{print "detail_rows=" n, "bad_anchors=" bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
   # detail_rows=410 bad_anchors=0
   ```

10. This ACCEPT can count as the first all-ACCEPT pin cycle only if the other
    PIN-V4 lenses agree. `ORCHESTRATOR.md` §3Z still requires `>=95% ACCEPT` for
    two consecutive cycles with zero open critical defects and no orphan
    unresolved REVISE, or explicit user sign-off (`ORCHESTRATOR.md:104-121`).
    This CH5 review finds no hidden-coupling blocker, but it does not waive the
    pass-level convergence rule.

## Exact Fold Edits If REVISE

None.

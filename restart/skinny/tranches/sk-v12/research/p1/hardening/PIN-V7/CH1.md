# SK-V12 S-P1 PIN-V7 CH1 Correctness Review

Verdict: ACCEPT
Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

- Convergence rule is understood and met from the CH1 side. The orchestrator
  requires >=95% ACCEPT for two consecutive cycles with zero open critical
  defects and no orphan unresolved REVISE
  (`restart/prompts/ORCHESTRATOR.md:118-123`), and the S-P1 profile prompt
  repeats the same handoff rule (`restart/prompts/skinny/PASS-1-PROFILE.md:177-180`).
  PIN-V5 broke the count with one CH5 REVISE
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:12-20`)
  and required two new clean cycles after its fold
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:41-43`).
  PIN-V6 is six ACCEPT / zero REVISE / zero REJECT and is explicitly the first
  clean cycle after the reset
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V6/CONSOLIDATED.md:12-20`).
  This CH1 review finds no correctness blocker to PIN-V7 serving as the second
  consecutive clean cycle with PIN-V6; the cycle-level decision still depends
  on the other PIN-V7 lenses also returning ACCEPT.

- Replay schema is correct. The tracked pin replay ledger has the expected
  10-field header at
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:1`
  (`lane`, `family`, `plane`, `corpus`, `mode`, `cwd`, `artifact`,
  `status_artifact`, `command`, `notes`) and the manifest states it contains
  458 pin-era command rows split as 82 PMU, 82 samply, 212 xctrace capture, and
  82 xctrace export rows
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:69-72`).
  Command run:

  ```sh
  awk -F '\t' 'NR==1{print "header_fields", NF; next}
    {rows++; fields[NF]++; lane[$1]++; if($4=="update-center") bad_update_hyphen++;
     if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) bad_mode++}
    END{print "rows", rows; for (f in fields) print "field_count", f, fields[f];
    for (l in lane) print "lane", l, lane[l]; print "bad_update_hyphen", bad_update_hyphen+0;
    print "bad_mode", bad_mode+0}' \
    restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  ```

  Output: `header_fields 10`, `rows 458`, `field_count 10 458`,
  `lane pmu 82`, `lane samply 82`, `lane xctrace-time-profiler-primary 82`,
  `lane xctrace-cpu-counters 82`, `lane xctrace-time-profiler-product-v2 48`,
  `lane xctrace-export 82`, `bad_update_hyphen 0`, `bad_mode 0`.

- Corpus keys and canonical modes are clean. S-P1 requires all 17 JSON corpora
  (`restart/prompts/skinny/PASS-1-PROFILE.md:67-77`), and the pin replay ledger
  uses `update_center`, not the stale command-alias spelling `update-center`
  (examples at
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:32-33`
  and `:454-457`). Command run:

  ```sh
  awk -F '\t' 'BEGIN{split("twitter citm_catalog canada apache_builds github_events update_center mesh random gsoc-2018 marine_ik instruments numbers unicode_mixed unicode_escapes unicode_basic distinct_values y_string_unicode", e, " "); for(i in e) want[e[i]]=1}
    NR>1{have[$4]=1}
    END{missing=0; extra=0; for (k in want) if(!(k in have)) missing++;
    for (k in have) if(!(k in want)) extra++;
    print "corpus_set_check", length(have), "missing", missing, "extra", extra}' \
    restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  ```

  Output: `corpus_set_check 17 missing 0 extra 0`. A separate mode-count check
  returned `track1 187`, `track2 187`, `real_typed_track1 42`,
  `real_typed_track2 42`, with no malformed modes.

- Capture/status artifact counts match the manifest. The replay surface and
  coverage tables require 82 PMU rows, 82 samply rows, 212 xctrace captures,
  82 XML exports, and 82 / 410 hot-leaf summary/detail rows
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:59-67`,
  `:116-124`). Commands run:

  ```sh
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "pmu_status", total, bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "samply_status", total, bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "xctrace_status", total, bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
  awk -F '\t' 'NR>1{total++; if($4!="SKIP") bad++} END{print "xml_export_status", total, bad+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
  awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++} END{print "hot_leaf_summary", n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
  awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++} END{print "hot_leaf_details", n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
  ```

  Output: `pmu_status 82 0`, `samply_status 82 0`,
  `xctrace_status 212 0`, `xml_export_status 82 0`,
  `hot_leaf_summary 82 0`, `hot_leaf_details 410 0`. I also checked
  nonzero artifacts for PMU stdout, samply artifacts, xctrace traces, and XML
  exports; all returned zero missing/empty rows.

- xctrace `rc=54` is not a bare waiver. The manifest requires accepted stop/save
  strings in the stdout path recorded by `capture_status.tsv`
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:108-112`).
  Commands run:

  ```sh
  awk -F '\t' 'NR>1{rc[$6]++} END{for (r in rc) print "xctrace_rc", r, rc[r]}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | sort
  awk -F '\t' 'NR>1 && $6==54 {print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv |
    while IFS= read -r f; do
      if rg -q 'Output file saved as' "$f" &&
         rg -q 'Reached specified time limit|Target app exited' "$f"; then
        printf 'ok\n'
      else
        printf 'bad\t%s\n' "$f"
      fi
    done |
    awk 'BEGIN{ok=0;bad=0} $1=="ok"{ok++} $1=="bad"{bad++} END{print "rc54_stdout_policy", ok, bad}'
  ```

  Output: `xctrace_rc 0 27`, `xctrace_rc 54 185`,
  `rc54_stdout_policy 185 0`.

- Capture-source authority is pin-era, not pre-pin. The manifest binds capture
  source `cf7848b2`, capture root `/tmp/skv12-pin-p1`, build root
  `/tmp/skv12-pin-profile-target-cf7848b2`, and the two profile binaries
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:8-20`).
  The S-P1 hardening status repeats the same authority surface and names the
  pin replay/status/self-time TSVs
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:27-48`).
  SPEC also lists `skv12-p1-pin-replay.tsv` as authority and binds the same pin
  roots into W0 profile-lock work
  (`restart/skinny/tranches/sk-v12/SPEC.md:16-23`, `:350-354`).

- The stale pre-pin authority blocker from PIN-V5 is folded. PIN-V5 required
  demoting the stale pre-pin convergence and SPEC profile authority paths
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:24-37`),
  and PIN-V6 rechecked that the blocker was folded
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V6/CONSOLIDATED.md:24-26`).
  Command run over the current live authority docs:

  ```sh
  rg -n '(/tmp/skv12-p1([^A-Za-z0-9_-]|$)|/tmp/skv12-profile-target-50bd1648|skv12-p1-replay.tsv)' \
    restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md \
    restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md \
    restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md \
    restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md \
    restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md \
    restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md \
    restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md \
    restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md \
    restart/skinny/tranches/sk-v12/SPEC.md
  ```

  Output: no matches (`stale_profile_authority_refs_live_docs 0`). Historical
  hardening cycle files still preserve old-root discussion, but those files are
  archived review evidence rather than live S-P1 authority.

- CSS L4 absence is correctly bounded. The user pin makes CSS L4 authoritative
  and raises the close bar to generated CSS L4 beating lightningcss
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-34`).
  The S-P1 status says the pin profile root has no generated CSS L4 Track 1
  runtime, no same-plane lightningcss comparator, and no strict equality oracle,
  and routes that absence to S-P2/S-P3 rather than claiming CSS behavior
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:55-59`,
  `:73-76`). The manifest says the same
  (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:171-176`).
  Command run:

  ```sh
  rg -n 'css|CSS|lightningcss|sheets|Sheets|bbnf_self|BBNF' /tmp/skv12-pin-p1
  ```

  Output: no matches (`pin_root_css_sheets_bbnf_refs 0`). This is not a CH1
  correctness blocker because S-P1 is not claiming CSS L4 profile coverage.

## Exact Fold Edits If REVISE

Not applicable; verdict ACCEPT.

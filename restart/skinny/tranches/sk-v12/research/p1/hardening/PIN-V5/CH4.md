# SK-V12 S-P1 PIN-V5 CH4 Cost / Replayability Review

Verdict: ACCEPT

Score: 98%

## Blocking Findings

None.

## Nonblocking Notes

- Review base and scope: `git rev-parse HEAD` returned
  `ecda8b131efca2fbf9a4acfe67efef2a3c13e8b4`, matching the requested
  `ecda8b13` base; the first `git status --short` was empty. A later
  `git status --short` showed the untracked `hardening/PIN-V5/` directory from
  concurrent challenge output; this review writes only this CH4 file. The CH4
  contract rejects missing rerun commands, run id, host triple, or build flags
  (`restart/prompts/skinny/PASS-1-PROFILE.md:143-146`), and S-P1 convergence
  needs two consecutive clean challenge cycles
  (`restart/prompts/ORCHESTRATOR.md:104-121`;
  `restart/prompts/skinny/PASS-1-PROFILE.md:177-180`).

- Replay identity is explicit. The manifest records capture source
  `cf7848b2`, initial S-P1 fold `b1043383`, prior PIN-V2 review base
  `d4ef80b2`, capture root `/tmp/skv12-pin-p1`, build root
  `/tmp/skv12-pin-profile-target-cf7848b2`, host/tool versions, and the exact
  cargo build command (`skv12-p1-capture-manifest.md:8-49`). The binary
  executability check
  `test -x /tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe && test -x /tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct`
  printed `binaries executable`.

- Replay row counts match the manifest and the tracked ledger shape. I ran:

  ```bash
  awk -F '\t' 'NR>1{total++; lane[$1]++; family[$2]++} END{print "total", total; for (k in lane) print k, lane[k]; for (k in family) print k, family[k]}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  awk -F '\t' 'NF!=10{print NR, NF}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  ```

  Results: 458 data rows, zero malformed-width rows, and the expected split of
  82 PMU, 82 samply, 82 primary Time Profiler, 82 CPU Counters, 48 product-v2
  Time Profiler, and 82 XML export rows. The manifest states the same split
  (`skv12-p1-capture-manifest.md:60-73`), and the replay TSV header carries
  explicit `artifact`, `status_artifact`, `command`, and `notes` columns
  (`skv12-p1-pin-replay.tsv:1`).

- The PIN-V3 corpus/mode replay defects remain folded. I reran:

  ```bash
  awk -F '\t' 'NR>1 && $5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ {bad++} END{print bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  awk -F '\t' 'NR>1 && $4=="update-center" {bad++} END{print bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  ```

  Both printed `0`. Representative `update_center` rows retain the
  `update-center.json` launch/file alias only in the command operand
  (`skv12-p1-pin-replay.tsv:66-67`, `:148-149`, `:230-231`, `:312-313`,
  `:360-361`, `:376-377`, `:454-457`), matching the PIN-V3 fold requirement
  (`hardening/PIN-V3/CONSOLIDATED.md:21-34`).

- Profiler lane status coverage is complete for the admitted pin surface. I ran:

  ```bash
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print total, bad+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print total, bad+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
  awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print total, bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
  ```

  Results were `82 0`, `82 0`, and `212 0`. `wc -l` also showed 82 commands in
  `/tmp/skv12-pin-p1/pmu/pmu-commands.sh` and 82 commands in
  `/tmp/skv12-pin-p1/samply/samply-commands.sh`. Completion stamps matched the
  manifest: PMU `2026-05-20T18:05:34Z`, samply `2026-05-20T18:15:35Z`, xctrace
  `2026-05-20T18:40:17Z` (`skv12-p1-capture-manifest.md:21-27`).

- Command, log, artifact, and status paths are present. I ran:

  ```bash
  awk -F '\t' 'NR>1{if(system("test -e " $7)!=0) bad++} END{print bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  awk -F '\t' 'NR>1{if(system("test -f " $8)!=0) bad++} END{print bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
  awk -F '\t' 'NR>1{if(system("test -e " $8)!=0) missing_stdout++; if(system("test -e " $9)!=0) missing_stderr++} END{print missing_stdout+0, missing_stderr+0}' /tmp/skv12-pin-p1/pmu/capture_status.tsv
  awk -F '\t' 'NR>1{if(system("test -e " $8)!=0) missing_artifact++} END{print missing_artifact+0}' /tmp/skv12-pin-p1/samply/capture_status.tsv
  awk -F '\t' 'NR>1{if(system("test -e " $8)!=0) missing_artifact++; if(system("test -e " $9)!=0) missing_stdout++; if(system("test -e " $10)!=0) missing_stderr++} END{print missing_artifact+0, missing_stdout+0, missing_stderr+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
  ```

  Results were `0`, `0`, `0 0`, `0`, and `0 0 0`.

- `rc=54` acceptance remains stdout-backed, not inferred from return code alone.
  The manifest says `rc=54` is accepted only when the stdout path records both an
  accepted stop condition and `Output file saved as`
  (`skv12-p1-capture-manifest.md:109-113`). I reran:

  ```bash
  awk -F '\t' 'NR>1 && $6==54 {print $9}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv | while IFS= read -r f; do if rg -q 'Output file saved as' "$f" && rg -q 'Reached specified time limit|Target app exited' "$f"; then echo ok; else echo bad; fi; done | awk '{if($1=="ok") ok++; else bad++} END{print ok+0, bad+0}'
  awk -F '\t' 'NR>1{if($6==54) rc54++; else other[$6]++} END{print "rc54", rc54+0; for (k in other) print "rc" k, other[k]}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
  ```

  Results were `185 0`, with `rc54 185` and `rc0 27`.

- XML export `SKIP` semantics are honest and replayable. The export status TSV
  uses `SKIP` because nonzero XML exports already existed; it is not relabeled as
  `PASS` (`skv12-p1-capture-manifest.md:67`, `:124`, `:159-161`;
  `p1e-hot-leaf-attribution.md:84-90`). I ran:

  ```bash
  awk -F '\t' 'NR>1{total++; if($4!="SKIP") bad++} END{print total, bad+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
  awk -F '\t' 'NR>1{if(system("test -e " $1)!=0) missing_trace++; if(system("test -s " $2)!=0) missing_export++} END{print missing_trace+0, missing_export+0}' /tmp/skv12-pin-p1/time_profile_export_status.tsv
  ```

  Results were `82 0` and `0 0`.

- Hot-leaf source anchors are concrete for the admitted xctrace-derived tables.
  The packet records 82 summary rows and 410 detail rows
  (`skv12-p1-capture-manifest.md:67-68`, `:163-169`;
  `p1e-hot-leaf-attribution.md:21-30`). I reran:

  ```bash
  awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++} END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
  awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++} END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
  ```

  Results were `82 0` and `410 0`. Samply is retained as companion artifact
  evidence; xctrace Time Profiler XML is the source-self-time authority, which is
  explicit in P1-E (`p1e-hot-leaf-attribution.md:82-92`).

- Mode III and CSS L4 absence are disclosed boundaries, not hidden local
  assumptions. The manifest says the pin root captures parse, direct, and typed
  JSON lanes only, with Mode III absent and CSS L4 unprofiled until generated CSS
  L4 Track 1 runtime, same-plane lightningcss comparator, and strict equality
  oracle rows exist (`skv12-p1-capture-manifest.md:172-177`). P1-C independently
  records zero fresh Mode III/CSS evidence under `/tmp/skv12-pin-p1`
  (`p1c-samply-mode-3.md:45-62`, `:96-110`).

- Replayability conclusion: the packet is replayable for this workspace without
  hidden local assumptions. It is host-bound to the declared macOS/xctrace/samply
  and Rust stack, but the capture source, build command, binary paths, `/tmp`
  roots, command rows, log paths, status TSVs, and export/hot-leaf derivation
  inputs are all explicit and present. PIN-V4 was the first all-ACCEPT pin cycle
  (`hardening/PIN-V4/CONSOLIDATED.md:19-20`) and routed PIN-V5 as the potential
  second (`hardening/PIN-V4/CONSOLIDATED.md:44-47`). This CH4 ACCEPT can count
  toward that second consecutive all-ACCEPT cycle; the cycle converges only if
  the PIN-V5 consolidation records all six lenses ACCEPT with zero open critical
  defects and no orphan unresolved REVISE.

## Exact Fold Edits If REVISE

None. Verdict is ACCEPT.

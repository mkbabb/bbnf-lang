# PIN-V1 CH5 - Hidden Coupling Review

Verdict: REVISE

Score: 74%

## Blocking Findings

1. The final PASS fold still carries stale partial-capture blockers as live text.
   This makes the same pin root both complete and unavailable, so downstream S-P2
   can either cite or reject the same hot-leaf evidence depending on which section
   it reads.

   Evidence:

   - `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:23-35`
     declares complete PMU/samply/xctrace/product hot-leaf authority, but
     `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:163-190`
     says hot-leaf attribution is unavailable and lists those same paths as
     missing; `:207-212` repeats that accepted product samply/xctrace attribution
     is absent.
   - `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:19-31`
     declares final hot-leaf authority, but `:75-93`, `:95-118`, and `:165-193`
     retain the in-progress inventory, blocker list, and all-unavailable ledger.
   - `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:29-37`
     declares final parse hot-leaf artifacts, but `:80-89` and `:137-145` still
     say parse samply/xctrace exports and derived hot-leaf tables are absent.

2. Track 1 and Track 2 are preserved in the raw TSVs, but the folded leading
   family summaries aggregate them back together at the plane level. That hides
   oracle/comparator-only hot leaves inside generated-profile summaries.

   Evidence:

   - `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:32-40`
     and `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:33-39`
     summarize top families by `plane` only.
   - Artifact path `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`: grouping
     by `plane,mode,top1_family` shows `runtime_support` direct rows are all
     `direct/track2` (14 rows), while `serde_json_oracle_read_parse` typed rows
     are all `typed/real_typed_track2` (7 rows). These are not generated Track 1
     antecedents.
   - This conflicts with the pinned separation in
     `restart/skinny/tranches/sk-v12/HANDOFF.md:53-57` and the P1-B warning that
     Track 2 is an oracle surface, not a same-output-plane CSS comparator
     (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:160-161`).

3. The export status vocabulary is folded as PASS while the cited status artifact
   records SKIP for every row. The XML files exist and are nonzero, but the
   manifest/docs should not call the status artifact 82/82 PASS without defining
   SKIP as an accepted "already exported" state.

   Evidence:

   - `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:31`
     says Time Profiler XML exports are PASS.
   - `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:35` and
     `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:29` cite
     `/tmp/skv12-pin-p1/time_profile_export_status.tsv` as PASS authority.
   - Artifact path `/tmp/skv12-pin-p1/time_profile_export_status.tsv`: all 82
     data rows have `status=SKIP`, while `/tmp/skv12-pin-p1/parse-xctrace/exports`
     contains 34 XML files and `/tmp/skv12-pin-p1/direct-xctrace/exports-v2`
     contains 48 XML files.

4. The generated-size / O(N) prerequisite is not routed in the pin-aware handoff
   consumed by this fold. Lock 16 is routed, but generated CSS runtime size
   accounting is not visible in the S-P1 packet or HANDOFF dispatch requirements.

   Evidence:

   - `restart/skinny/tranches/sk-v12/HANDOFF.md:112-125` lists S-P3 CSS row,
     comparator, GrammarConfig, escape-mask, fallback, union, and ASM-gen duties,
     but no generated LOC/module ceiling, regen/check command, O(N) grammar-size
     guard, or overflow response.
   - `restart/skinny/tranches/sk-v12/HANDOFF.md:141-147` lists telemetry fields
     consumed by the CSS gate, but no generated-size field.
   - `rg "generated-size|generated size"` over the six S-P1 docs and handoff has
     no hit.

## Nonblocking Notes

- CSS L4 is not replaced by Sheets, root CSS snippets, or reference-only report
  fixtures. The fold explicitly rejects those substitutions in P1-A
  (`p1a-samply-mode-1.md:154-159`), P1-C (`p1c-samply-mode-3.md:91-110`), and
  P1-F (`p1f-results-delta.md:85-93`, `:161-169`).
- Direct, product, and typed PMU rows are mostly separated correctly in P1-B and
  P1-D; the coupling defect is the folded hot-family summary, not the raw PMU TSV.
- No evidence found that a sidecar comparator `n/a` is treated as a win in the
  pin fold or hot-leaf TSVs.
- Lock 16 remains routed through the USER PIN and HANDOFF
  (`USER-PIN-W1-CSS-L4-SOTA.md:73-78`, `:98-99`; `HANDOFF.md:81`, `:121`,
  `:146`, `:162-163`). It needs the generated-size route restored beside it.

## Exact Fold Edits Required

1. In P1-A, P1-B, and P1-E, remove the obsolete partial-capture sections or move
   them under an explicitly historical note. The live body must agree with the
   final orchestrator fold and the `/tmp/skv12-pin-p1` artifacts.
2. Replace P1-B and P1-E leading-family summaries with mode-split tables:
   `parse/track1`, `parse/track2`, `direct/track1`, `direct/track2`,
   `typed/real_typed_track1`, and `typed/real_typed_track2`. Add a sentence that
   Track 2/oracle-only families are not generated Track 1 optimization
   antecedents.
3. Update P1-F `:186-188` so it says RESULTS hot leaves remain Criterion
   bindings, while P1-E has resolved separate pin-era xctrace hot-leaf tables.
4. Correct the export-status wording in the manifest, P1-A, and P1-B: either
   regenerate `/tmp/skv12-pin-p1/time_profile_export_status.tsv` with PASS rows,
   or define `SKIP` as accepted preexisting export and change the coverage label
   from PASS to "present/nonzero; status=SKIP".
5. Add generated-size/O(N) routing to the pin handoff or S-P1 fold handoff notes:
   expected generated CSS runtime LOC, generated module ceiling, regen/check
   command, grammar-size O(N) guard, and overflow disposition before W1b redress.

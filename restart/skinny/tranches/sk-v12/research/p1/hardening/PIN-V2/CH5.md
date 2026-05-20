# SK-V12 S-P1 PIN-V2 CH5 - Hidden Coupling Review

Verdict: ACCEPT

Score: 94%

## Blocking Findings

None.

## Nonblocking Notes

1. Track 1 / Track 2 separation is preserved in both raw hot-leaf TSVs and the
   folded summaries. `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`
   splits into `parse/track1` 17, `parse/track2` 17, `direct/track1` 17,
   `direct/track2` 17, `typed/real_typed_track1` 7, and
   `typed/real_typed_track2` 7; the detail TSV carries the same split at five
   rows per summary row. The fold mirrors that split in P1-A
   (`p1a-samply-mode-1.md:137-143`), P1-B
   (`p1b-samply-mode-2.md:32-46`), P1-E
   (`p1e-hot-leaf-attribution.md:32-45`), and the capture manifest
   (`skv12-p1-capture-manifest.md:173-185`).

2. The prior oracle-family coupling is fixed. Raw summary grouping shows
   `direct/track1/output_digest_hash` has all 17 direct Track 1 top rows,
   while `direct/track2/runtime_support` has 14 Track 2 rows and
   `typed/real_typed_track2/serde_json_oracle_read_parse` has all 7 typed Track
   2 rows. The fold explicitly says Track 2/oracle-only families are guard or
   comparator context, not generated Track 1 optimization antecedents.

3. Direct, parse, and typed profile surfaces are not normalized into one
   another. `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` remains 34 parse rows;
   `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv` splits into 34 direct rows and
   14 typed guard rows. P1-B keeps direct and typed product interpretation
   separate, and P1-D reports separate aggregate and per-row PMU tables for
   parse, direct, and typed guards (`p1d-pmu-cycles.md:85-114`,
   `:155-211`).

4. Tool roles are clear enough for CH5. PMU TSVs are cycles/Mbps/cost evidence,
   not self-time attribution; xctrace Time Profiler XML-derived summary/detail
   TSVs are the hot-leaf authority; samply is retained as companion artifact
   evidence. The manifest states this split at `skv12-p1-capture-manifest.md:55-66`,
   and P1-E repeats it in the artifact inventory at
   `p1e-hot-leaf-attribution.md:84-92`.

5. CSS L4 has not been replaced by Sheets, root-workspace CSS snippets, or
   reference-only artifacts. P1-A rejects `nonjson-pass-css-l4.json`, root CSS
   snippets, report fixtures, and lightningcss-only runs as substitutes
   (`p1a-samply-mode-1.md:145-158`). P1-C records no CSS/non-JSON/sheets command
   in the pin root and requires generated CSS Track 1 plus strict lightningcss
   comparator before any CSS profile/SOTA claim (`p1c-samply-mode-3.md:96-110`).
   P1-F records zero admitted CSS L4 rows and zero generated Sheets/BBNF-self
   rows (`p1f-results-delta.md:80-93`, `:177-182`).

6. I found no sidecar `n/a` paper win in the pin fold. The hot-leaf TSVs have no
   `n/a`, `unavailable`, `unknown`, `none`, or line-zero source anchors, and the
   P1-F fold treats current JSON rows as guard/diagnostic evidence rather than a
   CSS lightningcss comparator or CSS admission surface
   (`p1f-results-delta.md:30-33`, `:207-210`).

7. Generated-size/O(N) prerequisites are now routed beside Lock 16. HANDOFF
   requires generated CSS runtime LOC, module byte size, regen/check command,
   and O(N) grammar-size guard before W1b, then immediately routes the
   `escape_mask_64`/SIMD prerequisite (`HANDOFF.md:119-125`). The CSS gate
   telemetry list also includes Lock 16 status, generated LOC, generated module
   byte size, O(N) grammar-size status, and same-wave consumer class in the same
   field set (`HANDOFF.md:144-151`).

## Exact Fold Edits

None required for CH5.

# SK-V12 S-P1 PIN-V3 CH5 - Hidden Coupling Review

Verdict: ACCEPT

Score: 96%

## Blocking Findings

None.

## Nonblocking Notes

1. Track 1 / Track 2 separation is preserved in the replay ledger, manifest,
   and hot-leaf TSVs. Direct validation of
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv` found
   458 data rows, 10 fields per row, zero bad field counts, zero noncanonical
   modes, and zero bad planes. The derived hot-leaf summary splits into exactly
   `parse/track1` 17, `parse/track2` 17, `direct/track1` 17, `direct/track2`
   17, `typed/real_typed_track1` 7, and `typed/real_typed_track2` 7. The
   manifest records the same mode split and states that Track 2/oracle-only
   families are guard or comparator context, not generated Track 1 optimization
   antecedents (`skv12-p1-capture-manifest.md:189-201`).

2. Parse, direct, and typed boundaries remain distinct. The replay surface has
   34 parse PMU rows, 34 direct PMU rows, and 14 typed PMU rows; P1-D keeps the
   command shapes and status rows separate (`p1d-pmu-cycles.md:38-60`) and
   reports parse/direct/typed as separate PMU aggregates rather than a shared
   admission surface (`p1d-pmu-cycles.md:222-231`). P1-B likewise keeps direct
   and typed product hot leaves separate, preserving direct Track 1, direct
   Track 2, typed Track 1, and typed Track 2 interpretation
   (`p1b-samply-mode-2.md:33-50`, `:170-180`).

3. PMU, xctrace, and samply roles are no longer coupled. The manifest declares
   separate authority per tool: PMU command/status rows for cycles and cost,
   samply command/status rows as companion captures, xctrace capture/status
   rows for Time Profiler and CPU Counter bundles, and derived hot-leaf TSVs as
   the self-time tables (`skv12-p1-capture-manifest.md:57-68`). P1-D explicitly
   refuses to derive cycles/B from xctrace or samply companion artifacts
   (`p1d-pmu-cycles.md:62-76`), while P1-E treats xctrace XML exports and
   derived TSVs as hot-leaf authority and leaves PMU/samply as replay or
   companion evidence (`p1e-hot-leaf-attribution.md:82-92`).

4. The PIN-V2 replay fixes are present for this lens. The replay ledger sanity
   check in the manifest now proves zero noncanonical modes
   (`skv12-p1-capture-manifest.md:143-145`), and a direct recheck of the
   tracked ledger found every row mode in
   `track1|track2|real_typed_track1|real_typed_track2`. The xctrace
   `capture_status.tsv` has 212/212 PASS rows, including 185 `rc=54` rows whose
   recorded stdout logs contain both an accepted stop condition and
   `Output file saved as`; the manifest now points to stdout for that evidence
   (`skv12-p1-capture-manifest.md:109-113`, `:134-141`).

5. CSS L4 is not replaced by Sheets, root-workspace CSS snippets, or reference
   artifacts. P1-A rejects `nonjson-pass-css-l4.json`, report fixtures,
   lightningcss-only runs, and root CSS snippets as substitutes for a skinny
   generated Track 1 parser (`p1a-samply-mode-1.md:145-158`). P1-C records no
   CSS, non-JSON, or Sheets command in the pin root and requires generated CSS
   Track 1 plus strict lightningcss comparator/equality before any CSS hot-leaf
   or SOTA claim (`p1c-samply-mode-3.md:59-62`, `:91-110`). P1-F records zero
   admitted CSS L4 rows and zero generated Sheets/BBNF-self rows
   (`p1f-results-delta.md:80-93`, `:199-210`).

6. I found no sidecar `n/a` win. Direct `rg` over
   `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`,
   `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv`, and the readable
   parse/direct/typed tables found no `n/a`, `unavailable`, `unknown`, `none`,
   line-zero, or `UNRESOLVED_LINE_ZERO` source-anchor hits. P1-E's retained
   checks report 82/82 summary rows and 410/410 detail rows with no unresolved
   load-bearing anchors (`p1e-hot-leaf-attribution.md:97-116`, `:149-161`).
   The one absent non-JSON lane is explicitly CSS L4 bring-up, not a sidecar
   admission path (`p1e-hot-leaf-attribution.md:47-50`, `:195-201`).

7. Generated-size/O(N) and Lock 16 are routed together rather than split into
   independent paper gates. HANDOFF requires generated CSS runtime LOC, module
   byte size, regen/check command, and an O(N) grammar-size guard before W1b can
   proceed, then immediately routes the `escape_mask_64` SIMD prerequisite
   before any SIMD admission (`HANDOFF.md:119-125`). The telemetry binding
   keeps Lock 16 status, generated LOC, generated module byte size, O(N)
   grammar-size status, and same-wave consumer class in the same CSS L4 gate
   field set (`HANDOFF.md:144-151`).

8. The remaining schema nuance is nonblocking for CH5: the tracked pin replay
   TSV is a command ledger, while per-tool success and rc semantics live in the
   profiler-specific status TSVs. That split is explicit in the manifest
   (`skv12-p1-capture-manifest.md:57-73`) and does not currently hide a
   coupling path, but downstream folds should keep calling it command replay
   authority rather than row-admission authority.

## Exact Fold Edits

None required for CH5.

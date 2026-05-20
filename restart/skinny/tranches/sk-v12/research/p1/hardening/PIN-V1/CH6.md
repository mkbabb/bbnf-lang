# SK-V12 S-P1 PIN-V1 CH6 Anti-Paper-Close

Verdict: REVISE

Score: 76%

## Blocking Findings

1. The final fold is not cleanly superseding stale partial-capture notes in the
   lane files. `p1a-samply-mode-1.md` says the final pin root has parse samply,
   xctrace, and derived hot-leaf authority
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:23`),
   then later says the same root did not create parse samply/xctrace paths and
   that the derived hot-leaf TSVs are absent
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:80`,
   `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:137`,
   `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:204`).
   `p1b-samply-mode-2.md` has the same split: final product hot-leaf authority
   at lines 18-35, then "Unavailable for the pin capture" and "must not use
   P1-B as hot-leaf authority" at lines 163-190. `p1e-hot-leaf-attribution.md`
   is worse: final authority is declared at lines 17-31, but the body still
   says no samply/xctrace/XML/derived self-time artifacts were observed, all
   hot-leaf cells are unavailable, and the hot-leaf delta cannot be computed
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:75`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:95`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:165`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:210`).
   A top "supersedes" banner is not enough for CH6; the next pass can still cite
   the stale body as current authority.

2. The capture manifest has dual authority. The pin addendum correctly names
   `/tmp/skv12-pin-p1` and the current-head build root
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:9`),
   but the main Run Identity and capture sections still present pre-pin
   `/tmp/skv12-p1`, `50bd1648`, `/tmp/skv12-profile-target-50bd1648`, and old
   PMU aggregate values as if they are current
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:39`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:94`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:175`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:209`).
   In a post-pin rerun where pre-pin hardening does not count, the manifest must
   mark that body historical or rewrite it to the pin root.

3. Export status is overstated as PASS. The manifest claims "Time Profiler XML
   exports | 82 | PASS"
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:31`)
   and P1-B claims product-v2 XML exports are "48/48 PASS"
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:29`), but
   `/tmp/skv12-pin-p1/time_profile_export_status.tsv` has 82 rows whose status
   column is `SKIP`. The export files exist, so this is likely an already-present
   export state, not a missing artifact, but the claim is not backed by the TSV
   as written.

## Nonblocking Notes

- Artifact existence checks are strong. `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
  has 82/82 PASS rows, `/tmp/skv12-pin-p1/samply/capture_status.tsv` has 82/82
  PASS rows, and `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` has 212/212 PASS
  rows. Referenced PMU stdout/stderr, samply artifacts, and xctrace artifacts
  exist.
- The derived hot-leaf tables are concrete. `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`
  has 82 rows and `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` has 410
  rows; source anchors have no `:0`, `unknown`, or `none`, and the file:line
  targets exist in the workspace.
- CSS L4 is explicitly not admitted/profiled in the stronger files:
  P1-C records no CSS artifacts under the pin root and no generated Track 1
  parser, P1-D records CSS PMU 0/1, P1-F records 0 admitted CSS L4 rows, and the
  manifest addendum records CSS L4 unprofiled because no generated runtime or
  lightningcss same-plane comparator exists.
- The fold mostly avoids future-promise substitution: JSON profile data is
  framed as nomination/guard evidence, while CSS L4 requires its own generated
  Track 1 row, comparator, equality path, and measurement in S-P2/S-P3/W1.

## Exact Fold Edits Required

1. Rewrite `p1a-samply-mode-1.md` so Section 1, Section 2, and Section 6 no
   longer state that parse samply/xctrace exports and hot-leaf TSVs are absent.
   Replace the obsolete missing-artifact list with the final parse authority:
   `/tmp/skv12-pin-p1/samply/capture_status.tsv`,
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`,
   `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`, and
   `/tmp/skv12-pin-p1/time_profile_parse_table.md`.

2. Rewrite `p1b-samply-mode-2.md` lines 163-190 and 209-212 to point to the
   final direct/typed hot-leaf authority instead of saying product self-time is
   unavailable. Keep the CSS L4 absence language.

3. Rewrite `p1e-hot-leaf-attribution.md` lines 75-193 and 210-215 to be the
   final hot-leaf ledger, or delete those stale partial-capture sections. The
   surviving P1-E body must say that JSON parse/direct/typed hot-leaf rows exist
   in the summary/details TSVs, while CSS L4 remains absent because no generated
   Track 1 parser/comparator exists.

4. Rewrite `skv12-p1-capture-manifest.md` so the pin root is the primary
   authority throughout, or clearly label the `/tmp/skv12-p1` sections as
   historical pre-pin reference only. Update PMU aggregate values to the pin
   capture from P1-D: parse `2.971206 c/B`, direct `4.411311 c/B`, typed
   `3.137378 c/B`.

5. Replace export status wording of "PASS" with TSV-backed wording: either rerun
   exports and update `/tmp/skv12-pin-p1/time_profile_export_status.tsv` to PASS,
   or state that 82 export artifacts exist and the export status TSV records
   `SKIP` for already-present XML outputs.

# SK-V12 S-P1 Hardening PIN-V2 CH6

Lens: anti-paper-close integrity after the PIN-V1 fold.
Date: 2026-05-20.
Reviewer scope: current S-P1 packet at repo head `d4ef80b2`, capture root
`/tmp/skv12-pin-p1`, and assigned output file only.

## Verdict

ACCEPT.

Score: 94%.

The folded S-P1 packet is clean enough to advance to S-P2. The live claims are
backed by TSV, trace/export, or `skinny/RESULTS.md` authority; stale partial
capture blockers from PIN-V1 are no longer live; CSS L4 is explicitly absent
and not admitted; and the handoff routes CSS L4 bring-up to S-P2/S-P3 rather
than paper-closing it with a future promise.

## Blocking Findings

None.

## Evidence Checked

Single pin-era authority surface:

- `skv12-p1-capture-manifest.md:14-27` names `/tmp/skv12-pin-p1`, the
  `cf7848b2` capture source, the `b1043383` initial S-P1 fold, live binaries,
  and completion stamps.
- `skv12-p1-capture-manifest.md:49-51` marks `/tmp/skv12-p1`,
  `/tmp/skv12-profile-target-50bd1648`, and `skv12-p1-replay.tsv` historical
  only.
- `skv12-p1-capture-manifest.md:68-71` names the tracked
  `skv12-p1-pin-replay.tsv` as 458 pin-era command rows.
- Mechanical replay-ledger check: 458 data rows, 10 fields per row, no empty
  core artifact/status fields, and every listed artifact plus status artifact
  exists.

Capture and table backing:

- PMU status: 82/82 `PASS`.
- samply status: 82/82 `PASS`.
- xctrace status: 212/212 `PASS`.
- XML export status: 82/82 `SKIP`, with docs correctly defining this as
  already-existing nonzero XML rather than relabeling it as TSV `PASS`.
- Hot-leaf summary: 82 rows, 0 bad anchors.
- Hot-leaf details: 410 rows, 0 bad anchors.

No stale partial-capture contradiction:

- P1-A now records complete JSON parse PMU/samply/xctrace/hot-leaf evidence and
  points parse hot leaves at the `plane=parse` subset of the pin TSVs
  (`p1a-samply-mode-1.md:16-21`, `:23-59`, `:78-83`, `:131-143`).
- P1-B records complete product PMU/samply/xctrace/hot-leaf evidence, preserves
  Track 1 / Track 2 separation, and keeps product rows as JSON guard evidence
  (`p1b-samply-mode-2.md:23-49`, `:173-190`, `:198-210`).
- P1-E records final hot-leaf authority from the derived summary/detail tables
  and says no profiler process remains load-bearing (`p1e-hot-leaf-attribution.md:17-50`,
  `:82-95`, `:99-116`).

CSS L4 is not admitted or profiled:

- The manifest says CSS L4 remains unprofiled because no generated CSS L4 Track
  1 runtime, lightningcss same-plane comparator row, or strict equality oracle
  row exists (`skv12-p1-capture-manifest.md:152-157`).
- P1-A rejects root CSS snippets, report fixtures, and lightningcss-only runs
  as profile/admission substitutes (`p1a-samply-mode-1.md:145-158`,
  `:181-183`).
- P1-C records no CSS artifacts in the pin root and requires generated parser,
  comparator, and equality path before CSS hot-leaf, Mode III, or SOTA claims
  become measurable (`p1c-samply-mode-3.md:96-110`, `:118-127`).
- P1-F confirms `skinny/RESULTS.md` has zero admitted CSS L4 rows and no JSON
  row populates the `lightningcss_mbps + 1` close bar
  (`p1f-results-delta.md:19-36`, `:80-93`, `:171-210`).

No future promise substitutes for S-P2/S-P3 bring-up:

- `HANDOFF.md:103-128` requires pass re-derivation and a new S-P3 plan naming
  the exact CSS row, output plane, generated Track 1 path, comparator, equality
  command, benchmark command, gate command, `GrammarConfig`, generated size
  checks, and O(N) grammar-size guard.
- `HANDOFF.md:142-155` makes the CSS gate consume provenance, comparator Mbps,
  strict equality, profile/benchmark artifacts, generated size, O(N) status,
  JSON guard state, and gate status; missing lightningcss evidence and stale run
  ids fail closed.

## Nonblocking Notes

1. `p1f-results-delta.md:68-71` still says a parent-owned PMU replay process was
   running during that lane's extraction. The top of the same file and the
   manifest now record final complete status, so this is not a blocker, but the
   next doc touch should clarify that this was an in-method observation, not the
   final fold state.
2. `p1e-hot-leaf-attribution.md:143` says "A JSON-only fold is not
   pin-converged." In context the file records CSS L4 as a first-class absence
   row, so the statement is acceptable. A clearer future wording would be:
   "A fold that omits the CSS L4 absence row is not pin-converged."
3. `p1d-pmu-cycles.md:74` names a pending CSS L4 PMU row shape while also
   deferring to S-P3/W1 selection. This does not substitute for bring-up, but if
   S-P3 selects a different exact CSS row, update that line during the S-P3 fold.

## Exact Fold Edits If REVISE

N/A. Verdict is ACCEPT.

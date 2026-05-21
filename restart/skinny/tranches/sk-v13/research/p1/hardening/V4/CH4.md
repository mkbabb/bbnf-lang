# SK-V13 S-P1 V4 CH4 Cost / Reproducibility

Verdict: ACCEPT.

## Evidence

- V4 preserves checked-in, repo-relative source snapshots for both formerly
  temporary profilers: CSS at `support/harnesses/css_profiler/` and mode III at
  `support/harnesses/mode3/` (`support/profile-provenance-v3.md`,
  `support/mode3-harness-provenance.md`).
- The mode-III harness has checked-in source hashes, repo-relative
  dependencies, a rebuild command, and a verified rebuild binary hash matching
  the original profiler (`support/mode3-harness-provenance.md`).
- The CSS profiler has checked-in source hashes, a repo-relative rebuild
  command, and a verified rebuild binary hash matching the original profiler
  (`support/profile-provenance-v3.md`).
- The top-leaf and summary TSV pipelines are reproducible from checked-in
  scripts: `extract_hotleaf_top20.py` writes `hotleaf_top20.tsv`, and
  `summarize_profile_rows.py` regenerates `direct_summary.tsv` and
  `mode3_summary.tsv` (`support/profile-provenance-v3.md`,
  `p1e-hot-leaf-attribution.md`, `support/summarize_profile_rows.py`).
- Sequential local regeneration reproduced documented hashes for
  `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`,
  `/tmp/skv13-p1-v2/summary/direct_summary.tsv`, and
  `/tmp/skv13-p1-v2/summary/mode3_summary.tsv`
  (`support/profile-provenance-v3.md`).
- The retained V1 parse/typed profile limitation is explicitly disclosed: the
  exact original cargo build invocation was not preserved, so those rows remain
  auditable binary-hash artefacts rather than a fully rebuildable command
  surface. Given the declared no-`skinny/crates/` behavior-source delta and the
  durable rerun surfaces for the new evidence, this is now an accepted
  limitation, not a CH4 blocker.
- Branch/L1/LLC are honestly marked unavailable from the current xctrace export
  rather than inferred as zero (`p1d-pmu-cycles.md`,
  `support/evidence-ledger-v3.md`).

## Blockers

None.

Residual cautions: do not promote V1 parse/typed captures as fully rebuildable;
cite them only as auditable retained artefacts. Do not treat save-only or
function-only sidecar rows as interactive samply-equivalent symbol closure.

# SK-V13 S-P1 V5 CH4 Cost / Reproducibility

Verdict: ACCEPT.

## Evidence

- V4 recorded a 6/6 accepted challenge cycle, with CH4's prior blocker resolved
  by checked-in profiler sources, repo-relative rebuild commands, verified
  binary hashes, and summary reproducers. V5 is a confirmation cycle, not
  another fold requirement.
- Both formerly temporary harnesses now have durable source snapshots: CSS at
  `support/harnesses/css_profiler/` and mode III at `support/harnesses/mode3/`
  (`support/profile-provenance-v3.md`, `support/mode3-harness-provenance.md`).
- Reproducibility is adequate for CH4: the CSS and mode-III packets list source
  hashes, repo-relative rebuild commands, and rebuilt binary hashes matching
  the original profilers.
- The summary pipeline is reproducible from checked-in scripts: hot-leaf
  extraction and direct/mode-III summaries have commands and output hashes
  (`support/extract_hotleaf_top20.py`, `support/summarize_profile_rows.py`).
- The retained V1 parse/typed limitation is disclosed rather than hidden: those
  captures remain auditable binary-hash artefacts, not fully rebuildable command
  surfaces, and no `skinny/crates/` behavior-source delta is claimed.
- Missing branch/L1/LLC counters are marked unavailable from the current
  xctrace export, not inferred as zero.

## Blockers

None. CH4 accepts without requiring another profile fold.

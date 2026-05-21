# SK-V13 S-P1 V4 CH6 Anti-Paper-Close

Verdict: ACCEPT.

## Evidence

- The packet keeps the gate boundary explicit: P1-F states every
  classification is `profile_signal_not_gate_admission` and that only later
  gate-json/REDRESS waves can admit or demote rows. P1-D separately says PMU
  and cycle facts are profile facts, not row admissions.
- V4 does not paper over retained V1 limitations. The provenance file states
  the V1 parse/typed build command was not preserved and is retained as
  auditable capture artefacts with binary hashes, not as a fully rebuildable
  command surface (`support/profile-provenance-v3.md`).
- V4 materially improves reproducibility without upgrading profile facts into
  admissions: CSS and mode-III harness source snapshots are checked in with
  rebuild commands and verified binary hashes (`support/profile-provenance-v3.md`,
  `support/mode3-harness-provenance.md`).
- The offline sidecar extraction path is citable and reproducible. P1-E points
  to checked-in reproducers for hot-leaf and direct/mode-III summaries, with
  extractor and summary scripts providing the TSV generation logic
  (`p1e-hot-leaf-attribution.md`, `support/extract_hotleaf_top20.py`,
  `support/summarize_profile_rows.py`).
- The packet preserves unresolved evidence as unresolved. The canonical ledger
  says all rows are profile signals, not gate admissions; classifies
  direct/generated wrappers as JSON envelopes rather than grammar-neutral
  primitives; keeps mode-III rows as scanner/masking evidence with file-line
  limits; and records CSS as profiled nonparser overhead.
- Routed research is separated from implementation authority. REDRESS guards
  require future direct-row reopens to cite prior fixpoints and name a material
  differential, and they bar profile signals from reopening rejected route
  families or creating orphan SIMD authority.

## Blockers

None for CH6. Residual gaps remain deliberately non-closing: CSS parser hot
leaf is unresolved, ten typed rows remain missing product surfaces, parse/typed
V1 capture is auditable rather than fully rebuildable, and same-run strict
comparator admissions are future wave work.

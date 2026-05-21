# SK-V13 S-P1 V4 CH5 Hidden Coupling

Verdict: ACCEPT.

## Evidence

- V4 resolves the temp-only harness coupling for mode III: the packet preserves
  a checked-in source snapshot, repo-relative dependencies, a rebuild command,
  and an identical rebuilt binary hash (`support/mode3-harness-provenance.md`).
- V4 resolves the CSS profiler temp-source gap the same way: checked-in source
  snapshot, source hashes, repo-local rebuild command, and matching V2/V4
  binary hash (`support/profile-provenance-v3.md`).
- Sidecar coverage is explicit rather than assumed: direct has 34 profiles and
  34 sidecars with zero bad return codes, mode III has 85 profiles and 85
  sidecars with zero bad return codes, and CSS lists profile plus sidecar
  artefacts (`p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`,
  `p1e-hot-leaf-attribution.md`).
- Sidecar limitations are not hidden: parse save-only limitations are flagged,
  mode-III function-only sidecar status is canonicalized, and ASM/function-only
  rows are kept out of precise primitive attribution (`p1a-samply-mode-1.md`,
  `support/evidence-ledger-v3.md`).
- Untracked generated summaries are declared measurement artefacts, with
  checked-in reproducers and hashes: `extract_hotleaf_top20.py`,
  `summarize_profile_rows.py`, and V4 output hashes are cited
  (`support/profile-provenance-v3.md`, `p1e-hot-leaf-attribution.md`).
- Cross-surface coupling is stated and bounded: parse/typed are retained V1
  captures with no `skinny/crates/` source delta, direct is V2 non-panic
  capture, and CSS V2 throughput is explicitly method-mismatched and not
  treated as demotion/admission.
- The packet does not treat profiles as gate admissions: the canonical ledger
  marks all rows `profile_signal_not_gate_admission`, and P1-F repeats that
  boundary (`support/evidence-ledger-v3.md`, `p1f-results-delta.md`).

## Blockers

None.

Residual risks are disclosed rather than hidden: V1 parse/typed is auditable
rather than fully rebuildable, and raw profile artefacts plus regenerated TSVs
remain under `/tmp`; V4 records paths, rebuild/reproducer commands, and hashes.

# SK-V13 S-P1 V4 CH1 Correctness

Verdict: ACCEPT.

## Evidence

- V4 preserves the V3 correctness boundary: every profile row remains
  `profile_signal_not_gate_admission`, and unresolved, missing, and nonparser
  cases are explicitly classified rather than promoted as admissions
  (`support/evidence-ledger-v3.md`, `p1f-results-delta.md`).
- Direct-to-struct correctness blocker is resolved: V2/V4 records 17/17 direct
  Track 1 plus Track 2 coverage, 34 non-panic profiles, 34 sidecars, and zero
  bad return codes (`p1b-samply-mode-2.md`).
- Mode-III correctness blocker is resolved: 17 corpora x 5 probes = 85/85
  profiles with zero bad return codes; unsupported PEXT and dispatch-table
  probes are routed explicitly rather than counted as missing coverage
  (`p1c-samply-mode-3.md`).
- CSS declaration-values evidence is correctly scoped: strict equality is
  preserved, throughput is recorded, and the profile is marked timer/fact-sink
  dominated rather than a parser-hot-leaf proof (`p1f-results-delta.md`,
  `support/evidence-ledger-v3.md`).
- Typed coverage is honest: 7/17 generated typed rows are profiled, and the
  remaining 10 rows are marked missing product surface, not inferred from
  direct profiles (`p1f-results-delta.md`, `support/evidence-ledger-v3.md`).
- V4 resolves the prior reproducibility fold without changing correctness
  claims: mode-III and CSS harnesses now have checked-in source hashes,
  repo-relative rebuild commands, and verified rebuilt binary hashes
  (`support/mode3-harness-provenance.md`, `support/profile-provenance-v3.md`).
- Offline summary extraction is reproducible from checked-in scripts for
  top-leaf, direct, and mode-III summaries (`support/extract_hotleaf_top20.py`,
  `support/summarize_profile_rows.py`, `support/profile-provenance-v3.md`).

## Remaining Blockers

None for CH1.

Carry-forward limitations are not CH1 blockers: retained V1 parse/typed
captures are auditable-only, branch/L1/LLC remain
`unavailable_from_current_export`, CSS parser hot leaf remains unresolved, and
no S-P1 row is a gate admission.

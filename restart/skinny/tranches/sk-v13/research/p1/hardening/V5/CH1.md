# SK-V13 S-P1 V5 CH1 Correctness Confirmation

Verdict: ACCEPT.

## Evidence

- V5 reviews the same V4-accepted profile packet; V4 recorded 6/6 acceptance
  and required V5 only as the confirmation cycle, not another profile fold
  (`HARDENING-S-P1-V4-CONSOLIDATED.md`).
- Correctness boundaries remain explicit: every row in the canonical ledger is
  `profile_signal_not_gate_admission`, with separate labels for JSON envelopes,
  typed-only rows, CSS nonparser overhead, missing product surfaces,
  function-only sidecars, and unavailable counters (`support/evidence-ledger-v3.md`).
- Direct-to-struct coverage remains correct and non-panic: 17/17 Track 1 plus
  Track 2 direct profiles, 34 profiles, 34 sidecars, zero bad return codes, and
  logs containing timed-loop `PROBE_RESULT` evidence (`p1b-samply-mode-2.md`).
- Mode-III coverage remains correct: 17 corpora x 5 probes = 85/85 profiles
  with zero bad return codes; unsupported PEXT and duplicate-dispatch probes
  are explicitly routed rather than counted as misses (`p1c-samply-mode-3.md`).
- CSS declaration-values evidence remains correctly scoped: strict equality is
  preserved, but the profile is timer/fact-sink dominated and not treated as
  parser-hot-leaf proof or row demotion (`p1f-results-delta.md`,
  `support/evidence-ledger-v3.md`).
- Typed coverage is honest: seven generated typed rows are classified as JSON
  typed-only, and the ten absent rows are marked `missing-product-surface`, not
  inferred from direct profiles.
- V4 resolved the prior reproducibility risk without changing correctness
  claims: CSS and mode-III harnesses have checked-in source snapshots,
  repo-relative rebuild commands, verified binary hashes, and reproducible
  summary TSV scripts (`support/profile-provenance-v3.md`,
  `support/mode3-harness-provenance.md`).

## Blockers

None. No new profile fold is required for CH1.

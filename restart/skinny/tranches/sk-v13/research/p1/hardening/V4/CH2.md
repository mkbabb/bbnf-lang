# SK-V13 S-P1 V4 CH2 Generality / Lock 14

Verdict: ACCEPT.

## Evidence

- The packet keeps JSON profile envelopes out of grammar-neutral evidence.
  `dispatch_value` is not primitive proof, and parse rows are classified only
  as JSON parse envelopes, function-only sidecars, or JSON-confirmed unicode
  candidates (`p1a-samply-mode-1.md`).
- The canonical ledger defines the CH2 boundary: JSON parse/direct envelopes
  are not grammar-neutral primitives, unicode candidates are JSON-confirmed
  only, typed leaves cannot generalize to CSS/Sheets, CSS profile evidence is
  timer/fact-sink overhead, and every row is `profile_signal_not_gate_admission`
  (`support/evidence-ledger-v3.md`).
- Direct evidence remains bounded to JSON unless later waves prove otherwise:
  generated wrappers are JSON direct envelopes, the named unicode primitive is
  non-JSON-confirmed, and `y_string_unicode` Track 2 is timer noise
  (`support/evidence-ledger-v3.md`).
- Typed coverage is not overclaimed. P1-B states only seven generated typed
  rows exist and ten unsupported corpora were not invented; the ledger marks
  seven as JSON typed only and the rest as missing product surface
  (`p1b-samply-mode-2.md`, `support/evidence-ledger-v3.md`).
- CSS is present but not exaggerated: declaration-values profiling is
  timer/fact-sink dominated, and the CSS throughput method mismatch is not a
  demotion or admission (`p1e-hot-leaf-attribution.md`,
  `p1f-results-delta.md`).
- V4 added reproducibility support without weakening generality. Durable
  CSS/mode-III harness snapshots and checked-in regenerators are provenance,
  while extracted TSVs remain measurement artefacts, not committed benchmark
  outputs (`support/profile-provenance-v3.md`).

## Blockers

None for CH2. S-P2 may consume this packet as labeled profile evidence, but
must not treat JSON envelopes, JSON typed leaves, CSS timer/fact-sink samples,
or JSON-confirmed scanner/unicode candidates as grammar-neutral proof without
non-JSON benchmark confirmation.

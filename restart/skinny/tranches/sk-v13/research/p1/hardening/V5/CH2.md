# SK-V13 S-P1 V5 CH2 Generality / Lock 14

Verdict: ACCEPT.

## Evidence

- The packet still quarantines JSON parse evidence from grammar-neutral proof:
  parse rows are JSON parse envelopes, function-only sidecars, or
  JSON-confirmed unicode candidates; `dispatch_value` is not primitive proof
  (`p1a-samply-mode-1.md`, `support/evidence-ledger-v3.md`).
- The canonical status vocabulary preserves the Lock 14 boundary: JSON
  parse/direct wrappers are not grammar-neutral primitives, unicode/string
  candidates are JSON-confirmed only, typed leaves cannot generalize to
  CSS/Sheets, CSS evidence is nonparser overhead, and every row is
  `profile_signal_not_gate_admission`.
- Direct evidence remains bounded to JSON. Generated direct wrappers are JSON
  direct envelopes, `unicode_escapes` is only a JSON-confirmed unicode
  candidate, and `y_string_unicode` Track 2 is timer/noise rather than parser
  proof.
- Typed coverage is not overclaimed: only seven generated typed rows exist in
  the profile packet, ten corpora remain missing product surface, and P1-B says
  those rows were not invented by the V2 fold (`p1b-samply-mode-2.md`).
- CSS evidence is present but still not generalized into parser proof. The CSS
  row is timer/fact-sink dominated nonparser overhead, and P1-F keeps the
  throughput mismatch as profile signal only, not admission or demotion.
- V4 durable harness and summary-script additions improve reproducibility
  without weakening generality. They are provenance support, while extracted
  TSVs remain measurement artefacts rather than committed benchmark outputs
  (`support/profile-provenance-v3.md`, `p1e-hot-leaf-attribution.md`).

## Blockers

None for CH2. S-P2 may consume the packet as labeled profile evidence, but must
not treat JSON envelopes, JSON typed leaves, CSS timer/fact-sink samples, or
JSON-confirmed scanner/unicode candidates as grammar-neutral proof without
non-JSON benchmark confirmation.

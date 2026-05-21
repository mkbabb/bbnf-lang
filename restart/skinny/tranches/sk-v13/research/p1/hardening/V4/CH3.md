# SK-V13 S-P1 V4 CH3 Regression / REDRESS

Verdict: ACCEPT.

## Evidence

- The canonical ledger states all rows are `profile_signal_not_gate_admission`,
  separating profile evidence from admission authority. It classifies direct
  wrappers as JSON direct envelopes, unicode as JSON-confirmed candidates,
  mode-III scan rows as scanner candidates, and CSS as profiled nonparser
  overhead rather than primitive proof (`support/evidence-ledger-v3.md`).
- REDRESS 119/120 are handled correctly under the full-SOTA addendum: rows are
  wave-eligible, but S-P1 profile signals are not admissions or reopens. Future
  direct-row reopens must cite the prior fixpoint, name a material
  differential, and use same-harness strict comparator evidence
  (`p1f-results-delta.md`, `support/evidence-ledger-v3.md`).
- Union-adjacent structural SIMD evidence is bounded as a scanner micro-result,
  not a reopened REDRESS 96/97/98 union route. P1-C, P1-D, P1-E, and the
  ledger all require a material differential before any union attempt
  (`p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`,
  `p1e-hot-leaf-attribution.md`, `support/evidence-ledger-v3.md`).
- Pre-pin rejected route families remain guarded. P1-A and P1-B block
  dispatch-table/function-pointer, parser-local cursor, event sidecar,
  source-method digest, decoded-string stats, generic visitor, and related
  route reuse without REDRESS citation and material differential
  (`p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`,
  `support/evidence-ledger-v3.md`).
- SIMD/orphan language is preserved. P1-C, P1-E, and the ledger state that
  PEXT gaps, function-only ASM leaves, sidecar gaps, and
  `bulk_emit_positions_64_neon` attribution do not create orphan SIMD
  primitives or reopen REDRESS-126; future SIMD requires scalar reference,
  parity/checkasm, feature-mask disclosure, same-wave consumer, and zero-orphan
  disposition.

## Blockers

None. V4 preserves REDRESS guardrails, does not silently reopen blocked
families, and classifies profile/primitive status honestly enough for S-P1 from
the regression lens.

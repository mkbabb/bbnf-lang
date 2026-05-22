# SK-V13 W14.3 Challenge - Canada Parse-Only Admission

Date: 2026-05-22.
Disposition: ACCEPT.

## CH1 Correctness

The row may admit only as DOM output with strict equality evidence. Track 1 is
the throughput admission lane. Track 2 is required as an independent
correctness/oracle lane; its current throughput also clears the strict bar, but
that surplus must not weaken the report contract or replace Track 1 lower
confidence as the admission threshold. A report missing measured UTF-8, escape
completeness, strict artifact hash, or same-run sonic strict comparison is a
reject.

## CH2 Generality / Lock 14

W14.3 may add one `JsonParseOnlyAdmissionSpec` entry for `canada` and reuse the
existing table-driven W14 validator. It must not add a directive, BIR variant,
BackendShape, public substrate API, parser-owned fact slot, or JSON-specific
generic-crate leak. The configured table after W14.3 may contain only W14.1
`numbers`, W14.2 `citm_catalog`, and W14.3 `canada`.

## CH3 Regression / REDRESS

W14.3 cites REDRESS 102 for the historical parse-only firewall and follows
REDRESS 154/155 for the W14 report mechanism. It does not reopen REDRESS
96/97/98 union-substrate work, REDRESS 88/89 SIMD paths, or REDRESS 119 JSON
direct fixpoint rows. Existing CSS, direct, typed, W14.1 numbers parse, and
W14.2 citm parse admissions must not silently demote.

## CH4 Cost

The implementation is bounded to one spec-table entry, report validation
coverage, W14.3 artifacts, and result/status updates. Parser runtime,
generated JSON parser bodies, SIMD primitives, and decision-engine routing are
out of scope unless current Canada measurement falsifies the row evidence and
a fresh material differential is needed.

## CH5 Hidden Coupling

The same-wave consumer is the native JSON parse-only Criterion group
`json_canada`. The report must bind to `track1_generated`,
`track2_handcoded`, `sonic_rs_anchor`, and `serde_json`; stale rolling values,
permissive sonic, sidecar-only evidence, or row identity inferred from corpus
name alone cannot admit the row. Gate marking must remain spec-table driven so
unsupported parse-only rows stay `S / NO-GO`.

## CH6 Anti-Paper-Close

The positive rolling-table margin alone is insufficient. W14.3 closes only on
a gate-consumed `sk-v13-json-parse-only-v1` report plus current Criterion
evidence proving Track 1 lower-confidence throughput exceeds
`sonic_rs_anchor + 1 Mbps` for `json/canada/parse_only/main`.

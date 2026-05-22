# SK-V13 W14.2 Challenge - CITM Catalog Parse-Only Admission

Date: 2026-05-22.
Disposition: ACCEPT.

## CH1 Correctness

The row may admit only as DOM output with strict equality evidence. Track 1 is
the throughput admission lane. Track 2 is required as an independent
correctness/oracle lane but is not required to clear the sonic throughput bar.
A report that omits measured UTF-8, escape completeness, strict artifact hash,
or same-run sonic strict comparison is a reject.

## CH2 Generality / Lock 14

The W14.2 source change may generalize W14 parse-only admission through a
small row-spec table, but it must not add a directive, BIR variant,
BackendShape, public substrate API, parser-owned fact slot, or JSON-specific
generic-crate leak. The W14.2 table may contain only already-admitted W14.1
`numbers` and selected W14.2 `citm_catalog`; adding future positive-margin rows
requires later wave packets.

## CH3 Regression / REDRESS

W14.2 cites REDRESS 102 and the historical parse-only firewall as the material
differential. It does not reopen REDRESS 96/97/98 union-substrate work,
REDRESS 88/89 SIMD default paths, or stale retained string/control routes. A
failed report or Criterion miss is a measured REDRESS reject with the source
patch reverted and saved. Existing CSS, direct, typed, and W14.1 numbers parse
admissions must not silently demote.

## CH4 Cost

The implementation is bounded to report validation, `gate-json` row marking,
Lock 14 owner-scope recognition, W14.2 artifacts, and result/status updates.
Parser runtime and generated JSON parser bodies are out of scope unless the
measurement gate falsifies the current row evidence and a fresh material
differential is required.

## CH5 Hidden Coupling

The same-wave consumer is the native JSON parse-only Criterion group
`json_citm_catalog`. The report must bind to `track1_generated`,
`track2_handcoded`, `sonic_rs_anchor`, and `serde_json`; stale rolling values,
permissive sonic, sidecar-only evidence, or Track 1 / Track 2 coupling cannot
admit the row.

## CH6 Anti-Paper-Close

The positive rolling-table margin alone is insufficient. W14.2 closes only on
a gate-consumed `sk-v13-json-parse-only-v1` report plus current Criterion
evidence proving Track 1 lower-confidence throughput exceeds
`sonic_rs_anchor + 1 Mbps` for `json/citm_catalog/parse_only/main`.

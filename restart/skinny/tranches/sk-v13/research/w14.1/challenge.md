# SK-V13 W14.1 Challenge - Numbers Parse-Only Admission

Date: 2026-05-22.
Disposition: ACCEPT.

## CH1 Correctness

The parse-only row may admit only as DOM output with strict equality evidence.
Borrowed-view parity, lossy sonic comparison, Track 1-only throughput, or a
report that omits measured UTF-8 / escape completeness is a reject.

## CH2 Generality / Lock 14

The change is report and gate plumbing. It must not add a directive, BIR
variant, BackendShape, public substrate API, parse-plane substrate, or
JSON-specific generic-crate leak. Lock 14 status is carried in the companion
report and remains checked by `gate-json` before report validation.

## CH3 Regression / REDRESS

The material differential cites the historical parse-only firewall and does
not reopen REDRESS 96/97/98 union-substrate work. A failed report or Criterion
miss is a measured REDRESS reject with the source patch saved and reverted. An
admit updates only the selected row and cannot silently demote existing CSS,
direct, or typed admissions.

## CH4 Cost

The expected source diff is bounded to report schema, `gate-json` companion
flag validation, `xtask gate-json` passthrough, and W14.1 status artifacts.
Parser runtime, generated JSON parser code, and aarch64 SIMD code are out of
scope for this admission wave.

## CH5 Hidden Coupling

The same-wave consumer is the native JSON parse-only Criterion group
`json_numbers`. The report must bind to `track1_generated`,
`track2_handcoded`, `sonic_rs_anchor`, and `serde_json`; no benchmark-private
fixture shortcut or sidecar-only row movement may stand in for those lanes.

## CH6 Anti-Paper-Close

W14.1 cannot close on the existing rolling-table margin alone. The close
condition is a gate-consumed `sk-v13-json-parse-only-v1` report plus current
Criterion evidence proving Track 1 lower-confidence throughput exceeds
`sonic_rs_anchor + 1 Mbps` for `json/numbers/parse_only/main`.

# SK-V13 W14.4 Research - Marine IK Parse-Only Admission

Date: 2026-05-22.
Scope: `json/marine_ik/parse_only/main`.
Disposition: select for plan.

## Candidate

After W14.3, `marine_ik` is the largest remaining positive-margin OPEN
parse-only row. The current rolling table records Track 1 `12880` Mbps
against same-run strict threshold `10274` Mbps (`sonic_strict + 1`), for a
margin of `2606` Mbps. The row remains `S / NO-GO` solely because the
historical parse-only firewall classifies it as `view-boundary`.

The existing Criterion lanes provide the row-local strict admission material:

- `json_marine_ik/track1_generated`
- `json_marine_ik/track2_handcoded`
- `json_marine_ik/sonic_rs_anchor`
- `json_marine_ik/serde_json`

Track 2 is also above the strict threshold (`12563` Mbps vs `10274` Mbps), but
W14.4 preserves the W14 contract: Track 1 is the throughput admission lane, and
Track 2 is the independent correctness/oracle lane.

## Material Differential

W14.4 does not change parser runtime, generated JSON bodies, SIMD code, union
substrate, output digests, or decision-engine policy. It extends the
already-landed W14 parse-only admission table by adding one configured row and
one gate-consumed report for `json/marine_ik/parse_only/main`.

The material differential from REDRESS 102 is the SK-V13 addendum's
parse-only re-pin plus strict DOM evidence. The material differential from
W14.1-W14.3 is row-local: no non-selected parse-only row may move in this wave.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.4/`
- `skinny/REDRESS.md`

## Gate

`G-W14.4-JSON-PARSE-MARINE-IK` admits only if a companion
`sk-v13-json-parse-only-v1` report validates the row identity, DOM output
plane, strict equality artifact, measured UTF-8 path, escape completeness,
Lock 14 status, independent Track 2 oracle, native Criterion lane provenance,
and prior REDRESS 102 material differential. Track 1 lower-confidence
throughput must exceed same-run sonic strict by more than 1 Mbps.

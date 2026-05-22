# SK-V13 W14.3 Research - Canada Parse-Only Admission

Date: 2026-05-22.
Scope: `json/canada/parse_only/main`.
Disposition: select for plan.

## Candidate

The current SK-V13 rolling table leaves `canada` parse-only as the largest
remaining positive-margin OPEN row after W14.1 and W14.2. Track 1 is `16993`
Mbps against the same-run strict threshold `13934` Mbps (`sonic_strict + 1`),
for a rolling margin of `3059` Mbps. The row remains `S / NO-GO` because the
historical parse-only firewall still classifies it as `view-boundary` despite
same-run DOM comparator evidence.

The existing Criterion lanes provide the material needed for a row-specific
strict admission report:

- `json_canada/track1_generated`
- `json_canada/track2_handcoded`
- `json_canada/sonic_rs_anchor`
- `json_canada/serde_json`

Unlike W14.2, the current Track 2 oracle is also above the strict throughput
threshold (`16274` Mbps vs `13934` Mbps). W14.3 still treats Track 1 as the
throughput admission lane and Track 2 as the independent correctness/oracle
lane, preserving the W14 report contract.

## Material Differential

W14.3 does not change parser runtime, generated JSON bodies, SIMD code, union
substrate, output digests, or dispatch policy. It extends the already-landed
table-driven W14 parse-only admission surface by adding one configured row and
one gate-consumed report for `json/canada/parse_only/main`.

The material differential from REDRESS 102 is the SK-V13 addendum's parse-only
re-pin plus a strict DOM admission report with same-run Criterion lanes. The
material differential from W14.1/W14.2 is row-local: no new admission framework
is needed, and no non-selected parse-only row may move in this wave.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.3/`
- `skinny/REDRESS.md`

## Gate

`G-W14.3-JSON-PARSE-CANADA` admits only if a companion
`sk-v13-json-parse-only-v1` report validates the row identity, DOM output
plane, strict equality artifact, measured UTF-8 path, escape completeness,
Lock 14 status, independent Track 2 oracle, native Criterion lane provenance,
and prior REDRESS 102 material differential. Track 1 lower-confidence
throughput must exceed same-run sonic strict by more than 1 Mbps.

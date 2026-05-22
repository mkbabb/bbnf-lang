# SK-V13 W14.2 Research - CITM Catalog Parse-Only Admission

Date: 2026-05-22.
Scope: `json/citm_catalog/parse_only/main`.
Disposition: select for plan.

## Candidate

The fresh SK-V13 rolling table shows `citm_catalog` parse-only above the
pinned same-run sonic strict bar: Track 1 is `30035` Mbps against threshold
`25546` Mbps (`sonic_strict + 1`). The row remains `S / NO-GO` only because
the historical parse-only firewall marks it as `deferred`, `view-boundary`,
and `borrowed view over offset tape vs DOM`.

The current Criterion lanes also have the required same-host material for a
strict admission report:

- `json_citm_catalog/track1_generated`
- `json_citm_catalog/track2_handcoded`
- `json_citm_catalog/sonic_rs_anchor`
- `json_citm_catalog/serde_json`

Track 2 is a correctness/oracle lane here, not the throughput admission lane;
W14.2 must keep that contract explicit because Track 2 is below the
`sonic_strict + 1` throughput threshold while Track 1 clears it by a wide
margin.

## Material Differential

W14.2 does not reopen the W3 union substrate, string-scanner widening, retained
tiny-string probes, SIMD structural scanning, or parser runtime. The material
differential from REDRESS 102 is the same one W14.1 proved viable: a
gate-consumed DOM parse-only admission report under the 2026-05-21 addendum,
backed by current Criterion lanes and an independent Track 2 oracle.

The incremental differential from W14.1 is generality: replace the
numbers-only W14.1 report/gate firewall with a row table that can support
selected W14.N parse-only rows without copy-paste. W14.2 admits only
`json/citm_catalog/parse_only/main`.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.2/`
- `skinny/REDRESS.md`

## Gate

`G-W14.2-JSON-PARSE-CITM-CATALOG` admits only if a companion
`sk-v13-json-parse-only-v1` report validates the row identity, DOM output
plane, strict equality artifact, measured UTF-8 path, escape completeness,
Lock 14 status, independent Track 2 oracle, and native Criterion lanes for
`json_citm_catalog`. Track 1 lower-confidence throughput must exceed
same-run sonic strict by more than 1 Mbps.

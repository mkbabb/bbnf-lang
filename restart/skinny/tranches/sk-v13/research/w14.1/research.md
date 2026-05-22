# SK-V13 W14.1 Research - Numbers Parse-Only Admission

Date: 2026-05-22.
Scope: `json/numbers/parse_only/main`.
Disposition: select for plan.

## Candidate

The `numbers` parse-only row already clears the pinned same-run sonic strict
bar in the rolling table: Track 1 is `19110` Mbps against sonic strict
`13335` Mbps. The row remains `S / NO-GO` because the historical parse-only
firewall marks it as `deferred`, `view-boundary`, and
`borrowed view over offset tape vs DOM`, with no gate-consumed strict equality
report under the 2026-05-21 addendum.

## Material Differential

W14.1 does not reopen W3 union substrate, tape shape, SIMD structural scanning,
or parser runtime. The material differential is legal admission plumbing under
the addendum's parse-only re-pin: a gate-consumed W14.1 parse-only report that
proves strict equality, measured UTF-8, same-wave consumer path, independent
Track 2, and sonic+1 throughput for the selected row.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.1/`
- `skinny/REDRESS.md`

## Gate

`G-W14.1-JSON-PARSE-NUMBERS` admits only if a companion
`sk-v13-json-parse-only-v1` report validates the row identity, DOM output
plane, strict equality artifact, measured validation path, Lock 14 status,
and native Criterion lanes:
`json_numbers/track1_generated`, `track2_handcoded`, `sonic_rs_anchor`, and
`serde_json`.

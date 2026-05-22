# SK-V13 W14.1 Redress - Numbers Parse-Only Admission

Date: 2026-05-22.
Disposition: ADMIT.
Gate: `G-W14.1-JSON-PARSE-NUMBERS`.

## Summary

W14.1 admits `json/numbers/parse_only/main` under the 2026-05-21
parse-only re-pin. The parser runtime is unchanged; the material
differential is the new gate-consumed DOM strict-equality contract for a
row that was previously held behind the parse-only substrate guard.

The source change adds:

- `sk-v13-json-parse-only-v1` report parsing and validation.
- `gate-json --skv13-json-parse-only-report` and `xtask gate-json`
  passthrough.
- Generated `RESULTS.md` support for the W14.1 numbers parse-only
  admission.
- Rolling delta validation support for the admitted parse row while keeping
  legacy positive-margin parse rows `OPEN` until they carry equivalent
  provenance.
- A gate metadata fix that ignores stale non-required `real_typed_struct`
  Criterion metadata for fixtures that do not have a real typed product
  surface.

## Measurement

Native full-capture command:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity
```

W14.1 companion facts:

| Metric | Mbps |
|---|---:|
| Track 1 mean | 19102.844 |
| Track 1 lower confidence | 19002.696 |
| Track 2 mean | 19289.254 |
| sonic-rs strict mean | 13610.385 |
| Threshold | 13611.385 |
| Mean margin | 5491.459 |

The gate-generated `RESULTS.md` slope row records Track 1 `19267` Mbps,
Track 2 `19126` Mbps, sonic strict `13666` Mbps, and an admitted rolling
margin of `5600.00` Mbps.

The measurement artifact is
`restart/skinny/tranches/sk-v13/research/w14.1/numbers-parse-facts.json`
with SHA-256
`a4c6afeb13a342691fd1639f5a15bcecd274f2e8f7e8e2a7a410d653735fce50`.

## Verification

Passed:

```text
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_numbers_admit -- --nocapture
cargo test -p bbnf-bench skv13_json_parse_only_report_arg_allows_json_check_only -- --nocapture
cargo test -p bbnf-bench w14_1_numbers_parse_only_reopens_only_sonic_plus_one_numbers -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w14_1_parent_diff_under_w14_1_scope -- --nocapture
cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.1/skv13-W14.1-json-parse-only.json
```

## Result

`json/numbers/parse_only/main` moves from `S / NO-GO` to `A / GO`.
The remaining parse-only rows stay open unless a later wave supplies the
same strict DOM output-plane admission evidence and sonic+1 measurement.

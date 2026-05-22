# SK-V13 W14.1 Plan - Numbers Parse-Only Admission

Date: 2026-05-22.
Wave: W14.1.
Gate: `G-W14.1-JSON-PARSE-NUMBERS`.

## Intervention

Admit `json/numbers/parse_only/main` under the 2026-05-21 parse-only re-pin
by adding gate-consumed parse-only report plumbing. The row already clears the
same-run sonic strict bar in the rolling table; W14.1 supplies the missing
strict DOM output-plane admission evidence rather than changing parser runtime.

The implementation introduces:

- A `sk-v13-json-parse-only-v1` companion report schema for one row only:
  `json/numbers/parse_only/main`.
- A `gate-json` validator that requires DOM output, strict equality,
  independent Track 2, measured UTF-8, complete escape handling, Lock 14
  status, and native Criterion lanes.
- `xtask gate-json` passthrough support for the new companion report flag.
- A W14.1 REDRESS entry and row-status update only if native Criterion
  confirms Track 1 lower confidence throughput greater than sonic strict + 1
  Mbps.

## Acceptance

The wave admits only if:

- The companion report validates row identity, provenance, output plane, same
  wave consumer, strict equality artifact, Track 2 independence, measured
  validation path, Lock 14 state, and REDRESS citation.
- Native Criterion lanes are present for `json_numbers/track1_generated`,
  `json_numbers/track2_handcoded`, `json_numbers/sonic_rs_anchor`, and
  `json_numbers/serde_json`.
- `threshold_mbps == sonic_strict_mbps_after + 1.0`, and the Track 1 lower
  confidence bound also clears that threshold.
- The report has no `block_id`, affects only `json/numbers/parse_only/main`,
  and records `row_move_toward_sota_status` as `admitted`.

## Revert Protocol

If report validation or Criterion confirmation fails, revert the source patch,
save it at `/tmp/skv13-waveW14.1-rejected.patch`, append a measured REDRESS
reject, and do not update `RESULTS.md` or `ROLLING-SOTA-DELTA.md`.

If the admission passes, keep the gate/report/xtask plumbing, update the
numbers parse-only row status, and append REDRESS with the measurement artifact
hash.

## Measurement

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/numbers/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.1/skv13-W14.1-json-parse-only.json
```

# SK-V12 W1b-2b A4 - JSON Guard And Lock14 Interactions

Date: 2026-05-20.
Scope: read-only research for W1b-2b report/gate planning.

## Findings

W1b-2b must use a JSON-populated Criterion root for the JSON guard. The
acceptable root is either the existing accepted JSON Criterion root that
rendered the current `skinny/RESULTS.md`, or a fresh root populated by a JSON
bench capture before gate consumption. In command terms, this means
`CRITERION_HOME` must point at a directory containing the `json_<fixture>/...`
groups and `simd_structural_scan/canada_simd/...` rows consumed by
`skinny/crates/bbnf-bench/src/bin/gate.rs`.

CSS-only Criterion roots are invalid JSON guard roots. The JSON gate iterates
JSON fixtures, reads `json_<fixture>` groups for Track 1, Track 2/oracle,
sonic-rs, serde, direct, and typed rows, validates W0 capture metadata, and
also reads SIMD scan metadata. A root populated only by CSS L4 Criterion groups
cannot satisfy those reads, cannot prove the direct and typed JSON floors from
SPEC Section 0.5, and would turn W1b-2b into a producer-only CSS proof instead
of a same-wave gate-consumed JSON guard.

Lock14 is consumed before report-specific branching. `gate.rs` calls
`lock14_baseline::validate(&workspace)` at process start, before W1a/SK-V12
companion report validation and before normal JSON report rendering. A W1b-2b
CSS L4 SOTA companion report may therefore record
`lock14_status=pass:lock14_baseline::validate` only when the same gate process
has passed the baseline validation covering frozen roots, backend shape
surface, and generic-crate neutrality.

For the SK-V12 non-JSON report schema already in `report.rs`,
`json_guard_state` is gate-validated as either
`not_refreshed:no_behavior_drift` or `refreshed:*guards-pass*`. That is the
right contract to preserve for W1b-2b CSS L4 SOTA: use
`not_refreshed:no_behavior_drift` only when the implementation can prove no
JSON-producing behavior path moved and `skinny/RESULTS.md` is unchanged; use a
`refreshed:<run>:guards-pass` state only after a populated JSON Criterion root
has been consumed by `gate-json --check-results` or equivalent no-write guard
check.

## RESULTS.md Rule

`skinny/RESULTS.md` may move only for an actual W1b-2b admitted candidate or a
measured JSON guard demotion that must be recorded in the routed ledger. It must
not move for PASS-MEASURED-BASELINE, CSS-only measurement, stale/failing
companion report evidence, or a no-touch proof. SPEC Section 7.2 explicitly
says PASS-MEASURED-BASELINE records REDRESS evidence and does not move
`skinny/RESULTS.md`.

## Practical Guard Shape

Use a no-write companion invocation for W1b-2b:

```sh
CRITERION_HOME=/path/to/populated-json-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json \
  --skv12-css-l4-sota-report restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json \
  --advisory --check-results
```

The important property is not the exact temporary path; it is that the
Criterion root contains accepted JSON guard artifacts, while the companion
report points at W1b-2b CSS L4 SOTA evidence. Do not combine the companion
report with `--update-results`, `--write-results`, or volatile probe flags.

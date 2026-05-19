# SK-V10 W1 Research - Direct Output Contract

Pass: Wave Research.
Cycle: W1.
Date: 2026-05-19.
Scope: read-only analysis for the direct output/control-path contract.

## Inputs

- SPEC Section 4 dispatches W1 after W0 close. W0 closed under REDRESS 99.
- W1 is contract-only. It must not move `RESULTS.md` rows.
- Owner paths are `direct_struct.rs`, `report.rs`, `gate.rs`,
  `benches/json_parity.rs`, `xtask/src/main.rs`, and
  `research/p3/direct-contract/`.
- P3-C requires direct row movement to carry same-run strict direct comparator
  evidence on the digest plane, generated Track 1, independent Track 2/oracle,
  same-run run id, measured validation path, and gate-json consumption.
- P3-E blocks direct digest evidence from becoming typed product proof and
  blocks REDRESS 73/93 style helper transfer or scalar-parent folding without a
  direct-contract material differential.

## Current Code Shape

- `bbnf_bench::gate::classify_direct_projection` returns
  `N-direct` when generated Track 1 or Track 2 misses
  `sonic_rs_direct_to_struct * 1.10`.
- `report.rs` already validates:
  - schema-v3 required report fields;
  - uniform SK-V9-open run id;
  - direct native comparator source paths
    `criterion:json_<corpus>/sonic_rs_direct_to_struct/new/estimates.json` and
    `serde_json_direct_to_struct`;
  - direct comparator plane `digest`, comparator strictness `strict`,
    freshness `same-run-native`, and sidecar freshness `n/a`;
  - Track 2 independence status is present.
- `report.rs` still rejects all outcome/verdict movement from the W0 baseline.
  That is correct for W0 but insufficient for W2: W2 needs a gate predicate
  that can distinguish valid direct row movement from an arbitrary RESULTS edit.
- `TelemetryRow::workload` and `w0_telemetry` currently render direct rows as
  `strictness=deferred`, `parse_utf8=view-boundary`, and
  `measured_validation_path=view-boundary`. W1 should not change those current
  rows because W1 is not row-moving.

## Contract Gap

The missing executable contract is a predicate for future movement of a row
whose SK-V10 opening baseline is `N-direct / NO-GO`. That predicate should be
called by `gate-json` during report validation and should allow movement only
when all direct-contract fields are present. Rows that do not move should keep
the current W0 baseline invariant.

Required direct movement predicate:

- baseline row id is `json/<corpus>/direct_to_struct/main`;
- baseline outcome is `N-direct`;
- new outcome/verdict is `A / GO`;
- row output plane is `digest`;
- row strictness, `parse_utf8`, and measured validation path are `strict` /
  `measured-row` / `measured-row`;
- Track 2 independence status is `independent_verified`;
- same-wave consumer is not `gate_only`;
- REDRESS entry is present and not `none`;
- sonic-rs and serde_json native comparator evidence is same-run, strict,
  digest-plane, and sourced from the direct Criterion benches.

## Recommendation

Proceed to W1 plan with a narrow `report.rs` gate change and unit tests:

- Add a direct row movement contract validator to `Report::validate_sk_v8_w0`.
- Keep unchanged W0 baseline rows accepted.
- Let only `N-direct -> A / GO` direct rows through the new contract; reject
  missing strictness, output-plane mismatch, view-boundary validation,
  gate-only consumer, missing REDRESS, and broken comparator provenance.
- Do not edit `RESULTS.md` or benchmark bodies in W1.

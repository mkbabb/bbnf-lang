# SK-V7 Wave 0b Close: schema-v3 telemetry row builder

Intervention: schema-v3 telemetry row builder with same-run sonic-rs
strict/lossy provenance.

Outcome: ADMIT for reporting/provenance. The overall performance authority
remains `N-direct / NoGo`.

## Source Changes

- Replaced the legacy results table renderer with a schema-v3 telemetry row
  builder in `skinny/crates/bbnf-bench/src/report.rs`.
- Bumped benchmark metadata to schema `3` and attached workload, strictness,
  UTF-8, flaw-probe, output-plane, feature-mask, API-symbol, sidecar freshness,
  primitive-status, and hot-leaf provenance.
- Converted the duplicate sonic benchmark row into explicit same-run lossy
  provenance through `Deserializer::utf8_lossy()` while keeping Cargo features
  strict.
- Added same-run `serde_json` parse comparator values and kept C++ simdjson,
  yyjson, RapidJSON, and asmjson values as documented stale sidecar columns
  only when available.

## Verification

| Command | Result | Evidence |
|---|---|---|
| `cargo test -p bbnf-bench` | PASS | 26 tests passed, including schema-v3 render and required-field validation. |
| `cargo tree -p bbnf-bench --edges=features \| rg 'sonic-rs\|utf8_lossy\|sort_keys'` | PASS | Output shows `sonic-rs feature "sort_keys"` and `sonic-rs v0.5.8`; no `utf8_lossy`. |
| `cargo bench -p bbnf-bench --bench json_parity` | PASS | Criterion completed the full JSON corpus, direct rows, same-run sonic strict/lossy rows, and probe rows. |
| `cargo run -p bbnf-bench --bin gate --release` | EXPECTED FAIL: exit 5 | `skinny/RESULTS.md` regenerated with the schema-v3 header and exited only because the measured authority is still `N-direct / NoGo`. |
| `cargo run -p xtask --release -- gate-json` | EXPECTED FAIL: exit 1 wrapping gate exit 5 | Reached the same schema-v3 gate and failed only on the current performance verdict. |

## Falsifiability Checks

- PASS-ALPHA schema-v3 named columns render in `skinny/RESULTS.md`, including
  `Workload`, `Strictness`, `parse_utf8`, `escape_complete`, `flaw_probe`,
  `Output plane`, strict/lossy sonic, sidecar comparator columns, and hot-leaf
  provenance.
- Parse rows use `Workload=parse_only`.
- Same-run sonic strict Mbps and same-run sonic lossy Mbps are populated on
  parse rows. Lossy sonic rows are marked as flaw-probe provenance and are not
  used for verdict classification.
- Same-run `serde_json Mbps` is populated on parse and same-plane generated
  workload rows where the benchmark has a matching row.
- C++ sidecar columns are populated only from documented profile artefacts and
  remain `n/a` when no same-plane sidecar value exists.
- `Delta vs SK-V6` is present with explicit `n/a` text because W0b does not
  have a machine-readable SK-V6 baseline binding. This is recorded as honest
  provenance rather than inferred data.

## Row-Close Evidence

| Row | Workload | Track 1 Mbps | sonic-rs strict Mbps | sonic-rs lossy Mbps | Verdict |
|---|---|---:|---:|---:|---|
| `instruments` | `parse_only` | 18038 | 16312 | 18747 | `K / NO-GO` |
| `instruments` | `direct_to_struct` | 11972 | 12673 | n/a | `N-direct / NO-GO` |
| `unicode_basic` | `parse_only` | 11416 | 15596 | 15625 | `K / NO-GO` |
| `unicode_basic` | `direct_to_struct` | 8576 | 8502 | n/a | `A / GO` |

Schema repair did not reclassify `instruments` or `unicode_basic` by itself.
The row authority stays measurement-classified, and W1 may open with schema-v3
reporting in place.

## REDRESS

REDRESS item: 78.

W0b admits the schema-v3 telemetry and provenance substrate. It does not admit
any parser/runtime performance path and does not reopen the pre-blocked Class A,
SK-V5 UTF-8 fusion, or SK-V6 retained/direct materialization routes.

# SK-V15 W0 Redress: Baseline Telemetry Lock

Date: 2026-05-28.
Authority: G-Omega V9 authorized; SK-V15 W0 is the next implementation slice.
Evidence basis: `skv15-W0-plan.md`, W0 research reports A-F, and the
same-wave implementation checks listed below.
Status: REDRESS-ADMITTED.

## Scope

W0 is a telemetry and baseline lock. It may make the existing W0 manifest into
the SK-V15 carrier, prove same-wave gate consumption, and preserve the 51 JSON
guard rows as the SK-V15-open baseline. It does not change parser behavior,
runtime behavior, provider behavior, generated runtime output, benchmark
semantics, or CSS admission logic.

Required W0 row posture:

- 51 JSON rows remain `A / GO / strict / measured-row` on native Apple M5 Max /
  aarch64 telemetry with explicit SK-V15-open fields and
  `broadcast_group_id=none:independent`.
- 24 CSS W8R rows are retained only as diagnostic broadcast evidence unless a
  later wave supplies independent typed-output retime proof. Their shared W8R
  tuple must be visible through `measurement_row_id` and `broadcast_group_id`,
  and cannot close as 24 independent live CSS admits.
- The visible schema-v3 RESULTS table remains unchanged in shape. The W0
  telemetry manifest becomes the SK-V15 carrier.

## Implementation Approach

Use the existing RESULTS W0 manifest as the canonical carrier. Rename the
section to `## SK-V15 W0 Telemetry Manifest` and append the ten exact SK-V15
fields after `Comparator evidence`:

`measurement_row_id`, `measurement_origin`, `value_plane`,
`css_comparator_workload`, `generator_source`, `lock14_scan_scope`,
`lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and
`broadcast_group_id`.

The report producer should render the 42-cell manifest, but producer output is
not enough. `gate-json --check-results` must parse and validate every emitted
SK-V15 field, reject missing or blank fields, reject self-exempting gate
exclusion reports, reject hidden or visible CSS broadcast admission, and reject
live CSS admission from `full_parse_summary`, `fact_stream`,
`CSS_GENERATED_RS`, or hand-written CSS tokenizer provenance.

W0 may carry CSS W8R evidence only as diagnostic/non-admit telemetry. W1 owns
live CSS admit demotion/collapse; W5/W6 own typed CSS replacement and same-plane
retime proof.

## Protected Surfaces

Do not edit, regenerate, delete, normalize, or revert these surfaces in W0:

- `crates/core/src/runtime/**`
- `skinny/crates/runtime/src/grammars/**`
- `skinny/crates/codegen/**`
- root `xtask/src/main.rs` and `xtask/src/regen_simple_runtime.rs`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- historical SK-V12/SK-V13 research JSON
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`

Unrelated dirty work in those files must be preserved, not cleaned up or folded
into W0.

## Verification Checklist

Close evidence:

- `skinny/RESULTS.md` contains exactly one SK-V15 W0 telemetry manifest and no
  SK-V14 W0 manifest heading.
- The manifest has exactly 75 data rows: 51 JSON rows and 24 CSS diagnostic
  rows.
- Every manifest row has exactly 42 cells and non-empty values for all ten
  SK-V15 fields.
- JSON rows retain native Apple M5 Max / aarch64 telemetry, strict same-plane
  admission, explicit `css_comparator_workload=n/a:not-css`, and
  `broadcast_group_id=none:independent`.
- CSS W8R rows expose the shared W8R broadcast group and cannot be interpreted
  as independent live admits.
- Negative fixtures prove rejection for missing SK-V15 fields, 24 CSS admits
  sharing one `measurement_row_id`, 24 CSS admits with unique ids but identical
  hidden measurement signatures, `gate_exclusion_report=self-exempting:*`, and
  CSS live admission from full-parse/fact-stream/generated-provider provenance.
- Protected-surface diffs remain outside the W0 owner slice and were preserved,
  not staged, regenerated, or reverted.

Command evidence from the skinny workspace:

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --update-results --skv14-existing-results-capture
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
```

Results:

- `bbnf-bench skv15_w0`: 2 passed, 0 failed.
- `xtask skv15_w0`: 5 passed, 0 failed.
- RESULTS capture: completed and regenerated the SK-V15 W0 manifest from the
  existing-results carrier.
- `gate-json --check-results`: completed against the manifest validator.
- Invariants rechecked at close: lock count 16; Pattern H runtime file count
  67.

The check-results path is intentionally a RESULTS-manifest validation in W0.
It does not replay stale Criterion metadata from earlier profiles because W0 is
not a benchmark admission or retime wave; live benchmark replay and CSS retime
proof are routed to later wave owners.

## Disposition

REDRESS-ADMITTED. W0 closes as a carrier/gate-consumption slice: 42-cell SK-V15
manifest, 51 JSON guard rows preserved, 24 CSS W8R rows demoted to diagnostic
non-admission broadcast evidence, negative fixtures load-bearing, and no
protected behavior/provider surface moved. W1 owns CSS broadcast ledger
collapse; W5/W6 own typed CSS replacement and same-plane retime proof.

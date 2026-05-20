# SK-V12 Wave W1 Plan: Sheets Generated Direct-Sink Baseline

Inputs:

- `restart/skinny/tranches/sk-v12/SPEC.md:397` - W1 ordered target
  selection, fallback rule, and single selected-target redress discipline.
- `restart/skinny/tranches/sk-v12/SPEC.md:424` - exit gate
  `G-W1-GENERATED-NONJSON-BASELINE`.
- `restart/skinny/tranches/sk-v12/SPEC.md:112` - Section 0.4 telemetry
  fields that the non-JSON gate must consume.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A1-css-l4-preflight.md:10`
  - CSS has a concrete JSON-profiled codegen/runtime preflight failure.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A1-css-l4-preflight.md:52`
  - skipping CSS must cite the JSON-profiled blocker and missing generated CSS
  direct-sink module.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A2-sheets-preflight.md:8`
  - Sheets is the legal fallback after CSS preflight failure.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A2-sheets-preflight.md:17`
  - direct/sink is the selected Sheets output plane, not typed direct.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A4-codegen-runtime-seam.md:29`
  - W1 needs a selected non-JSON provider branch and generated runtime module,
  not removal of the JSON guard alone.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A5-bench-oracle-gate.md:37`
  - companion report row shape for the W1 baseline.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A6-redress-preblocks.md:45`
  - W1 revert protocol.

Intervention: Admit exactly one generated non-JSON baseline row by selecting
the Sheets formula direct/sink route:
`sheets/formula/direct_to_struct/main`.

## Ordered-Target Decision

CSS L4 declaration values are skipped at plan time. The concrete failure is
inside the W1 owner surface: skinny codegen remains JSON-profiled, the direct
renderer is JSON-shaped, and there is no generated CSS direct-sink runtime
module. That makes CSS unable to name the full generated Track 1 path, runtime
module, fixture corpus, oracle, compile/equality smoke, and gate consumer
inside one W1 redress.

The selected target is therefore Sheets formula, direct/sink:

- row id: `sheets/formula/direct_to_struct/main`
- grammar id: `sheets`
- domain: `non_json_generated:sheets`
- workload: `direct_to_struct`
- output plane: `direct_sink`
- workload class: `baseline`

Once this plan is accepted, W1 redress attempts Sheets only. A Sheets failure
records BLOCKED or REJECTED and cannot fall through to BBNF-self inside the
same redress.

## Owner Paths

Redress is authorized to touch only:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/json_provider.rs` only to preserve the JSON guard
  while delegating selected non-JSON emission elsewhere
- new selected provider/renderer files under `skinny/crates/codegen/src/`
  for Sheets direct/sink generation
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/sheets/`
- `skinny/crates/bbnf-bench/`
- selected Sheets fixtures and report artifacts under
  `restart/skinny/tranches/sk-v12/research/w1/`
- `skinny/RESULTS.md` only if W1 deliberately renders the admitted non-JSON
  row there
- `skinny/REDRESS.md` on admit, block, or reject

No other source path is authorized without returning to plan/CHALLENGE.

## Required Redress Shape

The implementation must:

1. Add a selected Sheets non-JSON provider path that emits a generated
   direct/sink runtime module from Sheets formula facts. It must not remove the
   JSON guard and route Sheets through JSON templates.
2. Add the generated runtime module under
   `skinny/crates/runtime/src/grammars/sheets/`, exported through
   `skinny/crates/runtime/src/lib.rs`.
3. Add a Sheets fixture corpus and an independent same-plane oracle/Track 2.
4. Add a Criterion `nonjson_baseline` row for
   `nonjson/sheets/formula`, sample count >= 30.
5. Extend the SK-V12 companion schema/gate so it consumes every Section 0.4
   field for W1, including `strictness`, `measured_validation_path`,
   `profile_artifact`, `scalar_reference_status`,
   `checkasm_or_parity_status`, and `comparator_set`.
6. Add a W1-specific Lock 14 authorization/update for the selected
   codegen/runtime/bench gate changes. This authorization must stay scoped to
   the W1 owner paths and must not allow generic JSON policy drift.
7. Produce an artifact-backed strict equality result, not only provenance
   strings in the report.

## Falsifiability Gate

Gate: `G-W1-GENERATED-NONJSON-BASELINE`.

The wave admits only if exactly one W1 row satisfies all of:

- `row_id = sheets/formula/direct_to_struct/main`
- generated Track 1 compiles from selected Sheets formula facts
- independent same-plane oracle/Track 2 compiles and runs
- strict output equality passes on the W1 Sheets fixture corpus
- Track 1 Mbps >= 1
- oracle/Track 2 Mbps >= 1
- sample count >= 30
- companion gate consumes every Section 0.4 field
- JSON guard state is either refreshed with guard floors passing or
  `not_refreshed:no_behavior_drift` with `skinny/RESULTS.md` unchanged
- Lock 14 and SPEC Section 2.1 pass

The report row must use:

- `schema_id = sk-v12-nonjson-generated-v1`
- `wave_id = SK-V12-W1`
- `domain = non_json_generated:sheets`
- `workload_class = baseline`
- `output_plane = direct_sink`
- `strictness = strict`
- `measured_validation_path = criterion+oracle+gate`
- `track2_independence_status = independent_verified`
- `oracle_status = same-plane:strict:independent:fresh`
- `baseline_row_id = none`
- `baseline_mbps = null`
- `threshold_mbps = null`
- `same_wave_consumer_class = companion_gate_generated_baseline`
- `scalar_reference_status = generated_scalar_reference`
- `checkasm_or_parity_status = strict_oracle_parity`
- `comparator_set = independent_oracle`

## Commands

Expected redress validation commands, run from `skinny/` unless noted:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1-target CRITERION_HOME=/tmp/skv12-w1-nonjson-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo bench -p bbnf-bench --bench nonjson_baseline -- nonjson/sheets/formula

RUSTFLAGS="-C target-cpu=native" \
  cargo run -p xtask -- gate-json --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1/skv12-W1-nonjson-baseline.json

cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" \
  cargo run -p xtask -- gate-json --advisory --check-results
git diff --exit-code -- RESULTS.md
```

If W1 deliberately renders the non-JSON row into `skinny/RESULTS.md`, the plan
requires a refreshed gate command that consumes that row and a corresponding
REDRESS entry naming the new result surface.

## Hard Cap

Redress cap: 75 minutes.

LOC budget: <=480 non-generated LOC for the Sheets route, excluding generated
runtime output and measured artifacts. If the implementation cannot fit this
budget, W1 records BLOCKED or returns to S-P3 for a split-wave revision before
source work continues.

## Same-Wave Consumer

The same-wave consumer is the generated Sheets direct/sink parser exercised by
the `nonjson_baseline` Criterion row, the independent oracle equality check,
and `gate-json --skv12-non-json-report`.

The gate report alone is not sufficient. The report must point to real
Criterion and equality artifacts for the same run id.

## Pre-Blocked Routes

Do not reopen:

- REDRESS 111 report fixture as baseline.
- REDRESS 112/113 future-phase promise.
- Hand-only non-JSON parser.
- Stale `sheets_witness`.
- JSON provider cloning under a neutral name.
- Generic JSON policy.
- New directive, BIR, or backend shape.
- REDRESS 70/71 typed-output shortcuts.
- Source-only baseline claims without measured Mbps.
- Parse-only admission, W3 union/substrate routes, or JSON direct residual
  movement.

## Revert Protocol

On failed measurement, equality, compile, Lock 14, Section 2.1, or gate
consumption:

1. Save the full failed patch to `/tmp/skv12-waveW1-rejected.patch`.
2. Revert codegen/runtime/bench/report/gate/RESULTS changes and generated
   files for Sheets as one slice.
3. Add a REDRESS entry recording W1 as BLOCKED or REJECTED, with the failed
   preflight, compile, equality, or measurement evidence.
4. Do not fall through to BBNF-self inside W1 redress.

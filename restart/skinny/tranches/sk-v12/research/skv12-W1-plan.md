# SK-V12 Wave W1 Plan V2: Sheets Generated Direct-Sink Baseline

Inputs:

- `restart/skinny/tranches/sk-v12/SPEC.md:112` - Section 0.4 telemetry
  fields that the non-JSON gate must consume.
- `restart/skinny/tranches/sk-v12/SPEC.md:397` - W1 ordered target
  selection, fallback rule, and single selected-target redress discipline.
- `restart/skinny/tranches/sk-v12/SPEC.md:408` - W1 plan must name generated
  Track 1 path, runtime module path, fixture corpus, oracle path, equality
  command, gate command, and rollback slice.
- `restart/skinny/tranches/sk-v12/SPEC.md:424` - exit gate
  `G-W1-GENERATED-NONJSON-BASELINE`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:37` - Lock 14 runs before the
  SK-V12 non-JSON report lane.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:381` - Lock 14 frozen roots
  include `crates/runtime/src` and `crates/codegen/src`.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:481` - parent-diff
  authorization is subject-scoped and must name an accepted wave owner slice.
- `skinny/crates/bbnf-bench/src/report.rs:171` - the current SK-V12 row struct
  is `deny_unknown_fields` and must be extended for all Section 0.4 fields.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A1-css-l4-preflight.md:10`
  - CSS has a concrete JSON-profiled codegen/runtime preflight failure.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A1-css-l4-preflight.md:52`
  - skipping CSS must cite the JSON-profiled blocker and missing generated CSS
  direct-sink module.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A2-sheets-preflight.md:8`
  - Sheets is the legal fallback after CSS preflight failure.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A2-sheets-preflight.md:17`
  - direct/sink is the selected Sheets output plane, not typed direct.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-challenge.md:21` - CH1
  requires exact paths, artifacts, commands, and Lock 14 citation.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-challenge.md:58` - CH4
  requires a component LOC/time budget before redress.

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

## Exact Redress Surface

Generated Track 1 source path:

- `skinny/crates/runtime/src/grammars/sheets/generated.rs`

Runtime module path:

- `skinny/crates/runtime/src/grammars/sheets/mod.rs`
- `skinny/crates/runtime/src/lib.rs`

Codegen path:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/sheets_direct.rs`
- `skinny/crates/codegen/src/json_provider.rs` only if needed to preserve the
  JSON guard while delegating selected non-JSON emission elsewhere

Fixture corpus:

- `restart/skinny/tranches/sk-v12/research/w1/fixtures/sheets-formulas.txt`

Independent oracle / Track 2 path:

- `skinny/crates/bbnf-bench/src/nonjson_sheets.rs`

Benchmark path:

- `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
- `skinny/crates/bbnf-bench/Cargo.toml`

Gate/report path:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs` only if needed for a printed gate
  label or report dispatch change
- `skinny/xtask/src/main.rs` only if the existing `--skv12-non-json-report`
  passthrough must be extended
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`

Artifacts:

- equality artifact:
  `restart/skinny/tranches/sk-v12/research/w1/skv12-W1-sheets-equality.txt`
- report artifact:
  `restart/skinny/tranches/sk-v12/research/w1/skv12-W1-nonjson-baseline.json`
- Criterion root:
  `/tmp/skv12-w1-nonjson-criterion`
- rejected patch path:
  `/tmp/skv12-waveW1-rejected.patch`

Result ledgers:

- `skinny/RESULTS.md` only if W1 deliberately renders the admitted non-JSON row
  there
- `skinny/REDRESS.md` on admit, block, or reject

No other source path is authorized without returning to plan/CHALLENGE.

## Required Redress Shape

The selected implementation is a narrow generated formula digest baseline:

1. `codegen::emit_from_source("sheets", SHEETS_FORMULA_SOURCE)` must route to
   `sheets_direct.rs` and emit only the generated Sheets direct/sink runtime
   files. The JSON profile remains guarded; Sheets must not pass through JSON
   templates.
2. `sheets_direct.rs` owns the selected Sheets formula facts and generated
   renderer. It may support only the W1 fixture grammar surface: formula
   prefix, identifiers, numbers, strings with doubled quote escape, cell refs,
   ranges, function calls, parentheses, comma/semicolon separators, whitespace,
   and common binary operators.
3. `runtime/src/grammars/sheets/generated.rs` exposes the generated Track 1
   direct/sink parser used by the benchmark. It must not import
   `sheets_witness` or `generated_json`.
4. `nonjson_sheets.rs` owns fixture loading, independent oracle digesting,
   strict Track 1 vs oracle equality, report row construction, and test hooks.
5. `nonjson_baseline.rs` benchmarks exactly `nonjson/sheets/formula` with
   sample count >= 30 and writes/points to the same run id used by the report.
6. `report.rs` extends `SkV12NonJsonRow` and validation to consume every
   Section 0.4 field, especially `strictness`, `measured_validation_path`,
   `profile_artifact`, `scalar_reference_status`,
   `checkasm_or_parity_status`, and `comparator_set`.
7. `lock14_baseline.rs` adds a W1-specific parent-diff authorization scoped to
   this exact W1 owner set. It must not authorize JSON generated output or
   generic policy changes.

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
- `row_id = sheets/formula/direct_to_struct/main`
- `grammar_id = sheets`
- `domain = non_json_generated:sheets`
- `corpus_or_workload = formula`
- `workload = direct_to_struct`
- `workload_class = baseline`
- `output_plane = direct_sink`
- `outcome_id = A`
- `verdict = GO`
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
- `json_guard_state = not_refreshed:no_behavior_drift` unless refreshed JSON
  guard evidence is produced

## Commands

Strict equality command:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench nonjson_sheets_strict_equality -- --nocapture
```

Benchmark command:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1-target CRITERION_HOME=/tmp/skv12-w1-nonjson-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo bench -p bbnf-bench --bench nonjson_baseline -- nonjson/sheets/formula
```

Gate command:

```sh
RUSTFLAGS="-C target-cpu=native" \
  cargo run -p xtask -- gate-json --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1/skv12-W1-nonjson-baseline.json
```

JSON guard commands:

```sh
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

## Cost Table

The Sheets redress must stay under <=480 non-generated LOC and <=75 minutes.
Generated `runtime/src/grammars/sheets/generated.rs`, Criterion output, JSON
report output, equality output, and REDRESS prose are outside the LOC cap but
inside the wall-clock cap.

| Component | Files | LOC cap | Time cap |
|---|---|---:|---:|
| Selected codegen route | `codegen/src/lib.rs`, `codegen/src/sheets_direct.rs` | 105 | 14 min |
| Runtime module wrapper | `runtime/src/lib.rs`, `runtime/src/grammars/sheets/mod.rs` | 20 | 4 min |
| Fixture/oracle/equality support | `bbnf-bench/src/nonjson_sheets.rs`, fixture file | 115 | 16 min |
| Criterion bench registration | `bbnf-bench/benches/nonjson_baseline.rs`, `bbnf-bench/Cargo.toml` | 55 | 8 min |
| Section 0.4 schema/gate extension | `bbnf-bench/src/report.rs`, focused tests | 105 | 14 min |
| Lock 14 W1 authorization | `bbnf-bench/src/lock14_baseline.rs`, focused tests | 45 | 7 min |
| Report/evidence plumbing | W1 report/equality artifact generation hooks | 25 | 4 min |
| Measurement and JSON guard commands | command execution only | 0 | 8 min |
| Total | selected W1 slice | 470 | 75 min |

If the slice exceeds any component cap by more than 10 LOC before measurement,
redress records BLOCKED and returns to S-P3 for a split-wave revision. It does
not continue by dropping schema fields, oracle independence, Lock 14, or
same-wave consumer evidence.

## Same-Wave Consumer

The same-wave consumer is the generated Sheets direct/sink parser exercised by
the `nonjson_baseline` Criterion row, the independent oracle equality test,
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

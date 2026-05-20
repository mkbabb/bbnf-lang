# SK-V11 W1b Phase 2 Plan: Generated Non-JSON Baseline And Oracle Lane

Status: P2 implementation route.
Owned artifact for this turn: `restart/skinny/tranches/sk-v11/research/w1b/w1b-plan-implementation.md`.
Source edits in this turn: none.

## Selected Target

Select exactly one target: `css_l4/declaration_values/direct/main`.

This is a measured-rejection plan, not a workaround and not a behavior
admission. CSS L4 remains the required first choice by SPEC Section 5 and by the
W1b research ordering. The direct plane is the narrowest admissible CSS choice:
it can be judged by stable declaration-value fact bytes without requiring the
full typed CSS runtime, typed aggregate payload model, or a new dependency owner.

The W1b gate cannot currently be made positive inside the allowed W1b surface.
The inspected skinny code has these blockers:

- `skinny/crates/codegen/src/lib.rs` routes both `emit_from_source` and
  `emit_typed_from_source` through `json_provider::ensure_runtime_profile` and
  JSON runtime templates.
- `skinny/crates/codegen/src/typed_direct.rs` is a JSON-syntax typed parser
  renderer, not a CSS L4 typed renderer.
- `skinny/crates/runtime/src/lib.rs` exports generated JSON plus proof-gated
  witnesses; it has no generated CSS L4 runtime module.
- `skinny/crates/bbnf-bench/Cargo.toml` does not carry `lightningcss`, and that
  manifest is not named by SPEC Section 5 as an owner path.
- The existing generated CSS L4 parser and typed runtime live under
  `crates/core/`, including a 107k-line generated parser, outside the skinny
  W1b owner surface and therefore inadmissible as Track 1 authority.

Therefore W1b must reject the baseline attempt unless a generated CSS L4 direct
Track 1 can be produced from the selected CSS grammar input without JSON
provider transit and consumed by the W1b gate in the same slice.

Rejected alternatives:

- CSS L4 typed: best semantic plane, but not admissible in W1b without importing
  the main-crate typed CSS stack or adding dependency/manifest owners.
- Sheets: first fallback only after CSS fails CHALLENGE; no independent
  same-plane oracle is currently in the skinny owner surface, and it does not
  seed W2's CSS-specific intervention.
- BBNF-self: last fallback only; self-host coupling risk is high, and it does
  not seed W2's CSS L4 proof.

## Gate

Primary SPEC gate: `G-W1b-NONJSON-BASELINE`.

Positive gate predicates, if the preflight unexpectedly succeeds:

- exactly one row id: `css_l4/declaration_values/direct/main`;
- generated Track 1 source under `skinny/crates/runtime/src/grammars/css_l4/`
  or a generated bench module named for this target;
- output plane: `css_l4_declaration_value_fact_bytes`;
- independent oracle source path in a reviewable W1b module under the existing
  `skinny/crates/bbnf-bench/benches/` owner path; the Criterion harness may call
  that module, but parser/oracle logic must not be hidden in the benchmark body;
- the oracle module does not import or call generated Track 1, generated JSON,
  generated SinkOnly helpers, generated typed helpers, benchmark-private parser
  code, runtime witness paths, or stale sidecars;
- strict byte equality between Track 1 fact bytes and oracle fact bytes on the
  selected corpus;
- finite same-run Track 1 Mbps and oracle Mbps with run id, host, flags, sample
  count, feature mask, output plane, oracle identity, oracle freshness,
  `track1_source_artifact`, `track1_source_kind`, generated input/output
  artifacts, strict equality artifact, source artifact, and
  `track2_independence_status` consumed by the gate;
- `outcome_id = "S"` and `verdict = "NO-GO"` for baseline-only authority;
- no `skinny/RESULTS.md` movement and no JSON parser row movement.

Measured-rejection gate predicates:

- the implementation records the selected target, generation preflight command,
  exact compile/codegen/runtime blocker, and absence of generated Track 1;
- W1a's `--w1a-non-json-report` gate still accepts its fixture;
- JSON `gate-json --with-cost-facts --check-results` still passes;
- no W1b report is accepted as positive without a generated Track 1 and strict
  oracle equality;
- the failed proof is preserved in `skinny/REDRESS.md` rather than hidden by a
  hand parser, main-crate CSS runtime, schema-only fixture, or digest-only
  shortcut.

## Owner Paths

Allowed positive-route owner paths:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/lower/`
- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/runtime/src/grammars/css_l4/`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_oracles/css_l4_decl_value.rs`
- `grammar/css/l4/`
- `restart/skinny/tranches/sk-v11/research/w1b/reports/`
- `restart/skinny/tranches/sk-v11/research/w1b/fixtures/`

Measured-rejection route should touch only:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- focused W1b rejection fixtures under
  `restart/skinny/tranches/sk-v11/research/w1b/fixtures/`
- `skinny/REDRESS.md`

Do not edit:

- `skinny/RESULTS.md`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/Cargo.toml`
- `crates/core/`
- `skinny/crates/bbnf-simd/`
- `skinny/crates/parse-that-regex/`
- generated JSON runtime files unless reverting the W1b slice that changed them
- more than one non-JSON target, workload, or output plane

## Implementation Route

1. Add the W1b sibling report validator and CLI gate without loosening W1a.
   The new report schema must reject W1a schema-only sentinels, `gate_only`,
   `non_json_gate_schema_only`, `A / GO`, JSON grammar ids, missing oracle source
   provenance, source/independence coupling, and producer-only fields.

2. Add a generated-Track-1 authority preflight for
   `css_l4/declaration_values/direct/main`.
   The preflight must attempt to name a real generated CSS L4 direct parser
   source under the W1b owner paths. It fails closed if the path would transit
   `json_provider`, main-crate `CssL4Parser`, `crates/core` runtime types,
   runtime witness modules, or a hand-authored parser.

3. Stop on the current expected blocker and record measured rejection.
   The expected blocker is that the skinny generated path is JSON-only and no
   generated CSS L4 Track 1 exists under `skinny/crates/runtime/src/grammars/`.
   Do not replace it with the main-crate CSS parser or a bench-local parser.

4. If the preflight unexpectedly proves a generated CSS L4 direct Track 1, add
   exactly one Criterion benchmark row:
   `nonjson_baseline/css_l4_declaration_values/direct`.
   Track 1 emits stable fact bytes such as:
   `decl:<index>\tproperty:<raw>\tvalue:<raw-normalized-token-stream>\n`.
   The oracle emits the same bytes from
   `skinny/crates/bbnf-bench/benches/nonjson_oracles/css_l4_decl_value.rs`.
   The bench may call that module; it must not contain the oracle parser or fact
   projection logic inline.

5. Generate or consume exactly one W1b report:
   `restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json`.
   The report must be accepted only by the W1b gate and must not update
   `skinny/RESULTS.md`.

## Tests

Focused `report.rs` tests:

- `w1b_non_json_baseline_rejects_w1a_schema_only_fixture`
- `w1b_non_json_baseline_rejects_missing_generated_track1`
- `w1b_non_json_baseline_rejects_missing_track1_source_artifact`
- `w1b_non_json_baseline_rejects_missing_generated_input_output_artifacts`
- `w1b_non_json_baseline_rejects_missing_strict_equality_artifact`
- `w1b_non_json_baseline_rejects_admission_claim`
- `w1b_non_json_baseline_rejects_oracle_coupling`
- `w1b_non_json_baseline_rejects_json_provider_source`
- `w1b_non_json_baseline_rejects_generated_track1_or_helper_oracle`
- `w1b_non_json_baseline_rejects_generated_json_oracle`
- `w1b_non_json_baseline_rejects_root_css_runtime_oracle`
- `w1b_non_json_baseline_rejects_benchmark_private_oracle`
- `w1b_non_json_baseline_rejects_stale_sidecar_or_w1a_oracle`
- `w1b_non_json_baseline_accepts_css_l4_direct_only_if_generated_and_equal`

Focused `bin/gate.rs` tests:

- `w1b_non_json_baseline_report_arg_extracts_single_path`
- `w1b_non_json_baseline_report_arg_rejects_json_result_flags`
- `w1b_non_json_baseline_report_arg_rejects_w1a_flag_combination`

If the positive route becomes possible, add one bench smoke test or unit helper
that asserts:

- selected target count is one;
- Track 1 source path is generated and non-JSON;
- Track 1 source kind, generated input artifact, generated output artifact, and
  strict equality artifact are present and gate-consumed;
- oracle source path is separate from Track 1;
- oracle source path names a reviewable module, not the Criterion harness body;
- strict fact-byte equality passes on the curated declaration corpus.

## Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Focused W1b tests:

```sh
cargo test -p bbnf-bench report::tests::w1b -- --nocapture
cargo test -p bbnf-bench --bin gate w1b -- --nocapture
```

Measured-rejection fixture must fail closed through the W1b gate:

```sh
if cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/fixtures/nonjson-css-l4-direct-missing-generated-track1.json; then exit 1; fi
```

W1a preservation:

```sh
cargo test -p bbnf-bench report::tests::w1a -- --nocapture
cargo test -p bbnf-bench --bin gate w1a -- --nocapture
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
```

Positive route only, if generated Track 1 exists before the hard stop:

```sh
CRITERION_HOME=/tmp/skv11-w1b-css-l4-direct RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_baseline -- css_l4_declaration_values
CRITERION_HOME=/tmp/skv11-w1b-css-l4-direct RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --write-w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json
cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json
```

JSON and worktree preservation:

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git -C .. diff --exit-code -- skinny/RESULTS.md
git -C .. diff --check
```

## LOC And Generated-Output Budget

SPEC hard cap: <=360 handwritten source/test/gate LOC; regenerated output capped
to the selected generated parser inputs.

Measured-rejection budget:

- `report.rs`: <=120 LOC for W1b validation helpers and focused tests.
- `bin/gate.rs`: <=45 LOC for the W1b CLI hook and argument tests.
- W1b rejection fixtures: <=60 compact JSON data lines.
- `skinny/REDRESS.md`: <=25 lines for the failed proof.
- generated output: 0 LOC.

Positive-route budget, only if the generated Track 1 preflight succeeds without
crossing a blocked owner:

- `report.rs`: <=150 LOC.
- `bin/gate.rs`: <=55 LOC.
- `benches/nonjson_baseline.rs`: <=70 LOC.
- `benches/nonjson_oracles/css_l4_decl_value.rs`: <=85 LOC.
- selected generated CSS L4 direct runtime output: only files under
  `skinny/crates/runtime/src/grammars/css_l4/`, produced from the named CSS L4
  declaration-values input; no generated JSON output movement.

Hard stop if the plan needs `skinny/crates/bbnf-bench/Cargo.toml`,
`crates/core/`, a second target, a hand-authored parser as Track 1, or more than
360 handwritten LOC.

## Exit And Redress

Expected W1b disposition: measured rejection of
`css_l4/declaration_values/direct/main` as currently unmeasurable inside W1b.

The REDRESS entry must name:

- selected target: `css_l4/declaration_values/direct/main`;
- generated Track 1 path attempted or proven absent;
- blocker: skinny codegen/runtime non-JSON generated Track 1 unavailable without
  JSON provider or main-crate CSS runtime transit;
- oracle path status: not admitted because Track 1 authority is absent;
- output plane: `css_l4_declaration_value_fact_bytes`;
- commands run and exact failure/pass status;
- W1a and JSON preservation results;
- statement that W2 remains blocked from creating the first measurable non-JSON
  baseline.

## Revert Protocol

Revert the W1b slice as one unit if any predicate weakens:

- W1b gate accepts a report without generated Track 1;
- W1b gate accepts schema-only W1a sentinel evidence;
- Track 2/oracle coupling is accepted;
- JSON provider or generated JSON runtime is used as non-JSON proof;
- `skinny/RESULTS.md` moves;
- more than one target or output plane lands;
- main-crate CSS runtime, `lightningcss` dependency edits, or manifest edits are
  smuggled into the W1b slice.

Audit before reverting:

```sh
git -C /Users/mkbabb/Programming/bbnf-lang diff -- skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/benches skinny/crates/runtime/src/grammars/css_l4 restart/skinny/tranches/sk-v11/research/w1b/fixtures restart/skinny/tranches/sk-v11/research/w1b/reports skinny/REDRESS.md
```

Revert only the W1b slice:

```sh
git -C /Users/mkbabb/Programming/bbnf-lang restore -- skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/benches skinny/crates/runtime/src/grammars/css_l4 restart/skinny/tranches/sk-v11/research/w1b/fixtures restart/skinny/tranches/sk-v11/research/w1b/reports
```

Do not use `git reset --hard`. Preserve the REDRESS record of the failed W1b
proof unless the user explicitly asks to revert documentation too.

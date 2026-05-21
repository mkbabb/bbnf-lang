# SK-V12 W1b-1 Plan - CSS L4 Scaffold

Status: selected plan proposal for CHALLENGE. No behavior/source authority until
CHALLENGE accepts the owner amendments below.

## Selection

Implement W1b-1 as a scalar-only generated CSS L4 declaration-values scaffold
for exactly `css_l4/declaration_values/direct_to_struct/main`. The output plane
is exactly `css_l4_declaration_value_fact_stream`. W1b-1 admits no CSS SOTA row,
does not use `lightningcss`, and does not touch `bbnf-simd` or aarch64 code.
W1b-2 owns the `track1_mbps > lightningcss_mbps + 1` admission bar.

The material generated Track 1 route is a CSS-specific codegen profile:

- add a `css_l4_declaration_values` runtime profile;
- add a CSS provider/templates surface that emits generated
  `mod.rs`, `config.rs`, `parser.rs`, and `generated.rs`;
- require a codegen reproducibility test that renders those files and
  byte-compares them with the committed runtime output;
- keep the CSS fact sink hand-owned and CSS-local so the oracle does not share
  generated Track 1 internals;
- export the runtime module from `runtime/src/lib.rs`;
- record the four CSS L4 grammar inputs as provenance/checksum inputs, without
  trying to teach W1b-1 the full unsupported CSS BBNF import/syntax surface.

This is a scaffold/equality wave, not the full CSS compiler.
The generic codegen selector may select a provider by profile metadata, but it
must not carry CSS token, fact-stream, or declaration policy in a generic
`if grammar_name == ...` branch.

## Required Owner Amendments

Retain SPEC Section 6 owner paths and amend the W1b-1 redress owner surface
with exactly these additional paths:

- `skinny/Cargo.toml`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/bbnf-bench/src/gate.rs` only if report validation needs a
  shared gate helper change beyond `report.rs`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/`

`skinny/xtask/src/main.rs` is not selected: its existing `gate-json`
passthrough already accepts `--skv12-non-json-report`.

## Fixture

Create the missing fixture at
`restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
with exactly:

```css
a { color: #ff00ff; width: 50%; opacity: .5; margin-left: -10px; }
b { background-color: rgb(255 128 0 / 0.5) !important; }
@media (min-width: 640px) { c { height: 100px; color: red; } }
```

Use the LF-terminated bytes. The selected fixture is 187 bytes with SHA-256
`cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`.

## Track 1 And Oracle

Track 1:

- generated runtime directory:
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`;
- generated files: `mod.rs`, `config.rs`, `parser.rs`, `generated.rs`;
- CSS-local hand file: `sink.rs`;
- fact stream schema: `css-l4-declaration-value-facts-v1`, LF-terminated,
  declaration order preserved, comments/whitespace omitted, property/function/
  unit/hash identifiers ASCII-lowercased, numeric lexemes preserved for W1b-1.

Independent oracle:

- add `cssparser` as a bench-only dependency through `skinny/Cargo.toml` and
  `skinny/crates/bbnf-bench/Cargo.toml`;
- implement the oracle in
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`;
- benchmark in `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`;
- reject any oracle route that calls generated Track 1, `runtime::generated_json`,
  root CSS runtime, `lightningcss`, `parse_that_regex`, or `bbnf-simd`.

The same-wave consumer is the `nonjson_css_l4` test/bench plus the companion
gate report. Equality is byte equality of retained fact streams before timing;
the report records a stream hash and artifact paths, not digest-only authority.
Retain Track 1 facts, oracle facts, and first-diff-on-failure artifacts under
`restart/skinny/tranches/sk-v12/research/w1b/artifacts/`.

## Gate And Report

Extend `SkV12NonJsonRow` and validation, not the main JSON `TelemetryRow` table,
with:

```text
strictness
grammar_checksum
input_checksum
input_bytes
measured_validation_path
profile_artifact
generated_loc
generated_module_bytes
grammar_size_guard
lock14_status
lock16_status
scalar_reference_status
checkasm_or_parity_status
```

For W1b-1 require:

- `row_id = css_l4/declaration_values/direct_to_struct/main`
- `output_plane = css_l4_declaration_value_fact_stream`
- `outcome_id = C`, `verdict = GO`
- finite `track1_mbps >= 1.0`
- finite `track2_or_oracle_mbps >= 1.0`
- `sample_count >= 30`
- `strict_output_equality = pass`
- `track2_independence_status = independent_verified`
- `same_wave_consumer_class = companion_gate_generated_baseline`
- `generated_loc > 0`
- `generated_module_bytes > 0`
- `grammar_size_guard = pass`
- `lock14_status = pass`
- `lock16_status = not_applicable:scalar_only`
- `scalar_reference_status = generated_scalar_track1`
- `checkasm_or_parity_status = parity_pass`

Do not add an outcome variant, do not add main `RESULTS.md` JSON columns, and
do not add a `lightningcss_mbps` placeholder in W1b-1.

## Budget

Hand-written LOC cap: 360.

Target allocation:

- codegen profile/provider/templates: 95 hand LOC, generated output separate;
- CSS runtime hand sink/export glue: 55 hand LOC;
- bench oracle/equality/bench harness: 145 hand LOC;
- report/gate schema validation: 65 hand LOC;
- fixture/report/REDRESS docs: 40 hand LOC.

Generated CSS runtime budget: record actual generated LOC and module bytes in
the report. Target <= 300 generated LOC and <= 14000 module bytes; a larger
result is allowed only if the report records `grammar_size_guard = pass` with an
O(N) explanation against the 405 LOC / 18114 byte CSS grammar-input baseline.

## JSON Guard

Because W1b-1 changes generic codegen selection, runtime exports, bench report
validation, and bench dependencies, redress must run the full JSON guard rerun:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1b-1-json-guard-target CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- bench-json --advisory
CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --with-cost-facts --advisory --check-results
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

If generated JSON output or JSON-producing behavior moves unexpectedly, redress
must also run `cargo run --manifest-path skinny/Cargo.toml -p xtask -- check-json`,
`check-real-typed`, and `check-conformance` before measuring.

## Redress Commands

From `skinny/` unless a command uses `--manifest-path`:

```sh
cargo test -p codegen css_l4_declaration_values_profile_fields_are_consumed -- --nocapture
cargo test -p codegen css_l4_declaration_values_generated_runtime_reproducible -- --nocapture
cargo test -p runtime css_l4_declaration_values -- --nocapture
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture

CARGO_TARGET_DIR=/tmp/skv12-w1b1-target \
CRITERION_HOME=/tmp/skv12-w1b1-css-l4-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- css_l4/declaration_values/direct_to_struct/main

RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json \
  --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json \
  --check-results
```

Preservation audit:

```sh
rg -n "runtime::generated_json|json_provider|CssL4Parser|CssStructBuilder|lightningcss" \
  crates/bbnf-bench/src/nonjson_css_l4.rs \
  crates/bbnf-bench/benches/nonjson_css_l4.rs \
  crates/runtime/src/grammars/css_l4_declaration_values
git diff --exit-code -- RESULTS.md
```

## Revert Protocol

If W1b-1 fails after source edits, save:

```sh
git diff --binary > /tmp/skv12-waveW1b-1-rejected.patch
```

Then revert only the W1b-1 owner slice, leaving unrelated work intact, and
record a measured `BLOCKED/FAIL` in `skinny/REDRESS.md`. Do not substitute
Sheets, BBNF-self, JSON rows, root CSS runtime, `complex-errors.css`, or a
report-only close. The fallback condition is not satisfied until W1b-2 records
measured CSS lightningcss comparator/admission redress or the user re-pins.

## Plan-Time Blockers

No blocker prevents CHALLENGE from evaluating this plan. Redress is blocked
unless CHALLENGE accepts the owner amendments above.

The high-risk issue is deliberate and must be adjudicated in CHALLENGE: the
skinny grammar parser cannot currently lower the full CSS L4 BBNF import/syntax
surface. This plan selects a generated CSS provider scaffold with grammar
checksums and strict oracle equality instead of full CSS BBNF lowering. If
CHALLENGE rules that insufficiently generated, W1b-1 must record
`BLOCKED/FAIL` for `G-W1b-1-CSS-L4-ORACLE`; it must not silently downgrade to a
hand-only or report-only row.

# SK-V15 Wave W2 Plan: Lock 14 / Lock 16 Gate Restoration

Inputs:

- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-A-lock14-scan.md`
- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-B-lock16-primitives.md`
- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-C-gate-self-exemption.md`
- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-D-dirty-tree.md`
- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-E-falsifiability.md`
- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-F-authority.md`
- `restart/skinny/tranches/sk-v15/SPEC.md:172` through `restart/skinny/tranches/sk-v15/SPEC.md:179`
- `restart/skinny/tranches/sk-v15/SPEC.md:283` through `restart/skinny/tranches/sk-v15/SPEC.md:299`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:144` through `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:160`

Intervention: restore Lock 14 / Lock 16 close coverage by making the gate
produce and consume a structured W2 coverage report. The report binds scan
roots, exclusions, primitive statuses, and gate consumers. Results-only and
companion gate paths must invoke the same lock-gate consumer instead of
returning before Lock 14 / Lock 16 coverage runs.

## Owner Paths

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs` only if typed report structs are
  needed after CHALLENGE
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/skv15_w0.rs` only if W0 telemetry rejection must parse the
  W2 schema
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` only if strict checkasm
  behavior must be tightened in the shared harness
- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-challenge.md`
- `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-redress.md`

No CSS provider/template deletion, no `CSS_GENERATED_RS` deletion, no root
runtime Pattern H edits, no generated CSS runtime rewrites, and no Decision
Engine cleanup are authorized in W2.

## Scan Roots And Exclusion Schema

The W2 coverage report must carry these columns:

`included_roots`, `excluded_roots`, `reason`, `owner`, `self_scan_status`,
`primitive_status`, `gate_consumer`, `affected_rows`, and `disposition`.

Required Lock 14 coverage roots:

- existing `GENERIC_SCAN_ROOTS`
- `crates/codegen/src/runtime_generator.rs`
- `crates/codegen/src/grammar_provider.rs`
- `crates/codegen/src/json_sink_direct.rs`
- `crates/codegen/src/json_typed_direct.rs`
- `crates/codegen/src/json_templates/`
- `crates/bbnf-bench/src/report.rs`
- `crates/bbnf-bench/src/bin/gate.rs`
- `crates/bbnf-bench/src/lock14_baseline.rs`
- `xtask/src/main.rs`
- `xtask/src/skv15_w0.rs`

Known provider/template/CSS leaks in those roots are not deleted in W2. They
must be reported with owner `SK-V15-W3` or `SK-V15-W6`, dependency row
`DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, `DEP-W6-CSS-GENERATED-RS`, or
`DEP-W6-CSS-SUMMARY-FACT-STREAM`, and disposition `routed`. Unknown or
self-exempting exclusions fail.

## Primitive Status Classification

Lock 16 must report every source-present `core::arch`, `target_feature`, and
`asm!` primitive as one of:

- `wired`
- `scalar-delegated`
- `deleted`
- `strict-checkasm-admitted`
- `architectural-block-with-REDRESS`
- `diagnostic-x86`

The same-wave change adds `checkasm_escape_mask_64` to
`xtask primitive-checkasm`. Admission evidence must be native Apple M5
Max/aarch64 with `BBNF_SIMD_STRICT=1`; x86/AVX files remain diagnostic-only.

## Gate Consumption

`cargo xtask gate-json --check-results` must validate the W0/W1 results
snapshot and then invoke a lock-gates-only bench-gate path that runs the W2
Lock 14 / Lock 16 consumer. It must no longer return before lock coverage.

Direct bench-gate companion report paths must either validate all requested
reports before returning or require `--check-results` and run the lock-gate
consumer. `--with-cost-facts` must not be a lock-gate bypass.

## Falsifiability Gate

- Required W2 roots are present in the coverage report.
- Required report columns are non-empty and parseable.
- Missing required roots reject.
- Any `self-exempting` or `diagnostic:pre-W2-incomplete` exclusion rejects.
- Known leak-bearing provider/template roots are routed, not silently passed.
- `checkasm_escape_mask_64` is in the strict `primitive-checkasm` list.
- Source-present aarch64 primitives have strict checkasm/parity or routed
  block status; x86 rows are diagnostic-only.
- `gate-json --check-results` runs lock-gate coverage.
- JSON 51/51 guard rows remain accepted.

Required commands:

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench lock14_baseline
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- primitive-checkasm
```

Global invariant checks:

```sh
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

## Budget And Challenge Trigger

SPEC W2 envelope: risk High, manual source/test LOC 120-280, generated output
reports/fixtures only, docs LOC 80-180, entry gate W1 admitted or CSS blocked,
exit gate roots/exclusions and source-present primitive status reported with
self-exemptions failing. Redress hard cap: 30 minutes.

W2 changes gate close semantics, so `DISPATCH-PROMPT.md` makes seven-lens
CHALLENGE mandatory before redress. If CHALLENGE finds the coverage report is
only paper telemetry, or the LOC budget requires folding W3/W6 cleanup into W2,
redress rejects and routes instead of widening scope.

## Revert Protocol

- Revert only W2 owner paths listed above.
- Preserve unrelated dirty root runtime/generated files and root xtask work.
- If lock-gate-only consumption cannot be implemented without forcing full
  benchmark regeneration, record a REDRESS rejection and route an Omega
  amendment rather than weakening the gate.
- If primitive status coverage cannot close on Apple M5/aarch64 within budget,
  record row-level intrinsic block with source-present inventory and strict
  checkasm evidence.

## Same-Wave Consumer

The same-wave consumer is `cargo xtask gate-json --check-results`. It consumes
W0/W1 `RESULTS.md`, the rolling delta, and the W2 lock-gate coverage path in
one command. `cargo xtask primitive-checkasm` is the strict Lock 16 evidence
producer consumed by the W2 primitive status report.

## Pre-Blocked Routes

- `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`: report and route in W2; neutralization
  belongs to W3 and deletion to W6.
- `DEP-W6-CSS-GENERATED-RS`: report in W2; delete only after W5 typed CSS
  proof and W6 old-proof retirement.
- `DEP-W6-CSS-SUMMARY-FACT-STREAM`: report in W2; retire in W6.
- Decision Engine scaffold and BackendShape lowerers remain W7-W9 work.

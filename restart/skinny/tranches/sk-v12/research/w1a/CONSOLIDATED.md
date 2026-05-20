# SK-V12 W1a Research Consolidated

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Phase: Research.

## Disposition

Research accepts W1a as dispatchable after W0. The legal intervention surface is
codegen/runtime metadata and gate consumption only: make generated CSS L4
emission legal by moving JSON policy behind `GrammarConfig` or an equivalent
per-grammar generated metadata surface. W1a must not emit or admit a CSS row.

## Six-Lens Findings

| Lens | Artifact | Finding |
|---|---|---|
| A1 | `A1-codegen-template-leaks.md` | The seven Lock 14 leaks are concentrated in the JSON provider/templates and direct renderer: structural alphabet, value dispatch, string/escape policy, number policy, object/key policy, flag interpretation, and JSON sink/view/kind bindings. |
| A2 | `A2-runtime-grammar-config.md` | Tape, `ValueRef`, `TapeBuilder`, and payload storage can remain structurally unchanged. Flag meanings and decode policy must move to grammar-owned metadata rather than generic substrate semantics. |
| A3 | `A3-lock14-gate-consumer.md` | The right executable consumer is the existing `lock14_baseline` path inside `bbnf-bench --bin gate`; adding a generic-crate neutrality scan there avoids outcome/schema churn and keeps `RESULTS.md` exactness intact. |
| A4 | `A4-regen-json-parity.md` | JSON regen/check ownership is currently byte-exact for expected files but does not reject stale extra files. `scan.rs` and `sink.rs` are both generated outputs and provider template inputs, so W1a must be explicit about file ownership. |
| A5 | `A5-ir-metadata-boundary.md` | W1a does not need IR changes. Existing `BackendIr`, recognizers, `BackendShape`, and BIR are sufficient; adding directives, BIR variants, `BackendShape` variants, or public substrate APIs is unnecessary and forbidden. |
| A6 | `A6-json-guard-redress.md` | Current `RESULTS.md` has JSON rows only, direct/typed guard floors pass, and W1a accounting should be REDRESS 121. No CSS SOTA claim is legal before W1b. |

## Selected Planning Boundary

The W1a plan should select a narrow implementation:

- add a codegen-private `GrammarProfile` / generated `config.rs` surface;
- keep JSON policy in generated JSON modules, not in CSS-reusable generic
  emitters;
- add a Lock 14 generic-crate scan consumed by `gate-json`;
- update JSON generation/tests for the new file roster if a config module is
  emitted;
- preserve JSON generated parity and guard floors;
- leave `skinny/RESULTS.md` unchanged unless a measured guard refresh rewrites
  it through the generator.

Do not select IR expansion, a hand CSS parser, a grammar-name branch in generic
substrate code, a new public tape API, or any CSS row admission in W1a.

## Verification Set For Plan

Minimum W1a redress evidence should include:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p codegen
cargo test -p runtime
cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
cargo test -p bbnf-bench lock14_baseline
cargo test -p bbnf-bench skv12_non_json_report
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

If W1a changes JSON-producing behavior or rewrites the generated report, the
plan must require a fresh isolated native JSON guard run and an explicit SPEC
Section 0.5 floor check before PASS.

# SK-V11 W1a CH6 Challenge: Anti-Paper-Close

Scope: mandatory CHALLENGE for W1a after S-P3 V4 convergence.
Lens: CH6 anti-paper-close and same-wave consumer.
Owned artifact: `restart/skinny/tranches/sk-v11/research/w1a/challenge/CH6-anti-paper-close.md`.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4.
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` W1a, telemetry, status,
  and CH6 references.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R1-gate-validator.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R2-report-metadata.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R3-fixtures-tests.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R5-telemetry-contract.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R6-redress-boundaries.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md`.

## CH6 Question

Can W1a falsely close as prose-only evidence?

Answer: only if the implementation treats a report shape, fixture file, status
paragraph, or gate-only consumer as evidence without same-wave gate
consumption. The current W1a plan avoids that route if the implementation keeps
the companion report strict, wires it to the same-wave CLI consumer, and proves
both pass and fail fixtures through commands before recording any status or
REDRESS movement.

## Accepted W1a Shape

W1a may add a companion non-JSON evidence lane only under the planned owner
paths:

- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/`.

The accepted shape is schema consumption, not behavior evidence:

- A strict companion report type must reject unknown fields and missing required
  non-JSON identifiers.
- The same wave must add a CLI consumer, such as
  `cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report <fixture>`.
- At least one pass fixture must be consumed by that CLI.
- Required fail fixtures must prove rejection of producer-only telemetry,
  Track 2/oracle coupling, and baseline/admission claims.
- The pass fixture must remain explicitly non-admitting, for example `S` /
  `NO-GO` with `same_wave_consumer_class=non_json_gate_schema_only`.
- JSON W0 validation must remain separate and strict.

This is enough to make non-JSON evidence gate-checkable for W1b. It is not
enough to claim generated non-JSON baseline authority, W2 intervention
admission, Lock 14 close, or SK-V11 close.

## Required Anti-Paper-Close Predicates

W1a cannot close unless all of these are true:

1. A gate-consumed companion report exists.
2. The companion report is consumed by a same-wave CLI path in
   `bbnf-bench --bin gate` or an equivalently owner-approved gate command.
3. The pass fixture succeeds through the CLI, not only through prose or a unit
   builder.
4. The producer-only fixture fails through the CLI or a focused validator test
   backed by the same strict parser.
5. The Track 2/oracle coupling fixture fails.
6. The admission or generated-baseline claim fixture fails.
7. No accepted W1a fixture can carry `outcome_id=A`, `verdict=GO`,
   `baseline_authority=true`, `wave_id=SK-V11-W1b`, or SK-V11 close language.
8. `gate-json --with-cost-facts --check-results` remains green.
9. `skinny/RESULTS.md` has no diff.
10. No source outside the W1a owner set is edited, especially `skinny/xtask`,
    `skinny/RESULTS.md`, codegen, runtime, parser, ASM, or grammar behavior
    paths.
11. Any REDRESS/status entry for W1a is written only after the required commands
    have run and the entry names the actual command outcome. A prose prediction
    is not a REDRESS record.

## Reject Conditions

Reject W1a if any of the following happen:

- The implementation emits a non-JSON report but no same-wave command consumes
  it.
- The CLI prints a pass while ignoring fields later used as evidence.
- Unknown fields are accepted in the companion report.
- Missing grammar id, domain, output plane, oracle source, run id, host, feature
  mask, consumer class, or Track 2/oracle independence passes.
- A non-JSON fixture uses `gate_only` to imply row movement or close.
- Producer-only telemetry, PMU/cycles/profile/probe data, sidecars, or stale
  comparators become evidence.
- JSON `gate-json` is weakened to admit non-JSON rows.
- `skinny/RESULTS.md` changes.
- A generated non-JSON baseline, W2 admission, or SK-V11 close is claimed.
- Any non-owner path is needed to make W1a pass.

## Command Evidence Required Before Close

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` after implementation:

```sh
cargo test -p bbnf-bench report::tests::w1a -- --nocapture
cargo test -p bbnf-bench --bin gate w1a -- --nocapture
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
if cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-producer-only-extra-field.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-track2-coupled.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-admission-claim.json; then exit 1; fi
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git -C .. diff --exit-code -- skinny/RESULTS.md
```

If fixture names change, substitute exact equivalents, but preserve one passing
non-admitting fixture and failing producer-only, Track 2/oracle coupling, and
admission/baseline fixtures.

## Challenge Result

The W1a plan can proceed as a non-admitting gate/report schema lane. It does
not falsely close as prose-only evidence as long as close is conditioned on the
same-wave CLI and fixture outcomes above. The companion report may establish
that W1b has a gate shape to consume; it may not establish a non-JSON baseline,
row admission, Lock 14 generality, or SK-V11 close.

DISPOSITION: ACCEPT

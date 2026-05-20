# SK-V11 W1a CH2 Challenge: Generality And Lock 14

Scope: W1a mandatory CHALLENGE, CH2 generality / Lock 14 lens.

Read set:

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4.
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` W1a and CHALLENGE references.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R1-gate-validator.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R2-report-metadata.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R3-fixtures-tests.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R5-telemetry-contract.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R6-redress-boundaries.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md`.

## CH2 Question

Does W1a establish a grammar-neutral non-JSON evidence lane without weakening
the existing JSON W0 validator, adding a hidden directive/BIR/substrate route,
or claiming Lock 14 from prose/schema presence alone?

## Finding

Accept the W1a shape as a CH2 entry challenge for the planned implementation.
The plan chooses the correct boundary: a companion non-JSON evidence report and
direct `bbnf-bench --bin gate` consumer, not a relaxation of JSON
`gate-json`, not a `RESULTS.md` row movement, and not a generated non-JSON
baseline.

The Lock 14 claim is limited to gate-consumable schema generality. That is
valid for W1a because Section 4 only asks the wave to make non-JSON evidence
consumable and reject bad evidence. It does not allow W1a to prove a generated
non-JSON parser, admit a direct/typed row, or close grammar generalization.
Those remain W1b/W2 obligations.

## Generality Checks

The planned grammar identifiers are bounded and non-JSON:

- `css_l4`
- `sheets`
- `bbnf_self`

The challenge accepts this allowlist because it is explicit, gate-consumed, and
paired with exact domains: `css_l4_bench`, `sheets_bench`, and
`bbnf_self_bench`. The gate must reject spelling drift such as `google_sheets`
unless the same wave updates the SPEC and fixtures. Row identity must stay
`<grammar_id>/<corpus>/<workload>/main`, with grammar/domain/row-id agreement
checked by the validator.

The plan correctly avoids using JSON-provider emission, renamed JSON helpers,
or old hand-written non-JSON runtimes as generality evidence. Those are
pre-blocked Lock 14 routes and would make W1a a REVISE.

## JSON Policy Preservation

W1a must not broaden these JSON W0 checks in place:

- `Report::validate_schema_v3()`.
- `Report::validate_sk_v8_w0()`.
- JSON row identity, comparator, profile, manifest, and run-id invariants.
- `gate-json --with-cost-facts --check-results`.

The companion-report plan is acceptable because it adds a sibling validation
path for non-JSON evidence rather than making JSON validators tolerate
non-JSON rows. JSON preservation still needs closure evidence from the later
implementation: existing JSON tests green, the JSON gate command green, and no
`skinny/RESULTS.md` diff.

## Direct CLI Consumer

A direct CLI consumer in `skinny/crates/bbnf-bench/src/bin/gate.rs` is enough
for W1a. `skinny/xtask` is outside the planned owner set, and Section 4 does
not require a new xtask wrapper. The sufficient consumer shape is:

```text
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report <path>
```

This command is a valid same-wave consumer only if it parses the strict
companion report, rejects unknown keys, validates every required field, and is
exercised against both passing and failing W1a fixtures. The existing xtask
`gate-json --with-cost-facts --check-results` command remains the JSON
preservation proof, not the non-JSON consumer.

## Lock 14 Failure Conditions

W1a must return to CHALLENGE as REVISE if the implementation does any of these:

- Relaxes the JSON W0 validator to pass non-JSON evidence.
- Edits `skinny/xtask`, `skinny/RESULTS.md`, generated output, codegen,
  runtime, parser, ASM, or grammar behavior for W1a closure.
- Adds a directive, BIR variant, `BackendShape`, public substrate API, hidden
  sidecar, or hidden schema fact.
- Accepts a non-JSON report with missing grammar, domain, output plane,
  oracle/comparator source, Track 2/oracle independence, run id, host, feature
  mask, sample context, or same-wave consumer class.
- Accepts producer-only telemetry, including unknown fields, PMU/cycles facts,
  profile facts, or diagnostic fields not consumed by the gate.
- Claims generated non-JSON baseline authority, direct/typed admission,
  parse-only SOTA, or SK-V11 close from W1a schema evidence.
- Treats JSON-provider output, a renamed JSON helper, or a hand-only non-JSON
  runtime as Lock 14 proof.

## Required Closure Evidence

For W1a closure, the implementation must show:

- A strict companion non-JSON report schema and validator in the owner gate/report
  surface.
- Pass and fail fixtures under the W1a research fixture path, including
  producer-only telemetry, Track 2/oracle coupling, and baseline/admission claim
  rejection.
- Direct CLI pass/fail commands consuming those fixtures.
- JSON `gate-json --with-cost-facts --check-results` still green.
- No `skinny/RESULTS.md` movement.
- No generated non-JSON baseline authority.

DISPOSITION: ACCEPT

Required changes: none for CH2 entry acceptance. W1a closure remains contingent
on landing the companion report, direct CLI consumer, and fail-closed fixture
evidence exactly within the W1a owner boundary.

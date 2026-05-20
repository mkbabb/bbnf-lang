# SK-V11 W1a CH3: REDRESS Regression / Preblock Challenge

Scope: W1a mandatory CHALLENGE, CH3 lens only. This review evaluates the Phase
2 plan for `G-W1a-NONJSON-GATE` after S-P3 V4 convergence.

Read set:

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4.
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` W1a and CHALLENGE rules.
- `skinny/REDRESS.md` recent entries and W1a-relevant preblocks.
- `restart/skinny/tranches/sk-v11/research/w1a/` research and plan artifacts.

## Finding

The W1a plan does not reopen a REDRESS-preblocked route as written. It selects a
companion non-JSON gate evidence lane in `bbnf-bench` report/gate code and W1a
fixtures only. That shape matches SPEC Section 4: consume non-JSON benchmark
evidence schema, keep JSON `gate-json` strict, add pass/fail fixtures, move no
parser rows, and claim no generated non-JSON baseline authority.

The plan's explicit owner slice is acceptable for CH3:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/`

The explicit non-owner exclusions are load-bearing:

- no `skinny/xtask` edits;
- no `skinny/RESULTS.md` edits;
- no codegen, runtime, parser, asm, grammar behavior, or generated output;
- no W3 union/event/class-column, streaming cursor, sidecar, structural vector,
  retained class lane, or cascade-lock route.

## Regression Checks

| Risk | CH3 result |
|---|---|
| W0 JSON validator relaxation | Acceptable only because the plan adds separate `NonJsonEvidenceReport` validation and leaves current JSON schema-v3/W0 validators and `gate-json --with-cost-facts --check-results` as preservation gates. |
| `RESULTS.md` row movement | Acceptable. The plan makes `git diff --exit-code -- skinny/RESULTS.md` an exit condition and assigns no non-JSON row to the main table. |
| Behavior or generated baseline leakage | Acceptable. W1a is schema-only; W1b owns first generated non-JSON baseline authority and W2 owns first admitted non-JSON intervention. |
| REDRESS 96/97 union route reopening | Acceptable. The plan does not touch runtime tape, structural cursors, class columns, sidecars, codegen templates, or parser control flow. |
| REDRESS 100/101/109 row-contract misuse | Acceptable. The plan may model future direct/typed fields but rejects `A / GO`, `SK-V11-W1b`, generated baseline flags, and row admission in W1a. |
| REDRESS 87 producer-only telemetry | Acceptable if implemented with `serde(deny_unknown_fields)` or equivalent strict key checking and a failing producer-only fixture. |
| REDRESS 34/35/48 Track 2 honesty | Acceptable if the coupled Track 2/oracle fixture fails and the validator consumes oracle source, plane, freshness, value, and independence status. |
| REDRESS 36-38/85/86 Lock 14 laundering | Acceptable because the plan requires canonical non-JSON grammar/domain identities and does not use JSON-provider emission, hand-only runtime proof, or generic JSON policy as evidence. |

## Required Redress Guardrails

Redress must fail or return to plan if any of these occur:

- JSON `gate-json --with-cost-facts --check-results` regresses or accepts a
  weaker W0 shape.
- `skinny/RESULTS.md` changes.
- `skinny/xtask`, codegen, runtime, parser, asm, grammar behavior, generated
  parser output, or generated runtime output changes.
- The W1a gate accepts missing required non-JSON fields.
- The W1a gate accepts producer-only extra fields.
- The W1a gate accepts Track 2/oracle coupling.
- The W1a gate accepts row admission, generated baseline authority, parse-only
  SOTA, direct-as-typed proof, stale sidecar proof, or documentation-only Lock
  14 proof.

The named failing fixtures are sufficient for CH3 if they falsify the core
regression routes:

- `nonjson-producer-only-extra-field.json` must fail.
- `nonjson-track2-coupled.json` must fail.
- `nonjson-admission-claim.json` must fail.

Additional unit tests may cover identity, domain, run id, host, feature mask,
output plane, and oracle-source omissions, but those tests must not be used to
relax the three fixture-backed CH3 falsifiers above.

DISPOSITION: ACCEPT

# SK-V11 W8 R5 - Non-JSON Axis Carry-Forward

Date: 2026-05-20.
Wave: W8 - Direct Residual Fixpoint And Row Reclamation.
Scope: read-only research for non-JSON axis carry-forward and grammar-generalization fixpoint.
Source edits: none.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 1, 4, 5, 6, 12, and 13.
- `restart/skinny/tranches/sk-v11/HANDOFF.md`.
- W1a artifacts under `restart/skinny/tranches/sk-v11/research/w1a/`.
- W1b artifacts under `restart/skinny/tranches/sk-v11/research/w1b/`.
- W2 entry artifact at `restart/skinny/tranches/sk-v11/research/w2/entry/w2-entry-blocked.md`.
- `skinny/REDRESS.md` items 111, 112, 113, and 118.

## Binding State

W1a closed only the gate/report schema lane. REDRESS 111 admits
`G-W1a-NONJSON-GATE` because the companion non-JSON report is consumed by
`bbnf-bench --bin gate -- --w1a-non-json-report`, producer-only and coupled
Track 2 fixtures fail, JSON `gate-json --with-cost-facts --check-results`
stays green, `skinny/RESULTS.md` does not move, and no generated non-JSON
baseline authority is claimed.

W1b did not close the generated non-JSON baseline lane. REDRESS 112 rejects
`G-W1b-NONJSON-BASELINE`: the selected
`css_l4/declaration_values/direct/main` target has no generated CSS L4 Track 1
inside W1b owner paths, because codegen still routes direct and typed emission
through `json_provider::ensure_runtime_profile`, runtime emission accepts only
the JSON grammar profile, and `skinny/crates/runtime/src/grammars/` contains no
generated CSS L4 runtime.

W2 did not admit a generated CSS L4 direct/typed intervention. REDRESS 113 and
`w2-entry-blocked.md` record W2 as `BLOCKED` before source dispatch because W2
requires W1b closed, must consume the W1b baseline, may not create the first
measurable non-JSON row, and has no defined
`ceil(W1b_css_baseline_mbps * 1.01)` threshold.

W7 adds no non-JSON recovery route. REDRESS 118 records that W7 admitted no
output digest/hash host-sink optimization, no non-JSON host-sink baseline, no
direct-row movement, and no reusable scalar oracle. HANDOFF therefore routes W8
only through SPEC Section 12 direct residual fixpoint and row reclamation with
W2-W7 dispositions carried forward.

## What W8 Can Close

W8 can close direct residual accounting. Under SPEC Section 12, W8 may
re-evaluate every remaining Section 0.4 direct residual row, admit only rows
that meet the strict generated Track 1 plus independent Track 2/oracle floors,
and record REDRESS uncloseable proofs for misses. It may also carry W2's
non-JSON `BLOCKED` state into W9 as an explicit close input.

W8 can close the non-JSON axis only in the narrow accounting sense that the
axis has a named, cited, non-paper disposition for SK-V11: W1a admitted schema
consumption, W1b rejected generated baseline authority, W2 was blocked before
intervention dispatch, and no later W3-W8 owner surface created a legal
replacement baseline/intervention route. That is a grammar-generalization
fixpoint record, not a grammar-generalization success.

W8 can write close wording that says the SK-V11 non-JSON generated-intervention
axis is unresolved and carried forward because the prerequisite generated
non-JSON Track 1 baseline never existed in SK-V11. This satisfies SPEC Section
12's "W2 admitted the non-JSON axis or recorded a BLOCKED route" entry
condition and prepares SPEC Section 13's allowed W9 escalation path.

## What W8 Cannot Close

W8 cannot admit the non-JSON grammar-generalization axis. SPEC Sections 5 and 6
require W1b to create exactly one generated non-JSON baseline and W2 to consume
that baseline for an intervention; both gates failed or blocked. W8 has no
authority to backfill W1b, invent `W1b_css_baseline_mbps`, create the first
measurable non-JSON row, or treat a gate/report fixture as generated parser
evidence.

W8 cannot relabel W1a evidence as behavior. W1a fixtures and the
`sk-v11-w1a-nonjson-v1` report schema prove fail-closed consumption of
non-JSON identifiers, oracle/source fields, and producer-only rejection. They
do not prove generated CSS L4, Sheets, or BBNF-self parser throughput, strict
output equality from generated Track 1, baseline authority, or an admitted
intervention.

W8 cannot claim Lock 14 by prose, JSON-provider emission, old hand non-JSON
runtimes, runtime witness modules, direct digest evidence, stale sidecars, or
future-phase promises. SPEC Section 1 requires same-wave CSS L4, Sheets, or
BBNF-self proof for generic/codegen/runtime-outside-JSON edits, and HANDOFF
names the live `json_provider` codegen path as a Lock 14 gate before any
generated-parser proof can admit.

W8 cannot use REDRESS 118 as a non-JSON substitute. W7 found no generated
non-JSON host-sink baseline inside its owner paths and moved no row. The only
legal W8 posture after W7 is direct residual fixpoint with the non-JSON block
preserved.

## Carry To W9 And Pass Alpha

Receiver: W9 close accounting.
Blocker: SK-V11 has no admitted generated non-JSON direct/typed parser
intervention because W1b rejected the baseline and W2 blocked before
intervention dispatch.
Receiving gate: `G-W9-CLOSE-SK-V11` may close only by escalating `BLOCKED` for
grammar-generalization fixpoint, not by claiming the non-JSON intervention
axis admitted.

Receiver: Pass Alpha / next tranche contract.
Blocker: the codegen/runtime surface is still JSON-profiled for runtime
emission, and the selected CSS L4 baseline has no generated Track 1. The next
contract must create an explicit generated non-JSON baseline wave with owner
authority before any generated non-JSON intervention wave.
Receiving gate: a future W1b-equivalent must produce exactly one generated
non-JSON Track 1 baseline, an independent same-plane Track 2/oracle, strict
output equality, gate-consumed provenance, and no JSON policy outside generated
per-grammar modules.

Receiver: Pass Alpha / W2-equivalent intervention.
Blocker: no `W1b_css_baseline_mbps` exists, so no `ceil(baseline * 1.01)`
threshold can be computed for CSS L4 intervention admission.
Receiving gate: an intervention wave may dispatch only after the baseline wave
lands and must consume the baseline rather than create it.

## Wording That Avoids Paper Close

Use:

- "W8 carries REDRESS 113's non-JSON `BLOCKED` route into W9."
- "SK-V11 records a grammar-generalization fixpoint, not a non-JSON
  intervention admission."
- "W1a admitted schema consumption only; W1b rejected generated baseline
  authority; W2 was blocked before source dispatch."
- "W9 may close only by escalating `BLOCKED` for grammar-generalization
  fixpoint unless a later Alpha/Pass Alpha contract supersedes SK-V11 with an
  explicit generated non-JSON baseline wave."

Do not use:

- "W8 closes non-JSON."
- "W1a proves non-JSON generality."
- "The CSS L4 route is satisfied by gate/report evidence."
- "W2 is skipped because W8 fixpoint covers the axis."
- "Future work will add the generated baseline" as a close condition.

## R5 Conclusion

The W8 closeable state is direct-plane fixpoint plus explicit carry-forward of
the non-JSON block. The non-JSON axis cannot be marked admitted in SK-V11. W9
must either state `BLOCKED` for grammar-generalization fixpoint in close docs
or defer the generated non-JSON baseline and intervention sequence to
Pass Alpha / the next tranche with concrete owner paths and gates. Any wording
that presents W1a schema consumption, W2 entry block, W7 host-sink block, or
W8 direct residual accounting as a non-JSON intervention close is paper close
and should be rejected.

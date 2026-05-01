# AZ-IV Loss-Prevention Synthesis - 2026-05-01

Six read-only agents re-audited the AZ-IV plan after the first hardening pass.
Their mandate was narrower than the original hardening cohort: prove BA/BB
requirements were not being dropped, enumerate chronic deferrals from the
recent tranche sequence, and tighten AZ-IV so performance, semantic parity,
substrate wiring, and legacy deletion cannot slip into another successor plan.

## Agents

| Agent | Angle | Disposition |
|---|---|---|
| Aquinas | BA/BB lossless coverage | accepted; AZ-IV now absorbs BA/BB functional requirements explicitly |
| Lagrange | last-10-tranche chronic deferrals | accepted; carry ledger and parking lot expanded |
| Ohm | type-inference semantic parity | accepted; W1 gates now reject normalizer-only and rule-name proof |
| Averroes | full substrate denominator | accepted; W2 now requires every active substrate fact to be consumed or deleted |
| Banach | post-AU/post-AZ/sonic-rs performance | accepted; W3 numeric floors and profile requirements strengthened |
| James | legacy and deletion ledger | accepted; W2 and global hard gates now name concrete legacy surfaces |

## Loss Risks Found

1. BA path/query requirements were only partially represented. The plan named
   the existing `runtime::path` surface, but not the full product semantics:
   compile-time/type-inference checking, grammar-aware diagnostics,
   zero-allocation traversal, host-binding isomorphism, and competitor path
   extraction benchmarks.
2. BB rewrite/ruler requirements were too weak. The earlier W2 text could have
   closed on a one-rule smoke. It now requires CVC enumeration, egraph residue,
   VM oracle, ranker/tiering, schema/provenance, grammar rewrite dirs,
   review/CI ledgers, generated-code effects, and parity/bench proof.
3. Semantic parity was at risk of remaining normalizer-mediated. AZ-IV now
   requires `TypeDesc`, `StructRegistry`, obligations, grammar facts, and
   generated projection tables to own payload, field, and discriminant choices.
4. Substrate activation lacked a denominator. W2 now must enumerate every
   mined fact, sidecar, rule, template, shape, scan, Pratt, view, regex, CSP,
   and egraph decision, then prove generated/runtime consumption or deletion.
5. Performance could have closed on parse-only rows. W3 now requires direct
   StructDirect document/value/path projection rows, same-harness sonic-rs
   comparisons, row-by-row post-AU and post-AZ floors, active watchdog rows,
   and profile artefacts for target misses.
6. Legacy cleanup was not concrete enough. The hard gates now name
   `emit_dfa_inline_body`, DTA walker/tape wording, old color compatibility,
   fallback-to-JSON substrate path, discarded Rust per-rule compile work,
   derive/bootstrap residue, duplicated host shims, stale package locks, and
   sidecar authority.

## Accepted BA Coverage

AZ-IV does not adopt BA's stale file layout. It preserves BA's requirements by
landing them inside the existing runtime/document/type-inference route.

| BA requirement | AZ-IV owner | Required proof |
|---|---|---|
| `Path`, `PathSegment`, typed path construction | W1 - Runtime Surface And Semantic Parity | existing `runtime::path` and `path!` surface is type-inference checked |
| compile-time grammar-aware path diagnostics | W1 - Runtime Surface And Semantic Parity | invalid path tests name segment, struct, and alternatives |
| lazy traversal without allocation | W1 - Runtime Surface And Semantic Parity | traversal allocation proof and path bench artefact |
| parent/ascent strategy investigation | W1 - Runtime Surface And Semantic Parity | either absorbed into zero-allocation traversal or retired with evidence that no sidecar is needed |
| sonic-rs/simdjson path extraction comparisons | W1 and W3 | 3-field JSON path access at least 20% over sonic-rs pointer; 30-field access parity-or-better |
| TS/Python/host binding isomorphism | W1 - Runtime Surface And Semantic Parity | executable TS proof and host signature/error ledger |
| no parallel path crate | all waves | no new path crate, resolver fallback, or parent-pointer sidecar |

## Accepted BB Coverage

AZ-IV does not adopt BB's stale derive-scanned rewrite route. It preserves BB's
rewrite semantics through the current IR/egraph/xtask pipeline.

| BB requirement | AZ-IV owner | Required proof |
|---|---|---|
| Ruler CVC enumeration | W2 - Optimization Substrate Activation | enumerator output with declared size bounds |
| egraph residue split | W2 - Optimization Substrate Activation | residue/oracle count in rewrite-chain artefact |
| VM oracle wrapper | W2 - Optimization Substrate Activation | oracle checks every egraph-silent candidate class |
| ranker and tiering | W2 - Optimization Substrate Activation | accepted/rejected rule tiers with retained-rule hit-rate |
| RON schema and provenance | W2 - Optimization Substrate Activation | loaded rules carry source, owner, proof, and target grammar |
| grammar-colocated rewrite dirs | W2 - Optimization Substrate Activation | every production grammar has a rewrite-dir decision |
| generated-code effects | W2 - Optimization Substrate Activation | generated Rust diff and parity/bench proof per accepted rule class |
| CI auto-accept and review ledger | W2/W3 | auto-accept/review status is wired or explicitly retired with reason |

## Chronic Deferrals Folded Into AZ-IV

| Deferral | AZ-IV owner | Closure mode |
|---|---|---|
| strict regen drift | W0 - Truth And Canonical Regen | live `cargo xtask regen --check` and tempdir parity |
| egraph `Map` extraction stripping | W0/W2 | named preservation test plus rewrite extraction proof |
| Sheets parity gap | W1 - Runtime Surface And Semantic Parity | full current suite from regenerated output |
| CSS Tailwind timeout and named-color payloads | W1/W2 | typed payload parity plus emitted scanner authority |
| TS backend string-only proof | W1 - Runtime Surface And Semantic Parity | syntax/typecheck, Node execution, EOF rejection |
| post-AU/post-AZ benchmark staleness | W3 - Measurement And Close | row-by-row floor table and no watchdog close |
| direct struct projection performance | W1/W3 | `bbnf_value_*` and path rows against sonic-rs peers |
| rewrite/ruler substrate without production consumer | W2 - Optimization Substrate Activation | full production chain or deletion |
| CSP/shape/SIMD/Pratt/view under-consumption | W2 - Optimization Substrate Activation | denominator ledger plus generated/runtime proof |
| DTA/dfa/tape/bootstrap/derive residue | W0/W2 | deletion, rename to real fact role, or compatibility archive |
| sibling topology drift | W0 - Truth And Canonical Regen | metadata/lock/docs/source-of-truth gates |

## Plan Edits Made

1. `AZ-IV.md` now states losslessness with respect to BA/BB, adds BA/BB carry
   rows, adds type-inference parity and direct projection performance
   invariants, and strengthens the global hard gates.
2. `GESTALT.md` now defines the BA/BB folding rule: keep semantics, reject
   stale mechanisms, and close only when coverage is lossless.
3. `PROGRESS.md` now records this audit and lists BA typed path/query, BB full
   rewrite/ruler program, post-AU/post-AZ/sonic-rs floors, and full substrate
   denominator in the close-honesty parking lot.
4. `waves/W0.md` now requires `W0-ba-bb-coverage.md` before implementation
   waves open.
5. `waves/W1.md` now owns type-inference semantic parity, BA path/query
   semantics, host-binding status, CSS/Sheets escape removal, TS execution,
   and shape generality.
6. `waves/W2.md` now owns BB's full rewrite/ruler program, CSP/regex
   consumer authority, shape/SIMD/structural-scan consumption, DTA/dfa cleanup,
   Pratt/view generality, and the full substrate denominator ledger.
7. `waves/W3.md` now owns same-harness sonic-rs projection parity,
   post-AU/post-AZ floors, watchdog-row restoration, IAI, profile artefacts,
   and final BA/BB lossless close proof.

## Non-Negotiable Consequence

AZ-IV cannot close by routing BA/BB, sonic-rs parity, rewrite/ruler activation,
or legacy deletion to a vague successor. Any residual must be named, evidenced,
and incompatible with the AZ-IV thesis; otherwise it remains an AZ-IV blocker.

# CH6 ANTI-PAPER-CLOSE - SK-V15 S-P3 V1

Verdict: REVISE

Scope: commit `4fe37c042` packet: `SPEC.md`, `DISPATCH-PROMPT.md`, and
`research/p3/p3a` through `p3f`, checked against
`PASS-3-SYNTHESIS-PLAN.md` CH6, `ORCHESTRATOR.md` 3W/3Z,
`SKINNY-TRIUMVIRATE.md`, and the SK-V15 SPEC.

CH6 test: every wave must close on measurement or executable gate proof, every
wave must carry a revert protocol, no wave may close on a future-phase promise,
and every primitive/provider/generated path/gate report must name a same-wave
consumer.

## Findings

| ID | Disposition | Evidence | Finding | Required fold |
|---|---|---|---|---|
| CH6-01 | REVISE | `p3c-falsifiability-gates.md:12` binds the pre-sequencing wave set and says P3-C must be revised if P3-B changes it. `p3b-wave-sequencing.md:17-21` adopts W0-W9 and splits REBUILD-F across W6/W7. `p3f-spec-draft.md:17-18` only maps the stale P3-C receivers in prose. | The final SPEC has W0-W9 gates, but the P3-C falsifiability artifact is not itself folded to the final wave graph. CH6 cannot accept a gate packet whose gate file says it must be revised before dispatch. | Revise P3-C or add a folded gate appendix that enumerates W0-W9 after P3-B sequencing. Split W6 spine and W7 lowerers explicitly, move FNV to W8, add W9 close reconciliation, and keep named rows/proofs, same-wave consumers, and revert protocols in that folded gate source. |
| CH6-02 | REVISE | `SPEC.md:73-77` allows close only with PASS-IMPL V2 acceptance or row-level intrinsic-block proof. `SPEC.md:371-385` makes W9 prepare SK-V16 input and says PASS-IMPL V2 may "route SK-V16 prune inputs." `DISPATCH-PROMPT.md:170-171` says W9 closes when dependency rows are admitted, redressed, or intrinsically blocked. | W9 is mostly evidence-bound, but the "routes SK-V16 prune inputs" phrase can be read as a future-phase close. CH6 forbids closing a wave because a later phase will handle the miss. | State that SK-V16 routing is routed remainder after proof, not close evidence. W9 may close only when PASS-IMPL V2 accepts the axis or records row-level intrinsic-block evidence with HEAD command output, generated diffs/manifests, strict parity/checkasm where relevant, and cold measurements where behavior changed. |
| CH6-03 | REVISE | `p3c-falsifiability-gates.md:172` names compile/lower/regenerate commands as the same-wave consumer for the combined Decision Engine wave. After the split, `SPEC.md:307-346` describes W6/W7 exits but does not name exact split-wave consumers or command-shaped gates. `DISPATCH-PROMPT.md:173-178` requires same-wave consumption for generated paths and gate reports. | W6 and W7 are not paper-close by intent, but their final SPEC language is weaker than P3-C after the split. "Can change generated behavior or selection" needs an executable consumer, not a self-report. | In W6, name the decision gate/test command or generated-selection fixture that consumes e-graph/CSP output and fails against the old scaffold. In W7, name the regenerate/test fixtures that consume each lowerer: EagerTape, OffsetTape, EventTape, SinkOnly, and CollapsedStage, or a gate-consumed rejected alternative for each. |

## Wave Audit

| Wave | Closure proof | Revert protocol | Same-wave consumer | CH6 result |
|---|---|---|---|---|
| W0 | Gate-consumed telemetry plus JSON baseline and CSS diagnostic broadcast checks (`SPEC.md:181-187`). | Present (`SPEC.md:189-190`). | Row gate consumes SK-V15 telemetry (`p3c-falsifiability-gates.md:47-50`). | ACCEPT |
| W1 | Gate rejects broadcast CSS admits; JSON guard stays inside W0 budget (`SPEC.md:205-210`). | Present (`SPEC.md:212-213`). | CSS report/gate renderer consumes `broadcast_group_id` (`p3c-falsifiability-gates.md:68-71`). | ACCEPT |
| W2 | Lock 14/16 scan roots, primitive status, parity/checkasm, and exclusion reports are gate proof (`SPEC.md:228-233`). | Present (`SPEC.md:235-236`). | Gate consumes exclusion report (`SPEC.md:221`, `p3c-falsifiability-gates.md:88-91`). | ACCEPT |
| W3 | Leak grep, generated-output proof, and JSON 51/51 rerun if JSON-adjacent (`SPEC.md:250-255`). | Present (`SPEC.md:257-258`). | Same-wave regen/check command (`SPEC.md:246`, `p3c-falsifiability-gates.md:108-110`). | ACCEPT |
| W4 | 67-file count, line-1 provenance scan, and regen/check or intrinsic block (`SPEC.md:271-276`). | Present (`SPEC.md:278-279`). | Pattern H gate consumes provenance (`p3c-falsifiability-gates.md:127-129`). | ACCEPT |
| W5 | Typed CSS output, same-workload cssparser retiming, distinct measurements, and JSON maintain (`SPEC.md:294-301`). | Present (`SPEC.md:303-305`). | CSS typed parser/value API and bench row consume any new primitive (`p3c-falsifiability-gates.md:148-152`). | ACCEPT |
| W6 | E-graph/CSP gates are executable in intent (`SPEC.md:318-323`) but need the split-wave consumer named. | Present (`SPEC.md:324-325`). | Too generic after P3-C split. | REVISE |
| W7 | Lowerer tests, generated diffs, and JSON maintain are evidence-bound (`SPEC.md:338-343`) but need per-lowerer consumers named. | Present (`SPEC.md:345-346`). | Too generic after P3-C split. | REVISE |
| W8 | Strict-product gate consumes quarantine metadata and negative fixtures (`SPEC.md:359-364`). | Present (`SPEC.md:366-367`). | Strict-product gate consumes quarantine metadata and adversarial tests (`p3c-falsifiability-gates.md:194-197`). | ACCEPT |
| W9 | PASS-IMPL V2 plus dependency-row proof is the right gate (`SPEC.md:380-385`), but SK-V16 routing language needs deferral guardrails. | Present (`SPEC.md:387-388`). | PASS-IMPL V2 consumes close packet and dependency rows (`DISPATCH-PROMPT.md:163-171`). | REVISE |

## Required Folds

1. Fold P3-C forward to the final W0-W9 wave graph. Remove the stale
   "P3-B does not exist" caveat or satisfy its own revision requirement before
   dispatch.
2. Amend W9 so SK-V16 routing is explicitly routed remainder after an accepted
   or intrinsically blocked PASS-IMPL V2 axis, never the proof that closes
   SK-V15.
3. Strengthen W6 and W7 in SPEC/DISPATCH with exact same-wave executable
   consumers. W6 needs the gate/test/fixture that consumes e-graph and CSP
   output. W7 needs the regenerate/test fixture for each BackendShape lowerer
   or a gate-consumed rejected alternative.
4. Preserve the existing good CH6 constraints: W0-W5 and W8 already close on
   measurement or executable gate proof; all W0-W9 waves have revert protocols;
   provider deletion remains blocked until same-wave or prior-wave replacement
   proof.

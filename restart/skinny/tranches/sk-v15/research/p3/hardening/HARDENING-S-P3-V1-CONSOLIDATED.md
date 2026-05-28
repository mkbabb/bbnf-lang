# SK-V15 S-P3 V1 Hardening Consolidated

Cycle: S-P3 Synthesis-Plan V1.
Date: 2026-05-28.
Input commit: `4fe37c042`.
Hardening root: `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/`.

## Verdict

ACCEPT-RATE: 0 / 7 = 0.0%.

Cycle verdict: REVISE.

V1 has the right prune-before-rebuild posture and preserves the S-P2 survivor
boundary, but it is not dispatchable. The hardening lenses found one dominant
fold class: the final packet must reindex from W0-W9 to W0-W11, split old W5
and old W7, make P3-C the canonical gate source, and promote telemetry,
dependency, cost, non-JSON receiver, gate-exclusion, and EventTape anti-sidecar
tables into SPEC and DISPATCH. Leaving these rules only in P3-B/P3-D/P3-E or
prose is paper-hardening.

## Lens Dispositions

| Lens | Disposition | Output | Required fold |
|---|---|---|---|
| CH1 CORRECTNESS | REVISE | `V1/CH1.md` | Rewrite P3-C to final topology, add per-wave `SK-V15-open` row gates, remove W8R numbers from CSS typed floors, and add per-candidate threshold rebinding. |
| CH2 GENERALITY | REVISE | `V1/CH2.md` | Bind generic-crate edits to non-JSON receivers, reject JSON-only proof, normalize telemetry fields, and require consumed exclusion reports. |
| CH3 REGRESSION | REVISE | `V1/CH3.md` | Promote NEW-CH3-V5-01 dependency table into SPEC/DISPATCH and normalize REDRESS pre-block clusters across all S-P3 surfaces. |
| CH4 COST | REVISE | `V1/CH4.md` | Add LOC/risk/cost columns; split old W5 and old W7 to remain cap-valid under the 30-minute redress cap. |
| CH5 HIDDEN COUPLING | REVISE | `V1/CH5.md` | Fold full forbidden sidecar/substrate vocabulary into executable gates and counter-bind EventTape as an existing BackendShape lowerer only. |
| CH6 ANTI-PAPER-CLOSE | REVISE | `V1/CH6.md` | Name exact executable consumers for Decision Engine and lowerer splits; make SK-V16 routing routed remainder, never close evidence. |
| CH7 OVERFIT-PRUNE | REVISE | `V1/CH7.md` | Reject telemetry aliases, make W8R CSS metrics diagnostic-only, promote gate-exclusion tables, and preserve the S-P2 rejected-route set. |

## Deduplicated V2 Fold Roster

| id | required fold | target files |
|---|---|---|
| S-P3-V2-F01 | Reindex final wave graph to W0-W11: W5 provider, W6 CSS retime/retire, W7 Decision spine, W8 lowerer harness/Eager/Offset, W9 Event/Sink/Collapsed/all-five, W10 FNV, W11 close. | `p3b-wave-sequencing.md`, `p3c-falsifiability-gates.md`, `p3f-spec-draft.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F02 | Rewrite P3-C as the canonical W0-W11 gate source; remove the stale "P3-B does not exist" caveat and stop relying on P3-F prose remapping. | `p3c-falsifiability-gates.md`, `p3f-spec-draft.md` |
| S-P3-V2-F03 | Add measured gate tables: row universe, gate class, target rows/scans, guard rows, threshold formula, CSS treatment, canonical telemetry fields, same-wave consumer, proof command, and REDRESS/revert action. | `p3c-falsifiability-gates.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F04 | Normalize telemetry to the ten canonical fields and reject alias-only telemetry unless a schema bump maps aliases and the gate consumes that mapping. | `p3b-wave-sequencing.md`, `p3d-telemetry-schema.md` if needed, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F05 | Remove W8R CSS metrics from live typed-admission floors; keep `2319.041`, `2362.037`, and `929.281` only as diagnostic negative fixtures until fresh typed-output cssparser rows exist. | `p3a-candidate-shortlist.md`, `p3c-falsifiability-gates.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F06 | Add candidate rebinding table for P3-A candidates 1-8 using `SK-V15-open` formulas, same-wave consumers, scalar/oracle/parity requirements, and reject/demotion actions. | `p3c-falsifiability-gates.md` |
| S-P3-V2-F07 | Promote NEW-CH3-V5-01 dependency table schema and initial rows into final dispatch surfaces; every delete/retire/demotion action must have a matching row. | `p3b-wave-sequencing.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F08 | Normalize pre-block clusters everywhere: REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration. | `p3a-candidate-shortlist.md`, `p3b-wave-sequencing.md`, `p3c-falsifiability-gates.md`, `p3e-preblocked-ledger.md`, `p3f-spec-draft.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F09 | Add cost/risk/LOC table columns: risk class, manual source/test LOC budget, generated LOC status, docs/ledger LOC budget, phase caps, split trigger, and same-wave consumer. | `p3b-wave-sequencing.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F10 | Name exact same-wave executable consumers for W7 Decision spine and W8/W9 lowerers, including tests/gates that fail against the old scaffold. | `p3c-falsifiability-gates.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F11 | Add W11 anti-deferral wording: SK-V16 routing is routed remainder only after PASS-IMPL V2 acceptance or row-level intrinsic-block proof, not close evidence. | `p3c-falsifiability-gates.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F12 | Fold the full CH5 forbidden set into executable gates and state EventTape is an existing BackendShape lowering only, never a sidecar vector, sixth shape, public substrate API, or alternate document projection. | `p3c-falsifiability-gates.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |
| S-P3-V2-F13 | Promote Lock 14/16 exclusion-report schema into SPEC and DISPATCH: included roots, excluded roots, reasons, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition. | `SPEC.md`, `DISPATCH-PROMPT.md`, `p3c-falsifiability-gates.md` |
| S-P3-V2-F14 | Add non-JSON proof receiver table for generic surfaces: codegen provider/runtime generator, lowerers, e-graph, CSP, cost facts, xtask regen, and gate/report code. | `p3c-falsifiability-gates.md`, `SPEC.md`, `DISPATCH-PROMPT.md` |

## Redeploy Notes

The redeployed support notes are advisory inputs for the V2 fold and are
retained under `V1/redeploy/`:

| note | purpose |
|---|---|
| `WAVE-REINDEX-FOLD-NOTES.md` | W0-W11 map, section migration, and stale-reference guardrails. |
| `GATE-TABLE-FOLD-NOTES.md` | Row universes, M0/M1/M-css-prune, candidate formulas, telemetry fields, and gate table columns. |
| `DEPENDENCY-PREBLOCK-FOLD-NOTES.md` | Dependency table schema, initial rows, normalized REDRESS clusters, and consuming exits. |
| `COST-CONSUMER-FOLD-NOTES.md` | LOC/risk budgets, split waves, Decision Engine consumers, lowerer consumers, and W11 anti-deferral wording. |
| `LOCK14-EVENTTAPE-FOLD-NOTES.md` | CH5 forbidden vocabulary, EventTape guard, exclusion-report schema, alias rejection, and non-JSON receivers. |

## Next Dispatch

Fold the V2 roster into S-P3 artifacts before any wave dispatch. After the V2
fold commit, run a fresh seven-lens hardening cycle over the folded packet.
S-P3 cannot advance to Pass Omega or implementation waves until hardening
returns at least 95% ACCEPT for two consecutive cycles with zero orphan REVISEs,
or an intrinsic block is proven under the pass discipline.

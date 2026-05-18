# SK-V8 S-P3 Hardening V3 CH5: Hidden Coupling

## Scope

This challenge reviews the V3 S-P3 packet through the CH5 hidden-coupling lens: sidecar substrate, parser-owned structural projection/cursor/facts, Track 1 / Track 2 coupling, telemetry-only W3 consumer, new substrate/API/`BackendShape`/`UnionTape` drift, and citation-label hiding. Inputs read: ORCHESTRATOR Sections 3W/3Z, PASS-3, live SPEC/DISPATCH/HANDOFF, P3-A through P3-F, `p3-v3-citation-fold.md`, SC-1 through SC-6, and V1/V2 consolidated hardening.

## Verdict

ACCEPT.

Confidence: 96%.

Blockers: none.

Required fold if REVISE: none.

## Evidence

| Surface | Finding | Evidence |
| --- | --- | --- |
| CH5 contract | The governing lens still asks whether any wave introduces parallel substrate, sidecar producer, Lock 1 renamed scanner, or Track 1 == Track 2 dishonesty; PASS-3 also requires the SPEC to forbid parser-owned projection, retained cursor, aux density table, and sidecar event vector. | `restart/prompts/ORCHESTRATOR.md:74-88`, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:102-145` |
| V1/V2 disposition | V1 accepted CH5 at 96%. V2 also accepted CH5 at 96% and found no sidecar substrate, parser-owned facts/cursors, Track 1/Track 2 coupling, telemetry-only W3 consumer, or API/substrate drift. V2 failed only CH1 traceability, not CH5. | `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:18-27`, `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:19-31` |
| V3 citation fold | V3 changes citation shape to stable section labels and explicitly preserves strict-vs-strict discipline, Lock 14, no-new directive/BIR/substrate/API/`BackendShape`/`UnionTape`, and G-Alpha/W0-only dispatch. The fold maps labels to resolving target classes, so I found no citation-label hiding of a CH5 route. | `restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md:18-35`, `restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md:37-58` |
| Dispatch lock | S-P3 remains planning-only. G-Alpha can dispatch W0 only; W1-W6 require W0 close, fresh plans, exact owners/gates, required challenge acceptance, and orchestrator/user dispatch. This blocks hidden implementation or W3 redress from citation relabeling. | `restart/skinny/tranches/sk-v8/SPEC.md:29-37`, `restart/skinny/tranches/sk-v8/SPEC.md:814-825`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-10`, `restart/skinny/tranches/sk-v8/HANDOFF.md:191-198` |
| No new substrate/API surface | The live SPEC forbids new directives, BIR variants, `BackendShape`, `UnionTape`, new substrate surface, public substrate API, parser-owned structural cursor/facts, and parallel/sidecar substrate. Section 10 repeats these as global blocks. | `restart/skinny/tranches/sk-v8/SPEC.md:230-251`, `restart/skinny/tranches/sk-v8/SPEC.md:767-785`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:171-180` |
| W3 one-Tape boundary | W3 is only Tier A structural-class cursor migration inside one retained `Tape`; the structural projection is admissible only as representation replacement and fails if retained beside the old offset append path or if parser-owned cursor/fact slots survive. | `restart/skinny/tranches/sk-v8/SPEC.md:551-563`, `restart/skinny/tranches/sk-v8/SPEC.md:565-592`, `restart/skinny/tranches/sk-v8/HANDOFF.md:56-96` |
| P3-A/B/C/F consistency | P3-A, P3-B, P3-C, and P3-F all preserve W3 as one retained Tape with generated JSON retained parsing as same-wave production consumer. They block sidecar/parser-owned cursor routes, `UnionTape`, new `BackendShape`, new BIR/directive/API, second source scan, and `tape_vs_tape` as W3 production consumer. | `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:16-18`, `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:27-35`, `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:52-63`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:44-54`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:150-199`, `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:13-38` |
| P3-D telemetry adjudication | P3-D's W3 fields are gate telemetry only. `retained_union_tape` is a row value used to prove cardinality one and replacement semantics, not a public `UnionTape` type or sixth `BackendShape`; P3-D explicitly states the additions introduce no directive, BIR variant, public substrate type, or sixth `BackendShape`. | `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:57-102`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:133-146`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:148-163` |
| P3-E pre-block ledger | P3-E globally blocks new directive/BIR/`BackendShape`/`UnionTape`/public substrate/parallel substrate, sidecar producer, parser-owned projection/cursor, telemetry-only W3 consumers, and Track 1 == Track 2 dishonesty. It also makes no-deferral explicit. | `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:36-49`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:51-116`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:129-141` |
| S-P2 substrate ceiling | SC-1/SC-2/SC-3/SC-4/SC-5/SC-6 keep the candidate narrow: one producer, one retained Tape, scan-written opaque ordinals, no parser-owned cursor/facts/sidecar, no retained `StructuralIndex` query API, no string-plane Tier B smuggling, and no `tape_vs_tape` W3 consumer. | `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:272-327`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:287-317`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:118-123`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:286-295`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:403-481`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:290-328`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:179-216`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:251-282`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:641-666` |
| Track 1 / Track 2 independence | RESULTS records Track 1 as generated runtime parse and Track 2 as independent hand-coded parser that never calls generated Track 1. REDRESS closes the earlier bench-private SinkParser dishonesty and rejects generated-helper transfer into Track 2 as free parity repair. | `skinny/RESULTS.md:216-219`, `skinny/REDRESS.md:420-458`, `skinny/REDRESS.md:2061-2088` |
| Sidecar/parser-owned projection history | REDRESS rejects parse-time aux side tables, `JsonEventCursor`, parser-local structural-mask cursor, sidecar vectors, and parser-owned structural cursors. The V3 packet routes those failures into SPEC/P3-E pre-blocks rather than hiding them under section labels. | `skinny/REDRESS.md:715-813`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:68-72`, `restart/skinny/tranches/sk-v8/SPEC.md:588-592` |

## Blockers

None.

The V3 citation fold does not create a hidden CH5 regression. Material CH5 gates remain resolvable through live SPEC/DISPATCH/HANDOFF exact sections and through P3-D/P3-E tables. W3 remains a conditional future wave and is blocked from sidecar substrate, parser-owned structural projection, retained cursor/facts, aux/density tables, telemetry-only consumers, public substrate/API drift, `BackendShape` drift, `UnionTape` drift, and Track 1 / Track 2 coupling.

## Residual Non-Blocking Risks

1. `retained_union_tape` remains an unfortunate telemetry value because it can be misread as `UnionTape`. It is not blocking here because P3-D binds it to gate telemetry only and the live SPEC/DISPATCH/P3-E explicitly forbid public `UnionTape`, new `BackendShape`, new substrate surface, and public substrate API.
2. Stable section labels reduce citation brittleness but require readers to resolve claims through the V3 fold's label map. CH5 is still reviewable because the live blocking requirements are duplicated in exact SPEC, DISPATCH, HANDOFF, and P3-D/P3-E rows.
3. W3 remains implementation-risky under the 90-minute cap. That is a CH4/cost and future wave-entry risk, not a CH5 blocker, because V3 requires split or REVISE before implementation if the W3 slice cannot fit.

## Required Fold If REVISE

Not applicable. Verdict is ACCEPT.

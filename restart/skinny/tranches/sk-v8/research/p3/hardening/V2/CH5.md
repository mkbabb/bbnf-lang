# SK-V8 S-P3 Hardening V2 CH5: Hidden Coupling

## Scope

Challenge lens: adversarial hidden-coupling review of the S-P3 V2 synthesis-plan packet after the V1 hardening fold. This review checks ORCHESTRATOR §3W/§3Z, PASS-3, live SPEC/DISPATCH/HANDOFF, P3-A through P3-F, SC-1 through SC-6, and the S-P3 V1 consolidated hardening result for sidecar substrate, parser-owned structural projection, retained cursor/fact slots, Track 1 / Track 2 coupling, telemetry-only W3 consumers, new substrate surface, and renamed UnionTape / BackendShape / API drift.

## Verdict

ACCEPT.

Confidence: 96%.

Blockers: none.

Required fold if REVISE: none; V2 does not require a CH5 fold.

## Evidence

| Surface | CH5 Finding | Evidence |
| --- | --- | --- |
| V1 to V2 governance | V1 already accepted CH5, and the V2 fold changes CH1/CH4 weaknesses without loosening the CH5 boundary. | `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:20-69`, `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:18-25` |
| Dispatch lock | S-P3 remains planning-only. G-Alpha may dispatch W0 only, and W1-W6 remain conditional on fresh evidence, exact plans, challenge acceptance, owners, same-wave consumers, and gates. This prevents a hidden W3 implementation path from being smuggled through V2. | `restart/skinny/tranches/sk-v8/SPEC.md:1-37`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:1-10`, `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:27-36` |
| No sidecar substrate | The live packet forbids new directives, BIR variants, BackendShape, UnionTape, public substrate/API, parser-owned cursor/facts, sidecar substrate, and parallel substrate. W3 is constrained to one retained Tape by representation replacement. | `restart/skinny/tranches/sk-v8/SPEC.md:230-251`, `restart/skinny/tranches/sk-v8/SPEC.md:767-785`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:22-30`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:38-47` |
| W3 one-Tape discipline | W3 permits only Tier A structural-class cursor migration inside the singular retained Tape. It fails if retained structural data survives beside the old offset append path or if parser-owned cursor/fact slots survive. | `restart/skinny/tranches/sk-v8/SPEC.md:551-563`, `restart/skinny/tranches/sk-v8/SPEC.md:565-591`, `restart/skinny/tranches/sk-v8/HANDOFF.md:56-96`, `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:50-61`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:42-48` |
| Parser-owned projection | Parser-owned structural projection, retained cursor/fact slots, aux density tables, sidecar event vectors, and structural side tables remain explicitly pre-blocked. V2 adds a pre-redress fit/split gate rather than opening any parser-resident projection. | `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:122-145`, `restart/skinny/tranches/sk-v8/SPEC.md:588-591`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:127-139`, `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:18-36` |
| Track 1 / Track 2 independence | W2/W4 typed and direct routes require generated Track 1 plus independent Track 2/oracle proof. Track 2 may not call generated Track 1, SinkOnly, typed helpers, shared parser internals, or benchmark-private parsers. | `restart/skinny/tranches/sk-v8/SPEC.md:470-497`, `restart/skinny/tranches/sk-v8/SPEC.md:620-655`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:148-197`, `skinny/RESULTS.md:216-219`, `skinny/REDRESS.md:420-458`, `skinny/REDRESS.md:2061-2088` |
| Telemetry-only W3 consumer | `tape_vs_tape`, `parse_only`, direct/SinkOnly, `path!`, and audit rows cannot close W3 as production consumers. The W3 same-wave consumer must be generated JSON retained parser / retained view consuming retained Tape positions/classes. | `restart/skinny/tranches/sk-v8/SPEC.md:565-585`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127-140`, `restart/skinny/tranches/sk-v8/HANDOFF.md:56-96`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:179-216`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:251-282` |
| Renamed API / substrate drift | The packet blocks renamed UnionTape / BackendShape / public API drift and generic JSON residue. P3-D's `retained_union_tape` wording is telemetry/adjudication nomenclature only and is counter-bound by SPEC's explicit ban on new UnionTape, BackendShape, and substrate surface. | `restart/skinny/tranches/sk-v8/SPEC.md:230-251`, `restart/skinny/tranches/sk-v8/SPEC.md:300-325`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:93-100`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:131-160`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:177-185` |
| S-P2 substrate ceiling | SC-1/SC-2/SC-3/SC-6 allow only a single retained Tape replacement path with scan-written class ordinals and no surviving StructuralIndex API, sidecar, aux cache, second source scan, or parser-owned fact/cursor slots. | `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:272-304`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:287-316`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:403-480`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:641-666` |

## Blockers

None.

The adversarial hidden-coupling review found no V2-introduced blocker. The packet keeps W3 as a conditional future wave, not an automatic implementation authorization, and keeps the only admissible structural projection inside the existing Tape representation replacement. No parallel substrate, sidecar producer, parser-owned cursor/fact slot, Track 1 / Track 2 coupling path, telemetry-only W3 consumer, public API drift, BackendShape drift, or UnionTape drift is admitted.

## Residual Non-Blocking Risks

1. P3-D uses the telemetry phrase `retained_union_tape`. This is not a blocker because the live SPEC and P3-D itself bind it as adjudication telemetry and separately ban public UnionTape, BackendShape, new substrate, and public API drift. W3 implementation review should still grep for this phrase to prevent implementer misreading.
2. W3 remains high-risk because deleting the old offset append path while adding generated retained-parser consumption can exceed the default W3 budget. V2's pre-redress fit/split gate correctly turns that into a future wave-entry gate rather than a hidden sidecar allowance.
3. Lock 14 grammar-neutrality remains dependent on W3/W5 challenge evidence. This is not a CH5 blocker because current S-P3 does not authorize those waves and the live SPEC already rejects grammar-specific JSON policy, renamed JSON policy, and generated drift.

## Required Fold If REVISE

Not applicable. Verdict is ACCEPT, with no required CH5 fold.

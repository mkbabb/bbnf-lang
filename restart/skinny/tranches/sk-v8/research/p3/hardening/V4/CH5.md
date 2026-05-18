# SK-V8 S-P3 Hardening V4 CH5: Hidden Coupling

## Scope

V4-only hidden-coupling review of the S-P3 packet after `p3-v4-exact-traceability-fold.md`. Lens focus: same-wave consumer proof, primitive/substrate/template orphan traps, W3 dependence on W4/W5 hidden follow-up, W1 CostFacts as future-only behavior proof, sidecar substrate, parser-owned projection/cursor/facts, Track 1 / Track 2 coupling, and new substrate/API/`BackendShape`/`UnionTape` drift.

## Verdict

ACCEPT.

Confidence: 96%.

Blockers: none.

Required fold if REVISE: none.

## Evidence

| Surface | Finding | Evidence |
| --- | --- | --- |
| V4 fold scope | V4 is traceability-only and explicitly preserves the G-Alpha/W0 dispatch lock, no-new directive/BIR/substrate/API/`BackendShape`/`UnionTape`, no parser-owned cursor/facts, no sidecar substrate, no consumer-later primitive, W3 Tier A/Tier B split, scalar/checkasm, same-wave production consumer, and `tape_vs_tape` demotion. | `restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md:17-39` |
| CH5 contract | CH5 requires no parallel substrate, sidecar producer, Lock 1 renamed scanner, or Track 1 == Track 2 dishonesty. PASS-3 additionally requires the SPEC to forbid parser-owned projection, retained cursor, aux density table, and sidecar event vector. | `restart/prompts/ORCHESTRATOR.md:74-88`, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:102-145` |
| Governance | V3 failed only CH1 traceability; CH5 accepted at 96. V4 can qualify only with every role accepting at >=95 and no open critical defect. S-P2 V7 authorizes S-P3 synthesis only, not W3, implementation, or G-Alpha close. | `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:11-33`, `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:50-53`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:7-20`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:44-64` |
| Dispatch lock | No implementation wave dispatches from S-P3. G-Alpha closed dispatches W0 only. W1-W6 remain blocked until W0 closes, exact owner paths/gates and same-wave consumers are named, required challenge accepts, and user/orchestrator dispatch occurs. | `restart/skinny/tranches/sk-v8/SPEC.md:29-37`, `restart/skinny/tranches/sk-v8/SPEC.md:814-825`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-10`, `restart/skinny/tranches/sk-v8/HANDOFF.md:191-198` |
| Global same-wave consumer rule | The live SPEC blocks any primitive, kernel, generated path, or substrate representation without a same-wave hot-path consumer and rejects closes on "future consumer" language. P3-C and P3-E mirror this as a global gate for every primitive/substrate/materializer route. | `restart/skinny/tranches/sk-v8/SPEC.md:230-251`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:24-32`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:103-116` |
| W1 CostFacts trap | W1's only consumer is `gate-json --with-cost-facts` and strict-admission refusal. Parser behavior and generated output must remain unchanged unless a separate challenged behavior consumer is accepted; W1 rejection blocks W2-W6 behavior waves. P3-E states W1 cannot use CostFacts as a performance result or reopen REDRESS routes by evidence bookkeeping alone. | `restart/skinny/tranches/sk-v8/SPEC.md:385-440`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:110-115`, `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:24-26`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:66-90`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:24-34` |
| W3 owns its consumer | W3 entry requires a fresh plan with exact owners, rows, same-wave production consumer, measured-path proof, scalar/checkasm, Lock 1 handling, fit estimate, and challenge acceptance. W3 exit requires exactly one retained Tape, no old offset append API, no parser-owned cursor/fact slots, generated JSON retained parser as Tier A production consumer, retained view/`ValueRef` parity, Track 2 independence, Lock 14 proof, and no telemetry-only row counts. | `restart/skinny/tranches/sk-v8/SPEC.md:506-592`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127-140`, `restart/skinny/tranches/sk-v8/HANDOFF.md:56-96`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:150-199`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:133-146` |
| No W3 hidden W4/W5 follow-up | W4 starts only after W2/W3 are admitted, rejected, routed, or W3 is explicitly blocked. W5 starts only after W1-W4 dispositions. W3 already carries Lock 14/non-JSON proof and retained-parser production consumer in its own exit gate, so it cannot close by promising W4 direct work or W5 audit later. | `restart/skinny/tranches/sk-v8/SPEC.md:605-655`, `restart/skinny/tranches/sk-v8/SPEC.md:663-713`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:20-30`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:36-50`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:201-267` |
| Substrate/API drift | The packet still forbids new directives, BIR variants, `BackendShape`, `UnionTape`, new substrate surface, public substrate API, parser-owned cursor/facts, sidecar substrate, and parallel substrate. P3-D's `retained_union_tape` remains telemetry-only and explicitly introduces no public substrate type or sixth `BackendShape`. | `restart/skinny/tranches/sk-v8/SPEC.md:230-251`, `restart/skinny/tranches/sk-v8/SPEC.md:767-785`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:59-102`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:179-187` |
| Track 1 / Track 2 independence | W2/W4 require generated Track 1 plus independent Track 2/oracle. W4 explicitly forbids Track 2 from calling generated SinkOnly, typed helpers, generated Track 1, or a shared benchmark-private parser. RESULTS records Track 2 as independent and never calling generated Track 1. | `restart/skinny/tranches/sk-v8/SPEC.md:51-52`, `restart/skinny/tranches/sk-v8/SPEC.md:637-649`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:223-235`, `skinny/RESULTS.md:217-218` |
| Pre-block/no-deferral ledger | P3-E says a route is either still blocked or reopened by same-wave evidence; a wave cannot close by promising W0 profiles, comparator repair, CostFacts, scalar/checkasm, a production consumer, REDRESS accounting, or non-regression measurement later. It globally blocks sidecars, parser-owned projection/cursors, telemetry-only W3 consumers, orphan primitives, and Track 1 == Track 2 dishonesty. | `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:14-20`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:36-49`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:129-141` |

## Blockers

None.

I found no V4 hidden-coupling blocker. The exact-traceability fold changes citation granularity but does not create a new closure route. Every primitive/substrate/template path is either paired with a same-wave production consumer or remains blocked. W3 cannot rely on W4 or W5 as a hidden follow-up because W3 must close its own retained-parser consumer, measured rows, Lock 14 proof, and no-sidecar/no-parser-owned checks before W4/W5 can matter. W1 CostFacts remains a gate/evidence wave only and cannot admit future behavior by itself.

## Residual Non-Blocking Risks

1. `retained_union_tape` remains a potentially confusing telemetry token, but V4 still counter-binds it with explicit `UnionTape`, `BackendShape`, public substrate API, and new-substrate bans.
2. W3 may still need splitting on cost once the exact W3 plan is written. The packet handles this as split-or-REVISE before implementation, not as a hidden follow-up consumer.
3. P3-A/P3-B/P3-E self-confidence values remain below 95 in their own artifacts, but their CH5-relevant semantics are folded into live SPEC/DISPATCH/HANDOFF and do not create an open critical CH5 defect.

## Required Fold If REVISE

Not applicable. Verdict is ACCEPT.

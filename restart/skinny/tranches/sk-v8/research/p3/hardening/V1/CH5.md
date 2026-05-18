# SK-V8 S-P3 Hardening V1 CH5: Hidden Coupling

Date: 2026-05-18.
Pass: S-P3 Synthesis-Plan.
Cycle: V1.
Lens: CH5 HIDDEN COUPLING.

## Scope

This review audits the S-P3 packet for hidden coupling: parallel substrate, sidecar producer, renamed-scanner / Lock 1 violation, Track 1 == Track 2 dishonesty, parser-owned structural projection, retained cursor, aux density table, sidecar event vector, telemetry-only W3 consumer, and whether the W3 tape plus structural-projection union remains one retained `Tape` by representation replacement.

Inputs reviewed: ORCHESTRATOR, PASS-3-SYNTHESIS-PLAN, PASS-ALPHA, SKINNY-TRIUMVIRATE, P3-A through P3-F, live `SPEC.md`, `DISPATCH-PROMPT.md`, `HANDOFF.md`, S-P2 SC-1 through SC-6 plus V7 consolidation, `skinny/RESULTS.md`, and `skinny/REDRESS.md`.

## Verdict

ACCEPT.

Confidence: 96%.

Blockers: none.

Required folds if REVISE: none.

## Findings

No blocking hidden-coupling defect found.

| Check | Disposition | Evidence |
|---|---|---|
| CH5 contract is explicitly carried | ACCEPT | PASS-3 defines this lens as parallel substrate, sidecar producer, renamed scanner / Lock 1, Track 1 == Track 2 dishonesty, and parser-owned projection/cursor/aux/sidecar-vector review (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134-138`). ORCHESTRATOR binds CH5 to no parallel substrate, no sidecar producer, no renamed scanner, no Track 1 == Track 2 dishonesty, and substrate union holding (`restart/prompts/ORCHESTRATOR.md:86-87`). |
| S-P2 V7 does not smuggle implementation authority | ACCEPT | V7 preserves the union as a lead W3 hypothesis only and blocks automatic W3/G-Alpha/implementation dispatch (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:46-51`). It also forbids new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate API, parser-owned cursor/facts, parallel substrate, and `tape_vs_tape` as W3 production consumer (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:61-64`). |
| W0 cannot introduce sidecar behavior | ACCEPT | W0 is telemetry-only, must reject stale sidecar strict claims, and must not change parser, scanner, SIMD, asm, codegen, product-plane behavior, or generated parser output (`restart/skinny/tranches/sk-v8/SPEC.md:309-343`). DISPATCH repeats the same W0 limits (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:53-81`). |
| W1 CostFacts is not a shadow route substrate | ACCEPT | W1 consumes CostFacts/comparator fields through `gate-json --with-cost-facts`, keeps generated JSON output and parser behavior unchanged unless a separate challenged behavior consumer is accepted, and pre-blocks producer-only CostFacts/telemetry (`restart/skinny/tranches/sk-v8/SPEC.md:374-400`). |
| W2 typed product plane preserves Track 1 / Track 2 honesty | ACCEPT | W2 requires exact Track 1 generated path, Track 2/oracle path, structural independence proof, and blocks Track 2/oracle calls into generated Track 1, generated SinkOnly, generated typed helpers, or shared benchmark-private parser (`restart/skinny/tranches/sk-v8/SPEC.md:426-456`). This folds REDRESS 34/35, which corrected the old bench-private parser / shared Track 1-Track 2 dishonesty (`skinny/REDRESS.md:420-458`). |
| W3 union remains one Tape by representation replacement | ACCEPT | SPEC Section 1 states W3 is representation replacement inside one retained `Tape`, not a new substrate, and forbids parser-owned structural cursor/facts and parallel/sidecar substrate (`restart/skinny/tranches/sk-v8/SPEC.md:207-215`). W3 Section 6 requires a single retained `Tape`, old offset append API absence, parser-owned cursor/fact slot absence, generated JSON retained parser as production consumer, and no telemetry-only row as consumer (`restart/skinny/tranches/sk-v8/SPEC.md:513-546`). The W3 pre-block list explicitly bans sidecar event vector, retained cursor, aux table, density cache, parser-owned class/fact slot, second source scan, old offset append path, `UnionTape`, `BackendShape`, BIR, directive, and public substrate API (`restart/skinny/tranches/sk-v8/SPEC.md:548-554`). |
| S-P2 SC files support, not weaken, the W3 one-Tape invariant | ACCEPT | SC-1 states candidate-only posture, one producer, one retained `Tape`, no parser-owned cursor/facts sidecar, and no new directive/BIR/BackendShape/public substrate/independent substrate (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:272-280`). SC-2 says the scan product is move-consumed into the retained `Tape`, with no post-build `StructuralIndex` query API, sidecar, aux table, density cache, parser-owned cursor, or parallel offset append path (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:294-299`). SC-3 requires exactly one retained `Tape`, move-only scan product, no clone/cache, no post-build attachment hook, and no generated-parser independent cursor (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:407-423`). SC-6 distinguishes the admissible union as replacement, while sidecar structural index plus old offset tape is the forbidden parallel-substrate failure (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:176-190`). |
| `tape_vs_tape` is not a hidden W3 consumer | ACCEPT | SC-5 limits `tape_vs_tape` to W0/W1 telemetry or gate-binding work until same-run structural-index competitor rows exist, and says it must not count as W3's production same-wave consumer (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:194-206`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:251-259`). P3-A, P3-B, P3-C, P3-D, P3-E, P3-F, SPEC, DISPATCH, and HANDOFF all preserve that boundary (`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:30-35`, `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:42-48`, `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:181-193`, `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:137-144`, `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:40-47`, `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:31-36`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:120-129`, `restart/skinny/tranches/sk-v8/HANDOFF.md:90-96`). |
| W4 direct triage preserves independent Track 2 and blocks parser-owned scratch | ACCEPT | W4 requires selected rows to meet Track 1 and Track 2 floors, forbids Track 2 from calling generated SinkOnly, generated typed helpers, generated Track 1, or a shared benchmark-private parser, and pre-blocks parser-owned scratch, Track 2 coupling, and digest-as-product proof (`restart/skinny/tranches/sk-v8/SPEC.md:597-615`). RESULTS also records Track 2 as an independent hand-coded parser over `runtime::tape`, with a signed checklist that it never calls `runtime::generated_json::parse` (`skinny/RESULTS.md:216-218`). |
| W5/W6 cannot hide generic JSON policy or close around a hidden substrate | ACCEPT | W5 audits generic crates for JSON policy and renamed residue, keeps allowed JSON surfaces bounded, and pre-blocks renamed JSON helpers plus generated behavior drift disguised as audit (`restart/skinny/tranches/sk-v8/SPEC.md:638-668`). W6 blocks close if there is missing same-wave consumer proof, unresolved Lock 1/Omega fork, sidecar/permissive strict admission, or dropped falsifier rows (`restart/skinny/tranches/sk-v8/SPEC.md:703-721`). |
| REDRESS pre-blocks are folded against sidecars/cursors/aux paths | ACCEPT | P3-E globally blocks sidecar producers, parser-owned structural projection, retained cursor, aux density table, sidecar event vector, `tape_vs_tape`/telemetry-only W3 consumers, and Track 1 == Track 2 dishonesty (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:38-47`). REDRESS 50/51/53 measured dense/sparse aux side tables, EventCursor, parser-local structural cursor, `StructuralIndex`, `Vec<JsonEvent>`, whitespace sidecar, and aux projection as rejected/non-canonical routes (`skinny/REDRESS.md:715-813`). |

## Residual Non-Blocking Risks

1. W3 naming still uses "cursor" and "union", which can be misread as a sidecar or sixth substrate if isolated from SPEC Section 6. The live packet neutralizes that risk by requiring representation replacement inside one retained `Tape`, old offset append deletion, and no `UnionTape` / `BackendShape` / public substrate API (`restart/skinny/tranches/sk-v8/SPEC.md:521-546`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:120-129`).

2. P3-D includes telemetry enum values such as `substrate_surface=retained_union_tape`; this is not a substrate introduction because P3-D says the additions are telemetry fields only and introduce no directive, BIR variant, public substrate type, or sixth `BackendShape` (`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:93-100`). Later implementation should keep that naming gate-consumed and non-API.

3. SC-6-L1-R1 remains a Pass Omega candidate. The live SPEC and DISPATCH correctly require W3 either to wait for Omega ratification or prove Lock 1 as written and route the residual (`restart/skinny/tranches/sk-v8/SPEC.md:500-509`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:120-129`).

## Disposition

CH5 disposition: ACCEPT.

Confidence: 96%.

No required folds.

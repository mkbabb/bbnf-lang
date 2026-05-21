# T-P2 V2 CH5 Hidden Coupling / Substrate Ownership

Pass: T-P2 Research. Cycle: V2. Lens: CH5 hidden coupling / Lock 1.
Date: 2026-05-21.
Scope: `restart/audit/totality/p2/2A-sota-landscape.md` through
`2F-parse-that-gaps.md`, `T-P2-V2-FOLD-ADDENDUM.md`,
`restart/prompts/totality/PASS-2-RESEARCH.md`, and SK-V13 scoping docs for
value/API union and SIMD/ASM/union.

## Verdict

ACCEPT.

V2 closes the V1 CH5 blockers at the research-contract level. The fold addendum
adds the missing Lock 1 vocabulary, and the six dossiers adopt it rather than
leaving substrate ownership implicit. Routes may still be conditional or
non-admitted, but they now fail closed when they require retained masks/classes,
parser-owned cursor/list state, a second tape, `UnionTape`, public substrate API,
or a new directive/BIR/`BackendShape` surface.

## Findings

| id | disposition | finding |
|---|---|---|
| CH5-V2-F1 | ACCEPT | The Lock 1 substrate fields are now canonical and gate-consumable. The addendum requires every e-graph candidate, backend rewrite, imported scanner plan, union candidate, and SIMD consumer to declare `substrate_target`, `retention_lifetime`, and `policy_owner`, with only `local_temp_only`, `existing_tape`, `direct_sink`, and `admitted_fact_output` allowed as targets (`T-P2-V2-FOLD-ADDENDUM.md:77-98`). The fields are then instantiated in primitive, resolver, hardware, and parse-that ledgers (`2B-primitive-vocabulary.md:136-149`, `2D-cost-model.md:66-87`, `2E-host-arch-esoterica.md:104-140`, `2F-parse-that-gaps.md:183-219`). |
| CH5-V2-F2 | ACCEPT | `CollapsedStage` no longer implies a retained FSM/mask lane. V2 defines it as an emitted-kernel strategy whose masks/classes/FSM state are local temporaries or admitted row output only (`T-P2-V2-FOLD-ADDENDUM.md:88-94`; `2D-cost-model.md:89-106`). The cost dossier also refutes existing `CollapsedStage` eligibility as admissible and keeps x86 AVX-512 evidence as background until an aarch64 same-wave consumer exists (`2D-cost-model.md:159-168`, `2D-cost-model.md:181-189`). |
| CH5-V2-F3 | ACCEPT | The parse-that route is fenced to compile-time facts and generated-loop-local runtime outputs. `bbnf-regex`/parse-that may import HIR, automata facts, byte classes, and scanner plans, but runtime masks/classes/cursors must be consumed in-loop into the existing tape, direct sink, or admitted fact output (`2F-parse-that-gaps.md:20-42`, `2F-parse-that-gaps.md:139-144`, `2F-parse-that-gaps.md:157-181`). 2F explicitly rejects adding new BIR variants or public substrate APIs to mirror parse-that internals unless a lock amendment is signed (`2F-parse-that-gaps.md:161-177`). |
| CH5-V2-F4 | ACCEPT | Fact streams are now output-plane contracts, not hidden sidecars. The addendum says fact streams are valid only with strict comparator/oracle provenance and gate-consumed telemetry (`T-P2-V2-FOLD-ADDENDUM.md:96-98`). 2C carries the same boundary: fact streams are admitted output-plane contracts, not retained internal sidecars (`2C-grammar-neutrality.md:78`, `2C-grammar-neutrality.md:126-134`). 2D's `SinkOnly`/fact route likewise requires strict same-plane equality and independent oracle, with `admitted_fact_output` only at the output row boundary (`2D-cost-model.md:75`). |
| CH5-V2-F5 | ACCEPT | V2 does not leak new directive, BIR, `BackendShape`, or public substrate API surface. The research prompt forbids absorbing such surfaces silently (`PASS-2-RESEARCH.md:200-208`), and V2 makes second tapes/public substrate APIs reject conditions (`T-P2-V2-FOLD-ADDENDUM.md:88-90`; `2D-cost-model.md:100-106`). 2C keeps `BackendShape` at the existing five-shape vocabulary (`2C-grammar-neutrality.md:72`, `2C-grammar-neutrality.md:135-143`). The SK-V13 value/API union scoping reaches the same conclusion: union selection must be codegen-private/per-grammar config, with no new directive, BIR variant, `BackendShape` variant, `UnionTape`, sidecar vectors, or public tape/substrate API (`sk-v13-scoping-value-api-union.md:242-303`). |
| CH5-V2-F6 | ACCEPT | The risky union/SIMD candidates are not paper-admitted. The addendum and 2B/2E require material differentials from REDRESS 88/89/96/97/98, same-loop ownership of masks/positions, and emission only into `existing_tape`, `direct_sink`, or `admitted_fact_output` (`T-P2-V2-FOLD-ADDENDUM.md:120-137`; `2B-primitive-vocabulary.md:224-242`; `2E-host-arch-esoterica.md:166-182`). The value/API scoping's C1/C2/C3 candidates remain future attempts with CHALLENGE risks and row gates, not accepted substrate expansions (`sk-v13-scoping-value-api-union.md:305-394`). |

## Required Redress If Any

None for CH5 in T-P2 V2.

Downstream T-P3/S-P3 work must instantiate the ledgers for any selected route.
That is admission work, not a V2 hidden-coupling defect. Any future wave that
chooses the public `GrammarConfig` option, adds a new directive/BIR/shape, or
retains scanner lanes outside the allowed lifetimes should be rejected unless the
user amends the lock first (`sk-v13-scoping-value-api-union.md:543-557`,
`sk-v13-scoping-value-api-union.md:601-606`).

## Evidence Checked

- `restart/audit/totality/p2/hardening/V1/CH5.md`
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`
- `restart/prompts/totality/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/locks/LOCKS.md`

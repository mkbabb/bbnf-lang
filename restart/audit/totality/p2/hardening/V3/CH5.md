# T-P2 V3 CH5 Hidden Coupling / Substrate Ownership

Pass: T-P2 Research.
Cycle: V3.
Lens: CH5 hidden coupling / Lock 1.
Date: 2026-05-21.
Agent: REDEPLOYED CH5.

## Verdict

ACCEPT.

V3 preserves the V2 CH5 acceptance and strengthens it enough for T-P3. The
central V3 admission ledger makes `substrate_target`, `retention_lifetime`, and
`policy_owner` gate-visible fields for the primitive, hardware, and parse-that
routes that carry the highest sidecar risk. The V2 Lock 1 substrate-kind
contract remains binding because the V3 addendum explicitly supplements rather
than replaces it. Across 2A-2F, runtime masks, class streams, scanner cursors,
and FSM state are fenced as local producers unless they are consumed into
`existing_tape`, `direct_sink`, or `admitted_fact_output` in the same admitted
row. V3 does not silently authorize a public substrate API, sidecar producer,
second tape, `UnionTape`, new directive, new BIR variant, or new
`BackendShape` variant.

## Evidence

| id | disposition | evidence |
|---|---|---|
| CH5-V3-01 | ACCEPT | `T-P2-V3-FOLD-ADDENDUM.md` centralizes the executable ledger for 2B/2E/2F with `substrate_target`, `retention_lifetime`, and `policy_owner` on every shared candidate row. The ledger uses only allowed Lock 1 targets: `local_temp_only`, `existing_tape`, `direct_sink`, and `admitted_fact_output`, with local or generated lifetimes and generated/caller/no-policy ownership. |
| CH5-V3-02 | ACCEPT | The ledger closes the known scanner-sidecar escape hatches. `scanner_plan_import` is `local_temp_only` / `local_loop` / `generated_grammar` and abrogates on retained mask/class/cursor streams. `bbnf_regex_hir_import` and `regex_info_to_backendexpr_facts` are compile-time or generated-function facts, not runtime substrates. `semantic_digest_simd_mix` targets `direct_sink` only and abrogates on semantic mismatch or admitted-row regression. |
| CH5-V3-03 | ACCEPT | Structural and union routes remain fenced. `pmull_cssc_structural_union_emit64` may emit only to `existing_tape`, `direct_sink`, or `admitted_fact_output`; it is still `source_backed` with the REDRESS material-differential checklist unmet. 2A, 2B, 2E, and 2F all preserve the rule that simdjson-style masks and parse-that scanner facts are transient producers, not retained class columns or parser-owned cursor substrates. |
| CH5-V3-04 | ACCEPT | `CollapsedStage` ownership is explicit enough. 2D classifies it as `collapsed_stage_transient`, with `local_temp_only` or `direct_sink` target and `local_loop` lifetime, and its abrogate condition rejects retained mask/FSM state or no row consumer. The V2 addendum still states `CollapsedStage` is an emitted-kernel strategy whose masks and FSM state are local temporaries unless emitted as the admitted row output. |
| CH5-V3-05 | ACCEPT | New public surfaces remain blocked. The V2 addendum rejects retained class/mask streams, parser-owned cursor/list state, public substrate APIs, `UnionTape`, and second tapes unless Lock 1 is amended. 2D adds that `backend_expr_language` must pass without a new BIR or `BackendShape` variant unless G-Omega changes the lock. 2F allows parse-that HIR/facts only through existing `BackendExpr` facts or an explicitly approved fact side table, not by importing parse-that internals as new BIR. |
| CH5-V3-06 | ACCEPT | Source vocabulary and ownership are disjoint enough for T-P3. 2B keeps Layer 0 as vendored macro/process infrastructure and Layer 1 as bbnf-owned grammar-neutral primitives. 2C assigns sink, fact, value, flag, and provider vocabulary to generated grammar-owned surfaces instead of generic substrate APIs. 2E keeps ISA gates in Lock 16 hardware manifests. 2F separates compile-time regex/HIR/scanner facts from runtime scanner outputs. |
| CH5-V3-07 | ACCEPT | V3 does not rely on field names alone. Candidate rows carry rollback paths and abrogate thresholds for retained streams, missing consumers, row misses, equality failures, semantic digest mismatches, row regressions, and REDRESS replay. This makes the ownership fields usable as T-P3 gate inputs rather than prose metadata. |

## Required Repairs

None for CH5 before the next consolidation.

T-P3 must carry these fields forward as hard manifest inputs. Any downstream
wave that encodes parser-owned cursor state, retained mask/class streams, a
public substrate API, a second tape, `UnionTape`, a new directive, a new BIR
variant, or a new `BackendShape` variant must be rejected unless the relevant
lock is explicitly amended.

## Evidence Read

- `restart/prompts/totality/PASS-2-RESEARCH.md`
- `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md`

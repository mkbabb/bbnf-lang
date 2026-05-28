# CH6 - ANTI-PAPER-CLOSE

Lens name: CH6 anti-paper-close.

Disposition: ACCEPT.

The V2 packet folds the V1 CH6 defect. I found no row that closes from source
density, source inventory, ISA feature presence, or a named paper alone. The
grounded, partial, and architecture-pressure rows now carry row-local
`transfer_reason`, `admission_gate`, `verification_action`, and `close_status`
fields either as explicit columns or as inline suffixes. Where a row is
SIMD/ASM/primitive-facing, it also names the scalar reference or scalar oracle,
parity/checkasm or equivalent, hardware gate, same-wave consumer, and row
movement target, or it marks the route diagnostic, scalar-delegated,
source-present-unwired, blocked, or refuted.

## Critical Findings

| id | severity | finding | evidence | convergence impact |
|---|---|---|---|---|
| CH6-V2-OK-01 | none | The six dossiers preserve the V2 standalone row shape for grounded/partial rows. Rows do not close merely because a source register is dense; they attach transfer, admission, verification, and close status inline. | 2A grounding rows at `restart/audit/totality/p2/2A-sota-landscape.md:51`-`63`; 2B row suffixes and manifest at `restart/audit/totality/p2/2B-primitive-vocabulary.md:59`-`76`, `:144`-`155`; 2C row-shape table at `restart/audit/totality/p2/2C-grammar-neutrality.md:59`-`75`; 2D row-shape columns at `restart/audit/totality/p2/2D-cost-model.md:57`-`68`; 2E manifest rows at `restart/audit/totality/p2/2E-host-arch-esoterica.md:71`-`82`; 2F admission manifest at `restart/audit/totality/p2/2F-parse-that-gaps.md:71`-`80`. | No block. |
| CH6-V2-OK-02 | none | 2A no longer lets SOTA parser citations close rows by themselves. simdjson, On-Demand, yyjson, cssparser, and lightningcss are diagnostic or source-present until same-plane row-local evidence exists; only the sonic-rs targeted-leaf row proposes primitive transfer and carries the scalar/parity/hardware/consumer/movement/cost gate inline. | `restart/audit/totality/p2/2A-sota-landscape.md:51`-`58`; workload/refutation rows at `:59`-`63`; retained-sidecar pre-block at `:110`; Lock 16 scalar-first amendment at `:111`. | No block. |
| CH6-V2-OK-03 | none | 2B and 2E are CH6-compliant positive controls for SIMD/ASM work. They keep source inventory separate from admission and require scalar oracle, strict checkasm/parity, Apple M5 Max/aarch64 hardware gate or scalar-delegate disclosure, same-wave consumer, and row-local movement target. | 2B primitive process rows at `restart/audit/totality/p2/2B-primitive-vocabulary.md:62`-`68`; 2B primitive/macro manifest at `:144`-`155`; 2E host primitive rows at `restart/audit/totality/p2/2E-host-arch-esoterica.md:73`-`82`. | No block. |
| CH6-V2-OK-04 | none | 2C moved generality assertions out of prose-only close. CSS syntax/value/selector, Sheets, BBNF-self, Pattern H, generated-provider, and full-surface Lock 14 routes carry row-local admission and verification actions rather than relying on later OQ/LAC text. | `restart/audit/totality/p2/2C-grammar-neutrality.md:61`-`75`; LAC cost/admission rows at `:144`-`149`. | No block. |
| CH6-V2-OK-05 | none | 2D splits the Decision Engine and BackendShape work into costed W7/W8/W9 units and keeps AVX-512 CollapsedStage diagnostic-only unless an aarch64 scalar/parity/hardware/consumer/movement gate is supplied. | Technique rows at `restart/audit/totality/p2/2D-cost-model.md:59`-`68`; W7/W8/W9 units at `:70`-`76`; x86/aarch64 refutation at `:97`. | No block. |
| CH6-V2-OK-06 | none | 2F no longer lets parse-that gaps close from "vendor/wire later" prose. Each gap row names owner, scalar oracle/reference, parity or check command, hardware gate, same-wave consumer, row movement target, verification action, close status, LOC/risk, wave owner, and hard-cap fit. CSS value parsing remains assigned to the generated CSS typed provider, not JSON string/number semantics. | `restart/audit/totality/p2/2F-parse-that-gaps.md:73`-`80`; CSS/generated-provider assertions at `:90`-`92`; LAC rows at `:119`-`122`. | No block. |

## Evidence Inspected

- Challenge authority: `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md`.
- Pass and orchestrator contracts: `restart/prompts/totality/PASS-2-RESEARCH.md`, `restart/prompts/ORCHESTRATOR.md`.
- Dispatch and fold authorities: `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`, `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`.
- Target dossiers: `restart/audit/totality/p2/2A-sota-landscape.md`, `restart/audit/totality/p2/2B-primitive-vocabulary.md`, `restart/audit/totality/p2/2C-grammar-neutrality.md`, `restart/audit/totality/p2/2D-cost-model.md`, `restart/audit/totality/p2/2E-host-arch-esoterica.md`, `restart/audit/totality/p2/2F-parse-that-gaps.md`.

Local scans used:

```text
rg -n "\|.*\|.*\|.*(grounded|partial|architecture-pressure).*\|" restart/audit/totality/p2/2{A,B,C,D,E,F}-*.md
rg -o "transfer_reason|admission_gate|verification_action|close_status|scalar_reference|scalar_oracle|parity_or_checkasm|hardware_gate|same_wave_consumer|row_movement_target|loc_estimate|risk_class|wave_owner|hard_cap_fit" restart/audit/totality/p2/2{A,B,C,D,E,F}-*.md
rg -n "source density|citation alone|source inventory|inventory|validated|proven|prove|close_status|diagnostic-only|source-present-unwired|partial-blocked|scalar-delegated|admissible-after-gate|checkasm|scalar_reference|scalar_oracle|same_wave_consumer|row_movement_target" restart/audit/totality/p2/2{A,B,C,D,E,F}-*.md
```

## Fold Requirements

None. CH6 returns ACCEPT for V2.

## Convergence Impact

CH6 does not block T-P2 V2 convergence. If the other V2 challenge lenses also
return ACCEPT, this cycle can count as the first clean T-P2 hardening cycle.
Per the challenge context, T-P2 still needs a second consecutive clean challenge
cycle before normal Section 3Z convergence; V2 alone cannot advance T-P2 to T-P3.

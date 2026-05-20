# SK-V11 W4 CHALLENGE V2 CH2 - Generality / Lock 14

Date: 2026-05-20.
Scope: W4 CHALLENGE V2, CH2 generality / Lock 14 lens.
Output: this file.
Disposition: ACCEPT.

## Read Set

- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md`
- `restart/skinny/tranches/sk-v11/SPEC.md` Section 8
- `skinny/REDRESS.md` REDRESS 113 and REDRESS 114
- `restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH2-generality-lock14.md`

## CH2 Question

Does W4 Plan V2 remain a JSON-local generated direct/container-tail slice, carry
REDRESS 113's non-JSON block without treating it as proof, and avoid generic
policy, directive, BIR, or substrate leakage?

## Verdict

ACCEPT.

V2 remains within the CH2 acceptance envelope from V1. It selects only P2-D D1
`container_tail_next` for exactly `random/direct_to_struct/main`, explicitly
states that W4 does not claim non-JSON generalization, and carries REDRESS 113's
blocked non-JSON route forward. The plan also repeats that W4 does not reopen
W3, parse-only, numeric-slot, object-carry, sidecar, directive, BIR, or substrate
work.

V2 can proceed only as JSON direct-plane closure/fixpoint work. It does not
close the SK-V11 non-JSON generated-parser axis, does not authorize generic
code behavior changes, and does not allow REDRESS 114's rejected numeric W3
evidence to be reused as W4 proof.

## Checks

| Check | Assessment | Evidence |
|---|---|---|
| JSON-local target | ACCEPT | V2 selects one scalar JSON-local container-tail helper consumed by generated Track 1 and independently mirrored in direct Track 2 (`w4-plan-container-tail-direct-v2.md:12`-`16`). The selected target set is exactly `random/direct_to_struct/main` (`:16`). |
| Non-JSON block carried | ACCEPT | V2 says W4 does not claim non-JSON generalization and that REDRESS 113 remains blocked (`w4-plan-container-tail-direct-v2.md:18`-`20`, `:156`). REDRESS 113 records W2 as `BLOCKED` because W1b admitted no generated non-JSON baseline and W2 may not create the first measurable non-JSON row (`skinny/REDRESS.md:3340`-`3355`). |
| REDRESS 113 not converted into proof | ACCEPT | SPEC Section 8 says REDRESS 113's W2 non-JSON block is carried forward and is not generic-edit proof (`SPEC.md:508`-`511`). V2 mirrors that boundary by treating the block as a carried non-JSON miss, not as Lock 14 closure. |
| REDRESS 114 numeric rejection respected | ACCEPT | REDRESS 114 rejects W3 under `G-W3-NUMERIC-SEQUENCE-DIRECT`, with no `RESULTS.md` row movement, and says W4 may dispatch only with REDRESS 113 carried forward (`skinny/REDRESS.md:3357`-`3380`). V2's material surface is container-tail control for `random`, not numeric slot, f64 fallback, mantissa widening, parse-only, or W3 substrate work (`w4-plan-container-tail-direct-v2.md:18`-`20`, `:194`-`206`). |
| Generic policy leakage | ACCEPT | SPEC Section 8 requires no JSON policy in generic crates (`SPEC.md:527`-`529`). V2's behavior paths are the JSON direct renderer, regenerated JSON runtime, JSON direct Track 2, and gate/report consumption (`w4-plan-container-tail-direct-v2.md:83`-`90`). Conditional generic/codegen paths are limited to renderer metadata/tests or require returning to CHALLENGE before semantic lowering changes (`:91`-`100`). |
| Directive/BIR/substrate leakage | ACCEPT | SPEC Section 8 requires CHALLENGE to prove no directive/BIR/substrate change (`SPEC.md:508`-`511`) and pre-blocks object/key/value-byte carry, retained cursor, class lane, sidecar, and Track 1/Track 2 coupling (`:526`-`538`). V2 says W4 does not reopen directive, BIR, or substrate work (`w4-plan-container-tail-direct-v2.md:18`-`20`) and reverts on owner-path/Lock 14 violation or missing same-wave gate/report consumption (`:192`-`206`). |
| Track 2 independence | ACCEPT | V2 keeps Track 1 and Track 2 helpers as separate source implementations and forbids Track 2 calls into generated Track 1, generated SinkOnly helpers, `container_tail_next_direct`, or any generated Track 1 tail symbol (`w4-plan-container-tail-direct-v2.md:75`-`79`, `:120`-`122`). |
| V1 CH2 boundaries preserved | ACCEPT | CH2 v1 accepted W4 only as JSON-local D1 for `random/direct_to_struct/main`, with REDRESS 113 carried and no generic lowering, BIR, directive, shared runtime, or grammar-neutral semantic edit (`w4-CH2-generality-lock14.md:32`-`45`, `:60`-`79`). V2 narrows and restates those same boundaries while folding CH1/CH4/CH5 fixes. |

## Required Redress Boundaries

- Keep `container_tail_next_direct` JSON-local in generated JSON direct code and
  JSON-specific hand Track 2.
- Do not treat W4 as non-JSON generated-baseline authority; REDRESS 113 remains
  blocked until a later accepted contract creates and measures that baseline.
- Do not edit generic lowering semantics, BIR, directives, public substrate APIs,
  retained sidecars/cursors, class lanes, hidden byte/class masks, or generic
  JSON policy.
- Consume W4 provenance in `gate-json` and `report.rs` in the same wave; fail
  closed on producer-only telemetry, below-floor evidence, coupled Track 2,
  wrong comparator plane, direct guard regression, or wrong provenance.
- Preserve REDRESS 114 as a rejected numeric W3 route; do not launder W3 numeric
  evidence, number policy, or parse-only facts through W4.

## Failure Conditions

Return REVISE before source work if implementation needs a generic semantic
lowering change, BIR/directive/substrate extension, broader owner path, new
telemetry schema, or a non-JSON/generalized claim.

Return REJECT/REDRESS if the landed slice couples Track 2 to generated helpers,
leaks JSON policy into generic code, relies on W3 numeric proof, misses same-wave
gate/report consumption, or uses REDRESS 113 as non-JSON proof.

DISPOSITION: ACCEPT

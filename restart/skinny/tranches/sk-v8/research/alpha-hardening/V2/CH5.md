# CH5 Hidden Coupling Challenge: SK-V8 Alpha V2

Date: 2026-05-17.
Lens: CH5 Hidden Coupling.
Scope: final SK-V8 tranche docs plus V1 CH5 and V1 consolidated findings.

Overall disposition: ACCEPT.

The final SK-V8 docs resolve the V1 hidden-coupling blockers. The packet is now
safe for G-Alpha review with the documented narrow dispatch scope: G-Alpha may
authorize W0 only, and W1-W6 remain conditional on W0 closure plus fresh plan
augmentation.

## Evidence Reviewed

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CH5.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CONSOLIDATED.md`

## V1 Blocker Resolution

| V1 hidden-coupling issue | V2 finding | Disposition |
|---|---|---|
| Producer-only telemetry or stale sidecar metadata | `SPEC.md` Section 0.4 requires every emitted telemetry field to be consumed by `gate-json` in the same wave. W0 must validate sidecar freshness and reject a malformed manifest. W0 also forbids parser, scanner, SIMD, asm, codegen, and product-plane behavior changes. | ACCEPT |
| Track 1/Track 2 honesty | `SPEC.md` Section 5 requires generated Track 1 from grammar facts plus explicit host/API schema facts, and requires Track 2/oracle not to call generated Track 1, generated SinkOnly, generated typed helpers, or a shared benchmark-private parser. W4 also keeps direct Track 1 and Track 2 structurally independent. | ACCEPT |
| Product-plane typed independence | `SYNTHESIS.md`, `SPEC.md`, and `HANDOFF.md` keep `direct_to_struct` digest rows as guard rows, not product-plane SOTA proof. New typed product rows must be `real_typed_struct` rows with explicit host/API schema facts and independent Track 2 or oracle evidence. | ACCEPT |
| Sidecar density classifier | The bitmap route is rejected as a default and moved to reserve research only after fresh W0/W1 evidence and challenge acceptance. `SPEC.md` Section 6 makes any density cache, sidecar event vector, retained cursor, aux table, parser-owned structural projection, or source-byte second scan a Lock 1 failure. | ACCEPT |
| Orphan primitives | `SPEC.md` Section 1 requires scalar reference and checkasm before primitive wiring, and Section 10 pre-blocks orphan primitive admission. Reopening pre-blocked primitive routes requires fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance. | ACCEPT |
| Lock 1 scanner drift guard | `SPEC.md` Section 6 requires exact W3 owner paths after W0/W1 and same-loop consumption for scanner/parser-template changes. Any second scan, retained cursor, aux table, density cache, sidecar event vector, or parser-owned structural projection fails Lock 1. `SPEC.md` Section 8 adds a renamed-JSON-policy audit for Lock 14 preservation. | ACCEPT |

## Remaining Blockers

None for CH5.

## Residual Conditions

- This ACCEPT does not authorize W1-W6 dispatch from the final packet alone.
- W0 remains telemetry-only and must fail if any required field is emitted but
  not validated by `gate-json`.
- Any future bitmap, density, scanner, primitive, parse, direct, or typed route
  must pass the final docs' post-W0 plan, challenge, same-wave consumer, and
  no-regression gates before admission.

CH5 V2 result: ACCEPT.

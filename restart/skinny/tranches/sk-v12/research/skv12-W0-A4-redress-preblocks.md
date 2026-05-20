# SK-V12 W0-A4: REDRESS And Pre-Blocks

Date: 2026-05-20.
Scope: SK-V12 W0 read-only audit of REDRESS-adjacent constraints and
pre-blocked routes.
Output: this file.

## Section 1 - Findings

W0 is strictly a telemetry/profile/report lock. The SPEC permits no parser,
scanner, SIMD, ASM, codegen behavior, generated runtime output, or benchmark
body change in W0.

REDRESS 111 admitted only the SK-V11 W1a companion non-JSON report lane. It did
not admit a generated non-JSON baseline. REDRESS 112 and REDRESS 113 rejected or
blocked generated non-JSON baseline/intervention attempts because the runtime
and codegen remained JSON-profiled and there was no generated baseline.

REDRESS 119 and REDRESS 120 bind the current tranche against JSON row
reclamation before generated non-JSON priority is resolved. SK-V12 is routed to
solve the generated non-JSON baseline first.

## Section 2 - Recommendations

W0 should enforce no behavior-source drift and should leave `skinny/RESULTS.md`
as the SK-V11 close surface unless the gate/report lock itself requires a
metadata-only refresh. Every W0 telemetry or report field emitted by the wave
must be consumed by `gate-json` or the named non-JSON companion gate.

W0 should write a `skinny/REDRESS.md` entry only if the lock fails, matching
SPEC Section 3.

## Section 3 - Risks

Any JSON row movement, parser/source behavior change, generated parser baseline
claim, or stale sidecar strict anchor inside W0 would collapse the wave boundary
and make W1's first material target ambiguous.

## Section 4 - Sources

- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 3 and Section 8
- `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`

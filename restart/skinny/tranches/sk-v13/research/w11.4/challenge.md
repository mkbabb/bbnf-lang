# SK-V13 W11.4 CHALLENGE - Direct Cursor Byte Fetch

Date: 2026-05-22.
Disposition: ACCEPT.
Gate: `G-W11.4-JSON-DIRECT-CURSOR-BYTE`.

## CH1 Correctness

ACCEPT. The proposed replacement preserves the only observable difference in
the current `bytes.get(*cursor).copied()` shape: if `*cursor >= bytes.len()`,
return `direct_error(input, *cursor, ExpectedValue)`. After that guard, the
unchecked byte load is in bounds. Cursor movement, parse branch selection, sink
calls, and error offsets stay unchanged.

The redress patch must update both `skinny/crates/codegen/src/json_sink_direct.rs`
and `skinny/crates/runtime/src/grammars/json/generated.rs`. Template/output
divergence is REVISE.

## CH2 Generality / Lock 14

ACCEPT. This is JSON-owned generated direct-sink code, not a generic-crate
policy change. It does not add grammar directives, BIR variants,
`BackendShape` variants, public substrate APIs, or non-JSON assumptions beyond
the existing JSON direct parser. Lock 14 still requires the standard generic
scan before admit.

## CH3 Regression / REDRESS

ACCEPT with guard probes. REDRESS 119/120 are history only under the full-SOTA
pin, and REDRESS 143 tried a distinct sink-stack specialization. W11.4 names a
fresh material differential: the generated direct parser byte-fetch envelope.

The redress run must measure `instruments` and at least the W11.3 guard cluster
(`mesh`, `random`, `canada`) plus `github_events` because it is object-heavy.
Any admitted-row demotion in RESULTS or rolling delta is a fail/revert unless
recorded as measured disposition.

## CH4 Cost

ACCEPT. The source edit is below 30 behavior LOC across one renderer and one
checked-in generated file. Measurement cost dominates; the native Criterion
regex is bounded to direct lanes for five rows. No codegen-wide regen is
required before the first redress measurement because the renderer and
checked-in generated output are edited together.

## CH5 Hidden Coupling

ACCEPT. The unsafe load is tightly coupled to the immediately preceding length
guard. The risk is not memory safety if the local pattern is followed; the risk
is accidental inconsistency between direct dispatch sites. Redress must patch
all four sites or none:

- `parse_value_direct`
- `parse_object_value_at_direct`
- `parse_array_element_at_direct`
- `parse_array_direct`

No helper extraction is required; if one is introduced, it must not hide error
kind/offset differences.

## CH6 Anti-Paper-Close

ACCEPT. A source patch alone does not admit W11.4. The wave admits only on a
fresh same-host strict direct row exceeding sonic strict + 1 Mbps with Track 2
independence and gate-consumed provenance. If no row admits, the patch is
reverted and saved at `/tmp/skv13-waveW11.4-rejected.patch` with REDRESS
measurements.

## Disposition

Proceed to redress. Owner paths are limited to the plan. Unrelated dirty CSS
sidecar JSON files remain out of scope and must not be staged.

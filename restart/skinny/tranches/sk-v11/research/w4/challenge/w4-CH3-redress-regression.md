# SK-V11 W4 CH3: REDRESS Regression Challenge

Date: 2026-05-20.
Scope: CH3 regression / REDRESS lens for
`restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md`.
This is research-only except this artifact.

Disposition: **ACCEPT - D1 redress authorized under the guardrails below**.

## Read Set

- `skinny/REDRESS.md` entries 63, 65, 84, 96, 97, 98, 102, 113, and 114.
- `restart/skinny/tranches/sk-v11/research/w3/w3-plan-number-span-emit-slot.md`.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R6-grammar-neutral-compatibility.md`.
- `restart/skinny/tranches/sk-v11/research/w3/redress/w3-redress-rejection.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R6-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md`.
- Adjacent W4 research R1-R5 for owner path, oracle, gate, and floor context.

## Finding

The W4 D1 plan does not repeat a preblocked REDRESS route as written. It selects
a scalar, JSON-local direct container-tail helper over the current byte cursor,
with generated Track 1 and independently implemented Track 2 consumption on the
single selected row `random/direct_to_struct`.

The distinction is load-bearing: D1 may classify the already-current
separator/close byte after a child value and return the next local cursor. It
may not carry an object key quote, object key offset, first value byte, colon
state, parent control byte, retained cursor, class lane, or sidecar fact across a
generated direct/typed boundary.

The W4 plan's SPEC Section 8 owner correction is also accepted for CH3. W4 R1
and R2 show that `skinny/crates/codegen/src/sink_direct.rs` is the source file
that actually emits generated direct `parse_direct`. Adding that path is an
owner-list correction for the existing generated direct renderer, not a new
behavioral route or generic JSON policy expansion.

## Regression Checks

| Risk | CH3 result |
|---|---|
| REDRESS 63 -> 65 / 84 object-carry transfer | Acceptable as written. REDRESS 63 admitted only retained array next-byte carry. REDRESS 65 rejected object next-key carry, and REDRESS 84 rejected object-pair value-byte control compaction. W4 D1 is distinct only if it stops at container tail classification after a parsed value and preserves normal object key parsing, colon handling, and value dispatch. |
| W3 substrate replay | Acceptable. The plan does not introduce `UnionTape`, class columns, structural-position vectors, streaming cursors, class lanes, parser-owned projections, retained sidecars, or parse-only row movement. W4 does not name W3 as a consumer or substrate dependency, matching REDRESS 96/97/98/102. |
| Numeric laundering after REDRESS 114 | Acceptable. The selected row is `random/direct_to_struct`, and the material differential is container-tail control, not `number_span_emit_slot`, f64 fallback, mantissa widening, UDOT, digit microkernels, or parse-that number policy. The W3 `mesh` numeric evidence remains rejected and must not be reused as W4 proof. |
| Track 1 / Track 2 coupling | Acceptable with mandatory enforcement. The generated Track 1 helper may live in `sink_direct.rs`, but Track 2 must remain a local hand-parser implementation in `direct_struct.rs` and must not call generated Track 1, generated helpers, or a shared generated helper. Admission still requires both tracks above the `random/direct_to_struct` floor of 7878 Mbps. |
| Previous dispatch/function-pointer routes | Acceptable. D1 is a scalar local helper, not a 256-entry function-pointer table, benchmark-only dispatch alternate, or duplicate canonical Track 1 row. D2 `direct_slot_dispatch` is not selected in this plan. |
| W2 non-JSON block | Acceptable. REDRESS 113 remains carried forward. D1 is JSON direct-plane closure/fixpoint work only and does not claim non-JSON generated baseline authority or grammar generalization. |
| Gate/report paper close | Acceptable only if gate/report consumption lands in the same redress slice. W4 must use existing direct-contract fields plus W4 provenance, and `gate-json --with-cost-facts --check-results` must consume the selected-row floor and reject coupled/below-floor evidence. |

## Required Redress Guardrails

Redress must return to plan or record rejection if any of the following occur:

- The D1 helper consumes or carries the next object key quote, key offset, colon
  state, first value byte, or parent/container control byte.
- The implementation adds a directive, BIR variant, `BackendShape`, public
  substrate API, retained cursor, class column, event cursor, structural vector,
  byte/class sidecar, or second scanner.
- Track 2 calls generated Track 1, generated helpers, a shared generated D1
  helper, or any benchmark-private generated parser route.
- W4 edits parse-that number scanner/materializer policy, f64 fallback,
  mantissa handling, UDOT, or digit microkernels.
- W4 admits any row other than the selected `random/direct_to_struct` row
  without a new CHALLENGE-selected target set.
- `random/direct_to_struct` fails either Track 1 or Track 2 floor 7878 Mbps,
  digest/output parity, direct guard floors, or same-run comparator evidence.
- Gate/report code admits W4 via producer-only telemetry, `gate_only`
  consumer metadata, missing REDRESS provenance, stale wave id, wrong comparator
  plane/source, or floor tables that differ between producer and validator.
- `sink_direct.rs` edits leak JSON policy into generic crates or hand-patch
  generated runtime output without regeneration/check-json consistency.

## Disposition

DISPOSITION: ACCEPT.

CH3 authorizes the W4 D1 `container_tail_next` redress attempt with the plan's
single-row `random/direct_to_struct` target, the `sink_direct.rs` owner
correction, independent Track 2 implementation, and same-wave gate/report
consumption. Any object-carry, W3-substrate, numeric-slot, coupled-Track-2, or
function-pointer/table-dispatch drift converts the attempt to REJECT.

# SK-V11 W4 CH3 V2 - REDRESS Regression Challenge

Date: 2026-05-20.
Scope: CH3 regression / REDRESS lens for
`restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md`.
This is research-only except this artifact.

Disposition: **ACCEPT - V2 remains clear of the REDRESS preblocks**.

## Read Set

- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R6-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH3-redress-regression.md`.
- `restart/skinny/tranches/sk-v11/SPEC.md` Section 8.
- `skinny/REDRESS.md` entries 16, 17, 18, 25, 50, 51, 53, 63, 65, 84,
  96, 97, 98, 102, 113, and 114.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R5-numeric-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R6-grammar-neutral-compatibility.md`.
- `restart/skinny/tranches/sk-v11/research/w3/redress/w3-redress-rejection.md`.

## Finding

V2 does not introduce a CH3 regression. It preserves the CH3 v1 accepted route:
one JSON-local scalar D1 `container_tail_next` helper for the generated direct
Track 1 parser, independently mirrored in the hand direct Track 2 parser, with
the selected target set restricted to `random/direct_to_struct/main`.

The V2 changes make the REDRESS boundary clearer than v1. V2 names the exact
helper signature, constrains tail classification to post-value `,` or close,
keeps empty-container handling outside the helper, requires malformed-tail
rejection in generated Track 1, hand Track 2, serde_json, and sonic-rs, and
requires W4 provenance/floor validation to reject stale W2/W10 direct-contract
evidence.

## Regression Matrix

| Risk | V2 result |
|---|---|
| Object carry | **Clear.** V2 says the helper is sink-free, post-value only, does not handle empty containers, does not retain a cursor/sidecar, and does not carry object key/value bytes across a boundary. That preserves the CH3 distinction from REDRESS 63 and does not reopen REDRESS 65 object next-key carry or REDRESS 84 object-pair value-byte control compaction. |
| W3 substrate | **Clear.** V2 does not introduce `UnionTape`, class columns, class lanes, structural-position vectors, streaming cursors, parser-owned projections, sidecars, parse-only evidence, or a W4-through-W3 consumer. REDRESS 96/97/98/102 remain carried as preblocks. |
| Numeric laundering | **Clear.** The selected row is `random/direct_to_struct`, and the differential is container-tail control. V2 does not reuse W3 `mesh` evidence and does not edit `number_span_emit_slot`, f64 fallback, mantissa widening, UDOT, digit microkernels, or generic number policy. REDRESS 114 remains rejected and fenced off. |
| Function-pointer dispatch | **Clear.** V2 selects D1 only. It does not select D2, add a 256-entry function-pointer table, revive an alternate dispatch-table benchmark, or duplicate canonical generated Track 1 rows. REDRESS 17 and REDRESS 25 remain closed. |
| Track coupling | **Clear with mandatory enforcement.** V2 requires separate Track 1 and Track 2 helper implementations and explicitly forbids Track 2 from calling `runtime::generated_json`, generated SinkOnly helpers, `container_tail_next_direct`, or any generated Track 1 tail symbol. It also requires source-level coupling tests plus same-output generated Track 1 vs independent Track 2 digest equality before admission. |

## Conditions That Preserve ACCEPT

W4 remains CH3-clear only if redress lands within the V2 envelope:

- Generated Track 1 may factor `container_tail_next_direct` in generated
  JSON-local direct code; Track 2 must implement equivalent local hand-parser
  logic without importing or calling the generated helper.
- The helper may classify the already-current post-value tail byte and advance
  the local cursor; it may not carry object key quotes, key offsets, colon
  state, first value bytes, parent control bytes, retained cursors, sidecars,
  or class-lane facts.
- Gate/report work must consume W4 as `SK-V11-W4` / `REDRESS-115` through the
  direct contract, use the 7878 Mbps `random/direct_to_struct` floor for both
  producer and validator, reject stale W2/W10 provenance, and reject Track 2
  coupling or below-floor false accepts.
- REDRESS 113's non-JSON block remains carried forward; W4 is JSON direct-plane
  closure/fixpoint work only, not a grammar-generalization or generic-code
  proof.

## Rejection Triggers

Convert this disposition to REJECT/REDRESS if implementation drifts into any
of the following:

- object key/value byte carry, object-pair compaction, parent control-byte
  carry, or close-after-comma success;
- W3 substrate, retained sidecars, structural/class cursors, second scanners,
  parse-only row claims, or W4-through-W3 dependency;
- numeric-slot work relabeled as W4 dispatch, including `number_span_emit_slot`,
  f64 fallback, mantissa widening, UDOT, or digit microkernels;
- function-pointer/table dispatch or benchmark-only alternate rows;
- Track 2 calling generated Track 1, generated helpers, or a shared newly added
  container-tail primitive;
- row admission without both `random/direct_to_struct` tracks clearing 7878
  Mbps, malformed-tail rejection across all four parsers, direct/typed guards,
  same-run comparator evidence, and same-wave gate/report consumption.

## Disposition

DISPOSITION: ACCEPT.

V2 remains clear of object carry, W3 substrate, numeric laundering,
function-pointer dispatch, and Track coupling. CH3 authorizes proceeding to the
V2 W4 D1 redress attempt under the stated fail-closed guardrails.

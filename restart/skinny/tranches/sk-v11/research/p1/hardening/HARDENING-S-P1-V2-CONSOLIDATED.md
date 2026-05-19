# SK-V11 S-P1 Hardening V2 Consolidation

Pass: S-P1 Profile. Cycle: V2 CHALLENGE -> V3 fold.
Date: 2026-05-19.
Scope: consolidate the six-lens S-P1 V2 challenge and record the narrow
Lock-14 wording fold applied before the next cycle.

## Lens Dispositions

| Lens | Disposition | Required fold |
|---|---|---|
| CH1 correctness | ACCEPT | None. |
| CH2 generality / Lock 14 | REVISE | Residual `array-walk`, `object leaves`, and `Number/array rows` summary prose needed grammar-neutral wording. |
| CH3 regression / pre-block | ACCEPT | None. |
| CH4 cost / reproducibility | ACCEPT | None. |
| CH5 hidden coupling | ACCEPT | None. |
| CH6 anti-paper-close | ACCEPT | None. |

V2 reached 5/6 ACCEPT with one non-critical wording REVISE. No lens requested
new capture, behavior source work, row admission changes, or gate changes.

## Fold Applied

- Replaced P1-B `array-walk rows` with `number_digit_span` plus
  `sequence_element_dispatch` / `container_dispatch`.
- Replaced P1-B `object/tape traversal` and `string/whitespace/object leaves`
  summaries with `container_dispatch`, `bounded_plain_string_scan`, and
  `ascii_whitespace_skip` vocabulary.
- Replaced P1-B `numeric/array guard` with `numeric/sequence-dispatch guard`.
- Replaced P1-E `Number/array rows` and `direct number arrays` with
  `Number/sequence-dispatch rows` and `direct number sequences`.

## V3 Entry

The V3 packet remains read-only profile evidence. It preserves the V2 capture
provenance, REDRESS pre-block matrix, row classifications, gate floors,
RESULTS state, and source/capture artifacts. V3 is ready for a fresh six-lens
challenge cycle.

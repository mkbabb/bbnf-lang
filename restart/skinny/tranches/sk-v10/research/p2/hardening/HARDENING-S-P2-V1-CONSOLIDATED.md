# SK-V10 S-P2 V1 Hardening Consolidation

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: consolidate CH1-CH6 dispositions for the SK-V10 research packet.
Output: this file.

## Disposition Summary

| Lens | Initial disposition | Folded disposition |
|---|---|---|
| CH1 correctness | REVISE | ACCEPT |
| CH2 generality / Lock 14 | REVISE | ACCEPT |
| CH3 regression / REDRESS | ACCEPT | ACCEPT |
| CH4 cost / micro-proof | REVISE | ACCEPT |
| CH5 hidden coupling / Lock 1 | REVISE | ACCEPT |
| CH6 anti-paper-close | REVISE | ACCEPT |

Final: ACCEPT, 6/6 after fold. No open critical defect. No unresolved REVISE.

## Folded Corrections

- Added `p2g-candidate-ledger.md` as the canonical S-P2 candidate and
  micro-proof ledger. S-P3 may shortlist only ledger-named candidates; aliases
  outside the ledger are inventory-only.
- Normalized candidate dispositions into `row-gated`, `proof-only`,
  `gate-only`, `inventory-only`, and `rejected`.
- Demoted non-current-host x86 instruction routes and broad ISA inventory to
  `inventory-only` for SK-V10 on Apple aarch64 unless S-P3 supplies a future
  same-host profile and instruction-specific anchors.
- Demoted `mask_next_and_emit_positions_64`, CSSC/CTZ default bulk emission,
  PMULL/VPCLMUL default prefix-XOR, structural cursor, W3/union, and Canada
  typed shortcuts to rejected or REDRESS-blocked inventory.
- Reframed P2-E generic APIs around caller-owned byte sets, class tables,
  offsets, digit accumulators, and policy structs. JSON quote/slash/`\u`,
  surrogate, number, whitespace, output, and row semantics now belong to
  generated per-grammar templates.
- Repaired P2-D Lock 1 wording: capacity pre-scans are diagnostic/env-only for
  SK-V10 row movement; consumer planes are retained `TapeBuilder`, generated
  direct `JsonSink`, real typed `DirectParser`, or independent hand Track 2,
  and those planes are not interchangeable.
- Repaired P2-B/P2-C/P2-D/P2-F traceability language for P1 anchors, gate-only
  telemetry, current-host eligibility, and DAV1D checkasm process source
  anchors.

## Validation

The fold is documentation-only and does not authorize source implementation.
Validation performed before commit:

```sh
rg -n "Cycle: V10|same sink event stream|master raw source|future x86 host wave|S-P3 would need" restart/skinny/tranches/sk-v10/research/p2/p2*.md
git diff --check
```

The remaining matches in hardening files are historical CHALLENGE findings, not
unfolded P2-A through P2-G source text.

## S-P3 Authorization

S-P2 V1 is accepted for S-P3 input. S-P3 must consume
`p2g-candidate-ledger.md` as the candidate-pool authority, then produce
`SPEC.md` and `DISPATCH-PROMPT.md` with measurable row gates, owner paths,
same-wave consumers, redress protocols, and pre-blocked routes.

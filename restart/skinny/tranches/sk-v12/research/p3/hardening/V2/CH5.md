# SK-V12 S-P3 V2 CH5 - Hidden Coupling

Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Date: 2026-05-20.
Lens: CH5 hidden coupling.
Scope: read-only adversarial review of SK-V12 S-P3 V2 packet.

## Verdict

ACCEPT.

## Findings

1. SPEC and DISPATCH explicitly block W3 substrate routes, parser-owned
   structural projections, retained cursors/lists, aux density/projection
   tables, event side vectors, whitespace bitmaps, retained class lanes,
   structural-position vectors, decoded-byte sidecars, and renamed scanners that
   retain facts outside the single tape/direct sink contract.
2. Provider/template escape hatches are closed: allowed grammar-specific inputs
   are limited, templates must remain grammar-neutral, providers/templates cannot
   carry handwritten parser policy, and host/API facts cannot supply parser
   control, generated Track 1 output, or admission shortcuts.
3. Track 1/Track 2 coupling is fail-closed.
4. Hand witness/report routes cannot be hidden as generated baselines.

## Required Folds

None.

## Residual Risk

Implementation-time W1 edits to `json_provider.rs` must remain narrowly scoped
to removing the JSON-only emission gate. The packet states that constraint.

# SK-V15 W9 Challenge: Remaining Lowerers And All-Five Gate

Status: ACCEPT.
Date: 2026-05-28.
Plan: `skv15-W9-plan.md`.

## CH1 Correctness

ACCEPT. The plan replaces the two remaining label-string lowerers with
expression-derived runtime-plan output and requires exact tests that fail the
old `rule X -> event_tape/collapsed_stage` scaffold.

## CH2 Generality

ACCEPT. The all-five gate consumes the canonical shape list rather than a
grammar-specific shortcut. The plan does not add a BackendShape variant or
grammar-family lowerer branch.

## CH3 Regression

ACCEPT. W9 does not reopen CSS admission, Pattern H provenance, or W10 FNV
quarantine. It only consumes `DEP-W9-LOWERERS-B`.

## CH4 Cost

ACCEPT WITH BOUND. A full production runtime emitter for every retained-tape
shape is outside this wave. The W9 obligation is runtime-relevant lowerer
output or a gate-consumed rejection, plus the all-five gate.

## CH5 Hidden Coupling

ACCEPT. EventTape is explicitly constrained to a canonical lowerer over the
existing tape. The plan rejects sidecar vectors, retained parser streams,
public substrate APIs, alternate document projections, public `UnionTape`, and
new/sixth `BackendShape` surfaces.

## CH6 Next-Wave Impact

ACCEPT. W10 inherits FNV quarantine only after W9 proves the lowerer gate.
W11 receives a consumed `DEP-W9-LOWERERS-B` row or a redressed blocker.

## CH7 Overfit-Prune

ACCEPT. The fixtures exercise lowerer structure and report consumption, not
benchmark rows or throughput thresholds. No SOTA claim is made from W9.

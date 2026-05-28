# SK-V15 W8 Challenge: EagerTape And OffsetTape Lowerer Plan

Status: ACCEPT.
Date: 2026-05-28.
Plan: `skv15-W8-plan.md`.

## CH1 Correctness

ACCEPT. The plan names exact fixture commands and replaces the scaffold with a
walk over `BackendExpr`, which can change with runtime-relevant grammar
structure.

## CH2 Generality

ACCEPT. The renderer is shape-generic for EagerTape/OffsetTape and does not
introduce a new BackendShape, substrate API, or grammar-named special case.

## CH3 Regression

ACCEPT. The plan does not reopen CSS admission, W8R numbers, or Decision Engine
scope. W7 remains the source of selected shape facts.

## CH4 Cost

ACCEPT WITH BOUND. W8 is not the all-five lowerer wave. If EventTape,
SinkOnly, or CollapsedStage gaps surface, route them to W9 rather than
expanding W8.

## CH5 Hidden Coupling

ACCEPT. A full runtime generator is not required for W8 if the generated
fixture proves `lower_to_rust` output changes from expression-derived
operations. Hand-patched generated files would reject.

## CH6 Next-Wave Impact

ACCEPT. W9 inherits the remaining lowerers and all-five gate; W8 should leave
those surfaces untouched.

## CH7 Overfit-Prune

ACCEPT. The plan uses tiny structural grammars only as lowerer fixtures. It
does not tune benchmark rows or admission thresholds.

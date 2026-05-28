# SK-V15 W7 Challenge: Decision Engine Spine Plan

Status: ACCEPT.
Date: 2026-05-28.
Plan: `skv15-W7-plan.md`.

## CH1 Correctness

ACCEPT. The plan names exact tests and binds the rewrite to extraction, not
metadata. The CSP predicate has an explicit SAT/UNSAT falsifier.

## CH2 Generality

ACCEPT. The rewrite keys on generic backend shape and rationale fields. It does
not introduce a grammar-specific fact, a CSS/JSON row switch, or a sixth
BackendShape.

## CH3 Regression

ACCEPT. `codegen::lower::rust::lower_to_rust` already fail-closes on missing
active-cost and CSP facts, so W7 strengthens an existing consumer instead of
adding an advisory parallel path.

## CH4 Cost

ACCEPT WITH BOUND. The implementation is confined to decision facts, passes,
and a codegen unit fixture. If lowerer implementation gaps surface, route them
to W8/W9 rather than expanding W7 into backend lowerer work.

## CH5 Hidden Coupling

ACCEPT. The generated-selection fixture must compare two valid generic target
fact sets for the same grammar. Hand-editing cost facts to force a body change
would reject.

## CH6 Next-Wave Impact

ACCEPT. W8 depends on W7 because lowerer fixtures need a non-advisory decision
spine. W7 closes only when the decision output can alter generated selection.

## CH7 Overfit-Prune

ACCEPT. The plan removes grammar-named status records from the decision record
and blocks any test that passes solely because it mentions JSON or CSS.

# SK-V14 W5B.1 Challenge V1 Consolidated

Date: 2026-05-26.
Scope: W5B.1 request-local import closure after initial plan and first source
trial.
Disposition: REVISE, folded into `skv14-W5B1-plan.md`.

## Lens Result

CH1 REVISE. The frontend output must be a cohesive closure surface, and resolved
imports must stay countable but no longer be reported as unsupported.

CH2 ACCEPT. Request path identity and source hashing remain local to the
provided source map.

CH3 REVISE. Strict closure is correct; the existing W5A runtime request test
fixture must provide the imported source rather than weakening fail-closed
behavior.

CH4 ACCEPT. W5B.1 remains inside W5B-FRONTEND and does not advance W5C, W5D, or
W6 work.

CH5 REVISE. The import parser must avoid immediate-string overfit and avoid APIs
newer than the skinny workspace MSRV.

CH6 REVISE. The W5A runtime contract becomes a fourth dedicated proof command.

CH7 ACCEPT. The route prunes filesystem lookup, unresolved-import tolerance, and
codegen behavior claims before W5B.4.

## Redress Dispatch Contract

Redress may edit `skinny/crates/grammar/src/lib.rs` plus fixture-only W5A runtime
request test code in `skinny/crates/codegen/src/lib.rs`. It must not edit
provider/template files, generated runtime files, xtask dispatch, rolling delta
artefacts, or grammar-provider generation behavior. Four exact proof commands
and four nonzero log greps are required before admit.

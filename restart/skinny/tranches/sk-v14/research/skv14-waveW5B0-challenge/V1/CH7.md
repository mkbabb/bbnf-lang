# SK-V14 W5B.0 CH7: Overfit-Prune

Date: 2026-05-26.
Scope: W5B.0 overfit-prune review.
Disposition: REVISE.

## Findings

The current W5B.0 state is docs-only. `lock14_baseline.rs` has no
`SK_V14_W5B_FRONTEND_OWNER_PATHS`, no W5B-FRONTEND parent-diff route, and none
of the exact `w5b_lock14_frontend_*` tests.

The topology guard is overfit to CSS L4 template directories. W5B.0 must guard
all `*_templates` directories, including `json_templates`, before source
frontend work starts.

No new fixture lookup, fake generated-header output, public `@ws` revival, or
new grammar-name branch leakage was found. The blocker is the absent Lock14
intervention.

## Required Folds

- Add `SK_V14_W5B_FRONTEND_OWNER_PATHS` and extend
  `current_lock14_owner_paths()`.
- Route W5B-FRONTEND subjects and keep W5C/W5D rejected.
- Replace CSS-only template counting with all-template counting.
- Reject modified provider/template paths while preserving `grammar_provider.rs`.
- Add all eight exact tests and dedicated per-test proof.

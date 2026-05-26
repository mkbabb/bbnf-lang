# SK-V14 W5B.0 CH2: Generality

Date: 2026-05-26.
Scope: W5B.0 Lock 14 generality review.
Disposition: REVISE.

## Findings

The current Lock 14 implementation does not generalise the topology guard far
enough for W5B. The template census is CSS-only and expects seven directories,
missing `json_templates`. The protected provider census is already grammar-
neutral if `grammar_provider.rs` remains excluded.

The status guard still rejects untracked, added, deleted, and renamed protected
provider/template paths but permits modifications. W5B.0 must close that hidden
coupling before W5B.1 source work begins.

## Required Folds

- Add `SK_V14_W5B_FRONTEND_OWNER_PATHS` and include it in
  `current_lock14_owner_paths()`.
- Route W5B-FRONTEND parent-diff subjects while preserving rejection for W5C and
  W5D subjects.
- Count all eight `*_templates` directories, including `json_templates`.
- Reject modified provider/template paths, with `grammar_provider.rs` as the
  sole provider exception.
- Add the eight exact W5B.0 tests required by `SPEC.md`.

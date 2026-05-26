# SK-V14 W5B.0 CH5: Hidden Coupling

Date: 2026-05-26.
Scope: W5B.0 hidden-coupling review.
Disposition: REVISE.

## Findings

The V8/SPEC authority is clear, but the implementation is still pre-W5B. The
hidden coupling remains in the provider/template guard: modified provider and
template files can still pass before W5C-GEN and W5D-DELETE own those surfaces.

The template census is also under-general because it only counts CSS L4 template
directories and misses `json_templates`.

## Required Folds

- Add W5B-FRONTEND owner and parent-diff routing, with explicit W5C and W5D
  rejection tests.
- Reject modified protected providers/templates while preserving
  `grammar_provider.rs`.
- Prove all eight `*_templates` directories are counted.
- Keep W5B.0 source-scope clean: no grammar, codegen, or xtask frontend source
  edits in this gate.

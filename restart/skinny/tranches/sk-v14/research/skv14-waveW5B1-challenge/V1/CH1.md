# SK-V14 W5B.1 CH1 V1: Contract Coherence

Date: 2026-05-26.
Scope: W5B.1 import-closure plan and first redress shape.
Disposition: REVISE.

## Findings

The plan correctly keeps import closure request-local, but its frontend output
surface is under-specified. Loose `source_hashes` and `import_edges` fields are
enough for the positive proof, but W5B.4 needs a coherent closure object that
keeps each edge's original specifier, importer hash, resolved path, and resolved
target hash together.

Resolved imports should also stop surfacing as
`BBNF-UNSUPPORTED-IMPORT-RESOLUTION`. Missing and cyclic imports fail closed
before facts return; a resolved import remains a materiality fact for
`validate_non_json_materiality`, not an unsupported construct.

## Required Folds

- Expose a cohesive frontend closure surface from `RuntimeSourceFacts`.
- Keep `RuntimeConstructKind::Import` countable.
- Remove resolved imports from `first_unsupported()` output.

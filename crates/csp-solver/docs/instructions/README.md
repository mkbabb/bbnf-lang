# Instructions

`crates/csp-solver` inherits shared orchestration through the bbnf-lang
top-level `docs/precepts/` submodule. From this directory, that shared canon
is at `../../../../docs/precepts/instructions/`.

## Local Rules

- Role: bbnf-lang CSP/COP substrate. No separate top-level consumer surface.
  No nested `docs/precepts` submodule.
- Use bbnf-lang local rules at `../../../../docs/instructions/README.md` for
  cargo target-dir discipline, nextest, profiling, and tranche protocol.
- Solver changes need focused crate tests plus the bbnf leaf test tier when
  they affect workspace behavior.
- Current consumer gap: sibling benchmark/dev-dependency coverage remains
  local tranche work, not shared precepts work.

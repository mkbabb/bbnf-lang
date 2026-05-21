# SK-V12 W1b-2 CH5 - Hidden Coupling

Verdict: REVISE.

Blockers:

- The independent source scanner is underspecified. The W1b-1 oracle fact
  stream depends on tokenization, nested block parsing, `!important` parsing,
  token-start recovery, and value-span boundaries. Forbidding direct cssparser
  calls is not enough to prevent a cssparser semantics clone.
- lightningcss can reorder normal and important declarations, while the current
  fixture does not exercise important/normal interleaving in one block.
- Nested traversal order is not proven. The cssparser oracle walks
  declarations and qualified rules recursively, while lightningcss has a richer
  rule enum including nested declarations.
- REDRESS must state fixture limits: seven declarations, thirteen tokens, one
  nested `@media`, no declaration after an important declaration in the same
  block, no comments in values, no strings/URLs/custom properties, and no
  duplicate-property cascade cases.

Required revision:

- Either add adversarial fixture coverage for important/normal interleaving and
  nested traversal, or explicitly limit W1b-2 claims to the frozen fixture shape
  and block any broader CSS SOTA claim from this comparator.

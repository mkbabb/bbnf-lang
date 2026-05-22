# SK-V13 W5 Redress - Regex Extraction + Decision Gate

Disposition: PASS with measured architectural block.

W5 lands `bbnf-regex` as an analysis-only crate and consumes it from both
`ir::nullability` and `passes` decision helpers. The extracted facts cover
nullable, first-set, byte-class, quoted-string, numeric, whitespace, and
unknown-regex behavior. Unknown first sets now fail closed for dispatch
disjointness instead of being skipped as non-overlapping.

The same-wave gate report records the named generated selection path
`passes::recognizers::derive_backend_shape_with_diagnostics`. It also records
the measured architectural block
`JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH`: W5 facts are consumed
by IR/passes, but the current generated JSON/CSS selection machinery has no
row-moving production selection that can consume regex facts alone. This is a
material differential from REDRESS 119/120, not a support-only admit.

Evidence:

- Fact artifact:
  `restart/skinny/tranches/sk-v13/research/w5/regex-facts.json`.
- Gate report:
  `restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json`.
- Fact artifact SHA-256:
  `0bbb10d28ec754a432e4ecae96de336fc6f3ea032276e10415e9d486c0c6be49`.

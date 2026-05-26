# SK-V14 W5B.2 CH5 V1: Hidden Coupling

Date: 2026-05-26.
Scope: W5B.2 public-syntax and scanner hidden-coupling review.
Disposition: REVISE.

## Findings

`@ws` remains retired through `parse_directive`, but `?w` has a hidden parser
coupling: the public parser can otherwise treat `ident ?w` as optional `ident`
plus a reference named `w`. That would make W5B.2's public-retirement proof
porous.

The scanner also needs explicit malformed checks for `?word`, leading `?w`, and
triple discard operators rather than merely counting prefixes.

## Required Folds

- Add a public-parser guard for `?w`-prefixed compatibility syntax.
- Fail closed on malformed whitespace modifiers and malformed discard
  operators in the runtime scanner.

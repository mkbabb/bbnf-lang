# SK-V14 W5B.1 CH5 V1: Hidden Coupling

Date: 2026-05-26.
Scope: W5B.1 import parser and compiler-version coupling.
Disposition: REVISE.

## Findings

Two hidden couplings need folds before source commit.

First, the import parser cannot assume the target quote immediately follows
`@import`. CSS L4 direct imports need that path, but future BBNF closure facts
also encounter directive forms with quoted targets after intervening syntax.
The W5B.1 parser should find the quoted target inside the directive without
introducing public syntax expansion or filesystem lookup.

Second, the redress used `Option::is_none_or`, which is newer than the skinny
workspace MSRV. W5B.1 must remain compatible with the workspace Rust version.

## Required Folds

- Parse the quoted import target inside the directive span rather than assuming
  an immediately following string.
- Avoid APIs newer than the skinny workspace MSRV.

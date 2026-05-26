# SK-V14 W5B.2 CH3 V1: Regression

Date: 2026-05-26.
Scope: W5B.2 regression review against W5A and W5B.1.
Disposition: REVISE.

## Findings

The initial plan carried W5A runtime and W5B.1 import graph checks, but the
consumer review identified two additional low-cost W5A witnesses that should
stay green: JSON unchanged output and Sheets/BBNF-self fail-closed behavior.

## Required Folds

- Add W5A grammar source-fact, JSON unchanged-output, and Sheets/BBNF-self
  fail-closed carries to the W5B.2 proof list.

# SK-V14 W5B.1 CH6 V1: Executable Verification

Date: 2026-05-26.
Scope: W5B.1 proof commands and log contract.
Disposition: REVISE.

## Findings

The three grammar proof commands are measurable and run as top-level exact test
selectors. CH3 adds one carry proof: the existing W5A runtime contract must pass
with a request-local imported source in its fixture.

## Required Folds

- Add the W5A codegen runtime contract command to the W5B.1 proof gate.
- Keep the dedicated `/tmp/skv14-w5b-<test-name>.log` plus nonzero `rg` evidence
  requirement for all four commands.

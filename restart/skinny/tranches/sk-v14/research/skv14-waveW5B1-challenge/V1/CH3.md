# SK-V14 W5B.1 CH3 V1: Regression

Date: 2026-05-26.
Scope: W5B.1 strict closure against existing W5A runtime request tests.
Disposition: REVISE.

## Findings

Strict request-local import closure is required by SPEC §8B W5B.1, and the
local proof correctly rejects `@import "tokens.bbnf"` when the request source
map omits `tokens.bbnf`. That exposes a W5A test-fixture regression:
`w5a_runtime_contract_consumes_source_and_metadata` builds a CSS request with an
import but only one request source.

Production CSS requests already route through source maps, so the fix is not to
weaken closure. The W5A fixture must supply the imported source under the
existing W5B-FRONTEND codegen test owner path, with no change to codegen
generation behavior.

## Required Folds

- Allow `skinny/crates/codegen/src/lib.rs` fixture-only edits in W5B.1.
- Carry the W5A runtime contract as a dedicated proof after W5B.1 source work.

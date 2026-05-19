# SK-V10 Alpha CH2 Generality And Lock 14

Date: 2026-05-19.

Scope: grammar generality, Lock 14, and non-JSON proof posture.

## Disposition

REVISE -> ACCEPT after fold.

## Findings

1. The Alpha contract routed non-JSON generalization to Totality T-P1, but did
   not make Lock 14 a refusal gate.
   Fold: `SYNTHESIS.md`, `HANDOFF.md`, Alpha-E, and Alpha-F now refuse
   generic-crate, codegen, or runtime-outside-json edits that leak JSON policy
   or lack named CSS L4 / Sheets / BBNF-self proof.
2. JSON-only wins were not explicitly bounded as product-plane JSON wins rather
   than generator-thesis proof.
   Fold: Alpha-E now states that JSON-only wins do not prove the generator
   thesis and that non-JSON proof remains binding.

## Result

The candidate set is JSON-frontier work only. It is not allowed to weaken the
grammar-neutral contract or use JSON-only success as totality evidence.

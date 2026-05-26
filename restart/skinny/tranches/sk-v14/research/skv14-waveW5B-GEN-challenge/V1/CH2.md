# SK-V14 W5B-GEN CHALLENGE V1 CH2 Generality

Date: 2026-05-26.
Lens: CH2 Generality.
Disposition: REVISE.

## Findings

The W5B-GEN rejection is sound: current production emission is provider-backed,
and CSS L4 source is only fact-scanned, not compiled into IR.

The proposed V7 split needs a generality fold. As written, W5B-FRONTEND is
framed as a CSS L4 grammar-source frontend, and W5C-GEN says it consumes
frontend IR for CSS L4 plus the existing JSON source path. That risks a CSS/JSON
sidecar split instead of the Lock 14 model: grammar source plus workspace
metadata with zero grammar-specific branches in generic crates.

## Required Fold

- Reframe W5B-FRONTEND as a generic BBNF grammar-source frontend/import/IR
  closure for the missing construct classes, with CSS L4 as the strict positive
  witness rather than the architectural owner.
- Reframe W5C-GEN as consuming the same generic frontend/request IR for all
  admitted grammars.
- Preserve Sheets/BBNF fail-closed or generated-role witnesses in the V7 route.
- Add explicit Lock 14 owner-path and parent-diff routing for the new W5B/W5C
  work, since the current gate only routes through W5A.

## Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:31`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-A-codegen-reachability.md:15`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-C-request-facts.md:21`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:52`
- `restart/locks/LOCKS.md:349`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-D-verification-gates.md:20`

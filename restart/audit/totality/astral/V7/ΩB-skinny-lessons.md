# Omega-B Skinny Lessons - Pass Omega V7 W5B-GENR

Date: 2026-05-26.
Scope: REDRESS-209, REDRESS-210, REDRESS-211, W5A admit, V5/V6 signoffs, and
W5B-GEN challenge V2/V3.
Disposition: ACCEPT.

## Lesson

V7's skinny lesson is narrower than V5 and V6: a source-consuming request
boundary is not a source frontend, and a provider-free generator cannot exist
until there is generic frontend/import/IR closure for the grammar source it must
consume.

| Evidence | Lesson | Surface impact |
|---|---|---|
| REDRESS-209 | Deletion/static centralization cannot stand in for rebuild capability. | Preserve rebuild-before-delete. |
| W5A admit `286233fa2` | Source and metadata enter the request; provider/template mesh remains. | Preserve W5A as admitted without overclaim. |
| REDRESS-210 + V6 | Request boundary does not mean runtime bytes are provider-free. | Split deletion behind generator body. |
| REDRESS-211 + V2/V3 challenge | Generator body still cannot admit because generic parser/frontend cannot compile CSS L4 constructs into IR. | Split W5B-GEN again. |

## Required Reflection

The corrected chain is:

1. W5A: admitted request boundary.
2. W5B-FRONTEND: generic BBNF frontend/import/IR closure, with CSS L4 as the
   strict positive witness.
3. W5C-GEN: provider-free runtime generator consuming request/frontend IR.
4. W5D-DELETE: provider/template deletion and Lock 14 baseline close.
5. W6: root-runtime collapse only after W5D-DELETE.

W5C-GEN is blocked until W5B-FRONTEND admits. W5D-DELETE is blocked until
W5C-GEN admits. W6, W7, and W8/W9/W10 remain blocked until the amended PRUNE
chain closes.

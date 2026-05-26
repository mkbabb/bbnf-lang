# Omega-A Coherence Audit - Pass Omega V6 W5BR

Pass: Pass Omega V6.
Date: 2026-05-26.
Scope: REDRESS-210 / W5BR amended SK-V14 wave graph.
Boundary: audit only; no V1 or SK-V14 dispatch surface is edited by this
artifact.

## Verdict

ACCEPT-WITH-REQUIRED-SURFACE-AMENDMENTS.

REDRESS-210 is coherent. W5A admitted a request boundary, not a provider-free
runtime generator body. W5B's current SPEC asks the campaign to delete
load-bearing provider/template surfaces before that body exists. The correction
is a wave-graph split: provider-free generator body first, deletion and Lock 14
post-W5 close second.

## Evidence

- W5A admit commit: `286233fa2`.
- W5B research commit: `2aab62ff6`.
- W5B reject/corrective commit: `f789b6e4a`.
- `skinny/REDRESS.md` item 210 records W5B as rejected under the current SPEC.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-plan.md` rejects
  deletion before provider-free generation exists.
- `restart/skinny/tranches/sk-v14/research/skv14-W5BR-corrective-packet.md`
  proposes W5B-GEN then W5C-DELETE.
- W5B research A-D names the provider roster, dispatch mesh, Lock 14 baseline
  requirements, and grep defect.

## Coherence Check

The V5 amendment was locally correct but incomplete. It split source-consuming
request capability from deletion, but W5A's admitted request still delegates to
`render_runtime_profile` internally. That means W5B cannot safely delete:

- `skinny/crates/codegen/src/css_l4_*_provider.rs`;
- `skinny/crates/codegen/src/json_provider.rs`;
- `skinny/crates/codegen/src/css_l4_*_templates/`;
- any JSON template residue still needed by `json_provider.rs`.

The proposed V6 graph is acyclic:

```text
W0 -> W1 -> W2 -> W3 -> W4 -> W5A
  -> W5B-GEN provider-free generator body
  -> W5C-DELETE provider/template deletion + Lock 14 close
  -> W6.0..W6.8 -> W7 -> W8/W9/W10 -> W11
```

W8/W9/W10 remain globally blocked until PRUNE-1 through PRUNE-5 close.

## Required Surface Amendments

| Surface | Disposition |
|---|---|
| `restart/locks/LOCKS.md` | Read/no-op. Lock 14 already requires this shape. |
| `restart/MASTER-PLAN.md` | Required. Split W5B into W5B-GEN and W5C-DELETE; update W6 dependency. |
| `restart/skinny/tranches/sk-v14/SPEC.md` | Required. Add W5C section, repair W5B entry/exit, move deletion gates, repair grep syntax. |
| `restart/skinny/tranches/sk-v14/SYNTHESIS.md` | Required. R3/C-1/P-6 wording must include provider-free generator body before deletion. |
| `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` | Required. R3 PRUNE-3 split becomes W5A/W5B/W5C. |
| `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` | Required. Add W5BR guard and next dispatch directive. |
| `restart/HANDOFF.md`, `restart/MIGRATION.md` | Required. Record REDRESS-210 and route next move to W5B-GEN after G-Omega V6. |
| `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` | Required limited alignment. BENCH/SUBSTRATE read/no-op unless drift is found. |

## Gate Binding

Until G-Omega V6 authorizes W5BR, do not patch dispatch surfaces and do not
delete provider or template surfaces.

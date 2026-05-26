# Omega-F Migration And Handoff - Pass Omega V8 W5B-FRONTENDR

Date: 2026-05-26.
Scope: migration, handoff, and dispatch reroute after REDRESS-212.
Disposition: ACCEPT-WITH-PATCH, G-Omega gated.

## Verdict

V8 should record REDRESS-212: W5B-FRONTEND is rejected under the V7 one-wave
SPEC shape, not because the frontend/import/IR target is wrong, but because the
required closure does not fit one capped W5B-FRONTEND redress slot. The V2
challenge left five orphan REVISEs, with the load-bearing defects in cap
accounting, Lock 14 first-checkpoint isolation, all-template/provider guards,
and maintain-proof authority.

Pass Omega V8 formalizes W5B-FRONTEND as W5B.0 through W5B.4. The next
executable SK-V14 dispatch becomes W5B.0 LOCK14-GATE. W5C-GEN, W5D-DELETE, W6,
W7, and W8/W9/W10 remain blocked until their amended predecessors close.

## Migration Impact

No public API, architecture, lock-count, BackendShape, source, generated output,
gate output, `RESULTS.md`, or rolling-delta migration lands in Omega-F V8. This
is a wave-graph and cap-accounting migration inside the V7-authorized
W5B-FRONTEND scope.

The implementation migration is future wave-local and splits the remaining
PRUNE-3B frontend closure:

| Wave | Migration receiver | Entry | Close condition |
|---|---|---|---|
| W5B.0 LOCK14-GATE | Add the W5B-FRONTEND owner-path roster, parent-diff subject routing, modified-provider/template rejection tests, all-template guard, and generic owner-path leak census. No grammar/codegen/xtask frontend edits. | W5A admitted, REDRESS-211 routed, V7 CRUD applied, REDRESS-212 recorded, and V8 CRUD/G-Omega applied. | Lock 14 admits only W5B paths; W5C/W5D subjects reject; provider/template modifications reject; all `_templates` paths are guarded; owner-path leak census passes. |
| W5B.1 IMPORT-CLOSURE | Add request-local import DAG resolution from request source maps, stable source hashing, missing-import fail-closed behavior, and import-cycle fail-closed behavior. | W5B.0 admitted. | Exact grammar tests prove import graph resolution, missing-import rejection, and import-cycle rejection; no public syntax, provider/template, or generator-body change. |
| W5B.2 LAYOUT-DISCARD | Lower `@ws`, `?w`, `>>`, and `<<` into request-local frontend facts without reviving public syntax. | W5B.1 admitted. | Exact grammar tests prove layout/discard lowering and public-retirement/fail-closed behavior; no provider/template topology change. |
| W5B.3 PRETTY-SPAN-PROJECTION | Lower `@pretty`, `@{...}` span capture, `->` projection metadata, and typed projections into request-local facts. | W5B.2 admitted. | Exact grammar tests prove lowering and malformed-input rejection; no new public directive, BIR variant, BackendShape variant, or substrate category. |
| W5B.4 REQUEST-CONSUMER | Consume the frontend closure through `emit_runtime_from_request`, preserve W5A JSON/Sheets/BBNF proof carry, run `regen-css` plus the seven CSS companions, and capture maintain proof. | W5B.3 admitted. | Same-commit consumer evidence closes W5B-FRONTEND: frontend closure is consumed before provider rendering; JSON unchanged-output proof holds; Sheets/BBNF-self proof holds; `regen-css` and all CSS companions pass; provider/template counts remain unchanged. |

Each W5B sub-wave carries the normal redress cap. The aggregate W5B-FRONTEND
budget is capped across W5B.0 through W5B.4 and may not borrow from W5C-GEN,
W5D-DELETE, W6, W7, or new-admit waves. W5B.0 through W5B.3 are not W5B close;
W5B-FRONTEND closes only after W5B.4 admits with same-commit consumer evidence.

The W5B maintain rule should be amended for this non-admit capability sequence:
exact no-diff proof on `skinny/RESULTS.md`,
`restart/skinny/ROLLING-SOTA-DELTA.md`, generated runtime outputs, and protected
grammar/source inputs is the controlling maintain evidence. `gate-json
--skv14-existing-results-capture` remains schema/freshness evidence only. If
Omega V8 retains the older +/-1.0% full-table wording, W5B.4 must instead run
fresh SK-V14-open maintain evidence; prose substitution is not sufficient.

## Handoff Directive

After V8 G-Omega closure and CRUD:

1. Dispatch W5B.0 LOCK14-GATE research, plan, challenge, and redress.
2. Do not dispatch W5B.1 until W5B.0 admits.
3. Do not dispatch W5B.2 until W5B.1 admits.
4. Do not dispatch W5B.3 until W5B.2 admits.
5. Do not dispatch W5B.4 until W5B.3 admits.
6. Do not treat W5B-FRONTEND as closed until W5B.4 admits.
7. Do not dispatch W5C-GEN until W5B-FRONTEND closes.
8. Do not dispatch W5D-DELETE until W5C-GEN closes.
9. Do not dispatch W6 until W5D-DELETE closes.
10. Preserve REDRESS-209, REDRESS-210, REDRESS-211, and REDRESS-212 as
    pre-blocked routes.

## G-Omega And CRUD Implications

G-Omega V8 is mandatory before any V8 wave-graph amendment lands in authority
surfaces. Until G-Omega closes, W5B.0 is not executable, W5B source redress
remains blocked, and the V7 one-wave W5B-FRONTEND shape remains the last applied
authority even though REDRESS-212 has rejected it.

Proposed CRUD receiver map after G-Omega V8:

| CRUD | Surface | V8 operation |
|---|---|---|
| CRUD-1 ARCHITECTURE | `restart/ARCHITECTURE.md` | Read/no-op unless Omega-C finds an unexpected public syntax, substrate, BackendShape, or architecture change. Current evidence requires none. |
| CRUD-2 MASTER-PLAN + SK-V14 SPEC authority | `restart/MASTER-PLAN.md`, `restart/skinny/tranches/sk-v14/{SPEC,SYNTHESIS,ORCHESTRATOR-PROMPT,DISPATCH-PROMPT}.md` | Replace one-slot W5B-FRONTEND with W5B.0..W5B.4, entry/exit gates, aggregate cap semantics, maintain-proof wording, and W5B close semantics. Dispatch prompt must name W5B.0 LOCK14-GATE first and forbid W5B.0..W5B.3 from unblocking W5C-GEN. |
| CRUD-3 LOCKS | `restart/locks/LOCKS.md` | Read/no-op unless Omega-C finds a Lock 14 amendment need. Preserve 16 locks and the five-shape BackendShape canon. |
| CRUD-4 HANDOFF + MIGRATION | `restart/HANDOFF.md`, `restart/MIGRATION.md`, `restart/skinny/tranches/sk-v14/HANDOFF.md` | Record REDRESS-212 / W5B-FRONTENDR, name W5B.0 LOCK14-GATE as next dispatch, and update blocks for W5C-GEN, W5D-DELETE, W6, W7, and W8/W9/W10. |
| CRUD-5 SKINNY CORPUS | `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` | Limited active-authority alignment if these surfaces still describe W5B-FRONTEND as a one-shot wave. BENCH and SUBSTRATE remain read/no-op unless drift is found. |
| CRUD-6 AUDIT + CLEANUP | `restart/audit/totality/astral/V8/{CRUD-LOG,G-OMEGA-SIGNOFF}.md` | Record G-Omega closure, CRUD operations, and verification that no source/generated/RESULTS/rolling-delta movement occurred during V8 CRUD. |

V8 CRUD may update spec, master-plan, handoff, migration, dispatch, and limited
skinny-corpus text only after G-Omega. It does not authorize W5B frontend source
edits directly; W5B.0 redress must first make the amended Lock 14 gate
executable and admitted.

## Pre-Dispatch Guards

Before W5B.0: verify Pass Omega V8 G-Omega closed, V8 CRUD landed,
REDRESS-212 is recorded, and authority surfaces name
`W5B.0 -> W5B.1 -> W5B.2 -> W5B.3 -> W5B.4 -> W5C-GEN -> W5D-DELETE -> W6`.

Before any W5B frontend/codegen/xtask source owner path changes: verify W5B.0
has admitted the Lock 14 owner-path roster, parent-diff subject routing,
modified-provider/template rejection tests, all-template guard, and generic
owner-path leak census.

Before W5C-GEN: verify W5B.4 admitted same-commit consumer evidence for import
closure, layout/discard lowering, pretty/span/projection lowering, W5A proof
carry, exact maintain proof or SPEC-authorized fresh full-table maintain, and
provider/template no-change proof.

Before W5D-DELETE: verify W5C-GEN admitted the provider-free runtime generator
body, provider-reachability greps, `regen-css`, all seven CSS companions,
`check-json`, JSON unchanged-output proof, and Sheets/BBNF-self proof.

Before W6: verify W5D-DELETE admitted provider/template deletion, old provider
dispatch/registry/template retirement, Lock 14 baseline close, and the required
post-delete proof bundle.

## Forward Lenses

- CH2-V8: require the Lock 14 leak census over all W5B generic owner paths and
  public-retirement tests for the full compatibility set.
- CH4-V8: verify W5B.0..W5B.4 cap accounting and prove no borrowing from
  W5C-GEN, W5D-DELETE, W6, or new-admit waves.
- CH5-V8: keep W5B.0 Lock14-only; provider/template guards must cover every
  `_templates` path and reject modified provider/template files.
- CH6-V8: require per-test/per-log nonzero proof and resolve maintain authority
  through either exact no-diff SPEC wording or fresh SK-V14-open maintain
  evidence in W5B.4.

# SK-V14 W5B-FRONTENDR Corrective Packet

Date: 2026-05-26.
Source wave: W5B-FRONTEND PRUNE-3B.
Disposition: Pass Omega V8 input.

## Rejection

W5B-FRONTEND V2 is rejected under the current SPEC shape. The target remains
valid: generic BBNF grammar-source frontend/import/IR closure must exist before
W5C-GEN can build a provider-free runtime generator body. The execution shape
is not valid under the current authority.

The amended SPEC grants W5B-FRONTEND one PRUNE-3B wave slot with one
implementation/redress cap and a <=1.0k C-1 part-A source/test LOC budget
(`SPEC.md:244`). The same cap section says LOC budgets are conjunctive with
the 90-minute cap, and any plan exceeding either limit must split before
dispatch or return REVISE (`SPEC.md:254` through `SPEC.md:260`). Section 8B
then requires the whole frontend closure: Lock 14 owner routing before source
edits, import graph consumption, `@ws`, `@pretty`, `?w`, `>>`, `<<`, span
capture, typed host projections, request-boundary consumption, JSON proof
carry, Sheets/BBNF-self proof carry, `regen-css`, and all seven CSS L4
companions (`SPEC.md:724` through `SPEC.md:751`).

The V2 plan tried to obey the hard redress cap by splitting that work into four
W5B-internal sub-slices (`skv14-W5B-FRONTEND-plan.md:45` through
`skv14-W5B-FRONTEND-plan.md:81`). The V2 CH4 lens correctly rejected that shape
because four 30-minute slices plus final verification do not fit W5B's single
cap and require either aggregate proof or formal sub-waves
(`skv14-waveW5B-FRONTEND-challenge/V2/CH4.md:9` through
`skv14-waveW5B-FRONTEND-challenge/V2/CH4.md:13`, and `CH4.md:31` through
`CH4.md:33`). The consolidated challenge reaches the same governance
conclusion: keeping the full closure under informal slices requires a
SPEC-level wave-graph amendment; narrowing the next redress to one cap-valid
slice would leave W5B-FRONTEND open while W5C-GEN remains blocked on
W5B-FRONTEND close
(`HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:28` through
`HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:35`).

There is a second SPEC conflict. SPEC's W5B-FRONTEND exit gate still says
"Full-table maintain: +/-1.0% on all rows" (`SPEC.md:750`), while the V2 plan
uses exact no-diff maintain for a non-admit capability wave. CH6 correctly
requires either fresh full-table maintain evidence or a SPEC amendment before
W5B-FRONTEND can accept (`skv14-waveW5B-FRONTEND-challenge/V2/CH6.md:9`
through `CH6.md:16`, and `CH6.md:33` through `CH6.md:37`).

## Dependency And Cap Cycle

Current V7 graph:

```text
W5A request boundary
  -> W5B-FRONTEND one-wave frontend/import/IR closure
  -> W5C-GEN provider-free runtime generator body
  -> W5D-DELETE provider/template deletion
  -> W6 root-runtime collapse
```

Actual execution dependency:

```text
W5A request boundary
  -> W5B.0 Lock 14 owner-routing gate
  -> W5B.1 import closure
  -> W5B.2 layout/discard compatibility lowering
  -> W5B.3 pretty/span/projection lowering
  -> W5B.4 request consumer and proof-carry gate
  -> W5C-GEN provider-free runtime generator body
  -> W5D-DELETE provider/template deletion
  -> W6 root-runtime collapse
```

The sub-slices are not deferrals. W5B-FRONTEND still closes only after every
sub-wave and its same-wave consumer evidence passes. The current contradiction
is authority and cap accounting: the needed work is already named by SPEC, but
SPEC has not assigned the formal sub-wave boundaries required to execute it
without overrunning the dispatch-hard-cap discipline.

## Proposed Omega V8 Amendment

Formalize W5B-FRONTEND as capped sub-waves while preserving V7's ownership and
blocking semantics.

| Wave | Proposed scope | Entry gate | Exit gate |
|---|---|---|---|
| W5B.0 LOCK14-GATE | Lock 14 W5B-FRONTEND owner-path roster, parent-diff routing, modified-provider/template rejection tests, all-template guard, and generic owner-path leak census. No grammar/codegen/xtask frontend edits. | W5A admitted + REDRESS-211 + V7 CRUD applied. | Lock 14 routing admits only W5B paths; W5C/W5D subjects reject; provider/template modification tests reject; all `_templates` paths are guarded; owner-path leak census passes. |
| W5B.1 IMPORT-CLOSURE | Request-local import DAG resolution from request source maps, stable source hashing, missing-import fail-closed behavior, and import-cycle fail-closed behavior. | W5B.0 admitted. | Exact grammar tests prove import graph resolution, missing-import rejection, and import-cycle rejection; no public syntax, provider/template, or generator-body change. |
| W5B.2 LAYOUT-DISCARD | Lower `@ws`, `?w`, `>>`, and `<<` into request-local frontend facts without public syntax revival. | W5B.1 admitted. | Exact grammar tests prove layout/discard lowering and public-retirement/fail-closed behavior for those constructs; no provider/template topology change. |
| W5B.3 PRETTY-SPAN-PROJECTION | Lower `@pretty`, `@{...}` span capture, `->` projection metadata, and typed projections into request-local facts. | W5B.2 admitted. | Exact grammar tests prove lowering and malformed-input rejection for pretty/span/projection constructs; no new public directive, no new BIR/BackendShape/substrate variant. |
| W5B.4 REQUEST-CONSUMER | Consume the frontend closure through `emit_runtime_from_request`, preserve W5A JSON/Sheets/BBNF proof carry, run `regen-css` plus seven companions, and capture exact maintain proof for this non-admit capability wave. | W5B.3 admitted. | Same-commit consumer evidence closes W5B-FRONTEND: frontend closure consumed before provider rendering; JSON unchanged-output proof holds; Sheets/BBNF-self proof holds; `regen-css` and all CSS companions pass; provider/template counts unchanged. |

Each W5B sub-wave carries the normal redress cap from `[dispatch-hard-cap]`:
HARD CAP 30 min; at 27 min commit whatever is safe and halt at 30 min. The
aggregate W5B-FRONTEND budget is therefore <=150 min across W5B.0 through
W5B.4, with no borrowing from W5C-GEN, W5D-DELETE, W6, or new-admit waves.
W5B-FRONTEND remains blocked until all five sub-waves admit, and W5C-GEN
remains blocked until W5B-FRONTEND closes.

V8 should also clarify the W5B maintain gate. W5B-FRONTEND is a non-admit
capability wave. The honest maintain proof is exact no-diff on `skinny/RESULTS.md`,
`restart/skinny/ROLLING-SOTA-DELTA.md`, generated runtime outputs, and protected
grammar/source inputs, while `gate-json --skv14-existing-results-capture`
remains schema/freshness evidence. If Omega chooses to retain the +/-1.0%
full-table wording for W5B, it must require a fresh SK-V14-open maintain run
inside W5B.4 instead of allowing a prose substitution.

## Required V8 Folds

Pass Omega V8 should amend:

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 2 and Section 8B to replace
  the one-slot W5B-FRONTEND cap with W5B.0 through W5B.4, entry/exit gates,
  aggregate cap, and final W5B close semantics.
- `restart/MASTER-PLAN.md` Section 13.3 to mirror the W5B sub-wave graph and
  keep W5C-GEN blocked on W5B-FRONTEND close.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` R3/R10 wording only where it
  enumerates PRUNE-3, preserving the V7 ordering.
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` and
  `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` to dispatch W5B.0 first
  and forbid treating W5B.0..W5B.3 as W5B close.
- `restart/HANDOFF.md`, `restart/MIGRATION.md`, and
  `restart/skinny/tranches/sk-v14/HANDOFF.md` to record the W5B-FRONTENDR
  gate and next dispatch.
- `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` only for limited
  active-authority alignment if those surfaces currently name W5B-FRONTEND as
  a one-shot wave.

LOCKS and ARCHITECTURE should remain read/no-op unless Omega-C finds an
unexpected public syntax, substrate, BackendShape, or Lock 14 amendment need.
No current evidence requires such an amendment.

## Risk Class

MEDIUM-HIGH. This is a wave-graph and cap-accounting correction inside the
already authorized V7 W5B-FRONTEND scope. It does not change the architecture,
lock count, BackendShape canon, or W5C/W5D/W6 ownership. It does affect SPEC,
MASTER-PLAN, dispatch surfaces, and handoff state before source redress can
continue.

## Next Action

Dispatch Pass Omega V8 with this packet as the skinny input. Inputs:

- W5B-FRONTEND V2 challenge archive:
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/`.
- Folded W5B-FRONTEND V2 plan:
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md`.
- Pass Omega V7 signoff and CRUD log, because V8 corrects the W5B-FRONTEND
  cap shape created by V7.
- REDRESS-209, REDRESS-210, and REDRESS-211 as pre-blocked routes that must not
  be reopened.

After V8 G-Omega authorization, redispatch W5B.0 LOCK14-GATE. Do not touch
W5B frontend/codegen/xtask source owner paths before the amended Lock 14 gate
sub-wave admits.

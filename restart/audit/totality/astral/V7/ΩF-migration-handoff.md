# Omega-F Migration And Handoff - Pass Omega V7 W5B-GENR

Date: 2026-05-26.
Scope: migration, handoff, and dispatch reroute after REDRESS-211.
Disposition: ACCEPT-WITH-PATCH.

## Verdict

V7 should record REDRESS-211: current W5B-GEN is rejected because W5A admitted
only the source-consuming request boundary, while HEAD still lacks the generic
BBNF frontend/import/IR closure needed for CSS L4 runtime generation. The next
executable SK-V14 wave becomes W5B-FRONTEND.

## Migration Impact

No public API migration lands in V7. The implementation migration is future
wave-local and splits the remaining PRUNE-3 chain:

- W5B-FRONTEND: generic BBNF grammar-source frontend/import/IR closure, with
  CSS L4 as strict positive witness; no provider/template deletion.
- W5C-GEN: provider-free runtime generator body consuming W5A request facts plus
  W5B-FRONTEND IR; no provider/template deletion.
- W5D-DELETE: provider/template deletion and Lock 14 baseline close.

W6 opens only after W5D-DELETE. W7 and W8/W9/W10 remain globally blocked until
PRUNE-1 through PRUNE-5 close.

## Handoff Directive

After V7 CRUD:

1. Dispatch W5B-FRONTEND research, plan, challenge, and redress.
2. Do not dispatch W5C-GEN until W5B-FRONTEND admits.
3. Do not delete provider/template files until W5D-DELETE.
4. Preserve REDRESS-209, REDRESS-210, and REDRESS-211 as pre-blocked routes.
5. Treat static centralization and reading committed generated output as
   rejected routes.

## Pre-Dispatch Guards

Before W5B-FRONTEND: verify Pass Omega V7 G-Omega closed, REDRESS-211 routed,
V7 CRUD landed, and SPEC/DISPATCH/HANDOFF name
`W5B-FRONTEND -> W5C-GEN -> W5D-DELETE`.
Also verify `skinny/crates/bbnf-bench/src/lock14_baseline.rs` has explicit
W5B-FRONTEND owner-path and parent-diff subject routing before touching any
frontend source owner path.

Before W5C-GEN: verify W5B-FRONTEND admitted executable compatibility-lowering
coverage for `@ws`, `@pretty`, `?w`, `>>`, `<<`, span capture, typed host
projections, and import graph consumption through W5A's request.
Also verify the Lock 14 gate has W5C-GEN owner-path and parent-diff subject
routing before touching any generator source owner path.

Before W5D-DELETE: verify W5C-GEN admitted provider-free production entrypoints,
`regen-css`, seven CSS companions, `check-json`, provider-reachability greps,
and no-grammar-name-branch greps.
If the deletion paths are not already routed by the W5C-GEN gate patch, add
W5D-DELETE owner-path and parent-diff subject routing before deleting providers
or templates.

## Forward Lenses

- CH3-V7: distinguish frontend/import/IR closure from provider-free generator
  body; prove rebuild capability before generator replacement or deletion.
- CH5-V7: grep both parser construct coverage and provider reachability; treat
  missing CSS L4 frontend support and live provider-mesh reachability as hidden
  coupling.

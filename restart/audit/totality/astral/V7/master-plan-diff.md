# Pass Omega V7 Master-Plan / SPEC Diff

Disposition: proposed. Do not apply until G-Omega V7 authorization.

## Summary

REDRESS-211 rejects W5B-GEN under the current SPEC shape. The amended wave graph
splits the missing frontend work out ahead of provider-free generation:

```text
W5A -> W5B-FRONTEND -> W5C-GEN -> W5D-DELETE -> W6 -> W7 -> W8/W9/W10
```

## MASTER-PLAN

- Supersede V6 W5BR wording in §13.3 with V7 W5B-GENR.
- Replace W5B-GEN row with W5B-FRONTEND:
  generic BBNF frontend/import/IR closure, CSS L4 strict positive witness,
  <=1.0k source/test LOC, <=90 minutes.
- Replace W5C-DELETE row with W5C-GEN:
  provider-free runtime generator body consuming request/frontend IR, <=1.0k
  source/test LOC, <=90 minutes.
- Insert W5D-DELETE:
  provider/template deletion plus Lock 14 baseline close, <=400 source/test LOC,
  <=90 minutes.
- Move W6 dependency from W5C-DELETE to W5D-DELETE.
- Move every C-1 envelope reference from
  `W5A + W5B-GEN + W5C-DELETE + W6` to
  `W5A + W5B-FRONTEND + W5C-GEN + W5D-DELETE + W6`.

## SK-V14 SPEC

- Update PRUNE wave list and manifest to include W5B-FRONTEND, W5C-GEN, and
  W5D-DELETE.
- Replace §8B with W5B-FRONTEND:
  generic BBNF grammar-source frontend/import/IR closure; CSS L4 constructs are
  compatibility-lowered into canonical IR, not admitted as new public syntax;
  no provider/template deletion.
- Replace §8C with W5C-GEN:
  provider-free runtime generator body consuming W5A request facts and
  W5B-FRONTEND IR; no provider/template deletion.
- Insert §8D W5D-DELETE:
  provider/template deletion and Lock 14 baseline close after W5C-GEN admits.
- Update W6 entry to W5D-DELETE admitted.
- Update W7 and W8/W9/W10 blockers to include the rerouted PRUNE chain.
- Update rerun ceilings and same-wave consumers:
  W5B-FRONTEND frontend construct coverage plus JSON/Sheets/BBNF witnesses;
  W5C-GEN regen/check/gate/provider-reachability consumers;
  W5D-DELETE deletion and Lock 14 baseline consumers.

## Lock 14 Owner-Path / Parent-Diff Routing

V7 CRUD must bind the Lock 14 routing defect found by CH5/CH6:

- W5B-FRONTEND cannot touch frontend source owner paths until
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` has a
  `SK_V14_W5B_FRONTEND_OWNER_PATHS` roster, parent-diff subject routing for
  `sk-v14-waveW5B-FRONTEND` and `sk-v14-waveW5B-FRONTEND-redress`, and a unit
  test proving the roster admits only those paths. Initial owner paths:
  `crates/grammar/src/lib.rs`, `crates/codegen/src/lib.rs`,
  `crates/codegen/src/grammar_provider.rs`, `xtask/src/main.rs`,
  `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
  `crates/bbnf-bench/src/lock14_baseline.rs`.
- W5C-GEN cannot replace the provider-backed production body until
  `lock14_baseline.rs` has a `SK_V14_W5C_GEN_OWNER_PATHS` roster, parent-diff
  subject routing for `sk-v14-waveW5C-GEN` and
  `sk-v14-waveW5C-GEN-redress`, and a unit test proving the roster admits only
  those paths. Initial owner paths: `crates/codegen/src/lib.rs`,
  `crates/codegen/src/grammar_provider.rs`, `xtask/src/main.rs`,
  `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
  `crates/bbnf-bench/src/lock14_baseline.rs`.
- Any new neutral module path is forbidden until the wave plan names it exactly
  and the Lock 14 gate patch adds that exact path plus a parent-diff unit test.
- W5D-DELETE owns provider/template deletion routing. If W5C-GEN has not already
  made deletion paths executable in the Lock 14 gate, W5D-DELETE must add its
  owner-path and parent-diff subject routing before deletion.

## Tranche Surfaces

- `SYNTHESIS.md`: R3/C-1 wording and cap envelope.
- `ORCHESTRATOR-PROMPT.md`: R3 PRUNE-3B/3C/3D split.
- `DISPATCH-PROMPT.md`: V7 guard, corrected wave chain, challenge routing, and
  `@ws` compatibility-lowering caveat.
- `HANDOFF.md`: record REDRESS-211 and next dispatch W5B-FRONTEND.

## V1 Handoff/Migration + Skinny Corpus

- `restart/HANDOFF.md`: status and next dispatch W5B-FRONTEND.
- `restart/MIGRATION.md`: Pass Omega V7 receiver block.
- `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md`: limited alignment.
- `restart/skinny/{BENCH,SUBSTRATE}.md`: read/no-op.

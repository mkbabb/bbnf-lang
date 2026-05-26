# Omega-D Master-Plan Reconciliation - Pass Omega V7 W5B-GENR

Date: 2026-05-26.
Scope: MASTER-PLAN and SK-V14 SPEC wave-graph reconciliation after REDRESS-211.
Disposition: ACCEPT-WITH-PATCH.

## Delta

`W5B-GEN` is rejected under the current SPEC. The amended graph is:

```text
W5A -> W5B-FRONTEND -> W5C-GEN -> W5D-DELETE -> W6 -> W7 -> W8/W9/W10
```

## Proposed Master/SPEC Amendment

- Replace graph claims `W5A + W5B-GEN + W5C-DELETE + W6` with
  `W5A + W5B-FRONTEND + W5C-GEN + W5D-DELETE + W6`.
- Replace `W5B-GEN PRUNE-3B` with `W5B-FRONTEND PRUNE-3B`: generic BBNF
  frontend/import/IR closure; CSS L4 is strict witness; no provider/template
  deletion.
- Replace old `W5C-DELETE PRUNE-3C` with `W5C-GEN PRUNE-3C`: provider-free
  generator body consuming request/frontend IR; providers/templates may remain
  only as unreachable residue.
- Insert `W5D-DELETE PRUNE-3D`: provider/template deletion plus Lock 14 baseline
  close after W5C-GEN.
- Caps: W5B-FRONTEND <=1.0k source/test LOC, W5C-GEN <=1.0k source/test LOC,
  W5D-DELETE <=400 source/test LOC, each <=90 minutes. W5A remains closed at
  921 LOC. W6 remains <=2.0k aggregate, <=90 minutes per sub-wave, <=810
  minutes aggregate.
- Same-wave consumers:
  W5B-FRONTEND: executable frontend construct coverage plus JSON/Sheets/BBNF
  witnesses.
  W5C-GEN: `regen-css`, seven companions, `check-json`, `gate-json`,
  provider-reachability and no-grammar-name greps.
  W5D-DELETE: deletion gates, `regen-css`, companions, `check-json`, and Lock
  14 baseline.
- W6 remains blocked until W5D-DELETE; W7 remains blocked until W6; W8/W9/W10
  remain globally blocked until PRUNE-1 through PRUNE-5 close.

## Lock 14 Routing Amendment

The SPEC patch must bind executable Lock 14 owner-path and parent-diff routing:

- W5B-FRONTEND entry requires a gate patch adding
  `SK_V14_W5B_FRONTEND_OWNER_PATHS`, parent-diff subject routing, and unit tests
  in `skinny/crates/bbnf-bench/src/lock14_baseline.rs` before frontend source
  redress. Initial owner paths: `crates/grammar/src/lib.rs`,
  `crates/codegen/src/lib.rs`, `crates/codegen/src/grammar_provider.rs`,
  `xtask/src/main.rs`, `xtask/src/regen.rs`, `xtask/src/regen_css.rs`,
  `crates/bbnf-bench/src/lock14_baseline.rs`.
- W5C-GEN entry requires the same gate pattern for
  `SK_V14_W5C_GEN_OWNER_PATHS` before generator source redress. Initial owner
  paths: `crates/codegen/src/lib.rs`, `crates/codegen/src/grammar_provider.rs`,
  `xtask/src/main.rs`, `xtask/src/regen.rs`, `xtask/src/regen_css.rs`,
  `crates/bbnf-bench/src/lock14_baseline.rs`.
- New neutral module paths are not implicitly authorized; the wave plan must
  name the exact path and extend the Lock 14 gate before touching it.

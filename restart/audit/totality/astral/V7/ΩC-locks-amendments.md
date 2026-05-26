# Omega-C Locks Amendments - Pass Omega V7 W5B-GENR

Date: 2026-05-26.
Scope: REDRESS-211 and the W5B-FRONTEND / W5C-GEN / W5D-DELETE split.
Disposition: NO LOCK AMENDMENT.

## Verdict

REDRESS-211 is a wave-graph and SPEC routing correction. The split inserts the
missing generic BBNF source frontend/import/IR closure before provider-free
generation, then delays provider/template deletion until the generator is
load-bearing. Lock 14 already requires the generated grammar-source plus
workspace-metadata path and already forbids provider/template and grammar-branch
workarounds.

## Invariants

- Lock count: 16.
- Lock 10 BackendShape canon remains five:
  `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`.
- `FactStream` remains a Lock 1 substrate-manifest category, not a sixth
  BackendShape.
- Lock 14 owner-path and parent-diff routing is required for W5B-FRONTEND and
  W5C-GEN, but that belongs in the V7 SPEC/wave gates and
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` before the corresponding
  redress commits, not in `restart/locks/LOCKS.md`.

## Required Gate Routing

V7 CRUD must make the Lock 14 routing executable in the SK-V14 SPEC:

- W5B-FRONTEND must add `SK_V14_W5B_FRONTEND_OWNER_PATHS` and subject routing
  for `sk-v14-waveW5B-FRONTEND` / `sk-v14-waveW5B-FRONTEND-redress` in
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` before it changes any
  frontend/codegen source path. The initial allowed frozen-root owner set is:
  `crates/grammar/src/lib.rs`, `crates/codegen/src/lib.rs`,
  `crates/codegen/src/grammar_provider.rs`, `xtask/src/main.rs`,
  `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
  `crates/bbnf-bench/src/lock14_baseline.rs`.
- W5C-GEN must add `SK_V14_W5C_GEN_OWNER_PATHS` and subject routing for
  `sk-v14-waveW5C-GEN` / `sk-v14-waveW5C-GEN-redress` before replacing the
  provider-backed production body. The initial allowed frozen-root owner set is:
  `crates/codegen/src/lib.rs`, `crates/codegen/src/grammar_provider.rs`,
  `xtask/src/main.rs`, `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
  `crates/bbnf-bench/src/lock14_baseline.rs`.
- Any new neutral module path proposed by W5B-FRONTEND or W5C-GEN is not
  implicitly authorized. The plan must name the exact path, update
  `lock14_baseline.rs` with that path in the wave-owned gate patch, and add a
  parent-diff unit test analogous to the current W5A test before touching the
  path.
- W5D-DELETE must add its deletion owner routing before provider/template
  deletion if the current Lock 14 gate has not already been widened by W5C-GEN.

## Proposed Locks Diff

Zero delta. CRUD-3 is read/no-op.

# AZ-III W0 Dispatch Packets

These packets are the orchestrator handoff for implementation agents. Each
agent works in a contained worktree and returns changed paths, commands run,
pass/fail output, and routed misses. No agent may edit outside its file
bounds without triggering the wave triumvirate.

## W1 - O5 Reclose

Dispatch up to 10 agents after the root dirty slice is assigned.

- W1.1 Regen drift: own `xtask/**`, generator source, and generated grammar
  files required by `cargo xtask regen --check`.
- W1.2 No-default build: own manifest/build-facing source required by
  `cargo build -p bbnf --no-default-features --profile ax-iter`.
- W1.3 Deletion and metadata audit: own package metadata and live-source
  scans for `tape`, `json-prototype`, public tape runtime, and JIT residue.
- W1.4 A1 residue redress: own stale prototype/JIT/provenance residue and
  active-doc cleanup.

Hard close requires archived outputs for regen, no-default build, metadata,
and deletion scans. Do not convert red scans into narrative passes.

## W2 - Semantic Parity and Bootstrap Canonicalization

Dispatch after W1 close, or in parallel only when file bounds do not overlap.

- W2.1 JSON parity: own sonic-rs and canonical/value parity tests plus
  proven shared materialization root causes.
- W2.2 CSS parity: own lightningcss, color, selector, bootstrap, and
  tailwind parity tests plus proven shared grammar/type/projection roots.
- W2.3 Sheets parity: own Sheets self/parity tests and shared
  lowering/projection/serializer root causes.
- W2.4 BBNF bootstrap canonical path: own generated self-host proof and
  removal or blocked-close handling for `bootstrap_parser.rs`.

Ignored semantic tests, early-return harness masking, and bootstrap fallback
keep W2 blocked until corrected or same-tranche redress is specified.

## W3 substrate authority - split into W3a / W3b / W3c

Per REAUDIT 2026-04-30 SYNTHESIS A3 the original single W3 wave was
restructured into three sequential waves so file bounds are disjoint and
each lane closes on its own evidence. Dispatch each as the primary
substrate wave for its axis after W0p and W1, and only parallel to W2
when bounds are disjoint.

### W3a - Fact and Type Authority

- W3a.0 Pipeline registry research: enumerate `MultiPathParser` /
  `ImportPrettyParser` / `SplitPrettyParser` callers; produce verdict at
  `audit/W3a-0-pipeline-registry-research.md` before W3a planning.
- W3a.1 Durable fact authority: own egraph, recognizer, node, and projection
  facts with at least one production consumer.
- W3a.2 Type obligation solver: own cycle and heterogeneous alternation
  obligations; delete the silent `BoxedEnum` fallbacks at
  `crates/ir/src/passes/types/constraint/reference.rs:74` and
  `crates/ir/src/passes/types/constraint/revise.rs:123`. No silent
  fallback may close the wave.

### W3b - CSP Strategy Globalization

- W3b.1 Strategy globalization: own `shape_dict`, shape/layout/dispatch
  constraints, and their production consumption. Replace any
  `shape_dict::install` no-op with a consumed constraint.

### W3c - Projection Consumption and Registry Authority

- W3c.1 Projection consumption: own StructDirect and emitter fallback
  removal for EBNF/CSS/Sheets/BBNF projection failures. File bounds carve
  with W2: shape-specific `crates/core/src/backend/rust/emitter/shapes/**/struct_direct.rs`
  belong to W2; the rest of `crates/core/src/backend/rust/emitter/**`
  belong to W3c.
- W3c.2 Registry authority: resolve pipeline registry holes at
  `crates/ir/src/registry/strategy.rs:257` for `MultiPathParser` /
  `ImportPrettyParser` / `SplitPrettyParser` per W3a.0 verdict.

Every substrate lands with its consumer and a test that fails when the
consumer is disconnected. Compatibility shims and no-op constraints are
blocked states, not close evidence.

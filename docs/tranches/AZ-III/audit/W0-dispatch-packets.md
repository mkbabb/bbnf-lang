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

## W3 - Fact, Type, CSP, and Projection Authority

Dispatch as the primary substrate wave after W0/W1, and only parallel to W2
when bounds are disjoint.

- W3.1 Durable fact authority: own egraph, recognizer, node, and projection
  facts with at least one production consumer.
- W3.2 Type obligation solver: own cycle and heterogeneous alternation
  obligations; no silent `BoxedEnum` fallback may close the wave.
- W3.3 CSP strategy globalization: own `shape_dict`, shape/layout/dispatch
  constraints, and their production consumption.
- W3.4 Projection consumption: own StructDirect and emitter fallback removal
  for EBNF/CSS/Sheets/BBNF projection failures.

Every substrate lands with its consumer and a test that fails when the
consumer is disconnected. Compatibility shims and no-op constraints are
blocked states, not close evidence.

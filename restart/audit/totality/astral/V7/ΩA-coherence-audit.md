# Omega-A Coherence Audit - Pass Omega V7 W5B-GENR

Date: 2026-05-26.
Scope: REDRESS-211 and the W5B-GENR wave-graph correction.
Disposition: ACCEPT-WITH-REQUIRED-SPEC-AMENDMENTS.

## Verdict

The W5B-GENR split is coherent as a wave-graph, cap, and routing correction. It
does not require `restart/ARCHITECTURE.md` or `restart/locks/LOCKS.md` changes
if V7 keeps the correction to sequencing and gates. Lock 14 already requires
grammar source plus workspace metadata generated output, and the V1 architecture
already owns grammar/import/IR/generator concepts.

V7 must not admit standalone `@ws` as a new public V1 directive. `@ws` is a
CSS L4 compatibility/frontend-lowering witness into the existing canonical
layout/IR contract, not a new grammar-surface amendment.

## Evidence

- REDRESS-211 rejects W5B-GEN and requires a split into frontend/import/IR,
  provider-free generation, then deletion: `skinny/REDRESS.md`.
- Corrective packet:
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md`.
- Live provider path remains at
  `skinny/crates/codegen/src/grammar_provider.rs:77`,
  `skinny/crates/codegen/src/lib.rs:180`, and
  `skinny/crates/codegen/src/lib.rs:233`.
- Current skinny parser accepts only `@import` and `@token` directives in
  `skinny/crates/grammar/src/lib.rs:320`.
- Invariants verified at HEAD: 16 locks, five BackendShape variants, and
  Pattern H = 67.

## Proposed Operations

- CRUD-1 ARCHITECTURE: read/no-op.
- CRUD-3 LOCKS: read/no-op; record zero delta.
- CRUD-2 MASTER/SPEC: patch the SK-V14 wave graph from
  `W5A -> W5B-GEN -> W5C-DELETE -> W6` to
  `W5A -> W5B-FRONTEND -> W5C-GEN -> W5D-DELETE -> W6`.
- Tranche-local SPEC surfaces must carry the `@ws` compatibility-lowering caveat
  so W5B-FRONTEND does not imply a public grammar syntax amendment.

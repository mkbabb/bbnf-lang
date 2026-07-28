# W2 — Immutable BBNF semantic spine

Purpose: replace the mutable state bag with one consuming source-to-event
pipeline and establish the scalar performance oracle before generality proof.

## Carrier

```text
grammar-module bytes
  -> parse-that Rust ModuleResult
  -> SourceModule + SourceMap + revision digest
  -> Program
  -> DependencyGraph + SCC condensation
  -> TypeSolution + DiagnosticSet
  -> AnalysisFacts
  -> LoweringPlan
  -> ScalarEventPlan
```

The parse-that/BBNF boundary is `ModuleResult -> SourceModule`: successful full
consumption, typed spans, source identity, and diagnostics enter; parser state,
memo tables, and regex-engine internals do not. Backends receive only the sealed
lowering plan and source tables.

## Required semantics

- stable arena `NodeId`;
- explicit `NodeRemap` after transformations;
- deterministic SCC worklist and resource budgets;
- first-order grammar-shape inference required by the sealed witness grammars:
  sequence/product, choice/sum, option, repetition, named records, transforms,
  recursive binders, occurs check, principality for admitted definitions;
- semantic types distinct from layout descriptions;
- immutable analysis products keyed by revision/node;
- typed failure instead of `Span`, `BoxedEnum`, epsilon, independent-minimum, or
  budget fallbacks.

Every inference rule needs an executable type-preservation witness. Broader
rank-1 schemes, rows, effects, traits, and full HM generalization are not
expansion hooks in this tranche; a later tranche must name a failing grammar
witness before proposing one.

## Optimizer law

Direct algorithms are the oracle.

- Remove the unsound ambient G3 family and stale writeback routes.
- Admit e-graphs only for a bounded action-free equality island whose rewrites
  preserve acceptance, value, span, diagnostics, and canonical output.
- Admit CSP only when a corpus witness proves a coupled global choice that direct
  enumeration cannot cheaply settle.
- Incomplete saturation, unsat, budget, extraction, and missing-child cases fail
  explicitly.

## Selected sink and scalar scanner

The only implemented sink is full-consumption scalar events.
`artifacts/W2-EVENT-SCHEMA.json` seals event kinds, payload encodings, span and
source identity, ordering, recovery/error events, numeric/string normalization,
and end-of-input. Conformance compares the canonical event stream itself against
an independent oracle. A cryptographic digest is retained only for benchmark
anti-elision and output accounting; digest equality never substitutes for
semantic stream equality. The sink does not build tape, DOM, path, or typed
outputs.

Before freezing this vertical, prototype the grammar-derived scalar lexical
classifier inside the consuming event parser. Record eligibility, fallback,
correctness, allocations, and end-to-end profile. This is the scalar oracle for
W4; it contains no NEON or hand ASM.

## Structure

Module boundaries follow owners above. Contract/crater/conformance tests use an
external isomorphic tree; small private unit laws may remain colocated. God
modules are split by semantic ownership; tiny wrappers without an invariant are
folded. File movement is not accepted without a reduced edge/owner count.

## Exit

- exact serialize/replay of every stage;
- randomized constraint/order and incremental-edit equality;
- the sealed witness set, including one recursive grammar, reaches a
  `ScalarEventPlan`;
- scalar event conformance and checksum laws pass;
- the consuming scalar scanner feasibility/profile row is accepted or rejected;
- no production backend reads mutable earlier phases;
- old `GrammarIR`, pointer DAG, fake authority, and silent fallbacks are absent
  from the selected production slice;
- architecture/API receives tri-fold hostile review.

Named evidence:

- `artifacts/W2-SOURCE-PLAN.schema.json`;
- `artifacts/W2-EVENT-SCHEMA.json`;
- `artifacts/W2-TYPE-LAWS.json`;
- `artifacts/W2-EVENT-CONFORMANCE.json`;
- `artifacts/W2-SCALAR-PROFILE.json`;
- `artifacts/W2-REPLAY.md`.

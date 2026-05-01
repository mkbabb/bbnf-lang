# AZ-IV Gestalt Plan

AZ-IV is not a new product surface. It is the consolidation tranche that makes the post-AZ-III architecture executable as one system.

## Current Architecture

The desired architecture is already chosen:

1. Grammar files and manifest metadata define the parser family.
2. The compiler lowers grammar into IR facts, type obligations, recognizer decisions, CSP decisions, and egraph nodes.
3. Rust generation emits direct struct materialization.
4. Runtime views and formatters consume those structs.
5. Benchmarks and parity tests compare generated parsers against external oracles.

The problem is not absence of substrate. The problem is divergence between selected substrate and consumed substrate, plus two forward plans whose requirements were at risk of being dropped when their stale mechanisms were rejected.

## Active Contradictions

1. BA and BB are forward-looking plans, but their docs contradict HEAD and each other.
2. `xtask regen` is canonical, yet strict regeneration is red for most manifest grammars.
3. CSP decisions exist, but selected engines/layout/dispatch choices still pass through sidecars or independent emitter ladders.
4. Rewrite/ruler storage exists, but loaded rules do not drive production egraph saturation/codegen.
5. DTA is live as a fact source, but comments still describe walker/tape runtime emission.
6. Runtime CSS color has a current typed model, but public compatibility color surfaces still reflect a 40-byte/tape-era projection.
7. TS backend exists, but current gates prove strings, not executable parser parity.
8. Sibling repositories still carry derive, lockfile, CSP-source, and docs-sync drift that can invalidate workspace claims.

## One-Path Target

AZ-IV accepts only one route:

```text
grammar + manifest
  -> IR facts and obligations
  -> CSP/egraph/recognizer decisions
  -> generated StructDirect parser
  -> typed runtime document/view
  -> oracle parity and benchmark evidence
```

Any surface outside that route is either:

- used by this route;
- internal diagnostic/dev tooling with explicit docs and tests;
- deleted or frozen as historical.

## BA/BB Folding Rule

BA path-query ambitions and BB rewrite/ruler ambitions are not abandoned. AZ-IV preserves the functional requirements while changing the mechanism where the old mechanism would create a second path.

- BA's Rust path surface uses `crates/core/src/runtime/path.rs`, `path!`, and generated per-document `*PathQuery` traits. It must still deliver compile-time/type-inference checking, zero-allocation traversal, external competitor benchmarks, and isomorphic host-binding status.
- BA's old `crates/bbnf-path*` layout is not canonical. Its semantics are canonical: typed path construction, grammar-aware diagnostics, host signature/error isomorphism, and sonic-rs/simdjson path-access comparisons.
- BB uses `crates/ir/src/rewrites`, `crates/egraph/src/ruler`, VM-residue oracle, ranker/tiering, and grammar-colocated `rewrites/*.ron`. A one-rule smoke is not enough.
- BB's old derive-scanned rewrite language is retired. Its semantics are canonical: egraph-first equivalence, VM only on residue, ranked/tiered rules, generated-code effects, and per-rule parity/bench proof.
- Current profiler/bench profiles remain canonical, but W3 must prove post-AU/post-AZ floors and same-harness sonic-rs projection parity.

Product expansion outside these semantics waits until AZ-IV closes cleanly. The semantics themselves are AZ-IV scope.

## Deletion Bias

AZ-IV deletes before adding:

- no `*_v2` modules;
- no compatibility feature flags;
- no restored derive pipeline;
- no DTA walker fallback;
- no second path-query crate;
- no generated tape/view bridge;
- no "consumer later" hooks.

If deletion is unsafe because a current consumer exists, the wave must name the consumer and refactor the surface to match its real role.

## Close Shape

AZ-IV closes only when:

1. docs and code agree on the active architecture;
2. regen and parity are coupled and green;
3. semantic parity is derived from type facts and generated projections, not normalizer escape hatches or rule-name tables;
4. existing optimization substrates produce emitted/runtime effects across their full denominator;
5. legacy claims are deleted or renamed;
6. benchmark rows beat post-AU/post-AZ floors and are measured, not watchdog-routed;
7. BA/BB coverage is lossless: every requirement is landed, retired with evidence, or routed to a named successor because it cannot fit the AZ-IV thesis.

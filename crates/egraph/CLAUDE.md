# CLAUDE.md — crates/egraph/

General-purpose e-graph for equality saturation. The shared substrate
consumed by both BBNF tiers: the grammar-IR e-graph in `crates/ir/src/egraph/`
and the bbnf-regex HIR e-graph in `parse-that/rust/regex/src/egraph/`.

The crate is deliberately **domain-agnostic** — the `Language` trait is the
only thing a consumer must implement to use the full pipeline
(intern → saturate → extract). Both grammar IR nodes (`GrammarENode`) and
regex HIR nodes (`HirENode`) implement `Language` via
`#[derive(egraph_derive::Language)]`, so both pipelines share this
single infrastructure.

## Structure

```
crates/egraph/
├── Cargo.toml
├── src/
│   ├── lib.rs           Re-exports the public substrate
│   ├── id.rs            Id (e-class identifier — newtype around u32)
│   ├── unionfind.rs     UnionFind (path-compressed union-find for e-class merging)
│   ├── eclass.rs        EClass (a single equivalence class — vec of e-nodes)
│   ├── egraph.rs        EGraph<N, A> (the main hash-cons structure with rebuild + union)
│   ├── language.rs      Language trait + LanguageChildren helper
│   ├── analysis.rs      Analysis trait + NoAnalysis substrate
│   ├── rewrite.rs       Rewrite trait + RewriteFn blanket impl
│   ├── scheduler.rs     Scheduler trait + BackoffScheduler + RunReport
│   ├── csp_scheduler.rs CspScheduler + DirtyDomain + ParentDirtyProp
│   ├── extract.rs       CostModel trait + AstSize default + Extractor
│   └── cost_weights.rs  CostWeights — shared cost coefficients used by both tiers
└── tests/
    ├── unionfind.rs     Union-find primitive tests
    ├── egraph_basic.rs  Basic interning + union + rebuild tests
    ├── derive_language.rs  egraph-derive Language proc-macro tests
    ├── saturation.rs    End-to-end saturation tests with toy rules
    └── csp_scheduler.rs CspScheduler dirty-propagation tests
```

## Key Types

- **`EGraph<N: Language, A: Analysis<N>>`** — the hash-cons e-graph. Owns the
  union-find, the e-classes, the canonical/uncanonical hash-cons table, and
  the per-class analysis data. `add` interns; `union(a, b)` merges; `rebuild`
  re-canonicalizes after a batch of unions; `total_nodes` and `union_count`
  drive scheduler work metrics.
- **`Id`** — newtype index into the union-find. Always canonicalized through
  `egraph.find(id)` before use across rebuild boundaries.
- **`Language`** — the trait every e-node enum implements. `discriminant()`,
  `children()`, `children_mut()`, plus the auto-derivable
  `for_each_child` / `map_children` from `egraph-derive`. Recursive fields
  (`Box<Hir>`, `Vec<Hir>`, etc.) project trivially to `Id` children.
- **`Analysis<N>`** — per-class lattice data. Implementations supply
  `make` (compute initial value from an e-node), `merge` (join when classes
  unify), and `modify` (post-merge invariants). The `NoAnalysis` substrate is
  the default — neither tier currently consumes per-class lattice data.
- **`Rewrite<N, A>`** — the rewrite-rule trait with separate `search` and
  `apply` phases. `search` returns a list of `(e-class id, match payload)`
  tuples; `apply` consumes the payload and installs new e-nodes.
- **`RewriteFn<N, A>`** — blanket-impl wrapper that exposes
  `run(egraph) -> usize` (returns the work delta = `total_nodes` +
  `union_count` change) and `run_on_dirty(egraph, dirty)` for incremental
  re-search over only the classes touched by the previous iteration.
- **`Scheduler`** — drives a list of rewrites to fixed-point.
  `BackoffScheduler` is the reference implementation: round-robin with
  per-rule iteration limits and node-growth caps. `CspScheduler` is the
  production default: it builds a CSP per iteration over the dirty class
  set, propagates `DirtyDomain` (a per-class bool lattice) via
  `ParentDirtyProp`, and only re-runs rules whose match-class set
  intersects the dirty closure.
- **`CostModel<N, A>`** — extraction cost. The default `AstSize` cost model
  picks the smallest e-node tree by node count. Custom cost models embed
  `CostWeights` for branch-factoring incentives.
- **`CostWeights`** — the shared coefficient struct (branch_cost,
  node_cost, dispatch_bonus, etc.). Both `GrammarCostModel` (in
  `crates/ir/src/egraph/cost.rs`) and `RegexExtractionCost` (in
  `parse-that/rust/regex/src/egraph/cost.rs`) embed `CostWeights` so
  branch-factoring incentives stay in sync across the two tiers.
- **`Extractor<N, A, C>`** — recursive cost-guided extraction. Walks each
  e-class, picks the lowest-cost e-node per class via memoized DP, returns
  the cheapest tree rooted at a given class id.

## Pipeline

```
trees of N           EGraph<N, A>              best N per root
─────────────►  ◄────────────────────►  ───────────────►
    add               saturate                  extract
                  (rules, sched)             (cost model)
```

Consumer responsibilities:

1. Implement `Language` for the e-node enum (auto-derivable via
   `#[derive(egraph_derive::Language)]`).
2. Implement `Analysis<N>` for per-class lattice data, or use
   `NoAnalysis` if no lattice is needed.
3. Implement `Rewrite<N, A>` for each rewrite rule.
4. Implement `CostModel<N, A>` for extraction (or use `AstSize`).
5. Build → saturate → extract:
   ```rust
   let mut eg: EGraph<MyNode, NoAnalysis> = EGraph::new();
   let root = my_translator::insert(&mut eg, &source_tree);
   eg.rebuild();
   let scheduler = CspScheduler::default();
   let report = scheduler.run(&mut eg, &rules);
   let extracted = Extractor::new(&eg, &MyCost::default()).find_best(root);
   ```

## Both Tiers

The grammar tier (`crates/ir/src/egraph/`) and the bbnf-regex HIR tier
(`parse-that/rust/regex/src/egraph/`) are **isomorphic**: same `egraph`
substrate crate, same `Rewrite` trait, same `CspScheduler`, mirrored rule
file layout, and shared `CostWeights`. A new optimization lands as a
mirrored rule pair — one file per tier — or as a single tier if its
domain is exclusive.

Instrumentation: `BBNF_EGRAPH_REPORT=1` prints the grammar-tier saturation
report; `BBNF_HIR_EGRAPH_REPORT=1` prints the HIR-tier report. Both report
per-rule fire counts and the iter/growth limit hits.

## Dependencies

- **csp-solver** — used by `csp_scheduler.rs` for the dirty-domain
  AC-3 propagation.
- **rustc-hash** — `FxHashMap` for the hash-cons table.
- **smallvec** — small-vector storage in `EClass`.

## Conventions

- `mod` declarations are private (`mod foo;`), with `pub use` re-exports
  in `lib.rs` controlling the public surface. Internals never leak.
- The `Rewrite::search` phase only reads the e-graph; `Rewrite::apply` is
  the only mutating step. The `RewriteFn::run` blanket impl enforces this
  separation by collecting matches first, then applying.
- `CspScheduler::run` returns a `RunReport` with `iterations`,
  `total_applied`, `final_nodes`, `final_classes`, `saturated`,
  `iter_limit_hit`, `growth_limit_hit` — same shape as `BackoffScheduler`'s
  report so consumers can swap schedulers.
- All inline tests in `src/` are forbidden; tests live in `crates/egraph/tests/`.

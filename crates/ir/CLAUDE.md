# CLAUDE.md — crates/ir/

Canonical grammar IR between the BBNF frontend and all backends (Rust codegen,
bytecode VM, TS interpreter, pretty-printing). Fully owned (no lifetimes),
serializable via MessagePack for WASM boundary transfer.

## Structure

```
bbnf-ir/
├── Cargo.toml
├── src/
│   ├── lib.rs            Thin re-export hub — pulls IR types out of `types/` and
│   │                     republishes them at `bbnf_ir::IrNode` etc. Also re-exports
│   │                     `vm::{bytecode, compiler, interpreter, debug}` plus the
│   │                     `CharSet128` / `regex_first` pass-throughs from `bbnf-regex`.
│   ├── types/            Pure type definitions consumed by every backend and pass.
│   │   ├── mod.rs        Sub-module map + `pub use` re-exports + `RuleId`/`StringId`/`FnId`.
│   │   ├── node.rs       `IrNode`, `AltBranch`, `AltDispatch`, `TokenDispatchArm`, `GrammarSpan`,
│   │   │                  walking helpers, `count_nodes`.
│   │   ├── map_expr.rs   `MapExpr`, `MapBinOp`, `MapUnaryOp`, constant-fold/introspection.
│   │   ├── fn_descriptor.rs  `FnDescriptor` — host-function descriptor enum.
│   │   ├── rule.rs       `IrRule`, `RuleMeta`, `RuleDirectives`, `MemoStrategy`,
│   │   │                  `DispatchHint`, `PrettyHints`, `SubVariant`,
│   │   │                  `parse_sep_hint` / `parse_split_hint`.
│   │   ├── type_desc.rs  `TypeDesc` — backend-agnostic type descriptor.
│   │   └── grammar.rs    `GrammarIR` top-level container, accessors, MessagePack/JSON.
│   ├── dag/              Hash-consed canonical Grammar DAG (`NodeId`-keyed substrate).
│   │   ├── mod.rs        Public `GrammarDag` struct, `ensure_dag` test helper.
│   │   ├── build.rs      `GrammarDagBuilder` — IR → DAG construction.
│   │   ├── intern.rs     Hash-cons interning of sub-expressions.
│   │   ├── extract.rs    DAG → IrNode materialization.
│   │   └── node.rs       `DagNode`, `NodeId`.
│   ├── egraph/           Grammar-tier e-graph (permanent secondary equivalence layer).
│   │   ├── mod.rs        EGraph saturation entry point.
│   │   ├── build_egraph.rs  IR → `GrammarENode` insertion.
│   │   ├── write_back.rs IR materialization from extracted e-nodes.
│   │   ├── node.rs       `GrammarENode` (`#[derive(Language)]` via `egraph-derive`).
│   │   ├── interner.rs   StringId/ChildId interning helpers.
│   │   ├── cost.rs       `GrammarCostModel` (shares `CostWeights` with the HIR tier).
│   │   └── rules/        Rewrite rules.
│   │       ├── mod.rs    `default_rules()` — retention policy documentation.
│   │       └── regex.rs  `DeduplicateAltBranches`, `SupersetAbsorbAlt`,
│   │                      `UnionMergeAlt`, `FuseAltRegexBranches`.
│   ├── recognizer/       Pattern recognizer substrate (plans + facts).
│   │   ├── mod.rs        Recognizer entry point.
│   │   ├── facts.rs      `NodeFacts` shape recognition.
│   │   └── plans.rs      Pattern plans emitted for the backend driver.
│   ├── vm/
│   │   ├── mod.rs        Re-exports bytecode / compiler / interpreter / debug.
│   │   ├── bytecode.rs   Opcode enum (incl. DebugBreak), `BytecodeProgram`, source map.
│   │   ├── debug.rs      `DebugState`, `StepMode`, `DebugAction`, `DebugSnapshot`, `TraceEntry`.
│   │   ├── compiler/
│   │   │   ├── mod.rs    `Compiler` struct, `compile` / `compile_with_debug` entry points,
│   │   │   │              `grammar_needs_memo` / `node_has_direct_left_recursion` predicates.
│   │   │   ├── emit.rs   `emit` / `current_offset` / `patch` / `patch_fail_jumps` primitives.
│   │   │   ├── rule.rs   `compile_rule` — rule entry, source map, debug breaks, memo wrap.
│   │   │   ├── node.rs   `compile_node` dispatch.
│   │   │   └── compound.rs  `compile_seq`/`compile_alt`/`compile_dispatch`/`compile_token_dispatch`/
│   │   │                    `compile_repeat`/`compile_binary_backtrack`/`compile_minus`/
│   │   │                    `compile_negate` + `DispatchPatchPlan`.
│   │   └── interpreter/
│   │       ├── mod.rs    `Interpreter` struct, `CallFrame` / `Checkpoint` / `RepeatState`,
│   │       │              main `run` loop, `parse_with_ir`, `collect_values_from` /
│   │       │              `track_furthest` / `emit_furthest_diagnostic` helpers.
│   │       ├── value.rs  `Value`, `ValueSlice`, `ParseDiagnostic`, `ParseResult`.
│   │       ├── leaves.rs `exec_match_string`, `exec_match_regex`, `exec_epsilon`,
│   │       │              `exec_dispatch_token`.
│   │       ├── control.rs  call/return + state save/restore/drop + whitespace trim.
│   │       ├── repeat.rs  repetition begin/end + finalize.
│   │       ├── construct.rs  `exec_make_array`, `exec_make_tagged`.
│   │       └── memo.rs   memoization check + store.
│   └── passes/
│       ├── mod.rs        Re-exports the public pass surface.
│       ├── metadata.rs   IR-level alias + transparent alternation detection.
│       ├── csp_domains.rs  `BoolDomain` / `CharSetDomain` lattices for fixed-point passes.
│       ├── lr.rs         `eliminate_direct_lr` / `eliminate_indirect_lr`.
│       ├── prefix.rs     `factor_common_prefixes` — trie-style byte-level splitting.
│       ├── span.rs       `refine_span_eligibility`, `compute_sp_method_rules`.
│       ├── regex_info.rs `compute_regex_info` — caches `bbnf_regex::RegexInfo` per pattern.
│       ├── context/      Context-facts propagation (role-in-rule).
│       │   ├── mod.rs    Re-exports.
│       │   ├── facts.rs  `ContextFacts`, `DiscriminationStrength`, `ScanSafety`.
│       │   └── propagate.rs  `compute_context_facts`.
│       ├── patterns/     Structural shape-fact recognition.
│       │   ├── mod.rs    Entry point.
│       │   └── recognize.rs  `recognize_patterns`.
│       ├── transform/    Destructive cross-rule normalizer.
│       │   ├── mod.rs    Re-exports `canonicalize_aliases` / `prune_unreachable` /
│       │   │              `inline_acyclic` / `fuse_single_use` / `eliminate_epsilon` /
│       │   │              `merge_literals` / `fuse_token_dispatch`.
│       │   ├── alias.rs  Alias-chain resolution.
│       │   ├── prune.rs  Dead-rule pruning.
│       │   ├── inline.rs Small acyclic body inlining.
│       │   ├── fuse.rs   Single-use rule fusing.
│       │   ├── optimize.rs  `eliminate_epsilon` + `merge_literals`.
│       │   └── fuse_token/
│       │       ├── mod.rs  `fuse_token_dispatch` public entry.
│       │       ├── detect.rs  `is_keyword_node`, `starts_with_different_token`,
│       │       │               `leading_first_set`, `strip_leading_keyword`.
│       │       └── factor.rs  `factor_with_token`, `try_factor_alt`, `FactorCtx`,
│       │                       `new_branches_with_rest`.
│       ├── sets/         Dependency, SCC, FIRST/FOLLOW, dispatch, sort.
│       │   ├── mod.rs    Re-exports.
│       │   ├── deps.rs   `compute_rule_deps`.
│       │   ├── scc.rs    Tarjan SCC.
│       │   ├── first_sets.rs  FIRST set fixed-point iteration.
│       │   ├── follow.rs `compute_follow_sets`.
│       │   ├── factor_lookahead.rs  `factor_regex_with_lookahead`.
│       │   ├── sort.rs   `sort_alt_branches`.
│       │   └── dispatch/
│       │       ├── mod.rs  `generate_dispatch_tables` — CSP-driven, rayon-parallel tree walk.
│       │       ├── domain.rs  `DispatchDomain` tri-state lattice.
│       │       ├── constraint.rs  `DisjointConstraint` pairwise check.
│       │       ├── first_set.rs  `node_first_set` / `nullable_part` / `suffix_follow` helpers.
│       │       ├── eligibility.rs  `precompute_dispatch_eligibility`, `collect_alts`,
│       │       │                    `is_pairwise_disjoint`.
│       │       ├── annotate.rs  `annotate_node` tree walk installing `AltDispatch`.
│       │       └── build.rs  `try_build_dispatch` / `try_build_fallback_dispatch`.
│       └── types/        CSP-driven `IrNode → TypeDesc` projection.
│           ├── mod.rs    `project_types` entry + structural-type correction walk.
│           ├── generate.rs  Constraint generation from the IR structure.
│           ├── subvariants.rs  Sub-variant collection + cross-rule uniqueness validation.
│           ├── utils.rs  `TypeMap` (NodeId-keyed), `try_flatten_pair` helper.
│           └── constraint/
│               ├── mod.rs  Re-exports.
│               ├── domain.rs  `TypeDomain` + `Domain`/`LatticeDomain` impls.
│               ├── helpers.rs  `assign` / `project_seq_type` / `join_types`.
│               ├── grounds.rs  `GroundConstraint`, `EqualConstraint`.
│               ├── seq.rs   `SeqConstraint`, `SeqChildKind`.
│               ├── alt.rs   `AltConstraint`, `AltInVecConstraint`.
│               └── operators.rs  `OptionalConstraint`, `RepeatConstraint`,
│                                  `ProjectConstraint`, `MapConstraint`.
```

## Key Types

- **`IrNode`** — Expression tree node (Literal, Regex, Epsilon, Seq, Alt, Repeat, Ref, Skip, Next, Minus, Negate, Map, OptionalWhitespace).
- **`GrammarIR`** — Top-level container: rules, entry point, string interning table, host function table, types, FOLLOW sets, `ws_pattern`, `collapse_simple_spans`, `debug_all`, `debug_labels`, `dag` (the durable DAG substrate), `type_map`.
- **`GrammarSpan`** / **`SourceMapEntry`** — Source location tracking for debug and error reporting.
- **`IrRule`** — Rule id + name + body (`IrNode`) + metadata (`RuleMeta`).
- **`RuleMeta`** — FIRST set, nullable, SCC info, memo strategy, dispatch hint, span eligibility, pretty hints, recover sync, sub-variants, `is_token`, `debug`.
- **`Op::DebugBreak`** — Bytecode opcode for debug instrumentation (rule entry/exit).
- **`DebugState`** / **`StepMode`** / **`DebugAction`** — VM debug stepping control (into, over, out, continue, breakpoint filtering).
- **`DebugSnapshot`** / **`TraceEntry`** — Captured parse state at a debug break: rule stack, offset, input slice.
- **`AltDispatch`** — 128-entry byte→branch dispatch table for alternations with disjoint FIRST sets.
- **`FnDescriptor`** — Host function descriptor (EnumWrap, BoxWrap, Custom closure).
- **`TypeDesc`** — Serialized type info (Span, Option, Vec, Tuple, BoxedEnum, Enum, Named).
- **`NodeId`** / **`GrammarDag`** — Hash-consed identity for every distinct sub-expression. All downstream `NodeId`-keyed maps (`TypeMap`, `NodeFacts`, alt strategies) resolve through `GrammarDag::node_for`.

## IR Pass Pipeline

16 operations (14 unique passes) run in this exact order (must stay in sync
with `bbnf/src/pipeline/compile.rs` and `bbnf-derive/src/lib.rs`):

1. `canonicalize_aliases` — resolve alias chains to direct references (O(1) lookup)
2. `prune_unreachable` — remove rules not reachable from entry (O(1) rule lookup)
3. `inline_acyclic` — inline small acyclic rule bodies at call sites (threshold: 4 nodes)
4. `prune_unreachable` *(second pass)* — remove rules made dead by inlining
5. `fuse_single_use` — inlines rules referenced exactly once regardless of body size, guarded by SCC membership
6. `prune_unreachable` *(third pass)* — remove rules made dead by fusing
7. `eliminate_epsilon` — simplify epsilon-containing sequences/alternations; extended to handle `Repeat(Epsilon,0,..)→Epsilon`, `Skip(Epsilon,x)→x`, `Next(x,Epsilon)→x`, nested `OptionalWhitespace` fusion
8. `merge_literals` — fuse adjacent literals in sequences (with string deduplication)
9. `factor_common_prefixes` — left-factor shared prefixes in alternations
10. `sort_alt_branches` — sort alternation branches for deterministic codegen
11. `refine_span_eligibility` — propagate span eligibility through rule graph
12. `compute_follow_sets` — FOLLOW set fixed-point iteration (with Repeat inner Seq propagation, regex FIRST sets)
13. `factor_regex_with_lookahead` — factor Alt branches with overlapping regex FIRST sets but disjoint continuation FIRST sets
14. `fuse_token_dispatch` — fuse `@token`-marked rules at dispatch call sites (inline body, preserve variant)
15. `generate_dispatch_tables` — build O(1) byte-dispatch for disjoint alternations via CSP pre-computation + parallel tree walk
16. `project_types` — populate `GrammarIR::type_map` and `ir.types` via AC-3 CSP propagation; `NodeId`-keyed throughout

**Removed in Tranche H-7**: `merge_regex_alts` and `simplify_regex_algebra`
used to run between `merge_literals` and `factor_common_prefixes` (18
operations total). They are now handled by the retained grammar-tier
e-graph rules (`DeduplicateAltBranches`, `SupersetAbsorbAlt`,
`UnionMergeAlt`, `FuseAltRegexBranches` in `crate::egraph::rules::regex`)
plus the HIR e-graph saturation in `bbnf-regex`. Grammar-tier rules run
during the single post-normalizer e-graph saturation; HIR saturation
runs inside `RegexInfo::analyze_from_hir` on every pattern string.

## Durable DAG substrate

`GrammarDag::from_ir` is called **exactly once per compile** in
`crates/core/src/pipeline/compile.rs` (currently line 430) after the
body-mutating facts passes converge (`compute_follow_sets`,
`factor_regex_with_lookahead`, `fuse_token_dispatch`) and before the
stable-DAG fact passes (`generate_dispatch_tables`,
`compute_regex_info`, `recognize_patterns`, `project_types`). Every
downstream `NodeId`-keyed consumer depends on this invariant.

`project_types` asserts `ir.dag.is_some()` at entry.
`generate_dispatch_tables` likewise expects it and panics with a
`pipeline::compile`-pointed message if absent. Tests and benches
that exercise a single pass in isolation must call
`bbnf_ir::dag::ensure_dag(&mut ir)` before invoking the pass
directly. A Tranche I grep test enforces exactly one production
call site for `GrammarDag::from_ir`, plus the `ensure_dag` helper
and test files.

`GrammarDag::node_for` maintains a `HashMap<*const IrNode, NodeId>`
reverse-pointer map for tree-occurrence lookup — valid for the
lifetime of the borrowed `&GrammarIR` the DAG was built from. This
is correct by design (the DAG outlives no tree mutation) and is
not part of the orphan pointer-identity purge.

## Serialization

MessagePack (`to_msgpack`/`from_msgpack`) for WASM boundary transfer.
JSON (`to_json`/`from_json`) for debugging.

## Dependencies

- **serde** + **rmp-serde** — serialization
- **csp-solver** — AC-3 propagation for `generate_dispatch_tables` + `project_types`
- **rayon** — parallel tree walks in `generate_dispatch_tables`
- **bbnf-regex** — `CharSet128`, `regex_first_chars`, `RegexInfo`
- **egraph** + **egraph-derive** — e-graph substrate + `Language` derive for `GrammarENode`

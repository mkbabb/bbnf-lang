# CLAUDE.md — crates/ir/

Canonical grammar IR between the BBNF frontend and all backends (Rust codegen,
bytecode VM, TS interpreter, pretty-printing). Fully owned (no lifetimes),
serializable via MessagePack for WASM boundary transfer.

## Structure

```
bbnf-ir/
├── Cargo.toml
├── src/
│   ├── lib.rs            GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc, FnDescriptor (re-exports vm/)
│   ├── charset.rs        CharSet128 — 128-bit ASCII bitset (portable, no SIMD)
│   ├── regex_first.rs    Conservative FIRST char extraction from regex patterns
│   ├── vm/
│   │   ├── mod.rs        Re-exports bytecode, compiler, interpreter
│   │   ├── bytecode.rs   Opcode enum (incl. DebugBreak), Program struct, DebugState, StepMode, DebugAction, DebugSnapshot, TraceEntry
│   │   ├── compiler.rs   GrammarIR → bytecode Program; compile_with_debug() for DebugBreak instrumentation
│   │   └── interpreter.rs  Bytecode VM interpreter (ParseResult, captures); rule_stack_snapshot() for debug frames
│   └── passes/
│       ├── mod.rs         Re-exports all passes
│       ├── alias.rs       canonicalize_aliases — resolve A = B chains
│       ├── prune.rs       prune_unreachable — remove rules unreachable from entry
│       ├── inline.rs      inline_acyclic — inline small acyclic rule bodies
│       ├── fuse.rs        fuse_single_use — inline single-use rules regardless of size
│       ├── optimize.rs    eliminate_epsilon, merge_literals
│       ├── merge_regex.rs merge_regex_alts — fuse Alt([Regex, ...]) into single pattern
│       ├── prefix.rs      factor_common_prefixes — left-factor shared prefixes
│       ├── span.rs        refine_span_eligibility, compute_sp_method_rules
│       ├── follow.rs      compute_follow_sets — FOLLOW set fixed-point iteration
│       ├── factor_lookahead.rs  factor_regex_with_lookahead — lookahead-based regex factoring
│       ├── dispatch.rs    generate_dispatch_tables — O(1) byte-dispatch for disjoint alts
│       ├── fuse_token.rs  fuse_token_dispatch — inline @token bodies at dispatch call sites
│       ├── sort.rs        sort_alt_branches — deterministic branch ordering
│       └── types/         project_types — IrNode → TypeDesc type projection
│           ├── mod.rs     Entry point (project_types), orchestration
│           ├── project.rs Core recursive projection (project_node, project_node_in_vec, project_seq)
│           ├── subvariants.rs  Sub-variant collection, walking, uniqueness validation
│           └── utils.rs   ProjectionCtx struct, ProjectionRules struct, TypeRecorder, try_flatten_pair helper
```

## Key Types

- **`IrNode`** — Expression tree node (Literal, Regex, Epsilon, Seq, Alt, Repeat, Ref, Skip, Next, Minus, Negate, Map, OptionalWhitespace).
- **`GrammarIR`** — Top-level container: rules, entry point, string interning table, host function table, types, FOLLOW sets, `ws_pattern`, `collapse_simple_spans`, `debug_all`, `debug_labels`.
- **`GrammarSpan`** / **`SourceMapEntry`** — Source location tracking for debug and error reporting.
- **`IrRule`** — Rule id + name + body (`IrNode`) + metadata (`RuleMeta`).
- **`RuleMeta`** — FIRST set, nullable, SCC info, memo strategy, dispatch hint, span eligibility, pretty hints, recover sync, sub-variants, `is_token`, `debug`.
- **`Op::DebugBreak`** — Bytecode opcode for debug instrumentation (rule entry/exit).
- **`DebugState`** / **`StepMode`** / **`DebugAction`** — VM debug stepping control (into, over, out, continue, breakpoint filtering).
- **`DebugSnapshot`** / **`TraceEntry`** — Captured parse state at a debug break: rule stack, offset, input slice.
- **`AltDispatch`** — 128-entry byte→branch dispatch table for alternations with disjoint FIRST sets.
- **`FnDescriptor`** — Host function descriptor (EnumWrap, BoxWrap, Custom closure).
- **`TypeDesc`** — Serialized type info (Span, Option, Vec, Tuple, BoxedEnum, Enum, Named).

## IR Pass Pipeline

18 operations (16 unique passes) run in this exact order (must stay in sync
with `bbnf/src/pipeline.rs` and `bbnf-derive/src/lib.rs`):

1. `canonicalize_aliases` — resolve alias chains to direct references (O(1) lookup)
2. `prune_unreachable` — remove rules not reachable from entry (O(1) rule lookup)
3. `inline_acyclic` — inline small acyclic rule bodies at call sites (threshold: 4 nodes)
4. `prune_unreachable` *(second pass)* — remove rules made dead by inlining
5. `fuse_single_use` — inlines rules referenced exactly once regardless of body size, guarded by SCC membership
6. `prune_unreachable` *(third pass)* — remove rules made dead by fusing
7. `eliminate_epsilon` — simplify epsilon-containing sequences/alternations; extended to handle `Repeat(Epsilon,0,..)→Epsilon`, `Skip(Epsilon,x)→x`, `Next(x,Epsilon)→x`, nested `OptionalWhitespace` fusion
8. `merge_literals` — fuse adjacent literals in sequences (with string deduplication)
9. `merge_regex_alts` — combine regex/literal alternation branches into one pattern (mixed literal+regex fusion)
10. `factor_common_prefixes` — left-factor shared prefixes in alternations
11. `sort_alt_branches` — sort alternation branches for deterministic codegen
12. `refine_span_eligibility` — propagate span eligibility through rule graph
13. `compute_follow_sets` — FOLLOW set fixed-point iteration (with Repeat inner Seq propagation, regex FIRST sets)
14. `factor_regex_with_lookahead` — factor Alt branches with overlapping regex FIRST sets but disjoint continuation FIRST sets
15. `fuse_token_dispatch` — fuse `@token`-marked rules at dispatch call sites (inline body, preserve variant)
16. `generate_dispatch_tables` — build O(1) byte-dispatch for disjoint alternations (regex FIRST sets via `regex_first` module)
17. `project_types` — populate `GrammarIR::types` with `TypeDesc` for each rule; `project_node_in_vec` sub-pass handles Vec context type projection

## Durable DAG substrate

`GrammarDag::from_ir` is called **exactly once per compile** in
`crates/core/src/pipeline/compile.rs` (currently line 409) after the
body-mutating facts passes converge (`compute_follow_sets`,
`factor_regex_with_lookahead`, `fuse_token_dispatch`) and before the
stable-DAG fact passes (`generate_dispatch_tables`,
`compute_regex_info`, `recognize_patterns`, `project_types`). Every
downstream `NodeId`-keyed consumer depends on this invariant.

`project_types` asserts `ir.dag.is_some()` at entry. Tests and
benches that exercise a single pass in isolation must call
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
- **regex** — used by `merge_regex_alts` pass for pattern validation 
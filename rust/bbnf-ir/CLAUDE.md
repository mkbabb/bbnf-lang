# CLAUDE.md — rust/bbnf-ir/

Canonical grammar IR between the BBNF frontend and all backends (Rust codegen,
bytecode VM, TS interpreter, pretty-printing). Fully owned (no lifetimes),
serializable via MessagePack for WASM boundary transfer.

## Structure

```
bbnf-ir/
├── Cargo.toml
├── src/
│   ├── lib.rs            GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc, FnDescriptor
│   ├── charset.rs        CharSet128 — 128-bit ASCII bitset (portable, no SIMD)
│   ├── regex_first.rs    Conservative FIRST char extraction from regex patterns
│   ├── compiler.rs       GrammarIR → bytecode Program
│   ├── interpreter.rs    Bytecode VM interpreter (ParseResult, captures)
│   ├── bytecode.rs       Opcode enum, Program struct
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
│       ├── dispatch.rs    generate_dispatch_tables — O(1) byte-dispatch for disjoint alts
│       ├── memo.rs        refine_memo_strategies — selective memoization heuristics
│       └── types/         infer_types — IrNode → TypeDesc inference
│           ├── mod.rs     Entry point (infer_types), orchestration
│           ├── infer.rs   Core recursive inference (infer_node, infer_node_in_vec, infer_seq)
│           ├── subvariants.rs  Sub-variant collection, walking, uniqueness validation
│           └── utils.rs   InferCtx struct, try_flatten_pair helper
```

## Key Types

- **`IrNode`** — Expression tree node (Literal, Regex, Epsilon, Seq, Alt, Repeat, Ref, Skip, Next, Minus, Negate, Map, OptionalWhitespace).
- **`GrammarIR`** — Top-level container: rules, entry point, string interning table, host function table, types, FOLLOW sets.
- **`IrRule`** — Rule id + name + body (`IrNode`) + metadata (`RuleMeta`).
- **`RuleMeta`** — FIRST set, nullable, SCC info, memo strategy, dispatch hint, span eligibility, pretty hints, recover sync, sub-variants.
- **`AltDispatch`** — 128-entry byte→branch dispatch table for alternations with disjoint FIRST sets.
- **`FnDescriptor`** — Host function descriptor (EnumWrap, BoxWrap, Custom closure).
- **`TypeDesc`** — Serialized type info (Span, Option, Vec, Tuple, BoxedEnum, Enum, Named).

## IR Pass Pipeline

15 operations (13 unique passes) run in this exact order (must stay in sync
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
11. `refine_span_eligibility` — propagate span eligibility through rule graph
12. `compute_follow_sets` — FOLLOW set fixed-point iteration (with Repeat inner Seq propagation, regex FIRST sets)
13. `generate_dispatch_tables` — build O(1) byte-dispatch for disjoint alternations (regex FIRST sets via `regex_first` module)
14. `refine_memo_strategies` — assign memoization strategies (None/Full/Selective)
15. `infer_types` — populate `GrammarIR::types` with `TypeDesc` for each rule; `infer_node_in_vec` sub-pass handles Vec context inference

## Serialization

MessagePack (`to_msgpack`/`from_msgpack`) for WASM boundary transfer.
JSON (`to_json`/`from_json`) for debugging.

## Dependencies

- **serde** + **rmp-serde** — serialization
- **regex** — used by `merge_regex_alts` pass for pattern validation 
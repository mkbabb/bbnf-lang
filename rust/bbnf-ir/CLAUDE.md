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
│   ├── compiler.rs       GrammarIR → bytecode Program
│   ├── interpreter.rs    Bytecode VM interpreter (ParseResult, captures)
│   ├── bytecode.rs       Opcode enum, Program struct
│   └── passes/
│       ├── mod.rs         Re-exports all passes
│       ├── alias.rs       canonicalize_aliases — resolve A = B chains
│       ├── prune.rs       prune_unreachable — remove rules unreachable from entry
│       ├── inline.rs      inline_acyclic — inline small acyclic rule bodies
│       ├── optimize.rs    eliminate_epsilon, merge_literals
│       ├── merge_regex.rs merge_regex_alts — fuse Alt([Regex, ...]) into single pattern
│       ├── prefix.rs      factor_common_prefixes — left-factor shared prefixes
│       ├── span.rs        refine_span_eligibility, compute_sp_method_rules
│       ├── follow.rs      compute_follow_sets — FOLLOW set fixed-point iteration
│       ├── dispatch.rs    generate_dispatch_tables — O(1) byte-dispatch for disjoint alts
│       ├── memo.rs        refine_memo_strategies — selective memoization heuristics
│       └── types.rs       infer_types — IrNode → TypeDesc inference
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

Passes run in this exact order (must stay in sync with `bbnf/src/pipeline.rs`
and `bbnf-derive/src/lib.rs`):

1. `canonicalize_aliases` — resolve alias chains to direct references
2. `prune_unreachable` — remove rules not reachable from entry
3. `inline_acyclic` — inline small acyclic rule bodies at call sites
4. `eliminate_epsilon` — simplify epsilon-containing sequences/alternations
5. `merge_literals` — fuse adjacent literals in sequences
6. `merge_regex_alts` — combine regex-only alternation branches into one pattern
7. `factor_common_prefixes` — left-factor shared prefixes in alternations
8. `refine_span_eligibility` — propagate span eligibility through rule graph
9. `compute_follow_sets` — FOLLOW set fixed-point iteration
10. `generate_dispatch_tables` — build O(1) byte-dispatch for disjoint alternations
11. `refine_memo_strategies` — assign memoization strategies (None/Full/Selective)
12. `infer_types` — populate `GrammarIR::types` with `TypeDesc` for each rule

## Serialization

MessagePack (`to_msgpack`/`from_msgpack`) for WASM boundary transfer.
JSON (`to_json`/`from_json`) for debugging.

## Dependencies

- **serde** + **rmp-serde** — serialization
- **regex** — used by `merge_regex_alts` pass for pattern validation

# CLAUDE.md — rust/bbnf-derive/

Proc-macro crate. Generates parser code from `.bbnf` grammar files at compile time.

## Structure

```
bbnf-derive/
├── Cargo.toml      proc-macro = true
└── src/
    └── lib.rs      Derive macro entry point (~267 lines)
```

## Interface

```rust
#[derive(Parser)]
#[parser(path = "grammar.bbnf", ignore_whitespace, remove_left_recursion)]
pub struct MyParser;

// Generates:
//   pub enum MyParserEnum<'a> { ... }
//   impl MyParser { pub fn rule_name<'a>() -> Parser<'a, MyParserEnum<'a>> { ... } }
//   pub const GRAMMAR_MyParser: [&'static str; 1] = [include_str!(...)];
```

## Attributes

- `path = "..."` — Grammar file path(s). Required.
- `ignore_whitespace` — Wrap parsers with whitespace trimming.
- `debug` — Emit `.debug(name)` on each rule parser.
- `use_string` — Convert `Span<'a>` outputs to `&'a str`.
- `remove_left_recursion` — Eliminate direct left-recursion before codegen.

## Compilation Pipeline

```
Grammar file(s)
  → Parse + import resolution (BBNFGrammar, ModuleRegistry)
  → Analysis: Tarjan SCC, topological sort, FIRST sets, aliases, span-eligibility
  → Optional left-recursion elimination (Paull's + direct)
  → Lower to IR (lower_to_ir → GrammarIR)
  → IR passes (15 operations / 13 unique passes, must mirror pipeline.rs ordering):
      1. canonicalize_aliases      9. merge_regex_alts
      2. prune_unreachable        10. factor_common_prefixes
      3. inline_acyclic           11. refine_span_eligibility
      4. prune_unreachable (2nd)  12. compute_follow_sets
      5. fuse_single_use          13. generate_dispatch_tables
      6. prune_unreachable (3rd)  14. refine_memo_strategies
      7. eliminate_epsilon        15. infer_types
      8. merge_literals
  → Rust codegen: ir_codegen/ → TokenStream (enum + parser methods)
  → Grammar array embedding (include_str!)
```

## Key Optimizations

- **Dispatch tables**: O(1) byte-match for alternations with disjoint FIRST sets.
- **SpanParser dual methods**: `rule()` + `rule_sp()` for span-eligible rules.
- **JSON fast-paths**: Pattern-detect string/number regexes → SIMD-accelerated parsers.
- **Regex coalescing**: Fuse `lit >> many(regex) << lit` → single regex.
- **Alias elimination**: `A = B` chains resolved to direct method calls.
- **Inlining**: Acyclic rules inlined up to depth limit; non-acyclic use `lazy(|| ...)`.
- **source_range codegen**: Single-pass min/max fold instead of Vec allocation.

## Dependencies

All heavy lifting in `bbnf` crate (grammar, analysis, generate, imports).
This crate is the thin macro entry point that orchestrates the pipeline.

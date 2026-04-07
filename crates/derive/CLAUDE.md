# CLAUDE.md — crates/derive/

Proc-macro crate. Generates parser code from `.bbnf` grammar files at compile time.

## Structure

```
bbnf-derive/
├── Cargo.toml      proc-macro = true
└── src/
    └── lib.rs      Derive macro entry point (~513 lines)
```

## Interface

```rust
#[derive(Parser)]
#[parser(path = "grammar.bbnf", remove_left_recursion)]
pub struct MyParser;

// Generates:
//   pub enum MyParserEnum<'a> { ... }
//   impl MyParser { pub fn rule_name<'a>() -> Parser<'a, MyParserEnum<'a>> { ... } }
//   pub const GRAMMAR_MyParser: [&'static str; 1] = [include_str!(...)];
```

## Attributes

- `path = "..."` — Grammar file path(s). Required.
- `debug` — Emit trace instrumentation on each rule parser.
- `remove_left_recursion` — Eliminate direct left-recursion before codegen.

## Compilation Pipeline

```
Grammar file(s)
  → Parse + import resolution (BBNFGrammar, ModuleRegistry)
  → Analysis: Tarjan SCC, topological sort, FIRST sets, aliases, span-eligibility
  → Optional left-recursion elimination (Paull's + direct)
  → Lower to IR (lower_to_ir → GrammarIR)
  → IR passes (18 operations / 16 unique passes, must mirror pipeline.rs ordering):
      1. canonicalize_aliases      10. factor_common_prefixes
      2. prune_unreachable         11. sort_alt_branches
      3. inline_acyclic            12. refine_span_eligibility
      4. prune_unreachable (2nd)   13. compute_follow_sets
      5. fuse_single_use           14. factor_regex_with_lookahead
      6. prune_unreachable (3rd)   15. fuse_token_dispatch
      7. eliminate_epsilon         16. generate_dispatch_tables
      8. merge_literals            17. project_types
      9. merge_regex_alts
  → Rust codegen: codegen/ → TokenStream (enum + parser methods)
  → Grammar array embedding (include_str!)
```

## Key Optimizations

- **Content-based codegen cache**: Disk cache in `target/.bbnf-cache/` keyed by hash of all grammar file contents (including transitive `@import` deps), parser attributes, struct ident, and crate version. Skips the entire pipeline on cache hit. Cache files are `{hash}.rs` containing serialized TokenStream. Uses atomic write (tmp + rename). Falls through to full generation on any cache error.
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

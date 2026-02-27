# CLAUDE.md — rust/bbnf-derive/

Proc-macro crate. Generates parser code from `.bbnf` grammar files at compile time.

## Structure

```
bbnf-derive/
├── Cargo.toml      proc-macro = true
└── src/
    └── lib.rs      Derive macro entry point (436 lines)
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
  → Phase 0: Parse + import resolution (BBNFGrammar, ModuleRegistry)
  → Phase 1: Tarjan SCC + topological sort + acyclic classification
  → Phase 2: Left-recursion removal (opt-in), FIRST sets
  → Phase 3: Ref counts, aliases, transparent alternations, span-eligibility
  → Phase 4: Type calculation (Expression → syn::Type)
  → Phase 5: Enum generation (one variant per nonterminal)
  → Phase 6: Parser generation (dispatch tables, JSON fast-paths, inlining)
  → Phase 7: Grammar array embedding (include_str!)
```

## Key Optimizations

- **Dispatch tables**: O(1) byte-match for alternations with disjoint FIRST sets.
- **SpanParser dual methods**: `rule()` + `rule_sp()` for span-eligible rules.
- **JSON fast-paths**: Pattern-detect string/number regexes → SIMD-accelerated parsers.
- **Regex coalescing**: Fuse `lit >> many(regex) << lit` → single regex.
- **Alias elimination**: `A = B` chains resolved to direct method calls.
- **Inlining**: Acyclic rules inlined up to depth limit; non-acyclic use `lazy(|| ...)`.

## Dependencies

All heavy lifting in `bbnf` crate (grammar, analysis, generate, imports).
This crate is the thin macro entry point that orchestrates the pipeline.

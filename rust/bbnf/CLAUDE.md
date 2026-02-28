# CLAUDE.md — rust/bbnf/

Core BBNF library: grammar parsing, static analysis, and Rust code generation.

## Structure

```
bbnf/
├── Cargo.toml
├── src/
│   ├── lib.rs            Re-exports all modules
│   ├── types.rs          Expression, Token, AST, Comments, ImportDirective, ParsedGrammar
│   ├── grammar.rs        BBNF parser combinators (BBNFGrammar)
│   ├── analysis/
│   │   ├── mod.rs        Re-exports
│   │   ├── deps.rs       Dependencies type, calculate_ast_deps, traverse_ast
│   │   ├── scc.rs        Tarjan SCC, topological sort, acyclic/non-acyclic classification
│   │   ├── charset.rs    CharSet (128-bit ASCII bitset)
│   │   ├── first_sets.rs FIRST set computation (fixed-point iteration)
│   │   ├── regex_first.rs regex_first_chars + helpers
│   │   ├── dispatch.rs   DispatchTable, FIRST set conflict detection
│   │   └── metadata.rs   Ref counts, aliases, transparent alternations, span-eligibility
│   ├── generate/
│   │   ├── mod.rs        Re-exports + orchestrator
│   │   ├── types.rs      ParserAttributes, GeneratedNonterminalParser, caches
│   │   ├── type_inference.rs  Expression → syn::Type
│   │   ├── patterns.rs   regex coalesce, sep_by, wrapped, any_span detection
│   │   └── codegen.rs    Expression → TokenStream parser synthesis
│   ├── optimize.rs       Direct left-recursion elimination
│   └── imports.rs        Module system: @import resolution, DFS loader
└── tests/
    ├── common/mod.rs     Shared helpers: nt(), lit()
    ├── analysis.rs       CharSet, regex_first, Tarjan SCC, ref counts, dispatch tables
    ├── optimize.rs       Left-recursion elimination
    └── imports.rs        Module graph loading (tempfile-based)
```

## Key Types

- **`Expression<'a>`** — AST node enum (25+ variants): Literal, Regex, Nonterminal, Concatenation, Alternation, Skip, Next, Many, etc.
- **`Token<'a, T>`** — Value + Span + optional comments. `Token::inner()` returns `&T`.
- **`AST<'a>`** — `IndexMap<Expression, Expression>`. Rule LHS → RHS, insertion-ordered.
- **`ParsedGrammar<'a>`** — Imports + AST.
- **`ImportDirective<'a>`** — Path, span, optional selective items.

## Modules

### types.rs — AST Types
All AST node types, `Token` struct (with `inner()` accessor), `Comment`, `Comments`, `PartialEq`/`Hash` impls. Separated from grammar.rs to break circular dependencies.

### grammar.rs — Parser
Recursive descent via `parse_that` combinators. Operator precedence (low→high):
alternation `|`, concatenation `,`, skip/next `<<`/`>>`, minus `-`, quantifiers `*`/`+`/`?`/`?w`.

Entry points: `BBNFGrammar::grammar()`, `BBNFGrammar::grammar_with_imports()`.

### analysis/ — Static Analysis
- **deps.rs**: Dependency graph construction (`calculate_ast_deps`), AST traversal (`traverse_ast`).
- **scc.rs**: Tarjan SCC — O(V+E) cycle detection. `SccResult` with reverse-topological ordering. Kahn's algorithm on SCC condensation DAG.
- **charset.rs**: 128-bit ASCII bitset (`[u32; 4]`). O(1) insert/lookup/union/disjointness.
- **first_sets.rs**: Fixed-point iteration. Acyclic rules: single pass. Cyclic SCCs: iterate until stable.
- **regex_first.rs**: Extract FIRST characters from regex patterns.
- **dispatch.rs**: `[i8; 128]` byte→branch mapping. Built only when all alternatives have disjoint, non-empty FIRST sets and are non-nullable. FIRST set conflict detection.
- **metadata.rs**: Reference counts, aliases, transparent alternations, span-eligible rules — codegen optimization metadata.

### generate/ — Code Generation
Emits `proc_macro2::TokenStream` for Rust parser methods.

- **types.rs**: `ParserAttributes`, `GeneratedNonterminalParser`, cache types, `DEFAULT_PARSERS`.
- **type_inference.rs**: Expression → `syn::Type` (Span, Option, Vec, tuple, Box<Enum>).
- **patterns.rs**: Pattern recognition — regex coalescing, sepBy detection, wrap detection, JSON fast-paths.
- **codegen.rs**: Expression → combinator calls (string, regex, then, one_of, lazy, etc.). Dispatch table codegen. Span coalescing.

Acyclic rules inline up to a depth limit. Non-acyclic rules wrapped in `lazy(|| ...)`.

### optimize.rs — Left-Recursion Elimination
Standard algorithm: `A = Aα | β` → `A = βA'`, `A' = αA' | ε`. Direct only.

### imports.rs — Module System
`ModuleRegistry` loads transitive imports via DFS. Cycle detection, selective import verification, name conflict detection. Non-transitive: A imports B, B imports C → A doesn't see C.

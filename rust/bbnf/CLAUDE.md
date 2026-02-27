# CLAUDE.md — rust/bbnf/

Core BBNF library: grammar parsing, static analysis, and Rust code generation.

## Structure

```
bbnf/
├── Cargo.toml
└── src/
    ├── lib.rs          Re-exports all modules
    ├── grammar.rs      BBNF parser → AST (523 lines)
    ├── analysis.rs     SCC, FIRST sets, dispatch tables (1675 lines)
    ├── generate.rs     Type inference + Rust parser codegen (1665 lines)
    ├── optimize.rs     Direct left-recursion elimination (218 lines)
    └── imports.rs      Module system: @import resolution (511 lines)
```

## Key Types

- **`Expression<'a>`** — AST node enum (25+ variants): Literal, Regex, Nonterminal, Concatenation, Alternation, Skip, Next, Many, etc.
- **`Token<'a, T>`** — Value + Span + optional comments.
- **`AST<'a>`** — `IndexMap<Expression, Expression>`. Rule LHS → RHS, insertion-ordered.
- **`ParsedGrammar<'a>`** — Imports + AST.
- **`ImportDirective<'a>`** — Path, span, optional selective items.

## Modules

### grammar.rs — Parser
Recursive descent via `parse_that` combinators. Operator precedence (low→high):
alternation `|`, concatenation `,`, skip/next `<<`/`>>`, minus `-`, quantifiers `*`/`+`/`?`/`?w`.

Entry points: `BBNFGrammar::grammar()`, `BBNFGrammar::grammar_with_imports()`.

### analysis.rs — Static Analysis
1. **Tarjan SCC** — O(V+E) cycle detection. `SccResult` with reverse-topological ordering.
2. **Topological sort** — Kahn's algorithm on SCC condensation DAG.
3. **CharSet** — 128-bit ASCII bitset (`[u32; 4]`). O(1) insert/lookup/union/disjointness.
4. **FIRST sets** — Fixed-point iteration. Acyclic rules: single pass. Cyclic SCCs: iterate until stable.
5. **Dispatch tables** — `[i8; 128]` byte→branch mapping. Built only when all alternatives have disjoint, non-empty FIRST sets and are non-nullable.
6. **Reference counts, aliases, transparent alternations, span-eligible rules** — codegen optimization metadata.

### generate.rs — Code Generation
Emits `proc_macro2::TokenStream` for Rust parser methods.

Key phases:
- Type inference: Expression → `syn::Type` (Span, Option, Vec, tuple, Box<Enum>).
- Parser synthesis: Expression → combinator calls (string, regex, then, one_of, lazy, etc.).
- Dispatch table codegen: `match byte { ... }` for disjoint alternations.
- Span coalescing: consecutive Span types merge into single Span.
- JSON fast-paths: `sp_json_string_quoted()`, `sp_json_number()` for known regex patterns.
- Regex coalescing: `literal >> many(regex) << literal` fused to single `sp_regex()`.

Acyclic rules inline up to a depth limit. Non-acyclic rules wrapped in `lazy(|| ...)`.

### optimize.rs — Left-Recursion Elimination
Standard algorithm: `A = Aα | β` → `A = βA'`, `A' = αA' | ε`. Direct only.

### imports.rs — Module System
`ModuleRegistry` loads transitive imports via DFS. Cycle detection, selective import verification, name conflict detection. Non-transitive: A imports B, B imports C → A doesn't see C.

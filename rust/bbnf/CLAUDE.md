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
│   ├── lower.rs         AST-to-IR lowering (Grammar → GrammarIR)
│   ├── pipeline.rs      Full lowering + codegen orchestrator (12-pass sequence)
│   ├── generate/
│   │   ├── mod.rs        Re-exports + orchestrator
│   │   ├── types.rs      ParserAttributes, GeneratedNonterminalParser, caches
│   │   ├── fast_paths.rs JSON pattern detection, SIMD-accelerated parser fast paths
│   │   ├── ir_codegen/   IR-based Rust codegen (split from monolithic ir_codegen.rs)
│   │   │   ├── mod.rs    Entry point, generate_all(), expression dispatch
│   │   │   ├── alt.rs    Alternation codegen (dispatch tables, sub-variants)
│   │   │   ├── seq.rs    Concatenation/sequence codegen
│   │   │   ├── repeat.rs Repetition codegen (many, sep_by, optional)
│   │   │   ├── wrap.rs   Skip/Next/Minus/Negate codegen
│   │   │   ├── infer.rs  IrNode → syn::Type inference
│   │   │   └── inline.rs Flat match-arm dispatch codegen (InlineCtx, emit_rule_body_inline)
│   │   ├── ir_enums.rs   Enum type generation from IR alternations
│   │   ├── ir_types.rs   IR-level type inference and mapping
│   │   ├── ir_pretty.rs  IR pretty-printing for debug output
│   │   ├── ir_span.rs    SpanParser dual-method codegen
│   │   └── prettify/
│   │       ├── mod.rs          @pretty codegen orchestrator
│   │       ├── to_doc.rs       to_doc() impl emission
│   │       ├── source_range.rs source_range() impl emission (single-pass min/max fold)
│   │       ├── prettify_utils.rs  Type helpers, expression unwrapping
│   │       ├── heuristics.rs   Auto-infer @pretty hints from rule shape
│   │       └── hints.rs        HINT_DEFS — shared hint names + descriptions
│   ├── optimize.rs       Direct left-recursion elimination
│   └── imports.rs        Module system: @import resolution, DFS loader
└── tests/
    ├── common/mod.rs     Shared helpers: nt(), lit()
    ├── analysis.rs       CharSet, regex_first, Tarjan SCC, ref counts, dispatch tables
    ├── optimize.rs       Left-recursion elimination
    ├── imports.rs        Module graph loading (tempfile-based)
    ├── lower.rs          AST-to-IR lowering tests
    ├── pipeline.rs       Full pipeline integration tests
    ├── recover.rs        @recover directive parsing and codegen
    └── regex_charclass.rs  Regex character class extraction tests
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
Emits `proc_macro2::TokenStream` for Rust parser methods. Two codegen paths:

- **ir_codegen/**: IR-based Rust codegen. Takes a `GrammarIR` and produces `TokenStream`. Split into sub-modules: `alt.rs` (alternation + dispatch tables), `seq.rs` (concatenation), `repeat.rs` (repetition + sep_by), `wrap.rs` (skip/next/minus/negate), `infer.rs` (IrNode → syn::Type), `inline.rs` (flat match-arm dispatch via `InlineCtx`/`emit_rule_body_inline`). Two codegen modes: combinator-based (default, builds parser combinator chains) and inline (emits flat match-arm dispatch). The `in_vec` parameter is threaded through codegen to emit `Vec<Enum>` instead of `Vec<Box<Enum>>` where safe.
- **ir_span.rs**: SpanParser dual-method codegen—generates `rule_sp()` alongside `rule()` for span-eligible rules.
- **types.rs**: `ParserAttributes`, `GeneratedNonterminalParser`, cache types, `DEFAULT_PARSERS`.
- **prettify/**: `@pretty` directive codegen. `to_doc.rs` emits `to_doc()` impls, `source_range.rs` emits `source_range()` impls (single-pass min/max fold instead of Vec allocation). `heuristics.rs` auto-infers hints from rule shape (toplevel, brace-delimited, large compound). `hints.rs` is the single source of truth for hint names/descriptions (shared with LSP).

Acyclic rules inline up to a depth limit. Non-acyclic rules wrapped in `lazy(|| ...)`.

### optimize.rs — Left-Recursion Elimination
Standard algorithm: `A = Aα | β` → `A = βA'`, `A' = αA' | ε`. Direct only.

### imports.rs — Module System
`ModuleRegistry` loads transitive imports via DFS. Cycle detection, selective import verification, name conflict detection. Non-transitive: A imports B, B imports C → A doesn't see C.

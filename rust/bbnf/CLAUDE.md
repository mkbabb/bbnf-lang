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
│   │   ├── mod.rs        Re-exports + regex_first_chars thin wrapper (delegates to bbnf_ir)
│   │   ├── deps.rs       Dependencies type, calculate_ast_deps, traverse_ast
│   │   ├── scc.rs        Tarjan SCC, topological sort, acyclic/non-acyclic classification
│   │   ├── charset.rs    CharSet (128-bit ASCII bitset)
│   │   ├── first_sets.rs FIRST set computation (fixed-point iteration)
│   │   ├── dispatch.rs   DispatchTable, FIRST set conflict detection
│   │   └── metadata.rs   Ref counts, aliases, transparent alternations, span-eligibility
│   ├── lower/           AST-to-IR lowering (Grammar → GrammarIR)
│   │   ├── mod.rs        Entry point (lower_to_ir), LowerCtx, orchestration
│   │   ├── string_interner.rs  StringInterner — dedup string literals
│   │   ├── fn_table.rs   FnTable — host function descriptors
│   │   ├── expression.rs Recursive expression/node lowering
│   │   └── metadata.rs   Rule metadata lowering (recover, pretty, token)
│   ├── pipeline.rs      Full lowering + codegen orchestrator (18-operation pass sequence)
│   ├── generate/
│   │   ├── mod.rs        Re-exports + orchestrator (hir_classify aliased as regex_classify)
│   │   ├── types.rs      ParserAttributes, GeneratedNonterminalParser, caches
│   │   ├── fast_paths/   Pattern detection, inline byte scanners, SIMD-accelerated fast paths
│   │   │   ├── mod.rs    Entry point + re-exports
│   │   │   ├── detect.rs Pattern detection for fast-path eligibility
│   │   │   ├── generalized.rs  Generalized regex-to-inline-scanner emission
│   │   │   ├── inline_scanners.rs  Inline scanner codegen (CSS ident, string, ws, etc.)
│   │   │   └── negated_class.rs  Negated char-class memchr emission
│   │   ├── hir_classify.rs HIR-based regex classification (Numeric, HexDigits, Identifier, QuotedString)
│   │   ├── codegen/      Monolithic Rust codegen
│   │   │   ├── mod.rs    Entry point, generate_all(), expression dispatch
│   │   │   ├── generate.rs  Top-level codegen orchestration
│   │   │   ├── helpers.rs   Shared codegen helpers
│   │   │   ├── alt/      Alternation codegen
│   │   │   │   ├── mod.rs       Alternation dispatch + sub-variants
│   │   │   │   ├── key_dispatch.rs  Key-based dispatch codegen
│   │   │   │   └── literal.rs  All-literal fast path
│   │   │   ├── seq.rs    Concatenation/sequence codegen
│   │   │   ├── repeat.rs Repetition codegen (many, optional)
│   │   │   ├── sep_by.rs Sep-by loop emission (bare, ws-aware, delimited-with-terminator)
│   │   │   ├── expr.rs   Leaf expression codegen
│   │   │   ├── alloc_emit.rs  Scratch emission + context generation
│   │   │   ├── delim_scan.rs  Delimiter-driven flat scanning optimization
│   │   │   ├── token_dispatch.rs  Token-level dispatch codegen
│   │   │   ├── trace.rs  Debug trace codegen (#[cfg(feature = "parser-trace")] instrumentation)
│   │   │   ├── prettify/ Prettify-mode codegen
│   │   │   │   ├── mod.rs    Entry point
│   │   │   │   ├── alt.rs    Prettify alternation codegen
│   │   │   │   ├── expr.rs   Prettify expression codegen
│   │   │   │   ├── repeat.rs Prettify repetition codegen
│   │   │   │   └── seq.rs    Prettify sequence codegen
│   │   │   └── span/     Span-only codegen
│   │   │       ├── mod.rs    Entry point
│   │   │       ├── alt.rs    Span alternation codegen
│   │   │       ├── expr.rs   Span expression codegen
│   │   │       └── repeat.rs Span repetition codegen
│   │   ├── ir_enums.rs   Enum type generation from IR alternations
│   │   ├── ir_types.rs   IR-level type projection and TypeMap lookup
│   │   └── regex_emit/   DFA-based regex emission
│   │       ├── mod.rs    Entry point
│   │       ├── audit.rs  Regex audit/validation
│   │       ├── dfa_emit.rs  DFA codegen emission
│   │       └── hir_walk.rs  HIR tree walking
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
- **dispatch.rs**: `[i8; 128]` byte→branch mapping. Built only when all alternatives have disjoint, non-empty FIRST sets and are non-nullable. FIRST set conflict detection.
- **metadata.rs**: Reference counts, aliases, transparent alternations, span-eligible rules — codegen optimization metadata.

### generate/ — Code Generation
Emits `proc_macro2::TokenStream` for Rust parser methods. Single monolithic codegen path:

- **codegen/**: Monolithic Rust codegen. Takes a `GrammarIR` and produces `TokenStream`. Single codegen path with arena allocation + optional prettify sub-path for `#[parser(prettify)]`. Split into sub-modules: `alt/` (alternation + dispatch tables + literal fast paths), `seq.rs` (concatenation), `repeat.rs` (repetition + optional), `sep_by.rs` (sep-by loop emission), `expr.rs` (leaf expressions), `alloc_emit.rs` (scratch emission + context generation), `delim_scan.rs` (delimiter-driven scanning), `trace.rs` (debug instrumentation). Sub-directory: `prettify/` (fused parse+format codegen). The `in_vec` parameter is threaded through codegen to emit `Vec<Enum>` instead of `Vec<Box<Enum>>` where safe. Type lookup via `TypeMap` populated by the `project_types` IR pass.
- **hir_classify.rs**: HIR-based regex classification via `regex-syntax` (`RegexClass` enum). `classify_regex()` decomposes patterns into Numeric/HexDigits/Identifier/QuotedString structurally. Aliased as `regex_classify` for backward compat. Used by `fast_paths/` for inline scanner selection and by `lower/expression.rs` for `FnDescriptor` specialization.
- **types.rs**: `ParserAttributes`, `GeneratedNonterminalParser`, cache types, `DEFAULT_PARSERS`.
- **fast_paths/**: Inline byte scanner emission. `detect.rs` for pattern detection, `inline_scanners.rs` for CSS ident/string/ws fast paths, `negated_class.rs` for memchr emission, `generalized.rs` for generalized regex-to-scanner.
- **regex_emit/**: DFA-based regex emission. `dfa_emit.rs` for DFA codegen, `hir_walk.rs` for HIR tree walking, `audit.rs` for regex validation.

### optimize.rs — Left-Recursion Elimination
Standard algorithm: `A = Aα | β` → `A = βA'`, `A' = αA' | ε`. Direct only.

### imports.rs — Module System
`ModuleRegistry` loads transitive imports via DFS. Cycle detection, selective import verification, name conflict detection. Non-transitive: A imports B, B imports C → A doesn't see C.

## Conventions

- **`@token` directive**: `@token ruleName ;` marks a rule as a lexical token. `RuleMeta::is_token` carries the flag through lowering. Implies span eligibility. Uses fusion-style inlining (body inlined at call sites, enum variant preserved for `@pretty` compatibility).
- **`@debug` directive**: `@debug ruleName ;` / `@debug * ;` instruments rules for trace output. `codegen/trace.rs` emits `#[cfg(feature = "parser-trace")]` instrumentation for monolithic paths. `RuleMeta::debug` and `GrammarIR::debug_all` carry the directive through lowering.
- **`hir_classify.rs`**: HIR-based regex classification via `regex-syntax` for during-parse value conversion. `classify_regex()` analyzes HIR components to detect `Numeric` (sign, fraction, exponent structure), `HexDigits`, `Identifier`, and `QuotedString` classes structurally. Aliased as `regex_classify` in `generate/mod.rs` for backward compat. Drives `FnDescriptor` specialization in `lower/expression.rs` and inline scanner selection in `fast_paths/`.
- **`fast_paths/` CSS extensions**: Beyond JSON, emits inline scanners for CSS patterns: `css_ident_fast` (identifiers), `css_string_fast` (quoted strings), `css_ws_comment_fast` (comment-aware whitespace), comma-or-whitespace `,|\s+`, and generalized char-class/negated-class loops via `generalized.rs`. The structural classifier feeds unsigned-numeric and identifier patterns into these fast paths automatically.
- **`->` lowering in `lower/expression.rs`**: `try_specialize_map_fn` detects `Regex(numeric) -> f64` and `Regex(hex) -> user_fn` patterns using `regex_classify`, upgrading generic `FnDescriptor::Custom` to `NumberConvert`, `HexConvert`, or `Constant`. Constant detection recognizes literal expressions, numeric suffixed values, and boolean keywords.

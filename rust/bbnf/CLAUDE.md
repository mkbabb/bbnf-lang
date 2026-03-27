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
│   ├── lower/           AST-to-IR lowering (Grammar → GrammarIR)
│   │   ├── mod.rs        Entry point (lower_to_ir), LowerCtx, orchestration
│   │   ├── string_interner.rs  StringInterner — dedup string literals
│   │   ├── fn_table.rs   FnTable — host function descriptors
│   │   ├── expression.rs Recursive expression/node lowering
│   │   └── metadata.rs   Rule metadata lowering (recover, pretty, no_collapse, token)
│   ├── pipeline.rs      Full lowering + codegen orchestrator (15-operation pass sequence)
│   ├── generate/
│   │   ├── mod.rs        Re-exports + orchestrator
│   │   ├── types.rs      ParserAttributes, GeneratedNonterminalParser, caches
│   │   ├── fast_paths.rs Pattern detection, inline byte scanners, SIMD-accelerated fast paths
│   │   ├── regex_classify.rs Structural regex classification (Numeric, HexDigits, Identifier, QuotedString)
│   │   ├── ir_codegen/   IR-based Rust codegen (split from monolithic ir_codegen.rs)
│   │   │   ├── mod.rs    Entry point, generate_all(), expression dispatch
│   │   │   ├── alt.rs    Alternation codegen (dispatch tables, sub-variants)
│   │   │   ├── seq.rs    Concatenation/sequence codegen
│   │   │   ├── repeat.rs Repetition codegen (many, sep_by, optional)
│   │   │   ├── wrap.rs   Skip/Next/Minus/Negate codegen
│   │   │   ├── infer.rs  IrNode → syn::Type inference
│   │   │   ├── inline.rs Flat match-arm dispatch codegen (InlineCtx, emit_rule_body_inline)
│   │   │   └── trace.rs  Debug trace codegen (#[cfg(feature = "parser-trace")] instrumentation)
│   │   ├── ir_enums.rs   Enum type generation from IR alternations
│   │   ├── ir_types.rs   IR-level type inference and mapping
│   │   ├── ir_pretty/    IR-based @pretty codegen
│   │   │   ├── mod.rs    Entry point (generate_prettify_ir), main loop
│   │   │   ├── patterns.rs  IR pattern detection (wrapped reps, key-value)
│   │   │   ├── heuristics.rs  Heuristic hint inference, mode resolution
│   │   │   ├── codegen.rs  Doc generation wrappers, sub-variant arms
│   │   │   └── utils.rs   Hint conversion, IR unwrapping helpers
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
- **regex_classify.rs**: Structural regex classification (`RegexClass` enum). `classify_regex()` decomposes patterns into Numeric/HexDigits/Identifier/QuotedString without exact string lists. Used by `fast_paths.rs` for inline scanner selection and by `lower/expression.rs` for `FnDescriptor` specialization.
- **types.rs**: `ParserAttributes`, `GeneratedNonterminalParser`, cache types, `DEFAULT_PARSERS`.
- **ir_pretty/**: IR-based `@pretty` codegen. `patterns.rs` detects wrapped repetitions and key-value structures. `heuristics.rs` infers hints from rule shape (toplevel, brace-delimited, large compound). `codegen.rs` generates doc wrappers and sub-variant match arms. `utils.rs` handles hint conversion and IR node unwrapping.
- **prettify/**: AST-based `@pretty` directive codegen. `to_doc.rs` emits `to_doc()` impls, `source_range.rs` emits `source_range()` impls (single-pass min/max fold instead of Vec allocation). `heuristics.rs` auto-infers hints from rule shape. `hints.rs` is the single source of truth for hint names/descriptions (shared with LSP).

Acyclic rules inline up to a depth limit. Non-acyclic rules wrapped in `lazy(|| ...)`.

### optimize.rs — Left-Recursion Elimination
Standard algorithm: `A = Aα | β` → `A = βA'`, `A' = αA' | ε`. Direct only.

### imports.rs — Module System
`ModuleRegistry` loads transitive imports via DFS. Cycle detection, selective import verification, name conflict detection. Non-transitive: A imports B, B imports C → A doesn't see C.

## Conventions

- **`@token` directive**: `@token ruleName ;` marks a rule as a lexical token. `RuleMeta::is_token` carries the flag through lowering. Implies span eligibility. Uses fusion-style inlining (body inlined at call sites, enum variant preserved for `@pretty` compatibility).
- **`@debug` directive**: `@debug ruleName ;` / `@debug * ;` instruments rules for trace output. `ir_codegen/trace.rs` emits `#[cfg(feature = "parser-trace")]` instrumentation for monolithic paths; the combinator path wraps with `.debug("name")`. `RuleMeta::debug` and `GrammarIR::debug_all` carry the directive through lowering.
- **`regex_classify.rs`**: Structural regex classification for during-parse value conversion. `classify_regex()` analyzes pattern components to detect `Numeric` (sign, fraction, exponent structure), `HexDigits`, `Identifier`, and `QuotedString` classes without exact string matching. Drives `FnDescriptor` specialization in `lower/expression.rs` and inline scanner selection in `fast_paths.rs`.
- **`fast_paths.rs` CSS extensions**: Beyond JSON, emits inline scanners for CSS patterns: `css_ident_fast` (identifiers), `css_string_fast` (quoted strings), `css_ws_comment_fast` (comment-aware whitespace), comma-or-whitespace `,|\s+`, and generalized char-class/negated-class loops via `emit_generalized_regex_direct`. The structural classifier feeds unsigned-numeric and identifier patterns into these fast paths automatically.
- **`->` lowering in `lower/expression.rs`**: `try_specialize_map_fn` detects `Regex(numeric) -> f64` and `Regex(hex) -> user_fn` patterns using `regex_classify`, upgrading generic `FnDescriptor::Custom` to `NumberConvert`, `HexConvert`, or `Constant`. Constant detection recognizes literal expressions, numeric suffixed values, and boolean keywords.

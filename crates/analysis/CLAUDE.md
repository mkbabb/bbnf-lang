# CLAUDE.md — crates/analysis/

LSP analysis engine. Pure analysis logic shared by the LSP server and WASM crate.

## Structure

```
bbnf-analysis/
├── Cargo.toml          v0.1.1, edition 2024
├── src/
│   ├── lib.rs          Re-exports all modules
│   ├── analysis.rs     LineIndex, symbol lookup utilities
│   ├── directives/
│   │   ├── mod.rs      Re-exports
│   │   └── hints.rs    HINT_DEFS — shared hint names + descriptions (moved from bbnf/generate/prettify/)
│   ├── state/
│   │   ├── mod.rs              DocumentState struct, new/update/ast methods
│   │   ├── types.rs            RuleInfo, ReferenceInfo, SemanticTokenInfo, DocumentInfo, token_types
│   │   ├── parsing.rs          OwnedAst (self_cell), CachedParseResult, parse_once
│   │   ├── pretty.rs           @pretty extraction, validation, semantic tokens
│   │   ├── diagnostics/        Full diagnostic generation (directory module)
│   │   │   ├── mod.rs          analyze_from_cache orchestrator
│   │   │   ├── early.rs        Parse-panic / failure / empty-AST stub builders
│   │   │   ├── extract.rs      RuleInfo loop + duplicate detection
│   │   │   ├── references.rs   Undefined nonterminals + unused rules
│   │   │   ├── cycles.rs       Tarjan SCC + cycle paths
│   │   │   ├── structure.rs    Empty body + alias + unreachable
│   │   │   ├── directives.rs   Validation glue for @import/@recover/@pretty/@debug/@token/@ws
│   │   │   └── ir_analysis.rs  try_compile_ir + IrAnalysis + format_charset_iter + format_type_desc
│   │   └── ast_utils/          AST walking helpers (directory module)
│   │       ├── mod.rs          Re-exports + format_char, is_empty_rhs
│   │       ├── references.rs   collect_references
│   │       ├── tokens.rs       collect_semantic_tokens
│   │       ├── spans.rs        compute_expression_end + wrapper
│   │       ├── format.rs       format_expression_short + format_value_expr_short
│   │       └── cycles.rs       build_cycle_path + compute_reachable_rules
│   └── features/
│       ├── mod.rs              Module declarations
│       ├── hover/              Hover orchestrator + per-kind renderers (directory module)
│       │   ├── mod.rs          Public hover() orchestrator + lowercase_first helper
│       │   ├── rule.rs         build_rule_definition_hover + build_rule_reference_hover
│       │   ├── import.rs       hover_import
│       │   ├── directive.rs    hover_recover / hover_debug / hover_ws
│       │   └── pretty.rs       hover_pretty + build_hint_hover + build_pretty_directive_hover
│       ├── goto_definition.rs  Local + cross-file + import path navigation
│       ├── references.rs       Local + cross-file reference finding
│       ├── rename.rs           Rule + reference rename (single document)
│       ├── completion.rs       Rule names, keywords, imported rules, @pretty hints
│       ├── document_symbols.rs Outline of all rules
│       ├── code_lens.rs        Reference counts per rule
│       ├── folding.rs          Multi-line rule folding
│       ├── code_actions.rs     Remove unused / define undefined rules
│       ├── formatting.rs       Document, range, on-type (trigger: `;`)
│       ├── semantic_tokens.rs  ruleDefinition, ruleReference, string, regexp, keyword
│       ├── inlay_hints.rs      FIRST sets (non-trivial rules), nullable markers
│       └── selection_range.rs  Expression-level expand/shrink selection
```

## Key Types

- **DocumentState** — owns text + `OwnedAst` (self-referential via `self_cell`). Entry point for all analysis.
- **OwnedAst** — `self_cell` wrapper: owns the leaked `&'static str` text and borrows it into the parsed AST.
- **DocumentInfo** — rules, diagnostics, semantic tokens, FIRST labels, nullable set, cycle paths, imports. Computed on every text change.
- **LineIndex** — pre-computed line starts for O(log n) offset↔position conversion.
- **IrRuleMeta** — per-rule metadata from `try_compile_ir()`: FOLLOW sets, dispatch table presence, memo strategy, span eligibility, inferred type.
- **ImportedItem** — imported rule with source path, used for hover enrichment on `@import` names.
- **DebugInfo** / **WsPatternInfo** — directive-specific metadata for hover and semantic tokens.

## Dependencies

- **bbnf** — grammar parser + analysis
- **pprint** — pretty printing
- **ls-types** — LSP type definitions
- **self_cell** — self-referential struct for AST caching
- **indexmap** — insertion-order HashMap
- **serde/serde_json** — serialization (for WASM interop)

## Feature Provider Pattern

Each file in `features/` exports a function that takes `&DocumentState` (plus position/range parameters) and returns an LSP response type. The LSP server (`lsp/src/server/protocol.rs`) and the WASM crate (`wasm/src/lsp.rs`) both call these functions directly—no protocol-level dispatch in the analysis crate itself.

## IR Pipeline Integration

When a grammar parses successfully, `state/diagnostics/ir_analysis.rs` runs `try_compile_ir()`—the full IR lowering and pass sequence from `bbnf::pipeline`—and caches per-rule `IrRuleMeta` on `DocumentInfo`. This enriches hover with FOLLOW sets, dispatch table presence, memo strategy, span eligibility, and inferred output type. The IR compilation is best-effort; analysis degrades gracefully if lowering fails.

---
title: Codegen Paths
order: 50
section: BBNF
---

# BBNF Code Generation Paths

Three independent pipelines transform `.bbnf` grammars into executable parsers and formatters.

## Pipeline Overview

```
                          .bbnf grammar source
                                 │
                    ┌────────────┼────────────┐
                    ▼            │             ▼
              bbnf::grammar      │        TS grammar.ts
              bbnf::analysis     │        TS analysis/
              bbnf::lower        │             │
                    │            │             ▼
                    ▼            │       [TS Interpreter]
               GrammarIR        │      ASTToParser() runtime
              (bbnf-ir)         │       combinator gen
                    │            │             │
          ┌─── 17 IR passes ────┤             ▼
          │   (bbnf-ir::passes) │        Parser<T> tree
          │                     │        (parse-that TS)
          ▼                     │             │
     [Rust AOT]                 │             ▼
   bbnf::generate               │        Direct execution
   → TokenStream                │        (prettier plugin)
          │                     │
          ▼                     ▼
   rustc → native        [Rust VM]
   (gorgeous, WASM)     bbnf-ir::compiler
                        → BytecodeProgram
                              │
                              ▼
                        bbnf-ir::interpreter
                        (WASM, gorgeous vm)
```

`bbnf-ir` is the shared core — it defines the canonical IR (`GrammarIR`, `IrNode`,
`IrRule`, `RuleMeta`, `TypeDesc`) and all 17 IR operations (15 unique passes). Both AOT and VM
consume the same optimized IR; they only diverge at the final step: Rust codegen
vs. bytecode compilation.

## 1. Rust AOT — `bbnf-derive` Proc Macro

Compile-time code generation. Reads `.bbnf`, emits Rust `TokenStream`.

**Entry:** `#[derive(Parser)]` + `#[parser(path = "grammar.bbnf")]`

**Phases (shared with VM through step 6):**
1. Parse grammar → AST (`bbnf::grammar`)
2. Resolve `@import` directives → module graph DFS
3. Extract `@recover`, `@pretty`, `@no_collapse`, `@debug` directives
4. Analysis — SCC detection, FIRST sets, alias detection, span eligibility
5. Lower AST → `GrammarIR` (`bbnf::lower`) — IR types from `bbnf-ir`
6. Apply 17 IR optimization passes (`bbnf-ir::passes`) — **same passes as VM**
7. **(AOT only)** Codegen → Rust `TokenStream` (`bbnf::generate`)

**IR Passes** (17 operations, 15 unique passes, in order):
`canonicalize_aliases` → `prune_unreachable` → `inline_acyclic` →
`force_inline` → `prune_unreachable` → `fuse_single_use` → `prune_unreachable` →
`eliminate_epsilon` → `merge_literals` → `merge_regex_alts` →
`factor_common_prefixes` → `refine_span_eligibility` → `compute_follow_sets` →
`factor_regex_with_lookahead` → `generate_dispatch_tables` → `project_types`

**Generated code per struct:**
- `ParserFn` trait impl — one method per rule, returns `Parser<'a, Enum<'a>>`
- `to_doc()` method — `@pretty`-directed `Doc` emission (if `#[parser(prettify)]`)
- `source_range()` — byte offset tracking for range formatting
- SpanParser methods (`_sp()`) for zero-copy eligible rules
- Dispatch tables for alternations, memoization caches for cyclic rules

**Attributes:**
| Attribute | Effect |
|-----------|--------|
| `path = "..."` | Grammar file path |
| `prettify` | Emit `to_doc()` codegen |
| `skip_recover` | Omit `@recover` codegen (well-formed input only) |
| `remove_left_recursion` | Paull transform before codegen |
| `ignore_whitespace` | Auto-trim |
| `debug` | Instrument all rules for trace output |
| `arena` | Monolithic arena codegen with BumpSlab |
| `span` | Span-only monolithic codegen (zero allocation) |

**Consumers:** gorgeous (5 built-in formatters), WASM AOT wrappers

## 2. Rust VM — Bytecode Compiler + Interpreter

Runtime grammar compilation. Shares steps 1-6 with AOT (parse → analyze → lower →
IR passes), then diverges: compiles the optimized `GrammarIR` to bytecode instead of
Rust `TokenStream`.

**Entry:** `bbnf_ir::compiler::compile()` + `bbnf_ir::interpreter::Interpreter::new()`

**Phases (shared with AOT through step 6):**
1. Parse grammar → AST (`bbnf::grammar`)
2. Resolve imports, extract directives
3. Analysis — SCC, FIRST sets, span eligibility
4. Lower AST → `GrammarIR` (`bbnf::lower`)
5. Apply 17 IR optimization passes (`bbnf-ir::passes`) — **same passes as AOT**
6. **(VM only)** Compile IR → `BytecodeProgram` (`bbnf-ir::compiler`)
7. Serialize via MessagePack (for WASM boundary crossing)
8. Interpret bytecode against input (`bbnf-ir::interpreter`)

**Bytecode opcodes** (`bbnf-ir::bytecode::Op`):
`MatchString` | `MatchRegex` | `Epsilon` | `Jump` | `Call` |
`SaveState` | `Dispatch` | `RepeatBegin` | ...

**Output:** `ParseResult { success, offset, value: Value, diagnostics }`
- `Value` variants: `Nil`, `Span(start, end)`, `Tagged { tag, span, children }`, `Array`

**Consumers:** WASM `compile_grammar()` + `parse_with_grammar()`, gorgeous `vm` feature

### Debug Codegen

`@debug` directives and the `#[parser(debug)]` attribute enable parse tracing across all three paths:

- **Compiled (AOT)**: `ir_codegen/trace.rs` emits `#[cfg(feature = "parser-trace")]` instrumentation around monolithic rule functions. Zero overhead when the feature flag is off.
- **Bytecode (VM)**: The compiler emits `Op::DebugBreak` opcodes at rule entry/exit. The interpreter supports stepping (into, over, out) and breakpoint filtering via `DebugState`.
- **DAP bridge**: `bbnf-lsp --dap` speaks Debug Adapter Protocol over stdin/stdout, bridging the VM interpreter to VS Code's debug UI—breakpoints on rules, call stack inspection, parse state variables.

### FnDescriptor Specialization — the `->` Pipeline

Grammar rules that use the `->` operator (value conversion) lower to `Map(inner, FnDescriptor)` nodes in the IR. The `try_specialize_map_fn` pipeline in `lower/expression.rs` detects specific patterns via `regex_classify` and emits specialized `FnDescriptor` variants instead of generic function calls:

| FnDescriptor | Lowered From | Emitted Code | Return Type |
|-------------|-------------|-------------|-------------|
| `NumberConvert` | `number -> /regex/ ;` where regex matches a numeric pattern | `::parse_that::css_number_scan_f64(state)` | `Option<f64>` |
| `HexConvert { fn_path }` | `hex -> /[0-9a-fA-F]+/ ;` with conversion function | Inline char-class byte loop + `fn_path(span.as_str())` | `Option<u32>` |
| `Constant { value }` | Literal match with fixed return value | Span construction elided, returns `value` directly | constant type |

`NumberConvert` is the highest-impact specialization. The default codegen for `number -> /[-+]?\d+(\.\d+)?([eE][-+]?\d+)?/ ;` would construct a `SpanParser::Regex`, dispatch through the enum, capture a `Span`, convert to `&str`, and call `str::parse::<f64>()`. The specialized path replaces the entire chain with a single `css_number_scan_f64` call—a hand-written byte scanner with fused Eisel-Lemire conversion that returns `Option<f64>` directly. Zero regex, zero string allocation, zero intermediate Span.

**Map fusion** further optimizes the common case where a specialized `FnDescriptor` feeds into an enum wrapper. The pairs `(NumberConvert, EnumWrap)` and `(Constant, EnumWrap)` are detected and fused to a single `.map()` closure, eliminating an intermediate allocation and function call boundary. The IR sees:

```
Map(Map(Regex, NumberConvert), EnumWrap)  →  Map(NumberScan, FusedNumberEnumWrap)
```

The result is one function call that scans bytes, converts to f64, and wraps in the target enum variant—three logical operations collapsed to one code path.

## 3. TypeScript Interpreter — Runtime Combinator Generation

Dynamic parser construction. Builds `parse-that` combinator tree at runtime.

**Entry:** `ASTToParser(ast, analysis?, firstNullable?, recovers?)`

**Phases:**
1. Parse grammar → AST (`BBNFToAST()` / `BBNFToASTWithImports()`)
2. Analysis — FIRST/FOLLOW sets, SCC detection, dispatch tables
3. Walk AST → build `Parser<T>` combinator tree
   - Pattern detection: wrap coalescing, sepBy detection, literal alternation dispatch
   - Memoization for cyclic rules, lazy references for user override
   - Zero-copy spans (`stringSpan`, `regexSpan`) for discarded results

**No code generation.** Produces live combinator objects, not source text.

**Consumers:** prettier-plugin-bbnf (parse `.bbnf` files for formatting)

---

## Crate Map

```
typescript/
  src/
    grammar.ts          Parse BBNF → AST
    analysis/           FIRST/FOLLOW, SCC, dispatch
    generate.ts         AST → Parser<T> combinator tree (runtime)
    optimize.ts         AST optimization (left-recursion)
    imports.ts          @import resolution
    imports-loader.ts   Module graph loading

rust/
  bbnf/                 Core library
    src/
      grammar.rs        Parse BBNF → AST
      analysis/         FIRST/FOLLOW, SCC, span eligibility
      lower/            AST → GrammarIR (mod, string_interner, fn_table, expression, metadata)
      generate/
        ir_codegen/     IR → Rust TokenStream (mod, alt, seq, repeat, wrap, inline, trace)
        ir_span.rs      IR → SpanParser methods
        ir_pretty/      IR → to_doc() methods (mod, patterns, heuristics, codegen, utils)
        prettify/       Doc generation helpers
        fast_paths.rs   Dispatch + memoization codegen
        regex_emit/     HIR-based inline regex compilation (mod, hir_walk, fallback)
        regex_classify.rs  Structural regex classification (Numeric, HexDigits, Identifier, QuotedString)
      optimize/         Left-recursion elimination (Paull)
      imports.rs        @import resolution
      pipeline.rs       Orchestrate parse → analyze → lower → pass → codegen

  bbnf-ir/              Canonical IR — shared by AOT and VM
    src/
      lib.rs            GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc types
      passes/           16 IR operations, 14 unique passes (used by both AOT and VM)
        canonicalize_aliases, prune_unreachable, inline_acyclic,
        force_inline, fuse.rs (fuse_single_use),
        eliminate_epsilon, merge_literals, merge_regex_alts,
        factor_common_prefixes,
        refine_span_eligibility, compute_follow_sets,
        factor_lookahead.rs (factor_regex_with_lookahead),
        generate_dispatch_tables, project_types
      compiler.rs       IR → BytecodeProgram (VM path only)
      interpreter.rs    BytecodeProgram → ParseResult (VM path only)
      bytecode.rs       Op enum, BytecodeProgram struct (VM path only)

  bbnf-derive/          Proc macro
    src/lib.rs          #[derive(Parser)] entry point

  bbnf-analysis/        Shared analysis for LSP
    src/                DocumentState, 17 LSP providers

  lsp/                  Language server binary (stdio)
    src/main.rs         bbnf-lsp entry point

wasm/                   WASM bindings (wasm-bindgen)
  src/
    lib.rs              init, memory management
    vm.rs               compile_grammar, parse_with_grammar, format_with_grammar
    gorgeous.rs         format_json, format_css, format_bnf, format_ebnf, format_bbnf
    lsp.rs              18 LSP feature exports
    analysis.rs         Grammar analysis exports
```

---

## LSP: Two Transports, One Analysis Engine

```
VS Code Extension                    Playground (browser)
       │                                    │
  bbnf-lsp binary                    WASM LSP exports
  (stdio transport)                  (direct function calls)
       │                                    │
       └────────── bbnf-analysis ───────────┘
                   (shared crate)
                 17 LSP providers
```

The VS Code extension launches `bbnf-lsp` as a subprocess over stdio.
The playground calls WASM-exported functions (`hover_at_offset`, `completions`, etc.).
Both use `bbnf-analysis::DocumentState` for incremental analysis.

**17 LSP Features:**
hover, definition, references, rename, completion, document symbols,
code lens, folding, code actions, formatting, semantic tokens,
inlay hints (FIRST sets + nullable), selection range,
range formatting, on-type formatting (`;`), incremental sync,
enhanced diagnostics (empty rule body)

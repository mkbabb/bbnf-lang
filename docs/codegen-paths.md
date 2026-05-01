---
title: Codegen Paths
order: 50
section: BBNF
---

# BBNF Code Generation Paths

The live Rust path is `cargo xtask regen`: `.bbnf` grammars lower into
`GrammarIR`, pass through shared IR facts and strategy selection, then
emit checked-in per-grammar Rust source under
`crates/core/src/grammar/generated/<ident>.rs`.

The old `#[derive(Parser)]` / `bbnf_derive` proc-macro path is retired
for production codegen. References below to historical VM or TypeScript
surfaces describe secondary/runtime execution paths, not a second
canonical Rust AOT generator.

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
   cargo xtask regen            │        Direct execution
   → checked-in Rust            │        (prettier plugin)
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

## 1. Rust AOT — xtask-generated checked-in source

Build-time code generation. Reads `.bbnf`, emits Rust source files under
`crates/core/src/grammar/generated/<ident>.rs`, and checks them in.

**Entry:** `cargo xtask regen` or `cargo xtask regen --grammar <ident>`.
CI uses `cargo xtask regen --check`.

**Phases (shared with VM through step 6):**
1. Parse grammar → AST (`bbnf::grammar`)
2. Resolve `@import` directives → module graph DFS
3. Extract `@recover`, `@pretty`, `@no_collapse`, `@debug` directives
4. Analysis — SCC detection, FIRST sets, alias detection, span eligibility
5. Lower AST → `GrammarIR` (`bbnf::lower`) — IR types from `bbnf-ir`
6. Apply 17 IR optimization passes (`bbnf-ir::passes`) — **same passes as VM**
7. **(AOT only)** Strategy selection via `EmitStrategy`
8. Rust codegen → `crates/core/src/grammar/generated/<ident>.rs`

**IR Passes** (17 operations, 15 unique passes, in order):
`canonicalize_aliases` → `prune_unreachable` → `inline_acyclic` →
`force_inline` → `prune_unreachable` → `fuse_single_use` → `prune_unreachable` →
`eliminate_epsilon` → `merge_literals` → `merge_regex_alts` →
`factor_common_prefixes` → `refine_span_eligibility` → `compute_follow_sets` →
`factor_regex_with_lookahead` → `generate_dispatch_tables` → `project_types`

Post-B2, `cargo xtask regen` is the single canonical Rust AOT
entrypoint. The pre-B2 bootstrap script, proc-macro expansion, and
Python post-process path retired entirely at B2.W2.

Current strategy status (post AZ-III terminal close, 2026-04-30; AZ-IV planned at master c2a1c39e):

After AZ-IV.W1 closes, parser strategy binding is **manifest-driven**: each grammar contributes a `[package.metadata.bbnf-grammars.<ident>]` row with builder/document paths. The 9-arm `EmitStrategy::for_grammar` allowlist retires; a synthetic grammar registered only via manifest must round-trip codegen without adding a Rust arm (`crates/core/tests/synthetic_grammar_strategy.rs`).

After AZ-IV.W3 closes, every generated parser exposes two parse modes:

- `parse(input) -> Result<Document, ParseErr>` (eager, full-tree materialization).
- `parse_with<P: PathSchema>(input, &path) -> Option<P::Output>` (lazy, path-driven; skips subtrees the path does not visit).

The two modes share the same generated parse functions. The recognizer plan (a per-grammar `<GRAMMAR>_PATH_PLAN: &[(RuleId, SegmentKind, Decision)]`) is emitted at codegen from the IR's `path_check` pass output (`crates/ir/src/passes/path_check.rs`); the plan is grammar-general — no rule-name match arms in the emitter.



- StructDirect: JSON, Google Sheets, CSS L4, BBNF, CSV, Math, BNF,
  CSS Pretty, and EBNF (9/9 production grammars).
- TapeDirect: deleted at AZ-II.cutover.O4; production return-model
  scan is zero for `Parsed<R>` / `TapeDirect` per AZ-III.W4
  structural audits.
- Fallback TapeDirect: deleted at AZ-II.cutover.O4 + AZ-III.W1.
  Unknown grammars fail generation loudly; no silent fallback.
- BBNF self-host: `bootstrap_parser.rs` DELETED 1505 LOC at
  AZ-III.W2.4 (`954d166b`); canonical generated path active with
  95/95 BBNF parity green.

**Generated code per grammar:**
- `parse(...) -> <Grammar>Document<'_>` for every production grammar.
- `parse(...) -> Parsed<'_, Self>` deleted at AZ-II.cutover.O4; no
  production source or generated Rust exposes `Parsed<R>` /
  `TapeDirect` / generated tape views per AZ-III.W4 structural
  audits.
- Per-rule monolithic parse functions selected by shape and
  `EmitStrategy`.
- Grammar-specific runtime builder/document accessors for StructDirect.
- Debug/prettify/projection helpers only when they are consumed through
  the live document API.

**Consumers:** core grammar modules, gorgeous built-in formatters, CLI,
tests, and future WASM AOT wrappers.

## 2. Rust VM — Bytecode Compiler + Interpreter

Runtime grammar compilation. Shares the grammar-to-IR semantics with
AOT, then diverges: compiles the optimized `GrammarIR` to bytecode
instead of writing checked-in Rust source.

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

The VM path does not materialize checked-in parser source. It shares
grammar parsing, import handling, lowering, and IR passes with AOT, then
emits `BytecodeProgram`. The pre-B2 `cargo expand` + Python
post-process bootstrap retired at B2.W2; the only source-materializing
Rust AOT path is `cargo xtask regen`.

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
crates/core/
  src/
    pipeline/           Parse -> lower -> IR passes -> egraph -> emit orchestration
    backend/rust/       Rust emitter, shape emitters, StructDirect bodies
    grammar/generated/  Checked-in per-grammar Rust output from cargo xtask regen
    runtime/            Grammar-specific documents/builders for the StructDirect path
    generate/regex/     HIR-based regex/scanner emission
  tests/                Grammar, parity, projection, reproducibility tests
  benches/              Divan bench suites

crates/ir/
  src/
    passes/             Type projection, materialization, CSP strategy, rewrites, facts
    registry/           Struct and emit strategy registries
    egraph/             IR egraph integration
    types/              TypeDesc and TypeDescInterner substrate

crates/egraph/          General egraph implementation
crates/csp-solver/      General CSP solver implementation
crates/simd-scan/       Scanner primitives and throughput benches
crates/tape/            DELETED at AZ-II.cutover.O5 / AZ-III.W1 (no longer in workspace)
crates/gorgeous/        Formatter surface and built-in grammar formatters
crates/analysis/        Shared analysis for LSP
crates/lsp/             Language server binary and benches
crates/bootstrap/       Bootstrap support crate
crates/ser/             Serialization support
crates/egraph-derive/   Derive helper for the egraph crate, not BBNF parser generation

wasm/
  src/                  WASM exports for VM/gorgeous/LSP/analysis surfaces
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

---
title: Optimization Timeline
order: 49
section: Performance
---

# Optimization Timeline

parse-that started as a TypeScript combinator library in 2020. BBNF followed in 2023 as a grammar compiler targeting parse-that's runtime—the `#[derive(Parser)]` proc-macro compiles `.bbnf` grammars to Rust through a 15-pass IR optimization pipeline. The monorepo consolidation in early 2026 unified bbnf, bbnf-derive, bbnf-ir, the LSP, a Vue 3 playground, and a VS Code extension under one roof.

What follows is the chronological arc of performance work, from the TypeScript prototype through the current Rust codegen. Where something didn't work, that's noted too.

## Cold Benchmarks

Early benchmarks reused a pre-constructed `Parser` across iterations, which measured combinator cache retrieval rather than actual parsing. Cold-parse numbers were 40–60% lower. All benchmarks now construct a fresh `BumpArena` and `Parser` per iteration—no shared state, no warm cache.

## TypeScript Foundations

Three optimization rounds took the TS parser from 746 to 4,779 ops/s (6.4x): mutable `ParserState` eliminated ~4,700 heap objects per parse (3.3x), Tarjan SCC with FIRST-set dispatch tables gave O(1) alternation (1.9x), and V8-specific tuning—`RegExp.test()` replacing `exec()`, megamorphic IC mitigation, `wrap()` inlining—closed the remaining gap. The full chronicle is in the TypeScript optimization document.

## Monolithic Arena Codegen

Combinator chains construct ~30 `Parser` objects with ~60 heap allocations per parse. Monolithic codegen generates direct recursive functions—`fn __rule_arena(state) → Option<ArenaEnum>`—with zero combinator overhead. `BumpArena<T>`, an `UnsafeCell`-based bump allocator with no `RefCell` borrow tracking, replaced `typed_arena`.

Sub-phases: IIFE closure elision, whitespace trim coalescing, single-site cyclic inlining, discarded Span skip, unified `SepByConfig`, type-aware Alt elision, B.1 Span collapse for non-prettify grammars.

CSS bootstrap.css went from ~7 MB/s to 300–760 MB/s.

## Span-Only Codegen

`#[parser(span)]` generates `fn __rule_span(state) → Option<Span>` for every rule. Zero allocations, no enum variants, no Vec. Used when only structural validation or byte-range extraction is needed.

## Delimiter-Driven Flat Scanning

`Wrap(Repeat(Alt))` is the dominant structure in CSS—selector lists, declaration blocks, media query lists. When Alt branches have overlapping FIRST sets, the dispatch table can't route on the first byte alone. Delimiter scanning detects single-byte "pivot" literals that distinguish branches and emits a forward `memchr` scanner loop, replacing recursive descent in the inner loop.

The arena path uses speculative dispatch: the scanner selects the branch, then existing recursive descent constructs the typed value. The span path replaces descent entirely—Span constructed directly from scanner offsets.

CSS bootstrap.css: 21→258 MB/s (12.3x).

## IR Pipeline

Fifteen optimization passes sit between the raw grammar AST and codegen. They're modeled after classical compiler techniques and run in a fixed order that both the AOT proc-macro and the VM bytecode compiler maintain in sync.

**Structural**: alias canonicalization, unreachable pruning, acyclic inlining, single-use fusion, epsilon elimination.

**Algebraic**: literal merging, regex Alt fusion, common prefix factoring (trie-style byte-level splitting).

**Analysis**: span eligibility refinement, FOLLOW set computation, dispatch table generation, memo strategy refinement, type inference.

Measured impact of compiler-analogue techniques:

| Technique | Effect |
|-----------|--------|
| `NumberConvert` (fused regex + f64 scan) | 7.3x on numeric-heavy rules |
| Scanner construction hoisting (LICM) | +54% |
| `hoist_dedup` shared sub-expressions (CSE) | +39% |
| Map fusion (operator fusion) | +15% |

## Grammar Directives

- `@ws /regex/` overrides the default whitespace pattern. CSS grammars use a comment-aware SIMD scanner.
- `@inline ruleName` force-inlines at all call sites. No enum variant, no function call.
- `@token ruleName` uses fusion-style inlining: body inlined, but the enum variant preserved for `@pretty` consumers.
- `@recover rule syncExpr` enables multi-error parsing with any valid BBNF expression as the synchronization point.
- `@{expr}` parses for structural validation but discards the typed result, returning raw Span.

## Regex HIR Compiler

`regex-syntax` exposes a structured HIR for any pattern. Rather than replacing the `regex` crate with another automaton engine, the approach recognizes common patterns in the HIR and emits inline code that LLVM can optimize as ordinary byte operations:

| Pattern | Emitted Code |
|---------|-------------|
| `[a-z]+` | Tight `while` loop with byte-range check |
| `[^"\\]+` | `memchr2(b'"', b'\\')` SIMD scan |
| `\d+`, `\w+`, `\s*` | `is_ascii_digit` / `is_ascii_alphanumeric` / `is_ascii_whitespace` loops |
| `--[\w-]+` | Match literal prefix bytes, then char-class loop |
| `,\|\s+` | Inline byte check + whitespace loop |

Three-tier system: fast-path detection, then HIR inline emission, then compiled DFA for the remainder.

## Ident-Dispatch and Key-Dispatch

CSS property dispatch was a 29-way sequential match. Ident-dispatch parses the identifier once, then byte-compares against a table to route—O(1) instead of O(n). A first-byte trie handles >16 all-literal alternations (named colors, CSS units). Later generalized from CSS-specific ident-dispatch to grammar-agnostic key-dispatch.

## CSS L4 Grammar

Full L4 semantic parsing: property validation, typed selectors (CSS Selectors L4), `@supports`/`@media` (Media Queries L5), specificity, 44 dimension units with `u8` discriminants, per-declaration keyword enums, and fused Eisel-Lemire numeric conversion during the parse itself.

## SIMD Optimizations

1. **memchr for negated character classes.** JSON string bodies match `[^"\\]+`. `memchr2(b'"', b'\\')` scans 16–32 bytes per cycle via SIMD instead of one byte at a time.

2. **Nibble LUT for balanced scanning.** `vpshufb`/`tbl` classifies structural bytes in 16-byte chunks. Two 16-byte lookup tables encode the target byte set.

3. **SIMD whitespace trimming.** `u8x16::simd_eq` against `[' ', '\t', '\n', '\r']` with a fast-path first-byte exit.

4. **Separator pre-counting.** `memchr_iter` counts separator occurrences to the terminator byte, giving exact `Vec::with_capacity` before the parse loop. Generalized to any single-byte separator.

5. **Eisel-Lemire f64.** Fused number scanning and f64 conversion in one pass.

**What didn't work**: per-token SIMD on short identifiers (3–15 bytes) regressed. The amortization is in wide-chunk structural scanning, not per-token classification.

## Bespoke DFA Engine

Six modules, ~2,000 lines in parse-that: Thompson NFA construction from `regex-syntax` HIR, byte equivalence classes (256→5–20 columns), subset construction, Hopcroft minimization, self-loop acceleration via memchr and nibble LUT, and tiered code generation (inline match-chain for ≤8 states, static transition table for 9–64).

The `regex` crate is no longer a production dependency. WASM binary size dropped by 1.3 MB. `regex-syntax` is retained for HIR parsing.

## Secondary Refinements

Inline scanner emitters: byte loops emitted directly in generated code for `scan_ident`, `scan_ws_block_comments`, `scan_string_quoted`. Modest improvement (~9% on CSS L4).

Vec capacity heuristics generalized from comma-only to any single-byte separator. Monolithic Minus/Negate codegen eliminated the last combinator fallbacks.

These are refinements, not breakthroughs—the ceiling was already broken by the regex HIR compiler, delimiter scanning, and dispatch generalization.

## VM Interpreter

`Rc<Value>` elimination from the interpreter stack, pre-compiled regexes with `FxHashMap` memoization, Op dispatch optimization (`Arc`→`u16` index with side table). JSON VM: data 93→128 MB/s (+38%), twitter 80→114 (+43%). CSS VM: normalize 143, bootstrap 75, tailwind 55.

## CSS vs lightningcss

BBNF L4 semantic parsing vs lightningcss on the same files:

| File | BBNF | lightningcss | Ratio |
|------|------|-------------|-------|
| normalize.css (6 KB) | 289 MB/s | 256 MB/s | 1.13x |
| bootstrap.css (281 KB) | 135 MB/s | 114 MB/s | 1.18x |
| tailwind.css (3.6 MB) | 121 MB/s | 88 MB/s | 1.38x |

The work isn't identical. BBNF fuses value conversion during the parse (`→ f64`, `→ u8`); lightningcss does post-parse AST walks, vendor prefix normalization, and CSS Nesting validation. The comparison holds at comparable semantic depth for the parse phase.

The gap widens on tailwind because dispatch tables and inline byte scanners amortize better across ~38K utility rules.

## Current Numbers

All numbers are cold per-parse throughput in MB/s. Fresh `BumpArena` + `Parser` per iteration. `mimalloc` global allocator.

### JSON

| File | Arena | Borrow | Copy |
|------|-------|--------|------|
| data.json (35 KB) | 1,197 | 1,029 | 877 |
| twitter.json (632 KB) | 1,340 | 1,165 | 916 |
| citm_catalog (1.7 MB) | 1,610 | 1,319 | 1,221 |
| canada.json (2.3 MB) | 964 | 731 | 724 |

### CSS

| File | Arena | Span | L4 Semantic |
|------|-------|------|-------------|
| normalize.css (6 KB) | 2,378 | 2,571 | 289 |
| bootstrap.css (281 KB) | 1,421 | 1,639 | 135 |
| tailwind.css (3.6 MB) | 1,370 | 1,425 | 121 |

### VM

| Workload | JSON (MB/s) | CSS (MB/s) |
|----------|-------------|------------|
| Small | 128 | 143 |
| Medium | 114 | 75 |
| Large | 75 | 55 |

The VM runs the same IR through a bytecode interpreter. It's 10–20x slower than AOT but compiles in ~2ms and runs in WASM—the engine behind the playground's live preview.

## What's Next

- Fused parse+format: single-pass Doc construction during parsing, eliminating the AST→to_doc()→render() pipeline
- Shortest-match DFA mode for lazy quantifiers
- `regex.bbnf`: self-hosted regex parser written in BBNF

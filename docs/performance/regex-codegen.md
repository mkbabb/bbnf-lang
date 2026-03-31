---
title: Regex Codegen
order: 46
section: Performance
---

# Regex Codegen

When the bbnf compiler encounters a regex pattern in a grammar (e.g., `/[a-zA-Z_][\w-]*/`), it doesn't emit a call to a regex engine. Instead, it tries three emission strategies in descending order of specialization, with the goal of eliminating all runtime regex machinery from the generated parser.

The result: every regex in the grammar becomes straight-line byte comparisons and loops---code that LLVM can optimize as aggressively as hand-written scanner logic.

## Three-Tier Emission Pipeline

### Tier 1: Fast Paths

`generate/fast_paths/` detects well-known scanner patterns and emits calls to hand-tuned byte scanners in parse-that. Detection uses two approaches:

- **Exact match** (`detect.rs`): canonical pattern arrays for JSON string/number, CSS whitespace+comments, identifiers, and quoted strings. The pattern must match verbatim---no fuzzy matching.
- **Structural classification** (`regex_classify.rs`): component analysis of the regex's structure. `classify_regex()` detects Numeric (sign, fraction, exponent), HexDigits, Identifier, and QuotedString patterns without exact string matching. This drives `FnDescriptor` specialization in the lowering stage and inline scanner selection.

The fast-path scanners themselves are highly specialized:

| Pattern | Scanner | Technique |
|---------|---------|-----------|
| JSON string | `scan_json_string` | `memchr2` for quote/backslash, skip-ahead on escape |
| JSON number | `scan_number_span_json` | Eisel-Lemire fast path for f64 conversion |
| CSS ident | `css_ident_fast` | Inline byte loop for `-?[a-zA-Z_][\w-]*` and `--[\w-]+` |
| CSS ws+comment | `css_ws_comment_fast` | Whitespace loop + `memchr` for `*/` in block comments |
| Quoted string | `css_string_fast` | `memchr2(quote, backslash)` loop with escape skip |
| Comma-or-ws | inline | Single byte check + whitespace loop |
| Negated class `[^XYZ]+` | `memchr1/2/3` | SIMD scan to first exit byte |
| Char ranges, small sets | inline | Direct byte predicates (`is_ascii_digit()`, range checks) |

### Tier 2: HIR Inline

`regex_emit/hir_walk.rs` parses the pattern via `regex-syntax` in byte mode, walks the HIR (High-level Intermediate Representation) tree, and emits direct Rust byte operations. Each HIR node type maps to a code fragment:

- **Literal** — slice comparison (single-byte `==` or multi-byte slice match)
- **Character class** — byte-range predicates with shorthand detection (`is_ascii_digit()`, `is_ascii_alphanumeric() || __b == b'_'`, `is_ascii_hexdigit()`, etc.)
- **Repetition** — tight `while` loops for class-based quantifiers (no per-iteration checkpoint overhead), checkpoint/restore for general sub-expressions
- **Alternation** — cascading `if/else` with first-byte dispatch for disjoint leading bytes
- **Concat** — sequential composition with `?` propagation via an IIFE closure returning `Option<()>`
- **Optional** — save/restore wrapper around the sub-expression
- **Anchors** — `state.offset` boundary checks

Lazy quantifiers (`*?`, `+?`) bail from this tier---greedy loops would over-consume. The exception is `.*?literal` sequences, where the concat handler can fuse the lazy scan with the following literal via `memmem`.

The HIR walker produces code that LLVM sees as plain byte operations. No function pointers, no automaton dispatch, no vtable indirection.

### Tier 3: DFA Compiled

`regex_emit/dfa_emit.rs` compiles the pattern to a minimized DFA via parse-that's [bespoke regex engine](../parse-that/regex-engine), then emits inline Rust code. Patterns with lazy quantifiers are rejected (DFAs inherently produce longest-match semantics), falling through to Tier 4.

Two sub-tiers based on DFA size:

**Tier A** (8 or fewer states): an inline match-chain state machine. Each state becomes a match arm with direct byte checks and jumps. Self-looping accepting states get tight `while` loops with optional memchr acceleration. LLVM sees every transition and can optimize aggressively---constant-fold dead arms, hoist invariants, vectorize the loop body.

For the common two-state pattern (e.g., `[a-z]+`, `\d+`, `[^"]+`)---a start state that enters and a self-looping accept state---the emitter detects this structure and generates a minimal entry check + tight scan loop, optionally accelerated by `memchr` when the DFA's self-loop state qualifies.

**Tier B** (9--64 states): a static transition table + driver loop. The transition table is flattened to a `static` byte array indexed by `state * num_classes + class`. Equivalence class mapping compresses the table. The driver loop reads one byte per iteration, looks up the class, indexes the table, and checks the accept bitmask.

```rust
// Emitted code structure (simplified):
static __CLASSES: [u8; 256] = [/* byte → equiv class */];
static __TRANS: [u8; N] = [/* state × class → next state */];
const __ACCEPT: u64 = /* bitmask */;

while __pos < __end {
    let __c = __CLASSES[src[__pos]];
    let __next = __TRANS[__s * num_classes + __c];
    if __next == 0xFF { break; }
    __s = __next;
    __pos += 1;
    if __ACCEPT & (1u64 << __s) != 0 { __last_accept = Some(__pos); }
}
```

The static arrays live in the binary's `.rodata` section. At runtime, there's no allocation, no regex compilation, no function pointer dispatch---just array indexing in a loop.

### Tier 4: LazyLock Fallback

`regex_emit/fallback.rs` emits a `LazyLock`-cached DFA compiled at runtime via `cached_dfa()`. The DFA is built once per pattern on first use (using parse-that's own engine, not the `regex` crate) and reused across calls.

This tier should be unreachable for well-formed grammars. It exists as a safety net for patterns that use features neither the HIR walker nor the DFA compiler can handle (e.g., certain word boundary assertions). The audit system validates that it's never reached in practice.

## Audit System

`regex_emit/audit.rs` provides `audit_regex_pattern(pattern)`, which classifies each grammar regex into its emission tier without emitting code:

```rust
pub enum RegexTier {
    FastPath(&'static str),
    FastPathFused(&'static str),
    HirInline,
    DfaCompiled { states: usize, classes: usize },
    LazyLockFallback,
}
```

The test harness in `tests/regex_audit.rs` scans every `.bbnf` file under `grammar/`, extracts all regex literals, and audits each one. The test reports a per-tier breakdown and flags any patterns that fall to Tier 4.

The audit runs as part of `cargo test` and serves as a regression gate: if a grammar change introduces a regex pattern that can't be compiled to inline code, the test catches it before the pattern silently degrades to runtime compilation.

## Coverage

All regex patterns across the JSON, CSS, EBNF, BNF, BBNF, and Google Sheets grammars resolve to Tiers 1--3. Zero LazyLock fallback. The typical distribution is heavily weighted toward Tier 1 (fast paths cover the high-frequency patterns) with Tier 2 handling the long tail of character class loops and simple alternations. Tier 3 picks up the occasional complex alternation or bounded repetition that the HIR walker can't inline.

## Integration with Monolithic Codegen

In the monolithic arena and span codegen paths (`ir_codegen/monolithic/`), regex emission is called directly from expression codegen. When the compiler encounters an `IrNode::Regex(pattern)`, it tries each tier in order:

1. `fast_paths::emit_regex_direct_call` — returns inline `Option<Span>` code if a fast path matches
2. `regex_emit::try_emit_regex_inline` — HIR walker
3. `regex_emit::try_emit_dfa_inline` — DFA compiler
4. `regex_emit::emit_regex_lazy_static` — fallback

The combinator codegen path (`ir_codegen/` default mode) uses `fast_paths::emit_regex_parser` and `fast_paths::emit_regex_span` instead, which wrap the same detection logic but return `Parser<Span>` or `SpanParser` values for composition with other combinators.

# Arena Allocation & Monolithic Codegen

## BumpArena

`BumpArena<T>` is an `UnsafeCell`-based bump allocator in `parse_that` that replaces `typed_arena::Arena`. The hot path—capacity check, push, return `&T`—has zero borrow tracking overhead. `typed_arena` wraps its internal `Vec` in `RefCell`, incurring 2 reads + 1 write + 1 branch per `alloc()` call. For a grammar like JSON with ~13K pair allocations per twitter.json parse, those borrow checks accumulate.

```rust
pub struct BumpArena<T> {
    current: UnsafeCell<Vec<T>>,
    rest: UnsafeCell<Vec<Vec<T>>>,
}
```

Growth is cold-path only: when the current chunk fills, it's moved to `rest` and a new chunk at 2x capacity is allocated. The returned `&T` references are valid for the arena's lifetime. Safe under the parsing contract: single-threaded, non-reentrant, no aliasing of chunk internals.

## Monolithic Codegen

The `#[parser(arena)]` attribute triggers monolithic code generation alongside the standard combinator-based path. Instead of constructing a chain of `Parser` objects (lazy closures, dispatch tables, sep_by combinators—~30 objects with ~60 heap allocations per parse), the monolithic emitter generates direct recursive functions:

```rust
fn __value_arena<'a>(state: &mut ParserState<'a>) -> Option<ArenaEnum<'a>> {
    let __byte = *state.src_bytes.get(state.offset)?;
    match __byte {
        b'{' => { /* inline object body */ }
        b'[' => { /* inline array body */ }
        b'"' => { /* SIMD string scanner */ }
        // ...
    }
}
```

Each public `rule_arena()` method wraps a single function pointer in `Parser::new()`—one SmallBox, zero vtable dispatches, O(1) construction.

### Optimization phases

| Phase | Technique |
|-------|-----------|
| 6 | IIFE closure elision—skip `(|| expr)()` when element has no `?` operator |
| 7 | Whitespace trim coalescing—2 trims per sep_by_ws_until iteration instead of 3 |
| 8 | BumpArena integration—zero RefCell overhead per alloc |
| 9 | Single-site cyclic inlining—pair body inlined into object's sep_by loop |
| 10 | Discarded OW skips Span construction; single-byte terminator uses `==` |
| 11 | Unchecked `get_unchecked` separator in delimited loops |
| — | Dispatch-guaranteed-byte elimination—skip redundant open-delimiter check after dispatch match |
| — | Unified `SepByConfig` + `emit_mono_sep_by_core`—three sep_by emitters → one |
| — | Type-aware Alt elision—heterogeneous Alts in Vec context coerce by value, not arena ref |
| — | B.1 Span collapse—Seqs of simple Span children + has_sp_method Refs collapse to single Span |
| — | `Vec::new()` default—no pre-allocation heuristics; Rust's growth handles nested and flat containers |

### Grammar directives

`@ws /regex/ ;` overrides what `?w` compiles to. CSS grammars use `@ws /(?s)(?:\s|\/\*.*?\*\/)*/ ;` to get SIMD `css_ws_comment_fast` instead of allocating a `ws` enum variant per call.

`@inline ruleName ;` force-inlines a rule at all call sites via a dedicated IR pass. The rule body is substituted at every `Ref`—no enum variant, no function. Guarded against direct self-recursion.

## JSON Results (cold per-parse, mimalloc)

All numbers are cold: fresh `BumpArena` + `Parser` constructed per iteration.

| Dataset | Size | Arena MB/s |
|---------|------|-----------|
| data.json | 35 KB | 1,261 |
| twitter.json | 632 KB | 1,347 |
| citm_catalog.json | 1.7 MB | 1,597 |
| canada.json | 2.3 MB | 1,115 |
| data_xl.json | 21 MB | 815 |

## CSS Results (cold per-parse, mimalloc)

| Dataset | Size | Pretty Arena | Parse-only | cssparser |
|---------|------|-------------|-----------|-----------|
| normalize.css | 6 KB | 659 | 639 | 323 |
| bootstrap.css | 281 KB | 276 | 939 | 437 |
| tailwind.css | 3.8 MB | 284 | 469 | 360 |

The pretty arena tier produces the full typed AST that gorgeous uses for formatting. Parse-only is the isolated parse phase (no `to_doc` or render). Prior to the delimiter-scan optimization, tailwind parsed at 28 MB/s—the structural overhead of recursive descent on ~38K tiny utility rules was the bottleneck. With delimiter scanning and span-only codegen, tailwind went from 28 MB/s to 2196 MB/s (arena) and 3585 MB/s (span-only).

### Delimiter-driven flat scanning

For `Wrap(Repeat(Alt))` patterns where the Alt's FIRST sets overlap (no dispatch table), the monolithic emitter detects distinguishing "pivot" bytes inside each branch and compiles a forward `memchr` scanner that selects the branch before entering recursive descent. All bytes are extracted from the grammar's own Literal nodes—the optimization is grammar-agnostic.

Detection criteria:
- Open and close are single-byte Literals
- The Repeat inner is an Alt without a dispatch table
- At least one branch contains a single-byte Literal at position > 0 (the pivot)
- Pivot, open, close bytes are pairwise distinct

In the span path, the scanner replaces the descent entirely (all Span output). In the arena path, the scanner determines which branch to call, then invokes that branch's existing recursive descent function—preserving full typed output for formatting. A pseudo-class guard handles ambiguous pivots: if the scanned value terminates at the open byte, the pivot was part of a compound token (e.g., `selector:pseudo{...}`), and the scanner falls back to the block branch.

## Span-Only Parsing

`#[parser(span)]` generates a third codegen tier—`fn __rule_span(state) -> Option<Span<'a>>` functions that return byte-range spans with zero allocation. No enum variants, no arena, no Vec. Every rule collapses to a start/end offset pair.

This tier is useful for validation-only parsing (syntax checking, linting) and for inner loops where the typed AST isn't needed. Direct `memchr` emission for negated character classes (`[^XYZ]+`, `[^XYZ]*`) bypasses the SpanParser enum dispatch entirely, calling `memchr::memchr1/2/3` inline.

The span path also serves as the fast lane for delimiter-driven flat scanning—the `memchr` scanner replaces recursive descent wholesale when the grammar matches the `Wrap(Repeat(Alt))` pattern.

## Vec Capacity

All monolithic Vecs use `Vec::new()`. Prior heuristics (`Vec::with_capacity(remaining/64)`) caused pathological over-allocation for deeply nested containers (canada.json: 2-element `[lon, lat]` arrays allocated 16K capacity each). Rust's default growth strategy (0→4→8→16→...) handles both nested containers (1-2 elements, 1 allocation) and flat lists (amortized O(1) growth) without special-casing.

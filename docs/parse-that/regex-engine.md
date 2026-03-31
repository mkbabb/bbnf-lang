---
title: Regex Engine
order: 15
section: parse-that
---

# Regex Engine

parse-that ships a bespoke NFA-to-DFA regex engine. It's designed as both a compile-time regex compiler for bbnf's codegen and a runtime matching engine for `SpanParser`. There's no dependency on the `regex` crate at runtime—only `regex-syntax` for HIR parsing.

All operations work on bytes, not codepoints. Unicode codepoint ranges are expanded to UTF-8 byte sequences via `regex_syntax::utf8::Utf8Sequences`, so the automaton never reasons about characters wider than a single byte.

## Architecture

The compilation pipeline is straightforward:

```
pattern string
  → regex-syntax HIR
  → Thompson NFA (ε-transitions, priority edges)
  → byte equivalence classes
  → subset construction → raw DFA
  → Hopcroft minimization → minimal DFA
```

The resulting `Dfa` struct drives both compile-time codegen (bbnf emits inline state machines from it) and runtime matching (`find_at` for `SpanParser::Regex`).

## NFA Construction

`nfa.rs` implements the Thompson construction algorithm. Each regex sub-expression becomes a fragment—a pair of start and accept states—composed via ε-transitions. The NFA operates entirely on `ByteSet` transitions: a transition fires when the input byte is a member of the set.

ε-transitions carry a `priority` field (lower values preferred) that encodes greedy/lazy semantics. For greedy quantifiers, the "match more" edge gets priority 0 and the "exit" edge gets priority 1; lazy quantifiers reverse this. During subset construction, priority ordering determines which NFA paths the DFA prefers when multiple ε-closures compete.

Supported constructs:
- Literals, character classes (byte and Unicode), alternation, concatenation
- Repetitions: `*`, `+`, `?`, `{n}`, `{n,}`, `{n,m}`—greedy and lazy variants
- Captures (treated as transparent—only the span matters)
- Start/end anchors

Not supported: backreferences, word boundaries. Patterns using these features return `None` from the NFA builder, triggering fallback in the caller.

### Unicode Handling

Unicode character classes deserve a note. A class like `\p{L}` (all Unicode letters) expands into hundreds of codepoint ranges. For each range, `Utf8Sequences` decomposes it into byte-level NFA paths. An ASCII-only fast path (`hi <= '\x7F'`) avoids the decomposition for ranges that fit in a single byte. The result is an NFA that correctly matches multi-byte UTF-8 sequences through ordinary byte transitions.

## Byte Equivalence Classes

`equiv.rs` compresses the 256-byte alphabet into equivalence classes. Two bytes are equivalent if and only if they have identical membership across every `ByteSet` in the NFA's transitions—they behave identically in every state, so the DFA can treat them as interchangeable.

The algorithm computes a signature for each byte: a bitvec recording which NFA transitions contain it. Bytes with identical signatures land in the same class. Typical patterns produce 5--20 classes, reducing the DFA transition table from 256 columns to a fraction of that.

Two implementations handle scale:
- **Fast path** (64 or fewer ByteSets): signature fits in a `u64` bitmask
- **Large path** (more than 64): signature stored as `Vec<u64>`

The output is a `[u8; 256]` lookup table mapping each byte to its class ID, plus the total class count.

## Subset Construction

`dfa.rs` builds the DFA via the standard worklist-driven subset construction. Each DFA state represents a set of NFA states (the ε-closure of some configuration). For each equivalence class, the algorithm computes `move(state_set, representative_byte)`, then takes the ε-closure of the result.

A state limit guard (default 512) prevents exponential blowup on adversarial patterns. `Dfa::compile()` returns `None` if the limit is exceeded—the caller falls back to a simpler emission strategy or runtime compilation.

## Hopcroft Minimization

After subset construction, Hopcroft's algorithm minimizes the DFA:

1. **Initial partition**: accepting states vs. non-accepting states.
2. **Iterative refinement**: for each block, check whether all members agree on target block via every equivalence class. If they disagree, split the block.
3. **Fixed point**: when no block can be split, the partition is stable.

State 0 in the input DFA is always mapped to state 0 in the output, preserving the start state identity. The minimized DFA is typically 30--60% smaller than the raw subset construction output.

## The `Dfa` Struct

```rust
pub struct Dfa {
    pub states: Vec<DfaState>,
    pub byte_classes: [u8; 256],
    pub num_classes: u16,
    pub accept_mask: u64,
}
```

`accept_mask` is a bitmask encoding which states are accepting—one bit per state, supporting up to 64 states for O(1) accept checks. States beyond 64 fall back to the `DfaState::is_accept` field.

Each `DfaState` holds a `transitions: Vec<StateId>` indexed by equivalence class. Dead transitions use the sentinel value `DEAD` (`u32::MAX`).

## Runtime Matching

`find_at(bytes, offset)` performs an anchored greedy match. It's a single-pass O(n) scan through the input:

```
byte → class lookup via byte_classes[256]
     → transition table[state * num_classes + class]
     → new state (or DEAD → stop)
     → accept check via u64 bitmask
```

The function tracks the position of the last accepting state and returns it as the match end. This gives longest-match semantics inherently—exactly what grammar lexing needs.

## Self-Loop Acceleration

`accel.rs` identifies DFA states where the vast majority of equivalence classes loop back to the same state. The canonical example is `[^"\\]+` in a JSON string scanner: the state self-loops on every byte except `"` and `\`. Scanning one byte at a time through megabytes of string content wastes the memory bus.

The `detect_accel` function examines each state and selects a strategy based on how many "exit bytes" break the self-loop:

| Exit Bytes | Strategy | Implementation |
|————|———-|—————-|
| 1 | `memchr::memchr` | SIMD-accelerated single-byte search |
| 2 | `memchr::memchr2` | SIMD two-byte search |
| 3 | `memchr::memchr3` | SIMD three-byte search |
| 4--8 | Nibble LUT | `vpshufb`/`tbl` SIMD instruction |
| 9--64 | Scalar LUT | 256-byte boolean lookup table |
| 65+ | None | No acceleration (byte-at-a-time) |

The nibble LUT strategy builds two 16-byte tables indexed by the low and high nibbles of each byte. A byte is an exit byte if `lo_lut[b & 0xF] & hi_lut[b >> 4] != 0`. On x86-64, this maps directly to a `vpshufb` instruction; on AArch64, to `tbl`.

Acceleration data is computed once after DFA construction and threaded into the codegen layer, where bbnf emits the appropriate SIMD calls inline.

## ByteSet

`byteset.rs` provides a 256-bit bitset—one bit per possible byte value. Stored as `[u64; 4]`, it supports O(1) membership tests, insert, and remove, plus bulk operations (union, intersection, complement, difference) that compile to a handful of bitwise instructions.

The iterator uses `trailing_zeros()` to yield set members in ascending order without scanning empty regions. `ExactSizeIterator` is implemented via `count_ones()` across all four words.

`ByteSet` is the foundation type for NFA transitions, equivalence class computation, and exit-byte detection in the acceleration layer.

## Roadmap

- **Shortest-match mode** for lazy quantifiers (currently patterns with `*?` or `+?` bail to the LazyLock fallback)
- **Packed DFA** for automata exceeding 64 states (Tier C in bbnf's codegen pipeline)
- **Multi-pattern DFA** with shared transition tables for parallel pattern matching
- **`regex.bbnf`** — a self-hosted regex parser written in BBNF, closing the bootstrapping loop

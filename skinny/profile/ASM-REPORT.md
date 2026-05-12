# ASM attribution — lazy-tape skinny, outcome G

Companion to the samply cycle profile. All assembly was produced with
`cargo asm -p runtime --lib <sym>` (and `-p simd-scan --lib`) against
`profile.release` (`opt-level=3, lto=thin, codegen-units=1, debug=true`).
ARM64 (Apple darwin).

cargo-asm `[N]` counts (its own instruction tally, includes prologue/epilogue
and jump-table macro instructions) appear in parentheses below; the table's
`insns` column re-counts mnemonic lines after slicing each dump to its own
symbol (cargo-asm dumps the requested symbol plus subsequent symbols in
source order, which had to be sliced apart).

## (a) Per-function metrics

| Function | lines | insns | branches | calls | indirect | panic sites |
|---|---:|---:|---:|---:|---:|---:|
| `parse_value` (generated.rs) | 888 | 689 | 155 | 19 | 0 | 7 |
| `simd_scan::scan_json_parse_index` | 521 | 430 | 68 | 18 | 0 | 1 |
| `simd_scan::scan_json_structurals` | 370 | 308 | 16 | 8 | 0 | 1 |
| `TapeAssembler::finish` | 249 | 189 | 25 | 19 | 0 | 0 |
| `view::span_for_value` | 237 | 146 | 27 | 9 | 0 | 2 |
| `parse_string` (generated.rs) | 230 | 161 | 29 | 6 | 0 | 3 |
| `JsonArrayValues::next` | 213 | 142 | 26 | 8 | 0 | 3 |
| `JsonObjectPairs::next` | 174 | 112 | 23 | 6 | 0 | 2 |
| `view::token_from_cursor` | 166 | 111 | 23 | 6 | 0 | 0 |
| `simd_scan::scan_json_tail` | 155 | 99 | 23 | 3 | 0 | 1 |
| `parse_literal` (generated.rs) | 131 | 82 | 8 | 5 | 0 | 2 |
| `view::scalar_span` | 106 | 65 | 13 | 3 | 0 | 1 |
| `JsonNodeKind::at_cursor` | 101 | 62 | 13 | 3 | **1** | 2 |
| `TapeBuilder::emit` | 100 | 57 | 5 | 2 | 0 | 1 |
| `TapeAssembler::new` | 73 | 44 | 5 | 3 | 0 | 0 |
| `TapeBuilder::patch_skip`* | 57 | 28 | 2 | 2 | 0 | 2 |
| `TapeBuilder::patch_skip_to_current_len` | 58 | 29 | 2 | 2 | 0 | 2 |
| `TapeBuilder::patch_end` | 57 | 28 | 2 | 2 | 0 | 2 |
| `simd_scan::scan_dispatch` | 54 | 30 | 5 | 1 | 0 | 0 |
| `RawVec<TapeToken>::grow_one` | 46 | 26 | 1 | 2 | 0 | 0 |
| `tape::checked_u32` | 34 | 15 | 1 | 1 | 0 | 1 |
| `tape::scan_structurals` (wrapper) | 6 | 1 | 1 | 0 | 0 | 0 |
| `tape::scan_parse_index` (wrapper) | 6 | 1 | 1 | 0 | 0 | 0 |

*Resolved via numeric selector after cargo-asm flagged the bare name as
ambiguous with `patch_skip_to_current_len`.*

The two `tape::scan_*` wrappers are one-instruction `b` (tail-call) shims
into `simd_scan::scan_json_*` — confirming the scan layer has no
intermediate logic.

## (b) Inlined functions (named in source but absent from the symbol table)

These were grepped in source as targets but produced no top-level symbol
under `cargo asm --list` — LLVM folded them into their callers:

- `generated.rs::parse_object`
- `generated.rs::parse_array`
- `generated.rs::parse_pair`
- `generated.rs::parse_number`
- `generated.rs::parse_json` (outer entry)
- `generated.rs::skip_ws`
- `generated.rs::peek`, `consume`, `sync_structural`, `error`
- `view.rs::next_sibling_cursor`
- `view.rs::string_body_range`
- `view.rs::string_end_cursor`
- `tape::TapeBuilder::token` / `token_count` / accessors (none appear)
- `tape::TapeToken::new` *did* survive as a 65-instruction symbol — token
  construction was not folded everywhere
- `tape::TapeBuilder::with_capacity` survived (104 instructions) —
  inflated by Vec allocation paths

Surviving symbols are exactly the recursive bodies and the few helpers
the inliner refused to swallow (`at_cursor` because of its jump table,
`token_from_cursor` because of `match` arms calling out, the iterator
`next`s because of the trait dispatch).

## (c) Bounds-check / panic-call density

Every count is the number of `bl core::panicking::*` /
`bl core::result::unwrap_failed` / `bl core::option::expect_failed` /
`bl alloc::raw_vec::handle_error` call sites within the function.

Highlights:

- **`parse_value`: 7 panic sites, 19 calls, 155 branches** — all
  `unwrap_failed` for `u32::try_from(usize)` on offsets, plus one
  `panic_bounds_check`. These guard the offset narrowing that the typed
  parser performs on every value-start. They are statically unreachable
  for inputs ≤4 GiB but LLVM cannot prove it.
- **`parse_string`: 3 panic sites** — one `panic_bounds_check` (state
  lookup against `structural_offsets`), two `unwrap_failed`.
- **`parse_literal`: 2 panic sites** — token-length narrowing.
- **`JsonNodeKind::at_cursor`: 2 distinct panic sites** —
  `panic_bounds_check` on `state.offsets[cursor]` and
  `expect_failed` on the byte → `JsonNodeKind` table miss. Plus a third
  `panic_fmt` for the `Option<u8>::unwrap` after byte fetch.
- **`JsonArrayValues::next` / `JsonObjectPairs::next`: 2–3 panic
  sites each**, all `at_cursor`-derived.
- **`span_for_value`, `token_from_cursor`, `scalar_span`: 1–2 each.**
- **`TapeBuilder::patch_*` and `TapeBuilder::emit` carry 1–2 panic
  sites each**, even though they are short — `u32::try_from(usize)` on
  end-offset patches and `Vec` index panics.

The `parse_value` recursive monolith alone has 7 panic edges and 155
branch instructions in 689 mnemonics — roughly one branch every 4–5
instructions. None can ever fire on the well-formed inputs the bench
uses, but each consumes an icache slot and a branch predictor entry.

## (d) Branch density (j*/b*/cb*/tb* mnemonics)

The `branches` column above is the raw count. As a ratio of branches to
total instructions:

| function | branches / insns |
|---|---:|
| `parse_value` | 155 / 689 = **22.5%** |
| `parse_literal` | 8 / 82 = 9.8% |
| `parse_string` | 29 / 161 = 18.0% |
| `at_cursor` | 13 / 62 = **21.0%** |
| `span_for_value` | 27 / 146 = 18.5% |
| `JsonArrayValues::next` | 26 / 142 = 18.3% |
| `JsonObjectPairs::next` | 23 / 112 = 20.5% |
| `token_from_cursor` | 23 / 111 = 20.7% |
| `simd_scan_json_parse_index` | 68 / 430 = 15.8% |
| `simd_scan_json_structurals` | 16 / 308 = **5.2%** |

`simd_scan_json_structurals` is the only function that looks SOTA-shaped
— one branch every 19 instructions, dense NEON, tight loop. Every other
hot function is hovering around 20% branch density, which is
small-state-machine / dispatch-table territory, not stream parsing.

## (e) Indirect calls / dispatch tables

| function | indirect | mechanism |
|---|---:|---|
| `JsonNodeKind::at_cursor` | 1 | `br x11` via `LJTI16_0` — jump table from `(byte - 34)` to one of 9 `mov w0, #N` arms |

That is the only indirect branch in any dumped function. There are no
vtable calls and no `call *%`/`jmp *%` patterns elsewhere — every other
indirection is a tail-call `b <symbol>` to a known callee.

The `at_cursor` jump table is fine in isolation, but it sits **inside
every call** from the iterators and span helpers (see below); the cost
is the *call* surrounding it, not the branch itself.

## (f) Top 3 suspicious functions

### 1. `parse_value` — 888 raw lines / 689 instructions / 155 branches / 7 panic sites / 22.5% branch density

This is the parse hot path and it shows three pathologies at once:

- **Whitespace classifier embedded in two places** (`LBB51_2` and
  `LBB51_20`): scalar `ldrb` → `cmp #32` → `lsl` mask byte-by-byte loop
  over `state.bytes`. The structural-index from simd-scan is *not* used
  to skip whitespace; the typed parser walks raw bytes again.
- **Calls `RawVec<u32>::grow_one`** — the offset Vec grows
  element-by-element through the typed parse, even though simd-scan
  already counted the structural offsets and could size it once.
- **Self-recursion plus calls into `consume_structural`, `parse_string`,
  `parse_literal`** — but `parse_object`, `parse_array`, `parse_pair`,
  `parse_number` are inlined. Result: one giant register-spilling body
  (176-byte stack frame, x19–x26 callee-saves) that dispatches on byte
  value with `cmp w8, #34 / #91 / #102 / #110 / #115 / #34 / #45` etc.

This is a **byte-driven recursive-descent parser running concurrently
with** a SIMD structural scan, not a SIMD-fed dispatcher.

### 2. `simd_scan::scan_json_parse_index` — 521 lines / 430 instructions / 18 calls

The "parse-index" variant (which is what the lazy-tape path uses;
emits structural offsets plus escape/control side-streams) is **40 %
larger and ~4x more branch-dense** than the bare
`scan_json_structurals`. It calls:

- `RawVec<u32>::grow_one` (offset Vec grows during the scan)
- `alloc::raw_vec::handle_error` (allocation failure path)
- `__rust_alloc` / `__rust_dealloc` / `__rust_no_alloc_shim_is_unstable_v2`
- `__Unwind_Resume` (the scan can unwind)

NEON op density: 39 SIMD instructions / 430 total = **9 %**.
Versus `scan_json_structurals`: 98 / 308 = **32 %**.

The "useful" SIMD ratio fell off a cliff between the two variants
because of the bookkeeping that the parse-index variant carries. This
is the function the parser actually fronts with — every byte goes
through here first.

### 3. `JsonNodeKind::at_cursor` — 101 lines / 62 instructions / 2 bounds checks / 1 jump table — but **not inlined**

`at_cursor` itself is small and well-shaped (one jump table, two range
checks, no allocation), but cargo-asm shows it as a standalone symbol
with `bl <...>::at_cursor` callers in every consumer:

- `JsonArrayValues::next`
- `JsonObjectPairs::next`
- `span_for_value`
- `token_from_cursor`

Per call to `at_cursor`: 1 function-call overhead + 2 bounds checks
(`offsets.len()` and `source.len()`) + 1 jump table + register
restoration. None of the bounds checks can fire on a tape produced by
the same parser, but LLVM cannot prove the invariant across the call
boundary, so the call refuses to inline. (Likely cause: the jump-table
form trips the inliner's size heuristic when called from a function
that itself calls `at_cursor` more than once.)

Every cursor walk pays this tax. The iterator `next`s are 142 / 112
instructions of mostly call-and-bounds-check before any useful work
happens.

## (g) Honorable mention — `TapeAssembler::finish`

249 lines, 189 instructions, **two `__rust_realloc` calls** + a
`__rust_dealloc` + `handle_error`. The "finish" stage is shrinking the
offset Vec and the escape Vec to their actual sizes after parsing — a
copy-and-realloc per parse, on the hot path. Not a hot-loop function,
but visible in single-parse latency for small payloads.

---

## Conclusion — predicted cycle attribution and SOTA shape

Three structural shapes dominate the assembly. Predicting where cycles
go (the samply profile will adjudicate):

1. **`parse_value` and its byte-driven dispatch — largest share.** A
   custom whitespace classifier runs twice per value boundary; the
   simd-scan structural stream is consulted only inside `parse_string`
   for the closing quote. Combined with 22 % branch density and an
   `unwrap_failed`/`u32::try_from` panic chain at every offset, this
   function looks like the cycle sink.

2. **`simd_scan::scan_json_parse_index` — second-largest share.** The
   side-stream-emitting variant carries allocator and grow-by-one
   overhead that the pure-structurals variant doesn't. NEON op
   density drops from 32 % to 9 %. For dense inputs (twitter), the
   parse-index variant is doing real SIMD work but at lower throughput
   than a tight `simd_scan_json_structurals` would.

3. **View-side `at_cursor` + iterator `next` — meaningful tail.** Each
   cursor walk pays a non-inlined call with two redundant bounds
   checks. If the bench measures parse-only this is small; if it
   measures parse+walk (which is what produces the throughput number)
   it adds up across object/array iteration.

**Does this match a SOTA-class JSON parser shape?** Partially.

The `simd_scan_json_structurals` kernel alone has the right shape: 5 %
branch density, 32 % NEON, no allocator calls in the loop body, no
panic edges in the main path. That matches simdjson / yyjson / sonic-c++
inner loops. The parse-index variant is half a step off — the
side-stream emission needs the same delta-write pattern simdjson uses
(reserve once, write unconditionally, never grow_one).

The typed-parse layer (`parse_value` + the iterator walks) does **not**
match SOTA shape:

- A hand-tuned parser fed by a SIMD structural index does **dispatch
  on the next structural index entry**, not on the next source byte.
  Whitespace is implicitly skipped because the structural index only
  records `{}[],:"`. This parser whitespace-skips by walking bytes,
  ignoring the index entirely except for closing quotes.
- A hand-tuned parser **pre-sizes the offset stream** from the
  structural count and writes unconditionally. This parser
  `RawVec::grow_one`s.
- A hand-tuned parser keeps cursor → kind classification **inlined or
  table-lookup, no call**. This parser pays a call + double bounds
  check per `at_cursor`.
- `TapeAssembler::finish` shouldn't realloc.

The asm shape is consistent with **a hand-written recursive-descent
parser bolted on top of a SIMD prescan, with the prescan output mostly
discarded and the post-parse Vec shrunk twice**. Outcome G's 11780 Mbps
on twitter is impressive given that, and explains why outcome G is not
yet at the SOTA ceiling — the simd-scan ceiling is being capped by the
scalar consumer.

Predicted samply ordering (high to low):

1. `parse_value` + inlined parse_object/array/pair/number bodies
2. `simd_scan::scan_json_parse_index` inner loop
3. `parse_string` (regex `match_json_string` call + structural-cursor
   sync)
4. `at_cursor` + iterator `next` (if walk is in-scope for the bench)
5. `TapeAssembler::finish` realloc tail
6. Allocator (`__rust_alloc` / `grow_one`) ambient

Items to verify against samply: whether `match_json_string` (parse_string's
regex helper, from parse_that_regex) is its own hot spot, and whether
the whitespace classifier shows up as its own bucket inside parse_value
or fuses into the parse_value sample.

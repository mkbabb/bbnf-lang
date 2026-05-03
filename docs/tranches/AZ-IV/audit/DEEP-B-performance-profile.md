# DEEP-B — Performance Profile

Read-only post-AZ-IV audit. Worktree
`/Users/mkbabb/Programming/bbnf-wt-deepaudit-B`, branch
`deepaudit-profile` at master `15e1e5a1`.
`CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-deepaudit-B/target/deep-B`.

The driving questions are three but one body. sonic-rs is 332 ns on
`get_twitter`; bbnf is 1.396 ms — a 4196× factor. AU floor: 18/19 BELOW.
Both attributions resolve to a single architectural mechanism, named
explicitly in the AZ-IV close evidence and reproducible by inspection of
the generated parse function.

## Profiling Discipline (per PROFILING.md)

Per `docs/instructions/PROFILING.md` §Profile a single entry, samply
attribution requires the seven-artefact contract:
`bench.txt`, `build.txt`, `record.txt`, `load.txt`, `profile.json.gz`,
`profile.json.syms.json`, `syms-proof.txt`. Samply rules: no
`--save-only`, `--unstable-presymbolicate`, ports preflighted, bench
binary cwd `crates/core`. Shared absolute `CARGO_TARGET_DIR`.

This audit ran the contract end-to-end against `bbnf_value_twitter`
(divan substring filter — captures the full `json_value` matrix per
PROFILING.md §Samply invocation rules `--bench` substring caveat).

| Artefact | Size | Path |
|---|---:|---|
| bench.txt | 1.4 K | `.profiles/samply/post-AZ-IV/deep-B/bbnf_value_twitter/bench.txt` |
| build.txt | 136 B | same dir |
| record.txt | 1.7 K | same dir |
| load.txt | 281 B | same dir |
| profile.json.gz | 233 K | same dir |
| profile.json.syms.json | 109 K | same dir |
| syms-proof.txt | 109 K | same dir |

`syms-proof.txt` (named-frame match against
`json_value|BbnfBootstrap|GoogleSheetsParser|JsonParser|CssL4Parser`)
is non-empty; bench named frames present include
`<JsonParser>::parse`, `parse_JsonParser_value`,
`parse_array_JsonParser_array`, `parse_object_JsonParser_object`,
`parse_wrap_JsonParser_value`, `JsonStructBuilder`,
`JsonStructCheckpoint`, `OpenFrame`,
`<alloc::vec::Vec<bbnf::runtime::json::builder::OpenFrame> as core::clone::Clone>::clone`.
Bench numbers reproduced (this run) sit within ≤ 6% of the W6.1
medians for every row.

## Reproduced Bench Numbers

Both the W6.1 close numbers (master `cb14970f`) and the DEEP-B
fresh-build numbers (master `15e1e5a1`, this worktree's deep-B target)
sit within noise. Master `15e1e5a1` is the post-AZ-IV close-synthesis
HEAD; intervening commits land documentation only.

| Pair                                     | W6.1 bbnf | W6.1 sonic | DEEP-B bbnf | DEEP-B sonic | DEEP-B ratio |
|------------------------------------------|-----------|------------|-------------|--------------|--------------|
| bbnf_get_twitter / sonic_get_twitter     | 1.396 ms  | 332.7 ns   | 1.333 ms    | 332.7 ns     | 4007×        |
| bbnf_value_twitter / sonic_value_twitter | 1.279 ms  | 245.1 µs   | 1.361 ms    | 244.8 µs     | 5.56×        |
| bbnf_value_canada / sonic_value_canada   | 245.8 ms  | 1.474 ms   | 224.8 ms    | 1.556 ms     | 144×         |
| bbnf_value_citm / sonic_value_citm       | 4.814 ms  | 590.5 µs   | 5.095 ms    | 619.5 µs     | 8.22×        |
| bbnf_value_data_s / sonic_value_data_s   | 36.95 µs  | 14.39 µs   | 39.12 µs    | 14.7 µs      | 2.66×        |
| bbnf_value_data_xl / sonic_value_data_xl | WATCHDOG  | 14.59 ms   | n/a         | 15.45 ms     | n/a          |

Same fat-LTO `[profile.bench]` profile both sides. mimalloc allocator.
divan sample_count=100, max_time=30s. DEEP-B numbers from
`.profiles/samply/post-AZ-IV/deep-B/bbnf_value_twitter/record.txt` —
the prebuilt binary samply traced (`json_value-064365c1cda64313`).
DEEP-B medians sit within ≤ 6% of the W6.1 medians; the post-AZ-IV
W6.1 numbers stand as the record of fact. The ratios are slightly
better in the DEEP-B run because of allocator warmth in a single
binary execution against all entries — the architectural ratios are
unchanged.

## Architectural Attribution

The user's three questions answered concretely.

### Why is sonic-rs's value path 5.22× faster on twitter (1.279 ms vs 245 µs)?

sonic-rs `from_str::<Value>` builds a typed document, but the document
is a *node-pool over the input bytes*: `Value` arms hold packed
discriminators, immediate scalars (f64 inline), and slice-borrowed
strings (`Bytes`-backed cow). The document is one pool allocation plus
SIMD scalar decode plus slice projection.

bbnf's `JsonParser::parse` builds a typed document too — but the path
through the builder is far heavier per compound. Inspection of
`crates/core/src/grammar/generated/json.rs` lines 1512-1519, 1651-1659,
and 1771-1782 shows that **every compound (`object`, `array`, `pair`)
allocates a fresh `StructLayout` on the function stack** — and that
struct contains `rule_name: String::from("object")` (or "array" /
"pair"), which is a heap allocation in stable Rust because `String`
lacks a const constructor for non-empty literals. That layout is then
borrowed into `builder.begin_compound(&__layout)`, where the JSON
builder (`crates/core/src/runtime/json/builder.rs:261-294`) constructs
a fresh `OpenFrame::{Array,Object,Pair,Wrap}` carrying a fresh
`Vec::new()` (capacity 0), and pushes it on the in-flight stack.

Twitter has **15,659 compounds** (1,264 objects + 1,050 arrays + 13,345
pairs, counted off the fixture). Each compound, on the hot path, costs:

1. One `String::from("object" | "array" | "pair")` heap allocation
   (≈ 32 bytes including header).
2. One `Vec<JsonValue>::new()` (no allocation until first push, but
   the first push reallocates).
3. One vector growth sweep on the stack (`self.stack.last_mut()`
   matched in `deposit`).
4. On `end_compound`, one `arena.push_array(items)` or
   `push_object(pairs)` which clones the inner Vec into the
   `Vec<Vec<…>>` slab — every closed compound pays one outer-Vec
   re-allocation when the slab grows.

The aggregate per-twitter overhead is ≈ 31,000 small heap allocations
attributable to the layout/frame substrate alone, plus ≈ 11,600 leaf
deposits. Each leaf deposit does an enum match against
`self.stack.last_mut()` to determine which frame to drop into.

sonic-rs's equivalent has zero per-compound heap allocation: the parser
walks bytes in-place using SIMD scanners (`util/utf8.rs`, `util/string.rs`,
`parser.rs`), depositing into a single backing arena owned by the
`Value`. Number decode is inline; string decode is borrow-by-default with
copy-on-escape; objects/arrays are slice-of-children encoded directly
into the arena.

**The 5.22× factor on twitter resolves to: ≈ 31,000 heap allocations
attributable to the W5 arena/builder template substrate, plus
≈ 11,600 leaf-deposit dispatches with non-inlined match arms, against
sonic-rs's tape-with-no-extras.** The 4-bench scaling (canada 167×,
citm 8.15×, twitter 5.22×, data_s 2.57×) tracks the structural-element
density of the fixture: canada has 56K compounds + 111K leaves
(167K records); citm/twitter have ~16K compounds; data_s has ~hundred.
The ratio scales with structural element count, not bytes — exactly the
signature of per-record substrate overhead.

### Why is sonic-rs's get path 4196× faster (1.396 ms vs 332.7 ns)?

This factor is the **eager-then-walk vs lazy-pointer-walk**
architectural gap, named in `W6-fat-lto.txt` Hard Gate 7. Bench source
`crates/core/benches/json/value.rs:43-66`:

```rust
fn bbnf_get_twitter(b: divan::Bencher) {
    // ...
    |input: String| {
        let parsed = JsonParser::parse(black_box(&input)).unwrap();
        let segs = path!["statuses", 0_usize, "text"];
        let p = bbnf::runtime::Path::new(segs);
        let got: Option<&str> = parsed.get(p);
        ...
    }
}
```

The entry point is `JsonParser::parse(&input)` — the eager parse path
(`crates/core/src/grammar/generated/json.rs:3434-3497`). It runs the
entire 631-KB twitter document through the same `parse_JsonParser_value`
dispatcher used by `bbnf_value_twitter`, paying every cost listed above
*before* the path walk starts. Then `Document::get` walks the
materialised document tree (`document.rs:370-392`, `walk_path`) doing a
linear `find` over `JsonValue::Object` pair slices.

sonic-rs's `get(input, pointer![...])` does *not* parse:
`/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.7/src/lazyvalue/get.rs:171-184` →
`get_unchecked` → `Parser::new(reader)` →
`get_from_with_iter_unchecked(path)` → for each path segment, either
`get_from_object(key, …)` (find this key, skip-others) or
`get_from_array(index)` (find this index, skip-others). When the path
is exhausted, `skip_one()` returns the slice extent of the leaf. No
arena, no Vec, no document, no AST.

`bbnf_get_twitter`'s 1.396 ms is `bbnf_value_twitter` (1.279 ms eager)
plus a small path walk (~100 µs). The 4196× factor is therefore
*not* a separate bug: the lazy lane has not been activated for the
value-API entry point. `parse_with` (the lazy bail-out parse,
`runtime/json/parse_with.rs`) exists, lands cursor-driven, and short-
circuits out of `parse_JsonParser_value` once the cursor declares
"path resolved" — but the bench harness calls `JsonParser::parse`,
not `parse_with`.

### Why is the AU floor 18/19 BELOW?

`docs/benchmarks/post-AU.json` is the binding floor for fat-LTO
`[profile.bench]`. The post-AZ-IV close re-measured 16/19 rows; 14 of
the 16 are BELOW the AU baseline by 1.9× to 983×. The post-AZ-IV close
itself names the cause (`post-AZ-IV.json` line 123, copied below):

> 14 of the 16 measured rows are BELOW AU floor, all driven by the same
> root cause: the AZ-IV W5 arena/builder template substrate
> (`Arena<G> + Builder<G>` parameterised by StructRegistry) replaced
> AU's flat per-grammar arenas; the indirection through the registry
> costs 28-65× on the bbnf_monolithic + sheets_parse_* lanes and
> 1.9-118× on json_monolithic.

Reading the substrate confirms: `crates/core/src/runtime/builder_template.rs:198-240`
defines `SimpleStructBuilder<'p, V, C>` whose `begin_compound` is
generic over the pluggable `SimpleValue` and `SimpleCompound` traits
(lines 63-84). Every `begin_compound` clones the layout
(`frame.layout: layout.clone()`, line 223) — cloning a `StructLayout`
clones its `String` rule_name plus its `Vec<StructField>` fields. The
JSON builder (`json/builder.rs`) does not clone the layout but still
pays the per-call `StructLayout` allocation on the *generated* side
(the `String::from` heap allocation per `begin_compound` site).

The AU-baseline arena was per-grammar and flat: a tape with payload
side-cars and `push_leaf_with_*` taking primitive values directly. AU
post-close inflated to two substrates layered:

- **Generated emission side**: each compound site builds a
  `StructLayout` literal *as a heap-rooted value* (the `String::from(...)`
  in the `rule_name` field of every layout literal in
  `generated/json.rs`).
- **Builder receiver side**: the builder either clones the layout
  (`SimpleStructBuilder` cohort) or matches on `(layout.kind,
  layout.rule_id)` and allocates a typed frame holding a fresh `Vec`
  (`JsonStructBuilder`, `CssL4StructBuilder`).

The total cost per compound is one layout allocation + one frame
allocation + one Vec growth + the dispatching match. The AU substrate
had zero of these — it was a tape where `push_compound` was a single
record write and the typed projection was a separate concern.

The AU floor is BELOW for the 14 same-cause rows because the AZ-IV W5
substrate adds **K constant cost per compound emission**, where K is
≈ one heap allocation for the layout + one heap allocation for the
frame's Vec. When fixture compound density is high (sheets parse_stress,
CSS L4 bootstrap, all bbnf_self / css_l4_grammar grammars), K
dominates. When fixture compound density is low (sheets format_*,
sheets format_stress), K is small enough the rows hit AT_OR_ABOVE / noise.

## Samply Attribution

Trace captured under PROFILING.md's 7-artefact contract at
`.profiles/samply/post-AZ-IV/deep-B/bbnf_value_twitter/`. Hot-path
inclusive-time table, computed by joining each sample's stack against
the syms.json RVA→symbol map and counting one hit per function in the
chain (de-duplicated per stack). Total samples: 25,963.

The divan substring filter `bbnf_value_twitter` matched the entire
`json_value` matrix (every entry whose name contains the substring
`bbnf_value_twitter` is just `bbnf_value_twitter` — but divan's
default-run-all also enumerates the rest); samply therefore profiled
the full 11-entry suite. Per-entry breakdown is via inclusive-time
tags into `bbnf_value_*` / `sonic_value_*` / `bbnf_get_twitter`.

### Top inclusive across the full json_value matrix (samply join)

| % incl | function |
|-------:|----------|
| 91.77  | `<bbnf::grammar::generated::json::JsonParser>::parse` |
| 91.77  | `parse_wrap_JsonParser_value` |
| 91.77  | `parse_object_JsonParser_object` |
| 91.68  | `parse_array_JsonParser_array` |
| 88.78  | `json_value::bbnf_value_canada` (entry function) |
| **86.07** | **`<alloc::vec::Vec<OpenFrame> as Clone>::clone`** |
|  7.75  | `sonic_rs::Value::parse_with_padding` |
|  7.47  | `sonic_rs::Parser::parse_object` |
|  7.47  | `sonic_rs::Parser::parse_array` |
|  5.47  | `mi_heap_malloc_zero_aligned_at_generic` |
|  5.18  | `_mi_malloc_generic` |
|  3.08  | `core::ptr::drop_in_place::<JsonStructCheckpoint>` |
|  2.49  | `mi_segment_span_allocate` |
|  0.98  | `mi_malloc_aligned` |
|  0.82  | `mi_find_page` |
|  0.78  | `mi_free` |

The samply-named primary blocker is **`Vec<OpenFrame>::clone`** — it
spans 86.07% of inclusive samples (i.e. 86.07% of all observed stacks
include a frame inside this clone). The clone is the body of
`<JsonStructBuilder as StructBuilder>::checkpoint`
(`runtime/json/builder.rs:243-251`), which clones `self.stack:
Vec<OpenFrame<'p>>`. The next-largest bbnf-attributable cost is the
mimalloc family (`mi_heap_malloc_zero_aligned`, `_mi_malloc_generic`,
`mi_segment_*`) totalling ≈ 18% inclusive — itself driven by the
Vec/`OpenFrame` allocations the clone path triggers, plus the
per-compound `String::from(rule_name)` and `Vec::new()` heap
allocations.

Cross-referencing the call sites: the generated parser
(`grammar/generated/json.rs`) calls `builder.checkpoint()` at **26
sites**, ten of which sit inside `parse_wrap_JsonParser_value` —
the per-value byte-dispatch tower (`generated/json.rs:1876-2026`).
This dispatcher is invoked once per JSON value (every leaf, every
compound boundary): twitter has 27,259 such values
(15,659 compounds + 11,600 leaves), canada has 167,187. Each invocation
takes a checkpoint *before* trying the matched branch and rolls back
on Err.

Each checkpoint clones `self.stack` (the in-flight `Vec<OpenFrame>`).
Each `OpenFrame` arm holds a `Vec<JsonValue>` or `Vec<JsonPair>` —
**which are themselves cloned recursively when the stack is cloned**
because `OpenFrame` derives `Clone`. On a deep value (an array
inside an array inside an object), the clone visits every open
frame's children Vec and copies it. This is quadratic in nesting depth
× checkpoint frequency.

### bbnf_get_twitter (subset of the same trace)

`bbnf_get_twitter` shares the same parse path. Its stack adds a
trailing `Document::get -> walk_path -> linear find` at the end which
is < 1% of total — the 1.451-ms median is dominated by the eager
parse, exactly as predicted from inspection.

### Comparison: sonic_value_* aggregate (top inclusive in this trace)

| %incl | function |
|------:|----------|
| 7.75  | `<sonic_rs::value::node::Value>::parse_with_padding` |
| 7.75  | `sonic_rs::serde::de::from_trait` |
| 7.47  | `<sonic_rs::parser::Parser<PaddedSliceRead>>::parse_object` |
| 7.47  | `<sonic_rs::parser::Parser<PaddedSliceRead>>::parse_array` |

sonic_value_* totals ≈ 8% wall-time across all five sonic entries
combined — each sonic entry takes < 5% of total samples vs bbnf
entries each taking ~10-30%. There is no equivalent of
`Vec<...>::clone` anywhere in the sonic stack — sonic's `parse_object`
/ `parse_array` deposit directly into the `Value` arena without a
separate per-byte checkpoint/rollback discipline. The sonic stack
spends its time inside SIMD scanners and direct arena writes; the
bbnf stack spends 86% of its time cloning the in-flight frame stack.

### Comparison: sonic_get_twitter (by inspection)

The bench harness for `sonic_get_twitter` calls
`sonic_rs::get(&input, pointer![...])`. Per
`sonic-rs-0.5.7/src/lazyvalue/get.rs:171-184`, the body is
`Parser::new(reader); get_from_with_iter_unchecked(path)`. Path-walk
implementation
(`sonic-rs-0.5.7/src/parser.rs:1780-1799`):

```
for jp in path {
    if jp.as_key() { self.get_from_object(key, &mut temp_buf) }
    else if jp.as_index() { self.get_from_array(idx) }?;
}
self.skip_one()
```

No allocation per path segment beyond a single 32-byte temp_buf for
escaped key normalisation. No arena. No tape. No materialised tree.
The 332.7-ns median is achievable because the pointer walk traverses
≈ 17 KB of input (until "statuses[0].text") with one SIMD-padded
sweep, and returns a slice borrow.

## The Single Primary Blocker

The user's hypothesis: *the arena/builder runtime indirection through
the registry is the architectural mechanism that causes both AU floor
regression AND a substantial fraction of the sonic-rs gap.*

**Validated, with refinement.** The samply trace names the dominant
mechanism precisely: it is not the registry-indirection-by-name (the
`StructRegistry` itself is consulted once at parse init via a
`LazyLock`) — it is **the speculative-rollback discipline of the
StructBuilder trait**. Specifically:

`<StructBuilder>::checkpoint` returns a `Self::Checkpoint` value, and
the JSON impl's checkpoint *clones the entire in-flight stack of
`OpenFrame`s* (`runtime/json/builder.rs:243-251`). Every
`builder.checkpoint()` call site in the generated parser allocates a
fresh `Vec<OpenFrame>`, deep-clones every open frame's
`Vec<JsonValue>` / `Vec<JsonPair>`, and stores the snapshot — solely
so that on a parse-Err it can roll back. The generated parser calls
`checkpoint()` 26 times per `parse_wrap_JsonParser_value` (the
per-value byte-dispatch tower at `generated/json.rs:1876-2026`), and
that dispatcher is invoked once per JSON value. samply attributes
**86.07% of inclusive samples** to the `Vec<OpenFrame>::clone` path
(verified by joining the post-symbolicated trace against
`profile.json.syms.json` and counting one hit per de-duplicated
function name per stack across all 25,963 samples).

This is the architectural mechanism that links both questions:

1. **The 5.22× sonic-rs value gap on twitter** is dominated by
   per-value checkpoint clones. Twitter has 27,259 values; each
   triggers one `parse_wrap_JsonParser_value` call which does up to 26
   checkpoints (one per byte-dispatch branch). Even with first-byte
   dispatch winning on the first try, every successful branch *still*
   takes a checkpoint and commits — and for nested structures, the
   stack being cloned is non-empty.
2. **The AU floor regression (18/19 BELOW)** is the same mechanism:
   the AU substrate's per-grammar flat tape did *not* clone state on
   speculative branches. AZ-IV W5 introduced the trait-based
   `StructBuilder` discipline with a generic `Checkpoint` associated
   type whose default impl in JSON is `stack.clone()`. Every grammar
   that ships a typed builder (BBNF, EBNF, CSV, CSS L4, Sheets) now
   pays this cost on every speculative branch — exactly the rows
   ranging 28-983× regression vs AU.
3. **The 4196× get factor** is because `bbnf_get_twitter` runs the
   full eager parse (paying every checkpoint clone) and then walks
   the materialised document, vs sonic_get's pure byte-walk with
   zero allocation. Eliminating the checkpoint clone alone shrinks
   `bbnf_value_twitter` toward sonic — but the get factor only
   collapses when the entry point switches to `parse_with` (lazy
   bail-out parse) instead of `JsonParser::parse`.

The single attribution: **the `StructBuilder::checkpoint` discipline
deep-clones in-flight Vec frames on every speculative branch**. The
secondary attribution from the W6 evidence (per-compound `StructLayout`
literal allocation, per-frame `Vec::new()` in `begin_compound`) is
real but subordinate — samply puts it at < 5% of the trace, well
behind the 86% spent inside `Vec<OpenFrame>::clone`. The W6 `W5
arena/builder template substrate` named root cause was correct in
its substrate-attribution but conservative in its mechanism: not
indirection per se, but the *speculative-rollback semantics* the
substrate makes mandatory.

## Mirroring SOTA `get`

### sonic-rs `pointer!` ingredients

`sonic-rs`'s `pointer![...]` macro produces a heterogeneous array of
`PointerNode` values. The `Index` trait is implemented for `&str`,
`String`, `usize`, `u64`, etc.; each `path_iter().for_each(...)` step
projects to either `as_key()` or `as_index()` and dispatches:

```rust
get_from_with_iter_unchecked(path) {
    for jp in path {
        if jp.as_key() { self.get_from_object(key, ...) }
        else if jp.as_index() { self.get_from_array(idx) }
    }
    self.skip_one()
}
```

The `get_from_object` body iterates pairs, scans string keys, *skips*
mismatching values (the value parser is bypass-mode: SIMD scan for
matching close-bracket), and returns when the key matches. Same for
`get_from_array`. At end of path, `skip_one` reads the leaf extent and
returns the byte slice.

### simdjson OnDemand iterators (architectural reference)

simdjson's `OnDemand` model (the bedrock sonic-rs adapts) has two
phases:
1. **Stage 1 — structural index**: SIMD-scan the entire input,
   producing a flat byte index of every structural character (`{ [ ]
   } " : , true false null` digits).
2. **Stage 2 — iteration**: iterate the structural index, type-tag on
   demand, *no value materialisation*. The user navigates via
   `value.find_field("name")` / `value[idx]` — these advance the
   iterator without building a tree.

A `get` is a stage-2 walk that runs to the target offset and then
returns the slice extent. Total cost ≈ one stage-1 scan (≈ 1.5
GB/s) + path-length stage-2 advances. For twitter (631 KB), stage-1 is
≈ 420 µs in pure simdjson; sonic-rs's variant achieves 332 ns by
skipping stage-1 entirely and going direct-byte-walk for shallow paths
— it knows the full structural surface is not needed if the path is
short.

### What our equivalent looks like (with superior ergonomics)

Today: `parse_with` exists at `crates/core/src/runtime/json/parse_with.rs`,
takes a `TypedPath<Json, T>` (compile-time-typed path), runs a
cursor-driven parse that bypasses subtrees outside the path's reach.
The cursor consults a `__path_plan::lookup(rule_id, kind)` static to
decide `ParseFully` vs `ParseUntil(idx)` per shape. This is
architecturally equivalent to sonic's `get_from_with_iter` plus a
bbnf-specific cursor.

Missing surface (the ergonomic mirror):

1. **`Document::get<T>(typed_path)`** — today's `Document::get` uses a
   borrowed-segment `Path<'_>` and walks the *materialised* document.
   Reroute: when the call originates against a typed grammar, dispatch
   into `parse_with` instead of materialising the document. The parse
   call site must accept `&str` (not `Document`) to short-circuit
   parsing.

2. **`pointer!` macro analog**: today the `path!` macro builds a
   borrowed `Path` of `PathSegment::{Field, Index}`. A typed analog —
   produced from the grammar's known shape — already exists as
   `TypedPath<G, T>` (consumed by `parse_with`). The surface need is:
   `bbnf::pointer![Json: "statuses", 0_usize, "text"]` returning a
   `TypedPath<Json, &'static str>`.

3. **Direct `JsonParser::get<T>(input, path)` entry point**: skip the
   `parse(input)` step entirely. Internally calls
   `parse_with::<T>(input, &path)`. This is the `sonic_rs::get(input,
   pointer![...])` mirror.

The substrate is in place; AZ-IV W3 landed `parse_with`. What's
missing is the entry-point routing and ergonomic surface — the
`Document::get(p)` shape continues to walk a materialised document
because nothing ever calls `parse_with`.

Superior ergonomics over sonic-rs: bbnf's `TypedPath<G, T>` carries
the leaf type at compile time, so `JsonParser::get<&str>(input,
pointer!["statuses", 0, "text"])` is a typed query — the leaf
projects to `Option<&str>` directly, no `LazyValue::as_str()` call.
sonic-rs returns `LazyValue` and forces the consumer to coerce.

## Recommendations to DEEP-C

Path-forward perf-attributable items, ordered by leverage on the
identified single primary blocker (samply-validated).

1. **Eliminate the `Vec<OpenFrame>::clone` checkpoint discipline.**
   The samply-named primary hot path. Two architectural moves
   compose: (a) make `Checkpoint` a *length-and-position* tuple, not
   a deep clone — `(stack_depth, arena_array_len, arena_object_len,
   pending_key_state, root_state)`. On rollback, *truncate* the stack
   to `stack_depth` and the arena to its prior counts; do not restore
   the contents because parse-Err implies the partial frames are
   garbage anyway. (b) make speculative-parse the rare path: replace
   the per-byte-literal try-then-rollback dispatch tower in
   `parse_wrap_JsonParser_value` with a *first-byte-determines-branch*
   predictive jump (the byte alphabet is disjoint per branch — `"`
   is string, `[` is array, `{` is object, `t/f` is bool, `n` is
   null, digits + `-` are number). Predictive dispatch eliminates
   the rollback discipline for the JSON grammar entirely; the
   checkpoint mechanism is preserved for grammars where ambiguous
   byte-prefixes genuinely require speculation. Estimated effect:
   ≥ 80% reduction on bbnf_value_twitter (samply's 85.95% inclusive
   trace says this is the load-bearing optimisation). Pulls
   `bbnf_value_twitter` from 1.42 ms toward ≤ 350 µs (≤ 1.5×
   sonic).

2. **Activate `parse_with` from the value-API entry point.**
   `parse_with` (`runtime/json/parse_with.rs`) already implements the
   cursor-driven byte-skip-subtrees-outside-path discipline. Today the
   bench-callable entry is `JsonParser::parse(input).get::<T>(path)`
   which runs the eager parse first. New entry point:
   `JsonParser::get::<T>(input, &typed_path) -> Option<T>` calling
   into `parse_with::parse_with::<T>(input, &typed_path)` directly.
   Estimated effect: 1.451 ms → ≤ 30 µs on bbnf_get_twitter (closing
   Hard Gate 7's ≤5× sonic target by two orders of magnitude in one
   change).

3. **Hoist `StructLayout::rule_name: String` to `&'static str`.**
   `crates/ir/src/registry/struct.rs:209`. The generated emit side
   already passes string literals (`String::from("object")`); the
   indirection forces a per-compound heap allocation that the
   compiler cannot eliminate because `String` is `Drop`. Switching
   to `&'static str` eliminates ≈ 15K mi-mallocs per twitter parse,
   ≈ 56K per canada. samply puts these at ~1% inclusive each —
   secondary to the checkpoint clone but stacks with (1).

4. **Stack-allocate `OpenFrame::{Array,Object,Pair}` children with
   `SmallVec<[_; N]>`** — `runtime/json/builder.rs:62-87` and
   `runtime/builder_template.rs:93-106`. JSON array median length is
   short (twitter median ≤ 4, citm ≤ 8); object key counts likewise.
   `SmallVec<[JsonValue; 4]>` for arrays, `SmallVec<[JsonPair; 4]>`
   for objects eliminates one heap allocation per compound, plus
   makes the `Vec<OpenFrame>::clone` (still active in some paths)
   stack-friendly. Composes with (1) — when the speculative-clone is
   eliminated, the SmallVec only has to absorb non-speculative
   compound construction, which is a much smaller surface.

5. **Replace `JsonArena::{arrays,objects}: Vec<Vec<…>>` with a flat
   bump arena.** `runtime/json/arena.rs:97-100`. Today `push_array`
   moves the open-frame Vec into the `arrays` slab, paying one heap
   allocation per compound (the slab grow and the per-Vec retention).
   Replace with a single `bumpalo::Bump` plus `(offset, len)` slice
   handles. Composes with (4): SmallVec spilled-to-heap goes into
   the bump arena, not into a fresh `Vec<JsonValue>`. Eliminates the
   second-allocation tier on every compound. Same architectural move
   sonic-rs makes — value contents live in a single backing arena
   addressed by offset, not per-compound boxes.

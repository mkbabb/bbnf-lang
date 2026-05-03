# SOTA Survey for BA-Restart — Grammar-Derived Parser Fleet

Date: 2026-05-03
Scope: Establish ground truth for direct-to-struct typed-record emission, path APIs, value APIs, and the pure-naming question for the grammar to typed-record step. Targets: sonic-rs, simdjson, simd-json (Rust port), lightningcss, serde_json, pest, nom, chumsky, parol.

---

## 1. Cross-Comparison Matrix

| Library         | Typed Derivation                   | Materialization     | Memory Model                | Dispatch                       | Path API                       | Throughput Headline                |
|-----------------|------------------------------------|---------------------|-----------------------------|--------------------------------|--------------------------------|------------------------------------|
| simdjson        | Hand-written DOM + On-Demand       | Lazy (On-Demand)    | Tape (DOM) / index-only (OD)| SIMD structural indexing       | `at_pointer("/foo/a/1")`       | 7 GB/s On-Demand; 13 GB/s UTF-8    |
| sonic-rs        | Hand-written `Value` + serde derive| Lazy by default     | Bumpalo arena (whole-doc)   | SIMD primary + scalar fallback | `pointer!["a","b",1]`+`get`    | 3-4x serde\_json; 436 µs twitter   |
| simd-json (Rust)| Hand-written, port of simdjson C++ | Tape-then-typed     | Tape + padded buffer        | SIMD                           | `at_pointer`                   | ~1-2 GB/s (varies)                 |
| serde\_json     | Hand-written `Value` + serde derive| Eager               | `Vec`/`Box`/`BTreeMap`      | Byte-by-byte recursive descent | `value.pointer("/a/b/1")`      | 500-1000 MB/s deser                |
| lightningcss    | Macro-derived (`define_properties!`)| Eager               | `cssparser` slices, no bumpalo| Hand-written recursive descent| Visitor trait + `visit_*`      | 4.16 ms vs cssnano 544 ms (Bootstrap)|
| pest            | Untyped `Pair` iterator (no struct typing)| Lazy        | Heap                        | Codegen recursive descent      | Tree-walk via `into_inner`     | n/a                                |
| nom             | Combinator output type (no derive) | Eager (composed)    | Zero-copy slices            | Trait dispatch                 | n/a                            | n/a                                |
| chumsky         | `.map()` composition (no derive)   | Eager (composed)    | Zero-copy slices            | GAT-driven optimizer           | n/a                            | 533 MB/s parse, 797 MB/s check     |
| parol           | **Auto-generated typed AST**       | Eager               | `Box` for recursion         | Generated LL(k)/LALR(1)        | n/a                            | n/a                                |

---

## 2. Per-Target Findings

### 2.1 sonic-rs

- **Source root:** `/src/lib.rs`, `/src/lazyvalue/`, `/src/value/`, `/src/pointer/`, `/src/parser.rs`. Reference: [GitHub tree](https://github.com/cloudwego/sonic-rs/tree/main/src).

- **Typed value derivation.** Hand-authored `Value` enum in `src/value/`. Strongly typed user structs piggyback on `serde::{Serialize, Deserialize}` derive macros — no bespoke schema synthesis. Notable: sonic-rs never builds a tape; it deserializes directly into the user struct and openly cites this as the reason it beats simd-json: "Sonic-rs directly parses the JSON into a Rust struct, and there are no temporary data structures." (README, [cloudwego/sonic-rs](https://github.com/cloudwego/sonic-rs)).

- **Path API.**
  ```rust
  use sonic_rs::{pointer, get_unchecked};
  let path = pointer!["a", "b", "c", 1];
  let value = unsafe { get_unchecked(json, &path).unwrap() }; // -> LazyValue
  ```
  Public exports include `get`, `get_unchecked`, `get_from_str`, `get_from_bytes`, returning `LazyValue<'a>` or `Result<LazyValue>`. The `pointer!` macro builds a `JsonPointer` heterogeneous over `&str` keys and `usize` indices ([docs.rs/sonic-rs](https://docs.rs/sonic-rs)).

- **Materialization.** Lazy-first. `LazyValue` is "a wrapper of a raw valid JSON slice"; conversion happens at `.as_str()` / `.as_i64()` time. `OwnedLazyValue` clones the slice into owned bytes for set/mutation. Hereupon the borrowed-slice apotheosis: zero-copy until the consumer demands a typed projection.

- **Memory model.** Whole-document **bumpalo-style arena** for parsed `Value`. README: "Sonic-rs uses a memory arena for the whole document, resulting in fewer memory allocations, better cache-friendliness, and mutability." LazyValue is just a `&[u8]` borrow of the input plus a single index; it allocates nothing.

- **Dispatch.** SIMD-primary on x86\_64 and aarch64 (long string scan, float fraction parse, field skip, whitespace skip), scalar fallback elsewhere. No structural-indexing tape pass like simdjson; sonic-rs scans inline.

- **Pratt / precedence.** N/A (JSON has no operator chain).

- **Error reporting.** serde\_json-style position display ("Expected ',' or ']' at line 1 column 17"), no recovery, fail-fast.

- **Throughput.** Apple M1 Pro, Criterion `from_slice` ([benchmark\_aarch64.md](https://github.com/cloudwego/sonic-rs/blob/main/docs/benchmark_aarch64.md)):

  | Dataset      | sonic-rs (unchecked) | simd-json | serde\_json |
  |--------------|----------------------|-----------|-------------|
  | twitter      | 436 µs               | 424 µs    | 831 µs      |
  | citm\_catalog| 854 µs               | 831 µs    | 1.376 ms    |
  | canada       | 3.144 ms             | 3.226 ms  | 4.988 ms    |

  No GB/s headline; consumers compute it from byte-count over time.

### 2.2 simdjson (C++)

- **Source root:** `/include/simdjson/`, `/src/`, `/doc/basics.md`, `/doc/ondemand.md`. Reference: [simdjson/simdjson](https://github.com/simdjson/simdjson).

- **Typed value derivation.** Hand-written `dom::element`, `dom::object`, `dom::array`, `ondemand::value`. No derive — schema-less. The 2024 paper "On-Demand JSON: A Better Way to Parse Documents?" by Keiser & Lemire (Software: Practice and Experience, 54(6), [Wiley](https://onlinelibrary.wiley.com/doi/10.1002/spe.3313); preprint [arXiv 2312.17149](https://arxiv.org/html/2312.17149v1)) frames On-Demand as deliberate retreat from materialised tape.

- **Path API.**
  ```cpp
  ondemand::document doc = parser.iterate(padded_json);
  int64_t v = doc.at_pointer("/foo/a/1").get_int64();
  ```
  RFC 6901 JSON Pointer — escape `/` as `~1`, `~` as `~0`. Each `at_pointer` call advances the cursor; preceding keys cannot be re-queried (On-Demand iterator constraint).

- **Materialization.** **Two distinct front-ends**:
  - **DOM** mode: Stage 1 SIMD structural indexing → Stage 2 builds the **tape** (a flat array of typed cells, one word per element, with offsets/lengths inline). Eager whole-document materialisation.
  - **On-Demand** mode (default since v1.0): keeps only the structural index; `document` is "an iterator over the JSON text", not a materialised tree. Values parse only on access, and **skipped values are never validated**. Key claim: "[the tape] data structure simply does not exist in the On Demand approach" — Keiser & Lemire 2024.

- **Memory model.** DOM: tape (`uint64_t[]` ~ 1 word per pseudo-structural char) + secondary string buffer; both pre-allocated at `parser` construction, reused per document. On-Demand: structural index alone (~4 bytes per pseudo-structural char), no secondary tape, requires `padded_string` (input buffered to 64-byte-aligned with 64 bytes of trailing slack for unaligned SIMD loads).

- **Dispatch.** SIMD structural indexing — bit-parallel scan over 64-byte chunks identifying `{ } [ ] : , "` and quote-state bitmaps with `pclmulqdq` for unmatched-quote elimination, plus UTF-8 validation in the same pass. Runtime CPU dispatch (AVX-512, AVX2, SSE4.2, NEON, ARM SVE) selected at first call.

- **Error reporting.** Codes (`SUCCESS`, `INCORRECT_TYPE`, `NUMBER_OUT_OF_RANGE`, ...); no source spans, no recovery.

- **Throughput.** Headline ([README](https://github.com/simdjson/simdjson) + [main site](https://simdjson.org/)):
  - Parsing: 7 GB/s On-Demand on Intel Skylake.
  - Minify: 12 GB/s.
  - UTF-8 validate: 30 GB/s.
  - NDJSON multithreaded: 3.5 GB/s.
  - twitter.json: ~2.2 GB/s.
  Original 2019 paper "Parsing Gigabytes of JSON per Second" ([arXiv 1902.08318](https://arxiv.org/abs/1902.08318)).

### 2.3 simd-json-rs (Rust port)

- The link `SunDoge/simd-json` 404s. The canonical Rust port is `simd-lite/simd-json`. Architecture mirrors simdjson C++ Stage 1 + Stage 2 tape. Tape-then-typed: more allocation than sonic-rs, hence sonic-rs's deserialization edge in the M1 Pro table (similar within margin for parse-to-DOM, but sonic-rs wins on parse-to-struct).

### 2.4 lightningcss

- **Source root:** `/src/visitor.rs`, `/src/properties/mod.rs`, `/src/values/`, `/src/parser.rs`, `/src/macros.rs`, `/src/rules/`, `/src/stylesheet.rs`. Reference: [parcel-bundler/lightningcss](https://github.com/parcel-bundler/lightningcss/tree/master/src).

- **Typed value derivation.** **Macro-derived from a property table** — the most schema-synthesis-flavoured of any target surveyed. `define_properties!` in `src/properties/mod.rs` (around line 430) emits `Property<'i>` and `PropertyId<'i>` enum variants with `Parse<'i>` and `ToCss` impls. Per-property value types live in `src/values/{length,color,...}.rs` (hand-written).
  - Supplementary macros: `enum_property!` (kebab-case ident enums), `define_shorthand!` (shorthand `Background<'i>`, `Border<'i>`, etc.).
  - Procedural derive: `lightningcss-derive::Visit` auto-implements `Visit` trait + `CHILD_TYPES` bitmask.

- **Path API.** **Visitor trait** in `src/visitor.rs`:
  ```rust
  pub trait Visitor<'i, T: Visit<'i>> {
      type Error;
      fn visit_types(&self) -> VisitTypes;
      fn visit_stylesheet(&mut self, _: &mut StyleSheet<'i, '_, T>) -> Result<(),Self::Error> { ... }
      fn visit_rule(&mut self, _: &mut CssRule<'i, T>) -> Result<(),Self::Error> { ... }
      fn visit_url(&mut self, _: &mut Url<'i>) -> Result<(),Self::Error> { ... }
      fn visit_color(&mut self, _: &mut CssColor) -> Result<(),Self::Error> { ... }
      fn visit_length(&mut self, _: &mut Length) -> Result<(),Self::Error> { ... }
      // ...visit_angle, visit_ratio, visit_resolution, visit_time,
      //    visit_custom_ident, visit_dashed_ident, visit_variable,
      //    visit_media_list, visit_selector_list, visit_function...
  }
  ```
  Bitflag `VisitTypes` (RULES = 1<<0 ... TOKENS = 1<<18) lets the framework prune subtree traversal whose `CHILD_TYPES` does not intersect the visitor's interests. This is depth-first, conditional traversal — name-driven, mechanical, **layout-flavoured**.

- **Materialization.** Eager parse to `StyleSheet<'i, _>`; visitor walks the materialised tree for transforms.

- **Memory model.** **Not bumpalo**. `Cargo.toml` ([master/Cargo.toml](https://github.com/parcel-bundler/lightningcss/blob/master/Cargo.toml)) lists `cssparser`, `cssparser-color`, `parcel_selectors`, `smallvec`, `bitflags`, `indexmap`, `itertools` — no bumpalo. Memory hygiene comes from `&'i str` slices over the input (lifetime `'i`), `CowArcStr<'i>` for case-folded names, and `SmallVec` for hot lists. Their README phrases this as "efficient use of memory" rather than arena allocation.

- **Dispatch.** Hand-written recursive descent atop Mozilla's `cssparser` tokenizer (which is itself byte-by-byte hand-written, table-driven for whitespace/identifier classes).

- **Pratt / precedence.** CSS calc() and similar use grammar-encoded precedence; no Pratt machinery — the grammar is shallow enough for recursive descent.

- **Error reporting.** `ParseError<'i, ParserError<'i>>` carries `(line, column)` and a `kind` (cssparser-derived); no recovery (fail-fast), but minifier preserves un-recognised tokens as raw `Token::*`.

- **Throughput.** Bootstrap 4 / animate.css / tailwind.css ([README](https://github.com/parcel-bundler/lightningcss/blob/master/README.md)):

  | Dataset      | lightningcss | esbuild   | cssnano    |
  |--------------|--------------|-----------|------------|
  | bootstrap-4  | 4.16 ms      | 17.20 ms  | 544.81 ms  |
  | animate.css  | 1.97 ms      | 11.86 ms  | 283.11 ms  |
  | tailwind.css | 43.37 ms    | 107.67 ms | 2.198 s    |

### 2.5 serde\_json (baseline contrast)

- **Source root:** `/src/value/mod.rs`, `/src/de.rs`, `/src/value/index.rs`. Reference: [serde-rs/json](https://github.com/serde-rs/json).
- **Typed value derivation.** Hand-written `Value` enum: `Null | Bool(bool) | Number(Number) | String(String) | Array(Vec<Value>) | Object(Map<String,Value>)`. User structs derive serde traits. No grammar layer.
- **Path API.**
  ```rust
  pub fn pointer(&self, pointer: &str) -> Option<&Value>;
  pub fn pointer_mut(&mut self, pointer: &str) -> Option<&mut Value>;
  ```
  RFC 6901 compliant — `~1` and `~0` escapes. Implementation uses `try_fold` over slash-separated tokens.
- **Materialization.** Eager. `from_str` constructs the full `Value` tree.
- **Memory model.** `Vec<Value>`, `String`, `BTreeMap` (or `IndexMap` with `preserve_order`); no arena; per-string heap allocation.
- **Dispatch.** Byte-by-byte hand-written recursive descent, no SIMD.
- **Error reporting.** `serde_json::Error` with line/column and category (`Io | Syntax | Data | Eof`); no recovery.
- **Throughput.** Documented baseline: "500 to 1000 megabytes per second deserialization and 600 to 900 megabytes per second serialization" ([README](https://github.com/serde-rs/json)).

### 2.6 pest

- **Typed value derivation.** `#[derive(Parser)]` from a `.pest` grammar — but emits an **untyped `Pairs<Rule>` iterator**, not typed structs. The user matches on `Rule::*` and walks `Pair::into_inner()`. This is the antithesis of grammar-derived typed records: no schema synthesis whatsoever.
- **Path API.** Tree walking only (`into_inner`, `find_first_tagged`, `next`); no random access.
- **Materialization.** Lazy iterator over pre-recorded `QueueableToken`s.
- **Memory model.** Heap-allocated token queue, `Rc`-shared input.
- **Dispatch.** Macro-generated recursive descent with packrat memoization.
- **Pratt / precedence.** Yes — `pest::pratt_parser::PrattParser` with infix/prefix/postfix operator declarations and precedence levels.
- **Error reporting.** Excellent — line/col + caret-pointed snippet via `pest::error::Error`.

### 2.7 nom

- **Typed value derivation.** None. Each combinator's output type is whatever its function/method composition yields; the user assembles types by hand. Zero codegen.
- **Path API.** None.
- **Materialization.** Eager — outputs are computed by the time the combinator returns.
- **Memory model.** Zero-copy by default — slice-based `&[u8]` / `&str`.
- **Dispatch.** Trait-based; combinator functions inline at compile time.
- **Pratt / precedence.** Not built-in; the user writes a Pratt loop manually atop `nom`.
- **Error reporting.** `Err<E>` parameterised over `E`; `VerboseError` adds context tags; `nom_locate` adds line/col.

### 2.8 chumsky

- **Typed value derivation.** Output by `.map()` composition; no schema-synthesis derive.
- **Path API.** None.
- **Memory model.** Zero-copy slices over input; nested-input support for token trees.
- **Dispatch.** Compile-time GAT-driven optimiser inlines parser composition.
- **Pratt / precedence.** Yes — `pratt` feature flag, `chumsky::pratt::*` operator-precedence builder.
- **Error reporting.** `Rich` (full diagnostics) vs `Simple` (cheap); integrates with [Ariadne](https://crates.io/crates/ariadne) for caret rendering.
- **Throughput.** ~533 MB/s parse, ~797 MB/s check-only on JSON benchmark per README.

### 2.9 parol (most-isomorphic to bbnf-lang's intent)

- Reference: [jsinger67/parol](https://github.com/jsinger67/parol), [AST Generation docs](https://jsinger67.github.io/AstGeneration.html).
- **Typed value derivation.** **Auto-generated typed AST from grammar.** Parol describes itself: "parol can automatically generate all types implied by your grammar." The pipeline:
  1. **Canonicalisation** — eliminate EBNF sugar (`[B]`, `{B}`, `(B)`) into pure productions.
  2. **Validation** — left-recursion check, unproductive non-terminals, unreachable symbols, left-factoring.
  3. **Type emission rules**:
     - Single production for non-terminal A → struct `A { ... }`.
     - Multiple productions → enum `A { Variant1(...), Variant2(...), ... }`.
     - Recursive references wrapped in `Box<T>` (with `minimize_boxed_types()` to elide where possible).
- Parol calls this step **"AST Generation"** with the underlying mechanism named **"Type Inference"**. Note: it is purely *deterministic and structural* — it derives types from production shape with no constraint-solver involved. So the name "type inference" here is a misnomer of convenience; what Parol does is closer to **layout lowering** than to type inference in the Hindley-Milner sense.

---

## 3. Tape-vs-Direct: Was Tape Feasible?

Synthesis from the tape-line of the survey:

| Architecture                       | Adopted by             | Trade                                                     |
|------------------------------------|------------------------|-----------------------------------------------------------|
| Stage 1 + Stage 2 tape (build)     | simdjson DOM, simd-json| One alloc-pass, one read-pass; cache-friendly; ~2x reads  |
| Structural index only + on-demand  | simdjson On-Demand     | Skipped values never parsed/validated; lowest memory      |
| Direct-to-struct (no tape)         | sonic-rs               | One pass; minimal allocations; no skip benefit            |
| Direct-to-Value tree (eager)       | serde\_json            | Simplest; per-string heap; no SIMD                        |
| Macro-emitted enum tree (eager)    | lightningcss           | Whole-AST in `'i` slices; visitor for transforms          |

**Verdict on tape resurrection.** sonic-rs publicly cites *the absence of a tape* as the reason it beats simd-json on parse-to-struct: "Sonic-rs is faster than simd-json because simd-json first parses the JSON into a tape, then parses the tape into a Rust struct. Sonic-rs directly parses the JSON into a Rust struct, and there are no temporary data structures." On-Demand makes the same argument from the other side: "the [tape] data structure simply does not exist."

Where tape *does* win: when the consumer wants random access to many fields and the document is hot in cache for multiple traversals (DOM use case), or when typed extraction is rare relative to byte-skip (structural query). For BBNF's stated direct-to-struct goal — the user knows the schema, wants typed records emitted — tape is dead weight; both SOTA contenders (sonic-rs for JSON, lightningcss for CSS) have demolished the tape and won by doing so.

**Recommendation.** Do not resurrect tape as substrate. Commit to **direct-projection into bumpalo-backed typed records**, with a **lazy-borrow mode** (sonic-rs `LazyValue`) for the path-API surface. The tape's only honest niche is "parse once, query N times against unknown schema" — if BBNF needs that, expose it as a separate `LazyValue<'a>` borrow over the input, not as a new IR.

---

## 4. Path / Value API Target — sonic-class Surface

To claim sonic-class superiority, the BBNF runtime must expose:

1. **Pointer-typed path constructor** isomorphic to `pointer!["a","b",1]` — heterogeneous over keys and indices, validated at compile time. (The `gh_path!` macro is the obvious BBNF analogue, with type-checked path against the grammar's emitted type tree.)

2. **Get / get-unchecked split.** Validated `get(input, &path) -> Result<View<'a,T>>` and `unsafe get_unchecked` for hot loops. (RFC 6901 string syntax `"/foo/a/1"` is a separate accommodation.)

3. **Lazy view, eager projection.** Returning a borrowing wrapper (call it `View<'a, T>` or steal `LazyValue<'a>`) which carries the slice and a type-tag; `.as_str()`, `.as_i64()`, `.as_<T>()` materialise on demand. This is exactly sonic-rs's `LazyValue` + `JsonValueTrait`.

4. **Visitor surface for transforms.** The lightningcss visitor trait is the SOTA reference for *editing* a typed tree — `visit_<Name>(&mut self, &mut T)` per emitted record, with a `VisitTypes` bitmask so transforms only walk relevant subtrees. BBNF's macro-emitted records get the visit\_\* methods *for free* since the type tree is generator output.

5. **Pratt for operator chains.** Both pest and chumsky expose Pratt-precedence builders; BBNF's grammar must emit a Pratt-driver for any rule that declares left/right associativity and precedence — neither nom nor sonic-rs help here.

6. **Error model with span + recovery.** chumsky `Rich`-class: source span on every node, recovery points at `;` / `}` / EOL. pest's caret-snippet rendering is the user-facing target.

---

## 5. Naming the Step — Pure Question

The candidates and their literature-grounded meanings:

| Term                  | Established usage                                                                                                                                     | Fit for BBNF?                              |
|-----------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------|
| schema synthesis      | Not a term-of-art in compiler literature; appears informally in DB / OpenAPI tooling. No SOTA parser uses this label. | Marketable but unmoored.                    |
| structural inference  | "Inference" in PL theory implies a constraint-solver (Hindley-Milner, bidirectional). Parol calls its step "type inference" but **does no inference** — it is pure structural unfolding. Misnomer if used. | Misleading — there is no solver.            |
| algebraic projection  | "Algebraic" (Krishnaswami & Yallop 2019, [PLDI](https://dl.acm.org/doi/10.1145/3314221.3314625)) refers to context-free expressions as an algebra; "projection" denotes component extraction from a product (Lean elaborator: ⟨s,t⟩.1). Combining the two is novel — and accurate. | Strong, but coined.                         |
| **layout lowering**   | "Lowering" is unambiguous compiler-literature term (LLVM's "ABI lowering library" 2025; "layout" in Rust monomorphisation). It denotes a **mechanical, name-driven, deterministic** transformation from a higher-level shape to a lower-level concrete representation. Parol's "AST generation" is exactly this: production-shape lowering to struct/enum layout. | **Best fit.**                               |
| type projection       | Lean / Coq usage: extract a component of a product type (`.1`, `.2`); Scala 3 inheritance projections. Means *one piece of an existing type*, not the construction of a type from a grammar. | Wrong axis.                                 |
| type collapsing       | Informal; sometimes used for sum-type quotients. Not a stable term.                                                                                  | Reject.                                     |
| type elaboration      | Elaboration (Lean, [Avigad et al.](https://www.andrew.cmu.edu/user/avigad/Papers/constr.pdf); Pierce, "Type Checking and Elaboration") = converting a **partially specified** expression into a fully-resolved typed term, often via unification and coercion insertion. Carries strong implication of solver. | Misleading — implies inference.             |
| type erasure          | Java generics, C++ `std::any`, Rust `dyn Trait`: discard type information at runtime. **Opposite direction** of what BBNF does. | Outright wrong.                             |

### Recommendation: **layout lowering**.

Justification:

1. **Mechanical and deterministic.** The grammar → type-tree step is a pure structural unfolding (concatenation → struct, alternation → enum, repetition → `Vec<T>`, recursion → `Box<T>`). No constraint solver, no fixpoint, no environment. "Lowering" captures this exactly; "inference" implies a solver that does not exist.

2. **Compiler-literature foundation.** "Lowering" is unambiguous — LLVM uses it (ABI lowering library, [LLVM blog 2025-08-25](https://blog.llvm.org/posts/2025-08-25-abi-library/)), MLIR uses it (dialect lowering passes), Rust uses it (HIR → MIR lowering, MIR → LLVM IR lowering). Every working compiler engineer reads "lowering" the same way.

3. **"Layout" not "type" — for a pointed reason.** Calling it *type* lowering invites confusion with type-checking and type-inference. Calling it *layout* lowering surfaces the right intuition: BBNF is choosing a **concrete in-memory shape** for each grammar production, the way a compiler chooses a struct layout from a higher-level record type. This also nests cleanly with backend-agnostic types ([backend-agnostic-types feedback]) — the layout lowering pass per backend resolves abstract `TypeDesc::Named` into Rust struct, TypeScript interface, WASM linear-memory layout.

4. **Distinguishes BBNF from its peers honestly.**
   - parol claims "type inference" but does layout lowering with a misnomer.
   - lightningcss does layout lowering via macros (`define_properties!`).
   - sonic-rs and serde\_json don't lower at all — they take user types as given.
   Adopting *layout lowering* as the term gives BBNF crisp self-identification and a straight line into compiler vocabulary.

5. **Subordinate vocabulary.** Within layout lowering, the sub-passes have natural names already:
   - **canonicalisation** (EBNF sugar → core productions, parol's term).
   - **shape projection** (production → struct/enum/Vec/Box choice).
   - **emission** (target-language code-gen).

   So: BBNF's pipeline is *grammar → canonicalised grammar → lowered layout (per-backend) → emitted module*.

---

## 6. Closing Posture for BA-Restart

1. **Term.** Adopt **layout lowering** for the grammar → typed-record step. It is the term every compiler engineer reads correctly on first pass.
2. **Tape.** Stay buried. sonic-rs and lightningcss both win by not having one. Direct-to-struct, bumpalo-arena-backed, with optional borrowing `View<'a,T>` for lazy access.
3. **API surface.** Mirror sonic-rs for path/value (typed `pointer!` + `get` + `LazyValue`). Mirror lightningcss for visitor (per-record `visit_*` + `VisitTypes` bitmask).
4. **Pratt.** Adopt chumsky/pest precedence-builder pattern in the lowering — emit a Pratt driver per declared operator-chain rule.
5. **Errors.** chumsky-Rich-class spans + pest-style caret rendering as the codegen target.

Last: keep the macro-emission discipline of lightningcss (per-record macros, no god-modules) — every grammar rule lowers to its own typed module, the way `define_properties!` emits per-property files into `src/properties/`. This satisfies the no-god-modules edict without architectural cost.

---

## Sources

- [cloudwego/sonic-rs](https://github.com/cloudwego/sonic-rs)
- [cloudwego/sonic-rs benchmark\_aarch64.md](https://github.com/cloudwego/sonic-rs/blob/main/docs/benchmark_aarch64.md)
- [docs.rs/sonic-rs](https://docs.rs/sonic-rs)
- [simdjson/simdjson](https://github.com/simdjson/simdjson)
- [simdjson.org](https://simdjson.org/)
- [On-Demand JSON: A Better Way to Parse Documents? — Keiser & Lemire 2024 (Wiley)](https://onlinelibrary.wiley.com/doi/10.1002/spe.3313)
- [On-Demand JSON arXiv preprint 2312.17149](https://arxiv.org/html/2312.17149v1)
- [Parsing Gigabytes of JSON per Second — Langdale & Lemire 2019 (arXiv 1902.08318)](https://arxiv.org/abs/1902.08318)
- [parcel-bundler/lightningcss](https://github.com/parcel-bundler/lightningcss)
- [parcel-bundler/lightningcss src tree](https://github.com/parcel-bundler/lightningcss/tree/master/src)
- [parcel-bundler/lightningcss Cargo.toml](https://github.com/parcel-bundler/lightningcss/blob/master/Cargo.toml)
- [serde-rs/json](https://github.com/serde-rs/json)
- [pest-parser/pest](https://github.com/pest-parser/pest)
- [rust-bakery/nom](https://github.com/rust-bakery/nom)
- [zesterer/chumsky](https://github.com/zesterer/chumsky)
- [jsinger67/parol — AST Generation](https://jsinger67.github.io/AstGeneration.html)
- [Krishnaswami & Yallop, A Typed Algebraic Approach to Parsing, PLDI 2019](https://dl.acm.org/doi/10.1145/3314221.3314625)
- [Avigad et al., Elaboration in Dependent Type Theory](https://www.andrew.cmu.edu/user/avigad/Papers/constr.pdf)
- [Lean Reference — Elaboration and Compilation](https://lean-lang.org/doc/reference/latest/Elaboration-and-Compilation/)
- [LLVM ABI Lowering Library blog post 2025-08-25](https://blog.llvm.org/posts/2025-08-25-abi-library/)
- [bumpalo crate](https://github.com/fitzgen/bumpalo)

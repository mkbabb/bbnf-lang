# LOCK-4 · Pass-3 · Family 4B — COMPTIME-FUSION — research

**Thesis (carried).** Combinators stay hand-written and lawful; their *compilation* is a
staging step. At grammar-construction time a combinator tree is specialized into a
monomorphic, vtable-free region — the per-combinator indirect call and per-combinator heap
box both vanish, and LLVM (Rust) or a build-time source emitter (V8) inlines the spine to the
leaf kernels. The specialization *is* the runtime; no runtime graph survives. This is partial
evaluation of the parser with respect to the grammar (the First Futamura projection). Pass 3
resolves the still-open mechanism questions with code-verified traces and the exact missing
measurements.

---

## Q1 — Does the *runtime* combinator API reach comptime monomorphization? (mechanism trace)

**Answer: no — not via any runtime→comptime bridge; only a build-time codegen step over a
comptime-known graph monomorphizes, and that step is a *different* program from the runtime
API.** Two disjoint machines exist and must not be conflated:

1. **The runtime graph (parse-that).** The public API builds a graph of *boxed closures*.
   `Parser<'a,O>{ parser_fn: SmallBox<dyn ParserFn+'a,S32>, flags:u8 }`
   (`rust/parse_that/src/parse.rs:61`); every `.then()/.or()/.map()` calls `SmallBox::new`
   (`parse.rs:74`) and dispatches through the `dyn ParserFn` vtable in `call` (`:80`).
   parse-that has **no codegen step at all** — the graph is constructed at load time and
   *interpreted by direct closure calls* at parse time. There is no point at which a runtime
   `Parser` value reaches type-monomorphization: the type `SmallBox<dyn ParserFn>` is already
   erased, so the compiler sees one opaque type for every combinator and instantiates nothing
   grammar-specific. Absent codegen-reflection (parse-that emits no source), **the
   runtime→comptime step does not exist.**

2. **The comptime graph (bbnf-lang generator).** Monomorphization lives *only* on the
   grammar-text→IR→emit path. Grammar text is parsed at build time (`cargo xtask regen`),
   lowered (`crates/core/src/lower/mod.rs` `lower_ln`) into a `GrammarIR`
   (`crates/ir/src/types/node.rs`), and a backend emitter
   (`crates/core/src/backend/emitter.rs`, `backend/mod.rs`) emits monomorphized Rust/TS
   source. This graph is comptime-known *because the grammar is a build-time input* — never
   because a runtime `Parser` was reflected. The VM path
   (`crates/ir/src/vm/interpreter/mod.rs`) is the runtime-graph analog on the bbnf side and it
   **interprets bytecode, never monomorphizes** (see Q7 for its setup tax).

**What a monomorphizing lowering of the runtime API demands.** The API return type must change
from an erased box to a *statically-known concrete type* — winnow's shape: `.then()` returns
`Seq<A,B>(A,B)` not `SmallBox<dyn>`, `.or()` returns `Alt<A,B>`, `.map()` returns `Map<P,F>`,
each `impl Parser` with an `#[inline] parse(&self,state)` calling `self.0.parse(...)` on a type
LLVM knows. Then `Seq<Str,Seq<Ws,Num>>` monomorphizes to one function inlined flat. The box
survives *only* at named dynamic boundaries — recursion via `lazy`
(`rust/parse_that/src/lazy.rs:21` `Box<dyn LazyParserFn>`) and host closures — identical in
spirit to the in-tree `SpanKind::Boxed` escape hatch
(`rust/parse_that/src/span_parser/mod.rs:131`). This is a **signature-level change to the
public API**, not a hidden pass: the return-type generic *is* the plan the compiler consumes.
The alternative that keeps the API's `-> Parser` erased type is the enum-dispatch realization
(M1, `span_parser`), which pays one predictable jump over a closed variant set instead of zero
— no generic blowup, no signature change, but not true monomorphization.

---

## Q2 — Arbitrary-host-closure surface (census + serializable descriptor)

**Today no descriptor can carry an arbitrary host closure.** `FnDescriptor`
(`crates/ir/src/types/fn_descriptor.rs:18`) has six variants — `EnumWrap`, `BoxWrap`,
`NumberConvert{allow_leading_dot}`, `HexConvert{fn_path:StringId}`, `SpanCapture`,
`Expr{expr:MapExpr, return_type}`. Every `->` map lowers to `Expr` (`:16` comment). `MapExpr`
(`crates/ir/src/types/map_expr.rs:17`) can express only: `Input`, `InputProp{prop}`,
`FnCall{name,args}`, `BinOp`, `UnaryOp`, and literals (`IntLit/FloatLit/BoolLit`). **There is
no `HostClosure` variant** — an arbitrary multi-statement closure is representable *only* by
collapsing it to one named `FnCall{name}`, whose body is then opaque.

**Per-corpus non-projection counts** (combinator `.map`/`.chain` closures that are NOT single
projections, so not `MapExpr`-expressible):

| corpus | `.map` sites | block-body `.map` (multi-stmt) | `.chain` (monadic) | non-projection total |
|---|---|---|---|---|
| **latex-paper** (`/Users/mkbabb/Programming/latex-paper/src`) | 98 | 21 | 2 | **23 (~23%)** |
| **value.js** (R4-canonical 3.1.0: 59 `all()` + `dispatch`) | projection-dense | ~0 block-body in current tree; 27 expr-body maps | 0 | **≈0** |

latex-paper is the context-dependent stress case (`src/grammar/{commands,text,environments}.ts`
carry the 21 block bodies; `.chain` at `src/grammar/compile.ts`); value.js is projection-clean
(its `all()`-of-mapped-parts sites reduce to `MapExpr`). The 23 latex sites are the
arbitrary-closure surface a projection-only mechanism cannot see.

**Proposed typed shape.** Add `FnDescriptor::HostClosure{ fn_id: HostFnId, arg_types:
Vec<TypeDesc>, ret_type: TypeDesc }` — a serializable *reference* into a host-fn lattice
(LOCK-7 owns the lattice), opaque body, analyzable signature. The body does not serialize; the
`fn_id` + typed signature does, and each backend resolves `fn_id` to its native function. This
is the minimal vocabulary that lets a host-closure leaf be *plannable* (typed, boxed at a named
boundary) rather than un-representable. Implied host-fn lattice: signatures form a product of
`(input TypeDesc → output TypeDesc)` arrows; totality is the measured fraction of `->` sites
whose closures reduce to `MapExpr` vs those needing a `HostClosure` reference (§Q3).

---

## Q3 — Opaque-barrier surface size

**`Opaque(HostFnId)` does not exist in code** (grep of `crates/ir/src` = 0 hits). The nearest
extant barriers are `HexConvert{fn_path}` (`fn_descriptor.rs:45`) and `MapExpr::FnCall{name}`
(`map_expr.rs:31`) — named host-fn references whose *bodies* are opaque to constant folding but
whose *call sites* remain visible to the graph. So the real barrier ratio is the fraction of
map nodes that would become the proposed `HostClosure` variant:

- **latex-paper: ~23% of map sites (23/98)** — far above the ≤2% floor. latex is *not* a
  projection-clean corpus; it is context-dependent by nature (BibTeX, command environments), so
  its analysis barrier is intrinsically large.
- **value.js: ≈0%** — its combinator surface is projection-dense (`all()`-of-mapped tuples,
  `dispatch` on function-name buckets), comfortably under the ≤2% floor.

**Finding for the family:** the ≤2% opaque floor is *achievable only on projection-dense
grammars*; it is a property of the corpus, not the mechanism. 4B must fuse *around* opaque
barriers (they anchor a box; the spine on either side still monomorphizes), and must report the
barrier ratio per corpus rather than assume a global floor. A grammar past a threshold opaque
ratio (latex) fuses mostly its structural spine while its value plane stays boxed — the win
concentrates on recognition, not projection, there.

---

## Q4 — Fusion-policy / blowup budget (the owed firing numbers)

The policy must be pluggable (per the pluggable-decision edict), a cost/size model choosing
per-site among `{inline-monomorphize | enum-node | box-here}`. In-tree precedents fix the
*shape* of the caps; pass 3 names the **firing thresholds** and the exact still-missing
measurement.

- **fuse-arity cap.** `seq!`/`alt!` unroll arity **2–8** (`rust/parse_that/src/combinators/macros.rs`
  `seq!` `:6`, `alt!` `:83`); beyond 8 they fall to the flat `Seq(Vec<SpanParser>)`/`OneOf`
  enum arm (`span_parser/mod.rs:88`) which is **O(1) in generated size**. Prior-art ceiling:
  winnow's tuple `impl Parser` blocks stop near 21, but instantiation compile-time grows
  superlinearly past ~12. **Propose cap = 8** (matches the macro precedent, well inside the
  superlinear knee).
- **dispatch-fanout cap.** A wide `alt!`/`OneOf` past **~4 arms** lowers to the first-char LUT
  (`typescript/src/parse/leaf.ts:100` `dispatch` Int8Array(128); Rust `leaf.rs:182`
  `TakeUntilScan::Lut(Box<[bool;256]>)`) — O(1) regardless of fanout, so a wide alternation
  never unrolls N-way.
- **boxing-depth cap.** Every recursion/`lazy` edge (`lazy.rs:21`) and every `HostClosure`
  (§Q2) forces a box, *capping monomorphization depth* so a recursive grammar cannot blow up —
  the box is the natural depth fuse.

**The firing number is not yet measured.** The owed datum: `.rlib`/`dist` **byte-size** and
**incremental-compile-time** deltas as a function of fuse-arity, on **CSS** (arity-fan-out
stress: many-alternative property grammars) *and* **value.js** (projection-fan-out stress: 59
`all()`), capped vs uncapped, against a named per-tranche size budget tripwired in CI. JSON
alone understates both stresses. No pass has produced this curve — it is the family's second
missing measurement (after the Q7-conditioned runtime fork).

---

## Q5 — Serializable-plan observable (one plan, two lowerings, byte-identical)

**The plan is serializable today, modulo the host-closure body.** `GrammarIR`
(`crates/ir/src/types/node.rs`), `FnDescriptor` (`#[derive(Serialize,Deserialize)]`,
`fn_descriptor.rs:17`), and `MapExpr` all serialize; the combinator node set is a near-bijection
with `IrNode`. The distinguishing observable: **emit one serializable `GrammarIR`, drive both
the Rust emitter (`crates/core/src/backend/emitter.rs` → monomorphized structs/enum) and a TS
emitter from it, and assert byte-identical parses over a shared corpus.** If a single plan
round-trips and reconstructs an identical fused region on both sides, 4B is the *Rust lowering
of a crossing terminal* (one plan, two lowerings) — not a standalone type-system artifact. The
only non-serializable residue is an arbitrary `HostClosure` *body*; it crosses as a `HostFnId`
reference resolved per-backend (§Q2), so **the plan is serializable modulo the host-fn
lattice**. This aligns 4B with the settled hand-forward: `bbnf_ir::IrNode` is a live
serializable 14-variant near-bijection carrying fusion passes; 4B is its *monomorphizing*
lowering, the VM interpreter its *interpreting* one — two distinct mechanisms over one
serializable plan, exactly the shape the lock asks 4B to demonstrate rather than assert. The
prototype's plan artifact + dual-parse oracle is the deciding evidence.

---

## Q7 — Setup vs value-plane vs recognizer cost (the setup tax, measured shape)

`Interpreter::new` (`crates/ir/src/vm/interpreter/mod.rs:114`) allocates, **per parse, before a
single byte is read**: seven `Vec::with_capacity` — `values`(64), `call_stack`(32),
`checkpoints`(32), `repeats`(16), `memo_starts`(8), `value_depth_stack`(8), `rule_stack`(16) =
**176 pre-reserved slots across 7 heap arrays** — plus `diagnostics: Vec::new()` and
`memo: FxHashMap::default()`. That is **8 Vecs + 1 FxHashMap = 9 allocations of pure setup**.
On short inputs (value.js parses ~10–40-byte CSS values) this setup *dominates* single-parse
cost; the recognizer touches only a few bytes while nine arrays are provisioned.

**Pooling that eliminates it.** Hoist the `Interpreter` into a reusable pool and reset with
`clear()`-not-drop: `Vec::clear` retains capacity, `FxHashMap::clear` retains buckets, so after
one warmup parse **per-parse setup allocs → 0**. The amortization curve: setup is 100% of a
1-parse cost on a tiny input, falls as `1/N` under a pooled interpreter reused across N parses,
asymptoting to zero. Clean three-way split the prototype must report: **recognizer cost** (byte
scan + spine dispatch) / **value-plane cost** (`Value` construction + `MapExpr` evaluation) /
**setup cost** (the 9 allocs). **Ordering law for 4B:** the fusion delta is measurable *only
after setup is pooled* — otherwise the 9-alloc setup noise swamps the spine-dispatch delta the
family is trying to isolate, exactly the confound that made the refuted `portable_simd`→NEON
"win" evaporate under fresh paired windows.

---

## Q8 — Predecessor shapes (report only, as inputs to 4B)

- **LOCK-5 (regex continuum).** XL-C has *retired the regex functor*: two domain-fit types — an
  opaque `Regex(StringId)` grammar tier and a rich `Hir` engine tier — bridged cheaply; the
  shared-IR question no longer hinges on ONE-HIR. **Input to 4B:** regex leaves in the fused
  spine are opaque `StringId`/scanner references (the SIMD kernels `decode.rs`,
  `structural_bitmap.rs`, `number_simd.rs`, `quote_parity.rs`). 4B fuses *around* them — the
  regex tier is a leaf substrate 4B *calls*, never a node it monomorphizes. Open sub-question
  for LOCK-5, not resolved here: whether the shared part is the e-graph rewrite library only, or
  the HIR type itself.
- **LOCK-7 (host-fn lattice).** The typed host-fn interface bounding grammar totality by
  declared signatures. **Input to 4B:** the proposed `HostClosure{fn_id,...}` descriptor (§Q2)
  resolves `fn_id` into LOCK-7's lattice; 4B *consumes* the signature contract and cannot define
  the lattice. Open sub-question for LOCK-7: the named totality floor per subsystem (what
  fraction of `->` sites may stay opaque host closures) — latex's ~23% (§Q3) is a data point
  that floor must accommodate or explicitly exclude.

---

## Substrate & seams (path:line, verified)

- `rust/parse_that/src/parse.rs:61,74,80` — boxed core (`SmallBox<dyn ParserFn>`,
  per-construction box, vtable `call`) — the dispatch 4B erases.
- `rust/parse_that/src/span_parser/mod.rs:51,58,88,131,182` — `SpanParser`/`SpanKind`
  enum-dispatch (M1 in-tree, vtable-free), flat `Seq`/`OneOf` `:88`, `Boxed` escape `:131`,
  `call_inner` match `:182`.
- `rust/parse_that/src/combinators/macros.rs:6,83` — `seq!`/`alt!` arity 2–8 flat fusion (M2).
- `rust/parse_that/src/lazy.rs:21` — recursion boundary that MUST stay boxed (depth cap).
- `typescript/src/parse/leaf.ts:39,100,179` — TS build-emitter precedents: `any` arity-2,
  `dispatch` LUT, `fuseAll` arity 2/3/general — the manual monomorphizations 4B-TS automates.
- `typescript/src/parse/parser.ts:53,82,88,131,137` — the `.parser(state)` megamorphic IC to
  erase inside emitted fused regions.
- `crates/ir/src/vm/interpreter/mod.rs:114` — `Interpreter::new` setup tax (Q7).
- `crates/ir/src/types/fn_descriptor.rs:18` — six variants, no `HostClosure` (Q2 gap).
- `crates/ir/src/types/map_expr.rs:17,31` — `MapExpr` grammar (projection ceiling); `FnCall`.
- `crates/core/src/backend/emitter.rs`, `backend/mod.rs` — the comptime Rust/TS emitter (Q1
  monomorphizing machine; Q5 one-of-two-lowerings).
- Scratch-copy law (R3 §8): rsync-exclude `target/.git/node_modules`, repoint the 7 external
  sibling patch paths, isolated `CARGO_TARGET_DIR`, nightly-2026-04-11 pin at repo root.

## Prior art (citations)

- **winnow / nom** — concrete-type combinators → monomorphization → inlining; `Rc/Box<dyn Fn>`
  measured **3–39× slower**; GATs forgone for build-time. The M3 Rust idiom and the box-cost
  datum. [Winnow 0.5](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/) ·
  [winnow combinator docs](https://docs.rs/winnow/latest/winnow/combinator/index.html)
- **Staged parser combinators** (Jonnalagedda et al., OOPSLA'14; LMS) — staging dissociates
  composition from execution; First-Futamura framing; ~25× over non-staged.
  [Staged Parser Combinators](https://manojo.github.io/2015/09/02/staged-parser-combinators)
- **flap** (Krishnaswami & Yallop) — staged typed combinators emitting deterministic token-free
  code, **2–6× ocamlyacc**, ~50% over hand-written recursive descent; the Q5 anchor for a staged
  terminal that crosses representations. [flap PDF](https://www.cl.cam.ac.uk/~nk480/flap.pdf)
- **In-tree precedents (unmeasured):** `span_parser` enum-dispatch, `seq!`/`alt!` macros, TS
  `fuseAll`/`any`/`dispatch` — the family's own evidence, never benched head-to-head against the
  boxed closure core. The refuted `portable_simd`→NEON shim is the paired-window discipline law;
  the banked lazy-diagnostic alloc lever (+13–15% from removing two ctor arrays) is the
  alloc/dispatch-conflation confound Q4/Q7 must exclude.

## Named risks & counterexamples

1. **Leaf-dominance nullifies the spine win.** On scan-heavy corpora the erased `blr` is noise;
   JSON-only overstates the family. *Guard:* mandatory projection-dense arm; report the shrink,
   not a headline; name the negative result if closures tie on scan-dominated shapes.
2. **Setup swamps the delta (Q7).** Unpooled `Interpreter::new` (9 allocs) buries the
   dispatch-elimination signal. *Guard:* pool setup FIRST, then measure fusion.
3. **Alloc/dispatch conflation.** A fusion number that also removes boxes cannot attribute the
   delta to dispatch-elimination (banked +13–15% is pure alloc). *Guard:* alloc-neutral pairing
   (same box count both arms) + disasm/IC probe isolating indirect-call count.
4. **Monomorphization blowup (Q4).** Deep generic `Seq` nesting explodes compile time / binary
   size on CSS/value.js. *Guard:* the pluggable arity(≤8)/fanout(≤4)/depth caps; enum (M1) for
   the generated surface; measured `.rlib`/`dist` + incremental-compile budget in CI.
5. **Opaque host closures cannot monomorphize (Q2/Q3).** latex's ~23% barrier forces boxes;
   over-eager fusion is unsound/uncompilable. *Guard:* `HostClosure{fn_id}` descriptor + a box
   at every named boundary; report the per-corpus barrier ratio.
6. **Two-arm window asymmetry.** A cross-run "win" that is load drift. *Guard:* sample-
   interleaved A,B,A,B, N≥200, two load windows, both arm orders, median + dispersion.

## What a prototype must test to move family 4B

1. **Pool the interpreter, then fork the runtime (the Q7-gated datum).** Pool `Interpreter::new`
   (clear-not-drop); split cost into recognizer / value-plane / setup; then measure `{boxed
   closure core (parse.rs), enum-dispatch (span_parser), monomorphized generic-struct (new M3
   arm)}` on one projection-dense + one scan-dominated grammar, **same SIMD leaf substrate on
   every arm**, paired-cold N≥200, two windows, both orders.
2. **Disasm gate (Rust).** `otool -tv` the M3 fused region: the vtable `blr` provably absent;
   spine lowers to `bl`/inlined leaf calls; leaf kernels byte-identical to baseline.
3. **First TS cross-language number.** Build-time emitter specializes one fused region (a
   value.js `all()` site) to lexical-member JS; paired in-process vs the `.parser`-IC path;
   prove no per-parser indirection; isolate dispatch- from allocation-elimination.
4. **Fusion-policy firing curve (Q4).** `.rlib`/`dist` size + incremental-compile deltas vs
   fuse-arity on CSS + value.js, capped vs uncapped, against a named CI budget.
5. **Crossing-plan oracle (Q5 decider).** Emit the serializable `GrammarIR`; drive both the Rust
   emitter and a TS emitter from it; assert byte-identical parses over a shared corpus. Working
   plan ⇒ 4B is the Rust lowering of a crossing terminal; impossibility ⇒ standalone.
6. **Host-closure descriptor (Q2 enabler).** Add `FnDescriptor::HostClosure{fn_id,arg_types,
   ret_type}`; round-trip-serialize a latex block-body `.map` as a `HostFnId` reference; prove
   the plan survives crossing modulo the host-fn lattice.

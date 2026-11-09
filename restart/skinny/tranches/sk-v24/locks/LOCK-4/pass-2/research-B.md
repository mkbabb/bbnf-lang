# LOCK-4 · Family 4B — COMPTIME-FUSION — research (pass 2)

**Thesis.** The combinator algebra stays hand-written and lawful, but its *compilation*
is a staging step: at grammar-construction time the combinator tree is specialized into a
monomorphic, vtable-free region so that the per-combinator indirect call and the
per-combinator heap box both vanish, and LLVM (Rust) or the JIT (V8, via a build-time
emitter) inlines the structural spine end-to-end down to the leaf kernels. No runtime graph
survives; the specialization *is* the runtime. This is partial evaluation of the parser
with respect to the grammar — the First Futamura projection — and its two most decisive
precedents already live inside parse-that, unmeasured against the closure core.

---

## 1. The mechanism made concrete

Family 4B has one idea (specialize the tree at build time, erase the spine dispatch) with
three lawful realizations. Two are *in-tree today* and were never benched head-to-head
against the boxed closure core; the third is the generalization the family proposes.

### M1 — enum-dispatch spine (in-tree: the `span_parser` tier)

`span_parser/mod.rs:51` declares `SpanParser { kind: SpanKind, flags: u8 }`; `SpanKind`
(`mod.rs:58`) is a 30-arm enum whose leaves carry no inner parser and no box
(`StringLiteral(&[u8])`, `TakeWhileByte(fn(u8)->bool)`, `TakeUntilAny1/2/3`,
`TakeUntilAnyLut(Box<[bool;256]>)`, `TakeUntilAnySIMD{lo_lut,hi_lut}`, `CompiledDfa`,
`AhoCorasickMatch`) and whose combinators are *flat* (`Seq(Vec<SpanParser>)`,
`OneOf(Vec<SpanParser>)`, `Many{inner,lo,hi}`, `Wrap{left,inner,right}`). Dispatch is a
single `match &self.kind` in `call_inner` (`mod.rs:182`), reached through an
`#[inline(always)]` `call` with a `flags==0` fast path (`mod.rs:137`) and an
`#[inline(never)]` cold path (`mod.rs:154`). The vtable indirect call (`SmallBox<dyn
ParserFn>` in the value core) is replaced by a compiler-lowered jump table over a closed
variant set. `SpanKind::Boxed` (`mod.rs:131`) is the single escape hatch — the only place a
`dyn ParserFn` survives — bounding the box-count of an entire grammar to the count of
genuinely dynamic (recursive / host-closure) nodes. This is 4B's enum realization already
running for the span-only hot path; it has *no published paired number vs the closure core*
— that is the family's first missing datum.

### M2 — flat comptime fusion (in-tree: the `seq!`/`alt!` macros)

`combinators/macros.rs` fuses an N-ary `seq!`/`alt!` into **one** `Parser::new(move
|state| …)` closure (arity 2–8 unrolled; `seq!` at `:6`, `alt!` at `:83`), collapsing the
N−1 intermediate boxes a chained `.then().then()` would allocate into a single box whose
body is straight-line `?`-propagated leaf calls with the backtrack checkpoint
(`state.offset = cp`) inlined per alternative. This is monomorphization by macro: the fused
region is a concrete closure the compiler sees whole. It proves the *shape* of the win
(one box, inlined spine) but keeps a box at the fused root and caps at arity 8.

### M3 — generic-struct monomorphization (the proposed generalization; winnow's shape)

Replace `Parser<'a,O>` = `SmallBox<dyn ParserFn>` (`parse.rs:61`) on the fusable spine with
concrete generic combinator structs — `Seq<A,B>(A,B)`, `Alt<A,B>(A,B)`,
`Map<P,F>(P,F)` — each `impl Parser` with an `#[inline]` `parse(&self, state)` that calls
`self.0.parse(...)` on a *statically known* type. There is no box and no indirect call
anywhere on the fused spine; `Seq<Str, Seq<Ws, Num>>` monomorphizes to one function LLVM
inlines to a flat sequence of leaf-kernel calls. This is exactly winnow's design (concrete
`Zip`-style types → monomorphization → inlining → LLVM end-to-end), and winnow's own
history is the load-bearing prior number: the `Rc<dyn Fn>`/`Box<dyn Fn>` formulation that
parse-that's value core still uses benchmarks **3–39× slower** than the monomorphized
concrete-type formulation ([epage, *Winnow 0.5*](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/)).
The box survives only at named dynamic boundaries (recursion via `lazy`, host closures),
identical to `SpanKind::Boxed`.

M1 and M3 are the same erasure by two means: M1 closes the variant set and pays one
predictable jump; M3 opens it to arbitrary user types and pays *zero* dispatch at the cost
of monomorphization blowup. M2 is the special case both subsume. The family's strongest
form picks M1 for the closed generated-grammar surface (a generator emits a fixed variant
set — enum is ideal, no blowup) and offers M3 for the open hand-written combinator API
(user types demand generics). The public `.then()`/`.or()` API is preserved either way: the
methods build the specialized node instead of boxing a closure.

### The TS face — a *different* mechanism over the same plan

V8 has **no compile-time monomorphization**: there is no analog of LLVM instantiating
`Seq<A,B>`. The lawful analog is a **build-time emitter** that generates specialized JS
source for a fused region — the arity-unroll already shipping in `leaf.ts`: `fuseAll`
(`leaf.ts:179`) hand-writes an arity-2 arm (`:183-212`) and arity-3 arm (`:214-251`) with
classic indexed `for` (no `for…of` iterator object), falling to a general arm (`:255-272`);
`any` unrolls arity-2 (`leaf.ts:39-57`); `dispatch` is an O(1) `Int8Array(128)` first-char
table (`leaf.ts:100-151`). These are *manual* monomorphizations of exactly the regions a
generator would emit. 4B-TS makes that emission automatic and grammar-driven: the same
build-time plan that picks the Rust variant emits the specialized JS closure, eliminating
the megamorphic `this.parser(state)` IC (`parser.ts:53,82,88,131,137`; `leaf.ts:44,49,62,140`)
inside the fused region — the region calls its members by *lexical name*, not through the
polymorphic `.parser` function-pointer field.

---

## 2. The directive questions, answered for 4B

**Q1 — does the monomorphization win survive when production leaf kernels run on both
arms?** This is the family's central risk and its honest answer is *partial, by grammar
shape*. R3's settled fact — "`Map`/value-plane `Box<dyn Fn>` indirection confines to scalar
leaves; the structural spine inlines" — plus the SIMD leaf census (JSON string decode
`decode.rs` 17 hits, `structural_bitmap.rs`, `number_simd.rs`, `quote_parity.rs`) means the
leaf kernels dominate the cycle budget on structure-light, scan-heavy corpora (twitter,
canada). When a leaf scans hundreds of bytes per node, erasing one `blr` per node is noise.
The win concentrates where the **spine-to-leaf ratio is high**: projection-dense grammars
(`all()`-of-many-tiny-mapped-parts, CSS declarations, value.js's 59 `all()` sites), where
each node does O(1) leaf work and the dispatch/box overhead is a large fraction. The
prototype must therefore report the *shrink*, not a headline: end-to-end paired-cold vs the
shipped tuned parser, on a projection-dense grammar **and** a scan-dominated grammar, with
the **same leaf substrate on both arms** (the enum/generic arm calls the identical SIMD
scanners), isolating dispatch-elimination from leaf time. The refutation law from the
`portable_simd`→NEON shim is the governing precedent: a 1.34–1.57× "win" evaporated to
neutral-to-slower under fresh paired windows because of two-arm window asymmetry — 4B's
number is worthless unless sample-interleaved A,B,A,B, N≥200, two load windows, both arm
orders.

**Q2 — V8 analog?** No direct analog; the TS face is a *different* build-time-emitter
mechanism over a shared plan (§1, TS face). Prior art: winnow/nom's concrete-type
monomorphization is the Rust idiom
([winnow combinator docs](https://docs.rs/winnow/latest/winnow/combinator/index.html); the
`Rc<dyn Fn>` 3–39× slowdown number). The first TS paired number must prove the emitted
region carries **no per-parser indirection** — that the fused JS calls members lexically,
the `.parser` megamorphic IC gone from the region — and must isolate *dispatch-elimination*
(fewer indirect calls) from *allocation-elimination* (the PROVE-11 lazy-diagnostic lever
already banked then-chain +13.6% / fused-all +15.6% purely from removing two ctor array
allocs, so an alloc-conflated fusion number is not attributable to fusion).

**Q3 — fusion policy bounding generated-size / monomorphization blowup.** This is 4B's
non-negotiable governor and the reason M1 (enum) is preferred for the generated surface.
Rust generic monomorphization of deep `Seq<A,Seq<B,Seq<C,…>>>` explodes type count and
compile time — winnow deliberately forgoes GATs partly for build-time
([winnow 0.5](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/)).
The policy must be *pluggable, not a hardcoded branch* (per the campaign's pluggable-decision
edict): a cost/size model choosing per-site among {inline-monomorphize, enum-node,
box-here}, bounded by (a) **fuse arity cap** — the macros already stop at 8; beyond it fall
to the flat `Seq(Vec)` enum arm (`span_parser` `mod.rs:88`) which is O(1) in generated size;
(b) **dispatch fan-out cap** — a wide `alt!`/`OneOf` past a threshold lowers to the
`dispatch` first-char LUT (`leaf.ts:100`) not an N-way unroll; (c) **boxing depth-cap** — a
recursion/`lazy` edge or host-closure forces a box (`SpanKind::Boxed`), capping
monomorphization depth so a recursive grammar cannot blow up. Evidence owed: `.rlib`/`dist`
size **and** incremental-compile deltas on **CSS and value.js**, not JSON alone — CSS is the
arity-fan-out stress (many-alternative property grammars), value.js the projection-fan-out
stress (59 `all()`). A named budget per the generated-size-budget precept, tripwired in CI.

**Q4 — standalone terminal, or the Rust lowering of a representation-crossing terminal?**
The strongest formulation is the **latter**: comptime-fusion is staging, and the stage-0
object is a *plan* — a serializable description of the fused region — of which the Rust
monomorphization and the TS emitter are two lowerings. This is precisely the staged-parser-
combinator result: staging dissociates parser *composition* from parser *execution*,
generating specialized code that runs ~50% faster than hand-written recursive descent and
>25× faster than the non-staged combinators (flap: token-free specialized code 2–6× ocamlyacc;
Jonnalagedda/Rompf/Odersky OOPSLA'14, LMS = First Futamura projection —
[flap](https://www.cl.cam.ac.uk/~nk480/flap.pdf);
[Staged Parser Combinators](https://manojo.github.io/2015/09/02/staged-parser-combinators)).
The observable that distinguishes "standalone terminal" from "Rust lowering of a crossing
terminal": **is the fused region's structure derivable from a language-neutral plan that also
drives the TS emitter, or is it intrinsic to the Rust type system and uncrossable?** If a
single plan emits both the Rust variant selection and the TS specialized closure and they
produce byte-identical parses over a shared corpus, 4B is the Rust face of a crossing
terminal (and converges with the shared-IR ownership question); if the Rust monomorphization
depends on type-system facts with no serializable image, it is standalone. The prototype's
plan artifact is the deciding evidence — and it is directly buildable, because the combinator
node set is a **verified near-bijection with `bbnf_ir::IrNode` (14 variants)**, `parse_with_ir`
is already live (`interpreter/mod.rs:373`), and `bbnf_ir` already ships a value-producing
bytecode VM (`vm/{bytecode,compiler,interpreter}`) and a MessagePack bus (`lib.rs`). The
staged plan is that IR; 4B is its *monomorphizing* lowering rather than its *interpreting* one.

---

## 3. Substrate & integration seams (path:line)

parse-that (READ-ONLY; prototype on a scratch copy per R3 §8 — rsync-exclude
`target/.git/node_modules`, repoint the 7 external sibling patch paths, isolated
`CARGO_TARGET_DIR`, nightly-2026-04-11 pin at repo root):

- `rust/parse_that/src/parse.rs:61` — `Parser<'a,O>{ parser_fn: SmallBox<dyn ParserFn+'a,S32>,
  flags:u8 }` (the boxed core 4B specializes away); `:41` `ParserFn` trait; `:45` blanket
  `Fn` impl; `:74` `SmallBox::new` per-construction box (the alloc erased); `:80` `call`
  `flags==0` fast path; `:97` `call_with_flags_cold`.
- `rust/parse_that/src/combinators/macros.rs` — `seq!` `:6`, `alt!` `:83` (in-tree flat
  fusion, arity 2–8; the M2 precedent).
- `rust/parse_that/src/span_parser/mod.rs:51,58,137,154,182` — `SpanParser`, `SpanKind`
  enum, `call`, cold path, `call_inner` match (the M1 precedent, vtable-free); flat
  combinators `Seq`/`OneOf` `:88-89`, fused `SepByWs` `:103` (a fusion of sep_by + ws-trim —
  proof the family already fuses *semantic* pairs, not just structure); escape hatch
  `SpanKind::Boxed` `:131`.
- `rust/parse_that/src/lazy.rs:21,31` — `Box<dyn LazyParserFn>` (the recursion boundary that
  *must* stay boxed — 4B's depth-cap anchor).
- `rust/parse_that/typescript/src/parse/leaf.ts:179` `fuseAll` (arity-2 `:183`, arity-3
  `:214`, general `:255`); `:39-57` `any` arity-2 unroll; `:100-151` `dispatch` LUT — the TS
  build-emitter precedents.
- `rust/parse_that/typescript/src/parse/parser.ts` — the `.parser(state)` megamorphic IC to
  erase inside fused regions (`:53,82,88,131,137`; `leaf.ts:44,49,62,140`).

bbnf-lang side (the crossing-plan substrate, editable in-campaign):
- combinator↔`bbnf_ir::IrNode` near-bijection (14 variants); `parse_with_ir` live at
  `crates/*/interpreter/mod.rs:373`; `bbnf_ir` VM at `vm/{bytecode,compiler,interpreter}`;
  MessagePack serialization at `lib.rs`. `FnDescriptor` carries **no host-closure variant** —
  the code-verified vocabulary gap 4B must close for host-closure leaves to be plannable
  (they otherwise force `SpanKind::Boxed` / `dyn Fn`).

---

## 4. Prior art (citations)

- **winnow / nom** — concrete-type combinators (`impl Parser`, tuple sequencing) →
  monomorphization → inlining; `Rc/Box<dyn Fn>` measured **3–39× slower**; GATs forgone for
  build-time. The direct Rust idiom for M3 and the box-cost datum.
  [Winnow 0.5](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/) ·
  [winnow combinator docs](https://docs.rs/winnow/latest/winnow/combinator/index.html)
- **Staged parser combinators** (Jonnalagedda, Coppey, Stucki, Rompf, Odersky, OOPSLA'14;
  Béguet/Jonnalagedda) — staging (LMS) dissociates composition from execution, eliminates
  intermediate structures; the First-Futamura framing; ~25× over non-staged.
  [Staged Parser Combinators](https://manojo.github.io/2015/09/02/staged-parser-combinators)
- **flap** (Krishnaswami & Yallop) — staged, typed combinators generating deterministic,
  token-free specialized code, **2–6× ocamlyacc**, ~50% faster than hand-written recursive
  descent. The Q4 anchor: staged combinators as a *terminal* that crosses representations.
  [flap PDF](https://www.cl.cam.ac.uk/~nk480/flap.pdf)
- **In-tree precedent** — `span_parser` (enum-dispatch) + `seq!`/`alt!` + TS `fuseAll`/`any`
  arity-unroll are the family's own unmeasured evidence; PROVE-11 (alloc lever) is the
  cautionary confound (fusion numbers must exclude allocation deltas); PROVE-09's refuted
  shim is the paired-window discipline law.

---

## 5. Named risks & counterexamples

1. **Leaf-dominance nullifies the spine win (Q1).** On scan-heavy corpora the erased `blr`
   is noise; a JSON-only bench overstates the family. *Guard:* mandatory projection-dense
   arm; report the shrink honestly; name the negative result if closures tie on the
   scan-dominated shape.
2. **Two-arm window asymmetry (the PROVE-09 trap).** A cross-run "win" that is load drift.
   *Guard:* sample-interleaved A,B,A,B, N≥200, two windows, both arm orders, median + dispersion.
3. **Alloc/dispatch conflation (Q2).** PROVE-11 shows removing two ctor arrays alone buys
   +13–15%; a fusion number that also removes boxes cannot attribute the delta to
   dispatch-elimination. *Guard:* an alloc-neutral pairing (both arms same box count) plus a
   disasm/IC probe isolating the indirect-call count.
4. **Monomorphization blowup (Q3).** Deep generic `Seq` nesting explodes compile time and
   binary size on CSS/value.js. *Guard:* the pluggable arity/fan-out/depth policy; enum (M1)
   for the generated surface; measured `.rlib`/`dist` + incremental-compile budget in CI.
5. **Recursion/host-closure cannot monomorphize.** `lazy.rs` boxes and `FnDescriptor`'s
   missing host-closure variant force a `dyn` boundary; over-eager fusion is unsound or
   uncompilable. *Guard:* `SpanKind::Boxed`-style depth-cap; close the `FnDescriptor` gap
   before claiming a host-closure leaf is plannable.
6. **Rust↔TS mechanism divergence (Q4).** If the Rust monomorphization has no serializable
   image, the "shared plan" is a fiction and 4B is two unrelated engines. *Guard:* the plan
   artifact + byte-identical dual-parse oracle is the deciding evidence, not an assertion.

---

## 6. What a prototype must test to move family 4B

1. **Common-baseline runtime fork (the under-determined datum).** One projection-dense
   grammar + one scan-dominated grammar, **same SIMD leaf substrate** on every arm; measure
   {boxed closure core (`parse.rs`), enum-dispatch (`span_parser`), monomorphized generic-struct
   (new M3 arm)} paired-cold, N≥200, two windows, both orders. Deliverable: the *measured*
   runtime fork with the per-shape shrink, not an inferred one.
2. **Disasm gate (Rust).** `otool -tv` the fused region on the M3 arm: the vtable indirect
   call (`blr`) is provably **absent**; the spine lowers to direct `bl`/inlined leaf calls.
   Byte-count the reduction; confirm leaf kernels identical to baseline.
3. **First cross-language number (TS, high-leverage — currently unbuilt).** A build-time
   emitter specializes one fused region (e.g. a value.js `all()` site) to lexical-member JS;
   paired-in-one-process vs the general `.parser`-IC closure path; prove no per-parser
   indirection in the region and isolate dispatch- from allocation-elimination.
4. **Fusion-policy budget.** On CSS + value.js: generated-size and incremental-compile deltas
   under the arity/fan-out/depth caps vs uncapped, against a named budget.
5. **The crossing-plan oracle (Q4 decider).** Emit the serializable stage-0 plan from
   `bbnf_ir`; drive both the Rust variant selection and the TS specialized closure from it;
   assert byte-identical parses over a shared corpus. Presence of a working plan ⇒ 4B is the
   Rust lowering of a crossing terminal; its impossibility ⇒ standalone.

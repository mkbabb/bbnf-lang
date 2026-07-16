# LOCK-4 / family 4B — COMPTIME-FUSION (research, pass 1)

*Rust: monomorphized fusion chains, ownership redesigned so the vtable/SmallBox question
dissolves. TS: a build-time fuser emits specialized chains (the arity-unroll generalized);
no runtime graph at all.*

---

## 1. The center of this family (what makes it 4B, not its siblings)

4B is **partial evaluation / staging of combinator *composition*.** The combinators stay
lawful hand-written primitives (constitution i); what changes is that their *composition* is
specialized away before the parse loop runs. A static sub-grammar — a nesting of
`then`/`or`/`map`/`opt`/`many`/`sep_by` with no recursion crossing it — collapses to ONE
straight-line, monomorphic parse routine. There is no combinator object graph walked at
runtime (that separates 4B from 4A CLOSURE-TO-GRAPH), no bytecode interpreter (4C VM-CORE),
and combinators are not re-expressed as sugar over the grammar HIR (4D GRAMMAR-SUGAR). The
fusion is invisible at runtime: comptime in Rust (the type system + LLVM inliner do it),
build-time in TS (an explicit fuser emits the specialized source).

The mechanism already exists in parse-that as two hand-written *proofs of concept that fusion
works*, each fusing only a fixed flat shape:

- **Rust `seq!`/`alt!`** (`rust/parse_that/src/combinators/macros.rs:1-302`) collapse an
  N-ary flat sequence/alternation's *N−1 intermediate `SmallBox`es into ONE*
  `Parser::new(move |state| …)` — the whole chain becomes a single closure that threads
  `state` by position, inlining every `$p.call(state)?` and folding the
  `furthest_offset`/offset-restore bookkeeping in line.
- **TS `fuseAll`** (`typescript/src/parse/leaf.ts:154-273`) fully unrolls arity-2
  (`:183-212`) and arity-3 (`:214-251`) `all(...)` into positional closures — "V8 sees a
  monomorphic call site with constant-folded parser bindings" — and builds exactly ONE flat
  result array instead of the N−1 nested 2-tuples the unfused `a.then(b).then(c)` allocates.

4B's thesis is: **generalize these two hand-cut specials into one uniform fuser** — arbitrary
nesting (not just flat seq/alt), all combinator shapes (map/opt/many/sep_by, not just the
sequence), any arity — on both sides, over a *shared fusion algebra* so the rust/ts shapes
stay isomorphic (constitution iii).

---

## 2. The Rust side — the vtable/SmallBox dissolution

### 2.1 What the box costs, precisely

`Parser<'a,Output>` is type-erased at *every* node: `parser_fn: SmallBox<dyn ParserFn<'a,
Output>+'a, S32>` (`rust/parse_that/src/parse.rs:61-64`), constructed by
`SmallBox::new(parser_fn)` in `Parser::new` (`:72-77`), invoked through the trait object at
`:83` `self.parser_fn.call(state)` (blanket `Fn` impl `:45-53`). Every combinator edge is
therefore an **indirect call through a vtable** and an S32 inline-or-heap box. The alloc cost
is *construction-time only* — PROVE-09's counting allocator measured constructing
`json_parser()` at **6 allocs / 5960 B** and parsing twitter.json at **5390 allocs** whose
mass is Vec growth in `many`/`sep_by` + the string arena, *not* the vtable boxes. So the box's
runtime cost is **not allocation** (which is why PT-2's arena — 86-site Parser-ownership slab —
was carried as low-reward/refuted, PROVE-09 §Arena, PT-2 §4 W4): it is the **indirect call and
the inlining barrier it erects**. LLVM cannot inline `p1.call → p2.call → …` across a `dyn`
edge, so it cannot constant-fold, fuse branches, or register-allocate across combinator
boundaries. That barrier is 4B's target, and it is an axis PROVE-09/PT-2 never measured (they
measured the SIMD shim and allocation). **4B's win, if any, is inlining, and it is currently
unproven.**

### 2.2 The redesign: generic-struct combinator algebra with a `.boxed()` recursion seam

Replace "erase every node" with "erase only at recursion." Each combinator becomes a generic
ADT struct carrying its children by *concrete type*, implementing a `Parser` trait with an
associated `Output`:

```
trait Parser<'a> { type Output; fn parse(&self, s:&mut State<'a>) -> Option<Self::Output>; }
struct Then<A,B>(A,B);   struct Or<A,B>(A,B);   struct Map<A,F>(A,F);
struct Many<A>(A);       struct Opt<A>(A);      struct SepBy<A,S>(A,S);
```

Composition is now *type-level* — `then(a,b): Then<A,B>` is a distinct monomorphized type per
site — and `#[inline]` on every `parse` lets LLVM fold an entire static chain into one
function with **no indirect calls, no box**. The "ownership question dissolves" because the
parser is a value (a nested struct) sized at compile time; there is nothing on the heap to own.

This cannot be total: a parser that references itself is an *infinite type*, and a
`Vec<Parser>` of differing shapes needs one type. So the honest form is a **hybrid**, exactly
the chumsky/combine design: monomorphize the fusion chains, and insert ONE `dyn` erasure only
at recursion points via a `.boxed()` node (`struct Boxed<'a,O>(Box<dyn Parser<'a,Output=O>+'a>)`).
parse-that already has the natural seam: `lazy.rs:20-45` boxes `Box<dyn LazyParserFn>` **once
per `lazy()`** and *caches the constructed `Parser`* (`cached_parser: Option<Parser>`,
`get()` :35-45) — recursion is the only place a box survives, and packrat memoization needs a
boundary there anyway. The `dyn` count drops from *per-combinator* to *per-recursion-cycle*.

### 2.3 The in-tree control: `span_parser/` already did this once

parse-that ships a **second, vtable-free tier**: `SpanParser<'a>` dispatched by an *enum*
`SpanKind<'a>` (`rust/parse_that/src/span_parser/mod.rs:51,58`) — "enum-dispatched,
zero-boxing for Span hot path" — with the full method surface mirrored
(`span_parser/{leaves,constructors,methods,combinators,sep_by,wrap,scan,span_scanner,
assertions}.rs`). This is a *closed-world enum* realization of the same anti-vtable thesis (a
`match` on `SpanKind` instead of a `blr`), and it is the **immediate empirical control** for
the family: 4B's Rust prototype must measure the generic-struct algebra *against both* the
SmallBox core and this existing enum tier, and decide whether 4B is "generic structs replace
SmallBox on the main path" or "promote the span-parser enum-dispatch to the main path." Two
sub-routes inside the family; the enum route trades monomorphized inlining for a predictable
`match`, the struct route trades a larger monomorphized surface for full fold-through.

### 2.4 Preserving the public API

The public `Parser<'a,Output>` single-type surface is load-bearing for consumers (via the
regex generated parser and the combinator core). 4B keeps it as a **`.boxed()` facade over a
monomorphized interior**: user-visible `Parser<'a,O>` is `Boxed` (the erased root), the
interior chain is monomorphized and folded. This is parsimony-bounded and non-breaking — the
surface type is unchanged, only the *interior representation* between recursion points is
specialized. (The alternative — exposing `impl Parser` generic types outward — breaks every
signature and is rejected on the consumer-gate/parsimony grounds.)

---

## 3. The TS side — the build-time fuser, no runtime graph

### 3.1 What the graph costs, precisely

The TS core is the *dual* problem: every combinator dispatches through the `.parser`
function-pointer field — `this.parser(state)` / `p.parser(state)` at
`typescript/src/parse/parser.ts:53,82,88,131,137` and `leaf.ts:44,49,62,140` — which R4 §6c
names "THE megamorphic IC of the engine; V8 sees many distinct closure shapes flowing through
one indirect call." Nested `Parser` objects each re-dispatch; the runtime graph *is* the
megamorphism. `mapState` even builds an `Object.create(state)` prototype-chain view per call
(`parser.ts:177-180`, a documented deopt risk). The build-time fuser dissolves the graph into
straight-line monomorphic JS: one specialized closure per static sub-grammar, no
`Parser`-object traversal, no megamorphic edge.

### 3.2 The mechanism: generalize the arity-unroll to a build-time codegen pass

`fuseAll` proves the shape at fixed arity (leaf.ts:183-251) — the fuser generalizes it to
(a) any arity and (b) any nesting including `map`/`opt`/`sep_by`. The lawful realization is a
**build-time source transform** (a `vite`/`ts` transform plugin at
`typescript/vite.config.ts`; `package.json` `sideEffects:false` already aids tree-shaking):
recognize the statically-analyzable combinator call-sites (`all(...)`, `any(...)`,
`dispatch(...)` — the arity-unroll's exact domain, plus bounded fluent `.then().map()`
chains over a single binding) and rewrite them to a specialized closure at build time. The
runtime keeps the interpreted `Parser` graph as the *fallback* for what cannot be statically
seen; the fused sites emit no graph. This is staging (partial evaluation): the parser
description is the program, the fuser is the first Futamura projection over it.

**eval / `new Function` is forbidden** — CSP-hostile, V8-deopt-prone, and outside artifact
constraints. The fuser must emit *source* (`.ts`/`.js`), never runtime-compile strings. That
keeps it isomorphic to the Rust comptime story: both emit specialized code ahead of the parse.

### 3.3 The honest ceiling from PROVE-11

The certified measure of the arity-unroll (PROVE-11, VERIFY CONFIRMED_DELTA) is the family's
number-to-beat and its warning: fused-`all` **+15.6%** and then-chain **+13.6%** reproduced,
**fused alloc −60.1% exact** — but **first-char dispatch only ~+5%, the claimed +10.2% halved**
across 24 paired rounds in two load windows. Reading: **the fusion win is allocation-shaped
(one flat array vs N nested tuples), not dispatch-shaped.** Eliminating the megamorphic edge
alone bought ~+5%; eliminating the intermediate tuple allocations bought the double-digits. A
4B TS prototype must *isolate the two axes* — dispatch-elimination vs allocation-elimination —
or it will over-claim the way the dispatch lane did. This is the §0/09 self-deception the
constitution (iv) outlaws.

---

## 4. The shared substrate (constitution iii — rust/ts isomorphism)

The isomorphic artifact is a **fusion algebra**: the closed set of combinator shapes
{`Then`, `Or`, `Map`, `Opt`, `Many`, `SepBy`, `Boxed`} + the fusion rewrite rules (how a
nested shape collapses to one specialized routine; where a `Boxed`/recursion boundary halts
fusion; where packrat memoizes; where diagnostics thread). Rust realizes the algebra as
generic structs — LLVM's monomorphizer+inliner *is* the fuser. TS realizes the *same shape set*
as a build-time codegen pass — the fuser is explicit because V8 has no monomorphizer. The shape
set and the rewrite rules are mirrored (LOCK-6's isomorphism plane); the *realization* is
idiomatic per language (LLVM laws vs V8 laws). This is precisely tagless-final + staging
(Kiselyov): one combinator description, two interpretations — a runtime graph (the current
core) *or* a fused/compiled form (4B) — sharing the algebra.

**Packrat & diagnostics survive fusion** (charter items). Fusion halts at `Boxed`/recursion,
which is exactly where packrat needs to memoize (memoization at recursion cycles, not at every
edge) — so the fused chain memoizes at its boundaries, not internally. Diagnostics thread
through the fused closure the way `seq!`/`alt!` already fold `furthest_offset.max` + offset
restore in line (macros.rs:91-97) — PT-E1 per-parse-scoped diagnostics (PT-3 W2) composes:
the fused closure reads a per-parse scope object, not a module-global.

---

## 5. Prior art (with citations)

- **chumsky 1.0 (Rust)** — the canonical monomorphized-combinator design: a `Parser` trait
  with associated `Output`, zero-cost generic combinators, explicit `.boxed()` for
  recursion/type-erasure. 4B's Rust §2.2 is this design applied to parse-that's surface.
- **nom / winnow (Rust)** — function-combinators returning `impl FnMut`, fully monomorphized,
  no vtable; `alt((...))`/tuple-sequences monomorphized by arity. winnow benchmarks land near
  hand-written — the empirical claim that monomorphized combinators *can* be zero-cost.
- **combine (Rust)** — the original "monomorphize-then-box-at-recursion" via `.boxed()`; the
  hybrid boundary 4B adopts.
- **Jonnalagedda, Béguet, et al., "Staged Parser Combinators for Efficient Data Processing"
  (OOPSLA 2014)** — LMS-staged combinators generating specialized code, ~2× over unstaged.
  THE canonical build-time-fuser prior art; directly the TS §3.2 thesis.
- **Li Haoyi, FastParse (Scala)** — macro-fused parser combinators with *no runtime combinator
  objects*; the arity-unroll-generalized analogue, and the compile-time-blowup cautionary tale.
- **Kiselyov, "Typed Tagless-Final Interpreters" + "Finally Tagless, Partially Evaluated"** —
  one combinator description, multiple interpretations (interpret vs compile/stage); the
  isomorphism substrate of §4.
- **Coutts, Leshchinsky, Stewart, "Stream Fusion" (ICFP 2007)** and GHC's inliner/SpecConstr —
  the general "eliminate intermediate structures by fusion" law fusion embodies; attoparsec
  (CPS + monomorphization) beating parsec is the parser-domain instance.
- **Swierstra & Duponcheel, "Deterministic, Error-Correcting Combinator Parsers" (1996)** —
  analyzable/optimizable combinators; the intellectual root of "the combinator graph is data
  you may optimize."
- **rustc/LLVM monomorphization + `#[inline]` + LTO** — the exact machinery that makes generic
  structs zero-cost, and whose compile-time/binary-size cost §6-R3 budgets.
- **V8 hidden classes / megamorphic ICs** (R4 §6) — the runtime cost the TS fuser dissolves.

---

## 6. Named risks & counterexamples (what could refute 4B)

- **R1 — the predicted-branch trap (§0/09).** The SmallBox indirect call is a *single
  fixed-target* branch the Apple-M OoO core's BTB predicts perfectly on the steady loop;
  inlining across it may buy nothing — the exact "obvious win that didn't reproduce" that
  refuted the PROVE-09 shim (window asymmetry). *Guard:* the prototype paired-measures cold,
  N≥200, two windows; a NEUTRAL result is a valid family outcome (PT-2 W4 arena-refutation
  precedent), not a failure to bury.
- **R2 — recursion forbids totality.** A `dyn`-free claim is false: self-reference is an
  infinite type. The `Boxed` seam is mandatory; "no vtable anywhere" over-claims. *Guard:*
  the hybrid boundary at `lazy` is designed in, not patched in.
- **R3 — comptime/bundle blowup.** Full monomorphization of deep chains explodes rustc
  codegen + binary size (and TS emitted-closure count); FastParse's macro expansion is the
  known cautionary case. The generated-size budget (precept) binds. *Guard:* budget gate in
  the prototype; a `.boxed()` cut-point policy caps monomorphization depth.
- **R4 — TS static-recognition is undecidable in general.** Recognizing arbitrary fluent
  chains across variable bindings/higher-order passing/re-exports cannot be total; the fuser
  handles the recognizable static forms and falls back to the runtime graph. "No runtime graph
  *at all*" is false for dynamic grammars. *Guard:* scope the claim to the fused sites; keep
  the interpreted graph as the sound fallback.
- **R5 — the win is allocation, not dispatch (PROVE-11).** Isolating dispatch-elimination may
  reproduce only ~+5% (the halved dispatch lane), the double-digits being the removed tuple
  allocations. *Guard:* the prototype separates the two axes and carries the *honest* number
  per axis.
- **R6 — API/consumer breakage.** A generic-`impl Parser` outward surface breaks every
  consumer signature. *Guard:* keep `Parser<'a,O>` as the `.boxed()` facade; monomorphize the
  interior only.
- **R7 — the `'a` input-lifetime trap.** The same lifetime that killed the arena (a
  `'static`/thread-local store of `'a`-parameterized parsers is unsound, PROVE-09 §Arena)
  binds the `Boxed` seam: the erased root must be `'a`-scoped, threaded, never smuggled.
  *Guard:* MIRI-clean, no lifetime `transmute`.

---

## 7. Supersessions of the extant baseline (constitution ix — named, not silent)

- **PT-2 §4 W4 (the arena)** targeted the *6 construction allocs* on an alloc axis and was
  carried as low-reward/refuted. 4B declares the SmallBox question dissolves on a **different
  axis — the indirect-call/inlining barrier, not allocation** — which PT-2/PROVE-09 never
  measured. Not a contradiction; an orthogonal, still-unproven axis.
- **PT-3 §4 W4 (the shape audit)** *preserves the arity-unroll monomorphizations by
  measurement*. 4B **generalizes** the arity-unroll into a build-time fuser — a divergence
  from "audit and pin as-is" to "extend into a codegen pass." Declared supersession; PT-3's
  V8-shape oracle becomes 4B's regression floor.
- Both are contingent on the same non-clobber/placement law (parse-that READ-ONLY; landing by
  owner/parse-that-side session via letter BBNF-PT-G) and the same scratch-copy recipe (R3 §8).

---

## 8. What a prototype MUST test to move this family

**Rust prototype (scratch copy per R3 §8; `CARGO_TARGET_DIR` isolated).**
1. Build a minimal generic-struct algebra (`Then`/`Or`/`Map`/`Opt`/`Many` + `Boxed`) with a
   `Parser` trait + associated `Output`, `#[inline]` throughout, and reimplement the **JSON
   grammar** (the `benches/parse_that/combinator.rs` target) both ways: SmallBox core vs
   monomorphized. Include the `span_parser/` enum tier as a third arm (the in-tree control).
2. **Paired, cold, N≥200, two windows** (the §0/09 discipline; divan paired-binary protocol,
   the six JSON corpora `apache-builds/canada/citm_catalog/data/data_xl/twitter`): does the
   monomorphized parser beat SmallBox? Report the honest ratio; expect single-digit % and be
   ready for the NEUTRAL/refutation outcome.
3. **`cargo asm` / `otool -tv`** (inspect-generated-output edict): confirm the indirect `blr`
   vtable calls are gone (folded to direct/inlined) in the monomorphized build — the mechanism
   proof, independent of the timing.
4. **Compile-time + binary-size delta** (the budget gate, R3) and **MIRI-clean** soundness of
   the `Boxed` `'a`-recursion seam (R7).

**TS prototype (scratch copy per PROVE-11 recipe; single build per process).**
5. A build-time fuser (ts-transform / vite plugin) generalizing `fuseAll` beyond arity-3 —
   arity-5+, and a bounded `all().then().map()` chain — emitting specialized source.
6. **The certified PROVE-11 method** (cold per-op median ns, single-build-per-process paired
   interleaving A,B,A,B…, N≥12, packrat state disclosed): does deeper build-time fusion beat
   the runtime graph? **Isolate dispatch-elimination from allocation-elimination** (R5); carry
   the honest per-axis number.
7. **Fallback correctness**: prove the fuser leaves `lazy`/recursion as the runtime graph and
   never mis-fuses a dynamic grammar; bundle-size delta; `tsc` clean; `npm test` parity; the
   three consumer suites (value.js/latex-paper/keyframes.js) as the close gate.

The family advances when (2)+(3) show a *real, inlining-attributable* Rust win (or a filed
refutation), and (6)+(7) show build-time fusion beating the runtime graph on an axis PROVE-11
did not already own — both under the constitution's paired-cold discipline, with the honest
number carried, never the cherry-picked window.

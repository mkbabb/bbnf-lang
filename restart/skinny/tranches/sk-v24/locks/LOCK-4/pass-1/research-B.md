# LOCK-4 / family 4B — COMPTIME-FUSION: the combinator tree specialized to straight-line code

*The combinator algebra stays a hand-written primitive set, but its **composition** is not
run at parse time. A combinator expression is a static tree known before the first byte; 4B
compiles that tree — Rust by monomorphization, TS by a build-time fuser — into specialized,
inlined, allocation-free code with **no runtime combinator graph**. The vtable/SmallBox
question does not get answered; it is dissolved, because a monomorphic chain has no vtable.*

---

## 1. The mechanism, made concrete

Both parse-that cores today pay the same tax: a combinator is an **opaque indirect call**.
Rust boxes each combinator behind `SmallBox<dyn ParserFn<'a,Output>+'a, S32>`
(`rust/parse_that/src/parse.rs:61-64`) and dispatches through it on every hot call
(`parse.rs:83` `self.parser_fn.call(state)`); TS routes every combinator through the
`this.parser(state)` function-pointer field (`typescript/src/parse/parser.ts:53,82,88,131`,
`leaf.ts:44,49,62,140`) — R4 §6c names this "THE megamorphic IC of the engine." The indirect
call is an inlining firewall: the compiler cannot see across it, so a three-combinator
sequence is three un-inlined calls threading a `&mut ParserState`, each re-loading state,
each defeating the register allocator.

4B's claim: for the parts of a grammar that are **statically known** — sequencing (`then`,
`all`, `skip`, `next`, `wrap`, `trim`), ordered choice (`or`, `any`), first-char dispatch,
and value maps — the composition can be specialized ahead of time so the firewall vanishes
and the chain becomes one flat function. This is the applicative/selective fragment of the
algebra; only the genuinely monadic combinator (`chain`/bind, whose continuation depends on a
parsed value — `parser.ts:124-144`) and recursion (`lazy`) stay dynamic. The design is
therefore two faces of one staging discipline:

- **Rust face — monomorphized typed combinators.** Replace the boxed-closure `Parser` on hot
  chains with generic combinator *structs* that own their children **by value** and implement
  `ParserFn` directly. `Seq2<P0,P1>` where `P0: ParserFn, P1: ParserFn` has a concrete type;
  its `call` inlines both children; there is no `dyn`, no `SmallBox`, no heap box, and — the
  load-bearing consequence — **no `'a`-scoped arena to thread**. The arena that could not be
  made lifetime-sound over the 86 constructor sites (blocked precisely because
  `Box<dyn ParserFn<'a>>` demands `'a`-scoped ownership) becomes unnecessary: a value-owned
  monomorphic struct needs no allocator at all.

- **TS face — a build-time fuser.** A build step consumes the static parser description and
  **emits specialized JS** — the generalization of the arity-2/3 unroll already hand-written
  in `fuseAll` (`leaf.ts:183-251`) to arbitrary arity and arbitrary combinator shape, so the
  emitted output is straight-line recursive descent with the combinator closures constant-
  folded away. "No runtime graph at all": the shipped parser for a fused region is a plain
  function, not a tree of `Parser` objects dispatched through `.parser`.

Both faces stage the **same combinator tree** into the **same straight-line shape**; that
shared shape is where the rust/ts isomorphism (constitution iii) lives — isomorphism of the
*fused form*, not of the closure plumbing.

## 2. Substrate — the Rust face (monomorphized fusion chains)

The blanket impl `impl<F: Fn(&mut ParserState)->Option<O>> ParserFn for F` (`parse.rs:45-53`)
is the hook: a combinator need not be a `Fn`; any type implementing `ParserFn` qualifies. The
substrate is a small set of generic combinator structs, each `#[inline]`-transparent:

```rust
pub struct Seq2<'a, A, B, P0, P1> { p0: P0, p1: P1, _pd: PhantomData<(&'a (), A, B)> }
impl<'a, A, B, P0: ParserFn<'a,A>, P1: ParserFn<'a,B>> ParserFn<'a,(A,B)>
    for Seq2<'a, A, B, P0, P1> {
    #[inline(always)] fn call(&self, s: &mut ParserState<'a>) -> Option<(A,B)> {
        let a = self.p0.call(s)?;      // inlined — no blr through a vtable
        let b = self.p1.call(s)?;      // inlined
        Some((a,b))
    }
}
```

This is exactly what the `seq!`/`alt!` macros (`combinators/macros.rs:1-302`) already do
*by hand up to arity 8* — collapse N−1 boxes into ONE and thread `furthest_offset`
(`macros.rs:91,96`) — but a macro still produces one boxed closure; the generic struct
produces a fully transparent type. Method chaining (`.then()`, `.skip()`, `.next()` at
`combinators/methods/sequence.rs:10-45`) returns `Parser::new(closure)` today; the 4B form
returns `Seq2 { p0: self, p1: next, .. }` with `impl ParserFn` in return position, so
`a.then(b).then(c)` has type `Seq2<Seq2<A,B>, C>` — one monomorphic value, one inlined `call`,
zero indirection. `Alt2`, `Map`, `Opt` follow identically.

**The erasure boundary — the one place a box survives.** A fully monomorphic tree cannot
express recursion (the type would be infinite) nor heterogeneous storage (a `Vec<rule>`).
`lazy()` is already that boundary: `LazyParser` holds `Box<dyn LazyParserFn>` and yields an
erased `Parser<'a,Output>` (`lazy.rs:20-64`). 4B keeps **exactly one erasure per named
grammar rule** — an `into_dyn(self) -> Parser<'a,O>` re-boxes a monomorphic chain into the
existing `SmallBox` at the rule seam. Fusion happens *within* a rule; recursion re-enters
through the rule's single box. This is the GHC stream-fusion pattern (fuse within a pipeline,
materialize at the boundary) transposed to parsing, and it is what bounds the
monomorphization so it does not blow up (§7 R1).

**Why this beats the arena and the shim.** The de-nightly measurement established the vtable
boxes are **6 construction allocs / 5960 B**, never the **5390 per-parse allocs** (Vec growth
in `many`/`sep_by` + string arena) — so an arena's reward ceiling is 6 allocs, trivial. 4B's
reward is a *different* quantity: eliminating the per-combinator indirect **call** and
unlocking **cross-combinator inlining** on the hot path — the thing `SmallBox<dyn>`
structurally forbids. The prototype must measure that (cyc/B + disasm `blr`-count), not alloc
(§8).

## 3. Substrate — the TS face (the build-time fuser)

The seed is already in the tree: `fuseAll` (`leaf.ts:179-273`) hand-unrolls arity-2 and
arity-3 into positional closures with one pre-sized result array, drop-`undefined`, and exact
backtracking/offset-restore — "so V8 sees a monomorphic call site with constant-folded parser
bindings" (`leaf.ts:172-176`); `dispatch` compiles an `Int8Array(128)` first-char table
(`leaf.ts:100-151`). 4B **generalizes the unroll from a fixed arity ladder to a code
generator**: given a static combinator expression (an `all([...])`, a `.then().then()` chain,
an `any([...])`/`dispatch` fan-out, a `.map()`), the fuser emits one specialized function
body — the arity-N sequence unrolled, the choice compiled to a `switch`/if-ladder over first
bytes, the maps inlined — with no `Parser` objects and no `.parser` indirection surviving into
the shipped `dist/`.

The staging vocabulary is the prior art's: this is parsley's Typed-Template-Haskell code
generation and flap's "token-free specialized code," re-homed to a TS build plugin. The
combinator tree is **defunctionalized** into an abstract-machine description, then the machine
is *emitted* rather than *interpreted* — the exact transform Willis/Wu/Pickering describe
(combinator tree → abstract machine → staged CPS codegen). Hook point: the existing
multi-entry `vite build` (`typescript/vite.config.ts`) gains a fusion pass over the modules
that construct static parsers; the emitted specialized functions replace the
`makeParser(fuseAll(...))` call sites (`leaf.ts:160-163`). The public `Parser` API is
unchanged — the fuser is a compilation of it, not a replacement (the API stays the authoring
surface; the fused function is what runs).

**The applicative/selective vs monadic boundary (the dynamic escape).** parsley's core insight
is that monadic bind generates *dynamic* structure a stager cannot see; selective functors
(`branch`) recover conditional choice while keeping the tree static. parse-that's `chain`
(`parser.ts:124-144`) is the monadic combinator — its continuation depends on the parsed
value, so it is the one node the fuser cannot fully specialize; it emits a boxed continuation
there and fuses everything around it. `then`/`all`/`or`/`opt`/`skip`/`next`/`wrap`/`map`/
`dispatch` are applicative/selective and fuse completely. This partition is the honest scope
of "no runtime graph": the graph collapses everywhere except the monadic and recursive
escapes, which are named and few.

## 4. rust/ts isomorphism (constitution iii, LOCK-6 face)

The isomorphism is at the **fused form**, measured not asserted. Both faces take the same
combinator tree and produce the same control-flow skeleton: sequence → N ordered `call`s
threading state with early-return on error and offset-restore; choice → checkpoint/try-ladder
threading `furthest_offset` (`macros.rs:91` ≡ `leaf.ts` isError/offset restore); dispatch →
first-byte table. The shared artifact is a **fusion IR** — a defunctionalized combinator-tree
description — from which the Rust monomorphizer and the TS fuser each lower. The isomorphism
linter/oracle is a **shared conformance corpus + differential test**: the Rust fused parser
and the TS fused parser must agree byte-for-byte on parse result, final offset, and
`furthest_offset`/diagnostics on the same inputs. Layout stays idiomatic per language (Rust:
generic structs; TS: emitted functions); behaviour is identical.

## 5. Integration seams (path:line)

| Seam | Location | Role in 4B |
|---|---|---|
| Rust `ParserFn` blanket impl | `rust/parse_that/src/parse.rs:41-53` | the monomorphic hook: combinator structs impl `ParserFn` directly |
| Rust boxed core | `parse.rs:61-77` (`SmallBox<dyn ParserFn>`, `Parser::new`) | what the fused structs replace on hot chains; kept only at the erasure boundary |
| Rust sequencing methods | `combinators/methods/sequence.rs:10-45` (`then/skip/next/wrap/trim`) | fusion targets — return `Seq*`/`impl ParserFn` instead of `Parser::new(closure)` |
| Rust hand-unrolled seed | `combinators/macros.rs:1-302` (`seq!`/`alt!`, arity ≤8) | the exact shape a general monomorphizer generalizes; already threads `furthest_offset` |
| Rust alternation/repetition | `combinators/methods/{alternation,repetition,sep_by}.rs` | `Alt*`, and the `many`/`sep_by` Vec-growth mass the fuser does NOT touch |
| Rust recursion/erasure boundary | `lazy.rs:20-64` (`Box<dyn LazyParserFn>` → erased `Parser`) | the one dyn box per named rule; fusion is within-rule |
| TS arity-unroll seed | `typescript/src/parse/leaf.ts:154-273` (`all`/`fuseAll`) | generalize arity-2/3 unroll → arbitrary-arity emitted code |
| TS dispatch table | `leaf.ts:100-151` (`Int8Array(128)`) | choice fan-out → compiled first-byte `switch` |
| TS sequencing/choice methods | `parser.ts:81-144` (`then/or/chain`) | fusion targets; `chain` (`:124-144`) is the monadic escape |
| TS build/entry seam | `parser.ts:34-49` (`parseState`), `vite.config.ts` | where the fuser pass hooks the multi-entry build |
| TS recursion boundary | `lazy.ts:19-25` (`createLazyCached`) | closure-local cache; the erasure/recursion escape |
| Perf floor (do-not-regress) | `typescript/test/benchmarks/pt-b3-fusion.bench.ts` | the standing fusion/dispatch/json regression bench |

## 6. Prior art

- **Willis, Wu, Pickering — "Staged Selective Parser Combinators"**, PACMPL/ICFP 2020,
  DOI 10.1145/3409002; library **parsley** (Typed Template Haskell). The canonical mechanism
  for 4B: represent the parser as a static tree, drop monadic bind (it generates dynamic
  structure), recover branching with **selective functors**, defunctionalize the tree into an
  abstract machine, then **stage** the machine into hand-written-quality code. The TS fuser IS
  this transform in a JS build step; the applicative/selective-vs-monadic partition (§3) is
  theirs. https://mpickering.github.io/papers/parsley-icfp.pdf
- **Krishnaswami & Yallop — "flap: A Deterministic Parser with Fused Lexing"**, PACMPL/PLDI
  2023, DOI 10.1145/3591269. Staged combinators emitting token-free specialized code, 2–6×
  ocamlyacc; the "specialized straight-line output, no runtime graph" target of both faces.
  https://www.cl.cam.ac.uk/~nk480/flap.pdf
- **Jonnalagedda et al. — "Staged Parser Combinators"** (Scala LMS), manojo.github.io 2015.
  Staging turns combinators into generators, shedding composition overhead; ~50% over
  hand-written recursive descent, >25× over the non-staged combinator form — the quantified
  case that fusion, not micro-tuning, is the lever.
  http://manojo.github.io/2015/09/02/staged-parser-combinators
- **Coutts, Leshchinskiy, Stewart — stream fusion / `foldr`/`build`** (GHC). The fuse-within-
  pipeline, materialize-at-boundary discipline that bounds monomorphization; 4B's within-rule
  fusion + single box at the `lazy` seam is its parsing transposition.
- **nom `Tuple` trait / winnow `Parser`-for-tuples** (rust-bakery/nom; epage, "Winnow 0.5",
  2023). Rust's proven monomorphic tuple-sequencing — combinators owned by value, no `Box`,
  arity-capped (the ceiling 4B respects, §7 R1). https://github.com/rust-bakery/nom ·
  https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/
- **`impl Trait` return / unboxed combinators** (m4rw3r, "Parser Combinator Experiments in
  Rust, Part 3", 2016). Returning `impl Parser` instead of `Box<dyn>` dissolves the box and
  restores the compiler's inlining — the parse.rs:62 substitution 4B performs.
  https://m4rw3r.github.io/parser-combinator-experiments-part-3

## 7. Named risks and counterexamples

- **R1 — monomorphization/emit blowup.** Full unroll of arbitrary arity explodes Rust type
  size and TS bundle size; nom caps its `Tuple` trait, the `seq!`/`alt!` macros cap at 8, and
  `fuseAll` unrolls only arity 2–3 then falls to an indexed `for` (`leaf.ts:255-272`).
  *Counterexample:* a 40-way `any` fully unrolled is worse than a table. *Guard:* fuse the
  measured hot shapes (arity 2–4, first-byte dispatch) and lower long/uniform fan-out to a
  loop/table — the `fuseAll` ladder is already the correct shape; generalize its *policy*, not
  unbounded unrolling. The generated-size budget applies to the TS emit as to any codegen.
- **R2 — recursion defeats monomorphization.** `impl ParserFn` cannot type a recursive rule;
  a naïve fuser loops forever or fails to compile. *Guard:* the erasure boundary is mandatory
  and singular — one box per named rule at `lazy.rs`; fusion never crosses it.
- **R3 — the win is only construction alloc (the low-reward trap).** If 4B is measured on
  allocs it repeats the arena's ceiling: 6 construction allocs, nothing per-parse.
  *Counterexample:* the de-nightly arena was correctly abandoned as high-risk/low-reward on
  that basis. *Guard:* 4B's gate is cyc/B + disassembly inlining evidence (Rust: `blr`-count
  through the vtable falls to zero on a fused chain; TS: `.parser` indirection absent from the
  emitted region), never alloc count.
- **R4 — the dispatch-lane over-claim.** The lazy-alloc lever's dispatch figure was reported
  +10.2% and **re-measured at ~+5%** across 24 paired rounds in two load windows — a claim
  sitting at the top of a wide noisy band that did not hold. *Counterexample:* fusing dispatch
  and reporting the single-window peak. *Guard:* the fusion win is proven for then-chain
  (+13.6%) and fused-all (+15.6%), marginal for dispatch; carry the honest number, paired
  interleaving mandatory even for "obvious" wins, no warm benches.
- **R5 — semantic drift under fusion.** A fused sequence that mis-threads `furthest_offset` or
  drops the drop-`undefined`/offset-restore semantics silently changes parse output or
  diagnostics. *Counterexample:* `fuseAll` had to reproduce EXACT drop-`undefined` + backtrack
  (`leaf.ts:170-176`); a general emitter can get the checkpoint wrong. *Guard:* differential
  oracle vs the un-fused reference on the full corpus (Rust ~400 tests, TS 124/124), plus
  `furthest_offset`/diagnostics parity — the fused form must be observationally identical.
- **R6 — the packrat/diagnostics seam.** A fully fused monolith erases the parser-node
  identities packrat memoization keys on (`packrat.ts` `getCijKey = id*OFFSET+offset`) and the
  per-parse-scoped diagnostic collection points. *Guard:* memo keys and diagnostic scopes
  attach to the **rule boundary** (the same erasure seam), not to fused-away internal nodes;
  fusion is within-rule so no memoizable/diagnosable boundary is lost.

## 8. What a prototype must test to move this family

1. **Rust inlining win (the core claim).** Build `Seq2/Seq3/Alt2` generic `ParserFn` structs;
   express the JSON combinator parser's hot chains through them; measure cold paired cyc/B on
   the six JSON corpora vs the current `SmallBox<dyn>` `.then()` form (paired interleaving,
   N≥200, two windows). Disassembly gate: `otool -tv`/`cargo asm` shows the child `call`s
   **inlined** (no `blr` through the parser vtable) on a fused chain. Report compile-time and
   binary-size delta (the R1 budget). Prove recursion still parses via one box at `lazy`.
2. **TS build-time fuser (the generalization).** Emit specialized functions for a static
   `all(...)` at arities 2–6, a `.then().then()` chain, and an `any`/`dispatch` fan-out;
   compare vs the runtime `fuseAll` on the certified paired harness (`process.hrtime.bigint`,
   per-sample A,B,A,B interleave, median-of-13 × ≥300k ops, N≥12 rounds, packrat unarmed,
   loadavg per round). Prove `dist/` semantic parity (124/124) and honest numbers (no dispatch
   over-claim). Confirm the emitted region contains **no** `.parser` indirection.
3. **The monadic/recursion escape.** Show a grammar mixing fused applicative regions with a
   `chain`/bind node and a recursive rule fuses everything except the two escapes, and that the
   escapes are the only boxes (Rust) / closures (TS) remaining — the "no runtime graph *except
   at named boundaries*" claim made falsifiable.
4. **Differential isomorphism.** One shared corpus; assert Rust-fused ≡ TS-fused on result,
   final offset, and `furthest_offset`/diagnostics — the constitution-iii parity, measured.

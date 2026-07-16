# LOCK-4 · Pass-3 · Family 4B — COMPTIME-FUSION

*The combinator core where fusion is monomorphization: Rust — the vtable/SmallBox box is
generated away, not redesigned around; TS — a build-time fuser emits specialized chains; no
runtime graph on the fast face. Every measurement names its HEAD; the substrate is
skinny@HEAD + the read-only parse-that scratch (R3 §8 recipe).*

---

## 0. Thesis and the decisive in-tree finding

4B's claim is that a combinator grammar is an **interpreter of itself** — the runtime `Parser`
graph is walked by indirect calls — and that **fusion is the first Futamura projection of that
interpreter**: partially-evaluate the combinator interpreter with respect to a fixed grammar and
a compiled, straight-line parser falls out, with the `dyn ParserFn` vtable, the per-construction
box, and the setup graph all erased. This is not a redesign of the box; it is the box's
**deletion by construction**.

The decisive finding: **parse-that already ships the projection, for the regex grammar.**
`rust/parse_that/src/regex/generated.rs` is `@generated` from `bootstrap/regex.bbnf` by the bbnf
generator, and it is a straight-line recursive-descent parser with **zero `SmallBox<dyn
ParserFn>` on its hot path**:
- `generated.rs:7` `pub struct RegexParser;` (a ZST) with free functions
  `fn __quantifier<'a>(state:&mut ParserState<'a>)->Option<RegexParserEnum<'a>>` (`:215`),
  `__alternation` (`:390`), `__concat` (`:434`), `__regex` (`:2276`) — recursion is cut by
  **named `fn` back-edges**, never by a boxed closure.
- The value node is ONE enum `RegexParserEnum<'a>` (an {enum-node}); its nodes live in an
  arena `__RegexParserEnumCtx{ __slab: BumpSlab }` (`:173`), constructed at parse entry
  `__RegexParserEnum_alloc(state)` (`:205`) and **`'a`-scoped by construction** (`:174`, `:196`
  `__c0(&'a self,...)->&'a [..]`).

Two more substrates already coexist in the same tree, and they are exactly the three arms of the
Q4 cost model {inline-monomorphize | enum-node | box-here}:
1. **Closure core** — `parse.rs:61` `Parser{parser_fn: SmallBox<dyn ParserFn+'a,S32>}`; every
   `.call()` is an indirect vtable call; construction boxes (PROVE-09: 6 allocs/5960 B). The
   current fast path; the {box-here} everywhere.
2. **`span_parser/` enum tier** — `span_parser/mod.rs:51` `struct SpanParser{kind: SpanKind<'a>}`,
   `:58` `enum SpanKind`, `:183` one `match &self.kind` over `Seq/OneOf/Many/Opt/Skip/Wrap/SepBy/
   Not/...` (`:208-227`) with `SpanKind::Boxed(inner)` (`:194`) as the explicit recursion escape.
   Vtable-free combinator algebra, {enum-node} materialized at RUNTIME.
3. **Generated route** — `regex/generated.rs` above. {inline-monomorphize + box-at-back-edge}
   materialized by CODEGEN.

4B's work is not to invent a fusion mechanism; it is to make these three a **pluggable,
cost-model-selected continuum** and to promote the generated route (regex → any bbnf grammar) to
the **fast face**, mirroring XL-D exactly: the build-time emitter is the fast face on both sides;
the runtime combinator graph survives only as the correctness oracle.

---

## 1. Q1 — does the runtime graph reach comptime monomorphization? (mechanism trace)

**No — not absent codegen.** The runtime API (`str("a").then(regex(...))`) builds a runtime value:
each combinator method returns a `Parser` holding `SmallBox<dyn ParserFn>` (`parse.rs:74`
`SmallBox::new(parser_fn)`). A `SmallBox<dyn>` is a **type-erased runtime value**; the concrete
closure types are already collapsed to one vtable pointer at construction. A comptime type
(`Then<Str,Regex>`) cannot be recovered from it — the monomorphization information is **gone by
the time the graph exists**. The `IrNode` variant (hand-forward fact) is likewise a runtime
serializable value; it carries structure but is not a Rust type. Therefore:

> The runtime combinator graph reaches comptime monomorphization by **exactly one** step: a
> build-time codegen over a **comptime-known grammar** (grammar-text/`IrNode` → Rust source), the
> step `regex/generated.rs` already embodies. There is no runtime-graph→comptime path absent
> codegen-reflection, which Rust does not have.

Two Rust sub-routes follow, and 4B keeps both with a clean division of labor:
- **Type-combinator route (no codegen).** Hand-written generic structs `Then<A,B>`, `Or<A,B>`,
  `Map<A,F:Fn>` with `impl Parser`; the USER writes the chain and `rustc` monomorphizes every
  `.call`, erasing the vtable. This is nom/winnow/chumsky. It cannot express a runtime-built graph
  (Q1) and **cannot close recursion** without a boxing boundary (§4 risk). It is the *ergonomic
  public API*, not the fast face.
- **Codegen route (the fast face).** The comptime-known grammar drives a build-time emitter to
  straight-line recursive-descent `fn`s + one value enum + a parse-scoped `BumpSlab`. This is the
  Futamura projection; it is what a monomorphizing lowering **demands of the API**: the grammar
  must be *reified at build time* (as text or `IrNode`), not merely constructed at runtime.

**What a monomorphizing lowering demands of the API (the concrete answer):** a build-time handle
on the grammar's call graph — the SCC decomposition (to place recursion boxes), the per-rule
arities (to size unrolls), and the per-`->` projection descriptors (to inline the value plane).
`IrNode` (14-variant near-bijection, serializable) is precisely this handle; the emitter consumes
it. The runtime combinator methods stay as a thin surface that either (a) build an `IrNode` for
the emitter, or (b) build the closure graph as the oracle.

---

## 2. Q4 — the fusion-policy cost model and its firing numbers

The three in-tree substrates are the three arms; the pluggable driver is a per-node choice keyed
on measured caps. This is rustc's MIR-inliner cost model / GHC's `-funfolding-use-threshold`
adapted to combinators. **Named caps, grounded:**

| Cap | Arm it gates | Grounding / firing number a prototype must fix |
|---|---|---|
| **fuse-arity** | inline-monomorphize → general loop | TS `fuseAll` today flips unroll→`for` at **arity 3** (`leaf.ts:214,255`); Rust `seq!/alt!` unroll to **8** (`macros.rs`, hand-written 2..8). The crossover is a V8-inline-budget (JS) / code-size (Rust) number, not a guess — measure where the unrolled arm stops beating the loop. |
| **dispatch-fanout** | chain-of-`if let` → enum `match` | `alt!` is a linear `if let…return` chain (`macros.rs:85-121`); `span_parser` `OneOf` is one `match`. Beyond ~N arms a jump-table `match`/first-char `dispatch` (leaf `dispatch` uses an `Int8Array(128)`/`byte_classes` LUT) wins over sequential trial. `RegexClass` (`classify.rs:55`) already the discriminator source. |
| **boxing-depth** | inline → box-here at SCC back-edge | **Mandatory, not optional:** every grammar cycle (JSON `value→array→value`) forces ≥1 boxing boundary or the concrete type is infinitely-sized. The number is topological: **exactly one box per SCC back-edge** (the generated route uses a named `fn` + arena slot instead of a box — cheaper). `lazy.rs:21` `Box<dyn LazyParserFn>` is today's back-edge box. |
| **`.rlib`/dist + incremental budget** | the global blowup ceiling | The BINDING risk (§4). chumsky/nom `type_length_limit` explosions and this repo's own **93 recorded `on_disk_cache` ICEs** (R3 §1) are monomorphization-bloat symptoms. Budget = generated `.rlib` size + `cargo build` wall-time delta per rule; TS = emitted `dist` bytes + V8 `--max-inlined-bytecode-size`. |

**The pluggability is real, not asserted:** the driver is the same decision-driver seat LOCK-2/3
own; 4B contributes the combinator-specific cost coefficients (per-node fuse benefit vs code-size
cost), fed the SCC graph from `IrNode`. Default policy: inline acyclic chains up to fuse-arity,
enum-node alternations past dispatch-fanout, box every SCC back-edge. A prototype must fix the
three thresholds on CSS **and** value.js by measuring emitted-code size and `cargo build`/`vite
build` time vs fuse arity across the full consumer surface (Q4's demand).

---

## 3. Q5 — one plan, two lowerings? (the distinguishing observable)

4B's honest disposition, consistent with the settled fact that the Rust and TS mechanisms are
**two distinct mechanisms, not one shared terminal**:

- **The Rust type-combinator route is intrinsic to the type system** — no serializable plan drives
  it; `rustc`'s type-checker *is* the fuser. This route cannot be the shared-plan terminal.
- **But the codegen route makes BOTH sides plan-driven.** One serializable plan — the comptime
  `IrNode` (or its grammar-text source) with its SCC/arity/projection annotations — drives (i) a
  Rust source emitter (emitting `fn __rule(state)->Option<Enum>` + `BumpSlab`, the `generated.rs`
  shape) and (ii) a TS source emitter (emitting `function parseRule(state){…}`). This is **one
  plan, two lowerings**, and it is the route 4B elects for the fast face.
- **The distinguishing observable Q5 asks for:** feed the SAME `IrNode` to both emitters; compile
  both; run both over a shared conformance corpus; assert **byte-identical parse trees / tape**.
  This is a LOCK-6 differential oracle, NOT a claim that one compiled artifact is shared. The
  observable that *proves* "one plan" is: mutate the plan (add a rule), regenerate both, and both
  parses change identically — provable, and the prototype must demonstrate it on a ≥3-rule grammar.

So: fused Rust regions are derivable from one serializable plan that also drives TS **on the
codegen route**; they are intrinsic to the type system **on the type-combinator route**. 4B ships
the codegen route as primary precisely because it reconciles Q1 and Q5 in one mechanism.

---

## 4. Q2/Q3 — the value plane and the opaque barrier under monomorphization

**Q2 (value plane through an arbitrary host closure).** In the monomorphized world the value
plane is *cheaper*, not harder, for the statically-typed case: `Map<A, F:Fn(A::Out)->B>` inlines
`F` with zero `FnDescriptor` indirection. The MapExpr-expressible projections all lower to inline
`Map` nodes / straight-line construction in the emitted `fn`. The barrier is only the **arbitrary
host closure not statically known at the fusion site**. For those, the emitter has three options
mirroring §2: a named `fn` item (still monomorphizable via a generic `F` — the common case), a
`fn`-pointer (one indirect call, no box), or — only if the closure must **cross to TS as data** —
a `HostClosure` descriptor. Today's `FnDescriptor` (classify.rs-driven) has **no `HostClosure`
variant**; that gap bites only at the serialization boundary (Q6/LOCK-6), never in pure-Rust
fusion. A census of value.js/latex-paper/keyframes `->` sites (how many are non-MapExpr) is owed
by a corpus prototype; 4B's structural answer is that non-MapExpr sites become **named host `fn`
calls**, which are call boundaries for the value plane but **not fusion-blockers for the
recognizer** (next paragraph).

**Q3 (opaque-barrier surface).** The key separation 4B contributes: **recognizer fusion is
independent of value-plane fusion.** `Opaque(HostFnId)` blocks value-plane fusion (the projection
is a call-out), but the *parse control flow* around it still fuses — the emitted `fn` calls the
host function and continues straight-line. So the barrier ratio for the RECOGNIZER plane can sit
near the ≤2% floor even when the value plane has many host calls; the two ratios must be counted
**separately**. A prototype must count, per corpus, (a) the fraction of `IrNode`s an analysis
cannot see through for RECOGNIZER fusion (expected ≪2%: only genuinely dynamic sub-parsers) vs
(b) the value-plane host-call fraction (larger, but not a recognizer barrier). Conflating them
inflates the barrier; separating them is the finding.

---

## 5. Q7 — the setup tax, and how comptime fusion is its endgame

PROVE-09 measured `json_parser()` **construction = 6 allocs / 5960 B**, paid once outside the
timed loop, and **per-parse twitter = 5390 allocs** (Vec growth in `many`/`sep_by` + string
arena — the real mass). The closure core's setup builds the whole `Parser` graph (N boxes). The
**generated route has no setup graph**: the parser is a set of `fn` items (a ZST `RegexParser`,
`generated.rs:7`); the only per-parse allocation is the `BumpSlab` reserved once at entry
(`with_capacity(n*32)`, `:181`) — a single arena alloc replacing the 6 construction boxes. So
comptime fusion is the **pooling/arena-elimination endgame Q7 seeks**: there is nothing to pool
because there is no constructed graph. The clean cost split 4B asserts: **recognizer** (straight-
line `fn` control flow, ~0 setup) / **value-plane** (inline `Map` + host `fn` calls) / **setup**
(one `BumpSlab` reserve, or zero for arena-free grammars). This also **sidesteps PROVE-09's W4
arena refutation entirely**: W4 failed trying to retrofit a `'a`-scoped arena through 86
`Parser::new` sites on the *closure* core (unsound `'static`-holds-`'a` trap). The generated route
is **born with the arena**, `'a`-scoped by construction (`generated.rs:205`), no retrofit, no
unsoundness — the vtable/SmallBox question dissolves because the fast path never had the box.

---

## 6. The TS half — the build-time fuser (arity-unroll generalized)

R4 §6c names `this.parser(state)` (`parser.ts:53,82,88,131,137`; `leaf.ts:44,49,62,140`) as **the
megamorphic IC of the engine** — many closure shapes flow through one indirect call. `fuseAll`
(`leaf.ts:179-273`) already hand-unrolls arity-2/3 to kill it *locally*. 4B generalizes this to a
**build-time emitter**: given the static grammar (its `IrNode`), emit one flat `function
parseRule(state){…}` per rule with **direct** calls (no `.parser` field read), the megamorphic IC
gone by construction. This is PROVE-14's proven lesson (build-time emitter is the fast TS face,
XL-D) applied to the recognizer, and PROVE-11's certified lazy-alloc lever
(then-chain +13.6%, fused-all +15.6%, alloc −60.1% — SPEC §0/11) rides atop it unchanged.
The TS blowup budget is V8's inline threshold: beyond fuse-arity the emitter must fall to the
general `for` arm (as `fuseAll` does at 3) or the fused `fn` exceeds `--max-inlined-bytecode-size`
and the IC-elimination benefit is lost to a non-inlined call. The armed/unarmed packrat state
(`packrat.ts:290`) is disclosed in every table (SPEC §0 honest-number law); dispatch carries at
**~+5%**, NOT the refuted +10.2% (SPEC §0/11 CONFIRMED_DELTA).

---

## 7. Prior art (citations)

- **Staged parser combinators — the canonical projection.** Jonnalagedda et al., *"Staged Parser
  Combinators for Efficient Data Processing"* (OOPSLA 2014): combinators + multi-stage programming
  (LMS) generate specialized recursive-descent, erasing combinator overhead — 4B's exact thesis.
  Krishnaswami & Yallop, *"A Typed Algebraic Approach to Parsing"* (PLDI 2019): typed combinators
  with static first-set/nullability analysis compiled to staged code (the analyzable-graph +
  staging pairing). Yallop et al., *flap* (PLDI 2023): staged combinators fused with lexing.
- **Futamura projections / partial evaluation** — Futamura 1971; the combinator interpreter
  specialized to a grammar = a compiled parser (the theoretical spine of §0).
- **Rust combinator libraries — the monomorphization tradeoff, measured.** `nom` 7+ (`Parser`
  trait, `impl Parser` monomorphized chains, no vtable). `winnow` (nom fork, deliberately bounds
  monomorphization, `&mut` cursor). `chumsky` 1.0 introduced an explicit `.boxed()` combinator to
  **cut** monomorphization at recursion/large grammars — direct evidence for the {box-here} arm
  and the `.rlib` blowup budget. `combine`, `pom` same tradeoff. `pest`/`rust-peg` are the codegen
  route (grammar→generated parser), as parse-that's own `regex/generated.rs` already is.
- **Compiler cost models.** rustc MIR inliner threshold; rustc **polymorphization** (reduce
  monomorphization bloat) — prior art for the blowup budget. GHC `-funfolding-use-threshold`,
  `INLINE`/`SpecConstr`, stream fusion (`build/foldr`) — the inline-budget + fusion analogy.

---

## 8. Named risks and counterexamples

- **R1 — monomorphization blowup (BINDING).** The type-combinator route explodes `.rlib` size /
  `cargo build` time; chumsky's `type_length_limit` and this repo's **93 `on_disk_cache` ICEs**
  (R3 §1) are the counterexample. *Guard:* the fuse-arity + `.rlib` budget caps (§2); the codegen
  route (emitting `fn`s, not nested generic types) largely sidesteps it — generated `fn`s are flat,
  not deeply-nested monomorphized types.
- **R2 — recursion cannot be totally fused.** Every grammar SCC forces a boxing/back-edge boundary;
  a fuser that promises total inlining is wrong. *Guard:* boxing-depth cap = exactly one back-edge
  per SCC; the generated route uses a named `fn` back-edge + arena slot, not a `Box`.
- **R3 — Q1 over-claim.** Asserting the runtime combinator API reaches comptime monomorphization
  without codegen is false (a `SmallBox<dyn>` is type-erased at construction). *Guard:* §1's
  mechanism trace; the honest claim is "codegen over a comptime-known grammar," nothing weaker.
- **R4 — Q5 over-claim.** Asserting Rust and TS share ONE compiled terminal contradicts the settled
  two-mechanisms fact. *Guard:* the shared object is the **plan** (`IrNode`), not the artifact; the
  observable is a differential oracle, not a shared binary.
- **R5 — value-plane serialization gap.** A `->` site needing a `HostClosure` descriptor to cross
  to TS has no `FnDescriptor` variant today. *Guard:* the gap is at the LOCK-6 boundary only; pure-
  Rust fusion never needs it; the corpus census (Q2) sizes it before any promise.
- **R6 — the arena unsoundness trap (already refuted for the closure core).** Do NOT retrofit an
  arena onto the `SmallBox` `Parser` type (PROVE-09 W4). *Guard:* the generated route is born with a
  `'a`-scoped `BumpSlab` (`generated.rs:205`); the closure core is left as the oracle, unchanged.

---

## 9. What a prototype must test to move family 4B

1. **Generalize the generated route from regex to a small bbnf grammar.** Emit JSON (or a 3-rule
   toy) as straight-line `fn`s + one value enum + `BumpSlab`, the `regex/generated.rs` shape.
   Gate: **zero `SmallBox`/`dyn ParserFn` on the hot path**; construction allocs **< 6** (PROVE-09
   baseline); per-parse recognizer allocs = one `BumpSlab` reserve; ~400 parse-that tests as the
   parity oracle; cold paired divan vs the closure core (no-regression, honest — SPEC §0/09 law).
2. **Fix the three Q4 firing numbers.** Measure emitted-code size + `cargo build`/`vite build`
   wall-time vs fuse-arity across CSS **and** value.js; find the unroll→loop crossover (Rust and
   V8 separately), the chain→enum dispatch-fanout crossover, and the `.rlib`/`dist` budget ceiling.
3. **One-plan-two-lowerings observable (Q5).** Drive Rust + TS emitters from ONE `IrNode` on a
   ≥3-rule grammar; assert byte-identical parse trees; mutate the plan, regenerate both, assert both
   change identically.
4. **The two barrier ratios (Q3), counted separately.** On value.js/latex-paper: recognizer-plane
   opaque fraction (expected ≪2%) vs value-plane host-call fraction; report both, never conflated.
5. **The TS build-time fuser (§6).** Emit `function parseRule(state){…}` for the value.js hot
   3-tuples; prove the `this.parser` megamorphic IC is gone (IC-state probe / `%GetOptimization
   status`) and PROVE-11's floors (+13.6/+15.6%, −60% alloc) hold atop it; disclose packrat state.

---

## 10. Q8 — predecessor shapes (report only, as inputs to 4B)

- **LOCK-5 (regex continuum).** XL-C retired the regex functor: two domain-fit types — opaque
  `Regex(StringId)` grammar tier, rich `Hir` engine tier, cheap bridge. Input to 4B: a regex leaf
  in a fused chain lowers as the opaque tier (a compiled DFA `find_at` call — a straight-line leaf,
  fully fusable into the recognizer), while the engine tier is off the parse hot path. The regex
  grammar's OWN parser (`regex/generated.rs`) is 4B's existing codegen-route proof. Open sub-
  question 4B inherits: whether the bbnf emitter and the regex emitter share one code-emit backend
  (they should — the regex parser is already generated by the bbnf generator).
- **LOCK-7 (host-fn lattice).** The typed host-fn interface bounds grammar totality; 4B's value-
  plane host closures (§4, Q2) are its consumers. Input to 4B: the lattice decides the declared
  signature shape a non-MapExpr `->` gets (named `fn` vs `fn`-ptr vs serializable `HostClosure`);
  4B needs the signature to be **statically typed at the fusion site** for inline monomorphization,
  so the lattice's "declared signature" requirement and 4B's "static type at fusion site"
  requirement are the same constraint viewed from two locks. Open: which lock owns the internal-
  descriptor evaluator (Hex/Number/Enum) — LOCK-3 IR-VM lowering vs LOCK-7 host dispatch — is
  unresolved here and reported, not settled.

---

## 11. bbnf-side substrate corroboration (the graduated-skinny tree, verified pointers)

The parse-that findings above are mirrored — and made countable — in the graduated bbnf-lang tree
(the `crates/` tree of record; `crates/ir`, `crates/core`, `crates/runtime`). These pointers
sharpen the same answers with the build-time-graph substrate the closure core does not carry.

**Q1 / the 14-variant plan.** `bbnf_ir::IrNode` is exactly `crates/ir/src/types/node.rs:30` — the
serde-serializable 14-variant near-bijection (`Literal, Regex, Epsilon, Seq, Alt, Repeat, Ref,
Skip, Next, Minus, Negate, Map, OptionalWhitespace, TokenDispatch`), born as data from grammar
text, not from the runtime API. It IS the comptime-known handle §1 demands; the Rust emitter lives
at `crates/core/src/backend/` and already monomorphizes it (the `generated.rs` shape, generalized
beyond regex).

**Q4 / the fusion firing numbers, live in-tree.** Three fusion passes already fire and their guards
ARE the caps: (a) `fuse_single_use` (`crates/ir/src/passes/transform/fuse.rs:44`) inlines
size-unbounded but is gated by `is_composite_seq` (`:201`, aggregate Seq≥2), `body_has_map`
(`:249`, the typed `->` invariant), and `is_consumer_pinned` (`:220`, `@pretty`/`@debug`) — the
legality caps a size cap must be added beside; (b) token-dispatch fusion `try_factor_alt`
(`crates/ir/src/passes/transform/fuse_token/factor.rs:23`) fires at **`branches.len() >= 4`** — the
concrete dispatch-fanout crossover, materialized as `IrNode::TokenDispatch`; (c) map fusion
`compile_map` (`crates/core/src/backend/driver/map.rs:16`) folds `Map(Map(x))` via
`emit_fused_map`. The pluggable cost model these feed: `crates/ir/src/cost.rs`,
`crates/ir/src/egraph/cost.rs`, and the CSP driver `crates/ir/src/passes/csp_strategy/mod.rs`.

**Q2 / Q3 / the value language and its barrier.** `FnDescriptor`
(`crates/ir/src/types/fn_descriptor.rs:18`) has **no `HostClosure`/`Opaque` variant** — variants
are `EnumWrap, BoxWrap, NumberConvert{allow_leading_dot}, HexConvert{fn_path}, SpanCapture,
Expr{MapExpr, return_type}`. `Expr` is the sole user-facing variant; its transparent language
`MapExpr` (`crates/ir/src/types/map_expr.rs:17`) is `IntLit/FloatLit/BoolLit/StringLit/Input/
InputProp/FnCall{name,args}/BinOp/UnaryOp`, constant-folded at `:109`. The barrier is precisely
`MapExpr::FnCall{name}` (named host fn, signature-visible, body-opaque) and true closures (not
`MapExpr`-expressible at all). The proposed serializable shape for the latter — for LOCK-7 to
ratify — is `HostClosure{ fn: HostFnId, sig: FnSig{inputs:[TypeDesc], output:TypeDesc}, purity }`,
carrying a typed boundary + effect flag so the fuser can type-check and box-vs-enum the seam while
still fusing the recognizer around it. `NumberConvert`/`HexConvert` are the sanctioned regex-scan +
conversion fusions (the LOCK-5 opaque-`Regex(StringId)`-tier seam).

**Q7 / the setup tax, named exactly.** The VM the report frames as the correctness oracle is
`crates/ir/src/vm/interpreter/mod.rs`; `Interpreter::new` allocates, per parse, **8 Vecs + one
FxHashMap** — `values`(cap 64), `call_stack`(32), `checkpoints`(32), `repeats`(16),
`memo_starts`(8), `value_depth_stack`(8), `rule_stack`(16), `diagnostics`(0), and `memo:
FxHashMap`. This is the Q7 "8 Vecs + FxHashMap" surface. Against PROVE-09's 5 390 per-parse allocs
it is a fixed ~9-alloc tax — negligible on large inputs, a large fraction on tiny ones (the
amortization curve rises as input → 0). Pooling the VM (`.clear()` keeping capacity) zeroes it for
the oracle; the generated fast face pays none of it (§5). Clean split to report: recognizer /
value-plane / setup — the generated face zeroes the setup column and attacks the value-plane
column at its `many`/`sep_by` Vec-growth source.

**Q6 / the internal-descriptor evaluator's home.** `NumberConvert/HexConvert/EnumWrap/BoxWrap/
SpanCapture` are **compiler-produced during lowering/specialization** (stated at
`fn_descriptor.rs:14-16`) — lowering artifacts, not user host functions. Disposition: the
internal-descriptor evaluator belongs to **LOCK-3 IR-VM lowering** (emitted by the driver
`map.rs` on the compiled face and by the VM leaves `crates/ir/src/vm/interpreter/leaves.rs` on the
oracle face); **LOCK-7 host dispatch owns only the user surface** (the `MapExpr::FnCall{name}`
targets + the future `HostClosure`). This split is the exact seam 4B relies on: everything internal
is fusible; only the LOCK-7 surface is the barrier. (Reported as the resolving evidence for the
sub-question §10 left open.)

**Q6 corpus.** The dual oracle runs over the three structural grammars already carried at
`crates/runtime/src/grammars/` — json (twitter/canada/citm), css_l4_declaration_values
(bootstrap/normalize/tailwind), sheets — with the emitted tape at `crates/runtime/src/tape/` as the
byte-diff surface against the VM `Value` tree. The live Rust golden (never a hand-faked blob) is
that emitter's own captured tape.

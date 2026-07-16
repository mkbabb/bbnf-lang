# LOCK-4 — THE COMBINATOR CORE · pass-1 prior-art research

*Researcher: prior-art, no family allegiance. Sweeps web + codebase for the combinator-algebra
problem class (parse-that Rust + TS): allocation model, fusion, packrat, diagnostics, rust/ts
parity. What exists, what won/lost elsewhere for THIS class, the constraints our evidence base
already imposes, the named traps. Registry deliberately unread (pass protocol).*

---

## 0. The problem class, framed

The four LOCK-4 families are not four features — they are four *compilation strategies for a
combinator algebra*, ordered by how much of the parser becomes a first-class analyzable object:

- **4A CLOSURE-TO-GRAPH** — combinators build a static, inspectable node graph (not opaque nested
  closures); fusion/packrat-placement/diagnostics become graph analyses; graph shape isomorphic
  rust↔ts.
- **4B COMPTIME-FUSION** — no runtime graph at all: Rust monomorphizes fusion chains (vtable/
  SmallBox dissolved); TS emits specialized chains at build time (arity-unroll generalized).
- **4C VM-CORE** — combinators lower to a compact bytecode VM, one instruction set shared rust↔ts;
  packrat + diagnostics are VM facilities.
- **4D GRAMMAR-SUGAR** — combinators are a construction API over the *same HIR the grammar plane
  uses* (LOCK-5's continuum); parse-that inherits the optimizer.

The constitution binds all four: **combinators stay lawful hand-written primitives** (clause i
exempts "primitives and combinators" from grammar-totality) — so every family is about the
*compilation and analysis* of those primitives, never their replacement by generated code
(LOCK-4 charter, PORTFOLIO.md:96). "Their compilation and analysis need not be naive."

The single most important external finding: **the exact 4A/4B/4C architecture already exists,
won, and is measured** — in Parsley (Haskell) and LPeg (Lua). We are not inventing; we are
porting a settled design into a dual-language, NEON-era, type-planed setting. That reframes
LOCK-4 from "which family" to "which family, given parse-that's monadic `chain` and the rust/ts
isomorphism edict."

---

## 1. The canonical winner: Parsley (the whole 4A→4B→4C pipeline, proven)

Parsley (Willis, Wu, Pickering — *Staged Selective Parser Combinators*, ICFP 2020) is the
definitive prior art and it collapses 4A+4B+4C into one pipeline:

1. user combinators are a **deep-embedded AST — the "Combinator Tree"** (this is 4A: the graph
   is the parser, "preserves and exploits semantic information about the original grammar and
   can be traversed to perform optimizations and analysis");
2. **law-based rewrites** optimize the tree (functor/applicative/alternative laws as rewrite
   rules — the e-graph-shaped opportunity LOCK-2 owns);
3. the tree is transformed into an **abstract machine in CPS form** (this is 4C: a
   continuation-passing automaton), analyzed further;
4. **staged partial evaluation** (Typed Template Haskell) emits Haskell "resembling hand-written
   recursive descent parsers" (this is 4B: no interpreter, no runtime graph — the abstraction is
   compiled *away*).

Outcome: parser-combinator ergonomics at **hand-written-parser performance** — the exact
"beyond-SOTA" bar LOCK-4 sets. Sources:
[Staged Selective Parser Combinators (ACM/PACMPL 3409002)](https://dl.acm.org/doi/10.1145/3409002),
[ICFP paper PDF](https://mpickering.github.io/papers/parsley-icfp.pdf),
[Design Patterns for Parser Combinators](https://www.cs.tufts.edu/~nr/cs257/archive/jamie-willis/parsing-patterns.pdf),
[hackage: parsley](https://hackage.haskell.org/package/parsley).

### 1a. The trap Parsley names — and parse-that walks straight into it

Parsley's headline constraint: **to exploit static grammar structure you MUST drop monadic
computation** — monadic bind generates *dynamic* structure the tree cannot see through. The
recovery mechanism is **selective functors** (Mokhov et al., *Selective Applicative Functors*,
ICFP 2019): `branch :: f (Either x y) -> f (x->a) -> f (y->a) -> f a` gives data-dependent choice
that is *still statically analyzable* — "restrict power to selective functors, which gives access
to some control-flow constructs while still permitting static analysis needed to compile away all
abstraction and overhead." Sources:
[Selective Applicative Functors (ACM 3341694)](https://dl.acm.org/doi/10.1145/3341694),
[hackage: selective](https://hackage.haskell.org/package/selective).

**This is the load-bearing constraint for 4A/4B/4C on parse-that.** parse-that ships a **monadic
`chain`** — TS `chain` 124-144 (R4 §2, "C-16: threads value on success"), and the Rust core is a
closure algebra with the same expressive class. Any static combinator-graph (4A), any total
comptime fuser (4B), any VM lowering (4C) must decide what happens at a `chain` node: either (a)
exclude `chain` from the analyzable subset and treat it as an opaque escape hatch (a *named
non-goal*, honest), or (b) reformulate the data-dependent sites as `branch`/selective and prove
the consumer grammars (value.js, latex-paper, keyframes.js) never need the monadic residue. This
is the family-discriminating question a pass-2 design must answer explicitly, not paper over.

---

## 2. The canonical VM: LPeg (the 4C reference instruction set, production-proven)

LPeg (Ierusalimschy; Medeiros & Ierusalimschy, *A Parsing Machine for PEGs*) is the settled
4C precedent and it ships in production (Lua's standard PEG library). Architecture directly
answers "one instruction set shared by rust and ts":

- a **register parsing machine**: PC register, subject pointer, and a **stack of call frames
  (return address) + backtrack frames (address + subject position)**;
- each parsing expression compiles to a program; **compound expressions combine their
  sub-programs' code** — i.e. the VM lowering is itself compositional, isomorphic to the
  combinator algebra;
- small instruction set: `Char`, `Jump`, `Call`, `Return`, `Commit`, `Choice`, `Fail`,
  `PartialCommit`, `BackCommit`, `Span` — backtracking is a *dynamic indirect jump* whose target
  is pushed on the stack and looked up at fail time (exception-handling shape).

The instruction set is **language-neutral** — which is precisely why 4C claims "layout
isomorphism by construction" for rust↔ts (LOCK-6's plane gets it for free). gpeg (Yedidia,
Harvard thesis) extends the LPeg machine to **incremental** reparse, and there is a LuaJIT port
(LPegLJ) — evidence the machine survives JIT compilation. Sources:
[A Parsing Machine for PEGs (PUC-Rio)](https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf),
[gpeg incremental PEG machine](https://github.com/zyedidia/gpeg),
[Yedidia thesis](https://zyedidia.github.io/notes/yedidia_thesis.pdf),
[LPegLJ (LuaJIT)](https://github.com/sacek/LPegLJ).

**4C caveat our own SPEC imposes:** the SPEC's certified wins are *SIMD scanner* kernels
(NEON shufti/vshrn string scan, §0/01/03) fused into the parse loop. A naive bytecode VM
*interpreter* re-introduces a dispatch cost per byte that a fused NEON scan specifically
eliminated — so 4C must be "measured against the closure core, not assumed" (PORTFOLIO.md:106),
and its instruction granularity must be coarse enough to host the NEON scanners as single
instructions (a `ScanClass`/`ScanString` op), never a per-byte `Char` loop on hot text.

---

## 3. The Rust field: winnow (4B wins raw speed), chumsky (4A-lite discard analysis), combine

- **winnow** — "the fastest Rust parser-combinator library," **< half nom's parse time** — wins
  by a **monomorphized "toolbox" model** with no boxing on the hot path. This is direct evidence
  that **4B (monomorphized fusion, no runtime graph) is the raw-speed leader in Rust**. But note:
  winnow's speed comes from generics + `impl Trait`, *not* from an arena or a VM.
  [Winnow 0.5: Fastest Rust Parser-Combinator?](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/)
- **chumsky** — a runtime combinator graph with a **GAT-based internal optimizer that
  communicates to sub-parsers whether output can be discarded** (check-mode vs emit-mode);
  zero-copy (outputs borrow the input); "within 20% of hand-written." This discard analysis is
  **exactly bbnf's no-value-discard concern inverted** — chumsky *proves* when a value is unused
  and skips its construction; bbnf's law forbids *accidental* discard. A 4A graph makes this a
  first-class analysis (which nodes' outputs are live). [chumsky docs](https://docs.rs/chumsky/latest/chumsky/),
  [chumsky repo](https://github.com/zesterer/chumsky).
- **combine** — the cautionary tale on the boxing question: parser-combinator traits "produce
  very large types"; escaping them costs either `Box<dyn Parser>` (vtable, pointer-chase, "may
  lose optimization opportunities") or a `parser!` macro for non-allocating anonymous parsers.
  This is parse-that's `SmallBox<dyn ParserFn>` question stated by the field.
  [combine docs](https://docs.rs/combine), [bumpalo arena](https://github.com/fitzgen/bumpalo).

**What the Rust field settles:** raw speed → monomorphize and don't box (4B/winnow). But
monomorphization has a *binary-size* cost (separate copy per concrete type) and, crucially, it
does **not** by itself deliver the cross-analysis (packrat placement, diagnostics points) that
4A/4C give. The field has no library that is simultaneously fully-monomorphized *and* fully
analyzable — that gap is where LOCK-4 can be genuinely beyond-SOTA (Parsley achieves it via
staging, which is 4A-tree + 4B-codegen fused).

---

## 4. The allocation model — what the SPEC already measured (the vtable/arena question, decided)

The 4B charter says "ownership redesigned so the vtable/SmallBox question dissolves." Our own
evidence base has already *measured* the reward ceiling, and it is small — this is the sharpest
internal constraint:

- **PROVE-09 REPORT + PT-2.md §W4:** an arena over the `Parser<'a,O>` vtable boxes moves only the
  **6 construction allocs (5960 B)**, never the **5390 per-parse allocs** — the real mass is
  `Vec` growth in `many`/`sep_by` + the string arena (PROVE-09 REPORT §Alloc; PT-2.md:322,332).
  "High-risk, low-reward by measurement."
- **The named unsoundness trap:** `ParserFn<'a>` is parameterized by the *input* lifetime `'a`;
  a `'static`/thread-local arena holding `'a`-closures is **unsound** — the arena must be
  `'a`-scoped and threaded through all **86 `Parser::new` sites** (PROVE-09 REPORT §Arena;
  PT-2.md:324-328). PT-2.W4 is therefore "lifetime-sound **or not at all**," and a *measured
  refutation is a valid close* (P4/P39, abrogate-before-patch).
- **`SmallBox<dyn ParserFn, S32>`** (R3 §5, parse.rs:61) already inline-stores ≤S32 closures; the
  nightly `smallbox` feature is the only blocker and `smallbox!` on stable is alloc-neutral
  (PROVE-09).

**Constraint 4B must respect:** any "the vtable dissolves" story either (a) confronts that the
per-parse alloc mass is `many`/`sep_by` Vec growth — so the real 4B win is *fusing collection
loops* (SmallVec inline storage already partly does this, R3 §5), not eliminating the vtable — or
(b) closes on a filed refutation. Claiming the arena as a *speed* lever without the paired
measurement repeats PROVE-09's error (below).

---

## 5. The refutation laws our evidence base binds onto LOCK-4

These are measured internal LAWs, not opinions — any pass-2 design that violates them is
dead on arrival:

- **§0/09 — no stable-port perf claims (REFUTED, now LAW).** PROVE-09 claimed portable_simd→NEON
  + de-nightly = 1.34–1.57× faster; the verify fleet **REFUTED** it to parity-or-slower under
  fresh **paired interleaving** — a classic two-arm window asymmetry (the "modified" arm sampled
  in a quieter window). Instruction census disproved the mechanism (shim emitted 2271 `addv.8b`
  vs 509 — *more* reduction work). **LAW: the Rust core reform (the 4B allocation territory)
  makes ZERO perf claim; its value is stability. Paired interleaving is mandatory even for
  "obvious" wins.** (SPEC §0/09; PT-2.md §1.1, §1.3.)
- **§0/11 — the honest diagnostic number (CONFIRMED_DELTA).** PROVE-11's lazy diagnostic-array
  alloc: then-chain **+13.6%**, fused-all **+15.6%**, fused alloc **−60%** — but the **dispatch
  lane reproduced at ~+5%, not the prover's +10.2%**. **LAW: diagnostics are a lazy, off-hot-path
  facility (fields `undefined`-at-birth, populated only under a per-parse flag); carry the
  dispatch lane at ~+5%; inflating it trips the audit.** (SPEC §0/11; PROVE-11 VERIFY; PT-3.md
  §1.3, §W1.) *Implication for 4A/4C:* diagnostics-points-as-graph/VM-facility must default to a
  cold, zero-alloc path — the graph/VM carries the *placement*, the runtime carries nothing until
  armed.
- **Warm benches are disinformation; cold per-parse, paired, N≥200, multi-window** (SPEC §0, §4;
  PT-3.md §1.3 — the naive `vitest bench` cross-run is load-confounded and refused as evidence).

---

## 6. Packrat & left recursion — the settled field wisdom (constrains 4A placement / 4C facility)

- **Warth et al. 2008** (bounded seed growth) supports direct+indirect left recursion by mutating
  the memo table into an iterative seed-grow — but **"its full version… is complex," and OMeta
  shipped only *direct* LR to avoid it.** **Medeiros'** approach avoids memoization, using the
  LR table "more like a stack." Memoization buys linear time but **costs a per-position table on
  every parse** — only worth it for backtracking/ambiguous grammars. Sources:
  [Warth: Packrat Parsers Can Support Left Recursion](https://web.cs.ucla.edu/~todd/research/pepm08.pdf),
  [Left Recursion in PEGs (Medeiros)](https://arxiv.org/pdf/1207.0443),
  [linear-time PEG packrat w/ LR (2026)](https://www.arxiv.org/pdf/2601.05012).
- **parse-that already embodies the settled wisdom:** packrat is **opt-in and armed-only** — a
  true no-op (`PACKRAT_ARMED` false) until the first `memoize()` is constructed, so the default
  LL(1) path pays nothing (R4 §6g, packrat.ts:156/290). This matches the field: **unconditional
  packrat is a pessimization.**

**Constraint for 4A/4C:** "packrat placement becomes a graph analysis" (4A) / "packrat is a VM
facility" (4C) is a *good* framing precisely because the field says packrat must be *placed
selectively* (only at recursion/ambiguity points), never global. The graph/VM is the natural home
for that placement decision — but the win must be measured against the armed baseline, and the
disclosure "armed vs unarmed" is mandatory in every table (PT-3.md §W4).

---

## 7. The TS / V8 plane — the megamorphic-IC problem (4B's TS half attacks it directly)

- **The megamorphic IC:** the engine's central hot call is `this.parser(state)` — many distinct
  closure shapes flow through one indirect call site (R4 §6c: parser.ts:53,82,88…; leaf.ts:44,
  49,62,140). V8 caches ≤4 hidden classes monomorphic/polymorphic; beyond that the site goes
  **megamorphic → slow dictionary dispatch**. Sources:
  [An Inline Caching Design for TS Performance (ACM 3428242)](https://dl.acm.org/doi/pdf/10.1145/3428242),
  [mrale.ph: what's up with monomorphism](https://mrale.ph/blog/2015/01/11/whats-up-with-monomorphism.html).
- **4B's TS story is a direct hit on this:** a build-time fuser that collapses N combinators into
  **one** specialized closure removes N megamorphic dispatches. parse-that already ships the seed:
  **arity-unrolled `any` (arity-2) and `fuseAll` (arity-2/3)** collapse the dispatch (R4 §6d,
  leaf.ts:39-57,183-251); the Rust `seq!`/`alt!` macros collapse N−1 boxes into one (R3 §5,
  macros.rs). Generalizing arity-unroll to an arbitrary build-time fuser IS 4B-TS.
- **Hidden-class discipline is load-bearing and already proven fragile:** PROVE-11 made
  `ParserState.suggestions/secondarySpans` `undefined`-at-birth so the shape is "fixed from birth
  and never transitions"; W4's `%HaveSameMap` oracle pins it (PT-3.md §W4). **Trap:** `mapState`'s
  `Object.create(state)` per-invocation builds a **prototype-chain view object** — a documented
  deopt risk (R4 §6b, parser.ts:177-180). Any 4A/4C runtime that adds per-parse object shapes
  risks the same deopt.

---

## 8. 4D (grammar-sugar) — the deepest unification, and its consumer-less-substrate risk

4D says combinators are a construction API over the same HIR the grammar plane uses (LOCK-5's
continuum); parse-that keeps its public API and *inherits the optimizer*; "the combinator engine
is the generator's runtime in disguise." Prior art: Parsley is *precisely* this shape in Haskell
(one deep-embedded tree serves both the combinator front-end and the staged code-gen back-end),
so 4D is not speculative. But two constitution clauses cut against a naive 4D:

- **Clause i exempts combinators from grammar-totality** ("save primitives and combinators").
  4D risks re-subordinating combinators *to* the grammar HIR — which is coherent only if the HIR
  is genuinely the shared substrate (LOCK-5's verdict), not if it forces combinators to become
  grammar-generated (forbidden).
- **Y.13 consumer-less-substrate (clause viii):** if the combinator engine becomes "the
  generator's runtime in disguise," it must retain a *live standalone consumer* (parse-that's
  public API + the three consumer suites value.js/latex-paper/keyframes.js) — otherwise 4D builds
  a substrate whose only consumer is the generator, which the constitution forbids in designs as
  in code. The R4 consumer census (value.js imports `Parser/all/any/dispatch/regex/…`,
  latex-paper imports `Parser/ParserState/regex/string/any`) is the standing proof that the
  public combinator API has real, external, non-generator consumers — 4D must preserve exactly
  that surface (PT-3.md §1.1 already commits "bbnf grows no combinator surface and consumes this
  backbone unmodified").

---

## 9. Rust↔TS isomorphism — what the class allows to be shared, and what it can't

The constitution (clause iii) + LOCK-6 want isomorphism "to a reasonable point, measured." The
prior art draws the line sharply:

- **A shared *graph/instruction* shape is realistic** — LPeg proves a language-neutral parsing-
  machine instruction set; 4C's "one instruction set shared by rust and ts" inherits this. 4A's
  "graph shape isomorphic across rust/ts" is the data-structure analogue.
- **A shared *runtime* is not** — the SPEC's own PROVE-14 lesson: byte-exact tape crossing and
  4.4× stop-early **but full-walk is a measured non-goal** (native JS parse is near-optimal; SPEC
  §0/14). V8 laws (monomorphism, hidden classes, megamorphic ICs) and LLVM laws (monomorphization,
  vtable, arena lifetimes) are genuinely different optimization regimes. So the isomorphism that
  survives is **structural (the graph/bytecode + oracle tables), not codegen** — TS emits
  V8-shaped closures, Rust emits monomorphized/NEON code, from a shared analyzable shape. This is
  the 4A/4C sweet spot and the 4B tension (4B's Rust and TS fusers are *different codegen* over a
  shared plan).

---

## 10. Synthesis — what the evidence base already decides, and the open question for pass-2

**Decided by evidence (not open to relitigation):**

1. The 4A→4B→4C pipeline is real and won (Parsley); LPeg fixes the 4C instruction-set reference;
   winnow fixes 4B-Rust as the raw-speed leader; the field has *no* library that is both fully
   monomorphized and fully analyzable — that union (Parsley's staging) is the beyond-SOTA target.
2. The arena/vtable question is **low-reward** (6 vs 5390 allocs) and **sound-or-refuted**; the
   Rust core reform makes **zero perf claim** (§0/09 LAW). The real per-parse alloc mass is
   `many`/`sep_by` Vec growth — the honest 4B target if any.
3. Diagnostics are a **cold, per-parse-scoped, zero-alloc-until-armed** facility (§0/11 + PROVE-11);
   dispatch carried at ~+5%.
4. Packrat is **placed, opt-in, armed-only** — never global (field wisdom + parse-that's own
   design); the graph/VM is its natural placement home.
5. Isomorphism that survives is **structural (shared graph/bytecode + oracle tables), not shared
   codegen** (PROVE-14 full-walk non-goal + V8/LLVM regime split).
6. All perf claims: **cold, paired, multi-window, N≥200; warm benches refused.**

**The open, family-discriminating question for pass-2:** parse-that's **monadic `chain`**
(parser.ts:124; Rust core equivalent) is the fault line. 4A/4B/4C all need the analyzable subset;
Parsley's answer is *drop monad, recover with selective `branch`*. Pass-2 must decide, with the
consumer grammars in hand (value.js/latex-paper/keyframes.js — do they use `chain` for genuinely
data-dependent parsing?), whether to (a) make the combinator algebra selective + treat `chain` as
a named opaque escape, or (b) keep monadic power and cap the static analysis at applicative/
alternative nodes (a smaller, honest optimizer). This is the hinge on which 4A/4B/4C's beyond-SOTA
claim stands or falls — and the constitution's "compilation need not be naive" is the licence to
resolve it, not dodge it.

---

### Sources

Codebase: PORTFOLIO.md (LOCK-4 charter + CONSTITUTION); SPEC.md §0 (certified swath + refutation
laws), §1–§4; R3 (parse-that Rust: SmallBox core parse.rs:61, alloc hotspots §5, scanner census
§2/§4); R4 (parse-that TS: combinator core §2, V8 surfaces §6, consumers §7); PROVE-09 REPORT+
VERIFY (de-nightly/arena, REFUTED perf); PROVE-11 REPORT+VERIFY (lazy diagnostic alloc,
CONFIRMED_DELTA); PT-2.md (Rust core reform, W4 arena); PT-3.md (TS backbone 1.1.0); INDEX.md.

Web: Parsley — [ACM 3409002](https://dl.acm.org/doi/10.1145/3409002) ·
[ICFP PDF](https://mpickering.github.io/papers/parsley-icfp.pdf) ·
[design patterns](https://www.cs.tufts.edu/~nr/cs257/archive/jamie-willis/parsing-patterns.pdf) ·
[hackage](https://hackage.haskell.org/package/parsley). Selective functors —
[ACM 3341694](https://dl.acm.org/doi/10.1145/3341694) ·
[hackage: selective](https://hackage.haskell.org/package/selective). LPeg —
[A Parsing Machine for PEGs](https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf) ·
[gpeg](https://github.com/zyedidia/gpeg) · [Yedidia thesis](https://zyedidia.github.io/notes/yedidia_thesis.pdf) ·
[LPegLJ](https://github.com/sacek/LPegLJ). Rust field —
[winnow](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/) ·
[chumsky docs](https://docs.rs/chumsky/latest/chumsky/) · [chumsky repo](https://github.com/zesterer/chumsky) ·
[combine](https://docs.rs/combine) · [bumpalo](https://github.com/fitzgen/bumpalo). Packrat/LR —
[Warth 2008](https://web.cs.ucla.edu/~todd/research/pepm08.pdf) ·
[Medeiros LR in PEGs](https://arxiv.org/pdf/1207.0443) ·
[linear-time packrat LR 2026](https://www.arxiv.org/pdf/2601.05012). V8 —
[IC design for TS (ACM 3428242)](https://dl.acm.org/doi/pdf/10.1145/3428242) ·
[mrale.ph monomorphism](https://mrale.ph/blog/2015/01/11/whats-up-with-monomorphism.html).

# LOCK-4 — THE COMBINATOR CORE · pass-1 PRIOR-ART

*parse-that Rust + TS combinator algebra, beyond-SOTA. No family allegiance — a sweep of
what exists, what won and lost elsewhere for this problem class, which constraints our own
evidence base already imposes, and the traps named in comparable systems. Citations inline.*

---

## 0. The problem class, stated precisely

A combinator core is a set of hand-written primitive parsers plus a composition algebra
(`then`/`or`/`many`/`map`/`chain`/…). The universal tension — named in every source below —
is that **combinators are ergonomic but the composition machinery is slow**: each combinator
is a higher-order function, and "functions are generally not inspectable, so static analyzers
treat all functions as black boxes and are unable to analyze anything that happens past a
monadic bind" (Morpheus, arXiv:2305.07901). Every SOTA answer attacks the *same* seam:
recover the static grammar structure the closures hid, then either specialize it (staging),
lower it (a VM), or analyze it (a graph). The four LOCK-4 families are the four historical
answers, and the literature has already adjudicated many of their trade-offs.

parse-that's two current cores sit at opposite poles of the classic fork:
- **Rust:** `Parser<'a,O>{ parser_fn: SmallBox<dyn ParserFn+'a, S32> }` (R3 §5, `parse.rs:61`)
  — **type-erased boxed closures.** Uniform, but pays dynamic dispatch + a construction box per
  combinator, and carries the arena/ownership question (PT-2 W4).
- **TS:** `Parser<T>` with a `.parser` function-pointer field dispatched megamorphically
  (R4 §6c, `this.parser(state)`), hand-specialized at hot leaves via **arity-unrolling**
  (`any`/`fuseAll` arity-2/3, R4 §6d, `leaf.ts:39-57,183-251`).

Neither is monomorphized in the nom/winnow sense; both are runtime closure graphs. That is
the starting point every family reforms.

---

## 1. The classic fork: monomorphized concrete types vs type-erased closures

The Rust ecosystem has run this experiment to completion.

- **nom / winnow** — fully **monomorphized** concrete combinator types; no `dyn`, no heap box
  on the hot path. Recent JSON benchmarks: "winnow leads the full-JSON benchmark, with nom
  next … clearly ahead of chumsky and pom." Zero-heap-alloc for applicative / same-type
  composition; the escape hatch to `Box<dyn Parser>` is needed only when a `flat_map`/`chain`
  returns *different* parser types (type erasure ⇒ heap + dynamic dispatch).
  [nom](https://github.com/rust-bakery/nom), [combine](https://github.com/Marwes/combine),
  [mono-vs-dispatch](https://users.rust-lang.org/t/monorphization-vs-dynamic-dispatch/65593)
- **The monomorphization tax cuts the other way.** "Monomorphization also comes at a cost: all
  those instantiations need to be compiled separately, which can increase compile time." chumsky
  is the cautionary tale — its fully-generic `Parser<I,O,E,'src>` produces deep nested types
  with notorious compile-time and inference cost.
- **chumsky 1.0 "zero-copy"** is the SOTA of the pure-generic route and is directly instructive:
  it uses **GATs for compile-time specialization** and "automatically detect[s] when an output is
  never used (`.then_ignore(...)`) and avoid[s] generating it at all" — a static dead-output
  analysis in the type system. Same win bbnf's type plane banks (SPEC §0/07 no-value-discard),
  same analysis 4A wants on a graph. [chumsky](https://github.com/zesterer/chumsky),
  [deepwiki](https://deepwiki.com/zesterer/chumsky)

**Verdict the ecosystem reached:** monomorphized-concrete wins raw throughput but loses
compile-time and — decisively for us — has **no TypeScript analog** (V8 does not monomorphize
generics; there is no concrete `Then<A,B>` type to emit). parse-that's type-erased closure graph
is therefore the correct *shared substrate* for rust/ts isomorphism (Constitution iii), and the
reform must lift performance *without* returning to a Rust-only monomorphized tower. This is why
4A (closure→graph), 4C (VM), and 4B-TS (build-time fuser) exist, and why a naive "just
monomorphize like nom" is a non-answer under the isomorphism law.

---

## 2. 4B COMPTIME-FUSION — staging is the proven, decades-deep answer

The single most load-bearing body of prior art for LOCK-4. The result is unambiguous: **the
grammar's static structure can be partially-evaluated away, yielding hand-written-quality code.**

- **Jonnalagedda, Coppey, Stucki, Rompf, Odersky — "Staged Parser Combinators for Efficient
  Data Processing", OOPSLA 2014.** Staging (LMS, runtime code generation) "dissociate[s] input
  parsing from parser composition, and eliminate[s] intermediate data structures and computations
  associated with parser composition at staging time." **Named challenge (live for us):**
  "maintain support for input-dependent grammars, which have no clear stage distinction" — i.e.
  monadic `chain`/`flat_map` resists staging. [dl.acm](https://dl.acm.org/doi/10.1145/2714064.2660241),
  [author](http://manojo.github.io/2015/09/02/staged-parser-combinators)
- **Willis, Wu, Pickering — "Staged Selective Parser Combinators", ICFP 2020 (Parsley).** The most
  direct blueprint for parse-that's *TS* side: "compile parser combinators to parsers of
  hand-written quality by … representing [the grammar] as a **tree**." To do so it **drops monadic
  computation** (which "generates dynamic structure") and recovers the lost power with **selective
  functors**; the tree is partially evaluated via Typed Template Haskell. This *is* "a build-time
  fuser emits specialized chains" (4B-TS charter), generalized past parse-that's hand arity-unroll.
  [parsley-icfp.pdf](https://mpickering.github.io/papers/parsley-icfp.pdf),
  [ICFP20](https://icfp20.sigplan.org/details/icfp-2020-papers/20/Staged-Selective-Parser-Combinators)
- **Krishnaswami & Yallop — "A Typed, Algebraic Approach to Parsing" (PLDI 2019) + "flap: A
  Deterministic Parser with Fused Lexing" (PLDI 2023).** Types certify a grammar unambiguous/
  one-pass, then generate deterministic code with *fused* lexing — the type plane as the enabling
  analysis, again the bbnf pattern. [flap.pdf](https://www.cl.cam.ac.uk/~nk480/flap.pdf),
  [parsing.pdf](https://www.cl.cam.ac.uk/~nk480/parsing.pdf)

**Won:** staging + a static grammar *tree* + dropping/deferring monadic bind. **Lost:** naive
monadic combinators — they cannot be staged and stay black boxes (Morpheus). **Trap for LOCK-4:**
parse-that ships monadic-shaped combinators (`chain` parser.ts:124-144; the Rust `ParserFn`
closure). A 4B/4A design promising full graph analysis must confront that `chain` is an
input-dependent stage boundary — either restrict it (Parsley's selective move) or accept an
opaque node there. A REAL expressiveness cost to name, not wave away.

---

## 3. 4C VM-CORE — the compact-bytecode answer (and the isomorphism dividend)

- **Medeiros & Ierusalimschy — "A Parsing Machine for PEGs", DLS 2008.** The canonical PEG bytecode
  VM: `Char, Jump, Call, Return, Choice, Commit, Fail, TestChar, TestCharset`. Grammar → compiled
  bytecode → run in a VM. `TestChar`/`TestCharset` prevent call-stack churn on predictive entry
  (the O(1)-dispatch idea parse-that hand-codes in its `Int8Array(128)` dispatch, R4 §6d).
  [ry08-4.pdf](https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf),
  [dl.acm](https://dl.acm.org/doi/10.1145/1408681.1408683)
- **LPeg** (Ierusalimschy, SPE 2009) is the production embodiment; **gpeg** (Yedidia, SLE 2021)
  extends the same machine to **incremental** parsing — evidence the VM route uniquely enables
  streaming/incremental (LOCK-9 9D). [lpeg-spe.pdf](https://www.cs.tufts.edu/~nr/cs257/archive/roberto-ierusalimschy/lpeg-spe.pdf),
  [gpeg](https://github.com/zyedidia/gpeg), [gpeg_sle21.pdf](https://people.seas.harvard.edu/~chong/pubs/gpeg_sle21.pdf)
- Lineage: the same "VM approach" Russ Cox popularized for regex (Thompson/Pike), so 4C ties
  cleanly to LOCK-5's regex continuum — one machine could host both.

**Decisive dividend for the constitution:** a shared instruction set gives **layout isomorphism
by construction** (4C charter) — the one design where rust/ts parity is *physical*, not maintained
by a linter (contrast 4A's "graph shape isomorphic across rust/ts", which still needs two
implementations). **What the VM loses:** an interpreter dispatch loop is slower than monomorphized
straight-line code; LPeg is fast but not nom-fast. The charter's own guard is right: 4C "measured
against the closure core, not assumed." Honest expectation: *compactness + shared layout +
streaming*, not raw-throughput supremacy.

---

## 4. 4A CLOSURE-TO-GRAPH / 4D GRAMMAR-SUGAR — defunctionalization & the shared HIR

- **Danvy & Nielsen — "Defunctionalization at Work" (BRICS RS-01-23).** The formal tool: convert
  higher-order functions into inspectable first-order data structures. "A regular expression
  matcher can be defunctionalized … into a version where functions are converted into inspectable
  data structures … [making] static analysis possible." This *is* 4A: opaque nested closures →
  analyzable graph nodes. [BRICS-RS-01-23.pdf](https://www.brics.dk/RS/01/23/BRICS-RS-01-23.pdf)
- **Morpheus (arXiv:2305.07901)** confirms payoff and boundary: composable *effects* make
  combinator programs tractable to analyze/verify; the wall is still the monadic bind.
- **The applicative/selective discipline** (Parsley §2; "Design Patterns for Parser Combinators",
  Haskell 2021) keeps the graph static: applicative parsers "always have a purely static
  structure," so fusion / packrat-placement / diagnostics become graph passes. 4D's "combinators
  are a construction API over the same HIR the grammar plane uses" is the same idea one level up —
  parse-that keeps its public API, the graph *is* the generator's IR (consumes LOCK-5's continuum;
  aligns with bbnf's existing HIR/e-graph home).

**Won:** applicative/selective + defunctionalized nodes ⇒ analyzable graph, ⇒ fusion, **selective
packrat placement**, and diagnostics all become graph analyses over one shape shared by rust/ts.
**Trap:** same monadic-bind wall (§2) — any `chain` node is an analysis boundary; the graph is
only as static as the algebra is selective-not-monadic.

---

## 5. Packrat — the literature is emphatic, and our default already agrees

The charter treats packrat as a *placement* decision (a graph analysis). Prior art strongly
validates **selective, opt-in** memoization and refutes blanket packrat:

- "Full memoization is **always much slower** than plain recursive descent parsing, which never
  triggered the exponential worst case in any of their tests." "Memoizing only a few selected
  nonterminals may yield a speedup … memoization does not serve as a performance optimization, but
  as a **safeguard against pathological worst-case** scenarios which are rare in practice."
- **LPeg deliberately uses a backtracking parsing machine, NOT packrat, to avoid the memory cost.**
  [bford.info/packrat](https://bford.info/packrat/), [wiki](https://en.wikipedia.org/wiki/Packrat_parser)
- Left-recursion support (parse-that's WDM packrat, R4 §1 `packrat.ts`) is Warth-Douglass-Millstein;
  a correctness feature, not a speed feature.

**Our evidence base already embodies the verdict:** parse-that ships packrat **OFF by default**,
arming permanently only at first `memoize()` (`PACKRAT_ARMED`, R4 §6g / packrat.ts:290) — a true
no-op on the LL(1) hot path. Any LOCK-4 design that memoizes eagerly or benches "packrat on" as the
default lane contradicts both the literature and the existing engine. **Binding gate law (PT-3
§1.2):** every perf table states armed-vs-unarmed.

---

## 6. Diagnostics / error recovery — labeled failures is the settled mechanism

- **Medeiros, Mascarenhas et al. — labeled failures + recovery expressions** ("Syntax Error
  Recovery in PEGs", arXiv:1806.11150; "Automatic Syntax Error Reporting and Recovery",
  arXiv:1905.02145). A label reports *and* carries a recovery expression to a synchronization
  point; automatic annotation (Standard vs Unique) reaches ~41-76% acceptable recovery. IDEs need
  trees for invalid input — the motivating case. [arXiv:1806.11150](https://arxiv.org/abs/1806.11150),
  [towards-automatic](https://arxiv.org/html/2507.03629)
- chumsky's rich recovery and parse-that's `recover` (Rust `recover.rs`; TS `recover` parser.ts:653)
  are this pattern in practice. PT-E1 per-parse-scoped diagnostics (PT-3 W2) is the *scoping* half;
  labeled-failure recovery is the *mechanism* half the graph can carry as node metadata (a 4A
  diagnostics-point analysis).

---

## 7. Constraints our OWN evidence base already imposes (binding, measured)

Not literature — certified facts of this campaign; they outrank any external "win."

1. **PROVE-09 REFUTED — the shim self-deception is LAW (SPEC §0/09).** The Rust
   `portable_simd`→NEON de-nightly's claimed **1.34-1.57×** did **not reproduce** on fresh paired
   interleaving; window A was uniformly the *other way* (0.81-0.98×). Instruction census disproved
   the mechanism: patched **2271 `addv.8b` vs 509** baseline — the shim did *more* reduction work.
   ⇒ **PT-2 makes ZERO perf claim; value = stability.** Any LOCK-4 perf lever (4B fusion, 4C VM)
   inherits this: **paired interleaving mandatory even for "obvious" wins**; two-arm window
   asymmetry is the classic self-deception. Warm benches are disinformation.
2. **PROVE-09 arena analysis — reward ceiling measured and small.** The vtable `SmallBox<dyn>`
   boxes are **6 construction allocs / 5960 B**, paid once *outside* the timed loop; the real mass
   is **5390 per-parse allocs** (Vec growth in `many`/`sep_by` + string arena), which an arena
   **cannot touch**. The construction arena is high-risk, low-reward *by measurement*. **Named
   soundness trap:** `ParserFn<'a>` is parameterized by the input lifetime; a `'static`/thread-local
   arena holding `'a`-closures is **unsound** — the arena must be `'a`-scoped and threaded through
   all 86 `Parser::new` sites, or **not land at all** (owner edict; PT-2 W4 = sound landing OR
   filed refutation).
3. **PROVE-11 CONFIRMED_DELTA — the honest-number law.** The TS lazy-diagnostic-alloc lever is real
   (emitted-JS eager-alloc census **6→0**; fused alloc **−60.1%** exact) but the **dispatch lane
   halved on re-measure** (+10.2% claimed → **~+5%** measured, misses the ≥10% gate). ⇒ PT-3 carries
   dispatch at ~+5% no-regression everywhere; inflating it is the §0/09 self-deception. The
   mechanism (fixed-from-birth hidden class, `undefined` not `[]`) is the correct V8 idiom and a
   do-not-regress invariant (PT-3 W4 same-map oracle).
4. **Refutation laws (Constitution iv / SPEC §0):** no digit-scan SIMD (02 NO_GO, CPI inversion
   0.13→0.30), no block-scheduling pursuit (04, M-class OoO already pipelines the single block), no
   stable-port perf claims (09), fold-over-cursor dead (2.3-4.9× behind), warm benches
   disinformation, **x86 dead** (4-layer abrogation reaches parse-that at PT-2 W1/W5).
5. **Constitution structural laws:** combinators are the *exception* to grammar-totality ("save
   primitives and combinators", clause i) — lawful hand-written primitives, but their *compilation
   and analysis need not be naive* (LOCK-4 task). Type plane costs **zero at runtime** (v).
   Colocation + extreme parsimony, **no god modules** (vi) — parse-that's `utils.ts` (TS) and the
   flat `simd128.rs` (Rust) are named god-module smells the reforms split (PT-2 W1 `simd/`, PT-3 W2
   `diagnostics/`). Consumer-less substrate forbidden (Y.13, viii): value.js / latex-paper (0.7.1
   skew!) / keyframes.js are the close gates for every TS wave.

---

## 8. The traps, named (for the synthesis fleet)

- **T1 — the monomorphization mirage.** "Just do it like nom/winnow" gives raw speed but has **no
  TS analog** and explodes Rust compile time (chumsky). Rejected by the isomorphism law.
- **T2 — the monadic-bind wall.** Staging/graph analysis (4A/4B/4C) cannot see past `chain`/
  `flat_map` (Morpheus; Jonnalagedda's "no clear stage distinction"; Parsley's monad-drop). Any
  full-analysis promise must state its treatment of parse-that's `chain`. Not wave-away-able.
- **T3 — the window-asymmetry self-deception.** PROVE-09's whole refutation. Every LOCK-4 number
  ships paired-interleaved, two windows, opposite arm order, or it is not evidence.
- **T4 — the arena that looks sound.** A `'static` arena or lifetime `transmute` passes the borrow
  checker via `unsafe` and appears to work. Sound-or-refuted, never sound-appearing (PT-2 W4,
  MIRI-clean gate). Reward ceiling is 6 allocs — do not oversell it.
- **T5 — eager packrat.** Benching packrat-on as default, or memoizing everything, contradicts the
  literature (full memoization "always slower") and the shipped default (armed-off). Selective
  placement only; state armed/unarmed in every table.
- **T6 — VM throughput overclaim.** A bytecode VM buys compactness + shared layout + streaming,
  **not** nom-throughput. Measure against the closure core; a loss is a named negative result.
- **T7 — the honest-number temptation.** Reading +8% dispatch in one favorable window and reporting
  "near gate." The honest carry is ~+5% (PROVE-11 VERIFY). Over-claim trips the challenge.

---

## 9. Synthesis pointer — where the families actually converge

The literature does **not** pit 4A/4B/4C as rivals so much as *layers*: a **defunctionalized static
graph** (4A, Danvy) is the shared IR; **staging** (4B, Parsley/Jonnalagedda) specializes it where a
stage boundary is clean; a **VM** (4C, LPeg) is the fallback lowering that guarantees rust/ts layout
isomorphism and unlocks streaming; **4D** says that graph is just the generator's own HIR wearing the
combinator API. The selective-functor discipline (§4) and the monadic-bind wall (T2) are the hinge
deciding how much of parse-that's algebra each layer reaches. The constitution's isomorphism demand
(iii) and the measured arena ceiling (§7.2) push away from a Rust-only monomorphized tower and toward
the shared-graph / shared-VM center — which is also the one that ties LOCK-4 to LOCK-5 (regex
continuum) and LOCK-6 (the isomorphism plane).

### Sources
- Jonnalagedda et al., Staged Parser Combinators, OOPSLA 2014 — https://dl.acm.org/doi/10.1145/2714064.2660241
- Willis, Wu, Pickering, Staged Selective Parser Combinators, ICFP 2020 — https://mpickering.github.io/papers/parsley-icfp.pdf
- Krishnaswami & Yallop, flap: Deterministic Parser with Fused Lexing, PLDI 2023 — https://www.cl.cam.ac.uk/~nk480/flap.pdf
- Krishnaswami & Yallop, A Typed, Algebraic Approach to Parsing — https://www.cl.cam.ac.uk/~nk480/parsing.pdf
- Medeiros & Ierusalimschy, A Parsing Machine for PEGs, DLS 2008 — https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf
- Ierusalimschy, LPeg (PEG pattern-matching), SPE 2009 — https://www.cs.tufts.edu/~nr/cs257/archive/roberto-ierusalimschy/lpeg-spe.pdf
- Yedidia, Fast Incremental PEG Parsing (gpeg), SLE 2021 — https://people.seas.harvard.edu/~chong/pubs/gpeg_sle21.pdf
- Danvy & Nielsen, Defunctionalization at Work, BRICS RS-01-23 — https://www.brics.dk/RS/01/23/BRICS-RS-01-23.pdf
- Morpheus: Automated Safety Verification of Parser Combinator Programs, arXiv:2305.07901 — https://arxiv.org/pdf/2305.07901
- chumsky (zero-copy, GAT dead-output elimination) — https://github.com/zesterer/chumsky · https://deepwiki.com/zesterer/chumsky
- nom / winnow / combine (monomorphized combinators; JSON bench) — https://github.com/rust-bakery/nom · https://github.com/Marwes/combine
- Monomorphization vs dynamic dispatch (Rust forum) — https://users.rust-lang.org/t/monorphization-vs-dynamic-dispatch/65593
- Ford, Packrat Parsing & PEGs — https://bford.info/packrat/ · https://en.wikipedia.org/wiki/Packrat_parser
- Medeiros & Mascarenhas, Syntax Error Recovery in PEGs (labeled failures), arXiv:1806.11150 — https://arxiv.org/abs/1806.11150
- Automatic Syntax Error Reporting and Recovery in PEGs, arXiv:1905.02145 — https://arxiv.org/abs/1905.02145
- Internal (binding): SPEC §0 refutations; PROVE-09 REPORT+VERIFY (shim REFUTED, arena ceiling); PROVE-11 REPORT+VERIFY (lazy-alloc CONFIRMED_DELTA); R3 §2/§5 (Rust core); R4 §6 (TS V8 surfaces); PT-2 / PT-3 tranche docs.
</content>

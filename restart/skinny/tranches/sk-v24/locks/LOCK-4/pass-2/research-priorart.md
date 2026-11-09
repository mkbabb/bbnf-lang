# LOCK-4 · THE COMBINATOR CORE — PASS-2 PRIOR-ART RESEARCH

Scope: the combinator algebra at beyond-SOTA — allocation model, fusion, packrat, diagnostics,
rust/ts semantic parity — decided on two open axes: **Q-runtime** (runtime graph / bytecode / nothing)
× **Q-ownership** (parse-that-private vs shared `bbnf_ir`). No family carries a verdict here. Every
citation is external URL or verified local `path:line`. No builds run.

---

## 0. THE PROBLEM CLASS

"Compile a combinator algebra to hand-written-quality parsers, in two host languages that obey
**opposite optimization laws** (LLVM monomorphization vs V8 hidden-class/IC), off one analyzable
representation." Four candidate compilation strategies map onto a decades-old literature split that
**already fractured along exactly the Q-runtime axis**:

- **Reify-and-stage** (runtime graph → analysis → specialized code): Parsley / staged selective
  combinators, `optimizing parser combinators`.
- **Monomorphize** (nothing survives to runtime; the types ARE the parser): nom / winnow / chumsky
  in Rust.
- **Lower to a parsing VM** (a bytecode program survives): LPeg's parsing machine, GPeg, Medeiros
  parsing machine.
- **Grammar-sugar over a shared IR**: PEG generators (pegjs/peggy), and — decisively — parse-that's
  OWN TS BBNF compiler, which already realizes build-time graph analysis feeding combinator emission.

---

## 1. CONSTRAINTS OUR EVIDENCE BASE ALREADY IMPOSES (binding, not open)

These are settled by measurement or by the Constitution; a pass-2 design that reopens them is a
defect, not a contribution.

- **The runtime-switch VM is DEAD on V8 — MEASURED, not asserted.** parse-that's own A.W3 built a
  `SpanParser` tagged discriminated union with a numeric-tag `switch` dispatcher (the direct V8
  analog of a bytecode VM / of Rust's `enum`-over-`Box<dyn>` win) and measured it **10–14% SLOWER**
  than monomorphic closure dispatch across three CSS-value workloads, adversarially re-run at −14%,
  then KILLED (`parse-that/docs/future-research.md:71-109`). The recorded law: "do not re-attempt the
  runtime-switch SpanParser approach on V8 without a fundamentally different encoding (e.g., a flat
  array-of-ops with a **generated specialized dispatcher** — not a recursive runtime `switch`)."
  → This is the single most load-bearing constraint on **4C (VM-core)**: a shared ISA that decodes
  via `switch` in TS is refuted *in this codebase*; only a build-time-specialized decoder survives.
  The Rust↔V8 asymmetry is explicit: "the OPPOSITE of the Rust `enum`-vs-`Box<dyn>` regime."

- **The de-nightly/arena perf claim is REFUTED and LAW (SPEC §0-09).** PROVE-09 claimed 1.34–1.57×
  from `portable_simd→NEON` + smallbox; the verify fleet found neutral-to-slower under paired
  interleaving, and `otool` showed the shim emits *more* reduction work (2271 vs 509 `addv.8b`)
  — the proposed mechanism is absent (`PROVE-09/VERIFY.md`). Standing lesson: **two-arm window
  asymmetry is the classic self-deception; paired interleaving is mandatory even for "obvious" wins.**
  The de-nightly PORT survives only as *stability*, zero perf claim (SPEC §0).

- **The arena reward ceiling is known and small (measured).** PROVE-09's counting allocator: JSON
  construction = **6 allocs**; parse twitter = **5390 allocs** (Vec growth in `many`/`sep_by` +
  string arena). The vtable arena moves only the 6, never the 5390 (`PROVE-09/REPORT.md`;
  `PT-2.md:311-356`). **The alloc mass is per-parse Vec growth, not vtable boxing** — any allocation
  story for 4B that targets the vtable is aiming at 0.1% of the mass.

- **The arena is soundness-blocked, not effort-blocked.** `ParserFn<'a>` is parameterized by the
  INPUT lifetime; a `'static`/thread-local arena holding `'a`-closures is *unsound*; sound threading
  touches all **86 `Parser::new` sites** and the `Parser` type. Owner edict: **"lifetime-sound or not
  at all"** — a measured refutation is a legitimate close (`PT-2.md:323-352`).

- **Lazy diagnostics is CONFIRMED_DELTA, not the headline.** PROVE-11's lazy `suggestions`/
  `secondarySpans` (symmetric with the pre-existing lazy `expected`) reproduces: fused +15.6%,
  unfused +13.6%, fused alloc **−60.1% exact** — but the **dispatch lane is ~+5%, NOT +10%**
  (`PROVE-11/VERIFY.md`). The diagnostics-alloc lever is real; the honest carry is ~+5% on dispatch,
  and over-claiming trips the W6 audit (`PT-3.md:210-218`).

- **Constitutional floors:** combinators remain lawful hand-written primitives (PORTFOLIO LOCK-4
  head); the type plane costs zero at runtime (v); no god modules / name-gates (vi); concrete
  artifacts or SPEC-ONLY (vii); consumer-less substrate forbidden (viii); rust/ts isomorphism only
  **to a reasonable point, measured** (iii — LOCK-6 owns the boundary). x86 dead, warm benches are
  disinformation (iv).

- **Settled substrate facts (DIRECTIVES, do not re-derive):** combinator node set is a verified
  near-bijection with `bbnf_ir::IrNode` (14 variants); `parse_with_ir` live at
  `interpreter/mod.rs:373`; `bbnf_ir` already ships a **live value-producing bytecode VM**
  (`vm/{bytecode,compiler,interpreter}`) + a MessagePack bus; `FnDescriptor` carries **no**
  host-closure variant (a code-verified vocabulary gap); value-plane `Box<dyn Fn>` indirection
  confines to scalar leaves, structural spine inlines.

---

## 2. PRIOR ART BY AXIS — WHAT WON, WHAT LOST, ELSEWHERE

### 4A — closure-to-graph (reify combinators as analyzable nodes)

**This won, in this exact codebase, on the TS side — and it is prior art, not hypothesis.**
parse-that's TS BBNF compiler already reifies the grammar into "a directed graph of Parser nodes"
and runs graph analyses at build time before emitting closures
(`parse-that/docs/perf-optimization-ts.md:102-169`), lifting 746→~4,800 ops/s:
- **Tarjan SCC for build order** — only genuine cycles get `lazy()` wrappers (JSON: ~8→exactly 2),
  each eliminated wrapper removes a closure-resolution + indirect call per invocation.
- **FIRST-set dispatch tables** — `Int8Array(128)` LL(1) lookahead computed from the grammar's FIRST
  sets; "formally equivalent to the LL(1) lookahead table in classical parser theory, but computed
  at runtime from the grammar." O(1) alt-dispatch when `isPerfect` (disjoint FIRST, non-nullable).
- **Regex coalescing, alias-chain collapsing, inline all(2)** — all graph-rewrite passes on the node
  graph. This is 4A's thesis (fusion/packrat/diagnostics-placement as *graph analyses*) already
  banked and measured.

External lineage: **Parsley / "Staged Selective Parser Combinators"** (Willis, Wu, Pickering, ICFP
2020) is the canonical statement — represent the parser as a *tree*, exploit the static grammar
structure, but you must **drop monadic bind** (it "generates dynamic structure that is hard to
optimize") and recover the lost expressiveness with **selective functors**
([mpickering.github.io/papers/parsley-icfp.pdf](https://mpickering.github.io/papers/parsley-icfp.pdf);
[dl.acm.org/10.1145/3409002](https://dl.acm.org/doi/10.1145/3409002)). **Trap for us:** parse-that's
`chain()`/`mapState()` are monadic (value-dependent continuation, R4 §2, `parser.ts:124-187`); a
fully-analyzable graph must special-case or forbid these — the ICFP-2020 monad tax is real. Willis'
"Design Patterns for Parser Combinators" is the practical companion
([cs.tufts.edu/~nr/cs257/archive/jamie-willis/parsing-patterns.pdf](https://www.cs.tufts.edu/~nr/cs257/archive/jamie-willis/parsing-patterns.pdf)).

Cross-lock steering (DIRECTIVES): the `fuse(Graph)→Graph` pass must **consume** the in-tree
equality-saturation engine (`egraph`/`egraph-derive`/`csp-solver`, live in `bbnf-regex` per R3 §3),
not clone one — LOCK-2's e-graph home.

### 4B — comptime-fusion (monomorphize; nothing survives to runtime)

Rust prior art is settled and shipping: **nom/winnow** achieve zero-cost via generic
monomorphization + inlining; **winnow** is positioned as the fastest Rust combinator lib and leads
full-JSON benches, nom next
([epage.github.io/blog/2023/07/winnow-0-5](https://epage.github.io/blog/2023/07/winnow-0-5-the-fastest-rust-parser-combinator-library/);
[rustprojectprimer.com/ecosystem/parsing](https://rustprojectprimer.com/ecosystem/parsing.html)).
**chumsky** trades some speed for ergonomics + error recovery. The static-vs-dynamic dispatch tax is
well-documented: `dyn Trait` incurs a vtable lookup and "dispels the magic of inlining"; generics
monomorphize to zero runtime overhead at a binary-size cost
([dev.to/kanywst rust-traits-deep-dive](https://dev.to/kanywst/rust-traits-deep-dive-static-vs-dynamic-dispatch-3eol)).
parse-that today boxes the vtable (`SmallBox<dyn ParserFn>`, `parse.rs:61`) — the monomorphized
`Seq/Alt`-struct route is the disasm-gated mechanism to remove it (DIRECTIVES 4B). **But the two
measured constraints above bound the reward:** (a) the alloc mass is per-parse Vec growth, not
boxing (5390 vs 6); (b) end-to-end paired benching against the *shipped tuned parser* with production
leaf kernels on both arms is mandatory — the PROVE-09 window-asymmetry trap is precisely how a
monomorphization "win" gets faked. **Named open cost:** monomorphization/generated-size blowup across
value.js's 59 `all()` + hot 3-tuples (R4 §6d) — fusion policy needs a fuse-arity / dispatch-fanout /
boxing-depth cap measured against a size budget (Constitution "generated-size budget").

TS has **no monomorphization**; the V8 analog is a *build-time emitter* — and parse-that already
ships it: arity-unrolled `any`/`fuseAll` (arity-2/3 specialized, `leaf.ts:39-57,183-251`), the
`dispatch` `Int8Array(128)` table (`leaf.ts:100-151`), string len-1 charCode + sticky-regex zero-alloc
fast paths (R4 §6d). The mechanism is *generated specialized closures*, NOT runtime dispatch — which
is exactly what the SpanParser refutation (§1) says is the only survivable TS form.

### 4C — VM-core (a bytecode program survives to runtime)

External winner: **LPeg's parsing machine** (Medeiros & Ierusalimschy, DLS 2008) — each PEG expression
compiles to a program of a stack VM (PC + subject registers + a stack of call/backtrack frames);
compound programs are combinations of subexpression programs; backtracking = pushed backtrack frames
(exception-handling-like)
([inf.puc-rio.br/~roberto/docs/ry08-4.pdf](https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf);
[dl.acm.org/10.1145/1408681.1408683](https://dl.acm.org/doi/10.1145/1408681.1408683)). **GPeg**
extends it with incremental parsing ([github.com/zyedidia/gpeg](https://github.com/zyedidia/gpeg)).
This is the canonical "layout-isomorphism-by-construction" argument for a shared rust/ts ISA.

**But the V8 refutation (§1) is decisive against a naive port:** a recursive `switch` decoder loses
to closures on V8 by 10–14%. The escape route parse-that's own doc names is a **flat array-of-ops
with a build-time-generated specialized dispatcher** — which converges 4C toward 4B (generate the
decoder, don't interpret). Cross-lock/abrogate-before-patch (DIRECTIVES 4C): a *private* bytecode ISA
must be adjudicated against the **already-live `bbnf_ir` VM** (`vm/{bytecode,compiler,interpreter}`)
— Constitution viii forbids a consumer-less second VM. The DIRECTIVES keystone is the **first
cross-language number**: a ~200-LOC TS switch-loop decoding a Rust-serialized bytecode program, dual
oracle (opcode-stream identity + parse identity) — and that decoder must be measured against the
closure core, expecting the −10% V8 penalty unless build-time-specialized.

### 4D — grammar-sugar over shared IR (combinators as construction API over the same HIR)

Prior art: PEG/packrat generators expose a grammar surface over a shared internal form. The **packrat
memoization** literature is the load-bearing part: Ford (ICFP 2002) — linear-time via full
memoization ([bford.info/pub/lang/packrat-icfp02.pdf](https://bford.info/pub/lang/packrat-icfp02.pdf));
**selective memoization** (memoize only the nonterminals a static analysis says need it — full
packrat over-memoizes and blows cache); **Pika parsing** (Hutchison 2020) reformulates packrat as
bottom-up dynamic programming, solving left-recursion AND error-recovery together
([arxiv.org/abs/2005.06444](https://arxiv.org/abs/2005.06444)); Warth/Medeiros for left-recursion in
packrat/PEG ([arxiv.org/pdf/1207.0443](https://arxiv.org/pdf/1207.0443)). parse-that already has an
**opt-in WDM packrat with left-recursion memoization**, OFF by default, arming permanently at first
`memoize()` (`packrat.ts`, R4 §6g; PACKRAT_ARMED latch) — so the LL(1) default path pays nothing.
The 4D question (DIRECTIVES) is where *state-effecting* combinators (`peek`/`lookAhead`/`recover`/
`mapState`) and **host-closure semantics** (the `FnDescriptor` gap) live in the IR vocabulary — a
measured minimal lawful variant set, tracking LOCK-5's substrate ruling + LOCK-7's host-fn-lattice as
hard DAG predecessors (do not resolve here).

**Named negative-result risk (DIRECTIVES 4D):** materialization tax. Arbitrary `.map`/projection
output reconciling with a tape-first runtime may lose to closures on the LL(1) shape — "name the
negative result if closures win." Local echo: parse-that measured BBNF-JSON at 540 MB/s vs
hand-rolled 926 MB/s (0.58×), attributing the gap to `lazy()` indirection + double-dispatch trim +
sep_by overhead — i.e. the graph-sugar path has a real, measured tax to close
(`parse-that/docs/future-research.md:127-139`).

---

## 3. TRAPS NAMED IN COMPARABLE SYSTEMS (checklist for pass-2 design)

1. **Window-asymmetry self-deception** (PROVE-09/VERIFY) — the canonical way a fusion/arena/VM "win"
   is faked. Paired A,B,A,B interleaving, two windows, opposite arm order, `otool`/disasm instruction
   census is mandatory. The DIRECTIVES bench discipline is this law verbatim.
2. **Rust↔V8 law inversion** — the enum/jump-table win in Rust is a *loss* on V8 (SpanParser −14%).
   Any "shared mechanism" claim must produce a **first TS paired number**, not infer from Rust.
   The execution face is unbuilt across ALL four formulations (DIRECTIVES keystone).
3. **Aiming allocation at the vtable** — 6-vs-5390 split: the vtable arena is high-risk, low-reward;
   the real mass is `many`/`sep_by` Vec growth + string arena.
4. **Monad tax on analyzability** (Parsley/ICFP-2020) — `chain`/`mapState` value-dependent
   continuations defeat a fully-static graph; selective functors or explicit host-fn variants are the
   priced escape, not a free one.
5. **Full-packrat over-memoization** — memoize everything and cache pressure eats the linear-time win;
   selective memoization / the arms-only default is the measured posture.
6. **Naive VM interpretation on V8** — recursive `switch` loses; only a build-time-specialized decoder
   (array-of-ops) survives. A second bytecode ISA also risks a consumer-less-substrate violation
   against the live `bbnf_ir` VM.
7. **Generated-size blowup** — monomorphization/unrolling across the full consumer surface (value.js
   59×`all`, latex-paper, keyframes) needs a named size + incremental-compile budget, not JSON alone.
8. **Unsound lifetime smuggling** — a `'static` arena holding `'a`-closures is the named unsoundness;
   sound-or-refuted, never sound-appearing (owner edict).

---

## 4. THE DECISIVE UNBUILT NUMBER (both keystones)

Every family is currently under-determined on the SAME missing evidence: (a) the **common-baseline
runtime fork** — the candidate runtimes (graph / bytecode / monomorphized / closure) over the *same*
grammar with the *same* leaf substrate, measured not inferred; and (b) the **first cross-language
number** — no rust/ts execution face exists for any formulation. A first honest TS paired number
(the ~200-LOC decoder graft of 4C, or the emitted-region indirection isolation of 4B) is high-leverage
for every front and no terminal can be named without it. The prior art says *where to expect the
result* (V8 will penalize interpretation ~10%; Rust monomorphization removes the vtable but not the
alloc mass), but the number itself is owed.

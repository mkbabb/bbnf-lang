# LOCK-5 — THE REGEX CONTINUUM — pass-1 prior-art research

Prior-art researcher, pass 1, no family allegiance. What exists, what won and lost elsewhere
for THIS problem class (a grammar-driven regex engine whose HIR is isomorphic to the grammar
HIR/IR, whose automata construction may be recast as solving, and whose regex literals may
lower into the grammar's own IR), which constraints our own certified evidence already
imposes, and the traps comparable systems have named. Local claims cited `path:line`;
external claims cited by URL.

---

## 0. THE TWO HIRs, READ CONCRETELY (the isomorphism problem is not abstract)

The constitution's clause (ii) — "true isomorphism between the regex engine's HIR and the
bbnf HIR/IR" — has an exact shape at HEAD. Both types were read.

**bbnf `ExprKind`** (`skinny/crates/ir/src/lib.rs:234-271`): arena-indexed
(`ExprId(usize)` into `exprs: Vec<Expr>`, `:33-39`), **span-carrying** (`Expr.span`, `:227`),
`Serialize/Deserialize`, **not** `Eq+Hash`. Variants: `Seq(Vec<ExprId>)`, `Alt{branches,mode}`,
`Repeat{body,min,max}`, `Optional`, `Literal{bytes,case}`, **`Regex{pattern:String}`** (an
OPAQUE escape hatch — the regex is an unparsed string, `:250-252`), `Ref{name,target}`,
`Annotation`, `Project{body,target,ty}`, `Discard{left,right,keep_left}`.

**parse-that `Hir`** (`parse-that/rust/regex/src/hir/mod.rs:216`, R3 §3): **tree-shaped**
(`Box<Hir>`/`Vec<Hir>`), **`Eq+Hash`** (explicitly, to key the per-compile `SaturationCache`),
**no spans**. Variants: `Empty`, `Literal(Vec<u8>)`, **`Class(CharClass)`**, **`Look(Look)`**,
`Repetition`, `Group(Box<Hir>)`, `Concat(Vec<Hir>)`, `Alternation(Vec<Hir>)`.

The overlap is real: Seq≈Concat, Alt≈Alternation, Repeat≈Repetition, Optional≈Repetition{0,1},
Literal≈Literal. The **gaps are the whole design problem**:

- bbnf has grammar-plane variants regex lacks (`Ref`, `Project`, `Discard`, `Annotation`).
- regex has byte-plane variants bbnf lacks (`Class` char-classes, `Look` zero-width assertions,
  `Empty`, `Group`). **bbnf hides ALL of these behind `Regex{pattern:String}`** — today the
  seam is a *string*, and `ir::nullability` already reaches across it via
  `bbnf_regex::analyze(pattern)` (`ir/src/lib.rs:331`) — a degenerate, one-directional,
  stringly-typed functor already in production.
- Representation mismatch: arena+span+Serialize (bbnf) vs tree+Eq/Hash (regex). The
  `Eq+Hash` is load-bearing for the saturation cache; the arena+span is load-bearing for
  diagnostics and IR ids. **A single merged type (5A) fights both invariants at once.**

**The e-graph rewrite library is ALREADY the shared part** (this is 5B's core claim, and it is
not speculative — it exists). parse-that's `bbnf-regex/src/egraph/` (1,253 LOC, R3 §3) does
HIR-level equality saturation (rules: repetition, union, flatten, superset, redundant) and
**depends on the very same `egraph` / `egraph-derive` / `csp-solver` crates bbnf-lang owns**
(patched in via `rust/.cargo/config.toml`, R3 §1 lines 47-49; the crates live at
`bbnf-lang/crates/{egraph,egraph-derive,csp-solver}`, verified present). Any LOCK-5 verdict
inherits a working shared e-graph substrate — the question is what rides on it, not whether it
exists.

**The coupling point to re-home or re-derive**: `classify.rs` (1,148 LOC, the single largest
regex file, R3 §3) — `RegexClass` enum + `canonical_pattern()` + `classify_regex_from_hir`,
which drives `FnDescriptor` specialization on the bbnf side. The task statement's "the 1148-LOC
classify re-derived or re-homed" and "classify becomes an IR analysis pass" (5A) is the concrete
crux: it is today a cross-crate analysis over the regex Hir that feeds bbnf codegen decisions.

---

## 1. WHAT WON AND LOST ELSEWHERE — automata construction (bears on 5C, 5A)

### Derivatives are the measured, SOTA-competitive form of "construction as rewriting"

5C posits "determinization, minimization (Hopcroft as rewrite-to-normal-form), prefilter-tier
selection as e-graph saturation + CSP extraction." The literature's answer to "automata
construction as rewriting to a normal form" is **not** e-graphs — it is **regex derivatives**,
and they are a proven win:

- **Owens, Reppy, Turon, "Regular-expression derivatives re-examined"** (JFP 2009): Brzozowski
  derivatives make lexer generators **"easier to write, more efficient, and more flexible"** than
  the classical RE→NFA→subset→DFA pipeline, and construct the DFA *directly* (derivative modulo
  similarity/ACI = the DFA state), no separate minimization pass.
  <https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf>
- **Antimirov partial derivatives** yield an NFA that is **consistently smaller** than Thompson's
  (parse-that uses Thompson, `automata/nfa.rs:449`) or Glushkov's, storing **one derivative per
  step** to avoid the intractable full automaton; "next literals" bound the alphabet iteration —
  the same role parse-that's byte-equivalence classes (`sets/equiv.rs`, `compute_byte_classes`)
  already play. <https://www.sciencedirect.com/science/article/pii/0304397595001824>
- **RE# (Microsoft, 2024)**: "High Performance Derivative-Based Regex Matching with Intersection,
  Complement and Lookarounds" — derivatives are now a *high-performance* engine, not just an
  elegance argument, and natively carry the `Look` variant parse-that's Hir already has.
  <https://arxiv.org/pdf/2407.20479>

**The adjudication weight**: derivatives ARE "rewrite-to-a-canonical-form"; the canonicalization
they need (ACI: associativity/commutativity/idempotence of union) is *exactly* what an e-graph
canonicalizes. So 5C's honest, measured substrate is **derivative construction with the e-graph
providing similarity/canonicalization** — which UNIFIES with the extant regex egraph rather than
replacing the classical NFA→DFA→Hopcroft pipeline wholesale. No cited result connects **DFA
minimization itself** to e-graph saturation (searched; the closest, the **eqsat MLIR dialect**
<https://arxiv.org/pdf/2505.09363>, and "Learned Graph Rewriting with Equality Saturation"
<https://arxiv.org/pdf/2407.12794>, are tensor/relational-query, not automata). **5C's
"Hopcroft as e-graph" is genuinely novel and therefore, under constitution (iv/vii), must be
measured head-to-head or shipped as a named negative result — it is NOT a settled win.**

### Regex-level rewriting as optimization is proven (bears on 5C's HIR-canon island)

"Optimizing Regular Expressions via Rewrite-Guided Synthesis" (arXiv 2104.12039) validates
rewrite-driven regex simplification — the layer parse-that's egraph already occupies. The
low-risk, already-real slice of 5C is **HIR canonicalization by saturation** (extant); the
high-risk slice is **automata construction by saturation** (unproven for this class).

---

## 2. WHAT WON AND LOST — regex-as-grammar-fragment (bears on 5D, 5A)

### Scannerless parsing is the 5D precedent — and its named trap is measured

5D ("regex literals lower DIRECTLY into the grammar's own IR — no separate engine on the
generator path") is scannerless / lexerless parsing: one formalism for lexical and phrase
structure. The literature's **named, measured caution**: a scannerless parser "tends to be
**significantly less efficient than a lexer-parser pipeline with regard to both time and
memory**" (Wikipedia/HandWiki, corroborated across the SGLR line,
Economopoulos-Klint-Vinju CC-2009 <https://homepages.cwi.nl/~jurgenv/papers/CC-2009.pdf>;
Local Lexing <https://arxiv.org/pdf/1702.03277> exists precisely as the middle path).
<https://en.wikipedia.org/wiki/Scannerless_parsing>

**Our own evidence makes this trap concrete and binding.** PROVE-10 (CONFIRMED, GO) proved the
DFA + start-state memchr prefilter delivers **8.8-29.5×** on prefilterable regex-leaf scans over
a byte-by-byte grammar loop, compile-neutral, 44/44 (`prototypes/PROVE-10/REPORT.md`). A naive
5D that inlines every regex literal into grammar combinators (Seq/Alt/Repeat over bytes)
**throws that DFA prefilter away** and lands squarely in the scannerless slowdown. The lawful 5D
is therefore *not* "dissolve the engine" — it is "the grammar IR gains a byte-recognizer node
that STILL compiles to the DFA + prefilter." The backend IR already anticipates this: it carries
both `BackendExpr::RegexProgram{pattern,span_kind}` and `Recognizer::SimdScan` today
(`ir/src/lib.rs:389-391, 416-421`) — regex is a *program*, not inlined bytes, and PROVE-10 says
keep it that way. The one-regex-home law (SPEC §1: "parse-that = the one regex home … consumed
not duplicated") reinforces: 5D's "standalone parse-that regex API survives as a thin front over
the same machinery" is the survivable reading; "no engine at all" is refuted by measurement.

### re2c / Ragel — the generated-code-size trap for inlined DFAs

The canonical "regex literal → hardcoded DFA in generated code" generators are **re2c**
(lookahead-TDFA, DFA as conditional jumps, ScienceDirect S266596382030018X) and **Ragel** (FSM
with inline actions on transitions). Their named trap is **generated-code explosion**: one
comparison found re2c emitting ~10K LOC of C where Ragel emitted ~900 for the same regex
(<https://sabotage-linux.github.io/blog/7/>; <https://re2c.org/>; <http://www.colm.net/open-source/ragel/>).
This is *our* `generated_loc_budget` law (MEMORY: generated-size-budget) made external: any 5A/5D
that inlines a DFA per regex leaf must budget generated size or blow the per-tranche line ledger.
Ragel's transition-action model is also the precedent for keeping `->` projections attached to
the automaton rather than inlining them.

---

## 3. WHAT WON AND LOST — one-IR vs lawful-functor (bears on 5A vs 5B)

- **5B (LAWFUL-FUNCTOR) has the strongest external precedent: MLIR / multi-engine crates.**
  MLIR = many dialects, one infrastructure, total lowering passes between them — behavioral
  isomorphism via lowering, not one merged type. The **eqsat MLIR dialect**
  (<https://arxiv.org/pdf/2505.09363>) shows the e-graph living *as a dialect* over which other
  dialects lower — precisely 5B's "the e-graph rewrite library is the shared part, each side
  keeps domain-fit representations." Rust's own **regex-automata** (burntsushi) is the
  same-crate-multiple-representations design: one `Hir`, many engines (NFA, lazy/hybrid DFA,
  one-pass, PikeVM) selected by a meta-strategy (R7 §4) — a functor-over-one-Hir, not one type
  for everything.
- **5A (ONE-HIR) is the totality-maximal but invariant-fighting option.** Merging arena+span+
  Serialize (bbnf) with tree+Eq/Hash (regex) into "one type in one home" must satisfy the
  saturation cache's `Eq+Hash` AND the IR's `ExprId`/span AND grow byte-plane variants
  (`Class`, `Look`) into bbnf `ExprKind` (subsuming the opaque `Regex{pattern:String}`). This is
  achievable but is a large, cross-crate refactor; its win is genuine (the `analyze(pattern)`
  string seam dies), its risk is a god-type or a cache that can't hash spans.
- **5B's "total functor each way" hides a real obligation**: the regex→bbnf direction must handle
  `Class`/`Look`, which bbnf lacks. Either bbnf grows byte-class + look primitives (then 5B ≈ 5A
  minus the merge), or the functor is *partial* on those — but the constitution says LAWFUL/total,
  tested by round-trip oracles. Round-trip oracle testing of HIR functors is standard and cheap;
  the design must state which side owns `Class`/`Look`.

---

## 4. CONSTRAINTS OUR CERTIFIED EVIDENCE ALREADY IMPOSES (binding on every family)

1. **PROVE-10 is the parity floor and the anti-inlining law.** The DFA + memchr prefilter
   (`Dfa::find`, 8.8-29.5×, compile-neutral, 44/44, CONFIRMED — SPEC §0 row 10) must survive any
   LOCK-5 rework. Full-inline 5D refuted by it; the DFA stays reachable from the grammar path.
2. **PT-1 already schedules the prefilter ladder** (PROVE-10 productionized → shufti/Teddy wide
   tier W2 → lazy DFA W3, `docs/tranches/sk-v24/PT-1.md`). LOCK-5's automata verdict must
   *interlock* with PT-1, not fork it — the shufti/Teddy/lazy tiers are the measured extensions,
   and PT-1.W3's lazy DFA is the meta-engine "never a wrong answer / state-cap bail" pattern
   (R7 §4) already specced.
3. **The e-graph + CSP substrate is shared and present** (`crates/{egraph,egraph-derive,
   csp-solver}`; regex egraph patched onto them). 5B's shared-part claim is *confirmed-existing*;
   5C's saturation islands have an engine to run on. Consumer-less substrate is forbidden (Y.13):
   any new e-graph island needs a same-wave consumer.
4. **§0 refutation discipline is the verdict gate for 5C.** "Wins stay, losses are named negative
   results" is 5C's own phrasing AND the constitution (iv/vii). No warm benches; cold paired,
   N≥200 (SPEC §0, §4). The automata-as-saturation claim is measured-or-SPEC-ONLY.
5. **PROVE-10's parsimony lesson is a direct warning to 5C.** PROVE-10 explicitly chose the
   prefilter over lazy determinization "for one-win parsimony … the DFA is untouched"
   (REPORT §1). Recasting the whole NFA→DFA→Hopcroft pipeline as e-graph is exactly the sprawling
   change PROVE-10 declined; it must clear a *measured head-to-head* bar (KISS-perf-bias, MEMORY).
6. **`ir::nullability` already crosses the seam via a string** (`ir/src/lib.rs:331`
   `analyze(pattern)`). Whatever HIR form wins, this call site is the existing, degenerate functor
   — the migration target, and a ready round-trip test anchor (regex nullability oracle already at
   `ir/src/lib.rs:651-677`).

---

## 5. THE TRAPS, NAMED (from comparable systems + our own campaign)

- **Scannerless slowdown** (Wikipedia/SGLR): inlining regex as grammar combinators regresses time
  AND memory — refuted locally by PROVE-10's DFA win. *Guards 5D.*
- **Generated-DFA size explosion** (re2c 10K vs Ragel 900 LOC): inlined per-leaf DFAs blow the
  `generated_loc_budget`. *Guards 5A/5D.*
- **The `Eq+Hash` vs arena+span collision**: one merged HIR must serve the saturation cache and
  the IR-id/diagnostic plane simultaneously. *Guards 5A.*
- **The partial-functor cheat**: a "total functor each way" that silently drops `Class`/`Look`
  on the regex→bbnf side is not lawful. *Guards 5B.*
- **Automata-as-e-graph is unproven at this class** (no cited DFA-minimization-by-saturation
  result; eqsat dialects are tensor/relational). Presenting it as settled violates measured-truth.
  *Guards 5C.*
- **classify.rs (1,148 LOC) → god-pass**: re-homing it into "an IR analysis pass" risks either a
  cross-crate leak (regex Hir details bleeding into bbnf IR) or a monolith. *Guards 5A/5D.*
  (MEMORY: no-god-modules, regex-generalized — "regex analysis lives in bbnf-regex, not
  bbnf-lang; only IR-specific items in bbnf-lang".)
- **Double-emit with the SIMD recognizer**: PT-1.W5 ⇄ GN-2.W1 — a self-loop scan site that is
  also an unanchored regex leaf must get exactly one lowering (`find_member` vs `dfa.find`), not
  both (INDEX.md interlocks; PT-1 §8). *Guards 5D's generator seam.*
- **Warm-bench self-deception** (PROVE-09's refutation, SPEC §0 row 09): the standing pathology;
  paired same-binary interleaving mandatory even for "obvious" regex wins.

---

## 6. SYNTHESIS FOR THE ADJUDICATION (no allegiance — the terrain, not a pick)

- **5B is the lowest-risk, already-substantiated route**: the shared e-graph exists, the string
  functor (`analyze`) exists as its degenerate ancestor, MLIR/regex-automata are the precedents,
  and PROVE-10's DFA survives untouched. Its real obligation is defining the functor totally over
  `Class`/`Look` (round-trip oracle) — cheap and standard.
- **5A is the totality-maximal route** the constitution's clause (i) most wants; its cost is the
  arena/span vs Eq/Hash merge and growing bbnf `ExprKind` with byte-plane variants — a large but
  bounded refactor whose payoff is deleting the stringly-typed seam. Note 5A and 5B *converge* if
  bbnf grows `Class`/`Look`: the difference becomes "one type" vs "two types + total functor."
- **5C splits cleanly into a proven half and an unproven half.** Proven: HIR canonicalization by
  saturation (extant regex egraph). Unproven and §0-gated: automata *construction/minimization* by
  saturation — for which **derivatives (Brzozowski/Antimirov/RE#), not e-graphs, are the measured
  SOTA**, and the honest 5C is "derivative construction with e-graph-provided ACI similarity,
  measured head-to-head against the classical Thompson→subset→Hopcroft pipeline PROVE-10 left
  intact."
- **5D is viable only in its "thin front over the same machinery" reading**; the "no engine on the
  generator path" reading is refuted by PROVE-10 + scannerless evidence. Its lawful form: a grammar
  IR byte-recognizer node that compiles to the SAME DFA + prefilter, with `->` projections carried
  on the recognizer (Ragel's transition-action precedent), one lowering per site (GN-2.W1
  interlock).

Cross-lock note: 5D and **4D (GRAMMAR-SUGAR — "combinators are a construction API over the same
HIR the grammar plane uses (LOCK-5's continuum)")** point at the same "one HIR, regex and
combinators are fronts" thesis, as does **3C (GRAMMAR-HOMOIOUSIA)**. LOCK-5's HIR verdict is
load-bearing for LOCK-3/4/7 and must be decided as the shared spine, not in isolation.

---

## Sources

Local (verified `path:line`): `skinny/crates/ir/src/lib.rs` (bbnf ExprKind :234-271, arena
:33-39, Regex-as-string :250, nullability/analyze :331, regex nullability oracle :651-677,
BackendExpr RegexProgram/Recognizer :389-421); `parse-that/rust/regex/src/hir/mod.rs:216`
(regex Hir, via R3); R3 §1/§3 (workspace, egraph patch-table, 6742-LOC map, classify 1148);
R7 §3/§4 (shufti/Teddy, regex-automata meta-engine); `prototypes/PROVE-10/REPORT.md` (DFA
prefilter GO, 8.8-29.5×); `docs/tranches/sk-v24/PT-1.md` (prefilter/wide/lazy waves);
`docs/tranches/sk-v24/INDEX.md` (interlocks); SPEC §0 (rows 09/10, refutation laws), §1
(one-regex-home).

External:
- Owens/Reppy/Turon, RE derivatives re-examined — <https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf>
- Antimirov partial derivatives — <https://www.sciencedirect.com/science/article/pii/0304397595001824>
- RE# derivative-based matching (2024) — <https://arxiv.org/pdf/2407.20479>
- Optimizing Regexes via Rewrite-Guided Synthesis — <https://arxiv.org/pdf/2104.12039>
- eqsat MLIR dialect — <https://arxiv.org/pdf/2505.09363>
- Learned Graph Rewriting w/ Equality Saturation — <https://arxiv.org/pdf/2407.12794>
- Scannerless parsing — <https://en.wikipedia.org/wiki/Scannerless_parsing>
- Faster Scannerless GLR (CC-2009) — <https://homepages.cwi.nl/~jurgenv/papers/CC-2009.pdf>
- Local Lexing — <https://arxiv.org/pdf/1702.03277>
- re2c (lookahead-TDFA) — <https://re2c.org/> ; <https://www.sciencedirect.com/science/article/pii/S266596382030018X>
- Ragel FSM compiler — <http://www.colm.net/open-source/ragel/>
- re2c vs Ragel generated-size — <https://sabotage-linux.github.io/blog/7/>
- Verbatim / Coqlex verified derivative lexers — <https://arxiv.org/pdf/2306.12411>

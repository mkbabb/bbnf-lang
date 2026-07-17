# LOCK-5 — THE REGEX CONTINUUM · pass-3 PRIOR-ART RESEARCH

Role: prior-art researcher, no family allegiance (CONSTITUTION). Maps the problem class, what
won/lost for it elsewhere, the constraints our own evidence already imposes, and the traps
named in comparable systems. Citations are external URLs or in-tree `path:line` verified this
pass. No REGISTRY read; families withheld by protocol.

---

## 0. The problem class, restated (PORTFOLIO LOCK-5 + constitution i/ii)

Not "build a regex engine." Four coupled obligations:

1. **Grammar totality (clause i)** — the regex grammar is bbnf-defined and BOOTSTRAPPED; the
   hand-written recursive-descent HIR parser becomes grammar-generated or a differential oracle.
2. **True isomorphism (clause ii)** — the regex HIR has true isomorphism to bbnf's HIR/IR
   (one type, or a lawful functor). This is the load-bearing, currently-UNMET requirement (§1).
3. **Automata as solver problems** — NFA→DFA→Hopcroft→prefilter recast as CSP/e-graph *where
   measured to win*; losses are named negative results (constitution vii).
4. **PROVE-10's prefilter tier / Teddy / lazy DFA folded; classify (1148 LOC) re-derived or
   re-homed; the existing engine's tests + consumers are the PARITY FLOOR.**

---

## 1. What exists in-tree (the constraints reality imposes)

- **A regex grammar ALREADY exists in bbnf**: `grammar/misc/regex.bbnf` (36 lines) — `expression
  = (term+, "|"?)+ ; term = factor - end ; char_class`, escapes, anchors, lookahead/lookbehind,
  back-references, flags. Clause i's bootstrap seed is PRESENT as a spec artifact — but it is NOT
  wired to the engine (§ below). It uses `->`-less EBNF (no projections yet) and embeds regex
  leaves (`/d+/`, `/x[0-9a-fA-F]+/`) — a self-reference that IS the bootstrap circularity.
- **The automata engine is `bbnf-regex` (`parse-that/rust/regex`, 6742 LOC, R3 §3), NOT the
  vendored copy.** Hand-written recursive-descent HIR parser (7 files: `hir/parser/{mod,class,
  escape,group,repetition,atom,cursor}.rs`); Thompson `Nfa` (449); subset construction +
  **Hopcroft minimization** `automata/dfa.rs` (438); `classify.rs` (1148, the largest file);
  and — critically — **`egraph/` (1253 LOC) already runs HIR equality-saturation** (rules:
  repetition/union/flatten/superset/redundant) over external `egraph`/`egraph-derive`/
  `csp-solver` path-deps. `pub enum Hir` at `hir/mod.rs:216`.
- **The vendored `skinny/crates/parse-that-regex/` is a PARTIAL copy** — only `number/`
  (Eisel–Lemire), `unicode/`, `integration/simd_scan_hook.rs`. It does NOT contain the automata
  engine. This is a live **fragmentation hazard**: "one regex home" already risks becoming two
  homes with different `Hir` surfaces.
- **The coupling is real but one-directional**: bbnf's `ir` crate imports `bbnf_regex::analyze`
  (`skinny/crates/ir/src/lib.rs:1`, used at `:331` for `ExprKind::Regex{pattern}` nullability).
  bbnf's own grammar IR is `GrammarIr`/`Rule`/`Expr`/`ExprKind` (`ir/src/lib.rs`), **a distinct
  representation from bbnf-regex's `Hir`.** No shared `Hir` type exists at HEAD — clause ii is
  UNMET; there are two trees, coupled only through `analyze()`. That gap is LOCK-5's whole point.

---

## 2. External SOTA — what won and lost for THIS class

- **Derivative-based construction beats regex→NFA→DFA (Owens/Reppy/Turon, JFP 2009).** Brzozowski
  derivatives build the DFA *directly* from the RE with no NFA intermediary; provably terminates
  (finitely many derivatives up to ACI-equivalence); the authors show derivative lexer-generators
  are "easier to write, more efficient, and more flexible" than classical NFA→DFA. WON for
  lexer-generation. Strongest evidence that a **single algebraic representation can subsume the
  NFA→DFA→minimize pipeline** — i.e. 5A/5C's thesis is not speculative. The catch: termination
  REQUIRES quotienting by associativity/commutativity/idempotence (smart constructors /
  canonicalization) — precisely what bbnf-regex's `egraph/` canonicalization already provides.
  [re-deriv.pdf](https://www.ccs.neu.edu/home/turon/re-deriv.pdf)
- **The derivative is ONE operator across regex AND grammar (Might/Darais/Spiewak, "Parsing with
  Derivatives," ICFP'11).** `d/dc` on a context-free grammar yields another grammar; regular
  languages are precisely the sublanguage where the derivative needs no laziness/fixpoint. This is
  the deepest theoretical charter for 5A's "regex HIR ⊂ bbnf HIR" — the two are literally one
  algebra differing only by recursion, so a single `Hir` is theory-sanctioned, not a shim.
  **The trap that gates it:** the naïve pearl was catastrophically slow; **Adams/Hollenbeck/Might
  (PLDI'16)** proved PwD is cubic and usable ONLY with aggressive memoisation + graph compaction.
  Verdict: derivatives are the elegant unifier, but a naïve derivative engine on the *generator hot
  path* is a KNOWN loss — derivatives belong to compile-time construction, never per-parse.
  [PwD pearl](http://david.darais.com/assets/papers/parsing-with-derivatives/pwd.pdf) ·
  [Adams PLDI'16](https://dl.acm.org/doi/10.1145/3360553)
- **E-graph equality saturation FOR regex is proven prior art (ReGiS / McClurg et al., PACT
  2022).** "Optimizing Regular Expressions via Rewrite-Guided Synthesis" builds an e-graph over
  regexes, equality-saturates under rewrite rules, and extracts simplified/faster regexes (ReDoS
  removal). WON for regex simplification; validates the existing bbnf-regex `egraph/` subsystem
  and 5C's "canonicalization/minimization as saturation + cost extraction." Scope caveat: ReGiS
  targets *simplification*, not *determinization* — using saturation to REPLACE Hopcroft is
  beyond published art, so 5C's determinization-as-extraction is genuinely novel and must be
  measured head-to-head, not assumed.
  [arXiv 2104.12039](https://arxiv.org/abs/2104.12039)
- **Regex embeds into PEG/grammar losslessly, WITH semantic care (Medeiros et al., "From Regexes
  to Parsing Expression Grammars," arXiv 1210.4992).** A formal transformation regex→PEG matching
  the same strings as a PCRE, accommodating possessive/lazy repetition and lookahead. The
  theoretical license for 5A ("regex HIR is a sub-language of bbnf HIR"). The TRAP it names:
  regex **leftmost-longest** vs PEG/grammar **leftmost-first ordered choice** differ — the
  embedding must add backtracking/lookahead to preserve semantics. Isomorphism must be a LAWFUL
  functor over match semantics, not a naive syntactic re-label.
  [arXiv 1210.4992](https://arxiv.org/pdf/1210.4992)
- **Bootstrap discipline is settled craft (rust-peg; CPython PEG, PEP 617).** rust-peg's grammar
  is written in rust-peg itself, a precompiled stage-0 is checked in to break the circular
  dependency, regenerated via `cargo xtask bootstrap`. CPython bootstraps its parser from a
  meta-grammar. Exactly the committed-stage-0 + fixpoint-regen model clause i / LOCK-7 needs for
  `regex.bbnf` — and it aligns with the standing **clean-regen** law (never hand-patch generated).
  [rust-peg](https://github.com/kevinmehall/rust-peg) · [PEP 617](https://peps.python.org/pep-0617/)
- **A bootstrapped derivative lexer CAN be total + verified (Verbatim / Verbatim++, Egolf/Lasser
  et al., CSF'21 / CPP'22).** A lexer generator implemented and *proved correct in Coq* on
  Brzozowski derivatives — Verbatim derivates on the fly, Verbatim++ pre-builds a DFA for speed.
  The strongest exemplar that a bbnf-defined, self-describing regex engine can be a total artifact
  with a machine-checkable oracle (LOCK-7D-style differential promotion) — and that derivatives,
  not Thompson+subset, are the natural fit for a self-hosting, verifiable core.
  [Verbatim (IEEE)](https://ieeexplore.ieee.org/document/9474322/) ·
  [Verbatim++ (ACM)](https://dl.acm.org/doi/pdf/10.1145/3497775.3503694)
- **Prefilter/lazy-DFA ladder is SOTA-settled (burntsushi regex-automata; Hyperscan shufti/
  truffle/teddy).** Meta-engine tiers literal→lazy(hybrid)DFA→one-pass→backtracker→PikeVM; lazy
  DFA builds states during search under a memory budget and on quit-byte/exhaustion returns a
  fallback — "never a wrong answer." This IS PROVE-10's design and PT-1 W2/W3's spec, transcribed
  onto bbnf-regex's own DFA. (R7 §3–4: burntsushi.net/regex-internals; Teddy ICPP21; Hyperscan
  NSDI'19.)
- **flap — fused derivative-lexer + typed combinator parser, ONE representation (ICFP'23).** The
  lexer (derivatives of regexes) and the context-free layer (typed combinators) share a single
  representation and lower together — the canonical "regex is a sub-language of the grammar, no
  separate engine on the parse path" precedent (5A/5D). WON for the fused-lowering thesis; the
  license for 5D's "regex leaves lower DIRECTLY into the grammar's own IR."
  [flap arXiv 2304.05276](https://arxiv.org/pdf/2304.05276)
- **Capture-leaf → `Project` has a won design: RE2C's tagged DFA (TDFA(1), Trofimovich).** Submatch
  is carried by s-tags/m-tags in a **one-pass, constant-memory** DFA (Laurikari + one-symbol
  lookahead), compiled to code with NO runtime engine; Ragel's alternative (embedded actions on
  transitions) is the messier loser (no compositional tags). Q3/Q8's "capture-leaf → `Project`
  byte-identical to a hand-authored grammar capture" should be judged against TDFA semantics, not
  re-invented. [re2c TDFA(1)](https://www.sciencedirect.com/science/article/pii/S266596382030018X) ·
  [Trofimovich arXiv 1907.08837](https://arxiv.org/pdf/1907.08837)
- **The FOLLOW-sound one-pass fraction is a MEASURED fraction with a fallback (regex-automata
  `dfa::onepass`).** One-pass applies only when preference-order and longest-match cannot disagree —
  "rarer for them to disagree… if they did, the regex typically isn't one-pass" — else the engine
  falls back. So Q7's one-pass/FOLLOW-sound coverage is a *number + fallback*, never 100%; the SPEC-
  ONLY sound-predicate + grammar-FOLLOW check is exactly this shape.
  [onepass Config](https://openrr.github.io/openrr/regex_automata/dfa/onepass/struct.Config.html)
- **Symbolic automata parametrize over an alphabet Boolean algebra (D'Antoni–Veanes, POPL'14).**
  Minimization is defined *over the algebra*, not the concrete alphabet — the theory answer to Q2's
  "where does `[α-ω]*` byte-expansion live": confine it to the automata layer as an alphabet
  parameter, keep it OUT of the shared type. Symbolic minimization wins only when the alphabet is
  large/symbolic — NOT the 256-byte case, which bears on 5C (below).
  [SFA minimization POPL'14](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/sfaMinimizationPOPL14.pdf)
- **Determinization/minimization-as-e-graph has ZERO published prior art (targeted sweep).**
  Equality-saturation graph rewriting is proven for tensor superopt and relational query rewrite
  ([2101.01332](https://arxiv.org/pdf/2101.01332), [2407.12794](https://arxiv.org/pdf/2407.12794)),
  and ReGiS proves it for regex *simplification* — but no source frames **DFA minimization** as
  e-graph saturation. On a 256-byte concrete alphabet, Hopcroft's O(n log n) partition refinement is
  *already* congruence-closure by another name, so the e-graph frame most plausibly RELOCATES the
  work at added cost. This makes 5C's minimization/determinization-as-extraction the likeliest
  **named negative result** — Q5's reference-Hopcroft head-to-head is how to earn it. And the
  *elegant algebraic alternative* to both Hopcroft and an e-graph, fitting the derivative posture:
  **Brzozowski double-reversal minimization** (reverse→determinize→reverse→determinize yields the
  minimal DFA) — if the engine goes derivative-first, near-minimality is inherited from
  smart-constructor canonicalization (§2 Owens) and reversal is the fallback; neither route needs
  a saturation engine to reach the minimal automaton. [Brzozowski/DFA-min](https://grokipedia.com/page/DFA_minimization)

---

## 3. Constraints our own evidence base imposes (binding)

- **PROVE-10 is the PARITY FLOOR (SPEC §0 #10, CONFIRMED).** Start-state memchr prefilter in a new
  `Dfa::find`: **8.8–29.5×** on prefilterable patterns, **0.97–1.01×** controls, **compile-
  neutral**, **44/44 tests**, 245-line additive patch. Any re-homing / re-derivation of the engine
  MUST preserve this measured win and the 44-test suite (PT-1 W1 born-verified non-regression).
- **The classical pipeline WORKS today** (Thompson NFA + Hopcroft in `dfa.rs`). Constitution vii +
  refutation culture: 5C's e-graph/CSP determinization must **beat it head-to-head or be filed as
  a named negative result** — it does not displace a working pipeline on elegance alone.
- **No shim perf claims (refutation 09, LAW).** bbnf-regex is pure scalar + `SmallVec` (no
  `portable_simd`); PROVE-10's speed comes from the `memchr` crate's own aarch64 NEON backend.
  Any SIMD prefilter (shufti/Teddy, PT-1 W2) uses real `vqtbl1q_u8`, measured paired — never a
  `portable_simd`→NEON shim asymmetry.
- **x86 dead; NEON-only; no SVE/SVE2/SME for byte work** (SPEC §0; R7 §0). Cost model locks to
  128-bit NEON.
- **Type plane costs zero at runtime** (constitution v); **macOS PMU restricted** → wall-clock
  paired same-binary ratios are the certified metric, absolutes directional (PROVE-10 §5).
- **Consumer-less substrate forbidden (Y.13, constitution viii).** A prefilter/lazy/e-graph tier
  without a same-wave caller does not open (PT-1 names an in-wave consumer per wave).

---

## 4. Traps named in comparable systems (the failure-mode census)

1. **Two-representation drift (the isomorphism trap).** bbnf `Expr`/`ExprKind` vs bbnf-regex
   `Hir` are distinct trees today, coupled only by `analyze()`. A naive "true isomorphism" that
   forces one type risks losing domain-fit (regex byte-classes, `Look`, `Repetition{min,max}` vs
   grammar `->` projections). 5B's lawful-functor-with-round-trip-oracles is the hedge the
   literature (1210.4992's semantic-preservation care) endorses.
2. **The vendored PARTIAL copy.** `skinny/crates/parse-that-regex/` carries number/unicode/
   integration but NOT automata — "one regex home" is already fracturing. Any port must re-home
   the WHOLE engine or declare the split lawful, not silently maintain two `Hir`s.
3. **Derivative/e-graph non-termination.** Brzozowski derivatives blow up without ACI
   quotienting; regex e-graphs (ReGiS) need a bounded rule-set + cost model or saturation runs
   unbounded — colliding with the **compile-neutral gate** (PROVE-10) and the generated-size
   budget. Canonicalization is mandatory, not optional.
4. **Regex-vs-grammar match semantics.** leftmost-longest (regex/POSIX) ≠ leftmost-first (PEG/
   ordered choice). An embedding that drops this silently changes match sets — a HARD-RED parity
   defect (PT-1 W2/W3 "correctness assert fires → HARD RED").
5. **Lazy-DFA cache thrash + state-cap bail correctness.** A high-entropy corpus can erase the
   lazy compile win (per-parse re-determinization); a bail that races a match on the cap-boundary
   step can return a partial answer. burntsushi's discipline (bail = fallback, never a wrong
   answer) + a cap-boundary oracle is the mitigation (PT-1 W3 risks §8).
6. **Bootstrap circularity / hand-patched generated.** `regex.bbnf` embeds regex leaves and must
   be parsed to parse itself; the only safe route is committed stage-0 + fixpoint gate
   (regen∘regen byte-identical, LOCK-7A) with the clean-regen law enforced — never hand-edit the
   generated parser.
7. **Warm-bench self-deception (09's standing lesson).** Two-arm window asymmetry inflates
   ratios; paired same-binary interleaving is mandatory even for "obvious" wins.
8. **Wide-entry shufti can LOSE to memchr's internal vectorization** on near-in-tier (4-byte)
   sets — `memchr3`-of-3 + a one-byte fallback can beat a naive `vqtbl1q_u8` LUT loop. The W2 cost
   model must gate shufti on a *measured crossover*, not a fixed width (PT-1 §8 counterexample).
9. **Verification-tax trap (Coqlex).** Formally-verified/correctness-first automata paths run
   slower; keep verification at the HIR/oracle layer (differential oracles, LOCK-7D-style), never
   as an in-the-hot-DFA-path cost that regresses PROVE-10's floor.
   [Coqlex arXiv 2306.12411](https://arxiv.org/pdf/2306.12411)
10. **Double-emit at the W5 ⇄ GN-2.W1 seam.** A self-loop scan site that is also an unanchored
    regex leaf must receive exactly ONE lowering (`find_member` XOR `dfa.find`) from a single
    recognizer-dispatch decision point — grep-gated on the generated file (PT-1 W5 risk).

---

## 5. Net for the design (evidence-anchored, no family verdict)

The literature CONFIRMS the continuum is buildable: derivatives (Owens et al.) prove one algebraic
representation subsumes NFA→DFA; ReGiS proves e-graph saturation optimizes regexes; 1210.4992
proves regex embeds into grammar with lawful care; rust-peg/CPython prove the bootstrap. The
in-tree reality CONSTRAINS it: `regex.bbnf` exists but is unwired; two `Hir`/`Expr` trees coupled
only by `analyze()`; PROVE-10's 8.8–29.5× + 44 tests are the immovable floor; a working Hopcroft
pipeline that 5C must beat head-to-head or refute. The unpublished frontier (and thus the
adjudication risk): determinization-as-extraction (beyond ReGiS's simplification scope) and a
single shared `Hir` that keeps regex-domain fit — both must be MEASURED, not asserted.

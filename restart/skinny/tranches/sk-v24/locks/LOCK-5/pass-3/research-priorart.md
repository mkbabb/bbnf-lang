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
- **Prefilter/lazy-DFA ladder is SOTA-settled (burntsushi regex-automata; Hyperscan shufti/
  truffle/teddy).** Meta-engine tiers literal→lazy(hybrid)DFA→one-pass→backtracker→PikeVM; lazy
  DFA builds states during search under a memory budget and on quit-byte/exhaustion returns a
  fallback — "never a wrong answer." This IS PROVE-10's design and PT-1 W2/W3's spec, transcribed
  onto bbnf-regex's own DFA. (R7 §3–4: burntsushi.net/regex-internals; Teddy ICPP21; Hyperscan
  NSDI'19.)

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

# LOCK-5 — THE REGEX CONTINUUM — PASS-3 PRIOR-ART SWEEP

Prior-art researcher, pass 3. Problem class: **a grammar-driven regex engine whose HIR is
isomorphic to the parser-generator's own HIR/IR, with NFA→DFA→minimization→prefilter recast
where a solver measurably wins, and regex literals in grammars lowering directly into the
generator's IR.** No family allegiance; charter is what EXISTS, what WON/LOST elsewhere for
this class, the constraints our own evidence base imposes, and the traps named in comparable
systems. Every external claim carries a URL; every codebase claim carries a `path`.

---

## 0. The two artifacts this lock actually governs (codebase truth, verified)

- **bbnf-regex `Hir`** (`parse-that/rust/regex/src/hir/mod.rs:216`): a flat enum
  `Empty | Literal(Vec<u8>) | Class(CharClass) | Look(Look) | Repetition | Group(Box<Hir>) |
  Concat(Vec<Hir>) | Alternation(Vec<Hir>)`. Classes flattened to non-overlapping ranges;
  `Look` = 6 zero-width position assertions; captures are transparent `Group`. 6,742 LOC crate,
  stable-clean, pure scalar+SmallVec, its own `egraph/` (1,253 LOC, HIR-level equality
  saturation) and `classify.rs` (1,148 LOC).
- **bbnf `ExprKind`** (`skinny/crates/ir/src/lib.rs:234`): the grammar IR carries regex as the
  OPAQUE `Regex { pattern }` leaf (per XL-C); the backend side has `BackendExpr::RegexProgram`
  (`:389`), `Recognizer` (`:416`), `SimdMode::Prefilter` (`:425`), `SimdSite` (`:430`). The
  structural nodes (Concat/Alt/Repeat/Optional/Group) are near-shape-identical to `Hir`'s — but
  the leaf types (`Class` byte-ranges, `Look`) have **no** bbnf counterpart. That asymmetry is
  the whole isomorphism question.
- **The grammar already exists**: `grammar/misc/regex.bbnf` is a full BBNF regex grammar
  (flags/expression/term/factor/char_class/lookahead/…) — the bootstrap seed for constitution
  clause (i). A second copy rides `bootstrap/regex.bbnf` (Q9's de-dup target).

**Verdict this grounds:** the settled XL-C two-type split (opaque grammar leaf + rich engine
`Hir` + cheap `string`/`format!` bridge) is not merely a local compromise — it is exactly the
shape the strongest external incumbent converged on (§3 below). Prior art RATIFIES it.

---

## 1. Regex⇄grammar unification — the derivative lineage (5A ONE-HIR / 5D)

The dream of "regex and grammar are ONE thing under one operation" is old and has a **measured
performance verdict**, not just an aesthetic one.

- **Owens, Reppy, Turon — "Regular-expression derivatives reexamined"** (JFP 2009). The modern
  canonical: Brzozowski derivatives build a DFA *directly* from a regex, minimal-by-construction
  via the derivative equivalence — no explicit NFA/subset step.
  <https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf>
- **Might, Darais, Spiewak — "Parsing with Derivatives" / "Yacc is dead"** (ICFP 2011 /
  arXiv 1010.5023): extends Brzozowski from regular to **context-free** languages — the actual
  theoretical unification of regex and grammar under a single `derivative` operation. The dream
  5A gestures at is provably realizable. <https://arxiv.org/pdf/1010.5023>
- **THE TRAP (measured): Adams, Hollenbeck, Might — "On the Complexity and Performance of
  Parsing with Derivatives"** (PLDI 2016): PWD is O(n·G) but the constant is brutal; it is
  practical *only* with aggressive memoization + compaction + graph GC. The "one elegant
  operation for regex+grammar" carries a real, historically prohibitive performance tax.
  <https://arxiv.org/pdf/1604.04695>
- **flap: A Deterministic Parser with Fused Lexing** (PLDI 2023, arXiv 2304.05276): the closest
  LIVING relative — derivative-based lexing fused with typed context-free combinators, one
  pipeline, **staged/compiled**. Proves the fusion is buildable at speed *when lowered to code*
  (not interpreted) — evidence FOR 5D-style direct lowering and AGAINST a runtime unified
  interpreter. <https://arxiv.org/pdf/2304.05276>
- **PEG-derivatives** (Moss, arXiv 1801.10490): derivative machinery extended to PEGs — relevant
  since parse-that is PEG-family. <https://arxiv.org/pdf/1801.10490>

**Bearing on LOCK-5.** 5A ("regex HIR is a sub-language of bbnf HIR, one type") is the PWD dream
in miniature. Prior art says: the unification is sound, but a *shared runtime type* forces the
regex leaf's byte-class machinery (Q2's `[α-ω]*` expansion) into the grammar's hot path, and the
measured PWD tax is the warning. 5D (lower to code) and 5B (functor, two domain-fit types) are
the versions prior art has actually shipped at speed. flap is the proof that fused lexing wins
**as staged codegen**, not as a shared interpreted IR.

---

## 2. Direct-to-code lowering — re2c / Ragel (5D GRAMMAR-FRAGMENT, Q7, Q8)

The exact prior art for "regex literals lower DIRECTLY into the generator's own IR, no separate
engine on the generator path."

- **re2c** (Trofimovich, *SCP* 2020, "A lexer generator based on lookahead-TDFA"): compiles
  regexes AOT to **direct-coded DFA** — conditional jumps + comparisons, not a table, not a
  runtime engine. Its **lookahead-TDFA** does submatch/capture extraction with *minimal*
  overhead — the direct precedent for Q8's "capture-leaf → `Project` byte-identical to a
  hand-authored grammar capture." <https://re2c.org/> ·
  <https://re2c.org/2020_trofimovich_re2c_a_lexer_generator_based_on_lookahead_tdfa.pdf>
- **Ragel — the named trap**: Ragel's direct one-pass FSM lowering **cannot handle general
  nondeterminism**; disambiguation operators paper over only simple cases, and "Ragel programs
  are not always correct" is a documented fundamental limitation
  (<https://news.ycombinator.com/item?id=19280499>). This is the empirical spine under **Q7**:
  only a **FOLLOW-sound subset** of regex leaves can be lowered one-pass into the surrounding
  grammar; the residue must fall to the compiled DFA. Do not expect a high one-pass fraction on
  patterns whose class overlaps its own FOLLOW — that is precisely where Ragel breaks.

**Bearing.** 5D is a *shipped* architecture (re2c is production, powers PHP's lexer). The
generator emitting straight-line DFA code with capture-as-submatch is proven ground. The gate is
Q7's honest fraction: re2c/Ragel history says the one-pass win is real but bounded by
FOLLOW-soundness, and the DFA path must remain the total fallback.

---

## 3. The strongest incumbent HIR — Rust `regex-automata` (5A/5B/5C, PROVE-10)

The single most relevant living system; bbnf-regex is visibly modeled on it.

- **The HIR** (Gallant, "Regex engine internals as a library"): `hir::translate` lowers AST→Hir,
  a representation "small enough to fit into your head" — flattens every class to non-overlapping
  ranges, resolves case-folding/Unicode at translate time, **throws out all literal forms in
  favor of one representation.** bbnf-regex's `Hir` is a near-clone (Empty/Literal/Class/Look/
  Repetition/Concat/Alternation). <https://burntsushi.net/regex-internals/>
- **The meta engine** (engine selection by pattern shape): literal-extraction → **Prefilter**
  (memchr/Two-Way single → **Teddy** multi SIMD → Aho-Corasick) → **lazy/hybrid DFA** (state
  cache under a memory budget; on cache-exhaust or a **quit byte** it bails to a fallback engine,
  *never a wrong answer*) → **one-pass DFA** (capture offsets fast, limited subset) → bounded
  backtracker → PikeVM. Selection is **hand-written heuristics**, NOT a CSP or e-graph.
  <https://github.com/BurntSushi/regex-automata>
- **Issue #11 "Analyze complexity before actual compile?"** is literally the question **Q5**
  poses (pre-size the DFA / decide the path before paying for it) — burntsushi's answer has been
  *cheap static heuristics*, not a solver. <https://github.com/BurntSushi/regex-automata/issues/11>

**Bearing.**
- 5C AUTOMATA-AS-EXTRACTION and Q6 (table-vs-CSP tier selection) face a strong incumbent whose
  answer is "hand heuristics suffice." A CSP/e-graph must **beat** that, not merely equal it;
  Q6's demand for a *concrete global codegen/LUT-budget coupling witness* is exactly right — if
  tier selection is per-site local, the heuristic table wins by parsimony and the CSP is a
  consumer-less substrate (constitution viii).
- PROVE-10 (CONFIRMED 8.8–29.5×, 0.99× controls, compile-neutral, +245 LOC additive) is the
  memchr **Byte1/2/3** rung of exactly this ladder, retargeted onto bbnf-regex's own DFA start
  state. The **Teddy/shufti wide-entry tier** (entry sets > memchr3) is the named next rung
  (PT-1.W2, `research/R7` §3) — prior art (Hyperscan/Teddy, ICPP'21) proves it exists and wins.
- The lazy-DFA **quit-byte / cache-exhaust → correct fallback** discipline is mandatory law for
  any lazy tier (PT-1.W3): the engine must degrade to a slower-but-correct path, never a wrong
  answer. This is the most-copied safety invariant in the field.

---

## 4. E-graph / equality-saturation over automata (5C, Q5, Q10)

- **Formal grounding**: the tree-automata ↔ e-graph duality is now proven — automaton states ≡
  e-classes, transitions ≡ e-nodes, a term is in the e-graph iff accepted by it viewed as a tree
  automaton ("Semantic Foundations of Equality Saturation," Suciu et al, arXiv 2501.02413).
  <https://arxiv.org/html/2501.02413> Real theoretical license for 5C's "minimization as
  rewrite-to-normal-form / congruence closure."
- **BUT no production regex engine minimizes via equality saturation.** Equality saturation's
  shipped wins are compiler/hardware/query optimization (egg, Caviar, BoolE) — none is a
  throughput-critical automata minimizer. bbnf-regex already runs equality saturation at the
  **HIR** level (`egraph/rules/{flatten,union,superset,repetition,redundant}.rs`, 1,253 LOC) —
  i.e. the e-graph is on the *simplify-the-regex* side, NOT the automata side. That is the
  cautious, prior-art-consistent placement.
- **THE TRAP**: novelty = unmeasured. Q5 is correctly framed — a solver-minimizer must be
  measured head-to-head against a **reference O(n·k·log n) Hopcroft** (the classical incumbent,
  Hopcroft 1971 partition refinement) AND against **Brzozowski double-reversal** (minimal DFA by
  two subset-constructions — the derivative lineage's own minimizer, already implicit in §1).
  Congruence-rebuild cost is the axis that kills naive e-graph minimizers; measure it, do not
  assert it.

**Bearing.** 5C wins stay only where measured; losses are named negative results (the family
charter and the §0 refutation culture demand it). The HIR-level e-graph is already earning its
keep (Q10 should census per-rule fire counts on the real corpus — bare vs projection-wrapped — to
confirm it is not a consumer-less substrate).

---

## 5. Constraints our own evidence base imposes (binding, non-negotiable)

From **SPEC §0** (measured, certified) + the pass-3 directives' RATIFIED ground:

1. **XL-C RATIFIED (do not re-open)**: two domain-fit types — grammar keeps opaque
   `Regex(StringId)`, engine keeps rich `Hir` grown with `Class`/`Look`; a cheap `string`/
   `format!` bridge reusing `simplify_hir` joins them. §3 shows the strongest incumbent
   (regex-automata) uses exactly this shape — prior art ratifies it, and it narrows 5A→5B: the
   lawful form is the **functor** (5B), not one shared runtime type (5A), because the leaf
   byte-class/`Look` machinery has no place in the grammar's hot type.
2. **one-regex-home**: parse-that/bbnf-regex is canonical; the vendored `skinny/crates/
   parse-that-regex` (3,022 LOC — number/eisel_lemire/unicode/integration subset) is the
   **measurement substrate only**; base-pin every number to HEAD (XL-H).
3. **PROVE-10 is the floor, CONFIRMED**: memchr prefilter tier, +245 LOC additive, 44/44 tests,
   compile-neutral. Any regression against it is a fail.
4. **§0 refutation laws bind LOCK-5**: no digit-scan SIMD (02 NO_GO — the eisel_lemire number
   path in the vendored copy is NOT a regex lever), no block-scheduling (04), **no
   portable_simd→NEON shim perf claims (09 REFUTED)** — bbnf-regex is pure scalar+SmallVec, its
   speed comes from the `memchr` crate's own aarch64 backend and real `vqtbl1q_u8` shufti, never
   a shim asymmetry. Warm benches are disinformation; **macOS userspace PMU is restricted** →
   wall-clock **paired same-binary** ratios are the certified metric, absolutes directional.
5. **True isomorphism (constitution ii)** is empirically *nearly* free on structural nodes
   (Concat/Alt/Repeat/Group ≅ `ExprKind`) and *impossible without leakage* on leaves — so the
   functor must confine byte-expansion to automata (Q2's confinement boundary) and the type must
   cost zero at runtime (constitution v).

---

## 6. The named traps (what killed comparable systems)

- **Derivative/PWD unification tax** (Adams 2016): elegant one-operation regex+grammar is
  historically slow without heavy memoization — do not build a *shared interpreted* IR; stage to
  code (flap's lesson).
- **Ragel nondeterminism wall**: one-pass direct lowering is correct only on a FOLLOW-sound
  subset — bounds Q7's fraction, mandates the DFA total-fallback.
- **CSP-where-a-table-suffices** (regex-automata's hand-heuristic incumbent): tier/engine
  selection is local per-site; a solver must exhibit a *global coupling witness* (Q6) or it is a
  consumer-less substrate (constitution viii). The single most likely over-engineering trap in 5C.
- **E-graph minimizer novelty**: no one has shipped equality-saturation automata minimization at
  throughput; congruence-rebuild cost is the killer — measure against Hopcroft AND Brzozowski
  double-reversal or it does not land (Q5).
- **Backtracking / ReDoS** (SoK arXiv 2406.11618): the DFA-first choice is *immune* by
  construction — a positive, but it becomes a trap the instant a backtracker fallback is added for
  a feature (backrefs, lookaround width). bbnf-regex's `Look` is zero-width only; keep it so.
  <https://arxiv.org/pdf/2406.11618>
- **Lazy-DFA wrong-answer risk**: the quit-byte / cache-exhaust → correct-fallback invariant is
  non-optional for the lazy tier (PT-1.W3). Never a wrong answer under budget exhaustion.

---

## 7. What is genuinely novel here (no prior art to lean on — measure everything)

- **Regex HIR as a lawful functor of a *parser-generator's* HIR, bootstrapped from a BBNF regex
  grammar** (constitution i+ii). re2c/regex-automata have HIRs; none is the functorial image of
  the host grammar generator's own IR, and none is itself grammar-defined. This is the lock's
  original contribution — and therefore the part with the least external cover and the highest
  measurement burden (Q1's exact per-pass dispatch census; Q3's byte-identity `from_expr`≡
  `from_hir` harness; Q9's break-the-discard grammar-driven path, SPEC-ONLY until LOCK-7/7A).
- **classify.rs re-homing** (1,148 LOC): the `RegexClass` taxonomy (Numeric/QuotedString/
  HexDigits/Identifier/CharClassQuantified/PrefixThenClass/AccelDriven) is a bespoke
  dialect-collapsing analysis with no external analog; Q1 must show whether re-homing it as an IR
  pass SHRINKS or merely RELOCATES LOC (measured, not asserted).

---

## Sources

- Owens/Reppy/Turon, RE derivatives reexamined — <https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf>
- Might/Darais, Yacc is dead / PWD — <https://arxiv.org/pdf/1010.5023>
- Adams et al, Complexity & Performance of PWD (PLDI'16) — <https://arxiv.org/pdf/1604.04695>
- flap: Deterministic Parser with Fused Lexing (PLDI'23) — <https://arxiv.org/pdf/2304.05276>
- Moss, PEG derivatives — <https://arxiv.org/pdf/1801.10490>
- re2c (lookahead-TDFA) — <https://re2c.org/> · <https://re2c.org/2020_trofimovich_re2c_a_lexer_generator_based_on_lookahead_tdfa.pdf>
- Ragel nondeterminism limitation (HN) — <https://news.ycombinator.com/item?id=19280499>
- Gallant, regex engine internals (HIR + meta engine) — <https://burntsushi.net/regex-internals/>
- regex-automata repo + issue #11 — <https://github.com/BurntSushi/regex-automata> · <https://github.com/BurntSushi/regex-automata/issues/11>
- Semantic Foundations of Equality Saturation (automata↔e-graph) — <https://arxiv.org/html/2501.02413>
- ReDoS SoK — <https://arxiv.org/pdf/2406.11618>
- Local: `parse-that/rust/regex/src/{hir/mod.rs:216, classify.rs:55, automata/, egraph/}`;
  `skinny/crates/ir/src/lib.rs:234`; `grammar/misc/regex.bbnf`; SPEC §0; PROVE-10/REPORT.md.

# LOCK-5 pass-3 — PRIOR-ART sweep: THE REGEX CONTINUUM (grammar-driven regex engine)

Family-blind. Problem class: a regex engine reborn grammar-first — bootstrapped regex
grammar, an HIR isomorphic to bbnf's HIR/IR, NFA→DFA→minimize→prefilter recast (where
measured to win) as solver problems, PROVE-10's tiers folded, the 1148-LOC classify
re-homed, the existing suite as parity floor. Below: what exists (in-tree + external), what
won/lost elsewhere for THIS class, the constraints our evidence base imposes, the named
traps. Citations are `path:line` (local, verified) or URL (external).

---

## 1. What already exists IN-TREE (the settled substrate — not speculation)

**The isomorphism seam is already built, and already lawful-functor shaped.**
`parse-that/rust/regex/src/egraph/translate.rs:1-9` declares the exact pattern the
constitution clause (ii) demands: `insert_hir` / `extract_hir` are *"the only place where
the owning type and the e-node type touch. Every other consumer … works against `HirENode`
exclusively; every consumer outside the e-graph pipeline (the parser, NFA/DFA compiler,
classifier) works against `Hir` exclusively."* That is a two-representation design with a
total functor each way, round-trippable — the LAWFUL-FUNCTOR shape, already embodied for the
`Hir ↔ egraph` boundary. Any "one HIR / shared rewrite library" claim inherits this seam as
prior art, not a greenfield.

**The e-graph rewrite library is ALREADY the shared part across both repos.** R3 §1: `bbnf-
regex/Cargo.toml:35-37` path-patches `egraph` / `egraph-derive` / `csp-solver` to
`../../bbnf-lang/crates/{egraph,egraph-derive}` and `csc411/.../csp-solver` — the *same*
crates bbnf-lang consumes. So "the e-graph rewrite library is the shared part; each side
keeps domain-fit representations" is not a proposal — it is the crate graph at HEAD. What is
NOT shared is the *node type*: regex has `HirENode` (`egraph/node.rs`), bbnf has `ExprKind`
(`skinny/crates/ir/src/lib.rs:234`; alongside `BackendShape:361`, `Recognizer:416`,
`SimdMode:425`). One-HIR-as-single-type would collapse those; lawful-functor keeps both and
shares only the saturation engine — the prior art (translate.rs + the field, §2) leans
functor: the two node vocabularies are genuinely different alphabets.

**classify already consumes HIR** — `classify.rs:177 classify_regex_from_hir(&Hir)` (the
`classify_regex(pattern)` string entry at `:157` parses then delegates). "classify becomes an
IR analysis pass" is *partially realized*: the 1148-LOC file is already an HIR consumer, not
a string scanner. Open work is re-homing/re-deriving 1148 LOC, not inventing the posture.

**HIR-rewrite egraph rules already exist** (R3 §3; `egraph/rules/{repetition,union,flatten,
superset,redundant}.rs`, 1253 LOC total) — Kleene-algebra-style algebraic simplifications on
HIR. This is the *winnable* half of "automata as extraction," already in-tree.

**PROVE-10 landed** the memchr prefilter tier: `Prefilter{None,Byte1,Byte2,Byte3}` +
`Dfa::find`, 8.8–29.5× on prefilterable / 0.97–1.01× controls / compile-neutral / 44/44,
245-line additive patch (PROVE-10 REPORT §1-3; SPEC §0 row 10 CONFIRMED). Disasm shows
memchr's own aarch64 NEON (`shrn.8b #4` movemask, PROVE-10 §2c) — speed from memchr's
backend, NOT a portable_simd shim.

---

## 2. What won / lost ELSEWHERE for this problem class

**regex-automata (burntsushi) is the reference architecture; bbnf-regex mirrors it.**
Pipeline `&str→Ast→Hir→NFA→{lazy/hybrid DFA, one-pass DFA, full DFA}`; HIR compiles to a
*byte*-transition NFA (unicode classes → UTF-8 automaton), byte-equivalence classes cut the
DFA alphabet. [burntsushi regex-internals](https://burntsushi.net/regex-internals/);
[regex_automata::nfa::thompson](https://docs.rs/regex-automata/latest/regex_automata/nfa/thompson/).
bbnf-regex is isomorphic: `hir/parser → automata/nfa.rs → dfa.rs (subset + Hopcroft) →
byte_classes` (R3 §3). **What WON:** the layered Ast→Hir→NFA→DFA tower with byte-classes is
settled SOTA — do not relitigate. **The LOCK-5 lesson:** regex-automata's HIR is regex's OWN,
distinct from any host-grammar IR — nobody in the production regex world shares one HIR node
type with a parser-generator's grammar IR. So "regex HIR is a sub-language of bbnf HIR (single
type, single home)" is a claim BEYOND SOTA; must be earned by measurement, and both the
in-tree translate.rs seam and the field's instinct are two-types-one-engine, not one-type.

**Prefilter tiers — Hyperscan lineage + regex-automata meta-engine.** shufti (nibble-LUT
`vqtbl` membership, ≤4 ops) / truffle (256-set, 2nd table) / Teddy (bucketed multi-literal,
2–72 literals). Meta-engine ladder: literal/prefilter → lazy(hybrid) DFA → one-pass DFA →
bounded backtracker → PikeVM, with a **quit-byte / cache-cap bail that is never a wrong
answer** (falls back). [Hyperscan NSDI'19](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf);
[Teddy ICPP21](https://oaciss.uoregon.edu/icpp21/views/includes/files/pap205s4-file2.pdf);
R7 §3-4. **What won:** the tier ladder + bail-to-fallback discipline. PROVE-10 landed the
bottom tier; PT-1 W2/W3 spec shufti/Teddy + lazy-DFA-with-state-cap (PT-1.md §4). Tier
*selection* is a small discrete cost decision (≤3→memchr, 4..N→shufti, literal-prefix→Teddy,
large→lazy) — a pluggable cost model, the one place a solver genuinely governs.

**Regex optimization via rewriting is real, published prior art** — "Optimizing Regular
Expressions via Rewrite-Guided Synthesis" [arXiv 2104.12039](https://arxiv.org/pdf/2104.12039):
Kleene-algebra axioms as eqsat rules over an e-graph, extract the minimal equivalent regex.
VALIDATES the in-tree `egraph/rules/` HIR-simplification layer. Does NOT validate saturating
over automata states.

**Bootstrapped grammar-defines-itself — CPython PEG (PEP 617).** The grammar parser is
bootstrapped from a meta-grammar with grammar actions generating AST nodes; the generator eats
its own meta-grammar. [PEP 617](https://peps.python.org/pep-0617/);
[van Rossum meta-grammar](https://medium.com/@gvanrossum_83706/a-meta-grammar-for-peg-parsers-3d3d502ea332).
Direct prior art for constitution clause (i) "the regex grammar IS bootstrapped and
bbnf-defined." **Derivatives lineage:** PEG-derivatives (Might/Adams) descend from Brzozowski
*regex* derivatives — regex-as-grammar-fragment has deep theory backing.
[Derivatives of PEGs, arXiv 1405.4841](https://arxiv.org/pdf/1405.4841).

---

## 3. Constraints our evidence base already imposes (SPEC §0 laws + refutations)

- **09 REFUTED — portable_simd→NEON shim perf did not reproduce** (SPEC §0). Binding on any
  wide-entry SIMD tier: bbnf-regex is pure scalar + SmallVec, *no portable_simd* (R3 §3; PT-1
  §REFUTATION GUARDS). A shufti tier MUST use real `vqtbl1q_u8`; W1's win is memchr's own NEON.
  No number from a shim window asymmetry — paired interleaving mandatory even for "obvious" wins.
- **02 NO_GO — no digit-scan SIMD; 04 refuted — no block-scheduling.** Orthogonal to
  prefilter/lazy work; do not resurrect either lever in this lock.
- **Warm benches are disinformation; x86 dead; PMU cyc/B where macOS allows, else paired
  wall-clock ratio certified / absolutes directional** (PROVE-10 §5 — PMU unavailable; the
  certified metric is the divan paired ratio). Cold, N≥200, paired same-binary lanes with a
  correctness assert at setup (PROVE-10 §2).
- **Consumer-less substrate forbidden (Y.13); parity floor = 44/44 bbnf-regex + the
  parse_that `--test regex` bridge** (49 dfa / 22 parser / nfa / utf8 / accel / equiv / hir
  subtests, currently **E0432-broken** — a `parse_that::regex::parser` re-export drift,
  verified pre-existing, PROVE-10 §3, PT-1 W4). Any re-home of classify/HIR keeps this green.

---

## 4. The named TRAPS (comparable-system failure modes — front and center)

**TRAP A — "automata as e-graph saturation" runs the isomorphism BACKWARDS and pays NP-hard
extraction to replace a polynomial pipeline.** Rosetta Stone: *e-graphs ARE tree automata*,
and the direction that works is **automata-minimization → e-graph-reduction** (use Hopcroft to
shrink an e-graph), NOT eqsat → automata-minimization.
[E-Graphs, VSAs, and Tree Automata (Remy Wang)](https://remy.wang/reports/dfta.pdf). Classical
Hopcroft is O(n log n), near-optimal; recasting it as "rewrite-to-normal-form" saturation is
predicted to LOSE. Compounding: **e-graph DAG-extraction is NP-hard with no constant-factor
approximation, and extraction dominates end-to-end eqsat time**
([e-boost ICCAD'25](https://arxiv.org/pdf/2508.13020);
[SmoothE ASPLOS'25](https://www.csl.cornell.edu/~yc2632/data/smoothe_asplos2025_final.pdf);
[Fast&Optimal Extraction OOPSLA'24](https://cse.hkust.edu.hk/~parreaux/publication/oopsla24a/)).
So "determinization + minimization + prefilter-selection as saturation + CSP extraction" risks
trading a polynomial classical pipeline for an NP-hard one. **The honest split the prior art
dictates:** HIR-level *algebraic* simplification (Kleene rewrites) is where e-graphs win and
already live in-tree (`egraph/rules/`, 1253 LOC); *automata construction* stays classical
(subset + Hopcroft); *prefilter-tier selection* is a small discrete pluggable cost decision,
not saturation. On this evidence, "measured head-to-head" will NAME minimization-by-eqsat a
negative result — the constitution's own disposition for it.

**TRAP B — "one HIR" collapses two genuinely different alphabets and over-couples.** translate.rs
and the entire production regex field keep the regex node type distinct and share only the
engine. Forcing `Hir` = a sub-language of bbnf `ExprKind` risks a god-type fitting neither
domain (regex needs byte-ranges/looks/UTF-8 automata; bbnf IR carries `BackendShape`/
`Recognizer`/`SimdMode`). The "reasonable-point" isomorphism the constitution mandates already
exists as the *functor*, not the *identity*.

**TRAP C — shufti wide-tier loses to memchr's own internal vectorization.** memchr2/memchr3 are
already NEON-vectorized; a naive shufti loop on a near-in-tier 4-byte set can lose (PT-1.md §8
names the counterexample the challenge must build). Gate shufti on a *measured* crossover, not
a fixed width.

**TRAP D — lazy-DFA cache thrash erases the compile win, or the state-cap bail returns a wrong
answer.** regex-automata's answer: LazyStateID reserves tag bits (unknown/dead/quit/start/match)
and the bail returns `MatchError`→fallback, *never a partial match* (R7 §4). Any lazy tier must
bind BOTH axes (≥5× compile AND 0.98–1.02× throughput) and prove the bail's fallback == eager
result (PT-1 W3 gate + §8 race counterexample).

**TRAP E — bench-only wiring / prototype residue.** PROVE-10 substituted an in-bench `assert_eq!`
for the uncompilable `--test regex` subtests (PROVE-10 §3); leaving that un-promoted to `tests/`
is the framing's named defect class. The re-home must promote to `tests/` (precept 16/54) and
repair the E0432 bridge (PT-1 W4) so DFA correctness rests on the real 49-subtest oracle.

**TRAP F — regex↔grammar double-emit at the generator seam.** A self-loop scan site that is also
an unanchored regex leaf could get both `find_member` (bbnf-simd shufti) and `dfa.find`
(bbnf-regex prefilter). Single decision point (recognizer dispatch), one lowering per site, grep
the generated file (PT-1 W5 ⇄ GN-2.W1 interlock, PT-1.md §4).

---

## 5. Synthesis for the adjudicators (family-blind)

The class is *unusually well-instrumented*: the isomorphism seam is built (translate.rs), the
shared engine is the crate graph (patched egraph), the bottom prefilter tier is landed and
CONFIRMED (PROVE-10), classify already consumes HIR, the HIR-rewrite egraph rules exist. The
genuinely open questions the prior art frames as decidable-by-measurement: (i) functor-vs-
identity for the two HIRs — prior art (translate.rs + regex-automata) leans functor; (ii) which
pipeline stages an e-graph legitimately governs — prior art draws a hard line at *HIR algebraic
simplification only*, automata construction classical, tier-selection a pluggable cost model,
because eqsat extraction is NP-hard and Hopcroft is near-optimal; (iii) the bootstrap of the
regex grammar — PEG/PEP-617 is the executable precedent. Every acceleration lands with its
consumer, its `tests/` oracle, and a cold paired bench, over the 44/44 + bridge parity floor.

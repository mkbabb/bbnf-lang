# LOCK-5 — THE REGEX CONTINUUM · pass-1 PRIOR-ART RESEARCH

Role: prior-art researcher, no family allegiance. This maps the problem class (a grammar-first,
bootstrapped regex engine whose HIR is truly isomorphic to bbnf's HIR/IR, with
NFA→DFA→minimize→prefilter recast as solver problems where measured to win), what has won and
lost for it elsewhere, the constraints our own evidence base already imposes, and the traps named
in comparable systems. Citations are external URLs or in-tree `path:line` verified this pass.

---

## 0. The problem class, precisely

The task (PORTFOLIO LOCK-5, constitution clauses i/ii) is not "build a regex engine." It is:

1. **Grammar totality** — the regex grammar is itself bbnf-defined and BOOTSTRAPPED (clause i); the
   hand-written recursive-descent HIR parser (`parse-that/rust/regex/src/hir/parser/`, R3 §3, 7
   files) becomes grammar-generated or a differential oracle (LOCK-7 tie).
2. **True isomorphism** — the regex HIR "has true isomorphism to the bbnf HIR/IR system" (clause
   ii). This is the load-bearing, and currently UNMET, requirement (see §1).
3. **Automata as solver problems** — NFA→DFA→Hopcroft→prefilter recast as CSP/e-graph "where that
   is measured to win," classical pipeline kept where it wins; losses named (5C).
4. **Fold PROVE-10's prefilter tier, the Teddy tier, and the lazy DFA**; re-derive or re-home the
   1148-LOC `classify.rs`; hold the existing engine's 44-test suite + consumers as the parity floor.

This is a *continuum*: literal `#[0-9a-fA-F]{3,6}` in a grammar leaf and a standalone
`bbnf_regex::parse(pat)` call should ride one substrate, not two engines.

---

## 1. The evidence base in-tree — what actually exists, and the isomorphism gap

**Three regex homes exist, and they diverge — this is the central constraint.**

| Home | Path | LOC | Nature |
|---|---|---|---|
| Full engine | `parse-that/rust/regex` (`bbnf-regex` 0.1.0) | 6742 | HIR→NFA→DFA→Hopcroft→classify→egraph (R3 §3) |
| Skinny stub | `skinny/crates/bbnf-regex/src/lib.rs` | 322 | thin `analyze` surface consumed by `ir` (`ir/src/lib.rs:1` `use bbnf_regex::analyze`) |
| JSON scanner | `skinny/crates/parse-that-regex` | ~1419 + unicode/number | NOT a regex engine — `match_string`, eisel_lemire float, utf8, `integration/simd_scan_hook.rs`; a leaf-scanner helper, misnamed as the vendored "regex" copy |

The SEED brief calls `skinny/crates/parse-that-regex/` "the vendored copy" of the engine. **It is
not** — verified this pass: its `lib.rs` public surface is `match_string_at_quote`,
`scan_tiny_string_prefix`, `decode_unicode_escape`, `validate_number`, `unescape_string`
(`parse-that-regex/src/lib.rs:168–867`), with `number/eisel_lemire/` and `unicode/`. It is a JSON
string/number/UTF-8 leaf matcher, not HIR→DFA. The real engine is `parse-that/rust/regex` (R3 §3);
skinny's `bbnf-regex` is a 322-LOC analysis stub. The "one-regex-home" standing law (SPEC §4) is
today VIOLATED by this triplication. LOCK-5's first job is to collapse it.

**The isomorphism gap is real and measurable.** In bbnf's IR, a regex leaf is carried as a **bare
`String`**: `ExprKind::Regex { pattern: String }` (`ir/src/lib.rs:250`), lowered to
`BackendExpr::RegexProgram { pattern: String, span_kind }` (`:389`) and to
`Recognizer::SimdScan { mode, alphabet, site }` (`:417`). The regex is re-parsed and classified
out-of-band by `bbnf_regex::analyze` on a String — there is **no shared HIR node**. bbnf's HIR is
`Alt / Repeat / Literal / Regex / Ref / Annotation / Project / Discard` (`ir/src/lib.rs:234–266`);
the engine's HIR is `Hir::{ByteRange, CodepointRange, CharClass, Look, Repetition, …}` (R3 §3,
`regex/src/hir/mod.rs:216`). These are two disjoint types. Clause-ii "true isomorphism" is thus a
GREENFIELD claim, not a refactor of an existing bridge — the honest starting point.

**PROVE-10 is the one certified, landed win** (GO, VERIFY CONFIRMED): a start-state `memchr`
prefilter + unanchored `Dfa::find`, **8.8–29.5×** on prefilterable patterns, **0.97–1.01×**
controls, compile-neutral, **44/44** tests, 245-line additive patch (SPEC §0 #10; PROVE-10
REPORT §2). Disassembly confirms `memchr`'s aarch64 NEON `vshrn #4` movemask hot path
(REPORT §2c). This is the prefilter FLOOR every family inherits.

**The e-graph substrate is already present, not hypothetical.** `regex/src/egraph/` is 1253 LOC —
HIR equality saturation with rules `repetition/union/flatten/superset/redundant`, `translate.rs`,
`cost.rs`, `saturation_cache.rs` (R3 §3), on external `egraph`/`egraph-derive`/`csp-solver` path
deps. So "regex canonicalization by e-graph" is a SHIPPING technique here, not a research bet —
the open question (5C) is narrower: does *automata construction* (determinize/minimize/prefilter
selection) as e-graph+CSP extraction beat the classical `subset_construction` + `hopcroft_minimize`
already in `dfa.rs:218/291`?

**The bridge break** (`parse_that::regex::parser` E0432, PROVE-10 §3, PT-1 W4) is a pre-existing
re-export drift, not caused by any regex work — it must be repaired for the DFA subtest oracle
(49 subtests) to gate anything.

---

## 2. What won and lost elsewhere for THIS class

### 2a. Regex→automata construction (the "grammar-first DFA" axis)

- **Thompson NFA + subset construction + Hopcroft minimization** — the classical pipeline, and
  exactly what `bbnf-regex` already ships (`nfa.rs:449`, `dfa.rs` subset `:218` / Hopcroft `:291`,
  byte-equivalence classes from `sets/equiv.rs`). Battle-tested; the parity floor.
- **Brzozowski / Antimirov derivatives** — the algebraic, *grammar-native* alternative: a DFA is
  built directly from the expression by repeated derivative, no explicit NFA. Antimirov partial
  derivatives yield an NFA with ≤ (1 + #letters) states, "consistently smaller state graph than
  Thompson's and Glushkov's"
  ([Regular-expression derivatives re-examined](https://www.researchgate.net/publication/220676673_Regular-expression_derivatives_re-examined);
  [Grokipedia: Brzozowski derivative](https://grokipedia.com/page/brzozowski_derivative)).
  Owens–Reppy–Turon showed derivatives give directly near-minimal DFAs with tiny code — attractive
  for a *grammar-defined* engine because the derivative IS a grammar-to-grammar rewrite (ties 5C/5D
  and LOCK-3 GRAMMAR-HOMOIOUSIA). WON in verifiers/compilers (small, provable); LOST at raw scan
  throughput vs a table DFA — derivatives shine at construction/minimization, not the match loop.
  KATch (NetKAT, [arXiv 2404.04760](https://arxiv.org/pdf/2404.04760)) is the 2024 SOTA datapoint:
  symbolic derivatives + BDDs beat classical automata for equivalence at scale.
- **Verdict for us:** classical pipeline is the throughput floor (keep `find_at`); derivatives are
  the credible mechanism IF the engine goes grammar-first, because they collapse
  parse+determinize+minimize into one rewrite system an e-graph can host — but they are a
  *construction-time* win, gated on measured match-loop parity (the 5C "losses are named" clause).

### 2b. Prefilter / acceleration tier (PROVE-10's lineage — WON, measured here)

- **rust `regex-automata` meta-engine**: literal/prefilter → **lazy (hybrid) DFA** → one-pass DFA →
  bounded backtracker → PikeVM; prefilter runs on start-state entry, a hit triggers an *anchored*
  confirm at that offset only; literal extraction is exact (skip) vs inexact (confirm),
  order-preserving for leftmost
  ([burntsushi regex-internals](https://burntsushi.net/regex-internals/);
  [docs.rs hybrid DFA](https://docs.rs/regex-automata/latest/regex_automata/hybrid/index.html)).
  This is the reference architecture PROVE-10 transcribed. WON decisively (8.8–29.5×).
- **Prefilter tiers**: memchr/Two-Way (single/rare-byte) → **Teddy** (SIMD bucketed multi-literal,
  2–72 literals) → Aho-Corasick (tens of thousands)
  ([Teddy ICPP21](https://oaciss.uoregon.edu/icpp21/views/includes/files/pap205s4-file2.pdf)).
- **Hyperscan shufti / truffle / teddy** (NSDI'19): decompose regex into string + FA components,
  SIMD fast-path finds candidates, slow back-end confirms
  ([nsdi19-wang](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf);
  [Langdale Suricon16](https://suricon.net/wp-content/uploads/2016/11/SuriCon2016_GeoffLangdale.pdf)).
  shufti = low/high-nibble `vqtbl1q_u8` LUT AND ≤4 ops; truffle = arbitrary 256-set via a second
  table; teddy = multi-literal buckets. These are the W2 wide-entry tier PT-1 specs, and the local
  `bbnf-simd` `classify_chunk_nibble` (R7 §3) is the same idiom — reuse, don't re-derive.

### 2c. Lazy / hybrid DFA (PROVE-10's *unchosen* half — PT-1 W3)

- Lazy DFA builds states *during* search under a memory budget; on cache exhaustion or a **quit
  byte** it returns `MatchError` and the meta-engine falls back — **"never a wrong answer"**
  (R7 §4; [burntsushi](https://burntsushi.net/regex-internals/)). `LazyStateID` reserves tag bits
  (unknown/dead/quit/start/match) — the compact-cache encoding to copy
  ([LazyStateID](https://shadow.github.io/docs/rust/regex_automata/hybrid/struct.LazyStateID.html)).
  WON for compile-time on pattern classes whose eager DFA blows up (bounded repetition, large
  Unicode classes); the state-cap bail is the correctness guarantee.

### 2d. Classification / literal extraction

- The 1148-LOC `classify.rs` (`RegexClass` + `canonical_pattern()`, drives `FnDescriptor`
  specialization on the bbnf side) and `info/{literal_prefix, first, one_pass, dfa_size, width}` are
  the local analysis surface (R3 §3). External precedent for re-homing this as an IR *analysis pass*
  rather than a bespoke module: burntsushi's `regex-syntax` `Hir` carries analysis
  (`is_alternation_literal`, `literal` extraction) as HIR queries, not a separate classifier — the
  "classify becomes an IR analysis pass" (5A) is the mainstream shape.

---

## 3. Constraints our evidence base already imposes (binding, not optional)

- **SPEC §0 #09 REFUTED — no shim perf claims.** The `portable_simd`→NEON shim's 1.34–1.57× did
  NOT reproduce under paired interleaving (emitted 2× the movemask reduces). Any LOCK-5 SIMD tier
  (shufti/truffle/teddy) must claim speed from *real* `vqtbl1q_u8`/`memchr`-NEON, measured paired —
  never from a shim asymmetry. `bbnf-regex` is pure scalar + SmallVec (R3 §3), so its wins come from
  the `memchr` crate's own NEON backend (PROVE-10) and hand NEON intrinsics only.
- **Warm benches are disinformation; PMU is restricted on macOS.** Cold per-parse, N≥200, paired
  same-binary lanes, divan median ratio certified / absolutes directional (SPEC §0, PT-1 §3
  precepts 43–49). PROVE-10 asserts identical match COUNT between lanes before timing — the
  correctness gate is inseparable from the perf gate.
- **The type plane costs zero at runtime** (constitution v) — regex classification/specialization is
  a compile-time IR analysis; nothing the continuum adds may show up in the hot match loop.
- **Colocation + no god modules** (constitution vi). `classify.rs` at 1148 LOC and the flat
  `prefilter.rs` growing kernels are named god-module risks: PT-1 W2 already specs
  `automata/prefilter/{mod,memchr,shufti,teddy}.rs` and `automata/lazy/` as directory-modules.
- **Bootstrap fixpoint** (LOCK-7 tie): a bbnf-defined regex grammar must satisfy regen∘regen =
  regen byte-identical, committed stage-0. Grammar-first construction cannot introduce a
  non-deterministic build.
- **Y.13 consumer-less substrate forbidden** (constitution viii): every automata/prefilter/lazy
  artifact lands with its in-wave consumer (PT-1 names one per wave; a prefilter without a caller
  does not open).
- **44/44 + the 49 DFA subtests are the parity floor** (LOCK-5 task; the E0432 bridge must be
  repaired to unlock them). Any re-derivation (derivatives, e-graph automata) is gated on
  byte-identical match sets vs the classical engine.
- **The bbnf HIR is the isomorphism target, and it is `String`-shaped today** (§1) — so 5A/5B are
  not "wire two existing HIRs together"; they are "design the shared/functor'd HIR that does not yet
  exist," with the engine's `Hir` and IR's `ExprKind` as the two endpoints.

---

## 4. Traps named in comparable systems (the failure-mode census)

1. **"Never a wrong answer" is the lazy-DFA invariant** — a state-cap bail must return `Bail`, never
   a partial/wrong match, even when the cap is hit on the same step as an accept (PT-1 §8
   counterexample: bounded-repetition matching exactly at the cap boundary). The classic bug is a
   race between cap and match.
2. **Leftmost-first order preservation** under prefilter/Teddy — inexact literals must be
   order-preserving or the leftmost semantics break (burntsushi, R7 §4). A prefilter that changes
   *which* match wins is a HARD correctness failure, not a perf note.
3. **shufti losing to `memchr`'s internal vectorization** — `memchr2`/`memchr3` are already SIMD; a
   naive shufti LUT can lose on 4-byte sets nearly in-tier (PT-1 §8). The cost model must gate shufti
   on a *measured* crossover, not a fixed width — a pluggable cost input, not a hardcoded branch
   (feedback: pluggable-components).
4. **Lazy-DFA cache thrash erasing the compile win** — a match walk revisiting many distinct states
   pays determinization per sample. The gate must bind BOTH axes: compile win AND ≥0.98× throughput
   (PT-1 W3).
5. **E-graph saturation blowup** — equality saturation over HIR/automata can explode; the existing
   `saturation_cache.rs` + a schedule bound it. 5C's automata-as-extraction risks unbounded
   candidate generation; the classical pipeline is the extraction FALLBACK, and losses must be named
   negative results, not silently dropped.
6. **Derivative-DFA non-termination without normalization** — Brzozowski derivatives terminate only
   modulo an ACI (associativity/commutativity/idempotence) normal form; without it the state set is
   infinite ([derivatives re-examined](https://www.researchgate.net/publication/220676673_Regular-expression_derivatives_re-examined)).
   The e-graph's canonicalization is exactly the ACI-normalizer — a natural fit, but a trap if the
   normal form is incomplete.
7. **The "one regex home" duplication trap** (SPEC §4) — three homes exist today (§1). Any design
   that leaves a second scanner engine (the `parse-that-regex` JSON matcher) as a parallel regex
   substrate re-violates the law. The continuum must subsume or explicitly demote it to a
   grammar-emitted leaf.
8. **Bootstrap staging cycle** — a grammar-defined regex parser that the regex grammar itself needs
   to parse is a chicken-and-egg; the committed stage-0 must break it (LOCK-7 STAGED-BOOTSTRAP), or
   the meta-circular oracle (7D) keeps the hand parser as ground truth until measured parity.
9. **Prototype residue** — PROVE-10 left an in-bench `assert_eq!` instead of a `tests/` oracle
   (its own disclosure); PT-1 W1 promotes it. The named defect class (bench-only wiring,
   feature-flag limbo, hand-schema crutches) is hunted explicitly by the adversary wave.

---

## 5. Family-relevant evidence (presented, not advocated)

- **5A ONE-HIR** — precedent: burntsushi folds analysis into `Hir` queries (no separate classifier);
  in-tree cost is the disjoint `Hir` vs `ExprKind` types (§1) and the `String`-carried seam.
  Enabling fact: bbnf HIR already has `Repeat/Alt/Literal/Regex` — a regex sub-language is
  representable; the automata pass would consume bbnf IR directly.
- **5B LAWFUL-FUNCTOR** — precedent: two domain-fit IRs with round-trip oracles is the conservative
  path; the shared `egraph`/`egraph-derive`/`csp-solver` crates (1253-LOC egraph module) are the
  natural shared part. Constitution iii ("reasonable point, measured not asserted") is the arbiter
  between 5A and 5B.
- **5C AUTOMATA-AS-EXTRACTION** — the e-graph substrate SHIPS (1253 LOC, HIR saturation). The
  measured question is narrow: determinize/Hopcroft/prefilter-selection as saturation+CSP vs the
  classical `subset_construction`/`hopcroft_minimize` already present. KATch (2024) is the "symbolic
  beats classical for equivalence at scale" datapoint; but match-loop throughput is a table-DFA game
  — losses must be named (the clause demands it).
- **5D GRAMMAR-FRAGMENT** — precedent for "regex literals lower directly into the grammar's own IR":
  the IR already carries `RegexProgram`/`SimdScan` recognizers (`ir/src/lib.rs:389,417`); the
  standalone API surviving "as a thin front over the same machinery" mirrors how `regex` exposes
  `regex-automata` under a facade. PROVE-10's `dfa.find` seam (PT-1 W5) is the concrete lowering
  point; the GN-2.W1 ⇄ PT-1.W5 interlock (find_member vs dfa.find, no double-emit) is the live
  hazard.

---

## 6. Sources

External: [burntsushi regex-internals](https://burntsushi.net/regex-internals/) ·
[regex-automata hybrid DFA](https://docs.rs/regex-automata/latest/regex_automata/hybrid/index.html) ·
[LazyStateID](https://shadow.github.io/docs/rust/regex_automata/hybrid/struct.LazyStateID.html) ·
[Hyperscan NSDI'19](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf) ·
[Teddy ICPP21](https://oaciss.uoregon.edu/icpp21/views/includes/files/pap205s4-file2.pdf) ·
[Langdale Suricon16](https://suricon.net/wp-content/uploads/2016/11/SuriCon2016_GeoffLangdale.pdf) ·
[Regular-expression derivatives re-examined](https://www.researchgate.net/publication/220676673_Regular-expression_derivatives_re-examined) ·
[Brzozowski derivative](https://grokipedia.com/page/brzozowski_derivative) ·
[KATch NetKAT (2024)](https://arxiv.org/pdf/2404.04760).
In-tree: R3, R7, PT-1, PROVE-10 REPORT/VERIFY; `ir/src/lib.rs:234–417`;
`parse-that-regex/src/lib.rs`; `skinny/crates/bbnf-regex/src/lib.rs`; SPEC §0/§4.

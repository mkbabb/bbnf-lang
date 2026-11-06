# LOCK-5 — THE REGEX CONTINUUM — PASS-2 PRIOR-ART RESEARCH

Problem class: **a grammar-first regex engine whose HIR is a lawful sub-language of / functor
onto the bbnf HIR-IR, whose NFA→DFA→minimize→prefilter pipeline is recast as solver problems
where measured to win, and whose regex literals in grammars lower into the generator's own IR
with the standalone parse-that API a thin front.** No family allegiance below — every route
(5A ONE-HIR, 5B LAWFUL-FUNCTOR, 5C AUTOMATA-AS-EXTRACTION, 5D GRAMMAR-FRAGMENT) is weighed on
evidence. Citations are `path:line` (local, verified) or URL (external).

---

## 1. THE IN-TREE SUBSTRATE (what exists — the floor, and the traps within it)

**Two live IR trees hold incompatible regex substrate (the DIRECTIVES' central caution).**
- **skinny `crates/ir`** — regex leaf is an *opaque* `ExprKind::Regex { pattern: String }`
  (`skinny/crates/ir/src/lib.rs:250`); the enum is 10 variants incl. `Project`/`Discard`
  (`:234-271`) — `Project` is already present, so the LOCK-1/LOCK-3 `->` concern is live at
  HEAD, not future. `nullability` already routes `Regex` through `analyze(pattern)` (`:331`) —
  a String-parse on every fixpoint iteration (a cost the merged-fragment routes would erase).
  There is **no grammar-tier regex e-graph** in this tree.
- **top-level `crates/ir`** — regex is interned `GrammarENode::Regex(StringId)` beside
  `Literal(StringId)` (`crates/ir/src/egraph/node.rs:26-27`) with a **434-LOC grammar-tier
  regex e-graph** (`crates/ir/src/egraph/rules/regex.rs`): `DeduplicateAltBranches`,
  `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches` (`Alt([Regex,Lit,Regex])→Regex("a|b|c")`).
  Its header states these rules *delegate the algebra* to `bbnf_regex::algebra` helpers and
  are **duplicated against the HIR-tier rules in `bbnf-regex::egraph::rules`** (`regex.rs:9-15`)
  — Formulation B's "does the duplicate copy die?" is concrete and *countable*: the algebra is
  written once, but the **rule wrappers exist twice, once per tier**.
- **LAW (DIRECTIVES ¶1):** a result in one tree is not a result in the other; every number
  must name its tree and be re-run in the reconciled tree of record.

**The full engine lives in parse-that, the vendored skinny copy is SLIM.** The 6742-LOC
engine (HIR 1064, automata 1023, classify 1148, egraph 1253) is at
`parse-that/rust/regex/` (R3 §3). The skinny "vendored copy"
`skinny/crates/parse-that-regex/` is a **1419-LOC subset** — utf8 (hoehrmann/block),
number/eisel_lemire, integration/simd_scan_hook, and a scalar+NEON `classify_string_content`
(`.../src/lib.rs:783-842`). It carries **no HIR / no automata / no classify enum / no egraph**.
So "vendor the regex home" (GN-1) and "the 1148-LOC classify re-homed" (LOCK-5 task) both
begin from the *parse-that* engine, not this slim shim. Naming the slim crate "the vendored
copy" is itself a trap.

**Formulation-D substrate is partly real in the top-level recognizer plane, absent elsewhere.**
`derive_recognizers` / `BackendExpr` / `ByteClass` / `regex_desugar` / `check_one_pass_eligible`
appear in **skinny** (`skinny/crates/passes/src/lib.rs`, `codegen/src/lower/`,
`bbnf-regex/src/lib.rs`) — but in the **top-level** tree the recognizer plane is a whole
directory-module (`crates/ir/src/passes/recognizers/*`, `crates/ir/src/recognizer/{mod,plans,facts}.rs`)
and `one_pass` surfaces in `passes/csp_strategy/mod.rs`, `recognizers/{identifier,quoted_string}.rs`.
`BackendExpr::ByteClass` / `regex_desugar` were **not found in the top-level tree** — evidence
that D's tape-reaching leaf (`ByteClass` → tape emitter, per the typed-materialization
invariant) is *spec-only* in the reconciled tree and must be built, not relocated.

**Two `regex.bbnf` already exist and diverge (promote+de-dup, never greenfield — DIRECTIVES ¶2).**
- `parse-that/rust/bootstrap/regex.bbnf` — a clean **L4 spec** using `>>`/`<<` discard sugar,
  natural recursion (alternation→concat→quantified→atom→group), self-hosting parse-that's
  parser; but its `parse_generated()` **discards its AST** (DIRECTIVES ¶2 — "break-the-discard").
- `grammar/misc/regex.bbnf` — an older EBNF-flavoured variant with lookahead/lookbehind/
  back-reference and `{}` repetition, carrying real bugs (`decimal = /d+/`, `hex = /x[0-9a-fA-F]+/`
  missing their escapes). Divergent, not authoritative.
Both are the seed; the work is de-dup + promote + wire the AST, riding the bootstrapped grammar
(constitution clause i).

**The classify enum: 1148 LOC, the single largest file** (`parse-that/rust/regex/src/classify.rs`):
`RegexClass` (`:55`) with `canonical_pattern()` (`:132`), `classify_regex` (`:157`),
`classify_regex_from_hir` (`:177`). The task asks whether re-homing it as an IR analysis pass
*shrinks* it or merely relocates it — a LOC-before/after prototype is the only honest answer.

---

## 2. WHAT WON / LOST ELSEWHERE FOR THIS PROBLEM CLASS (external SOTA)

**Derivatives unify the regex leaf with the grammar (5A / 5C).** Owens–Reppy–Turon,
*Regular-expression derivatives re-examined* (JFP 2009,
<https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf>): DFA states **are** regexes,
transitions **are** the derivative operator — a direct, minimal DFA construction that skips the
NFA and (with smart constructors modulo ACI) yields near-minimal DFAs without a separate
Hopcroft pass. Might–Darais–Spiewak, *Yacc is dead* / *Parsing with derivatives*
(<https://arxiv.org/pdf/1010.5023>): the derivative operator extends to **context-free
grammars and parser combinators** — the same operator serves regex leaves *and* the combinator
core (LOCK-4). This is the strongest external argument that "one HIR, one rewrite theory"
(5A/5B) is not a stretch but the literature's default. **Loss noted:** Brzozowski derivatives
can blow up exponentially vs Glushkov in the worst case (search corpus, grokipedia/Owens) —
so derivatives are a *construction/minimization* candidate to measure, not an assumed win
(binds Formulation C's "measure the saturation ratio ρ + completeness ceiling").

**E-graph IS a tree automaton → DFA minimization = e-graph minimization (5C).** Wang et al.,
*E-Graphs, VSAs, and Tree Automata: a Rosetta Stone* (<https://remy.wang/reports/dfta.pdf>;
EGRAPHS 2024/25 lineage): "because an e-graph is a (bottom-up deterministic) tree automaton,
the **same minimization procedure** reduces the size of any e-graph." This is the theoretical
license for 5C — determinization/minimization as saturation + extraction over the *same* derived
`Language` as the HIR rewrite theory. **But the incumbent baseline is a strawman:** the
vendored minimizer at `parse-that/rust/regex/src/automata/dfa.rs:358` is a **naive Moore refine
with a global "restart refinement from the beginning" on every split** — *not* reference
Hopcroft O(n·k·log n). Formulation C's charter is right to demand: (a) price the *delete-the-
restart* null first, (b) control against a *correct* Hopcroft, not the restart version — an
e-graph minimizer that only beats a self-inflicted O(n²) restart has proven nothing.

**One-pass DFA: eligibility is build-and-bail, not a predicate (5D).** regex-automata's
`dfa::onepass` (<https://docs.rs/regex-automata/0.4.9/regex_automata/dfa/onepass/>, BurntSushi):
"**there is no separate pass** for determining whether the NFA is one-pass — it just tries to
build the DFA and bails out if construction discovers it is not." Once a match state is seen
under leftmost-first, no more transitions are added. This directly answers Formulation D's
`check_one_pass_eligible` question: the *sound* design is bail-during-build, and the
give-back/overlap hazard the DIRECTIVES name (`[a-z]+z` on `"abz"`, greedy give-back) is
**exactly** the case where preference-order and longest-match disagree — "if they disagree the
regex typically isn't one-pass," so build-and-bail rejects it automatically. A hand-written
eligibility predicate that tries to *pre-decide* this is the trap; the reference engine refuses
to write one.

**Prefilter ladder — the CONFIRMED win, and its ceiling (5C tier stage).** regex-automata's
meta strategy (literal/memchr → Teddy → Aho-Corasick prefilter; lazy/hybrid DFA with quit-byte
budget bail) is R7 §3–4 and is already *measured* in-campaign: PROVE-10 GO,
**8.8–29.5× on prefilterable patterns, 0.99× controls, compile-neutral, 44/44 tests**
(`prototypes/PROVE-10/{REPORT.md,VERIFY.md}`; SPEC §0 #10 CONFIRMED). Its own disclosed ceiling:
**memchr tier only (1/2/3 entry bytes)**; ≥4-byte entry sets fall to `Prefilter::None` (parity)
— PT-1.W2 (shufti/Teddy) is the named future tier. Hyperscan (NSDI'19), shufti/truffle nibble-
LUT, Teddy bucketed multi-literal (R7 §3) are the vetted vocabulary.

**Ragel — projection-carrying transitions (5D).** Ragel (colm.net; Wikipedia) embeds **user
actions on arbitrary FSM transitions** integrated into the regex, compiling to one backtrack-
free DFA. This is the precedent for Formulation D's "can a recognizer carry `->` projections on
its transitions and still compile to one DFA+prefilter per site?" — Ragel proves the *shape* is
realizable; the open question is whether bbnf's typed `->` survives the DFA fold to the tape
emitter (typed-materialization invariant) rather than being erased at determinization.

**MLIR — the coproduct/bridge precedent (5B).** `unrealized_conversion_cast`
(<https://mlir.llvm.org/docs/DialectConversion/>) exists to *inter-mix two type systems during
progressive lowering*, then `-reconcile-unrealized-casts` deletes the casts that became no-ops.
This is precisely Formulation B's "joint coproduct (`Left(HirENode)|Right(GrammarENode)`),
bridge rewrites folded pre-emission" — MLIR's lesson is that the cross-domain vehicle is only
worth its weight if the bridge casts **provably vanish before emission**; a coproduct whose
casts persist is the anti-pattern the reconcile pass exists to hunt. Formulation B must exhibit
the vanishing, not assert it.

---

## 3. CONSTRAINTS OUR EVIDENCE BASE ALREADY IMPOSES

- **Constitution (PORTFOLIO head).** (i) grammar-totality — the regex grammar IS bootstrapped +
  bbnf-defined; (ii) **true isomorphism** — LOCK-5 decides the lawful form of HIR↔HIR-IR
  isomorphism, every other lock consumes the verdict; (v) type plane costs zero at runtime;
  (vi) colocation + extreme parsimony, no god modules, no name-gates; (viii) **Y.13 —
  consumer-less substrate is forbidden** (binds `Look`/`Class`: a leaf carried-but-never-read is
  illegal); (ix) extant GN/PT codex is baseline, divergences are *named* supersessions.
- **Typed-materialization invariant (MEMORY + DIRECTIVES ¶cross-lock).** Every `->` reaches the
  tape emitter; the new `ByteClass`/`Class`/`Look` leaves must reach it too — inference composes
  types, never loses them. Every `ExprKind`/`BackendExpr` edit is LOCK-3-governed.
- **Profiling law (SPEC §0, §4).** Cold per-parse, paired same-binary interleaving, N≥200,
  multi-window; **ratios certified, absolutes directional**; macOS restricts userspace PMU →
  wall-clock paired ratio is the certified metric (PROVE-10 §5). Warm benches are disinformation.
- **Type-plane routing (DIRECTIVES A).** `Class`/`Look` typing must route through LOCK-1's real
  HM + row-poly + widths, **not** skinny's stub `BuiltinTy` — and the census must be re-run on
  the *current 10-variant* enum (incl. `Project`), not the stale 8-variant snapshot.
- **CSP double-home (DIRECTIVES ¶cross-lock).** The prefilter/tier CSP is double-homed with
  LOCK-2's scan-plan CSP; **one home must be decided.** LOCK-5 owns HIR→DFA→classify; LOCK-9-9C
  consumes or retires. The Axis-1 type-plane choice gates LOCK-4-4D, LOCK-9-9C, LOCK-3-3C.
- **rust/ts face is a named LOCK-6 gap** (`Class`/`Look`/`ByteClass` + automata crossing to TS,
  PROVE-14 tape).

---

## 4. THE TRAPS (named, with the counterexample each demands)

1. **The two-tree measurement fraud.** A regex-egraph rule-death count in the top-level tree is
   meaningless in skinny (which has no grammar-tier regex egraph). *Every* number states its
   tree and re-runs in the reconciled tree. (DIRECTIVES ¶1.)
2. **The strawman minimizer.** Beating `dfa.rs:358`'s global-restart Moore proves nothing;
   control against correct Hopcroft, and price the delete-the-restart null *first* (C).
3. **The gate/corpus mismatch.** Formulation C's gate cites 50 patterns; the corpus holds 26 —
   re-cut or grow, and state which (DIRECTIVES C). Un-audited, any ρ/completeness number is void.
4. **Consumer-less enum leaf (Y.13).** Adding `Look{kind}` to the shared enum without a
   grammar-side reader is forbidden. Build a test that FAILS if `Look` is carried-but-never-read,
   or demote it to an annotation (DIRECTIVES A). Same discipline for `Class`.
5. **The god-type.** One type serving the saturation cache (`Eq+Hash`) *and* the IR
   (arena/`ExprId`/span/`Serialize`) risks a kitchen-sink type. Exhibit a single type meeting
   both without a god-type, or keep two joined by a total functor (5B) (DIRECTIVES A/B).
6. **Relocation masquerading as reduction.** Re-homing the 1148-LOC classify as an IR pass must
   *shrink* it (prototype LOC before/after), else it is a move dressed as a win (DIRECTIVES A).
7. **The eligibility predicate that lies.** A hand-written `check_one_pass_eligible` that
   pre-decides give-back soundness will mis-accept `[a-z]+z`; the reference answer is
   build-and-bail (regex-automata). Soundness test on the overlap case is mandatory (D).
8. **The silent wildcard.** Does `derive_recognizers` actually *select* on `ByteClass`, or does
   its `_ => {}` swallow it? Prove the chooser fires (D) — a wildcard-swallowed leaf is a
   no-silent-epsilon violation.
9. **The double-emit.** PT-1.W5 (`dfa.find`) ⇄ GN-2.W1 (`find_member`): a self-loop scan site
   that is *also* an unanchored regex leaf gets both lowerings. One recognizer dispatch, one
   lowering per site; grep the generated file for exactly one (PT-1 §8, INDEX interlock).
10. **Warm-bench / two-arm asymmetry self-deception** — the PROVE-09 refutation is law: a shim
    "win" that did not reproduce under paired interleaving. Every LOCK-5 number is cold, paired,
    same-binary, multi-window (SPEC §0-09, PT-1 §5).
11. **Prefilter tier ceiling as a hidden regression.** Wide entry sets (≥4 bytes) sit at
    `None`-parity today; claiming the ≥15× on such a site is the classic over-claim. The memchr
    tier is 1/2/3 bytes only (PROVE-10 §5).
12. **The discard trap.** `parse-that/rust/bootstrap/regex.bbnf`'s `parse_generated()` throws its
    AST away — a grammar path that parses-then-discards is a consumer-less grammar (Y.13 again).

---

## 5. ROUTE-LEVEL VERDICT SEEDS (evidence, no allegiance)

- **5A ONE-HIR** has the strongest external theory (derivatives close regex⊂CFG⊂combinators,
  *Yacc is dead*) and the cleanest constitution fit (clause ii "one system"), but must survive
  the god-type test (#5), the consumer-less-leaf test (#4), and prove classify *shrinks* (#6)
  under real HM typing — all on the 10-variant HEAD enum.
- **5B LAWFUL-FUNCTOR** is licensed by MLIR's coproduct/reconcile precedent, but only earns its
  keep if bridge casts provably vanish pre-emission and the duplicate grammar-tier rule copy
  *dies by a counted number* (concrete: the `regex.rs` rule wrappers duplicated across tiers).
- **5C AUTOMATA-AS-EXTRACTION** is licensed by the Rosetta-Stone (e-graph≡tree-automaton) but is
  the most exposed to the strawman-baseline trap (#2/#3); it must beat *correct* Hopcroft and
  price the restart-delete null, and characterize the finite-rewrite completeness ceiling.
- **5D GRAMMAR-FRAGMENT** has the Ragel precedent for transition-carried projections and the
  regex-automata build-and-bail answer for eligibility, but must prove `ByteClass` reaches the
  tape emitter (invariant), the chooser fires (not `_ => {}`), and cold-paired that a straight-
  line byte-class scan actually beats prefiltered `Dfa::find` (a tie reported as a tie).

Cross-cutting settled fact: the prefilter tier itself (any route's "tier stage") is **already
GO** (PROVE-10, SPEC §0 #10) — the LOCK-5 novelty is *where the tier decision is homed* (a global
SIMD-LUT/codegen-budget CSP shared across sites, vs a pluggable cost-sorted table) and whether
the automata pipeline *upstream* of it is recast as saturation — not the prefilter's existence.

---
Sources (external): re-deriv (Owens/Turon) khoury.northeastern.edu/home/turon/re-deriv.pdf ·
Yacc-is-dead arxiv 1010.5023 · Rosetta-Stone remy.wang/reports/dfta.pdf · regex-automata
onepass docs.rs 0.4.9 · Ragel colm.net · MLIR DialectConversion mlir.llvm.org · Hyperscan
NSDI'19 + Teddy/shufti (R7 §3). Local: paths cited inline, all `ls`/`grep`/`Read`-verified.

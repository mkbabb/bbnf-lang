# LOCK-5 · pass-3 · research-D — FAMILY 5D: THE GRAMMAR-FRAGMENT

**Charter.** Regex literals in grammars lower DIRECTLY into the grammar's own IR — no separate
engine on the generator path; the standalone parse-that regex API survives as a thin front over the
same machinery. This report makes the mechanism concrete, base-pinned to the substrate, and answers
the family-tagged questions Q7–Q11 with run evidence, integration seams as `path:line`, prior art,
and the exact prototype obligations that move the family.

Settled ground (XL-C, not re-opened): grammar tier keeps `Regex(StringId)` opaque; the engine tier
keeps a rich `Hir` grown with `Class`/`Look`; a cheap `string`/`format!` bridge over `simplify_hir`
joins them. 5D rides the generator path where the DIRECT lowering lives — it does NOT re-argue
type-plane ownership.

---

## THE MECHANISM MADE CONCRETE

5D's generator path is a three-stage collapse of the classical `pattern → HIR → NFA → DFA → find_at`
pipeline into grammar IR:

1. **Class source.** A regex leaf `Regex { pattern }` is analyzed once at compile time into an
   over-approximating byte class (its FIRST set) plus a give-back classification.
   `bbnf_regex::analyze(pattern) -> RegexFacts { first: FirstSet, kind: RegexKind, hir, string }`
   (`skinny/crates/bbnf-regex/src/lib.rs:45`) is the substrate surface; `RegexKind` today
   distinguishes only `QuotedString` (`:18`) vs generic — the analysis crate is deliberately thin,
   the rich 1148-LOC `classify` lives in the vendored `parse-that-regex`.

2. **Lowering decision (the FOLLOW-guard).** If the leaf is FOLLOW-sound (its trailing loop-class is
   disjoint from grammar FOLLOW at its position), it lowers to a **straight-line direct class-scan** —
   a `Recognizer::ByteClass` membership scan over the tape, no automaton. If it is genuinely
   give-back-prone it lowers to `Seq[Repeat, FOLLOW-guard]` or retreats to the DFA `Dfa::find`
   fallback. This is exactly the Glushkov/McNaughton–Yamada position-automaton construction
   (FIRST/LAST/FOLLOW over a marked expression) done as an IR lowering rather than an automaton build —
   the direct path IS the ε-free position NFA, specialized when deterministic.

3. **Tape emission.** The class-scan emits `BackendExpr::ByteClass { set }` reaching the offset tape;
   a capture group `(…)` inside the leaf lowers to a grammar `Project` node byte-identical to a
   hand-authored grammar capture. The DFA path (PROVE-10's `Dfa::find` prefilter tier, 8.8–29.5×)
   survives ONLY as the narrow fallback for the genuine-give-back trio.

**The standalone API survives thin.** parse-that's public `regex::` surface re-exports
`bbnf_regex::*` verbatim (`parse-that/rust/parse_that/src/regex/mod.rs:12`) and adds a front
(`parse_generated`, `:34`). Under 5D that front stays; it front-ends the same `analyze` + IR lowering
the generator uses. The engine is not duplicated on the generator path — it is *inherited*.

---

## Q7 — THE FOLLOW-SOUND ONE-PASS FRACTION (measured, both metrics fresh)

RAN on the live corpus: `skinny/grammars/json.bbnf` (15 rules) + `grammar/google-sheets/
google-sheets.bbnf` (39 rules, skinny dialect) + an injected canonical counterexample
`word = /[a-z]+/ ; sentence = word "and" word ;`. 16 `Regex` leaves. FOLLOW is a genuine Changed-flag
fixed point (nullable → first → follow closures, LLVM-style `while changed`, per-`ExprId`, `Ref`
sites bridge inter-rule), dual to the existing `first_bytes` at `skinny/crates/passes/src/lib.rs:911`;
FOLLOW **converged** for all three grammars. Disjointness is `first(C) ∩ FOLLOW(leaf) = ∅` with
`first(C)` from `bbnf_regex::analyze(pattern).first`.

| metric | value |
|---|---|
| brief-literal `first(C) ∩ FOLLOW = ∅` (straight-line lowering) | **3/16 = 18.8%** |
| bounded — give-back impossible by construction (no top-level unbounded `*`/`+`) | **6/16** |
| give-back-prone (trailing unbounded run) AND FOLLOW-sound | **3/10 = 30.0%** |
| effectively one-pass under `Seq[Repeat,suffix]` + grammar FOLLOW (bounded ∪ prone-sound) | **≈ 12/16 = 75%** |

**The 18.8% is a red herring, and the correct read INVERTS the Ragel trap.** The raw predicate is
sound but not complete — it over-flags on the *leading* first-byte even where give-back is
structurally impossible. Adjudicated per-leaf: 6/16 are bounded (fixed-length or delimiter-terminated:
`/[tT][rR][uU][eE]/`, `/[lL][eE][tT]\(/`, `/=?/`, …) — one-pass by construction, DFA never needed;
3/16 are conservative false-positives (`json::ws` `/[ \t\n\r]*/`, `cell_ref`, `range_end /\$?\d+/`)
whose loop-class is actually FOLLOW-disjoint or re-consumed by a nullable follower; **only 3/16 are
genuine give-back** — `counterexample::word` `/[a-z]+/` (follower `"and"` requires `'a'`, loop-class
`∋ 'a'` — THE trap), `sheets::string` `/"([^"]|"")*"/` (doubled-quote escape ambiguity),
`sheets::identifier` `/[A-Za-z_][A-Za-z0-9_.]*/` (trailing letter-class abuts a letter-led follower).

**Verdict for the generator-path architecture (I-1).** ~12/16 leaves are one-pass-safe; only 3
genuinely keep the DFA in play. **The direct class-scan is the DEFAULT generator path, gated by a
FOLLOW-guard; the DFA retreats to a narrow fallback for the genuine-give-back trio.** This DEFEATS
`check_one_pass_eligible` in the vendored `one_pass.rs`, whose `Repetition` arm recurses into `[a-z]`
and returns one-pass=`true` — greenlighting a straight-line lowering that would reject `"a and b"`.
The FOLLOW predicate refuses it (`overlap=[a]` asserted); the counterexample lands NON-disjoint, so
the predicate is sound exactly where the incumbent is not.

**The one confinement caveat, binding on any production predicate.** `analyze().first`
*under*-approximates alternations (for `/'…'!|[A-Za-z_]\w*!/` it returns `Exact {'\''}`, dropping the
letter branch) and returns `UNKNOWN` for `sheets::number` (analyzer limitation, not a grammar
property). A sound FOLLOW-sound verdict must draw `first(C)` from an **over**-approximation — the DFA
start-state class set (which PROVE-10's prefilter already computes as its entry-byte set) or a fixed-up
analyzer. The direct path's soundness rests on `first(C)` being an over-approximation; this is the
family's sharpest correctness obligation.

---

## Q8 — THE DIRECT-LOWERING PROTOTYPE (the substrate today, and what must be built)

**The substrate has no direct class-scan and no `ByteClass` node.** The only recognizer
`derive_recognizers` emits is `Recognizer::SimdScan { mode: SimdMode::Exact, alphabet:
StructuralAlphabet { bytes }, site: PreEntry }` (`skinny/crates/passes/src/lib.rs:361`), and it fires
ONLY when a literal is one of the structural bytes `{ } [ ] , : "` or a `Regex` leaf whose
`analyze().hir.kind == RegexKind::QuotedString` (`passes/src/lib.rs:342-353`). A general `[a-z]` class
contributes NOTHING — it is the **masked wildcard** the directive names: the derivation silently drops
every non-structural, non-QuotedString class. The `Recognizer` enum is at `skinny/crates/ir/src/
lib.rs:416` (`SimdScan` at `:417`); there is NO `ByteClass` variant and NO `BackendExpr::ByteClass`
anywhere in `ir`, `codegen`, or `passes` (grep: zero hits). The emitted recognizer comment is visible
in the generated artifacts (`codegen/src/json_templates/generated.rs:10`, `runtime/src/grammars/json/
generated.rs:10`: `recognizer: SimdScan Exact PreEntry alphabet="{}[],:\""`).

**What the prototype must build (concrete seams):**
- Add `Recognizer::ByteClass { set: ByteSet256, site }` at `ir/src/lib.rs:416`, fed from
  `analyze(pattern).first` (OVER-approx per Q7's caveat), not the QuotedString gate.
- Extend `derive_recognizers` (`passes/src/lib.rs:338`) to emit it for every FOLLOW-sound regex leaf —
  the Q7 FOLLOW fixed point becomes the derivation's gate, replacing the `RegexKind::QuotedString`
  test that masks non-structural classes.
- Route `BackendExpr::ByteClass { set }` to the tape through the codegen lower spine
  (`skinny/crates/codegen/src/lower/tape_plan.rs`, `sink_only.rs`) — a membership scan emitting a tape
  span, cold-pairable against `Dfa::find`.
- Capture-leaf → `Project`: a regex capture group lowers to the grammar's own `Project` node; the
  oracle is byte-identity of emitted tape vs a hand-authored grammar capture of the same span.

**The cold-pair the prototype must measure:** `ByteClassRecognizer` membership scan (direct, no
automaton) vs prefiltered `Dfa::find` (PROVE-10's `Prefilter::from_dfa` + `memchr` skip, certified
8.8–29.5× on prefilterable, 0.99× on controls) on the SAME leaf over the same skip-heavy corpus,
paired same-binary N≥200 cold. The hypothesis 5D defends: for FOLLOW-sound leaves with a *rare*
leading class the direct scan is parity-or-better than `Dfa::find` (both reduce to byte-membership
skip), so the automaton is off the generator path; where the class is *dense* (`[a-z]+`) both are
membership-bound and the direct path wins by eliding the DFA compile+dispatch. Tape-output equality is
the correctness gate; the ratio decides whether `Dfa::find` is reachable on the generator path or only
the give-back fallback.

---

## Q9 — ONE GRAMMAR-DRIVEN REGEX PATH (break-the-discard; SPEC-ONLY, gated LOCK-7/7A)

**The discard is located.** The bootstrapped regex parser's entry is
`parse-that/rust/parse_that/src/regex/generated.rs:2288` — `pub fn parse_generated(pattern: &str) ->
bool`. It returns a success bit only; **the parsed AST is discarded.** Break-the-discard = make it
return its `Hir` (or the grammar's own IR node), so the grammar-driven parse *threads through* rather
than proving yes/no.

**The break is already prototyped in a sibling scratch** (existence proof it is mechanical, not a
rewrite): a parse-that scratch carries `regex/mod.rs:34` `pub fn parse_generated(pattern: &str) ->
Result<Hir, ParseError>` + `parse_generated_with(:39)` — the discard replaced by a real AST return.
The generated parser already builds the structure; only the return type discards it. Promotion is
gated on LOCK-7/7A (bootstrap staging) and SPEC-ONLY until then.

**The de-dup is concrete: two regex grammars exist and must collapse to one.**
- `grammar/misc/regex.bbnf` (27 LOC) — the OLD EBNF form: `,`-concatenation, no `>>`/`<<` cuts,
  hand-shaped `expression = ( term + , "|"? ) +`. Legacy.
- `bootstrap/regex.bbnf` (72 LOC) — the L4 self-hosting spec: naturally-recursive (alternation →
  concat → quantified → atom → group → alternation), `>>`/`<<` directional cuts, full class-escape /
  unicode-escape coverage, explicit "survives the IR optimizer" leaf-inlining note.

De-dup = **retire `grammar/misc/regex.bbnf`, promote `bootstrap/regex.bbnf`** as the single
bbnf-defined regex grammar, and point the now-AST-returning `parse_generated` at it. Grammar-totality
picture: one bbnf-defined regex grammar feeding an AST-returning parse satisfies constitution clause
(i) (the regex grammar IS bootstrapped and bbnf-defined) ON the generator path — the regex literal is
parsed by the grammar's own machinery, lowered by the grammar's own IR, no separate hand engine on
that path. The standalone `Dfa` survives as the give-back fallback and the public API's match backend
(thin front), per charter.

---

## Q10 — THE GRAMMAR-TIER REGEX E-GRAPH FIRE CENSUS (measured; the projection barrier)

RAN over a 33-pattern `Alt`-of-regex corpus in the only tree where the grammar-tier regex e-graph
physically exists (`crates/ir/src/egraph/rules/regex.rs`), via the production saturation path
`build_and_saturate_reported`, tabulating `per_rule.work = node_delta + union_delta`. Two topologies,
differing only in branch wrapping:

| rule | **bare** `Alt([Regex,…])` | **projected** `Alt([Map(Regex,Tag),…])` |
|---|:---:|:---:|
| `superset-absorb-alt` | 11 | **0** |
| `union-merge-alt` | 19 | **0** |
| `fuse-alt-regex-branches` | 30 | **0** |
| `deduplicate-alt-branches` | 3 | **0** |
| saturation | 2 iters, total_applied=63 | **1 iter, total_applied=0, no unions** |

**The Map barrier makes the grammar-tier regex e-graph exactly inert wherever `->` projections
dominate.** The three regex-specific rules resolve a branch's regex via `class_regex(child)`, which
scans the child's e-class for a bare `GrammarENode::Regex` node and returns `None` on a `Map { inner,
fn_id }` child (a Map e-class holds no bare `Regex`); `FuseAltRegexBranches` sets `all_fusable=false`
on a Map child. The projected e-graph reaches its fixed point in ONE iteration having applied ZERO
rewrites — not "≤ noise", exactly zero. Real BBNF alternations almost always carry per-branch tags
(`(a -> A | b -> B)`), so the Map-wrapped case is the common case.

**Consequence for 5D.** 5D's direct lowering does NOT route through the grammar-tier e-graph — it
lowers the leaf straight to `ByteClass`/`Seq[Repeat,guard]`. The census confirms the grammar-tier
e-graph has essentially no consumer on the projection-dominated generator path (Y.13: a consumer-less
substrate is forbidden). This is *evidence FOR* the 5D thesis: the direct path needs no grammar-tier
regex e-graph, and keeping one on the generator path would be an un-consumed substrate. (The
engine-tier `Hir` e-graph — `simplify_hir` and the `crates/ir` `repetition`/`union`/`flatten`/
`superset` rules — is a separate, live thing on the fallback DFA path; this census concerns only the
*grammar*-tier copy.)

---

## Q11 — THE `->` REPRESENTATION (binding on bridge projection soundness; confirm with LOCK-1)

Q10 exposes the load-bearing dependency: **whether the regex tier can see through a projection is
entirely determined by how LOCK-1 represents `->`.**
- **If `->` is an inner-class `Map { inner, fn_id }` e-node** (current), the Map wraps the regex node
  in a distinct e-class; `class_regex(child)` finds no bare `Regex` and the regex rules are inert
  (measured 0/0/0). The direct 5D lowering must then unwrap the Map to reach the class — the
  projection is a *barrier* the lowering steps through, and the capture-leaf → `Project` byte-identity
  (Q8) is what preserves the projection across the unwrap.
- **If `->` is a typed EDGE** (an annotation on the same e-class carrying the class node, not a
  wrapping constructor), the regex node stays visible; the rules could fire *through* the projection
  and the direct lowering reads the class without unwrapping.

The ratified representation is binding on the bridge's projection soundness and on the `Project`
byte-identity oracle. **This must be confirmed with LOCK-1** (its `->` / document-shape family) — it is
a cross-lock fact 5D consumes, not one 5D decides. The 5D prototype's `Project` byte-identity gate (Q8)
is only meaningful once the representation is fixed: it asserts the lowered projection is byte-identical
to a hand-authored grammar capture *under that representation*.

---

## PRIOR ART (with citations)

- **Glushkov / McNaughton–Yamada position automaton; Berry–Sethi.** The ε-free NFA built directly from
  FIRST/LAST/FOLLOW over a *marked* regex. 5D's direct lowering IS this construction as grammar IR —
  the FOLLOW-guard (Q7) is the Glushkov FOLLOW relation, and a FOLLOW-sound leaf yields a
  *deterministic* position automaton (no subset construction). Classical result: the position
  automaton is deterministic iff the expression is "one-unambiguous" (Brüggemann-Klein & Wood, 1998,
  *One-unambiguous regular languages*) — the exact property Q7 measures per-leaf against grammar
  FOLLOW.
- **regex-automata `dfa::onepass` (Cox).** The one-pass condition — at each state, per input byte at
  most one non-dead transition — is the automaton-level statement of FIRST/FOLLOW disjointness;
  regex-automata reserves it for capture-without-backtracking. 5D generalizes it to grammar FOLLOW
  (the follower obligation Ragel ignores).
- **Ragel** (state-machine compiler, action-embedded FSMs) — the give-back trap. Ragel `scanner`
  machines use longest-match with backtracking; a naive embedded regex over a shared alphabet with its
  follower is exactly `word "and" word`. 5D's FOLLOW-guard is the discipline Ragel's longest-match
  semantics paper over.
- **flex / alex / GHC lexers** — maximal-munch DFA lexers: the *classical DFA-central* answer 5D
  displaces on the ≈75% one-pass-safe fraction, retreating the DFA to the give-back trio.
- **Owens–Reppy–Turon (2009), *Regular-expression derivatives re-examined*; Brzozowski.** Derivative
  construction builds a DFA directly from the expression with no explicit NFA — the archetype of "no
  separate engine, lower the regex straight to the recognizer," a candidate substrate for the direct
  class-scan when a leaf is deterministic.
- **egg / egglog e-class analyses.** The Map-barrier (Q10) is the e-graph "opaque constructor blocks
  structural rules" pattern: rules keyed on a bare node kind cannot see through a wrapping constructor
  without an e-class analysis / conditional that looks inside — the mechanism behind the measured
  0/0/0.

---

## NAMED RISKS AND COUNTEREXAMPLES

- **`first(C)` under-approximation silently unsoundens the FOLLOW-guard.** *Counterexample built:*
  `/'…'!|[A-Za-z_]\w*!/` — `analyze().first` returns `Exact {'\''}`, dropping the letter branch; a
  guard drawn from it greenlights a lowering that mis-handles the letter-led alternative. *Mitigation:*
  draw `first(C)` from the DFA start-state class set (over-approx), which PROVE-10's prefilter already
  computes. The family's sharpest correctness obligation.
- **Analyzer-opaque leaves stall the fraction.** `sheets::number` returns `first=UNKNOWN`;
  indeterminate, not a real give-back. *Mitigation:* over-approximate UNKNOWN to the full alphabet
  (conservative — forces guard/fallback, never a wrong direct scan).
- **The direct class-scan loses to `Dfa::find` on dense classes.** *Counterexample the prototype must
  build:* a dense `[a-z]+` leaf where the compiled DFA's anchored scan matches the membership scan — if
  the direct path shows no win, the DFA fallback is not "narrow" and the automaton stays on the
  generator path. *Mitigation:* the Q8 cold-pair is the arbiter; a loss keeps `Dfa::find` reachable, a
  win removes it from the generator path.
- **The `Map` barrier forces an unwrap the direct lowering gets wrong.** *Counterexample:* a projected
  capture leaf `(/[0-9]+/ -> Digits)` whose `Project` lowering drops the tag during the Map-unwrap.
  *Mitigation:* the Q8 byte-identity oracle vs hand-authored capture, under LOCK-1's ratified `->`
  representation (Q11).
- **Break-the-discard is gated and cannot land unilaterally.** Promoting `parse_generated` to
  AST-returning and retiring `grammar/misc/regex.bbnf` is SPEC-ONLY until LOCK-7/7A ratifies bootstrap
  staging; landing it early forks the bootstrap fixpoint.

---

## WHAT A PROTOTYPE MUST TEST TO MOVE THIS FAMILY

1. **The over-approx FOLLOW-guard.** Re-run Q7's fraction with `first(C)` from the DFA start-state
   class set (not `analyze().first`); confirm the genuine-give-back count stays 3/16 and no leaf flips
   sound→unsound. Gate: `word "and" word` still NON-disjoint; the three conservative false-positives
   (`ws`, `cell_ref`, `range_end`) now correctly sound.
2. **The `ByteClass` direct path end-to-end.** `Recognizer::ByteClass` (`ir/src/lib.rs:416`) + its
   emission in `derive_recognizers` (`passes/src/lib.rs:338`) + `BackendExpr::ByteClass` reaching the
   tape (`codegen/src/lower/tape_plan.rs`) — a ByteClassRecognizer that *actually fires* on a
   non-structural class (defeating the masked wildcard). Gate: the emitted tape for a FOLLOW-sound leaf.
3. **The cold-pair.** Direct membership scan vs `Dfa::find` on the same leaf/corpus, paired
   same-binary N≥200 cold, both a rare-class and a dense-class leaf. Gate: the ratio decides DFA
   reachability on the generator path.
4. **Capture → `Project` byte-identity**, under LOCK-1's ratified `->` representation (Q11). Gate:
   emitted tape byte-identical to a hand-authored grammar capture.
5. **Break-the-discard (SPEC-ONLY).** `parse_generated → Hir`/IR at
   `parse_that/regex/generated.rs:2288`; `grammar/misc/regex.bbnf` retired in favor of
   `bootstrap/regex.bbnf`; the AST threaded through one grammar-driven regex path. Gate: grammar-totality
   picture with the AST consumed, not discarded — held behind LOCK-7/7A.

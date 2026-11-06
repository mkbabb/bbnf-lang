# LOCK-5 — THE REGEX CONTINUUM — pass-1 SYNTHESIS (FAMILY-SPECS)

Synthesis of research-{A,B,C,D,priorart}. One cogent specification per family while the
routes remain incompatible; then the incompatibility map; then the two sharpest
uncertainties as runnable prototype briefs. Elegance earns nothing on its own — a family
stalled at a missing primitive as hard as the original problem is marked BLOCKED-candidate.

---

## 0. THE LOAD-BEARING TERRAIN FACT (resolves a cross-report contradiction)

The five reports cite two *different* grammar-IR types at two *different* paths, and this is
not a disagreement — it is **two parallel repo snapshots** the synthesis must hold in one hand:

| | Top-level `crates/ir` (workspace HEAD) | `skinny/crates/ir` (restart tree) |
|---|---|---|
| Grammar IR type | `IrNode` (`crates/ir/src/types/node.rs:30`) | `ExprKind` (`skinny/crates/ir/src/lib.rs:234`) |
| Regex leaf | `IrNode::Regex(StringId)` — **interned** id (`:36`) | `ExprKind::Regex { pattern: String }` — **opaque string** (`:250`) |
| E-graph node | `GrammarENode` derives `Language` (`crates/ir/src/egraph/node.rs:24`) | none inline; `backend_egraph.rs` seam |
| Grammar-tier regex rules | `crates/ir/src/egraph/rules/regex.rs` (434 LOC) | absent |
| classify | partly re-homed: `crates/ir/src/passes/materialization/classify.rs` | degenerate stub `skinny/crates/bbnf-regex/src/lib.rs` (322 LOC, 3-pattern table) |
| Regex engine | sibling repo `../parse-that/rust/regex` (6742 LOC, READ-ONLY) | vendored `skinny/crates/parse-that-regex/` (3022 LOC) |
| Backend regex leaf | `Recognizer`/`GrammarENode` path | `BackendExpr::RegexProgram { pattern, span_kind }` (`:389`) |

**Consequence for adjudication.** 5B/5C's substrate claims (`IrNode`, `GrammarENode`, the
434-LOC duplicate rules, the internal `translate.rs` functor) are all real *in the top-level
tree*. 5A/5D's substrate claims (opaque `Regex{pattern:String}`, four redundant string
re-parsers, the 3-pattern classify stub) are all real *in the skinny tree*. The families are
architectural centers, snapshot-independent; but any pass-2 prototype must **name its tree**,
and THE GESTALT must reconcile the two (the skinny tree is the graduating tree of record per
skv22; the top-level `crates/ir` is the more mature regex-egraph substrate). This reconciliation
is a LOCK-5→GESTALT obligation, recorded here, not resolved by any single family.

---

## FAMILY 5A — ONE-HIR (regex HIR is a *sub-language* of the bbnf HIR)

**Architecture.** There is one node algebra. Regex is the fragment of it carrying no rule
reference and no projection: `{Seq, Alt, Repeat, Optional, Literal, Class, Look}` = the bbnf
HIR minus `{Ref, Project, Annotation, Discard}`. Add exactly two leaves to the grammar
`ExprKind`/`IrNode` — `Class{set, negated}` (the one consuming leaf; `ByteSet256` promoted into
`ir`) and `Look{kind}` (the one zero-width leaf) — and the regex HIR *is* a subset of the bbnf
HIR: one Rust type, one crate. The GHC-Core / MLIR move: one small core, regex is the dialect
restricted to its class/assert/repeat fragment. Maximal reading of constitution (ii):
not isomorphism-to-a-point but **identity** — "there is nothing to map."

**Mechanism.** (1) regex text parses through the bootstrapped `grammar/misc/regex.bbnf` whose
`->` projections target `ExprKind` constructors directly (`char_class -> Class`,
`anchor -> Look`, `expression -> Alt`, …) — the hand-written regex parser retires. (2)
`nullability` gains total arms `Class => false`, `Look => true`, deleting the `analyze(pattern)`
string-escape at `lib.rs:331`. (3) `first_bytes`/`span_kind`/`regex_type` collapse into one
structural analysis over the unified tree; the 1148-LOC `classify.rs` is re-expressed as passes
deriving `{Whitespace,QuotedString,Numeric}` *structurally* (is-this-a-`Class`-star,
is-this-a-quote-delimited-`Seq`) rather than by string equality. (4) Thompson construction reads
`ExprKind` directly — `from_hir` becomes `from_expr(&GrammarIr, ExprId)`; `dfa.rs` downstream is
unchanged. (5) one `derive(Language)` on `ExprKind` gives one saturation engine for regex + grammar
canonicalization.

**Integration seams.** Extend: `skinny/crates/ir/src/lib.rs:234` (`ExprKind`) / `crates/ir/src/
types/node.rs:30` (`IrNode`). Delete: `skinny/crates/bbnf-regex` classify stub, `codegen/.../
regex_first.rs`, the 12 `Regex{pattern}` arms in `passes`, `BackendExpr::RegexProgram`. Re-home:
`parse-that/.../classify.rs`, `automata/nfa.rs:71`. Promote: `ByteSet256`. Front: `grammar/misc/
regex.bbnf` becomes a generated parser like any other.

**Gates it would carry (born-RED).** (G-5A.1) *Match-arm tax:* every grammar-plane arm added for
`Class`/`Look` graded trivial/total; **zero arms require regex-specific dispatch** (else regex
leaked into a grammar pass — refutation). (G-5A.2) *Structural-nullability parity:* the three
JSON leaves parsed through regex.bbnf, run through extended `nullability`, equal the current
`analyze()` oracle bit-for-bit (`skinny/crates/ir/src/lib.rs:651-677`). (G-5A.3) *Automata parity:*
`Dfa::find` from `ExprKind`-built NFA == vendored `Hir`→NFA path, byte-identical. (G-5A.4)
*Bootstrap fixpoint:* committed stage-0 regex→`ExprKind` lowering, `regen∘regen` byte-identical on
regex.bbnf + json.bbnf. (G-5A.5) *Eq/Hash-vs-span:* the merged type serves the saturation cache's
`Eq+Hash` AND the IR `ExprId`/span/`Serialize` plane without a god-type.

**Exact open questions.** (Q1) Is the match-arm tax trivial-total, or does at least one pass need
regex-specific `Class` handling (e.g. the type plane giving `Class` a scalar-byte type)? (Q2) Byte
vs codepoint: is `Class` byte-only with UTF-8 expansion confined to automata construction, or does
the shared enum acquire Unicode semantics no grammar leaf has? (`[α-ω]*` blowup.) (Q3) Is `Look`
consumer-less in every grammar pass (Y.13 violation), or does the anchored/unanchored distinction
PT-1.W5 needs give it a real grammar-side consumer — or must it demote to `Annotation`? (Q4) Can
the stage-0 bootstrap fixpoint be shown *now*, or is 5A SPEC-ONLY until LOCK-7/7A lands?

**BLOCKED assessment: NOT BLOCKED, LOCK-7-conditional.** No missing primitive is unsolvable; the
merge is a large-but-bounded cross-crate refactor. The one hard dependency is the bootstrap
circularity (regex.bbnf is bbnf-defined yet bbnf grammars contain regex leaves) — resolvable by a
committed stage-0 lowering (7A), but if G-5A.4 cannot be shown in pass-2, 5A is **SPEC-ONLY until
7A** and must say so.

---

## FAMILY 5B — LAWFUL-FUNCTOR (two domain-fit HIRs, one tested functor)

**Architecture.** Two HIRs stay: a domain-fit regex `Hir` (tree-shaped, `Box`/`Vec`, `Eq+Hash`
to key the saturation cache) and the grammar `IrNode`/`GrammarENode` (arena, span, `Serialize`).
They are joined by a **Prism** `F ⊣ G`: `F: Hir → GrammarENode` **total** (every regex construct
is a restricted grammar), `G: GrammarENode → Result<Hir, NotRegexExpressible>` **total as a
function, partial as an embedding** (returns the refusal at `Ref`, `Map`/`->`, `Minus`,
`TokenDispatch`). "Isomorphism to a reasonable point" (ii) is made *precise*: the regex-expressible
fragment is a reflective subcategory; the "reasonable point" is the measured coverage fraction of
grammar leaves for which `G` is defined. The shared part is the e-graph rewrite library
(`Language`/`Rewrite`/`Extractor`/`CostModel`/`Analysis`) — which both HIRs already instantiate.

**Mechanism.** Replace the opaque `Regex(StringId)` leaf + the string-level algebra bridge
(`pattern_is_superset(&str,&str)`, `try_union_patterns`) with the functor at HIR/e-graph level.
Strongest form: a **joint e-graph over a coproduct Language** `RegexOrGrammar = Left(HirENode) |
Right(GrammarENode)`; `F`/`G` become bridging rewrites (`Left(Concat[..]) ⇔ Right(Seq[..])`); the
round-trip law is a congruence fact the e-graph proves by saturation; the regex algebra fires
**once** on the `Left` sublanguage and is observed by `Right` through bridge classes. This is the
literal e-graph transcription of MLIR `unrealized_conversion_cast`: a bridge node that lives in the
joint graph during optimization and is **folded away before emission** (satisfies (v),
zero-runtime-cost). `classify` re-homes as an `Analysis<L>` on the joint graph (computed once).
Automata construction sits *below* the functor, consuming the extracted `Hir` unchanged — classical
NFA→DFA→Hopcroft→prefilter untouched (PROVE-10 inherited for free).

**Integration seams.** Regex HIR `parse-that/.../hir/mod.rs:216`; regex e-node `.../egraph/
node.rs:44`; the precedent internal functor `.../egraph/translate.rs:19,60` (proven at engine
scale); grammar IR `crates/ir/src/types/node.rs:30`; grammar e-node `crates/ir/src/egraph/
node.rs:24`; the weak string bridge to abolish `crates/ir/src/lib.rs:31-36` + `egraph/rules/
regex.rs:9-14`; the duplicated rules `crates/ir/src/egraph/rules/regex.rs` (434) ∥
`parse-that/.../egraph/rules/*` (678); shared `Language` `crates/egraph/src/language.rs:16`;
`C`-generic `Extractor` `crates/egraph/src/extract.rs:85-124`.

**Gates it would carry (born-RED).** (G-5B.1) *Prism coverage number:* over real grammars,
fraction of regex-shaped leaves that round-trip `F` then `G` to DFA-equivalence, every
non-expressible leaf named; ≥ the fraction the string bridge handles today (non-regression).
(G-5B.2) *Single-rule-drives-both:* one algebra rule (`UnionMergeAlt`/superset) fires once on the
joint graph, both sublanguages converge, the grammar-side 434-LOC copy is **deletable**. (G-5B.3)
*Round-trip oracle:* `dfa(h) ≅ dfa(G(F(h)))` (minimized-DFA isomorphism) across the 44-test
bbnf-regex corpus; any DFA divergence HARD RED. (G-5B.4) *Zero-bridge-residue:* `cargo expand`
confirms no bridge node / functor call survives into generated `.rs`; regen bit-identical; PROVE-10
`Dfa::find` preserved (≥8× on the prefilterable leaf).

**Exact open questions.** (Q1) Does the grammar-side copy of the regex rules actually *die*, or
does `GrammarENode` still need its own impls to reach grammar-only context — in which case 5B's
central value (write-algebra-once) evaporates and it collapses toward 5A? (`FuseAltRegexBranches`
mixes `Regex` and `Literal` grammar leaves — the counterexample to build.) (Q2) Can the two cost
models (regex DFA-size vs grammar tape-ops) disagree on representative *without corrupting the
shared union-find*? (Q3) Is the coverage number high enough to justify a functor, or so high that
the merger (5A) is simpler?

**BLOCKED assessment: NOT BLOCKED.** Every primitive is standard (MLIR dialect conversion,
egg/egglog, lens laws are all cited working precedents; the internal `translate.rs` functor already
ships). Risk is *value evaporation* (G-5B.2 fails), which is a measured refutation pushing toward
5A — a first-class negative result, not a block.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION (push the e-graph one level *below* the HIR)

**Architecture.** The regex engine already runs equality saturation over regex HIR on the shared
`egraph` crate with a CSP scheduler (`crates/egraph/src/csp_scheduler.rs:175`, already a running
CSP). 5C extends that *same* engine down one level, to the automaton itself — a new `Language` node
`AutomatonENode{accept:bool, succ:Box<[Id]>}` fed to the same `EGraph`, extracted by the same
`Extractor`. The charter's three operations get **three distinct verdicts**:

- **2A minimization-as-congruence-closure — LAWFUL, provably EXACT (the safe GO).** A DFA is a
  coalgebra; `rebuild()` runs congruence closure; two states merge iff same accept flag and every
  successor already co-classed. For a *complete* deterministic automaton this least-fixpoint (Moore
  direction) reaches the same Myhill–Nerode quotient as Hopcroft's greatest-fixpoint refinement
  (Rutten final-coalgebra; Bonchi–Pous up-to-congruence). **It plausibly beats the incumbent**,
  because `parse-that/.../automata/dfa.rs:291 hopcroft_minimize` is *not* Hopcroft — it is naive
  Moore refinement with a **global restart** (`:358`), `O(n²·k)` worst case, vs worklist-driven
  congruence with no restart.

- **2B determinization-as-derivative-saturation — elegant, subsumes minimization, THEORY-BOUNDED.**
  Brzozowski derivatives build the DFA directly from regex terms; the e-graph decides term
  equivalence by saturating the *existing five HIR rules*. Reuses `translate.rs`/`node.rs`/all rules
  unchanged — the most constitution-aligned casting possible, reached from the automata side. **The
  counterexample that makes it a negative result:** regex equivalence is PSPACE-complete
  (Meyer–Stockmeyer); no *finite* rewrite set is complete for it (Kozen's KAT completeness is
  schematic/infinite). The five rules are sound but incomplete, so saturation-derived DFAs are
  minimal only *up to the theory* — wherever two derivative terms are language-equal but not
  provably equal, distinct e-classes → distinct states → non-minimal DFA.

- **2C prefilter/accel-tier selection as CSP extraction — the clean CSP GO (no fixpoint hazard).**
  Genuinely discrete optimization, not partition refinement. Variables: per scan-site
  `tier ∈ {Memchr1,2,3, Shufti, Teddy, None}`; domain constraints = legality (exit-byte cardinality,
  nibble-LUT cleanliness, multi-literal prefix); objective = `skip-prob × kernel-cyc/B` via the
  `Extractor`/`CostModel`. Folds the lazy-vs-eager decision as a second variable gated on
  `estimate_dfa_size` vs `state_limit`.

**Integration seams.** Shared engine `crates/egraph/src/egraph.rs:23,155,202,251`; extraction
`extract.rs:85,115`; CSP scheduler precedent `csp_scheduler.rs:175`; node template
`parse-that/.../egraph/node.rs`+`translate.rs:22`; classical baseline `.../automata/dfa.rs:218,291`;
byte-class partition `sets/equiv.rs:94`; tier lattice `automata/accel.rs` + PROVE-10
`Prefilter::from_dfa`; CSP engine `crates/csp-solver`. READ-ONLY discipline: R3 §8 scratch-copy
recipe, cold paired N≥200, PROVE-10 protocol.

**Gates it would carry (born-RED).** (G-5C.1/2A) minimized state count **byte-identical** to a
reference `O(n·k·log n)` Hopcroft on every one of the 50 `regex_dfa_compile` patterns
(correctness); compile-time ratio + the small/large crossover reported (expected: win vs incumbent
restart-Hopcroft, honest crossover vs real-Hopcroft on small patterns). (G-5C.2/2B) minimality gap
`ρ = |states(saturate-only)| / |states(true-minimal)|` per pattern — GO only if `ρ=1` corpus-wide
AND time competitive, else the named negative result. (G-5C.3/2C) CSP beats a cost-sorted table
**only** where a *global* constraint (shared SIMD-LUT/codegen budget) makes it choose differently;
absent that, verdict = the pluggable table; identical match sets vs scalar scan
(`count_scalar == count_prefilter`). (G-5C.4) `DEAD` modeled as an absorbing e-class (partial-
transition congruence trap). (G-5C.5) deterministic extraction/CSP tie-breaks for regen bit-identity.

**Exact open questions.** (Q1) Does 2A actually beat the incumbent, and where is the real-Hopcroft
crossover? (Q2) Is `ρ=1` corpus-wide (landmark: one saturation replaces subset+Hopcroft) or `ρ>1`
somewhere (2B under-minimizes; keep 2A as closer)? (Q3) Does any corpus exhibit a global constraint
where the tier CSP beats greedy, or is the honest verdict "pluggable cost-sorted table"?

**BLOCKED assessment: 2A GO, 2C GO-conditional, 2B-as-MINIMIZER = BLOCKED-candidate.** 2B's
strongest claim (saturation subsumes minimization) requires a *finite complete equational theory
for regex equivalence* — a missing primitive that is not merely as hard as the original problem but
**provably impossible** (PSPACE-completeness; Kozen schematic). So 2B-as-minimizer is BLOCKED. But
2B-as-*determinizer* (produce a possibly-non-minimal DFA, close with 2A) is viable, and 2A/2C ship
independently — therefore **5C as a whole is NOT blocked; only its maximal claim is.** The family's
deliverable is: 2A (a landing minimizer) + 2C (a CSP-or-table with evidence) + 2B filed as its named
theory bound.

---

## FAMILY 5D — GRAMMAR-FRAGMENT (dissolve the regex leaf at the front; no engine on the generator path *by default*)

**Architecture.** A `/pat/` in a grammar *is* grammar in terser notation. One desugar pass parses
each pattern **once** into native `ExprKind` nodes of the same arena; from that point there is no
separate regex engine on the generator path — nullability, first/follow, typing, lowering, emission
all run the grammar's own machinery. The DFA is not deleted but **re-homed as one *optional*
lowering target** selected by the scan-plan cost model only for leaves that fail a one-pass check;
on the generator path that set is measured-near-empty. The standalone `parse-that` regex API
survives as a thin front (parse → one-rule `GrammarIr` → same desugar → same lowering; non-one-pass
patterns route to the DFA, preserving linear-time). Deepest reading of (i)+(ii): one HIR (the
grammar's), no functor, no automaton on the hot path.

**Mechanism.** One new leaf `ByteClass{set, negated}` (`ByteSet256` reused). One `GrammarIr →
GrammarIr` pass `regex_desugar` (sited in `passes/`, before every downstream analysis): for each
`Regex{pattern}`, parse with the real recursive-descent regex parser, translate its HIR to
`ExprKind` children (concat→`Seq`, `|`→`Alt{mode}`, `*`/`+`/`{m,n}`→`Repeat`, `?`→`Optional`,
literal→`Literal`, class/`\d\w\s`/`[^…]`→`ByteClass`, anchors→zero-width boundary node), replace the
node. **Post-pass invariant: zero `ExprKind::Regex` survive** (emitter catch-alls panic — no-silent-
epsilon). This deletes wholesale the four redundant string re-parsers + `BackendExpr::RegexProgram`.
`span_kind`/`RegexKind::{QuotedString,Numeric,Whitespace}` are recovered *structurally* (a
`ByteClass{negated}*` between `"`-delimiters IS the quoted-string recognizer), so `Recognizer::
SimdScan` is chosen by IR analysis over `ByteClass` shapes — keeping the PROVE-04/01 SIMD fast paths
while erasing the string oracle. Determinism thesis: real grammar leaves are nearly all *one-pass*
(follow-set disjoint from the class), which is the *same* first/follow disjointness the grammar's
`Alt`-dispatch already computes; a one-pass leaf emits `while set.contains(input[pos]){pos+=1}`,
residualized to NEON shufti — *faster* than a DFA (no table indirection).

**Integration seams.** New leaf `skinny/crates/ir/src/lib.rs:234`; new pass `passes/src/lib.rs`
(sibling of `nullability`); deletions `bbnf-regex` classify + `general_descent/regex_first.rs` +
`json_sink_direct/first_set.rs:97` + `passes` `regex_first_bytes/regex_type/span_kind` +
`BackendExpr::RegexProgram`; grammar front `grammar/src/lib.rs:1512` (`skip_regex`) unchanged;
recognizer selection `Recognizer::SimdScan` `ir/src/lib.rs:417`.

**Gates it would carry (born-RED).** (G-5D.1) zero `ExprKind::Regex` survive post-pass. (G-5D.2)
existing `nullability` + first-set over the desugared subtree = `analyze`/`regex_first_bytes`
byte-identical (the four-parsers→zero deletion justification). (G-5D.3) desugared JSON string/number/
ws emit with **match parity** vs `RegexProgram` AND **cold paired throughput ≥** RegexProgram
(determinism thesis; N≥200, `count_a==count_b`). (G-5D.4) **the one-pass fraction** of the 50+-pattern
bbnf-regex corpus — the single number deciding whether "no engine on the generator path" holds; a
low fraction is a named negative result. (G-5D.5) the thin front routes `(a+)+` to DFA (linear time)
and `#[0-9a-fA-F]{3,6}` to the straight-line scan — one decision point, 44+49 bridge tests green.

**Exact open questions.** (Q1) What *is* the one-pass fraction of real leaves — high enough that the
separate automaton is dead weight, or does the DFA stay load-bearing (in which case 5D reduces to
"grammar IR gains a recognizer node that STILL compiles to DFA+prefilter")? (Q2) Greedy give-back
(`[a-z]+z` on `"abz"`): does follow-set disjointness catch every non-one-pass leaf exactly? (Q3) Do
anchors/`\b` get a real zero-width consumer or are they consumer-less (Y.13)? (Q4) Does the
structural recognizer-chooser reproduce the PROVE-04/01 SIMD fast-path selection exactly?

**BLOCKED assessment: NOT BLOCKED; MAXIMAL READING REFUTED by certified evidence.** The "no engine
on the generator path" reading is **refuted by PROVE-10** (DFA + start-state memchr prefilter,
8.8–29.5×, compile-neutral, 44/44, SPEC §0 row 10) and the scannerless-slowdown literature: a naive
inline of every leaf into grammar combinators throws the DFA prefilter away and lands in the measured
scannerless regression. The *surviving* lawful 5D is "the grammar IR gains a byte-recognizer node that
STILL compiles to the SAME DFA + prefilter, with `->` projections carried on the recognizer (Ragel
transition-action precedent), one lowering per site (GN-2.W1 interlock)." Not a block — a named
narrowing of scope, and the one-pass fraction (G-5D.4) is the number that fixes where the narrowing
line falls.

---

## INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

**Axis 1 — type count / where regex structure lives (the load-bearing spine).**

- **5A ⊗ 5B — mutually exclusive endpoints.** 5A: ONE merged type, the functor deleted. 5B: TWO
  domain-fit types kept, joined by a lawful functor. They cannot both be the architecture. The
  precise conflict is an *invariant collision*: the saturation cache needs `Eq+Hash` on the regex
  representation; the grammar IR needs arena/`ExprId`/span/`Serialize`. 5A must satisfy BOTH in one
  type (risk: god-type or a cache that can't hash spans — G-5A.5); 5B keeps them apart *precisely
  to avoid that collision* and pays a functor-maintenance + duplication-survival cost instead.
  **They converge iff bbnf grows `Class`/`Look`** (priorart §3): the difference then collapses to
  "one type" vs "two types + a near-trivial total functor." The deciding numbers are 5A's match-arm
  tax (G-5A.1) + Eq/Hash-vs-span (G-5A.5) against 5B's rule-death (G-5B.2) + coverage (G-5B.1).

- **5B ⊗ 5D — mutually exclusive on the grammar-tier leaf.** 5B's raison d'être is replacing the
  *opaque grammar regex leaf* with a functor bridge while keeping both representations. 5D
  *dissolves that leaf* into native grammar nodes at desugar time — after which there is nothing
  for 5B's functor to bridge (the grammar tier no longer holds regex as an opaque leaf, so `G` has
  no opaque source and `F` no target leaf). If 5D lands, 5B's `G: GrammarENode → Option<Hir>`
  survives only as a *recovery* path to build a DFA for non-one-pass leaves — a demoted role, not
  5B's architecture. They conflict on: does the grammar tier hold regex as an opaque bridged leaf
  (5B) or as dissolved native structure (5D)?

**Axis 2 — same-spine siblings (share a mechanism ⇒ one family under the portfolio rule).**

- **5A ∧ 5D — COMPATIBLE / same spine, differ only on lowering policy.** Both add a byte-class leaf
  to the grammar HIR and dissolve the opaque regex string into native grammar nodes; both re-home
  classify as structural IR analysis; both delete the four string re-parsers. They differ *only* on
  what the unified HIR lowers to: 5A keeps automata construction (`from_expr` builds the NFA from
  the merged HIR); 5D routes to a straight-line combinator scan by default with the DFA as an
  opt-in target chosen by a one-pass check. **Under the portfolio's "two routes sharing a mechanism
  share a family" rule, 5A and 5D are one architectural center with two lowering policies** — 5D is
  the generator-path lowering corollary of 5A's type merger. Their only genuine tension is the
  `Look`/anchor leaf: 5A gives it a first-class `Look` variant; 5D gives it a zero-width boundary
  predicate node (and both must answer the same Y.13 consumer-less-`Look` question). Recommendation:
  **fold 5A+5D into a single ONE-HIR family with a lowering-policy sub-question**, and let the DFA
  re-homing be settled by the one-pass fraction (G-5D.4) — the PROVE-10-mandated floor keeps the DFA
  reachable regardless.

**Axis 3 — orthogonal layer (composes with any Axis-1 verdict).**

- **5C ∥ {5A, 5B, 5D} — orthogonal, below the HIR.** 5C operates on the *automaton*, consuming
  whatever HIR the Axis-1 winner produces. It composes with all three. Two couplings to name: (i)
  5C-2B reuses the *HIR-level* five rewrite rules as its equivalence theory — so 2B is coupled to
  whichever HIR-egraph the Axis-1 winner ships (5A's single derived `Language` or 5B's `Left`
  sublanguage); (ii) 5C-2A/2B produce the DFA that 5B's classical pipeline and 5D's opt-in lowering
  both consume, and 5C-2C's tier CSP feeds PROVE-10's prefilter seam that all families inherit.
  **5C is not in competition with the Axis-1 families; it is the automata engine underneath the
  one they pick.** Its only exclusion is *internal*: 2B-as-minimizer (BLOCKED-candidate) cannot
  replace 2A (the exact minimizer) — 2B determinizes, 2A closes.

**Summary of the mutually-exclusive set:** exactly one of {5A(+5D as its lowering policy), 5B} can
be the type-plane architecture; 5C rides on top of the winner; 5D's "no engine" maximal reading is
independently refuted by PROVE-10 and survives only as 5A's generator-path lowering policy.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs for pass-2)

Selected because they resolve the two axes that gate every downstream lock. Brief 1 collapses
Axis 1 (5A vs 5B vs the 5A/5D spine) with a single census+oracle. Brief 2 fixes the BLOCKED-
candidate verdict for 5C-2B and the GO for 5C-2A on one corpus.

### Brief 1 — the ONE-HIR match-arm tax + structural-nullability parity oracle (`bbnf-worktree`)

**Question.** Does merging regex into the grammar HIR tax grammar passes *trivially* (→ 5A/5D
dominate, no functor) or *leak regex* into them (→ 5B's functor is justified)? Decides Axis 1.

**Tree/method.** `skinny/` is its own cargo workspace (14 members). Worktree off `master`.
(A) *Tax census.* In `skinny/crates/ir/src/lib.rs:234` add two `ExprKind` variants — `Class {
set: ByteSet256, negated: bool }` (promote `ByteSet256` from `skinny/crates/bbnf-regex/src/
lib.rs:35`) and `Look { kind: LookKind }`. Run `cd skinny && cargo build --workspace 2>&1 | grep
-E "non-exhaustive|not covered"`. Enumerate every broken match across `ir`+`passes`+`codegen`;
grade each new arm TRIVIAL-TOTAL (structural one-liner, e.g. `Class => false` like `Literal`,
`Look => true` like `Optional`) vs REGEX-LEAK (needs regex-specific dispatch — e.g. the type
plane must give `Class` a scalar-byte type unlike any grammar leaf).
(B) *Parity oracle.* In a `tests/` module, hand-build the three `json.bbnf` leaf patterns
(`[ \t\n\r]*`, the quoted-string, the number) as `Class/Seq/Alt/Repeat/Optional` subtrees
(translate via the real HIR parser at `/Users/mkbabb/Programming/parse-that/rust/regex/src/hir/`),
run the extended `nullability`, and assert `nullable[ws]==true, nullable[string]==false,
nullable[number]==false` — mirroring the extant oracle at `skinny/crates/ir/src/lib.rs:651`.

**Pass/fail.** (A) COUNT of grammar-plane arms graded REGEX-LEAK: **0 ⇒ 5A/5D dominate (functor
unneeded); ≥1 ⇒ 5B justified** — name the leaking pass. (B) **3/3 nullability parity**, or 5A
breaks a green oracle (refutation).

### Brief 2 — 5C automata head-to-head: non-restart minimizer + derivative-saturator (`pt-scratch`)

**Question.** Is 2A congruence/worklist minimization *exact* and *faster* than the incumbent, and
does 2B derivative-saturation under-minimize (`ρ>1`, confirming BLOCKED-candidate) or collapse
determinize+minimize (`ρ=1`, a landmark)? Decides 5C.

**Method.** Engine READ-ONLY at `/Users/mkbabb/Programming/parse-that/rust/regex`; scratch-copy
per R3 §8: `rsync -a --exclude target …/regex/ $SCRATCH/regex/`, repoint patch-deps, isolated
`CARGO_TARGET_DIR`, bench profile fat-LTO/cgu=1/O3. Corpus: `benches/regex_dfa_compile.rs` (50
patterns — cyclic `[a-z]+`,`\w+`,`0x[0-9a-fA-F]+` AND acyclic `#[0-9a-fA-F]{3,6}` present).
Incumbent: `automata/dfa.rs:291 hopcroft_minimize` (naive Moore + restart at `:358`, `O(n²·k)`).
(2A) Add a non-restart worklist refinement minimizer sharing `crates/egraph` union-find, with a
congruence-closure fast path taken **only** on acyclic DFAs; model `DEAD` as one absorbing block.
(2B) Brzozowski-derivative determinization saturating the existing five rules
(`egraph/rules/{flatten,union,superset,redundant,repetition}.rs`); per pattern measure
`ρ = states(saturate-only)/states(true-minimal)`; named witness `(ab)*a ≡ a(ba)*`.

**Pass/fail.** (2A) minimized state count **byte-identical to incumbent on 50/50** (correctness
gate, cyclic included) AND cold-paired N≥200 wall ratio **<1.00** vs incumbent ⇒ 2A GO; report
the acyclic fraction where congruence is exact-and-free. (2B) **ρ=1 corpus-wide ⇒ landmark**; any
**ρ>1 ⇒ confirms 2B-as-minimizer BLOCKED-candidate** (theory-bounded), keep 2A as the closer.

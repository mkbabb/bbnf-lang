# LOCK-5 · Family 5B — LAWFUL-FUNCTOR — pass 1 research

**Thesis under development.** Two HIRs remain: a domain-fit regex `Hir` and the grammar
`IrNode`. They are joined by one total functor in the embedding direction and a partial
(lawful, characterized) projection in the reverse, tested by round-trip oracles that assay
*semantic* equality (DFA-equivalence), not syntactic identity. The shared part is the
e-graph rewrite library — the `Language` trait, `Rewrite`, `Extractor`, `CostModel`,
`Analysis` — which both HIRs already instantiate. The functor is not new machinery bolted
on; it is the completion of a structure the two engines already exhibit in embryo.

---

## 1. The extant substrate — the two-representation-plus-shared-Language shape already exists

The regex engine and the grammar IR **already** carry the exact skeleton 5B formalizes; the
functor is the one missing member.

**Regex side** (`parse-that/rust/regex`, the 6742-LOC one-home; skinny's `crates/bbnf-regex`
is a 322-LOC re-export stub — the real engine is the parse-that crate):

- Domain-fit owning HIR: `Hir` — flat enum, `Box<Hir>`/`Vec<Hir>` recursive children,
  `Empty | Literal(Vec<u8>) | Class(CharClass) | Look | Repetition | Group | Concat |
  Alternation` — `src/hir/mod.rs:216`. Derives `Eq + Hash` to key the per-compile saturation
  cache.
- E-graph node: `HirENode` — the *same* variants with `Id` children (`Box<[u8]>`, `Box<[Id]>`,
  a `Repetition { sub: Id, .. }`), `#[derive(Clone, Debug, PartialEq, Eq, Hash, Language)]` —
  `src/egraph/node.rs:44`.
- **The internal functor already lives here**: `src/egraph/translate.rs:19` `insert_hir`
  (owning `Hir` → e-graph class `Id`) and `:60` `extract_hir` (e-graph → cost-cheapest `Hir`).
  Its own doc-comment states the discipline verbatim: *"These translators are the only place
  where the owning type and the e-node type touch. Every other consumer ... works against
  `HirENode` exclusively; every consumer outside ... works against `Hir` exclusively."*
  (`translate.rs:1-10`). This is a *within-engine* lawful functor between two representations
  — the 5B pattern at miniature scale, proven and shipping.

**Grammar side** (`crates/ir`):

- Owning grammar IR: `IrNode` — `crates/ir/src/types/node.rs:30`. Regex enters the grammar
  tier as `IrNode::Regex(StringId)` — an **opaque interned pattern string** (`node.rs:147,175`
  treat it as a leaf alongside `Literal`, `Epsilon`, `Ref`).
- E-graph node: `GrammarENode` — `#[derive(..., Language)]`, `Literal(StringId) |
  Regex(StringId) | Epsilon | Ref | Seq(Box<[Id]>) | Alt(Box<[Id]>, Option<AltDispatch>) |
  Repeat{inner,lo,hi} | Skip | Next | Minus | Negate | OptionalWhitespace | Map | TokenDispatch`
  — `crates/ir/src/egraph/node.rs:23`. Note `Regex(StringId)` is a leaf here too.
- The IR crate **already consumes bbnf-regex by direct type re-export** — `CharSet128`,
  `regex_first_chars`, `classify` — `crates/ir/src/lib.rs:31-36`. This is the *current* seam,
  and it is **not a lawful functor**: it is raw type sharing plus string round-tripping.

**Shared library** — `crates/egraph` + `crates/egraph-derive`. Both `HirENode` and
`GrammarENode` derive the *same* `Language` trait (`crates/egraph/src/language.rs:16`:
`children`/`children_mut`/`matches`/`map_children`). The derive classifies fields by type —
`Id`→`SingleId`, `Box<[Id]>`/`Vec<Id>`→`SliceId`, else `Leaf`; opaque payloads use
`#[language(skip)]` (`egraph-derive/src/lib.rs:216-262`; `GrammarENode::TokenDispatch` uses
`#[language(skip)]` for its arms). `Extractor<'a, N: Language, A: Analysis<N>, C: CostModel<N>>`
is *already generic* over all three axes (`crates/egraph/src/extract.rs:85-124`), as is
`Rewrite<N, A>` (`crates/egraph/src/rewrite.rs:34`). **The shared rewrite library is not
aspirational — it is the crate both HIRs stand on today.**

## 2. The gap 5B closes — sharing is currently at the *string* level, and rules are written twice

The current cross-tier sharing is weak and lossy, and this is documented in the tree itself.
`crates/ir/src/egraph/rules/regex.rs` (434 LOC) opens: *"Each rule delegates the actual
regex-level work ... to `bbnf_regex::algebra` helpers, keeping the rule files thin and the
underlying algebra functions reusable by both the grammar tier and the HIR-tier rules in
`bbnf-regex::egraph::rules`."* (`rules/regex.rs:9-14`). So:

- The regex *algebra rules* exist **twice** — `bbnf-regex/src/egraph/rules/{flatten,redundant,
  repetition,superset,union}.rs` (678 LOC) for `HirENode`, and `crates/ir/src/egraph/rules/
  regex.rs` (434 LOC) for `GrammarENode` — each a distinct `Rewrite` impl pattern-matching a
  *different* Language, both delegating to the same primitive helpers.
- The shared helpers operate on **strings**: `pattern_is_superset(a: &str, b: &str)`,
  `try_union_patterns(a,b) -> Option<String>`, `byteset_to_pattern`, `extract_char_class_bytes`
  (`parse-that/rust/regex/src/algebra/mod.rs:19-95`). Every grammar-tier regex rewrite therefore
  **re-parses pattern strings**, runs algebra, and **re-serializes to a string** — a lossy,
  allocating round-trip through concrete syntax, precisely because the grammar tier holds regex
  as `Regex(StringId)` rather than as structure.

**5B's mechanism**: replace the opaque `Regex(StringId)` leaf and the string-level algebra
bridge with a **lawful functor at the HIR/e-graph level**. The regex `Hir` projects *into* the
grammar e-graph as structure (a `Concat`/`Alt`/`Repeat`/`Class` subtree the grammar tier can
see and rewrite with its *own* rules), and projects *back out* to a domain-fit `Hir` for the
NFA→DFA→prefilter pipeline. The algebra is then written **once** — as `Rewrite`s over the
shared `Language`, or carried by translating one e-graph — and the string round-trip dies.

## 3. The functor made concrete — a Prism, not an Iso; the reflective-subcategory boundary

The sharp formal handle: **regex is a partial view of grammar**. `F: Hir → GrammarENode` is
**total** (every regex construct is a restricted grammar: `Concat`→`Seq`, `Alternation`→`Alt`,
`Repetition`→`Repeat`, `Class`→a byte-membership leaf, `Group`→transparent, `Look`→a
zero-width assertion node). `G: GrammarENode → Option<Hir>` is **partial** — defined only on
the *regex-expressible fragment*; it returns `None` at `Ref` (recursion), `Map` (`->`
projection), `Minus`, `TokenDispatch`, and any construct with no regex denotation. This is
exactly a **Prism** `(review = F, preview = G)` with the prism laws as the round-trip oracle:
`G(F(h)) = Some(h)` up to DFA-equivalence (PreviewReview), and `F(g) ` reconstructs the
grammar subtree where `G(sub) = Some(g)` (ReviewPreview on the fragment).

"Isomorphism to a reasonable point" (Constitution ii) is thereby made *precise and measurable*:
the regex-expressible fragment is a **reflective subcategory** of grammar HIR; the functor pair
is an **adjunction** `F ⊣ G` whose unit is an iso on that subcategory. The "reasonable point" is
not asserted — it is the coverage fraction of real-grammar leaves for which `G` is defined, a
number a prototype produces (§7).

**The functor as an e-graph bridge (the strongest form).** Rather than two translation passes,
express `F`/`G` as **rewrite rules in one joint e-graph over a coproduct Language**
`RegexOrGrammar = Left(HirENode) | Right(GrammarENode)` (a sum type that itself derives
`Language` — every child is still an `Id`). The functor becomes bridging rewrites
(`Left(Concat[..]) ⇔ Right(Seq[..])`, etc.); the round-trip law becomes a **congruence fact**
the e-graph proves by saturation; and the shared regex algebra fires **once** on the `Left`
sublanguage and is *observed by* the `Right` side through the bridge classes. Extraction picks
the domain-fit representative per consumer (regex engine extracts `Left`→`Hir`; the generator
extracts `Right`→`GrammarENode`) using **each side's own cost model** — the `Extractor` is
already `C`-generic, so cost models stay domain-fit while the graph is shared. This is the
literal e-graph transcription of MLIR's `unrealized_conversion_cast`: a bridge node that lives
in the joint graph during optimization and is **folded away before emission** — satisfying the
zero-runtime-cost invariant (Constitution v) because no bridge node survives into generated Rust.

## 4. Where classify and the automata pipeline land

- **`classify` re-homes as an `Analysis<L>`, not a functor endpoint.** `RegexClass`
  (`parse-that/rust/regex/src/classify.rs:55`; `classify_regex_from_hir` `:177`, 1148 LOC —
  the single largest file) becomes an e-class *analysis* over the shared `Language`. As an
  `Analysis` (`crates/egraph/src/analysis.rs`), classification data attaches to e-classes and
  is computed *once* on the joint graph; because it reads the `Left` sublanguage it needs no
  duplication on the grammar side — the grammar tier reads the same `RegexClass` facts through
  the bridge classes. This directly answers the charter's "the 1148-LOC classify re-derived or
  re-homed."
- **Automata construction consumes the extracted `Hir`, unchanged.** NFA/DFA/Hopcroft/prefilter
  (`Nfa::from_hir` `src/automata/nfa.rs:71`; `Dfa::compile_with` `src/automata/dfa.rs:67`;
  `Dfa::find` + `Prefilter::from_dfa` from PROVE-10, `src/automata/prefilter.rs`) sit *below*
  the functor and are untouched — they consume the `Hir` that `G`∘extract yields. This keeps
  the automata pipeline classical (5C's saturation-as-determinization is a *different* family's
  bet; 5B keeps automata construction domain-fit and only shares the *front* algebra). The
  PROVE-10 prefilter tier (`Dfa::find`, 8.8–29.5× confirmed, §0 row 10) is inherited for free
  because the functor preserves the leaf structure prefilter derivation reads.

## 5. Prior art (cited)

- **MLIR dialect conversion** — the industrial precedent for "two IRs, total lowering, tested,
  bridge-folded." `DialectConversion` with `TypeConverter`, `ConversionTarget` (legal/illegal
  op marking), 1:N legalization, and `unrealized_conversion_cast` as the materialized bridge op
  that is required to fold away for conversion to succeed. 5B's joint-e-graph bridge *is* the
  `unrealized_conversion_cast` in e-graph clothing.
  https://mlir.llvm.org/docs/DialectConversion/
- **egg** (Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha; POPL 2021) — the `Language` trait,
  e-class analysis, cost-based extraction: the exact shape of `crates/egraph`. The
  generic-over-`Language` rewrite is what lets one rule serve two node types.
  https://dl.acm.org/doi/10.1145/3434304
- **egglog** (Zhang, Wang, Flatt, Willsey, Tatlock; PLDI 2023) — Datalog ∪ e-graph; the
  multi-relation model and *cross-domain* rules are the template for bridging two sublanguages
  in one saturating store; also the model for stating round-trip laws as facts.
  https://dl.acm.org/doi/10.1145/3591239
- **Lenses / bidirectional programming** (Foster, Greenwald, Moore, Pierce, Schmitt; POPL 2005,
  TOPLAS 2007) — the GetPut/PutGet well-behavedness laws are the *formal specification* of the
  round-trip oracle; a Prism is the "partial-view" lens the regex/grammar relation instantiates.
  https://dl.acm.org/doi/10.1145/1232420.1232424
- **regex-automata / regex-syntax** (burntsushi) — real-world proof the two-representation
  discipline is right at scale: `regex_syntax::hir::Hir` is kept *separate* from
  `nfa::thompson::NFA`, joined by a total `thompson::Compiler` — a domain-fit HIR + a total
  lowering, exactly 5B's posture below the functor.
  https://docs.rs/regex-automata/latest/regex_automata/
- **rustc / GHC IR towers** — `HIR → THIR → MIR` (total lowering, `-Zvalidate-mir` as the
  round-trip/consistency oracle) and GHC `Core → STG → Cmm` with `-dcore-lint`: the precedent
  that total inter-IR maps are *tested by validators*, which is precisely the round-trip oracle
  5B mandates. https://rustc-dev-guide.rust-lang.org/mir/
- **Reflective subcategories / adjunctions** (Mac Lane, *Categories for the Working
  Mathematician*, ch. IV) — the mathematics of "iso to a reasonable point": the regex-expressible
  fragment is a reflective subcategory; `F ⊣ G` with the unit iso on the subcategory is the exact
  statement of the charter's "isomorphism to a reasonable point."

## 6. Named risks & counterexamples

1. **The charter says "one total functor *each way*"; `G` is genuinely partial.** Grammar HIR
   has `Ref` (recursion), `Map`/`->`, `Minus`, `TokenDispatch`, host functions — none has a
   regex denotation. *Resolution*: `F` is total; `G` is total *into `Result<Hir,
   NotRegexExpressible>`* — total as a function, partial as an embedding. The honest reading of
   "each way" is the **Prism**, and the counterexample that proves it necessary is any recursive
   rule (`digit = [0-9] digit | [0-9]`) — regex cannot express it, so `G` *must* refuse. A
   prototype that claims a total iso is lying; the measured coverage number (§7) is the truth.
2. **Round-trip is only up to equivalence, never identity.** `F` then `G` on `Group(Box(a))`
   (transparent capture) yields `a` — the group is gone. `Concat([x])` collapses to `x`.
   *Resolution*: the oracle asserts **DFA-equivalence** (compile both `Hir`s to minimized DFA,
   check state-isomorphism) — reusing `Dfa::compile_with` + `hopcroft_minimize`
   (`src/automata/dfa.rs:67,90`) as the equivalence checker. Syntactic round-trip is the *wrong*
   law and will produce false failures.
3. **Duplication may survive anyway — collapse toward 5A.** If, after the functor, the grammar
   tier *still* needs its own regex rewrite impls (because `GrammarENode` rules must reach
   grammar-only context), the 434-LOC `rules/regex.rs` does not die and 5B's central value
   evaporates. *Counterexample to build*: a rule that is regex-algebra *and* touches a grammar
   sibling (`FuseAltRegexBranches` — `Alt([Regex, Lit, Regex]) → Regex("a|b|c")` — already
   mixes `Regex` and `Literal` grammar leaves, `rules/regex.rs:19`). The prototype must show this
   rule fires **once** on the joint graph and the grammar-side copy is deleted — or concede the
   boundary and *name* the residual duplication as the measured "reasonable point."
4. **Cost-model divergence corrupts extraction.** Regex cost (DFA size, prefilter-ability,
   `info/dfa_size.rs`) and grammar cost (kernel choice, tape ops, `crates/ir/src/cost.rs`) are
   *different objectives*. A single shared cost model would let regex-optimal extraction pick a
   grammar-suboptimal form. *Resolution*: the joint graph is shared; the **cost models are not**
   — each side calls `Extractor::new(&egraph, &its_own_cost)` (the `C`-generic is precisely for
   this). The prototype must verify the two extractions can disagree on representative without
   corrupting the shared union-find.
5. **Bridge nodes leak into generated code.** If a bridge/`unrealized_cast` class is reachable
   at emission, the functor costs runtime — violating Constitution v. *Resolution*: a
   legalization gate (MLIR's "all casts folded or conversion fails") — a `debug_assert` +
   generated-file grep that **zero** bridge nodes survive extraction. The counterexample is a
   regex leaf whose `G` is `None` but which is nonetheless reached — it must be a compile error,
   not a silent survivor (aligns with the no-silent-epsilon law).

## 7. What a prototype must test (decisive experiments, each a GO/NO_GO number)

1. **The Prism coverage number (the "reasonable point," made concrete).** Over the real grammars
   (`crates/runtime/src/grammars/{json,sheets,css_l4_declaration_values}` + csv), enumerate every
   regex-shaped leaf, run `F` then `G`, and report the fraction that round-trips to
   DFA-equivalence, with every non-expressible leaf *named*. **Gate**: coverage characterized and
   ≥ the fraction currently handled by the string-level algebra bridge (non-regression on
   sharing), with a named negative list. A low number is not a failure — it is the measured
   boundary; a *dishonest* total-iso claim is the failure.
2. **Single-rule-drives-both (the value proof).** Take one algebra rule — `UnionMergeAlt` /
   superset absorption (`crates/ir/src/egraph/rules/regex.rs`; `bbnf-regex/src/egraph/rules/
   union.rs`,`superset.rs`) — and show it fires **once** on the joint graph, both sublanguages
   converge to the same normal form, and the grammar-side duplicate is deletable. **Gate**: net
   LOC of the algebra counted once; `rules/regex.rs`'s string-round-tripping rules removed;
   `bbnf_regex::algebra`'s string helpers (`pattern_is_superset`, `try_union_patterns`) no longer
   on the grammar hot path. If the rule *cannot* be shared, that is the measured refutation
   pushing 5B toward 5A — a first-class result.
3. **The round-trip oracle as a differential test.** Wire `F`/`G` and assert
   `dfa(h) ≅ dfa(G(F(h)))` (minimized-DFA isomorphism) across the bbnf-regex conformance corpus.
   **Gate**: 44/44 bbnf-regex tests green (the parity floor, §0 row 10) + the round-trip oracle
   green on the full HIR test corpus; any DFA divergence is HARD RED.
4. **Zero-cost, bridge-folded generator path.** Emit a parser for a grammar with a regex leaf
   through the joint graph; inspect the generated `.rs` (`cargo expand`) to confirm **no bridge
   node and no functor call survive** — the regex leaf lowers to the same `Dfa::find`/scan the
   direct path emits, and regen is bit-for-bit idempotent. **Gate**: generated output identical
   (or measurably better) than the current `Regex(StringId)` path, zero bridge residue, PROVE-10
   prefilter preserved (`Dfa::find` present, ≥8× on the prefilterable leaf).

## 8. Seam ledger (path:line)

| Seam | Path:line | Role in 5B |
|---|---|---|
| Regex owning HIR | `parse-that/rust/regex/src/hir/mod.rs:216` | `G`'s codomain / `F`'s domain |
| Regex e-node (`Language`) | `parse-that/rust/regex/src/egraph/node.rs:44` | `Left` sublanguage |
| Internal functor (precedent) | `parse-that/rust/regex/src/egraph/translate.rs:19,60` | the 5B pattern, proven at engine scale |
| Grammar owning IR | `crates/ir/src/types/node.rs:30` | `IrNode::Regex(StringId)` = the opaque leaf 5B replaces |
| Grammar e-node (`Language`) | `crates/ir/src/egraph/node.rs:23` | `Right` sublanguage |
| String-level bridge (weak) | `crates/ir/src/lib.rs:31-36` re-export; `crates/ir/src/egraph/rules/regex.rs:9-14` | the lossy round-trip 5B abolishes |
| Duplicated regex rules | `crates/ir/src/egraph/rules/regex.rs` (434) ‖ `.../regex/src/egraph/rules/*` (678) | the duplication a shared functor collapses |
| String algebra helpers | `parse-that/rust/regex/src/algebra/mod.rs:19-95` | parse↔serialize round-trip to eliminate |
| classify (re-home target) | `parse-that/rust/regex/src/classify.rs:55,177` | becomes `Analysis<L>` on the joint graph |
| Shared `Language` trait | `crates/egraph/src/language.rs:16` | the shared part, per charter |
| Derive (FieldKind) | `crates/egraph-derive/src/lib.rs:216-262` | makes the coproduct Language derivable |
| Generic `Extractor`/`Rewrite` | `crates/egraph/src/extract.rs:85-124`; `rewrite.rs:34` | already `N/A/C`-generic — domain-fit cost, shared graph |
| Automata below the functor | `.../regex/src/automata/{nfa.rs:71,dfa.rs:67,90,prefilter.rs}` | untouched; consumes extracted `Hir` |

## 9. The family's strongest claim, stated plainly

5B is not a proposal to *build* a two-HIR system — the two HIRs, both instances of one
`Language` trait, already ship. 5B is the claim that the lawful, tested completion of the
already-present structure — a **Prism** `F ⊣ G` realized as an `unrealized_cast`-style bridge
in a **joint e-graph**, with round-trip proven by **DFA-equivalence** and the regex algebra
written **once** as `Rewrite`s over the shared `Language` — is *strictly better than both the
status quo (opaque string leaf + duplicated rules + lossy string round-trip) and the 5A
collapse (one HIR)*, because it keeps each engine's representation domain-fit (regex keeps
`Box`/`Vec` recursion and its NFA/DFA pipeline; the grammar keeps `Seq`/`Alt`/`Map`/`Ref`)
while the *optimization substrate* is genuinely, measurably shared. The prototype's job is to
produce the four numbers in §7 — coverage, single-rule-shared, DFA round-trip parity, and
zero-bridge-residue — that decide whether "isomorphism to a reasonable point" is a functor
(5B) or a merger (5A).

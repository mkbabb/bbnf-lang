# LOCK-5 · FAMILY 5B — THE LAWFUL FUNCTOR (research, pass 3)

## 0. The mechanism made concrete

Two domain-fit types, joined by a functor pair, normalized by one shared e-graph library.

- **Grammar tier** keeps the regex leaf OPAQUE: `ExprKind::Regex { pattern: String }`
  (`skinny/crates/ir/src/lib.rs:250`). The bbnf IR never carries `Class`/`Look`; it carries the
  concrete regex *syntax string* and nothing else.
- **Engine tier** keeps a rich flat HIR: `pub enum Hir { Empty, Literal, Class(CharClass),
  Look(Look), Repetition, Group, Concat, Alternation }`
  (`parse-that/rust/regex/src/hir/mod.rs:216`; `Look` variant already present, `CharClass` at
  `:45`, `Look` at `:172`, `Repetition` at `:194`). This is the automata-bearing representation.
- **Functor F (grammar → engine):** `Hir::parse(pattern) -> Result<Hir, ParseError>`
  (`parse-that/rust/regex/src/hir/parser/mod.rs:153`, `parse_with` `:158`). Total on well-formed
  patterns; the error carries a byte offset. A cheaper *partial* functor lives on the skinny side:
  `bbnf_regex::analyze(pattern: &str) -> RegexFacts` (`skinny/crates/bbnf-regex/src/lib.rs:45`),
  extracting exactly the fact bundle the grammar tier consumes — `{ nullable, first: FirstSet,
  byte_classes, hir: RegexHir{kind}, string: Option<StringFacts> }` — WITHOUT building automata.
- **Functor G (engine → grammar):** `Hir -> String` (concrete-syntax printer), then re-embed as
  `Regex { pattern }`. **This leg does not yet exist** (see §3, the single load-bearing gap).
- **Shared part (constitution clause ii, family charter "the e-graph rewrite library is the shared
  part"):** `egraph::insert_hir` / `extract_hir` (`.../regex/src/egraph/translate.rs:19,60`) map
  `Hir ↔ HirENode` (the derived e-graph `Language`); `simplify_hir(&Hir, &RegexExtractionCost)
  -> Hir` (`.../regex/src/egraph/mod.rs:143`) = insert + saturate + extract. The functor's
  canonical form is defined by this saturation, not by either type's constructors.

The "reasonable point" of the isomorphism (constitution clause ii / iii) is the **concrete regex
string**. The pair is a *retraction up to `simplify_hir`*, not a strict iso: `F ∘ G` returns to a
simplified HIR, and `G ∘ F` returns to a normalized string. The round-trip law is therefore
`simplify_hir(parse(print(h))) == simplify_hir(h)` — equality in the e-graph, not on the nose.

## 1. Q1 — the per-pass dispatch census (the decisive result)

Enumerated, not estimated. Every grammar-IR arm that touches a regex leaf, and what it consumes:

| Site | Consumes | Grows with `Class`/`Look`? |
|---|---|---|
| `ir/src/lib.rs:171` pretty | `pattern` (verbatim `/…/`) | no |
| `ir/src/lib.rs:331` nullable | `analyze(pattern).nullable` | no |
| `ir/src/lib.rs:655,661,667` regex-info / collect / program build | `pattern` | no |
| `passes/src/lib.rs:194` types | `Type::Builtin(regex_type(pattern))` | no |
| `passes/src/lib.rs:225` ws probe | `analyze(pattern).hir.kind == Whitespace` | no |
| `passes/src/lib.rs:348` scalar routing | `analyze(pattern).hir.kind == QuotedString` | no |
| `passes/src/lib.rs:790,814,836,1108,1562,1605,1638` structural predicates | `Regex { .. }` (wildcard) | no |
| `passes/src/lib.rs:911` first-sets | `regex_first_bytes(pattern)` | no |
| `passes/src/lib.rs:944` facts | `analyze(pattern)` | no |
| `passes/src/lib.rs:1341` materialization | `BackendExpr::RegexProgram { pattern }` | no |
| `passes/src/lib.rs:1512,1665` span classify | `span_kind(pattern)` over `analyze().hir.kind` | no |
| `grammar/src/lib.rs:1512` construction | builds `Regex { pattern }` | no |

**Finding: the grammar-tier census is INVARIANT under leaf growth.** Every arm funnels through
`analyze(pattern: &str)` or the raw string; none pattern-match a rich leaf. Growing the engine
`Hir` with `Class`/`Look` (already grown — the variants exist) touches ZERO of these ~20 arms.
The census growth is confined to the engine's own leaf consumers: `Nfa::from_hir`
(`.../regex/src/automata/nfa.rs:71`, dispatch at `:139` `Hir::Look`, `build_look` `:254`),
`classify_regex_from_hir` (`.../regex/src/classify.rs:177`), the e-graph rules
(`.../regex/src/egraph/rules/*`), and `regex_first_chars` (`.../regex/src/first.rs:158`).

This is 5B's structural dividend versus a single unified HIR: a one-type design forces every one
of these grammar-IR arms to acquire a `Class`/`Look` case (census *explosion* on the bbnf side);
5B pays *nothing* there. The classify re-home therefore does not SHRINK the grammar tier — there
is nothing to shrink; it stays entirely engine-side (1148 LOC in `parse-that/rust/regex`,
`crates/parse-that-regex` in skinny carries only number/unicode/integration scanning, 3022 LOC,
NOT the HIR/automata — the rich engine is the parse-that home per one-regex-home).

## 2. Q2 — typing confinement boundary

The grammar type of a regex leaf today is `Type::Builtin(regex_type(pattern))`
(`passes/src/lib.rs:194`) — a single opaque builtin derived FROM the fact bundle, never from a
`Class`/`Look` variant. **The confinement membrane is `analyze(pattern)`**: byte-set expansion
(`[α-ω] → ByteSet256`) happens in `bbnf_regex::first_set`/`parse_leading_class`
(`skinny/crates/bbnf-regex/src/lib.rs:120,162`) and in the engine's `transition_byte_sets`
(`.../regex/src/automata/nfa.rs:90`) — strictly downstream of the type. The 256-way expansion
NEVER enters the shared bbnf `Type`. 5B's binding contract to LOCK-1 is therefore narrow and
lock-safe: whatever HM shape LOCK-1 chooses for a regex leaf (row-poly record of
`{nullable, first, width, decode-class}` rather than a stub `BuiltinTy`), it is inferred from the
`RegexFacts` projection — `analyze()` is the fact source — and the automata's byte alphabet is not
part of that type. The derivation LOCK-1 must ratify: `regex-leaf : {nullable: bool, first:
ByteSet, decode: Class}` synthesized from `analyze(pattern)`, generalized at the rule seam; widths
(the CSP post-solve) read `RegexFacts.string` (`StringFacts{delimiter, escape}`,
`bbnf-regex/src/lib.rs:39`) for the quoted-string capacity. No `Class`/`Look` leaks into the row.

## 3. The load-bearing gap — functor leg G (`Hir → String`) does not exist

`grep 'impl Display for Hir'` over `parse-that/rust/regex/src` returns **zero hits**; the only
`write!` in `hir/mod.rs:246` prints `ParseError`, not `Hir`. `simplify_hir` canonicalizes
`Hir → Hir` but there is no `Hir → pattern`. Consequently the settled-ground "cheap
`string`/`format!` bridge reusing `simplify_hir`" is HALF-BUILT: the F leg (`parse`) and the
normalizer (`simplify_hir`) exist; the G leg (`print`) is unwritten. **This is the family's
central prototype obligation.** Prior art is exact: `regex_syntax::hir::print::Printer` is a
`Hir → pattern` writer in the regex crate — the reference algorithm for a total, escaping-correct
concrete-syntax printer (class ranges, `Look` → `^`/`$`/`\b`, `Repetition` → `*`/`+`/`?`/`{n,m}`,
`Group` elision). The G leg must round-trip against F under `simplify_hir` equality.

## 4. Q3 — automata parity by construction

Under 5B there is NO separate `from_expr`: the grammar path lowers a regex leaf via `F` then
reuses the ONE automata path `Nfa::from_hir` (`.../regex/src/automata/nfa.rs:71`; `from_pattern`
`:63` = parse ∘ from_hir). Parity is therefore definitional, not a coincidence to be re-verified:
`Nfa::from_hir(parse(pattern))` IS the engine's own `from_pattern(pattern)`. Group-elision and
`Optional ↔ Repetition{0,1}` are absorbed by `simplify_hir` before `from_hir`, so both callers see
the same normalized HIR. The measurable claim 5B owns is the *cheap-vs-full functor agreement*: on
the corpus, `analyze(pattern).first` (skinny slim classifier) must equal `regex_first_chars(hir)`
(`.../regex/src/first.rs:158`) and `analyze(pattern).nullable` must equal the engine's HIR
nullability — a differential oracle proving the partial functor is a faithful projection of the
total one. Throughput numbers derive from the engine's own `regex_dfa_compile` bench
(`parse-that/rust/regex/benches/`, cold N≥200 paired) run on HEAD; 5B inherits no band — the
functor adds no automata, so the engine's numbers ARE 5B's numbers.

## 5. Q4 — the `Look`-consumer test that can fail

Infrastructure exists: `nfa.rs:139` dispatches `Hir::Look`, `build_look` (`:254`) emits zero-width
gates for `Look::{Start,StartLF,StartCRLF,End,EndLF,EndCRLF}` (`:256-262`). The failing/passing
assertion the prototype exercises: build `Dfa` for `/foo$/` and `/foo/`; assert
`find(dfa_foo_end, "foobar") == None` (the `$` anchor is consulted, `bar` follows so `End` fails)
AND `find(dfa_foo, "foobar") == Some(0..3)`. This can only be satisfied if `Look::End` is honored
in the anchored `find_at` (`.../regex/src/automata/dfa.rs:116`). Note the slim skinny `analyze`
carries NO anchor model (`RegexKind` is coarse — `Whitespace/QuotedString/Numeric/Unknown`); anchor
semantics are engine-tier only. This is *correct* under 5B: the grammar tier stays opaque; anchored
matching is an automata concern the functor delegates wholesale to the engine.

## 6. Q11 — the `->` representation binds the bridge's soundness

In skinny IR, `->` is `ExprKind::Project { body: ExprId, target: String, ty: Option<String> }`
(`ir/src/lib.rs:261`) — a node that WRAPS the body, printed `body -> target` (`:185-192`). It is
**structurally external** to the regex leaf: a projected regex is `Project { body: Regex{pattern},
… }`; the `pattern` string never contains a `->`. This is the ratification the bridge needs: the
functor operates on the bare `pattern`; `G` (`Hir → String`) never has to reconstruct a projection
because projection lives one node up in the grammar IR. The `Map` barrier is therefore NOT
inner-class — it is an outer wrapping node — which is precisely what keeps the string bridge clean
and its projection soundness trivial. (LOCK-1 owns the final ratification; the extant representation
is a wrapping `Project`, and 5B's bridge is sound iff LOCK-1 keeps it wrapping rather than folding a
`Map` into the leaf.)

## 7. Prior art (citations)

- **regex crate `regex-syntax`**: `hir::Hir` (rich, domain-fit) is distinct from `ast::Ast`; the
  crate ships `hir::print::Printer` (Hir → pattern) and `ast::print` — the exact two-type +
  functor-pair pattern, with a textual round-trip. `regex-automata`'s lazy/hybrid DFA + prefilter
  ladder is the automata substrate (R7 §4; PROVE-10 REPORT, GO verdict, 9.6×–29× prefilter).
- **rustc**: HIR / THIR / MIR are separate domain-fit IRs connected by lowering functors; no single
  mega-IR — the census-invariance argument (§1) is the rustc lesson applied to regex leaves.
- **MLIR**: dialects are domain-fit IRs; conversion passes are functors; `mlir-translate` round-trip
  through the textual form is the verifier — "isomorphism to a reasonable point" = round-trippable
  concrete syntax, exactly 5B's string bridge.
- **GHC**: Core ↔ STG as staged representations, `-ddump` textual round-trip; `simplify_hir` is the
  analogue of Core's simplifier pass defining the canonical form.
- **egg / egglog**: the e-graph as the shared rewrite substrate with cost-driven extraction picking
  the canonical representative — `insert_hir`/`extract_hir` + `RegexExtractionCost` is this pattern
  (`.../regex/src/egraph/`, 1253 LOC; `cost.rs:102`).
- **Category theory**: `(F: Grammar→Engine, G: Engine→Grammar)` is a section/retraction, not an
  iso — `G∘F ≅ id` up to `simplify_hir`. Naming it a retraction (not "isomorphism") is honest and
  is the "reasonable point" the constitution demands measured, not asserted.

## 8. Named risks & counterexamples

- **G-leg escaping bugs break the retraction.** A `CharClass` containing `]`, `-`, `^`, or `\` must
  be re-escaped by the printer or `parse(print(h)) ≠ h`. *Counterexample the prototype must build:*
  `[\]^-]` round-tripped through print → parse → simplify. Mitigation: mirror
  `regex_syntax::hir::print` escaping tables; the round-trip oracle over the ≥50-pattern corpus is
  the gate.
- **`simplify_hir` is not idempotent / not confluent over the corpus**, so the retraction law
  `simplify(parse(print(h))) == simplify(h)` fails on some pattern. *Counterexample:* a pattern
  where two rewrite orders extract different-cost canonical HIRs. Mitigation: assert
  `simplify(simplify(h)) == simplify(h)` (idempotence) as a debug oracle before trusting the
  retraction (aligns with the changed-flag-convergence discipline).
- **The cheap functor `analyze` diverges from the full functor** on a corpus pattern (its
  classifier is heuristic — `bbnf-regex/src/lib.rs:92` matches literal prefixes like
  `-?(0|[1-9]`). *Counterexample:* a numeric spelling `analyze` mis-classifies `Unknown` while the
  full HIR is well-formed. Mitigation: the §4 differential oracle (`analyze.first` vs
  `regex_first_chars`); on disagreement, `analyze` must degrade to `Unknown/FirstSet::Unknown`
  (already its safe default, `:157`), never assert a wrong fact.
- **Anchor invisibility to the grammar tier** (§5) means a `/…$/` leaf's FOLLOW is computed as if
  unanchored. *Counterexample:* a grammar sequencing `/foo$/ bar` where the anchor makes the leaf
  unsatisfiable in-sequence. Mitigation: the type row (§2) must carry an `anchored: bool` fact from
  `analyze` (a new field) so FOLLOW-soundness sees it — a named `RegexFacts` extension, engine-side
  truth surfaced as a scalar fact, never a `Look` leak.

## 9. What a prototype must test to move family 5B

1. **Build functor leg G**: `Hir::print(&self) -> String` in `parse-that/rust/regex/src/hir/`
   (reference: `regex_syntax::hir::print`). Total; escaping-correct.
2. **Round-trip retraction oracle** (`tests/`, engine-side): over the `regex_dfa_compile` corpus
   (≥50, tagged acyclic/cyclic), assert `simplify_hir(parse(print(h))) == simplify_hir(h)` for
   every `h` reached by parsing the corpus — the isomorphism-to-a-reasonable-point made concrete.
3. **Idempotence guard**: `simplify_hir(simplify_hir(h)) == simplify_hir(h)` as debug_assert.
4. **Cheap-vs-full agreement oracle**: `analyze(p).first == regex_first_chars(parse(p))` and
   `analyze(p).nullable == hir_nullable(parse(p))` over the corpus; disagreements degrade to
   `Unknown`, never to a wrong fact.
5. **Look-consumer failing test** (§5): `/foo$/` rejects `"foobar"`, `/foo/` accepts — proving
   `Look::End` is consulted through `from_hir → Dfa::find`.
6. **Census-invariance proof**: mechanically confirm (grep + build) that adding a synthetic
   `Class`/`Look`-bearing pattern changes ZERO grammar-IR arms — every skinny site still compiles
   through `analyze()`/`pattern`. The negative result IS the family's evidence.
7. **Type-row confinement** (with LOCK-1): infer `Type` for a regex leaf as a row from `RegexFacts`
   (+ new `anchored` field), byte alphabet absent — proving no 256-way expansion enters the type.

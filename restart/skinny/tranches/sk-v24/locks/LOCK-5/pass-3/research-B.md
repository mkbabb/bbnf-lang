# LOCK-5 pass-3 — Family 5B: THE LAWFUL FUNCTOR

Settled ground (XL-C): two domain-fit types — grammar tier keeps `Regex(StringId)` opaque, engine
tier keeps a rich `Hir` grown with `Class`/`Look`, joined by a cheap bridge reusing the engine's
`simplify_hir`. This pass hardens that bridge with a per-arm dispatch census read from HEAD source,
routes the leaf through real HM, pins the `->` representation that governs projection soundness, and
names the exact assertions a prototype must fire. All measurements are structural counts from HEAD;
throughput/fire-count numbers are named as prototype obligations, un-run.

## Q1 — Per-pass dispatch census (read, not estimated)

The grammar tier's regex leaf is `IrNode::Regex(StringId)` (`crates/ir/src/types/node.rs:36`) and
its e-node twin `GrammarENode::Regex(StringId)` (`crates/ir/src/egraph/node.rs:27`). Across the IR
passes, **42 concrete match arms** touch this leaf, in two disjoint classes:

**Opaque-leaf grouping arms (26)** — `Regex(_)` bundled with `Literal(_)`/`Epsilon`/`Ref(_)` as a
childless leaf, the code never looking inside: `span.rs:82,234`; `metadata.rs:63`;
`types/registry.rs:362,439,480`; `types/mod.rs:393,549`; `types/generate.rs:157`;
`types/subvariants.rs:132`; `inspect/walk.rs:60`; `csp_strategy/components.rs:334`;
`payload/scalar_routing.rs:100`; `materialization/classify.rs:183`; `materialization/pin_sweep.rs:95`;
`audit/payload_coverage.rs:537`; `sets/sort.rs:82`; `sets/follow.rs:281`; `sets/deps.rs:62`;
`sets/dispatch/annotate.rs:163`; `sets/dispatch/eligibility.rs:156`; `sets/first_sets.rs:196,277,366`;
`sets/structural_alphabet.rs:399`; plus the node walkers in `types/node.rs:147,175`.

**Consuming arms (16)** — bind `sid`, fetch the string, run it through `RegexInfo`/classification:
`regex_info.rs:93`; `csp_strategy/mod.rs:839,1216,1329`; `materialization/classify.rs:589`;
`sets/sort.rs:193,287,338`; `sets/factor_lookahead.rs:138,218,314`; `sets/structural_alphabet.rs:282`;
`sets/first_sets.rs:106,234,388`; `sets/dispatch/first_set.rs:27`;
`recognizers/pattern_alphabet.rs:98`; `inspect/leading.rs:82`.

**The census verdict for 5B.** Under Formulation A (grow the *grammar* leaf to carry `Class`/`Look`)
every one of the 26 grouping arms loses leaf-exhaustiveness and must be revisited, and the 16
consuming arms must walk a new recursive shape — 42 arms disturbed, each 1–15 LOC, plus the loss of
the "regex is a leaf" invariant the whole pipeline rests on. Under 5B the grammar leaf stays
`Regex(StringId)`: **zero grammar-tier arms grow.** The `Class`/`Look` richness is confined to the
engine's `HirENode`, which *already* carries both (`Class(CharClass)`, `Look(Look)` in
`parse-that/rust/regex/src/egraph/node.rs`) and its owning twin `Hir` (`hir/mod.rs`, `Class`/`Look`
variants). So the directive's "does classify re-home SHRINK or RELOCATE" question is a Formulation-A
question: for 5B the 1148-LOC `classify.rs` neither shrinks nor relocates — it **stays put on the
engine side, unchanged** — and the functor simply lets the 16 consuming arms reach its results
structurally instead of by per-call string re-parse. The only LOC delta 5B claims is the ~215-LOC
grammar-tier rule-death (Q10 below), a real deletion, not a relocation.

## Q2 — Typing the leaf through LOCK-1's HM; the confinement boundary

The grammar leaf must NOT type as a `BuiltinTy` stub. Under LOCK-1's HM it types by its projection: a
bare `Regex(sid)` synthesizes `string` (a byte-span scalar — the same type `Literal` gets at
`types/generate.rs:157`), and a projected `Map{ Regex(sid), fn_id }` synthesizes the codomain of
`fn_id`. Row-poly and width judgments flow through the *outer* `Map`, never through the regex
internals: `Γ ⊢ Regex(sid) : string`, with no free type variable escaping into class structure.

**The confinement boundary is physical.** `[α-ω]*` byte-expansion is the transform
`CharClass → ByteSet → byte-equivalence classes → DFA transition table`, living in
`parse-that/rust/regex/src/sets/byteset.rs` and `automata/dfa.rs` (`byte_classes:[u8;256]`,
`compute_byte_classes`). The narrowest thing that crosses into the grammar tier is `StringId` — never
a `ByteSet`, never a `CharClass`. This is the load-bearing 5B claim: the shared type is `string`
(or the bridge's `String` pattern), and byte-expansion is quarantined in automata by construction,
because the only handle the grammar tier holds is the opaque id. The confinement is not asserted; it
is the direct consequence of the leaf being `Regex(StringId)` and every one of the 26 grouping arms
treating it as a scalar.

## Q3 — Automata parity re-derived from the narrowed type

Because 5B keeps the engine's own `Hir` as automata input, there is **no `from_expr`** — the NFA
builder is `Nfa::from_hir` (`automata/nfa.rs:71`), unchanged, consuming exactly the `Hir` the functor
round-trips to. Parity is therefore *by construction*, not by re-derivation: the functor's law is
`extract_bbnf(insert_bbnf(hir))` DFA-equivalent to `hir`, and `insert_hir`/`extract_hir`
(`egraph/translate.rs:22`) already round-trip every leaf verbatim
(`Hir::Class(c) → HirENode::Class(c.clone())`, `Hir::Look(l) → HirENode::Look(*l)`). The Group-elision
and `Optional↔Repeat{0,1}` normalizations the directive names are the engine's saturation rules — they
run inside the engine e-graph and extract back to a canonical `Hir` that `from_hir` compiles
identically. **The byte-identity harness** the prototype must run: for every corpus pattern compile
`Dfa` from `hir` and from `extract_hir(saturate(insert_hir(hir)))`, assert the minimized transition
tables are byte-identical (strict off, canonical on — `a|b|c` and `[abc]` must be *DFA*-equal, not
syntactically equal). Throughput must be re-run fresh from this narrowed path
(`--bench regex_dfa_compile`, cold per-parse) carrying no inherited band.

## Q4 — A `Look`-consumer test that can FAIL

The engine already consumes `Look` in NFA construction: `Hir::Look(look) => self.build_look(*look)`
(`automata/nfa.rs:139`), `build_look` (`:254`) emitting zero-width start/end assertions over the six
`Look` variants (`Start`/`End`/`StartLF`/`EndLF`/`StartCRLF`/`EndCRLF`, `hir/mod.rs:172`). The
failing-if-broken assertion, threaded THROUGH the functor:

```
let hir = parse("foo$");                          // Concat[Literal"foo", Look::End]
let round = extract_hir(saturate(insert_hir(&hir)));
assert_eq!(hir, round);                            // Look::End survives F∘G verbatim
let dfa = Dfa::compile(&round);
assert!( dfa.find_at("foo",   0).is_some());       // /foo$/ accepts "foo"
assert!( dfa.find_at("foobar",0).is_none());       // /foo$/ REJECTS "foobar" ← fails if Look dropped
assert!( Dfa::compile(&parse("foo")).find_at("foobar",0).is_some()); // /foo/ accepts "foobar"
```

It can fail three ways — the functor drops the `Look` leaf on insert/extract, saturation elides it, or
`build_look` ignores `End` — and passes only when `Look::End` is consulted end to end. This is the
concrete evidence that the functor preserves the engine's rich, consumer-backed leaves rather than a
decorative enum.

## Q10 — Grammar-tier rule census + fire counts

The grammar-tier regex e-graph is `crates/ir/src/egraph/rules/regex.rs` = 434 LOC, **4 rules**:
`DeduplicateAltBranches` (`:49`), `SupersetAbsorbAlt` (`:109`), `UnionMergeAlt` (`:194`),
`FuseAltRegexBranches` (`:229`), plus string helpers `regex_escape_literal`,
`pattern_has_top_level_pipe`. The engine-side rules the functor makes reachable: `superset.rs` (90),
`union.rs` (108), `flatten.rs` (107), `repetition.rs` (156), `redundant.rs` (68)
(`parse-that/rust/regex/src/egraph/rules/`). **Rule fates under the functor:** `DeduplicateAltBranches`
survives (pure e-class identity, grammar-general); `SupersetAbsorbAlt` dies (duplicate of engine
`superset.rs`); `UnionMergeAlt` dies (duplicate of engine `union.rs`); `FuseAltRegexBranches` is
re-expressed structurally (its `parts.join("|")` string fusion → build `HirENode::Alternation`, run
engine `union`/`flatten`/`superset`, extract back). Structural estimate ~215 grammar-tier LOC deleted
+ the two string helpers.

**The fire-count obligation (un-run).** Instrument each rule's `apply` with a counter over the engine
corpus (≥50 patterns), reporting per-rule fires on BOTH bare `Alt([Regex,…])` branches and
projection-wrapped `Alt([Map{Regex},…])` branches. The census decides: (a) whether the dying rules
ever fired enough to matter — a rule with zero fires is deleted with no behavioural risk; (b) whether
fusion ever meets projection-wrapped branches — if `Map`-wrapped Alt branches are common, the
projection-soundness guard (Q11) is load-bearing, not theoretical.

## Q11 — The `->` representation (BINDING on projection soundness)

Confirmed from HEAD: `->` is an **outer wrapper node**, not a field on the regex leaf.
`IrNode::Map { inner: Box<IrNode>, fn_id: FnId }` (`crates/ir/src/types/node.rs:79`) and its e-node
`GrammarENode::Map { inner: Id, fn_id: FnId }` (`crates/ir/src/egraph/node.rs:52`). So a projected
regex leaf is `Map{ inner: Regex(sid), fn_id }` — the projection is a *barrier one level above* the
leaf, keyed by `fn_id`. The regex leaf itself carries neither span nor projection; write-back is the
identity `GrammarENode::Regex(sid) => IrNode::Regex(sid)` (`crates/ir/src/egraph/write_back.rs:209`).

**This is binding on the bridge's projection soundness.** The functor may descend into an `Alt` and
fuse its `Regex` branches by structural `HirENode::Alternation`, but the fusion is sound ONLY when the
branches are bare `Regex` (no `Map` above them) or every branch's `Map` carries an equal `fn_id`.
Counterexample the string bridge cannot see and the functor must block: `[a-c] -> A | [d-f] -> B` is
`Alt([ Map{Regex[a-c],A}, Map{Regex[d-f],B} ])`; byte-set union to `[a-f]` would collapse two distinct
`Map` barriers and drop projections `A`, `B` — a wrong answer. Because `Map` is a structural node (not
a leaf annotation), the functor sees the barrier directly: fusion matches `Alt([Regex, Regex, …])`
(bare) and refuses `Alt([Map{…}, Map{…}])` unless `fn_id`s unify. The `Map`-as-barrier representation
is exactly what makes the guard *checkable*, vindicating 5B's side-annotation position with the
concrete node.

## Integration seams (path:line)

- `crates/ir/src/types/node.rs:36` — `IrNode::Regex(StringId)` (opaque leaf, no span/projection).
- `crates/ir/src/types/node.rs:79` — `IrNode::Map { inner, fn_id }` (the `->` outer barrier).
- `crates/ir/src/egraph/node.rs:27,52` — `GrammarENode::Regex`/`Map` (e-node twins).
- `crates/ir/src/egraph/write_back.rs:209` — identity write-back, projection-free leaf.
- `crates/ir/src/egraph/rules/regex.rs:49,109,194,229` — Dedup(survives)/Superset(dies)/Union(dies)/
  Fuse(re-expressed).
- `crates/ir/src/passes/regex_info.rs:93` + the 16 consuming arms — the per-call re-parse the functor
  replaces with a cached structural `Hir`.
- `parse-that/rust/regex/src/egraph/node.rs` — `HirENode` (`Class`/`Look` already present).
- `parse-that/rust/regex/src/egraph/translate.rs:22` — `insert_hir`/`extract_hir` (the functor model).
- `parse-that/rust/regex/src/automata/nfa.rs:71,139,254` — `from_hir`/`build_look` (the Look consumer).
- `parse-that/rust/regex/src/hir/mod.rs:172` — the `Look` enum (six variants).

## Prior art (citations)

- **"Trees That Grow"** (Najd & Peyton Jones, IFL 2016; GHC Core `XExtension`) — one AST family with
  per-phase domain-fit decorations; the anchor for two domain-fit HIRs over one god-type.
- **MLIR dialect conversion + `unrealized_conversion_cast`** — cross-dialect materialization casts
  inserted then legalized away pre-lowering; the model IF 5B ever unifies the two arenas into a
  coproduct (priced against the 215-LOC deletion, not assumed).
- **Bidirectional lenses** (Foster–Greenwald–Moore–Pierce–Schmitt, POPL 2005) — `GetPut`/`PutGet` are
  the functor round-trip law; here weakened to DFA-equivalence, not syntactic identity.
- **Adjoint functors `F ⊣ G`, coreflective subcategory** — `G∘F = id` on regex, `F∘G ⊑ id` on the
  grammar sublanguage where `G` is defined (undefined on `Ref`/`Minus`/`Negate`/`TokenDispatch`).
- **egg / egglog** (Willsey et al. POPL 2021; Zhang et al. PLDI 2023) — the `Language` trait and
  equality saturation both e-node types already derive; egglog multi-sort as coproduct precedent.
- **regex-automata `Hir`→NFA→DFA** — the lineage `bbnf-regex` re-implements bespoke; the functor leaves
  that engine untouched.

## What a prototype MUST test to move family 5B

1. **Round-trip oracle:** `extract_hir(saturate(insert_hir(hir)))` DFA-equivalent to `hir` over the
   ≥50-pattern corpus; `Look`/`Class` leaves bit-identical (the Q4 assertion firing).
2. **Structural `FuseAltRegexBranches`:** replace `parts.join("|")` with `HirENode::Alternation` +
   engine `union`/`flatten`/`superset` + extract-back; assert byte-identical fused DFA vs the string
   fusion; MEASURE grammar-tier LOC deleted (target ~215, prototype-earned).
3. **Projection soundness:** a test that FAILS on the current string bridge (`[a-c]->A | [d-f]->B`
   merges, dropping A/B) and PASSES under the functor (fusion blocked when `Map.fn_id`s differ).
4. **Partiality:** `Alt([Regex, Ref(ident)])` — `G` returns None on `Ref`, fusion skips the branch,
   no crash, no cross-branch fusion.
5. **Per-rule fire counts** over the corpus (bare + `Map`-wrapped Alt), proving the dying rules'
   deletion is behaviourally safe and whether the projection guard is exercised in practice.
6. **Narrowed throughput:** `regex_dfa_compile` cold per-parse re-run from the functor path, carrying
   no inherited band — parity or better against `from_hir` direct.

## Cross-lock (shareable)

- LOCK-1 owns the `Map{inner,fn_id}` typing (`types/node.rs:79`): the `->` codomain flows through the
  outer `Map`, the bare `Regex` types `string`; this is what makes the projection guard checkable.
- The `GrammarCostModel` (`crates/ir/src/egraph/cost.rs`, `egraph/mod.rs:15`) already spans both
  e-graphs — the charter's "shared part"; the functor formalizes existing sharing, invents none.
- Prefilter/tier selection (double-homed with LOCK-2) sits downstream of extraction; 5B touches only
  the HIR-canon island and the string→structural bridge, not tier selection.

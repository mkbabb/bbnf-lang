# LOCK-5 pass-3 — Family 5B: THE LAWFUL FUNCTOR

Settled ground (XL-C): two domain-fit types — the grammar tier keeps the regex leaf opaque, the
engine tier keeps a rich `Hir` grown with `Class`/`Look`, joined by a cheap string bridge reusing the
engine's saturation. This pass HARDENS that bridge against HEAD source, and returns a load-bearing
correction: the two functor endpoints do not live where the prior census placed them. Every path:line
below was read at HEAD; throughput and fire-count numbers are named as prototype obligations, un-run.

## The substrate correction (read, not assumed) — governs every answer below

There are, at HEAD, **two distinct grammar-tier regex homes and no vendored engine in skinny**:

1. **Skinny grammar tier (the tree of record the charter names).** `skinny/crates/ir/src/lib.rs`
   (678 LOC) carries the regex leaf as `ExprKind::Regex { pattern: String }` (`:250`) — a raw
   `String`, **not** `Regex(StringId)`. The `->` projection is a *separate structural outer node*
   `ExprKind::Project { body: ExprId, target, ty }` (`:261`, pretty-prints `" -> "` at `:187`). The
   compiled leaf is `BackendExpr::RegexProgram { pattern: String, span_kind: SpanKind }` (`:389`,
   `SpanKind` at `:440`). So on the skinny path the "opaque leaf" is a literal `String` and the
   "cheap bridge" is not `format!` — it is the pattern itself, carried verbatim from grammar tier
   (`ExprKind::Regex.pattern`) through lowering to `BackendExpr::RegexProgram.pattern`.

2. **Main bbnf-lang IR (the rich e-graph home).** `crates/ir/src/types/node.rs:36` `IrNode::Regex(StringId)`
   + `:79` `IrNode::Map { inner: Box<IrNode>, fn_id: FnId }`; e-node twins `GrammarENode::Regex`/`Map`
   (`egraph/node.rs:27,52`); identity write-back `GrammarENode::Regex(sid) => IrNode::Regex(sid)`
   (`egraph/write_back.rs:209`), `Map` reconstructed at `:311,321`. This is where the 4-rule regex
   e-graph lives (Q10). It is a *different crate* from skinny/crates/ir.

3. **No rich `Hir` engine is vendored into skinny.** `skinny/crates/parse-that-regex` (4441 LOC) is a
   number/utf8 utility crate — `number/eisel_lemire/`, `unicode/utf8_hoehrmann.rs`,
   `integration/simd_scan_hook.rs` — with **zero** `Hir`/`Class`/`Look`/`classify_regex`/`from_hir`
   (grep-verified). `skinny/crates/bbnf-regex/src/lib.rs` is a thin *analysis facade*: `RegexFacts`
   (`:2`), `RegexHir` (`:11`), `RegexKind` (`:16`), `FirstSet` (`:24`), `ByteClass` (`:30`),
   `ByteSet256` (`:35`), `analyze(pattern:&str)->RegexFacts` (`:45`) — first-sets, byte-classes,
   nullable — **not** a Hir→NFA→DFA engine. The only full rich `Hir` with `Class`/`Look`, the
   `insert_hir`/`extract_hir` functor, `from_hir`, `build_look`, and Hopcroft is the READ-ONLY
   canonical `parse-that/rust/regex/` (6742 LOC).

**Consequence for 5B.** The lawful functor's two objects are (grammar tier) skinny `String` /
main-IR `Regex(StringId)`, and (engine tier) the canonical `Hir`. Base-pinning throughput to "the
vendored substrate" is impossible today: skinny holds no engine to bench. The measurement substrate
is the canonical repo built via the R3 §8 scratch-copy recipe, OR a vendoring must first land the
engine crate into skinny. **This is the pass-3 prototype blocker to state plainly**, not a detail.

## Q1 — Per-pass dispatch census (read, on both homes)

**Skinny path — the consuming surface is ONE call.** The entire grammar tier holds the regex as a
`String`; the sole consumer is `bbnf_regex::analyze(pattern)` (`skinny/crates/ir/src/lib.rs:1` import,
`:331` `ExprKind::Regex { pattern } => analyze(pattern).nullable`). Downstream, codegen derives leading
bytes straight from the pattern string: `regex_first_bytes(span_kind, pattern)`
(`codegen/src/json_sink_direct/first_set.rs:77`), `general_descent/regex_first.rs`
("leading-byte-set derivation for `RegexProgram` leaf patterns"), emitted at
`general_descent/emit.rs:365` and `codegen/src/lib.rs:1444`. `general_descent/mod.rs:13-18` states the
descent path derives its match from the pattern's byte-sets with "no separate engine" — skinny already
*embodies* the two-domain-fit design: `String` leaf + a facts facade, no rich Hir on the hot path.
So on the tree of record **zero grammar arms grow under 5B**; the functor's only job is to let `analyze`
return a *cached richer* `RegexHir` (the type already exists, `bbnf-regex:11`) so repeated per-pattern
`analyze` calls amortize, and to bridge to the canonical `Hir` when true NFA/DFA matching (not just
leading-byte prefilter) is required.

**Main-IR path — 42 arms, unchanged verdict.** In the main e-graph home the leaf is
`IrNode::Regex(StringId)`, treated as a childless scalar in ~26 grouping arms (bundled with
`Literal`/`Epsilon`/`Ref` at `types/node.rs:147,175` and across `sets/`, `materialization/`,
`csp_strategy/`, `types/`) and consumed by ~16 arms via `RegexInfo`/classify. Under **Formulation A**
(grow the *grammar* leaf to carry `Class`/`Look`) all 26 grouping arms lose leaf-exhaustiveness and
the 16 consuming arms must walk a recursive shape — 42 arms disturbed, plus loss of the "regex is a
leaf" invariant. Under **5B** the grammar leaf stays opaque: the `Class`/`Look` richness is confined to
the canonical `HirENode`, which already carries `Class(CharClass)`/`Look(Look)`. So the 1148-LOC
`classify.rs` neither shrinks nor relocates — it **stays put on the engine side, unchanged** — and the
functor lets consumers reach its results structurally rather than by per-call string re-parse. The
only LOC delta 5B claims is the grammar-tier regex-rule death (Q10), a real deletion.

## Q2 — Typing the leaf through LOCK-1's HM; the confinement boundary

No `BuiltinTy` stub. The leaf types by its projection: bare `Regex` synthesizes `string` (a byte-span
scalar — skinny's `Literal` and `Regex` both reduce to a span consumed by the tape); a projected leaf
`Project { body: Regex, target, ty }` (skinny `:261`) / `Map { inner: Regex, fn_id }` (main `:79`)
synthesizes the codomain declared by `ty`/`fn_id`. Row-poly and width judgments flow through the
*outer* `Project`/`Map`, never through regex internals: `Γ ⊢ Regex(p) : string`, no free type variable
escaping into class structure.

**The confinement boundary is physical and already realized.** `[α-ω]*` byte-expansion is the
transform `CharClass → ByteSet → byte-equivalence classes → DFA table`. In skinny it lives inside
`bbnf-regex` (`ByteSet256` `:35`, `ByteClass` `:30`); in the canonical engine in `sets/byteset.rs` +
`automata/dfa.rs` (`byte_classes:[u8;256]`, `compute_byte_classes` from `sets/equiv.rs`). The
narrowest thing crossing into the grammar tier is the `String` (skinny) / `StringId` (main) — never a
`ByteSet`, never a `CharClass`. The confinement is not asserted; it is the direct consequence of the
leaf holding only the pattern text, and every grouping/emit arm treating it as a scalar whose bytes
are derived *inside* the facts facade (`regex_first_bytes` reads the pattern; the byte-sets never
surface as a grammar-tier type).

## Q3 — Automata parity re-derived from the narrowed type

Because 5B keeps the engine's own `Hir` as automata input there is **no `from_expr`** — the NFA builder
is `Nfa::from_hir` (`parse-that/rust/regex/src/automata/nfa.rs`, `Hir::Look(look)=>build_look(*look)`
at `:139`), consuming exactly the `Hir` the functor round-trips. Parity is *by construction*: the
functor's law is `extract_hir(saturate(insert_hir(hir)))` DFA-equivalent to `hir`, and the endpoints
already round-trip every leaf verbatim — verified at HEAD in `egraph/translate.rs`:
`insert_hir` (`:19`), `Hir::Class(c) => HirENode::Class(c.clone())` (`:26`),
`Hir::Look(l) => HirENode::Look(*l)` (`:27`); `extract_hir` (`:60`),
`HirENode::Class(c) => Hir::Class(c)` (`:97`), `HirENode::Look(l) => Hir::Look(l)` (`:98`). So
`F∘G = id` on `Class`/`Look` by construction — read, not conjectured. Group-elision and
`Optional↔Repeat{0,1}` are the engine's saturation rules; they run inside the e-graph and extract to a
canonical `Hir` that `from_hir` compiles identically.

**Byte-identity harness (prototype):** for each corpus pattern compile `Dfa` from `hir` and from
`extract_hir(saturate(insert_hir(hir)))`; assert the *minimized* transition tables are byte-identical
(canonical on — `a|b|c` and `[abc]` must be DFA-equal, not syntactically equal). Throughput re-run
fresh from this narrowed path (`cargo bench -p bbnf-regex --bench regex_dfa_compile`, cold per-parse,
built via R3 §8 scratch-copy because skinny holds no engine), carrying no inherited band.

## Q4 — A `Look`-consumer test that can FAIL

The canonical engine consumes `Look` in NFA construction: `build_look` (`automata/nfa.rs:254`) emits
zero-width assertions over six `Look` variants (`Start`/`End`/`StartLF`/`EndLF`/`StartCRLF`/`EndCRLF`,
`hir/mod.rs:172-184`, all read at HEAD). The failing-if-broken assertion threaded through the functor:

```
let hir   = parse("foo$");                          // Concat[Literal"foo", Look::End]
let round = extract_hir(saturate(insert_hir(&hir)));
assert_eq!(hir, round);                             // Look::End survives F∘G verbatim
let dfa   = Dfa::compile(&round);
assert!( dfa.find_at("foo",    0).is_some());       // /foo$/ accepts "foo"
assert!( dfa.find_at("foobar", 0).is_none());       // /foo$/ REJECTS "foobar"  ← fails if Look dropped
assert!( Dfa::compile(&parse("foo")).find_at("foobar",0).is_some()); // /foo/ accepts "foobar"
```

It fails three ways — the functor drops `Look` on insert/extract, saturation elides it, or `build_look`
ignores `End` — and passes only when `Look::End` is consulted end to end. Concrete evidence the functor
preserves consumer-backed leaves rather than a decorative enum.

## Q10 — Grammar-tier rule census + fire counts

The grammar-tier regex e-graph lives in the **main IR only** (skinny runs no regex e-graph — its
descent derives byte-sets directly, `general_descent/mod.rs:13`). `crates/ir/src/egraph/rules/regex.rs`
= 434 LOC, **4 rules**: `DeduplicateAltBranches` (`:49`), `SupersetAbsorbAlt` (`:109`),
`UnionMergeAlt` (`:194`), `FuseAltRegexBranches` (`:229`, whose `parts.join("|")` at `:300` + helper
`pattern_has_top_level_pipe` at `:332` do *string-level* fusion). The engine-side rules the functor
makes reachable: `superset.rs`(90), `union.rs`(108), `flatten.rs`(107), `repetition.rs`(156),
`redundant.rs`(68) (`parse-that/rust/regex/src/egraph/rules/`). **Rule fates:**
`DeduplicateAltBranches` survives (pure e-class identity); `SupersetAbsorbAlt` dies (duplicate of
engine `superset.rs`); `UnionMergeAlt` dies (duplicate of engine `union.rs`); `FuseAltRegexBranches`
re-expressed structurally (`parts.join("|")` → `HirENode::Alternation` + engine
`union`/`flatten`/`superset` + extract-back). Structural estimate ~215 grammar-tier LOC deleted + the
two string helpers.

**Fire-count obligation (un-run).** Instrument each rule's `apply` over the engine corpus (≥50
patterns), counting fires on BOTH bare `Alt([Regex,…])` and projection-wrapped `Alt([Map{Regex},…])`
branches. This decides (a) whether the dying rules ever fired enough to matter (zero-fire ⇒ deleted
with no behavioural risk) and (b) whether fusion meets projection-wrapped branches (if common, the
Q11 guard is load-bearing, not theoretical).

## Q11 — The `->` representation (BINDING on projection soundness)

Confirmed at HEAD on both homes: `->` is an **outer wrapper node, never a field on the regex leaf**.
Skinny: `ExprKind::Project { body: ExprId, target, ty }` (`skinny/crates/ir/src/lib.rs:261`), pretty
`" -> "` (`:187`), backend-tier `BackendExpr::ValueProject` (`:407`); the regex leaf itself is
`RegexProgram { pattern, span_kind }` (`:389`), `span_kind` selecting the scanner — the capture/span
discriminant, not a projection. Main: `IrNode::Map { inner: Box<IrNode>, fn_id }` (`:79`), e-node
`GrammarENode::Map { inner, fn_id }` (`egraph/node.rs:52`), identity leaf write-back
`GrammarENode::Regex(sid) => IrNode::Regex(sid)` (`write_back.rs:209`), `Map` reconstructed at
`:311,321`.

**Binding on the bridge's projection soundness.** The functor may descend an `Alt` and fuse `Regex`
branches by structural `HirENode::Alternation`, but the fusion is sound ONLY when branches are bare
(no `Project`/`Map` above) or every branch's projection carries an equal `fn_id`/`ty`. Counterexample
the *string* bridge cannot see and the functor must block: `[a-c] -> A | [d-f] -> B` is
`Alt([ Project{Regex[a-c],A}, Project{Regex[d-f],B} ])` — byte-set union to `[a-f]` would collapse two
distinct projection barriers and drop `A`/`B`, a wrong answer. Because `Project`/`Map` is a *structural
node*, the functor sees the barrier directly: it matches `Alt([Regex,Regex,…])` (bare) and refuses
`Alt([Project{…},Project{…}])` unless the projections unify. This is exactly what makes the guard
*checkable* and vindicates 5B's side-annotation position with the concrete node — and it is precisely
the fusion the main-IR string rule `FuseAltRegexBranches` (`join("|")`) does blindly today.

## Integration seams (path:line)

- `skinny/crates/ir/src/lib.rs:250` — `ExprKind::Regex { pattern: String }` (grammar-tier opaque leaf).
- `skinny/crates/ir/src/lib.rs:261` — `ExprKind::Project { body, target, ty }` (skinny `->` barrier).
- `skinny/crates/ir/src/lib.rs:389,440` — `BackendExpr::RegexProgram { pattern, span_kind }` + `SpanKind`.
- `skinny/crates/ir/src/lib.rs:1,331` — `bbnf_regex::analyze(pattern)` (the single consuming call).
- `skinny/crates/bbnf-regex/src/lib.rs:2,11,16,30,35,45` — `RegexFacts`/`RegexHir`/`ByteClass`/`ByteSet256`/`analyze` (facts facade; byte-expansion confined here).
- `skinny/crates/codegen/src/json_sink_direct/first_set.rs:77` + `general_descent/regex_first.rs` — `regex_first_bytes(span_kind, pattern)` (byte-set derived from pattern text).
- `crates/ir/src/types/node.rs:36,79` — main `IrNode::Regex(StringId)` / `Map { inner, fn_id }`.
- `crates/ir/src/egraph/{node.rs:27,52, write_back.rs:209,311, rules/regex.rs:49,109,194,229,300,332}` — e-node twins, identity write-back, the 4 dying/re-expressed rules.
- `parse-that/rust/regex/src/egraph/translate.rs:19,26,27,60,97,98` — `insert_hir`/`extract_hir` with Class/Look identity endpoints (the functor model, verified).
- `parse-that/rust/regex/src/automata/nfa.rs:139,254` — `from_hir`/`build_look` (the Look consumer).
- `parse-that/rust/regex/src/hir/mod.rs:172-184` — the six-variant `Look` enum.

## Prior art (citations)

- **"Trees That Grow"** (Najd & Peyton Jones, IFL 2016; GHC `XExtension`) — one AST family, per-phase
  domain-fit decorations; the anchor for two domain-fit HIRs over one god-type. Skinny's
  `String`-leaf-plus-facts-facade is a degenerate instance already in the tree.
- **Bidirectional lenses** (Foster–Greenwald–Moore–Pierce–Schmitt, POPL 2005) — `GetPut`/`PutGet` are
  the round-trip law; here weakened to DFA-equivalence, not syntactic identity.
- **Adjoint functors `F ⊣ G`, coreflective subcategory** — `G∘F = id` on regex (verified via
  translate.rs identity endpoints), `F∘G ⊑ id` on the grammar sublanguage where `G` is defined
  (undefined on `Ref`/`Minus`/`Negate`).
- **MLIR dialect conversion + `unrealized_conversion_cast`** — cast-then-legalize is the model IF 5B
  ever fuses the two arenas into a coproduct; priced against the ~215-LOC deletion, not assumed.
- **egg / egglog** (Willsey et al. POPL 2021; Zhang et al. PLDI 2023) — the `Language` trait both
  e-node types derive; egglog multi-sort as coproduct precedent for the shared rewrite library.
- **regex-automata `Hir`→NFA→DFA** — the lineage the canonical engine re-implements bespoke; the
  functor leaves that engine untouched.

## What a prototype MUST test to move family 5B

1. **Land or bridge the substrate.** Skinny holds no rich-Hir engine (parse-that-regex = numbers/utf8;
   bbnf-regex = facts facade). Either vendor the canonical engine into skinny or build it via the R3
   §8 scratch-copy — every number below depends on this and it must be step 0.
2. **Round-trip oracle:** `extract_hir(saturate(insert_hir(hir)))` DFA-equivalent to `hir` over ≥50
   patterns; `Look`/`Class` bit-identical (the Q4 assertion firing).
3. **Structural `FuseAltRegexBranches`:** replace `parts.join("|")` (main `regex.rs:300`) with
   `HirENode::Alternation` + engine `union`/`flatten`/`superset` + extract-back; assert byte-identical
   fused DFA vs the string fusion; MEASURE grammar-tier LOC deleted (target ~215).
4. **Projection soundness:** a test that FAILS on the string bridge (`[a-c]->A | [d-f]->B` merges,
   dropping A/B) and PASSES under the functor (fusion blocked when the `Project`/`Map` barriers differ).
5. **Partiality:** `Alt([Regex, Ref(ident)])` — `G` returns None on `Ref`; fusion skips the branch,
   no crash, no cross-branch fusion.
6. **Cached-facts amortization (skinny line):** make `bbnf_regex::analyze` return a memoized `RegexHir`;
   assert the many per-pattern `analyze` calls (nullable, first-set, byte-class) reuse one parse; MEASURE
   the re-parse count collapse over a grammar with repeated regex leaves.
7. **Narrowed throughput:** `regex_dfa_compile` cold per-parse re-run from the functor path — parity or
   better against `from_hir` direct.

## Cross-lock (shareable)

- **LOCK-1** owns projection typing: skinny `Project{body,target,ty}` / main `Map{inner,fn_id}` — the
  `->` codomain flows through the outer node, the bare regex types `string`; this is what makes the
  projection guard checkable. Confirm the ratified representation (Q11) is `Project`/`Map`-as-node, not
  a leaf annotation — binding on the bridge.
- The `GrammarCostModel` (`crates/ir/src/egraph/cost.rs`) already spans both e-graphs — the charter's
  "shared part"; the functor formalizes existing sharing, invents none.
- **Substrate reality is a cross-lock fact:** skinny/crates/parse-that-regex is misnamed (numbers/utf8,
  not regex) and skinny/crates/bbnf-regex is a facts facade — any lock base-pinning regex throughput
  "in skinny" must instead target the canonical READ-ONLY engine or gate on a vendoring.
- Prefilter/tier selection (double-homed with LOCK-2) sits downstream of extraction; 5B touches only the
  HIR-canon island and the string↔structural bridge, not tier selection.
</content>
</invoke>

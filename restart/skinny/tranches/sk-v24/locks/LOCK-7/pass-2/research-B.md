# LOCK-7 · Family 7B — TOTAL-SELF-HOST (pass-2 research)

The constitution's clause (i) taken to its maximal reading: bbnf grammar, regex grammar, IR, and
emitter templates *all* grammar-specified, the generator reduced to data plus a small kernel of
primitives. This report develops that route alone — its mechanism made concrete, its irreducible
floor, the substrate seams, prior art, and the two named counterexamples that bound it. The
verdict is a research answer, not a route.

---

## 1. The two axes and the frame

Total self-host of the bbnf-lang generator decomposes into two independent axes:

- **The FRONT** — bbnf-in-bbnf (`bbnf.bbnf` parsing its own source) and regex-in-bbnf. A
  recognizer/builder question over one grammar plane.
- **The BACK** — the middle IR (`BackendExpr`) and the IR→Rust emitter (`general_descent`)
  expressed as grammar data. This is the *structure-scoping* question and the hard one; it is
  where the family's floor lives.

The measurement substrate for both is the single grammar-NEUTRAL descent emitter,
`skinny/crates/codegen/src/general_descent/` (452+431+230+93 = 1206 LOC across
`emit.rs`/`view.rs`/`regex_first.rs`/`mod.rs`), reached for any `value`-less program via the shape
gate at `runtime_generator.rs:98` (`if !sink_only.has_rule("value")`). `bbnf.bbnf` has no `value`
rule, so it shape-dispatches here — the same branch Sheets travels. Every literal, dispatch key,
leaf first-byte set, kind tag, and stop-set the emitter uses is already DERIVED from the program's
`ByteLiteral`/`RegexProgram`/`ValueProject` nodes (`emit.rs:88-94` derives `LEAF_STOP` from
`program.literals`; `view.rs:103` derives kind tags from rule order; `view.rs:35` classifies `->`
projections from projection text). The emitter is thus *already "mostly data" in the derivation
sense* — it hard-codes no grammar-specific byte. What it is NOT is data in the *homoiousia* sense:
the WALK that consumes the IR (`emit_expr`, `emit_alt`) is imperative Rust. That distinction is
the whole of 7B.

---

## 2. Axis A — the IR plane (the joint LOCK-3 ⊗ LOCK-7 family)

**Scope question: does the middle-IR tower become bbnf-defined?** The IR is `BackendExpr`
(`ir/src/lib.rs:376`), a 13-variant sum: `Entry, Seq, Alt{mode,branches}, RepeatLoop{body,min},
OptionalBranch, ByteLiteral, RegexProgram{pattern,span_kind}, CallRule{callee}, SpanMark{kind,
label}, TapeEmit{kind}, DirectBuild{shape,fields}, ValueProject{body,target,ty}, Return`. Its
lowered twin `SinkOnlyExpr` (`lower/sink_only.rs:69`, isomorphic minus the erased
`SpanMark`/`TapeEmit` payloads) is what the emitter matches on. This sum is a *regular tree
grammar* — every variant is either a leaf (`ByteLiteral`, `RegexProgram`, `CallRule`, `Return`) or
a fixed-arity/starred product over itself. It is exactly the shape `bbnf.bbnf` already expresses
for its own `term`/`factor`/`rhs` tower (lines 29-52).

**`ir.bbnf` is therefore writable**, concretely: a grammar whose rules are the 13 variants, each a
concatenation of a discriminating literal and its typed children — e.g. `alt = "Alt" , "{" ,
alt_mode , "[" , ( backend_expr , "," ? ) * , "]" , "}" -> Alt`. This is LOCK-3 route 3C
GRAMMAR-HOMOIOUSIA made concrete for the IR node type.

**`ir.bbnf`'s consumer** (clause viii forbids consumer-less substrate): the `general_descent`
emitter walk itself. Under homoiousia, the transposition `BackendExpr → Rust` is a
grammar-to-grammar map — a *tree transducer* from `ir.bbnf`-parsed IR into the emitted recognizer
text. `emit_expr` (`emit.rs:288-393`) is the executable form of that transducer. So the joint
family is: **LOCK-3 supplies `ir.bbnf` (the IR as a bbnf grammar plus print/parse round-trip);
LOCK-7 supplies the emitter-as-transducer over it plus the named emit-kernel primitive (§3).** The
two locks share the transducer seam; that shared mechanism is what makes them one family.

**The subtlety that bounds it (the census floor).** Node *values* in `ir.bbnf` are pure `->` (each
variant projects to its constructor), but *build mechanisms* are host-bound: a `Seq(Vec<..>)`
needs an append (`add_expr`) and a `CallRule{callee}` needs a cross-rule link resolved after the
fact (`resolve_refs`, a TreeFold). `ir.bbnf` can DESCRIBE and PARSE the IR (the front); BUILDING an
IR value from a parse re-enters the host-function floor whose tier depends on the LOCK-1
inherited-attribute verdict. So the joint family is real but *gated*: a grammar-specified IR
*shape* lands immediately; a grammar-*built* IR lands only once the host floor (append +
cross-rule resolve) is priced by LOCK-1.

**If LOCK-3 does NOT adopt homoiousia — what remains hand-code.** The IR stays the Rust enum
`BackendExpr`; 7B's self-host is bounded to the FRONT (`bbnf.bbnf`/`regex.bbnf` parsing themselves
into a tape/view). The irreducible remainder is named exactly: (a) the emit-kernel walk
`general_descent/emit.rs` (§3), and (b) the lower pipeline `lower/sink_only.rs:174` `lower_expr`
(BackendExpr construction, 12 arms) plus `passes` shape/cost derivation — ~700 LOC of hand Rust a
front-only self-host does not touch.

---

## 3. Axis B — structure-scoping: the data-vs-escape ratio on one emitter blob

**Question: can codegen control-flow / dispatch / tape-assembly be expressed as grammar-plane data
without a borrow-checker escape?** Measured on the one blob `emit.rs` (452 LOC), classified region
by region into three strata:

| Stratum | Regions (`emit.rs`) | LOC | Nature |
|---|---|---:|---|
| **DATA (fixed kernel template)** | `render_header` 28-78; `render_prelude` 82-257 (`match_leaf`, `consume_quoted`, `skip_ws`, `match_literal`, `mark`, `parse_into_tape`, `structural_reserve`) | ~225 | Constant emitted text parameterized only by derived `stops`/`entry`/`prefix`. This IS the committable stage-0 kernel. |
| **WALK-FOLD (structural map)** | leaf arms of `emit_expr` — `CallRule` 357-360, `ByteLiteral` 361-364, `RegexProgram` 365-375, `ValueProject` 376-380, `Seq` 300-307, marker arms 381-391 | ~90 | A pure catamorphism over the IR sum; `->`-expressible IF a tree-fold combinator over `ir.bbnf` existed. |
| **IRREDUCIBLE ESCAPE (backtracking control)** | `RepeatLoop` 311-343; `OptionalBranch` 344-356; `emit_alt` 400-433; `emit_expr_probe` 438-452 | ~95 | Imperative, stateful, partial control. Not grammar-plane data. |

Ratio: **DATA ≈ 50%, WALK-FOLD ≈ 20%, ESCAPE ≈ 21%** (rest is boilerplate). The DATA half is
*already* grammar-derived and would commit verbatim as the seed kernel. The WALK-FOLD is
mechanically a fold and could ride a grammar transducer. The **21% escape is the semantic core,
not incidental** — the recognizer's whole correctness rests on it.

**The NAMED kernel-escape failure signature.** Every backtracking site emits the same idiom
(`emit_alt`, `emit.rs:414-424`, and its probe helper `emit_expr_probe:450`):

```
let mut _probe = _alt_start;
let _r: Result<(), ParseError<'i>> = (|| {
    let cursor = &mut _probe;          // emit.rs:450 — mutable shadow of the outer cursor
    ... body ...
    Ok(())
})();
if _r.is_ok() { *cursor = _probe; _matched = true; }   // commit-or-rollback
```

The signature is **commit-or-rollback over a mutable shadow cursor** — PEG ordered-choice
semantics. It resists grammar-plane expression on two concrete counts:

1. **Value-algebra mismatch.** A bbnf `->` projection is a *pure, total* map (matched span →
   value). This idiom is *stateful* (a `_matched` flag, a `_probe` cursor) and *partial* (early
   return through `?` on `ParseError`). There is no `->` form for "try; on failure consume nothing;
   set a mutable success bit."
2. **The borrow-checker bite is exact.** The escape exists specifically because the outer
   `cursor: &mut usize` and the inner `&mut _probe` are two live mutable paths to position state,
   kept disjoint ONLY by the IIFE closure scope (`emit.rs:448-451` documents this: "bind a local
   alias"). Flatten the closure and you get `E0499` — cannot borrow the cursor mutably twice. The
   IIFE is a borrow-checker *escape hatch*, not a stylistic choice.

**Conclusion for the axis.** Codegen dispatch and tape-assembly *marks* (the `mark`/`match_leaf`
calls, the kind-tag dispatch in `view.rs`) ARE expressible as grammar-plane data — they already
are, derived. Codegen *control flow* (ordered-choice backtracking, nullable-repeat progress
guards) is NOT: it is an irreducible hand-kernel primitive whose home is the combinator core
(LOCK-4) or a bytecode VM (LOCK-4C). A "total" self-host that claimed to erase this 21% would be
re-implementing a combinator/VM runtime under another name — the escape does not vanish, it
relocates. The honest total-self-host target is: **commit the DATA half + WALK-FOLD as
grammar/transducer; declare the ESCAPE a named primitive (the "emit backtrack kernel"), exactly as
production compilers do (§5).**

---

## 4. Substrate — what is already in place, what must be built

- **Emitter blob (measured):** `general_descent/emit.rs:288` (`emit_expr`), `:400` (`emit_alt`),
  `:438` (`emit_expr_probe`), prelude `:82`.
- **Shape gate that routes here:** `runtime_generator.rs:98`. Drive THIS branch; never
  `json_sink_direct`.
- **The IR sum to grammar-specify:** `ir/src/lib.rs:376` (`BackendExpr`, 13 variants),
  `lower/sink_only.rs:69` (`SinkOnlyExpr`), `lower/sink_only.rs:174` (`lower_expr`, the
  construction a grammar-built IR would replace).
- **Projection classification (the `->` value plane already present):** `view.rs:35`
  (`classify_projection` → Float/Bool/Tag/Span), `view.rs:52` (`collect_projection_kinds` walk),
  `view.rs:190` (`build_tag_table`). This proves `-> T` projections already thread through the
  VIEW emitter — but as view accessors, not as recognizer builders (see §6).
- **`bbnf.bbnf` self-source:** `grammar/bbnf/bbnf.bbnf` (89 lines, `@import`s
  `expressions`/`types`). It USES all three parser-gap constructs the FRONT self-host must first
  admit into the emitter route: `@{` atom (`term`, line 33), `|params|` closure (`closure`, line
  51), and `?w`→`->` attachment (`mapped_factor`, line 42; `identifier … -> Span`, line 9). Until
  the hand parser's atom/seq/closure gaps close, `bbnf.bbnf` cannot be routed through the emitter.
- **Regen host:** `xtask/src/regen.rs:19` (`write_targets`), dispatch `xtask/src/main.rs:39-52`.
  There is **no `regen-bbnf` and no `fixpoint` target in skinny xtask** (grep returns empty) — a
  self-host regen arm is net-new substrate, not a legacy transplant.
- **Host floor (the LOCK-1-gated seam):** `add_expr` (append) and `resolve_refs` (cross-rule
  TreeFold) are the build mechanisms `ir.bbnf`-driven construction re-enters.
- **Seed-inflation reference:** the skinny `grammar` crate hand parser is 2142 LOC. A *recognizer*
  seed is small (~305 LOC = the ~225 DATA prelude + the ~80 escape); a *builder* seed inflates
  toward that 2142.

---

## 5. Prior art (the "structure as data + kernel primitive" pattern is universal)

- **LLVM TableGen** is the canonical witness that the §3 split is SOTA, not a bbnf limitation.
  Target `.td` files declaratively describe registers, instructions, and *selection patterns* as
  data; `tablegen` emits C++ tables; the SelectionDAG/GlobalISel *matcher kernel* and emission
  control flow stay hand-written C++, compiled alongside. TableGen's docs state its purpose is
  generating "complex output files … coded in a declarative style" because "specifying it all
  manually would be unmaintainable" — the DATA is lifted, the KERNEL is not. bbnf's
  DATA-half/ESCAPE-half split mirrors the `.td`/hand-C++ split exactly.
  (https://llvm.org/docs/TableGen/, https://llvm.org/docs/TableGen/BackGuide.html)
- **MLIR ODS** rides TableGen to declare ops/traits/verifiers as records; generated C++ builders +
  a hand-written rewrite/verification kernel. Same split.
- **tree-sitter** commits `grammar.json` and `node-types.json` (generated tables) as data in the
  repo, generable "without requiring node/npm," paired with a hand-written C parse *runtime* plus
  optional hand-written C *external scanners* for constructs the grammar cannot express — the
  direct analogue of bbnf's "grammar expresses leaf semantics; host handles context-dependence"
  law. Its generator "propagates optionality upward via a fixpoint loop," the same fixpoint shape a
  bbnf staged bootstrap would use.
  (https://tree-sitter.github.io/tree-sitter/, https://en.wikipedia.org/wiki/Tree-sitter_(parser_generator))
- **rustc / GHC self-host** establish the seed-inflation law empirically: a *building* front-end
  (one that constructs a typed AST/IR, not merely recognizes) is large — the parser is thousands
  of LOC — because construction + name resolution is the bulk. Direct evidence for §6's inflation
  risk.
- **Menhir/ANTLR** self-describe their grammar syntax in their own notation but ship a hand-written
  bootstrap parser and a committed generated stage — the staged-bootstrap pattern — never a
  from-nothing meta-circular build.

The uniform lesson: **no production system erases the emitter kernel into data.** Total self-host
means maximal DATA lift + a *named, minimal, committed* kernel primitive. A 7B claim of "the
generator becomes mostly data" is defensible for the ~70% DATA+WALK-FOLD strata and false for the
~21% ESCAPE.

---

## 6. The shared open empirical question — RED-unknown, with the analysis that bounds it

*Does authoring `-> Span`/`-> T` builder projections on bbnf's structural rules over the
recognizer emitter reparse the corpus into GrammarIr-equivalent structure (within the
`add_expr`+`resolve_refs` host budget) while `regen∘regen` holds byte-identical — or does it
cascade, and at which construct?*

**RED-unknown; no verdict is seeded from the emitter's existence.** But the code fixes the shape
of the answer:

- **The emitter emits RECOGNIZERS, not GrammarIr builders.** `emit_expr` for `ValueProject`
  (`emit.rs:376-380`) emits *only the body* — "The projection … is consumed by the VIEW, not the
  recognizer." `DirectBuild` (`emit.rs:387`) emits a single `mark(tape, _node_start, kind)`. The
  product of a parse is a flat offset **tape + lazy view** (`view.rs` `{Prefix}Document`), NOT a
  `GrammarIr`. So a `-> Span`/`-> T` projection over `bbnf.bbnf` yields, through this path, a *tape
  whose nodes carry span/tag accessors* (`view.rs:118` `as_f64`/`as_bool`/`as_tag`), not an IR
  tree. **GrammarIr-equivalence is NOT native to the recognizer path** — it requires a second pass:
  `add_expr`+`resolve_refs` host functions walking the tape/view to rebuild the tree. Whether that
  host budget round-trips `bbnf.bbnf` is the actual open prototype, and it is the host floor LOCK-1
  must price.
- **`regen∘regen` byte-identity is structurally likely but near-vacuous here.** The emitter walk
  is deterministic over a `BTreeSet`-ordered program (`sink_only.rs:25` `literals: BTreeSet`,
  `emit.rs:88` `stops: BTreeSet` — sorted), so identical IR emits identical bytes. That certifies
  the *emit function is a function*, not that the *builder round-trips*. A fixed-const walk stamps
  identical bytes for any fixed input; triple-identical proves nothing about GrammarIr equivalence.
  The non-vacuous fixpoint subject must be **GrammarIr equivalence of the reparse**, not
  generated-`.rs` bytes.
- **The likely cascade point, named.** If a builder projection is authored, the first construct to
  break is the one the emitter *discards*: `CallRule{callee}` (`emit.rs:357`) emits a bare
  `parse_callee(...)?` with no captured child value — a cross-rule *reference*, not a built child.
  Rebuilding `Seq`/`Alt` children as GrammarIr from a tape that recorded only *offsets* (no
  parent/child edges) forces the host to re-derive nesting from offset order — exactly the
  `resolve_refs` TreeFold. The cascade, if it happens, begins at nested `Alt`-inside-`Seq` (`term`,
  `bbnf.bbnf:29-36`) where offset order alone underdetermines the tree.

**What the prototype must test (the family-moving experiment):**

1. Close the three parser gaps (`@{`, `|params|`, `?w`→`->`) so `bbnf.bbnf` routes through the
   `!has_rule("value")` branch. Precondition, not the experiment.
2. Author `-> Span`/`-> T` builder projections on `bbnf.bbnf`'s structural rules; drive
   `general_descent`, never `json_sink_direct`.
3. Add `add_expr`+`resolve_refs` host functions that walk the emitted tape/view into a `GrammarIr`.
   **Measure host LOC + reparse equivalence** against the hand parser's `GrammarIr` on the
   bbnf-source corpus.
4. Certify `regen∘regen` with subject = **GrammarIr equivalence** (`Empty ≡ Concat([])`
   normalized), not `.rs` bytes; plant a mutation (reorder one `Alt` arm) and confirm the fixpoint
   trips — a fixpoint that cannot fail certifies nothing.

The move that advances 7B is the pair (host-budget LOC, cascade construct). Small host budget + no
cascade ⇒ total self-host of the FRONT is real; inflation toward 2142 LOC (§5 law) ⇒ 7B's FRONT
collapses into a staged-bootstrap and only the emitter's DATA half is genuinely lifted.

---

## 7. Named risks and counterexamples

- **R-B1 — The escape relocates, it does not vanish.** Any claim to grammar-express the ~21%
  backtracking control is a disguised re-import of a combinator/VM runtime (LOCK-4C). Counter-test:
  flatten one `emit_alt` site into a non-closure form; observe `E0499`. The borrow error IS the
  proof the escape is irreducible on the current cursor model.
- **R-B2 — Builder seed inflation.** A GrammarIr-*building* seed inflates toward the 2142-LOC hand
  parser (rustc/GHC evidence, §5). Counter-guard: the prototype reports host-builder LOC as a hard
  number; if it exceeds ~½ the hand parser, "total self-host" is demoted to "front recognizer
  self-host + committed builder kernel."
- **R-B3 — `ir.bbnf` with no builder is consumer-less.** A grammar that only *parses* an IR text
  no one produces violates clause viii. Counter-guard: `ir.bbnf`'s consumer must be the live
  emitter transducer over real `passes`-produced IR, with a print∘parse round-trip oracle — never a
  synthetic IR-text corpus.
- **R-B4 — Byte-determinism theater.** `regen∘regen` byte-identity over a fixed-const walk is
  near-vacuous. Counter-guard: fixpoint subject = GrammarIr equivalence + planted-mutation
  falsification (§6.4).
- **R-B5 — Homoiousia gated on LOCK-1.** The joint LOCK-3⊗LOCK-7 IR family cannot land its *built*
  half until the inherited-attribute host tier (`add_expr`/`resolve_refs` signature) is priced by
  LOCK-1. Counter-guard: draft the family in two stages — grammar-specified IR *shape* (unblocked)
  and grammar-*built* IR (LOCK-1-gated) — never as one monolith.

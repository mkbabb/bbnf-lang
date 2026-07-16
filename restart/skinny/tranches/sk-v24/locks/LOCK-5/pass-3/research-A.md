# LOCK-5 · Family 5A — ONE-HIR · pass-3 research

**Thesis of the family.** The regex HIR is a sub-language of bbnf's own IR: regex leaves grow
inside `ir::ExprKind`, automata build directly from that IR, and `classify` dissolves into an
IR analysis pass. The settled two-type ground (grammar tier opaque `Regex(String)`, engine tier
rich `Hir` with `Class`/`Look`, `string`/`simplify_hir` bridge) is FACT and is honored below;
5A's job is to make concrete what the *single-home* variant costs and buys, measured against
that bridge, and to carry it SPEC-ONLY until LOCK-7/7A ratifies grammar-totality. Everything is
pointer-anchored.

## The substrate, as it actually stands (two homes, one heuristic seam)

- **Grammar tier (opaque).** `ir::ExprKind::Regex { pattern: String }` —
  `skinny/crates/ir/src/lib.rs:250`. The string is never structurally decomposed on the grammar
  side; it is threaded to the bridge verbatim.
- **The bridge.** `bbnf_regex::analyze(&str) -> RegexFacts` —
  `skinny/crates/bbnf-regex/src/lib.rs:45`. 322 LOC total. `RegexFacts{nullable, first, byte_classes,
  hir:RegexHir, string}` (:1). The "HIR" here is a **4-variant tag** `RegexKind{Whitespace,
  QuotedString, Numeric, Unknown}` (:16), NOT a tree. Classification is **string name-gating**:
  `classify` (:92) tests `pattern == r"[ \t\n\r]*"`, `pattern.starts_with("-?(0|[1-9]")`,
  `starts_with('"')`. This violates the no-name-gates edict (Constitution vi) and is the single
  strongest argument FOR 5A: the heuristic reconstructs structure a grown leaf already carries.
- **Engine tier (rich, in parse-that).** `bbnf-regex` = 6742 LOC.
  `parse-that/rust/regex/src/hir/mod.rs:117` `pub enum Hir { Empty, Literal(Vec<u8>),
  Class(CharClass), Look(Look), Repetition, Group(Box<Hir>), Concat(Vec<Hir>), Alternation(Vec<Hir>) }`.
  `CharClass` :45 (byte + codepoint ranges, "expanded to UTF-8 byte sequences **during NFA
  construction**" :53), `Look` :73. This is the "rich `Hir` grown with `Class`/`Look`" the charter
  names — the leaves already exist here.
- **classify.** `parse-that/rust/regex/src/classify.rs:55` `RegexClass` — 10 variants (`Numeric`,
  `QuotedString`, `HexDigits`, `Identifier`, `WhitespaceWithBlockComment`, `CharClassQuantified`,
  `PrefixThenClass`, `AccelDriven(u8)`, `Unknown`), `canonical_pattern()` :132, 1148 LOC (the
  single largest file). Re-lexes the pattern string internally.

## Q1 — the per-pass dispatch census (built, not estimated)

Every arm in the skinny pipeline that touches a regex leaf, today routing through the opaque
string + `analyze`:

| # | Pass / role | Site | Current arm (opaque) | Under grown `Class`/`Look` |
|---|---|---|---|---|
| 1 | nullability | `ir/src/lib.rs:331` | `Regex{pattern} => analyze(pattern).nullable` | structural: `Repeat{min:0}`/`Optional`/`Class`(non-empty→false) |
| 2 | pretty | `ir/src/lib.rs:171` | `Regex{pattern} => /.../ ` (format only) | recurse over leaf tree |
| 3 | types / infer_expr | `passes/src/lib.rs:194` + helper `regex_type` :224 | `=> Type::Builtin(regex_type(pattern))` (Unit if Whitespace else Span) | `Class → Span<width>`, `Look → Unit` (Q2) |
| 4 | scalar_routing / recognizers | `passes/src/lib.rs:348` | `Regex{pattern} if analyze(..).hir.kind==QuotedString => insert(b'"')` | match on `Class`/delimiter leaf directly |
| 5 | first_sets / first_bytes | `passes/src/lib.rs:911` + helper `regex_first_bytes` :943 | `=> regex_first_bytes(pattern)` (reads `analyze().first`) | `Class`'s ByteSet256 **is** the first-set, zero re-parse |
| 6 | materialization / materialize | `passes/src/lib.rs:1341` | `=> BackendExpr::RegexProgram{pattern, span_kind}` (→ `ir/lib.rs:389`) | `BackendExpr::ByteClass`/program from leaf tree |
| 7 | trivial-false census arms | `passes/src/lib.rs:790, 814, 836, 1108` | `Regex{..} => false / {}` (recovery, host-decode, dispatch-overlap, span-classify) | unchanged (structure-agnostic) |

**Shrink vs relocate — the honest split.** Two answers, because there are two homes:
- **Skinny substrate: genuine SHRINK.** The grown leaves make the entire string-heuristic block
  in `bbnf-regex/src/lib.rs` dead: `classify` (:92), `regex_is_nullable` (:107),
  `first_from_prefix` (:142), `parse_leading_class` (:162), `class_byte` (:200), `is_single_atom`
  (:220), `balanced_parenthesized` (:236) — ~200 LOC that re-parse the pattern string to recover
  first-sets/nullability that `Class`/`Repeat` leaves carry by construction. Deleting them removes
  the name-gate. Net skinny LOC: **negative**.
- **Engine classify: RELOCATE, not shrink.** The 1148-LOC `classify.rs` carries genuine
  information (tier selection: `AccelDriven`, `PrefixThenClass`, `CharClassQuantified`). As an IR
  analysis pass it keeps that content; the ONLY deletion is the internal re-lex step (it currently
  re-parses the pattern string it was already handed a tree for). Estimated re-lex delta ≈ the
  `hir/parser/` re-entry, small. So classify **relocates** with a minor de-dup, NOT a collapse —
  and this must be reported as a relocate to avoid overclaiming.

## Q2 — typing `Class`/`Look` through LOCK-1's HM, and the byte-expansion confinement

`regex_type` (`passes/src/lib.rs:224`) is a stub returning `BuiltinTy::{Unit,Span}`. Routed through
LOCK-1's real HM (row-poly + widths), a grown leaf types as:
- `Class(bs)` → `Span` carrying a **width var** (cardinality/length), solved as a bounded
  post-solve CSP over the numeric-width lattice (Constitution: unification for equality, CSP only
  for widths/overloads). Nullable is a boolean fact, not a type.
- `Look(_)` → the unit/zero-width type (consumes no input; contributes `nullable=true`).

**Where `[α-ω]*` byte-expansion lives — the confinement boundary.** It lives CONFINED TO AUTOMATA.
The parse-that Hir already states this: `CharClass` is "expanded to UTF-8 byte sequences **during
NFA construction**" (`hir/mod.rs:53`). The shared *type* must see only the abstraction (`Span` +
width + nullable bit) — never the 256-bit `ByteSet256`. Confinement boundary drawn concretely: the
`ByteSet256` (`bbnf-regex/src/lib.rs:35`) stays inside the engine-tier leaf; the grammar-tier type
carries no byte payload. This preserves Constitution v (type plane zero runtime cost) and keeps the
serialized `GrammarIr` (serde-derived, `ir/lib.rs:33`) from bloating with 256-bit sets per class.
Leak test the prototype must run: assert the `Type` produced for `[a-c]` and `[a-z]` is
**identical** (both `Span<width=1>`), proving byte content did not enter the type.

## Q3 — automata parity re-derived FROM the narrowed type

`from_expr` (NFA built from grammar IR grown leaves) must be transition-table byte-identical to the
incumbent `from_hir` (`parse-that/rust/regex/src/automata/nfa.rs:71`, Thompson,
`add_epsilon` priority-tagged :118). Two identity hazards, both nameable:
- **Group-elision.** `Hir::Group(Box<Hir>)` (`hir/mod.rs:129`) elides to its inner during NFA
  build; the grammar IR has no `Group` node (parens are structural in `ExprKind::Seq`/`Alt`), so
  `from_expr` must NOT emit an epsilon that `from_hir` elides — else the tables diverge by one
  state.
- **`Optional ↔ Repeat{0,1}`.** `ir::ExprKind::Optional(body)` (`ir/lib.rs:245`) vs
  `Repeat{min:0,max:Some(1)}` (:240) — the incumbent `Repetition` (`hir/mod.rs:95`) normalizes
  these to one shape; `from_expr` must apply the same normalization pre-Thompson.

Harness: for a corpus of grown leaves, build both NFAs and assert `transition_byte_sets`
(`nfa.rs:90`) equality state-for-state; derive throughput fresh from the type-narrowed build (carry
NO inherited band — the certified 8.8–29.5× prefilter figure (SPEC §0 feature 10) is a *match*-axis
number and does not transfer to a *construction*-parity claim).

## Q4 — a `Look`-consumer test that can FAIL

The skinny substrate has **no Look consumer at all**: `RegexKind` (`bbnf-regex/src/lib.rs:16`) has
no anchor variant and `analyze` ignores `^`/`$`. So `/foo$/` and `/foo/` are today
indistinguishable (both → `Unknown`, both would over-accept). This is precisely the
consumer-less-substrate hazard (Constitution viii): a `Look` leaf with no NFA-end consumer is
forbidden. The failing/passing assertion the prototype must exercise:
- Build NFA from the grown leaf for `/foo$/`; run against `"foobar"` → **reject** (Look::End
  consulted at accept), and against `"foo"` → accept.
- Build `/foo/`; run against `"foobar"` → accept.
- Assert the first two differ. This test FAILS on the current substrate (proving the gap) and
  passes only once `Look::{Start,End}` (`hir/mod.rs:73`) is threaded into the match loop.

## Q9 — one grammar-driven path: break the discard

Two `parse_generated` exist in parse-that (READ-ONLY, mapped):
- `parse-that/rust/parse_that/src/regex/generated.rs:2288` `pub fn parse_generated(pattern:&str)
  -> bool` — the **grammar-DRIVEN** parser generated from `rust/bootstrap/regex.bbnf`. It returns
  `bool`: a recognizer that **discards its AST**. This is the discard to break.
- `parse-that/rust/parse_that/src/regex/mod.rs:34` `pub fn parse_generated(pattern) ->
  Result<Hir,ParseError>` — a wrapper that DOES yield `Hir`, but via the hand-written
  `hir/parser/` (`mod.rs:153`), not the generated grammar path.

Promotion: thread the `->` projections through the generated parser so `generated.rs:2288` returns
`Hir` instead of `bool`, then differential-oracle it against the hand parser (the meta-circular
oracle pattern). This exercises exactly one grammar-driven regex path end-to-end.

**De-dup — two dialects, one canonical.** `parse-that/rust/bootstrap/regex.bbnf` (the "full L4
spec for self-hosting", `>>`/`<<`/`,` dialect, leaf-first with recursive spine) vs bbnf-lang
`grammar/misc/regex.bbnf` (27 lines, ISO-EBNF dialect: `{ }` repetition, `[ ]` optional, explicit
`lookahead`/`lookbehind`/`back_reference`/`anchor`). These are *different grammars of the same
language*. The bootstrap one is the self-hosting production spec; `grammar/misc/regex.bbnf` is a
spec-reference. Convergence must pick the bootstrap grammar as canonical and either delete or
demote the misc copy — carried SPEC-ONLY, gated on LOCK-7/7A per charter.

## Q10 — grammar-tier e-graph rewrite census

The rewrite library is real and lives engine-side: `parse-that/rust/regex/src/egraph/` = 1253 LOC.
Rules: `repetition.rs`(156), `union.rs`(108), `flatten.rs`(107), `superset.rs`(90),
`redundant.rs`(68); driver `mod.rs`(200), `translate.rs`(132), `cost.rs`(102),
`saturation_cache.rs`(68). Tests exist per rule (`regex/tests/egraph_rules_{flatten,repetition,
redundant,superset,union}.rs`) but assert firing on *hand-picked* inputs, not corpus frequency. The
prototype must instrument the saturation driver to emit **per-rule fire counts** over the ≥50
tagged corpus, on both bare leaves and projection-wrapped (`ExprKind::Project` over regex) branches
— because a rule that never fires on real branches is a consumer-less subsystem (viii).

## Q11 — the `->` representation (binding on bridge soundness)

Concretely, `->` is a **typed wrapping edge, not an inner-class Map barrier**. IR:
`ExprKind::Project { body: ExprId, target: String, ty: Option<String> }` (`ir/lib.rs:261`), lowered
to `BackendExpr::ValueProject { body, target, ty }` (`passes/lib.rs:1349` → `ir/lib.rs:407`). A
projected regex leaf (`/.../  -> Name: Type`) wraps the WHOLE regex; the matched span is projected
wholesale, and `infer_expr` passes the body type through transparently (`passes/lib.rs:204`
`Project{body,..} => self.infer_expr(*body)`). Consequence for the bridge: projection sees the
regex as one opaque span — there is **no inner-class projection** (you cannot `->` inside a
`Class`), so the bridge's `string`/`format!` round-trip is sound because nothing structural crosses
the Project boundary. LOCK-1 must ratify this (that `->` never needs to bind sub-leaf captures);
5D's capture-leaf ambition (a `Class` that itself projects) would break this and is the divergence
to watch.

## Prior art (cited)

- **rustc HIR/MIR + GHC Core**: the canonical "one typed IR, every pass is IR→IR, leaves are enum
  variants passes dispatch on" — the shape 5A imposes on regex. GHC Core is the purest single-IR
  precedent.
- **MLIR dialects**: a sub-language embedded in one IR with lawful lowering between dialects — the
  exact "regex HIR as a sub-language of bbnf HIR" isomorphism (Constitution ii); MLIR proves the
  embed-as-dialect route scales without a second IR home.
- **regex-syntax `Hir` vs regex-automata NFA** (BurntSushi): the type/automata split where the
  class stays symbolic in the HIR and is byte-expanded only at NFA build — the precedent for Q2's
  confinement boundary. regex-automata's memchr/Teddy prefilter ladder is the source PROVE-10 ported
  (SPEC §0 feature 10).
- **tree-sitter external scanners**: grammar-driven with a typed host-fn escape hatch — the
  hybrid-grammar-host analogue bounding what stays grammar-expressible (LOCK-7C).
- **egg / egglog**: equality saturation underpinning the `egraph/` rewrite rules (Q10).

## Named risks & counterexamples

1. **Contradicts the ratified two-type ground.** Growing `ExprKind` with `Class`/`Look` collapses
   the grammar tier's opaque `Regex(String)` — which is RATIFIED (XL-C). 5A is therefore SPEC-ONLY
   until LOCK-7/7A; the report does not re-open the adjudication, it prices the single-home variant.
2. **"Shrink" is only true skinny-side.** Engine classify RELOCATES (1148 LOC of tier logic
   survives). Claiming a global shrink would be dishonest; the win is name-gate deletion + re-lex
   de-dup, not a 1148-LOC collapse.
3. **The substrate deliberately lacks a real parser.** Deleting the heuristic `analyze` forces
   skinny to consume a REAL regex parser (vendor parse-that's `hir/parser/`, ~787 LOC across
   `class/escape/group/repetition/atom/cursor`). ONE-HIR's shrink is contingent on paying that
   import — the tension the family must own.
4. **Byte-expansion leak** into serialized `GrammarIr` if `ByteSet256` rides the grammar leaf —
   defeated only by the Q2 confinement (byte set stays engine-side).
5. **Consumer-less `Look`** — adding anchor leaves with no NFA-end consult is forbidden (viii); Q4
   is the guard.

## What a prototype must test to move 5A

1. **Byte-identity**: `Nfa::from_expr(grown ExprKind::Class/Look/Repeat)` transition-table-identical
   to `from_hir` on the incumbent `Hir`, with Group-elision + `Optional↔Repeat{0,1}` normalized
   (Q3).
2. **Look FAIL test**: `/foo$/` rejects `"foobar"` through the grown-leaf match loop; `/foo/`
   accepts (Q4).
3. **LOC-delta census**: actually re-home classify as an IR pass and diff LOC to settle
   shrink-vs-relocate empirically (Q1) — measure the skinny deletion and the engine relocation
   separately.
4. **Type-leak assertion**: `[a-c]` and `[a-z]` infer identical `Span<width>` (Q2 confinement).
5. **Discard-break**: `generated.rs`'s grammar-driven parser returns `Hir`, differential-equal to
   the hand parser over the corpus (Q9).
6. **Per-rule egraph fire counts** over the ≥50 tagged corpus, bare and projection-wrapped (Q10).

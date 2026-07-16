# LOCK-5 · Family 5B — LAWFUL-FUNCTOR — pass-3 research

## The thesis, made concrete

Two domain-fit regex types, joined by one total functor each way, tested by round-trip
oracles; the e-graph rewrite library is the shared substrate. The grammar tier keeps the
leaf opaque; the engine tier keeps a rich structural HIR; a cheap string/`format!` bridge —
reusing the engine's `simplify_hir` canonicalizer — joins them, so there is exactly ONE
automata path and no second NFA builder to keep in parity.

The substrate to harden already exists in pieces across three homes. The lawful-functor work
is to collapse them to two types + one functor, delete the divergent third, and pin every
number to the canonical engine at HEAD.

## The three regex representations extant (the divergence that 5B closes)

1. **Grammar tier — opaque leaf.** `ExprKind::Regex { pattern: String }`
   (`skinny/crates/ir/src/lib.rs:250`); backend twin `BackendExpr::RegexProgram { pattern:
   String, span_kind }` (`ir/src/lib.rs:389`), lowered at `passes/src/lib.rs:1341`. The
   pattern string survives verbatim to codegen (`codegen/src/general_descent/regex_first.rs`,
   `codegen/src/json_sink_direct/first_set.rs`). This IS the correct opaque grammar-tier
   type — keep it.

2. **The canonical engine — rich HIR.** parse-that `rust/regex` (`bbnf-regex`, 6742 LOC).
   `pub enum Hir` (`hir/mod.rs:216`) already carries the grown leaves: `Class(CharClass)`
   with an explicit `negated` flag (`hir/mod.rs:44`) and `Look` (`hir/mod.rs:172`,
   Start/End/StartLF/EndLF/StartCRLF/EndCRLF). Analysis surface: `RegexInfo`
   (`info/mod.rs:137` — `classification: RegexClass`, `first_chars: CharSet128`, `nullable`,
   `must_consume`, `min_len/max_len`, `literal_prefix/suffix`) via `analyze_from_hir`
   (`info/mod.rs:234`); the finite taxonomy `RegexClass` (`classify.rs:55`,
   `classify_regex_from_hir` at `:177`). This is the correct engine-tier type — keep it.

3. **The divergent shadow — a hand-rolled coarse facts crate.** `skinny/crates/bbnf-regex`
   is NOT a vendored copy of the engine: it is 322 LOC, one `lib.rs`, with its own
   `RegexFacts { hir: RegexHir { kind: RegexKind }, ... }` (`:2-24`), a `pub fn analyze(&str)
   -> RegexFacts` (`:45`), and a ~170-LOC hand-written string-matching `fn classify(&str) ->
   RegexKind` (`:92`). It has NO `Class`/`Look` structure, NO e-graph, NO automata. And it is
   what the grammar tier actually calls: `use bbnf_regex::analyze;` (`ir/src/lib.rs:1`),
   consumed at `nullability` (`ir/src/lib.rs:331` — `ExprKind::Regex { pattern } =>
   analyze(pattern).nullable`). A separate `skinny/crates/parse-that-regex` (3022 LOC) is an
   orthogonal scan/number/unicode utility (Eisel–Lemire, UTF-8 dispatch, `StringMode`) — not
   an HIR engine either.

**The 5B mechanism**: retire representation #3. Route the grammar tier's `analyze` through
the canonical engine's `RegexInfo::analyze_from_hir` so the opaque string leaf (#1) is joined
to the rich HIR (#2) by ONE functor. Measurement substrate = the canonical engine reached by
the workspace `bbnf-regex` path-dep at parse-that HEAD; base-pin every throughput/compile
number to that HEAD.

## The functor, both directions

**Forward (grammar → engine), total.** `pattern: &str` → `hir::parser::parse_with(pattern,
byte_mode)` (`hir/parser/mod.rs:158`) → `Hir` → `simplify_hir(hir, cost)` (`egraph/mod.rs:143`)
→ `RegexInfo::analyze_canonical` (`info/mod.rs:288`). Total on the leaf domain: a parse error
maps to `RegexClass::Unknown` (`classify.rs` fast-path), never a panic. The functor's OUTPUT
for the grammar tier is the finite `RegexClass` + scalar facts (`nullable`, `first_chars`,
`must_consume`, widths) — NOT the byte-expanded automaton. That confinement is the answer to
the type-leak question below.

**Reverse (engine → grammar string), currently PARTIAL — the named 5B gap.**
`RegexClass::canonical_pattern() -> Option<&'static str>` (`classify.rs:132`) returns a string
for only four shapes (two `Identifier`, two `QuotedString`); the parameterized variants
`CharClassQuantified`, `PrefixThenClass`, `AccelDriven`, `Unknown` return `None`
(`classify.rs:150`). So the reverse arrow is not total: a round-trip through the taxonomy loses
patterns it cannot re-spell. **This is the single concrete thing a 5B prototype must fix** —
make the reverse a total `Hir -> String` (structural unparse of `Class`/`Look`/`Repetition`/
`Concat`/`Alternation`, byte-range → `[...]`, `Look::End` → `$`), so the round-trip oracle
`parse ∘ unparse ∘ simplify_hir ∘ parse` reaches a fixpoint at the canonical HIR (string may
vary; the simplified HIR is the invariant). `canonical_pattern`'s `&'static` returns become a
fast path over the general unparser, not the whole reverse arrow.

**Shared part (already realized).** The e-graph rewrite library is `egraph` + `egraph-derive`
(skinny `Cargo.toml:36`, path `../crates/egraph`; parse-that consumes the same via patch).
`HirENode` (`egraph/node.rs`) is the `Id`-projected e-node, deriving `Language` via
`egraph_derive::Language`; it explicitly documents the owning-tree/e-node split as load-bearing
(egg/egglog/Cranelift ægraph precedent). The grammar-tier IR e-graph (`bbnf-ir`) and the regex
HIR e-graph use the SAME `egraph` engine — 5B's "shared part" is not aspirational, it is the
current wiring. `simplify_hir` gates on `needs_saturation` (`egraph/mod.rs:196`): leaves and
literals skip the build/saturate/drop entirely, so the bridge is near-free on the JSON/CSS
grammars that are mostly `Class`/`Literal`.

## LEAF-GROWTH — per-pass answers

**Q1 dispatch census.** The regex-leaf arms in the grammar/codegen pipeline that must consume
the grown engine facts (not the coarse `RegexFacts`):
- `ir/src/lib.rs:331` — `nullability`: today `analyze(pattern).nullable` on the 322-LOC crate;
  re-point to `RegexInfo{..}.nullable`.
- `ir/src/lib.rs:171` — pretty-printer `/pattern/` (string passthrough, unaffected).
- `passes/src/lib.rs:1341` — `ExprKind::Regex → BackendExpr::RegexProgram` (string passthrough).
- `codegen/src/general_descent/regex_first.rs` + `codegen/src/json_sink_direct/first_set.rs` —
  leading-byte-set derivation: today re-derived locally from the `RegexProgram` pattern; should
  consume `RegexInfo.first_chars: CharSet128` (`info/mod.rs:156`) directly.
- `codegen/src/general_descent/{emit,view}.rs`, `json_sink_direct/{dispatch,skeleton}.rs` —
  `RegexClass`-driven `FnDescriptor` specialization.

Classify re-home is a SHRINK, not a relocate: skinny's hand-rolled `classify` (`bbnf-regex/
src/lib.rs:92`, ~170 LOC) is DELETED outright (net −322 on skinny), its role subsumed by
parse-that's `classify_regex_from_hir` (`classify.rs:177`, 1148-LOC file) which already exists
and is richer (Look-aware, negation-aware, `PrefixThenClass`/`CharClassQuantified` variants the
shadow lacks). The system loses a duplicate; no new engine LOC is authored on the skinny side.

**Q2 typing through LOCK-1 HM, byte-expansion confinement.** The grammar-tier leaf carries no
type today. Under the functor, its type is derived from `RegexClass` + width facts — a finite
sum (`Numeric{f64-ish}`, `QuotedString{String}`, `HexDigits`, `Identifier`, …), each a `Named`
abstract type LOCK-1's HM resolves per backend. The byte-expansion of `[α-ω]*` — `CharClass::
to_positive_byte_ranges` and the UTF-8 sequence expansion (`hir/mod.rs:74`, invoked only in
`Nfa::build_hir`, `automata/nfa.rs`) — stays CONFINED to automata construction. The shared type
plane sees the class as an ATOM (a `RegexClass` variant + `CharSet128` first-set + width), never
the 256-bit byte set. Confinement boundary = `RegexInfo` (facts, atom-level) vs `Nfa::from_hir`
(bytes, automata-level); the type never crosses into the second. This satisfies the zero-runtime
type law: `RegexClass` is comptime-only, erased at emission.

**Q3 automata parity — why 5B needs no `from_expr`.** `from_expr` does NOT exist anywhere in
either tree (grep-confirmed). The current and only NFA entry is `Nfa::from_hir` (`automata/
nfa.rs:71`), reached via `from_pattern` → `parse` → `from_hir` (`:63`). Because the lawful
functor keeps the STRING as the interchange between tiers, there is a single automata builder;
parity is the identity, not a re-derivation. This is 5B's parsimony argument against sibling
routes that would introduce a second `from_expr` NFA path needing byte-identity proof: 5B has
nothing to keep in sync. If a grammar-fragment lowering ever adds `from_expr`, 5B's oracle is
`from_expr ≡ from_hir ∘ parse ∘ unparse` — but 5B does not require it to exist. Throughput is
re-derived fresh from the narrowed leaf at parse-that HEAD, carrying no inherited band.

**Q4 a Look-consumer test that can FAIL.** The shadow crate has no `Look`; `analyze(r"foo$")`
on `bbnf-regex/src/lib.rs:92` classify falls to a coarse kind and drops the anchor. The
canonical engine parses `$` to `Hir::Look(Look::End)` (`hir/mod.rs:172`). A failing/passing
assertion the prototype exercises: route `/foo$/` through the functor, compile the HIR to a DFA,
assert `find` REJECTS `"foobar"` and ACCEPTS `"foo"`; the anchored `/foo/` control accepts both.
Today this test FAILS on the skinny path (anchor lost) and PASSES on the canonical engine —
which is precisely the divergence 5B eliminates by making the grammar tier consume the engine.

**Q11 bridge projection soundness with LOCK-1's `->`.** `->` is `ExprKind::Project { body,
target, ty }` (`ir/src/lib.rs:250`). For `/[0-9]+/ -> Number`, the projection's inferred type
must be driven by `RegexClass::Numeric` (from the functor), not by the raw byte class. The
soundness obligation: the functor's `RegexClass` output must reach `Project`'s inference BEFORE
the byte-expansion barrier — i.e. the `Map`/`Project` sees the atom-level classification, and
the captured span (bytes matched by the DFA) is decoded per that classification. Confirm with
LOCK-1 whether `->` over a regex leaf is represented as an inner-class `Map` barrier (the
projection consumes a decoded value) or a typed edge (the projection consumes the raw span with
a type annotation); the answer binds whether the functor must decode-at-leaf or defer. Either
way, the classification — not the byte set — is the projection's type witness.

## Prior art (citations)

- **Owning-tree vs e-node split** is universal: egg (Willsey et al., POPL 2021, "egg: Fast and
  Extensible Equality Saturation"), egglog, Cranelift ægraphs — each draws the exact
  `Box<T>`-owning / `Id`-child distinction `egraph/node.rs` documents. 5B's two types are the
  same law one tier up: a domain-fit owning type per tier, one functor between.
- **regex-automata** (`regex` crate) separates `regex-syntax::hir::Hir` from the automata NFA/
  DFA; the engine here mirrors that but keeps `CharClass::negated` explicit rather than
  materializing complements (`hir/mod.rs:3`) — a deliberate, lawful representation choice that
  makes the reverse functor (unparse) tractable (`[^...]` re-spells directly).
- **Lawful functors / round-trip lenses**: the bidirectional-transformation literature (Foster
  et al., "Combinators for bidirectional tree transformations", POPL 2005) frames exactly the
  total-each-way + round-trip-law obligation 5B's oracle encodes; the get/put lens laws are the
  `parse ∘ unparse = id_HIR` property (up to `simplify_hir` canonicalization).
- **GHC/rustc HIR→Core lowering** is the "two IRs, one total lowering" precedent at scale; 5B is
  its regex-scale instance, distinguished by needing the reverse arrow (unparse) for grammar
  round-trip oracles.

## Named risks / counterexamples

- **The reverse functor is not free.** `simplify_hir` canonicalizes (dedup, superset absorb,
  union merge, flatten — `egraph/rules`), so `unparse(simplify_hir(parse(p)))` need not equal
  `p` as a STRING. The oracle must assert HIR-level fixpoint, not string equality. Counterexample
  to a naive string oracle: `[ab]|[bc]` → union-merged to `[abc]` → unparses to `[abc]` ≠ input
  string, but the canonical HIR is stable. Mitigation: oracle on the `simplify_hir` fixpoint.
- **Deleting the shadow changes `nullability` answers.** The 322-LOC crate's coarse `analyze`
  may return `nullable` values that differ from the engine's `is_nullable(hir)` on patterns it
  mis-classified (anchors, nested alternation). Counterexample: `foo$` — shadow is anchor-blind;
  engine says non-nullable with a `Look::End`. This is a CORRECTNESS FIX, but it moves
  grammar-analysis outputs; the prototype must diff nullability over the engine corpus
  before/after and confirm the deltas are strict improvements.
- **Byte-expansion leak.** If any grammar-tier pass reaches for `CharClass` byte ranges (not the
  atom facts), the type plane leaks 256-bit sets and the zero-cost invariant is at risk.
  Guard: the functor's public return exposes only `RegexInfo` (atom-level) — no `Hir`/`CharClass`
  handle escapes into `ir`/`passes`/`codegen`.
- **Two-crate name confusion.** `skinny/crates/bbnf-regex` (shadow, 322 LOC) and
  `skinny/crates/parse-that-regex` (scan utility, 3022 LOC) are BOTH not the engine; the engine
  is parse-that `rust/regex` (6742 LOC). Any measurement that runs against the skinny shadow is
  off-substrate. Base-pin to parse-that HEAD.

## What the prototype must test to move 5B

1. **Total reverse functor.** Implement `Hir -> String` structural unparse (all variants, incl.
   `Look`, negated classes, bounded repetition); prove the round-trip oracle over the engine's
   50+-pattern corpus: `parse(p)` and `parse(unparse(simplify_hir(parse(p))))` yield the same
   `simplify_hir` fixpoint. This is the lawful-functor proof.
2. **Grammar-tier re-point.** Replace `use bbnf_regex::analyze` (`ir/src/lib.rs:1`) with the
   engine's `RegexInfo::analyze_from_hir`; delete `skinny/crates/bbnf-regex` (net −322); re-run
   `nullability` over the corpus and diff — confirm the deltas are correctness improvements.
3. **The failing Look test.** `/foo$/` rejects `"foobar"`, accepts `"foo"`, through the functor
   to a DFA (`automata/dfa.rs` `Dfa::find`); `/foo/` control accepts both. Exercised, not
   asserted-by-inspection.
4. **Projection witness.** `/[0-9]+/ -> Number` — confirm `Project` inference receives
   `RegexClass::Numeric` (atom-level), the byte set never enters the type plane; functor cost
   (parse+simplify) recorded at parse-that HEAD, carrying no inherited band.

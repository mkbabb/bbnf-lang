# LOCK-5 · FAMILY 5B — THE LAWFUL FUNCTOR (research, pass 3)

Two HIRs, one total functor each way, tested by round-trip oracles; each side keeps a
domain-fit representation; the e-graph rewrite library is the shared part. This report makes
that mechanism concrete against the code that exists, pins the confinement boundary, censuses
the shared rewrite substrate, and names the one gap a prototype must close.

---

## 1. The two types already exist — the functor is asymmetric, and half-built

**Grammar tier (opaque).** `skinny/crates/ir/src/lib.rs:250` — `ExprKind::Regex { pattern: String }`;
the backend mirror is `BackendExpr::RegexProgram { pattern: String, span_kind }`
(`skinny/crates/ir/src/lib.rs:389`). The grammar plane carries regex as an *opaque payload* — in
skinny it is even a bare `String`, not the `Regex(StringId)` the settled ground names; StringId
interning is a later economy, not a semantic change. No `Class`/`Look`/`Repetition` structure is
visible to any grammar-tier pass. The only consumer that peeks inside is nullability
(`ir/src/lib.rs:331` — `ExprKind::Regex { pattern } => analyze(pattern).nullable`), and it peeks
through the engine's own analyzer, never by pattern-matching the string.

**Engine tier (rich).** `parse-that/rust/regex/src/hir/mod.rs:216` — `enum Hir { Empty, Literal(Vec<u8>),
Class(CharClass), Look(Look), Repetition(Repetition), Group(Box<Hir>), Concat(Vec<Hir>),
Alternation(Vec<Hir>) }`. `Class`/`Look` are **already grown** — not a future addition to
audit-for-arms: `CharClass` (`hir/mod.rs:45`) carries the `negated` flag verbatim (never
materializing the complement), and `Look` (`hir/mod.rs:172`) has six variants
(`Start/End/StartLF/EndLF/StartCRLF/EndCRLF`). The two types are already the domain-fit pair 5B
posits; nothing must be *added* to make them distinct.

**Forward functor F (bbnf → engine) is total and one line.** `Nfa::from_pattern`
(`parse-that/rust/regex/src/automata/nfa.rs:63`) *is* the functor: `from_hir(hir::parser::parse(pattern))`.
The `String` → `Hir` step is `hir::parser::parse` (`hir/parser/mod.rs:153`, `parse_with` `:158`);
`Hir` → automata is `from_hir` (`nfa.rs:71`). There is no distinct `from_expr`: Q3's "from_expr
byte-identical to from_hir" is a **tautology by construction** — `from_pattern` literally routes
through `from_hir`, so the two NFAs are the same object. Do not report it as a passing oracle; the
meaningful evidence is the canonicalization square in §4.

**Reverse functor G (engine → bbnf) DOES NOT EXIST — this is the family's one real gap.**
`rg 'impl.*Display for Hir|fn to_pattern'` over `parse-that/rust/regex/src` returns **zero**. There
is no `Hir → String`. The settled ground's "cheap `string`/`format!` bridge reusing `simplify_hir`"
therefore has no reverse leg today: you can canonicalize a `Hir` with `simplify_hir`
(`egraph/mod.rs:143`) but cannot write the canonical form back into a grammar-tier `Regex(String)`.
What *does* exist is a **classify-level fragment** of G: `RegexClass::canonical_pattern()`
(`classify.rs:132`) emits a canonical pattern string for parameterized variants "to avoid hardcoding
pattern literals" — but it returns `None` for `CharClassQuantified`/`PrefixThenClass`/
`AccelDriven`/`Unknown` (the data-carrying variants), so it is partial and class-granular, not
Hir-granular. **The prototype obligation (§7) is to build `Display for Hir` and prove
`parse ∘ display ≡_simplify id`.**

---

## 2. The confinement boundary (Q2) — byte-expansion lives strictly in automata

`[α-ω]*` byte-expansion never reaches the shared type. The narrowing is physical: `CharClass`
keeps codepoint/byte ranges + a `negated` bit (`hir/mod.rs:45`); the complement and the UTF-8
byte-sequence expansion are computed **only** at NFA construction — `to_positive_byte_ranges`
(`hir/mod.rs:74`, `negate_byte_ranges` `:107`) and `to_positive_codepoint_ranges` → `Utf8Sequences`
(`nfa.rs:12`, consumed inside `build_hir`). `to_positive_byte_ranges` even *panics* on a `Unicode`
class (`hir/mod.rs:82`) precisely to forbid byte-expansion outside the UTF-8 path. So Q2 is
unambiguous: **byte-expansion is confined to `automata/`; it does not leak into `Hir`, and it cannot
leak into the grammar tier because the grammar tier never sees `Hir` at all** — it sees a `String`.
Typing `Class`/`Look` through LOCK-1's HM is therefore a *non-question for the shared type*: the
shared type (`Regex(StringId)`) has one HM shape — an opaque scanner leaf whose projected type is
decided by `classify`/`RegexInfo`, not by row-poly over byte ranges. Widths and class cardinality
stay engine-interior facts surfaced through `RegexInfo` (`info/mod.rs`), never unification variables.
This boundary is what makes the two-type design lawful rather than a leak.

---

## 3. The shared part IS the e-graph crate — and the two rule sets are already near-mirror

5B's load-bearing claim ("the e-graph rewrite library is the shared part") is **already realized in
code, not aspirational.** Both tiers instantiate the *same* external crates — `egraph`,
`egraph-derive`, `csp-solver` (path-patched to `bbnf-lang/crates/{egraph,egraph-derive}` and
`csc411/…/csp-solver`, R3 §1). Concretely:

- Engine tier: `parse-that/rust/regex/src/egraph/mod.rs:48` — `use egraph::{CspScheduler, EGraph, Id,
  NoAnalysis, RewriteFn, Scheduler}`; `HirENode` derives `Language` via `egraph_derive::Language`
  (`egraph/mod.rs:9`); `HirEGraph = EGraph<HirENode, NoAnalysis>` (`:56`). Its header states it
  "Mirrors the grammar-tier e-graph in `bbnf-ir`" and `saturate_hir_egraph` "Mirrors
  `bbnf_ir::egraph::build_and_saturate` exactly — same scheduler, same iteration cap, same growth
  limit" (`egraph/mod.rs:82`).
- Grammar tier: `bbnf-lang/crates/ir/src/egraph/` — `GrammarENode` over the same `EGraph`, same
  `CspScheduler`, `default_rules()` at `crates/ir/src/egraph/rules/mod.rs:74`.

**Rule-set census (Q10 substrate).** The two default rule vectors are structurally parallel — this
IS the functor's shared spine:

| Engine (`egraph/rules/mod.rs:64` `default_hir_rules`) | Grammar (`crates/ir/src/egraph/rules/regex.rs`) |
|---|---|
| `FlattenAltConcat` | (absorbed into Fuse/normal-form) |
| `DeduplicateAlternation` | `DeduplicateAltBranches` (`regex.rs:49`) |
| `SupersetAbsorbClass` | `SupersetAbsorbAlt` (`regex.rs:109`) |
| `UnionMergeClass` | `UnionMergeAlt` (`regex.rs:194`) |
| `AbsorbRepetition` | `FuseAltRegexBranches` (`regex.rs:229`) |

Four of five rules are name-for-name mirrors over the Alternation family; the fifth pairs
engine-`AbsorbRepetition` against grammar-`FuseAltRegexBranches`. This near-isomorphism is the
strongest available evidence for 5B: the rewrite algebra is genuinely one library instantiated
twice, not two engines that happen to rhyme. **The census is RUNNABLE, not hypothetical** — both
tiers already print per-rule fire counts under an env flag: engine `BBNF_HIR_EGRAPH_REPORT=1`
(`egraph/mod.rs:95`, emits `rule={} work={}` per `report.per_rule`), grammar `BBNF_EGRAPH_REPORT=1`.
Q10 reduces to: run `cargo bench -p bbnf-regex --bench regex_hir_egraph` and the grammar
backend-egraph pass (`skinny/crates/passes/src/backend_egraph.rs`) over the ≥50-pattern corpus with
the flags set, and tabulate `per_rule.work`.

---

## 4. The round-trip oracles 5B must stand on (the two squares)

5B's charter is "tested by round-trip oracles." There are exactly two; only the second is
non-trivial today:

1. **Forward preservation (language square).** `L(from_hir(parse(p))) == L(from_hir(simplify_hir(parse(p))))`
   — canonicalization must not change the recognized language. `simplify_hir` (`egraph/mod.rs:143`) is
   already the sole canonicalizer feeding `RegexInfo::analyze_from_hir`; the oracle drives both HIRs to
   NFAs and checks match-set equality over a corpus. The `needs_saturation` fast path
   (`egraph/mod.rs:193`) means leaf/literal/class-only patterns are `clone`d unchanged, so the square
   is trivially exact on that majority; the load falls on Alternation/Repetition patterns.

2. **Reverse round-trip (the unbuilt one).** `parse(display(h)) ≡_simplify h` — requires §1's missing
   `Display for Hir`. This is the oracle that certifies the `format!` bridge is *lawful* rather than
   lossy. It must hold up to `simplify_hir` canonicalization (not byte-identity of the string), because
   `Display` will not reproduce incidental grouping/ordering.

**A concrete non-isomorphism the functor must normalize (named counterexample).** The two HIRs
disagree on `Optional`. Grammar tier has BOTH `ExprKind::Optional(ExprId)` (`ir/lib.rs:245`) and
`ExprKind::Repeat { min, max }` (`:240`) as distinct nodes. Engine tier has **no** `Optional`
variant — `?` is `Repetition { min:0, max:Some(1) }` (`hir/mod.rs:194`, enum `:216`). And engine
`Group(Box<Hir>)` is documented "treated as transparent" (`hir/mod.rs:227`) whereas the grammar tier
has no grouping node. So F must fold `Optional(x) → Repetition{0,1}` and G's `Display` must decide
whether to re-emit `x?` or `x{0,1}` — the round-trip square only closes if `simplify_hir` normalizes
both to one canonical form. This is precisely the Q3 "Optional↔Repeat{0,1}" and "Group-elision"
parity note, and it is the load-bearing case the prototype must exercise, not the tautological
from_pattern≡from_hir identity.

---

## 5. Q11 — the `->` representation and the bridge's projection soundness

The grammar-tier regex leaf is a *scanner leaf with no inner structure the projection can see*. A
`->` on a regex leaf attaches at the `ExprKind::Project { body, target, ty }` node
(`ir/lib.rs:261`) wrapping the `Regex`, lowering to `BackendExpr::ValueProject { body, target }`
(`ir/lib.rs:407`) / `BackendExpr::RegexProgram { pattern, span_kind }` (`:389`) — `span_kind` is
where the capture intent lands. Because the grammar tier never opens the pattern, **the `->` cannot
bind an inner sub-class** (there is no `[a-z]` node to project onto); it can only bind the whole
matched span. That is the bridge's soundness contract: the functor may canonicalize the *pattern*
freely (via `simplify_hir`) so long as the **matched byte span is preserved**, because that span is
the only thing the `Map`/`Project` barrier consumes. LOCK-1 must ratify one bit: is `->` on a
scanner leaf a `Map` barrier over the whole span (the safe reading, consistent with the opaque type)
or a typed edge that could reach inside? The code today supports only the former — there is no
inner-class projection surface — so the bridge is sound *iff* LOCK-1 confirms `->` on `Regex` is a
whole-span `Map`. Binding; must be confirmed, not assumed.

---

## 6. Prior art (citations)

- **rust-lang/regex + regex-automata** (BurntSushi): the `Hir`+`translate` split, the `Look`
  taxonomy (Start/End/line variants), and `negated`-class retention are the direct model; the engine
  HIR is a deliberate near-port (`hir/mod.rs:2` "Replaces `regex_syntax::hir::*`"). The `NoAnalysis`
  e-graph choice mirrors regex-automata keeping class-set algebra *outside* the parse tree.
- **egg / egglog** (Willsey et al., POPL'21): equality saturation + cost extraction is the shared
  substrate; `HirENode`/`GrammarENode` deriving one `Language` and sharing `CspScheduler` is the
  "one e-graph, two languages" pattern egg's `Language` trait enables. The `BackoffScheduler`
  → `CspScheduler` swap is the campaign's generalization.
- **MLIR dialects + conversion passes**: "two IRs, total lowering each way, verified by round-trip"
  is exactly MLIR's dialect-conversion legality/round-trip discipline. 5B is a two-dialect instance;
  the `Display for Hir` gap is MLIR's "no printer for this op" defect.
- **GHC Core Lint**: the language-preservation oracle (§4 square 1) is Core Lint's invariant — a
  lowering that changes the denotation is a compiler bug, checkable mechanically.

---

## 7. What a prototype MUST test to move family 5B

1. **Build `Display for Hir`** (the missing reverse-functor leg): all eight variants incl.
   `CharClass` negation-aware rendering and `Look` → `^`/`$`/line-anchors. ~80 LOC colocated in
   `parse-that/rust/regex/src/hir/`. This converts the settled `format!` bridge from prose into a
   checkable object.
2. **Reverse round-trip oracle**: over the ≥50-pattern corpus (tag acyclic/cyclic per Q5's manifest),
   assert `simplify_hir(parse(display(h))) == simplify_hir(h)` — GREEN on 100% or each failure names an
   un-normalized non-isomorphism (start with `Optional`/`Group`, §4).
3. **Forward language square**: `L(from_hir(h)) == L(from_hir(simplify_hir(h)))` by NFA match-set
   equality over the corpus — proves canonicalization is language-preserving (the bridge is *lawful*).
4. **The Q10 fire-count census, actually run**: `BBNF_HIR_EGRAPH_REPORT=1 cargo bench -p bbnf-regex
   --bench regex_hir_egraph` and the grammar-tier `BBNF_EGRAPH_REPORT=1` backend-egraph run over the
   same corpus; tabulate `per_rule.work` for both tiers and confirm the four mirror rules fire on the
   *same* branches (bare and `Project`-wrapped). A rule firing zero times over real branches is
   consumer-less (Constitution viii) — retire or justify it.
5. **LOCK-1 ratification of §5**: `->` on `Regex` is a whole-span `Map` — in writing; the bridge's
   projection soundness is contingent on it.

## 8. Named risks / counterexamples

- **The `format!` bridge is lossy on canonical order.** `Display` after `simplify_hir` may reorder
  Alternation branches (dedup/union rules commute them); if a grammar-tier consumer depends on branch
  order (`AltMode::Dispatch`, `ir/lib.rs:275`), re-embedding the canonical string changes dispatch.
  Mitigation: the reverse oracle compares *languages*, and G only fires for `AltMode::Speculative`
  leaves or re-runs dispatch analysis after re-embed.
- **Skinny vs full IR skew.** Two bbnf IR crates exist: `skinny/crates/ir` (lean,
  `Regex{pattern:String}`, no e-graph) and `crates/ir` (full, `egraph/rules/regex.rs`). The functor's
  grammar-tier home is ambiguous until the gestalt collapses them; 5B's shared-crate claim holds only
  for the full `crates/ir`. Name this as a supersession the census must resolve before it runs against
  skinny.
- **`canonical_pattern()` is a rival reverse map.** Building `Display for Hir` while
  `RegexClass::canonical_pattern()` (`classify.rs:132`) also emits pattern strings risks two divergent
  engine→string paths. Reconcile: `canonical_pattern` should be *derived from* `Display` (class →
  representative Hir → display), not hand-rolled, or the bridge has two truths.
- **The from_pattern≡from_hir tautology masquerading as an oracle.** `nfa.rs:63` makes Q3's
  byte-identity true by construction; it proves nothing about the functor. Real evidence is squares 1–2.

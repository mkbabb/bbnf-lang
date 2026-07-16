# LOCK-7 pass-2 — Family 7B: TOTAL-SELF-HOST

The route: bbnf grammar, regex grammar, IR grammar, and emitter templates all grammar-specified;
the generator collapses toward data; hand code shrinks to kernels and primitives. This report
develops that thesis to its strongest concrete form, measures its two hard edges (the IR-plane
homoiousia and the structure-scoping escape), and names the exact prototype that moves it.

---

## 1. The substrate, made concrete

### 1.1 The IR is already a pure algebraic value

`GrammarIr` (`skinny/crates/ir/src/lib.rs:34`) is `{ name, source_hash, rules: Vec<Rule>, exprs:
Vec<Expr> }` — an arena of `Expr { id, kind, span }`. `ExprKind` (`ir/src/lib.rs:234-271`) is a
flat 10-variant sum: `Seq(Vec<ExprId>)`, `Alt { branches, mode }`, `Repeat { body, min, max }`,
`Optional`, `Literal { bytes, case }`, `Regex { pattern }`, `Ref { name, target }`, `Annotation`,
`Project { body, target, ty }`, `Discard { left, right, keep_left }`. Every variant is a
sum-of-products over `ExprId`/scalars — the shape a bbnf `->` projection denotes. The census claim
("node values are pure `->`") is TRUE at the value level: `ir.bbnf` can express the GrammarIr ADT
with `-> Seq`, `-> Alt`, `-> Project`, etc., because `ExprId` is a `usize` newtype (`ir/src/
lib.rs:19`) an index projection reaches. This is the homoiousia beachhead.

### 1.2 But construction is host-bound, and one host fn is an inherited-attribute pass

Two build mechanisms live outside the value plane:

- `add_expr(kind, span) -> ExprId` (`ir/src/lib.rs:51`) — append into the arena, hand back the
  index. Pure append; trivially a typed host fn (`push` semantics).
- `resolve_refs(&mut self)` (`ir/src/lib.rs:84`) — builds a `HashMap<name, RuleId>` over ALL rules
  first (`:86-93`), then walks every `Expr` mutating `ExprKind::Ref { target }` in place
  (`:95-105`). This is an INHERITED-ATTRIBUTE cross-rule link: no `Ref` resolves until the whole
  rule table exists. `validate` (`ir/src/lib.rs:110`) + `validate_resolved_refs` (`:300`) are the
  paired well-formedness pass. These are the "host floor" the census names, and the floor's tier
  depends on the LOCK-1 inherited-attribute verdict — `resolve_refs` cannot be a leaf `->`; it is a
  global fold over the arena.

So the honest decomposition of `ir.bbnf`: the grammar plane covers the SHAPE (the ADT, node
values), the host floor covers the CONSTRUCTION (`add_expr` append + `resolve_refs` inherited link
+ `validate`). TOTAL-SELF-HOST does not erase the floor; it shrinks it to those named fns.

---

## 2. Scope axis — the joint LOCK-3⊗LOCK-7 IR-plane, and its named consumer

**If LOCK-3 adopts grammar-homoiousia (its 3C family):** the joint family is `ir.bbnf` as a grammar
whose parse target IS `GrammarIr` — the generator eats its own IR description. The transposition
tower (`passes::compile` → `lower::lower_to_rust` → the emitters) becomes a grammar-to-grammar map.
**`ir.bbnf`'s consumer is `codegen/src/lib.rs:101 emit_from_source`** — specifically
`passes::compile` (`lib.rs:103`) reading a `GrammarIr` and `lower::lower_to_rust` (`lib.rs:176`)
lowering it. That is a live, in-wave consumer (Constitution viii / Y.13) ONLY under the condition
stated next.

**The category mismatch that decides the family (the load-bearing finding).** Routing any grammar —
including `ir.bbnf` — through the extant emitter does NOT produce a `GrammarIr`. The general descent
emitter (`codegen/src/general_descent/emit.rs`) emits ONE `parse_<rule>` **recognizer** per rule
(`emit.rs:260-280`): signature `fn parse_rule(bytes, cursor: &mut usize, tape: &mut TapeBuilder) ->
Result<(), ParseError>`, writing offset marks onto a `Tape`. Its `-> Span`/`-> T` projections are
consumed by the lazy VIEW (`emit.rs:376-380` — "the projection is consumed by the VIEW, not the
recognizer"; `view.rs:112-118` emits `.span()`/`.as_f64()`/`.as_tag()` accessors). `grep` confirms
ZERO `add_expr`/`GrammarIr`/`resolve_refs` emission anywhere in `general_descent/` or
`json_sink_direct/`. So a self-hosted `ir.bbnf` parser built this way produces an IR-DOCUMENT
tape+view — NOT a live `GrammarIr` the tower can consume.

**Therefore `ir.bbnf`'s consumer must be a `build_grammar_ir(view) -> GrammarIr` host builder** that
walks the emitted view and calls `add_expr` + `resolve_refs`. That builder IS the census's
host-floor budget, made the joint family's central obligation. **If LOCK-3 declines the homoiousia
route, what remains is exactly this:** the IR ADT stays `->`-expressible as inert data, but its
construction and cross-rule linking stay hand-coded host — the grammar plane describes the IR's
*shape*, never its *building*. The family is non-vacuous iff LOCK-3 accepts the typed host-builder
floor (append + inherited link) as the sanctioned consumer; otherwise `ir.bbnf` is consumer-less
substrate.

---

## 3. Structure-scoping — the data-vs-escape ratio and the named kernel-escape signature

Measured on the one emitter blob the directive names: `general_descent/emit.rs` (452 LOC).

**Data (grammar-plane-expressible templates).** `render_header` + `render_prelude` (`emit.rs:28-
257`, ~230 LOC) is a FIXED string scaffold — the `ParseError` type, `skip_ws`, `match_literal`,
`match_leaf`, `consume_quoted`. Grammar-neutral; 100% templatable. The `writeln!` payloads inside
`emit_expr` (the emitted Rust text for `CallRule`, `ByteLiteral`, `RegexProgram` — `emit.rs:357-
375`) are pure text templates keyed on `SinkOnlyExpr` shape. Roughly **~90% of the emitter body is
data**: a shape→template dispatch that a grammar-plane emitter-template language (the route's
"emitter templates all grammar-specified") could denote directly.

**Escape (the irreducible hand-kernel).** ~40 LOC — the speculative-backtracking scaffolding for
`Alt`/`Optional`/`RepeatLoop`:

- `emit.rs:321,348,415` — the IIFE probe: `let _r: Result<(),ParseError> = (|| { ... })();` then
  match to commit/rollback.
- `emit.rs:450` — `let cursor = &mut _probe;` — the re-alias that lets the speculative walk write a
  SHADOW cursor so a failed branch leaves `*cursor` untouched.
- `emit.rs:329` — `if _probe == _before { break; }` — the nullable-loop progress guard.

**The NAMED kernel-escape failure signature.** The escape is the **linear `&mut TapeBuilder` + `&mut
usize` cursor threaded through every recognizer, plus the `&mut _probe` speculative re-alias**. This
has no grammar-plane denotation because it is a BORROW/LINEAR-TYPE discipline, not a value: the tape
is a single mutable resource written in-order, and speculative choice requires save/restore of a
mutable cursor across a closure boundary. Concretely — remove the `let cursor = &mut _probe`
re-alias at `emit.rs:450` and the generated code fails borrow-check with **E0499 ("cannot borrow
`*cursor` as mutable more than once at a time") / E0503 ("cannot use `*cursor` because it was
mutably borrowed")** across the Alt/Optional/Repeat probe closures. That E0499/E0503 signature is
the wall: the exact point at which "codegen control-flow as grammar-plane data" cannot proceed
without a hand kernel. **TOTAL-SELF-HOST therefore has a measured floor of ~10% of the emitter — the
tape-mutation + backtracking-cursor kernel — that stays hand-written.** The route is real but
bounded; the honest headline is "generator becomes ~90% data, ~10% irreducible linear-resource
kernel," not "generator becomes data."

---

## 4. The shared empirical question — does the builder-augment round-trip, or cascade?

`bbnf.bbnf` (`grammar/bbnf/bbnf.bbnf`) ALREADY carries `-> Span` on its structural rules
(`identifier:9`, `literal:13`, `regex:15`, `big_comment:17`, `comment:18`) and has NO `value` rule —
so it shape-dispatches through `runtime_generator.rs:98 !has_rule("value")` to
`emit_general_descent`, never `json_sink_direct`. Driving that branch surfaces three bounded parser
gaps and one structural verdict.

**The three cascade points (parser gaps, at path:line):**

1. **`?w`→`->` attachment.** `parse_seq` (`grammar/src/lib.rs:1361`) consumes a standalone `?w` as
   an `Annotation{name:"ws"}` sequence element (`:1367-1382`); `parse_postfix` (`:1404`) attaches
   `->` via `parse_projection` (`:1481`) only immediately after an atom. bbnf.bbnf's `big_comment =
   ( ... ) ?w -> Span` (line 17) puts `?w` BETWEEN the group and the `->`, breaking the postfix
   chain — `-> Span` never reaches the `(...) ?w` group. First cascade.
2. **`@{}` atom.** `term = ... | "@{" , rhs ?w , "}"` (bbnf.bbnf:33). The skinny front lexes `@{` at
   `grammar/src/lib.rs:420` as a directive-level `RuntimeHostCapture` (unsupported code
   `BBNF-UNSUPPORTED-HOST-CAPTURE`, `:222`), NOT as a term-level atom in `parse_atom` (`:1495`).
   `@{...}` inside a `term` cascades.
3. **`|params|` closures.** `closure = "|", identifier, (...) "|" ?w, rhs` (bbnf.bbnf:51);
   `rhs = closure | alternation` (:52). No closure atom exists in `parse_atom`; `|` is only the Alt
   separator (`parse_seq` breaks on `Some('|')`, `:1387`), so a leading `|` at rhs-start misparses
   as an empty alternative. Third cascade.

**The structural verdict (deeper than any construct).** Even with all three gaps closed, the
emitter's output CATEGORY is a Document tape+view (§2), not a `GrammarIr`. "Reparse the corpus into
GrammarIr-equivalent structure" therefore does NOT happen through the recognizer emitter alone: it
happens through recognizer+view PLUS a `build_grammar_ir` host builder invoking `add_expr` +
`resolve_refs`. The cascade is not "the parser breaks at construct X" — it is "the emitter round-
trips bbnf source into a tape/view; GrammarIr-equivalence is a SEPARATE host-builder obligation
inside the `add_expr`+`resolve_refs` budget." That is the answer to reparse: it round-trips to the
wrong category for free, and to the right category only at the host-builder's cost.

**On the fixpoint subject.** `regen∘regen` byte-identity is near-VACUOUS here: `emit_general_descent`
is deterministic string templating over a `SinkOnlyProgram` derived deterministically from
`GrammarIr`, so identical bytes certify only that the template is a function — a fixed emit path
stamps identical bytes for any stable front-parse. The NON-VACUOUS fixpoint subject is **GrammarIr
equivalence**: the self-hosted parser's `build_grammar_ir(bbnf.bbnf)` must equal the hand parser's
`parse_grammar(bbnf.bbnf)` as a `GrammarIr` (modulo the equivalence relation 7D pins — note
`parse_seq:1394` already normalizes `Seq([])` for the empty case and `:1400` collapses single-child
`Seq([x]) → x`, so `Empty ≡ Seq([])` and `Seq([x]) ≡ x` are structural givens the relation inherits).
Quote parity% only against that relation, never against emitted bytes.

---

## 5. Prior art (cited)

- **pest / `pest_meta`** — the closest analogue: pest's own grammar is `meta/grammar.pest`, and
  `pest_meta` ships BOTH a bootstrapped self-parser and a hand parser used differentially; the
  bootstrap regenerates the meta-parser. Exactly 7B's "bbnf-in-bbnf + hand parser as oracle" (feeds
  7D). github.com/pest-parser/pest — `pest_meta`, `pest_bootstrap`.
- **LALRPOP / Menhir** — LALRPOP parses its own `.lalrpop` grammar with a LALRPOP-generated parser
  plus a hand-written bootstrap fallback (the committed stage-0); Menhir self-describes its grammar.
  The staged-bootstrap discipline (committed stage-0, fixpoint) is settled art there.
- **MLIR ODS / TableGen** — the IR-as-declarative-data model: MLIR ops/types are specified in
  TableGen (ODS) and `mlir-tblgen` generates the C++ IR classes. `ir.bbnf` is the ODS analogue; the
  lesson is that the DATA describes op/type SHAPE while builders/verifiers stay hand-written C++ —
  precisely §2's "grammar covers shape, host covers construction (build + verify)." Direct support
  that the homoiousia IR-plane's floor is builders+verifiers, not the ADT.
- **egg/egglog `#[derive(Language)]`** — an enum with recursive `Id` fields is mechanically a
  language (memory: `derive-language-macro`). Confirms `ExprKind`'s `ExprId`-recursion is trivially
  projectable to a grammar/e-node description; the ADT side of `ir.bbnf` is cheap.
- **GHC (happy/alex bootstrapped), tree-sitter (grammar.js compiled, not self-hosted)** — the
  spectrum: self-hosting a grammar DSL is standard; self-hosting the EMITTER and IR is the frontier
  7B claims. None self-host the code emitter — the §3 ~10% linear-resource kernel is why.

---

## 6. Named risks and counterexamples

- **R1 — recognizer/builder category mismatch (the family-deciding risk).** The emitter emits
  recognizers+views, so `ir.bbnf`/`bbnf.bbnf` self-parse to a Document tape, not a `GrammarIr`.
  Consumer-less-substrate breach unless a `build_grammar_ir(view)->GrammarIr` host builder is the
  named consumer. Counterexample to guard: an `ir.bbnf` that regenerates byte-identically but has no
  builder reading it back is DEAD substrate passing a vacuous fixpoint.
- **R2 — the borrow-check escape is irreducible.** The `&mut` tape/cursor + `&mut _probe` re-alias
  (§3) has no grammar denotation; the E0499/E0503 signature is the hard floor. TOTAL-SELF-HOST
  cannot reach 100%; the honest ceiling is ~90% data. A route claiming full self-host of the emitter
  is refuted by this measured kernel.
- **R3 — `resolve_refs` is inherited-attribute-bound to LOCK-1.** If LOCK-1 declines inherited
  attributes, `resolve_refs` stays a hand host fn and the homoiousia IR-plane's construction floor
  does not shrink. The family cannot certify its host budget until LOCK-1's attribute verdict lands.
- **R4 — vacuous byte-determinism.** `regen∘regen` byte-identity on a fixed-const template path
  certifies nothing (§4). Any parity% must be sourced from a GrammarIr equivalence relation (7D's,
  with `Seq([x])≡x`, `Empty≡Seq([])`), not from emitted bytes.

---

## 7. What a prototype must test (to move family 7B)

1. **Close the three gaps and drive the branch.** Extend `parse_seq`/`parse_postfix` for `?w`→`->`
   attachment, `parse_atom` for the `@{}` term-atom and the `|params|` closure atom; route
   `bbnf.bbnf` through `emit_general_descent` (`runtime_generator.rs:99`), NEVER `json_sink_direct`.
   Gate: does it emit a `parse_grammar`/view without cascade?
2. **Author the builder consumer.** Write `build_grammar_ir(BbnfDocument) -> GrammarIr` over the
   emitted view, using `add_expr` (append) + `resolve_refs` (inherited link). Measure its LOC
   against the census budget — does a *building* seed inflate toward the 2142-LOC hand parser
   (`grammar` crate, R1 §1), or stay a thin fold?
3. **The non-vacuous fixpoint.** Assert `build_grammar_ir(self_parse(G)) ≡ parse_grammar(G)` as
   GrammarIr (under the 7D relation) for G ∈ {json.bbnf, css/l4/*.bbnf, sheets, csv.bbnf, bbnf.bbnf
   itself}. This is the real self-host proof; byte-identity of `regen∘regen` is reported only as a
   secondary, explicitly-labelled-vacuous check.
4. **Confirm the escape signature.** Delete the `let cursor = &mut _probe` re-alias at `emit.rs:450`;
   observe the E0499/E0503 borrow-check failure — pinning the named kernel-escape and the measured
   data-vs-escape ratio (~90/10) on emit.rs.

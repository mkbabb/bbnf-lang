# LOCK-7 · Family 7A — STAGED-BOOTSTRAP (pass 2)

The classic two-stage formulation carried to its strongest form on the greenfield skinny tree:
a small committed stage-0 artifact, a fixpoint gate on a chosen equivalence subject, minimal
bootstrap surface, maximal auditability. This report makes the mechanism concrete, re-derives
the headline numbers on the measurement substrate, chooses the fixpoint subject and defends its
non-vacuity, costs the legacy gate as rescue-vs-re-author, and names the born-RED gate and the
prototype that moves the family.

---

## 1. The mechanism made concrete

A staged bootstrap has three physical parts on skinny HEAD:

1. **The seed grammar** — `grammar/bbnf/bbnf.bbnf` (90 lines). It is already self-describing:
   rules `identifier`/`literal`/`regex`/`term`/`factor`/`mapped_factor`/`binary_factor`/
   `concatenation`/`alternation`/`closure`/`rhs`/`rule`/the seven `@`-directives/`grammar`
   (bbnf.bbnf:9-85). Leaf semantics are already `-> Span` projections (`identifier`,`literal`,
   `regex`,`big_comment`,`comment`), and `mapped_factor` names `value_expr : type` for the `->`
   map plane (bbnf.bbnf:9-42). It `@import`s `expressions`/`types` modules (bbnf.bbnf:4-5) —
   so the seed is not one file but a small `@import` closure.

2. **The generator** — the ONE shape-dispatched descent emitter, entered at
   `codegen/src/lib.rs:101 emit_from_source` → `runtime_generator.rs:98` `!has_rule("value")` →
   `general_descent::render` (mod.rs:38). Because `bbnf.bbnf` has no `value` rule it lands on the
   general branch by shape, not by name. That branch emits one `parse_<rule>` **recognizer** per
   rule, signature `fn parse_{ident}<'i>(bytes,&mut cursor,&mut TapeBuilder) -> Result<(),
   ParseError>` (emit.rs:269), plus a `parse_into_tape` entry returning a lazy `{Prefix}Document`
   over the source (emit.rs:105). ~1206 hand LOC total (`mod.rs` 93 + `emit.rs` 452 + `view.rs`
   431 + `regex_first.rs` 230).

3. **The committed stage-0** — a generated `crates/runtime/src/grammars/bbnf/{generated,parser,
   view,config,mod}.rs`, produced by a `regen-bbnf` xtask target (**does not exist yet** — see
   §3) mirroring the three live `regen-{json,css,sheets}` arms (`xtask/src/main.rs:39-52`).
   `write_targets` emits it; `check_target` (`xtask/src/regen.rs:36`) diffs it against the tree.

The staged trade against 7B (total self-host): the seed here **recognizes** bbnf and tape-marks
its structure — it does not yet **build** a `GrammarIr`. A staged bootstrap can therefore ship
with the hand parser (`grammar/src/lib.rs`, a single 2142-LOC file) remaining the production
front-end and the generated bbnf parser serving only as a differential oracle (the 7D pivot).
Total self-host requires the generated parser to *replace* those 2142 LOC — which requires
builder projections over the recognizers, the open question of §6.

---

## 2. The `seed_LOC` headline, re-derived on skinny

The honest seed is not "90 lines of grammar." A *staged* seed is the committed stage-0 **bytes**
plus the grammar plus the irreducible hand kernels the generated parser calls (`TapeBuilder`,
`skip_ws`, `is_text_byte`, the `LEAF_STOP` delimiter scan — emit.rs:100-259). The 90-line
grammar is the authored surface; the committed stage-0 `.rs` is what an auditor must trust
(Thompson's trusting-trust surface; cf. the Bootstrappable-Builds / GNU Mes principle that the
seed you *commit* is the seed you *audit*, not the source you *wrote*).

**The inflation risk is real and directional.** A *recognizer* seed is cheap — it emits ~1206
LOC of generator against a 90-line grammar and the committed stage-0 is a lazy-tape shell. A
*building* seed (one that reconstructs `GrammarIr`) must emit, per rule, the `add_expr` calls
that the hand parser writes by hand (`ir/src/lib.rs:51`), the `Ref{name,target}` construction
(`grammar/src/lib.rs:1524`), and the projection scan (`parse_projection`:1481). That generated
builder body plausibly approaches the hand parser's own LOC — i.e. a building seed inflates
*toward* the 2142-LOC hand parser it means to retire, because it is emitting the same
construction logic as data-driven code. **This is the number a prototype must produce** (§7):
`generated_builder_LOC / hand_parser_LOC`. If that ratio is ≥ ~0.8 the staged win is only
auditability, not code reduction; if it is ≤ ~0.4 the building seed is a genuine shrink.

**Prior-art anchor for the staged shape:** rustc's `stage0` (a committed/downloaded prior
compiler) → `stage1` → `stage2`, with the compiler's *own* bit-identical-stage check as the
fixpoint; tree-sitter's `grammar.js` → committed `src/parser.c` (the exact "grammar authored,
generated parser committed, regenerated in CI" model this family instantiates); GHC's staged
bootstrap. All three keep a committed generated artifact and gate on re-derivation — none claim
the *authored* grammar is the whole seed.

---

## 3. The `~88s → 0.5s` datum — VERIFIED a legacy transplant

`grep -riE "regen-bbnf|fixpoint|88|regen.regen|self.?host"` over `skinny/xtask/src` and
`skinny/crates/codegen/src` returns **nothing** on the bootstrap axis (only incidental
`byte-identical` comments at `runtime_generator.rs:48`, `lib.rs:334`, `json_sink_direct/
mod.rs:207`). There is **no `regen-bbnf` target, no fixpoint loop, and no 88s/0.5s datum on
skinny HEAD.** The concept lives only in the legacy outer tree (`crates/core/src/runtime/bbnf/`
— a full builder front-end with `arena.rs`/`builder.rs`/`document.rs`, GN-0 archive material).
The datum must be treated as **legacy provenance, not a skinny measurement**; any doc quoting it
against the greenfield tree is a supersession violation (Constitution ix). It may only be
re-earned by building the `regen-bbnf` target and timing it on skinny@HEAD under the honest
profile.

---

## 4. The fixpoint SUBJECT — choose GrammarIr equivalence, not `.rs` bytes

Two candidate subjects for the gate `stage2 ≡ stage1`:

- **(a) generated `.rs` bytes** — `regen∘regen` byte-identical.
- **(b) GrammarIr equivalence** — the generated parser, run on `bbnf.bbnf`, yields a `GrammarIr`
  equal (mod the §5 relation) to the hand parser's.

**Subject (a) is largely vacuous on this generator, and the report must say so.** The skinny
`check_target` is a **single-shot** gate: `emit_runtime_from_request` then `check_dir` diffs
against the committed tree (`regen.rs:36-52`) — it certifies `committed == emit(source)`, i.e.
the determinism of ONE emit, not the two-stage self-application. Worse, the emit path is
**template-dominated**: the JSON family splices a fixed `include_str!("json_templates/
generated.rs")` (`runtime_generator.rs:101`), and CSS splices the constant `CSS_GENERATED_RS`.
A fixed-const emit path stamps identical bytes for *any* structurally-admissible input, so
"regen∘regen is byte-identical" and even "triple-identical" **certify nothing about the parser's
behaviour** — they certify the template is a constant. Byte-determinism is necessary hygiene
(P53, clean-regen), not a self-hosting proof.

**Choose subject (b), with (a) demoted to a hygiene sub-gate.** GrammarIr equivalence is the
non-vacuous fixpoint: it exercises the generated parser *as a parser* over the seed and compares
the *structure it builds* to the hand parser's structure. Its precondition is that the generated
parser can build a `GrammarIr` at all — which returns us to §6 (recognizer-vs-builder). Until
that holds, the strongest *available* staged gate is the 7D differential oracle: generated
recognizer *acceptance* over a corpus vs hand-parser acceptance (a weaker relation — accept/reject
parity, not structural equality — but non-vacuous because the two parsers are independent code).

Non-vacuity test to bank: **plant a divergence** — feed a construct the generated parser handles
differently (e.g. `?w`→`->` attachment, §6) and confirm the gate goes RED. A gate that stays
green under a planted structural divergence is vacuous and must be widened.

---

## 5. The equivalence relation (pin before quoting parity)

Equality of two `GrammarIr`s cannot be raw struct equality: `ExprId`/`RuleId` are positional
indices (`ir/src/lib.rs:51-66`) and node emission order differs between a hand parser and a
generated one. The relation must be **structural up to**: (i) `ExprId` renumbering (compare the
`ExprKind` tree reachable from each rule body, not the flat `exprs` Vec); (ii) `Ref` resolution
(compare `name` before `resolve_refs`, `target` after — run `resolve_refs` on *both* sides or
neither, `ir/src/lib.rs:84`); (iii) the codex identity `Empty ≡ Seq([])` (the hand parser
already collapses an empty child list to `Seq(Vec::new())` at `grammar/src/lib.rs:1393`, so this
normalization is load-bearing, not cosmetic). Pin this relation-id **before** any parity% is
quoted (the 7D obligation; 7A consumes the same relation for its GrammarIr fixpoint subject).

---

## 6. The shared open question — builder projections over the recognizer emitter

This is 7A's pivotal unknown and it is genuinely **RED-unknown** — do not seed it from the
legacy builder or from the emitter's existence.

The general-descent emitter today emits recognizers whose bodies **discard the value**
(`parse_{rule}` returns `Result<(),…>`, emit.rs:269) while writing an *offset tape* keyed to the
grammar's structural bytes. `bbnf.bbnf` routed through it would yield a `BbnfDocument` lazy tape
over the grammar text — **not** a `GrammarIr`. To make the generated parser a *builder* (the
7B/total-self-host precondition, and the non-vacuous fixpoint subject of §4b), the `-> Span`/
`-> T` projections on bbnf's structural rules must reach the tape emitter and reconstruct
`ExprKind`/`Rule` nodes. Three concrete, bounded parser-augment gaps block even *parsing*
`bbnf.bbnf` through the hand front-end, and each is a construct the generated emitter must also
learn:

- **`?w` → `->` attachment.** `parse_postfix` explicitly bails on `?w` and returns the atom
  intact (`grammar/src/lib.rs:1449-1454`), leaving `?w` for `parse_seq`. But `big_comment`/
  `comment` in the seed are `( … ) ?w -> Span` (bbnf.bbnf:17-18) — the `->` must attach across
  the standalone `?w`. Today's `parse_postfix` loop checks `->` first (:1409) then `?w` (:1449),
  so a `) ?w -> Span` sequence needs the projection to bind past the whitespace marker.
- **`@{` atom.** `parse_atom` (`grammar/src/lib.rs:1495-1531`) handles `"`,`/`,`(`, ident-ref —
  **no `@{`** arm. `term` in the seed uses `"@{" , rhs ?w , "}"` (bbnf.bbnf:33) for host-splice
  atoms. The lowering seam for `@{}` is named at `grammar/src/lib.rs:222` (7C's charter).
- **`|params|` closures.** No closure arm in `parse_atom`/`parse_rhs`; `closure = "|" ,
  identifier , … , "|" ?w , rhs` and `rhs = closure | alternation` (bbnf.bbnf:51-52) are
  first-class grammar-function syntax the front-end does not yet admit.

The census the directive carries is confirmed in code: node values are pure `->`; the *build
mechanisms* are host-bound — `add_expr` is a pure append (`ir/src/lib.rs:51`), `resolve_refs` is
a name→RuleId map then a `Ref.target` patch over `exprs` (a bounded TreeFold, `ir/src/lib.rs:84-
108`). So the host floor for a *building* bbnf seed is exactly {`add_expr`, `resolve_refs`} —
small, and `resolve_refs`'s zero-runtime-cost round-trip is independently testable (the 7C/
CONTEXT-tier hook). The open question: authoring those `-> ExprKind`/`-> Rule` projections, does
the emitted parser reparse the seed corpus into GrammarIr-equivalent structure (within the
`add_expr`+`resolve_refs` budget) while the byte-hygiene sub-gate holds — **or does it cascade,
and at which construct** (the `@{}` atom and closure `rhs`-recursion are the prime cascade
suspects, since both re-enter `rhs`)? Drive the `!has_rule("value")` branch only; never
`json_sink_direct`.

---

## 7. Legacy fixpoint-gate: rescue-and-port vs re-author

The legacy builder front-end (`crates/core/src/runtime/bbnf/{arena,builder,document,view,
parse_with,serialize}.rs`, plus tests `bbnf_ast_parity.rs`/`shape_dict_bbnf.rs`) archives at
GN-0. Two routes for its fixpoint discipline:

- **Rescue-and-port.** Lift the legacy parity/fixpoint *test harness* (`bbnf_ast_parity.rs`) into
  a skinny `bbnf-oracle` submodule (the GN-5 fleet crate is the natural home), retargeted at the
  §5 relation. Cost: the harness is test code, not product; the port is a re-home + relation
  swap, not a rewrite. **Risk:** the legacy relation may bake legacy `Arena`/`Document` shapes
  that do not exist on skinny (`GrammarIr` is flat-Vec, not arena) — a silent relation mismatch.
- **Re-author.** Write the fixpoint gate fresh against the §5 relation and the skinny `GrammarIr`.
  Cost: ~1 new xtask subcommand (`check-bbnf-fixpoint`) + the relation impl. Cleaner, no legacy
  shape leakage, and the relation is authored once and shared with 7D.

**Recommendation to test, not to conclude:** re-author the gate (small, shape-correct), rescue
only the legacy *corpus* of adversarial bbnf inputs as fixtures. The legacy 2142-LOC-equivalent
builder itself is NOT rescued — it is superseded by the generated builder if §6 closes, or the
hand parser stays production and the generated parser is oracle-only (7D) if it does not.

**The born-RED gate for the parser-augment:** `regen-bbnf` produces `crates/runtime/src/grammars/
bbnf/` and `check-bbnf` is bit-for-bit (the byte-hygiene sub-gate, §4a); AND
`check-bbnf-fixpoint` asserts the generated parser's GrammarIr over the seed corpus is §5-equal
to the hand parser's (the non-vacuous gate, §4b). Both RED until the three §6 parser gaps close
and the projections build nodes. The gate is proven able-to-fail by the planted-divergence test
(§4).

---

## 8. Named risks & counterexamples

- **R1 — vacuous byte-gate marketed as self-hosting.** The template-splice emit path makes `.rs`
  byte-identity a constant-stamp, not a proof (§4). *Counterexample to guard:* a byte-identical
  `regen∘regen` on a grammar whose emit is a fixed template certifies the template, not the
  parser — the fixpoint MUST be GrammarIr equivalence.
- **R2 — building-seed LOC inflation.** A generated builder may approach the 2142-LOC hand parser
  (§2). *Counterexample:* if `generated_builder_LOC/hand_LOC ≥ 0.8`, the staged route's only win
  is auditability; the LOC-reduction claim is refuted and must be dropped.
- **R3 — cascade at `@{}`/closure recursion.** The `rhs`-recursive `@{}` atom and `|params|`
  closure (bbnf.bbnf:33,51-52) re-enter `rhs`; the recognizer's delimiter-scan leaf (emit.rs
  `LEAF_STOP`, "cannot cross a grammar delimiter") may not survive nested braces. *Counterexample:*
  a bbnf construct with a brace-nested host splice that the generated parser mis-scans while the
  hand parser accepts — the exact "at which construct" the prototype hunts.
- **R4 — legacy datum contamination.** Quoting 88s→0.5s against skinny (§3) is a Constitution-ix
  supersession breach. *Counterexample:* the number is absent from skinny HEAD; any doc citing it
  is transplanting legacy provenance.
- **R5 — relation under-specification.** Parity% quoted before the §5 relation-id is pinned is
  meaningless (positional `ExprId` renumbering, `Empty≡Seq([])`). *Counterexample:* two
  structurally-equal grammars comparing unequal because of `exprs`-Vec ordering.

---

## 9. What a prototype must test (to move 7A)

1. **Build `regen-bbnf`** (one `BBNF_TARGET` const + regen/check arms, `xtask/src/main.rs:39-52`
   pattern) and time it on skinny@HEAD — re-earn or retire the 88s→0.5s legacy datum (§3).
2. **Close the three §6 parser gaps** in `grammar/src/lib.rs` (`?w`→`->`, `@{}` atom, `|params|`
   closure) so `bbnf.bbnf` parses through the hand front-end — the precondition for any oracle.
3. **Author `-> ExprKind`/`-> Rule` builder projections** on bbnf's structural rules and drive
   the `!has_rule("value")` branch; measure whether the emitted parser reparses the seed corpus
   into §5-equivalent `GrammarIr` within the {`add_expr`,`resolve_refs`} budget, or names the
   cascade construct (§6, §8-R3).
4. **Emit the two ratios:** `generated_builder_LOC/hand_parser_LOC` (§2 inflation test) and the
   GrammarIr-parity% over the corpus (§4b), each with its relation-id (§5).
5. **Prove the fixpoint gate can fail:** plant a structural divergence and confirm RED (§4).

The family advances when (1)-(2) land green and (3) returns either a parity% with the cascade
absent, or the named construct where the builder-over-recognizer route breaks.

# LOCK-7 · Family 7B — TOTAL-SELF-HOST (pass-2 research)

**Charter.** Every syntactic plane of the generator carried as *data* — bbnf grammar self-parses,
regex grammar self-parses, the middle IR is grammar-defined, Rust-emission templates are
grammar-specified — with hand code shrunk to kernels/primitives, measured against build-time and
debuggability budgets. Pass-2 sharpens two structural questions (IR-plane scope; codegen-as-data
ratio) and settles the shared empirical question (does a builder-projection augment over the
recognizer emitter round-trip to GrammarIr while `regen∘regen` holds byte-identical).

The pass-2 substrate is exact: the general shape-dispatched descent emitter
(`crates/codegen/src/general_descent/`, ~1206 LOC) emits ONE `parse_<rule>` **recognizer** per
rule — recognizers over an offset tape, **not** GrammarIr builders. The decisive pass-2 finding is
that this archetype gap — recognizer-tape-view vs eager typed builder — is what 7B lives or dies
on, and it is orthogonal to the three bounded parser gaps.

---

## 1. Q1 — Scope axis: does LOCK-3 adopt grammar-homoiousia? The joint IR-plane family

### 1.1 The IR shape splits cleanly into a `->`-expressible data plane and a host-bound build floor

`GrammarIr` (`crates/ir/src/lib.rs:34-39`) is a flat arena — `rules: Vec<Rule>` + `exprs: Vec<Expr>`,
`ExprId`/`RuleId` indices — over a **10-variant sum** `ExprKind` (`ir/src/lib.rs:234-271`):
`Seq(Vec<ExprId>)`, `Alt{branches,mode}`, `Repeat{body,min,max}`, `Optional`, `Literal{bytes,case}`,
`Regex{pattern}`, `Ref{name,target}`, `Annotation`, `Project{body,target,ty}`, `Discard{left,right,
keep_left}`. The mid-IR is a second sum tower: `BackendShape`/`BackendExpr`/`Recognizer`/`SimdMode`/
`SpanKind`/`DirectBuild{Source,TypeRef,Scalar,Presence,Cardinality,Representation,Decode}`
(`ir/src/lib.rs:361-530`).

Every one of these variants is a **labeled product** — precisely the labeled-BNF shape BNFC turns
into an algebraic datatype, and precisely the shape parse-that's `RegexParserEnum` is *derived*
into (`parse-that/rust/parse_that/src/regex/generated.rs`, variants `char_class`/`alternation`/
`quantified` with synthetic `class_atom_0`/`group_0`). So the census datum "node values are pure
`->`" is confirmed structurally: an `ir.bbnf` grammar authors each variant as
`expr_kind = seq | alt | repeat | ... ; seq = "Seq" , "(" , expr_id * , ")" -> Seq : Seq ;` — the
`->` projection names the constructor exactly as the recognizer view already classifies `-> f64` /
`-> Nu8` (`general_descent/view.rs:35-47`).

But the **build mechanisms** are NOT `->`-expressible; they are the irreducible host floor:

- `add_expr` (`ir/src/lib.rs:51-55`) — arena append returning a fresh `ExprId`. A *mutation with
  identity allocation*, not a value projection. A `->` names a shape; it cannot mint an index into
  a growing `Vec` and hand back the handle.
- `resolve_refs` (`ir/src/lib.rs:84-108`) — the cross-rule TreeFold: build a `name→RuleId` HashMap
  over all rules, then a `&mut self.exprs` back-patch pass that fills `Ref{target: Some(rule_id)}`.
  This is an **inherited/synthesized attribute over the whole tree** (a name environment threaded
  down, a target id synthesized up) — an attribute-grammar propagator, not a local projection.

### 1.2 The joint LOCK-3⊗LOCK-7 IR-plane family (drafted)

**Data plane (7B owns, `->`-expressible, LOCK-3C aligned).** `grammar/ir/ir.bbnf` grammar-defines
`ExprKind` + the `BackendShape` tower as a labeled sum; the variant structs are *derived* (the
`RegexParserEnum` mechanism generalized). Transpositions AST→HIR→mid-IR→BackendShape become
grammar-to-grammar maps expressed as `->` projections over `ir.bbnf`-derived nodes.

**Build floor (typed host functions, hybrid-grammar-host law).** `add_expr` and `resolve_refs` are
declared **typed host-fn signatures** — `add_expr : ExprKind -> ExprId` (an allocator primitive)
and `resolve_refs : Grammar -> Grammar` (a cross-rule attribute pass). This is the exact
`@host`-directive hybrid the bbnf grammar already reserves (`bbnf.bbnf:73` `host_directive =
"@host" ?w , identifier , ( ":" , type_name ) ?`). Whether `resolve_refs` is *scheduled inside the
solver* as an attribute-grammar propagator or *stays a hand TreeFold* is the LOCK-1
inherited-attribute verdict this family defers to — the census says so explicitly and pass-2
confirms the dependency is real (the back-patch reads a global name environment).

**`ir.bbnf`'s consumer (Y.13, same-wave).** The lowering passes at `crates/passes/src/lib.rs`
(`derive_recognizers`, `derive_backend_shape`; ~2096 LOC) — they consume GrammarIr and produce
BackendShape today, and would consume `ir.bbnf`-derived nodes as grammar-to-grammar maps. Naming
this consumer is not optional: an `ir.bbnf` authored without the passes rewritten to eat it is
consumer-less substrate (constitution viii).

### 1.3 If LOCK-3 does NOT adopt: what remains

`ExprKind`/`BackendShape` stay hand Rust enums. 7B's Plane 3 degrades to a **7D-oracle-only**
`ir.bbnf`: a differential grammar checked *equivalent* to the hand enum (the derived-variant set
must match `ExprKind`'s variants) but never the production definition — because there is no
same-wave consumer, so promotion to production is forbidden by Y.13. The honest residue is: the
data-plane derivation is provably feasible (RegexParserEnum is the in-repo proof), but without
LOCK-3 rewriting `passes/` to consume it, the plane is a verifier, not a generator organ.

---

## 2. Q2 — Structure-scoping: the data-vs-escape ratio and the named kernel-escape signature

Measured on ONE emitter blob — `general_descent/emit.rs` (453 LOC, 428 non-blank): the descent
recognizer emitter, the most grammar-generic emission surface in skinny.

### 2.1 The ratio, honestly decomposed

Raw grep is misleading (61 `writeln!/write!/push_str` "emission" lines vs 96 `match`/control-flow/
fn-sig lines). The honest decomposition is by *role*:

- **FIXED template (100% data).** `render_header` + `render_prelude` (~230 LOC of `r#"..."#` raw
  strings: `ParseError`, `parse_into_tape`, `skip_ws`, `match_literal`, `match_leaf`,
  `consume_quoted`, `is_text_byte`, `mark`) are byte-identical for *every* grammar — pure template
  leaves a template grammar carries verbatim. Only `LEAF_STOP` (the structural-boundary alphabet)
  and `{prefix}` are substituted, and both are grammar-DERIVED data (`emit.rs:88-94`).
- **Shape→template map (data).** In `emit_expr` (`emit.rs:288-393`): `ByteLiteral→match_literal`,
  `RegexProgram→match_leaf` (with derived first-set + nullable flag), `CallRule→parse_X`,
  `Seq→ws-interleaved children`, `ValueProject→emit body` — a flat shape-keyed substitution, fully
  expressible as `IR-shape -> Rust-fragment` projections.
- **The escape.** The speculative-backtrack combinators — `RepeatLoop`, `OptionalBranch`, `Alt`
  (`emit.rs:311-355`, `400-433`) — each emit a `let _r: Result<...> = (|| { let cursor = &mut
  _probe; ... })();` closure over a **fresh mutable alias of the cursor**, commit on `Ok`
  (`*cursor = _probe`), restore-by-not-committing on `Err`, plus the RepeatLoop progress guard
  `if _probe == _before { break }` (`emit.rs:329`).

Verdict (a research answer, not a route): ~**60-70% of emit.rs is data** (fixed prelude + the flat
shape→template map covering leaf/seq/call), but **100% of the control-combinator emission is
escape**. "Mostly data" *survives* for leaf/sequence/call recognition; it *fails* at the PEG
backtracking core.

### 2.2 The NAMED kernel-escape failure signature — the PROBE-CURSOR ESCAPE

The signature is: *bind a fresh `&mut` alias of the cursor (`let cursor = &mut _probe`,
`emit.rs:450`), run the sub-walk against it inside a `(|| {...})()` closure that threads `?`
early-returns, then conditionally commit the alias back to the real cursor.* A template grammar
cannot express this — it is Rust **borrow/ownership control flow**: the `&mut _probe` liveness, the
closure capture of `_probe`, the `Result` early-return threading, and the RepeatLoop
non-progress break. Substituting a template fragment cannot introduce a scoped mutable borrow whose
liveness the borrow checker tracks; the backtrack *is* the ownership dance, not text.

Call it the **PROBE-CURSOR ESCAPE** (equivalently the backtrack-alias escape). It is exactly R3's
predicted degeneration (emitter-template-as-grammar is a mirage where context — borrow structure —
lives) made concrete and measured. It is the same bound BNFC and MLIR ODS hit: declarative plane
is data, the *combinator semantics* stay hand-code. The family-deciding question this reframes:
whether the PROBE-CURSOR ESCAPE is ONE fixed hand-written meta-combinator (a `speculate(shape)`
kernel the emitter *calls*, parametrized by the sub-shape) or N per-site escapes. If one kernel
absorbs all three combinators, 7B's "mostly data" holds with a named ~3-function backtrack floor;
if the escapes proliferate per grammar, 7C (bounded totality) is the honest form.

---

## 3. The shared empirical question — DECIDED: the archetype gap, not a parser gap

**Question.** Authoring `-> Span`/`-> T` builder projections on bbnf's structural rules over the
recognizer emitter: does the emitted parser reparse the corpus into GrammarIr-equivalent structure
(within the `add_expr`+`resolve_refs` host budget) while `regen∘regen` holds byte-identical — or
does it cascade, and at which construct? Drive the `!has_rule("value")` branch
(`runtime_generator.rs:98`), never `json_sink_direct`.

**Finding — it CASCADES, at the `ValueProject`→builder boundary, for an ARCHITECTURAL (not
lowering-rule) reason.**

The general descent emitter is a **recognizer + lazy tape view**, not a builder. The `->`
projection is **DROPPED by the recognizer**: `SinkOnlyExpr::ValueProject { body, .. }` emits only
its body so the leaf is matched and tape-marked — "the projection (target/ty) is consumed by the
VIEW, not the recognizer" (`emit.rs:376-380`). It resurfaces ONLY as *shallow lazy typed
accessors* in the view: `classify_projection` maps `-> f64`→`as_f64`, `-> true/false`→`as_bool`,
`-> Nu8`→`as_tag`, everything else→`span()` (`view.rs:35-47`, `118-184`). The product of a parse is
a `{Prefix}Document` wrapping an offset `Tape` + a `{Prefix}Kind`-per-node lazy decode
(`view.rs:255-431`) — NOT a typed sum-of-products AST.

So a `-> Span`/`-> T` builder projection over THIS emitter does not reparse into
GrammarIr-equivalent structure; it yields tape offsets + a lazy kind view. GrammarIr-equivalence
requires **eager materialization** into a typed AST — the `RegexParserEnum` archetype
(`parse-that/.../regex/generated.rs`), produced by the ORPHANED `bbnf_derive`→`cargo expand`+Python
path, which the skinny general_descent emitter architecturally lacks. The cascade is therefore not
a missing lowering rule and not one of the three bounded parser gaps (`?w`→`->` attach at
`grammar/src/lib.rs:1361 parse_seq`; `@{}` atom at `parse_atom`; `|params|` closures — all upstream
and bounded, and the hand parser already builds `ExprKind::Project` via
`grammar/src/lib.rs:1404 parse_postfix → 1481 parse_projection`). It is an **EMITTER-ARCHETYPE
gap**: the recognizer emits a tape offset where a builder must construct-and-return a typed node.

**The two archetypes, named.**
1. *Recognizer-tape-view* (skinny `general_descent`): `parse_<rule>(bytes,cursor,tape) ->
   Result<(),ParseError>`; product is an offset tape + lazy `{Prefix}Kind` view; `->` is a view
   accessor classifier. NO GrammarIr.
2. *Eager typed builder* (parse-that `regex/generated.rs`): `parse_<rule> -> Result<TypedNode>`;
   product is a sum-of-products AST; `->` is the constructor. GrammarIr-equivalent by construction.
   Its public producer face is `regex::parse_generated` (`parse-that/.../regex/mod.rs:34`) — parse
   to typed AST, then `host::lower` folds to `Hir` (`mod.rs:31,57`); a PRODUCER, not a recognizer.

**The byte-determinism caveat surfaces here.** The recognizer path IS deterministic (3× byte-
identical, the fixpoint precondition holds). But a fixpoint over *recognizer bytes* is the WRONG
subject for a GrammarIr round-trip: a recognizer that never reads `->` for structure stamps
identical bytes whether or not a builder would round-trip. `regen∘regen` byte-identity on archetype
1 certifies tape-recognizer reproduction, NOT builder equivalence — a non-vacuous fixpoint on 7B's
plane must be over archetype 2's *typed-AST* output (or over GrammarIr equivalence directly), never
the recognizer text.

---

## 4. The mechanism made concrete (pass-2, sharpened) — three archetypes, one meta-combinator floor

- **Plane 1 (bbnf self-parse) needs archetype 2, not archetype 1.** The general descent emitter
  cannot self-host bbnf into GrammarIr; a THIRD emitter must be authored: an eager typed-builder
  descent over `SinkOnlyProgram`, gated on a *shape predicate* (rules carrying `->` projections
  that demand materialization), never a grammar name (the zero-name-gate law; the last name-gate
  residue is already a GN-5.W3 CI grep-gate target). The builder emitter shares the PROBE-CURSOR
  ESCAPE kernel with the recognizer (builders backtrack too) — so the ~3-function backtrack floor
  is common to both archetypes and is the surviving hand-code kernel.
- **Plane 2 (regex self-parse).** Re-home the parse-that `RegexParserEnum` bootstrap
  (`regex/generated.rs`, the orphaned `bbnf_derive`+`cargo expand`+Python path) onto
  `cargo xtask regen-regex` emitting via archetype 2, deleting the sed/Python post-processor.
  Regex substrate is settled: grammar-tier `Regex(StringId)` opaque, engine-tier rich `Hir`,
  bridged by `simplify_hir`; the self-host obligation is the PARSER (regex.bbnf → typed AST),
  while engine-tier divergences (char-class canonicalization, quantifier normalization) are
  `host.rs` `lower` obligations, NOT oracle verdicts.
- **Plane 3 (IR grammar).** §1.2 — `ir.bbnf` data plane + `add_expr`/`resolve_refs` host floor;
  consumer = `passes/`.
- **Plane 4 (emitter templates).** §2 — ~2/3 data, with the PROBE-CURSOR ESCAPE meta-combinator as
  the irreducible kernel. The three per-grammar emitters (`json_sink_direct`, `css_scan_direct`,
  `general_descent`) collapse toward one template-driven lowering keyed on IR *shape*.
- **Regen/fixpoint infra.** `skinny/xtask/src/main.rs:40-48` dispatches `regen-json`/`check-json`,
  `regen-sheets`/`check-sheets`, `regen-css`, `regen-real-typed`; `xtask/src/regen.rs:20
  write_targets`, `:37 check_target` is the bit-for-bit staleness gate (blake-hashes rel-path +
  bytes, `regen.rs:177-184`). No `regen-bbnf`/`regen-regex`/`regen-ir` exists; these are the
  self-host targets to add. The fixpoint SUBJECT for 7B is archetype-2 typed-AST bytes or GrammarIr
  equivalence — never recognizer bytes (§3 caveat).

---

## 5. Prior art (with citations)

- **BNFC (Forsberg & Ranta)** — the thesis realized: one labeled-BNF grammar generates lexer,
  parser, AST datatypes, pretty-printer, skeleton transformer, multi-backend. Its labeled-BNF
  variants ARE §1.1's `ir.bbnf` shape; its known bound — generated parsers large/hard-to-debug,
  context-sensitive constructs escape to hand code — is precisely the PROBE-CURSOR ESCAPE and the
  build floor. Encouragement and cautionary bound in one.
- **MLIR ODS / LLVM TableGen** — industrial "IR + emitter as data": `.td` files declare ops, a
  fixed generator emits C++ op classes/verifiers/printers (Planes 3+4 in production). Lesson: the
  declarative plane is data; folding/canonicalization semantics stay hand-written — the totality is
  bounded exactly at §2's combinator kernel.
- **Spoofax / SDF3 + Stratego (Visser)** — SOTA workbench: syntax, transformation, static
  semantics declaratively specified; JSGLR origin-tracking is the debuggability-budget reference.
- **tree-sitter** — `grammar.js`→generated C tables; metagrammar NOT self-hosted, tables
  near-unsteppable — argues for 7B's typed-AST archetype-2 over tables.
- **META-II (Schorre 1964)** — the meta-circular fixpoint root; establishes that a committed
  stage-0 IS the whole game (the R4 chicken-and-egg mitigation).
- **egg / egglog** — rules-as-data; the rewrite plane Plane-3 transpositions consume (shared with
  LOCK-2's e-graph home) — relevant if `resolve_refs` becomes a scheduled attribute pass.
- **Attribute grammars (Knuth 1968; Kastens' Ordered AGs; JastAdd)** — the exact theory for
  `resolve_refs`: `Ref.target` is a synthesized attribute over an inherited name environment. The
  LOCK-1 verdict decides whether this is solver-scheduled (JastAdd-style demand evaluation) or a
  hand TreeFold.

---

## 6. Named risks and counterexamples

- **R1 — Archetype-2 does not exist in skinny.** The GrammarIr-equivalence claim requires an eager
  builder emitter that is unwritten; the only builder proof is parse-that's `RegexParserEnum` via
  an orphaned hack. *Counterexample:* until archetype 2 is authored and proven byte-equivalent to
  the hand parser's GrammarIr on the corpus, Plane 1's round-trip is UNPROVEN, and the fixpoint on
  archetype 1 (recognizer bytes) is vacuous for this claim (§3 caveat).
- **R2 — PROBE-CURSOR ESCAPE proliferation.** If the backtrack kernel cannot be factored into ONE
  parametrized meta-combinator (call `speculate(shape)`) shared by Alt/Repeat/Optional across both
  archetypes, the escapes multiply per grammar and "mostly data" degrades to 7C bounded totality
  with the combinator core named as the floor. *Counterexample:* measure whether one meta-combinator
  absorbs all three; failure names the exact combinator that resists.
- **R3 — No LOCK-3 adoption ⇒ dead `ir.bbnf`.** §1.3: without `passes/` rewritten to consume it,
  `ir.bbnf` is consumer-less (constitution viii) and demotes to 7D oracle-only.
- **R4 — `resolve_refs` depends on the LOCK-1 attribute verdict.** The cross-rule back-patch is an
  inherited/synthesized attribute; if LOCK-1 does not host attribute propagation, `resolve_refs`
  stays a hand TreeFold in the host floor — a permanent, named non-grammar organ.
- **R5 — Debuggability (charter budget).** archetype-2 generated parsers carry synthetic
  `class_atom_0`/`group_0` variants and `#![allow(...)]`; unsteppable without origin tracking. The
  `@debug`/source-map tower (`bbnf.bbnf:72 debug_directive`) must map generated frames→grammar
  spans before promotion; absent that, the hand parser survives as the debug build (7D oracle).
- **R6 — Build-time (charter budget).** The orphaned bootstrap proves proc-macro+`cargo expand` is a
  build hazard; archetype 2 must emit DIRECTLY through `xtask regen`. Measure `regen-bbnf`/
  `regen-regex` wall-time vs the hand parser's zero cost; overflow past a named budget keeps that
  plane 7D-only.

---

## 7. What a prototype must test to move family 7B

1. **Archetype-2 builder emitter (the decider).** Author an eager typed-builder descent over
   `SinkOnlyProgram`, keyed on a shape predicate for materializing `->` projections (never a
   grammar name). Prove ONE grammar — bbnf's `identifier`/`literal`/`regex` leaf rules, or JSON —
   emits a builder whose reparse is a typed AST byte-equivalent to the hand parser's GrammarIr on
   the json/css/sheets/csv corpus (differential oracle), AND `regen∘regen` byte-identical over the
   *typed-AST* output (the correct fixpoint subject, §3). This fork decides 7B (builder exists,
   mostly-data holds) vs 7C (recognizer+host only, bounded totality).
2. **PROBE-CURSOR ESCAPE factoring.** Attempt to reduce Alt/Repeat/Optional emission across BOTH
   archetypes to ONE `speculate(shape)` meta-combinator. Report whether one function absorbs all
   three or the escapes proliferate — the §2.2 / R2 metric that sets the data-vs-escape verdict.
3. **`ir.bbnf` minimal transposition.** Grammar-define `ExprKind` (the 10-variant sum); prove one
   grammar-to-grammar map (AST→HIR) matches the hand lowering on JSON, with `add_expr`/`resolve_refs`
   declared as the two typed host fns and `passes/` named as the same-wave consumer.
4. **Plane-2 re-home.** Move the parse-that `RegexParserEnum` bootstrap off `cargo expand`+Python
   onto `cargo xtask regen-regex` via archetype 2; prove byte-identical-or-improved vs current
   `generated.rs`; delete the post-processor; measure build-time delta.
5. **Budget verdict.** Aggregate `regen-bbnf`/`regen-regex` wall-time vs a named build-time budget;
   run a source-mapped panic/breakpoint probe in a generated archetype-2 parser vs a named
   debuggability budget. The maximal four-plane claim is earned only if both budgets hold AND the
   archetype-2 round-trip (test 1) is byte-clean; otherwise the honest disposition is bounded
   totality (7C) with the failed plane named.

**The move that decides the family:** test 1 (archetype-2 round-trip) gated on test 2 (does the
PROBE-CURSOR ESCAPE factor into one meta-combinator). Planes 1-3 have prior art proving feasibility
(RegexParserEnum in-repo, BNFC, MLIR ODS, attribute grammars); the recognizer-vs-builder archetype
gap and the backtrack-alias kernel are where "mostly data" is won or demoted to "bounded totality."

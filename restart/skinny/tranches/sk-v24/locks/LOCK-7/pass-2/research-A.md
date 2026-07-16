# LOCK-7 · Family 7A — STAGED-BOOTSTRAP (pass 2, research-A)

The classic two-stage self-host, made concrete on skinny: a committed stage-0 artifact, a
regeneration engine, and a fixpoint gate `regen∘regen = regen` that certifies a *non-vacuous*
equivalence. This family develops that formulation to its strongest form and states, with
measured pointers, exactly what a prototype must test to advance it.

## 1. The measurement substrate (what exists at skinny HEAD, XL-A)

The skinny Rust workspace is `skinny/` (repo root); grammar sources live in the OUTER `grammar/`
tree. The regeneration engine and its idempotency check already exist and are the substrate 7A
extends:

- **The regen engine.** `skinny/xtask/src/regen.rs:19` `write_targets` → `runtime_request` →
  `codegen::emit_runtime_from_request` → `EmittedSource::write_to_dir`. Its dual is
  `skinny/xtask/src/regen.rs:36` `check_target` → same emit → `EmittedSource::check_dir` (a
  stale-if-different assertion). Dispatch roster: `skinny/xtask/src/main.rs:39-52` —
  `regen-json`/`check-json`, `regen-sheets`/`check-sheets`, `regen-css`/`check-…`,
  `regen-real-typed`/`check-real-typed`. **There is NO `regen-bbnf` and NO `fixpoint` command.**
  `grep -niE 'fixpoint|regen.bbnf|byte.identical' skinny/xtask/src` returns only two comments in
  `regen_css.rs:35,83` about CSS replica collapse — nothing self-host, nothing fixpoint.
- **The emit branch bbnf would ride.** `skinny/crates/codegen/src/runtime_generator.rs:98`
  `if !sink_only.has_rule("value")` routes to `emit_general_descent`. `bbnf.bbnf` has no `value`
  rule, so it shape-dispatches to the SAME general recursive-descent emitter Sheets uses. That
  emitter (`codegen/src/general_descent/`, ~1206 LOC) emits, per its own header
  (`general_descent/emit.rs:1`, `mod.rs:1-25`), **one `parse_<rule>` recognizer per rule** —
  signature `fn parse_<ident><'i>(bytes, cursor: &mut usize, tape: &mut TapeBuilder) -> Result<(),
  ParseError<'i>>` (`emit.rs:259-269`). It builds a lazy offset tape + a view, NOT a `GrammarIr`.
  Regex leaves have no matcher: each `RegexProgram` is a grammar-sanctioned delimiter-scan
  Span-projection (`mod.rs:16-24` — "typed leaves deferred to `Span`").
- **The hand builder front-end** it would replace: `skinny/crates/grammar/src/lib.rs` (2142 LOC),
  `parse_grammar` at `:33` calling `parser.grammar.resolve_refs()?` — the cross-rule link
  (TreeFold) is the last build step. Node construction is `self.grammar.add_expr(ExprKind::…, span)`
  (append). Projections: `parse_postfix:1404` sees `->` and calls `parse_projection:1481`, which
  reuses `parse_runtime_projection` and wraps in `ExprKind::Project { body, target, ty }`.

## 2. The `seed_LOC` headline, re-derived on skinny — building seed vs recognizer seed

The 7A trade turns on WHAT the committed stage-0 is. Two seeds, measured:

- **A *building* seed inflates PAST the hand-parser, ~10×.** The legacy self-host generated
  `GrammarIr`-building bbnf parser is `crates/core/src/grammar/generated/bbnf.rs` = **21 557 LOC**
  (single grammar). The full legacy regen fleet reached 96 886 lines byte-identical across two
  cycles at one HEAD (`docs/benchmarks/archive/post-AX-W0a2g-progress.md:175`). Against the skinny
  hand builder front-end of **2142 LOC** (`grammar/src/lib.rs`), a building seed is ~10× the code
  it replaces — the auditability that is 7A's whole selling point (PORTFOLIO 7A "maximal
  auditability") evaporates. This is the DIRECTIVE's "building seed inflates toward the
  hand-parser LOC" made concrete: it does, and beyond, by an order of magnitude.
- **A *recognizer* seed is sub-hand-parser.** The general_descent recognizer emit for Sheets — a
  39-branch operator tower, structurally richer than `bbnf.bbnf`'s 20 rules — is
  `runtime/src/grammars/sheets/generated.rs` = **1591 LOC**; JSON = 960 LOC. A bbnf-in-bbnf
  recognizer seed lands in the same 1–1.6k band, BELOW the 2142-LOC hand parser. The recognizer
  seed keeps the auditability; the builder seed does not.
- **The trade vs total self-host (7B):** the recognizer seed self-hosts the *shape* of `bbnf.bbnf`
  (it re-recognizes the grammar corpus) but produces a tape, not a `GrammarIr` the compiler can
  consume — so it is a self-host of the *reader*, not the *front-end*. Total self-host requires the
  builder augment (§6), which is where the LOC inflates and where the open empirical question (§6)
  bites. 7A's strongest form is therefore: **commit the recognizer seed; gate it with a non-vacuous
  fixpoint; treat the builder augment as a separately-gated, born-RED escalation**, not a
  precondition.

## 3. The ~88s→0.5s datum is a legacy transplant (verified)

The DIRECTIVE flags "~88s→0.5s" for verification. Method (per DIRECTIVE): grep `regen-bbnf|fixpoint`
in skinny xtask — returns nothing (§1), so no skinny run ever produced this datum; it is imported.
Tracing it: the "88 s wall" is `docs/precepts/audits/REAUDIT-2026-04-30/06-throughput-commit.md:241`
— "**the 88 s wall is cargo's**" (a compile/rebuild wall; the codegen itself is "76 ms total
post-build"). The "≤ 0.5 s" is a *warm iter-check* invariant
(`docs/benchmarks/archive/post-B6-W1-walls.txt:47,61,179` — "warm iter-check ≤ 0.5 s"), i.e.
incremental cargo, not regen. **Conclusion: "88s→0.5s" conflates two cargo build-time numbers from
the legacy tree; it is NOT a self-host regeneration measurement and must not anchor any 7A claim.**
The only legitimate legacy self-host timing datum is qualitative: `docs/benchmarks/post-AZ-II.json:88`
"Codegen-emitted `BbnfBootstrap::parse` self-host (regen reproduces itself byte-equal AND admits
the BBNF fixture corpus)" — a *subject* statement, not a latency.

## 4. The fixpoint SUBJECT — choose generated `.rs` bytes, but prove non-vacuity

Two candidate subjects: (a) generated `.rs` bytes byte-identical; (b) `GrammarIr` equivalence
(parse `bbnf.bbnf` with hand parser and with the emitted parser, compare the built IR).

**Choose (a) generated `.rs` bytes** — it is what `check_dir` already computes (`regen.rs:36-49`),
it is the industry canon (rustc, GHC), and it is directly falsifiable. BUT the DIRECTIVE's warning
is decisive and is confirmed by legacy evidence: **a fixed-const emit path stamps identical bytes
for any input, so a triple-identical byte fixpoint over a fixed generator certifies nothing.**
Today's `check_target` re-runs the SAME committed generator over the SAME source — it proves the
source and the checked-in output agree, NOT that a bbnf-in-bbnf parser is a faithful reproduction.
That is the **vacuous fixpoint**.

The non-vacuity condition — the rustc `stage2 = stage3` law, not `stage1 = stage2`:

- **stage0**: the committed generator (hand `grammar/src/lib.rs` front-end + `codegen`) emits the
  bbnf parser P₁ from `bbnf.bbnf`.
- **stage1**: P₁ re-parses `bbnf.bbnf` and drives the emitter to produce P₂.
- **stage2**: P₂ re-parses `bbnf.bbnf` and produces P₃.
- **The gate is `bytes(P₂) == bytes(P₃)`** — a fixpoint over the *self-applied* transform. Only the
  second self-application certifies, because P₁ (built by the hand front-end) and P₂ (built by P₁)
  legitimately differ, exactly as rustc's stage1 ≠ stage2 while stage2 = stage3.

**Legacy proof this is non-vacuous — it actually cascaded.** The legacy self-host, when the seed
diverged, did NOT triple-match trivially: `docs/benchmarks/archive/post-AX-W0a2h-progress.md:135`
records "cycle 1 (97 558 lines) ≠ cycle 2 (169 446 lines); cycle 2 = cycle 3 idempotent at the
169k fixed point **BUT drops Pratt classification and sub-variants**. Regression from HEAD." This
is the counterexample that makes the byte-fixpoint non-vacuous AND exposes its ceiling: (i) a
divergent seed produced a 97k→169k jump on the first self-application, so the fixpoint is NOT
free — it discriminates; (ii) it converged at 169k while **losing semantic content** — so
byte-identity of P₂=P₃ certifies *reproduction*, not *correctness*. **Therefore the byte-fixpoint
must be paired with a semantic anchor** (a differential parse of the BBNF fixture corpus:
`GrammarIr` from hand parser ≡ structure recognized by the emitted parser), or it can converge on a
wrong-but-stable output. `GrammarIr` equivalence is thus not the fixpoint *subject* but the
fixpoint's mandatory *co-gate*.

## 5. Legacy fixpoint-gate — RESCUE-and-port vs re-author; the born-RED gate

The legacy gate exists: `xtask/src/regen_simple_runtime.rs:376` maps grammar `"bbnf"` →
`"regen-bbnf"`; the self-host assertion is `crates/core/tests/serialize_roundtrip.rs:267-279`
(codegen-emitted `BbnfBootstrap::parse`, no hand bootstrap) and the gate at
`crates/core/tests/egraph_semantic.rs:151` ("BBNF bootstrap (self-hosting)"). Legacy archives at
GN-0 (XL-A).

**Cost the two routes:**

- **RESCUE-and-port** the legacy `regen-bbnf` + its 21 557-LOC building seed: the seed is a
  *builder* emit (`BbnfBootstrap` constructs typed nodes), which the skinny general_descent emitter
  does NOT produce (it emits recognizers, §1). Porting the building seed means porting the legacy
  *builder* emitter path — a large orthogonal code-path (violates `no-orthogonal-codepaths`).
  Rejected as a transplant; the legacy seed is reference, not cargo.
- **RE-AUTHOR** on skinny's one generator: add a `BBNF_TARGET`/`regen-bbnf`/`check-bbnf` arm
  mirroring `SHEETS_TARGET` (`main.rs:42-43,191`, source `grammar/bbnf/bbnf.bbnf` 89 LOC + two
  `@import` modules `expressions.bbnf` 37 LOC, `types.bbnf` 7 LOC). This is the cheap path IF
  `bbnf.bbnf` parses through the hand front-end — but it does not yet (§7). The re-author cost is
  therefore the three parser gaps (§7), NOT the emitter.

**The born-RED gate for the parser-augment (7A's deliverable):** a `check-bbnf-selfhost` xtask arm,
RED until:
1. `grammar::parse_grammar(bbnf.bbnf)` succeeds (the three gaps closed) — proven by the parse not
   erroring, AND the built `GrammarIr` rule count == 20.
2. `emit_general_descent` produces P₁; P₁ compiles.
3. P₁ re-parses the BBNF fixture corpus and every document's recognized structure matches the hand
   parser's `GrammarIr` (the semantic co-gate, §4).
4. `bytes(P₂) == bytes(P₃)` (the self-applied byte fixpoint, §4).
Each numbered condition is independently RED-until-green; the gate cannot pass on a fixed-const
short-circuit because condition 3 differential-checks against the hand parser and condition 4 is
the second self-application.

## 6. The shared empirical question — recognizer emit vs `-> T` builder projection

`bbnf.bbnf` ALREADY carries `-> Span` projections on its structural leaves (`identifier:9`,
`literal:11-13`, `regex:15`, `big_comment:17`, `comment:18` all `-> Span`; `mapped_factor:42`
threads `-> value_expr : type`). The open question: does authoring `-> Span`/`-> T` builder
projections over the RECOGNIZER emitter reparse the corpus into `GrammarIr`-equivalent structure
within the `add_expr`+`resolve_refs` host budget, while `regen∘regen` holds byte-identical — or does
it cascade, and where?

**The concrete tension:** general_descent emits recognizers returning `Result<(), ParseError>` that
only tape-mark (`emit.rs:251` `fn mark`), and its regex leaves are Span-scans by construction
(`mod.rs:16-24`). A `-> Span` projection is therefore ALREADY what the recognizer does (mark a
span) — so `-> Span` rules should round-trip. But `-> T` for a structured `T` (e.g.
`mapped_factor -> Project{body,target,ty}`) requires the emitter to CONSTRUCT a node, which the
recognizer path does not do — it would demand a builder augment over the tape (materialize the tape
into `GrammarIr` via `add_expr`, then `resolve_refs`). **The predicted cascade point:** the first
non-`Span` projection — `mapped_factor:42`'s `-> value_expr` and the `@{}`/closure atoms
(`term:33`, `closure:51`), exactly the constructs that build compound IR nodes, not leaf spans.
Recognizer round-trip is expected to hold for the leaf-Span rules and cascade at the
compound-projection rules. This is unseeded and RED-unknown; §8 makes it the prototype's spine.
Drive the `!has_rule("value")` branch (`runtime_generator.rs:98`) — NEVER `json_sink_direct`.

## 7. Integration seams — the three parser gaps (path:line), mapped against `bbnf.bbnf`

To route `bbnf.bbnf` through the emitter the hand front-end must first PARSE it. Three bounded gaps
block that, each verifiable against the source:

1. **`?w`→`->` attachment** (`grammar/src/lib.rs:1361` `parse_seq`). `bbnf.bbnf:17`
   `big_comment = ( … ) ?w -> Span ;` places `?w` between the group and `->`. `parse_seq` on `?w`
   pushes a standalone `ExprKind::Annotation{name:"ws"}` child (`lib.rs:1366-1378`) and `continue`s;
   the following `->` is then seen at sequence level, but `->` is only consumed inside
   `parse_postfix:1409` attached to the *preceding atom* — the intervening `?w` orphans it, and
   `parse_atom` errors on `-` ("unexpected token", `:1529`). Fix: let a `?w`-terminated group carry a
   trailing `->` projection over the whole group.
2. **`@{}` atom** (`grammar/src/lib.rs:1495` `parse_atom`). `bbnf.bbnf:33` `"@{" , rhs ?w , "}"`.
   `parse_atom` has cases for `"`, `/`, `(`, ident (`:1499-1528`) — no `@`; it errors "unexpected
   token `@`". Fix: an `@{ rhs }` inline-group atom case.
3. **`|params|` closures** (`grammar/src/lib.rs:1361` `parse_seq` / rule-level `rhs`). `bbnf.bbnf:51`
   `closure = "|" , identifier , … , "|" ?w , rhs` and `rhs = closure | alternation`. `parse_seq`
   BREAKS on `Some('|')` (`:1381`) treating it as alternation delimiter; a leading `|params|` at rhs
   position is never recognized. Fix: at rule-body entry, a leading `|` opens a closure parameter
   list (the `GrammarCall`/first-class-closure feature already in the language design).

The emitter side is a re-author of one target const + two dispatch arms (`main.rs`, mirroring
`SHEETS_TARGET`), plus a generated dir `runtime/src/grammars/bbnf/`. No emitter change is required
for the recognizer seed; the builder augment (§6) is the escalation.

## 8. What a prototype must test to move this family

1. **Close the three parser gaps** (§7) minimally and re-parse `bbnf.bbnf`; assert `GrammarIr` rule
   count == 20 and that the 89-line source round-trips through `parse_grammar` without error.
2. **Emit the recognizer seed** via the `!has_rule("value")` branch; measure its LOC (predict
   1–1.6k, sub-hand-parser) and confirm it compiles.
3. **The self-applied byte fixpoint:** build P₁→P₂→P₃, assert `bytes(P₂)==bytes(P₃)` AND that
   `bytes(P₁)!=bytes(P₂)` is *permitted* (do not require stage1=stage2; require stage2=stage3).
4. **The semantic co-gate:** differential-parse the BBNF fixture corpus with hand parser vs P₁;
   assert structural equivalence — the guard against the 169k "drops Pratt classification"
   convergence-to-wrong-output failure.
5. **The builder-projection cascade probe:** author `-> Span` then a compound `-> T` projection over
   the recognizer emit and locate the exact construct where recognizer round-trip stops holding
   (predicted: `mapped_factor:42` / `@{}` / `closure`). Report the construct and whether the
   `add_expr`+`resolve_refs` budget absorbs it.

## 9. Prior art (citations)

- **rustc bootstrap** (`rustc-dev-guide`, "Bootstrapping the compiler"): stage0 downloaded, stage1
  built by stage0, stage2 built by stage1; the certified invariant is **stage2 == stage3** (a
  stage1-vs-stage2 identity is NOT expected and NOT the gate) — the direct model for §4's
  self-applied fixpoint and the reason stage1≠stage2 is legitimate.
- **GHC stage2/stage3 identity** (GHC Building Guide, "stages"): stage2 = stage3 by construction is
  the correctness argument; same canon.
- **META-II / Schorre (1964)** and the **OMeta / PEG bootstrap** lineage: a meta-parser turns its
  own meta-grammar into a new meta-parser; the fixpoint subject is the emitted parser text — the
  closest small-scale analogue to a bbnf-in-bbnf recognizer seed, its fixpoint precisely
  "emit(grammar) reproduces emit(grammar)".
- **tree-sitter** `grammar.js` → generated `parser.c`: the committed generated parser is checked in
  and CI regenerates to assert byte-identity — the exact `check_dir` discipline skinny already has,
  and evidence the recognizer (not builder) seed is the industry-standard self-host artifact.
- **lex/yacc & re2c**: generated-scanner-as-committed-artifact with a regen check — reinforces
  "commit the recognizer, gate byte-identity."

## 10. Named risks / counterexamples

- **R1 — Vacuous fixpoint (primary).** `check_target` today re-runs a fixed generator; a
  triple-identical byte match certifies nothing (a fixed-const emit stamps identical bytes for any
  input). *Guard:* the gate MUST be the self-applied stage2=stage3 (§4), not stage0's re-emit.
- **R2 — Convergence to wrong-but-stable output.** Legacy converged at 169k lines while dropping
  Pratt classification (`post-AX-W0a2h:135`). Byte-identity is necessary, not sufficient. *Guard:*
  the semantic co-gate (§8.4).
- **R3 — Building-seed LOC blow-up.** A `GrammarIr`-building seed is ~21.5k LOC (legacy), 10× the
  hand parser, destroying auditability. *Guard:* commit the recognizer seed; treat the builder
  augment as a separately gated escalation, never the stage-0 artifact.
- **R4 — The three parser gaps are not equal.** Gap 3 (`|params|` closures) reaches into rule-body
  dispatch and interacts with alternation `|`; it is the highest-risk of the three and may demand a
  `rhs`-entry restructure rather than a local patch. *Guard:* prototype gaps in isolation, gap 3
  last.
- **R5 — Recognizer seed self-hosts the reader, not the front-end.** A recognizer that reproduces
  its own bytes proves the *reader* round-trips; it does not prove bbnf can build `GrammarIr` from
  bbnf. *Honest scope:* 7A delivers a certified *recognizer* self-host + a born-RED builder-augment
  gate; total front-end self-host is 7B's burden and rides §6's cascade answer.

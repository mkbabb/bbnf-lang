# LOCK-7 — THE GRAMMAR TOTALITY & BOOTSTRAP · FAMILY-SPECS (pass 1, synthesis)

Synthesis of research-{A,B,C,D,priorart}, Fable-tier judgment. One cogent spec per family while
routes remain incompatible, then the incompatibility map, then the two sharpest prototype briefs.
Elegance earns nothing; a family stalled at a missing primitive equal in difficulty to the
original problem is marked **BLOCKED-candidate**. All `path:line` claims re-verified on disk at
HEAD (2026-07-16). Supersedes the 03:53 draft (which predates the current research set).

---

## 0 · SHARED SUBSTRATE REALITY (verified at HEAD — load-bearing corrections to the reports)

Confirmed by direct inspection, not quotation:

- **skinny greenfield has NO generated bbnf parser.** No `skinny/crates/bootstrap`, no
  `bbnf::grammar::generated`, no `BbnfBootstrap`. The `pub use … BbnfBootstrap` re-export and
  `crates/core/src/grammar/generated/bbnf.rs` (21,557 LOC) that 7A/7B/7D cite live in the
  **archived legacy root**, not the skinny tree GN-0 carries forward. **Consequence:** the
  fixpoint gate and the differential oracle have **no stage-1 artifact to point at** — it must be
  emitted fresh. The "diagnostic re-export already exists" premise is false for the tree being
  built. (priorart §2.2 correct; 7A §1.2 / 7D §2.1 over-credit legacy.)
- **The 21,557-LOC blow-up is a *legacy-generator* number.** The current skinny generator
  (leaf-inlining IR optimizer via `passes::compile`) has never self-emitted bbnf; its budget is
  **unmeasured**. The 21.5k is the anti-target/witness, not a prediction — the single most
  decisive unmeasured quantity in the lock (Brief 1).
- **`emit_from_source(name, source)` (codegen/src/lib.rs:101) parses with the HAND
  `parse_grammar` then emits a RUNTIME TAPE parser** — not a `GrammarIr` producer. The
  generator's own front-end IS `parse_grammar` (2142 LOC, skinny/crates/grammar/src/lib.rs:29).
  The multi-file `@import` closure is `grammar_provider.rs`'s *import source-set*, **not** the
  bbnf grammar-function `|params| rhs` closure — the latter's lowering is **unproven**.
- **bbnf.bbnf projects its leaves `-> Span`** (`identifier`/`string`/`regex`/`comment`, lines
  9–18); its structural rules carry no `->`. A parser emitted from it **recognizes bbnf and emits
  a span-tape — it does not BUILD `GrammarIr`.** The construction semantics (arena interning,
  `ExprId` edge-building, closure β-reduction, `@import` resolution) live in hand code,
  unexpressed by the grammar.
- **Corroborating witness on the parse-that side:** the committed self-hosted regex parser
  `parse_that::regex::generated` internally *produces* a `RegexParserEnum` AST but its public
  `parse_generated(pattern) -> bool` **discards it** — a self-host that recognizes, not builds.
  Production regex remains the hand engine. And a **third, divergent** regex grammar sits at
  `grammar/misc/regex.bbnf` (28 LOC, buggy escapes) vs canonical `parse-that/rust/bootstrap/
  regex.bbnf` — a clause (ii) one-home breach.
- **`@host` parsed but unlowered:** `RuntimeConstructKind::HostCapture` →
  `"BBNF-UNSUPPORTED-HOST-CAPTURE"` (grammar/src/lib.rs:222). The typed signature language
  (`host_directive`, bbnf.bbnf:73; `type_name`, types.bbnf) exists; the dispatch does not.
- **The string heuristic is real:** `has_parse_time_host_decode` sniffs
  `name.contains("host") && (value.contains("decode")||"parse")` (passes/src/lib.rs:794–798).
- **No self-host xtask targets:** main.rs dispatches json/sheets/css/real-typed only; `check_target`
  is a **single-application staleness** check (committed ≡ emit(source)), NOT a fixpoint.

### The spine finding: RECOGNIZE ≠ BUILD

All four families presume "self-hosted bbnf parser." The verified state is that bbnf.bbnf is a
**recognizer grammar**, not a **builder grammar**. Every family's central act needs the emitted
parser to *build* structure equivalent to `GrammarIr`, not merely accept the string:

- 7A's fixpoint `P2 = generator(P1.parse(bbnf.bbnf))` requires P1 to feed the generator the same
  `GrammarIr` parse_grammar does — impossible while P1 emits a span-tape; the gate degenerates to
  the *staleness* check already present.
- 7B's self-host must **build** the front-end (its own R3, "recognizing ≠ building").
- 7D's NormalAst parity compares *trees*; the generated side yields spans/tape, not GrammarIr.
- 7C is the **relief valve**: build semantics that cannot be `->`-expressed go behind typed host
  fns. Whether bbnf's *construction* is `->`-expressible or host-fn-bound is the pivot the whole
  lock turns on (Brief 2), and it is the genuine 7B↔7C incompatibility.

Closing the gap is grammar-authoring work (upgrade `-> Span` leaves to builder projections) whose
*hardness is the self-hosting itself* — so it is the work, not a BLOCKER — EXCEPT where a
construction step is recursive-with-state (arena interning, `ExprId` graph edges), where it may be
BLOCKED into 7C. **This is the one make-or-break gate all three vertical families share:** does
`emit_from_source("bbnf", …)` produce a parser that reparses the corpus? Until GREEN, the bbnf leg
of 7A/7B/7D is BLOCKED-candidate on the identical missing primitive — a general-descent emitter
that lowers bbnf's own closure / `@import` / `@{}` / recursive-alternation surface.

---

## 1 · FAMILY 7A — STAGED-BOOTSTRAP · disposition: ADVANCE (fixpoint gated on the builder grammar)

**Architecture.** GCC/META-II/rustc shape: minimal committed stage-0 `S0`; `P1 =
generator(S0.parse(bbnf.bbnf))`; `P2 = generator(P1.parse(bbnf.bbnf))`; gate `bytes(P1) ≡ bytes(P2)`
(regen∘regen = regen); promote `parse_grammar := P1`. Thesis: totality is earned by the *smallest
provably-idempotent trust root*, not the widest self-host.

**Mechanism / seams.** Extend `check_target` (regen.rs) from single-application staleness to a
`bootstrap-fixpoint` arm (two applications); commit + assert the blake3 input digest
(`validate_inputs`, printed-only today); add a `BBNF_TARGET` mirroring `JSON_TARGET` (main.rs:177)
over the bbnf triple; regex resolves as a *dependent* target (regex-as-opaque-span at bootstrap
breaks the nested cycle). Minimal S0 target ≤ ~600 LOC vs the 2142 hand parser — the auditability
lever.

**Gates.** (a) fixpoint byte-identity across ≥2 self-applications; (b) 3×-run determinism
(BTreeMap/sorted, reproducible-builds); (c) digest lock trips on stage-0-edit-without-regen; (d) a
falsification harness planting a non-deterministic emission + a stale digest, proving the gate
*can* fail; (e) S0 LOC ceiling.

**Open questions.** (1) **The fixpoint as stated is not achievable at HEAD** — the emitted parser
is a span-tape recognizer, so `generator(P1.parse(…))` cannot round-trip GrammarIr; the gate
degenerates to the *staleness* check unless bbnf.bbnf becomes a *builder grammar* (Brief 2). What
is the fixpoint *over* — generated `.rs` bytes (proves only determinism) or GrammarIr equivalence
(needs the builder grammar)? (2) Can S0 reach ~600 LOC while still *building* GrammarIr, or does
"build not recognize" inflate S0 back toward the hand parser (thesis-collapse)? **Not BLOCKED** —
the fixpoint *mechanism* half-exists; its correctness meaning is empty until Brief 2 resolves.

---

## 2 · FAMILY 7B — TOTAL-SELF-HOST · disposition: MIXED (A/D advance · B free · C BLOCKED-candidate)

**Architecture.** Drive clause (i) to its limit across four planes in dependency order, each with
born-RED build-wall + generated-LOC gates: **A** bbnf grammar · **B** regex grammar · **C** IR
grammar · **D** emitter templates-as-data. "Generator becomes mostly data."

**Mechanism / seams.** Widen the existing xtask regen mouth (no new engine): `regen-bbnf` /
`regen-regex`; the meta-circular closure feeds all repo `.bbnf` through the emitted parser and
diffs GrammarIr; Plane D converts `runtime_generator.rs` const templates (`CSS_PARSER_RS` etc.,
:339–:505) from programmatic glue to shape-selected template + hole-fill (MLIR/TableGen, already a
gradient).

**Gates.** build-wall cold ceiling vs hand baseline; `lint-loc` per-grammar ceiling (anti-target:
the 21,557-LOC ghost); meta-circular GrammarIr byte-parity on every input including bbnf.bbnf;
Plane-D codegen-LOC-drop with generated output unchanged (`check-css` bit-for-bit).

**Plane dispositions.**
- **A (bbnf):** ADVANCE-pending-measurement. Self-emit budget is unmeasured on the *skinny*
  generator (§0). Brief 1 is make-or-break: emit succeeds ∧ LOC ≤ named multiple → live; blow-up
  recurs → SPEC-ONLY. Even a good budget yields only a *recognizer* until Brief 2 resolves.
- **B (regex):** FREE-ADVANCE — already bootstrapped parse-that-side; work is reconciling
  `grammar/misc/regex.bbnf` into the canonical source (one home) + a fixpoint-gated skinny target.
- **C (IR-as-grammar):** **BLOCKED-candidate.** `GrammarIr` carries interned arenas, `ExprId`
  graph edges, Rust-typed payloads. 7B's own R2 concedes: if the IR needs host fns for its own
  recursion, Plane C *is* 7C's lattice. The missing primitive — "an IR fully expressible as a CFG"
  — is, for a graph-shaped interned IR, **equal in difficulty to the original self-hosting problem
  restated at the IR level.** Gated behind LOCK-3C; carried as a named boundary, not attempted.
- **D (templates):** ADVANCE (existing gradient); R5 (god-template) guarded by typed,
  shape-addressed selection.

**Open questions.** the budget number (Brief 1); whether Plane-A parity needs host fns for build
semantics (Brief 2, shared with 7C); debuggability of a generated front-end (an un-steppable 21k
parser is a named negative result if it recurs).

---

## 3 · FAMILY 7C — HOST-FUNCTION LATTICE · disposition: ADVANCE (lowest-risk in-tree win; the relief valve)

**Architecture.** Totality is a *measured percentage* bounded below by a **typed host-fn
interface**. Three strata: GRAMMAR · PRIMITIVES/COMBINATORS (the exempt floor — pure decode:
Eisel–Lemire, unescape, SIMD; a **closed, declared, checkasm-gated** `PrimitiveFloor`) · HOST
FUNCTIONS (the only legal home for context-dependent / recursive-with-state semantics, behind
`@host name : InType -> OutType` or `(InType, &mut CtxType) -> OutType`). The `HostContext` enum
{Pure, ParseState, TreeFold, CompileIO} is the cost axis, read *structurally* not by substring.

**Mechanism / seams.** Extend `@host name` (bbnf.bbnf:73) to a full typed signature, types from
`type_name` flowing into LOCK-1 inference as a typed leaf; **delete** the string heuristic
(passes/src/lib.rs:794–798), replace with a registry `context_of(HostCall)` lookup; **lower
`RuntimeConstructKind::HostCapture`** so `@{}` (`colorFunction`/`urlFunction`) stops emitting
`BBNF-UNSUPPORTED`; a new `crates/ir/host/` directory-module holds `HostSig`/`HostRegistry`.

**Gates.** (a) a PURE-DECODE `@host` type-checks and the zero-cost payload-no-write assertion
holds (runtime/src/lib.rs:42–44); (b) `check-json` bit-for-bit after killing the heuristic (JSON
floor = 0); (c) one real CONTEXT host fn (TOML `push_path`) round-trips against the `toml` crate;
(d) a totality-floor gate (sibling of `check-name-gates`) fails on floor regression OR a
grammar-name-specific host identifier; (e) purity *verified* by LOCK-1 (a `Pure` sig taking `&mut
Ctx` is a type error).

**Why it advances.** It is the **relief valve** for recognize-vs-build: the exact
GrammarIr-construction steps that resist `->` expression get a *typed, counted, floored* home
instead of dissolving totality into opaque host blocks (the yacc/PEG.js anti-pattern). Genuinely
orthogonal — it governs the horizontal leaf/context boundary, not the vertical bootstrap ladder,
and its in-tree win (delete one heuristic, light one dormant directive) needs no self-emit and
yields signal regardless of Brief 1.

**Open questions / dependency.** Load-bearing on **LOCK-1** for purity verification (named
dependency, not a block). The bbnf floor — how many CONTEXT host fns bbnf's own *construction*
needs — is exactly Brief 2, and sets whether 7B Plane A reaches parity in pure grammar (floor = 0)
or is 7C-bound (floor > 0). R8 (lying `Pure` sig) is discharged only if LOCK-1 checks it.

---

## 4 · FAMILY 7D — META-CIRCULAR-ORACLE · disposition: ADVANCE (the lawful entry ramp; cures Y.13)

**Architecture.** Self-hosting as **verification first, production later**. Emitted bbnf/regex
parsers run *beside* the hand parsers as differential **oracles**; promotion is subsystem-by-
subsystem, gated on measured AST-parity (against a hand-audited golden anchor, not majority vote) +
measured perf. The hand parser is the epistemic anchor until promotion, then the CI-only
differential reference. Matches the field's strongest production precedent (CPython pegen ran
old ∥ new for a full release cycle).

**Mechanism / seams.** A `self_host/` submodule under the GN-5.W1 `bbnf-oracle` directory-module:
`self_host/{bbnf,regex,normal_ast}.rs`; a `check-self-host` xtask arm reusing `write_targets`/
`check_target`; golden fixtures `test-fixtures/corpus/{bbnf,regex}/golden/*.sexp`; a
planted-mutation falsification (mirror `checkasm_parity`'s `INJECT_BUG` + strict/loose switch).
Corpus = `grammar/**/*.bbnf` (zero fabrication) + regex literals harvested from every `.bbnf`.

**Gates.** admitted% (parses at all) · parity% (byte-identical NormalAst) · cyc/B ratio; promotion
= zero unadjudicated divergences sustained two regen cycles; the gate's artifact is the NormalAst
diff, not a grep.

**Why it advances.** The **minimal lawful consumer** of the dormant `.bbnf` sources — cures the
clause (viii)/Y.13 violation the instant a parser is emitted (its consumer is the oracle).
Complementary to 7A: a fixpoint certifies *determinism*; the differential certifies *correctness*
against an independent implementation (Thompson's circle).

**Open questions.** (1) **The NormalAst crux collides with recognize-vs-build:** the generated
side yields a span-tape, not GrammarIr, so the differential compares only at *recognition/span
granularity* (a weak oracle missing wrong-precedence/wrong-construction) unless the builder-grammar
exists — the same upgrade 7A/7B need (Brief 2). (2) R4: the perf gate is vacuous for cold
compile-time bbnf parsing — the honest prize is **LOC deletion**, not speed; teeth exist only for
runtime-hot regex leaves (PROVE-10 floor). (3) R1 (dialect-rejection) is the expected first product
— *which* constructs the generated grammar under-specifies is the augment-BBNF task list (folded
into Brief 1).

---

## 5 · INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The prior-art thesis "they compose as a DAG 7A→7D→7C→7B" is **largely correct** — these are not
four routes to pick one of. The genuine incompatibilities are *localized to two decision axes*.

**INC-1 — Promotion trajectory: 7A (big-bang fixpoint) ⊥ 7D (incremental oracle).** Both govern the
SAME seam (`parse_grammar := P1`) with DIFFERENT decision procedures. 7A flips the whole front-end
the moment `bytes(P1) ≡ bytes(P2)` (a *determinism* proof; big-bang). 7D holds the hand parser as
oracle and promotes construct-by-construct on *differential parity* (a *correctness* proof;
incremental). They cannot BOTH be the promotion rule. **Resolution available:** the two *gates*
compose (require fixpoint ∧ differential), but the *trajectory* must be chosen — evidence tilts to
7D-trajectory (CPython oracle-first) with 7A's fixpoint as a determinism *precondition*, not the
trigger.

**INC-2 — Totality reach at the build boundary: 7B (IR/build in grammar) ⊥ 7C (build behind typed
host fns).** The sharpest genuine conflict. bbnf's GrammarIr *construction* (arena interning,
`ExprId` edges, closure β-reduction) is either `->`-expressible (7B: floor = 0, pure grammar) or
recursive-with-state and host-fn-bound (7C: floor > 0). Both cannot be true. This is
recognize-vs-build made into a boundary dispute; **empirically decidable per construction step**
(Brief 2). 7B Plane C is BLOCKED-candidate precisely because the IR is where this conflict is worst
(graph-shaped interned IR ⇒ the "CFG-expressible IR" primitive is as hard as the original problem).

**INC-3 — Hand-parser fate: 7B (retire on promotion) vs 7D (keep as permanent CI oracle).** *Not a
hard incompatibility* — composable: retire from the *production* path (7B) while keeping it as the
*CI-differential* reference (7D). A named supersession, not an exclusion.

**INC-4 — Trust-root minimization vs measured totality: 7A (smallest S0) vs 7B (most planes
grammar-driven).** A *tension of objective*, not a contradiction: 7A minimizes hand code you must
*trust*; 7B minimizes hand code that *exists*. They reconcile because 7A's minimal S0 is the
*bootstrap seed* while 7B's totality is the *destination front-end* (P1) — different artifacts,
explicit S0-vs-P1 split.

**Cross-cutting bound (not a pair): budget × debuggability.** If Brief 1 shows the self-emit budget
recurs the 21k blow-up, it simultaneously kills 7B Plane A (LOC gate) AND guts 7A/7D promotion (an
un-steppable generated front-end is a debuggability negative result). One measurement bounds three
families.

**Net.** No family is wholly excluded by another. The lock resolves to **one composed route**
(7A-determinism-floor ∧ 7D-oracle-trajectory ∧ 7C-typed-boundary → 7B-totality-per-plane) with two
open adjudications — INC-1 (trajectory) and INC-2 (build boundary) — plus the budget bound. 7B
Plane C stands alone as BLOCKED-candidate pending LOCK-3C.

---

## 6 · TWO SHARPEST UNCERTAINTIES → PROTOTYPE BRIEFS

Chosen to resolve the two adjudications that gate the whole composed route: the **budget/emit
make-or-break** (bounds 7A+7B+7D jointly) and the **build-boundary census** (adjudicates INC-2, sets
7C's bbnf floor, decides whether 7B Plane A can reach parity in pure grammar). Full briefs in the
structured output — `bbnf-worktree` (self-emit budget + recognition) and `spec-probe` (construction
census).

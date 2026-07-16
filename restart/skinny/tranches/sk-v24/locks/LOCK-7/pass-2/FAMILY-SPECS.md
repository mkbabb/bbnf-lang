# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 FAMILY SPECS (synthesis, post-prototype)

Distilled by the SYNTHESIS agent (Fable-tier), pass 2, from the five pass-2 research reports
(A/B/C/D/priorart), the **two pass-2 prototypes that RAN** (`PROTO-1.md` OPEN-1' cascade;
`PROTO-2.md` regex Tier-S), the pass-1 REGISTRY standing verdict, and CROSS-LOCK. One cogent spec
per family **while the routes remain incompatible**; then the incompatibility map. Elegance earns
nothing alone: a family stalled at a missing primitive equal in difficulty to the original problem
is marked **BLOCKED-candidate**.

**Base (XL-H).** Prototypes ran base-pinned at HEAD `c93fc4314`; every `path:line` below was
re-greped on the live skinny tree this pass (skinny workspace at repo `skinny/crates/…`, grammar
corpus at repo `grammar/…`). parse-that anchors at `/Users/mkbabb/Programming/parse-that`.

> **This rewrite supersedes the pre-prototype draft of this file.** The prior draft's §0 headline —
> "THE MAKE-OR-BREAK, still RED-unknown … PROTO-1 measured the WRONG path (`json_sink_direct`); this
> is untested as such" — is **discharged**. The pass-2 PROTO-1 drove the CORRECT branch
> (`!has_rule("value")` → `general_descent`) and the make-or-break is now **measured RED**. Both
> pass-2 briefs the prior draft returned have been RUN; §7 cuts two fresh ones for pass 3.

---

## §0 — THE SPINE ALL FOUR FAMILIES SHARE (now measured, not asserted)

1. **A grammar-NEUTRAL descent emitter EXISTS** — `skinny/crates/codegen/src/general_descent/`
   (~1206 LOC). `runtime_generator.rs:98 !has_rule("value")` → `emit_general_descent` (`:99/:147`).
   `bbnf.bbnf` has no `value` rule → shape-dispatches here, the branch `regen-sheets` travels. It
   emits **one `parse_<rule>` recognizer per rule** (tape offset-marks; `emit.rs:1` docstring, `:259`
   signature), **not GrammarIr builders**.
2. **THE MAKE-OR-BREAK IS RESOLVED — RED, AT A NEWLY-EXPOSED FLOOR (PROTO-1, RAN_MEASURED).** OPEN-1'
   ("does a `-> Span`/`-> T` builder-augment over the recognizer emitter reparse the corpus into
   GrammarIr-equivalent structure?") **cascades — but one layer BELOW where pass-1 placed the gap.**
   It does not reach the builder-projection question at all. Two measured walls:
   - **Wall 0 — the ORACLE (hand parser) itself rejects 3/4 corpus families, on ONE bug.**
     `grammar::parse_grammar` errors on `bbnf.bbnf @662`, `json.bbnf @319`, `css-l4 @715` — all the
     same defect: `parse_seq` (`grammar/src/lib.rs:1361`) consumes a standalone `?w` as
     `Annotation{name:"ws"}` (`:1373`) and `continue`s, so a following postfix `->` or binary
     `<<`/`>>` is met at the *next atom head* and rejected (`parse_postfix` attaches `->` only
     immediately after an atom, `:1409`). Sheets is the lone `Ok` (no `?w`-then-operator). **The
     three "bounded parser fixes" are genuinely un-landed at HEAD, and the `?w`-attach fix fires
     FIRST (bbnf.bbnf line 17, before `term` at line 33) — so the `@{`/closure atom gaps are never
     even reached.** Fix #2 is the precondition to everything.
   - **Wall 1 — `general_descent`'s LEAF LEXER is a Sheets/JSON delimiter-scan approximation that
     cannot tokenize bbnf's leaves.** bbnf's structural leaves are *regexes*
     (`identifier=/[_a-zA-Z][_a-zA-Z0-9-]*/`, `literal`, `regex=/(\\.|[^\/])+/`, comments — all
     `-> Span`). The emitter defers typed leaves to a Span delimiter-scan by design
     (`regex_first::first_bytes:81` + `emit.rs::{match_leaf:158, consume_quoted:233,
     is_text_byte:226}`). Measured against the real bbnf leaves it **hard-fails at the first regex
     literal**: `split_top_level_alternation:90`/`head_bytes:120` never descend into nested `(a|b)`
     groups → bbnf's `(\\.|[^\/])+` collapses to `first={'.'}` non-nullable → the `match_leaf`
     head-gate (`emit.rs:175`) REJECTS every `/…/` literal; `consume_quoted:233` honors doubled-quote
     escapes (`""`), not bbnf backslash `\"`; `is_text_byte:226` = `[A-Za-z0-9_.$]` excludes `-` (in
     the bbnf identifier class) and includes `.`/`$` (not) → over/under-runs identifiers.
   **VERDICT (PROTO-1):** the builder-augment is refuted **as posed** — the recognizer never produces
   correct bbnf leaf spans, so no `-> Span`/`-> T` projection can materialize. The residual gap is
   **strictly larger and sharper than pass-1's "add builder projections"**: `general_descent` needs a
   **grammar-neutral leaf EVALUATOR** (nested-alt first-sets, backslash escapes, `/`-delimited regex
   literals, the bbnf identifier class) **before** the builder question can be posed. See Brief A.
3. **Construction floor = 2, irreducible ∈ {0,1,2}** (unchanged; not reached by PROTO-1). Node
   *values* are pure `->` (floor 0). `add_expr` (`ir/src/lib.rs:51`, append — SOFT) and `resolve_refs`
   (`ir/src/lib.rs:84`, cross-rule name→RuleId TreeFold — HARD, an *inherited attribute*, host-bound
   iff LOCK-1 declines in-grammar inherited attributes).
4. **No self-host exists on skinny.** xtask carries `regen/check-{json,sheets,css,real-typed}`
   (`main.rs:40-47`); **no `regen-bbnf`, no `fixpoint`**. Stage-0 must be *authored*, not transplanted.
5. **The regex self-host ships as a PRODUCER, and is now MEASURED as an oracle (PROTO-2).** parse-that
   `regex/mod.rs:34 parse_generated -> Result<Hir>` runs the generated parser then `host::lower`.
   Under a pinned `ORACLE_REL_V1` it is **ORACLE-ONLY**: parity 0.7931 (Empty-normed, < 0.80 gate),
   GEN 1.77× slower. Details in §4.

**The unifying pass-2 fact.** BOTH the bbnf *recognizer seed* (7A) and BOTH bbnf-oracle routes (7D
Route T tape-canonical, Route B builder) presuppose `general_descent` can *recognize* bbnf — which
PROTO-1 shows it cannot. The entire bbnf leg of the ladder now rests on **one newly-exposed gap: the
leaf evaluator** (Brief A), which is plausibly a cross-lock coupling to LOCK-5/LOCK-9-9C (a real
regex leaf matcher = a compiled-DFA product of the bootstrapped grammar, XL-5). The regex leg (7D) is
independent and already measured.

---

## §1 — 7A · STAGED-BOOTSTRAP — spec

**Architecture.** Classic two-stage self-host: a committed stage-0 + a fixpoint gate certifying
`regen∘regen = regen`, minimal auditable bootstrap surface. On skinny this is *net-new* (§0.4). The
family's one non-redundant contribution is the **fixpoint LAW** (byte-identical `regen∘regen`, no
2-cycle) + a blake3 provenance ledger — not LOC reduction.

**Mechanism / the seed headline, now measured harder (research-A §2 + PROTO-1).** The seed axis is
**recognizer-USELESS-vs-building-adequate**, and PROTO-1 removes even the "recognizer seed is
producible now" consolation for bbnf: `general_descent` **cannot recognize bbnf** until the leaf
evaluator lands (§0.2 Wall 1). A *building* seed, if reached, inflates **past** the 2142-LOC hand
parser (in-repo witness: parse-that regex self-host is `generated.rs` 2293 + `host.rs` 524 LOC for a
*smaller* language), dissolving the auditability that is 7A's whole selling point. The `88s→0.5s`
datum is **REFUTED** as a legacy transplant (cargo's LTO rebuild, not regen; regen was ~76 ms).

**Fixpoint SUBJECT (research-A §3 + priorart §1).** Cross-generation `.rs` bytes
(`parser_{N+1} ≡ parser_N`, the rustc/GHC/kefir/shecc/pegen `stage2==stage3` law — stage1 differs by
uplift) with a **GrammarIr-equivalence floor** as the non-vacuity witness (a fixed-const path stamps
identical bytes for any input; only the *second* self-application certifies). That floor is
**the same object as 7D's `ORACLE_REL_V1`** (§5 convergence) — do not double-home it.

**Integration seams.** A `BBNF_TARGET` const mirroring `SHEETS_TARGET`; re-author the fixpoint arm
(~200-250 LOC) against `emit_runtime_from_request`; reuse `regen.rs:37 check_target` for the
byte-gate. **Do NOT** rescue the legacy bespoke `emit_typed_bbnf_*` cohort — a NAMED refutation
(overfit audit); the only lawful route is the grammar-neutral general emitter.

**Gates (born-RED).** (a) `check-bbnf-selfparse`: `parse_grammar(bbnf.bbnf)==Ok` — RED until Wall-0
fix #2 lands (measured un-landed). (b) leaf-evaluator gate — RED until §0.2 Wall 1 clears (Brief A).
(c) cross-generation byte-fixpoint + IR floor, with a planted input-mutation that MUST change the
bytes (a fixpoint surviving an input mutation is a fixed-const path, REJECTED).

**Status: ADVANCED (narrowed), retreated at the producibility claim.** Its fixpoint law composes onto
7D (OPEN-2/XP-2). As a *distinct* cutover family it collides with 7D at the hand-parser fate (§5
INC-1). **Open questions:** the leaf evaluator (Brief A) now gates even the recognizer seed; whether a
*building* S0's inflation dissolves the auditability the bootstrap exists for.

---

## §2 — 7B · TOTAL-SELF-HOST — spec

**Architecture.** Constitution (i) maximal: bbnf + regex + IR + emitter templates *all*
grammar-specified. Decomposes into a **FRONT** (bbnf/regex-in-bbnf) and a **BACK** (IR + emitter as
grammar data).

**Axis A — the IR plane (joint LOCK-3 ⊗ LOCK-7, XL-1).** `BackendExpr`/`ExprKind` are regular tree
grammars → `ir.bbnf` is *writable* as inert data (LOCK-3 3C GRAMMAR-HOMOIOUSIA made concrete). But the
category verdict is measured (research-B §2): routing any grammar through the emitter produces a
recognizer+view TAPE, **not a `GrammarIr`** — so `ir.bbnf`'s only lawful consumer is a
`build_grammar_ir(view)->GrammarIr` host builder (the census's host floor). **Conditional on LOCK-3
picking 3C**; else consumer-less (Y.13). Node *shape* is unblocked; a grammar-*built* IR re-enters
the host floor and is LOCK-1-gated. → LOCK-3 adjudication; draft in two stages, never one monolith.

**Axis B — structure-scoping, MEASURED (research-B §3).** On `emit.rs` (452 LOC): **DATA ≈ 50%**
(fixed kernel template, grammar-derivable) · **WALK-FOLD ≈ 20%** (a catamorphism over the IR sum,
`->`-expressible given a tree-fold) · **IRREDUCIBLE ESCAPE ≈ 21%** — the speculative-backtracking
scaffolding (`emit.rs:448` `let cursor = &mut _probe`, the IIFE probe, the nullable-loop guard).
**Named kernel-escape signature:** commit-or-rollback over a mutable SHADOW cursor — delete the
`&mut _probe` re-alias and the generated code fails borrow-check with **E0499/E0503** across the
Alt/Optional/Repeat probes. It resists grammar expression on two exact counts (value-algebra mismatch:
`->` is pure/total, the idiom is stateful `?`-early-return; plus the E0499 borrow bite). Prior art is
uniform (TableGen, MLIR ODS, tree-sitter external scanners): no production system erases the emitter
kernel into data; it *relocates* into a combinator/VM runtime (LOCK-4C).

**Status: BANKED · maximal claim BLOCKED-candidate.** "Generator becomes mostly data" holds for the
~70% DATA+WALK-FOLD, **false for the ~21% ESCAPE** — a missing primitive (grammar-plane backtracking)
equal in difficulty to the original problem. PROTO-1 further undercuts Plane A: `general_descent`
cannot even recognize bbnf (§0.2). 7B is a **research answer, not a route**; its scope questions home
in LOCK-3 (XL-1); the data-vs-escape ratio (now measured) refutes the maximal pole.

---

## §3 — 7C · HOST-FUNCTION-LATTICE — spec

**Architecture.** Totality as a *bounded, measured* quantity: a three-tier lattice keyed on the one
structural fact read off a signature — **whether it threads `&mut Ctx`**. **PURE** `fn(Span)->T`
(the `->` projections + `@{}` span-capture; counted as grammar coverage) · **CONTEXT** `fn(&mut
Ctx,…)` (build-time mechanisms mutating GrammarIr/tape — `resolve_refs`, TOML `push_path`; the
parser-generator instance of *inherited attributes*; comptime, O(structure)) · **RECURSIVE/HOST**
(opaque signed escape, floored + audited). The tier is **INFERRED from the signature**, not declared
— the concrete answer to pass-1's defeatible `@pure` string.

**Independence split (research-C §2, XL-3).** **Lock-free trio** (no LOCK-1): (1) delete the string
heuristic `has_parse_time_host_decode` (`passes/src/lib.rs:794`; fires on `name.contains("host") &&
…"decode"|"parse"` — a latent name-gate; consumers at `:620` `ShapeRationale::HostFnParseTime` and
`:764` `requires_eager_tape` survive, only the DETECTION moves to a typed node) → regen bit-for-bit;
(2) lower `@{}` — add `ExprKind::SpanCapture`, a `parse_atom:1495` `@{` arm; (3) *name* `resolve_refs`
the CONTEXT-tier floor. **LOCK-1-bound:** typed `HostFnSig` into inference; tier-off-`&mut Ctx`; the
floor itself (∈ {0,1,2}).

**Deliverable + gate.** A two-axis totality% counter — LEAVES (`Project`/`SpanCapture`/`Regex`/
`Literal`) × BRANCHES (`Alt`/`Repeat`/`Optional`/`Discard`) — per subsystem, PURE/CONTEXT/HOST
breakdown; floor-gate `xtask check-totality-floor` (a GN-5.W3 `check-name-gates` sibling) with
planted-breach falsification (below-floor host leaf trips it; `@host parse_toml_datetime` name-gate
flagged by an AST walk of bodies, not just node counts; real fleet passes clean).

**The binding correction, now DEEPER.** The census/totality% deliverable NEEDS bbnf to parse+build →
rides the leaf evaluator + builder-augment (§0.2), a **larger** dependency than pass-1 recorded.
"Needs no self-emit" holds for the *gates* (T1/T2/T3/T5), NOT the deliverable. Coupled at T4 (shares
the `@{}` wall) and T6.

**Status: ADVANCED, the SURVIVOR.** Ships value *now* (T2/T4, no other lock, leaf-evaluator-
independent); orthogonal mechanism the winner consumes; supplies the totality% metric every ladder
reports against. **The cheapest signal in the lock — T2 (delete one heuristic, run `check-json`
bit-for-bit) — is STILL UN-RUN across two passes. Highest-ROI missing probe. See Brief B.** Open:
LOCK-1 inherited-attribute verdict fixes the floor; CSS floor unmet until T4 (`color.bbnf:220`/
`values.bbnf:69` depend on `@{}`).

---

## §4 — 7D · META-CIRCULAR-ORACLE — spec

**Architecture.** Self-hosting as **verification first**: generated parsers run as differential
ORACLES vs the hand parsers; promotion is gated on a *pinned* equivalence relation + a measured perf
floor, subsystem by subsystem. ORACLE-ONLY is a lawful terminal.

**The equivalence relation, PINNED AND RUN AS CODE (PROTO-2, the family's load-bearing act).**
`ORACLE_REL_V1 = R1 ∘ R2 ∘ R3`: **R1** unit-collapse to a fixpoint (`Concat([]) ≡ Empty ≡ Alt([])`,
`Concat([x]) ≡ x`, `Alt([x]) ≡ x`, both sides; `norm` verified idempotent); **R2** same-stage
comparison (both post-`host::lower`, shared `ParseOptions`); **R3** group/class/quantifier
field-for-field. All **four planted mutations trip** (greedy→lazy, drop group, bind the `bool` twin =
vacuous → rejected, alt mis-assoc) — the oracle is proven able to fail.

**Regex Tier-S — MEASURED: ORACLE-ONLY / BLOCKED (PROTO-2).** Parity **0.7931 Empty-normed
(46/58; 45/58 strict; committed-51 subset 0.7843)** — BELOW the 0.80 gate. Perf **GEN 1.77× slower**.
Twelve divergences, 100% classified: **9 × `host.rs` OBLIGATION** — one bug class, bounded `{n,m}`
(`parse_braced_quantifier` receives the span WITH braces `{3,5}` and never strips them →
`u32::parse` fails; `pre_recognizer_full=true` proves the parser made the SAME tree) → **route to
LOCK-5**; **3 × oracle VERDICT** rooted in `bootstrap/regex.bbnf` (`.` parsed as a class-literal node
not the dot-metachar; `(?s)` inline-flag becomes a literal capturing group) → **bank in the oracle
ledger; need `regex.bbnf` grammar fixes, NOT `host.rs`**. **XL-2 routing is now SETTLED: 9→LOCK-5,
3→regex.bbnf oracle bank.** regex fails promotion on TWO independent counts (3 grammar-locus verdicts
LOCK-5 cannot discharge + the perf gate) — it stays ORACLE-ONLY even after LOCK-5's fold completes.

**bbnf Tier-S — gated on the leaf evaluator (BOTH routes).** Route T (tape-canonical, no builder)
and Route B (builder-augment) BOTH presuppose `general_descent` recognizes bbnf — refuted at the leaf
floor (§0.2). So the bbnf oracle is not deliverable today at any tier; it rides Brief A. Until then
bbnf is not even Tier-R. (Namespace pin retained: bind the `regex/mod.rs:34` producer for Tier-S,
never the `generated.rs:2288` `bool` twin — M3 proved that vacuous.)

**Promotion ledger.** Rows (subsystem × tier); columns `rel_id` · `binding` (exact producer symbol) ·
`parity_pct` (golden-anchored, NOT majority-vote) · `totality_pct` (**from 7C's counter**, XP-1) ·
`perf_ratio` (cold paired) · `fixpoint` (**7A's subject**, XP-2) · `state`. First COMPLETE row banked:
`rel_id=ORACLE_REL_V1 · binding=parse_generated_with(->Hir) · parity=0.7931 · perf=1.768× ·
state=BLOCKED (ORACLE-ONLY)`.

**Status: ADVANCED, the POSTURE winner.** The lawful entry ramp; ORACLE-ONLY the lawful terminal. The
regex ledger row is the lock's first fully-partitioned measured result. **Open:** bbnf Tier-R/S both
ride Brief A; the canonical regex `.bbnf` is LOCK-5-substrate-fork-blocked (XL-2/XL-7).

---

## §5 — THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

**INC-1 · 7A big-bang fixpoint ⊥ 7D incremental oracle — composition EXCEPT at the cutover posture.**
The *mechanisms* compose: graft 7A's fixpoint LAW onto 7D's oracle artifact, decoupled from promotion
(needs neither perf≥1.0 nor parity=1.0, only byte-identical re-emit) = OPEN-2/XP-2. What collides is
the **hand-parser fate**: 7A-as-distinct-family retires the hand parser at cutover; 7D keeps it
forever as the golden anchor. You cannot both retire and retain. Adjudication: 7D's retain-forever
wins on the measured edge (regex perf 1.77× slower ⇒ promotion may never fire; PROTO-2), so 7A
survives only as the fixpoint-law graft, not a cutover route.

**INC-2 · 7B build-in-grammar ⊥ 7C build-behind-host — RESOLVED-IN-DIRECTION toward 7C, on a LOCK-1
dial.** The census is host-bound (floor 2, irreducible 1), but floor ∈ {0,1,2} pending LOCK-1's
inherited-attribute verdict + a value-language arena primitive. Incompatible only under the demand for
ONE fixed answer; correctly the two ends of a LOCK-1-parameterized dial.

**INC-3 · 7A codegen-as-trusted-primitive ⊥ 7B Plane-D codegen-as-grammar-plane — RESOLVED toward
7A's pole (owed a named supersession).** The pass-2 measurement decides it: DATA ≈50% / ESCAPE ≈21%
on `emit.rs`, the escape E0499-bound. `general_descent` (hand-Rust walking the grammar) is disk
evidence FOR the trusted-primitive pole. The grammar-plane maximal claim is refuted for the escape;
7B Plane-D maximal pole is a BLOCKED-candidate.

**INC-4 · hand-parser fate — composable; the kernel-carve-out boundary.** Retire-production vs
keep-CI-oracle compose (the cutover INC-1 shows never fires; 7D keeps). 7C is orthogonal for
T1/T2/T3/T5, coupled at T4/T6. The 7C stratum-2↔3 `&mut Ctx` seam IS the surviving ladder's
kernel-carve-out boundary (XP-3).

**NEW — INC-5 · the leaf-evaluator gate is a cross-lock coupling, not a within-lock choice.** PROTO-1
relocates the bbnf make-or-break from a within-LOCK-7 builder-augment to a **leaf evaluator** whose
lawful form is "route `/regex/` leaves through the compiled regex matcher" = LOCK-9 9C / LOCK-5's
engine (XL-5). So 7A/7B-Plane-A/7D-bbnf are no longer LOCK-7-internal: they are **downstream of
LOCK-5/9C**. This is a DAG edge the omega gestalt must carry, not a family incompatibility — but it
means no bbnf-leg family can converge inside LOCK-7 alone.

**Convergence (not incompatibility), named.** The 7A fixpoint SUBJECT and the 7D relation are the
**same object** — 7A's GrammarIr-equivalence floor *is* `ORACLE_REL_V1`. One relation, consumed by
both. (XP-4: PROTO-2's `@host resolve_refs : (&mut GrammarIr) -> Result<(),ValidationError>` is
simultaneously 7C's first CONCRETE CONTEXT/TreeFold instance AND the HARD floor a future builder-augment
must house — 7C's floor census IS the ladder's builder-augment host budget.)

**Lawful gestalt shape (hardened by the prototypes).** **7C ships regardless** (orthogonal mechanism +
the totality% metric; leaf-evaluator-independent) **+ 7D as the single ladder posture** (oracle-first;
regex row banked ORACLE-ONLY; bbnf rows gated on Brief A) **+ 7A's fixpoint law grafted onto 7D**
(OPEN-2, decoupled from promotion) **+ 7B banked** (maximal pole BLOCKED-candidate). The whole bbnf
leg now rests on ONE newly-exposed, cross-lock gap: the **leaf evaluator on `general_descent`**
(Brief A) — no longer the builder-augment.

---

## §6 — BLOCKED-candidate register

- **NEW — the leaf evaluator on `general_descent` (the re-cut make-or-break).** `general_descent`
  cannot tokenize bbnf's regex/quoted/identifier/comment leaves (§0.2 Wall 1, PROTO-1). Whether
  giving it a grammar-neutral leaf matcher is a **bounded arm-swap** (route `/regex/` leaves through
  the extant LOCK-5 compiled matcher) or a **missing primitive equal to the original problem**
  (re-author regex matching inside the emitter) is UNRESOLVED. Until Brief A answers it, the entire
  bbnf leg (7A recognizer seed, 7B Plane A, 7D bbnf Tier-R/S) is **BLOCKED-candidate**, and the block
  is a LOCK-5/9C dependency (INC-5), not a LOCK-7-internal patch.
- **7B Plane-D (maximal "generator becomes mostly data").** Missing primitive = grammar-plane
  expression of the ~21% backtracking-control escape (commit-or-rollback over a shadow cursor,
  E0499-bound). Equal in difficulty to the original problem (it *is* authoring a combinator/VM
  runtime). BLOCKED-candidate; the bounded form (DATA+WALK-FOLD lifted, ESCAPE a named committed
  primitive) is not blocked but is ⊆ {LOCK-3 3C + the leaf-evaluator + builder-augment}.
- **7B Axis-A built-IR** and **7C floor finalization** are **LOCK-1-gated, not blocked** (the
  inherited-attribute verdict, floor ∈ {0,1,2}).
- **7D regex** is **BLOCKED (measured), not a candidate**: ORACLE-ONLY on two independent counts
  (3 grammar-locus verdicts + perf); the row is banked, honest, complete.

---

## §7 — The two sharpest uncertainties (prototype briefs, returned via StructuredOutput)

Both pass-2 briefs RAN; these are the pass-3 successors, chosen for breadth across families and for
maximal advance:

1. **Brief A — the grammar-neutral LEAF EVALUATOR on `general_descent`** (bbnf-worktree). The direct
   successor to the refuted OPEN-1'. Decides whether the entire bbnf leg (7A/7B-Plane-A/7D-bbnf) is
   reachable or is BLOCKED on a LOCK-5/9C-coupled primitive (INC-5). Runnable; binary pass/fail.
2. **Brief B — 7C-T2, the cheapest un-run signal in the lock** (bbnf-worktree). Delete
   `has_parse_time_host_decode` → structural predicate; run the `check-*` fleet bit-for-bit. Converts
   the SURVIVOR family's headline ("one-file cure, ships now") from SPEC_ONLY to RAN, and tests
   whether the string heuristic is load-bearing. Leaf-evaluator-independent; advances 7C directly.

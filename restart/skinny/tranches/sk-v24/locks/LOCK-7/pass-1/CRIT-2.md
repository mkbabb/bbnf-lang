# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — ADVERSARIAL CRITIC 2, pass 1 (cross-lock weight)

Critic 2 of 2. Cross-lock charter. This pass reads the **09:51 FAMILY-SPECS** against the two
**freshest** probes — `PROTO-1.md` (10:05) and `PROTO-2.md` (10:02) — which POST-DATE the doc by
~11–14 min, so the doc has again had no chance to reconcile its own commissioned evidence (the exact
structural pathology the prior CRIT-2 round flagged, now recurring one round later). Every
load-bearing claim was re-verified by me on the **live skinny tree at HEAD `fe160a91a`** (not a
worktree, not a quote). The single most important thing this pass found is on disk and it inverts
the lock's headline: **the "general descent emitter is architecturally absent" thesis — PROTO-1's
CASCADE verdict, FAMILY-SPECS §0's "missing primitive", and the prior round's XL-4 "re-author a
machine skinny never had" — is FALSE.** A general, grammar-walking recursive-descent emitter exists,
is 1206 LOC, and is wired into xtask serving a non-JSON/non-CSS grammar today.

**Supersession note.** This file replaces the 05:09 CRIT-2 draft (which critiqued the pre-refresh
04:51 doc and the 05:01 probes). Its XL-4 ("skinny never had a general-descent path") and its REC-1
("the general descent emitter is architecturally absent") are RETRACTED on disk evidence below.

---

## 0. DISK RE-VERIFICATION — the load-bearing facts, compiled/greped at live HEAD `fe160a91a`

| # | Claim under test | Source | Verdict on the LIVE tree |
|---|---|---|---|
| D1 | A general grammar-walking descent emitter is **architecturally absent**; "CompiledLowering emits a JSON parser for every grammar" | PROTO-1 (10:05) VERDICT; §0 "missing primitive"; prior XL-4 | **FALSE.** `runtime_generator.rs:147 emit_general_descent` → `general_descent::render(sink_only, profile_id)`; module `crates/codegen/src/general_descent/{mod,emit,regex_first,view}.rs` = **1206 LOC**. `emit.rs:22 for rule in &program.rules`, `:260 render_rule`, `:288 emit_expr { match expr … }` — a recursive per-rule walk. mod.rs:3: *"the THIRD lowering path of the ONE generator: it walks a … tower: one `parse_<rule>` recognizer per rule."* |
| D2 | The emitter is reached by a **grammar-name** gate (JSON/CSS hard-coded) | PROTO-1; §0 | **FALSE.** `emit_sink_only:97-99`: `if !sink_only.has_rule("value") { return emit_general_descent(…) }` — comment: *"Shape-based, never a grammar name."* Prefix from `general_descent::grammar_pascal(profile_id)`, *"carries no hard-coded type token."* |
| D3 | The general emitter serves a real non-JSON/non-CSS grammar **today** | — (new) | **TRUE.** `xtask/src/main.rs:42 "regen-sheets"`, `:191 grammar/google-sheets/google-sheets.bbnf`, `:205 output crates/runtime/src/grammars/sheets`. Sheets self-emits through `general_descent`. |
| D4 | The skinny hand parser **rejects `->` after a regex leaf** at byte ~421 ("`unexpected token '-'`") | §0 (09:51); PROTO-1 Run A (offset 417) | **FALSE at HEAD.** `grammar/src/lib.rs:1404 parse_postfix` → `:1409 if …starts_with("->")` → `:1481 parse_projection` → `:1486 ExprKind::Project`. The `->` postfix handler is live. PROTO-1's Run-A description (*"parse_seq breaks only on `; ) \| EOF`; parse_atom accepts only `" / ( ident`"*) is the **pre-SK-V18 parser**; PROTO-1 ran worktree `wf_63419230-36d-32`, stale. |
| D5 | CSS emit is a raw-string template (`CSS_PARSER_RS`) that ignores the grammar — the "phantom" | PROTO-1; prior CRIT-2 §0 | **HALF-TRUE, and it names the wrong path.** `runtime_generator.rs:401 CSS_PARSER_RS` raw const **still exists** and is emitted by `emit_scan:211`. BUT the live CSS path is `css_scan_direct/{mod,projection}.rs` + `lower/css_scan.rs::ScanAlphabet::derive_alphabet`, **grammar-derived** (mod.rs:113 *"Grammar-derived structural alphabet"*); `CSS_GENERATED_RS` is gone (mod.rs:13). LOCK-3 REGISTRY confirms this independently (its VERIFIED BASELINE row). PROTO-1 measured the `emit_scan` template and never drove `emit_general_descent` or the scan-direct path. |
| D6 | `add_expr` is append-only, no interner; `resolve_refs` is a separate whole-grammar pass | PROTO-2 (10:02) | **CONFIRMED.** `ir/src/lib.rs:52 let id = ExprId(self.exprs.len()); :53 self.exprs.push(…)`; `:84 pub fn resolve_refs(&mut self)`. |
| D7 | string heuristic `name.contains("host") && (…"decode"\|"parse")` | 7C | **CONFIRMED** — `passes/src/lib.rs:~798` (per LOCK-3 baseline the surrounding lines have shifted; the heuristic is present). |
| D8 | `@{}` HostCapture unlowered → `BBNF-UNSUPPORTED-HOST-CAPTURE` | 7C / §0 | **CONFIRMED** — `grammar/src/lib.rs:222`. |

**The reconciliation D1–D5 forces.** The lock has conflated two distinct propositions and shipped the
false one as its headline:

- *"No general descent emitter exists in skinny"* — **FALSE** (D1/D2/D3). A wired, shape-dispatched,
  grammar-walking recursive-descent emitter serves Sheets.
- *"The general emitter emits RECOGNIZERS, not GrammarIr BUILDERS"* — **TRUE** (mod.rs:9 *"one
  `parse_<rule>` recognizer per rule"*; PROTO-2 D6 floor-2 for the build side). RECOGNIZE≠BUILD
  (FAMILY-SPECS §0.5) **survives intact** — but as a *builder-projection gap on top of an extant
  general emitter*, not the *absence of the emitter*.

PROTO-1's own last paragraph accidentally states the truth and mis-tenses it: *"If a future tranche
builds the general emitter first, the projection lowering that rides on top of it is small (≈3 target
kinds, no runtime arena)."* The general emitter is not a future tranche — it is `general_descent`,
present tense, at HEAD. So the residual bbnf gap is exactly that small rider (projection lowering +
the mod.rs:17 coarse-4-variant regex-classifier caveat + closures), **on top of a machine that
exists** — the "rescue-and-port + bounded augment" framing FAMILY-SPECS 7A used and PROTO-1/XL-4
overrode. **XL-4 as written ("the largest under-gated obligation … re-author a machine skinny never
possessed") is the single largest ERROR in the lock's record, not its largest gap.**

---

## 1. THE THREE-WAY CONTRADICTION IN THE RECORD (this pass's core finding)

The lock now carries **three mutually contradictory verdicts on its two make-or-break facts**, none
reconciled, and the newest doc (09:51) is stale against both newest probes (10:02/10:05):

**CON-1 · the emitter.** FAMILY-SPECS §0 bullet 3 cites *"self-emit succeeds, 998 LOC, 3×
byte-identical … the 21k blow-up does NOT recur"* as a POSITIVE budget result and declares the budget
make-or-break *"retired."* PROTO-1 (10:05) VERDICT is **CASCADE / FAIL** — the only `Ok` is a
JSON/CSS phantom, the builder never returns Ok. **And my D1 shows PROTO-1 is itself wrong on the
stronger claim** (a general emitter exists; PROTO-1 tested the wrong two paths, `emit_scan` +
`emit_sink_only`→JSON, and never reached the `!has_rule("value")` branch that routes bbnf-shaped
grammars to `general_descent`). So the doc credits a phantom success (masked-fallback misuse) while
the probe that should correct it draws the wrong negative from an incomplete path-census. **Both are
wrong, in opposite directions.**

**CON-2 · the construction floor.** The 05:01 census (folded into FAMILY-SPECS §3 as the open
"floor = 0 or > 0") concluded floor = 0. PROTO-2 (10:02) **re-adjudicates and OVERTURNS it to floor
= 2** (`add_expr` SOFT/Hs, `resolve_refs` HARD/Hh; irreducible floor 1 after any value-language
extension). FAMILY-SPECS §3/§5 still says INC-2 *"has no measurement that dissolves it, only one that
locates the boundary"* and leaves floor open — stale against the 10:02 overturn. The prior CRIT-2's
REC-2 (*"N_host_construction == 0; Plane C block FALSIFIED"*) is **superseded** by the 10:02 floor-2;
the record contains both the floor-0 and floor-2 verdicts with no supersession marker.

**CON-3 · the parse wall.** FAMILY-SPECS §0 (09:51) and PROTO-1 (10:05) both re-assert *"the hand
seed rejects `-> Span` after a regex leaf, byte 421/417."* CRIT-1 and the prior REGISTRY corrected
this to *"parse_postfix:1409 handles `->`; the real wall is byte 662 (`?w`→`->`)."* **My D4 confirms
CRIT-1: the `->` handler is live at :1409.** The fresh round (09:51 doc + 10:05 proto) **re-broke a
correction the record had already made** — a regression in the ledger, driven by PROTO-1 re-running a
stale worktree.

Net: the lock's two headline facts each have a contradiction pair in the record, and the authoritative
doc reflects the *older* side of both. No convergence talk is admissible until CON-1/2/3 are
adjudicated against live HEAD (they are cheap — I did D1–D6 in one grep pass).

---

## 2. CROSS-LOCK VIEW (critic-2 charge) — contradictions, double-homes, missing edges

**XL-1 · `ir.bbnf` double-homed 7B-GapB/PlaneC ⊗ LOCK-3 3C — CONFIRMED cross-lock, and PROTO-2 did
LOCK-3's feasibility work under a LOCK-7 harness.** PROTO-2's census maps every construction site to
`ir/src/lib.rs` and concludes the node-*values* are `->`-expressible — that IS LOCK-3 3C
GRAMMAR-HOMOIOUSIA (*"the IR is itself grammar-specified"*) verbatim. LOCK-3's REGISTRY is live and
does **not** name 3C as its converged family (its baseline is a four-edge transposition chain, and it
tracks 3A/3B/3C/3D). So `ir.bbnf` is homed in both LOCK-3 (3C) and LOCK-7 (7B Gap B); if LOCK-3
converges on 3A/3B/3D, `ir.bbnf` has **no consumer in the IR spine** → constitution (viii)/Y.13
consumer-less substrate. The portfolio's *"two routes sharing a mechanism share a family"* rule
(line 9-10) is triggered. **Owed in GESTALT-B: a joint family or a named supersession, with PROTO-2
credited as cross-lock (LOCK-3⊗LOCK-7) evidence, not a LOCK-7 local.** FAMILY-SPECS' *"gated behind
LOCK-3C; carried as a named boundary"* is closer than the prior round but still does not name which
LOCK-3 verdict it presumes — an open cross-lock gate.

**XL-2 · the regex differential oracle double-homed 7D ⊗ LOCK-5, and its parity headline is LOCK-5's
bug list.** The 7D regex ledger (Hand `bbnf_regex` vs Gen `.bbnf`→`RegexParserEnum`→`host::lower`→
`Hir`) is LOCK-5B LAWFUL-FUNCTOR material. Its 12 divergences (9× bounded `{n,m}` reject, `.`
metachar, `(?s)`) are bugs in **`regex/host.rs` — LOCK-5's semantic fold**. The 0.76 parity headline
measures LOCK-5's incomplete fold surfaced through a LOCK-7 harness; whoever moves parity edits
LOCK-5. **Sharpened by LOCK-5's own REGISTRY (P1):** LOCK-5 is itself blocked on a *two-tree
reconciliation* (skinny `crates/ir` opaque-regex-leaf vs top-level `crates/ir` interned) — so the
canonical `.bbnf` the 7D oracle anchors on is not merely "awaits LOCK-5's pick," it awaits LOCK-5
resolving a substrate fork. 7D's regex edge is thus gated on a LOCK-5 precondition deeper than
FAMILY-SPECS names. Adjudicate: bootstrap-verification (LOCK-7) vs functor-round-trip (LOCK-5)? Both
claim it.

**XL-3 · 7C typed leaf ⊗ LOCK-1 — the split is clean, and PROTO-2 hands 7C its first real host fn.**
{delete `passes:798`, lower `@{}` HostCapture `:222`, name `resolve_refs`} are LOCK-1-free. {type
the `@host` signature into inference, read the tier off `&mut Ctx`, prove zero-cost} are LOCK-1
consumers. PROTO-2 (D6) produced 7C's first concrete CONTEXT/TreeFold instance — `@host resolve_refs
: (&mut GrammarIr) -> Result<(), UnresolvedRef>` — closing the prior "no CONTEXT host fn exists
(usage=0)" gap **as a target**, though its purity/zero-cost verification still needs LOCK-1. The
"most independently viable" claim must be split exactly on this line; FAMILY-SPECS states the LOCK-1
dependency but does not split the deliverable list.

**XL-4 · RETRACTED and INVERTED.** The prior round's XL-4 (*"skinny never had a general-descent path;
the gate is re-author, the largest under-gated obligation"*) is **false on disk** (D1/D2/D3). The
corrected cross-lock fact: skinny HAS a general grammar-walking recognizer-emitter (`general_descent`,
1206 LOC, wired `regen-sheets`). The genuine obligation is **strictly smaller** — route bbnf through
`emit_general_descent` (bbnf, like Sheets, has no `value` rule, so it shape-dispatches there) and
close (a) builder-projection lowering `-> Span`/`-> T` (recognizer→builder; PROTO-2 floor-2 is the
host-fn budget for this), (b) the mod.rs:17 coarse-4-variant regex classifier, (c) closures. This is
a *born-RED augment gate on an extant machine*, and it should replace XL-4 in the ledger. The GN-0
archival concern narrows correspondingly: the 21,557-LOC legacy `bbnf.rs` is not the only feasibility
proof — `general_descent`+`regen-sheets` is a *live, in-skinny* feasibility proof for a non-trivial
operator-tower grammar.

**XL-5 · LOCK-9 9C consumes LOCK-7's regex totality with no reciprocal RED edge — STANDS.** 9C
SCANNER-AS-DFA dissolves scanning into *"compiled DFA/prefilter products of the bootstrapped
grammar."* On the 7D evidence that path is 1.77–1.81× slower and 9/51-incomplete (LOCK-5's fold). If
9C banks on the bootstrapped regex being production-grade it inherits a non-firing route. Born-RED in
the omega DAG; neither charter names it. (Unaffected by my D1 finding — this is the regex leg, not
the bbnf front-end.)

**XL-6 · constitution (ix) extant-tranche consciousness — 7C good, 7A/7B/7D undeclared net-new.** 7C
explicitly re-cuts GN-5 (TOML fifth grammar, `check-name-gates` sibling) and PROTO-2's `resolve_refs`
host fn is a concrete GN-5.W3 extension — lawful. But the GN codex carries **no** bootstrap/fixpoint
wave, so 7A/7B/7D are net-new-vs-GN tranches; lawful only if declared as named additions.
FAMILY-SPECS does not declare it. Owed. **New sub-edge:** FAMILY-SPECS §0 fact 1 lists "sheets" as an
existing xtask target while simultaneously asserting "no general emitter" — the Sheets target IS the
general-emitter self-refutation sitting inside the doc's own fact list, undeclared.

**XL-7 · one-home breach, cross-repo (clause ii) — STANDS, deeper than named.** Two divergent regex
grammars (`grammar/misc/regex.bbnf` present here; `parse-that/rust/bootstrap/regex.bbnf` cross-repo).
Collapse is owed to LOCK-5, but LOCK-5's REGISTRY shows the collapse is entangled with its two-tree
substrate fork (XL-2). LOCK-7 cannot treat this as a free "re-home"; it is downstream of a LOCK-5
precondition.

---

## 3. PER-FAMILY CONVERGENCE + ENUMERATED OPEN GAPS

Numbers are the conservative critic floor. I move two off the prior round with named disk reasons:
7A/7B feasibility is *better* than the doc/PROTO-1 pessimism (the emitter exists, D1), but the
record's triple contradiction (§1) is itself a convergence hazard, so I hold both near their floors.

### 7A — STAGED-BOOTSTRAP — **43%**

Determinism precondition GREEN (3× byte-identical on the request-facts path). Fixpoint *mechanism*
half-exists (`check_target`). D1 improves the picture the doc paints (there IS an artifact to fixpoint
over — `general_descent`'s output for a bbnf-shaped grammar), but the *builder-meaning* of the
fixpoint is still gated on the recognizer→builder augment (D6 floor-2).

- **G-7A-a** The fixpoint's SUBJECT is unresolved (§1 open-Q1) and now sharper: over generated `.rs`
  bytes it proves only determinism (already GREEN, and GREEN even for the general_descent path);
  over `GrammarIr` equivalence it needs the builder-projection augment (D6). The doc does not choose.
- **G-7A-b** Rung 1 = json/css staleness check re-pointed = **gate-that-cannot-fail** (disclosed).
  The distinguishing Rung 2 gates on the builder augment, NOT (per D1) on an absent machine.
- **G-7A-c** Seed disposition (frozen-forever vs retire-after-fixpoint) unresolved; frozen-seed ⇒
  hand parser is production forever ⇒ 7A-default is self-VERIFYING (⊆ 7D), a masked-fallback smell.
- **G-7A-d** S0 ≤ ~600 LOC while *building* (not recognizing) unproven; on D6's floor-2 census the
  builder inflates toward the hand parser — thesis-collapse risk unquantified.
- **G-7A-e** The augment-on-general_descent gate (corrected XL-4) is absent from 7A's born-RED list;
  the record still carries the false "re-author absent machine" framing.
- **G-7A-f** Twin-ladder (one fixpoint over bbnf+regex, or two) unresolved; the regex leg is gated on
  LOCK-5's two-tree fork (XL-2), not free.

### 7B — TOTAL-SELF-HOST — **35%**

Better *characterized* than any prior round: construction values are `->`-expressible (D6 steps 1-3),
the general emitter EXISTS (D1), and the residual is a bounded builder-projection augment + a floor-2
host budget. But the doc's Plane-A basis ("998-LOC self-emit succeeds") is a phantom the doc still
credits, and Plane C's stated block is contested (floor-0 vs floor-2).

- **G-7B-a** Plane A: the doc's *"ADVANCE … self-emit budget MEASURED 998 LOC"* rests on the
  RequestFacts phantom (CSS/JSON template, ignores the grammar). By the doc's OWN gate the *faithful*
  emit did not build bbnf. **BUT** — unlike the prior CRIT-2 which demoted to SPEC-ONLY on "absent
  emitter" — the correct disposition is **RE-CHARACTERIZE, not SPEC-ONLY**: the general emitter (D1)
  exists and emits a *recognizer* for bbnf-shaped grammars; Plane A's real status is "recognizer
  demonstrable via general_descent; builder-parity gated on the projection augment + floor-2." The
  998-LOC number remains a phantom (unmeasured for genuine bbnf builder output).
- **G-7B-b** Plane A LOC gate PASSED ON A PHANTOM (998 LOC certified on a byte-identical CSS copy) —
  gate-that-cannot-fail. The genuine bbnf builder-emit LOC is unmeasured; the 21.5k anti-target
  un-bounded. (Now *measurable*, since the machine to produce it exists — route bbnf through
  general_descent and count.)
- **G-7B-c** Plane C: the BLOCKED-candidate RATIONALE (*"interned graph IR as hard as the original
  problem"*) is empirically WRONG for the node-values (D6 steps 1-3 stateless) — but PROTO-2 (10:02)
  also shows it is RIGHT for the one linking edge (`resolve_refs`, Hh, floor-1 irreducible). So Plane
  C is neither cleanly falsified (prior CRIT-2 REC-2) nor cleanly blocked — it splits at the phase
  boundary: node-building expressible, cross-rule linking host-bound. Re-diagnose before pass 2.
  Plus XL-1 (LOCK-3 3C double-home, conditional on an un-named verdict).
- **G-7B-d** Prototype-evidence-misuse guard: PROTO-2's *"construction is `->`-expressible"* is a
  CENSUS result (target shape), NOT a buildable-emitter result; do not lift it as a 7B advance. The
  buildable evidence is D1 (general_descent emits recognizers) — which is *recognizer*, not *builder*.
  Keep the three distinct: shape-expressible (PROTO-2) ⊂ recognizer-emittable (D1) ⊂ builder-emittable
  (unbuilt).
- **G-7B-e** Diagnostics parity (generated front-end spans as rich as 2142 LOC of hand machinery?)
  unmeasured; rustc's deliberate hand-parser retention is standing evidence against.
- **G-7B-f** Plane D (templates-as-data): the sole live-gradient plane, but INC-3 (codegen-as-trusted-
  primitive ⊥ codegen-as-grammar-plane) is undissolved; the load-bearing *"control-flow/dispatch/
  tape-assembly expressible without a borrow checker"* is the unconverged hard part (elegant-reduction
  trap / god-template risk). Note D1's `general_descent` is *hand-Rust that walks the grammar*, NOT a
  template grammar — it is 7A's "trusted primitive" made flesh, evidence FOR INC-3's 7A pole, against
  Plane D's maximal claim.

### 7C — HOST-FUNCTION-LATTICE — **60%**

Still the survivor; most in-tree value; PROTO-2 gives it a measured bbnf floor (construction values
0, arena `add_expr` +1 SOFT, linking `resolve_refs` +1 HARD, `@import` +1 IO, β-reduction absent) and
a first concrete CONTEXT host fn.

- **G-7C-a** CSS floor UNMET — `@{}` HostCapture = `BBNF-UNSUPPORTED` (D8) while `color.bbnf`/
  `values.bbnf` depend on it. Headline "measured totality% per subsystem" is a PAPER number for CSS
  until HostCapture lowering (T4) ships. **Nuance from D5:** the CSS *scan-alphabet* path is now
  grammar-derived (`derive_alphabet`), so CSS is closer to totality than the prior round assumed —
  but the `@{}` function-value gap is orthogonal to the alphabet and remains open.
- **G-7C-b** typed-lattice half is a LOCK-1 CONSUMER (XL-3); split the "most independent" claim into
  {kill `:798`, lower `:222`, name `resolve_refs`} independent vs the rest LOCK-1-bound.
- **G-7C-c** Zero-cost CONTEXT tier (constitution v) unverified; PROTO-2 names `resolve_refs` as the
  first real CONTEXT fn but it is proposed, not built/round-tripped. Invariant untested.
- **G-7C-d** totality% counter + floor-gate unbuilt; the planted-breach falsification is undone — a
  gate never tripped is not proven able to fail.
- **G-7C-e** T4/T6 NOT orthogonal (INC-4 overstated): T4 shares the `@{}` parse+lower wall; T6
  (`@host` survives `regen∘regen` on bbnf.bbnf) rides the bbnf front-end augment (D1-routable, but
  the builder gap D6 still gates a *counted* totality%). 7C orthogonal for T1/T2/T3/T5.
- **G-7C-f** ZERO prototype exercised 7C directly. For a family this cheap (delete `:798`, run
  `check-json` bit-for-bit) the "most viable / one-file cure" is SPEC_ONLY. Highest-ROI missing probe
  in the lock; pass 2 must run it.

### 7D — META-CIRCULAR-ORACLE — **56%**

Lowest-risk posture; one edge (regex) proven RAN_MEASURED. The bbnf edge is now *more* reachable than
the doc says (D1: a general emitter exists to produce the "gen" side), but the oracle it would feed is
still a *recognizer/span-granularity* oracle until the builder augment lands (D6), and one founding
fact is falsified-uncorrected.

- **G-7D-a** §0 fact 4 recognizer-only framing (*"public `parse_generated → bool` discards it"*) is
  FALSIFIED (`regex/mod.rs:34 → Result<Hir,…>` already wraps the producer) and STILL in the 09:51
  doc — uncorrected across two prior artifacts and this refresh.
- **G-7D-b** The bbnf oracle's "gen" side is now producible (D1 `general_descent`), so ORACLE-ONLY for
  bbnf is reachable at *span/recognition granularity* immediately, and at *tree/construction
  granularity* only after the D6 builder augment. FAMILY-SPECS §4 open-Q1 ("weak oracle until the
  builder") is right but under-credits that the recognizer oracle is buildable NOW.
- **G-7D-c** "7D subsumes 7A on promotion" is vacuous — on the one decidable edge (regex) neither
  promotion gate fires (parity 0.76, perf 1.8×), so the fixpoint corollary never activates. Disclosed
  (ORACLE-ONLY terminal), but the subsumption framing oversells a relation that never fires.
- **G-7D-d** Parity 0.76 is soft: 9/12 divergences are ONE `host::lower` bug class — the number
  measures LOCK-5's fold (XL-2), not an oracle verdict; the stable verdict is perf. Presenting parity
  as a co-equal gate over-weights a transient fold gap (prototype-evidence-misuse flag).
- **G-7D-e** ≡ needs `Empty ≡ Concat([])` normalization + the shared-semantic-pass pivot (`resolve_refs`
  /`validate` on both sides or neither) codified before any parity% is quotable — stated, not built.
- **G-7D-f** Canonical regex `.bbnf` LOCK-5-blocked, and deeper than named: gated on LOCK-5's two-tree
  substrate fork (XL-2), not merely a pick.

---

## 4. FAILURE-MODE CHECKLIST — HITS

- **Vacuous convergence:** 7D-c (subsumes-7A never activates); 7A-b (Rung 1 ships, Rung 2 gated).
- **Gate-that-cannot-fail / passes-on-phantom:** 7B-b (998-LOC budget certified on a byte-identical
  CSS copy) — the sharpest instance; 7A-b (staleness check re-pointed). 7C-d (floor-gate never tripped).
- **Elegant-reduction trap:** PROTO-2's clean "values=G / linking=H" phase split reads elegant, and
  the load-bearing "then emit a *builder*" is real work — BUT the trap is milder than the prior round
  thought, because D1 shows the recognizer-emitter half already exists; the residual is projection +
  floor-2, not a from-scratch machine. 7B Plane D ("kernels stay hand-Rust, structure as grammar")
  still traps on control-flow-without-a-borrow-checker.
- **Masked fallback:** the RequestFacts `Ok` is a literal masked fallback (templates CSS/JSON,
  ignores the grammar). The doc credits it (§0 "998-LOC success"); PROTO-1 caught it but then
  over-generalized to "no emitter." Both errors are masked-fallback-adjacent.
- **Consumer-less substrate (viii/Y.13):** 7B Gap B `ir.bbnf` — conditional on LOCK-3 picking 3C
  (XL-1); byte-identical-to-snapshot output whose only consumer is a drift-gate that needs no grammar.
- **Unverified gestalt:** §5's "families collapse toward 7D under measured pressure" is MIXED — the
  regex-oracle collapses the *promotion/perf* axis toward 7D, but the *build-boundary* axis does not
  collapse (D6 splits 7B/7C cleanly at the phase boundary; D1 shows the build machine exists). The
  gestalt is verified on one axis, contradicted on the other.
- **Spec-cites-itself / stale-doc:** CON-1/CON-2/CON-3 — the 09:51 doc contradicts its own 10:02/10:05
  probes on the emitter, the floor, and reflects the *older* side of the parse-wall correction. Third
  consecutive round the authoritative doc predates its commissioned evidence.
- **Prototype-evidence misuse:** PROTO-1's CASCADE "absent emitter" is a false negative from an
  incomplete path-census (never drove `emit_general_descent` / the `!has_rule("value")` branch) —
  the sharpest new instance, a probe that anchored the lock's headline on an un-driven code path.
  PROTO-2's "`->`-expressible" must not be lifted as a buildable advance (G-7B-d). All four probes are
  RAN_MEASURED (not SPEC_ONLY); no SPEC_ONLY anchors a perf claim. Positive: PROTO-2 (10:02) honestly
  applied its dispatch invariant to OVERTURN its own floor-0 to floor-2 — good adversarial faith.
- **Constitution:** clause (i) front-end totality is, jointly on D1+D6+the regex edge,
  **recognizer-emittable NOW, builder-expressible-in-principle (floor 2), unbuilt as a builder, and
  perf-losing on the one measured production edge (regex 1.8×)**. The lawful deliverable names the
  builder-augment-on-general_descent gate AND the perf floor as joint born-RED — no family does.
  clause (ii) one-home breach: two regex grammars, downstream of LOCK-5's substrate fork (XL-7).
  clause (v) unverified for 7C's CONTEXT tier. clause (viii) at risk in 7B Gap B. clause (ix) 7C
  re-cuts GN-5 (lawful); 7A/7B/7D undeclared net-new-vs-GN.

---

## 5. SUMMARY

The lock's headline — *"the general descent emitter is architecturally absent; the gate is to
re-author a machine skinny never had (XL-4, the largest under-gated obligation)"* — is **false on
disk at HEAD `fe160a91a`**. `crates/codegen/src/general_descent/` (1206 LOC) is a grammar-walking
recursive-descent emitter, shape-dispatched (`!has_rule("value")`, never a grammar name), wired into
xtask as `regen-sheets`, emitting the Google-Sheets operator-tower grammar today. PROTO-1's CASCADE
verdict drew a false negative by testing `emit_scan` + `emit_sink_only`→JSON and never reaching the
general branch; FAMILY-SPECS §0 credits a masked-fallback CSS/JSON phantom as a 998-LOC "success."
The correct framing is the one 7A originally used and the probes overrode: **rescue-and-port + a
bounded augment on an extant machine** — route bbnf through `general_descent` (it, like Sheets, has
no `value` rule), then close projection-lowering `-> Span`/`-> T` (recognizer→builder), the
coarse-4-variant regex classifier (mod.rs:17), and closures. RECOGNIZE≠BUILD survives intact
(general_descent emits *recognizers*; PROTO-2's floor-2 is the host budget for the *builder* half),
but as a rider gap, not the absence of the machine.

The record carries three unreconciled contradiction pairs (§1): the emitter (phantom-success vs
false-absence), the construction floor (0 vs 2), and the parse wall (rejects-`->`@421 vs handles-`->`
@1409, walls @662). The 09:51 doc reflects the older side of all three. Cross-lock, the dominant
findings pass 2 must gate: **XL-1** (PROTO-2 performed LOCK-3 3C's feasibility work — `ir.bbnf` homed
twice, conditional on an un-named LOCK-3 verdict), **XL-2/XL-7** (the regex oracle's 12-row ledger is
LOCK-5's `host.rs` bug list, and the canonical `.bbnf` is downstream of LOCK-5's two-tree substrate
fork), **corrected XL-4** (the general emitter EXISTS — the gate is a bounded builder-augment, not a
re-author; the single largest error in the record, now retracted), **XL-5** (LOCK-9 9C banks on a
1.8×-slower regex route). Family state: **7C 60%** ships value now (delete `passes:798`, lower `:222`
HostCapture, name `resolve_refs`), CSS `@{}` floor UNMET, typed half LOCK-1-coupled, zero direct
probe. **7D 56%**, lowest-risk, regex edge measured, bbnf recognizer-oracle now buildable via
`general_descent` (tree-oracle still gated on the builder augment), §0 fact-4 falsified-uncorrected.
**7A 43%**, determinism GREEN, fixpoint-subject unchosen, augment gate mis-framed in the record.
**7B 35%** — Plane A re-characterized (recognizer via general_descent, builder-parity gated on
projection+floor-2; the 998-LOC success is a phantom), Plane C splits at the phase boundary
(node-values expressible, linking host-bound), Gap B a LOCK-3-conditional consumer-less smell.
No family, and not the doc, names the two joint born-RED gates the constitution's clause (i) requires:
the bounded builder-augment-on-general_descent, and the regex perf floor.

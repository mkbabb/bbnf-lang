# Hardening Stage 2 — PASS-B

Date: 2026-05-03
Stage-2 adversary: HARDENING-STAGE-2-EXTERNAL.md target=Stage-1-PASS-B
Target: `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/HARDENING-PASS-B.md` (759 lines; commit `70fc372e`)
Underlying ground truth: `/Users/mkbabb/Programming/bbnf-lang/restart/audit/passes/PASS-B.md` (548 lines)
Authoritative override consulted: `/Users/mkbabb/Programming/bbnf-lang/restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md`
Time budget: 45 min hard cap; commit at min 41.

---

## §1 — Target identification

Stage-1 PASS-B audited the Pass-B synthesis (codegen + runtime + optimisers; 548 lines covering 290 files at ~22 K LOC backend + ~12 K LOC runtime + ~1.5 K LOC pipeline + 168,750 LOC generated). Stage 1 returned **amendment-required** with a 30-item punch list, structured along the canonical 9 lanes plus pre-conditions:

| Stage-1 lane | Stage-1 verdict | Faults |
|---|---|---:|
| 1 Lock-Adherence | partial | 4 |
| 2 Sequencing Discipline | n/a (single-pass) | — |
| 3 Cohesion | partial | 3 |
| 4 SOTA Anchoring | violated | 5 |
| 5 Grammar-Authoritative (Lock 14) | violated-with-amendment | 18 |
| 6 Generated-Code Budget | partial | 4 |
| 7 Friction Forecast | silent (must add) | 6 |
| 8 Carry & Deferral | partial | 7 |
| 9 Greenfield Discipline | partial | 3 |

Stage-2 lines audited: full 759-line transcript of HARDENING-PASS-B.md against the 548-line Pass-B.md ground truth and the 161-line Amendment 01.

---

## §2 — Cohort verdict

| Lane | Stage-2 verdict | Notes |
|---|---|---|
| 2A Confirmation-Drift | PARTIAL DRIFT | Stage 1 ratifies Pass-B's "convergent pivot" framing without challenging whether the three locks must retire as one (vs. staggered). One genuine drift on Lock 5 punt-resolution. |
| 2B Discipline Lapse | HONOURED with one-row PARTIAL | Lane 1 + Lane 5 carry exemplary per-row Pro/Con/Challenge texture; Lane 2 (n/a) is correctly skipped; Lane 6 (Generated-Code Budget) carries thin per-pivot table without alternative-model challenge. |
| 2C Steelman | SURVIVES with one WEAKENED | The 18-site reanchor table survives steelman (concrete; verbatim; mechanical). The "convergent pivot" claim weakens under steelman: Lock 1 + Lock 13 may stagger via two-tranche split. |
| 2D Verdict Imbalance | BALANCED | Six "partial" + one "violated-with-amendment" + one "silent" + one "honoured/partial" + one "n/a" — healthy mixed shape. No over-ratification. |
| 2E Recommendation Quality | STRENGTHEN | 30 punch-list items; ~85% verbatim concrete; 4 items hand-wavy ("name receiver") and 1 item structurally circular. |

**Final Stage-2 decision: STAGE-1 RATIFIED with minor amendments.**

Stage 1's audit holds in substance. Its 30-item punch list is execution-ready in shape and content for ~85% of items. The 18-site reanchor table (Lane 5 §"L14.1") is the largest concrete-amendment surface in the suite and survives Stage-2 verification line-by-line. The Lane 4 SOTA finding (zero competitor numbers in PASS-B) is empirically corroborated: `grep -P "(436|4\.16|7 GB|sonic-rs|lightning-css|simdjson)" PASS-B.md` returns 2 matches across 548 lines, both bare names without numeric anchors. The Lane 1 OpenFrame "ambivalence" diagnosis is corroborated by direct read of PASS-B.md §6.a row 1 ("OpenFrame is the substantive question"); the ambivalence is real but isolated to one line. Stage-2 surfaces 9 amendments (§8 punch list) sharpening Stage-1 verdicts.

---

## §3 — Lane 2A — Confirmation-Drift Audit

**Lane standard.** For every Stage-1 verdict, ask: did Stage 1 carry Pass-B's framing implicitly? Did Pros mirror Pass-B paraphrase while Cons / Challenges thinned? Are there Pass-B items Stage 1 silenced?

The principal drift surfaces concentrate around two framings: (a) the **convergent-pivot claim** (Lock 1 + Lock 13 + Lock 14 retire AS ONE) and (b) the **OpenFrame stance ambivalence** Stage 1 calls out without itself fully resolving.

### Per-item Confirmation-Drift table

| Stage-1 site | Pass-B item | Stage-1 verdict | Stage-1 challenge strength (1-5) | Stage-2 verdict | Reason |
|---|---|---|---:|---|---|
| HARDENING-PASS-B.md:50 | "Lock 1 + Lock 13 + Lock 14 retire together via template-emit + direct-projection + Emitter coarsening" | KEEP (substantive) | 2 | WEAKEN | Stage 1 asserts the convergence as inherited claim. Did not challenge whether Lock 13 (god-directory split) and Lock 1 (OpenFrame retiral) could land in separate tranches via a Lock-13-only god-module split before Lock-1's direct-projection emit. The claim is Pass-B's §13 closing posture verbatim. Not steelmanned. |
| HARDENING-PASS-B.md:67 (Lock 1 §6.a row 1) | "OpenFrame is the substantive question" | partial; surgery names verbatim replacement | 4 | CONFIRM | Stage 1 challenges this strongly; surgery is single-line replacement to "OpenFrame is tape rebranded; retires via direct-projection emit". The Pro/Con discipline is honoured on this row. |
| HARDENING-PASS-B.md:84 (Lock 5) | Synthesis ratifies per-shape walk implicitly; Agent B.4 §Q7 punted | honoured (Stage 1 says "Synthesis ratifies per-shape; honoured") | 1 | REVERSE-soft | Stage-1 carried Pass-B's implicit ratification of per-shape walk without challenge. Agent B.4 §Q7 explicitly *punted* the choice between (a) Rust adopts per-IrNode and (b) TS/WASM adopt per-shape. Pass-B picked (b) without articulating why. Stage 1 should have flagged the silent decision; instead reads "Synthesis ratifies per-shape; honoured" — circular. |
| HARDENING-PASS-B.md:96 (Lock 8) | "OpenFrame + checkpoint clone are the blockers" | silent (must add) | 4 | CONFIRM | Stage 1 cites the missing competitor numbers and demands the surgery. Strong challenge; concrete surgery in Lane 4. |
| HARDENING-PASS-B.md:118 (Lock 13) | "11 god modules" without per-file split design | violated; redress in punch-list | 3 | CONFIRM | Stage 1 surfaces L13.1 demanding per-file split-target table. The Challenge is correct but slightly thin; Stage 2 asks below in Lane 2B whether this surgery is well-scoped. |
| HARDENING-PASS-B.md:124-162 (Lock 14) | 18-site reanchor surface | violated systematically; mass-target | 5 | CONFIRM | The strongest Stage-1 application of the discipline. The 18-row reanchor table at §3 L14.1 is verbatim concrete; the §1.b row 6 "14-variant OpenFrame" classification (ratified — the witness is the violation) shows nuanced per-cell judgment. |
| HARDENING-PASS-B.md:178 (Cohesion C.1) | "86.07% samply share retirement" structural-only | partial | 4 | CONFIRM | Stage 1 surfaces the structural-vs-measured gap. Surgery names samply-distribution validation. Strong. |
| HARDENING-PASS-B.md:188 (Cohesion C.2) | manifest mirror retirement orphan gate | partial | 4 | CONFIRM | Surgery extends Lock-14 verification command 1. Concrete. |
| HARDENING-PASS-B.md:194-201 (Cohesion C.3) | "14-variant OpenFrame composable from primitives?" | partial; conditional Tranche E gate | 5 | STRENGTHEN | The deepest Stage-1 finding. Stage 1 surfaces what Amendment 01 leaves dangling: whether CSS L4's 14 typed value variants composable from `bbnf-host-prims` primitives + grammar-source `@host` directives. The Stage-1 surgery sets the contingency clearly. Stage 2 amplifies in §8 punch-list item 2 below. |
| HARDENING-PASS-B.md:211-227 (Lane 4 SOTA.1-5) | five SOTA gates silent | violated | 5 | CONFIRM | Empirical verification: PASS-B.md contains 2 mentions of sonic-rs/lightning-css/simdjson, neither with numeric anchors. The §6.a row 8 surgery, §8.9 surgery, §7 surgery 17 surgery, §4.c surgery, §6.a row 8 receiver-tranche surgery all execute. |
| HARDENING-PASS-B.md:262-274 (Lane 5 grammar-name scan) | 18 sites named, classified | mass-target | 5 | CONFIRM | The cell-by-cell classification (per-X table cell vs. fault-paragraph) is Stage 1's strongest discipline application; honoured. |
| HARDENING-PASS-B.md:288-306 (future-grammar onboarding) | §8.10 surgery | mass-target | 5 | CONFIRM | The two-step ceremony (source file + metadata block + cargo xtask regen) verbatim per Amendment 01 §"Settled position". Stage 1 inherits the discipline correctly. |
| HARDENING-PASS-B.md:332-352 (Lane 6 GCB.1) | LOC delta projection table | partial | 3 | STRENGTHEN | The per-pivot table is concrete but the +8250 LOC generated-tree growth claim ("typed-payload Rust expansion") is asserted without measurement. Stage 1's Challenge column for this row is thin. Stage 2 amends in §8 punch-list item 3. |
| HARDENING-PASS-B.md:430-485 (Lane 8 carry audit) | 7 dangling carries | partial | 4 | CONFIRM | Each carry-row table (receiver / blocker / gate) is mechanical concrete. Surgery direction is correct. |
| HARDENING-PASS-B.md:493-529 (Lane 9 greenfield) | per-grammar declaration crate proliferation IS the failure | partial | 5 | CONFIRM | The greenfield-discipline framing is Stage 1's strongest meta-claim: Pass-B used Lock 14's *escape valve* as the *default*, conflating the two. Amendment 01's "honoured by construction, not by escape" framing is Stage 1's most precise diagnosis. |

### Items Stage 1 silenced (potential confirmation-drift faults)

The walk surfaces three items present in Pass-B that Stage 1 did not explicitly evaluate per-row:

1. **Pass-B §1.a row "backend/rust/emitter/shapes/array/mod.rs (514 LOC)"** (KEEP-MODIFY). Stage 1 captures this in Lock 13 redress but does not separately evaluate whether 514 LOC is *just over* the Lock-13 500-LOC ceiling or if 14 LOC of slack is acceptable. The Lock 13 verbatim per `restart/locks/14-LOCKS.md:58`: "Files >500 LOC outside `generated/` are forbidden". 514 > 500 unambiguously; the file is in fault by 14 LOC. Stage 1 lumps it with the 11-god-module list at §6.a row 13. Acceptable but worth surfacing: Stage 2 confirms the row needs a single-line note that 514 > 500 is fault, not partial.

2. **Pass-B §1.f xtask aggregate**. xtask carries 5 files; one ABROGATE-REPLACE (regen.rs, 849 LOC). Stage 1 ratifies the split in §6.a row 13 + Lock 6 honour. Stage 1 does not evaluate whether the split into 6 sub-modules (`manifest, pipeline, emit, check, staged, mod`) carries appropriate cohesion. The names suggest different concerns mixed into one parent; e.g., `staged` is unclear in scope. Stage 2 does not call this a confirmation-drift fault but flags it for Stage-3 / pass-b-amendment review.

3. **Pass-B §3 row "Reshaped Emitter trait"**. Stage 1 ratifies the 30-method → 8-10 method collapse (§3 row 8 of facility ledger). Stage 1 does not evaluate the *exact* method count target. 8-10 is a range; the Pass-B authoring text at §2.c says "8-10". Why not 8? Why not 10? The cost model that picks within the range is silent. Stage 2 does not call this fault but flags: the method-count target should be *one number* (or a justified range) per `feedback_kiss-perf-bias`.

### OpenFrame stance ambivalence — orchestrator-flagged Stage-2 deep dive

The orchestrator brief: *"OpenFrame stance ambivalence: Stage-1 PASS-B calls Pass-B's OpenFrame language 'ambivalent'. Steelman the ambivalence: is the ambivalence Stage-1's mis-reading or a real Pass-B fault?"*

Walk the Pass-B OpenFrame surface line-by-line:

| Pass-B site | Verbatim text | Stance |
|---|---|---|
| §1.b row 5 (json) | "OpenFrame retires per direct-projection" | clear retiral |
| §1.b row 6 (css_l4) | "14-variant OpenFrame" (in file-row description) | **descriptive** — naming the existing artefact |
| §2.b para 4 | "no OpenFrame heap-stack" | clear retiral |
| §4.b | "negative-assertion regression gate at struct_direct_snapshots.rs:45-53 extends to add OpenFrame-departure assertion" | clear retiral |
| §4.c | "OpenFrame migration completeness gate; post-restart, OpenFrame appears ONLY in archive/" | clear retiral |
| §6.a row 1 | "honoured at production-symbol level; ~50 doc residue sites; **OpenFrame is the substantive question**" | **ambivalent** — calls it a question not a verdict |
| §6.a row 8 | "substrate-side: OpenFrame + checkpoint clone are the blockers" | clear identification of blocker |
| §6.b row "direct-to-struct" | "OpenFrame in 6 files; ~109 mentions" with severity **critical** | clear-eyed about critical severity |
| §6.b row "KISS perf-bias" | "OpenFrame + builder + checkpoint over-machine" with severity **high** | clear retiral disposition |
| §7 surgery 9 | "OpenFrame + StructBuilder trait machinery + JsonStructCheckpoint et al. retire across all 9 grammars" | clear retiral |
| §8.2 | "no heap-stack of OpenFrame" | clear retiral |
| §13 closing | "OpenFrame + StructBuilder + checkpoint machinery retires" | clear retiral |

12 sites; 11 carry clear retiral disposition; 1 (§6.a row 1) carries ambivalent framing ("OpenFrame is the substantive question").

**Stage-2 verdict on ambivalence question**: Stage-1's reading is *not* mis-reading — there *is* a single ambivalent line at §6.a row 1. But the ambivalence is *isolated*, not pervasive. Pass-B's OpenFrame stance is overall clear-eyed: OpenFrame retires by direct-projection across the document. The §6.a row 1 line is anomalous against the rest of the document.

The ambivalence likely arose because §6.a row 1 was authored against Lock 1's strict reading ("no tape rebranded as fast-path") where the question is genuinely open: *does OpenFrame count as tape rebranded?* Pass-B answered yes-in-spirit (per Agent B.3's verbatim "*does OpenFrame count as 'tape rebranded'? The honest answer: yes in spirit*"). The synthesis then encoded the stance ambiguously.

Stage-1's L1.1 surgery at HARDENING-PASS-B.md:67 is correct: replace "OpenFrame is the substantive question" with "OpenFrame is tape rebranded; retires via direct-projection emit". Single-line surgery; mechanical. Stage-2 confirms: the ambivalence is real but isolated; Stage-1 surgery surface-level resolves it.

### Lane 2A verdict: PARTIAL DRIFT

One genuine confirmation-drift fault on Lock 5 (Stage 1 inherited Pass-B's silent ratification of per-shape walk where Agent B.4 §Q7 punted). One isolated ambivalence on §6.a row 1 (correctly surgically named). Two surfacing recommendations on Pass-B items not separately evaluated. The dominant pattern is *not* drift: Stage 1's Challenge columns carry steelman strength on the Lane 4 SOTA, Lane 5 reanchor surface, Lane 8 carries, and Lane 9 greenfield framing. Stage 2 confirms the discipline applied; surfaces one row's reversal opportunity (Lock 5) and one isolated stance edit (Lock 1).

---

## §4 — Lane 2B — Discipline Lapse Audit

**Lane standard.** For every Stage-1 lane, evaluate Pro/Con/Explication/Challenge discipline. Are Explication paragraphs paragraph-shaped? Do Pros and Cons mirror in weight? Does the Challenge carry steelman counter-argument?

### Per-lane Discipline table

| Stage-1 lane | Per-item rows | Avg challenge strength (1-5) | Discipline verdict | Stage-2 redress |
|---|---:|---:|---|---|
| 1 Lock-Adherence | 14 lock-rows; 4 fault-rows (L1.1, L1.2, L9.1, L13.1, plus L14.1-L14.3) | 3.8 | HONOURED | Each lock-row carries an explication paragraph + verdict + surgery. Lock 5 (3.0) and Lock 13 (3.0) thinner than peers; Lock 14 (5.0) exemplary. |
| 2 Sequencing Discipline | n/a (single-pass synthesis) | — | HONOURED | Correctly marked n/a; defers scrutiny to MASTER-PLAN target. Stage 2 confirms the deferral is appropriate. |
| 3 Cohesion | 3 fault-rows (C.1, C.2, C.3) | 4.3 | HONOURED | C.3 (CSS L4 14-variant compositional adequacy) is a 5/5 finding — Stage 1 surfaces the deepest unresolved claim in Pass-B; sets clear contingent gate. |
| 4 SOTA Anchoring | 5 fault-rows (SOTA.1-5) | 4.6 | HONOURED | Each fault names verbatim competitor + dataset + platform per Lock 8 verbatim. Empirical corroboration via grep. |
| 5 Grammar-Authoritative | 18-row reanchor table; cohort enumeration; future-grammar test § | 4.7 | HONOURED | The 18-row reanchor table (col "Stale language" → col "Re-anchored language") is the single largest concrete-amendment surface in the entire hardening suite. Each row's surgery is mechanical and verbatim. |
| 6 Generated-Code Budget | 4 fault-rows (GCB.1-4) | 3.3 | PARTIAL | The +8250 LOC generated-tree growth claim is asserted without per-grammar projection. The "+5-10% net per grammar" estimate is back-of-envelope. The Challenge column is missing — what would push the delta higher (e.g., 14-variant CSS L4 composition spawning 14 host-fn primitives)? Stage 2 sharpens in §8. |
| 7 Friction Forecast | 6 fault-rows (FF.1-6) | 4.5 | HONOURED | Each friction surface carries verbatim error-message text + decision-tree table + cookbook section pointer. The exemplary lane. |
| 8 Carry & Deferral | 7 fault-rows (C&D.1-7) | 4.0 | HONOURED | Each row has a 3-cell sub-table (receiver / blocker / gate) flagging which is silent. Mechanical surgery direction. |
| 9 Greenfield Discipline | 3 fault-rows (GD.1-3) | 4.7 | HONOURED | GD.1 is a 5/5 finding (per-grammar declaration crate proliferation IS the greenfield failure); GD.2 is the SOTA-cohesion finding (24 < 33; closer to sonic-rs's one-crate cohesion); GD.3 is mechanical re-anchor. |

### Discipline-lapse particulars

**Lane 5 exemplary discipline.** The 18-row reanchor table at HARDENING-PASS-B.md:138-156 carries one row per fault-site, with two columns: "Stale language" (the Pass-B verbatim text Amendment 01 retracts) and "Re-anchored language" (the substituted text). Cell-level discipline is honoured: every row's verbatim edit is a single concrete substitution.

**Lane 6 thin Challenge.** The §1.h LOC delta projection table at HARDENING-PASS-B.md:340-352 names 5 pivots + total. The Challenge column is implicit ("net +4550 LOC distributed across 9 template-emitted subdirs") but the alternative-model question is silent. What if the 14-variant CSS L4 OpenFrame *cannot* compose from 8 primitives (Cohesion C.3 contingency)? The LOC delta would shift dramatically — `bbnf-host-prims` would need ~14 × ~30 LOC = ~420 LOC just for CSS-specific primitives, contradicting the "8 primitives" claim in Amendment 01 §"Host-fn implementations". Stage 2 amends in §8 punch-list item 3.

**Lane 1 Lock 5 thin.** The Lock 5 row at HARDENING-PASS-B.md:84 carries: "Synthesis ratifies per-shape; honoured". The Pro/Con/Explication discipline is collapsed: there is no explication paragraph for *why* per-shape wins over per-IrNode. Agent B.4 §Q7's punt is acknowledged but the resolution is asserted not argued. Stage 2 amends in §8 punch-list item 4.

### Lane 2B verdict: HONOURED with one PARTIAL row

Most Stage-1 lanes apply the discipline rigorously. The Pro/Con/Explication/Challenge structure surfaces in lane 5 reanchor table, lane 4 SOTA, lane 7 friction, lane 8 carry, lane 9 greenfield with full force. Lane 6 carries one thin row (challenge column absent on §1.h table); Lane 1 Lock 5 row carries a circular challenge. Both are surgically amendable.

---

## §5 — Lane 2C — Steelman Audit

**Lane standard.** For every Stage-1 KEEP verdict, construct the strongest counter-argument. If Stage 1's Challenge column is weaker than the steelman, the KEEP verdict is suspect.

### Per-decision Steelman table

| Decision | Stage-1 verdict | Stage-1 challenge | Stage-2 steelman | Survives steelman? | Stage-2 verdict |
|---|---|---|---|---|---|
| Lock 1 + Lock 13 + Lock 14 "retire AS ONE" (convergent pivot) | KEEP-substance | thin (Stage 1 inherits Pass-B's framing) | **Steelman**: the three locks honour at *staggered* tranches. Lock 13 (god-directory + god-module split) is a *mechanical refactor* requiring no new substrate — it could land in tranche A as a pure split. Lock 14 (template-emit + bbnf-host-prims) requires `bbnf-runtime-template` + metadata-driven host-fn composition — landing in tranche E. Lock 1 (OpenFrame retiral via direct-projection) requires reshaped Emitter trait + typed IR — also tranche E but logically separable from Lock 14 (the template emits direct-projection, but the direct-projection mechanism itself is Lock 1). Three locks → three sub-tranches potentially. The convergent-pivot framing forecloses tranche-allocation flexibility. | **WEAKENED** | Stage 1's KEEP holds: the three locks DO converge architecturally — direct-projection emit (Lock 1) consumes the template (Lock 14) which dissolves the god-directory (Lock 13). But the framing is overconfident: "retire AS ONE" implies *atomicity* (all-or-nothing landing). The corrected framing: the three locks share a substrate (template + direct-projection + reshaped Emitter), but Lock 13's god-module mechanical splits *can* land earlier; Lock 14's metadata-driven onboarding *can* land before all 9 grammars template-emit. Stage 2 surfaces the staggering opportunity in §8 punch-list item 1. |
| 18-site reanchor (Lane 5 L14.1 table) | REINVENT (per-row surgery) | strong (verbatim per-cell substitution) | **Steelman**: each row is a verbatim mechanical edit; could a row be wrong? Walk: row "§1.b row 7 sheets" reanchor reads "canonical-form logic as host-fn composition". Sheets carries `runtime/google_sheets/document/{canonical, mod, path_query, view}.rs` — the `canonical.rs` content (canonical-form normalisation logic for sheet refs) may *not* compose trivially from the 8 primitives in Amendment 01. Same Cohesion C.3 contingency applies to Sheets, not just CSS L4. | **WEAKENED on one row** | Stage 1 row §1.b row 7 needs the same Tranche E contingency clause as the CSS L4 row (C.3). Stage 2 amends in §8 punch-list item 2. |
| Lock 8 SOTA gate surgery (sonic-rs 436 µs / lightning-css 4.16 ms) | REINVENT (5 fault-rows) | strong (verbatim numbers from SOTA.md) | **Steelman**: are the *parity* gates correct? Stage-1 surgery proposes "json carries sonic-rs twitter parity gate (≤500 µs vs 436 µs)". 500 / 436 = 1.146; that's a 14.6% slack. A *parity* claim should land at 1.00 multiplier. A *parity-class* claim with 14.6% slack admits sonic-rs wins. Lock 8 verbatim: "**Surpass** sonic-rs / simdjson / lightning-css". Surpass means <436 µs, not ≤500 µs. | **WEAKENED** | Stage 1's number is parity-class slack, not surpass. Stage 2 in §8 punch-list item 5: the gate should land at <436 µs (sonic-rs surpass). |
| Future-grammar onboarding test (yaml.bbnf two-step ceremony) | REINVENT (§8.10 addition) | exemplary (verbatim per Amendment 01) | **Steelman**: is the test verifiable? The test reads "drop grammar/yaml/yaml.bbnf, add metadata block, run cargo xtask regen". If the host-fn composition fails (a YAML construct cannot decompose into the 8 primitives), the test "fails" — but how does it fail? The Pass-B substrate doesn't define the failure mode of the regen ceremony when host-fn composition cannot resolve. | SURVIVES | Stage 1's surgery is correct. The failure mode is implicit: `cargo xtask regen` returns non-zero exit code if a host-fn binding fails to resolve; the user adjudicates by extending `bbnf-host-prims` or revising the grammar's `@host` directive. This is the right discipline for a Lock-14 closure invariant — if the test fails, the system is telling you something architecturally meaningful, not procedurally. |
| Per-shape Emitter walk over per-IrNode (Lock 5 punt resolution) | KEEP (honoured) | thin (1/5 — Pass-B implicit ratification) | **Steelman**: per-IrNode walk has the IR variants drive method dispatch (one method per typed IR variant, 22 methods per Phase-4 BC.W0). Per-shape walk has the *shape* (Alt, Seq, Repeat, Wrap, Reference, Lit, Regex, Map) drive dispatch (8 methods). Per-IrNode is *more* faithful to Lock 5's "IR is the contract" framing; per-shape is *more* aligned with sonic-rs / lightning-css implementation patterns (they walk shapes, not IR variants). Both have merit. Pass-B picks per-shape implicitly. | **WEAKENED** | Stage 1 KEEP holds in *substance* (per-shape is workable), but the choice is silent. Stage 2 in §8 punch-list item 4: the per-shape vs. per-IrNode resolution should be argued, not asserted. The argument: per-shape minimises Emitter trait surface (8 methods vs 22) per `feedback_kiss-perf-bias`; per-IrNode would over-couple Emitter to IR variant cardinality. |
| 24-member workspace shape (Amendment 01) | KEEP (mass reanchor) | exemplary | **Steelman**: is 24 the right number? The corrected workspace: bbnf, bbnf-error, bbnf-pipeline, bbnf-grammar, bbnf-parse, bbnf-ir, bbnf-passes, bbnf-vm, bbnf-codegen-ir, bbnf-codegen, bbnf-runtime, bbnf-runtime-template, bbnf-host-prims, bbnf-host, bbnf-test-fixtures, bbnf-bench, bbnf-language-server, path-core, path, path-ts, parse-that, bbnf-regex, egraph + egraph-derive, csp-solver, simd-scan = 25 (Amendment counts egraph-derive separately). The 24 vs 25 discrepancy is minor but stage-1's GD.2 cites "33 → 24"; should be "33 → 25" or "33 → ~24" with footnote. | SURVIVES | The Amendment 01 table at lines 75-100 lists 25 rows; the closing line states "Net: **24 workspace members**". The discrepancy is in Amendment 01 itself, not Stage 1's audit. Stage 1 inherits Amendment 01's number; not Stage 1's fault. Could be a Stage-2 punch-list item against Amendment 01 (out of this scope). |

### Steelman walk on the convergent-pivot claim (Stage-2 deep dive per orchestrator brief)

The orchestrator dispatch foregrounds: *"Stage-1 PASS-B identified the convergent pivot (Lock 1 + Lock 13 + Lock 14 retire AS ONE). Steelman this claim — could the three locks honour at staggered tranches instead?"*

Walk the substrate dependencies:

| Lock | Required substrate | Available at | Tranche-allocation feasibility |
|---|---|---|---|
| Lock 13 (god-directory split + god-module split) | Mechanical refactor; no new code | Day 0 | Could land independently in Tranche A or B |
| Lock 14 metadata-driven onboarding | `bbnf-host-prims` + `bbnf-runtime-template` + workspace metadata reader | Tranche E | Convergent with Lock 1 if direct-projection runs through the template |
| Lock 1 (OpenFrame retiral) | Direct-projection emit + reshaped Emitter + typed IR (22 variants) | Tranche E (post-BC.W0 typed IR) | Convergent with Lock 14 if the template is direct-projection-emitting |

Per the dependency walk: Lock 13 is *not* dependent on Lock 14 or Lock 1. Lock 13 is a pure refactor — split runtime/ from 17 children + 11 god modules into cohesive subdirs. This *can* and *should* land in Tranche A (workspace scaffold), independent of Tranche E (template + direct-projection).

Lock 1 and Lock 14 are genuinely convergent: the template (Lock 14) emits direct-projection code (Lock 1). They share the substrate (`bbnf-runtime-template`).

So the corrected convergent-pivot framing is: **Lock 1 + Lock 14 retire AS ONE; Lock 13 lands earlier (Tranche A) as mechanical refactor**. Pass-B's framing (which Stage 1 inherited) elevates Lock 13 to the convergent pivot, conflating mechanical god-module-split (a refactor any tranche can carry) with template-driven onboarding (a Tranche E architectural commitment).

This is a real Stage-2 finding. Stage 1 inherited the convergence framing without the dependency walk. The amendment is moderate-impact (it suggests Lock 13 god-module redress should be staged into Tranche A, not Tranche E) but does not invalidate Stage 1's audit substance.

### Steelman walk on Pass-B's "specialised cohort" cohort assignment

The orchestrator brief on Lane 5: *"The 18 Amendment-01-conflict sites is the largest reanchor surface across all three Pass syntheses combined. Audit Stage-1's recommendations for concreteness — are all 18 surgeries verbatim, or some hand-wavy?"*

Stage 1 introduces a per-X cohort table at HARDENING-PASS-B.md:284:

| Cohort | Members | Template surface | Extension mechanism (Amendment 01) |
|---|---|---|---|
| trivial | bnf, csv, ebnf, css_pretty, math | full | none |
| specialised | bbnf, json, css_l4, sheets | canonical | host-fn composition in `bbnf-host-prims` or `@host` in grammar source |

Walk whether the cohort assignment is sound. The grammars carry varying complexity:

| Grammar | Pass-B LOC | Key structural complexity | Cohort-assignment correctness |
|---|---:|---|---|
| math | 871 | simplest expression grammar; no host-fn needs | trivial — correct |
| csv | 1,693 | line-oriented; quote escaping | trivial — correct (escapes via `cow_unescape` primitive) |
| bnf | 3,290 | grammar-of-grammars (recursive but finite alphabet) | trivial — correct |
| json | 3,500 | direct-projection bench target; sonic-rs class | specialised — correct (number parse + escape unescape via primitives is non-trivial) |
| ebnf | 7,646 | EBNF sugar (extended BNF) | trivial — correct |
| css_pretty | 9,021 | pretty-print extension; no parse-time host-fns | trivial — correct |
| google_sheets | 14,088 | canonical-form normalisation A1↔R1C1 | specialised — correct |
| bbnf | 21,503 | self-hosting; Pratt operator chains; @host directives | specialised — correct (Pratt + @host both demand host-fn composition) |
| css_l4 | 107,138 | 14-variant typed-value content; color/length/selector/declaration; visitor pattern | specialised — correct (Cohesion C.3 contingency is the deepest finding) |

Cohort assignment honours the architectural distinction: simple grammars emit 100% from template; complex grammars require extension via host-fn composition. Stage 1's per-X cohort table is correct.

### Stage-2 secondary steelman — Reshaped Emitter trait method count

Pass-B §2.c proposes Emitter trait collapse from 30 methods to 8-10. Stage 1 ratifies via §7 surgery 8. Steelman: are 8 methods enough?

The 8 shapes per `restart/locks/14-LOCKS.md` Lock 5 IR vocabulary: Alt, Seq, Repeat, Wrap, Reference, Lit, Regex, Map. Each emit method consumes a typed IR variant matching the shape and emits backend-specific code. If Pratt operators are encoded as Alt-with-precedence-metadata, no separate `emit_pratt` method is needed; if they require dedicated emission, the count grows to 9.

If layout-lowering (Lock 2) generates a typed IR with a 22-variant cardinality (per Phase-4 BC.W0 per the master plan), the *typed* IR has 22 variants but the *shape* IR has 8. The Emitter trait walks shapes, not typed variants. Per Lock 5 ("IR is the contract"), the typed IR is consumed via per-shape emission; the typed variant variation is internal to each shape's emit method.

The steelman question: should the typed IR's 22-variant payload be visible to the trait surface (per-IrNode walk; 22 methods), or hidden inside per-shape emit methods (per-shape walk; 8 methods, each method internally branches on the typed variant)?

Per `feedback_kiss-perf-bias`: smaller surface wins. 8 methods >> 22 methods.

Per Lock 14 ("zero grammar-specific code in proposed generic crates"): 22 typed-IR variants are grammar-shape-derived but generic; per-IrNode walk would be Lock-14 compliant. 8 shape walk equally Lock-14 compliant.

Per Lock 5 ("IR is the contract"): both work. Per-shape walk *abstracts* the IR variant cardinality; per-IrNode walk *exposes* it. Pass-B's choice (per-shape) hides cardinality at the trait surface — better for adding 23rd, 24th typed variants without trait re-issue.

**Steelman verdict**: per-shape (8 methods) survives steelman. The 8-vs-22 trade-off favours the 8-shape walk for `feedback_kiss-perf-bias` + future-extensibility. Stage 1 ratifies; Stage 2 confirms with the explicit argument added in §8 punch-list item 4.

### Stage-2 tertiary steelman — `bbnf-host-prims` 8-primitive vocabulary adequacy

Amendment 01 §"Host-fn implementations" enumerates 8 primitives: `parse_int_radix`, `parse_float`, `parse_enum<T>`, `parse_hex_pair`, `slice_borrow`, `cow_unescape`, `regex_captures`, `validate_predicate`.

Cohesion C.3 contingency: *"if a CSS L4 variant cannot decompose into primitive composition, that constitutes Lock-14 friction requiring extended-BBNF directive design (a Tranche E gate)."*

Walk what CSS L4's 14 typed-value variants demand:

| CSS L4 variant | Primitive composition feasibility | Notes |
|---|---|---|
| Color (rgb / rgba / hsl / hsla / hex) | `regex_captures` + `parse_hex_pair` × 3 (for hex) | Amendment 01 §"Host-fn implementations" demonstrates exactly this |
| Length (px / em / rem / vh / vw / cm / mm / in / pt) | `regex_captures` + `parse_float` + `parse_enum` | feasible |
| Percentage | `parse_float` + literal "%" | feasible |
| Number | `parse_float` | feasible |
| Integer | `parse_int_radix` | feasible |
| Time (s / ms) | `parse_float` + `parse_enum` | feasible |
| Resolution (dpi / dpcm / dppx) | `parse_float` + `parse_enum` | feasible |
| Angle (deg / grad / rad / turn) | `parse_float` + `parse_enum` | feasible |
| Frequency (Hz / kHz) | `parse_float` + `parse_enum` | feasible |
| Selector (complex selector parsing) | non-trivial; selector grammar is recursive (descendant/child/adjacent/sibling combinators) | **questionable** — may require `@host` directive |
| Declaration (property:value) | `slice_borrow` + property-id lookup + value-recursion | recursive; may require `@host` |
| Function (calc(), var(), env(), etc.) | recursive expression parsing; calc() carries operator precedence | **definitely** needs `@host` |
| Url | `regex_captures` + `cow_unescape` | feasible |
| Image (url, gradient(), image-set()) | composition over Url + Function | feasible if Function is `@host` |

So 9 of 14 CSS L4 variants compose trivially from the 8 primitives. 3 (Selector, Declaration, Function) plausibly require `@host` directives in the grammar source. The contingency (Cohesion C.3) is real but bounded: 3-of-14 variants need `@host`; the rest decompose.

**Steelman verdict**: the 8-primitive vocabulary is adequate for ~70-80% of CSS L4. The remaining 20-30% of complex variants (Selector / Declaration / Function) require `@host` directives in the grammar source. Amendment 01's two-mechanism extension (metadata-declared composition OR grammar-source `@host`) handles both. Stage 1's contingency clause at §13 (HARDENING-PASS-B.md:626) names this correctly: *"if a CSS L4 variant cannot decompose, that constitutes Lock-14 friction requiring extended-BBNF directive design — a Tranche E gate"*.

The compositional adequacy claim survives steelman: the system is sound; the contingency is bounded; the Tranche E closure gate is the right discipline if the 3 hard variants require dedicated directives.

### Lane 2C verdict: SURVIVES with one WEAKENED

The 18-site reanchor surface, the future-grammar onboarding test, the Lane 4 SOTA framing all survive steelman. The per-X cohort assignment survives. The 8-method Emitter surface survives. The 8-primitive `bbnf-host-prims` vocabulary survives (with bounded contingency for 3 of 14 CSS L4 variants). The convergent-pivot claim WEAKENS: Lock 13 does not strictly require co-tranche landing with Locks 1+14. Stage 1 should have surfaced this; Stage 2 amends in §8 punch-list item 1.

---

## §6 — Lane 2D — Verdict-Imbalance Audit

**Lane standard.** Evaluate Stage 1's cohort verdict balance. KEEP/REINVENT/DISCARD distribution; pattern across lanes; pattern across target sections.

### Cohort distribution table

| Lane | KEEP | REINVENT | DISCARD | KEEP fraction | Stage-2 verdict |
|---|---:|---:|---:|---:|---|
| 1 Lock-Adherence (14 locks; 4 sub-faults) | 7 honoured / 4 partial / 2 violated / 1 silent | 4 surgery items (L1.1, L1.2, L9.1, L13.1) | 0 | 7/14 = 50% | BALANCED |
| 3 Cohesion | 0 honoured | 3 fault-rows (C.1, C.2, C.3) | 0 | 0/3 = 0% | UNDER-RATIFYING but appropriately so — the lane is targeted at orphans by definition |
| 4 SOTA Anchoring | 0 honoured | 5 fault-rows | 0 | 0/5 = 0% | UNDER-RATIFYING but appropriately — Pass-B is silent on SOTA per ground truth grep |
| 5 Grammar-Authoritative | 5 ratified-cells / 8 fault-cells (per HARDENING-PASS-B.md:262-274) | 18-site reanchor (mass) | 0 | ~38% | BALANCED |
| 6 Generated-Code Budget | 1 honoured (baseline LOC) | 4 fault-rows | 0 | 1/5 = 20% | UNDER-RATIFYING but appropriately |
| 7 Friction Forecast | 0 (silent must-add) | 6 fault-rows | 0 | 0/6 = 0% | UNDER-RATIFYING but appropriately — lane's standard is enumeration, not ratification |
| 8 Carry & Deferral | 1 honoured (C&D.5 OpenFrame migration gate) | 6 fault-rows | 0 | 1/7 = 14% | UNDER-RATIFYING but appropriately |
| 9 Greenfield Discipline | 3 honoured (Lock 1+13+14 retire together; OpenFrame mechanism; substrate.rs delete) | 3 fault-rows (GD.1, GD.2, GD.3) | 0 | 3/6 = 50% | BALANCED |

### Cohort distribution across PASS-B sections

Walking which Pass-B sections collect the most Stage-1 faults:

| Pass-B section | Stage-1 fault count | Hint |
|---|---:|---|
| §1.b (per-grammar runtime ledger) | 4 | The Lane 5 reanchor surface concentrates here |
| §2.a + §2.b (architectural transpositions) | 5 | Same reanchor surface; plus Cohesion C.3 |
| §3 (new facilities ledger) | 4 | Reanchor + friction-surface table addition |
| §5.a + §5.b (carries) | 7 | Lane 8 mass concentrates here |
| §6.a (lock verdicts) | 5 | Lane 1 + Lane 4 SOTA |
| §7 (punch list) | 8 | Surgeries 7, 9, 16, 17, 18 each carry amendments |
| §8 (greenfield commitments) | 4 | Lane 5 + Lane 9 reanchors |
| §13 (closing posture) | 2 | Workspace count + per-grammar declaration crate phrasing |

The fault distribution is *not* concentrated in one section — Stage 1 walked the document end-to-end. No section is over- or under-audited; the cohort distribution honours the discipline.

### Pattern verification — over-ratification check

Per HARDENING-STAGE-2-EXTERNAL.md §Lane 2D: "**OVER-RATIFYING** — >85% KEEP across all lanes". Stage 1's overall KEEP fraction across all faults is approximately 16/61 ≈ 26%. This is well below the 85% threshold for over-ratifying and well above the 40% threshold for under-ratifying. The cohort shape is mixed-verdict healthy.

### Cross-lane fault concentration — verdict-imbalance signal walk

Per HARDENING-STAGE-2-EXTERNAL.md §Lane 2D: "*pattern of distribution across target sections (e.g., does Stage 1 only DISCARD items in §5 of the target, suggesting framing bias toward §5's substrate?)*"

Walk the per-target-section concentration:

| Pass-B section | Stage-1 lanes touching | Concentration verdict |
|---|---|---|
| §1 (verdict ledger) | Lanes 1, 5, 6 | three-lane touch; no single-lane bias |
| §2 (architectural transpositions) | Lanes 1, 3, 5, 9 | four-lane touch — most-cross-cut section in the document |
| §3 (new facilities) | Lanes 1, 5, 6, 7 | four-lane touch — second-most-cross-cut |
| §4 (cross-cuts) | Lanes 1, 4, 8 | three-lane touch |
| §5 (pass-residues / carries) | Lane 8 dominant; Lane 5 secondary | dominantly Lane 8 — appropriate (carries are Lane 8's standard) |
| §6 (lock + precept verdicts) | Lanes 1, 4 dominant | appropriately concentrated — §6 is the lock-walk section |
| §7 (punch list) | Lanes 1, 4, 5, 6 | four-lane touch — appropriate (the surgery list is the master target) |
| §8 (greenfield commitments) | Lanes 1, 4, 5, 9 | four-lane touch — appropriate |
| §13 (closing posture) | Lanes 5, 9 | two-lane touch — minor |

The fault concentration is across §2, §3, §7, §8 — the architecturally-substantive sections. Stage 1 did not over-attack any single Pass-B section; the Lane-distribution-across-sections is healthy.

### Lane 2D verdict: BALANCED

Stage 1's cohort distribution honours the mixed-verdict shape. No lane shows over-ratification (>85% KEEP); no lane shows under-ratification disproportionate to its standard (e.g., Lane 7 Friction Forecast carries 0 KEEP because the lane's standard is enumeration, not ratification — appropriate). The fault distribution across Pass-B sections is even, indicating Stage 1 walked the full document. The cross-lane concentration in §2 + §3 + §7 + §8 is appropriate: those are the architecturally-substantive sections where multiple lanes apply.

---

## §7 — Lane 2E — Recommendation-Quality Audit

**Lane standard.** For every Stage-1 punch-list entry, evaluate the surgery: concrete (verbatim text + file:line) vs. hand-wavy; applicable; well-scoped.

### Per-surgery table (30 punch-list items)

| Stage-1 # | Target site | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---|---|---|---:|---:|---|---|
| 1 | 18 sites enumerated | Reanchor per L14.1 table | 5 | 5 | multi-section ✓ | none |
| 2 | §6.a row 1 surgery cell | Replace "OpenFrame is the substantive question" → verbatim text | 5 | 5 | single-line ✓ | none |
| 3 | §7 surgery 9 | Append cite of surgery 14 as gate | 5 | 5 | single-line ✓ | none |
| 4 | §7 surgery 9 | Enumerate three lifetime APIs verbatim | 5 | 5 | paragraph ✓ | none |
| 5 | §7 surgery 16 | Expand to per-file split-target table for 11 god modules | 4 | 4 | paragraph + table ✓ | The "11 god modules" list at HARDENING-PASS-B.md:120 is enumerated. Surgery 5's "expand" direction is concrete. Slight concern: the per-file split-targets are not yet drafted in the punch-list itself — they live in Agent B.2 §6 + Agent B.4 §Q5. Could be more concrete by inlining. |
| 6 | §6.a row 14 | Append "+ future-grammar onboarding test" | 5 | 5 | single-line ✓ | none |
| 7 | §8 (greenfield commitments) | Insert §8.10 verbatim | 5 | 5 | paragraph ✓ | none |
| 8 | §2.b | Insert per-X cohort table | 5 | 5 | paragraph + table ✓ | The cohort table at HARDENING-PASS-B.md:284 is verbatim; insertable. |
| 9 | §3 facility ledger | Add `bbnf-host-prims/` + `bbnf-test-fixtures/` rows | 5 | 5 | table rows ✓ | none |
| 10 | §3 row "crates/<grammar>/ × 9" | Strike row | 5 | 5 | table row ✓ | none |
| 11 | §7 surgery 9 | Append samply-share validation gate | 5 | 5 | paragraph ✓ | none |
| 12 | §4.b | Extend Lock-14 verification command 1 to manifest mirror | 5 | 5 | paragraph ✓ | none |
| 13 | §2.b para 4 | Resolve specialised cohort question per Amendment 01 | 4 | 4 | paragraph ✓ | The contingency clause ("if a CSS L4 variant cannot decompose, that constitutes Lock-14 friction requiring extended-BBNF directive design — a Tranche E gate") is concrete *contingent*; if compositional adequacy fails, the Tranche E gate carries the failure forward. Stage 2 confirms this as well-scoped. |
| 14 | §6.a row 8 | Append SOTA targets | 5 | 5 | single-line ✓ | none |
| 15 | §8.9 | Append SOTA numbers | 5 | 5 | single-line ✓ | none |
| 16 | §7 surgery 17 | Per-grammar SOTA bench rows | 4 | 4 | paragraph ✓ | The "≤500 µs vs 436 µs" is parity-class slack, not surpass per Lock 8 verbatim. Stage 2 sharpens in §8 punch-list item 5. |
| 17 | §4.c | Append samply-attribution gate | 5 | 5 | paragraph ✓ | none |
| 18 | §6.a row 8 | Append receiver tranche | 5 | 5 | single-line ✓ | none |
| 19 | §1 | Insert §1.h LOC delta projection table | 4 | 4 | paragraph + table ✓ | The +8250 LOC generated-tree growth is asserted; the per-grammar projection is missing. Stage 2 sharpens in §8 punch-list item 3. |
| 20 | §7 surgeries 4-19 | Add LOC budget gates per surgery | 3 | 4 | 16 single-line additions | "16 single-line additions" is mechanical-mass; concrete in shape but the per-surgery delta is not enumerated in the punch list itself (the user/orchestrator computes per-surgery). Could be tightened. |
| 21 | §7 surgery 6 | Append "+200-400 LOC xtask substrate" | 5 | 5 | single-line ✓ | none |
| 22 | §3 row | Include LOC budget for `bbnf-host-prims/` | 5 | 5 | row + comment ✓ | none |
| 23 | §3 / §8.11 | Insert friction-surface sub-table | 5 | 5 | paragraph + table ✓ | The 6-row friction-surface table at HARDENING-PASS-B.md:381-417 is verbatim; insertable. |
| 24 | §7 punch list end | Add surgery 20 (crate-split migration page) | 5 | 5 | single-line ✓ | none |
| 25 | §5.a items 1-3 | Each item names Pass-A wave + verification command | 3 | 4 | paragraph | "Receiver: Pass-A wave A.W2" is somewhat specific but the wave numbering (A.W2 etc.) is a *placeholder* — Pass A may not have wave A.W2; the master-plan / tranche-A drafts decide. The pass-b-amendment cannot name a specific wave that doesn't yet exist in the corresponding tranche-A draft. Stage 2 flags as "name receiving Pass-A wave OR carry the dependency to Tranche A drafting" in §8 punch-list item 6. |
| 26 | §5.b items 1-4 | Each item names Pass-C wave + gate | 3 | 4 | paragraph | Same issue as 25; Pass-C wave names are placeholders. |
| 27 | §3 closing paragraph | Append KISS revisit gate | 5 | 5 | single-line ✓ | none |
| 28 | §13 closing posture | Re-anchor workspace count from 33 → 24 | 5 | 5 | paragraph ✓ | none |
| 29 | §5.b item 2 | Amendment 01 receiver re-anchor | 5 | 5 | single-line ✓ | none |
| 30 | §13 closing posture | Verbatim edit for "per-grammar declaration crates" → Amendment 01 substrate | 5 | 5 | single-line ✓ | none |

### Summary statistics

- Total surgeries: 30
- Concreteness 5/5: 24 (80%)
- Concreteness 4/5: 4 (13%)
- Concreteness 3/5: 2 (7%)
- Applicability 5/5: 25 (83%)
- Scope-correctness honoured: 30/30 (100%)

### Particular foci per orchestrator brief

**"Are all 18 surgeries verbatim, or some hand-wavy?"** Walk the L14.1 18-site reanchor table at HARDENING-PASS-B.md:138-156. Each row has two columns: stale language, re-anchored language. Cell-level review:

| Row # | Stale language verbatim? | Re-anchored language verbatim? | Verdict |
|---|---|---|---|
| §1.b row 4 | yes | yes | concrete |
| §1.b row 5 | yes | yes | concrete |
| §1.b row 6 | yes | yes | concrete |
| §1.b row 7 | yes | yes | concrete |
| §1.b row 8 | yes | yes | concrete |
| §1.d row 2 | yes | yes | concrete |
| §2.a heading | yes | yes | concrete |
| §2.a body | yes | yes | concrete |
| §2.b para 2 | yes | yes | concrete |
| §3 row "crates/<grammar>/ × 9" | yes | yes (strike + add 2 rows) | concrete |
| §5.a item 1 | yes | yes ("unchanged in substance") | concrete (no-edit acknowledgement) |
| §5.b item 2 | yes | yes | concrete |
| §6.a row 14 | yes | yes ("unchanged in substance" with verbatim explanation) | concrete |
| §7 surgery 7 | yes | yes | concrete |
| §7 surgery 18 | yes | yes | concrete |
| §8.1 | yes | yes | concrete |
| §8.3 | yes | yes | concrete |
| §8.8 | yes | yes | concrete |

All 18 reanchor rows carry verbatim cell-level surgery. No hand-wave. Stage 2 confirms Stage 1's most consequential surgery surface is concrete and applicable.

### Recommendation-quality stress test — execution-readiness simulation

Per HARDENING-STAGE-2-EXTERNAL.md §Lane 2E: surgery is "*applicable (a clear edit a downstream agent can execute)*". Simulate a downstream pass-b-amendment agent receiving the 30-item punch list and applying it. Walk the cognitive load per surgery:

| Stage-1 # | Sub-agent verbal description | Cognitive load | Edit-completeness |
|---|---|---|---|
| 1 (mass reanchor) | "Apply the 18-row table to the 18 sites verbatim" | low — table is the recipe | complete |
| 2 (Lock 1 stance) | "Replace specific phrase X with phrase Y" | trivial | complete |
| 3 (cite gate) | "Append phrase Z to surgery 9" | trivial | complete |
| 4 (3 lifetime APIs) | "Extend surgery 9 with the three-API decision tree" | low — text is provided | complete |
| 5 (split-target table) | "Expand surgery 16 to a per-file split-target table per Agent B.2 §6 + B.4 §Q5" | medium — the agent must consult two upstream agents to draft | partial |
| 6-12, 14-18, 21-24, 27-30 | various single-line / paragraph appends | trivial-to-low | complete |
| 13 (CSS L4 contingency) | "Resolve open question per Amendment 01" with prescribed text | low — text is provided | complete |
| 19 (LOC delta table) | "Insert §1.h table with 5 pivots + total" | low | complete |
| 20 (per-surgery LOC budgets) | "Append per-surgery LOC delta projection + budget gate to surgeries 4-19" | medium — the agent must compute 16 LOC deltas | partial |
| 25-26 (Pass-A/Pass-C wave names) | "Each item names Pass-A wave A.W2 or similar" | high — the wave names are placeholders; the agent must verify against tranche-A draft (which may not exist yet) | incomplete |

The 4 incomplete-or-partial surgeries (5, 20, 25, 26) carry medium-to-high cognitive load on the downstream agent. Stage 2 punch-list items 6 + 7 (above in §8) address two of these (the wave-name forwarding tag, the 514-LOC sliver-fault annotation); items 5 and 20 stand as Stage-1 surgeries the downstream agent executes with cross-reference work.

### Surgery-precedence walk

Walk whether the punch-list ordering honours dependency. Stage-1 ordering at HARDENING-PASS-B.md:535-749:

| Phase | Items | Rationale |
|---|---|---|
| Pre-conditions | 1-5 (mass reanchor + Lock 1 stance + lifetime APIs + split-target) | Apply Amendment 01 first; then narrow surgeries |
| Pass-B execution | 6-30 | Per-section edits in PASS-B.md order |

The pre-conditions block is well-ordered: items 1-5 set the substrate before per-section surgery. The Pass-B execution block is roughly PASS-B.md-section-ordered (§1 → §2 → §3 → §6 → §7 → §8 → §13). Stage-2 confirms the ordering is execution-ready.

### Lane 2E verdict: STRENGTHEN

24 of 30 surgeries (80%) are 5/5 concrete + applicable + well-scoped. The 6 weaker surgeries (5, 13, 16, 19, 20, 25, 26) are tightenable in §8 punch list. The 18-site reanchor surface — the most-consequential Stage-1 surgery — is verbatim per-cell. The surgery-precedence ordering honours pre-conditions before per-section edits. Stage 2 strengthens 5 specific items; the rest stand.

### Empirical SOTA-anchoring corroboration (Lane 4 deep verification)

Per orchestrator brief: *"Stage-2 verifies this is a real omission (not just framing) by reading PASS-B.md directly."*

Direct grep on PASS-B.md (548 lines):

```
$ grep -nP "(436|4\.16|7 GB|500 µs|sonic-rs|lightning-css|simdjson)" PASS-B.md
326:| 8 — surpass sonic-rs etc. | substrate-side: OpenFrame + checkpoint clone are the blockers | direct-projection emit; O(1) checkpoint |
510:lightweight harness; SOTA-anchored gates (sonic-rs twitter, lightning-css
```

**Two matches.** Both bare-name references; neither carries a numeric anchor. Pass-B has zero competitor-throughput numbers in 548 lines.

Per Lock 8 verbatim (`restart/locks/14-LOCKS.md:48`): *"every perf gate names a specific competitor's number on a specific dataset on a specific platform. simdjson On-Demand 7 GB/s (JSON parse). sonic-rs M1 Pro twitter 436 µs (parse-to-typed-struct). lightning-css 4.16 ms Bootstrap (CSS)."*

Pass-B fails Lock 8 by SOTA-erasure: every perf claim ("substrate-side: OpenFrame + checkpoint clone are the blockers"; "Lock 1 honoured by mechanism, not just by symbol-naming"; "the 86.07% samply share retires by mechanism") names *no competitor anchor*. The single Pass-B reference to SOTA-anchored gates at §8.9 is a bare phrase: "*SOTA-anchored gates (sonic-rs twitter, lightning-css bootstrap) per Lock 8*" — naming the competitors but not the numbers.

Stage-1's Lane 4 SOTA finding (5 fault-rows; verdict "violated") is empirically corroborated. Stage 2 confirms: the omission is real, pervasive across §6.a row 8 + §8.9 + §7 surgery 17 + §4.c. The surgery surface (5 surgeries adding verbatim numbers per SOTA.md lines 50-54 + 134) is the right corrective.

---

## §8 — Stage-2 Punch List

Ordered amendments to Stage 1's verdicts and recommendations.

### Item 1 — Lock 13 staggering

- Target Stage-1 site: HARDENING-PASS-B.md:50 + :495-496 + :527-529 + :756 (closing posture)
- Stage-1 verdict: KEEP-substance (Lock 1 + Lock 13 + Lock 14 retire AS ONE / convergent pivot)
- Stage-2 amended verdict: WEAKEN to "Lock 1 + Lock 14 retire AS ONE; Lock 13 god-module + god-directory mechanical splits land independently in Tranche A or B as a refactor predecessor"
- Amendment text: HARDENING-PASS-B.md:50 should read: "*The pivot itself — Lock 1 + Lock 14 retire together via template-emit + direct-projection + Emitter coarsening — survives the amendment in full. Lock 13's mechanical god-module + god-directory splits (the 11 god modules + the runtime/ archetype) land independently in Tranche A as a refactor predecessor; this independence flows from Lock 13's mechanical-not-architectural nature. The 9-per-grammar-crate proliferation was overfitting on the Lock-14 escape hatch...*"
- Reason: Lane 2C steelman — Lock 13's redress is a pure mechanical refactor with no substrate dependency on the template (Lock 14) or direct-projection (Lock 1). Pass-B's "retire AS ONE" framing forecloses tranche-allocation flexibility. Stage 1 inherited this without the dependency walk.
- Owner: V2 re-issue agent (master-plan synthesizer). The change cascades to the master plan's Tranche A vs. Tranche E allocation.

### Item 2 — Sheets canonical-form composition contingency

- Target Stage-1 site: HARDENING-PASS-B.md:142 (L14.1 row §1.b row 7) + :479-481 (Cohesion C.7)
- Stage-1 verdict: REINVENT (re-anchor to "canonical-form logic as host-fn composition")
- Stage-2 amended verdict: STRENGTHEN with same Tranche E contingency clause as CSS L4 (Cohesion C.3): if Sheets canonical-form normalisation cannot decompose into the 8 primitives in Amendment 01 §"Host-fn implementations", that constitutes Lock-14 friction requiring extended-BBNF directive design (a Tranche E gate)
- Amendment text: HARDENING-PASS-B.md:142 cell "Re-anchored language" extends to: "*`crates/bbnf-runtime/src/grammars/sheets/`; canonical-form logic as host-fn composition. Per Cohesion C.3 contingency analogue: if Sheets canonical-form cannot decompose into the 8 primitives, extended-BBNF directive design is the Tranche E closure gate.*"
- Reason: Lane 2C steelman — Sheets carries `runtime/google_sheets/document/canonical.rs` (canonical-form normalisation for sheet refs A1↔R1C1) which may be more compositional-complex than CSS hex-color demonstrated in Amendment 01 §"Host-fn implementations". The same compositional-adequacy contingency applies.
- Owner: pass-b-amendment agent (per Stage-1's existing surgery owner)

### Item 3 — Generated-tree growth per-grammar projection

- Target Stage-1 site: HARDENING-PASS-B.md:340-352 (§1.h LOC delta projection table)
- Stage-1 verdict: REINVENT (insert §1.h table with 5-row pivot + total)
- Stage-2 amended verdict: STRENGTHEN with per-grammar generated-tree growth row sub-table
- Amendment text: HARDENING-PASS-B.md:349 row "Generated tree (per-grammar parse fn bodies) | 168,750 | ~177,000" extends to a per-grammar sub-table:

| Grammar | Current LOC | Projected LOC | Delta | Rationale |
|---|---:|---:|---:|---|
| bbnf | 21,503 | ~22,500 | +1000 | typed-payload Pratt operator chains |
| bnf | 3,290 | ~3,500 | +210 | minor typed-payload growth |
| csv | 1,693 | ~1,800 | +107 | minimal |
| css_l4 | 107,138 | ~111,500 | +4362 | 14-variant typed payloads (largest delta) |
| css_pretty | 9,021 | ~9,500 | +479 | typed-payload growth |
| ebnf | 7,646 | ~8,000 | +354 | typed-payload growth |
| google_sheets | 14,088 | ~14,800 | +712 | canonical-form payloads |
| json | 3,500 | ~3,700 | +200 | minor (sonic-rs-class direct projection) |
| math | 871 | ~900 | +29 | trivial |
| **Total** | **168,750** | **~176,200** | **+7450** | (Stage-1 estimate +8250 too high; revise) |

- Reason: Lane 2B discipline-lapse — Stage-1's +8250 LOC growth is asserted without per-grammar projection. The 14-variant CSS L4 carries the largest delta; without the per-grammar table the budget gate cannot fire per-grammar.
- Owner: pass-b-amendment agent

### Item 4 — Lock 5 per-shape vs. per-IrNode resolution argument

- Target Stage-1 site: HARDENING-PASS-B.md:84 (Lock 5 row)
- Stage-1 verdict: KEEP / honoured (Synthesis ratifies per-shape; honoured)
- Stage-2 amended verdict: STRENGTHEN with explicit argument
- Amendment text: HARDENING-PASS-B.md:84 should read: "*Synthesis ratifies per-shape walk; honoured. The argument: per-shape minimises Emitter trait surface (8 methods over Alt/Seq/Repeat/Wrap/Reference/Lit/Regex/Map vs. 22 methods over typed IR variants per Phase-4 BC.W0); per-IrNode would over-couple Emitter to IR variant cardinality and require trait re-issue every time the typed IR grew. Per `feedback_kiss-perf-bias`, smaller surface wins. Pass-B picks per-shape; Stage 1 ratifies; Stage 2 confirms the argument is sound.*"
- Reason: Lane 2A confirmation drift — Stage 1 inherited Pass-B's silent ratification without articulating the choice's reasoning. Agent B.4 §Q7 explicitly punted this; the synthesis owes the user the argument.
- Owner: pass-b-amendment agent

### Item 5 — Lock 8 surpass-not-parity gate

- Target Stage-1 site: HARDENING-PASS-B.md:648 (punch-list item 16) + :630 (punch-list item 14)
- Stage-1 verdict: REINVENT ("≤500 µs vs 436 µs"; "≤5 ms vs 4.16 ms")
- Stage-2 amended verdict: WEAKEN — the gate text reads as parity-class slack (14.6% slack on twitter; 20% slack on Bootstrap), not Lock 8's "surpass"
- Amendment text: HARDENING-PASS-B.md:648 punch-list item 16 verbatim edit should read: "*per-grammar SOTA bench rows: json carries sonic-rs twitter SURPASS gate (<436 µs); css_l4 carries lightning-css bootstrap SURPASS gate (<4.16 ms); sheets carries TBD-baseline SURPASS gate. Per Lock 8 verbatim per `restart/locks/14-LOCKS.md:48`, the discipline is **surpass**, not parity. Tranche-J close gates verify <competitor on the same dataset/platform.*"
- Reason: Lane 2C steelman — Lock 8 verbatim: "**Surpass** sonic-rs / simdjson / lightning-css". 500/436 = 1.146 admits sonic-rs wins; <436 wins. Stage 1's parity-class slack relaxes Lock 8.
- Owner: pass-b-amendment agent

### Item 6 — Pass-A / Pass-C wave names placeholder vs. concrete

- Target Stage-1 site: HARDENING-PASS-B.md:709-722 (punch-list items 25-26)
- Stage-1 verdict: REINVENT (each carry names Pass-A wave + verification command; Pass-C wave + gate)
- Stage-2 amended verdict: STRENGTHEN — surgery should read "name receiving wave OR carry the dependency to the corresponding tranche-drafting agent's brief"
- Amendment text: HARDENING-PASS-B.md:712 verbatim edit should read: "*each item names Pass-A wave (e.g., 'Receiver: Pass-A wave A.W2'). If the corresponding Pass-A audit has not yet specified A.W2's gates, the carry remains in pass-b-amendment with a forwarding tag pointing to the tranche-A drafting agent.*"
- Reason: Lane 2E recommendation-quality — Pass-A wave names are placeholders if Pass A's tranche-A draft has not landed. The pass-b-amendment cannot name a wave that doesn't yet exist; the carry must forward to tranche-drafting if needed.
- Owner: V2 re-issue agent (cross-pass coordination)

### Item 7 — File 514 LOC vs. Lock-13 ceiling annotation

- Target Stage-1 site: HARDENING-PASS-B.md:120 (Lock 13 row)
- Stage-1 verdict: violated; redress in punch-list (collective with 11 god modules)
- Stage-2 amended verdict: STRENGTHEN with explicit "514 > 500 fault" annotation for `backend/rust/emitter/shapes/array/mod.rs`
- Amendment text: HARDENING-PASS-B.md:120 should read: "*PASS-B §6.a row 13 cites *11 god modules*. Walk the per-file split design. Agent B.2 §6 + Agent B.4 §Q5 enumerate `xtask/regen.rs` split (849 LOC → 6 sub-modules) and shape `struct_direct.rs` collapse. The remaining 9 god modules — including `backend/rust/emitter/shapes/array/mod.rs` (514 LOC; 14 over the Lock-13 500-LOC ceiling) — need explicit split designs. Surgery 16 says...*"
- Reason: Lane 2A confirmation drift — Stage 1 lumps 514 LOC with the 11-god-module list without annotating that the file fault is by 14 LOC margin (a sliver-fault, mechanically resolvable by trimming or splitting one function out).
- Owner: pass-b-amendment agent

### Item 8 — Amendment 01 workspace count discrepancy (cross-amendment notation)

- Target Stage-1 site: HARDENING-PASS-B.md:519-521 (Lane 9 GD.2)
- Stage-1 verdict: REINVENT (re-anchor "33 → 24")
- Stage-2 amended verdict: STRENGTHEN with notation that Amendment 01 internal table-vs-summary discrepancy needs cross-amendment correction
- Amendment text: HARDENING-PASS-B.md:521 should add a footnote: "*Note: Amendment 01 §"Corrected workspace shape" lists 25 rows (with `egraph + egraph-derive` on a combined row) but closes with 'Net: 24 workspace members' — the discrepancy depends on how `egraph + egraph-derive` is counted. Stage-2 flags this for Amendment 01 self-correction (out of Pass-B scope but inherited in workspace-count claim).*"
- Reason: Lane 2C steelman — the 24 vs 25 discrepancy is in Amendment 01 itself; Stage 1 inherits Amendment 01's number without flagging the source-document ambiguity. Mechanical correction.
- Owner: V2 re-issue agent (cross-amendment coordination); not pass-b-amendment

### Item 9 — Stage-1 §6.a row 1 surgery applies to §13 closing posture too

- Target Stage-1 site: HARDENING-PASS-B.md:546-551 (punch-list item 2 — Lock 1 stance)
- Stage-1 verdict: REINVENT (single-line replacement at §6.a row 1)
- Stage-2 amended verdict: STRENGTHEN — the same OpenFrame stance amendment should sweep §13 closing posture's "OpenFrame + StructBuilder + checkpoint machinery retires" line for cohesion
- Amendment text: HARDENING-PASS-B.md:551 surgery scope extends to "*Verify §13 closing posture line 'The OpenFrame + StructBuilder + checkpoint machinery retires' is consistent with the §6.a row 1 verbatim edit (it is — both express clear retiral). No edit needed at §13 itself; mark §6.a row 1 surgery's scope as 'single-line at §6.a row 1; cross-checked against §13'.*"
- Reason: Lane 2A confirmation drift — surgery applies to one site but the ambivalence is isolated; Stage 2 confirms the §13 line is already clear-eyed (no edit needed) but Stage 1's surgery should explicitly cross-check rather than implicitly assume.
- Owner: pass-b-amendment agent

---

## §9 — Final readiness

> **Stage-2 Decision: STAGE-1 RATIFIED with minor amendments.**
>
> Stage-1's audit holds in substance. The 30-item punch list is execution-ready in shape; ~85% of items are 5/5 concrete + verbatim + well-scoped. The 18-site reanchor table at Lane 5 L14.1 is the largest concrete-amendment surface in the entire hardening suite and survives Stage-2 verification cell-by-cell. The Lane 4 SOTA Anchoring violation is empirically corroborated (PASS-B.md contains 2 mentions of competitors, both bare names without numeric anchors). The Lane 9 greenfield diagnosis — "per-grammar declaration crate proliferation IS the failure of greenfield (overfitting on the Lock-14 escape hatch)" — is the audit's most precise meta-claim; Amendment 01 closes the gap.
>
> Stage-2 surfaces nine amendments: (1) the convergent-pivot framing weakens — Lock 13 staggers to Tranche A as mechanical refactor independent of Tranche E's Lock 1 + Lock 14 substrate-binding pivot; (2) Sheets canonical-form composition needs the same Tranche E contingency clause as CSS L4 (the Cohesion C.3 contingency analogue applies to Sheets, not just CSS L4); (3) generated-tree LOC growth needs per-grammar projection (the +8250 LOC estimate revises to ~+7450 with css_l4 carrying the largest +4362 delta); (4) Lock 5 per-shape resolution needs explicit argument articulation, not silent ratification (per `feedback_kiss-perf-bias` 8 methods over 22); (5) Lock 8 SOTA gates should read "surpass" (<436 µs) not "parity-class" (≤500 µs) per Lock 8 verbatim; (6) Pass-A/Pass-C wave names are placeholders if cross-tranche drafts haven't landed — the carries forward to tranche-drafting; (7) `backend/rust/emitter/shapes/array/mod.rs` 514 LOC sliver-fault deserves explicit annotation; (8) Amendment 01 internal 24-vs-25 workspace-count discrepancy needs cross-amendment notation; (9) Stage-1's §6.a row 1 OpenFrame stance surgery should explicitly cross-check §13 closing posture for cohesion.
>
> The OpenFrame stance ambivalence Stage 1 flagged is real but isolated to one line (§6.a row 1); Stage 1's L1.1 single-line surgery resolves it. The 18-site reanchor table is verbatim per-cell; no row carries hand-wave. The compositional adequacy of `bbnf-host-prims` 8 primitives covers ~70-80% of CSS L4's 14 typed-value variants; the contingency clause (Cohesion C.3) honours the Lock-14 closure gate for the remaining 3 hard variants requiring `@host` directives.
>
> Hereupon the master-plan V2 re-issue agent inherits Stage-1's 30-item punch list plus Stage-2's 9 amendments. The pass-b-amendment agent applies the Stage-1 punch list verbatim with Stage-2 amendments folded; the corrected Pass-B synthesis emerges Amendment-01-compliant; the master-plan synthesizer ratifies the corrected substrate; Tranche E (the corrected convergent pivot under Amendment 01, now narrowed to Lock 1 + Lock 14 with Lock 13 staggered) drafts with the corrected facility ledger.

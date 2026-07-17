# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-2 · CRIT-2 (Adversarial Critic 2 of 2)

*CROSS-LOCK-weighted adversarial critique. Where CRIT-1 owns within-lock soundness, this pass
weights the CROSS-LOCK ledger as binding fact and hunts double-homing, contradiction, and cession
against the extant GN/PT codex and the sibling locks (LOCK-1/2/6/7). Every family run against the
failure-mode checklist + Constitution + prototype-evidence misuse. All live anchors independently
re-greped by hand this pass on the actual working tree; I take nothing on the synthesis's word.*

---

## 0. BASELINE — independently re-verified live this pass

- **HEAD = `90db093bf44a5c20729a7c851dcea3ec0a544417`.** `git diff --stat 3f2350f15..HEAD -- crates`
  is **EMPTY** — the FAMILY-SPECS pin (`3f2350f15`) is byte-identical to live HEAD over `crates`, so
  every prototype number transfers verbatim and XL-H wrong-artifact-risk is **discharged for all six
  prototypes** (HEAD advances since are `restart/`+ledger-only). Independent diff, not §0's word.
- **Anchors I re-greped by hand:**
  - `passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` — `ty`
    dropped. ✓ **The thread is NOT landed on live** (see CL-4).
  - `runtime_generator.rs:98` **`if !sink_only.has_rule("value")` is STILL LIVE** — a rule-NAME
    query. PROTO-1-t1's "EXECUTED-RETIRED" never touched the tree; `has_value_dispatch` appears
    **nowhere** in `runtime_generator.rs`. ✓ (concur CRIT-1 §0)
  - `passes/src/lib.rs:960` `admits_collapsed_stage` = `target.avx512bw && matches!(…Entry…)`;
    `avx512bw`@`:380` = `cfg!(all(target_arch="x86_64", target_feature="avx512bw"))` — **dead on
    aarch64**. ✓
  - `direct_schema.rs:52` `DirectTypeKind` — **Struct-only**, no `Sum`. ✓
  - `general_descent/emit.rs:300-303` — `Seq(children)`: `if index > 0 { writeln!(… skip_ws …) }`
    **unconditional** on whether the child emits code. PROTO-1's byte-coupling is a real live
    artifact, not a probe fiction. ✓
  - walkers: `json_sink_direct/dispatch.rs` **318**, `general_descent/view.rs` **431**. ✓
  - `DirectBuildField.target`: the four `target: Some` hits are all `ExprKind::Ref { target:
    Some(rule) }` (`passes:913,1104,1552,1599`); `DirectBuildField.target` is `Some` **nowhere** —
    the `.target` dead-field claim (G-3B-4) holds. ✓

---

## 1. HEADLINE CROSS-LOCK FINDING — THE STALE-SYNTHESIS DEFECT CRIT-1 FLAGGED WAS *NOT FOLDED*, AND THE 22:28 RE-CUT IS NOW STALE AGAINST TWO **NEWER** PROTOTYPES AS WELL

Hygiene, but load-bearing for cross-lock trust: the gestalt consumes this FAMILY-SPECS as LOCK-3's
ledger of record. Timeline by `ls` mtime:
`CRIT-1 20:15` → `FAMILY-SPECS re-cut 22:28` → `PROTO-1-a 22:58` → `PROTO-2-3A-c 23:01`.

**1a — The 22:28 re-cut did not fold CRIT-1's three mandated corrections.** Reading the **current**
22:28 FAMILY-SPECS:
- §5 table STILL reads *"3B-G1: whole fold … **UNRUN** — the spine's load-bearing gate"* — but
  PROTO-1.md (19:49) **RAN it and returned FAIL/DEMOTE.** Not UNRUN; run-and-failed-as-written.
  **CRIT-1 §1a un-applied.**
- §0/§6 STILL cite *"PROTO-1 `:204`-thread"* and *"PROTO-2 fold-fact-purity trace"* — but the
  resident PROTO-1.md is the whole-fold FAIL and PROTO-2.md is the 3A non-vacuity PASS. Citations
  dangle exactly as CRIT-1 §1b described. **Un-applied.**
- §1 G-3A-α STILL reads *"born-RED on LOCK-1 … until it lands the contract degrades to
  string-equality."* **CRIT-1 gap-1 re-cut un-applied.**

**1b — Two prototypes landed AFTER the 22:28 cut and are entirely unfolded; both change the map:**
- **PROTO-1-a (22:58) — RAN_UNMEASURED.** Executes PROTO-1's named route step 1 (`emit.rs:302`
  per-Seq-child `skip_ws` → *semantic* `skip_ws`) and **localizes the entire G-3B-1 byte-coupling to
  ONE bounded, one-commit reconcilable artifact**: under the semantic-skip rule a non-consuming
  marker never earns a separator, so marker deletion becomes byte-neutral. This *advances* PROTO-1's
  FAIL to "byte-satisfiable after one characterized sheets re-baseline" — **but the regen was never
  machine-captured (cold `ax-iter` exceeded the cap); byte-neutrality is DERIVED, a falsifiable
  prediction, not a confirmed run.** FAMILY-SPECS reflects none of it.
- **PROTO-2-3A-c (23:01) — RAN_MEASURED.** Proves the 3A contract non-vacuity is checkable
  **LOCK-1-FREE** via a total `str→BuiltinTy` resolver; an injected `:Span→:F64` mis-declaration
  RED-flags exactly the `string` rule and only there, LOCK-1-free. **G-3A-α's letter —
  "gate-that-cannot-fail until LOCK-1" — is REFUTED-as-stated.** §1 still asserts the opposite.

**1c — Cross-lock consequence.** GESTALT-B is briefed to consume "rigorously converged cores with
exact named gaps." A FAMILY-SPECS that (i) mis-states its own killer gate as UNRUN, (ii) dangles two
proof citations, and (iii) contradicts its two newest prototypes on the lock's *distinctive* 3A
organ, is **not a trustworthy input to the weave.** The stale-synthesis error is now
structural-recurrent-and-uncorrected — the flag was raised at 20:15 and the 22:28 re-cut did not
fold it. **The agglomerator owes a frozen prototype set + full re-fold before pass-3 or the gestalt
inherits a false ledger.** I do not re-price families for this (the underlying evidence is sound);
I price it as a **blocking process gate on the registry.**

---

## 2. THE SIX CROSS-LOCK HARDENINGS (the distinctive CRIT-2 weight)

### CL-1 · XL-B is a systemic single-point-of-failure, and it is WORKING, not final
Three of four families cede their **hardest unconverged part** to LOCK-2 through XL-B: **3C-C2**
(transpositions-as-grammar-maps → the missing term-rewriting/attribute-eval engine, ceded), **3D**
(per-site polyvariant staging, ceded), and the **decision-driver seat** (BackendShape/cost
selection) LOCK-3 claims to own. XL-B is stamped *"WORKING for pass 3+; final at GESTALT-B."* So
those cessions are **not homed yet** — parked against an unratified seam. This is an
**elegant-reduction trap at the cross-lock layer**: 3C-C2 and 3D-staging both read *"…and LOCK-2's
engine solves the per-site decision,"* where that clause **is** the unconverged hard part, and
LOCK-2 pass-2 carries its own open CSP/e-graph gaps. Neither lock converges the shared seam
independently; XL-B must be **co-converged at the gestalt**, not inherited settled. FAMILY-SPECS
tags cessions "(provisional on XL-B)" but does not weight that *provisional-on-WORKING* = unhomed.
**Gap for GESTALT-B: co-ratify XL-B before crediting any 3C-C2 / 3D-staging cession.**

### CL-2 · The `derive_materialization_roles` / `has_rule("value")` seam is QUADRUPLE-homed; only the 3B⊥3C fork is adjudicated
FAMILY-SPECS §6 collapses 3C⊥3B on this seam (single-funded to 3B by XL-3 / PROTO-1-t1). But the
same ~90-line byte-literal classifier (`passes:1403`) + name-gate (`:98`) is independently claimed
by **four** parties:
- **3B** — relocate into the fold (classifier moved, not retired).
- **3C** — "dissolve via grammar" (roles become grammar-declared productions; classifier deletable).
- **3D** — research-D §2: *"its Static literal frontier is a BTA query … subsumes
  `derive_materialization_roles`"* (binding-time half).
- **GN-3.W1** — the **true** retirement owner (`DirectTypeKind::Sum`, a LOCK-1/GN wave).

§6 collapses only the first two. **The 3D claim and the GN-3.W1 ownership are un-collapsed** — three
families "improve" one classifier whose real retirement belongs to a fifth party's Sum node. As
written, 3D's §2 subsumption silently contends with 3B for the seam §6 already awarded to 3B.
**Gap: add the 3D-BTA claim and the GN-3.W1 owner to the §6 incompatibility map; collapse to one
home.**

### CL-3 · The CONSUME-half of the `:204` thread is double-homed across the LOCK-3/LOCK-1 boundary, and the binding ledger names a non-existent live file
XL-F(1) (BINDING): *"`TypeAnalysis::from_ir` at `backend/driver/analysis.rs` is the real type
consumer."* LOCK-3 research A **and** B independently verify that file is **absent from live
skinny**. FAMILY-SPECS §6 routes this to GESTALT-B as a named-supersession challenge with evidence —
**correct, and I credit it** (concur CRIT-1). The weight: the *entire payoff* of threading
`Project.ty` is its consumer, and LOCK-3 homes it on `classify_projection` / `DirectBuildField.target`
(**live**) while the binding ledger homes it on `TypeAnalysis::from_ir` (**greenfield / absent**).
Until GESTALT-B rules whether XL-F(1) describes the GN-1 loom or is a stale live pointer, **the
CONSUME-half — the load-bearing reward of every family's `:204` claim — has two candidate homes and
the authoritative one is unresolved.** 3A's "consume, don't re-derive" and 3B's "read by join" both
measured the **live** home; if XL-F(1) is authoritative and points at greenfield, those prototypes
exercised a consumer that will not be the tree-of-record consumer. A real, un-de-risked contradiction
on the one PROVEN organ.

### CL-4 · The `:204` thread's AUTHOR is GN-3.W2 — the lock's single most-cited proof is downstream of an unlanded sibling wave
Research-D §3: *"This is exactly the wire GN-3.W2 threads."* FAMILY-SPECS §6: *"one author (a GN-3
wave, no-value-discard) and three consumers."* On **live** `:204` still drops `ty` (verified). So
the thread + verify predicate — the *"PROVEN (PROTO-1)"* row — is a verifier over a thread that
**GN-3.W2 must first author**, and GN-3.W2 has not landed. Every family's `:204` claim (3A verify,
3B column, 3C declare, 3D erase) is a **consumer of an unlanded GN wave.** FAMILY-SPECS names the
author once (§6) but the per-family sections treat the thread as present. **This must be a born-RED
edge (owner GN-3.W2) enumerated in EACH family, not only §6** — none of the CONSUME/VERIFY/ERASE
halves can converge until the AUTHOR half lands, and that author is outside LOCK-3. The lock's
convergence is capped by a cross-lock dependency it does not own.

### CL-5 · PROVE-12 (3D's flagship) is a retired-target measurement not re-run on the moved base — a SPEC_ONLY perf anchor if 3D inherits "already measured"
FAMILY-SPECS §4 concedes PROVE-12's target `find_component_delim` is *"retired from `codegen/src`;
PMU not re-run on the moved base"* and the flagship *"stays RETIRED."* Yet research-D §5 leans on it
as *"Value sub-plane, CSS: **already measured** — PROVE-12's ins/B −3.7…−6.4%."* Under Constitution
vii + prototype-evidence-misuse, **a perf number whose target is retired and whose PMU is not re-run
on the current base is SPEC_ONLY** — it may not anchor a live 3D claim. The priorart calls PROVE-12
"the only family with a CONFIRMED in-tree prototype"; that CONFIRMED status is stale on the moved
base. **Gap: re-measure PROVE-12 on the current base or re-label 3D's CSS inheritance SPEC-ONLY.**

### CL-6 · The multi-backend mandate has ZERO pass-2 family coverage — a charter clause with a named consumer (LOCK-6/8 TS face) and no LOCK-3 substrate
The LOCK-3 charter names *"multi-backend (Rust now, TS later) over a backend-agnostic middle"* as a
spine mandate. XL-D (RATIFIED): the fast TS face is the **build-time emitter (4B-TS)** — binding
input to LOCK-6/LOCK-8, which must consume **LOCK-3's middle**. Yet **no family spec (3A/3B/3C/3D)
carries a backend-agnostic-middle gate, and no prototype tested the crossing surface.**
`json_typed_direct.rs` emits *Rust* structs; priorart §7 warns *"that Rust-ness must sit below the
agnostic middle, or the TS backend inherits a Rust-shaped IR it cannot consume."* This is the mirror
of a Y.13 violation: a **named downstream consumer (LOCK-6/8) with no substrate designed for it.**
The clause is silently absent from every family's open gates; GESTALT-B → GESTALT-Ω inherits an
un-designed backend-agnostic contract. **Gap: LOCK-3 must carry an explicit backend-agnostic middle
contract (tape layout + typed schema + decode classes = the PROVE-14 crossing surface) as a named
gate, owner shared with LOCK-6.**

---

## 3. PER-FAMILY HARDENING (convergence % + enumerated open gaps)

### FAMILY 3A — CONTRACT-TOWER · **60%** (FAMILY-SPECS 60%; a genuine non-vacuity uplift, fully offset by the spec writing the vacuous form + the E1 elegant-reduction tell + a new coarse-inference born-RED + CL cessions)

**Genuinely earned (verified via resident prototypes):** (a) the refinement oracle RAN (PROTO-2-3A):
5-fault matrix localized to its arm, zero false positives, a same-discriminant payload corruption
caught, E2↔E3 separated. (b) non-vacuity PROVEN **twice** — in principle (PROTO-2.md: one `Span`
body fans to Float/Bool/Span declared kinds) **and LOCK-1-free** (PROTO-2-3A-c: injected `:Span→:F64`
localizes RED with no name→Type table). **G-3A-α is REFUTED-as-stated** — the contract fires *today*.

**Failure-mode checklist:**
- **Gate-that-cannot-fail WRITTEN INTO THE SPEC (unfolded):** §1 mechanism-2 / G-3A-a scope the
  predicate to `Project{ty:Some}`; PROTO-2.md/PROTO-2-3A-c prove that scoping is **presence-only** on
  the live fleet (only 4 `:ty` sites, all `-> input:Span` over regex bodies, all trivially
  consistent). Non-vacuity is earned by the target-only `ty:None` cases (24/28). **The *correct*
  predicate fires; the *specified* one cannot.**
- **Elegant-reduction trap (intact):** the oracle ran on **E3**, a definitionally 1:1 rename (that is
  *why* it is "free"). The **E1-infer injected-fault** matrix — where the contract's meaning lives —
  is only digest-grounded (PROTO-2-3A limit 1) or grammar-source-injected (PROTO-2-3A-c, not in-pass
  `infer` mutation). "Runs and localizes" proven where easy.
- **New born-RED the LOCK-1-free proof exposes (prototype-honest):** PROTO-2-3A-c limit 2 —
  `regex_type` types *every* non-ws regex as `Span`, so the LOCK-1-free contract distinguishes only
  Span-vs-wrong-builtin; a `number … -> f64:F64` would **mis-pass**. Fine-grained fidelity needs
  **richer inference than `regex_type`** — neither LOCK-1 naming nor currently in any gate.

**Enumerated open gaps (3A):**
1. Re-cut mechanism-2 / G-3A-a to the **value-category (`ProjectionKind`)** surface. Unfolded.
2. **E1-infer injected-fault localization** (mutate `infer`, show E3 GREEN while E1 localizes). Unrun.
3. **Richer `regex_type`** (numeric-regex ⇒ F64) — new born-RED, un-gated.
4. G-3A-a name→Type resolution (born-RED, owner **LOCK-1**) — nominal `:ty` arm only.
5. Open-q (ii): prove `render`@`:78` is a pure projection of L2, else E4 oracle owed. Unrun.
6. Open-q (iii): CSS `Err⇒SinkOnly / Ok⇒Scan` fork — edge oracle or declared two-root tower. Undecided.
7. Promote the +607-LOC PROTO-2-3A probe to a shipping `differential(edge,corpus)` CI contract. Unrun.
8. **Backend-agnostic-middle contract (CL-6)** — no oracle carries the TS crossing surface. Unrun.

### FAMILY 3B — SINGLE-IR-MULTIPASS · **53%** (FAMILY-SPECS 55%; DOWN — the killer gate ran and failed-as-written; the reconciliation is UNMEASURED; the true −100-LOC walker re-base is wholly unrun)

**Genuinely earned:** fact-layer purity proven by trace; the emit-dispatch fork retirement executed
byte-identical **in a worktree** (PROTO-1-t1, 3/3); PROTO-2's `dispatch_alt_count` discriminator
**refuted by execution** — the honest discriminator is the ≥4-of-6 value-category census.

**Failure-mode checklist:**
- **Killer gate CAN fail and DID:** PROTO-1.md returned FAIL/DEMOTE — deleting zero-consumer markers
  perturbs `sheets/generated.rs` by −3 `skip_ws` lines via the verified `emit.rs:300-303` rule.
  "delete the plane" ⊥ "byte-identical to committed" for sheets in this baseline; §2's demotion
  trigger **fired.** A new gap the pass added.
- **The reconciliation is UNMEASURED:** PROTO-1-a localizes the obstruction to one commit
  (semantic-`skip_ws`) and predicts byte-neutrality — but **RAN_UNMEASURED** (cold build cap; regen
  not captured). De-diffuses the obstruction; does not discharge the gate.
- **Elegant-reduction trap (the true hard part, wholly unrun):** the −100-LOC reward hangs entirely
  on re-basing `dispatch.rs` (318) + `view.rs` (431) onto `GrammarIr` byte-identically. PROTO-1 ran
  only the marker deletion; `view.rs`/`general_descent` shown **byte-fragile**. G-3B-1a is the
  "and-then-the-hard-part" and **no code touched it.** CONTINGENT, not banked.
- **Masked-fallback / DRY breach that never landed (legacy alias still live):** PROTO-1-t1's green
  slice was obtained by **porting `derive_materialization_roles.value` into codegen** — a DRY /
  no-orthogonal-codepaths breach its own text flags — and it **never landed**; `has_rule("value")`
  is **still live at `:98`** (verified), its comment *"Shape-based, never a grammar name"*
  mislabeling a rule-NAME query. Registry must carry G-3B-2 "executable in worktree, single-home
  unwired, unlanded," NOT "sub-gate discharged."

**Enumerated open gaps (3B):**
1. Re-specify G-3B-1 as **sheets-re-baseline-then-delete**, then **machine-confirm** PROTO-1-a's
   byte-neutrality (regen captured, not derived) AND execute the *whole* fold. THE killer gate.
2. **G-3B-1a walker re-base** — `dispatch.rs` (318) + `view.rs` (431) onto `GrammarIr`, byte-identical.
   `view.rs` fragile. The entire −100-LOC reward rests here. Unrun.
3. Single-home wiring of the value-role discriminator (retire the codegen duplicate). Unrun.
4. `css_scan` fourth-level (open-q ii) — confirm no emission state hides outside `GrammarIr`. Untraced.
5. Per-`RuleId` vs per-`ExprId` key (G-3B-3) — partial migration = no-orthogonal-codepaths defect.
   Undecided for the typed fleet.
6. **True RETIREMENT vs relocation** — born-RED on `DirectTypeKind::Sum` (GN-3.W1).
7. **Seam quadruple-home (CL-2)** — collapse the 3B/3C/3D/GN-3.W1 claims to one owner.

### FAMILY 3C — GRAMMAR-HOMOIOUSIA · **32%** (concur FAMILY-SPECS + CRIT-1)

Correctly held **BLOCKED (C1) / BLOCKED-candidate (C2)**. The C1/C2 split is the family's honesty.
No new prototype this pass — 3C is the one family whose sharpest uncertainty was **not** tested.

**Failure-mode checklist:**
- **Consumer-less substrate (dispositive, correctly owned):** `ir.bbnf` has no live consumer; sole
  consumer LOCK-7·7B, unlanded. Not independently landable (Y.13).
- **Product-only-subset trap (named):** `DirectTypeKind` Struct-only excludes every `Alt` = excludes
  `ExprKind`/`BackendExpr` themselves; must not stand for the formulation. Correct.
- **Elegant-reduction trap (self-named, real):** C2 stalls at a missing term-rewriting/attribute-eval
  primitive *"of difficulty equal to the original problem."* Importing it breaches
  orthogonal-subsystem/parsimony; ceding it is LOCK-2's seat. Honest — hence BLOCKED-candidate.
- **SPEC-ONLY (Constitution vii):** distinctive 3C content is 0% convergence-eligible; the one live
  win (`:204` thread) is the shared single-owner thread (author GN-3.W2 — CL-4), **not distinctively
  3C.**

**Enumerated open gaps (3C):**
1. `DirectTypeKind::Sum` — born-RED, **GN-3.W1**.
2. Named in-wave consumer — **LOCK-7·7B**, no live reader (Y.13).
3. Fixpoint `regen∘regen = regen` byte-identical over grammar-generated `ir/src/lib.rs`. **LOCK-7·7A.**
4. C2 cession to LOCK-2 — **provisional on XL-B (WORKING, not final; CL-1).**
5. The E6 `ExprKind::Alt → DirectTypeKind::Sum` byte-identity prototype (research-C §7.2 / CRIT-1
   gap 5) — the **cheapest concrete C1 proof, UNRUN**; the only run that could move C1 off
   0%-convergence-eligible before its external gates.

### FAMILY 3D — STAGED-RESIDUAL · **37%** (FAMILY-SPECS 38%; slightly down — nothing distinctive ran; the flagship perf anchor is stale; the seam claim is un-adjudicated)

**BANKED into 3B's attribute store, correctly** (the `Stage` column is one 3B attribute; a separate
staging spine ⊥ "fewest transpositions").

**Failure-mode checklist:**
- **Spec-cites-itself (named):** *"type-plane erasure = stage-0 erasure"* — its warrant is
  Constitution v restated. A definitional restatement, not an independent result.
- **Consumer-less substrate ON THE LIVING TREE:** the `admits_collapsed_stage`@960 `avx512bw` gate is
  a **verified-DEAD shape** on aarch64 — a genuine Y.13 consumer-less-substrate. Evidence of the
  problem; 3D's G-3D-iii proposes to re-home it, **unrun.**
- **Prototype-evidence misuse (CL-5):** PROVE-12 retired-target, PMU not re-run on the moved base;
  3D's "already measured" CSS inheritance is SPEC_ONLY until re-run.
- **Un-adjudicated seam claim (CL-2):** research-D §2 claims `derive_materialization_roles` for BTA
  subsumption, contending with 3B's §6-awarded home — un-collapsed.

**Enumerated open gaps (3D):**
1. `binding_times(CssScanProgram)` on **real** CSS IR — S0/S1 partition == PROVE-12's set
   (byte-identical iff correct). Unrun.
2. Re-home `admits_collapsed_stage` off `avx512bw` onto a grammar-derived binding-time predicate;
   RED→GREEN, no x86 smuggled back. Unrun.
3. Type-plane JSON first-byte cold paired-PMU — real ins/B or a named NEUTRAL negative. UNMEASURED.
4. Re-measure PROVE-12 on the current base (CL-5 / XL-H). Unrun.
5. The unification thesis proven concretely, not restated from Constitution v.
6. Per-site polyvariant staging cession — **provisional on XL-B (WORKING; CL-1).**

---

## 4. SPINE VERDICT (cross-lock-weighted)

The convergent spine (3B substrate + 3A discipline + 3D attribute; 3C banked) is **architecturally
sound and the family composition holds** — I concur with CRIT-1 and FAMILY-SPECS on the shape. But
weighting the cross-lock ledger sharpens the picture in three ways the within-lock view understates:

1. **Three of four families cede their hardest part to XL-B (WORKING, not final) — the spine's
   convergence is gated on a cross-lock seam that is itself unconverged** (CL-1). A systemic
   elegant-reduction trap: LOCK-3 cannot independently close 3C-C2 or 3D-staging.
2. **The one PROVEN organ (the `:204` thread) is neither authored by LOCK-3 (author: GN-3.W2, CL-4)
   nor is its consumer home settled (XL-F(1) names an absent file, CL-3).** The flagship proof is a
   verifier straddling two unresolved cross-lock boundaries.
3. **The multi-backend charter clause has zero family coverage (CL-6)** — a named consumer
   (LOCK-6/8 TS face) with no LOCK-3 substrate, inherited un-designed by the gestalt.

Convergence, cross-lock-weighted: **3A 60%, 3B 53%, 3C 32%, 3D 37%.** **No family is
convergence-eligible** (pass 2 of ≥3; each carries ≥1 unrun-or-failed load-bearing gate; two
consecutive clean passes + fresh non-author audit outstanding for all four).

**Blocking process gate the registry MUST clear before pass-3 (escalated — CRIT-1 raised it, the
22:28 re-cut did not fold it):**
1. **Freeze the prototype set, then FULLY re-fold** PROTO-1 (α FAIL), PROTO-2 (β PASS), PROTO-1-a
   (UNMEASURED reconciliation), PROTO-2-3A-c (LOCK-1-free non-vacuity, G-3A-α REFUTED) into the
   family dispositions, the §5 table, and the §6 map — the FAMILY-SPECS is stale against **four**
   resident prototypes and dangles two proof citations.
2. **Re-cut G-3A-a to the value-category surface** (the `:ty`-only predicate is a gate-that-cannot-
   fail written into the spec).
3. **Add to the §6 incompatibility map:** the 3D-BTA claim on `derive_materialization_roles` (CL-2),
   the GN-3.W2 authorship of the `:204` thread as a per-family born-RED edge (CL-4), and a
   backend-agnostic-middle gate (CL-6).
4. **Route to GESTALT-B, not re-litigated here (binding per CROSS-LOCK):** XL-B co-ratification
   (CL-1), the XL-F(1) absent-file consumer contradiction (CL-3, already routed by §6 — credit),
   PROVE-12 re-measurement / SPEC-ONLY re-label (CL-5).

*(Credit noted, concurring with CRIT-1: FAMILY-SPECS §6 correctly surfaces the XL-F(1)
`TypeAnalysis::from_ir` contradiction as an explicit named-supersession challenge routed to
GESTALT-B with evidence — the silent re-home a prior critic flagged is corrected. That cross-lock
finding is properly disposed.)*

# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-2 · CRIT-2 (Adversarial Critic 2 of 2)

*CROSS-LOCK-weighted adversarial critique. CRIT-1 carries the within-lock soundness audit; this critic
weights the CROSS-LOCK view (portfolio charter): where LOCK-3 **contradicts** or **double-homes** work
against the extant GN/PT codex and the other eight locks, where the CROSS-LOCK ledger's binding
adjudications are **inconsistent with the live substrate**, and where the "convergent spine" (§6)
silently **presumes** a sister lock's verdict. Every live anchor re-greped by hand on the working tree
this pass; CROSS-LOCK ledger entries treated as binding facts, challenges named for routing to
GESTALT-B with evidence per Constitution ix. The failure-mode checklist + Constitution + refutation
laws + prototype-evidence misuse are applied, but through the cross-lock lens.*

---

## 0. BASELINE — re-verified live (independent of CRIT-1)

- **HEAD = `06e70053f`** (`git rev-parse HEAD`), FAMILY-SPECS §0 pin CORRECT; `ce51c36fc` (PROTO-2,
  PROTO-2-3A) and `c93fc4314` (PROTO-1, PROTO-1-t1) both ancestors, `git diff --stat …HEAD --
  skinny/crates` empty. XL-H discharged for all four prototypes.
- **Anchors re-greped by hand (all CONFIRMED on live):** `passes:204`
  `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (ty dropped) ✓; `direct_schema.rs:52`
  `DirectTypeKind` has **one** variant `Struct`, **no `Sum`** ✓; `runtime_generator.rs:98`
  `if !sink_only.has_rule("value")` — **STILL LIVE, a rule-name query**, with a comment that reads
  *"Shape-based, never a grammar name"* (the comment **denies** what the code does) ✓;
  `admits_collapsed_stage`@`passes:959` = `target.avx512bw && matches!(…Entry(_))` — **dead on
  aarch64** ✓.
- **`.target` dead-field — VERIFIED independently.** `grep "target: Some"` in passes/codegen/ir → 4
  hits, **all four `ExprKind::Ref { target: Some(rule) }`** (`passes:913,1104,1552,1599`) — the
  resolved-Ref rule id, a *different field* from `DirectBuildField.target: Option<DirectBuildTarget>`
  (`ir:470`). `DirectBuildField.target` is `Some` **nowhere** and **read nowhere** (`grep "\.target"`
  in `codegen/src/lower` returns zero non-Ref hits). The field is genuinely dead. **Load-bearing for
  my headline cross-lock finding (§1).**
- **`from_ir`/`TypeAnalysis` — VERIFIED absent from live skinny.** `grep -rn "from_ir\|TypeAnalysis"
  skinny/crates` = **0 hits**; present only in legacy `crates/core` (`backend/driver/analysis.rs`,
  `grammar/schema/*`, …) which archives at GN-0 (XL-A). **Load-bearing for §1.**

---

## 1. HEADLINE CROSS-LOCK FINDING — THE `:204`-THREAD PROGRAM IS CONSUMER-LESS ON THE MEASUREMENT SUBSTRATE, AND THE CROSS-LOCK LEDGER MASKS IT

This is the finding CRIT-1 (within-lock) cannot reach, because it is a contradiction *between* the
binding CROSS-LOCK ledger and the living tree — precisely critic-2's charter.

**The construction.** The entire LOCK-3 convergent spine is organised around one shared obligation:
thread the declared `Project.ty` at `passes:204` (single-owner, `no-value-discard`), then **the other
three families CONSUME it** — 3A *verifies* consistency, 3B *threads* it as `attrs.node_type`, 3C
*declares* it, 3D *erases* it as S0 (FAMILY-SPECS §7 directive 2, §6 table, §5). Every family's
distinctive content hangs off a live consumer of that threaded type existing.

**The contradiction.** Under **XL-A** (binding): *"skinny@HEAD is the MEASUREMENT SUBSTRATE — the only
living implementation … the legacy top-level crates/ archives at GN-0."* On that sole living substrate,
the threaded type has **exactly one candidate consuming edge** — the `DirectSchemaSet` seam
(`derive_materialization_plan → json_typed_direct::render`, FAMILY-SPECS §7.1 itself names it "the live
consuming edge"). But:

1. The consuming FIELD on that seam — `DirectBuildField.target` — is **DEAD** (verified §0: `Some`
   nowhere, read nowhere; sole `DirectSchemaSet` producer is the hand file
   `xtask/src/real_typed_schema.rs`). The seam that would consume the threaded type **does not read
   it**.
2. **XL-F(1)** (binding) asserts: *"TypeAnalysis::from_ir at backend/driver/analysis.rs is the real
   type consumer."* Verified §0: `from_ir`/`TypeAnalysis` are **absent from live skinny** and exist
   only in legacy `crates/core`, **which archives at GN-0**.

**Therefore, on the measurement substrate, the `:204`-thread type output is CONSUMER-LESS** (Constitution
viii / Y.13, forbidden) until GN-3.W1 (`DirectTypeKind::Sum`) + GN-3.W2 (thread `.target`) land a live
reader. And the CROSS-LOCK ledger **actively masks** this: XL-F(1) points at a consumer that does not
exist on the substrate and archives at GN-0.

FAMILY-SPECS §7.1 **does surface XL-1** as a named-supersession challenge (reading (a) "greenfield
consumer" vs (b) "stale pointer") — credit noted; it corrects the silent re-home the prior CRIT-2
flagged. **But it stops one inference short.** It routes the *pointer* correction; it does **not** draw
the *program-level* consequence: until a live consumer lands, **PROTO-1's `:204`-thread "DISCHARGED for
the fact plane" (FAMILY-SPECS §0) is discharged into a fact plane with no downstream reader on the
substrate** — a threaded value verified by 3A, consumed by no one. The thread is `no-value-discard`
compliant *in isolation*; the *program* is Y.13-suspect *in aggregate*. This must be a first-class
cross-lock edge, not a footnote: **the `:204`-thread cannot be called "converged" for any family until a
live GN-3 consumer of the threaded type exists on skinny@HEAD.** Route to GESTALT-B as: *XL-F(1) is a
greenfield-consumer pointer (reading a), MUST be relabelled, and the substrate consumer is born-RED
GN-3.W1+W2 — every 3A/3B/3C/3D "consumes the thread" claim inherits that born-RED edge.*

---

## 2. THE SYNTHESIS IS STALE AGAINST ITS OWN TWO COMMISSIONED BRIEFS (confirmed; cross-lock consequence added)

CRIT-1 §1 documents this within-lock in full and I concur on every point (mtimes verified independently:
FAMILY-SPECS **19:32**, PROTO-1.md **19:49**, PROTO-2.md **19:52** — the two §8 briefs landed 17–20 min
*after* the synthesis cut, overwriting `PROTO-1.md`/`PROTO-2.md` whose *old* content the synthesis still
cites at §0/§6). I add only the cross-lock consequences the within-lock frame does not surface:

- **Brief α → PROTO-1.md (FAIL/DEMOTE 3B):** the whole-fold byte-identity gate is **unsatisfiable for
  sheets** (the `emit.rs:300-305` per-Seq-child `skip_ws` coupling) without a prior sheets re-baseline.
  Cross-lock weight: the re-baseline is a **generated-file mutation** — admissible under
  `clean-regen-discipline` only as *fresh regen output*, never a hand-patch — so the gate as written
  (*"delete plane + byte-identical to committed"*) is not merely unrun, it is **mis-specified against a
  standing law**: "byte-identical to committed" and "delete the plane" are mutually exclusive for sheets
  *by construction*. The honest gate is "byte-identical after a **declared** sheets separator
  re-baseline," a *weaker* convergence claim than the synthesis banks.
- **Brief β → PROTO-2.md (PASS-in-principle, but REFUTES the spec's own predicate):** non-vacuous
  (24/28) — but *only over `Project{ty:None}` target-only projections*. Scoped to `Project{ty:Some}`
  **exactly as FAMILY-SPECS §1 mechanism-2 and G-3A-a write `verify_project_types`**, all 4 `:ty`
  projections are `-> input : Span` over regex bodies, so `infer_expr(body)==Span==resolved(ty)` —
  **presence-only, the vacuity feared.** The spec's literal predicate is the gate-that-cannot-fail; the
  *correct* predicate (value-category / `ProjectionKind`) fires TODAY with no LOCK-1 name→Type
  resolution. **G-3A-a's disposition "gate-cannot-fail until LOCK-1" is refuted for the value-category
  scoping.**

**Structural cross-lock consequence (beyond CRIT-1's "freeze the prototype set"):** the recurring
stale-synthesis error means the §8 StructuredOutput briefs are returned to the gestalt as
open-uncertainties that are **already discharged** (one FAIL, one PASS-with-spec-correction). A gestalt
that ingests §8 verbatim re-commissions two settled prototypes and, worse, carries 3B forward at 55%
when its own demotion trigger has **fired**. The registry MUST fold α/β into the dispositions before any
gestalt ingest.

---

## 3. CROSS-LOCK CONTRADICTIONS & DOUBLE-HOMES (the distinctive critic-2 section)

**X-2 (HARD, unnamed by the synthesis) · 3B-single-rich-IR ⊥ LOCK-2·2B (IR-IS-EGRAPH).** LOCK-2's 2B
(portfolio): *"the middle IR is an e-graph end to end; passes are rewrite groups; equality saturation
under a schedule; extraction emits BackendShape directly."* LOCK-3's 3B (the convergent substrate):
*"one rich `GrammarIr` refined in place by annotating passes (the LLVM shape); BackendShape as
attributes."* Verified live: the middle IR is `GrammarIr` + `node_types: HashMap<ExprId,Type>`
(`passes:93`), **not** an e-graph. **These are two different middle-IR substrates and cannot both be the
tower's shape.** FAMILY-SPECS §6 invokes *"the same LOCK-2 solver seam"* for scheduling the attribute
plane but **never names this contradiction**. Under **XL-B** (LOCK-3 owns the tower's shape; LOCK-2
operates WITHIN it at named decision points = **islands**), 3B is compatible with LOCK-2's **2A**
(EGRAPH-ISLANDS) and **incompatible** with LOCK-2's **2B**. **The LOCK-3 convergent spine therefore
silently presumes LOCK-2 converges to 2A, not 2B.** If LOCK-2 lands 2B, the 3B substrate premise breaks.
An un-declared cross-lock convergence dependency; must be a named GESTALT-B edge: *3B's single-rich-IR
presumes LOCK-2·2A; 2B is a named incompatibility, adjudicated by XL-B in 3B's favour only if LOCK-2 does
not overturn XL-B at GESTALT-B.*

**X-3 (entanglement folded as settled) · the type-plane schedule is TRIPLE-HOMED.** FAMILY-SPECS §6:
*"the type level is a schedulable attribute plane, and its schedule … is the same LOCK-2 solver seam."*
But (a) LOCK-1 owns type inference/solving; (b) LOCK-1·1D says *"the type lattice lives in an e-graph …
Shares the engine with LOCK-2"*; (c) LOCK-3·3B carries the type as an attribute on its tower. Three locks
claim the schedule of one plane. Not *obviously* wrong (1D explicitly shares LOCK-2's engine; XL-B seats
LOCK-2 within LOCK-3's tower), but the synthesis presents a three-lock seam as a settled one-liner. Route
as a named dependency: *type-attribute-plane scheduling = {LOCK-1·1D lattice} ∘ {LOCK-2 engine} ∘
{LOCK-3·3B attribute carriage}; ownership boundary cut at GESTALT-B, not assumed.*

**X-4 (double-home) · 3C-C1 ⊥ LOCK-7·7B both author the "IR grammar."** FAMILY-SPECS G-3C-2 assigns
C1's *consumer* to LOCK-7·7B (*"TOTAL-SELF-HOST literally enumerates 'IR grammar'"*). But 7B (portfolio)
*"bbnf grammar, regex grammar, **IR grammar**, emitter templates — all grammar-specified"* also claims to
**author** it. FAMILY-SPECS resolves by split (3C authors the `Alt→Sum` data-model map; 7B is the
bootstrap-fixpoint consumer) — clean *if honoured*, but asserted, not adjudicated, and both registries
will present "IR grammar" as theirs at GESTALT-B. Name it: *the IR-grammar data-model map is single-homed
in 3C-C1 (author) with LOCK-7·7B as sole consumer (bootstrap fixpoint); LOCK-7 must NOT re-author it — a
`no-orthogonal-codepaths` / double-home edge for GESTALT-B dedup.*

**X-5 (convergence ceiling) · LOCK-3 cannot reach 100% before GESTALT-B, by XL-B chaining.** Two
dispositions — 3C-C2 (grammar-to-grammar transposition) and 3D-staging-form — are **ceded to LOCK-2
"(provisional on XL-B, WORKING not final)"** (§3 G-3C-4, §5 I-4, §7.3). XL-B is *WORKING, final at
GESTALT-B*. Therefore these cessions **cannot finalize until GESTALT-B**, so LOCK-3's convergence is
**structurally capped below 100% (zero open gaps) until the gestalt runs** — even the "convergent spine"
inherits the ceiling. FAMILY-SPECS half-acknowledges the provisionals but does not frame the ceiling as
*hard*. State it plainly: *LOCK-3 returns as a rigorously-converged core with named gaps; 100% is
XL-B-gated and belongs to GESTALT-B.*

**X-6 (Ω reconciliation, latent) · `no-ts-ir` (LOCK-3) vs LOCK-4·4C / LOCK-8·8C shared-instruction-IR.**
FAMILY-SPECS §7.4 + I-5: *"the backend-agnostic middle is tape + schema (PROVE-14), not a shared
instruction IR; no-ts-ir holds."* **Consistent** with XL-D (fast TS face = build-time emitter; runtime
decoders refuted) — credit. **But** LOCK-4·4C (VM-CORE, *"one instruction set shared by rust and ts"*)
and LOCK-8·8C (*"shared graph/instruction shape"*) posit a shared instruction IR across rust/ts for the
**combinator** plane. Different planes (bbnf generator backend vs parse-that combinator runtime), so no
live contradiction — but "is there a shared cross-language instruction IR?" is answered NO by LOCK-3 and
YES by LOCK-4/8; GESTALT-Ω must reconcile into one coherent story, not ship both unexamined. A one-line Ω
edge, flagged not adjudicated.

**X-7 (live Constitution-vi violation with a denying comment) · the `has_rule("value")` name-gate.**
`runtime_generator.rs:98` is a **live rule-name gate** (Constitution vi: *"no name-gates"*) carrying a
comment *"Shape-based, never a grammar name"* that **contradicts the code**. PROTO-1-t1 retired it in a
worktree (byte-identical GREEN) — but that **never landed** (verified: the name-gate is still on HEAD)
and the slice obtained its green by **porting `derive_materialization_roles.value` into codegen**, a DRY
/ `system-cohesion` / `no-orthogonal-codepaths` violation its own text flags. So: a live constitution
violation, a misleading comment masking it, and a retirement *unproven in production* (proven only via a
Constitution-forbidden duplicate home). Carry as an **open live-tree defect**, not a discharged sub-gate;
correct the denying comment the moment the gate lands.

---

## 4. PER-FAMILY — convergence % (cross-lock-weighted) + enumerated open gaps

### FAMILY 3A — CONTRACT-TOWER · **54%** (FAMILY-SPECS 60%; CRIT-1 62%)

Earned: the refinement-oracle DISCIPLINE RAN (PROTO-2-3A — localizes a 5-fault matrix to its arm, 0
false positives, catches a same-discriminant payload corruption, separates E2 from E3); non-vacuity
proven-in-principle (Brief β). I weight **below** CRIT-1 for two cross-lock reasons: (a) the contract's
*subject* — the type plane it validates — is **consumer-less on the substrate** (§1), so 3A verifies a
value nothing downstream reads on skinny@HEAD; (b) the meaning-bearing edge (E1-infer injected-fault) is
only digest-grounded — the organ is proven on the **free** 1:1 E3 rename (elegant-reduction trap intact
on the load-bearing edge).

Open gaps:
1. **Re-cut mechanism-2 / G-3A-a to the value-category (`ProjectionKind`) surface** — the `:ty`-only
   predicate is refuted vacuous by Brief β. NEW (surfaced by the newest prototype), UNFOLDED by the
   synthesis.
2. **E1-infer injected-fault localization** — mutate `infer`, show E3 stays GREEN while E1 localizes.
   PROTO-2-3A digest-grounded E1 only. The load-bearing widening. Unrun.
3. **Substrate consumer for the verified type** (§1) — until GN-3.W1+W2 land a live reader, 3A verifies
   a consumer-less value; carry as a born-RED cross-lock edge, not a discharged thread.
4. **G-3A-a name→Type resolution** (born-RED, owner LOCK-1) — needed only for the nominal `:ty` arm; the
   value-category arm is independent. Carry, do not gate the family.
5. **Open-q (ii/iii/iv)** — `render`@`runtime_generator.rs:78` pure projection of L2 (E4 oracle owed?);
   the CSS `Err⇒SinkOnly / Ok⇒Scan` fork as edge-oracle-or-two-root-tower; the lossy-arm honest-oracle
   property. Undecided/unrun.
6. **Promote the +607-LOC PROTO-2-3A probe to a shipping `differential(edge,corpus)` CI contract** — the
   stated 3A deliverable is a contract, not a probe. Unrun.

### FAMILY 3B — SINGLE-IR-MULTIPASS · **48%** (FAMILY-SPECS 55%; CRIT-1 54%)

Earned: fold fact-layer purity proven by trace; emit-dispatch fork retirement executed byte-identical
(PROTO-1-t1, 3/3); `dispatch_alt_count` discriminator refuted-by-execution. I weight **below** CRIT-1
because the cross-lock defects §3 land on 3B's substrate claim: the killer gate **RAN and FAILED-as-
written** (Brief α; demotion trigger fired), the substrate premise **silently presumes LOCK-2·2A over
2B** (X-2), and the value it substrates is **consumer-less on the tree** (X-1). The architecture survives
(semantically deletable, `strict_product` invariant); the byte-identity proof does not, and its
cross-lock premises are unnamed.

Open gaps:
1. **Re-specify G-3B-1** as a *declared-sheets-re-baseline-then-delete* gate (the as-written
   byte-identity gate is refuted for sheets by PROTO-1.md and mis-specified against clean-regen), then
   execute the *whole* `emit(&GrammarIr,&Attributes)` fold. THE killer gate. Unrun/refuted-as-written.
2. **G-3B-1a re-walk re-base** — `json_sink_direct/dispatch.rs` (318) + `general_descent/view.rs` (431)
   onto `GrammarIr`, byte-identical. general_descent shown byte-**fragile**; the −100 net-LOC reward
   rests entirely here. The elegant-reduction "and-then-the-hard-part." Unrun.
3. **X-2 — declare 3B's LOCK-2·2A presumption** as a named cross-lock edge; 2B is the named
   incompatibility. Unfolded.
4. **Single-home wiring** of the value-role discriminator — retire the codegen duplicate PROTO-1-t1
   introduced (Constitution-forbidden as production); land it, and correct the denying comment (X-7).
   Unrun.
5. **Substrate consumer** for the threaded `attrs.node_type` (§1). Born-RED GN-3.
6. **Per-`RuleId` vs per-`ExprId` key** (G-3B-3) — fleet check; a partial migration is a
   `no-orthogonal-codepaths` defect. Undecided.
7. **True RETIREMENT vs relocation** of the byte-literal classifier — born-RED on `DirectTypeKind::Sum`
   (GN-3.W1). Never asserted retired absent the node.

### FAMILY 3C — GRAMMAR-HOMOIOUSIA · **30%** (FAMILY-SPECS 32%; CRIT-1 32%)

Concur BLOCKED (C1) / BLOCKED-candidate (C2); the C1/C2 cleave (research-C) is the family's honesty.
Distinctive content **0% convergence-eligible**, SPEC-ONLY (Constitution vii). I weight marginally below
for the X-4 double-home (a new open gap) and the C2 refutation being genuinely load-bearing: bbnf maps
bytes→tape, the live transpositions are tree→tree, **no bbnf grammar IS the map** — a missing primitive
of difficulty equal to the original problem, correctly BLOCKED-candidate and ceded to LOCK-2 (provisional
on XL-B).

Open gaps:
1. `DirectTypeKind::Sum` — born-RED, GN-3.W1 (`direct_schema.rs:52` Struct-only verified; +420 LOC).
   Product-only subset excludes every `Alt` = excludes `ExprKind`/`BackendExpr`; must not stand for the
   formulation.
2. Named in-wave consumer for C1 — LOCK-7·7B; no live reader (Y.13). Admissible only as a born-RED
   GESTALT-B edge.
3. **X-4 — IR-grammar double-home** with LOCK-7·7B (author-vs-consumer split asserted, not adjudicated).
   NEW cross-lock gap.
4. Fixpoint `regen∘regen = regen` byte-identical over a grammar-generated `ir/src/lib.rs`
   (derives/doc-comments/impls). LOCK-7·7A.
5. C2 cession to LOCK-2 — provisional on XL-B (WORKING). Chains the X-5 ceiling.
6. The E6 `Alt→Sum` byte-identity prototype (research-C §7.2) — the cheapest concrete C1 proof, the one
   run that could move C1 off 0%-eligible before its external gates. UNRUN this pass.

### FAMILY 3D — STAGED-RESIDUAL · **35%** (FAMILY-SPECS 38%; CRIT-1 37%)

Concur BANKED into 3B's attribute store. Nothing 3D-distinctive ran. The unification thesis
("type-plane erasure = stage-0 erasure") **spec-cites-itself** (warrant = Constitution v, a definitional
restatement). `binding_times` on real IR UNRUN; the PROVE-12 anchor not re-run on `06e70053f`; the JSON
type-plane PMU delta UNMEASURED (PROVE-09 law). Cross-lock weight down for the live-tree x86 defect: the
dead `CollapsedStage` gate is a **live violation of the SPEC §0 refutation law "x86 dead"** on the tree
(a gate gated on `avx512bw`), which 3D must fix — evidence of the problem, not the solution.

Open gaps:
1. `binding_times(CssScanProgram)` on REAL CSS IR — prove the S0/S1 partition *equals* PROVE-12's
   hand-residualized set (parity byte-identical iff correct). Unrun.
2. Re-home `admits_collapsed_stage` off `avx512bw` onto a grammar-derived binding-time predicate
   (RED→GREEN, no x86 smuggled back). A live "x86 dead"-law fix owned by LOCK-3; confirm no double-home
   with LOCK-9's x86 purge (parse-that-side). Unrun.
3. Type-plane JSON first-byte cold paired-PMU — real ins/B or a named negative. UNMEASURED.
4. Re-confirm PROVE-12 on `06e70053f` (XL-H hygiene; base moved). Unrun.
5. The unification thesis proven concretely, not restated from Constitution v.

---

## 5. VERDICT (cross-lock)

The convergent spine (3B substrate + 3A discipline + 3D attribute; 3C banked) is **architecturally
sound and the family composition holds** — I concur with the synthesis and CRIT-1 on the *shape*. But
weighted across the ledger and the sister locks, three cross-lock facts the synthesis leaves unnamed
**lower the numbers and add born-RED edges**:

1. **X-1 — the `:204`-thread program is consumer-less on the measurement substrate** (dead `.target`;
   `from_ir` archives at GN-0); the CROSS-LOCK ledger XL-F(1) masks it by naming a legacy-only consumer.
   Every "consumes the thread" claim (3A/3B/3C/3D) inherits a born-RED GN-3.W1+W2 substrate-consumer edge.
2. **X-2 — 3B silently presumes LOCK-2·2A over 2B**; the two middle-IR substrates are incompatible and
   the presumption is unstated. A named GESTALT-B edge.
3. **X-5 — LOCK-3's convergence is XL-B-gated**: 3C-C2 and 3D-staging cessions cannot finalize until
   GESTALT-B, so LOCK-3 returns as a rigorously-converged *core with named gaps*, not 100%.

Plus the two commissioned briefs both landed *after* the synthesis and **change the map** (§2): 3A's
contract has proven meaning but its *specified* predicate is the vacuous one; 3B's killer gate RAN and
FAILED-as-written (demotion trigger fired). And a **live Constitution-vi name-gate with a denying
comment** (X-7) still sits on HEAD.

**No family is convergence-eligible** (pass 2 of ≥3; each carries ≥1 unrun/failed load-bearing gate;
each carries ≥1 born-RED cross-lock edge; two consecutive clean passes + fresh non-author audit
outstanding for all four). Numbers: **3A 54% · 3B 48% · 3C 30% · 3D 35%.**

**Three cross-lock corrections the registry MUST fold before any gestalt ingest:**
1. **Relabel XL-F(1) and draw its consequence** — it is a greenfield-consumer pointer (reading a), MUST
   be labelled as such, and the substrate consumer is born-RED GN-3.W1+W2; the `:204`-thread is not
   "converged" for any family until a live consumer exists on skinny@HEAD (§1).
2. **Name the LOCK-2·2A presumption and the XL-B convergence ceiling** as explicit GESTALT-B edges (X-2,
   X-5) — the spine's substrate premise and its 100%-ceiling both depend on unfinalized cross-lock
   adjudications.
3. **Fold Briefs α (FAIL) and β (PASS-with-spec-correction) into the dispositions and the §6 table**,
   and freeze the prototype set before the next synthesis cut — the §8 StructuredOutput briefs are
   already discharged, not open uncertainties for the gestalt to re-commission (§2).

*(Credit where due: FAMILY-SPECS §7.1 correctly surfaces the XL-F(1) contradiction as a routed challenge
rather than a silent re-home — the prior CRIT-2 defect is fixed. §7.4's `no-ts-ir`/PROVE-14 tape+schema
middle is cross-lock-consistent with XL-D and LOCK-6. The C1/C2 cleave and the C2 refutation are the
most honest costing in the lock. The failures above are of *completeness against the ledger*, not of
architectural judgement.)*

# LOCK-3 — THE IR TRANSPOSITION SPINE · REGISTRY

*Living adjudication ledger across the ≥3-pass convergence loop. The AGGLOMERATOR folds each
pass (research + FAMILY-SPECS + PROTOs + CRIT-1 + CRIT-2) into this file. Convergence % is the
**critics' floor** unless the agglomerator can name why a critic erred; no family approaches 100%
until it has zero open gaps + a fresh audit + two consecutive clean passes. Status is one of
ADVANCED / BANKED / BLOCKED / RETIRED.*

Charter: the IR tower and its transpositions (grammar AST → typed HIR → mid-IR → backend shapes →
emission); lowering contracts and invariants; residualization (PROVE-12) generalized; the decision
driver's seat; multi-backend (Rust now, TS later) over a backend-agnostic middle. Standing laws:
`no-value-discard`, `no-silent-epsilon`, `preserve-rich-ast`.

---

## VERIFIED BASELINE (pass-2 **RE-FOLD** — supersedes the 15:56 fold, which was cut against a stale prototype set)

**Why this is a re-fold, not a fresh pass.** The 15:56 pass-2 fold folded FOUR prototypes (PROTO-1
`:204`-thread, PROTO-2 fold-fact-purity trace, PROTO-1-t1 seam-slice, PROTO-2-3A E3 oracle). Pass-2
then **continued**: research re-swept (~22:00), FAMILY-SPECS re-cut (**22:28**), and **two more
prototypes RAN** — **PROTO-1-a** (T_semskip, 22:58) and **PROTO-2-3A-c** (contract non-vacuity,
23:01) — after which **both critics re-authored** (CRIT-1 **23:18**, CRIT-2 **23:22**). This RE-FOLD
incorporates all **six** resident prototypes and both new critics. The 15:56 dispositions are
superseded where the new evidence moved them (3B down; 3A's contract-gate refuted; two gate states
re-cut).

**Base law (XL-H) — wrong-artifact-risk DISCHARGED for all six prototypes.** The measurement
substrate for LOCK-3 family code lives in the **nested skinny repo**, not the outer bbnf-lang
worktree. Pins:

| Layer | Commit | Reconciliation |
|---|---|---|
| skinny substrate (research + PROTO-1/-t1/-2/-2-3A pin) | `3f2350f15` | FAMILY-SPECS anchor |
| skinny substrate mid-probe advance (PROTO-1-a / -2-3A-c) | `1a0d3091c` | **docs/ledger-only** ("owner model edict"); `emit.rs` byte-identical to `3f2350f15`; T_semskip edit **not** swept in (verified) |
| outer bbnf-lang HEAD (both critics) | `90db093bf` | `git diff --stat 3f2350f15..90db093bf -- crates` **EMPTY** (both critics independently) |
| prototype outer pin (PROTO-1-a / -2-3A-c) | `ce51c36fc` | ff-only from stale `d2b32f6e3`; `is-ancestor` TRUE; `diff --stat ce51c36fc 3f2350f15 -- skinny/crates` EMPTY |

Every prototype number transfers to HEAD verbatim; the intervening commits touched only
`restart/…/tranches/` + ledgers, never `skinny/crates`.

**Anchor table — all re-greped BY HAND by BOTH critics on live (CONFIRMED):**

| Fact | Live truth (`3f2350f15` == `90db093bf` over crates) |
|---|---|
| `->` ty-drop | `passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` |
| declared ty is a **raw string** | `ir/src/lib.rs:261` `Project.ty: Option<String>`; the live `Type` enum (`passes:235`) has **NO `Declared` variant** — variants are `Var/Builtin/Seq/Alt/List/Option/Rule` |
| name-gate STILL LIVE | `runtime_generator.rs:98` `if !sink_only.has_rule("value")` — **rule-NAME query**; PROTO-1-t1's retirement lived only in a worktree, never landed (`has_value_dispatch` appears nowhere) |
| schema sum-wall | `direct_schema.rs:52` `DirectTypeKind` **Struct-only** (single variant, zero `Sum`) |
| dead stage gate | `passes:960` `admits_collapsed_stage = target.avx512bw && matches!(…Entry…)`; `avx512bw`@`:380` = `cfg!(all(target_arch="x86_64", target_feature="avx512bw"))` — **permanently false on aarch64** |
| byte-coupling site | `general_descent/emit.rs:300-303` per-Seq-child `skip_ws` (index>0), **unconditional on whether the child emits code** — PROTO-1's coupling + PROTO-1-a's repair anchor, both real |
| `.target` dead field | `DirectBuildField.target = Some` **nowhere** (the four `target: Some` hits are all `ExprKind::Ref`); CONSUME-as-stated genuinely refuted, not a probe artifact |
| **XL-1 (verified ×2)** | `TypeAnalysis`/`from_ir`/`analysis.rs` **absent from live `skinny/crates/`**; exist only in legacy `crates/core/.../driver/analysis.rs` over legacy `GrammarIR` |

**THE CITATION-CHAIN DEFECT (both critics, agglomerator-confirmed by reading the resident files).**
The PROTO filenames were **overwritten**; two of the lock's claimed proofs point at files that now
hold *different* probes:
- **`PROTO-1.md` (19:49) NOW = the whole-fold G-3B-1 gate** ("execute the whole `(GrammarIr,
  Attributes)` fold — regen×check byte-identical") → **VERDICT: FAIL / DEMOTE.** The old `:204`-thread
  content is gone (survives in git history + prose).
- **`PROTO-2.md` (19:52) NOW = the 3A contract non-vacuity precursor** (`proj_witness.rs`, driving the
  real `infer`) → the `:204`-thread witness. The old fold-fact-purity trace is gone (git history).
- `PROTO-1-t1.md`'s own header (`:15`) confirms it: it names avoiding "clobbering the existing
  `PROTO-1.md` (the `:204`-thread probe) and `PROTO-2.md` (the fold…)" — proving both were later
  overwritten.

Consequence: this registry cites prototypes by **descriptive content + resident filename**, and
pass-3 owes a **frozen, stably-named prototype set** (the stale-synthesis defect has now recurred a
**THIRD** time at the same structural cause — prototypes landing inside the authoring window; see
CURRENT DISPOSITIONS headline).

**Two facts bind every family (unchanged, sharpened).** (1) The tower is **3B-shaped today** — a flat
`ExprId`/`RuleId` arena + attribute side-tables (`node_types`, `nullability`), then three tree
rebuilds (`lower_expr`, `materialize_rule`, `SinkOnlyExpr`) before a **fractured** emission stage;
LOCK-3 hardens (3A/3B) or re-founds (3C/3D), every divergence a named supersession. (2) The
**`Project.ty`@204 thread** is single-owner; its *author* is the **unlanded GN-3.W2 wave** (CL-4), its
CONSUME home is **disputed across the LOCK-3/LOCK-1 boundary** (live `classify_projection` /
`DirectBuildField.target` vs. greenfield-absent `TypeAnalysis::from_ir`, CL-3), and the consistency
CONTRACT over the threaded value is **RUN and non-vacuous LOCK-1-free** for the builtin subset
(PROTO-2-3A-c) — a change from the 15:56 fold, which called it "vacuous until LOCK-1."

---

## CURRENT DISPOSITIONS (pass-2 RE-FOLD — the fold of all six prototypes + both new critics)

| Family | Status | Convergence | One-line |
|---|---|---|---|
| **3A · CONTRACT-TOWER** | **ADVANCED** (discipline-on-3B-substrate; standalone typed-tower **BANKED**) | **60%** =60 | Double organ uplift — the E3 refinement oracle localizes (PROTO-2-3A) **and** the contract is **non-vacuous LOCK-1-free** (PROTO-2-3A-c, G-3A-α REFUTED-as-stated) — **fully offset** by the E1-infer in-pass mutation matrix still unrun (elegant-reduction on the sharp edge), a NEW coarse-inference born-RED, and the spec-text carrying the refuted "cannot-fail" form. |
| **3B · SINGLE-IR-MULTIPASS** | **ADVANCED** (the convergent substrate) | **53%** ↓55 | The killer gate **G-3B-1 RAN → FAIL-as-written** (sheets −3 `skip_ws`; JSON/CSS/real-typed byte-identical); PROTO-1-a's T_semskip rescue localizes the whole obstruction to one bounded commit but is **RAN_UNMEASURED**; the −100-LOC walker re-base (G-3B-1a) is **wholly unrun** and `view.rs` is byte-fragile. Convergence must not rise on a fired demotion. |
| **3C · GRAMMAR-HOMOIOUSIA** | **BLOCKED** (bank the gift) | **32%** =32 | The **C1/C2 split** is the real analysis progress; C1 born-RED on `DirectTypeKind::Sum` + a LOCK-7·7B consumer, C2 refuted-on-live and ceded to LOCK-2 (provisional on XL-B). **The one family whose sharpest uncertainty (the E6 sum-edge) was NOT tested this pass.** SPEC-ONLY; 0% distinctive-eligible. |
| **3D · STAGED-RESIDUAL** | **BANKED** (stage-attribute subsumed into 3B) | **37%** ↓38 | Nothing 3D-distinctive ran; the unification thesis **spec-cites-itself**; leverage rests on a **retired-target** PROVE-12 (PMU not re-run on the moved base — SPEC_ONLY per CL-5), an already-shipped `ScanAlphabet`, the shared `:204` thread, and a **proven-DEAD gate** (`admits_collapsed_stage` reads `avx512bw`). |

**Floor rule applied.** 3A floor(60,60,60)=**60**, 3B floor(55,53,53)=**53**, 3C floor(32,32,32)=**32**,
3D floor(38,37,37)=**37** (FAMILY-SPECS / CRIT-1 / CRIT-2). The agglomerator re-verified every crux
fact both critics rest on (base byte-identity over crates, `:204` ty-drop, no `Type::Declared`
variant, name-gate survival, `.target` dead, Struct-only schema, dead `avx512bw` gate, `emit.rs:302`
per-Seq-child coupling, PROTO-1.md=whole-fold-FAIL, PROTO-2-3A-c=non-vacuity-PASS) — **the critics did
not err; the floors stand unadjusted.** One place I could have moved a number and consciously did not:
3A's non-vacuity organ is a genuine second executed uplift that would argue toward 62-63 by the
"credit executed organs" rule, **but** it simultaneously exposed a *new* born-RED (the coarse-inference
ceiling that mis-passes numeric `F64`) and left the actually-load-bearing E1-infer in-pass mutation
matrix unrun — the offsets are real, both critics independently landed 60, and I take their floor.

**THE HEADLINE PROCESS DEFECT — STALE SYNTHESIS, THIRD RECURRENCE (both critics, agglomerator-confirmed
by mtime).** `FAMILY-SPECS.md` = **22:28**; `PROTO-1-a.md` = **22:58**; `PROTO-2-3A-c.md` = **23:01**.
The 22:28 synthesis was cut **30-33 min before its two newest prototypes finished**, and those two are
exactly the ones that move 3A (contract refuted-as-stated) and 3B (rescue route). Worse, the 22:28
re-cut **did not fold CRIT-1's (20:15) three prior corrections** — G-3B-1 still "UNRUN," the PROTO
citations still dangling, G-3A-α still "born-RED on LOCK-1." **This RE-FOLD is the corrective:** it
folds all six prototypes, re-cuts G-3A-α and G-3B-1, and re-homes the dangling citations. Pass-3 owes
a **frozen prototype set before the synthesis cut** — CRIT-2 prices this as a **blocking process gate
on the registry**, not a re-price of families (the underlying evidence is sound).

---

## FAMILY 3A — CONTRACT-TOWER · ADVANCED (discipline) · 60%

**What is earned (RAN/PROVEN this pass — the distinctive organ ran TWICE).**
1. **The refinement oracle localizes (PROTO-2-3A, E3).** `differential(edge,corpus)` localizes a
   5-fault matrix to its arm with **zero false positives** — including a *same-discriminant payload
   corruption a tag-only walk misses* — separates an E2 input fault (O2 RED) from E3 (GREEN), and
   proves the zero-witness⇒`unverified` discipline on a 13/13-arm census + live `json` BackendIr (15
   rules / 100 nodes). "Names the edge, never merely a failure."
2. **The contract is non-vacuous LOCK-1-FREE (PROTO-2-3A-c, RAN_MEASURED, 2/2 GREEN, EXIT 0).** The
   spec's premise was subtly wrong — there is no `Type::Declared` variant; the declared ty is a raw
   `Option<String>` at `ir:261`. A **total, LOCK-1-free** `parse_builtin: str→BuiltinTy` resolves the
   **4 live Sheets `: Span` declares** (rules `string`/`sheet_prefix`/`cell_ref`/`identifier`) and
   compares them to `infer_expr(body)` **today**: 4/4 `BuiltinConsistent`, 0 violations. The
   discriminator — inject `string: Span → F64`, re-parse+re-infer — flags **exactly 1
   `BuiltinViolation` at rule=`string` and ONLY there** while a `ty`-blind structural check stays
   GREEN. JSON negative control: 0 `Project` sites. **G-3A-α's letter — "gate-that-cannot-fail until
   LOCK-1, vacuously true whenever `ty=Some`" — is REFUTED-as-stated; open question (i) = YES.**
   Production footprint 0 (pure test, +252 LOC).

**Why no uplift is banked despite the double green (both critics, verified).**
- **The load-bearing edge is still tested only where it is free.** E3 is a definitionally 1:1
  `BackendExpr↦SinkOnlyExpr` rename — that is *why* it is a "free oracle." PROTO-2-3A-c injects at the
  **grammar-source** layer (a `: Span → : F64` string edit), which is the honest LOCK-1-free
  falsification, **but it is not the in-pass `infer`/`materialize` mutation** where the contract's
  meaning lives. The elegant-reduction tell survives on the sharpest edge.
- **A NEW born-RED the LOCK-1-free proof itself exposed — the coarse-inference ceiling.** `regex_type`
  types *every* non-ws regex as `Span`, so the contract distinguishes `Span` from a wrong builtin (the
  `F64` injection) yet would **mis-PASS** a hypothetical `number … -> f64 : F64` (its numeric regex
  body also infers `Span`). Full-fidelity consistency is **bounded above by inference precision** — a
  born-RED distinct from LOCK-1 naming, and NOT in any gate list before this pass.
- **The residual LOCK-1 dependency is narrowed, not removed.** The two commented Sheets TODOs
  (`: String`, `: CellRef` aggregate, src lines 11/62) name types `parse_builtin` cannot resolve —
  the *genuine* LOCK-1 residue. G-3A-a (name→`Type`) is now needed **only for the nominal/aggregate
  arm**, not the builtin arm.
- **The runner is a probe, not the deliverable.** `+607` (t2) and `+252` (c) probe tests, not the
  shipping `differential(edge,corpus)` CI contract (Constitution vii).

**Open gaps (re-cut against the RAN prototypes).**
- **G-3A-α (RE-CUT — REFUTED-as-stated; now a value-category surface gap).** The predicate must be
  re-cut from the vacuous `ty:Some` form to the **value-category (`ProjectionKind`) surface**; the
  contract fires today for the builtin subset. Record the refutation; do not carry "cannot-fail until
  LOCK-1."
- **G-3A-β (E1-infer in-pass injected-fault matrix UNRUN — the load-bearing edge).** Mutate
  `infer`/`materialize` themselves; show E3 stays GREEN under each while E1/E2 localize. Only
  digest-grounded (t2) + grammar-source-injected (c) so far — not the in-pass organ mutation.
- **G-3A-γ (coarse-inference ceiling, NEW born-RED).** Richer `regex_type` (numeric-regex ⇒ F64) so
  the builtin contract gains fine-grained fidelity; today it under-fires the numeric case. Distinct
  from LOCK-1 naming.
- **G-3A-δ (E3 is a lossy correspondence, not a round-trip).** The map erases `SpanMark.kind` +
  `TapeEmit.kind`; a fault flipping only an erased `kind` is invisible to E3 (must be caught at E2).
- **G-3A-ε (E4-as-projection unrun; CSS two-rooted tower undecided).** Prove `render`
  (`runtime_generator.rs:78`) is a pure projection of L2 (open q ii), else an E4 oracle is owed; decide
  the CSS `Err⇒SinkOnly / Ok⇒Scan` fork (`:40-71`, open q iii).
- **G-3A-ζ (the runner is a probe, not the shipping `differential(edge,corpus)` CI contract).**
- **G-3A-η (backend-agnostic-middle contract — CL-6, NEW).** No 3A oracle carries the TS crossing
  surface (tape layout + typed schema + decode classes = PROVE-14); a named downstream consumer
  (LOCK-6/8 TS face, XL-D) has no substrate designed for it.
- **G-3A-a (name→`Type` resolution, born-RED, owner LOCK-1).** Now scoped to the **nominal/aggregate
  arm only** (the two commented TODOs) — does NOT gate the builtin arm.

**Pass-3 directives.**
1. **Run the E1-infer in-pass injected-fault matrix** (mutate `infer`/`materialize`; E3 stays GREEN,
   E1/E2 localize) — the load-bearing widening PROTO-2-3A itself named.
2. **Land richer `regex_type`** (numeric-regex ⇒ F64) so the builtin contract closes the coarse-Span
   mis-pass and the F64/Span distinction becomes a real live check (G-3A-γ).
3. Promote `parse_builtin` + the site adjudicator (c) and the E3 digest oracle (t2) into ONE shipping
   `differential(edge,corpus)` CI contract — every byte-identity gate in the lock an instance of one
   edge oracle (3A's deliverable).
4. Prove E4/`render` is a pure projection of L2 (open q ii); decide the CSS two-root question (open q
   iii).
5. **Carry a backend-agnostic-middle contract as a named gate (CL-6)**, owner shared with LOCK-6.
6. Carry G-3A-a (name→`Type`) as a born-RED edge on **LOCK-1** for the nominal/aggregate arm only.

**Withheld from researchers:** do **not** re-litigate the standalone typed-multi-level-tower (BANKED);
do **not** re-argue that the E3 oracle localizes an injected fault (PROVEN, zero false positives); do
**not** re-run the E3 correspondence on the shipping map (GREEN on live); do **not** re-assert G-3A-α
in the "cannot-fail until LOCK-1" form (REFUTED — fires today, LOCK-1-free).

---

## FAMILY 3B — SINGLE-IR-MULTIPASS · ADVANCED (substrate) · 53%

**What is earned.** The cleanest structural argument in the lock, with running evidence. (1) Fact-layer
purity **PROVEN by trace** (the overwritten PROTO-2-original): `materialize_rule`'s emit-order splice is
a pure derivation recomputable inside one fold. (2) The `has_rule("value")` emit-dispatch fork retirement
**EXECUTED byte-identical in a worktree** (PROTO-1-t1, 3/3 `check-{json,sheets,real-typed}`), with
PROTO-2's `dispatch_alt_count` discriminator **refuted by execution** (Sheets carries dispatch Alts —
`check-sheets` RED with `missing Number RegexProgram`); the honest discriminator is the **≥4-of-6
value-category census** (`roles.value`). (3) The whole-fold gate **RAN** and returned a *precise,
architecturally-survivable* FAIL with a bounded rescue route — genuine progress even as it fails the
byte-identity contract.

**The killer gate RAN → FAIL-as-written (PROTO-1.md, the resident whole-fold probe). This is the
central correction of the re-fold.** Deleting the 4 zero-consumer marker variants + re-expressing
leaves **JSON `generated.rs` = 0 diff, CSS = 0 diff, real-typed = 0 diff, sheets = −3 `skip_ws`
lines** (diverged). Cause: `emit.rs:300-303` emits a `skip_ws` separator before *every* `Seq` child at
index>0, regardless of whether the child consumes input — the four pure markers manufacture separators
as Seq-slot fillers. **VERDICT: FAIL / DEMOTE to "structurally-motivated, unproven" under the strict
byte-identity gate** — the gate as written ("delete + four byte-identical regens") is *unsatisfiable
for sheets without first re-baselining*. Two nuances the FAIL itself surfaces, both load-bearing:
- **The spine-critical path is byte-robust.** JSON (`json_sink_direct/dispatch.rs`, the 91.5% hot leaf),
  CSS, real-typed are **byte-identical** under marker deletion — dispatch.rs already treats the plane as
  no-ops and **re-bases onto `GrammarIr` cleanly** (CLAIM B holds for the JSON walker). Only
  `general_descent` (view.rs's sibling emit.rs) is byte-fragile, and only via the incidental
  Seq-separator rule. So G-3B-1a is **asymmetric**, not uniformly unrun: the JSON-walker half has
  positive running evidence; `view.rs` is the fragile half.
- **The plane is semantically deletable.** It is a denormalized copy of `MaterializationPlan.descriptors`
  (`TapeEmit.kind`/`DirectBuild.shape` single-homed there); `strict_product` is invariant. Nothing is
  lost — only the *byte-identity proof* is refuted, by an incidental artifact the plane manufactures.

**The rescue RAN_UNMEASURED (PROTO-1-a, T_semskip).** Executes step 1 of PROTO-1's route: replace the
per-Seq-child `skip_ws` with a separator emitted **only between two input-consuming constructs** (a new
recursive `consumes_input` predicate; markers non-consuming). Correctness anchor: `render_rule`
(`emit.rs:272`) already emits `skip_ws` at rule entry, and `skip_ws` is idempotent — the only
semantic separator is the one *between* two consuming siblings. Under this rule a marker never earns a
separator, so **marker deletion becomes byte-neutral** (DERIVED): the whole G-3B-1 obstruction
localizes to **ONE bounded, one-commit reconcilable artifact** (`emit.rs:302`), converting "unsatisfiable"
to "byte-satisfiable after one characterized sheets re-baseline." +32/−2 LOC, one file, compilable and
type-shaped against the real variant set. **BUT: the cold `ax-iter` build did not finish inside the
50-min hard cap; no regen delta was machine-captured.** Byte-neutrality is a **falsifiable prediction,
not a confirmed run** — admissible only labelled UNMEASURED; must NOT be banked as "obstruction resolved."

**Open gaps.**
- **G-3B-1 (RE-CUT — RAN FAIL-as-written; rescue UNMEASURED).** Re-specify as *sheets-re-baseline-then-
  delete*; machine-confirm PROTO-1-a's byte-neutrality (regen captured, not derived) via the exact
  command PROTO-1-a lists; only then execute the *whole* `emit(&GrammarIr,&Attributes)` fold across
  {json,css,sheets,real-typed}. NOT "UNRUN" — the demotion trigger fired.
- **G-3B-1a (the elegant-reduction "and-then-the-hard-part" — the −100-LOC-bearing edge, asymmetric).**
  The reward hangs on re-basing `json_sink_direct/dispatch.rs` (318) + `general_descent/view.rs` (431)
  onto `GrammarIr` byte-identically. PROTO-1 gives **positive evidence for the dispatch.rs/JSON half**
  (byte-identical under marker deletion) but the **full SinkOnlyExpr-deletion re-base is unrun for both
  walkers**, and `view.rs`/general_descent is shown byte-fragile. Report net-LOC **CONTINGENT**, not banked.
- **G-3B-2 (relocation ≠ retirement; a DRY smell, born-RED — the slice NEVER LANDED).** PROTO-1-t1's
  green slice was obtained by **porting `derive_materialization_roles.value` into codegen** (a
  `system-cohesion`/`no-orthogonal-codepaths` breach its own text flags) and **never landed** —
  `has_rule("value")` is **still live at `:98`** (verified ×2). Registry reads "executable in worktree,
  single-home unwired, unlanded," NOT "sub-gate discharged." True RETIREMENT stays born-RED on
  `DirectTypeKind::Sum` (GN-3.W1) + a value-role attribute.
- **G-3B-3 (materialization key unconfirmed).** per-`RuleId` vs per-`ExprId` for the typed fleet (CSS L4
  inline alternatives the live candidate); a partial migration is a `no-orthogonal-codepaths` defect.
- **G-3B-4 (CONSUME REFUTED-as-stated, honestly homed).** `.target` is a **dead field** (`Some` nowhere,
  never read); true CONSUME sits behind three walls inside G-3B-1's fold, not a separable move.
- **G-3B-5 (byte-identity incidental-vs-semantic; css fourth-level).** Attribute every non-identical byte
  on the *executed* fold; trace `lower/css_scan.rs` for a hidden fourth value level (grammar prefixes,
  entry-rule threading not in `GrammarIr`).
- **G-3B-6 (seam QUADRUPLE-home — CL-2, NEW).** The `derive_materialization_roles`/`has_rule("value")`
  seam is claimed by **four** parties: 3B (relocate), 3C (dissolve-via-grammar), 3D (research-D §2: BTA
  subsumes its Static literal frontier), and GN-3.W1 (the true retirement owner via `Sum`). §6 collapsed
  only 3B⊥3C. Collapse all four to one home.

**Pass-3 directives.**
1. **Machine-confirm PROTO-1-a's T_semskip byte-neutrality** (regen captured), re-specify G-3B-1 as
   sheets-re-baseline-then-delete, then RUN the whole fold across {json,css,sheets,real-typed}. THE
   killer gate.
2. **Re-base `dispatch.rs` (318) + `view.rs` (431) onto `GrammarIr` byte-identically** (G-3B-1a) — the
   real −100-LOC content; credit the dispatch.rs/JSON positive signal, prove the view.rs fragile half.
   Report net-LOC CONTINGENT.
3. **Single-home the value-role discriminator** — thread passes-computed `roles.value` onto the
   attribute bundle; retire the codegen-side DRY duplicate PROTO-1-t1 introduced (Constitution-forbidden
   as production).
4. Trace `css_scan` for a hidden fourth level; run the fleet key-check (per-RuleId vs per-ExprId) with
   the exact counterexample rule.
5. **Collapse the quadruple-homed seam (CL-2)** to one owner in the §6 incompatibility map.

**Withheld from researchers:** do **not** build any `PassManager`/`PreservedAnalyses` invalidation
machinery (consumer-less until a mutating Transform exists); do **not** call the name-gate "retired"
(PROTO-1-t1 proved it byte-identically *relocatable in a worktree*, not retired, and it never landed);
do **not** bank PROTO-1-a's byte-neutrality as fact (UNMEASURED).

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA · BLOCKED · 32%

**The honest disposition; the C1/C2 split is the real analysis progress. The one family whose sharpest
uncertainty was NOT tested this pass — no 3C prototype ran.**
- **C1 — the IR *data model* is grammar-specified.** Bounded born-RED: `DirectTypeKind::Sum`
  @`direct_schema.rs:52` (verified Struct-only; owner **GN-3.W1**, +420 LOC) + a consuming **LOCK-7·7B**
  wave (TOTAL-SELF-HOST enumerates "IR grammar"). On live there is **no `ir.bbnf`, no meta-grammar, one
  grammar (`json.bbnf`)**; C1's sole reader is an **unlanded LOCK-7 wave** (Y.13 consumer-less
  substrate), admissible only as a born-RED edge under GESTALT-B.
- **C2 — the IR *transpositions* are grammar-to-grammar maps. REFUTED on live** (the sharpest
  counterexample): bbnf maps bytes→tape; `lower_expr`/`materialize_rule` are tree→tree folds with no
  byte stream, and `derive_materialization_roles` + `BackendShape` selection are whole-grammar analyses
  / cost-directed decisions expressible in *no* rewrite-rule language. Realizing it demands a
  term-rewriting/attribute-eval engine bbnf lacks → orthogonal-subsystem breach OR collapse into
  **LOCK-2's engine**. **BLOCKED-candidate** (missing primitive of difficulty *equal to* the original
  problem), **ceded to LOCK-2** — provisional on XL-B (WORKING, not final; CL-1).

**Note — one pass-1 blocker DISCHARGED by XL-C.** XL-C (RATIFIED) states verbatim "LOCK-3 3C no longer
hang[s] on ONE-HIR" — the LOCK-5 HIR-functor verdict is no longer a blocker; priced into the 32%.

**The one win that stands alone (not distinctively 3C).** 3C's `->`-thread is the **same single-owner
`:204` thread** (author GN-3.W2, CL-4) PROTO-2/-2-3A-c exercised; 3C **consumes** it.

**Open gaps (all externally owned).**
- **G-3C-1 (`DirectTypeKind::Sum`, born-RED, GN-3.W1).** The product-only subset excludes every `Alt` =
  excludes `ExprKind` (10-arm) / `BackendExpr` (13-arm) themselves — **must not stand for the
  formulation** (elegant-reduction trap by construction).
- **G-3C-2 (named consumer, born-RED, LOCK-7·7B).** No live `ir.bbnf`; sole reader unlanded (Y.13).
- **G-3C-3 (fixpoint, LOCK-7·7A).** `regen∘regen = regen` byte-identical over a grammar-generated
  `ir/src/lib.rs` carrying derives/doc-comments/impls — likely forces per-derive/per-impl annotations.
- **G-3C-4 (C2 cession).** Ceded to LOCK-2, provisional on XL-B (WORKING; CL-1 — co-ratify before crediting).
- **G-3C-5 (the E6 sum-edge byte-identity prototype UNRUN — CRIT-1/CRIT-2 gap 5).** research-C §7.2's
  `ExprKind::Alt → DirectTypeKind::Sum` E6 map emitting byte-identical to `derive_shape_facts`'s
  hardcoded variant list is the **cheapest concrete C1 proof** — the *only* run that could move C1 off
  0%-convergence-eligible before its external gates land. SPEC-ONLY this pass.

**Pass-3 directives.**
1. Hold **BLOCKED**. **Do not author `ir.bbnf`** until (a) GN-3.W1 lands `DirectTypeKind::Sum` and (b) a
   consuming LOCK-7·7B wave is named. Track both as born-RED dependency edges with owners.
2. Keep the C1/C2 costing apart — the family's honesty. C2 stays refuted-on-live/ceded-to-LOCK-2
   (provisional on XL-B); do not re-import a rewrite engine into 3C's charter.
3. *Only if* a consumer is named, OR as a pure scoping probe: run the **E6 product-only sum-edge
   byte-identity + `regen∘regen` fixpoint** (research-C §7.2) — proves the emitter is expressive enough
   for compiler-internal ADTs, **explicitly labeling that it excludes the sum**. This is the one 3C run
   available before its external gates.

**Withheld from researchers:** do **not** re-derive the blockers (settled); do **not** propose authoring
the IR meta-grammar as pass-3 mechanism (consumer-less-forbidden); the `ir/cost.rs` substrate stays
hand-written primitives — do not god-modularize it into the meta-grammar.

---

## FAMILY 3D — STAGED-RESIDUAL · BANKED · 37%

**Nothing 3D-distinctive ran this pass (Constitution vii).** 3D's content over "3B + a shipped
`ScanAlphabet` const" is exactly two claims: (i) `BindingTime ∈ {Static,Dynamic}` as a typed IR fact,
(ii) "type-plane erasure = stage-0 erasure." **Both SPEC-only**, and (ii) **spec-cites-itself** (warrant
= Constitution v restated, a definitional identity). The stage mechanism **is architecturally a 3B move**
(`Stage` is one attribute slot); the separate-staging-spine formulation stays **BANKED into 3B's
`Attributes` store**.

**The leverage is weaker than the 38% tick implies — four soft anchors.**
- **PROVE-12 is a retired-target measurement, and its "already measured" inheritance is SPEC_ONLY
  (CL-5).** Target `find_component_delim` is retired from `codegen/src`; the numbers stand on base
  `d2b32f6e3` against a site no longer on the emit path, and the **PMU was NOT re-run on the moved
  base**. research-D §5 leans on it as "already measured" — under Constitution vii + prototype-evidence
  misuse, that inheritance is **SPEC_ONLY until re-run**.
- **`ScanAlphabet`** (the surviving live residual) is **already shipped**, not 3D-authored — declared
  dependency, not convergence.
- **G-3D-1 (the DEAD gate — strongest live fact, but evidence of a PROBLEM not a win).**
  `admits_collapsed_stage` (`passes:960`) reads `avx512bw`, **permanently false on aarch64** → the JSON
  claim's `CollapsedStage` is a **dead shape** firing `CollapsedStageNotViable` whenever declared — a
  genuine Y.13 consumer-less-substrate on the living tree.
- **The RETIRED sub-verdict holds:** the PROVE-12-`u128`-byte-identity flagship stays **RETIRED**.

**Open gaps.**
- **G-3D-i (unification unproven — the distinctive, 0% run).** Attach a binding-time fact to real IR and
  prove "type-plane = stage-0" beyond restatement — the same `:204` thread as an S0 residual to a Rust
  *type* (3D consumes it).
- **G-3D-ii (`binding_times(CssScanProgram)` == PROVE-12 partition, unrun).** Checksum parity oracle
  (byte-identical iff the Stage0 set equals the hand-residualized set).
- **G-3D-iii (CollapsedStage re-home, unrun).** Re-home admission off `avx512bw` onto a grammar-derived
  binding-time predicate → admissible on aarch64 via `decision_csp`, no x86 smuggled back; RED→GREEN on
  `CollapsedStageNotViable`.
- **G-3D-iv (JSON type-plane PMU delta, UNMEASURED).** Absent a cold-paired-PMU delta the JSON claim
  collapses to "typed façade, same speed" (acceptable only if declared; PROVE-09 law).
- **G-3D-v (PROVE-12 re-measurement, CL-5).** Re-confirm on the current base or re-label 3D's CSS
  inheritance SPEC-ONLY.
- **G-3D-vi (seam claim un-adjudicated, CL-2).** research-D §2 claims `derive_materialization_roles` for
  BTA subsumption, contending with 3B's §6-awarded home — collapse in the §6 map.
- **G-3D-vii (per-site polyvariant staging cession).** Provisional on XL-B (WORKING; CL-1).

**Pass-3 directives (all under the banked banner; reactivate to ADVANCED only if the unification is
proven concretely — binding_times on real IR + CollapsedStage re-home + a measured type-plane delta).**
1. Run `binding_times` on the REAL CSS IR; prove the Stage0/Stage1 partition **equals** PROVE-12's
   hand-residualized set (checksum parity oracle).
2. **Re-home `admits_collapsed_stage`** off the dead `avx512bw` gate onto a grammar-derived binding-time
   predicate — no stub, no x86 back.
3. **Prove the unification concretely** by threading `Project.ty`@204 as an S0 residual to a Rust *type*.
4. **Re-measure PROVE-12 on the current base** (CL-5 / XL-H) or re-label the CSS inheritance SPEC-ONLY;
   measure the JSON type-plane cold paired-PMU delta — real ins/B or a named negative.

**Withheld from researchers:** the PROVE-12-`u128`-byte-identity claim is **RETIRED** — do not re-pose;
do **not** re-author `css_scan_direct` (landed) — declare the dependency; per-site staging-form scheduling
is **LOCK-2's** seat (gated on a real consumer, provisional on XL-B), not a LOCK-3 hardcoded emitter branch.

---

## REDIRECT / CROWDING DECISIONS (pass-2 RE-FOLD)

1. **3A-tower ⊗ 3B-single stays dissolved into the hybrid — running evidence BOTH sides, now doubled.**
   Resolved toward **one IR + per-pass contracts**; 3A-as-standalone-typed-tower stays **BANKED**.
   3A-discipline + 3B-substrate **compose**: PROTO-2-3A's E3 oracle ran on the *same live BackendIr*
   the 3B fold folds, and PROTO-2-3A-c proved the contract non-vacuous on the *same live grammar* the
   3B fold emits.
2. **3D's separate-staging-spine stays BANKED into 3B's attribute store.** `Stage` is an attribute, not
   a level; nothing 3D-distinctive ran to reopen the spine.
3. **The seam-dissolution is single-funded to 3B — but the seam is QUADRUPLE-homed (CL-2, escalated).**
   `has_rule("value")`@98 + `derive_materialization_roles`@1403 are claimed by 3B (relocate), 3C
   (dissolve-via-grammar), 3D (BTA subsume, research-D §2), and GN-3.W1 (true retire via `Sum`). §6
   collapsed only 3B⊥3C. The gestalt funds **one** structural home (3B) for the *relocation*; the *true
   retirement* is GN-3.W1's; the 3D-BTA and GN-3.W1 claims must be collapsed into the §6 map. Funding
   more than one relocation-home is a double-home breach.

**No new redundancy to bank this pass** (the critics show the advancing families composing, not
converging to one mechanism). The **underexplored formulation to open** is not a rival mechanism but
the *unrun in-pass half* of the proven 3A organ: **G-3A-β, the E1-infer `infer`/`materialize` mutation
matrix** — the edge the contract's meaning lives on, now that the grammar-source-layer non-vacuity is
proven (PROTO-2-3A-c). For 3C, the one available run is **the E6 sum-edge scoping probe** (research-C
§7.2), explicitly sum-excluded.

## CROSS-POLLINATION (the one named, justified — now EXECUTED on both organs)

**3A's refinement-oracle organ ⟶ 3B's `(GrammarIr, Attributes)` fold.** Upgraded from "partially
executed" (15:56 fold) to **executed on both 3A organs against live artifacts the 3B fold produces**:
PROTO-2-3A ran the E3 correspondence oracle on the live `json` BackendIr (15 rules/100 nodes), and
PROTO-2-3A-c ran the E1/contract non-vacuity oracle on the live Sheets grammar. The graft: each
analysis/emit pass over the single IR carries a **checkable output contract** — a `verify(grammar,attrs)`
predicate + a per-edge differential digest (E3) + a declared-vs-inferred consistency check (E1) — so
the discipline of a nanopass tower is recovered **without a second tree**. This *is* the convergent
spine's "one IR (3B) + per-pass contracts (3A discipline)."
*Deferred graft:* 3D's residual-count oracle is a 3A-style refinement oracle, but 3D is BANKED and its
distinctive organ never ran — re-evaluate only after the 3D re-cut (G-3D-i…iii) lands on real IR.

## THE CONVERGENT SPINE (hypothesis; architecturally sound; rests on load-bearing UNRUN/UNMEASURED halves)

**One IR (3B substrate) + per-pass contracts (3A discipline) + a stage attribute (3D erasure); 3C
banked** until GN-3.W1 (sums) + a named LOCK-7 wave unblock it. The families **compose**, not compete.
State after the six-prototype fold:

| Component | State after pass-2 RE-FOLD |
|---|---|
| `:204` thread witness + verify predicate (`proj_witness`, real `infer`) | **PROVEN** (resident PROTO-2.md; author = unlanded GN-3.W2, CL-4) |
| Fold fact-layer purity (`materialize_rule` recomputable) | **PROVEN by trace** (PROTO-2-original, git history / prose — filename overwritten) |
| **3A E3 refinement oracle — localizes an injected fault to its edge** | **RAN** (PROTO-2-3A) — on the free E3 edge; E2↔E3 separation, zero false positives |
| **3A *contract* non-vacuity (declared-vs-inferred, LOCK-1-FREE)** | **RAN_MEASURED — G-3A-α REFUTED-as-stated** (PROTO-2-3A-c); non-vacuous for the builtin subset, bounded above by inference precision |
| **3A E1-infer *in-pass* mutation matrix** | **UNRUN** — the edge the contract's meaning lives on (grammar-source injection ≠ in-pass `infer` mutation) |
| Seam relocation (`has_rule("value")` → value-role census) | **RAN byte-identical in a worktree, NEVER LANDED** (PROTO-1-t1) — DRY home; name-gate still live @`:98` |
| PROTO-2's `dispatch_alt_count` discriminator | **REFUTED by execution** — working = ≥4-of-6 value-category census |
| **3B-G1: execute the WHOLE fold, regen×check byte-identical** | **RAN → FAIL-as-written** (PROTO-1.md: sheets −3, rest 0); rescue localizes to one commit but **UNMEASURED** (PROTO-1-a) |
| 3B-G1a walker re-base (dispatch.rs 318 + view.rs 431 onto `GrammarIr`) | **UNRUN** (JSON/dispatch.rs half byte-robust under marker deletion; view.rs fragile) — net-LOC CONTINGENT |
| CONSUME (`.target` → reproduce `real_typed_schema.rs`) | **REFUTED-as-stated** (`.target` dead); behind the 3B fold |
| 3D binding_times on real IR + CollapsedStage re-home | UNRUN (BANKED) |
| 3C C1 / C2 | BLOCKED (Sum + LOCK-7 consumer) / **BLOCKED-candidate** (refuted, ceded to LOCK-2 prov. XL-B) |
| **Backend-agnostic middle contract (CL-6)** | **UNDESIGNED** — named consumer (LOCK-6/8 TS face, XL-D) with no LOCK-3 substrate |

**The load-bearing gates for pass 3 (both critics converge):** 3B rests on **G-3B-1** (RAN FAIL; rescue
UNMEASURED; the −100-LOC walker re-base wholly unrun) and 3A rests on **G-3A-β** (the E1-infer *in-pass*
mutation matrix — the contract non-vacuity is now proven at the grammar-source layer, but the in-pass
organ mutation is the edge that could still be vacuous). No family is convergence-eligible (pass 2 of
≥3; each carries ≥1 UNRUN/FAILED/UNMEASURED load-bearing gate; two consecutive clean passes + fresh
non-author audit outstanding for all four).

---

## §6 INCOMPATIBILITY MAP — additions this re-fold (CL-2/CL-4/CL-6)

The prior §6 (one hard fork 3A-tower⊥3B resolved to 3B; 3C⊥3B seam single-funded to 3B; 3C-C2⊥charter
BLOCKED-candidate; 3D⊥separate-spine compatible-as-attribute) stands. Three additions the critics owe
into the map:
- **The seam is QUADRUPLE-homed (CL-2):** add the 3D-BTA claim (research-D §2) and the GN-3.W1 true-
  retirement owner to the 3B/3C fork; collapse to one home (3B relocation / GN-3.W1 retirement).
- **The `:204` thread's AUTHOR is unlanded (CL-4):** GN-3.W2 must author the thread before any
  consumer (3A verify / 3B column / 3C declare / 3D erase) can converge — a **born-RED edge (owner
  GN-3.W2) enumerated in EACH family**, not only §6. The lock's convergence is capped by a cross-lock
  dependency it does not own.
- **The multi-backend charter clause is UNDESIGNED (CL-6):** a named downstream consumer (LOCK-6/8 TS
  face, XL-D RATIFIED) with no LOCK-3 backend-agnostic-middle substrate — the mirror of a Y.13
  violation. Add a backend-agnostic-middle gate (tape layout + typed schema + decode classes = the
  PROVE-14 crossing surface), owner shared with LOCK-6.

---

## CROSS-LOCK / GESTALT DIRECTIVES (pass-2 RE-FOLD)

1. **XL-1 — the `TypeAnalysis::from_ir` consumer contradiction: route to GESTALT-B as a named-
   supersession challenge with evidence (credited by both critics as properly disposed).** XL-F(1)
   settles binding: "TypeAnalysis::from_ir at backend/driver/analysis.rs is the real type consumer."
   **Agglomerator-verified + both critics independently re-greped:** `TypeAnalysis`/`from_ir`/
   `analysis.rs` **absent from live `skinny/crates/`**; exist only in legacy `crates/core` over legacy
   `GrammarIR`. The live consuming edge on the measurement substrate is the `DirectSchemaSet` seam.
   Two readings, both routable with evidence: **(a)** XL-F(1) describes the greenfield tree-of-record
   consumer (reconcilable under XL-A) — then it must be **labeled** so; **(b)** it is a stale live-spine
   pointer and XL-F(1) needs amendment. Owed: an explicit named-supersession line (Constitution ix).
2. **The `:204` thread — AUTHOR unlanded, CONSUME home disputed (CL-3/CL-4, escalated).** The thread is
   witnessed present-and-inferrable (resident PROTO-2.md), and its contract is non-vacuous LOCK-1-free
   (PROTO-2-3A-c) — **but its AUTHOR is the unlanded GN-3.W2 wave** and its CONSUME home is double-homed
   (live `classify_projection`/`DirectBuildField.target` vs greenfield-absent `TypeAnalysis::from_ir`).
   Assign the author to **exactly one owner (GN-3.W2)**; carry it as a born-RED edge in each family; do
   not let "the thread is witnessed" read as "the thread is landed." Entangled with XL-1: *what*
   consumes the value is the XL-1 dispute; *who authors* it is CL-4.
3. **XL-B is a systemic single-point-of-failure, WORKING not final (CL-1) — co-ratify before crediting
   any cession.** Three of four families cede their hardest unconverged part to LOCK-2 through XL-B:
   3C-C2 (the missing term-rewriting engine), 3D (per-site polyvariant staging), and the decision-driver
   seat. Those cessions are **parked against an unratified seam** — an elegant-reduction trap at the
   cross-lock layer, where the ceded clause *is* the unconverged hard part and LOCK-2 pass-2 carries its
   own open CSP/e-graph gaps. **Co-ratify XL-B at GESTALT-B before crediting any 3C-C2 / 3D-staging
   cession.** LOCK-3 owns *where* the driver sits; LOCK-2 owns *what engine* — a LOCK-3 hardcoded
   emitter branch for staging-form is incompatible with the boundary.
4. **The multi-backend charter clause has ZERO family coverage (CL-6, NEW — the sharpest new cross-lock
   gap).** LOCK-3's charter names "multi-backend over a backend-agnostic middle"; XL-D (RATIFIED) makes
   the build-time emitter (4B-TS) binding input to LOCK-6/8, which must consume LOCK-3's middle. Yet no
   family carries a backend-agnostic-middle gate. `json_typed_direct.rs` emits *Rust* structs;
   priorart §7 warns that Rust-ness must sit *below* the agnostic middle or the TS backend inherits a
   Rust-shaped IR it cannot consume. **LOCK-3 must carry an explicit backend-agnostic-middle contract
   (tape layout + typed schema + decode classes = the PROVE-14 crossing surface) as a named gate, owner
   shared with LOCK-6** — else GESTALT-B → GESTALT-Ω inherits an un-designed contract.
5. **PROVE-12 re-measurement / SPEC-ONLY re-label (CL-5).** PROVE-12's target is retired and its PMU was
   not re-run on the moved base; the "already measured" CSS inheritance is SPEC_ONLY until re-run on the
   current base. Do not let 3D inherit "already measured."
6. **`no-ts-ir` holds** across 3C/3D — the meta-grammar emits TS *type declarations*, never a runtime TS
   IR; the backend-agnostic middle is **tape + schema** (PROVE-14), not a shared instruction IR.
7. **BLOCKING PROCESS GATE (both critics — the registry's own hygiene).** The stale-synthesis defect has
   recurred a THIRD time at the same structural cause (prototypes landing inside the authoring window).
   Pass-3 MUST (a) **freeze the prototype set before the synthesis cut**, (b) **re-home the overwritten
   PROTO filenames under stable descriptive names** (two claimed proofs currently point at overwritten
   files), (c) full re-fold. This RE-FOLD discharges (c) for pass-2; (a)+(b) are pass-3's mandate.

---

## PASS HISTORY

### Pass 2 — RE-FOLD (folded by AGGLOMERATOR, Fable-tier)

- **Why a re-fold:** the 15:56 pass-2 fold was cut against an EARLIER 4-prototype set. Pass-2 continued —
  research re-swept (~22:00), FAMILY-SPECS re-cut (22:28), **two more prototypes RAN** (PROTO-1-a
  T_semskip 22:58; PROTO-2-3A-c contract non-vacuity 23:01), **both critics re-authored** (CRIT-1 23:18,
  CRIT-2 23:22). This RE-FOLD incorporates all **six** resident prototypes + both new critics.
- **Agglomerator verification (live):** confirmed base byte-identity over `skinny/crates` across all
  pins (`3f2350f15` == `1a0d3091c` docs-only == `90db093bf` HEAD; prototype pin `ce51c36fc` ff'd from
  stale `d2b32f6e3`, is-ancestor TRUE) → XL-H discharged for all six prototypes. Read the resident PROTO
  files by hand and confirmed the **citation-chain defect**: `PROTO-1.md` NOW = whole-fold G-3B-1
  **FAIL/DEMOTE** (sheets −3, JSON/CSS/real-typed 0); `PROTO-2.md` NOW = 3A non-vacuity precursor
  (`proj_witness`). Verified `PROTO-2-3A-c` = RAN_MEASURED non-vacuity (2/2 GREEN, EXIT 0). Re-greped the
  anchor table (no `Type::Declared` variant; name-gate live @`:98`; `.target` dead; `avx512bw` dead;
  `emit.rs:302` coupling). **The critics did not err; the floors stand.**
- **Corrections folded (the two critics' mandate):** (1) **G-3A-α REFUTED-as-stated** — the contract
  fires today LOCK-1-free for the builtin subset (re-cut from the vacuous `ty:Some` form to the
  value-category surface; a NEW born-RED coarse-inference ceiling opened). (2) **G-3B-1 re-cut from
  "UNRUN" to "RAN FAIL-as-written; rescue UNMEASURED"** — the killer gate ran and demoted; PROTO-1-a
  localizes the obstruction to one bounded commit (byte-neutrality DERIVED). (3) The dangling PROTO
  citations re-homed by descriptive content; a frozen stably-named set mandated for pass-3.
- **Dispositions set:** 3A ADVANCED-discipline **60%** (=60; double organ uplift fully offset by unrun
  E1-in-pass matrix + new coarse-inference born-RED); 3B ADVANCED-substrate **53%** (↓55; killer gate
  RAN-FAIL, rescue UNMEASURED, walker re-base unrun); 3C BLOCKED **32%** (=32; E6 sum-edge untested,
  SPEC-ONLY); 3D BANKED **37%** (↓38; nothing distinctive ran, PROVE-12 SPEC_ONLY on moved base,
  spec-cites-self, DEAD gate).
- **Redirects:** hybrid dissolution reaffirmed (evidence doubled — both 3A organs ran on live 3B
  artifacts); 3D-staging-spine stays banked into 3B; the seam is now **QUADRUPLE-homed (CL-2)** —
  collapse mandated. Underexplored formulation to open: the E1-infer in-pass mutation matrix (3A) + the
  E6 sum-edge scoping probe (3C).
- **Cross-pollination:** 3A refinement-oracle ⟶ 3B attributes-fold upgraded to **executed on both 3A
  organs** (E3 on live BackendIr + non-vacuity on live grammar).
- **Cross-lock (six hardenings folded):** XL-1 (`from_ir` contradiction, credited-disposed); CL-1 (XL-B
  co-ratify before cession); CL-2 (quadruple-homed seam); CL-3 (CONSUME home disputed); CL-4 (`:204`
  author = unlanded GN-3.W2, born-RED per family); CL-5 (PROVE-12 SPEC_ONLY on moved base); **CL-6 (the
  multi-backend clause has zero family coverage — a named consumer with no substrate; NEW named gate)**.
- **Process:** the stale-synthesis defect recurred a THIRD time; this RE-FOLD discharges the re-fold;
  pass-3 owes a frozen prototype set + stable PROTO naming (BLOCKING process gate).
- **Lock-wide:** no family convergence-eligible (pass 2 of ≥3; each carries ≥1 UNRUN/FAILED/UNMEASURED
  load-bearing gate; two consecutive clean passes + fresh non-author audit outstanding for all four).

### Pass 2 — initial fold (folded by AGGLOMERATOR, Fable-tier) — SUPERSEDED IN PART by the RE-FOLD above

- **Inputs:** research-{A,B,C,D,priorart}, FAMILY-SPECS (frozen 15:10), four prototypes — PROTO-1
  (`:204`-thread), PROTO-2 (fold fact-purity trace), PROTO-1-t1 (seam-slice), PROTO-2-3A (E3 oracle) —
  CRIT-1, CRIT-2. Anchored to `c43126422` (byte-identical to the RE-FOLD pins over crates).
- **Dispositions then set:** 3A 60%, 3B 55%, 3C 32%, 3D 38%. **Superseded where the two later prototypes
  moved them:** 3B → 53% (killer gate RAN FAIL, not UNRUN), 3D → 37% (PROVE-12 SPEC_ONLY re-price), and
  3A's G-3A-α refuted (contract fires LOCK-1-free). The redirects, cross-pollination, and convergent-spine
  hypothesis carry forward unchanged except as re-cut above.
- **Process note (in hindsight):** this fold was itself cut before pass-2's two sharpest prototypes ran —
  the stale-synthesis defect that recurred a third time and this RE-FOLD corrects.

### Pass 1 (folded by AGGLOMERATOR, Fable-tier)

- **Inputs:** research-{A,B,C,D,priorart}, FAMILY-SPECS, PROTO-1 (level-count adjudication, RAN_MEASURED),
  PROTO-2 (PROVE-12 corollary, RAN_UNMEASURED/REFUTED), CRIT-1, CRIT-2. Anchored `b3cb8c16b`.
- **Agglomerator verification:** 203=`Annotation` (non-defect) / 204=`Project` ty-drop; 51-commit
  staleness of the PROTO baseline; CSS IR-driven via `css_scan_direct`/`ScanAlphabet`;
  `find_component_delim` retired; `binding_times`/`Stage` absent; `has_rule("value")`@98 survives;
  `DirectTypeKind` Struct-only. **Floor taken unadjusted.**
- **Dispositions set:** 3A ADVANCED-as-discipline (tower BANKED) 55%; 3B ADVANCED-substrate 50%; 3C
  BLOCKED 30%; 3D BANKED (PROVE-12-`u128` gate RETIRED) 38%.
- **Redirects:** 3A-tower⊗3B dissolved to hybrid; 3D-staging-spine banked into 3B attribute store;
  seam-dissolution single-funded to 3B. Cross-pollination: 3A refinement-oracle ⟶ 3B attributes fold.
- **Lock-wide:** no family convergence-eligible (pass 1 of ≥3).

<!-- Pass 3 appends below this line -->

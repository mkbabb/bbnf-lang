# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-2 · CRIT-1 (Adversarial Critic 1 of 2)

*WITHIN-LOCK-weighted adversarial critique — internal soundness of each family spec, and whether
the pass-2 prototypes tested the sharpest uncertainty. Every family run against the failure-mode
checklist (vacuous convergence, spec-cites-self, gate-that-cannot-fail, elegant-reduction trap,
legacy aliases, masked fallback, unverified gestalt, consumer-less substrate) + the Constitution
(grammar totality, isomorphism, refutation laws, zero-runtime types, parsimony, Y.13) +
prototype-evidence misuse (SPEC_ONLY / UNMEASURED anchoring a claim). Live claims re-greped by hand
on the working tree this pass. CROSS-LOCK entries treated as binding; challenges named for GESTALT-B
routing, never re-litigated here.*

---

## 0. BASELINE — verified live this pass (every sharp anchor re-greped by hand)

- **outer HEAD = `90db093bf`** (`git rev-parse HEAD`). Source tree lives at top-level `skinny/crates/`
  (CONFIRMED present in this worktree; FAMILY-SPECS §0's "NOT restart/skinny, docs only" is correct).
- **FAMILY-SPECS §0 anchor table — all six CONFIRMED on live `skinny/crates`:**
  - `passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` — `ty` dropped ✓
  - `codegen/src/runtime_generator.rs:98` `if !sink_only.has_rule("value")` — **STILL LIVE** (PROTO-1-t1's
    retirement lived only in a worktree, never landed) ✓
  - `codegen/src/direct_schema.rs:52` `enum DirectTypeKind { Struct { … } }` — **single variant, zero `Sum`** ✓
  - `admits_collapsed_stage`@`passes:959-960` = `target.avx512bw && matches!(…Entry…)`; `avx512bw`@`:380` =
    `cfg!(all(target_arch="x86_64", target_feature="avx512bw"))` — **permanently false on aarch64 → DEAD gate** ✓
  - `codegen/src/general_descent/emit.rs:303` per-Seq-child `skip_ws` (index>0) + `:272` rule-entry `skip_ws`
    — PROTO-1's byte-coupling site AND PROTO-1-a's idempotency anchor, both real ✓
  - `.target` dead field — `grep 'DirectBuildTarget::' + 'target: Some(DirectBuildTarget'` over
    codegen/passes/ir = **zero hits**: the field is constructed `Some` **nowhere** → CONSUME-as-stated
    is genuinely refuted, not a probe artifact ✓
- **XL-H wrong-artifact-risk discharged** for the four *cited* prototypes (all base-pinned to ancestors of
  live over `skinny/crates`). But see §1: the two prototypes that actually move the map this pass are
  **not the four the synthesis cites.**

---

## 1. HEADLINE WITHIN-LOCK DEFECT — THE SYNTHESIS IS STALE FOR A **THIRD** TIME, EXACTLY AS PRIOR-CRIT PREDICTED

The prior CRIT-1 (20:15) closed with: *"the stale-synthesis error has now recurred twice at the same
structural cause (prototypes landing inside the authoring window) … Pass-3 must gate the synthesis cut
on a frozen prototype set, or this recurs a third time."* **It recurred a third time.** Verified mtimes:

| artifact | mtime | content |
|---|---|---|
| **FAMILY-SPECS.md** | **22:28** | the current synthesis |
| PROTO-1-a.md | **22:58** | 3B T_semskip — the G-3B-1 *rescue* route |
| PROTO-2-3A-c.md | **23:01** | 3A contract-oracle — **G-3A-α REFUTED-as-stated** |

The 22:28 synthesis was cut **30–33 minutes before its two newest prototypes finished**, and those two
are the ones that materially change 3A and 3B. Two consequences, both internal-soundness defects:

**1a — G-3A-α is asserted in the spec in the exact form the newest prototype refutes.** FAMILY-SPECS §1
G-3A-α reads: *"the contract is a GATE-THAT-CANNOT-FAIL until LOCK-1 … vacuously true whenever `ty=Some`
… True non-vacuity is born-RED on LOCK-1."* **PROTO-2-3A-c (23:01) executed the refutation:** a total,
**LOCK-1-free** `parse_builtin: str→BuiltinTy` resolves the 4 live `: Span` declares and compares them to
`infer_expr(body)` TODAY; injecting `string: Span → : F64` flags `BuiltinViolation` **at exactly the
`string` rule and only there**, while a `ty`-blind check stays GREEN. The spec's §1 open-question (i)
(*"can the contract EVER be non-vacuous, or is it structurally doomed until LOCK-1?"*) is **answered YES /
NOT-doomed** — but the spec still carries the DOOMED framing as a live gate and still lists it "UNRUN +
gate-cannot-fail until LOCK-1" in the §5 table. **This is a gate-that-cannot-fail written into the spec,
after it was proven able to fail.** Must be re-cut before pass-3 consumes the map.

**1b — the killer gate is listed "UNRUN" when it RAN (FAIL) and was then advanced (unmeasured).**
FAMILY-SPECS §2 G-3B-1 and the §5 table both say **"UNRUN — the spine's load-bearing gate,"** citing only
*"Trace (PROTO-2) + one slice (PROTO-1-t1)."* This is wrong in two directions:
  - **PROTO-1.md (19:49) RAN it → FAIL/DEMOTE.** The minimal marker deletion perturbs `sheets/generated.rs`
    by −3 `skip_ws` lines via the `emit.rs:303` per-Seq-child rule (verified live). The gate as written
    ("delete + four byte-identical regens") is **unsatisfiable for sheets without a prior re-baseline**.
  - **PROTO-1-a.md (22:58) advanced it → RAN_UNMEASURED.** T_semskip localizes the entire obstruction to
    ONE bounded artifact (`emit.rs:303` per-child → semantic `skip_ws`) and *predicts* the deletion then
    goes byte-neutral — **but the ax-iter build did not complete inside the cap, so the byte-neutrality is
    DERIVED, "a falsifiable prediction, not a confirmed run."**

  So the honest state of G-3B-1 is **"RAN FAIL-as-written; rescue route implemented as compilable code but
  its byte-neutrality UNMEASURED"** — materially different from the spec's flat "UNRUN."

**1c — the PROTO citation chain is broken (artifact-hygiene defect, unfixed from prior pass).** §0 header
cites *"PROTO-1 `:204`-thread"* and *"PROTO-2 fold-fact-purity trace"*; the §5 table rows *"`:204` thread …
**PROVEN** (PROTO-1)"* and *"Fold fact-layer purity … **PROVEN by trace** (PROTO-2)."* **Neither file now
holds that content** — the resident `PROTO-1.md` is the whole-fold FAIL, `PROTO-2.md` is the 3A
non-vacuity PASS. Two of the lock's four claimed proofs point at overwritten filenames; a reader who
follows the citation lands on a *different* probe. The overwritten `:204`-thread and fold-purity results
survive only in git history / the prose. **The agglomerator owes: re-home under stable names, re-point
every §0/§5 citation.** This is the SAME defect the prior CRIT-1 §1b flagged; it was not repaired.

---

## 2. PER-FAMILY HARDENING — convergence % + enumerated open gaps

### FAMILY 3A — CONTRACT-TOWER · **60%** (spec: 60 — hold; a real β/oracle uplift, cancelled by a stale gate + an unrun load-bearing edge)

**Genuinely earned this pass.** (a) PROTO-2-3A ran the refinement oracle: `differential(edge,corpus)`
localizes a 5-fault matrix to its arm with **zero false positives**, catches a *same-discriminant* payload
corruption a tag-only walk misses, separates an E2 input fault (O2 RED) from E3 (GREEN) — "names the edge,
never merely a failure," with the G-3A-b zero-witness⇒`unverified` discipline proven on a 13/13-arm census
+ live json (15 rules/100 nodes). (b) PROTO-2-3A-c proved the contract **fires TODAY, LOCK-1-free** (§1a).

**Internal-soundness caps (why no uplift is banked despite two green probes):**
- **The load-bearing edge is still tested only where it is free.** The oracle's GREEN + localization ran on
  **E3 — a definitionally 1:1 `BackendExpr↦SinkOnlyExpr` rename** (that is *why* it is a "free oracle").
  The **E1-infer injected-fault matrix — mutating `infer`/`materialize` themselves — is UNRUN.**
  PROTO-2-3A-c injects at the *grammar-source* layer (a `: Span → : F64` string edit), which is the honest
  LOCK-1-free falsification, **but it is not the in-pass organ mutation** the contract's meaning lives on.
  The elegant-reduction tell survives on the sharpest edge: "3A's organ runs and localizes" is proven where
  it is cheap, asserted where it is dear.
- **The spec's own `verify` predicate is written in the vacuous form** (mechanism-2 / G-3A-α scoped to
  `ty:Some`), which PROTO-2-3A-c narrows further: the builtin resolver works, but the **coarse-inference
  ceiling** means `regex_type` types *every* non-ws regex `Span`, so the contract can distinguish `Span`
  from a wrong builtin yet would **mis-PASS `number … -> f64 : F64`** (numeric regex also infers `Span`).
  The full-fidelity contract is **bounded above by inference precision** — a gate that *under*-fires on the
  numeric case today, distinct from LOCK-1 naming. Not credited in the spec's gate list.
- **Consumer discipline:** the shipping deliverable is `differential(edge,corpus)` as a **CI contract**; what
  exists are `+607` (t2) and `+252` (c) *probe tests*. Promotion unrun — do not let the probe stand for the
  contract (Constitution vii).

**Enumerated open gaps (3A):**
1. **Re-cut G-3A-α / mechanism-2 to the value-category (`ProjectionKind`) surface + record the
   PROTO-2-3A-c refutation** — the spec still asserts "cannot-fail until LOCK-1"; it CAN fail today. (Spec-text defect.)
2. **E1-infer injected-fault matrix** — mutate `infer`/`materialize`, show E3 stays GREEN while E1/E2
   localize. The load-bearing widening; only digest-grounded + source-mutation-grounded so far. UNRUN.
3. **Coarse-inference ceiling** — richer `regex_type` (numeric-regex ⇒ F64) so the builtin contract gains
   fine-grained fidelity; today it mis-passes numeric declares. Born-RED, distinct from LOCK-1 naming. UNRUN.
4. **G-3A-a name→Type resolution** (born-RED, owner LOCK-1) — needed only for the *nominal/aggregate* residue
   (the two commented Sheets `: String` / `: CellRef` TODOs). Carry; does NOT gate the builtin arm.
5. **Open-q (ii)** — prove `render`@`runtime_generator.rs:78` is a pure projection of L2, else an E4 oracle is owed. UNRUN.
6. **Open-q (iii)** — CSS `Err⇒SinkOnly / Ok⇒Scan` two-root fork (`runtime_generator.rs:40-71`): first-class
   edge oracle or declared two-root tower. Undecided.
7. **Promote `differential(edge,corpus)` to a shipping CI contract** (the stated 3A deliverable, not a probe test). UNRUN.

### FAMILY 3B — SINGLE-IR-MULTIPASS · **53%** (spec: 55 — DOWN: the killer gate RAN FAIL-as-written; its rescue is UNMEASURED; the −100-LOC hard part is untouched)

**Genuinely earned.** Fact-layer purity PROVEN by trace (the overwritten PROTO-2); the `has_rule("value")`
emit-dispatch fork EXECUTED byte-identical (PROTO-1-t1, 3/3), with PROTO-2's `dispatch_alt_count`
discriminator **refuted by execution** (Sheets carries dispatch Alts; the honest surface is the ≥4-of-6
value-category census). One real green slice on the fold path.

**Internal-soundness hits — the substrate's number should NOT rise on this pass:**
- **G-3B-1 RAN and FAILED-as-written (PROTO-1.md).** Per FAMILY-SPECS §2's own demotion rule, the trigger is
  **met and fired**: the byte-identity gate is *unsatisfiable for sheets* absent a re-baseline (`emit.rs:303`
  incidental-separator coupling, verified live). The architecture survives (the plane is a denormalized copy
  of `descriptors`, semantically deletable, `strict_product` invariant) — the **byte-identity PROOF does
  not.** The spec calling this "UNRUN" understates a fired demotion trigger (§1b).
- **The rescue (PROTO-1-a) is RAN_UNMEASURED — prototype-evidence-misuse risk.** T_semskip is *compilable*
  and its byte-neutrality is *plausible* (skip_ws idempotency + rule-entry `skip_ws`@`:272`), but the build
  did not complete; the "G-3B-1 is byte-satisfiable after one bounded re-baseline" conclusion rests on a
  **DERIVED prediction, not a run.** Admissible only labelled UNMEASURED; it must NOT be banked as
  "G-3B-1 obstruction resolved." The one-line confirmation command PROTO-1-a supplies is pass-3's job.
- **G-3B-1a — the elegant-reduction "and-then-the-hard-part" — is ENTIRELY UNRUN.** The −100 net-LOC reward
  hangs on re-basing `json_sink_direct/dispatch.rs` (318, verified) + `general_descent/view.rs` (431,
  verified) onto `GrammarIr` **byte-identically**. `view.rs`/`general_descent` is shown byte-fragile
  (PROTO-1). Deleting `SinkOnlyExpr` adds back a comparable re-walk; a reordered/differently-keyed walk
  breaks identity. The whole net-LOC claim is CONTINGENT on this unrun re-base — report CONTINGENT, not banked.
- **G-3B-2's green slice is a masked-fallback / DRY smell** its own text flags: PROTO-1-t1 ported
  `derive_materialization_roles.value` **into codegen** (a `system-cohesion`/`no-orthogonal-codepaths`
  breach) to prove *executability*. The single-home wiring (thread `roles.value` onto the attribute bundle)
  is UNRUN. Registry must read "executable, single-home deferred," not "sub-gate discharged."

**Enumerated open gaps (3B):**
1. **Confirm PROTO-1-a's T_semskip byte-neutrality by a completed run** (the exact command it lists), then
   re-specify G-3B-1 as *sheets-re-baseline-then-delete*; only then execute the *whole*
   `emit(&GrammarIr,&Attributes)` fold across {json,css,sheets,real-typed}. THE killer gate. UNRUN/UNMEASURED.
2. **G-3B-1a re-walk re-base** — dispatch.rs(318) + view.rs(431) onto `GrammarIr`, byte-identical; view.rs
   shown fragile. The entire −100-LOC reward rests here. UNRUN.
3. **Single-home wiring** of the value-role discriminator — retire the codegen duplicate PROTO-1-t1
   introduced. UNRUN (Constitution-forbidden as production).
4. **css_scan fourth-level** (open-q ii) — confirm no emission state (grammar prefixes, entry-rule threading)
   hides outside `GrammarIr` in `lower/css_scan.rs`. Untraced.
5. **Per-`RuleId` vs per-`ExprId` key** (G-3B-3) — fleet check (CSS L4 inline alternatives the live
   candidate); a partial migration is the `no-orthogonal-codepaths` defect. Undecided.
6. **True RETIREMENT (vs relocation)** of the byte-literal classifier — born-RED on `DirectTypeKind::Sum`
   (GN-3.W1, verified Struct-only); never asserted retired absent the node.

### FAMILY 3C — GRAMMAR-HOMOIOUSIA · **32%** (concur with FAMILY-SPECS)

Correctly held BLOCKED (C1) / BLOCKED-candidate (C2); the C1/C2 split is the honest cut and the family's
real analytic progress. **3C is the one family whose sharpest uncertainty was NOT tested this pass** — no
3C prototype ran. Its cheapest concrete proof — the `ExprKind::Alt → DirectTypeKind::Sum` E6 map emitting
byte-identical to `derive_shape_facts`'s hardcoded variant list (research-C §7.2) — remains **SPEC-ONLY**,
and is the *only* run that could move C1 off 0%-convergence-eligible before its external gates land.
Constitution-vii verdict: **distinctive 3C content is 0% convergence-eligible, SPEC-ONLY this pass.** The
one live win (the `:204` thread) is the shared single-owner thread, not distinctively 3C (3C consumes it).
Y.13 correctly cited: C1's sole reader is an unlanded LOCK-7·7B wave (consumer-less substrate on the living
tree); C2 stalls at a missing term-rewriting primitive of difficulty equal to the original problem (ceded
to LOCK-2, provisional on XL-B WORKING — not final).

**Enumerated open gaps (3C):**
1. `DirectTypeKind::Sum` — born-RED, GN-3.W1 (verified Struct-only). Product-only subset excludes every
   `Alt` = excludes `ExprKind`/`BackendExpr` themselves; **must not be treated as standing for the formulation.**
2. Named in-wave LOCK-7·7B consumer for C1 — no live reader (Y.13). Admissible only as a born-RED GESTALT-B edge.
3. Fixpoint `regen∘regen = regen` byte-identical over a grammar-generated `ir/src/lib.rs` carrying
   derives/doc-comments/impls (LOCK-7·7A). UNRUN.
4. C2 cession to LOCK-2 — provisional on XL-B (WORKING, not final).
5. The E6 sum-edge byte-identity prototype (research-C §7.2) — cheapest concrete C1 proof. UNRUN.

### FAMILY 3D — STAGED-RESIDUAL · **37%** (spec: 38 — hold/down; no new evidence, spec-cites-self intact)

Correctly BANKED into 3B's attribute store (`Stage` = one column). **Nothing 3D-distinctive ran this pass.**
Internal-soundness hits:
- **spec-cites-itself.** The distinctive thesis *"type-plane erasure = stage-0 erasure"* has as its warrant
  Constitution v (zero-runtime-cost types) restated — a definitional identity, not an independent result.
- **consumer-less substrate on the LIVING tree (Y.13).** `admits_collapsed_stage`@`959` → `avx512bw` → false
  on aarch64 (verified) makes `CollapsedStage` a **DEAD shape** that fires `CollapsedStageNotViable` whenever
  declared. This is evidence of the *problem*, correctly, not a bank.
- **prototype-evidence:** `binding_times` on real IR UNRUN; the PROVE-12 perf anchor **not re-run** on live
  (target `find_component_delim` retired; PMU stale); the type-plane JSON PMU delta the directive demands is
  **UNMEASURED** (PROVE-09 law: absent a cold-paired number the JSON claim collapses to "typed façade, same
  speed," acceptable only if so declared). `ScanAlphabet` is already-shipped, not 3D-authored.

**Enumerated open gaps (3D):**
1. `binding_times(CssScanProgram)` on REAL CSS IR — prove the S0/S1 partition *equals* PROVE-12's
   hand-residualized set (checksum parity oracle). UNRUN.
2. Re-home `admits_collapsed_stage` off `avx512bw` onto a grammar-derived binding-time predicate (via
   `decision_csp`, no x86 smuggled back); RED→GREEN on `CollapsedStageNotViable`. UNRUN.
3. Type-plane JSON first-byte cold paired-PMU — real ins/B or a named negative. UNMEASURED.
4. Re-confirm PROVE-12 on live `90db093bf` (XL-H hygiene; base moved, target retired). UNRUN.
5. The unification thesis proven concretely, not restated from Constitution v.

---

## 3. SPINE VERDICT (within-lock)

The convergent spine (3B substrate + 3A discipline + 3D attribute; 3C banked) is **architecturally sound and
the family composition holds** — §6's incompatibility map is correct and the `:204`-thread one-author /
three-consumer resolution is clean. But the internal soundness of the **spec document** has degraded against
its own newest evidence for the third consecutive pass, and two of the three families that could have
converged carry a fired-or-unmeasured load-bearing gate:

- **3A — 60%.** Contract proven non-vacuous & LOCK-1-free (PROTO-2-3A-c) and the oracle localizes
  (PROTO-2-3A) — a real double uplift — but the **spec still asserts G-3A-α in the refuted "cannot-fail"
  form**, the **E1-infer organ-mutation matrix is unrun** (elegant-reduction on the sharp edge), and the
  **coarse-inference ceiling under-fires the numeric case**. Uplift and spec-correction-debt cancel.
- **3B — 53%.** Killer gate G-3B-1 **RAN FAIL-as-written**; the PROTO-1-a rescue is **RAN_UNMEASURED**; the
  −100-LOC-bearing walker re-base (G-3B-1a, 318+431 LOC) is **entirely unrun** and view.rs is fragile; the
  green slice is a **DRY masked-fallback**. Down from the spec's 55 — convergence must not rise on a fired demotion.
- **3C — 32%.** BLOCKED / SPEC-ONLY; sharpest uncertainty (E6 sum-edge) untested. Concur.
- **3D — 37%.** BANKED; spec-cites-self; `CollapsedStage` DEAD gate on the living tree; PROVE-12 stale. Concur/down.

**No family is convergence-eligible** (pass 2 of ≥3; each carries ≥1 unrun/failed/unmeasured load-bearing
gate; two consecutive clean passes + fresh non-author audit outstanding for all four).

**Corrections the registry MUST fold before pass-3 (priority order):**
1. **Re-fold PROTO-1-a (22:58) and PROTO-2-3A-c (23:01) into the family dispositions, the §5 table, and
   G-3A-α/G-3B-1** — the synthesis is stale for a THIRD time against its own sharpest prototypes; the newest
   pair are already-discharged results, not pass-3 directives.
2. **Reconcile the broken PROTO citation chain (§1c)** — re-home the overwritten `:204`-thread and
   fold-purity-trace results under stable names; re-point every §0/§5 citation. Two claimed proofs currently
   have no resident artifact. (Unfixed from prior pass.)
3. **Freeze the prototype set before the next synthesis cut** — the stale-synthesis defect has now recurred
   at the same structural cause three times; pass-3 must gate the cut on a frozen set or it recurs a fourth.
4. **Correct the two mislabelled gate states** — G-3A-α is REFUTED-as-stated (fires today, LOCK-1-free);
   G-3B-1 is RAN-FAIL-as-written + rescue-UNMEASURED — neither is "UNRUN."

*(Credit, well-handled: §7.1 correctly surfaces the XL-F(1) `TypeAnalysis::from_ir` contradiction — verified
absent from live `skinny/crates` — as a named-supersession challenge routed to GESTALT-B rather than a silent
re-home. The `DirectSchemaSet` live-consuming-edge finding is sound and properly disposed.)*

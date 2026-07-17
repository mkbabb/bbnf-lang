# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 CRIT-2 (ADVERSARIAL, cross-lock weighted)

Critic 2 of 2, **re-issued against the 19:33 re-cut FAMILY-SPECS** (the earlier CRIT-2 at 15:44
critiqued the 15:12 synthesis and is superseded by this file). Read: PORTFOLIO (LOCK-7 +
CONSTITUTION), CROSS-LOCK.md (XL-A…XL-H, consumed as binding fact), FAMILY-SPECS.md (post-fold
re-cut, 19:33), **all four RAN probes** — PROTO-3 (leaf evaluator, 15:25), PROTO-2/7C-T2 (15:26),
**UF-2/PROTO-1.md (the static totality counter, 19:58)**, **UF-1/PROTO-2-UF1.md (the structural
oracle, 20:04)** — CRIT-1 (within-lock), research A/B/C/D/priorart. As critic 2 I weight the
CROSS-LOCK view: where LOCK-7 contradicts, double-homes, or mis-scopes its coupling to the extant
GN/PT codex and the other locks, and where the re-cut is already stale against its own newest probes.

**Disk verifications I ran this pass (constitution vii — not doc-quoting):**
- `grep -rn parse_postfix_ops skinny/crates/grammar/src/lib.rs` at HEAD `06e70053f` → **ABSENT**;
  `git status --porcelain` on that file → **EMPTY**. **The "landed Wall-0 `?w`-fix" cited in §0.6
  and §1-gate-(a) is NOT landed** — it exists only inside the PROTO-3/UF-2 probe patches. UF-2
  independently confirms: on pristine base the bbnf closure **fails to BUILD** at offset 662.
- `grep -noE 'XL-[0-9]+' FAMILY-SPECS.md` → live dangling cites **`XL-3` (line 197)** and
  **`XL-5` (line 325)**; the §0 Dead-ID note re-points only XL-1/XL-2. The dead-ID defect CRIT-1
  C-3 / old-CRIT-2 flagged is **only partially fixed**.
- UF-2 (PROTO-1.md, PASS) and UF-1 (PROTO-2-UF1.md, RAN) both landed **after** the 19:33 freeze;
  §3 line 233 still calls UF-2 **"UN-RUN"** and §7 proposes both as pass-3 **briefs**.

---

## §0 — WHAT THE RE-CUT FIXED, AND THE INTEGRITY DEFECT IT RE-INTRODUCED

**Credit where earned (the re-cut is a real improvement over the 15:12 draft).** Three defects the
first critique pass raised are genuinely closed: (1) PROTO-3 and 7C-T2 are folded (the leaf evaluator
now reads BLOCKED-missing-primitive, not "UNRESOLVED"; 7C-T2 reads RAN-PASS-but-no-op, not "un-run");
(2) **XL-C is now consumed as SETTLED fact** — §4 explicitly stops re-litigating the regex two-tree
fork ("the synthesis consumes XL-C as fact, does not re-litigate"), closing old-CRIT-2 §3-2; (3) the
BREACH-2 regex-report clobber and the XL-1/XL-2 dead-IDs are acknowledged in the §0 housekeeping note.
The synthesis also lands the genuinely load-bearing §0.6 correction (the counter is build-side, not
emit-blocked) — and **UF-2 then VINDICATED it** (the counter RAN, PASS). This is the strongest single
move of the pass and it correctly lifted 7C off the "counter is blocked" cap.

**BREACH-3 (recurrence) · The re-cut froze ~25 min before its own two flagship probes ran — the
THIRD consecutive instance of the same stale-artifact pattern.** CRIT-1 C-1 and old-CRIT-2 BREACH-1
both diagnosed "FAMILY-SPECS froze before its decisive probes." The 19:33 re-cut fixed *that*
instance (folding PROTO-3/7C-T2) and **immediately reproduced it**: §7 presents UF-2 and UF-1 as
"the two sharpest uncertainties (prototype briefs) … runnable TODAY … each with a numeric pass/fail,"
and §3 stamps UF-2 **"UN-RUN (the corrected flagship)."** Both RAN within 31 minutes of the freeze,
both with numeric verdicts (UF-2 PASS 1.0000×3+injected-0.9825; UF-1 52/53 charitable / 25/53 strict).
A registry cannot be scored on a synthesis that files as "future briefs" two probes that have executed
and that **materially move two of its four family verdicts** (§2). The fold-forward is owed a THIRD
time; the process defect is now structural, not incidental, and the gestalt must be told the synthesis
document lags its own evidence by one probe-cycle **every pass**.

---

## §1 — THE CROSS-LOCK HEADLINE, UPDATED BY UF-1: INC-5 IS NOT "ONE CROSS-REPO GAP"

The synthesis's unifying claim — repeated in §0, §4, §5, §6 — is that *"the whole bbnf leg rests on
ONE cross-repo gap: LOCK-5's engine-tier `match_at` (INC-5)."* **UF-1 refutes the "ONE gap" framing
by running the structural oracle the synthesis itself only proposed.** With leaves treated as opaque,
**52/53 bbnf rules are branch-isomorphic** through the `!has_rule("value")` sink-only lowering; the
`Seq/Alt/Optional/Repeat/CallRule` skeleton survives intact. So bbnf **IS structurally Tier-R at the
branch level** — directly contradicting §4's "bbnf is not even Tier-R." INC-5 resolves into **three
disjoint seams, only ONE of which is cross-repo:**

1. **Parser front-end (`?w`-then-`->`)** — in-repo, LOCK-7-owned, a bounded `parse_postfix_ops`
   refactor. **NOT cross-repo, and NOT landed** (my grep; §3-1).
2. **Leaf floor (`match_at`)** — the ONLY genuinely cross-repo seam (LOCK-5, parse-that, XL-C).
3. **One JSON materialization-role misfire (`bool_value` → `BbnfBool` on bbnf's `bool_lit`)** —
   in-repo, LOCK-7/LOCK-3-owned, a single named bug in the shared emitter.

**Consequence for convergence accounting:** the cross-repo CAP is real but it is **one-third of
INC-5, not the whole leg**. Two of three seams are in-lock bounded fixes LOCK-7 can author. The
synthesis's "ONE cross-repo gap → the whole leg blocked → hard cap on every bbnf-leg family" is an
**over-attribution to the cross-repo dependency**; UF-1 shows in-lock reachability is higher than the
synthesis's own headline concedes. The % should reflect a NARROWER cross-repo cap plus two in-lock
RED edges — not a monolithic cross-repo block.

**The second cross-lock finding UF-1 surfaces — grammar-neutrality is violated at a THIRD layer.**
§0.1's headline is that the emitter is *"grammar-NEUTRAL at the BRANCH level, grammar-SPECIFIC at the
LEAF level."* UF-1 shows a third stratum the synthesis omits: the **materialization-role layer**
(`derive_materialization_roles.bool_value` in `passes`) is **JSON-specific and misfires on bbnf**,
wrapping `"true"|"false"` in a `BbnfBool` `DirectBuild` sequence. This is JSON semantics leaking onto
the "general" path — a coupling that lives inside the **LOCK-3 codegen/passes tower (XL-B)**, not the
leaf floor. The two-level branch/leaf framing is incomplete; the general_descent emitter carries
JSON-specific role inference that contaminates every non-JSON grammar routed through it. Name this as
the emitter's third grammar-specific layer, homed in LOCK-3.

---

## §2 — PER-FAMILY CONVERGENCE (critic-2 floor) + ENUMERATED GAPS

### 7A — STAGED-BOOTSTRAP — **40%** (synth 40; concur, on hardened grounds)

The family remains a **corollary + a graft** (fixpoint LAW → 7D; fixpoint SUBJECT ≡ 7D's
`ORACLE_REL_V1`). Its survival test — "name ONE thing 7A owns post-graft that 7C+7D do not" — is
still **unanswered** in the re-cut; it is a disposition, not yet a family. Pass-2 HARDENED rather than
advanced it, on two counts my disk work adds:

Open gaps:
1. **Standalone existence unproven** — the survival test is stated (§1) and left open; 7A owns
   nothing 7C+7D do not, pending an answer.
2. **The one concrete pass-2 gain is NOT in the tree of record.** §0.6/§1 call the Wall-0 `?w`-fix
   "landed"; my grep shows `parse_postfix_ops` absent at HEAD with no uncommitted delta, and UF-2
   confirms the pristine bbnf closure fails to build at offset 662. So gate (a) "cleared … by the
   landed Wall-0 `?w`-fix" is **false as written** — the fix is worktree-local to the probes. Even
   the prerequisite parser fix 7A's whole leg rests on is unlanded (XL-H / clean-regen).
3. **Fixpoint gate is SPEC_ONLY** — never run on any artifact; the one runnable-without-LOCK-5 path
   (UF-3, on the regex self-emit artifact) was **NOT executed this pass** either. The elegant-reduction
   trap stands: "the fixpoint gate certifies non-vacuously" is the hard, un-run step.
4. **Fixpoint SUBJECT still hedged** (bytes-vs-GrammarIr) though §1 asserts the GrammarIr floor ≡
   `ORACLE_REL_V1`; a hedge is not convergence — collapse it decisively or the double-home persists.

### 7B — TOTAL-SELF-HOST — **32%** (synth 32; concur)

Open gaps:
1. **The INC-3 DATA/ESCAPE ratio went UN-RUN a THIRD pass.** DATA≈50 / WALK-FOLD≈20 / ESCAPE≈21 is a
   static line-count read (research-B §3); the synthesis's word "MEASURED" (§0-Plane-D) is unearned.
   The E0499/E0503 kernel-escape is *checkable* (delete `&mut _probe`, observe the borrow-fail) but
   was again asserted, not shown as a RAN compile-fail — SPEC_ONLY on the family's load-bearing claim.
2. **The shares sum to ~91%; ~9% of `emit.rs` is unattributed** — a measured total that does not
   close (CRIT-1 §2 concurs); name the residue or the ratio is soft at the margin the pole leans on.
3. **UF-1 refuted a load-bearing 7B structural premise (partial credit, partial de-risk).**
   research-B §4's predicted `term` 8-branch alternation collapse "at emit.rs:388" is **REFUTED** —
   at the `SinkOnlyExpr` level `term`'s Alt survives intact; the collapse is a render-time concern
   strictly downstream. Good for the branch skeleton, but it does **not** touch the ESCAPE ratio,
   which is the family's actual open claim and still un-run.
4. **Plane-A cross-repo BLOCKED, narrowed by UF-1** to the leaf floor only (not "the whole leg");
   **Plane-B is LOCK-3-conditional and consumer-less (Y.13) unless LOCK-3 picks 3C** (XL-B WORKING,
   not settled) — the family's own axis is contingent on another lock's unmade verdict.

### 7C — HOST-FUNCTION-LATTICE — **56%** (synth 57; held −1 for the vacuity discount)

The survivor, and UF-2 delivered the pass's best positive: the totality counter **RAN, PASS**,
build-side, three subsystems — the critics' "blocked behind the leaf evaluator" cap is genuinely
lifted. That earns 7C most of the synthesis's uplift. But UF-2 also exposes the ceiling the synthesis
under-states:

Open gaps:
1. **The flagship metric currently reads a NON-DISCRIMINATING CONSTANT.** UF-2's real-fleet numbers:
   json / sheets / bbnf-closure all = **1.0000** totality, because `Tier-2 = 0` — **no
   `ExprKind::Host` variant exists**, so no real grammar can reach Tier-2. The counter is non-vacuous
   **only under a synthetic injection** (json 1.0000 → 0.9825). §3 concedes this
   (`host_registry="skinny-none"` → fleet reads ~100%), which is honest — but a metric that reports a
   saturated 1.0 for every real subsystem cannot yet **discriminate** anything; it "runs" without
   yet "measuring." The uplift from "blocked" to "RAN" is real; the uplift to "a live totality
   signal" is not — that awaits the unbuilt Host node.
2. **The tier mechanism (`ExprKind::Host`, infer tier off `&mut Ctx`) is UNBUILT and LOCK-1-gated.**
   7C-T2 delivered a **shape-sniff on an always-empty slot** (`value.is_some()`), not the promised
   typed node; UF-2's own injection had to model Tier-2 as an `@host`-named `Annotation` sentinel
   *because no host node exists*. The family's defining construct is still absent.
3. **7C-T2 is a proven NO-OP** (dead name-gate; inverted negative control also byte-identical) — a
   gate that cannot fail on the committed fleet. Honest hygiene, correctly removes a latent GN-5.W3
   name-gate, but not evidence for the *lattice*.
4. **UF-2 contradicts the record on the frontend precondition** — the bbnf-closure run needed the
   **uncommitted** `?w->` fix (my grep confirms it is unlanded); the counter runs untouched only on
   **2/3** subsystems, not the "3/4" the synthesis implies.
5. **CSS floor UNMET until T4** (`@{}` = `BBNF-UNSUPPORTED`); **floor ∈ {0,1,2} is LOCK-1-pending** —
   the headline census count is definitional until LOCK-1's inherited-attribute verdict lands.

### 7D — META-CIRCULAR-ORACLE — **51%** (synth 51; concur, but stale against UF-1)

The strongest *measured* evidence in the lock (the regex ORACLE-ONLY row is an honest RAN RED), and
UF-1 **improves** the bbnf half — yet the synthesis is already stale against UF-1 and leans on
favorable framing on both its rows.

Open gaps:
1. **§4's "bbnf is not even Tier-R" is SUPERSEDED by UF-1** — 52/53 branch-isomorphic; bbnf **is**
   structurally Tier-R. The synthesis records UF-1 only as a §7 *brief*; its own family verdict
   contradicts the probe that ran 31 min later. Fold it: bbnf Tier-R is structurally reachable,
   blocked at three narrower seams (§1), not "not even Tier-R."
2. **UF-1's headline uses the CHARITABLE reading (52/53); the STRICT reading is 25/53.** Leading
   with 52/53 while strict is less than half the rules is the same favorable-of-two-readings framing
   old-CRIT-2 flagged for the regex parity headline. Report both; the strict gap (the `?w`→`Seq([])`
   rewrite over 27 rules) is a real leaf-blind-invisibility assumption, not free.
3. **UF-1 surfaced a NEW correctness bug in the shared oracle path** — `bool_lit` mis-materializes as
   JSON `BbnfBool`. A bounded, named defect, but it means the general emitter would **mis-emit** bbnf
   even once the leaf floor lands; it belongs in the ledger as a bbnf-row blocker, not omitted.
4. **Regex row: parity headline picks the highest of three sub-gate figures** (0.7931 Empty-normed vs
   0.7759 strict 45/58 vs 0.7843 committed-51). The synthesis §4 now correctly annotates "report as
   primary" the strict 0.7759 — credit — but the surrounding prose still leads Empty-normed. All three
   clear below 0.80 so the verdict is robust; report strict first throughout.
5. **0.80 gate provenance UNCITED** (§4 concedes "latent gate-tuning risk"); the **grammar-driven
   corpus sampler is unbuilt** (parity over committed 51/58 only — generality unmeasured); the 3
   `regex.bbnf` verdicts await LOCK-5's fold onto the ratified split (XL-C) — external remainder.

---

## §3 — CROSS-LOCK CONTRADICTIONS & DOUBLE-HOMING (critic-2's charter)

1. **MIS-SCOPED CROSS-REPO CAP — INC-5 is "ONE cross-repo gap" ⊥ UF-1's three seams.** The single
   most important cross-lock correction of the pass: two of the three INC-5 seams (parser front-end,
   `bool_lit` materialization) are **in-lock/in-repo**, only the leaf floor is cross-repo (LOCK-5,
   XL-C). The synthesis caps the whole bbnf leg on the cross-repo gap; the cap is narrower. §1.

2. **"LANDED" THAT IS NOT LANDED — XL-H / clean-regen breach.** §0.6 and §1-gate-(a) assert the
   Wall-0 `?w`-fix is "landed"; disk says otherwise (`parse_postfix_ops` absent at HEAD, no
   uncommitted delta; UF-2 confirms pristine-base build failure). Measurement/state must name its
   committed HEAD (XL-H); a bootstrap gate cannot cite a fix that lives only in probe worktrees.

3. **JSON-SEMANTICS CONTAMINATION INTO THE LOCK-3 TOWER — undeclared coupling.** The `bool_value`
   materialization role misfiring on bbnf (UF-1) is a grammar-specific behaviour inside the
   `general_descent`/`passes` emitter tower (LOCK-3, XL-B). §0.1's "branch-neutral / leaf-specific"
   dichotomy omits this third grammar-specific stratum. Name it as a LOCK-3-homed coupling.

4. **RESIDUAL DEAD-ID CITES — the CRIT-1 C-3 fix is only partial.** `XL-3` (line 197, the 7C
   LOCK-1-independence split) and `XL-5` (line 325, INC-5 one-regex-home) remain unmapped numeric
   cites; the §0 housekeeping note re-points only XL-1/XL-2. A settled fact cited by a dead ID is
   unverifiable per constitution-vii. Re-point `XL-3`→(LOCK-1 coupling; no lettered adjudication
   exists — either cite the relevant XL letter or drop the tag) and `XL-5`→**XL-C**.

5. **CONSISTENT / CREDIT — XL-C now consumed as fact.** §4 stops re-litigating the regex two-tree
   fork; old-CRIT-2 §3-2 is closed. PROTO-3's "skinny `bbnf-regex` is a classifier; the matcher is
   LOCK-5/parse-that's" is consistent with XL-C one-regex-home and the top-level-`bbnf-regex`-at-GN-0
   verdict. No contradiction; coupling correctly directed.

6. **DOUBLE-HOME correctly collapsed** — 7A's fixpoint SUBJECT ≡ 7D's `ORACLE_REL_V1` is named as
   convergence, not double-homed. The remaining risk is that 7A, post-collapse, has no standalone
   content (§2-7A-1).

7. **CROSS-PROBE OFFSET INCONSISTENCY (minor).** UF-1 reports the closure `?w->` wall at byte 2343;
   UF-2 reports it at offset 662 (single-anchor bbnf.bbnf). Plausibly a concatenation-order artifact,
   but two probes citing different offsets for the same wall should be reconciled before the ledger
   fold, lest the front-end seam read as two bugs.

---

## §4 — FAILURE-MODE CHECKLIST HITS (enumerated)

- **Vacuous convergence / gate-that-cannot-fail** — 7C-T2 PASS (dead heuristic; inverted control also
  byte-identical) [§2-7C-3]; **the totality counter reads a saturated 1.0000 on every real grammar**
  (Tier-2 impossible until the Host node lands) — runnable but non-discriminating [§2-7C-1]; 7A's
  fixpoint gate SPEC_ONLY with no ran falsification, and UF-3 un-run this pass [§2-7A-3].
- **Elegant-reduction trap ("and-then-the-hard-part")** — §5's "lawful gestalt shape" reduces the
  bbnf leg to "ONE gap: the leaf evaluator." UF-1 shows the leaf floor is real but is one of three
  seams, and the leaf floor itself *is* the original problem (author a regex engine) — the reduction
  both over- and under-states: over-attributes to cross-repo, under-states the leaf floor's difficulty.
- **SPEC_ONLY anchoring a claim** — 7B's DATA/ESCAPE "MEASURED" is a static line-read, un-run a third
  pass; the E0499 borrow-fail asserted, never shown as a RAN compile-fail [§2-7B-1].
- **Consumer-less substrate (Y.13)** — 7B Plane-B `ir.bbnf` unless LOCK-3 picks 3C [§2-7B-4]; 7A as a
  standalone family (corollary + graft) [§2-7A-1].
- **Stale self-citation (spec-cites-a-frontier-already-crossed)** — BREACH-3: §7 proposes UF-1/UF-2
  as briefs and §3 stamps UF-2 "UN-RUN" though both RAN, moving 7C and 7D verdicts [§0].
- **Prototype-evidence framing smell** — UF-1's 52/53 charitable headline over 25/53 strict
  [§2-7D-2]; regex parity prose still leads Empty-normed over the strict primary [§2-7D-4].
- **State-not-named / "landed" that is not landed** — XL-H breach on the Wall-0 `?w`-fix [§3-2].
- **NOT hit (checks that pass, stated for completeness)** — no legacy alias, no masked fallback;
  7C-T2 correctly REMOVES a latent name-gate; `general_descent` is real (1206 LOC, not a stub);
  XL-C is now consumed, not re-litigated (a fixed prior hit); the gestalt/DAG edge is genuine, not
  unverified.

## §5 — VERDICT

The re-cut is a **genuine improvement that arrived one probe-cycle stale, a third time.** It folds
PROTO-3/7C-T2 and consumes XL-C correctly — then freezes 25 minutes before UF-2 and UF-1 run, so its
two flagship briefs are already answered (UF-2 PASS lifts 7C's counter off "blocked"; UF-1 refutes
"bbnf is not even Tier-R" and splits INC-5 into three seams, only one cross-repo). The lock's honest
terminal shape: **7C is the survivor (its counter now RUNS but reads a non-discriminating 1.0 until
the unbuilt Host node lands); 7D is the measured-negative posture winner (regex ORACLE-ONLY below
gate; bbnf structurally Tier-R but blocked at leaf-floor + front-end + one materialization bug); 7A is
a corollary+graft whose one gain is not even committed; 7B's maximal pole rests on a static read
un-run for a third pass.** The cross-repo cap is real but **narrower than the synthesis states** — two
of three INC-5 seams are in-lock. Before this folds to REGISTRY: fold UF-1/UF-2 (BREACH-3); correct
"landed" to "worktree-local, unlanded" (§3-2); re-point `XL-3`/`XL-5` (§3-4); add the `bool_value`
JSON-contamination coupling to the LOCK-3-homed ledger (§3-3); and hold the bbnf-leg families to the
NARROWED cross-repo cap plus two in-lock RED edges, not a monolithic block. Design clarity rose again;
reachability is now *higher in-lock than the synthesis concedes* on the bbnf branch skeleton, and
*lower* than "ships a live totality signal" on 7C. Critic-2 floors: **7A 40 · 7B 32 · 7C 56 · 7D 51.**

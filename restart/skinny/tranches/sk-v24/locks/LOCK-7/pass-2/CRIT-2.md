# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 CRIT-2 (ADVERSARIAL, cross-lock weighted)

Critic 2 of 2. Read: PORTFOLIO (LOCK-7 + CONSTITUTION), CROSS-LOCK.md, FAMILY-SPECS.md (synth,
15:12), PROTO-1 (13:37), PROTO-2 (15:26, on-disk), PROTO-3 (15:25), research A/B/C/D/priorart, the
pass-1 REGISTRY. Load-bearing disk facts re-greped at HEAD this pass (numbers below). As critic 2 I
weight the CROSS-LOCK view: where LOCK-7 contradicts, double-homes, or under-weights its coupling to
the extant GN/PT codex and the other locks.

**Disk verifications I ran (constitution vii — not doc-quoting):**
- `grep -rE 'struct (Dfa|Nfa|Program|Instruction)\b|transition_table|fn match_at' skinny/crates`
  → **EMPTY**. PROTO-3's central claim is TRUE: skinny has **no regex-execution engine at any tier**.
- `general_descent/{mod,emit,regex_first,view}.rs` = **1206 LOC** (confirmed).
- `has_parse_time_host_decode` at `passes/src/lib.rs:794`; consumers `:620`, `:764` (confirmed).
- The `?w` marker `{name:"ws"}` built at `grammar/src/lib.rs:1373` — the **only** annotation the
  parser constructs (confirms 7C-T2's vacuity basis).
- `bbnf-regex` `RegexKind = {Whitespace, QuotedString, Numeric, Unknown}` at `lib.rs:16` — a
  4-bucket **classifier**, no consume/match method (confirms PROTO-3).

---

## §0 — THE TWO BOOKKEEPING BREACHES THAT MUST BE FIXED BEFORE AGGLOMERATION

These are not family critiques; they are integrity defects in the pass-2 artifact set that would
corrupt the REGISTRY fold if carried silently.

**BREACH-1 · The synthesis is STALE against its own prototypes; two `§7` pass-3 briefs ALREADY
RAN.** FAMILY-SPECS froze at **15:12**. **PROTO-3** (leaf evaluator = §7 **Brief A**) ran at **15:25**
and **PROTO-2/7C-T2** (§7 **Brief B**) ran at **15:26** — *after* the synthesis. So §7's "these are
the pass-3 successors, chosen for maximal advance" proposes as future work **two probes that were
executed 13 minutes later**. Worse, PROTO-3 does not merely "run Brief A" — it **resolves the lock's
central open question to a HARDER verdict** than the synthesis records. §0.2/§6 still call the leaf
evaluator "UNRESOLVED … BLOCKED-candidate … whether a bounded arm-swap or a missing primitive is
UNRESOLVED." **PROTO-3 answers it: MISSING PRIMITIVE, cross-repo, equal to the original problem.**
The synthesis's own headline conclusion ("the whole bbnf leg rests on ONE newly-exposed gap: the
leaf evaluator [Brief A]") is written as an *open* gap; it is a *measured-closed-negative* gap. The
FAMILY-SPECS must be re-synthesized against PROTO-3, or the REGISTRY must fold PROTO-3 directly —
either way, treating §6/§7's "UNRESOLVED / pass-3" as current is a stale-artifact error of the exact
kind pass-1's own REGISTRY warned about ("FAMILY-SPECS §0 predates both definitive probes and was
never reconciled" — it happened AGAIN).

**BREACH-2 · `PROTO-2.md` was OVERWRITTEN; the synthesis's load-bearing regex citation is orphaned.**
FAMILY-SPECS intro names "the two pass-2 prototypes that RAN (PROTO-1.md OPEN-1' cascade; **PROTO-2.md
regex Tier-S**)," and §4/§5-XP-4 cite "PROTO-2" for the parity=0.7931 oracle **and** for the `@host
resolve_refs` CONTEXT instance. The **on-disk `PROTO-2.md` (15:26) is 7C-T2** — an entirely different
probe. The regex-oracle content now survives only in `PROTO-2.patch` (a parse-that scratch copy's
`oracle_7d.rs`) and `pass-1/PROTO-2-7D-regex-oracle.md`. A fresh auditor verifying §4's "0.7931" or
§5's "resolve_refs" against `PROTO-2.md` finds a 7C dead-code probe instead. The 7D regex row — the
**strongest measured result in the lock** — has lost its primary evidentiary anchor to a filename
collision. Re-home it (`PROTO-2-7D-regex.md`) and re-point every §4/§5 citation before fold.

---

## §1 — THE CROSS-LOCK HEADLINE THE SYNTHESIS UNDER-WEIGHTS

**Constitution clause (i) — "the regex grammar IS bootstrapped and bbnf-defined" — is UNMET AT THE
SUBSTRATE LEVEL on skinny, and LOCK-7 cannot meet it.** PROTO-3 + my grep prove skinny holds *no
executable regex engine*: opaque interned string (grammar tier) + a 4-bucket first-set classifier
(`bbnf-regex`) + JSON-delimiter-fixed scanners (`parse-that-regex`). A *bootstrapped* regex grammar
presupposes an engine to run the bootstrapped parser against; there is none. The engine is **LOCK-5's
deliverable**, and per **CROSS-LOCK XL-C (RATIFIED)** its canonical home is **parse-that** (one-regex-
home; skinny's copy is measurement substrate; the top-level `bbnf-regex` archives at GN-0). Therefore:

> **7A (recognizer/builder seed), 7B-Plane-A, and 7D-bbnf (Tier-R and Tier-S) cannot converge inside
> LOCK-7. They are downstream of a CROSS-REPO lock (LOCK-5, homed in parse-that). Three of four
> families are gated on a primitive LOCK-7 does not own and cannot author.**

The synthesis states this (INC-5) but files it as "a DAG edge the omega gestalt must carry, not a
family incompatibility." That is true and *insufficient*: an out-of-lock, cross-repo BLOCK is a hard
**cap on convergence %** for every family that rides it, not a routing footnote. INC-5 is the most
important fact in the lock and it should headline every affected family's disposition, with the % held
down accordingly. The pass-1→pass-2 trajectory for the bbnf leg is a **regression in reachability**
(pass-1: "bounded builder-augment, disk-located, in-lock"; pass-2: "missing engine, cross-repo") even
as design clarity rose — the % must reflect the block, not the clarity.

---

## §2 — PER-FAMILY CONVERGENCE (critic-2 floor) + ENUMERATED GAPS

### 7A — STAGED-BOOTSTRAP — **40%** (pass-1 42%; held down −2)

The family has **near-zero independent content left**, and pass-2 sharpened the block, not the route.
Its two deliverables both collapse into other families: (i) the fixpoint LAW is *grafted onto 7D*
(OPEN-2, XP-2) — not standalone; (ii) the fixpoint SUBJECT is, by §5's own "convergence, named," **the
same object as 7D's `ORACLE_REL_V1`** — not standalone. Its gate (byte-identical `regen∘regen`)
presupposes bbnf self-emit, now **BLOCKED cross-repo** (§1/PROTO-3). The one concrete pass-2 gain
(Wall-0 `?w`-attach fix, +12 lines, json ERR→Ok) is a *parser* fix that is a **prerequisite of the
leaf-evaluator/7D leg**, not a 7A deliverable. **Vacuous-family risk: 7A is a corollary + a graft, not
a route** — this is the standing open question and it is unresolved.

Open gaps:
1. **Standalone existence unproven.** After the fixpoint-law graft (→7D) and the shared-relation
   convergence (→7D), what does 7A own that 7D+7C do not? Un-answered; the family may be a
   disposition, not a family.
2. **Fixpoint gate is born-RED and cannot go GREEN inside the campaign's control** — presupposes bbnf
   self-emit, BLOCKED on LOCK-5's engine (§1). No non-vacuous determinism evidence exists (the
   88s→0.5s datum REFUTED as legacy transplant; the 3× byte-identical "determinism" is a fixed-const
   CSS path per pass-1, still uncorrected as *positive* evidence).
3. **Fixpoint SUBJECT still hedged** (bytes-vs-GrammarIr) though §5 asserts the GrammarIr floor ≡
   ORACLE_REL_V1 — if so, say it decisively and delete the bytes branch; a hedge is not convergence.
4. **The non-vacuity witness (planted input-mutation MUST change the bytes) is DESIGNED, not RUN** —
   gate (c) is SPEC_ONLY; a fixpoint law with no ran falsification is a gate-that-cannot-fail until
   exercised.

### 7B — TOTAL-SELF-HOST — **32%** (pass-1 33%; held ≈flat −1)

Open gaps:
1. **INC-3 / Plane-D data-vs-escape ratio was the pass-1 directive's "sharpest UN-INSTRUMENTED
   experiment" — and went UN-RUN AGAIN in pass-2.** No probe converted an emitter blob toward data and
   *measured* the escape fraction. The DATA≈50% / WALK-FOLD≈20% / ESCAPE≈21% split is a **static
   line-count read (research-B §3), not a running prototype** — the synthesis's word "MEASURED" is
   generous. (The E0499/E0503 borrow argument *is* checkable — delete the `&mut _probe` re-alias and
   observe the compile-fail — but that too is asserted, not shown as a ran compile-fail in pass-2.)
   So the maximal-pole refutation rests on reading, not running: a constitution-vii SPEC_ONLY smell on
   the family's load-bearing claim.
2. **Plane-A is cross-repo BLOCKED** (§1), same gap as 7A/7D-bbnf — not 7B-distinguishing.
3. **Plane-B (`ir.bbnf`) is LOCK-3-conditional and consumer-less (Y.13) unless LOCK-3 picks 3C** —
   the synthesis correctly routes this to LOCK-3 but the family cannot converge while its own axis is
   contingent on another lock's unmade verdict. Note the ~91% (50+20+21) accounting leaves ~9%
   unattributed on `emit.rs` — where?
4. **BLOCKED-candidate on a missing primitive** (grammar-plane backtracking) that §6 concedes "is
   authoring a combinator/VM runtime" = LOCK-4C. Correct disposition (banked), but it means 7B's
   convergence is a *ceiling*, not a *floor* — 33%→32% is the honest cap.

### 7C — HOST-FUNCTION-LATTICE — **54%** (pass-1 55%; held ≈flat −1)

The survivor, and pass-2 landed its cheapest gate (7C-T2 PASS) — pass-1 open-gap (b) "zero gate
executed" is genuinely CLOSED. But the closure reveals the win is **thinner than the "one-file cure,
ships now" headline**, and the deliverable-gap HARDENED.

Open gaps:
1. **The 7C-T2 PASS is near-VACUOUS — a gate that cannot fail on the current fleet.** PROTO-2's own
   numbers: the string heuristic *never fires* on any committed grammar (`"ws".contains("host")` is
   false; no host annotation exists). The structural replacement is byte-neutral **because the code is
   DEAD**, not because the new predicate reproduces host-decode semantics. The **negative control** —
   the *inverted* predicate `value.is_none()` that fires `true` on every `?w` — **ALSO passes
   byte-identical**. The proto frames this as "robustness"; it is in fact proof that **eager-tape
   selection does not flow from this predicate at all** on the committed fleet. So the RAN result
   certifies *dead-code deletion is safe*, NOT that the tier mechanism is validated — there is no
   grammar in the corpus that exercises a parse-time host decode, so byte-identity is guaranteed
   regardless of predicate. "One-file cure, ships now" is RAN, but what ships is a dead-heuristic
   removal. (Honest, useful, and worth doing — but not evidence for the *lattice*.)
2. **The family's DEFINING deliverable — the totality% counter — rides the BLOCKED leaf evaluator**
   (§1). The synthesis concedes "holds for the gates, NOT the deliverable"; but a family whose
   *metric-that-names-it* is cross-repo blocked cannot exceed the mid-50s. This gap HARDENED pass-2
   (leaf evaluator went from "open" to "BLOCKED-missing-primitive").
3. **The tier mechanism (infer tier off `&mut Ctx`) is UNBUILT and LOCK-1-gated** — typed `HostFnSig`
   into inference (T1/T5) needs LOCK-1 to admit a typed leaf. The CONTEXT-tier zero-cost invariant is
   proposed (`resolve_refs`), never round-tripped; TOML `push_path` unbuilt.
4. **CSS floor UNMET** until T4 (`@{}` is `BBNF-UNSUPPORTED`; `color.bbnf:220`/`values.bbnf:69` depend
   on it) — CSS totality% is a paper number.
5. **The floor ∈ {0,1,2} is a LOCK-1-dependent number, not fixed** — the family's headline census
   count is definitional pending LOCK-1's inherited-attribute verdict.

### 7D — META-CIRCULAR-ORACLE — **51%** (pass-1 53%; held down −2)

The posture winner and the strongest *measured* evidence in the lock (the regex ORACLE-ONLY row is a
genuinely RAN, honest RED). But pass-2 HARDENED its bbnf leg to BLOCKED and its regex parity fell
below the gate — both push the honest number down, not up.

Open gaps:
1. **bbnf Tier-R AND Tier-S are now hard-BLOCKED cross-repo** (PROTO-3, §1), not merely "gated on
   Brief A." Pass-1 said "gen side buildable NOW at recognizer granularity via `general_descent`" —
   **PROTO-1 refuted even that** (`general_descent`'s leaf lexer cannot tokenize a single bbnf regex
   leaf). So bbnf is **not even Tier-R**. The synthesis records this; the % must follow.
2. **Regex parity DROPPED below the gate pass-to-pass and the drop is un-reconciled.** Pass-1
   REGISTRY: "0.76 structural, **0.80 under Empty≡Concat([])**" — *at* the 0.80 gate. Pass-2: **0.7931
   Empty-normed — BELOW** it. The difference is promotion-eligible vs BLOCKED. Which normalization is
   canonical? PROTO-2.patch's `ORACLE_REL_V1` is the pinned relation, so 0.7931 supersedes — but the
   synthesis presents 0.7931 as settled without flagging that it *flipped* the pass-1 at-gate number.
   Name the reconciliation.
3. **Parity headline picks the most favorable of three sub-gate figures** — 0.7931 Empty-normed vs
   0.7759 strict (45/58) vs 0.7843 committed-51. All below 0.80 so the verdict is robust, but leading
   with the highest is a mild prototype-evidence framing smell; report the strict figure as primary.
4. **"Canonical regex `.bbnf` is LOCK-5-substrate-fork-BLOCKED (XL-2/XL-7)" is PARTLY STALE** — the
   two-tree fork is **RATIFIED in CROSS-LOCK XL-C** (grammar tier opaque `Regex(StringId)`; engine
   tier rich `Hir`; string bridge). The synthesis re-litigates as "fork-blocked" a question the
   orchestrator has already adjudicated; it should consume XL-C as settled and re-characterize the gap
   as "awaiting LOCK-5's fold *onto the ratified split*," not "blocked on an unmade fork."
5. **The grammar-driven corpus sampler is unbuilt** — parity is over the committed 51/58 only; the
   golden anchor is a fixed set, so the parity number's *generality* is unmeasured.

---

## §3 — CROSS-LOCK CONTRADICTIONS & DOUBLE-HOMING (critic-2's charter)

1. **DOUBLE-HOME (bbnf leaf evaluator) — 7A/7B-A/7D-bbnf ⊗ LOCK-5 + LOCK-9-9C, CROSS-REPO.** Correctly
   named (INC-5) but under-weighted (§1). The required primitive ("a general regex matcher `fn
   match_at(pattern,input,start)->Option<end>`") **exists nowhere in either repo's skinny-visible
   surface** (my grep). It is LOCK-5's engine, homed in parse-that (XL-C). This is a **cross-repo DAG
   edge the omega gestalt must carry as born-RED**, and it caps three families. Not a within-lock choice.
2. **STALE-VS-RATIFIED — 7D's "substrate fork blocked" ⊥ XL-C.** As §2-7D-gap-4: the lock treats the
   regex two-tree split as an open LOCK-5 fork; CROSS-LOCK XL-C RATIFIED it. Re-point to XL-C.
3. **CONSISTENT (good) — XL-5 / one-regex-home.** PROTO-3's finding (skinny's `bbnf-regex` is a
   classifier; the matcher must come from LOCK-5/parse-that) is *consistent* with XL-C's one-regex-home
   and the top-level `bbnf-regex`-archives-at-GN-0 verdict. No contradiction; the coupling is real and
   correctly directed.
4. **XL-6 self-refutation still un-declared.** The pass-1 REGISTRY flagged that FAMILY-SPECS lists
   `sheets` as a live `general_descent` target *while* the bbnf leg treats the general emitter as
   inadequate — Sheets IS the general-emitter existence proof. Pass-2 resolves the *emitter-exists*
   half (§0.1) but the **leaf-lexer is Sheets/JSON-specific** (PROTO-1 Wall-1): the emitter exists and
   is grammar-neutral *at the branch level* but grammar-SPECIFIC *at the leaf level*. That nuance is
   the whole make-or-break and it should be stated as such in the GN re-cut, not left implicit.
5. **7B-Plane-B ⊗ LOCK-3 3C (XL-1) and 7C floor ⊗ LOCK-1 (XL-3)** — both correctly routed; both mean
   the affected axes cannot converge inside LOCK-7. Fine as routing, but they are convergence caps.

---

## §4 — FAILURE-MODE CHECKLIST HITS (enumerated)

- **Vacuous convergence / gate-that-cannot-fail** — 7C-T2 PASS (dead heuristic; inverted control also
  passes byte-identical) [§2-7C-1]; 7A's fixpoint gate born-RED with no ran falsification [§2-7A-4].
- **The elegant-reduction trap ("and-then-the-hard-part")** — §5's "lawful gestalt shape" rests on the
  bbnf leg being reachable via "ONE gap: the leaf evaluator." PROTO-3 shows that one gap **IS the
  original problem** (author a regex engine). The load-bearing reduction step is unconverged and
  cross-repo [§1].
- **SPEC_ONLY anchoring a claim** — 7B's DATA/ESCAPE ratio "MEASURED" is a static line-count read, not
  a run; the INC-3 running experiment went un-run a second pass [§2-7B-1].
- **Consumer-less substrate (Y.13)** — 7B-Plane-B `ir.bbnf` unless LOCK-3 picks 3C [§2-7B-3]; 7A as a
  standalone family (corollary + graft) [§2-7A-1].
- **Spec-cites-itself / stale self-citation** — BREACH-1 (§7 proposes as pass-3 two probes already run;
  §6 calls "UNRESOLVED" what PROTO-3 resolved) and BREACH-2 (PROTO-2.md overwritten, §4/§5 orphaned).
- **Prototype-evidence misframing** — parity headline picks the highest of three sub-gate figures and
  presents a pass-to-pass parity DROP (0.80→0.7931) as settled without flagging the flip [§2-7D-2/3].
- **Re-litigating a CROSS-LOCK adjudication** — 7D's "substrate fork blocked" vs ratified XL-C [§3-2].
- **Constitution (i) unmet at substrate** — no regex engine in skinny; the lock's raison-d'être is
  cross-repo blocked [§1]. **Not** a name-gate/legacy-alias/masked-fallback hit — those checks pass
  (7C-T2 correctly REMOVES a latent name-gate; no fallback masking found; general_descent is real, not
  a stub — the disk confirms 1206 LOC).

## §5 — VERDICT

The lock is **HONEST but THIN, and its pass-2 artifact set is internally out of date.** The two
genuinely load-bearing RAN probes (7D regex oracle; PROTO-3 leaf evaluator) both return NEGATIVE
(ORACLE-ONLY / BLOCKED-missing-primitive); the one PASS probe (7C-T2) is dead-code removal. Nothing
self-hosts; three of four families are cross-repo BLOCKED on LOCK-5's non-existent regex engine. The
survivor (7C) ships a dead-heuristic deletion now and its defining metric later. **Before this folds
to REGISTRY, fix BREACH-1 (re-synthesize/fold against PROTO-3) and BREACH-2 (re-home the overwritten
PROTO-2 regex evidence), and cap the bbnf-leg families' convergence to reflect the cross-repo block —
design clarity rose, reachability fell, and the % must track the latter.** Critic-2 floors: **7A 40 ·
7B 32 · 7C 54 · 7D 51.**

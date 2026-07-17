# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 CRIT-1 (adversarial, WITHIN-LOCK)

Critic 1 of 2. Weight: internal soundness of each family spec + whether the four RAN prototypes
tested the *sharpest* uncertainty, not a lesser one. Cross-lock adjudication (XL-A…XL-H) is
consumed as binding fact; challenges are noted for the gestalt, not re-litigated here. Written
against the 19:33 FAMILY-SPECS re-cut (supersedes the earlier round-1 CRIT-1 file).

## §A — FACTS I RE-VERIFIED ON THE LIVE SUBSTRATE (not taken on trust)

I re-greped `skinny/crates` at live HEAD `06e70053` (the prototypes base-pinned to
`c93fc4314`/`ce51c36fc`, both ancestors; the structural facts below are branch-invariant). **Every
load-bearing disk citation in FAMILY-SPECS is accurate** — a materially higher factual-grounding
score than most locks this campaign:

- regex-engine grep `struct (Dfa|Nfa|Program|Instruction)|transition_table|fn match_at` over
  `crates/` = **EMPTY** (confirmed). INC-5 / §0.2 stands: no general matcher at any tier.
- `general_descent/{emit,mod,regex_first,view}.rs` = **452/93/230/431 = 1206 LOC** (exact).
- `has_parse_time_host_decode` at `passes/src/lib.rs:794` (exact), recursive, keyed on node shape.
- `bbnf-regex::RegexKind` = `{Whitespace,QuotedString,Numeric,Unknown}` at `lib.rs:16`; `classify`
  is a hand string-prefix bucketer, **no consume/match method** (confirmed 7C/PROTO-3 claim).
- `xtask` carries `regen-{json,sheets,css,real-typed}` + `check-*`; **NO `regen-bbnf`, `regen-regex`,
  `fixpoint`, `BBNF_TARGET`** (confirmed §0.4 — stage-0 is net-new, not transplantable). `SHEETS_TARGET`
  exists (`main.rs:200`) — the mirror 7A proposes for `BBNF_TARGET`.
- **`ExprKind::Host` does NOT exist** (grep empty). This is the pivot for CRIT-1-α below.
- `add_expr:51` / `resolve_refs:84` / `ExprKind` enum `:234` / `RuntimeConstructKind::HostCapture →
  "BBNF-UNSUPPORTED-HOST-CAPTURE":222` / `parse_directive "ws":1323` — all exact.

**Consequence for grading.** The synthesis's *factual* spine is sound. My findings are therefore not
"the facts are wrong" but "the convergence percentages over-reward *runnability* and *design
clarity* over *discriminating signal*, and three families lean on experiments explicitly labeled
NEVER-RUN." The synthesis itself says "% tracks reachability, not clarity" (§0 close) — I hold it to
that and dock where a RAN prototype tested a lesser question than the family's real make-or-break.

---

## §B — 7A · STAGED-BOOTSTRAP — FAMILY says BANKED 40% → **CRIT-1: 38%**

**Internal soundness: the family fails its OWN survival test, and the test is unrun.** §1 states the
survival condition verbatim: "name ONE thing 7A owns post-graft that 7C+7D do not, or 7A RETIRES."
The spec then answers its own question — after the fixpoint-law graft onto 7D and the shared-relation
convergence (7A's GrammarIr floor ≡ 7D's `ORACLE_REL_V1`), 7A "owns **nothing 7C+7D do not**" except
**UF-3** (the byte-fixpoint signal on the regex artifact). So 7A's entire non-redundant existence
reduces to a single experiment that **has never run** (scheduled "the pass after UF-1/UF-2 land").

**The elegant-reduction step, correctly self-flagged but still un-discharged.** Gate (c) — the
cross-generation byte-fixpoint + GrammarIr floor + planted-mutation non-vacuity — is marked
**NEVER RUN on any artifact (SPEC_ONLY)**. This is the canonical elegant-reduction: the family's one
distinctive asset (the fixpoint LAW) is the unconverged "and-then-the-hard-part." The synthesis is
honest that it is SPEC_ONLY and that UF-3 is runnable *today* on the regex producer without LOCK-5 —
which is exactly why 7A should not be at 40% until UF-3 actually runs. A family whose sole survival
warrant is an unrun probe is a **RETIRE-candidate held open by one pending experiment**, not a 40%
bank.

**A genuine, un-measured hazard the spec waves through.** §1's inflation argument ("a *building* seed
inflates PAST the 2142-LOC hand parser, dissolving 7A's auditability") is argued **entirely from an
in-repo analogy** (the parse-that regex self-host = 2293+524 LOC for a *smaller* language). No
building seed for bbnf was ever emitted or counted. The whole "auditability is 7A's selling point and
a building seed destroys it" thesis — the thing that pushes 7A to BANKED rather than ADVANCED — rests
on an un-run LOC projection. Flag it as such.

**Enumerated open gaps (7A):**
1. Survival test UNMET and UNRUN — 7A owns nothing post-graft but UF-3, which has not run. RETIRE or
   run UF-3 next pass; 40% presumes a bank the evidence does not yet earn.
2. Fixpoint gate (c) = SPEC_ONLY, never run on any artifact — the elegant-reduction step.
3. Building-seed inflation thesis is an in-repo *analogy*, never measured on a real bbnf building seed.
4. Byte-gate vacuity self-acknowledged (template-splice `include_str!` makes bare byte-identity a
   constant-stamp) — the planted-mutation-that-moves-bytes sub-gate is specified but unbuilt.
5. Recognizer seed BLOCKED cross-repo on LOCK-5 `match_at` (INC-5) — inherited cap, not 7A-internal.

---

## §C — 7B · TOTAL-SELF-HOST — FAMILY says BANKED 32% → **CRIT-1: 30%**

**Internal soundness: the central quantitative claim is a static line-count read that does not sum to
100 and was NEVER RUN.** §2 Plane-D's headline — DATA ≈ 50% / WALK-FOLD ≈ 20% / IRREDUCIBLE ESCAPE
≈ 21% — is a static read of `emit.rs` (452 LOC). The synthesis itself concedes the shares sum to
**~91%** ("~9% of `emit.rs` unclassified — name the residue or the ratio is soft"). A 9%
unclassified residue on the number that decides whether "the generator becomes mostly data" is not a
rounding nit: it is enough to swing the DATA-vs-ESCAPE verdict, and it is unnamed.

**The twice-un-run experiment.** The E0499/E0503 borrow-fail that anchors the "IRREDUCIBLE ESCAPE"
claim (delete the `&mut _probe` re-alias inside the speculative-cursor IIFE → compile-fail) is
"*checkable* but asserted, not shown as a RAN compile-fail" (§2) and INC-3 confirms "no probe
CONVERTED a blob and measured the escape by RUNNING." So the load-bearing claim — that the ~21% is a
missing primitive *equal in difficulty to authoring a VM* — is inferred from reading, never
demonstrated. For a family whose whole thesis is a difficulty classification, the difficulty was not
measured. This is cheap to run (one deletion, observe rustc) and its absence is the sharpest 7B gap.

**Cross-lock gating correctly identified (not a ding, credit).** Plane-A BLOCKED cross-repo (same
leaf primitive, INC-5); Plane-B conditional on LOCK-3 picking 3C, and the consumer-less-substrate
hazard (Y.13) is correctly named if 3C is declined. Plane-B's dependency chain (node-value `->`
projections presuppose LOCK-1 GN-3 Walls 1–2) is honestly stated. These are lawful external caps.

**Enumerated open gaps (7B):**
1. The 50/20/21 Plane-D split is a static read, NEVER RUN; shares sum to ~91% — 9% residue unnamed.
2. E0499/E0503 escape = checkable-but-asserted; the compile-fail experiment is twice-un-run.
3. Plane-A BLOCKED cross-repo (INC-5, LOCK-5 `match_at`).
4. Plane-B consumer-less unless LOCK-3 adopts 3C; live-node construction gated on LOCK-1 (GN-3 W1–2).
5. Maximal pole ("generator becomes mostly data") is BLOCKED-candidate on the grammar-plane
   backtracking primitive — self-labeled equal-in-difficulty, prior-art-uniform (no production
   system erases the kernel), un-refuted but also un-instrumented.

---

## §D — 7C · HOST-FUNCTION-LATTICE — FAMILY says ADVANCED 57% "the SURVIVOR" → **CRIT-1: 50%**

**This is the sharpest within-lock finding of the pass. CRIT-1-α — the flagship metric is a gate
that CANNOT FAIL on any real grammar, and the RAN prototype tested the wrong sharpness.**

The 7C deliverable is the two-population totality% counter, `totality% = 1 − Σ(Tier-2)/Σ(all)`.
Tier-2 = "a host function threading `&mut Ctx`." **But `ExprKind::Host` does not exist** (I re-greped:
empty). PROTO-1/UF-2's own METHOD says it plainly: *"The IR has NO `ExprKind::Host` variant today, so
NO real grammar can reach Tier-2."* Therefore on **every real grammar in the committed fleet**,
Σ(Tier-2) ≡ 0 and totality% ≡ **1.0000 exactly** — json, sheets, and the bbnf 53-rule closure all
read 1.0000 (UF-2 table). The metric is a **constant** on all real inputs. The only sub-1.0 number in
the entire probe (json 1.0000 → 0.9825) came from **injecting a synthetic sentinel** — modeled as an
`@host`-named `Annotation`, explicitly *not* the `ExprKind::Host` node the tier is defined over,
*because that node does not exist*.

So UF-2's "PASS — RAN_MEASURED" answers the question *"does the counter execute build-side without
the leaf evaluator?"* — a real uncertainty, and yes, it does; the critics' build-vs-emit conflation
IS corrected, credit where due. But it does **not** answer the sharper question that governs 7C's
value: *"does the counter produce a discriminating signal on real grammars?"* The answer to that is
**NO** — it is definitionally 1.0 until a real Tier-2 host lands, and the check-totality floor-gate
therefore **cannot trip on the current fleet** (falsifiable only by synthetic injection). This is the
textbook "gate that cannot fail" + "vacuous convergence" pair, sitting on the SURVIVOR family's
flagship. The synthesis discloses it (§3: "the counter reads ~100% until a real Tier-2 host is
wired… its floor is exercised only once a genuine host lands") — but then banks 57% and calls it
"ships value *now*." Disclosure is not discharge. The value shipped now is a *runnable harness over a
metric with no live signal*.

**CRIT-1-β — the "lock-free trio that ships value now" is one proven no-op + one unbuilt node + one
designation.** Decompose what actually ADVANCES 7C:
- (1) delete `has_parse_time_host_decode` — **PROTO-2/7C-T2 proved this is a NO-OP.** The predicate
  AND its logical inversion are both byte-identical across all four `check-*` targets, proving eager-
  tape selection *does not flow from it at all* on the committed fleet (`"ws".contains("host")` is
  always false; the only annotation built is `?w`). Honest hygiene (removes a latent GN-5.W3 name-
  gate), but "ships value" overstates a byte-for-byte no-op. §3 itself downgrades this to "the win is
  thinner than 'one-file cure, ships now' reads."
- (2) lower `@{}` → real `ExprKind::Host` — **UNBUILT.** T2 keyed on `value.is_some()`, a shape-sniff
  on an always-empty slot, *not* the typed node. §3: "T2 discharged the string-gate *refutation*, not
  the *construction*."
- (3) name `resolve_refs` the CONTEXT floor — **designation only, no code.**
- (4) run the counter — ran, reads constant 1.0 (CRIT-1-α).

Of the four, one is a measured no-op, one is unbuilt, one is a label, and one produces no live signal.
That is a thin basis for ADVANCED / 57% / "the only lock-internal-closeable family." 7C *is* the
survivor (the only route not capped by INC-5), and the counter IS specified and executes — those earn
it a real, non-trivial number. But 57% over-weights "runnable" against "discriminates," and the
"ships value now" framing is contradicted by 7C's own T2 finding. I place it at **50%**.

**CRIT-1-γ — the zero-runtime-cost invariant (Constitution v) for the CONTEXT tier is UN-VERIFIED.**
§3 lists "the CONTEXT-tier zero-cost invariant (`resolve_refs∘resolve_refs` no-op + `pretty()`
unchanged + TOML `push_path` residue=0) un-round-tripped" as an open gap. The tier lattice's entire
lawfulness rests on Tier-1 being "comptime, zero residual" — and that is asserted, not measured. Until
it is round-tripped, the claim that CONTEXT coverage "counts as grammar coverage" (and is thus *not*
subtracted from totality%) is an un-verified premise baked into the metric's denominator choice.

**Enumerated open gaps (7C):**
1. **[CRIT-1-α]** Flagship counter reads constant 1.0 on the entire real fleet (no `ExprKind::Host`);
   floor-gate cannot fail without synthetic injection — vacuous-on-real-input until a genuine Tier-2
   host is wired. UF-2 tested runnability (PASS) not discrimination (de-facto NO).
2. **[CRIT-1-β]** The real `ExprKind::Host` node + T4 `@{}` lowering UNBUILT; T2 was a shape-sniff.
3. **[CRIT-1-γ]** CONTEXT-tier zero-residual invariant (Constitution v) un-round-tripped.
4. Tier mechanism (the `&mut Ctx` structural read as the pluggable decision point) is specified but
   never exercised over a real host — the anti-gaming property is argued, not demonstrated.
5. CSS floor unmet until T4 (`@{}` lowering; `color.bbnf:220`/`values.bbnf:69`).
6. Floor ∈ {0,1,2} LOCK-1-pending (inherited-attribute verdict) — lawful external cap.

---

## §E — 7D · META-CIRCULAR-ORACLE — FAMILY says ADVANCED 51% "the POSTURE winner" → **CRIT-1: 50%**

**Internal soundness: the strongest MEASURED artifact in the lock — and I credit it.** `ORACLE_REL_V1
= R1∘R2∘R3` was **pinned and RUN AS CODE**; `norm` verified idempotent; **all four planted mutations
trip** (greedy→lazy, drop-group, bind-the-bool-twin=vacuous→rejected, alt mis-assoc). R2's same-stage
discipline (both post-`host::lower`) genuinely forecloses the fold-bug-masquerading-as-grammar-
divergence failure mode. The 12/12 divergence classification into two disjoint buckets (9× `host.rs`
bounded-`{n,m}` obligation → LOCK-5; 3× `regex.bbnf` verdict → oracle bank) is real diagnostic work.
This is the only family whose load-bearing act is a running experiment rather than a static read.

**But the measured result is a NEGATIVE, and the % should read that honestly.** The single (near-)
complete ledger row is `regex · ORACLE-ONLY · parity 0.7759 · perf 1.77× slower · fixpoint EMPTY ·
BLOCKED`. Regex fails promotion on two independent counts. The posture (oracle-first, ORACLE-ONLY as a
lawful terminal) is well-argued with the strongest field precedent (pegen ran old ∥ new a full
cycle) — that earns the "posture winner" title. But a posture validated by exactly one measured row,
and that row a negative, is a 50%-family, not a comfortable 51%. The delta from my number is small;
I flag the *reasons*, which are load-bearing for the gestalt:

- **0.80 gate provenance UNCITED** — self-flagged latent gate-tuning risk. Robust *here* only because
  all three parity metrics clear below it; but an uncited gate that the headline verdict turns on is a
  standing hazard the moment any metric lands near 0.80.
- **Corpus generality UNMEASURED** — parity is over the committed **51/58** hand corpus; the "grammar-
  driven corpus sampler" is unbuilt. The generality of 0.7759 is unknown; it could move either way on
  a sampled corpus. This is the sharpest 7D internal gap and it was not probed this pass.
- **`fixpoint` column EMPTY** — the ledger's one complete row is one column short; fillable via UF-3
  (runnable today without LOCK-5), but unrun.
- **bbnf rows RED / BLOCKED cross-repo** (INC-5) — inherited cap; UF-1 usefully tightened the *edge*
  (below) but did not lift the block.

**Enumerated open gaps (7D):**
1. Only measured ledger row is a NEGATIVE (regex ORACLE-ONLY on 2 counts); posture rests on one row.
2. 0.80 gate provenance uncited — latent gate-tuning risk.
3. Parity measured over the hand 51/58 corpus only; grammar-driven sampler unbuilt → generality
   unmeasured (the sharpest un-probed 7D uncertainty).
4. `fixpoint` column empty (UF-3, unrun).
5. bbnf Tier-R/S BLOCKED cross-repo; the 3 `regex.bbnf` verdicts LOCK-5-substrate-bound.

---

## §F — CROSS-CUTTING FAILURE-MODE SWEEP (the checklist, applied)

- **Vacuous convergence / gate-that-cannot-fail — PRESENT, 7C (CRIT-1-α).** The flagship totality%
  gate cannot trip on any real committed grammar. Disclosed, under-weighted in the %.
- **Elegant-reduction trap — PRESENT ×2, both self-flagged: 7A's SPEC_ONLY fixpoint gate; 7B's
  twice-un-run E0499.** The bbnf-leg's global "and-then-LOCK-5-ships-`match_at`" reduction is the
  largest, but it is **lawfully externalized** as a cross-repo born-RED DAG edge (INC-5/XL-INC5) with
  a grep-measured missing primitive, not hidden inside a family — the correct handling. Not a
  violation; a cap.
- **Spec-cites-itself circularity — CHECKED, clean.** 7A's fixpoint subject ≡ 7D's `ORACLE_REL_V1`
  is handled as *convergence* ("do not double-home"), not circular self-justification. Correct.
- **Legacy aliases / masked fallbacks — CHECKED, clean; one removed.** 7C-T2 *deletes* a dead
  name-gate and proved (via the inverted-predicate control) that no live behavior flowed through it —
  the opposite of masking a fallback. No new alias introduced anywhere.
- **Consumer-less substrate (Y.13/viii) — CORRECTLY POLICED.** 7B Plane-B flagged consumer-less
  unless LOCK-3 adopts 3C; 7C's counter has a named consumer (7D ledger `totality_pct`, XP-1).
- **Refutation laws (Constitution iv) — HONORED.** The `88s→0.5s` datum is caught as a REFUTED legacy
  LTO-transplant and quoting it against skinny is named a Constitution-ix breach. No forbidden-lever
  claim appears.
- **Zero-runtime-cost types (Constitution v) — ONE OPEN HOLE:** 7C's CONTEXT-tier zero-residual
  invariant is un-round-tripped (CRIT-1-γ).
- **Prototype-evidence misuse (SPEC_ONLY anchoring a perf claim) — NONE FOUND.** 7A makes no perf
  claim; 7D's 1.77× is measured; the only perf number that could be misused (88s→0.5s) is explicitly
  refuted. Clean.
- **Grammar totality (Constitution i) — HONEST NEGATIVE, not a violation.** LOCK-7 reports it cannot
  reach totality on the bbnf leg and says why (grep-measured missing `match_at`). Reporting a measured
  wall is the constitution working, not failing.

## §G — DID THE PROTOTYPES TEST THE SHARPEST UNCERTAINTY? (critic-1 charge)

- **PROTO-3 (leaf evaluator) — YES, sharpest possible.** Bounded-arm-swap vs missing-primitive is
  the whole bbnf-leg make-or-break; answered decisively (missing primitive, 0/7 leaves route,
  grep-verified empty substrate). Exemplary.
- **UF-1 / PROTO-2 (structural oracle) — YES, well-chosen.** "Is the block leaf-only or does it
  poison the branch skeleton?" is exactly the right next question; RAN, refuted research-B's `term`-
  collapse prediction (term's 8-branch Alt survives), isolated the sole divergence to one named
  `bool_value` materialization misfire. **Caveat for the gestalt:** the headline "branch skeleton
  SOUND (52/53)" is the **charitable-only** reading; the **strict** reading is **25/53** (< half),
  and the 27-rule strict/charitable delta is precisely the `?w` rules. The 52/53 leans entirely on
  the argued-invisible `Annotation(?w)→Seq([])` normalization plus a hand leaf-elision to make the
  oracle parse at all (raw closure fails at byte 2343). The result is genuine but its headline number
  is a chosen reading, not a both-readings-agree fact — carry both numbers forward.
- **PROTO-2 / 7C-T2 (dead name-gate) — a REAL but LESSER uncertainty.** It turned out near-vacuous
  (proved a no-op). Adequate rigor (baseline/negative/inverted-predicate controls), but it tested a
  bounded refutation, not 7C's construction.
- **UF-2 / PROTO-1 (totality counter) — tested a real uncertainty, NOT the sharpest one.** It proved
  the counter runs build-side (correcting the critics' cap — a genuine gain). But 7C's governing
  uncertainty is *"does the metric discriminate on real input?"* and the PASS **masks** the de-facto
  answer (no: constant 1.0). The prototype scored a PASS on the easier question while the harder
  question's negative sits in the METHOD text unbadged. This is the one place a prototype's headline
  verdict runs ahead of what it actually establishes (CRIT-1-α).

## §H — SUMMARY (CRIT-1 numbers vs FAMILY-SPECS)

| family | FAMILY-SPECS | CRIT-1 | why the dock |
|--------|-------------|--------|--------------|
| 7A STAGED-BOOTSTRAP | BANKED 40% | **38%** | survival test unmet+unrun; sole asset (fixpoint) SPEC_ONLY; inflation thesis is an in-repo analogy |
| 7B TOTAL-SELF-HOST | BANKED 32% | **30%** | 50/20/21 split static-read, un-run, sums to 91%; E0499 twice-un-run |
| 7C HOST-FN-LATTICE | ADVANCED 57% | **50%** | flagship counter constant 1.0 on real fleet (gate can't fail); trio = 1 no-op + 1 unbuilt + 1 label; zero-cost invariant un-round-tripped |
| 7D META-CIRC-ORACLE | ADVANCED 51% | **50%** | strongest artifact, but only measured row is a negative; corpus 51/58 generality unmeasured; gate provenance uncited; fixpoint col empty |

**Overall verdict.** The lock is factually the best-grounded of the campaign (every disk citation
re-verified true), and its self-flagging discipline is high — it names its own SPEC_ONLY and un-run
steps. My dockings are not "the facts are wrong" but "the percentages reward *runnability* and
*design clarity* over *discriminating signal*, exactly the trap §0 warns against." The two priority
next-pass acts are both cheap and both about turning a runnable-but-signal-less artifact into a
signal: **(i)** land a real `ExprKind::Host` and a single genuine Tier-2 host so 7C's counter can move
off 1.0 and its floor-gate can actually trip (converts CRIT-1-α/β); **(ii)** run **UF-3** so 7A's sole
survival warrant and 7D's empty `fixpoint` column are filled by a RAN number rather than a promise. A
distant-third: run the E0499 compile-fail (7B, one deletion) to convert the twice-un-run difficulty
claim. No family in this lock is at 100% and none should be — the bbnf leg's cross-repo cap (INC-5)
is correctly externalized and correctly caps every bbnf-leg family below convergence talk.

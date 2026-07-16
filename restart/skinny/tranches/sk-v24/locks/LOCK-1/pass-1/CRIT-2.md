# LOCK-1 — THE SOLVER SPINE · pass-1 · CRIT-2 (adversarial critic 2 of 2 — CROSS-LOCK view)

*Charge: harden/challenge/refine every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself circularity, gates that cannot fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate) + constitution
violations + prototype-evidence misuse — and, as critic 2, weight the CROSS-LOCK view: where does
this lock contradict or DOUBLE-HOME work against the extant GN/PT codex and the other locks?
Verified against HEAD, not taken on the reports' word. This run consumes the two probes that
postdate CRIT-1 / the registry: `PROTO-1B-clash-run2` and `PROTO-2-recursion-parity-probe2` (both
09:39), which neither the current CRIT-1 (05:07) nor REGISTRY (05:15) could have folded.*

---

## 0. WHAT I VERIFIED AT SOURCE (cross-lock deltas — three are net-new to the pass)

I re-grepped HEAD for every cross-lock coupling the four families lean on. Five verified facts;
three cut against the specs *and against the registry fold*, and are not in any pass-1 artifact.

### 0a. The staleness has RE-COMPOUNDED: the registry itself is now stale
CRIT-1 §0 correctly caught that FAMILY-SPECS (04:41) was sealed before PROTO-1B-clash (04:48) and
PROTO-2-recursion-parity (05:01). But **two independent re-runs landed at 09:39** —
`PROTO-1B-clash-run2` and `PROTO-2-recursion-parity-probe2` — *after* CRIT-1/CRIT-2 (05:07) and
*after* REGISTRY (05:15). So the current registry fold ("RETIRE governs on the recursion axis") is
anchored on the **superseded** `PROTO-2-recursion-parity` (v1) and has **not** consumed `probe2`,
whose decisive new measurement **shifts the 1D verdict from clean-RETIRE to CONTESTED-lean-KEEP**
(§0d, §5). **This is unverified-gestalt at the ledger level: the adjudicating document is one
probe-generation behind its own evidence, twice over.**

### 0b. [NET-NEW] 1D double-homes against a LIVE `TypeAnalysis` in the LOCK-3 backend driver
1D's FAMILY-SPECS integration-seams paragraph proposes "`TypeENode`+`type_rules`+`TypeAnalysis`
net-new beside the engine." **`TypeAnalysis` already exists on root** —
`crates/core/src/backend/driver/analysis.rs:16` `pub struct TypeAnalysis`, constructed by
`TypeAnalysis::from_ir(ir)` (`:221`), re-exported at `backend/mod.rs:22`. This is **the live
consumer of the type system inside the backend driver** — i.e. exactly the LOCK-3 seam
(IR → backend shapes) the whole type plane feeds. Two consequences, both unhandled:
(i) 1D's proposed net-new `TypeAnalysis` is a **name collision** with a shipping struct (parsimony
/ name-gate hazard, Constitution vi); (ii) **no family names `TypeAnalysis::from_ir` as the consumer
it feeds.** §0.1's "1B/1D are not consumer-less substrate" is *true* but the specs found the wrong
consumer — they cite `backend/rust/ir_types.rs` + `backend/ts/projection.rs` and miss the
**driver-level** `TypeAnalysis` that owns `from_ir`. The actual LOCK-1→LOCK-3 handoff is blind in
all four specs.

### 0c. [NET-NEW] 1B's "one constraint store" contradicts the live base — root ALREADY federates TWO `Csp`s
1B's thesis: "every judgment (equality, width, capacity, decode class, cost) is ONE constraint
system." On the surviving base this is **already false, by construction**: root runs
`crates/ir/src/passes/types/` (the type-projection `Csp<TypeDomain>`) **and**
`crates/ir/src/passes/csp_strategy/{constraints/{dispatch,layout,shape,engine},mod}.rs` (LOCK-2's
backend-shape `Csp`) — **two separate stores over the same vendored `csp-solver`, deliberately
federated**. The literal single-store form would have to **merge LOCK-1's type CSP with LOCK-2's
strategy CSP** — a cross-lock consolidation 1B never proposes and cannot make alone. The registry's
"federation IS 1A wearing a CSP coat" *understates the collision*: the federation is not
intra-LOCK-1, it is **LOCK-1 ⊕ LOCK-2**, and unifying it is a GESTALT-B call. 1B's single-store
thesis is not merely BLOCKED-candidate on R-B1 (per-revise tag dispatch) — it is **cross-lock
double-homing** the type plane and the decision driver.

### 0d. [NET-NEW] probe2's decisive measurement: the `List ≠ Row` collapse is TOTAL and recovered by a MASKED FALLBACK
`PROTO-2-recursion-parity-probe2` adds the measurement the v1 probe (and the registry fold) lacked:
the `array ≡ object` collapse is present **not only in `ir.types` but in the `struct_registry`
sidecar codegen actually consumes** — `array` and `object` have **byte-identical** `StructLayout`
entries, and `LayoutKind` has **no Row/Map variant** at all. The distinction is absent from the
**entire general type pipeline**. It is recovered downstream **only by a bespoke per-grammar
hand-written codegen template** (`skinny/crates/codegen/src/json_templates/value.rs`) dispatched by
a **hard-coded per-grammar table** (`json→regen-json`, `math→regen-math`). Verified concern: **the
registry's "RETIRE 1D as unused capacity" verdict RESTS on treating that per-grammar template as
legitimate — i.e. it rests on a masked fallback**, which `no-workarounds` + `typed-materialization-
invariant` + Constitution i (grammar totality) all count *against*. probe2's own verdict flips to
**CONTESTED-lean-KEEP** on exactly this ground. (I challenge the lean itself in §5 — the gap is real
but engine-agnostic — but the registry cannot cite v1-parity's RETIRE as settled once probe2 exists.)

### 0e. [confirmed] The `union` amendment is a THREE-lock blast radius, verified concretely
`crates/egraph/src/egraph.rs:~236` at HEAD still carries the `let changed = A::merge(…); if changed
{ analysis_pending.push(dst) }` guard — **1D's `union()`-scheduling amendment is reverted, off-tree**
(as CRIT-1 §0.3 / registry X5 state). I add the concrete blast radius: root's e-graph is shared by
**LOCK-2's grammar tier** (`crates/ir/src/egraph/{node,build_egraph,cost,rules}.rs`) **and LOCK-5's
regex tier** — `crates/ir/src/egraph/rules/regex.rs` **exists and rewrites on the same engine.** So
the reverted amendment (schedule `modify` on every merge) would fire on **every regex-tier and
grammar-tier saturation union**, not only type merges. 1D's disposition therefore requires a
saturation perf measurement across **three** locks (1, 2, 5), and the "free for no-op analyses" claim
is un-measured on the two hottest consumers. Registry X5 called this "double-homed"; the regex-rules
file makes it triple.

*(Also confirmed, unchanged from CRIT-1/registry: `TypeDomain::join` has zero call sites in the type
pass — the "confirmed shipping defect" is dead code; the live carrier is `assign` (LWW, unreachable
for clashes); the reachable clash site (Alt-join `join_types`) raises `HeterogeneousAltJoin` + a
named obligation — sound. run2 adds a non-commutativity proof of the dead `join` and independently
reproduces the live-path soundness. I accept all of this and do not re-litigate it; my contribution
is the cross-lock layer above.)*

---

## 1. THE CROSS-LOCK CROSS-CUTTING FINDING — the type-of-record is NOT a free LOCK-1 choice

The incompatibility map frames the fork as "one exclusive equality-engine axis, choose one," with
the three carriers defining equality over three different *types of record* (`TypeDesc` vs fused
monotype vs `TypeENode`). **That framing is LOCK-1-parochial.** The type-of-record is constrained
from FOUR directions the specs treat as internal:

1. **LOCK-3 (IR transposition)** owns the downstream consumer `TypeAnalysis::from_ir` (§0b) and the
   `struct_registry`/`LayoutKind`/`DirectTypeKind` lowering (§0d). Whatever record LOCK-1 picks must
   lower through `LayoutKind` — which today **cannot express `List ≠ Row`** (probe2). That is a GN-3
   wall-(b) IR-expressiveness gap (`DirectTypeKind` has one `Struct`; add `Sum`/`Row`), **LOCK-3's to
   widen**, engine-agnostically. No engine choice closes it; a `LayoutKind::Row` variant does. **The
   "1D uniquely derives `List ≠ Row`" claim is an elegant-reduction trap** (§5).
2. **LOCK-6 (isomorphism plane)** decides what crosses the rust/ts boundary. Only `TypeDesc` has a
   *shipping* TS consumer (`backend/ts/projection.rs`); a fused monotype (1A) and `TypeENode` (1D)
   have **no demonstrated TS mirror**. Constitution iii + LOCK-6 make the record a *joint* choice;
   LOCK-1 cannot pick `TypeENode` and defer "how TS consumes e-class extraction" to later.
3. **LOCK-2 (graph engine)** owns the e-graph engine 1D would amend (§0e) and the `csp_strategy` store
   1B would have to merge with (§0c). Both 1B and 1D reach *into* LOCK-2's substrate; only 1A is
   engine-local.
4. **LOCK-7 (grammar totality)** owns the GN-3 codex whose §1.5 the specs void inline (§6, ix), and
   owns the grammar-authoritative-vs-codegen-distributed architecture call the 1D keep-hook turns on
   (§0d). That call is a LOCK-7/GESTALT-B adjudication, **not** a LOCK-1 recursion race.

**Net:** LOCK-1 does not own its own decisive variable. The carrier fork (1A vs 1B) and the 1D
keep-hook are both **partly cross-lock**, and every family spec treats them as internal. This is the
lock's largest blind spot, invisible from the within-lock (CRIT-1) vantage.

---

## 2. FAMILY 1A — UNIFICATION-FIRST · convergence 56%

Prior-art-endorsed architecture (HM(X)/OutsideIn(X), research-priorart §2A/§5), proven seed
(PROVE-08), post-probe indirectly evidenced by the *incumbent's* nominal-fold termination
(PROTO-2-parity/probe2: the running pass closes recursion nominally and terminates on JSON, Sheets,
CSS L4). Architecturally soundest. But its win is a **carrier displacement on the surviving base**,
and the cross-lock consumer it must feed is unnamed.

Enumerated open gaps:
1. **Carrier-displacement justification stays collapsed (CRIT-1 §0.2, confirmed).** The "running CSP
   is unsound as shipped" lever is dead code; 1A-as-carrier now rests on prior-art familiarity +
   "raises clashes" (which the running Alt-join also does). Fork open, tilt unearned.
2. **[cross-lock] 1A does not name its LOCK-3 consumer.** It threads `->` into `DirectTypeKind::Sum` /
   `DirectScalar` but never names `backend/driver/analysis.rs::TypeAnalysis::from_ir` (§0b) as the
   sink, nor `backend/ts/projection.rs` (LOCK-6). A fused monotype must lower to *both*; the spec
   asserts the emit half is "genre-standard" (priorart §2H) but never shows the fused monotype
   reaching the driver `TypeAnalysis` the incumbent `TypeDesc` already feeds.
3. **Lineage inverts the cost (registry G-1A-1).** Scaffold is on **retiring** skinny; "activate
   `TypeFacts`" becomes "port PROVE-08 into root + displace the running CSP" — the full 1A⊥1B collision
   at displacement cost. Unmeasured.
4. **Zero direct 1A prototype — every gate (G1–G6) asserted.** Recursion termination rides the
   *incumbent's* proof, not a union-find re-seed's.
5. **Row discipline (O1) unresolved + unmeasurable** — zero `->` corpus; sequences behind GN-3.W3.
6. **Numeric-width residue (O2) UNMEASURED** — census maps width→arity, numeric→decode, both pre-tagged
   single-valued; the fork's true residue axis is unrun.

## 3. FAMILY 1B — ONE-CONSTRAINT-STORE · convergence 54%

Best-evidenced (runs on the surviving base; run2 re-confirms the live clash path sound). The specs'
"unsound as shipped" indictment is refuted. But 1B's *distinguishing* thesis is defeated three ways,
one of them cross-lock and net-new.

Enumerated open gaps:
1. **[cross-lock, NET-NEW] the single-store thesis contradicts the live base (§0c).** Root already
   federates the type CSP and the `csp_strategy` CSP; "one store" means merging LOCK-1's store with
   LOCK-2's — a GESTALT-B consolidation, not a LOCK-1 build. The literal thesis is **cross-lock
   double-homing**, strictly worse than the registry's "federation IS 1A" framing (which located the
   collapse inside LOCK-1). BLOCKED-candidate stands, and the wall is bigger than R-B1.
2. **"Collapse to 1A" retirement is UNSOUND as argued (X3/X4).** Pre-registered census gates FAIL on
   all four corpora; "THIN — decisive" is reached by a **post-hoc re-cut** of the gate; the census
   measures 1B's own pass, which has **zero overload/legality code** (grep-verified) — an instrument
   structurally blind to the rich residue it purports to refute. Spec-cites-itself circularity (the
   richness read is 1B's own `HeterogeneousAltJoin` output) compounds it.
3. **The carrier inversion is live (CRIT-1 §3.4).** Cheapest sound spine may be *the running CSP
   hardened* (keeps both backend consumers + sound live path + additive `assign` hardening) vs a
   union-find re-seed that deletes running code. Unadjudicated.
4. **R-B1 store-shape UNBUILT** — the decode-class-depends-on-sum-variant counterexample built neither
   way; the family's sharpest decision is on paper.
5. **`assign` hardening not shown backward-safe** — refusing the overwrite touches code asserted by
   `types_heterogeneous_alt`; no probe confirmed the corpus still solves.

## 4. FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence 40%

FOLD is correct (checking front + SCC seam + span field over the winner; reuses PROVE-08's `Unifier`
→ closest kin to 1A). Structurally untestable on the current corpus.

Enumerated open gaps:
1. **Make-or-break head-to-head (R-C2/G2) STRUCTURALLY UNRUNNABLE — zero `->` corpus.** Not merely
   un-run; there is no input. 1C's distinct identity is unfalsifiable until GN-3.W3 authors typed
   grammars. The disposition is a bet.
2. **On zero-`->` grammars 1C ≡ its host engine** (spec-admitted degeneracy); addressable surface
   near-empty.
3. **[cross-lock] substrate on the RETIRING tree.** Every seam (`synth`/`check` split, `RuleSig`, the
   `Project` mode-switch, `PassDiagnostic` span-add) lives in **skinny**; on root the diagnostics type
   differs. The `PassDiagnostic.span: Option<SourceSpan>` add is owed against the **root** base, not
   the skinny one the spec cites.
4. **Sub-span granularity ceiling (R-C3).** `Project.ty` is one `String` with whole-expr span only —
   the quality ceiling on 1C's entire thesis is a sub-span the IR (a LOCK-3 field) does not carry.

## 5. FAMILY 1D — TYPES-AS-ECLASSES · convergence 44%

**The most contested disposition, and it MOVED this pass — but not the way the registry recorded.**
The registry folds "RETIRE governs (v1-parity), structural-μ hook architecture-gated." `probe2`
(fresher, un-consumed) escalates the structural-μ concern to a **live typed-materialization-invariant
violation recovered by a masked fallback** (§0d) and leans **KEEP**. As critic 2 I hold BOTH against
each other and find the honest position is neither the registry's clean RETIRE nor probe2's lean-KEEP:

**The structural-μ gap is REAL but ENGINE-AGNOSTIC — probe2 commits the elegant-reduction trap.**
probe2 argues "1D would *derive* `List ≠ Row` from the grammar (its `List(#0) ≠ Row(#0)`), which is
the capability the running pipeline lacks." Read closely: the running pipeline lacks it because
**`LayoutKind` has no Row variant and `DirectTypeKind` has one `Struct`** (§0d) — a GN-3 wall-(b)
IR-expressiveness gap owned by **LOCK-3**, closable by adding a `Row`/`Sum` variant that **any** of
the three engines can populate. "1D derives it" reduces to "1D + a widened `LayoutKind` derives it" —
and-then-the-hard-part (the record can *hold* `List ≠ Row`) is the actual missing primitive, and it
is **not** an e-graph capability. So probe2's KEEP conflates a fixable engine-agnostic IR gap with a
unique 1D capability. **1D's sole surviving distinctive advantage is thereby cosmetic** relative to
"widen `LayoutKind` + let the running CSP populate it." The masked-fallback finding (§0d) is a **real
Constitution-i / no-workarounds defect that must be fixed** — but it indicts the *codegen-distributed
architecture*, not in favor of an *e-graph engine*; the fix is a LOCK-3 record widening, adjudicated
under LOCK-7's grammar-authoritative call.

Enumerated open gaps:
1. **[cross-lock] "zero new engine" FALSIFIED; the amendment is a THREE-lock blast radius (§0e).**
   Reverted at HEAD; would fire on LOCK-2 grammar-tier + LOCK-5 regex-tier (`egraph/rules/regex.rs`)
   unions; "free for no-op analyses" un-measured on the two hottest consumers. Cannot be settled in
   LOCK-1.
2. **[cross-lock] name-collision + no TS path.** Net-new `TypeAnalysis` collides with the live LOCK-3
   driver struct (§0b, Constitution vi); e-graph-extracted types have no LOCK-6/TS mirror (Constitution
   iii). 1D ranks last on isomorphism.
3. **AC-vs-injective coexistence UNBUILT (elegant-reduction trap).** Positional injectivity is unsound
   for AC `Sum`; the probe stayed sound by running the two mechanisms in **separate e-graphs** — tested
   *around*, never *through*. Confluence asserted.
4. **The RETIRE itself is architecture-contingent (not clean).** "Unused capacity" presupposes the
   codegen-distributed-nominal architecture and the masked fallback that recovers it (§0d); neither
   probe cleanly decides 1D until the grammar-authoritative call is made — a LOCK-7/GESTALT-B call.
5. **PROTO-2 (a)–(e) proof rests on OFF-TREE code** (the reverted `union` amendment) — gates passing on
   code not in the tree = SPEC_ONLY masquerading as MEASURED (Constitution vii). Determinism gate held
   only on a 10-class toy; 191-rule CSS byte-identity unexercised.

---

## 6. CONSTITUTION & PROTOTYPE-EVIDENCE AUDIT (cross-lock weighting)

- **i (grammar totality) — VIOLATION SURFACED, owned jointly with LOCK-7.** probe2's masked fallback
  (§0d): `List ≠ Row` recovered by a hand-written per-grammar codegen template dispatched by a
  hard-coded `json→regen-json` table — semantics hand-coded per grammar, not grammar-driven. A real
  clause-i defect the registry's RETIRE quietly rests on; must be carried to LOCK-7 as a named
  totality gap, not absorbed as "1D unused capacity."
- **iii (rust/ts isomorphism) — 1D unhandled.** e-graph-extracted types have no TS consumer; only
  `TypeDesc` feeds `backend/ts/projection.rs`. LOCK-6 constrains the record; the specs treat it as
  internal (§1.2).
- **v (zero runtime) — asserted by all four, exercised by none.** Named pass-2 gap, acceptable at
  pass-1. No family drove `git diff crates/runtime/` empty.
- **vi (parsimony / no name-gates) — 1D `TypeAnalysis` name-collision (§0b).**
- **ix (extant-tranche consciousness) — PROCEDURAL VIOLATION.** FAMILY-SPECS §0 voids GN-3 §1.5 ("no
  CSP or e-graph consumer exists") inline as "factually superseded." GN-3 is committed codex; clause ix
  requires a **named supersession** carried to GESTALT-B with LOCK-7 sign-off, not an inline "factually
  void." Registry X2 flags it; I weight it cross-lock — the supersession is LOCK-7-owned.
- **viii (consumer-less substrate) — retired correctly (§0.1), but the specs found the WRONG consumer**
  (§0b): they cite the backend `ir_types.rs`/`projection.rs` and miss the driver-level
  `TypeAnalysis::from_ir` that owns the handoff. Consumer-blindness, the inverse hazard.
- **Prototype-evidence misuse — the registry cites the SUPERSEDED probe.** Three probes now bear on
  1D: PROTO-2 (v1 strawman → KEEP), PROTO-2-recursion-parity (v1 incumbent → RETIRE),
  PROTO-2-recursion-parity-**probe2** (fresh incumbent+sidecar → CONTESTED-lean-KEEP). The registry's
  "RETIRE governs" fold cites v1-parity and **never consumed probe2** (§0a). The freshest measurement
  reaches a different lean; the ledger is one generation stale.
- **Gates that cannot fail — PROTO-1 RICH gate (numeric axis blind) + PROTO-2 gates on off-tree code**
  (§5.5). Both stand from CRIT-1; I add that probe2's own PASS/FAIL gate is *definitional* ("load-
  bearing" = observable defect vs = derivation obligation) — a gate whose outcome is chosen by defining
  a word, which is why probe2 flags owner adjudication rather than asserting a result.

---

## 7. PASS-2 CROSS-LOCK DIRECTIVES (what must be built or adjudicated across locks)

1. **Re-fold the registry to consume `run2` + `probe2`** (§0a). Minimum: record the 1D verdict as
   CONTESTED-with-a-masked-fallback (§0d), not clean-RETIRE.
2. **[GESTALT-B/LOCK-3] Name the real consumer and widen the record engine-agnostically.** Add
   `LayoutKind::Row` / `DirectTypeKind::{Sum,Row}` (GN-3 wall-b) and route the type plane through
   `backend/driver/analysis.rs::TypeAnalysis::from_ir` (§0b). This closes the `List ≠ Row` gap
   **without** an e-graph and dissolves 1D's sole surviving advantage into an IR-expressiveness fix.
3. **[LOCK-7/GESTALT-B] Adjudicate grammar-authoritative vs codegen-distributed** — the real 1D
   keep-or-retire pivot, and the home of the clause-i masked-fallback defect (§0d, §6). Not a
   recursion race.
4. **[GESTALT-B/LOCK-2] If 1B's single-store is pursued, cost the LOCK-1⊕LOCK-2 store merge** (§0c) —
   type CSP + `csp_strategy` CSP into one store — as an explicit cross-lock consolidation, or bank the
   literal thesis and keep the federation (which is 1A's stratification).
5. **[GESTALT-B/LOCK-2/LOCK-5] Route the `union` amendment through a three-lock saturation
   measurement** (§0e) — grammar tier + regex tier (`egraph/rules/regex.rs`) + type tier. The "free
   for no-op analyses" claim is un-measured on the two hottest consumers; land or retire the amendment
   in-tree.
6. **[LOCK-6] Fix the TS-consumption path for whichever record wins** — only `TypeDesc` has a shipping
   TS consumer; a fused monotype (1A) or `TypeENode` (1D) must show its `backend/ts/projection.rs`
   mirror before the record is chosen (Constitution iii).
7. **[within-lock, additive] Land the `assign` raise-on-disagreement hardening** (record `TypeClash`,
   refuse overwrite) and confirm the corpus still solves — ships regardless of carrier winner.
8. **Author one typed `->` grammar (GN-3.W3)** so 1C's R-C2 and 1A's row discipline become falsifiable
   at all. Until then 1C's 40% and 1A's row clause are ceilings.

---

## 8. CONVERGENCE SUMMARY

| Family | Disposition (spec) | CRIT-2 | Dominant cross-lock open gap |
|---|---|---:|---|
| **1A** UNIFICATION-FIRST | ADVANCE (spine) | **56%** | endorsed architecture, but carrier justification is dead code; **unnamed LOCK-3 consumer** (`TypeAnalysis::from_ir`) + unshown TS lowering; port+displace onto retiring→root; zero direct prototype |
| **1B** ONE-CONSTRAINT-STORE | ADVANCE fed. / BANK thesis | **54%** | live path sound (refutes "unsound"), but **single-store = LOCK-1⊕LOCK-2 store merge** (root already federates two `Csp`s); census unsound (X3/X4); carrier inversion live |
| **1C** BIDIRECTIONAL-LOCAL | FOLD | **40%** | make-or-break diagnostic head-to-head structurally unrunnable (zero `->`); substrate on the **retiring** tree; span add owed against **root** |
| **1D** TYPES-AS-ECLASSES | BANK (recursion keep) | **44%** | recursion-keep retired vs incumbent, BUT probe2 escalates `List ≠ Row` to a **masked-fallback / clause-i violation**; yet the gap is **engine-agnostic** (`LayoutKind::Row` fix, not unique to 1D); `union` amendment a **3-lock** blast radius; `TypeAnalysis` name-collision; no TS path |

**Lock-wide (cross-lock).** The pass's central conclusion — HM(X) trunk + bounded defaulting residue
+ 1C folded in — is prior-art-consonant and probably right. But the two forks it leaves open are
**both cross-lock and both mis-sized by the specs**: (a) the carrier seat (1A reseed vs 1B harden) is
adjudicated as intra-LOCK-1 when 1B's literal thesis is a **LOCK-1⊕LOCK-2 store merge** and both
carriers must feed a **LOCK-3 driver consumer no spec names** and a **LOCK-6 TS mirror only `TypeDesc`
has**; (b) the 1D question is adjudicated as a recursion race when probe2 shows it is really a **LOCK-3
record-expressiveness gap** (`LayoutKind` lacks Row) recovered by a **LOCK-7/clause-i masked fallback**
— a gap any engine closes once the record is widened, so 1D's engine buys nothing the widening does
not. **No family converges; none is cleanly retired.** The type-of-record is not LOCK-1's to choose
alone, and every family spec pretends it is.

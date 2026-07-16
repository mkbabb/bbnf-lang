# LOCK-1 — THE SOLVER SPINE (type inference & solving) · REGISTRY

*Living adjudication ledger across the ≥3-pass convergence loop. The AGGLOMERATOR folds each
pass (research + FAMILY-SPECS + PROTOs + CRIT-1 + CRIT-2) into this file. Convergence % is the
**critics' floor** unless the agglomerator can name why a critic erred; no family approaches 100%
until it has zero open gaps + a fresh audit + two consecutive clean passes. Status is one of
ADVANCED / BANKED / BLOCKED / RETIRED.*

Charter: type inference & solving for the bbnf grammar plane — `->` projections, document shapes,
sums/products, capacities, numeric widths, decode classes — inferred, solved, erased to zero
runtime cost, with grammar-sourced diagnostics. GN-3's three walls (information / IR-expressiveness
/ front-end `->` thread) are the floor. Standing laws: `no-silent-epsilon`, `no-value-discard`,
zero-runtime-cost (`git diff crates/runtime/` empty), consumer-less-substrate forbidden (Y.13).

---

## VERIFIED BASELINE (re-anchored by the agglomerator against live this pass)

The lock references **two live, git-tracked, separately-buildable cargo workspaces** — re-verified
this pass, both real:

- **`skinny/`** (`skinny/Cargo.toml` members `crates/ir, passes, codegen, …`) — carries the **1A/1C
  substrate**: the dormant HM(X) scaffold. Confirmed verbatim: `skinny/crates/passes/src/lib.rs:204`
  is `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (the `->` discard); `TypeFacts`
  with `subst`/`obligations` populated by nothing. A **true description of code that exists**.
- **root `/Cargo.toml`** (members `crates/core, ir, egraph, csp-solver, …`; the active `cargo xtask
  regen` tree) — carries the **live 1B/1D substrate**: the running lattice-CSP type inference
  (`crates/ir/src/passes/types/constraint/*`, `TypeDomain: LatticeDomain`, `HeterogeneousAltJoin`
  live at `crates/ir/src/types/type_desc.rs:63,209`, both backend consumers) and the shipped
  `crates/egraph`. Confirmed live.

**THE LINEAGE DECISION IS RESOLVED — and it inverts the fork's cost accounting (agglomerator
correction to CRIT-2 X1).** CRIT-2 scored 1A/1C down partly for an *undecided, conflicting* lineage
("FAMILY-SPECS says skinny retired; SK-V22 says skinny graduates — the authorities conflict"). That
sub-claim is **stale**: project memory records SK-V22's graduation premise as **superseded by
skv24-handoff — "skinny retires."** So the direction is decided: **root is the greenfield base;
skinny retires.** This does **not** rescue 1A/1C — it *confirms and sharpens* CRIT-2's cost-inversion:

- **1B/1D substrate survives on the base** (root, the active regen tree). "Already runs, don't
  rebuild" is a **real win**, not an argument from an archiving tree.
- **1A/1C substrate is on the RETIRING tree** (skinny). Their headline mechanism — "activate the
  dormant `TypeFacts` scaffold = mechanism + Y.13 repair" — becomes **"port PROVE-08 + the scaffold
  into root, and reconcile with / displace the running 1B CSP"** — the full HARD-INCOMPATIBLE 1A⊥1B
  collision at displacement cost, not a free scaffold flip. 1A/1C's apparent cheapness does not
  survive the lineage resolution.

Anchors re-verified verbatim this pass (agglomerator re-ran the greps; **the critics did not err on
any load-bearing fact**):

| Fact | Live truth | Bearing |
|---|---|---|
| `->` ty-drop (1A/1C target) | `skinny/…/passes/src/lib.rs:204` `Project { body, .. } => infer_expr(*body)?` — verbatim | 1A/1C mechanism site, on the **retiring** tree |
| 1B running type system | `HeterogeneousAltJoin` live `type_desc.rs:63,209`; `TypeDomain: LatticeDomain`; both backends consume | 1B ships in root — not speculative substrate |
| **R-B2 defect** | `constraint/domain.rs:72-75` `(Some(existing), Some(ty)) if existing != ty => { self.solved = Some(ty.clone()); }` — comment *"may refine types during convergence"* | last-writer-wins on a genuine clash; silent-overwrite where `no-silent-epsilon` demands an obligation — **CONFIRMED live** |
| **1D "zero new engine" FALSIFIED** | `egraph.rs:236-238` still `let changed = A::merge(…); if changed { analysis_pending.push(dst) }`; `rebuild_pass_count` **absent from tree** | PROTO-2's `union()`-scheduling amendment (+30/−4) is **reverted, not in tree**; on the shipped engine injectivity never fires on a structural `Sum`/`Arrow`/`List` merge |
| **X4 overload-absence** | `crates/ir/src/passes/types/` has **zero** `overload`/`escape`/`legality` code | the census measured an engine that **structurally cannot** exhibit the overload/decode residue the rich thesis rests on |
| Both probes non-reproducible | `crates/core/tests/lock1_residue_census.rs` **absent**; `crates/egraph/tests/type_injectivity.rs` **absent** | PROTO-1 & PROTO-2 survive only as `.patch.diff`; a pass-2 reproducibility obligation for both |

**Consequence.** The lock is a genuine **engine-of-record fork on the surviving base (root)**: adopt
the *proven-elsewhere* union-find HM core (1A, PROVE-08 + skinny scaffold — but now a **port +
displacement** into root) versus complete the *already-running-here* lattice-CSP inference (1B) —
with 1D's e-graph a **third engine that modifies the shared substrate** and 1C a **discipline that
layers onto the winner**. The honest adjudicator is **residue richness** (prior-art §2G/§4.3, R-B4) —
and **that axis is NOT cleanly closed**: the one census (PROTO-1) measured document-shape residue
from ground truth (genuinely thin, ≤2%) but pre-tagged width/capacity/decode single-valued and ran
on an instrument with **no overload code at all** (X4). The "collapse to 1A" verdict is **framed for
document shapes, unearned for the numeric/overload axis.**

---

## CURRENT DISPOSITIONS (as of pass-1)

| Family | Status | Convergence | One-line |
|---|---|---|---|
| **1A · UNIFICATION-FIRST** | **ADVANCED** (the prior-art-endorsed trunk; unproven in tree) | **52%** | The endorsed HM(X) spine with a real dormant scaffold — but **zero prototype**, the one recursion probe favored its rival 1D, and the lineage resolution turns "activate scaffold" into "port + displace the running root CSP." |
| **1B · ONE-CONSTRAINT-STORE** | **ADVANCED** (the running type-system-of-record; one confirmed defect) | **58%** | The **only family that runs on the surviving base**; carries the pass's one verified bug (R-B2); its retirement-to-1A rests on a gate-manipulated, wrong-engine census (X3/X4) — **unsound as argued**; store-shape (R-B1) unbuilt. |
| **1C · BIDIRECTIONAL-LOCAL** | **BANKED** (subsumed as 1A's diagnostics-and-locality discipline; standalone-fourth-engine formulation banked) | **38%** | Not a fourth engine — a discipline that layers onto the winner (naturally 1A). Its make-or-break diagnostic head-to-head (R-C2) was **never run**; raison d'être **entirely unmeasured**; retired-lineage substrate. |
| **1D · TYPES-AS-ECLASSES** | **BANKED** (engine subordinates under LOCK-2; "zero new engine" **RETIRED** as a named negative) | **48%** | The only ran-and-measured family; the **recursion win is genuine**. But "zero new engine" is **falsified** (reverted core-engine amendment), the cross-lock cost is unmeasured, and the distinctive cost-extraction engine is **idle** where the census found the mass (overload residue = 0, verified). |

**Floor rule applied.** Floors are the min across the two critics, taken **unadjusted**:
1A floor(70,52)=52 · 1B floor(68,58)=58 · 1C floor(55,38)=38 · 1D floor(60,48)=48. The agglomerator
re-verified every crux fact both critics rest on (R-B2 arm, the `egraph` `if changed` guard +
`rebuild_pass_count` absence, X4 overload-absence, both probe absences, the skinny `:204` discard,
the live root `HeterogeneousAltJoin`) — **the critics did not err on any load-bearing fact.** The one
correction (CRIT-2's stale "authorities conflict" on X1) **resolves the lineage against 1A/1C**, so
it moves **no floor upward**. Floors stand.

---

## FAMILY 1A — UNIFICATION-FIRST · ADVANCED · 52%

**What is genuine (verified).** The HM(X)/OutsideIn(X) stratification is the field's settled answer
(prior-art §2A/§5) — *generate equality by unification, defer the theory residue to a bounded solver*
— and it is the exact shape prior-art §5 and GN-3 endorse. The union-find leaf core is proven
(PROVE-08 CONFIRMED_DELTA); the dormant `TypeFacts { subst, obligations }` scaffold really exists in
`skinny/crates/passes` and the `:204` discard is verbatim, so "activation = mechanism + Y.13 repair"
is a **sound description of real code**. On paper it is the endorsed trunk.

**Open gaps.**
- **G-1A-1 [lineage resolution inverts the cost — the scaffold is on the RETIRING tree].** With
  skinny retiring (SK-V24) and root the base, 1A's "activate the dormant scaffold" becomes "port
  PROVE-08 + `TypeFacts` into root and **reconcile with / displace the running 1B CSP**" — the full
  1A⊥1B HARD-INCOMPATIBLE collision at displacement cost. 1A's apparent cheapness does not survive.
  **The fork's true stakes on the surviving base.**
- **G-1A-2 [no prototype for 1A's OWN recursion story — the recursion probe tested a rival].** Gate
  (i) — recursive `value` infers one 6-variant `Sum` via the nominal `Rule(id)` fold, terminating —
  is the make-or-break both 1A and 1D name first. **PROTO-2 proved it for 1D's e-graph cycle and
  explicitly claims native cyclicity *beats* 1A's nominal fold; 1A's fold guard was never built.**
  The only recursion evidence in the pass argues *against* 1A. SPEC_ONLY (Constitution vii).
- **G-1A-3 [row discipline unpicked].** Leijen scoped-labels vs Gaster–Jones lacks-predicates — still
  "Unresolved." "records, informally" re-derives Wand's principal-type bug (prior-art §2C trap 2).
  Load-bearing for the document-shape half.
- **G-1A-4 [numeric-width residue UNMEASURED, not merely unresolved].** 1A §1.7 defers width to "a
  CSP or an ordered defaulting lattice, which Brief 1 may show suffices." PROTO-1 did **not** measure
  numeric width (its `width` tag is Seq/tuple-arity; numeric goes to `decode-class`, pre-tagged
  single-valued). 1A's own central sub-question is **open on the evidence** — contra the synthesis's
  implication that the census fixed it.
- **G-1A-5 [generalization criterion "named, not stated"].** The value-restriction criterion
  (prior-art trap 4) is unwritten; low-risk (rules are pure schema producers) but a real open item.
- **G-1A-6 [two-systems merge (R6) + zero-runtime asserted, not exercised].** No pass-1 artifact
  demonstrated deleting `codegen::type_plane::hm::Ty`, `git diff crates/runtime/` empty, or derive
  wall-time within GN-3's ~230µs.

**Pass-2 directives.**
1. **Build the 1A nominal-fold recursion prototype** and **race it against PROTO-2's cyclic
   extraction** — make the recursion axis an actual head-to-head, not a one-sided assertion. This is
   the single experiment that either earns or retires 1A's distinct recursion story.
2. **Cost the lineage port explicitly:** specify what "port PROVE-08 into root + reconcile with the
   running 1B CSP" concretely costs, since the scaffold now lives on the retiring tree (G-1A-1).
3. **Build the numeric-width probe the census skipped** — a corpus field genuinely under-determined
   between u8/u16/u32, measured for whether the choice is *defaulting* (ordered lattice, prior-art
   §2G) or *search*. This is 1A §1.7's open question and the fork's true residue axis.
4. Pick and state the row discipline (Leijen vs Gaster–Jones); write the generalization criterion.

**Withheld from researchers:** do **not** re-derive the HM(X) stratification (settled, endorsed); do
**not** re-argue that union-find dominates equality asymptotically (settled, prior-art §2B). Build the
recursion race and the width probe — argue nothing more on paper.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · ADVANCED · 58%

**What is genuine (verified at source).** 1B is the **only family that runs on the surviving base** —
`TypeDomain: LatticeDomain`, per-former propagators, `ObligationSink`, `HeterogeneousAltJoin`, both
backend consumers, end-to-end tests. That is not speculative substrate; it ships. And the pass's
**one confirmed defect is real** (read verbatim this pass): `constraint/domain.rs:72-75`, the
`(Some(existing), Some(ty)) if existing != ty` arm with comment *"may refine types during
convergence"* — last-writer-wins on a genuine unification clash: non-monotone, order-dependent, a
silent overwrite where `no-silent-epsilon` demands a raised obligation. **R-B2 CONFIRMED.**

**Open gaps.**
- **G-1B-1 [R-B2 unfixed — AND it biases the census that judges the family].** PROTO-1 reads
  homo/hetero residue from the *solved* `TypeDesc`; a clash this arm silently swallowed into a single
  type is counted as `equality`, never as residue. **The THIN verdict and the R-B2 defect are not
  independent — the defect manufactures thinness.** PROTO-1 did not instrument how often the
  `existing != ty` arm fires, so the bias is unquantified (mitigating: propagate converged in 0.26s
  with no oscillation, suggesting the arm rarely fired — but "rare" is asserted, not counted).
- **G-1B-2 [the "collapse to 1A" retirement is UNSOUND as argued — X3/X4].** (X3) Both PROTO-1
  pre-registered gates FAIL on all four corpora (`eq+sum ≥95%` lands 82–89%; `structural-non-eq ≤2`
  lands 18/1020/53/87); the "THIN — decisive" verdict is reached by **discarding both and
  substituting a post-hoc third gate** chosen because it yields THIN. (X4) The census measures 1B's
  *own impoverished pass*, which has **zero overload/legality code** (grep-verified this pass), so
  the rich-residue thesis is tested against an instrument that structurally cannot exhibit it.
  **Retiring the running type-system-of-record on that census is unjustified.** Genuinely open.
- **G-1B-3 [R-B1 store-shape UNBUILT].** Federation vs single sum-domain — FAMILY-SPECS: "needs a
  prototype, not paper." **No prototype ran.** The decode-class-depends-on-sum-variant counterexample
  was not built either way. The decision the family "cannot make on paper" is exactly as unmade.
- **G-1B-4 [distinctness dilution — is 1B, post-census, a distinct family or 1A's residue-solver?].**
  The tree runs the federated form (`Csp<TypeDomain>` + `Csp<CostFiniteDomain>` sequenced by
  `solve_with_given`), which *is* HM(X) with X = the solver — i.e. 1A's architecture with a CSP
  residue solver. "One store" honestly means one engine + one obligation channel + one discipline,
  not one variable arena. Whether that dilution retires the *thesis* (not the code) rides on R-B4,
  which the unsound census failed to close.
- **G-1B-5 [R-B2 fix not shown backward-safe].** Fixing `join` to fail-explicit changes live shipping
  code asserted by `types_heterogeneous_alt`; no probe confirmed the corpus still solves once
  refinement is reserved for bottom→ground.

**Pass-2 directives.**
1. **Fix R-B2 to fail-explicit** (raise a clash obligation; reserve refinement for bottom→ground),
   confirm the corpus still solves (`types_heterogeneous_alt` green), **then re-run the census
   counting `existing != ty` arm hits** — sever the defect↔thinness entanglement (G-1B-1).
2. **Re-run the residue census on an instrument that MODELS overload/legality** (X4) — the census
   cannot refute the rich-residue thesis about judgments its instrument does not implement. Honor the
   registered gate or formally re-cut it **in advance**, not post-hoc (X3).
3. **Build R-B1 both ways** (federated-`solve_with_given` vs single sum-domain) on the
   decode-class-depends-on-sum-variant counterexample — the store-shape decision the family cannot
   make on paper (G-1B-3).

**Withheld from researchers:** do **not** re-assert "1B collapses to 1A" on the current census — it
is unsound (X3/X4) and the bank decision is *deferred to the faithful re-run*; do **not** propose new
csp-solver *engine* features (vendor sync gate) — stay on the consumer side (`crates/ir/src/passes/`)
adding *domains*, per the `ThreadSafe` precedent.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · BANKED · 38%

**Why BANKED (redirect / crowding).** The incompatibility map is decisive: 1C is **not a fourth
engine — it is a strict superset of 1A that degenerates to 1A's synthesis where there are no
annotations** (R-C2, self-admitted), reuses PROVE-08's `Unifier` as its subsumption reconciler, and
its natural form is *one family: a union-find trunk (1A) with a bidirectional checking + diagnostics
layer (1C).* Per the redirect rule, the **standalone-fourth-engine formulation is BANKED**;
bidirectional check/synthesize survives **subsumed as 1A's diagnostics-and-locality discipline** (the
named cross-pollination below). It reactivates to ADVANCED only when the R-C2 head-to-head proves the
layer earns a name.

**What is genuine.** Bidirectional check/synthesize improves error locality (prior-art §2D);
`classify_arrow` is a two-line bidirectional seed already in-tree; the `Infer`/`infer_expr` substrate
it splits exists in `skinny/crates/passes`. As a discipline layered on the winning engine, the idea
is sound and the graft is clean.

**Open gaps.**
- **G-1C-1 [the make-or-break diagnostic head-to-head was NEVER run].** FAMILY-SPECS and research-C
  §6.2 both name R-C2 the family's make-or-break: *does 1C localize a diagnostic where unify-first
  does not?* No PROTO exercised "feed `string -> decode_…(input) : Bool` to unify-first and to
  `check_expr`; assert 1C's `TypeError.span == Project.SourceSpan` while unify-first reports at a
  synthesized interior." **The single experiment that justifies 1C's separate identity is
  unexecuted** — its distinctness is asserted, not measured.
- **G-1C-2 [degeneracy + tiny surface].** On generic `json.bbnf` (zero `->`) 1C ≡ 1A. Its value
  exists only on typed grammars — and PROTO-1's own numbers show heterogeneous residue ≤2% even on
  CSS L4, i.e. very few sites where checking-against-a-declared-type differs from synthesis. The
  distinctness surface is small **and** unmeasured.
- **G-1C-3 [retired-lineage substrate + R-C5 span gap].** Every seam (`synth`/`check` split,
  `RuleSig`, the `Project` mode-switch, the `PassDiagnostic` span-add) lives in the **retiring**
  skinny workspace; if the base is root the diagnostics type differs from the archived
  `PassDiagnostic` R-C5 targets, so even the substrate gap is mis-addressed.
- **G-1C-4 [R-C4 subtype-join-not-equality / R-C3 sub-span].** Both design-fixed, neither built;
  getting the numeric seam wrong collapses 1C into spurious-clash (too strict) or lost-width (too
  loose). `Project.ty` is one `String` — sub-span is the quality ceiling on the whole thesis.

**Pass-2 directives (under the banked banner; reactivate to ADVANCED only if R-C2 lands).**
1. **Run the R-C2 diagnostic head-to-head** — the single experiment that decides whether 1C is a
   named discipline or a footnote on 1A. If unify-first reports the same span on the same erroneous
   grammar, 1C carries no distinct value and stays banked as a mere synthesis alias.
2. If R-C2 lands: re-target the substrate against the **root** base (not the retiring skinny
   `PassDiagnostic`), and specify the `Project` sub-span enrichment (R-C3) as the quality ceiling.

**Withheld from researchers:** do **not** re-argue 1C as a distinct fourth engine (BANKED — it is a
discipline on the winner); do **not** develop the width seam (R-C4) as a *separate* subsystem — it is
the same width residue 1A/1B own; 1C only *consumes* the resolved width at the subsumption site.

---

## FAMILY 1D — TYPES-AS-ECLASSES · BANKED · 48%

**Why BANKED, with "zero new engine" RETIRED as a named negative (redirect / crowding).** PROTO-2
advanced 1D factually on the **recursion axis** (injectivity=unification (a)–(d); width+injectivity
fixpoint converges in 2 passes ≤ bound, no oscillation (e)); the native-cyclic μ-extraction for free
via the shipped visited set is a **genuine, measured win** and 1D is genuinely ahead of 1A's *unbuilt*
nominal fold there. Credit where due. **But its distinctive engine (equality saturation +
cost-extraction) is idle where the census found the mass** (overload/cost residue = 0 in all four
corpora, verified) — pure HM has a unique principal type, so there is **nothing to extract**. Its only
proven edge (free sum + recursion) is precisely what FAMILY-SPECS says 1A reaches more cheaply via the
nominal fold. Per the redirect rule, the **types-as-eclasses engine is BANKED** — it subordinates as
LOCK-2 substrate (the 1B⊥1D soft-incompatibility resolves by subordination, not deletion) — and its
charter's **"zero new engine — only a third `Language`" claim is RETIRED as a named negative:**

> **RETIRED sub-verdict.** "Zero new engine" is **falsified, verified against HEAD.** PROTO-2's own
> patch modifies the *shared* engine `crates/egraph/src/egraph.rs::union` — deleting the `if changed
> { analysis_pending.push }` guard so `modify` fires on **every** merge, plus a `rebuild_pass_count`
> field. At HEAD the guard is **still present** (`egraph.rs:236-238`) and `rebuild_pass_count` is
> **absent** — the amendment is **reverted**. So on the shipped engine injectivity **never fires** on
> a structural `Sum`/`Arrow`/`List` merge (they do not move the width lattice). 1D's net-new primitive
> is (~22 LOC modify hook) **+ a core-engine scheduling change to the substrate LOCK-2/LOCK-5 consume**
> — not a free ~30-LOC hook, and not "a third Language only." The registry corrects the VIABLE-AT-RISK
> "~30 LOC classical primitive on a shipped engine" grading, which predated PROTO-2 (03:49 < 04:10).

**Open gaps.**
- **G-1D-1 [CROSS-LOCK blast radius — "free" is asserted, never measured].** The amended `union()`
  sits on LOCK-2's `GrammarENode` and LOCK-5's `RegexENode` hot saturation path; every union there
  now unconditionally enqueues `analysis_pending`, re-canonicalizes, and calls `modify` (a no-op for
  `GrammarAnalysis`/`NoAnalysis`) — extra worklist churn per union across two other locks. PROTO-2
  verified **correctness** (37 pass) and **explicitly makes no perf claim.** A masked cross-lock cost;
  1D's disposition **cannot be settled inside LOCK-1** — it needs LOCK-2's sign-off on the engine change.
- **G-1D-2 [AC-vs-injective coexistence UNBUILT — the elegant-reduction trap].** Positional
  injectivity is **unsound for AC constructors** (`Sum`): two differently-ordered same-arity sums
  falsely unify (`null`≡`str`). The probe stayed sound by testing the two mechanisms in **separate
  e-graphs** — `Sum`-as-tuple injectivity with no AC rewrites (a), AC-sum rewrites with injectivity
  dormant on `Sum` (b/c/d). **The hard interaction — AC canonicalization and injectivity both live on
  `Sum`, gated by the constructor-injective-vs-AC flag PROTO-2 calls "the one non-negotiable design
  obligation" — was never built.** The prototype tested *around* the soundness question, not through it.
- **G-1D-3 [idle-machinery raison-d'être still unmet].** FAMILY-SPECS: two convergent negatives retire
  1D — a thin residue **and** a sufficient nominal fold. PROTO-2 retired only the *recursion*
  condition (native cyclicity beats the fold) and did **not** retire the *residue/idle-machinery*
  condition; PROTO-1 delivers the thin residue (document shapes), so the correct reading is **1D is
  sound and elegant, not necessary** — "elegance earns nothing." (The thin-residue leg is itself on the
  unsound X3/X4 census, so even this retirement is not cleanly earned.)
- **G-1D-4 [determinism gate + TS isomorphism unproven at scale].** R4 byte-identical extraction held
  only on the 10-class toy sum; the GN-3 W3 gate (byte-identical `DirectSchemaSet` over the real typed
  grammars under FxHashMap iteration order) was not exercised. How a TS backend consumes e-graph-derived
  types (constitution iii) is unspecified.

**Pass-2 directives (under the banked banner; reactivate only if the raison-d'être is met concretely).**
1. **Build the AC-sum + injectivity coexistence** with the constructor-injective flag **active on
   `Sum` under saturation** — the untested interaction that is 1D's real soundness question (G-1D-2).
2. **Route the `union()` amendment through LOCK-2 as a named cross-lock cost** and **measure it**
   against LOCK-2/LOCK-5 saturation (the "free for no-op analyses" claim) — this is a GESTALT-B/LOCK-2
   adjudication, not a LOCK-1-local decision (G-1D-1, X5).
3. Provide the **1A nominal-fold opponent** (see 1A directive 1) so the recursion "win" is an actual
   head-to-head; and re-assess idle-machinery only after the faithful overload-modeling residue census
   (1B directive 2) — if it stays 0, the cost extractor has nothing to rank and 1D does not reactivate.

**Withheld from researchers:** the **"zero new engine" claim is RETIRED** — do not re-pose it; do not
carry the `union()` amendment as a free LOCK-1 hook (it is a shared-engine change); do not model width
as distinct `Int8`/`Int16` *constructors* (R3 — must be an analysis-lattice fact on one `Int` class or
injectivity has nothing to decompose).

---

## REDIRECT / CROWDING DECISIONS (this pass)

The four families collapse to a **two-way equality-engine fork + one banked discipline + one banked
engine** on the surviving base (root):

1. **1C → BANKED into the 1A trunk as a discipline.** Both critics + the incompatibility map agree 1C
   is "not a fourth engine but a discipline that layers onto the winner (naturally 1A)." The
   standalone-family formulation is banked; bidirectional check/synthesize survives as 1A's
   diagnostics-and-locality layer (the named graft below). Reactivation gate: R-C2 head-to-head.
2. **1D → BANKED as LOCK-2 substrate; its "zero new engine" charter claim RETIRED.** The distinctive
   cost-extraction engine is idle on this plane (overload = 0, verified); the recursion win is real but
   duplicable by 1A's fold; the engine amendment is a LOCK-2 cost, not a LOCK-1 free hook. The
   1B⊥1D soft-incompatibility resolves by **subordination under LOCK-2**, not deletion.
3. **The 1A/1B equality-engine fork stays OPEN — both ADVANCED — because the adjudicating measurement
   is unsound/unbuilt.** The conservative call: 1A (endorsed architecture, zero prototype, now a
   port+displace onto root) vs 1B (running system-of-record, one confirmed defect, retirement
   unearned). Banking either on the current evidence would be perverse — banking 1B needs a *sound*
   residue census (the present one is X3/X4-flawed); banking 1A needs a reason to discard the endorsed
   spine there is none for. The bank decision is **deferred to a named pass-2 experiment**: the
   R-B2-fixed, overload-modeling residue census + the 1A recursion race + the numeric-width probe.
4. **Open the underexplored formulation the four charters all under-cite — the ATTRIBUTE-GRAMMAR
   center (prior-art §2E).** All three engines re-derive the cross-rule seam independently (1A: SCC
   generalize-after-fixpoint; 1B: federation sequencing via `solve_with_given`; 1C: SCC bounded
   fixpoint). Silver/JastAdd's synthesized/reference/circular-attribute discipline with a termination
   lattice is "the deepest whole-system analogue and is under-cited in the family charters" — a
   genuinely underexplored formulation of the *cross-rule seam* that could unify what the three engines
   each hand-roll. Open it as a pass-2 research thread (not a fifth engine — a seam discipline).

## CROSS-POLLINATION (the one named, justified graft this pass)

**1C's bidirectional check/synthesize ⟶ the 1A trunk.** Both families are developed enough to name the
graft: 1A has the union-find core (PROVE-08) + the `classify_arrow` bidirectional seed; 1C has the
mode-switch + the SCC discipline mapped onto the *same* `infer_expr`. The graft: 1A's `infer_expr`
becomes `synth_expr`, 1C's `check_expr` overlays it, and **PROVE-08's `Unifier` is the shared
subsumption reconciler** (equality-unify for non-numeric leaves; MLsub subtype-join at the numeric
seam, R-C4). This *is* the incompatibility map's "1C ∥ 1A compatible overlay" and the reason 1C is
banked-as-discipline rather than a competing engine. *Deferred graft (not this pass):* 1D's
native-cyclic μ-extraction vs 1A's nominal fold — cannot graft an unbuilt comparison; re-evaluate only
after the 1A recursion prototype races PROTO-2 (1A directive 1).

## CROSS-LOCK / GESTALT DIRECTIVES

1. **X1 LINEAGE — RESOLVED at the SK-V24 level, and it inverts the fork cost.** skinny **retires**
   (skv24-handoff supersedes SK-V22's graduation); **root is the greenfield base.** 1B/1D substrate
   survives on the base ("already runs" is a real win); 1A/1C's skinny scaffold is on the retiring tree
   → their mechanism becomes a **PROVE-08 port into root + reconciliation with the running 1B CSP**.
   GESTALT-B must ratify this base call and carry the port cost in the 1A/1C accounting.
2. **X2 — record the GN-3 §1.5 override as a NAMED SUPERSESSION (Constitution ix).** FAMILY-SPECS §0
   correctly finds prior-art C4 / GN-3 §1.5's "no CSP or e-graph consumer exists in this tranche" is
   *factually void for the root lineage* (both engines ship with live backend consumers — verified).
   But GN-3 §1.5 is a committed codex adjudication; calling it "factually false" inline is a
   correction, not a supersession. **Amend GN-3 §1.5 with a formal named supersession** or the lock
   violates its own constitution clause. Procedural, owed.
3. **X5 — 1D↔LOCK-2 is DOUBLE-HOMED, not merely substrate-shared.** 1D *modifies* LOCK-2's engine
   (the `union()` amendment), it does not merely consume it. The disposition of 1D's substrate cannot
   be "banked under LOCK-2" quietly — the engine change is a LOCK-2/GESTALT-B adjudication with a
   **required perf measurement** across the grammar + regex tiers.
4. **Land or retire both probes in the tree.** PROTO-1 (`lock1_residue_census.rs`) and PROTO-2
   (`type_injectivity.rs` + the `egraph` amendment) are **absent from HEAD** — pass-1 evidence lives
   only in `.patch.diff`, non-reproducible. Land them (behind the base decision) or state them retired.
5. **Zero-runtime-cost gate is asserted by every family, exercised by none** — a named gap for all four
   in pass-2 (`git diff crates/runtime/` empty on every wave).

---

## PASS HISTORY

### Pass 1 (folded by AGGLOMERATOR, Fable-tier)

- **Inputs:** research-{A,B,C,D,priorart}, FAMILY-SPECS, PROTO-1 (judgment-mix census, RAN_MEASURED —
  document-shape residue only), PROTO-2 (1D injectivity + cyclic-sum, RAN_MEASURED on a reverted engine
  patch), CRIT-1 (within-lock), CRIT-2 (cross-lock).
- **Agglomerator verification (live tree):** confirmed every crux fact both critics rest on — R-B2 arm
  live at `constraint/domain.rs:72-75`; the `egraph.rs:236-238` `if changed` guard **live** +
  `rebuild_pass_count` **absent** (PROTO-2 engine amendment reverted → 1D "zero new engine" falsified);
  `crates/ir/src/passes/types/` has **zero** overload/legality code (X4 — the census measured an engine
  that cannot exhibit the rich residue); both probe tests **absent** from HEAD; skinny `:204` `->`
  discard verbatim; root `HeterogeneousAltJoin` live. **The critics did not err on any load-bearing
  fact.** One correction: CRIT-2 X1's "authorities conflict / lineage undecided" is **stale** —
  SK-V24 handoff supersedes SK-V22 (skinny retires, root is base); the correction **resolves the
  lineage against 1A/1C** (their scaffold is on the retiring tree), so it raises no floor.
- **Floors taken unadjusted:** 1A 52% · 1B 58% · 1C 38% · 1D 48% (min across critics; critic-2 harsher
  on all four, verified sound).
- **Dispositions set:** 1A ADVANCED (endorsed trunk, unproven, now a port+displace) 52%; 1B ADVANCED
  (running system-of-record, R-B2 confirmed, retirement unearned) 58%; 1C BANKED (discipline on 1A;
  standalone-engine banked) 38%; 1D BANKED (LOCK-2 substrate; "zero new engine" RETIRED as named
  negative) 48%.
- **Redirects:** 4 families → a 2-way equality-engine fork (1A vs 1B, both ADVANCED, bank-decision
  deferred to a sound census) + 1C banked-as-discipline + 1D banked-as-LOCK-2-substrate. New
  formulation opened: the attribute-grammar cross-rule-seam center (prior-art §2E). Cross-pollination:
  1C bidirectional overlay ⟶ 1A trunk (shared PROVE-08 `Unifier` reconciler).
- **Lock-wide:** no family convergence-eligible (pass 1 of ≥3; first fresh audit only now). The
  residue axis is measured for document shapes only (thin, but on an X3/X4-flawed census that cannot
  see numeric/overload residue); the recursion axis is a one-sided 1D build; the lineage inverts the
  1A/1C cost. The synthesis verdict ("collapse to 1A-trunk + optional 1C") is **prior-art-consonant
  and probably right, but framed — not earned — on pass-1 evidence.**

<!-- Pass 2 appends below this line -->

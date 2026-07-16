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
`typed-materialization-invariant`, zero-runtime-cost (`git diff crates/runtime/` empty),
consumer-less-substrate forbidden (Y.13), `pluggable-components`.

---

## VERIFIED BASELINE (re-anchored by the agglomerator against live tree this pass)

The lock references **two live, git-tracked, separately-buildable cargo workspaces** — re-verified
this pass, both real:

- **`skinny/`** (`skinny/Cargo.toml`) — carries the **1A/1C substrate**: the dormant HM(X) scaffold.
  Confirmed verbatim: `skinny/crates/passes/src/lib.rs:204` is
  `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (the `->` discard); `TypeFacts` with
  `subst`/`obligations` populated by nothing.
- **root `/Cargo.toml`** (the active `cargo xtask regen` tree) — carries the **live 1B/1D
  substrate**: the running lattice-CSP type inference (`crates/ir/src/passes/types/constraint/*`,
  `TypeDomain: LatticeDomain`, `HeterogeneousAltJoin` live at
  `crates/ir/src/types/type_desc.rs:63,199,209`, both backend consumers) and the shipped
  `crates/egraph`. Confirmed live.

**LINEAGE — RESOLVED, and it inverts the fork's cost accounting.** Project memory
`project_skv24_handoff.md` governs and states verbatim: *"Skinny RETIRES. Supersedes
skv22-graduation's tree-of-record premise … its code does not graduate wholesale — every carry
re-earns its place."* So **root is the greenfield base; skinny retires.** This resolves CRIT-2's X1
("authorities conflict; SK-V22 says skinny graduates") — CRIT-2 relied on the *superseded* memo.
**But the resolution does not rescue 1A/1C — it inverts their cost against them:** their scaffold
sits on the **retiring** tree, so "activate the dormant `TypeFacts`" becomes "port PROVE-08 + the
scaffold into root and reconcile with / displace the running 1B CSP" — the full 1A⊥1B collision at
displacement cost. 1B/1D substrate, by contrast, already runs on the surviving base.

### Anchors re-verified verbatim this pass (agglomerator re-ran every grep)

| Fact | Live truth | Bearing |
|---|---|---|
| `->` ty-drop (1A/1C target) | `skinny/…/passes/src/lib.rs:204` `Project { body, .. } => infer_expr(*body)?` — verbatim | 1A/1C mechanism site, on the **retiring** tree |
| 1B running type system | `HeterogeneousAltJoin` live `type_desc.rs:63`; `TypeDomain: LatticeDomain`; both backends consume | 1B ships on the surviving base — not speculative substrate |
| **`TypeDomain::join` is DEAD CODE** | `grep -rn '\.join(' crates/ir/src` → only `FactWidth::join` (facts.rs:223) + `str.join("\|")` (regex.rs:300). `TypeDomain::join` (domain.rs:66-80) has **ZERO call sites** in the type pass | **CORRECTS prior draft.** The "confirmed shipping defect" is unreachable dead code — NOT a live bug |
| Live equality carrier | `assign` (`revise.rs:27-35`) `*slot = Some(ty)` no clash check — last-writer-wins under an **uncapped** monotonic loop (`monotonic.rs:29-44`) | latent hazard: two incompatible *grounds* would flip-flop forever — but **unreachable** under well-formed tree IR |
| Live clash path is SOUND | the only reachable clash site (Alt join, `revise.rs:116` `join_types`) lifts distinct leaves to `HeterogeneousAltJoin` + a named obligation → raise-on-disagreement | 1B is **sound on every reachable surface** — the opposite of the prior "unsound as shipped" verdict |
| **1D "zero new engine" FALSIFIED** | `egraph.rs:236-238` still `let changed = A::merge(…); if changed { analysis_pending.push(dst) }`; `rebuild_pass_count` **absent** | PROTO-2's `union()`-scheduling amendment (+30/−4) is **reverted, not in tree**; on the shipped engine injectivity never fires on a structural `Sum`/`Arrow`/`List` merge |
| **X4 overload-absence** | `crates/ir/src/passes/types/` has **zero** `overload`/`escape`/`legality` code (grep empty) | the census measured an engine that **structurally cannot** exhibit the overload/decode residue the rich thesis rests on |
| Both probes non-reproducible | `crates/core/tests/lock1_residue_census.rs` **absent**; `crates/egraph/tests/type_injectivity.rs` **absent** | PROTO-1 & PROTO-2 survive only as `.patch.diff`; a pass-2 reproducibility obligation |

**The material correction this pass.** The prior registry draft (and the FAMILY-SPECS it folded)
branded `TypeDomain::join` (domain.rs:72-77, the `existing != ty => overwrite` arm) the **"confirmed
shipping defect … Wand's principal-type-losing bug in production."** Both current critics (CRIT-1
§0.2, CRIT-2 §0a) and the handed-in **PROTO-1B** refute this: the arm is **dead code**, and the live
pass raises `HeterogeneousAltJoin([Bool, F64])` + a named obligation on two incompatible `->` arrows
(measured — `cargo test -p bbnf-ir --test lattices clash`, 2 passed). The single most-cited concrete
finding of pass-1 was a defect in unreachable code. This **removes the specs' main lever** for
"seed 1A's union-find instead of the running CSP," and **reopens the carrier fork.**

**Consequence.** The lock is an **engine-of-record fork on the surviving base (root)**, and it is
*more open* than the prior draft claimed: adopt the *proven-elsewhere* union-find HM core (1A —
now a **port + displace** into root) versus **harden the *already-running-here* lattice-CSP** (1B —
already sound on the reachable surface; needs only additive `assign` hardening), with 1D a third
engine that *modifies* the shared substrate and 1C a discipline that layers onto the winner. The
honest adjudicator remains **residue richness** — and **that axis is NOT cleanly closed**: the one
census (PROTO-1) measured document-shape residue thin (≤2%) but pre-tagged width/capacity/decode
single-valued, substituted a post-hoc gate (X3), and ran on an instrument with **zero overload
code** (X4). "Collapse to 1A" is **framed for document shapes, unearned for the numeric/overload
axis.**

---

## CURRENT DISPOSITIONS (as of pass-1, corrected fold)

| Family | Status | Convergence | One-line |
|---|---|---|---|
| **1A · UNIFICATION-FIRST** | **ADVANCED** (prior-art-endorsed trunk; unproven in tree) | **55%** | The endorsed HM(X) spine — but **zero direct prototype**, its carrier-displacement justification (CSP "unsound as shipped") **collapsed to dead code**, and the lineage resolution turns "activate scaffold" into "port + displace the running root CSP." |
| **1B · ONE-CONSTRAINT-STORE** | **ADVANCED** (the running system-of-record; live path SOUND) | **55%** | The **only family that runs on the surviving base**, and PROTO-1B proves it **raises on every reachable clash** — its "unsound as shipped" indictment is refuted (dead code). Retirement-to-1A **lost its main prop**; may be the *cheaper* spine (harden, don't delete). Store-shape (R-B1) unbuilt; census (X3/X4) can't judge it. |
| **1C · BIDIRECTIONAL-LOCAL** | **BANKED** (subsumed as the winner's diagnostics-and-locality discipline) | **40%** | Not a fourth engine — a discipline that layers onto the winner (naturally 1A). Its make-or-break head-to-head (R-C2) was **never run across two brief-cycles**; raison d'être **structurally unfalsifiable today** (zero `->` corpus). |
| **1D · TYPES-AS-ECLASSES** | **BANKED** (recursion-keep RETIRED; narrow structural-μ hook survives, architecture-gated) | **45%** | Only ran-and-measured family. **PROTO-2-recursion-parity → RETIRE:** the running CSP is finite+terminating on every real recursive grammar; 1D's finer μ-sum is unused capacity. "Zero new engine" **falsified**; cost-extraction **idle** (overload = 0). The **one** surviving hook — 1D's structural μ-type (`List ≠ Row`) — turns on an **unadjudicated architecture call** (grammar-authoritative vs codegen-distributed) that a standing law bears on. |

**Floor rule applied.** Floors = **min across the two current (post-probe) critics**, taken
**unadjusted**:
`1A floor(68,55)=55 · 1B floor(65,55)=55 · 1C floor(55,40)=40 · 1D floor(52,45)=45`.
The agglomerator re-verified every crux fact both critics rest on (dead-code `TypeDomain::join`, the
live `assign` carrier, the `egraph.rs:236-238` guard + `rebuild_pass_count` absence, X4
overload-absence, both probe absences, skinny `:204`, live `HeterogeneousAltJoin`) — **the critics
did not err on any load-bearing fact.** The one correction (CRIT-2's X1 "authorities conflict" is
stale — skv24-handoff supersedes SK-V22, skinny retires) **resolves lineage against 1A/1C**, so it
moves **no floor upward.** Floors stand.

> **Correction to the prior draft's scores.** The earlier registry text (52/58/38/48) was folded
> against the *pre-probe* critic drafts (cited as CRIT-1 70/68/55/60, CRIT-2 52/58/38/48) — which
> repeated the dead-code mis-attribution and predate PROTO-1B (04:48) and PROTO-2-recursion-parity
> (05:01). The current CRIT-1.md (68/65/55/52) and CRIT-2.md (55/55/40/45) *consume* both probes.
> This fold re-anchors on them: **55 / 55 / 40 / 45.** The 1A↔1B tie is the honest read — the
> carrier fork is genuinely open and neither carrier dominates on the corrected evidence.

---

## FAMILY 1A — UNIFICATION-FIRST · ADVANCED · 55%

**What is genuine (verified).** HM(X)/OutsideIn(X) stratification is the field's settled answer
(prior-art §2A/§5): generate equality by unification, defer the theory residue to a bounded solver.
The union-find leaf core is proven (PROVE-08); the dormant `TypeFacts { subst, obligations }`
scaffold really exists in `skinny/crates/passes` and the `:204` discard is verbatim. On paper it is
the endorsed trunk — and PROTO-2-recursion-parity now **indirectly evidences** its recursion story:
the *running* pass already closes recursion **nominally** (`BoxedEnum`/`Enum` knot on sight) and
terminates finitely on JSON, Sheets (mutual), and CSS L4 — i.e. a nominal fold, 1A's own mechanism,
is empirically proven to work in tree.

**Open gaps.**
- **G-1A-1 [lineage inverts the cost — scaffold is on the RETIRING tree].** With skinny retiring and
  root the base, "activate the dormant scaffold" becomes "port PROVE-08 + `TypeFacts` into root and
  reconcile with / displace the running 1B CSP" — the full 1A⊥1B HARD-INCOMPATIBLE collision at
  displacement cost. 1A's apparent cheapness does not survive.
- **G-1A-2 [the carrier-displacement argument LOST its lever this pass].** The prior draft justified
  paying the displacement cost by "the running CSP is overwrite-unsound as shipped." That is **dead
  code** (verified). With the live CSP path proven sound (PROTO-1B), the reason to prefer union-find
  *as the carrier* over the running CSP shrinks to prior-art familiarity + "raises clashes
  correctly" — which the running Alt-join *also* does. **The carrier fork is more open, not less.**
- **G-1A-3 [no DIRECT 1A prototype — every gate asserted].** The nominal `unify(Rule i,Rule j)=i==j`
  fold was never built as 1A code; its recursion story rides on the *incumbent's* proof
  (PROTO-2-parity) and is *attacked* by the strawman PROTO-2. SPEC_ONLY on its own (Constitution vii).
- **G-1A-4 [row discipline unpicked].** Leijen scoped-labels vs Gaster–Jones lacks — still
  "Unresolved," principal-type-critical (Wand's trap), **unmeasurable** (zero `->` corpus).
- **G-1A-5 [numeric-width residue UNMEASURED, not resolved].** PROTO-1's `width` tag is Seq/tuple
  *arity*; numeric width goes to `decode-class`, pre-tagged single-valued (X4). 1A §1.7's own central
  sub-question (defaulting lattice vs CSP for widths) is open on the evidence, contra the synthesis's
  implication that the census fixed it. (PROTO-2 test-(e) hints a monotone join suffices — but on a
  *manufactured* residue, not 1A's, and not measured on a corpus.)
- **G-1A-6 [generalization criterion + zero-runtime asserted, not exercised].** The value-restriction
  criterion is unwritten (low-risk — rules are pure schema producers); no artifact drove
  `git diff crates/runtime/` empty or the second type system deleted.

**Pass-2 directives.**
1. **Build the 1A nominal-fold recursion prototype and race it against the running CSP** (not the
   strawman) — make the recursion axis an actual head-to-head. PROTO-2-parity already shows the
   incumbent terminates finitely; the open question is whether a *union-find* re-seed matches it at
   equal or lower cost, or merely re-derives what already ships.
2. **Cost the lineage port explicitly** (G-1A-1): what "port PROVE-08 into root + reconcile with /
   displace the running 1B CSP" concretely costs, and measure `git diff crates/runtime/` + derive
   wall-time for the union-find seed against the running-CSP seed head-to-head.
3. **Build the numeric-width probe the census skipped** (X4) — a hand-authored `->` micro-fixture
   genuinely under-determined between u8/u16/u32, measured for *defaulting* (ordered lattice) vs
   *search*. This is the fork's true residue axis.
4. Pick and state the row discipline; write the generalization criterion.

**Withheld from researchers:** do **not** re-derive HM(X) stratification (settled, endorsed); do
**not** re-argue union-find dominates equality asymptotically (settled, prior-art §2B). Build the
recursion race, the width probe, and the port cost — argue nothing more on paper.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · ADVANCED · 55%

**What is genuine (verified at source).** 1B is the **only family that runs on the surviving base** —
`TypeDomain: LatticeDomain`, per-former propagators, `ObligationSink`, `HeterogeneousAltJoin`, both
backend consumers, end-to-end tests. And **PROTO-1B strengthened it in a way the specs never
absorbed:** the live equality path is **sound on every reachable clash.** Measured — feeding two
incompatible `->` arrows (`Bool` vs `F64`) yields `ir.types[0] = HeterogeneousAltJoin([Bool, F64])`
(not a single leaf) + one named `HeterogeneousAltJoin { rule: Some(0) }` obligation. The
"confirmed shipping defect" the prior draft leaned on is **dead code** (`TypeDomain::join`, 0 call
sites); the family raises on disagreement where it matters.

**Open gaps.**
- **G-1B-1 [the "unsound as shipped" indictment is REFUTED — but a latent hazard remains].** The
  real carrier `assign` (revise.rs:27) is last-writer-wins under an **uncapped** loop — two
  incompatible *grounds* on one var would flip-flop forever (worse than overwrite). It is
  **unreachable** under well-formed tree IR (the generator never puts two grounds on one scalar var;
  the only clash site, Alt join, raises). **Gap: add the additive hardening** — on
  `Some(existing) != Some(ty)` record `TypeObligation::TypeClash` and refuse the overwrite (keep
  `existing`, return `Unchanged`), matching the Alt-join discipline. Cheap, ships regardless of
  carrier winner. Not confirmed backward-safe (G-1B-4).
- **G-1B-2 [the "collapse to 1A" retirement is UNSOUND as argued — X3/X4].** (X3) Both PROTO-1
  pre-registered gates FAIL on all four corpora (`eq+sum ≥95%` lands 82–89%; `structural-non-eq ≤2`
  lands 18/1020/53/87); "THIN — decisive" is reached by **substituting a post-hoc third gate** chosen
  because it yields THIN. (X4) The census measures 1B's *own* pass, which has **zero overload/legality
  code** (grep-verified), so the rich-residue thesis is tested against an instrument that structurally
  cannot exhibit it. **Retiring the running system-of-record on that census is unjustified.** Open.
- **G-1B-3 [R-B1 store-shape UNBUILT].** Federation-`solve_with_given` vs single sum-domain — "needs
  a prototype, not paper." No prototype ran; the decode-class-depends-on-sum-variant counterexample
  was built neither way. The decision the family "cannot make on paper" is exactly as unmade.
- **G-1B-4 [distinctness dilution — AND a possible inversion the prior draft missed].** The tree runs
  the federated form (`Csp<TypeDomain>` + `Csp<CostFiniteDomain>` sequenced), which *is* HM(X) with
  X = the solver — 1A's architecture wearing a CSP coat. **But post-refutation, the inversion CRIT-1
  §3.4 raises is live:** the *cheapest sound spine* may be *the running CSP hardened* (both backend
  consumers + sound live path + only additive hardening), **not** a union-find re-seed that *deletes*
  running code (1A's G-1A-1 cost). Whether "one store" is a distinct thesis or 1A's residue-solver
  rides on R-B4, which the unsound census failed to close.
- **G-1B-5 [`assign` hardening not shown backward-safe].** Adding clash-raising changes live code
  asserted by `types_heterogeneous_alt`; no probe confirmed the corpus still solves once overwrite is
  refused.

**Pass-2 directives.**
1. **Land the additive `assign`/`join` raise-on-disagreement hardening** (record `TypeClash`, refuse
   the overwrite) — converts the latent oscillation hazard into an explicit obligation. Confirm the
   corpus still solves (`types_heterogeneous_alt` green). Ships regardless of the carrier winner.
2. **Re-run the residue census on an instrument that MODELS overload/legality** (X4), honoring the
   registered gate or formally re-cutting it **in advance** (X3). The census cannot refute the
   rich-residue thesis about judgments its instrument does not implement.
3. **Adjudicate the carrier inversion (G-1B-4):** measure "running-CSP hardened" vs "union-find
   re-seed" head-to-head on derive wall-time + `git diff crates/runtime/` + LOC-deleted. The prior
   tilt to union-find rested on a dead-code defect.
4. **Build R-B1 both ways** on the decode-class-depends-on-sum-variant counterexample (G-1B-3).

**Withheld from researchers:** do **not** re-assert "1B collapses to 1A" on the current census (X3/X4
unsound; bank-decision deferred to a faithful re-run); do **not** propose new csp-solver *engine*
features (vendor sync gate) — stay consumer-side (`crates/ir/src/passes/`) adding *domains*, per the
`ThreadSafe` precedent.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · BANKED · 40%

**Why BANKED (redirect / crowding).** The incompatibility map is decisive: 1C is **not a fourth
engine — it is a strict superset of 1A that degenerates to 1A's synthesis where there are no
annotations** (R-C2, self-admitted), reuses PROVE-08's `Unifier` as its subsumption reconciler, and
its natural form is one family: a union-find (or CSP) trunk with a bidirectional checking +
diagnostics layer. Per the redirect rule the **standalone-fourth-engine formulation is BANKED**;
bidirectional check/synthesize survives **subsumed as the winner's diagnostics-and-locality
discipline** (the named graft below). It reactivates to ADVANCED only when R-C2 proves the layer
earns a name.

**What is genuine.** Bidirectional check/synthesize improves error locality (prior-art §2D);
`classify_arrow` is a two-line bidirectional seed already in-tree; the `infer_expr` substrate it
splits exists in `skinny/crates/passes`. As a discipline layered on the winning engine the idea is
sound and the graft is clean.

**Open gaps.**
- **G-1C-1 [the make-or-break head-to-head was NEVER run — across two brief-cycles].** R-C2: *does
  `check_expr` localize `TypeError.span` at the `Project` span where unify-first reports at a
  synthesized interior?* No PROTO exercised it (PROTO-1/1B/2/2-parity all test other axes). 1C's
  distinct identity is **asserted, not measured** — the pass's most conspicuous omission, unchanged.
- **G-1C-2 [structurally unfalsifiable today].** On generic `json.bbnf` (zero `->`) 1C ≡ 1A; the
  typed grammars that would exercise it (twitter/citm/canada) **do not exist** — G2 cannot even run
  until GN-3.W3 authors them. PROTO-1 shows heterogeneous residue ≤2% even on CSS L4, so the
  addressable surface is small *and* empty. The disposition is a **bet**, not a finding.
- **G-1C-3 [retired-lineage substrate + R-C5 span gap].** Every seam (`synth`/`check` split,
  `RuleSig`, the `Project` mode-switch, the `PassDiagnostic` span-add) lives in **retiring** skinny;
  on the root base the diagnostics type differs — even the substrate gap is mis-addressed. `PassDiagnostic`
  is `RuleId`-only; a `span: Option<SourceSpan>` add is owed regardless of engine winner.
- **G-1C-4 [R-C4 subtype-join / R-C3 sub-span — design-fixed, unbuilt].** MLsub subtype-join reserved
  for numerics; `Project.ty` is one `String` with whole-expr span only — a sub-span the IR does not
  carry is the quality ceiling on 1C's entire thesis.

**Pass-2 directives (under the banked banner; reactivate to ADVANCED only if R-C2 lands).**
1. **Run the R-C2 diagnostic head-to-head** — the single experiment that decides whether 1C is a
   named discipline or a footnote on 1A. Needs a typed `->` grammar authored first (blocks on
   GN-3.W3 or a hand-authored micro-fixture).
2. **Land the `PassDiagnostic.span` add** against the **root** base (not the retiring skinny type),
   and specify the `Project` sub-span enrichment (R-C3) — additive, owed regardless of winner.

**Withheld from researchers:** do **not** re-argue 1C as a distinct fourth engine (BANKED — a
discipline on the winner); do **not** develop the width seam (R-C4) as a *separate* subsystem — it is
the same width residue 1A/1B own; 1C only *consumes* the resolved width at the subsumption site.

---

## FAMILY 1D — TYPES-AS-ECLASSES · BANKED · 45% · recursion-keep RETIRED

**Why BANKED, recursion-keep RETIRED (reconciling the two conflicting probes).** Two probes reached
**opposite verdicts on the same retire-or-keep question**, and the pass must record both:

- **PROTO-2 (strawman)** raced 1D against 1A's *unbuilt* nominal fold → injectivity = unification
  (a–d), width+injectivity fixpoint converges in 2 passes no oscillation (e), native cyclic
  μ-extraction free via the visited set → **"1D does NOT retire."**
- **PROTO-2-recursion-parity (incumbent)** raced 1D against the **running** root CSP → the running
  pass is **finite + terminating** on JSON, Sheets (mutual), CSS L4 (191 rules, 44 recursion knots,
  zero overflow); 1D's finer μ-sum (`Sum[6]`, `List(#0) ≠ Row(#0)`) vs the running `Sum[4]`
  (`array == object == List(Enum)`) is **strictly more faithful but unused capacity** → **RETIRE.**

**Adjudication:** the incumbent-parity verdict governs — the strawman its keep rested on is exactly
what the parity probe falsifies. **1D's recursion "win" beats only an unbuilt opponent; against the
running system it buys nothing on the divergence axis.** The recursion-keep is therefore **RETIRED**.
Combined with the census's thin document-shape residue and the **verified-idle cost-extraction**
(overload/cost residue = 0; pure HM has a unique principal type — nothing to extract), 1D's
distinctive engine is idle where the mass is and empty where its raison d'être lives.

> **RETIRED sub-verdicts (named negatives).**
> - **"Zero new engine" — FALSIFIED, verified at HEAD.** PROTO-2's patch modifies the *shared* engine
>   `egraph.rs::union` (delete the `if changed` guard; push `analysis_pending` unconditionally; add
>   `rebuild_pass_count`; +30/−4). HEAD still carries the guard (`egraph.rs:236-238`) and lacks
>   `rebuild_pass_count` — **the amendment is reverted.** On the shipped engine injectivity **never
>   fires** on a structural `Sum`/`Arrow`/`List` merge. 1D is a ~22-LOC hook **+ a core-engine
>   scheduling change to the substrate LOCK-2/LOCK-5 consume** — not "a third Language only."
> - **Cost-extraction — IDLE (retired on the evidence, X4-caveated).** Overload/cost residue = 0
>   everywhere; the extractor has nothing to rank. (The thin-residue leg is on the X3/X4-flawed
>   census, so this retirement is *plausible, not airtight* — a faithful overload-modeling census
>   could in principle revive it.)
> - **Recursion-axis keep — RETIRED (PROTO-2-recursion-parity).** The running CSP terminates finitely
>   on every real recursive grammar; native cyclicity beats only the strawman.

**The ONE surviving hook (architecture-gated — do NOT retire outright).** 1D is the *only* path that
yields a self-contained **structural** recursive TypeDesc at the rule level (`List ≠ Row`, all arms,
structural back-edges). The running solver deliberately emits a **nominal** skeleton and lets codegen
(`crates/core/src/runtime/json/value.rs` + `JsonStructBuilder`) supply the 6-variant `JsonValue`
downstream. **CRIT-2 G1D-4 is right that the RETIRE reasoning leans on that hand-code fallback** —
and the standing law **`typed-materialization-invariant`** ("every `->` reaches the tape emitter;
inference composes types, never loses them; parity = full typed-AST equivalence") **bears directly on
it.** If the greenfield is **grammar-authoritative-typed** (project memory
`grammar-authoritative-status`), a TypeDesc that collapses `array == object` is a **wall**, and 1D's
structural μ-type is precisely what closes it. That architectural choice — **grammar-authoritative
vs codegen-distributed-nominal** — is **unadjudicated**, and it is the *only* ground on which 1D
revives. Hence BANKED (not RETIRED): the engine and cost-extraction are retired; the structural-μ hook
survives **pending the architecture adjudication**, which is a GESTALT/cross-lock call, not a
recursion race.

**Open gaps.**
- **G-1D-1 [CROSS-LOCK blast radius — "free" asserted, never measured].** The amended `union()` sits
  on LOCK-2's `GrammarENode` and LOCK-5's `RegexENode` hot saturation path; every union there would
  unconditionally enqueue, re-canonicalize, and call `modify` (no-op for those analyses) — extra
  churn per union across two locks. PROTO-2 verified correctness (37 pass) and **makes no perf
  claim.** 1D's disposition **cannot be settled inside LOCK-1** — needs LOCK-2 sign-off + a saturation
  measurement.
- **G-1D-2 [AC-vs-injective coexistence UNBUILT — the elegant-reduction trap].** Positional
  injectivity is **unsound for AC constructors** (`Sum`): two differently-ordered same-arity sums
  falsely unify. The probe stayed sound by running the two mechanisms in **separate e-graphs**
  (injectivity dormant on `Sum` in the cyclic test; AC-rewrites absent in the injectivity test). The
  hard interaction — AC canonicalization *and* injectivity both live on `Sum` under saturation, gated
  by the injective-vs-AC flag PROTO-2 calls "the one non-negotiable design obligation" — was tested
  *around*, never *through*. Confluence asserted.
- **G-1D-3 [the RETIRE itself is architecture-contingent, not clean].** The "unused capacity" verdict
  presupposes the codegen-distributed-nominal architecture and quietly rejects the
  grammar-authoritative one; neither PROTO-2 cleanly decides 1D until that call is made (the
  structural-μ hook above).
- **G-1D-4 [determinism gate + TS isomorphism unproven at scale].** Byte-identical extraction held
  only on a 10-class toy sum; the GN-3.W3 gate (byte-identical `DirectSchemaSet` over real typed
  grammars under FxHashMap iteration at 191-rule CSS) is unexercised. TS-backend consumption of
  e-graph-derived types (Constitution iii) unspecified — 1B's verified consumers include
  `backend/ts/projection.rs`.

**Pass-2 directives (under the banked banner; reactivate only if the structural-μ hook is *needed*).**
1. **Adjudicate the architecture (the decisive experiment, not a recursion race):**
   grammar-authoritative-typed vs codegen-distributed-nominal. This is a GESTALT/cross-lock call
   bearing `typed-materialization-invariant`. If grammar-authoritative wins, 1D's structural μ-type is
   the wall-closer and 1D reactivates on that narrow ground; if codegen-distributed wins, 1D is fully
   RETIRED (the hand-code fallback is legitimate, not a masked one).
2. **Route the `union()` amendment through LOCK-2 as a named cross-lock cost and MEASURE it** against
   LOCK-2/LOCK-5 saturation (the "free for no-op analyses" claim) — a GESTALT-B/LOCK-2 adjudication.
3. **Build the AC-sum + injectivity coexistence** with the constructor-injective flag **active on
   `Sum` under saturation** (G-1D-2) — only if 1D reactivates.

**Withheld from researchers:** the **"zero new engine" claim is RETIRED** — do not re-pose it; do not
carry the `union()` amendment as a free LOCK-1 hook (shared-engine change); do not model width as
distinct `Int8`/`Int16` *constructors* (must be an analysis-lattice fact on one `Int` class, or
injectivity has nothing to decompose); do **not** re-run the strawman recursion race (retired).

---

## REDIRECT / CROWDING DECISIONS (this pass)

The four families collapse to a **two-way equality-engine fork + one banked discipline + one banked
engine** on the surviving base (root):

1. **1C → BANKED into the winning trunk as a discipline.** Both critics + the incompatibility map
   agree 1C "is not a fourth engine but a discipline that layers onto the winner (naturally 1A)."
   Reactivation gate: R-C2 head-to-head.
2. **1D → BANKED; recursion-keep + "zero new engine" + cost-extraction all RETIRED as named
   negatives.** The 1B⊥1D soft-incompatibility resolves by **subordination under LOCK-2**, not
   deletion. The one surviving hook (structural-μ) is architecture-gated (above).
3. **The 1A/1B equality-engine fork stays OPEN — both ADVANCED, tied at 55% — because the
   adjudicating measurement is unsound/unbuilt AND the prior tilt was on dead code.** The families
   have **converged to ONE architecture** (HM(X): equality trunk + bounded residue) — but the
   **redundancy is at the architecture level, not the build level.** The live difference is the
   *carrier*: 1A re-seeds union-find (deleting the running CSP — G-1A-1 displacement cost) vs 1B
   *hardens* the running CSP (additive, keeps both backend consumers). That is a **genuine
   build-cost fork with different runtime-diff footprints**, not vacuous crowding — so **neither is
   force-banked.** Banking 1B needs a *sound* census (present one X3/X4-flawed); banking 1A needs a
   reason to discard the endorsed spine there is none for. The bank decision is **deferred to named
   pass-2 experiments**: the hardened-vs-reseed carrier measurement (1B directive 3) + the
   overload-modeling census (1B directive 2) + the 1A recursion race (1A directive 1) + the
   numeric-width probe (1A directive 3).

**Underexplored formulations to OPEN (the crowding reveals two the charters under-cite):**
- **The pluggable residue-solver X** (standing law `pluggable-components`). 1A/1B have collapsed onto
  HM(X); the *real* open design point is **X**, the residue solver — which PROTO-1 says is an *ordered
  defaulting lattice* (not a CSP, not an extractor), but on an X3/X4-flawed census. Open X as a
  first-class pluggable seam (cost model / defaulting order / decode-class registry), so the carrier
  fork and the residue fork are decided independently rather than bundled.
- **The attribute-grammar cross-rule seam** (prior-art §2E, Silver/JastAdd). All three engines
  re-derive the cross-rule seam independently (1A: SCC generalize-after-fixpoint; 1B: federation
  sequencing via `solve_with_given`; 1C: SCC bounded fixpoint). The synthesized/reference/circular-
  attribute discipline with a termination lattice is the deepest whole-system analogue and is
  under-cited — open it as a pass-2 research thread (a *seam discipline*, not a fifth engine).

## CROSS-POLLINATION (the one named, justified graft this pass)

**1C's bidirectional check/synthesize ⟶ the winning trunk (1A or hardened-1B).** Both families are
developed enough to name the graft: the trunk has the equality core (PROVE-08 `Unifier` for 1A, or
the sound Alt-join for 1B) + the `classify_arrow` bidirectional seed; 1C has the mode-switch + SCC
discipline mapped onto the *same* `infer_expr`. The graft: `infer_expr` becomes `synth_expr`,
`check_expr` overlays it, and **the trunk's equality reconciler is shared** (equality-unify for
non-numeric leaves; MLsub subtype-join at the numeric seam, R-C4). This is the incompatibility map's
"1C ∥ trunk compatible overlay." *Deferred graft (not this pass):* 1D's native-cyclic μ-extraction
vs a nominal fold — cannot graft an unbuilt comparison, and the parity probe retired it; re-evaluate
only if the architecture adjudication revives 1D's structural-μ hook.

## CROSS-LOCK / GESTALT DIRECTIVES

1. **X1 LINEAGE — RESOLVED at the SK-V24 level; it inverts the fork cost.** skinny **retires**
   (`skv24-handoff` supersedes SK-V22's graduation); **root is the greenfield base.** 1B/1D substrate
   survives on the base ("already runs" is a real win); 1A/1C's skinny scaffold is on the retiring
   tree → their mechanism becomes a **PROVE-08 port into root + reconciliation with / displacement of
   the running 1B CSP.** GESTALT-B ratifies this base call and carries the port cost in 1A/1C
   accounting. (This corrects CRIT-2 X1, which relied on the superseded SK-V22 — a named critic error,
   but one that moves no floor upward since it inverts cost *against* 1A/1C.)
2. **X2 — record the GN-3 §1.5 override as a NAMED SUPERSESSION (Constitution ix).** FAMILY-SPECS §0
   finds prior-art C4 / GN-3 §1.5 ("no CSP or e-graph consumer exists") *factually void for root*
   (both engines ship with live backend consumers — verified). But GN-3 §1.5 is a committed codex
   adjudication; **amend it with a formal named supersession** rather than an inline "factually
   false." Procedural, owed to GESTALT-B.
3. **X5 — 1D↔LOCK-2 is DOUBLE-HOMED, not merely substrate-shared.** 1D *modifies* LOCK-2's engine
   (the reverted `union()` amendment). Its disposition cannot be "banked under LOCK-2" quietly — the
   engine change is a LOCK-2/GESTALT-B adjudication with a **required saturation perf measurement**
   across the grammar + regex tiers.
4. **Land or retire both probes in the tree.** PROTO-1 (`lock1_residue_census.rs`) and PROTO-2
   (`type_injectivity.rs` + the `egraph` amendment) are **absent from HEAD** — pass-1 evidence lives
   only in `.patch.diff`, non-reproducible. Land them (behind the base decision) or state them retired.
   PROTO-1B (`lattices/clash.rs`) and PROTO-2-recursion-parity (`lock1_recursion_parity.rs`) likewise
   survive as patches; land the additive `assign` hardening test regardless of carrier.
5. **Zero-runtime-cost gate is asserted by every family, exercised by none** — a named gap for all
   four in pass-2 (`git diff crates/runtime/` empty on every wave).

---

## PASS HISTORY

### Pass 1 (folded by AGGLOMERATOR, Fable-tier) — CORRECTED FOLD

- **Inputs:** research-{A,B,C,D,priorart}; FAMILY-SPECS (04:41, stale — sealed before its two
  decisive briefs ran); PROTO-1 (judgment-mix census, RAN_MEASURED — document-shape residue only);
  PROTO-2 (1D injectivity vs strawman nominal fold, RAN_MEASURED on a reverted engine patch, "keep");
  **PROTO-1B-clash** (RAN_MEASURED, 04:48 — CSP-equality soundness); **PROTO-2-recursion-parity**
  (RAN_MEASURED, 05:01 — 1D vs the *running* CSP, "retire"); CRIT-1 (within-lock, post-probe,
  68/65/55/52); CRIT-2 (cross-lock, post-probe, 55/55/40/45).
- **Correction to the prior draft.** The earlier registry text mis-attributed the "confirmed shipping
  defect" to `TypeDomain::join` (domain.rs:72-77) as **"CONFIRMED live"** and used **stale pre-probe
  critic scores** (52/58/38/48). This fold **corrects both**: the agglomerator re-verified against the
  live tree that `TypeDomain::join` has **zero call sites** (dead code); the live carrier is `assign`
  (revise.rs:27, last-writer-wins but unreachable for clashes); and the reachable clash site (Alt
  join) **raises `HeterogeneousAltJoin` + a named obligation** — the live 1B pass is **sound on every
  reachable surface** (PROTO-1B, measured). The material consequence: the specs' main lever for "seed
  1A's union-find instead of the running CSP" is void, and **the carrier fork reopens.**
- **Agglomerator verification (live tree, this pass):** `TypeDomain::join` 0 call sites (dead);
  `assign` last-writer-wins under uncapped `monotonic.rs` loop; `egraph.rs:236-238` `if changed` guard
  **present** + `rebuild_pass_count` **absent** (1D amendment reverted → "zero new engine" falsified);
  `crates/ir/src/passes/types/` has **zero** overload/legality code (X4); both PROTO-1/2 tests absent
  from HEAD; skinny `:204` `->` discard verbatim; root `HeterogeneousAltJoin` live at
  `type_desc.rs:63`; `skv24-handoff` memory confirms **skinny retires, root is base.** **The current
  critics did not err on any load-bearing fact.** One named critic error: CRIT-2 X1's lineage
  contradiction relied on the *superseded* SK-V22 (skv24-handoff resolves it) — but the resolution
  inverts cost *against* 1A/1C, raising no floor.
- **Floors taken unadjusted (min across current post-probe critics):** 1A 55% · 1B 55% · 1C 40% ·
  1D 45%.
- **Dispositions set:** 1A ADVANCED (endorsed trunk, unproven, port+displace) 55%; 1B ADVANCED
  (running system-of-record, live path SOUND, retirement-to-1A prop refuted, possibly the cheaper
  spine) 55%; 1C BANKED (discipline on the winner; standalone-engine banked) 40%; 1D BANKED
  (recursion-keep + "zero new engine" + cost-extraction RETIRED as named negatives; structural-μ hook
  survives architecture-gated) 45%.
- **Redirects:** 4 families → a 2-way equality-engine fork (1A vs 1B, both ADVANCED, tied 55%,
  bank-decision deferred to a sound census + a carrier measurement) + 1C banked-as-discipline + 1D
  banked-as-LOCK-2-substrate with recursion-keep retired. Two underexplored formulations opened: the
  **pluggable residue-solver X** (`pluggable-components`) and the **attribute-grammar cross-rule seam**
  (prior-art §2E). Cross-pollination: 1C bidirectional overlay ⟶ the winning trunk (shared reconciler).
  The two conflicting PROTO-2 verdicts reconciled: **RETIRE governs on the recursion axis** (strawman
  falsified by incumbent parity), with a structural-μ keep-hook gated on the unadjudicated
  grammar-authoritative-vs-codegen-distributed architecture call (`typed-materialization-invariant`).
- **Lock-wide:** no family convergence-eligible (pass 1 of ≥3; first fresh post-probe audit only now).
  The residue axis is measured for document shapes only (thin, but on an X3/X4-flawed census blind to
  numeric/overload residue); the recursion axis retired 1D against the running incumbent; the lineage
  inverts the 1A/1C cost. The synthesis verdict ("HM(X) trunk + bounded residue + 1C folded in") is
  **prior-art-consonant and probably right — but the carrier seat (union-find reseed vs running-CSP
  hardened) is genuinely open on the corrected evidence, and the 1D structural-μ question turns on an
  architecture call this lock cannot make alone.**

<!-- Pass 2 appends below this line -->

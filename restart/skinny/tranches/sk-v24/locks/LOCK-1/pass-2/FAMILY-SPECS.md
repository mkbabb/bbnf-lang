# LOCK-1 — THE SOLVER SPINE · pass-2 FAMILY-SPECS (SYNTHESIS, Fable-tier)

*One cogent specification per family while the carrier routes remain incompatible; then the
incompatibility map. This supersedes the 15:03 hardened re-fold on the four defects its own two
sharpest probes (PROTO-1 workspace-divorce, PROTO-2-iso) and both critics surfaced after it was
sealed. Every load-bearing fact re-verified against the live tree at **HEAD `0bb33a77b`** (the
critics pinned `c43126422`, the probes `ce51c36fc`; both are ancestors, and the crux greps still
hold at `0bb33a77b` — `assign` is still raw LWW, `TypeDesc::Row` still 0, no `TypeClash` arm).*

---

## 0. GOVERNING FACTS (binding; not re-litigated in this lock)

**From CROSS-LOCK (facts, conveyed):** XL-A skinny RETIRES — skinny@HEAD is the MEASUREMENT
SUBSTRATE only; greenfield/root is the tree-of-record-to-be; ports scope organ-level. XL-C the
regex axis keeps two domain-fit types (no ONE-HIR). XL-D the fast TS face is the BUILD-TIME
emitter (runtime decoders are oracles only) — so `type_desc_to_ts` sits on the *blessed* fast
face, not a refuted path. XL-F the real LOCK-1→LOCK-3 handoff is `TypeAnalysis::from_ir`
(`crates/core/src/backend/driver/analysis.rs:22`) + a second `from_ir`
(`rust/view/named_types.rs:100`); whichever record wins lowers through it. XL-H every measurement
names its HEAD and base-pins; a `.patch`-only artifact is SPEC_ONLY.

**HEAD-verified truths (re-run at `0bb33a77b`):**
- `assign` (`crates/ir/src/passes/types/constraint/revise.rs:27`) is the raw 3-line LWW
  (`if slot==Some(&ty){false} else {*slot=Some(ty);true}`). **X8 unmet after two passes.**
- `TypeDomain::join` (`domain.rs`) has **0 call sites** — the "shipping defect" 1A's
  carrier-displacement lever rested on is **doubly dead** (deadness + non-commutativity of the dead
  arm). The live carrier is `assign` (LWW), reachable-clash-sound (Alt-of-compounds raises a
  `HeterogeneousAltJoin` obligation, not a silent single-leaf).
- The running type CSP (`passes/types/mod.rs:51` `project_types`, `propagate()` at `:67`/`:127`,
  obligations published `:486`) carries **zero let-generalization** (grep `generali|instantiat|
  polymorph|scheme` over `passes/types/` = 1 module doc-comment). `TypeObligation`
  (`obligation.rs:56`) = `{UnresolvedCompoundRef, HeterogeneousAltJoin}` — **no `TypeClash`**.
- `TypeDesc::Row` grep = 0; `LayoutKind` = `{Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}` —
  no Row. `type_desc_to_ts` (`ts/projection.rs:11`) is the **sole** `TypeDesc→TS` sink.
- **Workspace divorce (PROTO-1, dispositive):** `skinny/` is its own `[workspace]`; `skinny/crates/ir`
  is package `ir`, `crates/ir` is `bbnf-ir`; `grep TypeDesc|StructRegistry|LayoutKind skinny/crates/`
  = 0. The `json_templates` mask (`skinny/…/runtime_generator.rs:98 has_rule("value")`) lives on the
  divorced retiring side; Open R's classifier + its `TypeAnalysis::from_ir` sink live on the bbnf-ir
  side. A perfectly-landed Row derivation **cannot cross the boundary** to close the mask.

**Critics' floors (min across the two on-disk pass-2 critics, taken unadjusted):**
`1A min(55,55)=55 · 1B min(55,54)=54 · 1C min(40,40)=40 · 1D min(43,43)=43.` CRIT-2 binds 1B/1D;
both bind 1A/1C. No family is convergence-eligible (pass 2 of ≥3; no clean-pass streak).

**The consensus bank — HM(X), banked as a TARGET, not an artifact.** Prior-art (§2/§5) and both
critics agree the field's settled answer is HM(X)/OutsideIn(X): an equality trunk (union-find over
an ADT/row/sum term algebra) + let-generalization per rule (instantiate-fresh per `Ref`) + a
bounded decidable post-solve for the width/overload residue, with a bidirectional diagnostics
surface. **But the "1A and 1B collapsed onto one architecture" claim is vacuous at the polymorphism
layer:** the running carrier (1B) has no let-gen, and 1A (which would carry it) is unbuilt. HM(X) is
the *direction* every family aims at; **no family instantiates it.** The floors are computed against
an architecture no artifact exercises — pass 3 must build it (Brief 1) or re-open the collapse.

---

## FAMILY 1A — UNIFICATION-FIRST · ADVANCED (carrier cand. #1, LEAN-against) · 55%

**Architecture.** The type plane is a single directed unifier over one union-find store, seeded
from PROVE-08's proven 221-LOC leaf core (`Unifier`: path-halving `find`, constructor-binding
`unify`, honest clash). The monotype algebra grows three principled axes:
`App(Nom(RuleId), args)` (nominal-fold recursion — each rule is its own constructor, descent never
crosses a `Nom`, termination by construction), `Row{fields, tail: Closed|Open(RowVar)}`
(Rémy/Leijen scoped labels — `List ≠ Row ≠ Map` falls out of *what the repeat body unifies to*), and
`Sum` (AC-canonical variant set). Let-generalization per SCC of the rule graph (level/rank-based,
O(1) amortized) is 1A's *distinctive rightness* — it alone would carry polymorphism, the false-fusion
guard the banked HM(X) names. CSP is inverted to a bounded post-solve for widths/overloads only.

**Mechanism / seams.** Displace skinny's monotone fold at `passes/src/lib.rs:204` (the `->` discard)
with `classify_arrow` + `unify` into the rule's nominal var; grow `Ty` to the algebra above; port
into greenfield and reconcile-with / displace the running `project_types` CSP. New `TypeDesc::{Row,
Sum}` arms lower through `TypeAnalysis::from_ir` + `named_types.rs` and mirror through
`type_desc_to_ts`.

**Gates it would carry.** G1 nominal-fold terminates + parity-matches the running pass on JSON/Sheets/
CSS L4 at ≤ derive wall-time (baseline = the running pass, not a strawman). G2 Row-without-hint
(twitter→closed Row, generic json→Map, array→List, one run). G3 let-gen sharing (a reused helper
instantiates to two distinct monotypes). G4 width post-solve (defaulting vs search on an
under-determined fixture). G5 lower through the sink + TS mirror. G6 port cost (LOC-deleted + derive
wall-time + runtime `git diff` empty). G7 the `HeterogeneousAltJoin` obligation *retires* under
unification rather than coexisting.

**Exact open questions.**
- **[categorical iso failure — SHARPEST]** A fused monotype is not a `TypeDesc` value → it has **no
  `type_desc_to_ts` path at all** (PROTO-2-iso, HEAD-real). Choosing 1A requires authoring a
  brand-new `*_to_ts` sink from scratch. 1A ranks **last on iso**. Constitution iii is LOCK-6's
  verdict — LOCK-1 banks this as a fact, cannot seat the fork on it alone.
- **[carrier UNRUN]** Option A (union-find re-seed) was sized from source (deletes ~101 LOC,
  re-invents the `BoxedEnum`→structural refinement lattice inside union-find), never built. G1's
  parity race is SPEC_ONLY. Its sole displacement justification is doubly dead.
- **[row unmeasurable]** Zero row-shaped `->` in the real corpus (json's four arrows are all scalar
  leaf projections) — Leijen vs Gaster–Jones (Wand's trap) cannot be exercised; behind GN-3.W3.
- G7 (obligation-retirement) unproven; G3/G4 asserted.

**Not BLOCKED-candidate** — the missing primitives (row unifier, level generalization) are settled
25-year prior art, not problems equal to the original. But it is LEAN-against: categorical iso loss +
displacement cost + zero direct in-tree prototype.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · ADVANCED (carrier cand. #2, LEADING) · 54% · single-store BLOCKED-candidate

**Architecture.** Every judgment (structural equality, numeric width, decode class, overload
legality, `List ≠ Row`) is one constraint system over the vendored `csp-solver`; unification is
equality propagation inside the running store; a bounded post-solve resolves only the width residue.
The claim is *recognition*, not invention: the store already exists and runs
(`passes/types/mod.rs:51`, `Csp<TypeDomain>` + AC-3 `propagate()` to a monotone fixed point), wired
to both LOCK-3 sinks and the TS mirror. Widen the variable to a product lattice over disjoint axes
(width = total order, `join=max`; decode/legality = small lattices/bitsets); harden the disagreement
policy additively.

**Mechanism / seams.** B1 split `assign`/`join` by axis: width takes `max` and continues; structural
disagreement records `TypeObligation::TypeClash{node,lhs,rhs}` beside `UnresolvedCompoundRef`/
`HeterogeneousAltJoin`, reusing the `ObligationSink` (`obligation.rs:182`) drained at `mod.rs:486`.
B4 the sum node's domain must be *product-shaped per variant* (a `Sum[decode₀,…]` vector, not a flat
cell) and the variant→decode dependency must be an `ImplicationConstraint` in the same worklist
(read at the fixpoint, not mid-sweep — changed-flag-convergence discipline). Open R adds
`TypeDesc::{Row,Sum}` and a store propagator deriving `Row` from a repeat-of-pairs body.

**Gates it would carry.** B1 lands + `types_heterogeneous_alt` stays green + runtime `git diff`
empty. B2 head-to-head vs Option A (derive wall-time + LOC-deleted + runtime-diff); the store must
prove monotone convergence (no oscillation). B3 product-domain census on a *modeling* instrument
(overload/legality/decode-class) with pre-registered gates. B4 both ways (flat unsound cell vs
per-variant cells).

**Exact open questions.**
- **[X8 unmet ×2 — FATAL, carrier-INDEPENDENT]** The additive `assign` hardening — the one deliverable
  both critics + the registry call "ships regardless of carrier" — is still raw LWW at HEAD. The
  refinement-aware form is required (refuse concrete-vs-concrete, *permit* BoxedEnum→structural
  refinement — the naive refuse-all freezes the corpus's refinements). Until it is HEAD-reproducible,
  no carrier verdict is MEASURED and 1B has shipped nothing. **(Brief 2.)**
- **[vacuous-convergence — SHARPEST NEW]** 1B does **not** ship the banked HM(X): the running CSP has
  zero let-generalization; no 1B gate tests false fusion; whether `RefConstraint` suspension is
  observationally equivalent to instantiate-fresh-per-`Ref` is asserted, never shown. The "net-0
  cheapest diff" premise omits an unbuilt let-gen graft into the monotone `propagate()` sweep.
  **(Brief 1.)**
- **[consumer-less substrate, viii]** B1's `TypeClash` is parity-inert (codegen reads `ir.types` +
  `struct_registry`, never `type_obligations`); its only consumer is 1C's unbuilt LSP bridge. "Ships
  regardless" and "consumer-less until 1C lands" cannot both hold unscheduled — **name the joint wave.**
- **[X3 double-home — the single-store thesis is BLOCKED-candidate]** The literal one-heterogeneous
  store must merge LOCK-1's `Csp<TypeDomain>` with LOCK-2's `Csp<StrategyDomain>` (`csp_strategy/`,
  same vendored `csp-solver`) — a GESTALT-B consolidation 1B cannot make alone; the missing primitive
  (a cross-lock store merge) is of a different order than the judgment it carries. **Bank the literal
  thesis; ship the federation** (which is HM(X) = 1A's architecture — 1B's residual distinctiveness
  collapses to "cheapest diff," itself contingent on the unbuilt let-gen graft).
- B3 census + B4 store-shape unbuilt across two passes ("residue ≈ 0" reads the pass-1 census's own
  coarse output over a corpus with zero overload code — spec-cites-itself).

**Iso advance (HEAD-real, banked):** 1B is the **only carrier that already crosses to TS** —
`HeterogeneousAltJoin([Row,Row]) → type_desc_to_ts` (Sum-crossing HEAD-real; Row-crossing SPEC_ONLY
pending the Row vocabulary landing).

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · BANKED (diagnostics discipline, reactivation-gated) · 40%

**Architecture.** Not a fourth engine — a strict superset of the winning trunk that degenerates to
it where nothing is declared. Two judgments over the existing IR: `synth` where no annotation
constrains, `check` at every `->` ascription (body checked against the declared `Ty`, the projection
node synthesizes `Ty` outward). Rule-local check/synth; global fixpoint only at cross-rule `Ref`
seams. The design center is *where a failure is reported*: a declared projection the body cannot
satisfy names the `->` span, never a synthesized interior. Reuses PROVE-08's `classify_arrow`
(ascription-checks / bare-synthesizes) promoted from a one-shot lexer into the pass's control mode →
closest kin to the trunk (shared reconciler).

**Mechanism / seams.** Two net-new data: (1) a `span` field on the type obligation
(`obligation.rs:56`), sourced from the failing node's `NodeId → RuleMeta.source_span`; (2) a
check-mode arm at the `Map`/`Project` node — unify the body's synthesized type against the declared
`τ`, emit `ProjectionMismatch{span, declared, synthesized}` on clash rather than silently grounding.
Skinny mirror: populate the already-declared-but-empty `TypeObligation.span`
(`skinny/…/lib.rs:138-144`), closing the standing Y.13 consumer-less-substrate violation.

**Gates it would carry.** R-C2 (make-or-break): on a hand-authored declared/synthesized-*mismatch*
`->` fixture, `check` localizes at the `->` span where `synth` reports at a synthesized interior —
side by side, expected-vs-actual. Additive-span landing: diff purely additive, full corpus green,
runtime `git diff` empty. Record-only census (de-risk R-C2): count current mismatches per corpus
before any fail-closed flip.

**Exact open questions.**
- **[make-or-break has NO input — structurally unrunnable]** The greenfield json's four arrows are all
  scalar leaf projections — none a mismatch. R-C2 needs a hand-authored mismatch fixture
  (`frac = digits "." digits -> Ratio`, body synthesizes a two-`Span` `Seq`). 1C's distinct identity
  is *unfalsifiable* until that fixture (or GN-3.W3) lands. **A bet, not a finding.**
- **[GN-3.W3 wall]** The natural inline `(string -> key)` trips the typed-materialization invariant
  (`wrap.rs:242`); the rule-level `: key` form clears it (PROTO-1 §4, RAN). 1C's 40% is a ceiling
  until the inline thread is built.
- **[consumer-less, viii]** The `type_obligations → lsp_types::Diagnostic{range}` bridge is unbuilt —
  the *same* bridge 1B's B1 depends on. **The banked wave is STILL UNNAMED** (risks falling between
  carrier-selection and GESTALT-B).
- **[sub-span ceiling — a LOCK-3 field]** `Project.ty` is one whole-expr `String` span; a wrong
  ascription cannot underline the `T`.

**Reactivation gate:** R-C2 lands and shows the check moves the span → 1C earns a name as the winner's
diagnostics discipline; else it stays a footnote on the trunk.

---

## FAMILY 1D — TYPES-AS-ECLASSES · BANKED · 43% · structural-μ UNPROVEN (not "dissolved"); recursion-keep + "zero new engine" + cost-extraction RETIRED

**Architecture.** The type lattice lives in the in-tree `crates/egraph` (hash-consed congruence
closure = union-find + rebuild); a type var is a fresh e-class, `unify = union`, `substitution = find`.
A `TypeNode` `Language` (derive-projected) carries width/decode leaves, `Row`/`Sum` (order-canonical),
`Tuple` (positional). An `Analysis<TypeNode>` `KeyShape` lattice classifies record-vs-list by
inference (closed literal keys → `Row`; homogeneous key regex → `Map`; conflict → the hard point).
AC-`Sum` is canonical-by-construction (sort children before hash-cons — no rewrite, no n! blowup);
positional `Tuple` stays distinct. Cost-model extraction is the pluggable residue seam (Open X).

**What RETIRED (named negatives, reconfirmed).** Recursion-keep: the running CSP is
finite+terminating on JSON/Sheets/CSS L4 (probe2) — native cyclicity beats only the unbuilt strawman.
"Zero new engine": FALSIFIED — the `union` scheduling amendment is reverted at HEAD and fires on
LOCK-2 grammar-tier + LOCK-5 regex-tier (`egraph/rules/regex.rs`) unions — a **3-lock blast radius**,
a GESTALT-B call. Cost-extraction: IDLE — overload/cost residue = 0 (X4-caveated).

**The one surviving hook (structural `List ≠ Row`) is UNPROVEN and BLOCKED-candidate.**
- **[make-or-break UNBUILT]** On the *real* json `object` body the classifier returns `Unknown`
  (PROTO-2, RAN). Deriving `Row` without a `->`-declared `key` role is unshown; extraction (handed a
  `Row` node → finite Sum) ≠ inference (deriving the node). The `key` role IS the workaround's
  content relocated into the type layer — a missing primitive equal in difficulty to the original.
- **[the divorce refutes "dissolve"]** PROTO-2 ("Open R DISSOLVES 1D") ⊥ PROTO-1 ("BLOCKED"); the tree
  adjudicates for PROTO-1 (PROTO-2's own `git diff skinny/` = empty — it never touched, never *could*
  touch, the mask). The structural axis is blocked behind **GN-1 codepath unification** (a LOCK-7⊕
  LOCK-3 totality build against the tree-of-record), strictly larger than "vocabulary + who-declares-
  key." **Re-homed to GN-1/GESTALT-B; NOT a LOCK-1 recursion race.**
- **[engine-agnostic]** `List ≠ Row` is absent because `LayoutKind`/`TypeDesc` lack the vocabulary
  (a LOCK-3 record-expressiveness gap, GN-3 wall-b) — adding it lets *any* engine populate it,
  including the running CSP. 1D's sole advantage is cosmetic relative to "widen the record + let the
  running CSP fill it."

**Exact open questions.** name-collision (`TypeENode` net-new `TypeAnalysis` vs the live LOCK-3 driver
struct); no TS path (`TypeENode` is not a `TypeDesc` → no `type_desc_to_ts`, ranks last on iso, tied
1A); AC-sum + injectivity coexistence unbuilt (tested *around* via separate e-graphs, never *through*
one saturation); the `union` amendment's 3-tier saturation perf unmeasured.

**LOCK-1 residue only:** the engine-agnostic `TypeDesc::Row` vocabulary widening (a LOCK-3 fix, X4).
Reactivate 1D only if the structural-μ hook is proven NEEDED *and* 1D-specific — which the GN-1 loom
decides, not a LOCK-1 probe.

---

## THE INCOMPATIBILITY MAP — which families cannot coexist, and precisely why

The four families are **not four engines**; they collapse to a **two-way equality-carrier fork + one
banked discipline + one banked engine**, all aiming at the same banked HM(X) target.

**1A ⊥ 1B — the equality-carrier fork (the ONE live incompatibility).** Both ship HM(X); they differ
only in the *equality carrier*, and the two carriers cannot both seat:
- **1A** re-seeds equality from a union-find core, **deleting** the running `project_types` CSP
  (~101 LOC + the refinement lattice re-invented). Its displacement justification (running CSP
  "unsound") is **doubly dead**. It is **categorically last on iso** (a monotype is not a `TypeDesc`
  → no `type_desc_to_ts`; a brand-new sink must be authored).
- **1B** **hardens** the running CSP (additive; keeps both backend consumers; Sum-crossing to TS is
  HEAD-real). It **does not ship let-gen** (the polymorphism layer of the banked HM(X)).
- The fork has **two decision axes**: *build-cost* (LEAN-1B: additive net-0 vs delete-and-reinvent)
  and *iso* (1B pre-eliminates 1A categorically — but iso is LOCK-6's verdict to render). **Pass 3
  must declare which axis is dispositive before building:** if iso governs, the seat is nearly closed
  FOR 1B and Option A need not be built; if build-cost governs, build Option A and race parity. *Do
  not demand both briefs without a dispositive one.* **Precondition to both:** whether let-gen grafts
  additively into the running CSP (1B keeps its cheapest-diff title) or forces the union-find re-seed
  (the fork tilts to 1A) — Brief 1 decides this, and thereby whether the fork is even real.

**1C ∥ trunk — COMPATIBLE overlay (not an incompatibility).** 1C is a checking front + span field over
the winning carrier (1A or hardened-1B); the graft is named — `infer_expr → synth_expr`, `check_expr`
overlays, the trunk's equality reconciler shared (equality-unify for non-numeric leaves; subtype-join
at the numeric seam). It reactivates on R-C2; it never competes for the carrier seat.

**1D ⊥ everything on the STRUCTURAL axis — but re-homed out of the fork.** 1D's only live claim
(structural `List ≠ Row` by inference) is (a) BLOCKED-candidate (needs the `key`-role hint = the
workaround relocated), (b) engine-agnostic (any carrier fills a widened `TypeDesc::Row`), and (c)
blocked behind GN-1 codepath unification (the mask is on the divorced retiring workspace). It is **not
incompatible with the carrier fork** — it is *orthogonal and re-homed to GESTALT-B*. Its `union`
amendment is separately incompatible with LOCK-2/LOCK-5 saturation (3-lock blast radius) and must not
re-enter LOCK-1 as a free hook.

**Two double-homes that no single family can settle (routed to GESTALT-B):**
1. **Open X (residue defaulting-lattice) ⊕ LOCK-2 `csp_strategy` CSP** — same vendored `csp-solver`,
   two federated stores. Is width-defaulting LOCK-1's residue seam or LOCK-2's decision driver?
2. **The iso-carrier seat ⊕ LOCK-6** — Constitution iii is LOCK-6's verdict; LOCK-1 banks the
   HEAD-real negative (only `TypeDesc` crosses) as a fact but cannot seat the carrier on iso alone.

**Named supersessions owed (Constitution ix):** GN-3 §1.5 ("no CSP/e-graph consumer exists") is
factually overridden — formalize with LOCK-7 sign-off (recorded WORKING in XL-F(1)). The structural
axis re-homed to GN-1 must be a *named* supersession, not a silent XL-A contradiction.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs — returned via StructuredOutput)

1. **LET-GEN-FALSE-FUSION** (bbnf-worktree) — does the running CSP already keep a reused helper's two
   uses distinct, or does it conflate them (false fusion)? Decides whether 1B ships the banked HM(X)
   additively or the fork tilts to 1A's re-seed; resolves the vacuous-convergence hit (X10) that the
   whole lock's floors rest on.
2. **ASSIGN-CLASH-ARM** (bbnf-worktree) — land the refinement-aware `assign` clash arm in-tree and
   prove `types_heterogeneous_alt` stays green with runtime `git diff` empty. The carrier-INDEPENDENT
   deliverable stalled two passes (X8); converts SPEC_ONLY → MEASURED and unblocks the consumer-less
   knot when co-scheduled with the 1C LSP bridge wave.

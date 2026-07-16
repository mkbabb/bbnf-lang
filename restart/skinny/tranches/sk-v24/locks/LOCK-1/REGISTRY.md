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
  `TypeDomain: LatticeDomain`, `HeterogeneousAltJoin` live at `crates/ir/src/types/type_desc.rs:63`)
  and the shipped `crates/egraph`. Confirmed live.

**LINEAGE — RESOLVED, and it inverts the fork's cost accounting.** Project memory
`project_skv24_handoff.md` governs: *"Skinny RETIRES … its code does not graduate wholesale —
every carry re-earns its place."* **Root is the greenfield base; skinny retires.** This inverts
1A/1C's cost: their scaffold sits on the **retiring** tree, so "activate the dormant `TypeFacts`"
becomes "port PROVE-08 + the scaffold into root and reconcile with / displace the running 1B CSP" —
the full 1A⊥1B collision at displacement cost. 1B/1D substrate already runs on the surviving base.

### Anchors re-verified verbatim this pass (agglomerator re-ran every grep at HEAD)

| Fact | Live truth | Bearing |
|---|---|---|
| `->` ty-drop (1A/1C target) | `skinny/…/passes/src/lib.rs:204` `Project { body, .. } => infer_expr(*body)?` | 1A/1C mechanism site, on the **retiring** tree |
| 1B running type system | `HeterogeneousAltJoin` live `type_desc.rs:63`; `TypeDomain: LatticeDomain`; both backends consume | 1B ships on the surviving base — not speculative substrate |
| **`TypeDomain::join` is DEAD CODE** | `grep '\.join(' crates/ir/src/passes/types/` → **zero call sites** (re-confirmed this pass); impl `domain.rs:66-80` only | the "confirmed shipping defect" is unreachable dead code — NOT a live bug |
| **Dead `join` is non-commutative LWW** (run2) | `ground(Bool).join(ground(F64))` → `true`, leaves `Some(F64)`; reverse leaves `Some(Bool)` — last-writer-wins, not a lattice LUB | a real defect *in dead code*; strengthens "the case against 1B was mis-attributed" |
| Live equality carrier | `assign` (`revise.rs:27`) `*slot = Some(ty)`, no clash check — LWW under an **uncapped** monotonic loop | latent hazard (two incompatible *grounds* flip-flop) — **unreachable** under well-formed tree IR |
| **Live clash path is SOUND** (run2, 2nd fixture) | live pass: `ir.types[0] = HeterogeneousAltJoin([Bool, F64])` (NOT a single leaf); `type_obligations = 1`, clash obligation `HeterogeneousAltJoin{rule:Some(0)}` = 1; `is_single_silently_chosen_ground_leaf = false` | 1B is **sound on every reachable surface** — twice-measured now; the "unsound as shipped" verdict is **doubly dead** |
| **List ≠ Row collapse is TOTAL** (probe2) | `array` and `object` share a **byte-identical** `struct_registry` `StructLayout` (kind=Struct, rule_type=Vec(Enum)) — collapse present in the sidecar **codegen actually consumes**, not just `ir.types` | the general pipeline **structurally cannot** name `List ≠ Row` |
| **`LayoutKind` has no Row/Map** | `crates/ir/src/registry/struct.rs:58` → `{Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}` | a **LOCK-3 record-expressiveness** gap (GN-3 wall-b), engine-agnostic — no engine choice closes it; a `Row` variant does |
| **`List ≠ Row` recovered by a masked fallback** | `skinny/crates/codegen/src/json_templates/value.rs` hand-matches raw `{ } [ ]`; dispatched by a hard-coded per-grammar table (`json→regen-json`); shipped `crates/core/src/runtime/json/value.rs` hand-encodes distinct `Array`/`Object` | a **clause-i / `no-workarounds` / `typed-materialization-invariant`** defect the RETIRE verdict quietly rests on |
| **Collapse is SPECIFIC, not general** (probe2) | Sheets `comparison_expr = Tuple(mu, List(Tuple(Str, mu)))` — the running CSP **does** emit structural μ where the body is Seq/Tuple; the collapse is confined to **heterogeneous-Alt-of-compounds** (JSON `value`) | narrows 1D's structural advantage to a **single grammar shape**, not a general capability |
| Running CSP is finite+terminating | probe2: JSON (8 rules, 2 knots), Sheets (37 rules, 20 mutual knots), CSS L4 (191 rules, 44 knots) — Finite=true, terminating=true, **zero stack overflows** | 1D's native-cyclicity "win" beats only the strawman; the incumbent already terminates |
| **Real LOCK-1→LOCK-3 consumer** (CRIT-2 0b) | `crates/core/src/backend/driver/analysis.rs:16` `TypeAnalysis`, `::from_ir` (`:22`, ctor `:221`); a **second** `from_ir` at `rust/view/named_types.rs:100` | **no family names it**; specs cite the backend-leaf consumers and miss the driver seam; a **name collision** for 1D's proposed net-new `TypeAnalysis` |
| **Root already federates TWO `Csp`s** (CRIT-2 0c) | type CSP `passes/types/` **and** LOCK-2 backend-shape CSP `passes/csp_strategy/{constraints,mod}.rs` — both over the vendored `csp-solver` | 1B's literal single-store = a **LOCK-1⊕LOCK-2 store merge**, a GESTALT-B call 1B cannot make alone |
| **`union` amendment = 3-lock blast radius** (CRIT-2 0e) | `egraph.rs:236` `if changed { analysis_pending.push(dst) }`, no `rebuild_pass_count` — **reverted**; shared engine drives LOCK-2 `egraph/{node,build_egraph,rules}` **and** LOCK-5 `egraph/rules/regex.rs` (verified present) | 1D's hook fires on grammar + regex + type unions; "free for no-op analyses" un-measured on the two hottest consumers |
| X4 overload-absence | `crates/ir/src/passes/types/` has **zero** `overload`/`escape`/`legality` code (grep empty) | the census measured an engine that **structurally cannot** exhibit the overload/decode residue the rich thesis rests on |
| Probes non-reproducible from HEAD | `lock1_residue_census.rs`, `type_injectivity.rs`, `lock1_recursion_parity.rs`, `lattices/clash.rs` survive only as `.patch.diff`; the egraph amendment is reverted | a pass-2 reproducibility / land-or-retire obligation |

**The material corrections this pass (re-anchoring onto the newest evidence).**

1. **The stale ledger.** The prior registry draft (05:15) cited critic scores **68/65/55/52** (CRIT-1)
   and **55/55/40/45** (CRIT-2) and folded on **PROTO-2-recursion-parity v1**. Both critics on disk
   are newer — they explicitly consume the **09:39 re-runs** (`PROTO-1B-clash-run2`,
   `PROTO-2-recursion-parity-probe2`) that postdate that fold — and land at **64/63/52/48** (CRIT-1)
   and **56/54/40/44** (CRIT-2). This fold re-anchors on the on-disk critics and the two 09:39 probes.
2. **The "shipping defect" is doubly dead.** run2 independently reproduces the deadness of
   `TypeDomain::join` (0 call sites), adds a non-commutativity proof of that dead arm, and re-measures
   the live clash path **sound** on a second fixture. The specs' main lever for "seed 1A's union-find
   instead of the running CSP" is void — **the carrier fork is more open, not less.**
3. **The recursion-axis RETIRE stands, but the structural-fidelity question re-opened.** probe2 shows
   the `List ≠ Row` collapse is **total in the sidecar codegen consumes** (not just `ir.types`),
   recovered by a **bespoke per-grammar template** — a masked fallback the RETIRE quietly leaned on.
   But two facts **bound the 1D keep**: (a) the gap is **engine-agnostic** (a `LayoutKind::Row` fix any
   engine populates — CRIT-2), and (b) the collapse is **specific to heterogeneous-Alt-of-compounds**
   (Sheets already emits structural μ — probe2). 1D's disposition is **CONTESTED**, not clean RETIRE.

---

## CURRENT DISPOSITIONS (pass-1, re-anchored fold)

| Family | Status | Convergence | One-line |
|---|---|---|---|
| **1A · UNIFICATION-FIRST** | **ADVANCED** (prior-art-endorsed trunk; unproven in tree) | **56%** | The endorsed HM(X) spine. But it **fuses** a well-evidenced *architecture* with an unproven *union-find carrier* whose justifying defect (CSP "unsound as shipped") is **doubly dead** (run2); **zero direct prototype**; it must feed a **LOCK-3 consumer (`TypeAnalysis::from_ir`) no spec names** and show a TS mirror. |
| **1B · ONE-CONSTRAINT-STORE** | **ADVANCED** (running system-of-record; live path SOUND) — single-store thesis **BLOCKED-candidate** | **54%** | The **only family that runs on the surviving base**, twice-proven **sound on every reachable clash** (run2). Its "unsound" indictment is refuted; it may be the *cheaper* spine (harden, don't delete). But its **distinguishing** thesis (one heterogeneous store) is **BLOCKED**: literally it is a **LOCK-1⊕LOCK-2 store merge** (root already federates two `Csp`s). Census (X3/X4) cannot judge it. |
| **1C · BIDIRECTIONAL-LOCAL** | **BANKED** (the winner's diagnostics-and-locality discipline) | **40%** | Not a fourth engine — a checking front + SCC seam + span field over the winning reconciler (reuses PROVE-08's `Unifier` → closest kin to 1A). Make-or-break head-to-head (R-C2) **structurally unrunnable** (zero `->` corpus, no typed grammars); span-add owed against **root**. A bet, not a finding. |
| **1D · TYPES-AS-ECLASSES** | **BANKED** (recursion-keep + "zero new engine" + cost-extraction RETIRED; structural-μ hook survives, **CONTESTED**) | **44%** | Only ran-and-measured family. Recursion-keep **RETIRED** vs the finite+terminating incumbent; cost-extraction **IDLE** (overload = 0). The `List ≠ Row` gap it targets is **REAL** (masked fallback confirmed) but **engine-agnostic** (a `LayoutKind::Row` fix, not unique to 1D) and **specific** to one grammar shape; its true make-or-break — **deriving Row by *inference*** without a per-grammar hint — is **UNBUILT** and BLOCKED-candidate on structure. `union` amendment a **3-lock** blast radius; `TypeAnalysis` name-collision; no TS path. |

**Floor rule applied.** Floors = **min across the two current post-probe critics** (on-disk
CRIT-1 64/63/52/48, CRIT-2 56/54/40/44), taken **unadjusted**:
`1A min(64,56)=56 · 1B min(63,54)=54 · 1C min(52,40)=40 · 1D min(48,44)=44`.
The agglomerator **re-verified at HEAD every load-bearing fact both critics rest on** — dead-code
`TypeDomain::join` (0 sites), the live `assign` carrier, `egraph.rs:236` guard present +
`rebuild_pass_count` absent, `LayoutKind` has no Row (struct.rs:58), `TypeAnalysis::from_ir` live +
a second `from_ir`, `csp_strategy/` second Csp, `egraph/rules/regex.rs` present, `json_templates`
masked fallback, skinny `:204`. **The critics did not err on any load-bearing fact; no floor moves.**
CRIT-2 is the binding floor on all four families.

> **Movement vs the stale draft (52/58/38/48 → 55/55/40/45 → now 56/54/40/44).** The newest
> critics nudge **1A up** (CRIT-2 55→56: architecture endorsed, carrier lever dead but not fatal),
> **1B down** (CRIT-2 55→54: the single-store thesis is a *cross-lock* double-home, worse than the
> intra-lock "1A in a coat" framing), and **1D down** (CRIT-2 45→44: probe2's masked fallback + the
> engine-agnostic gap). The 1A/1B gap (56 vs 54) is **within noise — the carrier fork stays open.**

---

## FAMILY 1A — UNIFICATION-FIRST · ADVANCED · 56%

**What is genuine (verified).** HM(X)/OutsideIn(X) stratification is the field's settled answer
(prior-art §2A/§5): generate equality by unification, defer the theory residue to a bounded solver.
The union-find leaf core is proven (PROVE-08); the dormant `TypeFacts` scaffold really exists; the
`:204` discard is verbatim. probe2 **indirectly evidences** its recursion story: the running pass
already closes recursion **nominally** and terminates finitely on JSON, Sheets (mutual), CSS L4 —
a nominal fold, 1A's own mechanism, is empirically proven to work in tree.

**The conflation both critics isolate.** "1A ADVANCE (spine)" fuses (i) *1A-the-architecture* —
union-find trunk + bounded residue + let-generalization — prior-art-endorsed and incumbent-backed;
with (ii) *1A-the-union-find-carrier* — deletes the running root CSP, re-seeds equality from PROVE-08
`hm.rs`. (i) is well-evidenced; (ii) is an in-scope migration whose justification is now doubly dead.

**Open gaps.**
- **G-1A-1 [lineage inverts the cost — scaffold is on the RETIRING tree].** "Activate the scaffold"
  becomes "port PROVE-08 + `TypeFacts` into root and reconcile with / displace the running 1B CSP" —
  the full 1A⊥1B collision at displacement cost. Unmeasured.
- **G-1A-2 [the carrier-displacement argument is DOUBLY dead].** The prior lever ("the running CSP is
  overwrite-unsound as shipped") is dead code, now re-confirmed by run2 (deadness + non-commutativity
  of the dead arm) and refuted by run2's second sound-live-path fixture. The residual case for
  union-find-as-carrier is prior-art familiarity + "raises clashes" — which the running Alt-join
  **also** does. **The carrier fork is unresolved and 1A's tilt is unearned.**
- **G-1A-3 [no DIRECT 1A prototype — every gate (G1–G6) asserted].** The nominal-fold termination is
  evidenced only **second-hand** via the incumbent (probe2), never via 1A-union-find code. SPEC_ONLY
  on its own (Constitution vii).
- **G-1A-4 [cross-lock — 1A does not name its LOCK-3 consumer].** It threads `->` into
  `DirectTypeKind::Sum`/`DirectScalar` but never names `backend/driver/analysis.rs::TypeAnalysis::from_ir`
  (the real sink) nor `backend/ts/projection.rs` (LOCK-6). A fused monotype must lower to **both**; the
  emit half is asserted "genre-standard," never shown reaching the driver `TypeAnalysis` the incumbent
  `TypeDesc` already feeds.
- **G-1A-5 [numeric-width residue UNMEASURED, not resolved].** PROTO-1's `width` tag is Seq/tuple
  *arity*; numeric width routes to `decode-class`, pre-tagged single-valued (X4). "Defaulting dominates"
  is plausible and prior-art-consonant, but **not established on the pass's own instrument**.
- **G-1A-6 [row discipline unpicked + unmeasurable].** Leijen vs Gaster–Jones, principal-type-critical
  (Wand's trap), **zero `->` corpus** to decide it. Sequences behind GN-3.W3.
- **G-1A-7 [generalization criterion + zero-runtime asserted, not exercised].** Value-restriction unwritten
  (low-risk — rules are pure schema producers); no artifact drove `git diff crates/runtime/` empty or the
  second type system deleted.

**Pass-2 directives.**
1. **Build the 1A nominal-fold recursion prototype and race it against the running CSP** (not the
   strawman). probe2 shows the incumbent terminates finitely; the open question is whether a union-find
   re-seed matches it at equal-or-lower cost, or merely re-derives what already ships.
2. **Cost the lineage port explicitly** (G-1A-1): measure `git diff crates/runtime/` + derive wall-time +
   LOC-deleted for the union-find seed vs the running-CSP seed head-to-head, and **name the LOCK-3 sink**
   (`TypeAnalysis::from_ir`) + the TS mirror the fused monotype must reach.
3. **Build the numeric-width probe the census skipped** (X4) — a hand-authored `->` micro-fixture
   genuinely under-determined between u8/u16/u32, measured *defaulting* (ordered lattice) vs *search*.
4. Pick and state the row discipline; write the generalization criterion.

**Withheld from researchers:** do **not** re-derive HM(X) stratification (settled, endorsed); do **not**
re-argue union-find dominates equality asymptotically (settled, prior-art §2B). Build the recursion race,
the width probe, the port cost, and the named LOCK-3/TS lowering — argue nothing more on paper.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · ADVANCED · 54% · single-store thesis BLOCKED-candidate

**What is genuine (verified at source).** 1B is the **only family that runs on the surviving base**,
and run2 **strengthened it a second time**: the live equality path is **sound on every reachable
clash** — two incompatible `->` arrows (`Bool` vs `F64`) yield `ir.types[0] = HeterogeneousAltJoin([Bool,
F64])` + one named obligation (`is_single_silently_chosen_ground_leaf = false`). The "confirmed shipping
defect" is dead code with 0 call sites, now with a non-commutativity proof that it is genuinely broken —
*in code the pass never reaches.*

**Open gaps.**
- **G-1B-1 [the "unsound as shipped" indictment is DOUBLY REFUTED — a latent hazard remains].** The
  real carrier `assign` (revise.rs:27) is LWW under an **uncapped** loop — two incompatible *grounds*
  would flip-flop forever — but **unreachable** for clashes (the only reachable clash site, Alt join,
  raises). **Gap: land the additive hardening** — on `Some(existing) != Some(ty)` record
  `TypeObligation::TypeClash` and refuse the overwrite. Cheap, ships regardless of carrier. **Not shown
  backward-safe** (G-1B-5).
- **G-1B-2 [cross-lock, the sharper wall — single-store = LOCK-1⊕LOCK-2 merge].** Root **already
  federates** the type CSP (`passes/types/`) and the backend-shape CSP (`passes/csp_strategy/`) over the
  same vendored solver. The literal single heterogeneous store must **merge LOCK-1's store with LOCK-2's**
  — a GESTALT-B cross-lock consolidation 1B cannot make alone. This is **strictly worse** than the prior
  "federation IS 1A in a coat" framing (which located the collapse inside LOCK-1). **The single-store
  thesis is BLOCKED-candidate** — bigger than R-B1's per-revise tag dispatch.
- **G-1B-3 [the "collapse to 1A" retirement is UNSOUND as argued — X3/X4].** Both PROTO-1 pre-registered
  gates **FAIL** on all four corpora (eq+sum 82–89% < 95%; structural-non-eq 18/1020/53/87 ≫ 2); "THIN —
  decisive" is reached by a **post-hoc third gate**. The census measures 1B's *own* pass, which has
  **zero overload/legality code** (X4) and per probe2 **cannot even represent `List ≠ Row`** — an
  instrument structurally blind to the rich residue it purports to refute, reading its own coarse output
  (spec-cites-itself circularity).
- **G-1B-4 [the carrier inversion is live — the pass's most important unadjudicated question].** With
  §0's lever removed, **1B-as-running-CSP-hardened** is a live candidate for the carrier seat itself: it
  keeps both backend consumers, has a sound live clash path, needs only additive hardening. The spec's
  "federation IS 1A wearing a CSP coat" may have it backwards — the cheapest sound spine may be *the
  running CSP hardened*, not a union-find re-seed that **deletes running code** (1A's G-1A-1 cost).
- **G-1B-5 [`assign` hardening not shown backward-safe].** Refusing the overwrite touches code asserted
  by `types_heterogeneous_alt`; no probe confirmed the corpus still solves once overwrite is refused.
- **G-1B-6 [R-B1 store-shape UNBUILT].** The decode-class-depends-on-sum-variant counterexample the
  research demanded be built both ways was built **neither** way. The family's sharpest design decision
  is on paper.

**Pass-2 directives.**
1. **Land the additive `assign`/`join` raise-on-disagreement hardening** (record `TypeClash`, refuse the
   overwrite) — converts the latent oscillation hazard into an explicit obligation. **Confirm the corpus
   still solves** (`types_heterogeneous_alt` green). Ships regardless of carrier winner.
2. **Adjudicate the carrier inversion (G-1B-4):** measure "running-CSP hardened" vs "union-find re-seed"
   head-to-head on derive wall-time + `git diff crates/runtime/` + LOC-deleted. The prior tilt to
   union-find rested on a dead-code defect.
3. **Re-run the residue census on an instrument that MODELS overload/legality AND can express `List ≠
   Row`** (X4), honoring the registered gate or re-cutting it **in advance** (X3). The census cannot
   refute a rich-residue thesis about judgments its instrument does not implement.
4. **Build R-B1 both ways** on the decode-class-depends-on-sum-variant counterexample (G-1B-6); if the
   single-store is pursued, **cost the LOCK-1⊕LOCK-2 store merge** (G-1B-2) as an explicit cross-lock
   consolidation, or bank the literal thesis and keep the federation.

**Withheld from researchers:** do **not** re-assert "1B collapses to 1A" on the current census (X3/X4
unsound); do **not** propose new csp-solver *engine* features (vendor sync gate) — stay consumer-side
(`crates/ir/src/passes/`) adding *domains*, per the `ThreadSafe` precedent.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · BANKED · 40%

**Why BANKED (redirect / crowding).** 1C is **not a fourth engine — a strict superset of 1A that
degenerates to 1A's synthesis where there are no annotations** (R-C2, self-admitted), reuses PROVE-08's
`Unifier` as its subsumption reconciler → per the shared-mechanism rule, closest kin to 1A. Its natural
form is one family: an equality trunk with a bidirectional checking + diagnostics layer. **The
standalone-fourth-engine formulation is BANKED;** bidirectional check/synthesize survives **subsumed as
the winner's diagnostics-and-locality discipline** (the named graft below). It reactivates to ADVANCED
only when R-C2 proves the layer earns a name.

**Open gaps.**
- **G-1C-1 [the make-or-break head-to-head (R-C2) is STRUCTURALLY UNRUNNABLE — never run across all
  brief-cycles].** *Does `check_expr` localize `TypeError.span` at the `Project` span where unify-first
  reports at a synthesized interior?* There is no input: **zero `->` corpus**, twitter/citm/canada typed
  grammars absent. 1C's distinct identity is **unfalsifiable today**, not merely un-run. The disposition
  is a bet.
- **G-1C-2 [on zero-`->` grammars 1C ≡ its host engine].** PROTO-1's ≤2% hetero-residue even on CSS L4
  bounds the addressable surface further; it is small *and* empty.
- **G-1C-3 [cross-lock — substrate on the RETIRING tree; span-add owed against ROOT].** Every seam
  (`synth`/`check` split, `RuleSig`, the `Project` mode-switch, the `PassDiagnostic` span-add) lives in
  **skinny**; on root the diagnostics type differs. `PassDiagnostic` is `RuleId`-only; a
  `span: Option<SourceSpan>` add is owed against the **root** base — additive, untouched, bank regardless.
- **G-1C-4 [sub-span granularity ceiling — a LOCK-3 field].** `Project.ty` is one `String` carrying
  whole-expr span; a wrong ascription cannot underline the `T`. The quality ceiling on 1C's entire thesis
  is a sub-span the IR does not carry.

**Pass-2 directives (under the banked banner; reactivate to ADVANCED only if R-C2 lands).**
1. **Run the R-C2 diagnostic head-to-head** — the single experiment that decides whether 1C is a named
   discipline or a footnote on 1A. Needs a typed `->` grammar authored first (blocks on GN-3.W3 or a
   hand-authored micro-fixture).
2. **Land the `PassDiagnostic.span` add against the ROOT base** (not the retiring skinny type), and
   specify the `Project` sub-span enrichment — additive, owed regardless of winner.

**Withheld from researchers:** do **not** re-argue 1C as a distinct fourth engine (BANKED); do **not**
develop the width seam as a *separate* subsystem — it is the same width residue 1A/1B own; 1C only
*consumes* the resolved width at the subsumption site.

---

## FAMILY 1D — TYPES-AS-ECLASSES · BANKED · 44% · recursion-keep RETIRED; structural-μ hook CONTESTED

**The disposition MOVED this pass — but not to a clean verdict.** Three probes now bear on 1D:
PROTO-2 (strawman → keep), PROTO-2-recursion-parity **v1** (incumbent → retire), and
PROTO-2-recursion-parity **probe2** (incumbent + sidecar → **CONTESTED**). The registry now folds
probe2, which the prior draft never consumed.

- **Recursion-keep — RETIRED (reconfirmed).** probe2: the running CSP is **finite + terminating** on
  JSON (8 rules, 2 knots), Sheets (37 rules, 20 mutual knots), CSS L4 (191 rules, 44 knots), zero
  overflow. 1D's finer μ-sum beats only the **unbuilt** strawman; against the running system its native
  cyclicity buys nothing on the divergence axis.
- **Cost-extraction — IDLE (retired on the evidence, X4-caveated).** Overload/cost residue = 0
  everywhere; the extractor has nothing to rank. (The thin-residue leg rests on the X3/X4-flawed census,
  so this is *plausible, not airtight.*)
- **"Zero new engine" — FALSIFIED + reverted at HEAD.** `egraph.rs:236` carries the old `if changed`
  guard; `rebuild_pass_count` absent. The injectivity hook needs a **shared-engine** scheduling change —
  a **3-lock** blast radius: it fires on LOCK-2 grammar-tier (`egraph/{node,build_egraph,rules}`) **and**
  LOCK-5 regex-tier (`egraph/rules/regex.rs`, verified present) unions, not only type merges.

**The ONE surviving hook is now DOUBLY challenged — the honest verdict is CONTESTED, not clean KEEP.**
1D is the only path that would yield a self-contained **structural** recursive TypeDesc at the rule level
(`List ≠ Row`, structural back-edges), and probe2 confirms the gap is **real and recovered by a masked
fallback**: the `array ≡ object` collapse is **total in the `struct_registry` sidecar codegen consumes**,
and `List ≠ Row` is restored only by a **bespoke per-grammar hand-written codegen template**
(`json_templates/value.rs`) dispatched by a hard-coded table — a `no-workarounds` /
`typed-materialization-invariant` / clause-i defect the RETIRE quietly leaned on. **But two facts bound
the keep:**
- **[CRIT-1] The make-or-break is Row *inference*, not extraction — and it is UNBUILT.** Every probe
  proved 1D's *extraction* (handed a `Row` node → finite `Sum[6]` with `List ≠ Row`). **No probe shows
  1D's type Language can *derive* that the `object` rule body (repeat-of-kv-pairs) is a `Row` rather than
  a `List` WITHOUT the same semantic hint the bespoke template encodes by matching `{ }` vs `[ ]`.** That
  classification **is** the workaround's content. If 1D needs a per-grammar hint, it *relocates* the
  workaround into the type Language, it does not *close* it → **BLOCKED-candidate on the structural axis.**
- **[CRIT-2] The gap is ENGINE-AGNOSTIC.** `List ≠ Row` is absent because `LayoutKind` has no Row variant
  and `DirectTypeKind` has one `Struct` (a **LOCK-3** record-expressiveness gap, GN-3 wall-b). Adding
  `LayoutKind::Row` / `DirectTypeKind::{Sum,Row}` lets **any** engine — including the running CSP —
  populate it. "1D derives `List ≠ Row`" reduces to "1D **+ a widened `LayoutKind`** derives it," and the
  actual missing primitive (the record can *hold* the distinction) is **not an e-graph capability**. **1D's
  sole surviving advantage is thereby cosmetic** relative to "widen the record + let the running CSP fill it."
- **[probe2] The gap is SPECIFIC, not general.** Sheets `comparison_expr = Tuple(mu, List(Tuple(Str, mu)))`
  — the running CSP **already** emits structural μ where the body is Seq/Tuple. The collapse is confined to
  **heterogeneous-Alt-of-compounds** (JSON `value`). 1D's structural win narrows to a single grammar shape.

**Open gaps.**
- **G-1D-1 [cross-lock — the `union` amendment is a 3-lock blast radius, un-measured on the hot paths].**
  Reverted at HEAD; would fire on grammar-tier + regex-tier unions; "free for no-op analyses" is
  un-measured on LOCK-2/LOCK-5 saturation. Cannot be settled inside LOCK-1.
- **G-1D-2 [cross-lock — name-collision + no TS path].** Net-new `TypeAnalysis` collides with the live
  LOCK-3 driver struct (`backend/driver/analysis.rs:16`, Constitution vi); e-graph-extracted types have
  **no LOCK-6/TS mirror** (only `TypeDesc` feeds `backend/ts/projection.rs`, Constitution iii). 1D ranks
  last on isomorphism.
- **G-1D-3 [the Row-*inference* step is UNBUILT and of the same difficulty as the workaround].** The true
  make-or-break (§ above) was never prototyped. Until it is, 1D's structural keep is BLOCKED-candidate.
- **G-1D-4 [AC-vs-injective coexistence UNBUILT — the elegant-reduction trap].** Positional injectivity is
  **unsound for AC constructors** (`Sum`). PROTO-2 stayed sound by running the two mechanisms in **separate
  e-graphs**; the hard interaction (AC canonicalization + injectivity both live on `Sum` under saturation,
  gated by a flag) is asserted-confluent, tested *around*, never *through*.
- **G-1D-5 [the RETIRE/KEEP is architecture-contingent + cross-lock].** It turns on grammar-authoritative
  (a coarse `TypeDesc` is a wall → 1D closes it) vs codegen-distributed (the template is the sanctioned
  home → 1D is cosmetic) — a LOCK-7/GESTALT-B call. Both probe verdicts presuppose an unadjudicated
  architecture. And the masked fallback (`json_templates`) is a **real clause-i defect** owned jointly
  with LOCK-7, independent of 1D's fate.
- **G-1D-6 [determinism at scale + off-tree proof].** PROTO-2's (a)–(e) rest on the **reverted** `union`
  amendment (SPEC_ONLY masquerading as MEASURED, Constitution vii); byte-identity held only on a 10-class
  toy; 191-rule CSS under `FxHashMap` iteration untested.

**Pass-2 directives (under the banked banner; reactivate only if the structural-μ hook is *needed AND*
1D-specific).**
1. **Build 1D's actual make-or-break: does a `TypeENode` Language DERIVE `Row` from the `object`
   repeat-of-pairs body WITHOUT a per-grammar hint (G-1D-3)?** This, not extraction, decides whether 1D
   *closes* the `json_templates` workaround or *relocates* it. If it needs the hint → BLOCKED-candidate.
2. **[GESTALT-B/LOCK-3] Widen the record engine-agnostically** — add `LayoutKind::Row` /
   `DirectTypeKind::{Sum,Row}` (GN-3 wall-b) and route the type plane through
   `backend/driver/analysis.rs::TypeAnalysis::from_ir`. This closes `List ≠ Row` **without** an e-graph
   and **dissolves 1D's sole surviving advantage into an IR-expressiveness fix.**
3. **[LOCK-7/GESTALT-B] Adjudicate grammar-authoritative vs codegen-distributed** — the real 1D
   keep/retire pivot, and the home of the clause-i masked-fallback defect (`json_templates/value.rs`).
   Record it as a **named totality gap**, not "1D unused capacity."
4. **[GESTALT-B/LOCK-2/LOCK-5] Route the `union` amendment through a three-lock saturation measurement**
   (grammar + regex + type tiers) — never a free LOCK-1 hook; land or retire it in-tree. Build the AC-sum +
   injectivity coexistence *through*, not around (G-1D-4) — only if 1D reactivates.

**Withheld from researchers:** the **"zero new engine" claim is RETIRED** — do not re-pose it; do not
carry the `union()` amendment as a free LOCK-1 hook; do not model width as distinct `Int8`/`Int16`
*constructors* (must be an analysis-lattice fact on one `Int` class); do **not** re-run the strawman
recursion race (retired).

---

## REDIRECT / CROWDING DECISIONS (this pass)

The four families collapse to a **two-way carrier fork + one banked discipline + one banked engine** on
the surviving base (root):

1. **The architecture converged — bank it as CONSENSUS, not as a family.** Both critics + the
   incompatibility map agree 1A and 1B have collapsed onto **one architecture: HM(X) — an equality trunk
   + a bounded residue solver + let-generalization.** Per the redirect rule this is the redundant
   mechanism; it is **BANKED as the settled floor** (do not re-litigate it). **But the redundancy is
   architecture-level, not build-level** — so *neither carrier is force-banked* (named reason below).
2. **The carrier fork stays OPEN — 1A vs 1B, both ADVANCED (56 vs 54, within noise).** The live
   difference is not the architecture but the *equality carrier*: 1A re-seeds union-find (**deleting** the
   running CSP — G-1A-1 displacement cost) vs 1B **hardens** the running CSP (additive, keeps both backend
   consumers). This is a **genuine build-cost fork with different `git diff crates/runtime/` footprints**,
   and the prior tilt to 1A rested on a **doubly-dead defect** (run2). Banking 1B needs a *sound* census
   (present one X3/X4-flawed); banking 1A needs a reason to discard the endorsed spine (there is none).
   **Bank decision deferred to named pass-2 measurement:** the hardened-vs-reseed carrier race (1B dir 2)
   + the overload-modeling census (1B dir 3) + the 1A recursion race (1A dir 1) + the numeric-width probe
   (1A dir 3).
3. **1C → BANKED as a discipline on the winning trunk.** Reactivation gate: R-C2 head-to-head (needs a
   typed `->` grammar).
4. **1D → BANKED; recursion-keep + "zero new engine" + cost-extraction RETIRED as named negatives.** The
   structural-μ hook survives **CONTESTED**, doubly challenged (engine-agnostic + Row-inference unbuilt);
   its disposition is a LOCK-7/GESTALT-B architecture call plus an unbuilt inference probe, not a LOCK-1
   recursion race.

**Underexplored formulations to OPEN (the crowding reveals three):**
- **[NEW this pass] The engine-agnostic record-widening — `LayoutKind::Row` / `DirectTypeKind::{Sum,Row}`.**
  probe2 + both critics converge on this: the `List ≠ Row` capability that 1D claims as its raison d'être
  is a **LOCK-3 record-expressiveness gap any engine closes once the record is widened**, decoupling
  *structural fidelity* from *engine choice*. Open it as a first-class pass-2 formulation — it may make the
  entire engine axis (1A vs 1B vs 1D) orthogonal to the structural question that seemed to justify 1D.
- **The pluggable residue-solver X** (`pluggable-components`). 1A/1B have collapsed onto HM(X); the real
  open design point is **X** — which PROTO-1 says is an *ordered defaulting lattice* (not a CSP, not an
  extractor), but on an X3/X4-flawed census. Open X as a first-class pluggable seam (cost model /
  defaulting order / decode-class registry) so the carrier fork and the residue fork are decided
  independently rather than bundled.
- **The attribute-grammar cross-rule seam** (prior-art §2E, Silver/JastAdd). All three engines re-derive
  the cross-rule seam independently (1A: SCC generalize-after-fixpoint; 1B: federation sequencing; 1C: SCC
  bounded fixpoint). The synthesized/reference/circular-attribute discipline with a termination lattice is
  the deepest whole-system analogue — open it as a pass-2 research thread (a *seam discipline*, not a fifth
  engine).

## CROSS-POLLINATION (the one named, justified graft this pass)

**1C's bidirectional check/synthesize ⟶ the winning trunk (1A or hardened-1B).** Both families are
developed enough to name the graft: the trunk has the equality core (PROVE-08 `Unifier` for 1A, or the
sound Alt-join for 1B) + the `classify_arrow` bidirectional seed already in-tree; 1C has the mode-switch +
SCC discipline mapped onto the *same* `infer_expr`. The graft: `infer_expr` becomes `synth_expr`,
`check_expr` overlays it, and **the trunk's equality reconciler is shared** (equality-unify for
non-numeric leaves; MLsub subtype-join at the numeric seam). This is the incompatibility map's "1C ∥ trunk
compatible overlay." *Deferred graft (not this pass):* 1D's native-cyclic μ-extraction vs a nominal fold —
cannot graft an unbuilt comparison, and the parity probe retired it; re-evaluate only if the architecture
adjudication revives 1D's structural-μ hook.

## CROSS-LOCK / GESTALT DIRECTIVES

1. **X1 LINEAGE — RESOLVED at the SK-V24 level; it inverts the fork cost.** skinny **retires**
   (`skv24-handoff` supersedes SK-V22's graduation); **root is the greenfield base.** 1B/1D substrate
   survives on the base; 1A/1C's skinny scaffold is on the retiring tree → their mechanism becomes a
   **PROVE-08 port into root + reconciliation with / displacement of the running 1B CSP.** GESTALT-B
   ratifies this base call and carries the port cost in 1A/1C accounting. (Corrects CRIT-2's earlier X1,
   which relied on the superseded SK-V22 — a named critic error that moves no floor, since it inverts cost
   *against* 1A/1C.)
2. **X2 [NET-NEW consumer] — the real LOCK-1→LOCK-3 handoff is `TypeAnalysis::from_ir`, and NO family
   names it.** The specs cite the backend-leaf consumers (`backend/rust/ir_types.rs`,
   `backend/ts/projection.rs`) and miss the **driver-level** `crates/core/src/backend/driver/analysis.rs::
   TypeAnalysis::from_ir` (+ a second `from_ir` at `rust/view/named_types.rs:100`) that owns the handoff.
   Whichever record wins must lower through it. 1D's proposed net-new `TypeAnalysis` is a **name collision**
   with this shipping struct (Constitution vi).
3. **X3 [NET-NEW cross-lock] — 1B's single-store is a LOCK-1⊕LOCK-2 store merge.** Root already federates
   the type CSP and the `csp_strategy` CSP; the literal thesis cannot be built inside LOCK-1. Either cost
   the merge as an explicit GESTALT-B consolidation or bank the literal thesis and keep the federation.
4. **X4 — the `List ≠ Row` gap is a LOCK-3 record-expressiveness fix, NOT an engine capability.** Add
   `LayoutKind::Row` / `DirectTypeKind::{Sum,Row}` (GN-3 wall-b); this dissolves 1D's advantage and closes
   a real clause-i masked fallback (`json_templates`). Owned by LOCK-3 + LOCK-7, engine-agnostic.
5. **X5 — record the GN-3 §1.5 override as a NAMED SUPERSESSION (Constitution ix).** FAMILY-SPECS §0 voids
   prior-art C4 / GN-3 §1.5 ("no CSP or e-graph consumer exists") inline as "factually superseded"; GN-3 is
   committed codex — amend it with a formal named supersession + LOCK-7 sign-off, not an inline "factually
   void."
6. **X6 — 1D↔LOCK-2/LOCK-5 is DOUBLE-HOMED (a 3-lock blast radius).** The reverted `union()` amendment sits
   on LOCK-2's `GrammarENode` **and** LOCK-5's `RegexENode` (`egraph/rules/regex.rs`) hot saturation path;
   its disposition needs LOCK-2/LOCK-5 sign-off + a **required saturation perf measurement** across all
   three tiers — never a free LOCK-1 hook.
7. **X7 [LOCK-6] — the TS-consumption path constrains the record.** Only `TypeDesc` has a shipping TS
   consumer (`backend/ts/projection.rs`); a fused monotype (1A) and `TypeENode` (1D) have **no demonstrated
   TS mirror**. Show it before the record is chosen (Constitution iii).
8. **X8 — land or retire the four probes in the tree.** PROTO-1 (`lock1_residue_census.rs`), PROTO-2
   (`type_injectivity.rs` + the reverted egraph amendment), PROTO-1B (`lattices/clash.rs`),
   PROTO-2-recursion-parity (`lock1_recursion_parity.rs`) survive only as `.patch.diff` — non-reproducible
   from HEAD. Land them (behind the base decision) or state them retired; land the additive `assign`
   hardening test regardless of carrier.
9. **Zero-runtime-cost gate is asserted by every family, exercised by none** — a named gap for all four in
   pass-2 (`git diff crates/runtime/` empty on every wave).

---

## PASS HISTORY

### Pass 1 (folded by AGGLOMERATOR, Fable-tier) — RE-ANCHORED FOLD (consumes run2 + probe2 + on-disk critics)

- **Inputs:** research-{A,B,C,D,priorart}; FAMILY-SPECS (04:41, stale — sealed before its decisive briefs
  ran); PROTO-1 (judgment-mix census, RAN_MEASURED — document-shape residue only, X3/X4-flawed instrument);
  PROTO-2 (1D injectivity vs strawman, RAN_MEASURED on a reverted engine patch, "keep"); **PROTO-1B-clash**
  (04:48) **+ run2** (09:39, RAN_MEASURED — CSP-equality soundness, non-commutativity of dead `join`, live
  path SOUND on a 2nd fixture); **PROTO-2-recursion-parity** (05:01) **+ probe2** (09:39, RAN_MEASURED —
  total `array≡object` collapse in the `struct_registry` sidecar, `LayoutKind` has no Row, masked-fallback
  `json_templates`, collapse SPECIFIC to hetero-Alt-of-compounds, incumbent finite+terminating on all
  grammars); CRIT-1 (within-lock, post-run2/probe2, **64/63/52/48**); CRIT-2 (cross-lock, post-run2/probe2,
  **56/54/40/44**).
- **What this fold corrects vs the prior draft (05:15).** (a) The prior draft cited **stale** critic scores
  (68/65/55/52, 55/55/40/45) and folded on **v1** parity; this fold re-anchors on the on-disk critics
  (64/63/52/48, 56/54/40/44) and the **09:39 re-runs**. (b) run2 makes the "shipping defect" **doubly dead**
  (deadness + non-commutativity proof + a 2nd sound-live-path fixture), so 1A's carrier-displacement lever
  is void and the carrier fork is **more** open. (c) probe2 re-opens the structural-fidelity question the
  clean RETIRE closed: the `List ≠ Row` collapse is **total in the sidecar codegen consumes** and recovered
  by a **masked fallback** — but it is **engine-agnostic** (a `LayoutKind::Row` fix) and **specific** to one
  grammar shape, and 1D's true make-or-break (Row *inference*) is **unbuilt** → 1D moves from clean RETIRE
  to **CONTESTED**.
- **Agglomerator verification (live tree, this pass):** re-ran every crux grep — `TypeDomain::join` 0 call
  sites; `assign` LWW; `egraph.rs:236` `if changed` guard present + `rebuild_pass_count` absent (1D
  amendment reverted); `LayoutKind` = {Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}, **no Row**
  (struct.rs:58); `TypeAnalysis::from_ir` live (analysis.rs:16/22/221) + a 2nd `from_ir` (named_types.rs:100);
  `csp_strategy/` second Csp present; `egraph/rules/regex.rs` present (3-lock); `json_templates/value.rs` +
  shipped `runtime/json/value.rs` present (masked fallback); skinny `:204` `->` discard verbatim; root
  `HeterogeneousAltJoin` live (type_desc.rs:63). **The critics did not err on any load-bearing fact.** One
  named prior critic error (CRIT-2's stale-SK-V22 X1) is resolved by `skv24-handoff` and moves no floor.
- **Floors taken unadjusted (min across current post-run2/probe2 critics):** 1A 56% · 1B 54% · 1C 40% ·
  1D 44%. CRIT-2 binds on all four.
- **Dispositions set:** 1A **ADVANCED** (endorsed HM(X) trunk; unproven in tree; carrier-displacement lever
  doubly dead; unnamed LOCK-3 consumer) 56%; 1B **ADVANCED** (running system-of-record, live path SOUND,
  possibly the cheaper spine — single-store thesis **BLOCKED-candidate**: a LOCK-1⊕LOCK-2 store merge) 54%;
  1C **BANKED** (discipline on the winner; standalone-engine banked; R-C2 structurally unrunnable) 40%; 1D
  **BANKED** (recursion-keep + "zero new engine" + cost-extraction RETIRED; structural-μ hook **CONTESTED** —
  engine-agnostic + Row-inference unbuilt) 44%.
- **Redirects:** the HM(X) *architecture* is BANKED as consensus (redundant mechanism, settled floor);
  neither *carrier* force-banked (a genuine build-cost fork, prior tilt on dead code) — bank-decision
  deferred to named pass-2 experiments. Three underexplored formulations opened: **the engine-agnostic
  record-widening** (`LayoutKind::Row`, NEW — may make the whole engine axis orthogonal to the structural
  question), the **pluggable residue-solver X**, the **attribute-grammar cross-rule seam**. Cross-pollination:
  1C bidirectional overlay ⟶ the winning trunk (shared reconciler). 1D's three probe verdicts reconciled:
  **recursion RETIRE governs**, structural-μ **CONTESTED** and gated on a LOCK-7 architecture call +
  an unbuilt Row-*inference* probe.
- **Lock-wide:** no family convergence-eligible (pass 1 of ≥3). The two sharpest unadjudicated questions,
  both partly **cross-lock** and both mis-sized by the specs: (i) the **carrier seat** — harden the running
  CSP (1B) or delete it for a union-find re-seed (1A)? — decided by a build-cost measurement, not the void
  "unsound" defect; and (ii) whether 1D's type Language can **derive** `List ≠ Row` by inference (closing the
  `json_templates` masked fallback) or merely **relocates** it — and whether a `LayoutKind::Row` widening
  dissolves the question for every engine at once. The synthesis verdict ("HM(X) trunk + bounded defaulting
  residue + 1C folded in") is **prior-art-consonant and probably right** — but it was earned by framing on a
  zero-`->` corpus and a pipeline that cannot express `List ≠ Row`, and every family spec pretends the
  type-of-record is LOCK-1's to choose alone when it is constrained from LOCK-3/LOCK-6/LOCK-7.

<!-- Pass 2 appends below this line -->

---

## PASS 2 (folded by AGGLOMERATOR, Fable-tier) — the probes RAN but did not LAND; the banked architecture is itself unbuilt

**Inputs.** research-{A,B,C,D,priorart} (pass-2); FAMILY-SPECS **hardened re-fold** (15:03 — authored
mid-pass, *before* its own two sharpest probes); PROTO-1 (15:16, the WORKSPACE DIVORCE — Open R BLOCKED,
RAN on the real grammar); PROTO-2 (13:40, Row-inference — DISSOLVE claim, but the Row vocabulary is
patch-only); PROTO-2-iso (15:23, the carrier iso floor — PASS, but the Row half rides uncommitted
vocabulary); CRIT-1 (within-lock, HEAD `c43126422`, **55/55/40/43**); CRIT-2 (cross-lock, HEAD
`c43126422`, **55/54/40/43**); CROSS-LOCK ledger XL-A…XL-H (binding).

**AGGLOMERATOR live-tree re-verification (I ran every crux grep at HEAD `c43126422` — the critics' own
pin, now == current HEAD; `ce51c36fc` the probe pin is an ancestor):**

| Fact | Verified truth at HEAD | Bearing |
|---|---|---|
| `assign` carrier (`revise.rs:27`) | **raw 3-line LWW** (`if slot==Some(&ty){false} else {*slot=Some(ty);true}`) — NOT the refinement-aware `match` | **B1 hardening UNLANDED. X8 unmet after TWO passes** — the one carrier-INDEPENDENT deliverable is still absent |
| `TypeDesc::Row` | `grep Row type_desc.rs` = **0** | Open R vocabulary **SPEC_ONLY** (unlanded) |
| `LayoutKind` | `{Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}` — **no Row** | record-expressiveness gap intact (X4) |
| `TypeDomain::join` | **0 call sites** (dead); non-commutative LWW *in dead code* | the "shipping defect" 1A's carrier-displacement rested on is **doubly dead** — reconfirmed |
| **let-generalization in `passes/types/`** | **1 hit — a module DOC-COMMENT** ("the *generalized* csp-solver crate", the crate name) | **the running CSP (leading carrier 1B) carries ZERO let-gen — the banked HM(X) is unbuilt at the polymorphism layer** |
| `has_rule("value")` mask | present `runtime_generator.rs:98` | masked fallback intact; clause-i defect unclosed |
| **workspace divorce** | `skinny/` own `[workspace]`; main `Cargo.toml` lists skinny **0×**; `skinny/crates/ir` = pkg **`ir`**, `crates/ir` = pkg **`bbnf-ir`**; skinny references `TypeDesc\|StructRegistry\|LayoutKind` **0 hits** | **Open R physically CANNOT reach the skinny mask** — the structural axis is a GN-1 codepath-unification blocker |
| `type_desc_to_ts` (`ts/projection.rs:11`) | **sole `TypeDesc→TS` sink** | a fused monotype (1A) & a `TypeENode` (1D) are not `TypeDesc` → **no TS path** (categorical iso failure) |
| probe test files | `carrier_iso_ts.rs`, `proto2_row_shape.rs`, `constraint/shape.rs` **ALL ABSENT** | **both pass-2 probes SPEC_ONLY by XL-H**; only their HEAD-real halves bank |
| `TypeAnalysis::from_ir` | live `analysis.rs:16/22/221` + 2nd `from_ir` `named_types.rs:100` | the LOCK-3 sink the record must lower through; 1D's net-new `TypeAnalysis` is a name-collision |

**The critics did not err on any load-bearing fact.** Floors bind **unadjusted** = min across the two
on-disk critics: **1A min(55,55)=55 · 1B min(55,54)=54 · 1C min(40,40)=40 · 1D min(43,43)=43.** CRIT-2
binds 1B and 1D; both bind 1A and 1C.

**The four material findings this pass (what changed since pass 1's 56/54/40/44):**

1. **BOTH pass-2 probes are SPEC_ONLY; X8 is unmet after TWO passes.** The `assign` hardening — the single
   deliverable both critics + the registry called "ships regardless of carrier" — is *still* the raw LWW at
   HEAD. `TypeDesc::Row`, `shape.rs`, `carrier_iso_ts.rs` are patch-only. This is a two-pass stagnation
   signal, not a scope reveal. **But the probes' DISPOSITIVE structural facts DID run** and bank (below).
2. **The banked HM(X) architecture is ITSELF unbuilt — the deepest new hit (vacuous-convergence).** §5.1
   banks "HM(X) = equality trunk + bounded residue + **let-generalization**" as the settled floor 1A and 1B
   *share*; §1A calls 1A "the **ONLY** family that carries polymorphism." These contradict, and the tree
   adjudicates against the floor: the running CSP (1B) has **zero** let-gen (verified). So "1A and 1B
   collapsed onto one architecture" is **false at the polymorphism layer** — the exact layer let-gen exists
   to guarantee (false-fusion soundness) — and the convergence floors (43–55%) are computed against an
   architecture **no family instantiates.** 1B's "cheapest net-0 diff" omits an unbuilt let-gen graft; no 1B
   gate tests false fusion (G3 is 1A-only).
3. **The carrier fork gained a SECOND axis — and it may be MISFRAMED as build-cost.** PROTO-2-iso's
   **HEAD-real negative** (only `TypeDesc` crosses `type_desc_to_ts`; a fused monotype [1A] and a `TypeENode`
   [1D] categorically have NO TS path) seats the carrier on **iso**, independent of build-cost. Under the iso
   lens, 1A-as-carrier is **pre-eliminated categorically** and building Option A measures a race whose loser
   is already out. But Constitution iii is **LOCK-6's verdict to render** — LOCK-1 banks the negative as a
   fact, it cannot seat the carrier on iso alone.
4. **The structural axis (`List ≠ Row`) is RE-HOMED to GN-1** — a LOCK-7⊕LOCK-3 codepath-unification (the
   loom) obligation, NOT a LOCK-1 brief. PROTO-1 proves Open R lands in `bbnf-ir` while the mask lives in the
   divorced skinny `ir` workspace that references no `TypeDesc`; a perfectly-landed, perfectly-annotated Row
   derivation cannot cross the workspace boundary, and deleting the mask **regresses** (routes JSON to
   `emit_general_descent`, which has no Row/List classifier). This **strictly under-sizes** pass-1's
   "engine-agnostic + Row-inference unbuilt" framing (→ 1D −1). **Brief 1 as written tests the retiring
   artifact and repeats the plan PROTO-1 refuted** — a gate-that-cannot-pass; withdraw it.

**The HEAD-real probe facts that DO bank (credit where the probes RAN — not wholesale SPEC_ONLY):**
- **(a) the workspace divorce** — Open R cannot reach the skinny mask; the structural axis is a GN-1 loom
  build (PROTO-1 §1, RAN at `ce51c36fc`, re-verified by both critics + agglomerator at HEAD).
- **(b) GN-3.W3 wrap gate CLEARED for the rule-level form** — `keystr = /"[^"]*"/ -> input : key` lowered
  cleanly (10 passed; `wrap.rs:242` did NOT fire); the *inline* `(string -> key)` still trips it. So the
  wrap trap is NOT the structural blocker — the workspace divorce is (PROTO-1 §4, RAN).
- **(c) PROTO-2-iso's Sum-crossing** — `HeterogeneousAltJoin([Row,Row]) → type_desc_to_ts` union-name ↔
  Rust boxed-enum-view, on **pristine HEAD** (the Sum vocabulary predates the probe); + the
  monotype/`TypeENode`-have-no-`*_to_ts` negative (HEAD-real, decisive on iso). **The Row-crossing half rides
  uncommitted vocabulary → SPEC_ONLY.** Honest split: *Sum-crossing HEAD-real; Row-crossing SPEC_ONLY.*

---

## CURRENT DISPOSITIONS (pass-2 fold)

| Family | Status | Conv. | Δ vs pass-1 | One-line |
|---|---|---|---|---|
| **1A · UNIFICATION-FIRST** | **ADVANCED** (carrier cand. #1, LEAN-against) | **55%** | −1 | Endorsed HM(X) trunk; **the only family that would carry let-gen** — its one distinctive rightness. But **categorically LAST on iso** (a fused monotype is not a `TypeDesc`, HEAD-real); Option A (union-find re-seed) **UNRUN**; deletes ~101 LOC + re-invents the refinement lattice; displacement lever doubly dead. |
| **1B · ONE-CONSTRAINT-STORE** | **ADVANCED** (carrier cand. #2, LEADING) — single-store BLOCKED-candidate | **54%** | 0 | The only family that runs on the surviving base; **iso-safe (Sum-crossing HEAD-real)**; cheapest additive diff. But **does NOT ship the banked HM(X)** — zero let-gen; **X8 unmet ×2** (the ships-regardless deliverable still absent); clash obligation **consumer-less**; single-store = LOCK-1⊕LOCK-2 merge. The Sum-iso advance offsets the new debits → held. |
| **1C · BIDIRECTIONAL-LOCAL** | **BANKED** (diagnostics discipline, reactivation-gated) | **40%** | 0 | Not a fourth engine — a checking front + span field over the winning trunk. Make-or-break (R-C2) **still has no input** (greenfield json's 4 arrows are all scalar leaf projections — none a mismatch); GN-3.W3 wall reconfirmed; LSP bridge unbuilt; **banked wave still UNNAMED**. |
| **1D · TYPES-AS-ECLASSES** | **BANKED** — recursion-keep + "zero new engine" + cost-extraction **RETIRED**; structural-μ **UNPROVEN (not dissolved)** | **43%** | −1 | Structural axis **blocked behind GN-1 codepath unification** (PROTO-1 divorce, strictly larger than pass-1's framing); PROTO-2 "DISSOLVES" ⊥ PROTO-1 "BLOCKED" — **the tree adjudicates for PROTO-1**; Row/TS SPEC_ONLY; `union` amendment a 3-lock blast radius; `TypeAnalysis` name-collision. |

---

### FAMILY 1A — pass-2 delta (ADVANCED · 55%, −1)

**What advanced.** The let-gen finding is the one place 1A is *distinctively right*: it alone would carry
polymorphism (`generalize(infer(body))` per rule, `instantiate` per `Ref` — the false-fusion guard). Held,
not cut further, on that.

**What cut it (−1).** PROTO-2-iso converts pass-1's "TS mirror unshown" into a **categorical HEAD-real iso
failure**: a fused monotype is not a `TypeDesc` value, so it has **no `type_desc_to_ts` path at all** —
choosing 1A requires authoring a brand-new `*_to_ts` sink from scratch. This is a sharper mark than a
parity-margin loss and ranks 1A **last on iso**.

**Open gaps (updated).** G-1A-1 (port cost — deletes ~101 LOC, re-invents refinement lattice — LEAN-against,
Option A UNRUN); G-1A-2 (displacement lever doubly dead — reconfirmed); G-1A-3 (zero direct 1A prototype,
G1–G7 asserted); **G-1A-4 [SHARPENED — categorical iso failure]** no `type_desc_to_ts` for a non-`TypeDesc`
carrier; G-1A-5 (width residue UNRUN on this pass's instrument); G-1A-6 (row discipline unmeasurable — zero
row-shaped `->` in the real corpus, verified); **G-1A-7 [NEW] the `HeterogeneousAltJoin`-retires-under-
unification claim (research-A §4) is unproven** — no probe shows the Sum obligation retiring rather than
coexisting with union-find's structural arm.

**Pass-3 directives.** (1) **Decide the carrier axis FIRST** — if iso governs, 1A is pre-eliminated and
Option A need not be built; if build-cost governs, build Option A (union-find re-seed) + race parity against
the running pass base-pinned. (2) Only if 1A survives the axis choice: build the direct nominal-fold
prototype, cost the lineage port head-to-head, run the width probe, pick the row discipline.

**Withheld:** do not re-derive HM(X) stratification (settled); do not re-argue union-find dominates equality
asymptotically (settled); do not build Option A *before* the carrier axis is declared dispositive.

### FAMILY 1B — pass-2 delta (ADVANCED · 54%, held — LEADING carrier)

**What advanced (HEAD-real).** PROTO-2-iso banks 1B's **Sum-crossing** as HEAD-real
(`HeterogeneousAltJoin → type_desc_to_ts`, pristine HEAD) — 1B is the only carrier that already crosses to
TS. On the iso axis 1B leads decisively.

**What held it down (offsetting the iso advance — CRIT-2's −2 vs the re-fold's inflated 56 floor).**
**G-1B-NEW-1 [SHARPEST] 1B does NOT ship the banked HM(X)** — the running CSP has zero let-generalization
(verified); "one architecture" is unsound at the polymorphism layer; **no 1B gate tests false fusion**; the
"net-0 cheapest diff" omits an unbuilt let-gen graft into the monotone `propagate()` sweep. **G-1B-1 [X8
unmet ×2]** the additive `assign` hardening is STILL the raw LWW at HEAD. **G-1B-3 [consumer-less]** B1's
`TypeClash` obligation is parity-inert (codegen reads `ir.types`+`struct_registry`, never
`type_obligations`); its only consumer is 1C's unbuilt LSP bridge — the joint wave is unnamed. **G-1B-2
[X3]** single-store = LOCK-1⊕LOCK-2 merge (verified two federated `Csp<_>` over one `csp-solver`) —
BLOCKED-candidate. **G-1B-5/6** census (G2) + B4 store-shape (G3) unrun/unbuilt across two passes.

**Pass-3 directives.** (1) **LAND the refinement-aware `assign` clash arm in-tree** (X8, third call) — the
form that refuses concrete-vs-concrete but permits `BoxedEnum→structural` refinement (the naive refuse-all
freezes 69 refinements, measured); confirm `types_heterogeneous_alt` green + runtime `git diff` empty.
Co-schedule the 1C LSP bridge — name the joint wave. (2) **Build a false-fusion fixture for 1B and cost the
let-gen graft**, or stop banking the "one architecture" collapse. (3) Re-run the residue census on a
MODELING instrument (overload/legality/decode-class); build B4 both ways.

**Withheld:** do not re-assert "1B collapses to 1A" on the current X3/X4-flawed census; do not propose
csp-solver *engine* features (stay consumer-side, add domains); do not call the 1A/1B collapse "one
architecture" until let-gen is instantiated.

### FAMILY 1C — pass-2 delta (BANKED · 40%, held)

No pass-2 evidence moved it; PROTO-2 *reconfirms* the substrate wall (the inline `string -> key` trips
`wrap.rs:242`) rather than lifting it. Correctly banked as the winner's diagnostics/locality discipline.

**Open gaps (held).** G-1C-1 (make-or-break has no input — needs a hand-authored declared/synthesized-
*mismatch* `->` fixture; the greenfield json's 4 arrows are all scalar leaf projections); G-1C-2 (GN-3.W3
wall broken); G-1C-3 (LSP bridge unbuilt — the SAME bridge 1B's B1 depends on, viii); G-1C-4 (sub-span
ceiling, a LOCK-3 field). **The banked wave is STILL UNNAMED** (both critics) — it risks falling between
carrier-selection and GESTALT-B.

**Pass-3 directives.** (1) Hand-author the mismatch fixture (`frac = digits "." digits -> Ratio`) and run
R-C2 — the single experiment that decides whether 1C earns a name. (2) **NAME THE WAVE** that lands the
`PassDiagnostic.span` add + the `type_obligations → lsp_types::Diagnostic{range}` bridge — jointly with
1B's B1 (the shared consumer), regardless of carrier winner.

**Withheld:** do not re-argue 1C as a distinct fourth engine (banked); do not develop the width seam as a
separate subsystem (1C only *consumes* the resolved width at the subsumption site).

### FAMILY 1D — pass-2 delta (BANKED · 43%, −1)

**What cut it (−1).** PROTO-1's workspace divorce proves the structural axis is blocked behind **GN-1
codepath unification** (a LOCK-7⊕LOCK-3 totality build against the tree-of-record), strictly larger than
pass-1's "engine-agnostic + Row-inference unbuilt." PROTO-2 ("Open R DISSOLVES 1D") and PROTO-1 ("BLOCKED")
are **flatly contradictory verdicts on the same question**; the tree adjudicates for PROTO-1 (PROTO-2's own
`git diff skinny/` = empty — it never touched, never *could* touch, the mask).

**Open gaps (updated).** **G-1D-1 [the divorce refutes "dissolve"]** Open R cannot close the mask; the
structural axis is a GN-1 loom build. **G-1D-3 [make-or-break UNBUILT]** on the real `object` body the
classifier returns `Unknown` — deriving `Row` without the `key` role is unshown; extraction ≠ inference.
G-1D-1(union) (3-lock blast radius, unmeasured, GESTALT-B); G-1D-2 (`TypeENode` name-collision + no TS path
— ranks last on iso); G-1D-4 (AC-sum + injectivity coexistence unbuilt — tested *around*, never *through*);
G-1D-5 (RETIRE/KEEP architecture-contingent + the masked fallback is a clause-i defect owned jointly with
LOCK-7, independent of 1D's fate).

**Pass-3 directives.** (1) **[GN-1 / GESTALT-B, NOT a LOCK-1 brief]** re-home the structural axis; withdraw
Brief 1 as written; LOCK-1's residue is ONLY the engine-agnostic `TypeDesc::Row` vocabulary widening (a
LOCK-3 fix, X4). (2) The `union` amendment stays a 3-lock GESTALT-B/LOCK-2/LOCK-5 saturation call — must not
silently re-enter via 1D's saturation claims. **Reactivate 1D only if the structural-μ hook is proven NEEDED
AND 1D-specific — which the GN-1 loom decides, not a LOCK-1 recursion race.**

**Withheld:** "zero new engine" is RETIRED — do not re-pose; do not carry `union()` as a free LOCK-1 hook;
do not model width as distinct `Int8`/`Int16` constructors; do not re-run the strawman recursion race
(retired); do NOT re-run Brief 1 as written against the retiring skinny substrate.

---

## REDIRECT / CROWDING DECISIONS (pass-2)

1. **The banked HM(X) architecture is REOPENED as a QUALIFIED consensus — a TARGET, not an artifact.** Pass
   1 banked HM(X) as the settled floor. Pass 2 proves the running carrier omits let-generalization → the
   "one architecture" collapse is **vacuous at the polymorphism layer.** The architecture stays banked as
   the *direction*, but pass 3 must **build it** (a false-fusion fixture + the let-gen graft) or stop banking
   the 1A/1B collapse. This is a redirect *inward*, not a new family.
2. **The carrier fork stays OPEN (1A 55 vs 1B 54) but gains a decisive SECOND axis.** No carrier is
   force-banked. But the axis is now two-dimensional: **build-cost** (1B additive net-0 vs 1A deletes ~101
   LOC — LEAN-1B) *and* **iso** (1B's `TypeDesc` crosses HEAD-real; 1A's monotype categorically cannot —
   1A pre-eliminated). Pass 3 must **declare which axis is dispositive** before building — if iso governs,
   the seat is nearly closed FOR 1B and Option A need not be built; if build-cost governs, build Option A.
   *Do not demand both briefs without a dispositive one.* The iso seat routes to **LOCK-6/GESTALT-B** for
   ratification (Constitution iii is LOCK-6's verdict, conveyed).
3. **1C → BANKED (held).** Reactivation gate: R-C2 head-to-head on a hand-authored mismatch fixture.
4. **1D → BANKED (held), structural axis RE-HOMED to GN-1.** Not a LOCK-1 recursion race — a LOCK-7⊕LOCK-3
   codepath-unification build. Withdraw Brief 1.

**Underexplored formulations — the register updated:**
- **[NEW this pass] The let-generalization graft into the monotone CSP.** The pass's deepest unbuilt piece:
  can let-gen grow *additively* into the running `propagate()` sweep (so 1B ships the banked HM(X)), or does
  it require the union-find re-seed (1A)? **This single question decides whether the "one architecture"
  collapse is real** and whether 1B's "cheapest diff" survives. Open it as a first-class pass-3 seam.
- **[RESOLVED-as-blocked] The engine-agnostic record-widening (Open R).** Pass 1 opened it as possibly
  making the engine axis orthogonal to structure. Pass 2 CLOSES that question in the negative *in-tree*: on
  the real grammar every engine returns `Unknown`, and Open R cannot reach the mask across the workspace
  divorce. It is no longer "underexplored" — it is a **named GN-1 cross-lock obligation.** Its LOCK-1 residue
  is only the `TypeDesc::Row` vocabulary widening.
- **[held-open] Open X (pluggable residue solver)** — UNRUN two passes AND **double-homes** with LOCK-2's
  `csp_strategy` CSP (2C, same vendored `csp-solver`). Route the double-home to GESTALT-B; run the width
  micro-fixture on a modeling instrument before "defaulting dominates" is banked.
- **[held-open] Open S (attribute-grammar cross-rule seam)** — asserted-as-reading, still unproven; either
  demonstrate the running `mod.rs:72-127` cycle-break IS a monotone-lattice circular AG or stop counting it
  resolved.

## CROSS-POLLINATION (pass-2)

**1C's bidirectional check/synthesize ⟶ the winning trunk (1A or hardened-1B) — STANDS** (both families
developed enough; the graft is named: `infer_expr → synth_expr`, `check_expr` overlays, the trunk's equality
reconciler shared). *No new graft this pass.* The deferred 1D μ-extraction graft stays deferred (the
comparison is unbuilt and the parity probe retired it; re-evaluate only if the GN-1 loom revives 1D's
structural hook). PROTO-2-iso's Sum-crossing is banked as **evidence** (1B's iso is HEAD-real), not a graft.

## CROSS-LOCK / GESTALT DIRECTIVES (pass-2 — supersedes pass-1 where noted)

1. **X1/XL-A — lineage RESOLVED (held):** skinny retires; skinny@HEAD is the MEASUREMENT SUBSTRATE only;
   root/greenfield is the tree-of-record-to-be. The carrier port cost tilts against 1A/1C.
2. **X2/XL-F — the LOCK-3 sink is `TypeAnalysis::from_ir` (held).** Whichever record wins lowers through it;
   1D's net-new `TypeAnalysis` is a name-collision.
3. **X3 — 1B single-store = LOCK-1⊕LOCK-2 merge (held, verified).** Bank the literal thesis; keep the
   federation.
4. **X4 — `List ≠ Row` vocabulary widening (`TypeDesc::Row`/`LayoutKind::Row`) is a LOCK-3 fix, engine-
   agnostic (held).** But **widening alone does not DERIVE the fact** — pass 2's correction.
5. **[NEW] X9 — the structural axis is a GN-1 LOOM obligation (LOCK-7⊕LOCK-3), routed to GESTALT-B.** The
   `List ≠ Row` closure is codepath unification against the tree-of-record, not a mask deletion on retiring
   skinny. **Brief 1 is WITHDRAWN as written** (it tests the retiring artifact, a gate-that-cannot-pass, and
   silently contradicts XL-A — correct it into a named supersession, Constitution ix).
6. **[NEW] X10 — the banked HM(X) is unbuilt at the polymorphism layer.** The convergence floors measure
   against an architecture no family instantiates. Build a false-fusion fixture for 1B + cost the let-gen
   graft, or re-open §5.1's "one architecture" collapse.
7. **[NEW] X11 — TWO double-homes to adjudicate at GESTALT-B.** (a) Open X (residue defaulting-lattice)
   double-homes with LOCK-2's `csp_strategy` CSP — same vendored `csp-solver`, two federated stores; (b) the
   iso-carrier seat is a **LOCK-6** verdict (Constitution iii) — LOCK-1 banks the HEAD-real negative as a
   fact but cannot seat the carrier on iso alone.
8. **X6 — 1D's `union` amendment is a 3-lock blast radius (held):** LOCK-2 grammar-tier + LOCK-5 regex-tier
   (`egraph/rules/regex.rs`); needs LOCK-2/LOCK-5 sign-off + a 3-tier saturation-perf measurement.
9. **X7/Constitution iii — the TS path constrains the record (held, now HEAD-measured on the Sum half):**
   only `TypeDesc` crosses (`type_desc_to_ts`); monotype (1A) and `TypeENode` (1D) have no TS mirror.
10. **X8 — LAND the additive `assign` hardening in-tree (THIRD call).** Unmet after two passes; the single
    carrier-INDEPENDENT deliverable; co-schedule the 1C LSP bridge (its only consumer). Land or retire the
    four probes in-tree — all three pass-2 probe test files are absent at HEAD.
11. **XL-F(1) / Constitution ix — formalize the GN-3 §1.5 supersession with LOCK-7 sign-off** (recorded
    WORKING in the CROSS-LOCK ledger); correct Brief 1's silent XL-A contradiction into a named supersession.

**Lock-wide (pass 2 of ≥3).** No family is convergence-eligible (no clean-pass streak; floors 43–55%). The
hardened re-fold was an honest advance — it downgraded both pass-1 headlines to LEAN/UNPROVEN and banked
HM(X) as the floor — **but it was authored before its own two sharpest probes**, and the tree adjudicates
four defects it could not see: the structural axis is a **GN-1 loom obligation** (Brief 1 withdrawn); the
banked HM(X)-with-let-gen is **itself unbuilt** (vacuous-convergence at the polymorphism layer); Open X and
the iso-carrier seat **double-home** against LOCK-2 and LOCK-6; and both probes are **SPEC_ONLY with X8
unmet ×2**. The three durable, MEASURED banks: the **doubly-dead** displacement lever; the **workspace
divorce** (structural axis → GN-1); and PROTO-2-iso's **Sum-crossing + monotype-has-no-TS negative** (1B
leads on iso, HEAD-real). The carrier seat is **LEAN-1B, not CLOSED**; the structural axis is **UNSOLVED by
every family and re-homed to GN-1**; the banked HM(X) is a **target, not an artifact.**

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

## PASS 2 (folded by AGGLOMERATOR, Fable-tier) — DEFINITIVE RE-FOLD on the newest round: X8 is WORKTREE-not-tree-of-record, let-gen is REFUTED-as-inapplicable, the build-cost lever is WRONG-TREE

*This re-fold **supersedes an intra-pass earlier pass-2 draft** (folded against the 20:02 critics @ pin
`c43126422`, scores 55/55/40/43 & 55/54/40/43, on the 19:27/19:37 probe round). Within pass 2 the probes
**re-ran** at 22:21–22:23 and CRIT-1 (22:35) explicitly supersedes its own 20:02 predecessor; CRIT-2
(22:39) is cross-lock-weighted and NEW. The definitive round lands the floors **lower** and adds three
findings the earlier draft could not see — the X8 tree-of-record over-claim, the CL-1 wrong-tree
build-cost lever, and the closure-panic elegant-reduction contingency. The re-anchor discipline mirrors
pass-1's own stale-draft correction: consume the newest on-disk critics, re-verify every crux at HEAD.*

**Inputs.** research-{A,B,C,D,priorart} (pass-2); FAMILY-SPECS (18:46, pin `06e70053f`); **PROTO-1 RE-RUN**
(22:23, base-pin `ce51c36fc` — let-gen graft vs union-find re-seed, **RAN_MEASURED → REFUTATION-AS-LAW**);
**PROTO-2 RE-RUN** (22:21, X8 third call — **PASS/RAN_MEASURED *in the probe's private worktree*,
staged-not-shipped**); PROTO-2-iso (carrier iso floor — Sum-crossing HEAD-real, Row-crossing SPEC_ONLY);
CRIT-1 (within-lock, HEAD `3f2350f15`, **54/53/40/42**); CRIT-2 (cross-lock, HEAD `3f2350f15`,
**52/58/40/43**); CROSS-LOCK ledger XL-A…XL-H (binding, conveyed as fact).

**AGGLOMERATOR live-tree re-verification (every crux grep re-run at the TRUE current HEAD `1a0d3091c` —
ONE TICK NEWER than the critics' pin `3f2350f15`, which is a confirmed ancestor; the probe pin `ce51c36fc`
is also an ancestor):**

| Fact | Verified truth at HEAD `1a0d3091c` | Bearing |
|---|---|---|
| `assign` carrier (`revise.rs:28-33`) | **raw 3-line LWW** (`if slot.as_ref()==Some(&ty){false} else {*slot=Some(ty);true}`) | **X8 UNSHIPPED after two passes + a third probe re-run.** PROTO-2's "landed/already-staged" is refuted at the tree |
| `git diff --cached --stat` | **EMPTY** (nothing staged) | PROTO-2's "already fully staged in the index" is a **worktree/index artifact in the probe's ephemeral tree**, absent from the tree of record |
| `TypeClash` / `is_fully_ground` / `record_domain_clashes` in `crates/ir/src` | **0 hits (absent)** | the X8 mechanism does not exist at HEAD; its test file `type_clash_arm.rs` **absent** |
| let-gen in `passes/types/` (`grep generali\|instantiat\|polymorph\|scheme`) | **1 hit — a module DOC-COMMENT** ("generalized csp-solver") | the running carrier (1B) carries **ZERO** let-generalization — reconfirmed |
| `IrNode` inter-rule linkage (`types/node.rs:61`) | **`Ref(RuleId)` only** — no `Call`/`Apply`/`Lambda`/`Closure`/type-param | **no polymorphic surface exists** — PROTO-1's refutation: false-fusion is structurally unreachable |
| closure lowering | `lower/expression/mod.rs:168` `.expect("closure: missing body child")` + `lower/value_expr/simple_kinds.rs:199` `.expect(...)` | **the elegant-reduction trap**: "monomorphic by construction" rests on a **panicking, `#[ignore]`d** closure feature |
| `TypeDesc::Row` / `Sum` (`type_desc.rs`) | **absent** — only `HeterogeneousAltJoin` (`:63`) | Open R / iso-Row-crossing vocabulary **SPEC_ONLY**; only the Sum arm is HEAD-real |
| `TypeDomain::join` call sites in `passes/types/` | **0 (dead)** — live carrier is `assign` (sound) | 1A's "running CSP overwrite-unsound" displacement lever is **doubly dead** — reconfirmed |
| `type_desc_to_ts` (`ts/projection.rs:11`) | **sole `TypeDesc→TS` sink**, dispatches on `TypeDesc` variants only | a fused monotype (1A) & a `TypeENode` (1D) have **no TS path** — categorical iso elimination, HEAD-real |
| `csp-solver = "0.1"` | pinned in **BOTH** `crates/ir/Cargo.toml:18` + `crates/core/Cargo.toml:20` | 1B's literal single-store = a **LOCK-1⊕LOCK-2 federation merge** over the shared vendored crate |
| probe test files | `type_clash_arm.rs`, `proto1_false_fusion.rs`, `carrier_iso_ts.rs` **ALL ABSENT** | **all three pass-2 probes are worktree-only / SPEC_ONLY by XL-H**; only their HEAD-real halves bank |

**The critics did not err on any load-bearing fact** — re-verified one tick newer than their pin. Floors
bind **unadjusted** = min across the two on-disk critics: **1A min(54,52)=52 · 1B min(53,58)=53 ·
1C min(40,40)=40 · 1D min(42,43)=42.** CRIT-2 (cross-lock) binds 1A; CRIT-1 (within-lock) binds 1B, 1C, 1D.
*No floor is adjusted upward:* every axis on which a probe "moved" moved on worktree artifacts that did not
reach the tree of record, and the two RAN_MEASURED results (PROTO-1 refutation, PROTO-2 worktree-PASS) are
correctly priced by the critics as, respectively, a *contingent* bank and an *unshipped* deliverable.

**The five material findings this round (what changed since the intra-pass draft's 55/54/40/43):**

1. **X8 is WORKTREE, not tree-of-record — the sharpest evidence-integrity finding (−nothing new to floor,
   but corrects the ledger).** PROTO-2 (22:21) escalates to "already fully staged / landed / RAN_MEASURED
   … verified carrier-independently." At HEAD the index is **empty**, `assign` is the **raw LWW**,
   `TypeClash` is **absent**, the test file is **absent**. The 28 green tests are honest **in the probe's
   private base-pinned worktree**; they **did not propagate**, and "staged in the index" is refuted by the
   empty index. Under XL-H + Constitution vii, "landed" on a SPEC_ONLY artifact is an **over-claim** — the
   ledger records X8 as **proven-landable + functionally-PASS-in-worktree, UNSHIPPED at the tree of
   record**, not "closed." A synthesis and its own commissioned probe disagreeing on whether the lock's
   headline deliverable shipped — with the probe wrong against the tree — is a convergence-blocking
   evidence-integrity fault, not a bookkeeping nit.
2. **PROTO-1 REFUTATION-AS-LAW — let-generalization is INAPPLICABLE, not a missing floor.** The 22:23 re-run
   RAN_MEASURED: `pair` gets **one** type entry (one `RuleId` → one `TypeVarId`); both uses `a`/`b` fuse to
   `Tuple([BoxedEnum, BoxedEnum])` — and **fusing is CORRECT**, because BBNF rules are *monomorphic by
   construction* (`Ref(RuleId)` only; zero type-parametric application in `IrNode`). The literal PASS
   condition ("two uses → two distinct monotypes") is **unreachable and would be a soundness *bug* if
   reached.** A meaningful let-gen graft is a **non-additive union-find re-seed but UNOWED** (0-ary scheme =
   identity); a decorative no-op graft was declined per no-workarounds. **The pass-1 "banked HM(X)-with-
   let-gen floor" is thereby REFUTED as a category import with no referent** — 1B's LEAN build-cost survives
   **not** by grafting HM(X) but by monomorphism making HM(X)'s let-gen inapplicable. INC-1 does **not**
   re-open on build-cost. This retires the §0-void "vacuous convergence" that priced the 43–55% floors.
3. **CL-1 (CRIT-2 headline, NEW) — the INC-1 build-cost lever is WRONG-TREE.** `crates/ir` (1B's carrier)
   and `crates/core` (where all three probes ran, `-p bbnf`/`-p bbnf-ir`) are the **legacy top-level
   `crates/` that archives at GN-0** (GN-0.md:17; XL-A binding). The tree-of-record is the **unbuilt
   greenfield GN-1 loom**, seeded from **skinny** organs — not from `crates/ir`. So 1B's "additive net-0 on
   the surviving base" is a diff against an **archiving** tree; when the carrier actually lands in
   greenfield, **1B is *also* a port**. The real INC-1 lever — *which carrier organ ports most cleanly into
   the loom* — is **UNMEASURED (the loom does not exist).** INC-1 is **NOT CLOSED-for-1B on build-cost**; it
   survives **LEAN-1B on iso alone** (CL-5, HEAD-real). This de-rates the confidence, and double-homes the
   carrier port as a **GN-1 loom obligation**, not a clean intra-LOCK-1 choice.
4. **Closure-panic elegant-reduction (CL-3, NEW) — G-CRIT-α is co-owned LOCK-1⊕LOCK-7.** PROTO-1's
   "monomorphic by construction" is load-bearing but rests on beta-reduction that **does not run**: closure
   lowering `.expect`s child(4) (`mod.rs:168`) and its body tests are `#[ignore]`d. The refutation-as-law
   (finding 2) is therefore **LOCK-7-contingent**: if 7B/7C revive parametric rules / `GrammarCall`/`@host`
   into the IR, false-fusion becomes **reachable** and 1B's monomorphic soundness **voids** — the union-find
   let-gen graft re-enters and INC-1 re-opens. **Owed: G-CRIT-α — a CI-enforced monomorphization-invariant
   gate** (no parametric/`Call`/`Closure` node survives lowering into the type pass), co-owned with LOCK-7,
   so "monomorphic by construction" is a *checked* invariant, not an incidental property of a broken feature.
5. **Consumer-less WORSENED to DOUBLY-INERT (viii).** X8's `TypeClash` is **producer-inert on the real
   corpus** (the refuse arm fires only on the synthetic clash fixture — never on any production grammar →
   the four `project_types_*` byte-identity passes structurally **cannot fail**, and the obligation stream
   is **empty** on all real input) **AND consumer-less** (its only reader is 1C's *unbuilt* LSP bridge). The
   joint wave must carry a **real-grammar firing fixture**, not merely the bridge, or X8 lands a mechanism
   that on production input neither fires nor is read. X8 is also a **PARTIAL** hardening: `is_fully_ground`
   treats `Enum`/`Named` as non-ground, so two disagreeing `Named` map-results still LWW-overwrite silently.

**The HEAD-real facts that DO bank (credit where the probes RAN — not wholesale SPEC_ONLY):**
- **(a) PROTO-1's monomorphism refutation** — `IrNode` = `Ref(RuleId)` only, both uses fuse correctly,
  goldens byte-identical, runtime diff 0. Let-gen is inapplicable; 1B's LEAN build-cost survives. *Banks
  MEASURED, CONTINGENT on LOCK-7 (G-CRIT-α).*
- **(b) X8 is proven-landable, backward-safe, and cross-lock-clean** — 28 tests green in worktree; the
  refinement-aware arm permits all ~69 `BoxedEnum→structural` refinements (none frozen) and the patch
  touches only `crates/ir/src/passes/types/{domain,revise,mod,obligation}.rs` + one test — it **does not
  disturb the shared vendored `csp-solver`** (a genuine cross-lock positive). *Banks as substrate, UNSHIPPED
  at the tree; the "landed" language is corrected to worktree-only.*
- **(c) PROTO-2-iso's Sum-crossing HEAD-real** — `HeterogeneousAltJoin → type_desc_to_ts` on pristine HEAD;
  + the **decisive negative** (a fused monotype [1A] and a `TypeENode` [1D] have **no** `type_desc_to_ts`
  path). *1B leads on iso, HEAD-real.* The **Row-crossing half rides uncommitted `TypeDesc::Row` → SPEC_ONLY**
  and is an X4/LOCK-3 fix any carrier consumes (not 1B-distinctive). Iso-completeness (round-trip oracle vs
  author-asserted strings) remains a **LOCK-6 verdict** (Constitution iii) — 1B's iso is *path-existence*,
  not measured isomorphism.

---

## CURRENT DISPOSITIONS (pass-2 DEFINITIVE fold)

| Family | Status | Conv. | Δ vs pass-1 (56/54/40/44) | One-line |
|---|---|---|---|---|
| **1A · UNIFICATION-FIRST** | **ADVANCED** (carrier cand. #1, LEAN-against) | **52%** | −4 | Endorsed HM(X)-as-architecture (PROVE-08 leaf-scale). Carrier case LEAN-against on **iso alone** (a fused monotype is not a `TypeDesc` → no `type_desc_to_ts`, HEAD-real); build-cost is **UNADJUDICATED at the correct tree** (CL-1 wrong-tree, not "near-eliminated"); Option A **UNRUN**; displacement lever doubly dead; distinctiveness (let-gen) **MOOTED** by PROTO-1 but *dormant, not dead* — revives only if LOCK-7 lands un-reduced parametric IR. |
| **1B · ONE-CONSTRAINT-STORE** | **ADVANCED** (carrier cand. #2, LEADING) — single-store **BLOCKED-candidate** | **53%** | −1 | The **leading carrier CANDIDATE** — the lead is real on **iso** (Sum-crossing HEAD-real, rivals categorically eliminated) + cheapest diff *relative to the archiving base*. But **X8 unshipped ×2+re-run** (worktree-only, over-claimed "landed"); the obligation is **doubly-inert** (producer-inert on real corpus + consumer-less); the **distinguishing product-store (B3/B4) is UNRUN**; build-cost lever **wrong-tree** (CL-1); soundness **LOCK-7-contingent** (CL-3); single-store = LOCK-1⊕LOCK-2 merge. Convergence on "leading candidate," not on "shipped" or "the one heterogeneous store." |
| **1C · BIDIRECTIONAL-LOCAL** | **BANKED** (diagnostics/locality discipline, reactivation-gated) | **40%** | 0 | Correctly banked as the winner's diagnostics overlay (TS-lowerable by construction). Make-or-break **R-C2 NOT RUN** — still no input (greenfield json's 4 arrows are all scalar leaf projections, none a mismatch); GN-3.W3 inline-`(string -> key)` wall reconfirmed; consumer **WORSENED** (even once the bridge lands, 1C reads an *empty* `TypeClash` stream on production input); the **joint wave is still UNNAMED**. Deferred, not converged. |
| **1D · TYPES-AS-ECLASSES** | **BANKED** — recursion-keep + "zero new engine" + cost-extraction **RETIRED**; structural-μ hook **UNPROVEN (re-homed GN-1)** | **42%** | −2 | Engine thesis dead (three legs retired as named negatives). The surviving hook (structural `Row`-from-body derivation) is **UNBUILT** (measured `Unknown` on the real body; extraction ≠ inference) and **re-homed to the GN-1 loom** (LOCK-7⊕LOCK-3). LOCK-1's *sole* retained residue — `TypeDesc::Row`/`LayoutKind::Row` vocabulary — **does not exist at HEAD** (SPEC_ONLY, a LOCK-3 fix any carrier consumes); `union` amendment a 3-lock blast radius over relocating substrate; `TypeAnalysis` name-collision; no TS path. |

---

### FAMILY 1A — pass-2 DEFINITIVE delta (ADVANCED · 52%, −4)

**What is endorsed (unchanged).** HM(X)/OutsideIn(X) is the field's settled answer (prior-art §2A/§5),
proven at leaf scale (PROVE-08), correctly **BANKED-as-architecture**. No theory dispute.

**What cut it to the CRIT-2 floor (52).** (i) The **iso** elimination stays categorical and HEAD-real (a
fused monotype has no `type_desc_to_ts` path → 1A ranks LAST on iso). (ii) CRIT-2's CL-1 correction **removes
the "near-eliminated on build-cost" credit** the earlier draft leaned on: 1A's port-cost inflation was
measured against the **archiving** `crates/ir`; the greenfield-loom port cost of the 1A organ vs the 1B organ
is the *real* INC-1 lever and is **UNMEASURED**. So 1A is **not near-eliminated on build-cost — build-cost is
unadjudicated at the correct tree.** (iii) 1A's one distinctive rightness (let-gen / false-fusion soundness)
is **MOOTED** by PROTO-1 — but *dormant behind a LOCK-7 grammar-closures decision*, not dead: it revives if
closures ever land as un-reduced parametric IR.

**Open gaps (updated).**
- **G-1A-1 [port-cost UNRUN + WRONG-TREE].** Option A (union-find re-seed) never built; the parity/wall-time
  race is SPEC_ONLY *and* was framed against the archiving base — the greenfield-loom port cost is the real
  question and cannot be measured until GN-1 exists.
- **G-1A-2 [displacement lever doubly dead].** `TypeDomain::join` 0 call sites (live carrier `assign`, sound);
  1A's "running CSP overwrite-unsound" tilt targets dead code — unearned. Reconfirmed at HEAD.
- **G-1A-3 [distinctive advantage MOOTED-but-CONTINGENT].** G3/let-gen mooted by PROTO-1 (no false fusion to
  prevent) — contingent on the LOCK-7 closure-staging decision; 1A's carrier case has no *present* distinctive
  win, only distinctive costs.
- **G-1A-4 [`HeterogeneousAltJoin` retirement unproven].** No probe shows the Join variant retiring under
  unification vs coexisting with a union-find structural arm.
- **G-1A-5/6 [width + row unmeasured].** Zero row-shaped `->` in the real corpus (json arrows are scalar leaf
  projections); Leijen-vs-Gaster-Jones unfalsifiable on this instrument; width residue unrun.

**Pass-3 directives.** (1) **Do not build Option A before the carrier axis is declared dispositive** — if iso
governs (LOCK-6's call), 1A is pre-eliminated categorically and the port need not be built; if build-cost
governs, the port must be raced **on the greenfield loom** (not the archiving base), which does not yet exist
→ ratify 1A as **banked-architecture / not-carrier** and stop scoring its unbuilt parity race, OR build Option
A only once GN-1 exists. (2) Only if 1A survives the axis choice: build the direct nominal-fold prototype,
name the LOCK-3 sink (`TypeAnalysis::from_ir`) + TS mirror it must reach, run the width probe, pick the row
discipline.

**Withheld from researchers:** do not re-derive HM(X) stratification (settled); do not re-argue union-find
dominates equality asymptotically (settled, prior-art §2B); do not build Option A before the carrier axis is
declared, nor measure its port-cost against the archiving `crates/ir` (CL-1 — wrong-tree).

### FAMILY 1B — pass-2 DEFINITIVE delta (ADVANCED · 53%, −1 — LEADING carrier CANDIDATE)

**What advanced (HEAD-real, durable).** PROTO-2-iso banks 1B's **Sum-crossing** as HEAD-real and eliminates
the rivals categorically on the same instrument — 1B is the only carrier whose record already crosses to TS.
On the iso axis 1B leads decisively (path-existence; iso-*completeness* is a LOCK-6 verdict). PROTO-1's
refutation-as-law **preserves 1B's LEAN build-cost** by making let-gen inapplicable — 1B ships false-fusion
soundness *vacuously*, not by carrying an unbuilt graft.

**What held it to the CRIT-1 floor (53, below CRIT-2's 58).**
- **G-1B-1 [X8 WORKTREE-not-tree-of-record — the escalated "landed" is refuted].** At HEAD the index is empty,
  `assign` is the raw LWW, `TypeClash`/test-file absent. X8 is **proven-landable + PASS-in-worktree**, NOT
  shipped. Do not score closed (Constitution vii, XL-H). Two passes + a re-run and the one carrier-independent
  deliverable has reached the tree of record with **nothing**.
- **G-1B-2 [DOUBLY-INERT obligation, viii, WORSENED].** `TypeClash` is producer-inert on the real corpus (the
  refuse arm never fires on any production grammar → byte-identity passes *cannot fail*, stream is empty) AND
  consumer-less (only reader = 1C's unbuilt LSP bridge). The joint wave must carry a **real-grammar firing
  fixture**, not merely the bridge.
- **G-1B-3 [X8 is PARTIAL].** `is_fully_ground` treats `Enum`/`Named` as non-ground → two disagreeing `Named`
  map-results still LWW-overwrite silently. Whether ground-only is the complete X8 or a first slice is open.
- **G-1B-4 [DISTINGUISHING thesis B3/B4 UNRUN, spec-cites-itself].** Neither probe touched the product-lattice
  residue census (B3) or the per-variant `Sum` store shape (B4). "Residue ≈ 0" still reads its own coarse
  output over a **zero-overload corpus**, unchanged across two passes. What is proven is the *structural-
  equality* store 1B already was; the namesake ("one store, five axes") is **unexercised**.
- **G-1B-5 [build-cost lever WRONG-TREE, CL-1].** 1B's "additive net-0" is measured against the archiving
  `crates/ir`; the greenfield-loom port cost is unmeasured. **INC-1 is NOT CLOSED-for-1B on build-cost** — it
  survives LEAN-1B on **iso alone**.
- **G-1B-6 [soundness LOCK-7-contingent, CL-3].** Monomorphic-by-construction soundness holds only while
  closures stay pre-IR-reduced; grammar-closures is a live design line. If parametric rules revive, the
  let-gen graft re-enters and INC-1 re-opens.
- **G-1B-7 [single-store = GESTALT-B, X3].** Literal one-heterogeneous-store = a LOCK-1⊕LOCK-2 merge over the
  shared vendored `csp-solver = "0.1"` (verified in both crates) — GN-0.W2 physically relocates it to
  `crates-infra/`. Caps 1B's within-LOCK-1 convergence below 100%; the namesake cannot be seated here.

**Pass-3 directives.** (1) **LAND X8 into the tree of record** (raw LWW → refinement-aware arm) — the form
that refuses concrete-vs-concrete but permits `BoxedEnum→structural` (the naive refuse-all freezes ~69
refinements, measured); confirm `types_heterogeneous_alt` green + `project_types_*` byte-identical + runtime
`git diff` empty. **BUILD (not co-name) the joint LSP wave** with 1C, including a **real-grammar firing
fixture** so `TypeClash` is neither producer-inert nor consumer-less; decide **ground-only vs full X8** (author
an `Enum`/`Named`-level disagreement fixture). (2) **Run the DISTINGUISHING probes B3/B4** on a *modeling*
instrument (real overload/legality/decode-class) — the sharpest uncertainty the pass skipped; this is what
moves 1B past its equality-carrier ceiling. (3) **State the Map-barrier posture** (XL-C holds 5B's functor
retirement on a LOCK-1 decision LOCK-1 has not made — surface it, don't silently contradict).

**Withheld from researchers:** do not re-assert "1B collapses to 1A / one architecture" — PROTO-1 refutes the
let-gen premise, and the collapse is architecture-level not carrier-level; do not propose csp-solver *engine*
features (stay consumer-side, add domains); do not measure the "cheapest diff" against the archiving
`crates/ir` (CL-1); do not re-run the equality-carrier axes X8/iso already settled — run B3/B4.

### FAMILY 1C — pass-2 DEFINITIVE delta (BANKED · 40%, held)

Correctly BANKED as the winning trunk's diagnostics/locality overlay (mechanism clear, TS-lowerable by
construction). **Unmoved** — the decisive experiment (R-C2) was not the probe target; a pass that resolved
1B's easy axes while leaving 1C's make-or-break untouched has **deferred** 1C, not converged it.

**Open gaps (held/worsened).**
- **G-1C-1 [make-or-break has NO INPUT].** No typed-mismatch `->` corpus; json's 4 arrows are scalar leaf
  projections. 1C's distinct identity ("check moves the span to the `->`") is unfalsifiable until a hand
  fixture exists. Blocks on GN-3.W3 or a hand-authored mismatch.
- **G-1C-2 [GN-3.W3 wall].** Inline `(string -> key)` trips the typed-materialization invariant
  (`lower/expression/wrap.rs`); only the rule-level form clears it — caps 1C's addressable surface (a
  LOCK-3⊕GN-3 dependency, not intra-1C).
- **G-1C-3 [consumer WORSENED, viii].** 1C's span-obligation and X8's `TypeClash` share the same unbuilt LSP
  bridge; per G-1B-2 the stream is *empty on real grammars*, so even once the bridge lands 1C consumes an
  empty stream on production input. **The joint wave must carry a firing fixture, not just the bridge** — and
  neither exists at HEAD. The wave is still UNNAMED/unscheduled.
- **G-1C-4 [sub-span ceiling, a LOCK-3 field].** `Project.ty` is one whole-expr `String` span; a wrong
  ascription cannot underline the offending sub-term — the quality ceiling is a sub-span the IR does not carry.

**Pass-3 directives.** (1) **Hand-author the mismatch fixture** (`frac = digits "." digits -> Ratio`, body
synthesizes a two-`Span` `Seq`, `Ratio` declared scalar) and **run R-C2** — print synthesis-first (reports at
a synthesized interior `NodeId`) vs check (reports at the `->` span) side by side; if check does not move the
span, 1C retires to synthesis-first. (2) **NAME AND SCHEDULE the joint wave** — the `PassDiagnostic.span` add
+ the `type_obligations → lsp_types::Diagnostic{range}` bridge + a **real-grammar firing fixture**, jointly
with 1B's X8 (the shared consumer), regardless of carrier winner.

**Withheld from researchers:** do not re-argue 1C as a distinct fourth engine (banked); do not develop the
width seam as a separate subsystem (1C only *consumes* the resolved width at the subsumption site).

### FAMILY 1D — pass-2 DEFINITIVE delta (BANKED · 42%, −2)

**Engine thesis dead; three legs RETIRED as named negatives** (recursion-keep beats only a strawman —
incumbent finite+terminating on JSON/Sheets/CSS L4; "zero new engine" FALSIFIED — `union` amendment reverted
at HEAD, 3-lock blast radius; cost-extraction IDLE — overload residue = 0). No dispute.

**What cut it (−2).** LOCK-1's **sole** retained artifact from 1D — `TypeDesc::Row`/`LayoutKind::Row` — is
verified **ABSENT at HEAD** (0 hits; only `HeterogeneousAltJoin`). The residue the earlier draft retained
*for LOCK-1* is itself **SPEC_ONLY at the tree of record** — a promise contingent on the GN-1 loom, not a
landed fact. The surviving hook (structural `Row`-from-body derivation) is **UNBUILT** and its home is not
LOCK-1: on the real `object` body the classifier returns `Unknown`; deriving `Row` needs a `->`-declared
`key` role (engine-agnostic — any carrier reads it). **Extraction ≠ inference.**

**Open gaps (updated).**
- **G-1D-1 [D1 derivation UNBUILT, re-homed GN-1].** True make-or-break; not run; a LOCK-7⊕LOCK-3 codepath-
  unification question, not a LOCK-1 recursion race.
- **G-1D-2 [retained residue X4 SPEC_ONLY at HEAD].** `TypeDesc::Row`/`LayoutKind::Row` do not exist at HEAD —
  a real open IR fix (a masked-fallback the ledger keeps RED), not a done deal.
- **G-1D-3 [name-collision, vi].** Net-new `TypeAnalysis` collides with the live `driver/analysis.rs:22`
  (+ a 2nd `from_ir` at `named_types.rs:100`).
- **G-1D-4 [no TS path, iii].** `TypeENode` is not a `TypeDesc` → no `type_desc_to_ts` mirror; ranks last on
  iso with 1A.
- **G-1D-5 [3-lock blast radius over relocating substrate, CL-6].** The `union` amendment fires on LOCK-2
  grammar-tier + LOCK-5 regex-tier — a GESTALT-B/LOCK-2/LOCK-5 saturation-perf call, over substrate GN-0.W2
  relocates to `crates-infra/`. Never a free LOCK-1 hook.

**Pass-3 directives.** (1) **[GN-1 / GESTALT-B, NOT a LOCK-1 brief]** the structural axis stays re-homed; the
old "land Row then delete the `has_rule` mask and diff byte-identical" brief is **WITHDRAWN** (a
gate-that-cannot-pass on the retiring/archiving substrate — correct it into a named GN-1 supersession,
Constitution ix). LOCK-1's residue is ONLY the engine-agnostic `TypeDesc::Row` vocabulary widening (a LOCK-3
record-expressiveness fix). (2) The `union` amendment stays a 3-lock GESTALT-B/LOCK-2/LOCK-5 saturation call —
must not silently re-enter via 1D's saturation claims. **Reactivate 1D only if the GN-1 loom proves the
structural hook NEEDED AND e-graph-specific — which a recursion race cannot decide.**

**Withheld from researchers:** "zero new engine" is RETIRED (do not re-pose); do not carry `union()` as a
free LOCK-1 hook; do not model width as distinct `Int8`/`Int16` constructors; do not re-run the strawman
recursion race (retired); do NOT re-run the withdrawn mask-deletion brief against the archiving substrate.

---

## REDIRECT / CROWDING DECISIONS (pass-2 definitive)

1. **The banked HM(X)-with-let-gen floor is REFUTED-as-inapplicable, not merely "unbuilt."** Pass 1 banked
   HM(X) (equality trunk + bounded residue + **let-generalization**) as the settled floor. PROTO-1's re-run
   proves the running carrier is **monomorphic by construction** (`Ref(RuleId)` only), so let-gen is a
   *category import with no referent* — false-fusion is vacuous, the graft is unowed. **Bank the equality-
   trunk + bounded-residue architecture as CONSENSUS; strike the let-gen leg as inapplicable to this
   grammar type system.** This is not "build the floor next pass" — it is "the floor was a phantom
   requirement," which retires the vacuous-convergence that priced the 43–55% floors. **Contingency:** the
   refutation is LOCK-7-gated (G-CRIT-α) — if parametric rules revive, the leg re-opens.
2. **The carrier fork stays OPEN (1A 52 vs 1B 53, within noise) — LEAN-1B on ISO, build-cost UNADJUDICATED.**
   No carrier is force-banked. The prior draft's two-axis frame (build-cost ∥ iso) is corrected: **build-cost
   is wrong-tree (CL-1) and unmeasured at the greenfield loom**; the fork survives LEAN-1B **on iso alone**
   (CL-5, HEAD-real). Pass 3 **must not demand Option A on the archiving base** — either declare iso
   dispositive (LOCK-6's verdict → 1A pre-eliminated, fork nearly closed FOR 1B) or defer the build-cost race
   to when GN-1 exists. The iso seat routes to **LOCK-6/GESTALT-B** to render (iso-completeness, not
   path-existence, is LOCK-6's call).
3. **1C → BANKED (held).** Reactivation gate: R-C2 head-to-head on a hand-authored mismatch fixture, co-landed
   with the named LSP wave + a real-grammar firing fixture.
4. **1D → BANKED (held), structural axis RE-HOMED to GN-1.** Not a LOCK-1 recursion race — a LOCK-7⊕LOCK-3
   codepath-unification build; the retained `TypeDesc::Row` residue is SPEC_ONLY at HEAD.

**Underexplored formulations — the register updated (crowding redirect):**
- **[RESOLVED-as-refuted this round] The let-generalization graft into the monotone CSP.** Pass-1/earlier-
  draft opened it as the deepest unbuilt piece. PROTO-1's re-run **CLOSES it in the negative**: there is no
  polymorphic surface to graft onto; a meaningful graft is a non-additive re-seed but **unowed**. No longer
  underexplored — it is a **refutation-as-law** (LOCK-7-contingent). *Redirect: open **G-CRIT-α** (the
  CI-enforced monomorphization-invariant gate) as the successor thread, co-owned LOCK-1⊕LOCK-7.*
- **[NEW — the underexplored formulation to OPEN] 1B's product-store distinguishing thesis (B3/B4).** The
  entire second-round probe budget bought the **1B-favorable equality axes** (X8, iso, let-gen) — all three
  on which 1B was already ahead — while 1B's **namesake** (one store carrying equality + width + decode +
  legality + backend-shape; the per-variant `Sum` store shape) went **UNRUN for two passes**. This is the
  sharpest untested uncertainty and the only formulation that distinguishes 1B from "the structural-equality
  store it already was." **Open B3/B4 on a modeling instrument as the first-class pass-3 thread.**
- **[held-open] Open X (pluggable residue solver)** — UNRUN two passes AND double-homes with LOCK-2's
  `csp_strategy` CSP (2C, same vendored `csp-solver`, GN-0.W2-relocated). Route the double-home to GESTALT-B;
  run the width micro-fixture on a modeling instrument before "defaulting dominates" is banked. **G4 (Open-X
  two-mode seam) is DEMOTED to "staged for GESTALT-B"** — it can be staged in LOCK-1, never closed here.
- **[held-open] Open S (attribute-grammar cross-rule seam)** — asserted-as-reading, still unproven; either
  demonstrate the running `mod.rs` cycle-break IS a monotone-lattice circular AG or stop counting it resolved.

## CROSS-POLLINATION (pass-2 definitive)

**1C's bidirectional check/synthesize ⟶ the winning trunk (1A or hardened-1B) — STANDS** (both families
developed enough; the graft is named: `infer_expr → synth_expr`, `check_expr` overlays, the trunk's equality
reconciler shared). *No new graft this pass.* The deferred 1D μ-extraction graft stays deferred (the
comparison is unbuilt; the parity probe retired it; re-evaluate only if the GN-1 loom revives 1D's structural
hook). PROTO-2-iso's Sum-crossing banks as **evidence** (1B's iso is HEAD-real), not a graft. **NEW cross-lock
graft named, not this-pass-buildable:** X8's `TypeClash` + 1C's span-obligation ⟶ the **one joint LSP wave**
(shared consumer bridge + a shared real-grammar firing fixture) — co-scheduled, carrier-independent.

## CROSS-LOCK / GESTALT DIRECTIVES (pass-2 definitive — supersedes the intra-pass draft where noted)

1. **CL-1 [NEW, the largest correction] — re-base INC-1 onto the tree of record.** Strike "1B runs on the
   surviving base": `crates/ir` archives at GN-0 (GN-0.md:17; XL-A). Re-frame the build-cost lever as
   *greenfield-loom port cost of the 1B organ vs the 1A organ* and mark it **UNMEASURED until GN-1 exists.**
   Do **not** record INC-1 CLOSED-for-1B on build-cost; it survives LEAN-1B on **iso alone** (CL-5). Double-
   homes the carrier port as a GN-1 loom obligation.
2. **CL-3 [NEW] — co-own G-CRIT-α with LOCK-7.** PROTO-1's monomorphism refutation-as-law is contingent on
   LOCK-7 not reviving parametric rules into the IR (closures panic at HEAD — `mod.rs:168`,
   `simple_kinds.rs:199`; tests `#[ignore]`d). Route a named-supersession note to LOCK-7 (7B/7C): *if
   `GrammarCall`/`@host`/parametric rules survive lowering, false-fusion reopens and 1B's carrier soundness
   needs the union-find graft after all.* Fix the closure panic or ledger the surface frozen-monomorphic,
   CI-gated (G-CRIT-α).
3. **X8 [third call] — LAND into the tree of record + BUILD the joint LSP wave.** Unshipped after two passes +
   a re-run; the probe's "landed/staged" is worktree-only (index empty at HEAD). Co-name **and schedule** the
   1C bridge; carry a **real-grammar firing fixture** (else producer-inert + consumer-less); decide ground-
   only vs full X8. Cross-lock-clean (does not touch the shared `csp-solver`).
4. **X2/XL-F — the LOCK-3 sink is `TypeAnalysis::from_ir` (held).** Whichever record wins lowers through it (+
   the 2nd `from_ir` at `named_types.rs:100`); 1D's net-new `TypeAnalysis` is a name-collision (vi).
5. **X3 / CL-2 — 1B single-store = LOCK-1⊕LOCK-2 merge over the shared `csp-solver` (held, verified, firmed).**
   GN-0.W2 relocates `csp-solver` to `crates-infra/`; the residue seat (Open X) double-homes with 2C. Both
   route to **GESTALT-B**; bank the literal single-store thesis, ship the federation.
6. **X4 / CL-5 — `TypeDesc::Row`/`LayoutKind::Row` is a LOCK-3 record-expressiveness fix, engine-agnostic
   (held); land it into HEAD so the iso Row-crossing ceases to be worktree-only.** Widening alone does **not**
   DERIVE the fact (the classifier returns `Unknown` on the real body) — the derivation is the GN-1 obligation.
7. **X9 — the structural axis (`List ≠ Row`) is a GN-1 LOOM obligation (LOCK-7⊕LOCK-3), routed to GESTALT-B.**
   The mask-deletion brief is WITHDRAWN as a gate-that-cannot-pass on the retiring/archiving substrate;
   correct it into a named supersession (Constitution ix).
8. **X6 / CL-6 — 1D's `union` amendment is a 3-lock blast radius (held):** LOCK-2 grammar-tier + LOCK-5
   regex-tier over substrate GN-0.W2 relocates; needs LOCK-2/LOCK-5 sign-off + a 3-tier saturation-perf
   measurement.
9. **X7 / Constitution iii — the TS path constrains the record (held, HEAD-measured on the Sum half):** only
   `TypeDesc` crosses `type_desc_to_ts`; monotype (1A) and `TypeENode` (1D) have no TS mirror. Iso-
   **completeness** (round-trip oracle) remains a **LOCK-6 verdict** — 1B's iso is path-existence, not
   measured isomorphism.
10. **Homing hygiene — DEMOTE G4 (Open-X two-mode width seam) to "staged for GESTALT-B."** It can be staged in
    LOCK-1, never closed here; else it is a gate-that-cannot-pass-in-this-lock.
11. **Constitution v (zero-runtime) — exercised only on WORKTREE changes, never on a LANDED change** (empty
    index means it has never been exercised on a committed change). Named gap for all four families.

**Lock-wide (pass 2 of ≥3).** No family is convergence-eligible (no clean-pass streak; floors 40–53%). This
definitive re-fold is an honest advance over the intra-pass draft: it **catches the X8 tree-of-record
over-claim** (proven-landable-in-worktree ≠ shipped; the index is empty at HEAD), **re-bases INC-1 off the
archiving `crates/ir`** (CL-1 — build-cost is wrong-tree, the fork survives LEAN-1B on iso alone), and
**names the closure-panic contingency** (CL-3 — the monomorphism refutation is LOCK-7-gated, G-CRIT-α). The
budget went to the **1B-favorable equality axes** (X8, iso, let-gen) — none of which shipped to the tree —
while the **distinguishing product-store (B3/B4)** and **1C's make-or-break (R-C2)** went unrun for a second
pass. The durable, MEASURED banks: **PROTO-1's refutation-as-law** (let-gen inapplicable to the monomorphic
carrier — 1B's LEAN build-cost survives, LOCK-7-contingent); **PROTO-2-iso's Sum-crossing + monotype/
TypeENode-have-no-TS negative** (1B leads on iso, HEAD-real); and **X8 proven-landable-and-backward-safe-in-
worktree, cross-lock-clean, but UNSHIPPED.** The honest ceiling: LOCK-1 banks **1B as the leading carrier
CANDIDATE** but cannot *seat* the carrier — single-store, iso-completeness, and residue all route to
GESTALT-B / LOCK-6; the structural axis is re-homed to GN-1; and X8 must actually land. The roof is "1B
ranked leading, seat exported, deliverable unshipped," ~53%.

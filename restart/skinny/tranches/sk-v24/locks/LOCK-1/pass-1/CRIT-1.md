# LOCK-1 — THE SOLVER SPINE · pass-1 · CRIT-1 (adversarial critic 1 of 2 — WITHIN-LOCK view)

*Charge: harden/challenge/refine every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself circularity, gates that cannot fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate) + constitution
violations + prototype-evidence misuse. Critic 1 weights internal soundness of each family spec
and whether the prototypes actually tested the sharpest uncertainty. Every load-bearing claim
re-verified against HEAD this pass, including the two probes that ran AFTER the prior critic
draft (PROTO-1B-clash-run2 09:39, PROTO-2-recursion-parity-probe2 09:39).*

---

## 0. WHAT I VERIFIED AT SOURCE THIS PASS (six facts, independently confirmed)

I re-ran every structural check rather than trust the reports. All six load-bearing facts hold:

| # | claim | verification at HEAD | verdict |
|---|---|---|---|
| 1 | `TypeDomain::join` is dead on the type path | `grep '\.join(' crates/ir/src/passes/types/` → **zero call sites**; impl at `domain.rs:66` only | **CONFIRMED dead** |
| 2 | live equality carrier is `assign`, last-writer-wins, no clash channel | `revise.rs:27` verbatim: `*slot = Some(ty); true`, no disagreement check | **CONFIRMED** |
| 3 | the general pipeline cannot express `List ≠ Row` | `registry/struct.rs:58` `LayoutKind ∈ {Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}` — **no Row/Map variant** | **CONFIRMED — decisive for 1D** |
| 4 | zero `->` in every in-tree grammar | all 8 `.bbnf` (json/css/sheets/bbnf/ebnf/bnf/…) → `grep -c '\->'` = **0** each | **CONFIRMED** |
| 5 | 1D's `union` amendment is reverted at HEAD | `egraph.rs:235-237` carries the `if changed { push }` guard — the **old** form, not 1D's unconditional push | **CONFIRMED reverted** |
| 6 | typed per-document grammars (twitter/citm/canada) do not exist | only `.json` test data + criterion dirs; **no `.bbnf`** | **CONFIRMED absent** |

**The dominant finding: the newest probes REFRAME the lock a second time, and this critique's own
predecessor (CRIT-1 @ 05:07) is now partly overtaken exactly as FAMILY-SPECS was.** The 05:07
critic confidently pushed 1D BANK→**RETIRE** ("retire 1D harder than the spec allows"). The
09:39 probe (`PROTO-2-recursion-parity-probe2`) **inverts that** by checking a step the 05:07
critic accepted on faith. Both the spec's KEEP-on-strawman and the prior critic's RETIRE-on-
"structure-supplied-downstream" rested on **the same unexamined assumption** — that the general
pipeline delegates `List ≠ Row` to a designed downstream consumer. probe2 traced *where*
downstream and found: it is delegated to a **bespoke per-grammar hand-written codegen template**
(`skinny/crates/codegen/src/json_templates/value.rs`, hard-matching raw `{ } [ ]`), dispatched by
a per-grammar table (`json → regen-json`). That is the workaround pattern the standing laws
(`no-workarounds`, `typed-materialization-invariant`) count *against*. So neither prior verdict
survives contact with the evidence; 1D's disposition is **genuinely contested-definitional**, and
the contest turns on an architecture choice (grammar-authoritative vs codegen-distributed) that
LOCK-1 cannot settle alone.

### 0.1 The dead-code defect is now DOUBLY confirmed — the carrier fork stays wide open
run2 independently reproduces §0.2 of the prior critique and adds a **non-commutativity proof**:
`ground(Bool).join(ground(F64))` overwrites to `F64`; the reverse order lands `Bool` — so the
disagreement arm (`domain.rs:72-77`, verified verbatim) is a non-commutative last-writer-wins,
NOT a lattice LUB. But it is **dead** (fact 1). The live clash site (Alt join) routes through
`join_types` → `HeterogeneousAltJoin` + a named obligation — raise-on-disagreement, verified
sound by run2's live-pass test (`ir.types[0] = HeterogeneousAltJoin([Bool, F64])`, `1 ≥ 1`
obligation). **Consequence unchanged and now twice-measured: the spec's single strongest lever
for preferring 1A's union-find carrier over the running CSP — "the running CSP is unsound as
shipped" — is void. The carrier fork is MORE open than the spec claims, not less.**

*(One probe-hygiene nit: run2 writes "`join` returns `Some(Bool)`". The signature is `-> bool`
(verified `domain.rs:66`); it returns `true` and leaves the slot `Some(Bool)`. Loose wording,
correct substance — flagged for registry accuracy, not a finding.)*

---

## 1. THE CROSS-CUTTING FINDING — THE THIN VERDICT IS STILL MEASURED ON A CORPUS THAT LACKS THE PHENOMENON, AND NOW ON A PIPELINE THAT CANNOT EXPRESS IT

The spine verdict ("1A + ordered defaulting lattice; retire 1B one-store and 1D cost-extraction
on the evidence") rests on PROTO-1's THIN census. Two structural defects, both re-verified, plus
a third the newest probe exposes:

1. **The instrument cannot register the quantity (gate that cannot fail).** "Genuine
   multi-valued residue" is read only from `HeterogeneousAltJoin`. Width/capacity/decode-class
   are **pre-tagged single-valued by the taxonomy**; numeric bit-width — the exact judgment the
   fork centers on (prior-art §2G, PROVE-08's admitted U8Tag→u32 loss) — **is not a census
   category at all**. A null reading from a blind instrument is not a measured zero.

2. **The corpus lacks the feature under test (fact 4, verified).** Zero `->` in every in-tree
   grammar; twitter/citm/canada typed grammars absent (fact 6). The spec marks 1A's row-poly and
   1C's diagnostics "unfalsifiable / no corpus" — **then still treats THIN as load-bearing for
   retiring 1B and 1D on the same corpus.** You cannot declare the corpus too `->`-poor to
   *measure* 1A/1C and rich enough to *retire* 1B/1D.

3. **NEW — the census pipeline provably cannot express the richest structural distinction there
   is (fact 3).** `LayoutKind` has no Row variant, so `array` (list-of-value) and `object`
   (row-of-kv) are **structurally forced to the same `Struct` layout** — confirmed in `ir.types`
   AND the `struct_registry` sidecar (probe2). The census counts "residue" over an inference
   engine that **cannot represent `List ≠ Row`**; its THIN reading partly reflects an
   *expressiveness ceiling of the instrument*, not a thinness of the domain. This is the
   elegant-reduction trap in evidence form: "the residue is thin AND-THEN the hard part —
   whether the domain has structure the current engine simply cannot name — is measured by the
   engine that cannot name it."

The 1A-spine conclusion is prior-art-consonant and probably right, but pass-1 earned it by
framing on the wrong corpus and the wrong instrument, not by measurement.

---

## 2. FAMILY 1A — UNIFICATION-FIRST · convergence 64%

Prior-art-endorsed architecture (research-priorart §5), proven seed (PROVE-08), and the running
pass *is empirically a terminating nominal fold* (probe2: JSON/Sheets/CSS L4 all close recursion
nominally, finite). Internally the soundest *architecture*. **But the spec fuses two different
1A's, and the load-bearing one is unproven and lost its justification twice this pass.**

The conflation: "1A ADVANCE (the spine)" fuses (i) *1A-the-stratified-architecture* — union-find
trunk + bounded residue solver + let-generalization — which prior art and the incumbent both
support; with (ii) *1A-the-union-find-carrier* that **deletes the running root CSP and re-seeds
equality from PROVE-08's `hm.rs`**. (i) is well-evidenced; (ii) is an in-scope migration the
spec's own §5.1 admits is 1A's "quietest liability," and its justification (running CSP unsound
as shipped) is now **doubly dead** (§0.1).

**Enumerated open gaps:**
1. **Carrier-displacement (O3) lost its load-bearing argument, now twice-measured.** The reason
   to delete the running CSP for union-find was "the CSP is overwrite-unsound as shipped." Fact 1
   + run2 refute it (dead code + sound live path). The residual case for union-find-as-carrier is
   prior-art familiarity + "raises clashes correctly" — which the running Alt-join *also* does.
   **The carrier fork is unresolved and the spec's tilt is unearned.**
2. **Row discipline (O1) unresolved AND structurally unmeasurable.** Leijen vs Gaster–Jones,
   principal-type-critical (Wand's trap), **zero `->` corpus** (fact 4) to decide it. Sequences
   behind GN-3.W3.
3. **Numeric-width residue (O2): CSP vs defaulting — UNMEASURED, not resolved.** The census
   cannot see it (§1.1). The claim "defaulting dominates" is plausible and prior-art-consonant,
   but not established on the pass's own instrument.
4. **No direct 1A prototype — every gate (G1–G6) asserted.** The nominal fold's termination is
   evidenced only *second-hand* via the running incumbent (probe2), not via 1A-union-find code.
5. **The residue-thinness argument now inherits fact 3.** 1A's "the residue is a defaulting
   lattice, not a CSP" is measured on a pipeline that cannot express `List ≠ Row`; the argument
   is not falsified, but its evidentiary base is weaker than the spec presents.
6. **Generalization criterion (value restriction) named, not stated; zero-runtime (v) asserted,
   not exercised** — no probe drove `git diff crates/runtime/` empty for 1A.

---

## 3. FAMILY 1B — ONE-CONSTRAINT-STORE · convergence 63%

The best-*evidenced* family (runs, both backend consumers, end-to-end tests), and run2
**strengthens it a second time** — the live equality carrier (Alt join) is raise-on-disagreement,
sound, now proven by two independent fixtures. Yet the spec, frozen before both clash probes,
brands 1B's equality "unsound as shipped" and leans on that to tilt toward 1A. **The spec is
pessimistic about dead code.**

**Enumerated open gaps:**
1. **"The confirmed shipping defect" is REFUTED, twice (§0.1).** `TypeDomain::join` is dead;
   the live clash site raises correctly. Latent hazard: `assign` last-writer-wins under an
   uncapped monotonic loop (two incompatible grounds would oscillate) — but **unreachable** for
   clashes under well-formed tree IR. Gap: land the additive `assign`/`join` `TypeClash`
   hardening (run2's minimal repair) regardless of carrier — cheap, converts the latent hazard
   into an explicit obligation. **Untested for backward-safety** against the shipping consumers.
2. **The single-store thesis is genuinely BLOCKED-candidate (R-B1) — and R-B1 was never built by
   any probe across all runs.** The decode-class-depends-on-sum-variant counterexample the
   research demanded be built both ways was built **neither** way. The family's sharpest design
   decision (federation vs one tag-dispatched heterogeneous domain) remains pure paper.
3. **The census retiring 1B's distinctness is 1B's OWN output (spec-cites-itself circularity).**
   PROTO-1 reads richness from 1B's solved `HeterogeneousAltJoin`; and per fact 3 that output
   cannot even *represent* `List ≠ Row`, so "1B's residue is thin" is partly "1B's type-of-record
   is too coarse to hold structure" — a circular and instrument-limited measurement.
4. **The inverted carrier fork — the pass's most important unadjudicated question.** With §0.1
   removing "seed 1A's union-find instead," 1B-as-running-CSP-hardened is a live candidate for the
   carrier seat *itself*: it keeps both backend consumers, has a sound live clash path, needs only
   additive hardening. The spec's "federation IS 1A wearing a CSP coat" may have it backwards —
   the cheapest sound spine may be *the running CSP hardened*, not a union-find re-seed that
   deletes running code.
5. **Zero-runtime (v) asserted, not exercised.**

---

## 4. FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence 52%

Least converged, structurally **untestable on the current corpus**. The FOLD disposition (1C is a
checking-front + SCC-seam + span-field over the winning engine, not a fourth engine) is correct
and prior-art-consonant. But its entire raison d'être is unmeasured and *unmeasurable* today.

**Enumerated open gaps:**
1. **The make-or-break diagnostic head-to-head (G2) CANNOT RUN — zero `->` corpus (fact 4) and no
   typed grammars (fact 6).** 1C's distinct value is **structurally unfalsifiable today**, not
   merely un-run. The disposition is a bet, not a finding — and it has been skipped across every
   brief-cycle in the pass.
2. **On zero-`->` grammars, 1C ≡ its host engine (spec-admitted degeneracy).** Addressable
   surface near-empty; PROTO-1's ≤2% hetero-residue even on CSS L4 bounds it further.
3. **Substrate gap real, unbuilt.** `PassDiagnostic` is `RuleId`-only; 1C's headline needs a
   `span` field. Additive, untouched — bank regardless of engine winner.
4. **Sub-span granularity ceiling (O3).** `Project.ty` is one `String` carrying whole-expr span;
   a wrong ascription cannot underline the `T` — the quality ceiling on 1C's whole thesis is a
   sub-span the IR does not carry.
5. **Is 1C a family at all?** It reuses PROVE-08's `Unifier` verbatim → per the shared-mechanism
   rule, closest kin to 1A → a discipline layer, not an engine center. The 52% is a ceiling until
   a typed corpus lets G2 run.

---

## 5. FAMILY 1D — TYPES-AS-ECLASSES · convergence 48%

**The disposition inverts a second time, and my sharpest new finding lands here: 1D's KEEP rests
on an unbuilt inference step of the same difficulty as the workaround it claims to eliminate.**

probe2 reframes 1D from "cosmetic recursion nicety" (prior critic's RETIRE) into a real
structural-fidelity question: the general pipeline provably cannot express `List ≠ Row` (fact 3,
verified — `LayoutKind` has no Row variant), and today that distinction is recovered by a bespoke
per-grammar hand-written codegen template — a workaround the standing laws disfavor. So far the
KEEP-lean is fair. **But probe2, the spec, and the prior critic all miss the load-bearing hole:**

- **The elegant-reduction trap, unexposed until now.** PROTO-2 and probe2 both prove 1D's
  *extraction*: given an e-graph already containing a `Row(#0)` node, extraction yields a finite
  `Sum[6]` with `List(#0) ≠ Row(#0)`. But the fixture was **handed** the `Row` constructor. **No
  probe demonstrates 1D's *inference* — that a `TypeENode` Language can classify the `object`
  rule body (a repeat-of-kv-pairs) as `Row` rather than `List` WITHOUT the same semantic hint the
  bespoke template encodes by hand-matching `{ }` vs `[ ]`.** That classification is precisely the
  workaround's content. If 1D needs a per-grammar hint to know `object` is a Row, it does not
  *close* the workaround — it *relocates* it into the type Language. 1D's headline capability
  ("derive `List ≠ Row` from the grammar") is therefore **asserted, not proven**, and the missing
  primitive (repeat-of-pairs → Row inference) is **a wall of the same character as the original
  problem** — the BLOCKED-candidate test, on the axis that actually decides the KEEP.

**Enumerated open gaps:**
1. **"Zero new engine" FALSIFIED and reverted at HEAD (fact 5, verified).** The injectivity hook
   needs the `union`-scheduling change to the *shared* engine LOCK-2/LOCK-5 consume; HEAD carries
   the old guard. 1D's (a)–(e) proof rests on off-tree code — a reproducibility obligation and a
   cross-lock cost, not free reuse.
2. **NEW (the elegant-reduction trap above): the Row-inference step is unbuilt and of the same
   difficulty as the workaround.** 1D's structural-fidelity win is real only IF the type Language
   can *derive* Row-ness; that derivation was never prototyped. Until it is, 1D's KEEP is
   BLOCKED-candidate on the structural axis, not merely contested.
3. **AC-vs-injective coexistence UNBUILT — tested around, never through.** Positional injectivity
   is unsound for AC constructors (`Sum`); PROTO-2 stayed sound only by running the two mechanisms
   in separate e-graphs. The hard interaction (AC canonicalization + injectivity both live on
   `Sum` under saturation, gated by a flag) is asserted-confluent, unproven.
4. **The KEEP/RETIRE is definitional and cross-lock, not settleable in LOCK-1.** It turns on
   grammar-authoritative (a coarse TypeDesc is a wall → 1D closes it) vs codegen-distributed (the
   template is the sanctioned home → 1D is cosmetic). That architecture choice belongs to
   GESTALT-B / LOCK-3 / LOCK-7. **Both PROTO-2 verdicts presuppose an unadjudicated architecture.**
5. **Cost-extraction idle (PROTO-1) — 1D's original raison d'être has nothing to extract over
   (overload/cost residue exactly 0).** Even under KEEP, 1D shrinks to a *structural* argument
   only; the cost-lattice machinery is void.
6. **Determinism at scale + TS-backend isomorphism (Constitution iii) unaddressed.** Byte-identity
   held on a 10-class fixture; 191-rule CSS under `FxHashMap` iteration untested; how a TS backend
   consumes e-graph-derived types is unspecified (1B ships `backend/ts/projection.rs`).

**Net disposition: not cleanly KEEP, not cleanly RETIRE.** The spec's KEEP-on-strawman and the
prior critic's RETIRE-on-"supplied-downstream" both fail; 1D is CONTESTED, gated on (a) a
GESTALT-level architecture adjudication and (b) an unbuilt Row-inference prototype that is the
true make-or-break.

---

## 6. CONSTITUTION & PROTOTYPE-EVIDENCE AUDIT (lock-wide)

- **iv / v (refutation laws / zero-runtime, no perf claim):** RESPECTED. Every probe disclaims
  perf. Zero-runtime is *asserted by all four, exercised by none* — a named pass-2 gap per family,
  acceptable at pass-1.
- **vi (parsimony / no god modules):** RESPECTED. 1A `types/{row,scheme}`, 1D
  `egraph/{type_node,type_analysis,type_rules}` — clean splits.
- **vii (concrete artifacts or SPEC-ONLY):** MIXED — the sharpest breach. All probes RAN and are
  labeled MEASURED, but the *synthesis* (FAMILY-SPECS 04:41) fails to consume the four probes that
  landed after it (04:48–09:39); its "confirmed defect" is void and its 1D disposition is
  overtaken. This is a synthesis ignoring its own later measurements, not a fabricated number.
- **viii (consumer-less substrate):** the pass's cleanest result — 1B/1D ship with live backend
  consumers; prior-art C4's veto correctly retired (verified: `type_obligations` matched in both
  `backend/rust/ir_types.rs` and `backend/ts/projection.rs`).
- **ix (extant-tranche consciousness):** GN-3 §1.5 adjudicated CSP/e-graph OUT of the type plane;
  the live-lineage discovery is a *supersession* of that verdict but is declared inline
  ("factually superseded") rather than as a **named** supersession with the GN-3 doc amended — a
  procedural ix gap to carry to GESTALT-B.
- **Masked fallbacks:** TWO now confirmed. (a) `assign` last-writer-wins — latent, unreachable for
  clashes, but the spec mis-attributes it to dead `TypeDomain::join`, inflating the case against
  1B. (b) The `json_templates/value.rs` bespoke per-grammar template supplying `List ≠ Row` the
  general pipeline cannot express (fact 3) — a hand-code-covers-for-coarse-inference pattern the
  standing laws forbid, and the true crux of the 1D question.
- **Gates that cannot fail:** PROTO-1's registered gates both FAIL on all four corpora (eq+sum
  82–89% < 95%; structural-non-eq 18/1020/53/87 ≫ 2); "THIN — decisive" is reached by a **post-hoc
  third gate** chosen because it yields THIN. Honest pass-1 output is INCONCLUSIVE-on-registered-
  gates + a proposed re-cut, not a decisive retirement of 1B/1D.
- **Prototype-evidence misuse:** the spec cites only PROTO-2's strawman (keep). Both recursion-
  parity probes measure against the *running* incumbent and reach a **contested** verdict that the
  spec never carries. And no probe tested 1D's actual make-or-break (Row *inference*, §5.2).

---

## 7. PASS-2 DIRECTIVES (what must be built, not argued)

1. **Re-cut FAMILY-SPECS to consume all four post-04:41 probes** (run2 + probe2 included). The
   spec is stale; folding them reopens the carrier fork and turns the 1D verdict from
   KEEP-on-strawman into CONTESTED-gated-on-inference.
2. **Adjudicate the inverted carrier fork on corrected facts:** running-CSP-hardened vs union-find
   re-seed. The spec's union-find tilt rested on a doubly-dead defect (§0.1). Measure derive
   wall-time and `git diff crates/runtime/` for *both* seeds; weigh 1B's kept consumers + sound
   live path + additive hardening against 1A's deletion of running code.
3. **Land the `assign`/`join` raise-on-disagreement hardening** (run2 minimal repair) — additive,
   ships regardless of carrier winner; then *verify the corpus still solves* once overwrite is
   refused (the untested backward-safety gap, §3.1).
4. **Build 1D's actual make-or-break: does a `TypeENode` Language DERIVE `Row` from the `object`
   rule body without a per-grammar hint (§5.2)?** This, not extraction, decides whether 1D closes
   the `json_templates` workaround or relocates it. If it needs the hint, 1D is BLOCKED-candidate
   on structure.
5. **Build a numeric-width probe the census structurally cannot run (§1.1)** — a field genuinely
   under-determined between u8/u16/u32, measured defaulting-lattice vs search. Needs a typed
   micro-fixture (blocks on GN-3.W3 or a hand-authored `->` grammar).
6. **Carry 1D's `union` amendment as a named LOCK-2/isomorphism cost with a saturation
   measurement** (it is off-tree, fact 5) — never a free hook; and build the AC-sum + injectivity
   coexistence *through*, not around (§5.3).
7. **Author ≥1 typed `->` grammar (GN-3.W3)** so 1C's G2 and 1A's row discipline become falsifiable
   at all. Until then 1C's 52% and 1A's row clause are ceilings no pass can raise.
8. **Record the GN-3 §1.5 override as a NAMED supersession** (Constitution ix); land the residue
   probes in-tree or mark them retired (they live only in `.patch.diff`, and the egraph amendment
   is reverted — non-reproducible from HEAD).

---

## 8. CONVERGENCE SUMMARY

| Family | Disposition (spec) | Critic-1 | Dominant open gap |
|---|---|---:|---|
| **1A** UNIFICATION-FIRST | ADVANCE (spine) | **64%** | conflates well-evidenced architecture with an unproven union-find carrier whose justifying defect is doubly-dead (§0.1); no direct prototype; residue framing rests on a pipeline that cannot express `List ≠ Row` |
| **1B** ONE-CONSTRAINT-STORE | ADVANCE fed. / BANK thesis | **63%** | branded "unsound as shipped" — twice-refuted (live path sound); may be the cheaper spine (running-CSP-hardened), not 1A's coat; R-B1 store-shape unbuilt by any probe |
| **1C** BIDIRECTIONAL-LOCAL | FOLD | **52%** | make-or-break diagnostic head-to-head structurally un-runnable — zero `->` corpus + no typed grammars (facts 4, 6); raison d'être unfalsifiable today |
| **1D** TYPES-AS-ECLASSES | BANK (recursion keep) | **48%** | KEEP rests on an UNBUILT Row-inference step of the same difficulty as the workaround it claims to close (§5.2); "zero new engine" falsified + reverted at HEAD; AC-vs-injective untested; verdict definitional/cross-lock |

**Lock-wide.** Pass-1 produced one clean verification (consumer-less veto retired), four measured
probes, and a synthesis frozen before the decisive four landed and never re-cut. The 1A-spine +
defaulting-lattice conclusion is prior-art-consonant and probably right, but it was earned by
*framing on the wrong corpus and the wrong instrument* (zero `->`, a pipeline that cannot express
`List ≠ Row`) and on a *doubly-dead defect* cast as a production bug. The newest probes shift the
lock a second time: run2 confirms the incumbent running CSP is reachable-sound (reopening the
carrier fork the spec prematurely closed toward 1A), and probe2 dissolves BOTH the spec's KEEP and
the prior critic's RETIRE for 1D — exposing that neither checked what actually supplies structure,
and that 1D's true make-or-break (Row *inference*, not extraction) was never tested. **No family
converges; none is cleanly retired.** The two sharpest unadjudicated questions are now (i) the
inverted carrier fork — harden the running CSP or delete it for a union-find re-seed? — and (ii)
whether 1D's type Language can *derive* the structural distinction the general pipeline provably
cannot name, or merely relocates the per-grammar workaround.

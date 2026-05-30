---
lens: CH6 ANTI-PAPER-CLOSE
pass: T-P2-research (SK-V17 totality fold)
cycle: V1
reviewer: CH6 (V1)
generated_at: 2026-05-29T00:00:00Z
subject_dossiers: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
master_head: 91b6893b0
t_p1_source_sha: 445925167
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 (CH6), §8, ORCHESTRATOR §3W
focus: no fold deferred to "a future wave will detail"; each carries a concrete scalar-ref/manual/measurement; no engineered-defer
sections_reviewed: 41
accept: 37
revise: 3
reject: 1
accept_pct: 90.2
---

# CH6 ANTI-PAPER-CLOSE — T-P2 SK-V17 V1

## Lens posture

CH6 scans for paper-close: a technique claimed "validated/proven" on citation
density alone; reference-stuffing (N sources cited, none integrated); a fold that
fails to state the bbnf-specific reason it transfers; and — the load-bearing axis
for THIS dispatch — **engineered-defer**: a fold-proposal or research question
shoved to "a future wave / a later research pass will detail this" WITHOUT a
concrete scalar-ref, manual/published-precedent, or measurement attached at THIS
pass.

**The legitimate-defer boundary (binding for every disposition below).** The
T-P1 CONVERGED entry binding and PASS-2 §1 establish SK-V18 as the *implementation*
wave; T-P2 is forward-architecture *grounding*, not implementation (§intro lines
19-25). Therefore "the impl lands in SK-V18" / "200-700 LOC SK-V18 fold" is NOT a
CH6 defer — it is the research→implementation boundary the pass is built on. CH6
flags only deferral of the *grounding itself*: where a dossier owes a citation, a
scalar-ref, or a measurement at THIS pass and instead points at a later research
cycle. A fold that names its concrete admission artefact NOW (scalar-ref path,
checkasm command, manual precedent, measured regression) and schedules only the
*coding* for SK-V18 is ACCEPT.

## Census

| dossier | sections | ACCEPT | REVISE | REJECT |
|---|---|---|---|---|
| 2a-sota-landscape | 8 (6 folds + 2 refutations) | 8 | 0 | 0 |
| 2b-primitive-vocabulary | 9 (L1-L9 folds) | 9 | 0 | 0 |
| 2c-grammar-neutrality | 7 (6 folds + onboarding) | 6 | 1 | 0 |
| 2d-cost-model | 7 (FOLD-2D-01..07) | 7 | 0 | 0 |
| 2e-host-arch | 6 (FOLD-2E-A..F) | 5 | 0 | 1 |
| 2f-fold-gaps | 9 (F1-F9) | 8 | 1 | 0 |
| cross-cutting | — | (folded into above) | 1 | 0 |
| **total** | **41** | **37** | **3** | **1** |

Accept rate **90.2%** — below the §3W ≥95% bar, with **1 REJECT** + **3 REVISE**.
Per PASS-2 §3 ("Cycle V1 expects ≥30% REVISE; an all-ACCEPT wave is paper-close"),
the V1 result is correctly non-paper-close; the REJECT is the load-bearing
anti-paper-close catch.

---

## REJECT (1)

### CH6-V1-R01 — 2E U-2E-04 + frontmatter: engineered-defer of the aarch64 esoterica grounding to "a future 2E-esoterica cycle"

**File:line.** `2e-host-arch.md:338` (U-2E-04 verify_action), compounded by
`2e-host-arch.md:10` (frontmatter `primary_sources_cited: 11 # … Lemire/Validark/Arm ISA primitive citations`).

**The defect — engineered-defer.** U-2E-04 reads:

> "2E (this dossier) + **a future 2E-esoterica cycle** ground a source-backed
> aarch64 candidate per ARCH `:1206,:1279` before any admission; until then
> mechanically refused. No admission this pass."

This is the textbook CH6 paper-close phrasing the lens exists to catch: the
*grounding action itself* (a source-backed aarch64 candidate for the
CollapsedStage/NEON-FSM question) is deferred to an unnamed "future 2E-esoterica
cycle." It is an engineered-defer because:

1. **It is a defer-loop.** 2D's UNKNOWN-2D-05 verify_action (`2d-cost-model.md:264`)
   says "**2E must supply** a primary aarch64 source-backed technique
   (PMULL/CSSC/UDOT/BCAX FSM-dispatch lineage)." 2D points AT 2E; 2E (U-2E-04)
   points at a *future* 2E. The grounding owed at this pass is grounded by neither.
2. **The frontmatter overcounts.** `2e:10` declares the 11 sources include
   "Lemire/Validark/Arm ISA primitive citations," yet a body grep for
   `PMULL|VPCLMUL|CSSC|UDOT|DotProd|LD4|BCAX|EOR3|svmatch|Validark|ICPP|Sneller|asmjson`
   returns the named-primitive set **zero times** — the sole aarch64-esoterica row
   (`2e:87` D') merely *refuses* the CollapsedStage and cites only
   `ARCH:1206/:1276-1282` + `SPEC:854`. The frontmatter claims aarch64 ISA
   citations the body never integrates — reference-stuffing in the source count
   (CH6 §3: "N sources cited, none integrated").

**Mitigating, but insufficient.** The *admission* is correctly fenced — aarch64
CollapsedStage is "mechanically refused this pass, no admission," which honours
the no-silent-6th-shape and aarch64-only discipline. But CH6 does not gate on
admission; it gates on *grounding-defer*. The question of whether NEON-FSM-dispatch
ever closes is a real open UNKNOWN — that is fine as an Open Research Question —
but the verify_action must NOT route the grounding to "a future cycle." The pass
either grounds the aarch64 candidate (per the §2 2E scope row's mandate: each
esoterica entry = published citation + abstract-primitive name + hardware gate) or
records it as a firmly bounded UNKNOWN whose verify_action names a CONCRETE source
to consult NOW (the Arm A64 ISA manual section, the Lemire/Validark posts), not a
future research pass.

**Concrete fix (must FOLD in V2).**
- Delete "+ a future 2E-esoterica cycle" from `2e:338`. Re-author U-2E-04's
  verify_action to name the concrete grounding artefact at THIS pass: cite the Arm
  A64 ISA manual entry for the candidate FSM-dispatch primitive lineage
  (PMULL/`tbl`/BCAX), the Lemire 2026 "matching characters on ARM" NEON `svmatch_u8`
  post, and Validark 2024 — the exact sources `2b:73` and `2b:166-167` already
  ground for the eq-set classifier — and state the bbnf-specific verdict (NEON has
  no AVX-512-mask-register branchless-FSM analogue; the candidate is bounded-refuted
  on the published aarch64 ISA, not deferred). The verify_action then reads as a
  *recorded refutation/UNKNOWN grounded on a named manual*, not a punt.
- Correct the `2e:10` frontmatter source count to match the body: either integrate
  the Lemire/Validark/Arm citations into a real grounding row, or drop them from
  the count (they are presently uncited in the body, so the count is inflated).

**Cross-reference to fold V2.** When 2E re-authors U-2E-04, 2D's UNKNOWN-2D-05
verify_action (`2d:264`) must be reconciled so it no longer reads "2E must supply"
against a 2E that supplies nothing — point both at the same named aarch64 manual
source, or jointly record the bounded refutation. (Disposition source: CH6-V1-R01;
2D folds the reconcile.)

---

## REVISE (3)

### CH6-V1-V01 — 2C onboarding "test" stated as a verification posture, not exercised at this pass

**File:line.** `2c-grammar-neutrality.md:227-245` (Candidate SK17-2C-ONBOARD,
"Future-grammar onboarding test").

**The concern.** The candidate's NAME ("Future-grammar onboarding test") and its
verdict ("GRAMMAR-NEUTRAL GATE (the verification, not a fold)") describe a
falsifier that the dossier asserts but does not *run* at this pass. The grep
predicate is given (`rg 'JsonParser|CssL4Parser|…' crates/{ir,simd-scan,…}/src/`
= ZERO, `2c:233`) and the per-grammar-dir census predicate is given
(`find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` = ZERO in the
generic root, `2c:243-244`) — but the dossier does not report having *executed*
them at HEAD `91b6893b0`. CH6 reads this as a grounding that names a test without
attaching its result NOW: the "test" is a future action stated as a present gate.

**Why REVISE not REJECT.** The grammar-neutrality the onboarding test guards IS
grounded elsewhere by-construction (type-param `G:EventGrammar` carriage, `2c:69`;
alphabet-as-data, `2c:70`) and the by-exercise scope is honestly bounded to JSON+CSS
(`2c:77,285-289`). The defect is narrow: an asserted falsifier predicate with no
executed result. This is the soft end of paper-close (a test cited, not run), not a
fabricated claim.

**Concrete fix (FOLD in V2).** Either (a) run the two predicates at HEAD and report
the live result inline (e.g. "executed at `91b6893b0`: `rg 'JsonParser|CssL4Parser'
crates/ir/src crates/simd-scan/src` = 0 hits; `find … -type d` = 0 per-grammar dirs
in the generic root"), turning the gate into a measured fact NOW; or (b) reclassify
SK17-2C-ONBOARD explicitly as a *verify_action* under Open Research Questions with
the predicate as the concrete check, not as a §2 fold candidate — so it is not
counted among the grounded folds while unexecuted.

### CH6-V1-V02 — 2F F9 path-(b) "core realisation NON-ZERO/UNKNOWN" carries no concrete sizing route

**File:line.** `2f-fold-gaps.md:401-425` (F9, Lock-2 `StructLayout` reconcile),
specifically `:411-412` and the LAC-2F-FOLD-05 `loc/risk` cell (`:490`).

**The concern.** F9 correctly catches the V2 mis-pricing and refuses to under-price
path-(b) ("`LayoutFacts` is grep-zero in crates/ … so path-(b)'s crates/core
realisation is NON-ZERO/UNKNOWN, not ~0 LOC"). That refusal is sound and
anti-overfit. But CH6's complementary axis is that an UNKNOWN must carry a concrete
*next measurement*, not float as "UNKNOWN." F9 names the cost UNKNOWN but gives no
verify_action that would *resolve* the sizing — no "count the `backend_shape`
read-sites that path-(b) would need to introduce in core," no pointer to the skinny
`LayoutFacts` definition (`skinny/crates/passes/src/lib.rs:85,91`, which the dossier
DOES cite at `:411`) as the template to size against. The UNKNOWN is honest but
under-specified for grounding.

**Why REVISE.** The under-specification is a missing concrete sizing route, not a
fabrication; path-(a)'s 960-site count IS concrete (`grep StructLayout crates/`=960).
Only path-(b)'s realisation lacks a measurement route.

**Concrete fix (FOLD in V2).** Add a verify_action to F9 / 2F-FOLD-U-set: "size
path-(b) by counting the `backend_shape`/`LayoutFacts` consumer sites the skinny
side-table model (`skinny/crates/passes/src/lib.rs:85,91`) would require introducing
into `crates/core` (`grep -rn 'backend_shape' crates/` baseline), reported at HEAD."
This converts NON-ZERO/UNKNOWN into a bounded, measurable next step grounded on the
already-cited skinny template — no future-pass defer.

### CH6-V1-V03 (cross-cutting) — five dossiers re-derive an identical 6-fold/refutation skeleton; convergence on the LAC-1E-14 precedent risks citation-echo over independent grounding

**File:line.** `2a:178-209` (FOLD-2A-D) ≈ `2c:157-177` (SK17-2C-D) ≈ `2d:78-102`
(FOLD-2D-01) ≈ `2e:180-211` (FOLD-2E-D) ≈ `2f:223-255` (F4) — all five ground the
tape-as-substrate-not-6th-shape verdict on the SAME LAC-1E-14 / `FactStream`
precedent (`LOCKS.md:100-116`), with near-identical prose.

**The concern.** CH6 §3 flags reference-stuffing and citation-density-as-proof. The
LAC-1E-14 precedent is a REAL manual (the locks ledger) and each dossier DOES attach
a bbnf-specific transfer reason — so this is not a confabulation (CH1's lane) nor a
pure paper-close. But five agents converging on one precedent with one prose
template means the convergence rests on a single load-bearing citation echoed five
ways. If LAC-1E-14 were mis-read, every dossier inherits the error; the apparent
"5-source agreement" is one source counted five times. The §2 candidate-enumeration
is meant to be *load-bearing* per dossier, not a shared boilerplate.

**Why REVISE not REJECT.** The precedent reading IS correct (verified: `FactStream`
is explicitly "a substrate-manifest classification only; it is NOT a 6th
`BackendShape` variant," and the 5-shape canon holds) and each dossier attaches at
least one distinct anchor (2D adds `ARCH:1088` "five ways the substrate may project";
2F adds the impl-already-folds reading; 2A adds the `substrate_target` four-value
mapping). The grounding is sound; the risk is methodological echo.

**Concrete fix (FOLD in V2).** Across the cohort, ensure each D-fold carries at
least ONE independent corroborating anchor beyond LAC-1E-14 (e.g. the
`admits_collapsed_stage` predicate co-requiring `target.arch==x86`, ARCH `:1206`,
which mechanically proves no 6th-shape route is needed on aarch64 — an independent
mechanical fact, not a precedent-echo). 2D already does this (`:65,:69`); 2A/2C/2E/2F
should each carry the independent mechanical anchor so the verdict does not rest on a
single echoed precedent. No content change to the (correct) verdict — only
defence-in-depth so the 5-fold agreement is 5 independent groundings, not 1×5.

---

## ACCEPT (37) — anti-paper-close strengths confirmed

The dossiers are, on the whole, strongly anti-paper-close. Confirmed positives:

- **Concrete admission artefacts named NOW, not deferred.** 2b's L5/L6
  (`comment_body_mask_64`, `bracket_depth_mask_64`) are net-new with scalar-ref
  ABSENT — but the dossier names the exact REQUIRED-NEW artefact paths
  (`src/scalar/comment_body_mask_64.rs`, `tests/checkasm_comment_body_mask_64.rs`,
  `2b:228-229,244-245`), the ISA primitive (`overflowing_add` carry, Arm A64 ISA,
  NOT PMULL per REDRESS-88), and the "scalar ref + checkasm BEFORE wiring" Lock-16
  gate. This is the *anti*-paper-close pattern: the admission cost is concrete and
  present, only the coding is SK-V18. ACCEPT (FOLD-L5, FOLD-L6).

- **2b OQ-2b-04 is a legitimate implementation-defer, not paper-close.** "A future
  wave with scalar-ref (**present**)…" — the scalar-ref exists today; the deferral
  is the NEON *body*, carried with the concrete bbnf reason ("lo6 table path is
  JSON-only … no CSS antecedent"). The transfer reason is stated; ACCEPT.

- **Every fold states a bbnf-specific transfer reason.** No fold rests on "SOTA does
  it this way." 2A-F each carry the grammar-neutral verdict + the divergence
  antecedent + the measured anchor (2-3× recognizer win `RESULTS.md:5-55`; 118×
  AZ-IV; 28-65×/983×/10583× regression; 86.07% samply pathology). Measurement
  density is high (per-dossier concrete-anchor counts 4-15).

- **Refutations are first-class and grounded on the literature's actual position**
  (CH6 §3 + §8.5): the 6th-shape refutation cites the G-Omega gate + SPEC §9
  pre-block; the aarch64-CollapsedStage refutation cites the mechanical
  `admits_collapsed_stage` x86 co-requirement; the udot-orphan-kernel refutation
  (`2b:146`) cites "no benched CSS antecedent." These are bounded refutations with
  named fences, not deferrals.

- **2D's cost-model folds carry published primary sources integrated into the
  verdict** (Tate POPL 2009 equality saturation, Willsey egg POPL 2021, BURG LOPLAS
  1992, Langdale/Lemire VLDB-J 2019, Mison VLDB 2017) — each tied to a live skinny
  path:line (`passes/lib.rs:392,498,499`), not cited in isolation. The prior-2D
  tautology refutation (`2d:257`, "a candidate set is not a derivation proof") is
  itself an anti-paper-close catch carried forward.

- **The hygiene action CH1-V5-001 is resolved on disk, not deferred** (2c:14-16,
  2e:69-75, 2f:25-39 all verify the enumerated form executes at HEAD).

## Verdict

CH6 returns **90.2% ACCEPT, 1 REJECT (CH6-V1-R01), 3 REVISE
(CH6-V1-V01/V02/V03)**. The wave is correctly non-paper-close (the §3 ≥30%-REVISE
expectation is met; the all-ACCEPT failure mode is avoided). The single load-bearing
catch is the **engineered-defer of the aarch64 esoterica grounding to "a future
2E-esoterica cycle" (CH6-V1-R01)** — the one place a dossier punts a grounding it
owes at this pass to a later research cycle, compounded by a frontmatter source-count
that overcounts uncited aarch64 ISA citations. The three REVISEs are soft
paper-close edges (an asserted-but-unexecuted onboarding predicate; an UNKNOWN
without a concrete sizing route; a precedent echoed five ways without per-dossier
independent corroboration). Every REJECT/REVISE carries a file:line + a concrete
fold-fix; none is orphaned. The pass does NOT advance on CH6 this cycle (R01 is an
open critical-defect-class grounding-defer per the convergence criterion); fold into
V2.

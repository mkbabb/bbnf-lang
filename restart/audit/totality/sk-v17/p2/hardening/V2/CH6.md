---
lens: CH6 ANTI-PAPER-CLOSE
pass: T-P2-research (SK-V17 totality fold)
cycle: V2
reviewer: CH6 (V2)
generated_at: 2026-05-29T00:00:00Z
subject_dossiers: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
master_head: 91b6893b0
t_p1_source_sha: 445925167
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 (CH6), §8, ORCHESTRATOR §3W
focus: no fold deferred to "a future wave will detail"; each carries a concrete scalar-ref/manual/measurement; no engineered-defer
prior_cycle: V1 (CH6.md — 90.2% ACCEPT, 1 REJECT CH6-V1-R01, 3 REVISE CH6-V1-V01/V02/V03)
sections_reviewed: 41
accept: 41
revise: 0
reject: 0
accept_pct: 100.0
verdict: PASS (all V1 dispositions folded + verified at HEAD; no new engineered-defer)
---

# CH6 ANTI-PAPER-CLOSE — T-P2 SK-V17 V2

## Lens posture

CH6 scans for paper-close: a technique claimed "validated/proven" on citation
density alone; reference-stuffing (N sources cited, none integrated); a fold that
fails to state the bbnf-specific reason it transfers; and — the load-bearing axis
for THIS dispatch — **engineered-defer**: a fold-proposal or research question
shoved to "a future wave / a later research pass will detail this" WITHOUT a
concrete scalar-ref, manual/published-precedent, or measurement attached at THIS
pass.

**The legitimate-defer boundary (binding, carried verbatim from V1).** PASS-2 §1
+ the dispatch establish SK-V18 as the *implementation* wave; T-P2 is
forward-architecture *grounding*, not implementation. "The impl lands in SK-V18" /
"200–700 LOC SK-V18 fold" is NOT a CH6 defer — it is the research→implementation
boundary the pass is built on. CH6 flags only deferral of the *grounding itself*:
where a dossier owes a citation, a scalar-ref, or a measurement at THIS pass and
instead points at a later research cycle. A fold that names its concrete admission
artefact NOW (scalar-ref path, checkasm command, manual precedent, measured
regression) and schedules only the *coding* for SK-V18 is ACCEPT.

**V2 posture — fold-verification cycle.** V1 returned 90.2% ACCEPT with one
load-bearing REJECT (the aarch64-esoterica grounding-defer-loop, CH6-V1-R01) and
three REVISE. The pass did NOT advance on CH6 V1. V2's mandate is to verify each
V1 disposition was *folded* (not paper-hardened) AND to re-scan the cohort for
*new* engineered-defer the V2 regeneration may have introduced. Every V1
disposition fold below was confirmed against the live tree at HEAD `91b6893b0`,
not merely against the dossier's claim that it folded — the V2 anti-paper-close
discipline is that a "folded" assertion is itself worthless without a ground-truth
re-execution.

## Census

| dossier | sections | ACCEPT | REVISE | REJECT |
|---|---|---|---|---|
| 2a-sota-landscape | 8 (6 folds + 2 refutations) | 8 | 0 | 0 |
| 2b-primitive-vocabulary | 9 (L1–L9 folds) | 9 | 0 | 0 |
| 2c-grammar-neutrality | 7 (6 folds + onboarding) | 7 | 0 | 0 |
| 2d-cost-model | 7 (FOLD-2D-01..07) | 7 | 0 | 0 |
| 2e-host-arch | 6 (FOLD-2E-A..F) | 6 | 0 | 0 |
| 2f-fold-gaps | 9 (F1–F9) | 9 | 0 | 0 |
| cross-cutting (V03 echo) | — | (folded into above) | 0 | 0 |
| **total** | **41** | **41** | **0** | **0** |

Accept rate **100.0%** — at the §3W ≥95% bar. The §3 "Cycle V1 expects ≥30%
REVISE; an all-ACCEPT wave is paper-close" rule governs **V1**, not V2: V1
correctly carried 4 non-ACCEPT dispositions (the load-bearing REJECT among them);
V2 is a fold-verification cycle whose all-ACCEPT result is the *evidence of
convergence*, not paper-close, because every V2 ACCEPT is grounded on a
ground-truth re-execution at HEAD (greps run, files stat'd, line refs resolved) —
not on citation density. This is the convergence-cycle all-ACCEPT the criterion
admits, distinct from the V1-paper-close all-ACCEPT the rule bars.

---

## V1 DISPOSITIONS — FOLD VERIFICATION (the load-bearing V2 audit)

### CH6-V1-R01 (REJECT) — engineered-defer of the aarch64 esoterica grounding → **FOLDED + VERIFIED**

**V1 defect.** 2E U-2E-04 routed the aarch64-esoterica grounding (the
CollapsedStage/NEON-FSM question) to "a future 2E-esoterica cycle" — a
defer-loop: 2D's UNKNOWN-2D-05 (`2d:264`) said "2E must supply" the aarch64
source; 2E (U-2E-04) pointed at a *future* 2E. Compounded by a `2e:10`
frontmatter source count claiming Lemire/Validark/Arm ISA citations the body
never integrated (reference-stuffing in the count).

**V2 fold — verified at `2e-host-arch.md:431` (U-2E-04 verify_action).** The
defer-loop is deleted. U-2E-04 now reads: "**CONCRETE sources NOW (CH6-V1-R01
fold; no defer-loop):** the Arm A64 ISA manual FSM-dispatch lineage + Lemire 2026
`svmatch_u8`-on-NEON port + Validark 2024 (the same sources 2B grounds at
2b:73/166-167). **Bounded refutation recorded:** NEON has no AVX-512-mask
branchless-FSM analogue — the `FSM_DISPATCH_THREADED`/`CollapsedStage` spine is
x86/AVX-512-pinned (ARCH `:1278-1282`), mechanically refused on aarch64 at
`admits_collapsed_stage` (LAC-2D-06). No admission this pass … This resolves
2d:264 UNKNOWN-2D-05 (\"2E must supply the aarch64 source\"): the source is
supplied here and the refutation is bounded — no future-cycle defer." The
grounding-action is now *performed at this pass* (concrete sources named, bounded
refutation recorded), not punted. This is the exact correction CH6-V1-R01's
concrete-fix mandated.

**The other defer-loop end — verified reconciled at `2d-cost-model.md:277`.** The
V1 defect was a *loop* (2D→2E→future-2E); folding only the 2E end would leave
2D's verify_action pointing at a 2E that now supplies. Confirmed reconciled: 2d:277
UNKNOWN-2D-05 verify_action now reads "The aarch64 source set is CONCRETE and
already grounded — Arm A64 ISA manual FSM-dispatch lineage + Lemire 2026
`svmatch_u8` post + Validark 2024 … This is NOT a defer-loop (CH6-V1-R01
reconciled): the verify_action names the concrete refuting sources, it does not
punt to 'a future cycle'." Both ends of the loop closed.

**Frontmatter overcount — verified corrected at `2e-host-arch.md:10`.** Now reads
`primary_sources_cited: 9 # … frontmatter count = exercised-in-body, not
inventory; CH6-V1-R01`. The count was dropped from 11 to 9 (exercised-in-body)
and explicitly annotated as the CH6-V1-R01 fold. The reference-stuffing in the
source count is removed.

**Ground-truth verification at HEAD `91b6893b0`.** The named concrete sources are
the SAME the sibling 2B dossier already grounds for the eq-set classifier
(2b:73/166-167), and the bounded-refutation mechanical anchor (`admits_collapsed_stage`
x86 co-requirement) is live: `byte_class_from_table_64_neon` /
`bitmap_prefix_xor_64_neon` confirmed as 3-line scalar passthroughs to
`crate::scalar::*` (the table/prefix-are-scalar-delegate close-state the refutation
rests on), and the one real NEON body `byte_class_from_eq_set_64.rs` exists (3705
bytes). The grounding is real, not a citation gesture.

**Disposition: ACCEPT (R01 discharged).** The single V1 load-bearing catch is
fully folded — the grounding-defer is converted to a grounded bounded-refutation
with concrete sources named NOW, both defer-loop ends reconciled, and the
frontmatter overcount corrected. No residual.

### CH6-V1-V01 (REVISE) — 2C onboarding test asserted, not exercised → **FOLDED + VERIFIED**

**V1 concern.** The "Future-grammar onboarding test" (`2c:227-245`) named two
falsifier predicates but did not report executing them at HEAD — a test cited,
not run.

**V2 fold — verified at `2c-grammar-neutrality.md:252-285`.** SK17-2C-ONBOARD is
reclassified as "**a verify_action with a measured HEAD baseline**, not an
asserted clean pass" (`2c:261`). Both predicates are now executed at HEAD with
live results reported inline:
- Predicate 1 (`2c:262-269`): `rg 'JsonParser|CssL4Parser' crates/ir/src
  crates/simd-scan/src` = 7 hits, all in `crates/ir/src/registry/strategy.rs`
  (`:132,:137,:149,:197-198,:292,:315`), classified as the catalogued ARCH-3A-D09
  string-ident-registry leak surface under a monotonic-decrease-to-zero rule — NOT
  asserted clean.
- Predicate 2 (`2c:270-275`): `find crates/core/src/runtime -mindepth 1 -maxdepth 1
  -type d` = grammar dirs all carrying `// @generated by xtask regen-<grammar>` +
  `tape/` (the shared neutral substrate); zero hand-written per-grammar dirs.

**Ground-truth re-execution at HEAD `91b6893b0`.** Both predicates re-run live by
this lens:
- P1: `rg 'JsonParser|CssL4Parser' crates/ir/src crates/simd-scan/src` = **7
  hits**, all in `crates/ir/src/registry/strategy.rs` at exactly `:132,:137,:149,
  :197,:198,:292,:315` — matches the dossier verbatim. Confirmed they are
  string-ident registry data (`idents: &["JsonParser","JsonGrammar"]`) + doc
  comments, not `match grammar {}` runtime dispatch.
- P2: `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` = **10 dirs**
  (json, css_l4, bbnf, bnf, ebnf, math, csv, google_sheets, css_pretty + tape) —
  9 grammars all generated + the neutral tape substrate.

**Minor count nuance (not a CH6 defect).** The dossier prose at `2c:271` says "8
grammar dirs" while the inline list at `2c:272` enumerates 9 (`json,css_l4,bbnf,
bnf,ebnf,math,csv,google_sheets, css_pretty`); the live count is 9 grammars + tape.
This off-by-one in a recapped count is a CH1-CORRECTNESS-lane nit, not a CH6
paper-close concern — V01 demanded the predicate be *executed and reported*, which
it now is; the load-bearing claim (all per-grammar dirs @generated, zero
hand-written, Predicate 1 honestly NON-ZERO) holds exactly. Flagged to CH1 for
the count reconcile; CH6 disposition unaffected.

**Disposition: ACCEPT (V01 discharged).** The asserted-but-unexecuted predicate is
now a measured HEAD fact with the live result reported inline — the soft-paper-close
edge is closed, and the honesty is exemplary (Predicate 1 reported NON-ZERO and
catalogued, not whitewashed clean).

### CH6-V1-V02 (REVISE) — 2F F9 path-(b) UNKNOWN with no sizing route → **FOLDED + VERIFIED**

**V1 concern.** F9 correctly refused to under-price path-(b) ("`LayoutFacts`
grep-zero … NON-ZERO/UNKNOWN, not ~0 LOC") but gave the UNKNOWN no concrete *next
measurement* — an honest-but-under-specified UNKNOWN.

**V2 fold — verified at `2f-fold-gaps.md:473-491`.** F9 now carries a "path-(b)
sizing route (CH6-V1-V02 fold — UNKNOWN → bounded measurable step)" section. The
sizing step is concrete: "size as the 0→N introduce-site delta — baseline
`grep -rcn StructLayout crates/`=960 (rename surface) vs `grep -rcn
'backend_shape\|LayoutFacts' crates/`=0 (side-table surface); the price is the
side-table carrier introduce-cost + every read-site routed through
`backend_shape.get(rule)`." The verify_action (`2f:482-488`) names T-P3 as the
sizer with the exact grep baseline and the skinny `LayoutFacts` template
(`skinny/crates/passes/src/lib.rs:90,:96`) as the carrier to size against. The
LAC-2F-FOLD-05 `loc/risk` cell (`2f:582`) carries the same converted sizing.

**Ground-truth verification at HEAD `91b6893b0`.** Both grep baselines re-run live:
- `grep -rn 'StructLayout' crates/` = **960** — matches the dossier's rename-surface
  baseline exactly.
- `grep -rn 'backend_shape\|LayoutFacts' crates/` = **0** — matches the
  side-table-surface baseline exactly (the NON-ZERO/UNKNOWN claim verified).
- Skinny template: `pub struct LayoutFacts` at `lib.rs:91`, `backend_shape` field
  at `:96`, second decl at `:385` — the dossier cites `:90,:96,:385`; the `:90` is
  a 1-line off-by-one for the struct decl (the field refs are exact). Negligible,
  CH1-lane.

**Disposition: ACCEPT (V02 discharged).** The floating UNKNOWN is converted to a
bounded, measurable 0→N introduce-site delta grounded on the already-cited skinny
template and two live-verified grep baselines — no future-pass defer. This is the
exact "UNKNOWN must carry a concrete next measurement" CH6-V1-V02 demanded.

### CH6-V1-V03 (REVISE, cross-cutting) — five D-folds echo LAC-1E-14 → **FOLDED + VERIFIED**

**V1 concern.** Five dossiers (2a/2c/2d/2e/2f) grounded the
tape-as-substrate-not-6th-shape verdict on the SAME LAC-1E-14 / FactStream
precedent with near-identical prose — a single load-bearing citation echoed five
ways (apparent 5-source agreement = 1 source × 5).

**V2 fold — verified across the cohort.** Each D-fold now carries the independent
`admits_collapsed_stage` x86-co-requirement mechanical anchor (ARCH `:1206`)
beyond the shared LAC-1E-14 precedent:
- 2a (`2a:204-212`): "**Independent mechanical anchor (beyond the LAC-1E-14
  precedent, CH6-V1-V03).** The no-6th-shape verdict does NOT rest on the
  precedent-echo alone: `admits_collapsed_stage` mechanically co-requires
  `target.arch==x86`…" — "so the 5-fold agreement is 5 independent groundings, not
  1×5 (CH6-V1-V03)."
- 2c (`2c:180-182,:311,:331,:370`): "the `CollapsedStage` row at `ARCH:1206`
  mechanically proves no 6th-shape route is needed on aarch64."
- 2d (`2d:235-248`): the `admits_collapsed_stage` predicate mechanically refuses
  CollapsedStage on aarch64 (`ARCHITECTURE.md:1206`, LAC-2D-06).
- 2e (`2e:234-240`): "**Independent corroborating anchor (CH6-V03 fold):** the
  `admits_collapsed_stage` x86 co-requirement at ARCH `:1206` mechanically proves
  no 6th-shape route is needed on aarch64 … This anchor is independent of the
  LAC-1E-14/FactStream citation the other D-folds share."
- 2f (`2f:82`): "`admits_collapsed_stage` is x86-bound (ARCH:1151) so mechanically
  refuses on aarch64 — independent corroboration of no-6th-shape beyond LAC-1E-14
  (CH6-V1-V03)."

**Why this closes V03.** The mechanical anchor is a *different kind* of evidence
than the precedent: LAC-1E-14 is a manual-precedent ("the locks ledger classified
FactStream as a substrate-manifest category, not a 6th shape"); ARCH:1206 is a
mechanical fact ("the only shape that would consume hand-written NASM is x86-pinned
in the predicate, so aarch64 cannot acquire a 6th shape"). If LAC-1E-14 were
mis-read, the ARCH:1206 anchor still independently proves the aarch64 no-6th-shape
verdict. The 5-fold agreement now rests on two orthogonal grounds per dossier, not
one echoed five times. (Note a minor cross-dossier line-ref drift — 2f cites the
predicate at ARCH:1151/:1282, others at :1206; both resolve to the
`admits_collapsed_stage` x86-co-requirement region; CH1-lane reconcile, not a CH6
concern.)

**Disposition: ACCEPT (V03 discharged).** Defence-in-depth achieved; the verdict
(correct, unchanged) no longer rests on a single echoed precedent.

---

## NEW-DEFER RE-SCAN (V2 regeneration introduced no engineered-defer)

A cohort-wide scan for engineered-defer phrasing the V2 regeneration might have
introduced (`future cycle | future wave will | later research pass | will detail |
deferred to | TBD`, filtering the negative/reconciled forms) returned **two
hits, both legitimate**:

1. `2a:73` — "grounded (per-primitive admission **deferred to 2b**)". This is a
   *within-pass cross-reference* to the sibling V2 dossier (2b carries the L1–L9
   primitive manifest), NOT a defer to a future research pass. The eq-set fan is
   already grounded inline at 2a:73 as the one real NEON body; only the
   per-primitive admission *table* is partitioned to 2b. Scope-partition between
   sibling dossiers of the same cycle is the §2 scope-matrix design (disjoint
   scopes), not paper-close. ACCEPT.

2. `2f:610` — "resolved on disk, **not deferred to a later pass**." This is the
   *negative* form — the CH1-V5-001 hygiene affirmation that the
   enumerated-filename residual is folded on disk. Not a defer; an explicit
   non-defer. ACCEPT.

No new engineered-defer. The V2 regeneration held the anti-paper-close discipline.

---

## ACCEPT (41) — anti-paper-close strengths confirmed at HEAD

The V1 strengths are preserved and the V1 dispositions folded; spot-confirmed at
HEAD `91b6893b0`:

- **L5/L6 net-new masks remain the anti-paper-close exemplar** (`2b:85`). Net-new,
  scalar-ref ABSENT, but naming the exact REQUIRED-NEW artefact paths NOW
  (`src/scalar/comment*`, `src/scalar/bracket*`), the ISA idiom
  (`overflowing_add` carry NOT PMULL per REDRESS-88; scalar running-balance NOT CTZ
  per REDRESS-89), and the "scalar ref + checkasm BEFORE wiring" Lock-16 gate.
  Verified live: `ls src/scalar/comment* src/scalar/bracket*` = no matches (genuinely
  absent, honestly named). The admission cost is concrete and present; only the
  coding is SK-V18. The model anti-paper-close pattern.

- **The eq-set fan is a real NEON body, table/prefix are honest scalar delegates**
  — verified live: `byte_class_from_eq_set_64.rs` = 3705 bytes (the one real body);
  `byte_class_from_table_64_neon` / `bitmap_prefix_xor_64_neon` are genuine 3-line
  passthroughs to `crate::scalar::*`. The close-state taxonomy (`2b:155`, `2e:281-292`)
  matches ground truth exactly — no NEON-body claim is made where only a scalar
  delegate exists.

- **Every fold states a bbnf-specific transfer reason; no fold rests on "SOTA does
  it this way."** Each D-fold carries the grammar-neutral verdict + the divergence
  antecedent + the measured anchor; the materialization-plane fold (2e A.#2) carries
  the lightningcss-bar-is-UNMEASURED-PENDING honesty (CH7-001 fold) rather than a
  fabricated CSS speedup — itself an anti-paper-close correction.

- **Refutations are first-class and bounded, not deferrals.** The 6th-shape
  refutation (G-Omega gate + SPEC §9 + the ARCH:1206 mechanical anchor); the aarch64
  CollapsedStage bounded-refutation (concrete sources named, NEON-has-no-AVX-512-mask
  analogue); the FSM/frame `source-present-unwired` close-state. Each is a bounded
  refutation with a named fence, not a punt.

- **The hygiene action CH1-V5-001 is resolved on disk, not deferred** (2c:15-17,
  2e:92-98, 2f:610 all verify the enumerated form executes at HEAD; the residual
  brace-glob occurrences survive only inside hardening artefacts as quoted-defect
  records, correctly not altered).

## Verdict

CH6 returns **100.0% ACCEPT, 0 REVISE, 0 REJECT**. Every V1 disposition —
the load-bearing REJECT CH6-V1-R01 (aarch64-esoterica grounding-defer-loop) and
the three REVISE (CH6-V1-V01 onboarding-predicate-unexecuted, CH6-V1-V02
path-(b)-UNKNOWN-without-sizing-route, CH6-V1-V03 LAC-1E-14-five-way-echo) — is
**folded and verified against the live tree at HEAD `91b6893b0`**, not merely
asserted folded: the onboarding greps re-run live (P1=7 in strategy.rs, P2=9
grammars +tape all @generated); the path-(b) sizing baselines re-run live
(StructLayout=960, backend_shape|LayoutFacts=0); the L5/L6 scalar files confirmed
absent; the eq-set/table/prefix close-states confirmed against the actual NEON
sources; both defer-loop ends (2E U-2E-04 + 2D UNKNOWN-2D-05) reconciled; the 2E
frontmatter overcount corrected to exercised-in-body. The cohort-wide new-defer
re-scan found zero engineered-defer the V2 regeneration introduced (the two
defer-phrasing hits are a within-pass sibling cross-reference and a negative
non-defer affirmation).

The all-ACCEPT V2 result is the **convergence-cycle** all-ACCEPT the criterion
admits — every ACCEPT grounded on a ground-truth re-execution — distinct from the
V1-paper-close all-ACCEPT the §3 rule bars (V1 correctly carried the
≥30%-REVISE + the load-bearing REJECT). CH6 advances on V2 with zero open
critical defects and zero orphan REVISE. Two CH1-lane line-ref nits surfaced for
CH1 (the 2c "8 vs 9 grammar dirs" count recap; the 2f ARCH:1151/:1206 predicate
line-ref drift) — neither is a CH6 paper-close concern and neither gates the V2
verdict.

# SK-V17 S-P3 CHALLENGE — CH4 COST (V2)

Lens: CH4 COST. Cycle: V2. Date: 2026-05-29. Master HEAD: `f87ee713a`.
Subject: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Mandate (PASS-3 §3 CH4 + ORCHESTRATOR §3W): every wave carries a LOC budget, a hard cap,
a research/plan/redress phase breakdown, and a same-wave-consumer requirement per
primitive; wave count ≤ 12; shortlist ≤ 8; net-new NEON (L5/L6) carries scalar-ref +
checkasm-new BEFORE wiring; every wave has a revert protocol.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Each carries `path:line` + concrete fix.

---

## §0 — Verification ledger (what I confirmed at HEAD)

| Check | Result | Evidence |
|---|---|---|
| Master HEAD = `f87ee713a` | CONFIRMED | `git rev-parse --short HEAD` = `f87ee713a` |
| L5 net-new (`comment_body_mask_64`) absent today | CONFIRMED net-new | `ls aarch64/comment_body_mask_64.rs scalar/comment_body_mask_64.rs` → No such file |
| L6 net-new (`bracket_depth_mask_64`) absent today | CONFIRMED net-new | `ls aarch64/bracket_depth_mask_64.rs scalar/bracket_depth_mask_64.rs` → No such file |
| L5/L6 checkasm-new absent today | CONFIRMED net-new | `ls tests/checkasm_{comment,bracket}_*` → No such file |
| L1 scalar twin + checkasm PRESENT | CONFIRMED | `scalar/byte_class_from_eq_set_64.rs` + `tests/checkasm_byte_class_from_eq_set_64.rs` present |
| SPEC wave count | 6 (W0–W5), Sections 3–8 | SPEC `:262-269` manifest; sections `:337/:390/:494/:583/:663/:725` |
| SPEC shortlist count | 8 active (L1–L8), L9 conditional not counted | SPEC `:271` |
| SK-V8 SPEC (mirrored shape) manifest form | per-wave LOC-budget column + global phase-cap table; ≤650 fit-proof escape on the highest-LOC wave | sk-v8/SPEC.md `:7,:9-15` |

---

## §1 — V1 fold audit (the four V1 REVISEs the V2 packet had to fold)

V1 CH4 returned 1 ACCEPT + 4 REVISE. The V2 packet claims "folds the V1 CHALLENGE
dispositions" (SPEC `:5`). Each V1 REVISE is verified folded:

| V1 item | V1 verdict | Folded in V2? | Evidence |
|---|---|---|---|
| CH4-2 P3-B 5-wave vs binding 6-wave SPEC | REVISE | **FOLDED** | `p3b:3` now `Cycle: V2`; `p3b:10-31` §0 fold note re-authors to six waves; `p3b:121-136` manifest is W0–W5; `p3b:132` "Wave count = 6 (W0-W5)" |
| CH4-3 L7 same-wave-consumer placement contradictory | REVISE | **FOLDED** | L7 single-valued to W1: `p3b:100-111` "L7 lands in W1 … The V1 placement of L7 in the NEON wave is RETIRED"; `p3c:50` "L7 is NOT here — L7 single-valued to W1, SPEC.md:391,430"; SPEC `:391,:430,:498-499` lists L7 in W1, NOT W3 |
| CH4-4 phase-breakdown numeric conflict (SPEC global 30/30 vs P3-B 20/15/30) | REVISE | **FOLDED** | SPEC phase-cap table `:282-289` now reads Research 20 / Plan 15 / Implementation-redress 30, matching P3-B `:156,:208,:262,:324,:369,:389`. The numbers now agree across SPEC and P3-B |
| CH4-5 W2 ≤650 fit-proof escape hatch undefined | REVISE (minor) | **FOLDED** | SPEC `:501-508` now defines the fit proof: a pre-redress per-artefact LOC accounting (the four generated artefacts named separately) accepted by CHALLENGE, attributing the over-450 lines to a named intrinsic cause; "Without an accepted fit proof, ≤450 binds … MUST split or return REVISE. The fit proof is itself ≤5 LOC of plan prose." `:266` manifest cross-refs "(defined §5)" |

All four V1 REVISEs are genuinely folded — not paper-folded. CH4-4 and CH4-5 (the two
that bound the SPEC itself) are both resolved in the SPEC text; CH4-2 and CH4-3 (the
cohort-reconciliation pair) are resolved in P3-B + P3-C.

---

## §2 — Per-wave cost-axis matrix (LOC · hard cap · phase breakdown · same-wave consumer · revert)

Every wave of the SPEC audited against the five CH4 axes. The manifest table at SPEC
`:262-269` and the phase-cap table at `:282-289` are the global cost ledger; the per-wave
sections (Sections 3–8) carry same-wave-consumer + revert + LOC notes.

| Wave | LOC budget | Hard cap | Phase breakdown | Same-wave consumer | Revert protocol | Verdict |
|---|---|---|---|---|---|---|
| W0 | `:264` 0 behavior + ≤300 harness/gate/test | `:264` ≤90 min | global `:282-289` (20/15/30) | `:377` gate-json consumes every field | `:384` ACCEPT | ACCEPT |
| W1 | `:265` ≤450 src/test; gen named separately | `:265` ≤90 min | global `:282-289` | `:478` L3 cursor IS L2's consumer | `:487` ACCEPT | ACCEPT |
| W2 | `:266` ≤450 default / ≤650 w/ defined fit proof | `:266` ≤90 min | global `:282-289` | `:568` generated projection reads tape; L8 read by L3 | `:576` ACCEPT | ACCEPT |
| W3 | `:267` ≤450 src/test; gen SIMD named | `:267` ≤90 min | global `:282-289` | `:645` tape decode consumes `Vec<u32>` index | `:654` ACCEPT | ACCEPT |
| W4 | `:268` ≤300 src/test | `:268` ≤90 min | global `:282-289` | `:713` post-W1 recognizer spine | `:720` ACCEPT | ACCEPT |
| W5 | `:269` 0 default / ≤150 Lock-14 cleanup | `:269` ≤90 min | global `:282-289` | `:763` close checklist + regen gate | `:771` ACCEPT | ACCEPT |

**Result: every wave carries an LOC budget, a ≤90-min hard cap, a phase breakdown, a
same-wave consumer, and a revert protocol.** All five mandatory axes are present per wave.
Wave count = 6 (≤12 ceiling, SPEC `:271`). Active shortlist = 8 (L1–L8; L9 conditional,
correctly NOT counted — SPEC `:271`, P3-A `:48,:56-58`). The phase-cap form mirrors SK-V8
(`sk-v8/SPEC.md:7,:9-15` — per-wave LOC-budget column + global phase table); the SPEC does
NOT invent a new shape.

---

## §3 — Net-new NEON primitive cost gate (L5/L6) — the highest-cost-risk axis

CH4's net-new clause: L5 (`comment_body_mask_64`) and L6 (`bracket_depth_mask_64`) MUST
carry scalar-ref + checkasm-new BEFORE wiring. Verified at source: both kernels, both
scalar twins, AND both checkasm differentials are genuinely ABSENT at HEAD (§0 ledger).
The SPEC binds them correctly:

- W3 owner paths name all six net-new files: `aarch64/comment_body_mask_64.rs` +
  `scalar/comment_body_mask_64.rs` + `tests/checkasm_comment_body_mask_64.rs` (L5) and the
  bracket triple (L6) — SPEC `:599-602`.
- W3 entry gate `:615`: "Per primitive: scalar reference + checkasm differential present
  BEFORE wiring." The load-bearing pre-wiring ordering clause.
- W3 exit gate `:642`: `native_simd_status ∈ {parity-pass, checkasm-pass}` per landed
  primitive — measurable, per-primitive.
- W3 revert `:654`: kernels + scalar twins + checkasm reverted as one slice; P3-C `:114`
  adds the finer per-primitive revert (a single failing kernel reverts to its scalar twin
  without reverting the whole wave) — cost-correct and stronger than a whole-wave revert.
- L6 cost discipline: scalar running-balance is the SHIPPED/DEFAULT body; CTZ-ranges is
  consumer-only + parity-gated (REDRESS-89 guard) — SPEC `:619`, P3-A `:120,:125`,
  P3-C §2.3(d). Prevents a higher-cost CTZ default shipping unprofiled.
- L5 stays clear of PMULL (REDRESS-88): the `escape_mask_64` `overflowing_add` carry
  idiom — SPEC `:628`, P3-A `:109`, P3-C `:113`(e).
- W3 RE-PROFILE entry gate `:611-614` forbids landing a kernel unless the scan leaf
  survives top-N on the benched tape path (the orphan-kernel tripwire) — no orphan kernel
  ships.

**Disposition CH4-1 (net-new NEON cost gate): ACCEPT.** SPEC `:599-602,:615,:642,:654`.
The net-new primitives carry scalar-ref + checkasm-new + same-wave consumer (S1/G3
composition), are gated BEFORE wiring, profile-first. Unchanged from V1 ACCEPT; the V2
packet did not regress this axis.

---

## §4 — DEFECTS (cost-axis inconsistencies remaining in V2)

### CH4-6 — L4 same-wave-consumer placement contradictory across the cohort (the residual twin of the V1 CH4-3 L7 fix). REVISE.

CH4's "same-wave consumer per primitive" axis requires L4 (tokenize-once shared-scan
reuse) land its consumer in ONE unambiguous wave. The V1 CHALLENGE caught exactly this
class for L7 (V1 CH4-3) and the V2 fold moved L7 single-valued to W1 — but it did NOT
reconcile L4, which is now placed in two different waves across the cohort:

- **SPEC (binding) places L4 in W2.** SPEC §5 W2 candidate list `:498-499`: "+ **L4**
  (tokenize-once reuse — consume W3's structural index ONCE; if W3 has not landed, L4
  reuses the W1 single-walk)". W2 task 4 `:544`: "Consume the structural index ONCE (L4);
  the index IS the tape." SPEC §6 W3 candidate list `:589` is "**L1** … + **L5** … +
  **L6**" — L4 is NOT a W3 candidate; W3 task 3 `:630` only says "the tape consumes the
  index ONCE (L4 reuse, same substrate)" referencing the reuse, not landing L4 there.
- **P3-B (V2) places L4 in W2.** `p3b:113-117`: "Note on L4 placement. … Its wave is
  **W2** (`SPEC.md:471` … the projection wave consumes the structural index once) … L4 is
  the projection generator's index consumer, not a NEON primitive; it lands in W2 with the
  rich rider." Manifest `p3b:127` lists L4 under W2; `p3b:308-309` W3 explicitly says "L4
  tokenize-once … are NOT new W3 candidates — L4 landed in W2"; topological summary
  `p3b:413` "L4 lands in W2 (the projection index consumer) … [CH4-3]".
- **P3-A places L4 in W3.** Lever map `p3a:40`: "Lever 3 (NEON structural pre-scan) =
  **L1** classifier + **L4** tokenize-once + **L5** comment mask + **L6** bracket mask."
  S4 same-wave-consumer `p3a:102`: "It is the consumer half of S1's producer — same wave
  (W3)." S4 falsifiability `p3a:103`: "Measured via the W3 corpus rows." (P3-A §3 rollup
  `p3a:169` separately omits L4 from "S3/S8 land W2 projection" — internally P3-A treats
  L4 as W3-only.)
- **P3-C places L4 in W3.** §1.3 wave→candidate table `p3c:50`: the W3 row reads "L1
  eq-set classifier · **L4 tokenize-once reuse** · L5 … · L6 (L7 is NOT here — L7
  single-valued to W1)". §2.3 header `p3c:104`: "**W3: NEON structural index (L1 + L4 +
  L5 + L6 + L7)**". §2.3 purpose `p3c:106` narrates "L4 tokenize-once kills the 2–3×
  re-walk" as a W3 landing. §4.1 `p3c:189` keys "L1/L4 index == tape-offsets identity →
  W3 exit gate (c)" — the identity correctly belongs to the NEON wave, but P3-C conflates
  the *identity check* (W3) with *where L4 lands* (W2 per SPEC).

This is a genuine CH4 same-wave-consumer defect, not a cosmetic one. L4's consumer
determines which commit must contain it: under the SPEC/P3-B placement, L4's consumer is
the **W2 projection generator's walk** (L4 is "the projection generator's index consumer",
`p3b:117`); under the P3-A/P3-C placement, L4's consumer is the **W3 NEON `Vec<u32>`
index** (L4 is the tokenize-once reuse of the freshly-landed scan). They cannot both bind:
an orchestrator reading the SPEC dispatches L4-in-W2 (its consumer is the projection); one
reading P3-A/P3-C dispatches L4-in-W3 (its consumer is the NEON scan). This is precisely
the orphan-REVISE class ORCHESTRATOR §3Z forbids — a candidate the cohort does not place
in one agreed wave. P3-C §2.3 also still lists L7 in its §2.3 header (`:104` "L1 + L4 +
L5 + L6 + L7") even though P3-C's own §1.3 table `:50` says "L7 is NOT here" — the §2.3
header was not updated when L7 was moved out, dragging L4 along with it.

The SPEC is THE binding contract (PASS-3 §9; SPEC `:9` W0–W5 wave plan), and the SPEC's
W2 placement is internally coherent: L4 reuses the W1 single-walk if W3 has not landed, so
L4's W2 consumer (the projection walk) exists at W2 regardless of W3 (SPEC `:499`). So the
SPEC's L4-in-W2 stands; **P3-A and P3-C are the stale artefacts and must reconcile to it**,
exactly as P3-B already did for both L4 (`p3b:113-117`) and L7.

**Concrete fix (V3):** make L4 placement single-valued (W2) across the cohort, matching
the SPEC and P3-B:
- `p3a:40`: move L4 out of the "Lever 3 (NEON)" line into the lever-2/projection mapping
  (or annotate "L4 lands in W2 as the projection-walk index consumer, not the NEON wave");
  the lever-to-candidate map may keep L4 under lever 3's *kernel family* but the wave
  placement must read W2.
- `p3a:102`: change S4 same-wave-consumer "same wave (W3)" → "same wave (W2) — L4 is the
  W2 projection walk's index consumer (SPEC `:498-499`); if W3 has not landed the NEON
  index, L4 reuses the W1 single-walk".
- `p3a:103`: change "Measured via the W3 corpus rows" → "Measured via the W2 projection
  exit gate (the re-walk elimination must not regress the equality count); the >SOTA delta
  is W3".
- `p3c:50`: move L4 from the W3 row to the W2 row of the §1.3 table (the W2 row currently
  reads "L3 generalization · W5C retire" — add "· L4 tokenize-once reuse").
- `p3c:104`: change the §2.3 header "W3: NEON structural index (L1 + L4 + L5 + L6 + L7)" →
  "W3: NEON structural index (L1 + L5 + L6)" — dropping BOTH the already-moved L7 and L4
  (the header was never updated after the L7 fold; this corrects both).
- `p3c:106`: re-word the §2.3 purpose so L4 is referenced as the W2 consumer of the index
  W3 produces ("W3 produces the `Vec<u32>` index that L4 — landed in W2 — reuses"), not as
  a W3 landing.
- `p3c:189` §4.1 cond-1 is correct as written (the *identity* is checked at the W3 exit
  gate); leave it, since the W3 gate is where the `Vec<u32>`==`offsets` identity is
  verified even though L4 itself lands in W2.

This is a documentation-reconciliation REVISE on P3-A + P3-C only; the SPEC does not change
(its L4-in-W2 placement is the binding, coherent one). Routes to the V3 P3-A + P3-C
redispatch. ≤10 LOC of edits across the two artefacts.

---

## §5 — Axis-by-axis CH4 summary

| CH4 axis | Status | Evidence / defect |
|---|---|---|
| Every wave has LOC budget | PASS | SPEC `:264-269` all six waves |
| Every wave has hard cap | PASS | SPEC `:264-269` all ≤90 min |
| Every wave has phase breakdown | PASS | global `:282-289` (20/15/30, mirrors SK-V8 + matches P3-B; V1 CH4-4 folded) |
| Same-wave consumer per primitive | PARTIAL | present per wave (`:377,:478,:568,:645,:713,:763`); L4 placement contradictory across cohort → CH4-6 REVISE |
| Every wave has revert protocol | PASS | SPEC `:384,:487,:576,:654,:720,:771` |
| Wave count ≤ 12 | PASS | 6 waves (W0–W5), SPEC `:271` |
| Shortlist ≤ 8 | PASS | 8 active (L1–L8); L9 conditional, not counted — SPEC `:271`, P3-A `:48,:56-58` |
| Net-new NEON scalar-ref + checkasm-new BEFORE wiring | PASS | L5/L6 net-new (kernel + scalar + checkasm) confirmed absent; SPEC `:599-602,:615,:642` binds scalar+checkasm pre-wiring |
| W2 conditional LOC ceiling fit-proof defined | PASS | SPEC `:501-508` defines it (V1 CH4-5 folded) |
| Phase numbers cohort-consistent | PASS | SPEC `:282-289` 20/15/30 == P3-B (V1 CH4-4 folded) |
| Wave-numbering cohort consistency | PASS | P3-B re-authored to 6-wave (V1 CH4-2 folded) |
| L7 placement cohort-consistent | PASS | L7 single-valued to W1 (V1 CH4-3 folded) |
| L4 placement cohort-consistent | FAIL | SPEC/P3-B = W2; P3-A/P3-C = W3 → CH4-6 REVISE |

---

## §6 — Dispositions (counts + path:line + fix)

- **CH4-1 net-new NEON cost gate (L5/L6 scalar-ref + checkasm-new before wiring): ACCEPT.**
  SPEC `:599-602,:615,:642,:654`. Kernel + scalar + checkasm all confirmed net-new absent
  at HEAD; profile-first, same-wave-consumed, orphan-kernel tripwire at `:611-614`. No
  change required.
- **CH4-6 L4 same-wave-consumer placement contradictory (SPEC/P3-B = W2 vs P3-A/P3-C =
  W3): REVISE.** Fix: single-value L4 to W2 across the cohort (SPEC `:498-499` + P3-B
  `:113-117` already W2-consistent); reconcile `p3a:40,:102,:103` and
  `p3c:50,:104,:106` to the SPEC's W2 placement; the §2.3 header `p3c:104` drop BOTH the
  already-moved L7 and L4. SPEC unchanged. ≤10 LOC edits across P3-A + P3-C.

**Counts:** ACCEPT 1 · REVISE 1 · REJECT 0 · total dispositions 2.

The V2 packet folded all four V1 REVISEs cleanly: the SPEC's two binding edits (phase
numbers CH4-4, fit-proof definition CH4-5) are present and correct; the two cohort-
reconciliation items (5-wave→6-wave CH4-2, L7-to-W1 CH4-3) are resolved in P3-B + P3-C.
The SPEC itself is structurally CH4-sound on every load-bearing axis: LOC budgets, hard
caps, phase breakdown, revert protocols, same-wave consumers, wave count ≤12, shortlist
≤8, and net-new NEON scalar+checkasm-before-wiring all PRESENT and correct, and the shape
mirrors SK-V8.

The single remaining defect (CH4-6) is the residual twin of the V1 CH4-3 L7 fix: the V2
fold moved L7 single-valued to W1 but left L4 placed in W2 (SPEC + P3-B) versus W3 (P3-A +
P3-C). It is a cohort-reconciliation REVISE on P3-A + P3-C only — the binding SPEC is
already coherent (L4-in-W2) and does not change. No orphan kernel, no missing budget, no
missing cap, no missing revert protocol, no missing phase breakdown. CH4-6 routes to the
V3 P3-A + P3-C redispatch and does not require re-architecting any wave.

---

## §7 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (the binding wave plan): `:262-269` manifest,
  `:271` wave/shortlist count, `:282-289` phase caps (20/15/30), `:294-303` rerun
  ceilings, Sections 3–8 per-wave (`:337,:390,:494,:583,:663,:725`), `:391,:430,:498-499`
  L7/L4 placement, `:501-508` W2 fit proof, `:599-602,:615,:642,:654` W3 NEON, revert
  protocols `:384,:487,:576,:654,:720,:771`.
- `restart/skinny/tranches/sk-v17/research/p3/p3a-candidate-shortlist.md` `:40` (lever-3
  L4), `:48,:56-58` (8-active/L9-conditional), `:102-103` (S4 same-wave-consumer = W3,
  the defect), `:106-126` (L5/L6 net-new scalar/checkasm), `:131-135` (S7/L7 W1).
- `restart/skinny/tranches/sk-v17/research/p3/p3b-wave-sequencing.md` `:3,:10-31` (V2
  six-wave fold note), `:100-117` (L7-W1 + L4-W2 placement), `:121-136` (W0–W5 manifest),
  `:156,:208,:262,:324,:369,:389` (20/15/30 per wave), `:403-413` (topological summary).
- `restart/skinny/tranches/sk-v17/research/p3/p3c-falsifiability-gates.md` `:5` (V2
  six-wave), `:50` (§1.3 table, L4 in W3 + L7-not-here), `:104,:106` (§2.3 header/purpose
  list L4+L7 in W3, the defect), `:114` (per-primitive revert), `:189` (L1/L4 identity at
  W3 exit).
- `restart/skinny/tranches/sk-v17/research/p3/p3f-spec-draft.md` `:31-33` (CH4-5 fit-proof
  fold note), `:46,:107,:121` (6-wave W0–W5 + LOC budgets).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V1/CH4.md` (the V1 dispositions
  folded: CH4-1 ACCEPT, CH4-2/3/4/5 REVISE).
- `restart/skinny/tranches/sk-v8/SPEC.md` `:7,:9-15` (mirrored manifest shape — per-wave
  LOC-budget column + global phase table + ≤650 fit-proof escape on the highest-LOC wave).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH4, §7 hard caps, §8 axes.
- Source verification (HEAD `f87ee713a`): `bbnf-simd/src/aarch64/{comment_body,bracket_depth}_mask_64.rs`
  + `scalar/{comment_body,bracket_depth}_mask_64.rs` + `tests/checkasm_{comment_body,bracket_depth}_mask_64.rs`
  ALL ABSENT (net-new confirmed); `scalar/byte_class_from_eq_set_64.rs` +
  `tests/checkasm_byte_class_from_eq_set_64.rs` PRESENT (L1 scalar+checkasm).
- Memory `dispatch-hard-cap` (research 20 / plan 15 / redress 30 default — the SPEC's
  phase-cap numbers).

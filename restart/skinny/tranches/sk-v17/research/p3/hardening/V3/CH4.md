# SK-V17 S-P3 CHALLENGE — CH4 COST (V3)

Lens: CH4 COST. Cycle: V3. Date: 2026-05-29. Master HEAD: `f87ee713a`.
Subject: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Mandate (PASS-3 §3 CH4 + ORCHESTRATOR §3W): every wave carries a LOC budget, a hard cap,
a research/plan/redress phase breakdown, and a same-wave-consumer requirement per
primitive; wave count ≤ 12; shortlist ≤ 8; net-new NEON (L5/L6) carries scalar-ref +
checkasm-new BEFORE wiring; every wave has a revert protocol.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Each carries `path:line` + concrete fix.

---

## §0 — Verification ledger (re-confirmed at HEAD this cycle)

| Check | Result | Evidence |
|---|---|---|
| Master HEAD = `f87ee713a` | CONFIRMED | `git rev-parse --short HEAD` = `f87ee713a` |
| L5 kernel `comment_body_mask_64.rs` (aarch64 + scalar) absent today | CONFIRMED net-new | `ls bbnf-simd/src/{aarch64,scalar}/` → no `comment_body_mask_64.rs` |
| L6 kernel `bracket_depth_mask_64.rs` (aarch64 + scalar) absent today | CONFIRMED net-new | `ls bbnf-simd/src/{aarch64,scalar}/` → no `bracket_depth_mask_64.rs` |
| L5/L6 checkasm-new absent today | CONFIRMED net-new | `ls bbnf-simd/tests/` → no `checkasm_comment_body_mask_64.rs` / `checkasm_bracket_depth_mask_64.rs` |
| L1 scalar twin + checkasm PRESENT | CONFIRMED | `bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs` + `tests/checkasm_byte_class_from_eq_set_64.rs` present |
| SPEC wave count | 6 (W0–W5), Sections 3–8 | SPEC `:262-269` manifest; sections `:337/:390/:494/:588/:668/:730` |
| SPEC active shortlist | 8 (L1–L8); L9 conditional NOT counted | SPEC `:271` |
| SK-V8 SPEC (mirrored shape) manifest form | per-wave LOC-budget column + global phase-cap table; ≤650 fit-proof escape on highest-LOC wave | sk-v8/SPEC.md `:7,:9-15` |

The net-new gate's load-bearing premise (L5/L6 do not yet exist as kernel, scalar, or
checkasm) is re-verified true at HEAD this cycle — not inherited from V1/V2.

---

## §1 — V2 fold audit (the one V2 REVISE this cycle had to fold)

V2 CH4 returned 1 ACCEPT (CH4-1 net-new NEON gate) + 1 REVISE (CH4-6 L4 same-wave-consumer
placement contradictory across the cohort: SPEC/P3-B placed L4 in W2, P3-A/P3-C placed L4
in W3). This was the residual twin of the V1 CH4-3 L7→W1 fold. Verified folded:

| V2 item | V2 verdict | Folded in V3? | Evidence |
|---|---|---|---|
| CH4-6 L4 placement contradictory (SPEC/P3-B = W2 vs P3-A/P3-C = W3) | REVISE | **FOLDED** | P3-A `:40` "**L4** is NOT a NEON kernel … L4 single-values to **W2** per the binding SPEC (`SPEC.md:494–499`) … (the CH4-6 fold, twin of the L7→W1 fold)"; P3-A `:96-102` S4 same-wave-consumer "S4 lands in **W2** … single-valued to W2 across the cohort (the CH4-6 fold)"; P3-C `:49` §1.3 W2 row now carries "L4 tokenize-once reuse"; P3-C `:50` §1.3 W3 row now reads "L1 eq-set classifier · L5 … · L6 (L7 single-valued to W1 …; L4 single-valued to W2, `SPEC.md:497-499`)"; P3-C `:104` §2.3 header now "**W3: NEON structural index (L1 + L5 + L6)**" — BOTH L7 and L4 dropped from the header; P3-C `:106` "**L4 and L7 do NOT land here**" |

The V2 CH4-6 REVISE is genuinely folded — not paper-folded. The two stale artefacts
(P3-A, P3-C) were the ones flagged in V2 §4, and both now read L4-in-W2 matching the
binding SPEC and P3-B. The §2.3 header `p3c:104` — which V2 flagged as still listing both
the already-moved L7 and the to-be-moved L4 — now reads `(L1 + L5 + L6)`, correcting both.
The binding SPEC did not change (its L4-in-W2 placement was already coherent in V2); the
cohort reconciled to it, exactly as V2 prescribed. No orphan REVISE survives.

---

## §2 — Per-wave cost-axis matrix (LOC · hard cap · phase breakdown · same-wave consumer · revert)

Every wave of the SPEC audited against the five CH4 axes. The manifest table at SPEC
`:262-269` and the phase-cap table at `:282-290` are the global cost ledger; the per-wave
sections (Sections 3–8) carry same-wave-consumer + revert + LOC notes; P3-B carries the
per-wave research/plan/redress breakdown.

| Wave | LOC budget | Hard cap | Phase breakdown | Same-wave consumer | Revert protocol | Verdict |
|---|---|---|---|---|---|---|
| W0 | `:264` 0 behavior + ≤300 harness/gate/test | `:264` ≤90 min | p3b `:175` 20/15/30 | `:377` gate-json consumes every field | `:384` present | ACCEPT |
| W1 | `:265` ≤450 src/test; gen named separately | `:265` ≤90 min | p3b `:232-233` 20/15/30 + CHALLENGE 90 | `:399,:478` L3 cursor IS L2's consumer; L7 sized by L2 | `:487` present | ACCEPT |
| W2 | `:266` ≤450 default / ≤650 w/ defined fit proof `:501-508` | `:266` ≤90 min | p3b `:287-290` 20/15/30 + CHALLENGE 90 | `:573-574` generated projection reads tape; L8 read by L3; L4 reuses index ONCE | `:581` present | ACCEPT |
| W3 | `:267` ≤450 src/test; gen SIMD named | `:267` ≤90 min | p3b `:350-352` 20/15/30 + CHALLENGE 90 | `:650-651` tape decode consumes `Vec<u32>` index; scan+tape together-or-neither | `:659` present | ACCEPT |
| W4 | `:268` ≤300 src/test | `:268` ≤90 min | p3b `:395-397` 20/15/30 | `:718-719` post-W1 recognizer spine | `:725` present | ACCEPT |
| W5 | `:269` 0 default / ≤150 Lock-14 cleanup | `:269` ≤90 min | p3b `:415-417` 20/15/30 | `:768-769` close checklist + regen gate | `:776` present | ACCEPT |

**Result: every wave carries an LOC budget, a ≤90-min hard cap, a per-wave 20/15/30
phase breakdown, a same-wave consumer, and a revert protocol.** All five mandatory
axes present per wave. Wave count = 6 (≤12 ceiling, SPEC `:271`, P3-B `:150`). Active
shortlist = 8 (L1–L8; L9 conditional, correctly NOT counted — SPEC `:271`, P3-A `:56-58`).
The phase-cap form mirrors SK-V8 (`sk-v8/SPEC.md:7,:9-15` — per-wave LOC-budget column +
global phase table; the W2 ≤650 fit-proof escape on the highest-LOC wave); the SPEC does
NOT invent a new shape.

Global cost ledger is internally consistent: SPEC phase-cap table `:282-290` (Research 20 /
Plan 15 / Implementation-redress 30; CHALLENGE 90 when first-of-class) == P3-B per-wave
breakdowns (`:175,:232,:287,:350,:395,:415`) == memory `dispatch-hard-cap` (research 20 /
plan 15 / redress 30). No numeric conflict survives (the V1 CH4-4 conflict stays folded).

---

## §3 — Net-new NEON primitive cost gate (L5/L6) — the highest-cost-risk axis

CH4's net-new clause: L5 (`comment_body_mask_64`) and L6 (`bracket_depth_mask_64`) MUST
carry scalar-ref + checkasm-new BEFORE wiring. Re-verified at source this cycle: both
kernels, both scalar twins, AND both checkasm differentials are genuinely ABSENT at HEAD
(§0 ledger). The SPEC binds them correctly:

- **Global non-negotiable** SPEC `:245-246`: "Scalar reference + checkasm parity required
  before primitive wiring. Per SIMD primitive: scalar twin + checkasm differential, both
  pre-wiring." Plus `:247-248` "Same-wave consumer per primitive … No orphan kernel ships."
  The cost gate is bound at the SPEC §1 level, not only inside W3.
- **W3 owner paths** name all six net-new files: `aarch64/comment_body_mask_64.rs` +
  `scalar/comment_body_mask_64.rs` + `tests/checkasm_comment_body_mask_64.rs` (L5, SPEC
  `:604-605`) and the bracket triple (L6, SPEC `:606-607`).
- **W3 entry gate** `:620`: "Per primitive: scalar reference + checkasm differential
  present BEFORE wiring." The load-bearing pre-wiring ordering clause.
- **W3 exit gate** `:646`: `native_simd_status ∈ {parity-pass, checkasm-pass}` per landed
  primitive — measurable, per-primitive. Telemetry field declared SPEC `:179`.
- **W3 revert** `:659`: kernels + scalar twins + checkasm reverted as one slice; P3-C
  `:114`(b)/exit-gate adds the per-primitive granularity (each new kernel lands scalar-ref
  + checkasm + same-wave consumer or it does not land) — cost-correct.
- **L6 cost discipline**: scalar running-balance is the SHIPPED/DEFAULT body; CTZ-ranges is
  consumer-only + parity-gated (REDRESS-89 guard) — SPEC `:622-623`, `:634`, P3-A `:120`,
  P3-C `:113`(d). Prevents a higher-cost CTZ default shipping unprofiled.
- **L5 stays clear of PMULL** (REDRESS-88): the `escape_mask_64` `overflowing_add` carry
  idiom — SPEC `:631-632`, P3-A `:109`, P3-C `:113`(e).
- **W3 RE-PROFILE entry gate** `:616-619`: forbids landing a kernel unless the scan leaf
  survives top-N on the benched tape path (the orphan-kernel tripwire) — no orphan kernel
  ships; if no scan leaf survives, W3 lands no kernel and the >SOTA gate is evaluated on
  the W2 plane.
- **Same-wave consumer** SPEC `:650-651`: "the tape's structural decode consumes the
  `Vec<u32>` index in the same commit; scan + tape land together or neither" — L1/L5/L6 all
  consumed by the tape decode in the W3 commit (P3-A `:69,:113,:124` name S1 as the
  composition consumer).

**Disposition CH4-1 (net-new NEON cost gate): ACCEPT.** SPEC `:245-246,:604-607,:620,:646,
:659`. The net-new primitives carry scalar-ref + checkasm-new + same-wave consumer, are
gated BEFORE wiring at both the SPEC §1 global level and the W3 entry gate, and are
profile-first behind the orphan-kernel tripwire. Unchanged from V1/V2 ACCEPT; the V3
cohort did not regress this axis.

---

## §4 — Cost-axis residual sweep (no new defects)

A full sweep of the five axes plus the two count ceilings and the net-new gate surfaces no
new CH4 defect this cycle:

- **L4 placement (V2 CH4-6)** — RESOLVED §1. Single-valued to W2 across SPEC `:494-499,
  :544`, P3-A `:40,:102`, P3-B `:113-117` (L4-W2 note, unchanged from V2), P3-C `:49,:50,
  :104,:106`. No orphan REVISE.
- **L7 placement (V1 CH4-3)** — STAYS folded. Single-valued to W1: SPEC `:396,:446-448`,
  P3-A `:131,:135`, P3-B `:116-119`, P3-C `:50,:106`. The §2.3 header `p3c:104` no longer
  lists L7.
- **L2's same-wave consumer** is L3, named in the same commit — SPEC `:399` "L2's same-wave
  consumer is L3 (P2 §L3); they land together or neither", `:478`. No orphan kernel.
- **L8's same-wave consumer** is L3 (the projection reads the flags), same wave — SPEC
  `:574`, P3-A `:146`. L8 rides the EXISTING sparse pair (no new vector, no widened record)
  — SPEC `:515-516`, P3-A `:142`; zero net-new LOC for the substrate, bounded cost.
- **L9 (conditional)** carries a measurable ≥+5% gate, NOT a paper-close — SPEC `:711-715`
  "A lift below +5% disposes L9 as NOT-WARRANTED (recorded measurably, not a failure)". W4
  dispatches only if the post-W1 re-profile fires the gate (SPEC `:670-680,:695-697`); not
  counted against the active 8 (P3-A `:56-58`). Its conditional status means it adds no
  fixed cost to the wave budget unless the gate fires.
- **W2 ≤650 fit-proof** is DEFINED (V1 CH4-5 stays folded) — SPEC `:501-508`: a pre-redress
  per-artefact LOC accounting, ≤5 LOC of plan prose, CHALLENGE-accepted, with the over-450
  attributed to a named intrinsic cause; without it ≤450 binds. P3-B `:289-290` mirrors.
- **Generated output does not consume the source LOC budget** but is named + diff-audited +
  in the revert slice — SPEC `:276-277`; every wave that emits generated output names it
  (`:424` W1, `:514` W2, `:608` W3). No hidden generated-LOC sprawl.
- **Rerun ceilings** are bound per wave (SPEC `:294-303`): one gate refresh + one confirm;
  extra reruns are REDRESS cost evidence, not retry room — caps the verification cost.

---

## §5 — Axis-by-axis CH4 summary

| CH4 axis | Status | Evidence / note |
|---|---|---|
| Every wave has LOC budget | PASS | SPEC `:264-269` all six waves |
| Every wave has hard cap | PASS | SPEC `:264-269` all ≤90 min |
| Every wave has phase breakdown (R/P/redress) | PASS | global `:282-290` (20/15/30) + P3-B per-wave `:175,:232,:287,:350,:395,:415`; CHALLENGE 90 first-of-class |
| Same-wave consumer per primitive | PASS | `:377,:399,:478,:574,:650-651,:718-719,:768`; L4 cohort-consistent (W2) — CH4-6 folded |
| Every wave has revert protocol | PASS | SPEC `:384,:487,:581,:659,:725,:776` |
| Wave count ≤ 12 | PASS | 6 waves (W0–W5), SPEC `:271`, P3-B `:150` |
| Shortlist ≤ 8 | PASS | 8 active (L1–L8); L9 conditional, not counted — SPEC `:271`, P3-A `:56-58` |
| Net-new NEON scalar-ref + checkasm-new BEFORE wiring | PASS | L5/L6 net-new (kernel + scalar + checkasm) confirmed absent at HEAD; SPEC `:245-246,:604-607,:620,:646` binds scalar+checkasm pre-wiring; orphan-kernel tripwire `:616-619` |
| W2 conditional LOC ceiling fit-proof defined | PASS | SPEC `:501-508` (V1 CH4-5 stays folded) |
| Phase numbers cohort-consistent | PASS | SPEC `:282-290` 20/15/30 == P3-B (V1 CH4-4 stays folded) |
| Wave-numbering cohort consistency | PASS | P3-B six-wave (V1 CH4-2 stays folded) |
| L7 placement cohort-consistent | PASS | L7 single-valued to W1 (V1 CH4-3 stays folded) |
| L4 placement cohort-consistent | PASS | SPEC/P3-A/P3-B/P3-C all = W2 (V2 CH4-6 folded) |
| W4 conditional closes on measurement, not promise | PASS | ≥+5% gate SPEC `:711-715`; below-band = NOT-WARRANTED recorded, not paper-close |

---

## §6 — Dispositions (counts + path:line + fix)

- **CH4-1 net-new NEON cost gate (L5/L6 scalar-ref + checkasm-new before wiring): ACCEPT.**
  SPEC `:245-246,:604-607,:620,:646,:659`. Kernel + scalar + checkasm all re-confirmed
  net-new absent at HEAD; profile-first behind the orphan-kernel tripwire (`:616-619`),
  same-wave-consumed (S1 composition), gated BEFORE wiring at both SPEC §1 and the W3 entry
  gate. No change required.

**Counts:** ACCEPT 1 · REVISE 0 · REJECT 0 · total dispositions 1.

The V3 cohort folded the single V2 CH4-6 REVISE cleanly: L4 is now single-valued to W2
across all four artefacts (SPEC, P3-A, P3-B, P3-C), reconciling to the binding SPEC exactly
as V2 prescribed; the `p3c:104` §2.3 header that V2 flagged as carrying stale L7+L4 now
reads `(L1 + L5 + L6)`. Every load-bearing cost axis holds on the binding SPEC: LOC budgets,
≤90-min hard caps, per-wave 20/15/30 phase breakdowns, revert protocols, same-wave
consumers, wave count = 6 (≤12), active shortlist = 8 (≤8, L9 conditional uncounted), the
W2 fit-proof, and net-new NEON scalar+checkasm-before-wiring — all PRESENT and correct, and
the shape mirrors SK-V8. No orphan kernel, no missing budget, no missing cap, no missing
revert protocol, no missing phase breakdown, no cohort placement contradiction. CH4 returns
a clean cycle: 1 ACCEPT, 0 REVISE, 0 REJECT.

---

## §7 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (the binding wave plan): `:245-248` (SIMD
  scalar+checkasm-before-wiring + same-wave-consumer + no-deferrals non-negotiables),
  `:262-269` manifest, `:271` wave/shortlist count, `:276-277` generated-LOC rule,
  `:282-290` phase caps (20/15/30 + CHALLENGE 90), `:294-303` rerun ceilings, Sections 3–8
  per-wave (`:337,:390,:494,:588,:668,:730`), `:396,:446-448` L7-W1, `:494-499,:544` L4-W2,
  `:501-508` W2 fit proof, `:604-607,:616-620,:646,:659` W3 NEON, `:711-715` W4 ≥+5% gate,
  revert protocols `:384,:487,:581,:659,:725,:776`, telemetry `:176,:179-180`.
- `restart/skinny/tranches/sk-v17/research/p3/p3a-candidate-shortlist.md` `:40` (L4-W2
  CH4-6 fold), `:56-58` (8-active/L9-conditional), `:67-69` (L1 scalar+checkasm+S2
  consumer), `:96-102` (S4/L4 W2 single-valued), `:107-113` (L5 net-new scalar/checkasm/S1),
  `:118-124` (L6 net-new scalar-default/checkasm/S1), `:129-135` (L7/S7 W1), `:140-146`
  (L8/S8 W2), `:153-160` (L9 conditional ≥+5% admission gate).
- `restart/skinny/tranches/sk-v17/research/p3/p3b-wave-sequencing.md` `:116-119` (L7-W1
  note), `:113-117` (L4-W2 note, unchanged from V2), `:150` (6-wave / 8-active counts),
  `:175,:232-233,:287-290,:350-352,:395-397,:415-417` (per-wave 20/15/30 + CHALLENGE 90 +
  LOC budgets).
- `restart/skinny/tranches/sk-v17/research/p3/p3c-falsifiability-gates.md` `:49-50` (§1.3
  table: L4 in W2 row, W3 row drops L4+L7), `:104` (§2.3 header now `L1 + L5 + L6`), `:106`
  ("L4 and L7 do NOT land here"), `:112-113` (W3 maintain budget + exit gate b/d/e
  scalar+checkasm), `:189` (L1/L4 identity at W3 exit gate (c) + W2 exit gate (g)).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V2/CH4.md` (the V2 dispositions
  folded: CH4-1 ACCEPT, CH4-6 REVISE).
- `restart/skinny/tranches/sk-v8/SPEC.md` `:7,:9-15` (mirrored manifest shape — per-wave
  LOC-budget column + global phase table + ≤650 fit-proof escape on the highest-LOC wave).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH4, §7 hard caps, §8 axes.
- Source verification (HEAD `f87ee713a`): `bbnf-simd/src/{aarch64,scalar}/comment_body_mask_64.rs`,
  `.../bracket_depth_mask_64.rs`, `tests/checkasm_{comment_body,bracket_depth}_mask_64.rs`
  ALL ABSENT (net-new confirmed); `scalar/byte_class_from_eq_set_64.rs` +
  `tests/checkasm_byte_class_from_eq_set_64.rs` PRESENT (L1 scalar+checkasm).
- Memory `dispatch-hard-cap` (research 20 / plan 15 / redress 30 — the SPEC's phase-cap
  numbers).

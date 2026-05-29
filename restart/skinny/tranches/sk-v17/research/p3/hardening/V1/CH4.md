# SK-V17 S-P3 CHALLENGE — CH4 COST (V1)

Lens: CH4 COST. Cycle: V1. Date: 2026-05-29. Master HEAD: `f87ee713a`.
Subject: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Mandate (PASS-3 §3 CH4 + ORCHESTRATOR §3W): every wave carries a LOC budget, a hard
cap, a research/plan/redress phase breakdown, and a same-wave-consumer requirement per
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
| L1 scalar twin + checkasm PRESENT | CONFIRMED | `scalar/byte_class_from_eq_set_64.rs` + `tests/checkasm_byte_class_from_eq_set_64.rs` present |
| SPEC wave count | 6 (W0–W5), Sections 3–8 | SPEC `:257-264`, `:329/:382/:466/:534/:614/:669` |
| SK-V8 SPEC (mirrored shape) manifest form | global phase-cap + per-wave `Impl/redress cap` column; NO per-wave research/plan/redress line | sk-v8/SPEC.md `:216-224` |

---

## §1 — Per-wave cost-axis matrix (LOC · hard cap · phase breakdown · same-wave consumer · revert)

Every wave of the SPEC was audited against the five CH4 axes. The manifest table at SPEC
`:257-264` and the phase-cap table at `:275-282` are the global cost ledger; the per-wave
sections (Sections 3–8) carry same-wave-consumer + revert + LOC notes.

| Wave | LOC budget | Hard cap | Phase breakdown | Same-wave consumer | Revert protocol | Verdict |
|---|---|---|---|---|---|---|
| W0 | `:259` 0 behavior + ≤300 harness/gate/test | `:259` ≤90 min | global `:275-282` | `:369` gate-json consumes every field | `:376` ACCEPT | ACCEPT |
| W1 | `:260` ≤450 src/test; gen named separately | `:260` ≤90 min | global `:275-282` | `:450` L3 cursor IS L2's consumer | `:459` ACCEPT | ACCEPT |
| W2 | `:261` ≤450 default / ≤650 w/ fit proof | `:261` ≤90 min | global `:275-282` | `:519` generated projection reads tape; L8 read by L3 | `:527` ACCEPT | ACCEPT |
| W3 | `:262` ≤450 src/test; gen SIMD named | `:262` ≤90 min | global `:275-282` | `:596` tape decode consumes `Vec<u32>` index | `:605` ACCEPT | ACCEPT |
| W4 | `:263` ≤300 src/test | `:263` ≤90 min | global `:275-282` | `:657` post-W1 recognizer spine | `:664` ACCEPT | ACCEPT |
| W5 | `:264` 0 default / ≤150 Lock-14 cleanup | `:264` ≤90 min | global `:275-282` | `:707` close checklist + regen gate | `:715` ACCEPT | ACCEPT |

**Result: every wave carries an LOC budget, a ≤90-min hard cap, a same-wave consumer, and
a revert protocol.** All five mandatory axes are present per wave. The wave count is 6
(≤12 ceiling). The active shortlist is 8 (L1–L8; L9 conditional, correctly NOT counted —
SPEC `:266`, P3-A `:48,:58`).

---

## §2 — Net-new NEON primitive cost gate (L5/L6) — the highest-cost-risk axis

CH4's net-new clause: L5 (`comment_body_mask_64`) and L6 (`bracket_depth_mask_64`) MUST
carry scalar-ref + checkasm-new BEFORE wiring. Verified at source: both kernels and both
scalar twins are genuinely ABSENT at HEAD (§0 ledger). The SPEC binds them correctly:

- W3 owner paths name all six net-new files: `aarch64/comment_body_mask_64.rs` +
  `scalar/comment_body_mask_64.rs` + `tests/checkasm_comment_body_mask_64.rs` (L5) and
  the bracket triple (L6) — SPEC `:550-553`.
- W3 entry gate `:566`: "Per primitive: scalar reference + checkasm differential present
  BEFORE wiring." This is the load-bearing pre-wiring ordering clause.
- W3 exit gate `:592`: `native_simd_status ∈ {parity-pass, checkasm-pass}` per landed
  primitive — measurable, per-primitive.
- W3 revert `:605`: kernels + scalar twins + checkasm reverted as one slice; P3-C `:114`
  adds the stronger per-primitive revert (a single failing kernel reverts to its scalar
  twin without reverting the whole wave) — a cost-correct, finer-grained revert that the
  SPEC summarises and P3-C details.
- L6 cost discipline: scalar running-balance is the SHIPPED/DEFAULT body; CTZ-ranges is
  consumer-only + parity-gated (REDRESS-89 guard) — SPEC `:570`, P3-A `:120,:125`,
  P3-C §2.3(d). This prevents a higher-cost CTZ default from shipping unprofiled.
- L5 stays clear of PMULL (REDRESS-88): uses the `escape_mask_64` `overflowing_add` carry
  idiom — SPEC `:579`, P3-A `:109`.

**Disposition CH4-1 (net-new NEON cost gate): ACCEPT.** SPEC `:550-553,:566,:592,:605`.
The net-new primitives carry scalar-ref + checkasm-new + same-wave consumer (S1/G3
composition) and are gated BEFORE wiring, profile-first (W3 RE-PROFILE entry gate `:563`
forbids landing a kernel unless the scan leaf survives top-N on the benched tape path —
the orphan-kernel tripwire). No orphan kernel ships.

---

## §3 — DEFECTS (cost-axis inconsistencies)

### CH4-2 — P3-B wave-numbering diverges from the binding SPEC (5-wave vs 6-wave). REVISE.

`p3b-wave-sequencing.md:53-59,:73-83,:255-261` sequences a **FIVE-wave** plan
(W0/W1/W2/W3/W4) with this lever-to-wave map: W1 = levers 1+2 (L2/L3/L8/L7), **W2 = lever
3 NEON (L1/L4/L5/L6/L7)**, **W3 = lever 4 commit-by-construction (L9, conditional)**, W4 =
close. P3-B `:54` states verbatim "a **five-wave** topological order (W0…W4)" and `:85`
"Wave count = 5 (W0-W4)".

The binding SPEC `:257-264` (Sections 3–8) and P3-C `:43-54` + P3-F `:81` all sequence a
**SIX-wave** plan: W1 = PRUNE+tape (L2/L3/L8), **W2 = layout projection generator**
(L3-full/L8/L4), **W3 = NEON** (L1/L5/L6), **W4 = commit-by-construction (L9)**, W5 =
close. P3-C `:54` states "Wave count = **6 (W0–W5)**". P3-F `:81` states "wave count is 6
(W0–W5)".

This is a real cost-axis defect: the same candidate set is bound to **different wave
ordinals** across the cohort. Under P3-B, NEON is W2 and L7 lands in W2; under the SPEC,
NEON is W3 and L7 lands in W1 (SPEC `:391` puts L7 in W1, gated behind L2). The owner of
"NEON wave" cannot be unambiguously dispatched: an orchestrator reading P3-B dispatches
"W2 = NEON"; reading the SPEC dispatches "W2 = projection, W3 = NEON." This is exactly the
orphan-REVISE class ORCHESTRATOR §3Z forbids — a wave the cohort does not agree on.

The SPEC is THE binding contract (PASS-3 §9; SPEC `:9` "W0–W5 wave plan"), so the SPEC's
6-wave structure stands; **P3-B is the stale artefact and must be reconciled to it.**

**Concrete fix (V2):** rewrite `p3b-wave-sequencing.md` to the 6-wave structure:
- `:53-59` change "five-wave (W0…W4)" → "six-wave (W0…W5)"; split the current W2 into W2
  (layout projection generator, L3-full/L8/L4) + W3 (NEON, L1/L5/L6); renumber the
  conditional L9 wave W3→W4 and close W4→W5.
- `:61-71` the L7-placement note: reconcile to the SPEC, which places L7 in W1 (`:391`,
  "gated behind L2"; if W3 has not landed the index, L7 sizes from a conservative
  byte-proportional bound) — NOT in the NEON wave as P3-B `:67` asserts. P3-B's "L7 in W2
  as the NEON-count consumer" contradicts SPEC `:430`. Either P3-B adopts the SPEC's
  W1-L7 placement, or the SPEC is the one in error — see CH4-3.
- `:75-87` rebuild the wave manifest table to W0–W5; `:255-261` rebuild the topological
  diagram to show W2 (projection) between W1 and W3.
This is a documentation-reconciliation REVISE on P3-B only; the SPEC does not change for
this item. Routes to the V2 P3-B redispatch.

### CH4-3 — L7 same-wave-consumer placement is internally contradictory across the cohort. REVISE.

CH4's "same-wave consumer per primitive" axis requires that L7 (one-shot SIMD reserve)
land its consumer in the SAME wave. The cohort places L7 inconsistently:

- SPEC `:391` lists L7 as a W1 candidate: "L7 (one-shot SIMD reserve, gated behind L2)";
  `:430` "L7 sizes `offsets` from the W3 scan count in one cold reserve (L7 is gated
  behind the tape; if W3 has not landed the index, L7 sizes from a conservative
  byte-proportional bound)."
- P3-C §1.3 table `:48` puts L7 in W1; but `:50` ALSO writes "L7 (if not in W1)" in the
  W3 row — hedging both placements.
- P3-B `:67` argues L7's activation "belongs in W2 [P3-B's NEON wave] ... L7 is therefore
  sequenced in W2 as the NEON-count consumer."
- P3-A S7 `:135` says L7's same-wave consumer is "S2 (the tape it sizes); gated behind
  S2/S3 + the NEON scan."

The cost defect: L7's consumer is the `offsets` reserve, but the COUNT it reserves from
is the NEON scan count, which does not exist until the NEON wave. If L7 lands in W1 (SPEC)
it must size from a byte-proportional bound (SPEC `:430`), then be RE-SIZED off the NEON
count in the NEON wave — that is TWO touches of L7 across two waves, which violates
"same-wave consumer per primitive in one commit" unless the SPEC explicitly accounts for
the W1→NEON-wave re-size as a single logical primitive landed in W1 with a NEON-wave
refinement. The SPEC `:430` does state the conservative-bound fallback, which makes the
W1 landing self-consistent (L7's consumer — the reserve — IS in W1), but the cohort text
(P3-B `:67`, P3-C `:50`) still asserts the contradictory "L7 in the NEON wave" placement.

**Concrete fix (V2):** make L7 placement single-valued across the cohort. Recommended
resolution (cheapest, SPEC-consistent): **L7 lands in W1** with the byte-proportional cold
reserve as its W1 consumer (SPEC `:430` already says this), and the NEON wave's
exact-count refinement is folded into the W1 L7 budget as a "W1 lands the reserve; the
NEON wave tightens the bound off the real count without a second L7 commit" note. Then:
- P3-B `:61-71,:80-81`: move L7 from the W2/NEON row to the W1 row; delete the "L7 in W2
  as the NEON-count consumer" paragraph.
- P3-C §1.3 `:48,:50`: delete the "L7 (if not in W1)" hedge in the W3 row; L7 is W1-only.
- SPEC `:540` W3 candidate list correctly does NOT list L7 (it lists L1/L5/L6) — SPEC is
  already consistent; the fix is purely P3-B + P3-C reconciliation to the SPEC.

### CH4-4 — Per-wave phase breakdown is global-only in the SPEC and numerically conflicts with P3-B. REVISE.

CH4 requires "a phase breakdown (research / plan / redress per `SKINNY-TRIUMVIRATE.md`)"
per wave. The SPEC carries the breakdown GLOBALLY (Phase caps table `:275-282`: Research
30 min / Plan 30 min / CHALLENGE 90 / Impl-redress 90), mirroring the SK-V8 SPEC shape
(sk-v8/SPEC.md `:216-224` — a single `Impl/redress cap` column + the global phase
convention, NO per-wave research/plan/redress line). The mirrored-shape global form
satisfies the "phase breakdown present" requirement and is INTERNALLY consistent.

The defect is a NUMERIC conflict with P3-B, which gives EACH wave a per-wave breakdown of
**research 20 / plan 15 / redress 30** (P3-B `:109,:144,:193,:233,:250`) — different
numbers from the SPEC's global research 30 / plan 30. Two values for the same phase budget
exist in the cohort. P3-B `:109` cites "dispatch-hard-cap default" (20/15/30
research/plan/redress per the memory `dispatch-hard-cap`), whereas the SPEC `:279-280`
cites SK-V8's research 30 / plan 30. Both are defensible reads of different sources, but
they cannot both bind.

**Concrete fix (V2):** reconcile the phase numbers to one source. The
`dispatch-hard-cap` discipline (research 20 / plan 15 / redress 30) is the
project-canonical default per the memory index and is the value P3-B used; PASS-3 §7's
phase table (Research 45 / etc.) is the OTHER candidate. Recommended: adopt the
`dispatch-hard-cap` 20/15/30 numbers in the SPEC `:277-282` phase table (replacing the
SK-V8-inherited 30/30) so the SPEC and P3-B agree, OR add a one-line per-wave phase note
to each SPEC wave section. Either is ≤15 LOC of doc edit. The SPEC's W2/W3 first-of-class
CHALLENGE 90-min wall (`:281`) is correct and stays. This is a numeric-reconciliation
REVISE; the global-phase-table form itself is ACCEPTABLE (matches the mirrored shape).

### CH4-5 — W2 LOC budget asymmetry (≤450/≤650) lacks a same-axis fit-proof definition. REVISE (minor).

SPEC `:261` grants W2 "≤450 source/test LOC default; ≤650 only with accepted pre-redress
fit proof." This is the ONLY wave with a conditional LOC ceiling. The "pre-redress fit
proof" term is not defined anywhere in the SPEC (grep: appears only at `:261` and the
rerun table `:290` does not define it). A cost ceiling whose escape hatch is undefined is
a cost-budget escape hatch CH4 must pin: what evidence promotes 450→650? Without a
definition, W2 can claim 650 LOC unboundedly.

This mirrors SK-V8 W3 (`:221` "≤650 only with accepted pre-redress fit proof"), so the
phrasing is inherited — but SK-V8 used it for the substrate-union wave (W3), whereas
SK-V17 applies it to the projection-generator wave (W2). The generator is the highest-LOC
wave (it emits document/value/view/visitor for every CSS grammar), so the 650 ceiling is
plausibly needed; the defect is only that the PROOF gate is undefined.

**Concrete fix (V2):** add one sentence to SPEC `:261` or the W2 section (`:466-532`)
defining the fit proof: e.g. "the pre-redress fit proof is a W2-research artefact showing
the four generated artefacts (document/value/view/visitor) cannot be emitted within 450
hand-written generator LOC for the CSS grammar set, with the per-artefact LOC estimate."
≤5 LOC doc edit. Until defined, the default ≤450 binds.

---

## §4 — Axis-by-axis CH4 summary

| CH4 axis | Status | Evidence / defect |
|---|---|---|
| Every wave has LOC budget | PASS | SPEC `:259-264` all six waves |
| Every wave has hard cap | PASS | SPEC `:259-264` all ≤90 min |
| Every wave has phase breakdown | PARTIAL | global `:275-282` present (mirrors SK-V8); numeric conflict with P3-B → CH4-4 REVISE |
| Same-wave consumer per primitive | PARTIAL | present per wave (`:369,:450,:519,:596,:657,:707`); L7 placement contradictory across cohort → CH4-3 REVISE |
| Every wave has revert protocol | PASS | SPEC `:376,:459,:527,:605,:664,:715` |
| Wave count ≤ 12 | PASS | 6 waves (W0–W5), SPEC `:266` |
| Shortlist ≤ 8 | PASS | 8 active (L1–L8); L9 conditional, not counted — SPEC `:266`, P3-A `:48,:58` |
| Net-new NEON scalar-ref + checkasm-new BEFORE wiring | PASS | L5/L6 net-new confirmed absent; SPEC `:550-553,:566,:592` binds scalar+checkasm pre-wiring |
| Wave-numbering cohort consistency | FAIL | P3-B 5-wave vs SPEC 6-wave → CH4-2 REVISE |
| W2 conditional LOC ceiling fit-proof defined | FAIL (minor) | `:261` undefined escape hatch → CH4-5 REVISE |

---

## §5 — Dispositions (counts + path:line + fix)

- **CH4-1 net-new NEON cost gate (L5/L6 scalar-ref + checkasm-new before wiring): ACCEPT.**
  SPEC `:550-553,:566,:592,:605`. Profile-first, same-wave-consumed, orphan-kernel
  tripwire at `:563-565`. No change required.
- **CH4-2 P3-B wave-numbering divergence (5-wave vs binding 6-wave SPEC): REVISE.**
  Fix: reconcile `p3b-wave-sequencing.md:53-59,:73-87,:255-261` to the SPEC's 6-wave
  W0–W5 structure (split projection/NEON; renumber L9 wave + close). SPEC unchanged.
- **CH4-3 L7 same-wave-consumer placement contradictory: REVISE.**
  Fix: single-value L7 to W1 (SPEC `:391,:430` already W1-consistent); delete the
  NEON-wave-L7 assertions in `p3b:61-71,:80-81` and the `p3c:50` "(if not in W1)" hedge.
- **CH4-4 per-wave phase breakdown numeric conflict (SPEC global 30/30 vs P3-B 20/15/30):
  REVISE.** Fix: adopt the `dispatch-hard-cap` 20/15/30 numbers in SPEC `:277-282`
  (≤15 LOC doc edit) so SPEC and P3-B agree; global-phase-table form itself is acceptable.
- **CH4-5 W2 ≤650 fit-proof escape hatch undefined: REVISE (minor).**
  Fix: define the fit proof at SPEC `:261` / W2 section (≤5 LOC doc edit); until defined,
  ≤450 binds.

**Counts:** ACCEPT 1 · REVISE 4 · REJECT 0 · total dispositions 5.

The SPEC itself is structurally CH4-sound on its load-bearing axes (LOC budgets, hard
caps, revert protocols, same-wave consumers, wave count ≤12, shortlist ≤8, net-new NEON
scalar+checkasm-before-wiring all PRESENT and correct). The four REVISE items are
cohort-reconciliation defects: P3-B is a STALE 5-wave artefact out of sync with the
binding 6-wave SPEC (CH4-2, the load-bearing one), the L7 placement and phase numbers are
inconsistent across artefacts (CH4-3/CH4-4), and one W2 LOC escape hatch is undefined
(CH4-5). None requires re-architecting a wave; all are ≤15-LOC reconciliation edits or a
P3-B rewrite to match the SPEC. No orphan kernel, no missing budget, no missing cap, no
missing revert protocol. The binding SPEC needs only CH4-4 (phase numbers) + CH4-5
(fit-proof definition) edits; CH4-2/CH4-3 are P3-B/P3-C reconciliation.

---

## §6 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (the binding wave plan): `:257-264` manifest,
  `:266` wave/shortlist count, `:275-282` phase caps, `:286-295` rerun ceilings, Sections
  3–8 per-wave (`:329,:382,:466,:534,:614,:669`), `:391/:430` L7, `:550-566,:592,:605`
  W3 NEON.
- `restart/skinny/tranches/sk-v17/research/p3/p3a-candidate-shortlist.md` `:48,:58,:106-126`
  (L5/L6 net-new scalar/checkasm status), `:135` (L7 consumer).
- `restart/skinny/tranches/sk-v17/research/p3/p3b-wave-sequencing.md` `:53-87,:109,:144,
  :193,:233,:250,:255-261` (the 5-wave divergence + per-wave phase numbers + L7 placement).
- `restart/skinny/tranches/sk-v17/research/p3/p3c-falsifiability-gates.md` `:43-54,:114`
  (6-wave map + per-primitive revert).
- `restart/skinny/tranches/sk-v17/research/p3/p3f-spec-draft.md` `:81-83` (6-wave count).
- `restart/skinny/tranches/sk-v8/SPEC.md` `:216-224` (mirrored manifest shape — global
  phase convention, single Impl/redress cap column).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH4, §7 hard caps, §8 axes.
- Source verification (HEAD `f87ee713a`): `bbnf-simd/src/aarch64/{comment,bracket}_*` +
  `scalar/{comment,bracket}_*` ABSENT (net-new confirmed); `scalar/byte_class_from_eq_set_64.rs`
  + `tests/checkasm_byte_class_from_eq_set_64.rs` PRESENT (L1 scalar+checkasm).
- Memory `dispatch-hard-cap` (research 20 / plan 15 / redress 30 default).

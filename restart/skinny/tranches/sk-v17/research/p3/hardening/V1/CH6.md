# SK-V17 S-P3 CHALLENGE — CH6 ANTI-PAPER-CLOSE (V1)

Lens: CH6 ANTI-PAPER-CLOSE. Pass: S-P3 Synthesis-Plan. Cycle: V1. Date: 2026-05-29.
Reviewer charge (PASS-3 §3 CH6 + ORCHESTRATOR §3W): does every wave close on
**measurement**, not a future-phase promise? No wave deferred without a named
receiver + gate. L9 commit-by-construction carries its post-CF-1 re-profile gate
concretely. Every exit gate measurable + telemetry-bound (N>=50). Every wave carries a
revert protocol. Every candidate's same-wave consumer named (no orphan kernel).
Subject artefacts: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Master HEAD `f87ee713a`.

---

## §1 — Verdict summary

The SPEC is, on the core CH6 axis, **strong**: every wave (W0–W5) carries a measurable
exit gate keyed to a bench row / grep / counter / checkasm — never a "wired"/"integrated"
prose close (`SPEC.md:360-367,436-448,507-517,583-594,650-655,694-705`); the
no-deferrals non-negotiable is explicit (`SPEC.md:244-246` "No wave closes on a
future-phase promise"); all 6 revert protocols are present
(`grep -c "Revert protocol" = 6`); all 6 same-wave consumers are named
(`grep -c "Same-wave consumer" = 7`, 6 waves + the §2.1 generality line); every deferral
has a named receiver + gate (tailwind residual → REDRESS + PASS-ALPHA §8 WARN
`SPEC.md:611,703`; Sheets/BBNF-self → SK-V18 proof `SPEC.md:107-109,321`;
hidden-coupling escapes → Pass Omega + G-Omega `SPEC.md:726`).

Four REVISE defects remain, all on the CH6 axis. One is load-bearing for my specific
charge (the L9/W4 exit threshold is prose where P3-C bound a number); two are
cross-artefact reconciliations (a wave-numbering desync between P3-B and the SPEC; an
orphaned +40% gate in P3-C §2.1); one is a timing-precision reconciliation of the L9
re-profile antecedent. None is a REJECT — the SPEC's spine is sound and every defect is
a one-edit fix.

Counts: **ACCEPT 14 · REVISE 4 · REJECT 0** (18 dispositioned units: 6 SPEC waves + the
SPEC no-deferrals/revert/consumer spine + 5 P3 cohort artefacts + the L9 conditional gate
as a cross-cutting unit).

---

## §2 — Per-unit dispositions

### ACCEPT — SPEC W0 baseline + telemetry lock (`SPEC.md:329-380`)
W0 closes on measurement: `SK-V17-open` captured + per-corpus lightningcss full-CSSOM
median emitted N>=50 cold for all four corpora (`:362-363`); `gate-json` rejects four
malformed-row classes "proven by a fixture row" (`:366`); NO behavior LOC (`:367`).
Named receiver (`gate-json` consumes every emitted field, `:369`), revert protocol
(`:376-378`), no future-phase promise. Telemetry-bound: §0.4 binds N>=50 + median +
cold + full-cssom and rejects producer-only fields (`:191-193`). CH6-clean.

### ACCEPT — SPEC W1 tape activation (`SPEC.md:382-464`)
W1 closes on **substrate truth**, not a promise: `tape_activated=true` via grep non-zero
over `grammars/css_l4_*/` + `PayloadArena` write/alloc counters (`:438-440`);
`emit_fact_stream` retired as live plane + `w5c_profile_array_retired=true` via grep ZERO
(`:441-443`); EXACT 8-field equality re-proven (`:444-445`); JSON 51/51 ±1.0% (`:446`).
The explicit "NO speed admission this wave (equality is the gate before speed)" (`:447`)
is the correct CH6 posture — W1 does not paper-close on an unmeasured speed claim; it
closes on falsifiable substrate facts and defers speed to W3. Same-wave consumer named
(L3 cursor read IS L2's consumer, `:450-451`); revert protocol present (`:459-461`).
CH6-clean. (See REVISE-2: the relationship to P3-C's +40% W1 gate must be reconciled,
but the SPEC's choice is itself defensible and CH6-clean.)

### ACCEPT — SPEC W2 layout-driven projection (`SPEC.md:466-532`)
Closes on `lazy_view_generated=true` + `css_rich_ast_preserved=true` (value-plane
population parity vs eager baseline) + per-corpus typed-median N>=50 emitted + equality
re-proven + JSON 51/51 + Lock-14 CSS-L4 proof (`:507-517`). Every clause is a
grep/count/bench fact. Same-wave consumer named (`:519-520`), revert (`:527-529`).
CH6-clean. Minor: "no worse than the W1 typed-tape baseline (no regression)" (`:515`) is
a 0% floor; P3-C §2.2 used a -2.0% tolerance — the SPEC is stricter, which is acceptable,
but the tolerance band should be stated for measurability against variance (folded into
REVISE-4 as a cosmetic).

### ACCEPT — SPEC W3 NEON structural index — THE >SOTA gate (`SPEC.md:534-612`)
This is the load-bearing measurement close: ">=1 regular corpus (animate OR bootstrap)
crosses `delta_vs_lightningcss > 1.0×` at N>=50 cold median on the typed plane"
(`:585-587`), strict plane (`css_comparator_plane=full-cssom`), preserve-rich-ast +
equality re-proven. tailwind admit-or-honest-REDRESS (`:588-589`). `native_simd_status ∈
{parity-pass, checkasm-pass}` per landed primitive (`:592`). The entry gate carries the
no-orphan-kernel guard concretely: "if no scan leaf survives as top-N, W3 does NOT land a
NEON kernel ... and the >SOTA gate is evaluated on the W2 plane" (`:563-565`). Same-wave
consumer (`:596-597`), revert (`:605-607`). CH6-clean.

### ACCEPT — SPEC W5 close (`SPEC.md:669-719`)
Closes on `dirty_generated_state=clean` (regen --check 9/9 exit 0, `:696`), Lock-14
audit, every wave admitted/rejected/routed, JSON 51/51, equality re-proven, and the
tranche criterion recorded TRUE **or** the honest residual recorded + escalated per
PASS-ALPHA §8 WARN — "NOT paper-closed" (`:702-703`). Pre-blocks paper close explicitly
(`:710-713`). Revert protocol (`:715-716`). CH6-clean.

### ACCEPT — SPEC no-deferrals / revert / same-wave-consumer spine
(`SPEC.md:240-246,242-243,251-252`)
"No wave closes on `wired`/`advisory`/`future consumer`/`integrated`/`paper close`
language without a measured bench row. No wave closes on a future-phase promise."
(`:244-246`). "Same-wave consumer per primitive ... No orphan kernel ships (SK-V5
failure)" (`:242-243`). "Every miss becomes REDRESS evidence or an explicit routed
residual — never a silent retreat" (`:251-252`). The exact CH6 charter, encoded as a
non-negotiable. ACCEPT.

### ACCEPT — P3-A same-wave-consumer naming (`p3a:69,80,91,102,113,124,135,146,159`)
Every shortlist candidate S1–S9 names its same-wave consumer (S1→S2; S2→S3; S3 IS S2's
consumer; S4 IS S1's consumer; S5/S6→S1 composition; S7→S2; S8→S3; S9→post-CF-1 spine
GATED on re-profile). No orphan kernel. ACCEPT.

### ACCEPT — P3-A L9 conditional carried as not-in-active-8 (`p3a:152-161,58`)
L9 is carried as S9 CONDITIONAL, "NOT in the active 8; admits only post-re-profile," with
a HARD admission gate (`p3a:160`) — the re-profile is "the falsifiability gate itself."
The conditional is correctly disposed as a measured admission, not a paper-promise.
ACCEPT.

### ACCEPT — P3-C §3 falsifiability table + §3.x self-audit (`p3c:152-168`)
The consolidated binding table gives every wave a strict-plane threshold or a
grep/equality close; §3.x asserts "no wave closes on a future-phase promise" and "W1/W2
close on equality + grep + counters (substrate truth), W3/W4/W5 close on a crossed Mbps
ratio" (`p3c:165-168`). Measurable end-to-end. ACCEPT (the §2.1 W1 +40% item is the
REVISE-2 inconsistency, scoped below, but §3 itself is clean).

### ACCEPT — P3-C W3/W5 tranche-level falsifiability (`p3c:111,139,161`)
The one bracket-gating criterion is a concrete ratio `max(typed/lcss@W0) > 1.0` at N>=50
with equality + JSON + preserve-rich-ast; "If false at W5 close → BLOCKED, residual gap +
hot leaf recorded in REDRESS, NOT paper-closed" (`p3c:161`). CH6-clean.

### ACCEPT — P3-D telemetry binding / producer-only rejection (per SPEC §0.4 fold)
The SK-V17 columns are gate-consumed; "a producer-only field fails the wave" is carried
into the SPEC (`SPEC.md:193`). Every exit threshold above maps to a named column
(`css_track1_typed_median_mbps`, `delta_vs_lightningcss`, `css_typed_summary_equal`,
`tape_activated`, etc., `SPEC.md:154-176`). Telemetry-bound, N>=50 enforced. ACCEPT.

### ACCEPT — P3-E pre-blocked ledger receiver routing
Residuals route to named receivers (SK-V18, Pass Omega, REDRESS) rather than being
silently deferred; the per-wave pre-block list is the gate that prevents a deferral from
re-entering unnamed. ACCEPT on the CH6 axis (CH3 owns the completeness check).

### ACCEPT — P3-F SPEC-shape fidelity (`p3f:50-83,85-131`)
P3-F's §3 explicitly states "Every behavior wave closes on a measured bench row, never a
paper-close (CH6)" (`p3f:87`) and reproduces the 6-wave (W0–W5) gate set consistent with
the SPEC and P3-C. ACCEPT.

### ACCEPT — Deferral receivers all named with gates
No wave is deferred without a named receiver + gate (the primary CH6 charge): W1–W5 each
name the prior wave's close + (for W1/W2/W3) CHALLENGE acceptance as the entry gate
(`SPEC.md:30-31,410-418,483-491,559-570,636-642`); W4 is doubly-conditional with the
post-W1 re-profile as receiver (`SPEC.md:806-807`); tranche residuals route to SK-V18 /
Pass Omega / PASS-ALPHA §8 WARN. ACCEPT.

---

### REVISE-1 (LOAD-BEARING for CH6 charge) — SPEC W4/L9 exit gate is prose where P3-C bound a number
**Path:** `SPEC.md:653`. **Disposition:** REVISE.

The SPEC W4 exit gate reads: "A measured lift (N>=50 cold median) on the gated corpus vs
the W3 plane (the corpus the re-profile identified...)." This is the **one wave my lens
is specifically charged to scrutinize** ("L9 commit-by-construction carries its
post-CF-1 re-profile gate concretely"), and its lift threshold is **prose ("a measured
lift"), not a number**. P3-C §2.4 (`p3c:125`) and the §3 binding table (`p3c:158`) both
bind it concretely: "`track1_typed@W4 median > track1_typed@W3 median` by **>= +5%** on
the corpus where the recognition-control loop is hot." A bare "a measured lift" permits a
+0.3% noise-band delta to close the wave — the precise paper-close shape CH6 exists to
catch (a control-flow change claiming a "lift" that is variance, not signal).

**Concrete fix:** replace `SPEC.md:653` with:
"A measured lift of **>= +5%** N>=50 cold median on the gated corpus vs the W3 plane (the
corpus the re-profile identified the recognition-control loop as hot on); a delta below
+5% disposes L9 as not-warranted (recorded, not a failure), per P3-C §2.4/§3."
This imports P3-C's already-bound threshold into the load-bearing contract.

### REVISE-2 — P3-C §2.1 W1 +40% gate is orphaned + contradicts P3-C's own §3
**Path:** `p3c:83` (W1 "Mbps thresholds" row) + `p3c:85` exit-gate item (d) vs
`p3c:168` (§3.x). **Disposition:** REVISE.

P3-C §2.1 lists, as a W1 **exit gate (measurable)** item (d), "The +40% fact-stream-
improvement threshold met on >=3/4 corpora" (`p3c:85`), with `track1_typed@W1(c) >= 1.40
× fs@W0(c)` (`p3c:83`). But P3-C's own §3.x self-audit (`p3c:168`) says "W1/W2 close on
equality + grep + counters (substrate truth)" — i.e. NOT on a speed threshold. The SPEC
follows §3.x, not §2.1: SPEC W1 explicitly declares "NO speed admission this wave"
(`SPEC.md:447`) and carries no +40% gate. So the +40% is **orphaned**: it is an exit-gate
clause in P3-C §2.1 that neither the SPEC nor P3-C's own §3 enforces. An exit-gate item
that the contract does not carry is the inverse paper-close risk — a gate asserted in the
research artefact but absent from the binding plan, so a reader cannot tell whether W1
admits on substrate-truth-only or on substrate-truth-plus-+40%.

This must be reconciled one way. Given the "equality before speed" non-negotiable
(`SPEC.md:249-250`) and the SPEC's deliberate "NO speed at W1" posture, the cleanest fix
is to **demote** the +40% in P3-C §2.1 from "Exit gate (measurable) item (d)" to a
**diagnostic sizing signal** (it proves the alloc floor fell, but is not an admission
gate — W1 admits on equality + tape-activation grep + PayloadArena counters).

**Concrete fix:** in `p3c:83` and `p3c:85` item (d), re-label the +40% as
"DIAGNOSTIC (sizing): the alloc-floor-kill signal — expected `track1_typed@W1(c) >= 1.40
× fs@W0(c)` on >=3/4 corpora; a miss is a REDRESS sizing note, NOT a W1 admission
failure (W1 admits on substrate truth: equality + tape-activation grep + PayloadArena
counters; equality-before-speed, SYNTHESIS §0.1)." Then `p3c:168` and the SPEC W1 gate
agree.

### REVISE-3 — P3-B uses a 5-wave numbering that desyncs from the SPEC/P3-C/P3-F 6-wave plan
**Path:** `p3b:53-59,77-83,158-261` (entire W0–W4 manifest + topological diagram).
**Disposition:** REVISE.

P3-B collapses the plan into **five waves W0–W4**: W0 infra, **W1 tape, W2 NEON, W3 L9
(conditional), W4 close** (`p3b:77-83`, diagram `p3b:257-261`). The SPEC, P3-C, and P3-F
all use **six waves W0–W5**: W0 infra, W1 tape, **W2 layout-projection, W3 NEON, W4 L9
(conditional), W5 close** (`SPEC.md:257-264`; `p3c:62-142`; `p3f:81`). P3-B folds the
SPEC's W2 (layout-projection) into its W1, and renumbers everything downstream. Because
P3-B is the wave-sequencing artefact the SPEC is supposed to fold, a reader cross-checking
the SPEC against P3-B will mis-map every behavior wave — the SPEC's "W3 NEON" is P3-B's
"W2 NEON," the SPEC's "W4 L9" is P3-B's "W3 L9," the SPEC's "W5 close" is P3-B's "W4
close." This is not a CH6 paper-close per se, but it is a **gate-traceability hazard**:
the load-bearing per-wave exit gates cannot be cross-validated against P3-B without a
silent renumbering, and the L9 re-profile gate's wave-anchor ("post-W1" vs "post-W2") is
exactly where this matters (REVISE-4).

**Concrete fix:** renumber P3-B to the 6-wave plan the SPEC adopts — split P3-B's W1 into
**W1 (tape: L2/L3/L8/L7-grow)** and **W2 (layout-projection generalization: L3-full/L8/L4)**
per SPEC §4/§5, shift NEON to **W3**, L9 to **W4**, close to **W5**. Update the diagram
(`p3b:257-261`) and the §2.x manifest. P3-B's own note (`p3b:226-228`) already concedes
"P3-C/F resolve whether the re-profile is keyed to W1-close or W2-close" — that punt is
the symptom; the SPEC resolved it to W1 and P3-B must adopt the SPEC's numbering rather
than leave a parallel scheme.

### REVISE-4 — L9 post-CF-1 re-profile timing is imprecise in P3-B/P3-C; SPEC is correct but should cite the antecedent
**Path:** `p3c:120` ("post-W1/W3"), `p3b:226-228` ("P3-C/F resolve") vs `SPEC.md:616,637`
("post-W1"). **Disposition:** REVISE (P3-B, P3-C reconcile to the SPEC; SPEC adds the
antecedent citation).

The L9 admission antecedent must be unambiguous, since L9's entire admission IS a
measurement (CH6: the conditional must carry its gate concretely). The SPEC commits to
"post-W1 typed-tape re-profile" (`SPEC.md:616,637`). This is the **faithful translation of
the locked S-P2 antecedent**: HARDENING-S-P2-V3 §3 L9 (`:239-244`) states the
recognition-control loop is "un-masked by the retired alloc floor" — the masking agent is
the ~64% String **alloc floor** (retired at W1/CF-1), NOT the ~69% scan leaf (collapsed at
W3 NEON). So "post-CF-1 = post-W1" is correct, and the SPEC is CH6-clean. But P3-C §2.4
hedges "post-W1/W3 typed-tape re-profile" (`p3c:120`) and P3-B punts the resolution
(`p3b:226-228`), leaving two artefacts implying the re-profile might be post-NEON. An
implementer reading P3-C could take the re-profile post-W3 and mis-time the L9 gate.

**Concrete fix:** (a) in P3-C §2.4, change "post-W1/W3 typed-tape re-profile" to
"**post-W1** (post-CF-1, post-alloc-floor-retirement) typed-tape re-profile; the masking
agent is the String alloc floor retired at W1, NOT the scan leaf collapsed at W3, per
HARDENING-S-P2-V3 §3 L9" (`p3c:120`); the W4 speedup baseline stays "vs W3" (correct —
W4 improves over the W3-NEON plane). (b) P3-B adopts the same "post-W1" anchor when
renumbered (REVISE-3). (c) SPEC §7 (`SPEC.md:616` and `:637`) add the parenthetical
"(post-CF-1 = post-alloc-floor; the recognition-control loop is un-masked by the retired
String floor, not by the W3 scan collapse — HARDENING-S-P2-V3 §3 L9)" so the antecedent
is self-documenting in the contract.

---

## §3 — CH6 charge checklist (explicit pass/fail)

| CH6 charge | Status | Evidence |
|---|---|---|
| Every wave closes on measurement, not a future-phase promise | PASS | `SPEC.md:360-367,436-448,507-517,583-594,650-655,694-705`; no-deferrals `:244-246` |
| No wave deferred without a named receiver + gate | PASS | W1–W5 entry gates name prior-wave close + CHALLENGE; W4 doubly-conditional; residuals → SK-V18/Pass Omega/PASS-ALPHA §8 |
| L9 commit-by-construction carries its post-CF-1 re-profile gate concretely | PARTIAL → REVISE-1 + REVISE-4 | admission antecedent concrete (`SPEC.md:637`); **exit-lift threshold prose, missing +5% (REVISE-1)**; timing-anchor imprecise in P3-B/P3-C (REVISE-4) |
| Every exit gate measurable + telemetry-bound (N>=50) | PASS | §0.4 N>=50/median/cold/full-cssom enforced (`:147-151,188-191`); each gate maps to a named column |
| Every wave carries a revert protocol | PASS | 6/6 present (`grep -c "Revert protocol" = 6`) |
| Every candidate's same-wave consumer named (no orphan kernel) | PASS | `p3a:69-159`; SPEC `:450-451,519-520,596-597,657-658`; orphan-kernel guard `:563-565` |
| Wave count <= 12; shortlist <= 8 active | PASS | 6 waves (`SPEC.md:266`); 8 active + L9 conditional (`SPEC.md:266-267`) |

---

## §4 — Counts

- **ACCEPT: 14**
- **REVISE: 4** (REVISE-1 SPEC W4/L9 +5% threshold [load-bearing]; REVISE-2 P3-C §2.1 W1
  +40% orphan; REVISE-3 P3-B 5-wave numbering desync; REVISE-4 L9 re-profile timing
  anchor in P3-B/P3-C/SPEC)
- **REJECT: 0**

ACCEPT rate (CH6 lens): 14 / 18 = **77.8%**. Below the §3Z 95% convergence bar — the
four REVISE dispositions must fold into V2. None is a critical/blocking defect; all four
are single-edit reconciliations and the SPEC spine is CH6-sound.

---

## §5 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (the wave plan under review; W0 §3 / W1 §4 /
  W2 §5 / W3 §6 / W4 §7 / W5 §8; §0.4 telemetry; §1 non-negotiables; §9 pre-blocks).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md` (the S-P3
  cohort; p3c §2.4/§3 the L9 gate; p3b §2 the 5-wave manifest; p3a §2 same-wave
  consumers).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §3 L9 (`:228-250`, the post-CF-1 antecedent: recognition-control loop un-masked by the
  retired alloc floor), §6 (binding shortlist condition 5).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH6 (`:140-145`), §8 bbnf axes,
  §9 closing posture.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (close conditions), §0.5
  (per-corpus + tranche success criterion), §3 (four-lever stack).
- Master HEAD `f87ee713a`.

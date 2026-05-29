# SK-V17 S-P3 CHALLENGE — CH6 ANTI-PAPER-CLOSE (V2)

Lens: CH6 ANTI-PAPER-CLOSE. Pass: S-P3 Synthesis-Plan. Cycle: V2. Date: 2026-05-29.
Reviewer charge (PASS-3 §3 CH6 + ORCHESTRATOR §3W): does every wave close on
**measurement**, not a future-phase promise? No wave deferred without a named
receiver + gate. L9 commit-by-construction carries its post-CF-1 (= post-W1) re-profile
gate concretely. Every exit gate measurable + telemetry-bound (N≥50). Every wave carries
a revert protocol. Every candidate's same-wave consumer named (no orphan kernel). The
SPEC forbids deferral ("no wave closes on a future-phase promise").
Subject artefacts: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Master HEAD `f87ee713a`.

---

## §1 — Verdict summary

V2 folds all four V1 CH6 REVISE dispositions cleanly. The SPEC's CH6 spine is sound and
every behaviour wave closes on a falsifiable bench row / grep / counter / checkasm — never
a "wired"/"integrated" prose close. The four V1 fixes are now in the contract:

- **V1 REVISE-1 (W4/L9 exit threshold prose → number) — FOLDED.** `SPEC.md:706-710` now
  binds "A measured lift of **≥ +5%** (N≥50 cold median, `track1_typed@W4 ≥ +5%` vs the
  W3 plane) … A lift below +5% disposes L9 as NOT-WARRANTED (recorded measurably, not a
  failure) — a noise-band close on the conditional wave does not admit (P3-C §2.4/§3)."
  This is the load-bearing fix for my specific charge; the +0.3%-noise paper-close shape
  is now barred.
- **V1 REVISE-2 (P3-C §2.1 orphaned +40% W1 gate) — FOLDED.** `p3c:83` now reads "**NO
  speed admission this wave**" and explicitly states the +40% was "demoted to diagnostic
  per CHALLENGE V1 D2/REVISE-2" with "no S-P1/S-P2 trace for the 1.40× denominator";
  `p3c:88,155,168` are all consistent (W1 closes on substrate truth, the ratio is
  non-gating diagnostic sizing telemetry). SPEC W1 (`:447,475`) agrees.
- **V1 REVISE-3 (P3-B 5-wave desync) — FOLDED.** `p3b:3` is now Cycle V2; `p3b:10-31`
  §0 re-authors P3-B "to the SPEC six-wave manifest verbatim" (W0 infra / W1 PRUNE+tape /
  W2 layout-projection / W3 NEON / W4 L9-conditional / W5 close), with L7→W1, L4→W2,
  L1/L5/L6→W3, L9 post-W1. The parallel numbering scheme is retired; gate traceability
  across P3-B / P3-C / P3-F / SPEC is restored.
- **V1 REVISE-4 (L9 post-W1 antecedent imprecise) — FOLDED.** `SPEC.md:665-672` now
  carries the antecedent self-documenting: "The re-profile is keyed to **post-W1**, NOT
  post-W3: the S-P2 antecedent … is unmasked by the retired alloc floor — which falls in
  W1 — NOT by the W3 scan collapse." `p3c:120,193` and `p3b:28-29,52-66,215-217`,
  `p3a:156,160,215-217` all carry the post-W1 anchor with the alloc-floor masking citation.

**One residual REVISE remains** — the W2 exit-gate maintain budget at `SPEC.md:564` uses a
bare "no regression" 0% floor where P3-C §2.2 (`p3c:97`) and the §3 binding table
(`p3c:156`) both bind a measurable **-2.0%** tolerance. This is the cosmetic V1 itself
flagged inside its REVISE-4 fold (V1 CH6 §2 W2-ACCEPT note: "the tolerance band should be
stated for measurability against variance") and did NOT get folded into the SPEC. It is a
genuine CH6 measurability defect: a 0% floor cannot be cleanly falsified against N≥50
median bench variance (a -0.5% noise sample is indistinguishable from a real regression),
and the SPEC's W2 number diverges from P3-C's already-bound number. One-edit fix; not a
REJECT — the SPEC's spine is CH6-sound.

Counts: **ACCEPT 16 · REVISE 1 · REJECT 0** (17 dispositioned units: 6 SPEC waves + the
SPEC no-deferrals/revert/consumer spine + 5 P3 cohort artefacts + the L9 conditional gate
+ the four V1-fold reconciliations as one cross-cutting verified unit).

ACCEPT rate (CH6 lens): 16 / 17 = **94.1%**. At the §3Z 95% threshold's edge; the single
REVISE is a one-edit SPEC↔P3-C number reconciliation, no orphan, no critical defect.

---

## §2 — Per-unit dispositions

### ACCEPT — V1 REVISE-1 fold: W4/L9 +5% exit threshold (`SPEC.md:706-710`)
The load-bearing fix for my charge. The W4 exit gate now binds the lift concretely:
"≥ +5% (N≥50 cold median, `track1_typed@W4 ≥ +5%` vs the W3 plane) … a noise-band close
on the conditional wave does not admit (P3-C §2.4/§3)." Matches P3-C §2.4 (`p3c:125`) and
the §3 binding table (`p3c:158`). The conditional wave can no longer paper-close on
variance. ACCEPT.

### ACCEPT — V1 REVISE-2 fold: P3-C W1 +40% demoted to diagnostic (`p3c:83,85,88,155,168`)
The orphaned +40% W1 exit-gate clause is demoted to non-gating diagnostic sizing telemetry
with an explicit author-invented-gate disavowal ("no S-P1/S-P2 trace for the 1.40×
denominator"). P3-C §2.1, §3 table, and §3.x self-audit now agree with SPEC W1's "NO speed
admission this wave" (`SPEC.md:447`). The inverse paper-close risk (a gate asserted in
research but absent from the contract) is cleared. ACCEPT.

### ACCEPT — V1 REVISE-3 fold: P3-B re-sequenced to the 6-wave SPEC manifest (`p3b:10-31`)
P3-B's §0 V2 fold note re-authors verbatim to `SPEC.md:257-264`: W0/W1-PRUNE+tape/
W2-projection/W3-NEON/W4-L9/W5-close, with L7→W1, L4→W2, L1/L5/L6→W3, L9 post-W1, and
`regen --check` 9/9 keyed to W5. The V1 gate-traceability hazard (SPEC's "W3 NEON" =
P3-B's "W2 NEON") is eliminated; every per-wave exit gate cross-validates 1:1. ACCEPT.

### ACCEPT — V1 REVISE-4 fold: L9 post-W1 antecedent self-documented (`SPEC.md:665-672`)
The SPEC §7 now states the masking agent unambiguously: the recognition-control loop is
"unmasked by the retired alloc floor — which falls in W1 — NOT by the W3 scan collapse,"
and "the LOCKED 28.87%+2.45% recognition-control figures are NOT a measured rollback
antecedent — P1-E measured ZERO speculative checkpoint/rollback self-time on either benched
plane." P3-C §2.4 (`p3c:120`), P3-B (`p3b:28-29,52-66`), and P3-A (`p3a:156,160`) all carry
the post-W1 anchor. An implementer can no longer mis-time the L9 re-profile to post-W3.
ACCEPT.

### ACCEPT — SPEC W0 baseline + telemetry lock (`SPEC.md:368-378`)
Closes on measurement: `SK-V17-open` captured + per-corpus lightningcss full-CSSOM median
N≥50 cold for all four corpora (`:370-371`); `gate-json` rejects four malformed-row
classes "proven by a fixture row" (`:374`); "NO parser/scanner/SIMD/codegen behavior or
generated parser output change lands" (`:375`). Named receiver (`gate-json` consumes every
field, `:377-378`), revert protocol (`:384-386`), no future-phase promise. CH6-clean.

### ACCEPT — SPEC W1 tape activation (`SPEC.md:461-489`)
Closes on substrate truth: `tape_activated=true` via grep non-zero + `PayloadArena`
write/alloc counters (`:463-465`); `w5c_profile_array_retired=true` via grep ZERO
(`:466`); the no-dangling-`emit_fact_stream`-round-trip-assert grep (`:467-471`) — a strong
anti-strand check; EXACT 8-field equality (`:472-473`); JSON 51/51 ±1.0% (`:474`); "NO
speed admission this wave (equality is the gate before speed)" (`:475`). The migration of
the seven test fns + three codegen asserts in the same commit (`:453-459,467-471`) closes
the SK-V5 orphan-strand failure mode by construction. Same-wave consumer named (`:478-479`);
revert (`:487-489`). CH6-clean.

### REVISE-1 (residual) — SPEC W2 exit-gate maintain budget is a bare 0% floor where P3-C bound -2.0%
**Path:** `SPEC.md:564`. **Disposition:** REVISE.

The W2 exit gate reads: "Per-corpus typed-median Mbps emitted at N≥50 cold for all four
corpora; the typed plane is no worse than the W1 typed-tape baseline (**no regression vs
`SK-V17-open`**)." This is a **0% floor with no variance tolerance**. P3-C §2.2 (`p3c:97`,
the "Mbps thresholds" row) and the §3 binding table (`p3c:156`) both bind W2 as
"`track1_typed@W2(c)` no worse than **-2.0%** vs `track1_typed@W1`" — a measurable band.
Against N≥50 median bench variance, a bare "no regression"/0% floor is not cleanly
falsifiable: a -0.4% noise sample on one corpus would breach a literal 0% floor yet be
indistinguishable from measurement noise, so an implementer cannot tell whether W2 closes
or HALTS. This is precisely the measurability gap CH6 guards (a gate the bench cannot
cleanly adjudicate is a soft close), and it is a **SPEC-vs-P3-C number divergence** on the
load-bearing contract. V1 CH6 flagged this exact item inside its W2-ACCEPT note ("the
tolerance band should be stated for measurability against variance ... folded into
REVISE-4 as a cosmetic") and it did not reach the SPEC in the V2 fold.

**Concrete fix:** replace `SPEC.md:564` with:
"Per-corpus typed-median Mbps emitted at N≥50 cold for all four corpora; the typed plane
is **no worse than -2.0% median vs the W1 typed-tape baseline** (the variance-tolerant
no-regression band, P3-C §2.2/§3; W2 is a generality refactor, not a speedup wave)."
This imports P3-C's already-bound -2.0% band into the contract, making the W2 close
falsifiable from the bench and reconciling the SPEC W2 number with P3-C. (The W2
`track1_full_parse` maintain budget should likewise read "≥ -2.0%" for the same reason,
matching `p3c:98`; the SPEC W2 maintain line already uses ±1.0% for JSON, which is
intentionally tighter and correct.)

### ACCEPT — SPEC W3 NEON structural index — THE >SOTA gate (`SPEC.md:632-643`)
The load-bearing measurement close: "≥1 regular corpus (animate OR bootstrap) crosses
`delta_vs_lightningcss > 1.0×` at N≥50 cold median on the typed plane"
(`css_comparator_plane=full-cssom`), preserve-rich-ast + equality re-proven; tailwind
admit-or-honest-REDRESS (`:638-639`); `native_simd_status ∈ {parity-pass, checkasm-pass}`
per landed primitive (`:641`). The entry gate carries the no-orphan-kernel guard
concretely (`:611-614`): "if no scan leaf survives as top-N, W3 does NOT land a NEON
kernel (no orphan kernel) and the >SOTA gate is evaluated on the W2 plane." L7's W1 default
body is a "conservative byte-proportional bound — never a per-corpus literal" (`:448`),
which is measurable and not a magic constant. Same-wave consumer (`:645-646`), revert
(`:654-656`). CH6-clean.

### ACCEPT — SPEC W4 commit-by-construction (CONDITIONAL) (`SPEC.md:663-723`)
The conditional wave carries its gate concretely on both halves: (1) admission is a
measured antecedent — "this wave dispatches ONLY if the post-W1 typed-tape re-profile
(N≥50) surfaces the recognition-control loop … or a speculative-rollback leaf as top-N
self-time" (`:665-667`), and "If the re-profile does NOT surface a rollback leaf, W4 does
NOT dispatch; L9 is recorded as not-needed (NOT a failure)" (`:674-675`) — a correctly
disposed conditional, not a paper-promise; (2) the exit lift is now the bound +5%
(`:706-710`, REVISE-1 fold). Same-wave consumer is concrete and present-tense: "the post-W1
CSS recognizer spine … the live consumer on the post-W1 profile, not a promised future
consumer" (`:713-714`). Byte-identical-tape checkasm (`:705`); revert (`:720-721`).
CH6-clean.

### ACCEPT — SPEC W5 close (`SPEC.md:725-772`)
Closes on `dirty_generated_state=clean` (regen --check 9/9 exit 0, `:752`), Lock-14 audit,
every wave admitted/rejected/routed, JSON 51/51, equality re-proven, and "The tranche
success criterion (≥1 regular corpus crosses) is recorded TRUE, or the honest residual is
recorded and escalated per PASS-ALPHA §8 (WARN) — NOT paper-closed" (`:758-759`).
Pre-blocks paper close explicitly (`:766-769`). Revert (`:771-772`). CH6-clean.

### ACCEPT — SPEC no-deferrals / revert / same-wave-consumer spine (`SPEC.md:243-252`)
"No deferrals. A wave cannot close on 'wired', 'advisory', 'future consumer',
'integrated', or 'paper close' language without a measured bench row. No wave closes on a
future-phase promise" (`:243-251`). "Same-wave consumer per primitive … No orphan kernel
ships (SK-V5 failure)" (`:247-248`). "Every miss becomes REDRESS evidence or an explicit
routed residual — never a silent retreat" (`:256-257`). The exact CH6 charter, encoded as
a non-negotiable. Structural audit: `grep -c "Revert protocol" = 6`, `grep -c "Exit gate"
= 6`, `grep -c "Same-wave consumer" = 7` (6 waves + §2.1), `grep -c "Downstream effect" =
6`. ACCEPT.

### ACCEPT — P3-A same-wave-consumer naming + L9 conditional (`p3a:69,80,91,102,113,124,135,146,159-160`)
Every active candidate S1–S8 names its same-wave consumer; "the scan + the tape land
together or neither — no orphan kernel" (`p3a:69`). L7 is explicitly "not deferred behind
the NEON scan … single-valued to W1" (`p3a:135`). L9/S9 is carried as CONDITIONAL with a
HARD post-W1 admission gate where "the falsifiability gate is the re-profile itself"
(`p3a:160`) — a measured admission, not a paper-promise. ACCEPT.

### ACCEPT — P3-C §3 binding table + §3.x self-audit (`p3c:152-168`)
Every wave carries a strict-plane Mbps threshold or a grep/equality/counter close; §3.x
asserts "no wave closes on a future-phase promise" and "W1/W2 close on equality + grep +
counters (substrate truth) … W3/W4/W5 close on a crossed Mbps ratio (speed truth)"
(`p3c:168`). The strict plane (`track1_typed` preserve-rich-ast ÷ lightningcss full-CSSOM)
is used for every comparator delta; cssparser is flaw-probe-only. Measurable end-to-end.
ACCEPT. (The W2 -2.0% band here is correct and is the source-of-truth the SPEC REVISE-1
above must adopt.)

### ACCEPT — P3-C tranche-level falsifiability (`p3c:161`)
The single bracket-gating criterion is a concrete ratio `max(typed/lcss@W0) > 1.0` at N≥50
with equality + JSON + preserve-rich-ast; "If false at W5 close → BLOCKED, residual gap +
hot leaf recorded in REDRESS, NOT paper-closed." CH6-clean.

### ACCEPT — P3-D telemetry binding / producer-only rejection (SPEC §0.4 fold, `SPEC.md:186-198`)
Every exit threshold maps to a named, gate-consumed column; "Every emitted field must be
consumed by `gate-json` in the same wave; a producer-only field fails the wave"
(`:197-198`). N≥50 + median + cold + full-cssom enforced as rejection rules. Telemetry-
bound. ACCEPT.

### ACCEPT — P3-E pre-blocked ledger receiver routing
Residuals route to named receivers (SK-V18, Pass Omega, REDRESS) with gates rather than
silent deferral; the per-wave pre-block list (`SPEC.md:817-823`) is the gate preventing an
unnamed re-entry. ACCEPT on the CH6 axis (CH3 owns completeness).

### ACCEPT — P3-F SPEC-shape fidelity
P3-F reproduces the 6-wave (W0–W5) gate set consistent with SPEC and P3-C and carries the
CH6 close-on-measurement posture. ACCEPT.

### ACCEPT — Deferral receivers all named with gates (the primary CH6 charge)
No wave deferred without a named receiver + gate: W1–W5 entry gates name the prior wave's
close + (W1/W2/W3) CHALLENGE acceptance (`SPEC.md:30-31,428-436,521-529,609-619,688-696`);
W4 is doubly-conditional with the post-W1 re-profile as receiver
(`SPEC.md:862-863`); tranche residuals route to SK-V18 / Pass Omega / PASS-ALPHA §8 WARN
(`SPEC.md:759,774-775`); hidden-coupling escapes route through Pass Omega + G-Omega
(`SPEC.md:781-782`). ACCEPT.

---

## §3 — CH6 charge checklist (explicit pass/fail)

| CH6 charge | Status | Evidence |
|---|---|---|
| Every wave closes on measurement, not a future-phase promise | PASS | `SPEC.md:368-375,461-475,547-565,632-643,703-711,750-761`; no-deferrals `:243-251` |
| No wave deferred without a named receiver + gate | PASS | W1–W5 entry gates; W4 doubly-conditional `:862-863`; residuals → SK-V18 / Pass Omega / PASS-ALPHA §8 |
| L9 commit-by-construction carries its post-CF-1 (post-W1) re-profile gate concretely | PASS | admission antecedent concrete + self-documenting `:665-672`; exit-lift +5% bound `:706-710` (V1 REVISE-1 folded); timing-anchor post-W1 in P3-A/B/C (V1 REVISE-4 folded) |
| Every exit gate measurable + telemetry-bound (N≥50) | PARTIAL → REVISE-1 | §0.4 N≥50/median/cold/full-cssom enforced (`:153-198`); every gate maps to a named column; **W2 maintain budget is a bare 0% floor (`:564`), not the bench-falsifiable -2.0% band P3-C bound — REVISE-1** |
| Every wave carries a revert protocol | PASS | 6/6 present (`grep -c "Revert protocol" = 6`) |
| Every candidate's same-wave consumer named (no orphan kernel) | PASS | `p3a:69-160`; SPEC `:478-479,568-569,645-646,713-714`; orphan-kernel guard `:611-614` |
| Wave count ≤ 12; shortlist ≤ 8 active | PASS | 6 waves (`SPEC.md:271`); 8 active + L9 conditional (`:271-272`) |

---

## §4 — Counts

- **ACCEPT: 16**
- **REVISE: 1** (REVISE-1 residual: SPEC W2 exit-gate maintain budget `:564` is a bare 0%
  "no regression" floor where P3-C §2.2/§3 binds a bench-falsifiable -2.0% band — the
  unfolded V1 cosmetic; SPEC↔P3-C number divergence; single-edit fix)
- **REJECT: 0**

ACCEPT rate (CH6 lens): 16 / 17 = **94.1%**. At the §3Z 95% edge. All four V1 CH6 REVISE
dispositions are folded; the single residual is a one-edit SPEC↔P3-C reconciliation with
no orphan and no critical defect. Folding REVISE-1 (importing P3-C's -2.0% W2 band into
`SPEC.md:564`) lifts the lens to 17/17 = 100% with the SPEC's CH6 spine unchanged.

---

## §5 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (the wave plan under review; W0 §3 / W1 §4 /
  W2 §5 / W3 §6 / W4 §7 / W5 §8; §0.4 telemetry; §1 non-negotiables; §9 pre-blocks).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md` (the S-P3
  cohort; p3c §2.2/§2.4/§3 the W2/W4 gates; p3b §0 the V2 6-wave re-sequence; p3a §2 the
  same-wave consumers + S9 conditional).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V1/CH6.md` (the V1 CH6
  dispositions REVISE-1..4 verified folded in V2).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §3 L9 (the post-CF-1 = post-W1 antecedent: recognition-control loop un-masked by the
  retired alloc floor), §6 (binding shortlist condition 5).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH6 (`:140-145`), §8 bbnf axes,
  §9 closing posture.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (close conditions), §0.5 (per-corpus
  + tranche success criterion), §3 (four-lever stack).
- Master HEAD `f87ee713a`.

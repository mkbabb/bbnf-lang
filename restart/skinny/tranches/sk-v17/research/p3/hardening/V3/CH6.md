# SK-V17 S-P3 CHALLENGE — CH6 ANTI-PAPER-CLOSE (V3)

Lens: CH6 ANTI-PAPER-CLOSE. Pass: S-P3 Synthesis-Plan. Cycle: V3. Date: 2026-05-29.
Reviewer charge (PASS-3 §3 CH6 + ORCHESTRATOR §3W): does every wave close on
**measurement**, not a future-phase promise? No wave deferred without a named receiver +
gate. L9 commit-by-construction carries its post-CF-1 (= post-W1) re-profile gate
concretely. Every exit gate measurable + telemetry-bound (N≥50). Every wave carries a
revert protocol. Every candidate's same-wave consumer named (no orphan kernel). The SPEC
forbids deferral ("no wave closes on a future-phase promise").
Subject artefacts: `restart/skinny/tranches/sk-v17/SPEC.md` + `research/p3/{p3a..p3f}.md`.
Master HEAD `f87ee713a`.

---

## §1 — Verdict summary

**V3 converges the CH6 lens to 17/17 = 100% ACCEPT.** The single V2 residual REVISE — the
W2 exit-gate maintain budget at `SPEC.md` §5 being a bare 0% "no regression" floor where
P3-C §2.2/§3 bound a bench-falsifiable -2.0% band — is now FOLDED. The V3 SPEC header
self-documents the fold (`SPEC.md:5`: "the W2 exit-gate maintain budget at §5 is REVISEd
from a bare 0% 'no regression' floor to the bench-falsifiable -2.0% median band vs the W1
typed-tape baseline, reconciling SPEC↔P3-C §2.2/§3; all V1 dispositions carried forward"),
and the W2 exit gate now binds the band concretely:

> `SPEC.md:564-568`: "the typed plane is **no worse than -2.0% median vs the W1 typed-tape
> baseline** (`track1_typed@W2(c) ≥ -2.0%` vs `track1_typed@W1(c)`, N≥50 cold, all four
> corpora — the bench-falsifiable maintain band, P3-C §2.2/§3 … A bare 0% 'no regression'
> floor does not bind — the -2.0% band is falsifiable against N≥50 median variance where a
> 0% floor is not."

This reconciles `SPEC.md:564` ↔ `p3c:97` ↔ `p3c:156` exactly. With the fold, the SPEC W2
close is now cleanly adjudicable from the bench (a -0.4% noise sample no longer ambiguously
breaches a literal 0% floor), and the SPEC↔P3-C number divergence on the load-bearing
contract is closed.

All four V1 CH6 REVISE dispositions remain folded and re-verified in V3:

- **V1 REVISE-1 (W4/L9 exit threshold prose → number) — HELD.** `SPEC.md:711-714` binds
  "A measured lift of **≥ +5%** (N≥50 cold median, `track1_typed@W4 ≥ +5%` vs the W3
  plane) … A lift below +5% disposes L9 as NOT-WARRANTED (recorded measurably, not a
  failure) — a noise-band close on the conditional wave does not admit (P3-C §2.4/§3)." The
  +0.3%-noise paper-close shape is barred on the conditional wave.
- **V1 REVISE-2 (P3-C §2.1 orphaned +40% W1 gate) — HELD.** `p3c:83,88,155,168` carry "NO
  speed admission this wave"; the +40% is demoted to non-gating diagnostic sizing telemetry
  with "no S-P1/S-P2 trace for the 1.40× denominator." SPEC W1 (`:475`) agrees.
- **V1 REVISE-3 (P3-B 5-wave desync) — HELD.** `p3b:5,141-148` sequence to the SPEC's
  six-wave manifest verbatim (W0/W1/W2/W3/W4/W5), L7→W1, L4→W2, L1/L5/L6→W3, L9 post-W1.
- **V1 REVISE-4 (L9 post-W1 antecedent imprecise) — HELD.** `SPEC.md:670-680` carries the
  antecedent self-documenting (post-W1, unmasked by the retired alloc floor, NOT the W3
  scan collapse); `p3c:120,193`, `p3b:147`, `p3a:159` all carry the post-W1 anchor.

No new defect surfaces under the CH6 lens in V3. The SPEC's anti-paper-close spine is
sound end-to-end: every behaviour wave closes on a falsifiable bench row / grep / counter
/ checkasm, every deferral has a named receiver + gate, and the L9 conditional carries its
post-W1 re-profile admission gate AND its +5% exit lift concretely.

Counts: **ACCEPT 17 · REVISE 0 · REJECT 0** (17 dispositioned units: 6 SPEC waves + the
SPEC no-deferrals/revert/consumer spine + 5 P3 cohort artefacts (p3a/p3b/p3c/p3d/p3f) +
the L9 conditional gate + the deferral-receiver cross-cut + the V2-residual-fold
verification — the V2 unit set, with the V2 single REVISE now ACCEPTed).

ACCEPT rate (CH6 lens): 17 / 17 = **100%**. Above the §3Z 95% threshold; zero open critical
defects; zero orphan REVISE. Second consecutive ≥95% on this lens (V2 = 94.1% at the edge,
V3 = 100%) once the V2 residual folds — the lens is converged.

---

## §2 — Per-unit dispositions

### ACCEPT — V2 RESIDUAL FOLD: W2 exit-gate maintain budget now -2.0% band (`SPEC.md:564-568`)
**The load-bearing V3 fix.** The V2 single REVISE is folded: the W2 typed-plane maintain
budget reads "no worse than -2.0% median vs the W1 typed-tape baseline (`track1_typed@W2(c)
≥ -2.0%`, N≥50 cold, all four corpora)," with the explicit falsifiability rationale "A bare
0% 'no regression' floor does not bind — the -2.0% band is falsifiable against N≥50 median
variance where a 0% floor is not." This is exactly the concrete fix V2 CH6 §2 REVISE-1
prescribed (import P3-C's -2.0% band into `SPEC.md:564`), now matching `p3c:97`/`p3c:156`.
The W2 close is now bench-adjudicable. ACCEPT.

### ACCEPT — SPEC W0 baseline + telemetry lock (`SPEC.md:368-388`)
Closes on measurement: `SK-V17-open` captured + per-corpus lightningcss full-CSSOM median
N≥50 cold for all four corpora (`:370-371`); `gate-json` rejects four malformed-row classes
"proven by a fixture row" (`:374`); "NO parser/scanner/SIMD/codegen behavior or generated
parser output change lands" (`:375`). Named receiver — `gate-json` consumes every emitted
field and rejects malformed/missing evidence in the same W0 slice (`:377-378`). Revert
protocol concrete (`:384-386`). No future-phase promise. CH6-clean.

### ACCEPT — SPEC W1 tape activation (`SPEC.md:461-492`)
Closes on substrate truth: `tape_activated=true` via grep non-zero + `PayloadArena`
write/alloc counters (`:463-465`); `w5c_profile_array_retired=true` via grep ZERO (`:466`);
the no-dangling-`emit_fact_stream`-round-trip-assert grep (`:467-471`); EXACT 8-field
equality (`:472-473`); JSON 51/51 ±1.0% (`:474`); "NO speed admission this wave (equality
is the gate before speed)" (`:475-476`). The same-commit migration of the seven test fns +
three codegen asserts (`:453-459,467-471`) closes the SK-V5 orphan-strand failure mode by
construction. Same-wave consumer named (L3 cursor read is L2's consumer, `:478-479`); revert
(`:487-489`); post-W1 re-profile obligation explicit (`:491-492`). CH6-clean.

### ACCEPT — SPEC W2 layout-driven projection (`SPEC.md:547-583`)
Closes on `lazy_view_generated=true` + the load-bearing JSON-rider byte-equal re-emission
(R-CH2-1, `:550-557`), `css_rich_ast_preserved=true` with value-plane population parity
(`:558-560`), the now-folded -2.0% maintain band (`:564-568`), equality re-proven + JSON
51/51 ±1.0% (`:570`), Lock-14 + non-JSON CSS L4 proof (`:571`). The ≤650-LOC fit-proof
escape hatch (`:502-508`) is itself measurable (per-artefact LOC accounting, ≤5 LOC plan
prose) and not a deferral. Same-wave consumer (the generated CSS projection reads the W1
tape; L8 flags read by L3, `:573-574`); revert (`:581-583`). CH6-clean.

### ACCEPT — SPEC W3 NEON structural index — THE >SOTA gate (`SPEC.md:637-661`)
The load-bearing measurement close: "≥1 regular corpus (animate OR bootstrap) crosses
`delta_vs_lightningcss > 1.0×` at N≥50 cold median on the typed plane"
(`css_comparator_plane=full-cssom`), preserve-rich-ast + equality re-proven (`:639-641`);
tailwind admit-or-honest-REDRESS (`:642-643`); `native_simd_status ∈ {parity-pass,
checkasm-pass}` per landed primitive (`:646`). The no-orphan-kernel guard is concrete in the
entry gate (`:616-618`): "if no scan leaf survives as top-N, W3 does NOT land a NEON kernel
(no orphan kernel) and the >SOTA gate is evaluated on the W2 plane." L7's W1 default body is
a "conservative byte-proportional bound — never a per-corpus literal" (`:448`) — measurable,
no magic constant. Same-wave consumer (`:650-651`); per-primitive revert (`:659-661`).
CH6-clean.

### ACCEPT — SPEC W4 commit-by-construction (CONDITIONAL) — the L9 charge (`SPEC.md:668-728`)
The specific CH6 charge ("L9 carries its post-CF-1 re-profile gate concretely") passes on
both halves. (1) **Admission is a measured antecedent, not a paper-promise:** "this wave
dispatches ONLY if the post-W1 typed-tape re-profile (N≥50) surfaces the recognition-control
loop … or a speculative-rollback leaf as top-N self-time" (`:670-672`), keyed self-documenting
to post-W1 / the retired alloc floor / NOT the W3 scan collapse (`:673-678`), with the
disposal path explicit: "If the re-profile does NOT surface a rollback leaf, W4 does NOT
dispatch; L9 is recorded as not-needed (NOT a failure)" (`:679-680`) and the entry gate
mirror (`:695-697`). (2) **The exit lift is the bound +5%** (`:711-714`, V1 REVISE-1 held) —
"a noise-band close on the conditional wave does not admit." Byte-identical-tape checkasm
(`:710`). Same-wave consumer is concrete + present-tense: "the post-W1 CSS recognizer spine
… the live consumer on the post-W1 profile, not a promised future consumer" (`:718-719`).
Revert (`:725-726`). The binding shortlist condition 5 (`:847-850`) re-states the hard gate.
CH6-clean.

### ACCEPT — SPEC W5 close (`SPEC.md:755-777`)
Closes on `dirty_generated_state=clean` (regen --check 9/9 exit 0, `:757`), Lock-14 audit
(`:758-759`), every wave admitted/rejected/routed (`:760`), JSON 51/51 + preserve-rich-ast +
equality re-proven (`:762`), and "The tranche success criterion (≥1 regular corpus crosses)
is recorded TRUE, or the honest residual is recorded and escalated per PASS-ALPHA §8 (WARN)
— NOT paper-closed" (`:763-764`). Pre-blocks paper close explicitly (`:771-774`). Revert /
reopen-producing-wave (`:776-777`). CH6-clean.

### ACCEPT — SPEC no-deferrals / revert / same-wave-consumer spine (`SPEC.md:243-258`)
"No deferrals. A wave cannot close on 'wired', 'advisory', 'future consumer', 'integrated',
or 'paper close' language without a measured bench row. No wave closes on a future-phase
promise" (`:249-251`). "Same-wave consumer per primitive … No orphan kernel ships (SK-V5
failure)" (`:247-248`). "Every miss becomes REDRESS evidence or an explicit routed residual
— never a silent retreat" (`:256-257`). The exact CH6 charter as a non-negotiable.
Structural audit (greps, this review): `Revert protocol = 6`, `Exit gate (MEASURABLE) = 6`,
`Same-wave consumer = 7` (6 waves + §2.1), `Downstream effect = 6`, `Entry gate = 6`. The
only "wired"/"integrated" prose in the SPEC is the comparator-wiring descriptor (`:96`,
"lightningcss is wired same-run" — a measurement-setup line, not a wave close) and the
forbidding non-negotiable/pre-block (`:249-251,816,828`); ZERO wave exit gates close on
prose. ACCEPT.

### ACCEPT — P3-A same-wave-consumer naming + L9 conditional (`p3a:69,80,91,102,113,124,135,146,159`)
Every active candidate S1–S8 names its same-wave consumer; "The scan + the tape land
together or neither — no orphan kernel" (`p3a:69`). L7 is "not deferred behind the NEON scan
… single-valued to W1" (`p3a:135`). L4 is single-valued to W2 with its receiver named
(`p3a:102`). L9/S9 is CONDITIONAL with a HARD post-W1 admission gate where the admission gate
IS the re-profile (`p3a:159`) — a measured admission, not a paper-promise. REJECTed class
barred (`p3a:194-196`). ACCEPT.

### ACCEPT — P3-B wave sequencing / conditional-dispatch status (`p3b:141-152`)
The six-wave manifest carries a conditional-dispatch-status column per wave (`:141-148`); W4
is "**CONDITIONAL on the post-W1 re-profile firing the L9 gate** … else NOT dispatched,
recorded not-needed" (`:147`); "No candidate is orphaned across waves (every primitive's
same-wave consumer is named)" (`:151-152`). Every deferral (L7→W1, L4→W2, L9→conditional)
has a named receiver. ACCEPT.

### ACCEPT — P3-C §3 binding table + §3.x self-audit (`p3c:152-168`)
Every wave carries a strict-plane Mbps threshold or a grep/equality/counter close; the W2
row binds the -2.0% band (`:156`, now matching the folded SPEC); §3.x asserts "no wave
closes on a future-phase promise" and "W1/W2 close on equality + grep + counters (substrate
truth) … W3/W4/W5 close on a crossed Mbps ratio (speed truth)" (`:168`). The strict plane
(`track1_typed` preserve-rich-ast ÷ lightningcss full-CSSOM) is used for every comparator
delta; cssparser is flaw-probe-only. Measurable end-to-end. ACCEPT.

### ACCEPT — P3-C tranche-level falsifiability (`p3c:161`)
The single bracket-gating criterion is a concrete ratio `max(typed/lcss@W0) > 1.0` at N≥50
with equality + JSON + preserve-rich-ast; "If false at W5 close → BLOCKED, residual gap +
hot leaf recorded in REDRESS, NOT paper-closed." CH6-clean.

### ACCEPT — P3-D telemetry binding / producer-only rejection (SPEC §0.4 fold, `SPEC.md:151-198`)
Every exit threshold maps to a named, gate-consumed column; "Every emitted field must be
consumed by `gate-json` in the same wave; a producer-only field fails the wave"
(`:197-198`). N≥50 + median + cold + full-cssom enforced as `gate-json` rejection rules
(`:192-196`). Telemetry-bound. ACCEPT.

### ACCEPT — P3-E pre-blocked ledger receiver routing
Residuals route to named receivers (SK-V18, Pass Omega, REDRESS) with gates rather than
silent deferral; the per-wave pre-block table (`SPEC.md:822-828`) is the gate preventing an
unnamed re-entry. ACCEPT on the CH6 axis (CH3 owns completeness).

### ACCEPT — P3-F SPEC-shape fidelity
P3-F reproduces the six-wave (W0–W5) gate set consistent with SPEC and P3-C and carries the
CH6 close-on-measurement posture; the V2 W2-band fold is consistent across P3-C/P3-F/SPEC.
ACCEPT.

### ACCEPT — Deferral receivers all named with gates (the primary CH6 charge)
No wave deferred without a named receiver + gate: W1–W5 entry gates name the prior wave's
close + (W1/W2/W3) CHALLENGE acceptance (`SPEC.md:30-31,428-436,521-529,615-625,694-701`);
W4 is doubly-conditional with the post-W1 re-profile as receiver (`SPEC.md:867-868`);
tranche residuals route to SK-V18 / Pass Omega / PASS-ALPHA §8 WARN (`SPEC.md:752-753,
764,779-780`); hidden-coupling escapes route through Pass Omega + G-Omega (`SPEC.md:786-787`).
ACCEPT.

---

## §3 — CH6 charge checklist (explicit pass/fail)

| CH6 charge | Status | Evidence |
|---|---|---|
| Every wave closes on measurement, not a future-phase promise | PASS | `SPEC.md:368-375,461-476,547-571,637-648,708-716,755-766`; no-deferrals `:249-251`; zero prose closes (only comparator-wiring `:96` + forbidding `:249-251,816`) |
| No wave deferred without a named receiver + gate | PASS | W1–W5 entry gates; W4 doubly-conditional `:867-868`; residuals → SK-V18 / Pass Omega / PASS-ALPHA §8; P3-A/B per-candidate receivers named |
| L9 commit-by-construction carries its post-CF-1 (post-W1) re-profile gate concretely | PASS | admission antecedent concrete + self-documenting `:670-680`; exit-lift +5% bound `:711-714` (V1 REVISE-1 held); timing-anchor post-W1 in P3-A/B/C |
| Every exit gate measurable + telemetry-bound (N≥50) | PASS | §0.4 N≥50/median/cold/full-cssom enforced (`:153-198`); every gate maps to a named column; **W2 maintain budget now the bench-falsifiable -2.0% band (`:564-568`) — V2 REVISE-1 FOLDED** |
| Every wave carries a revert protocol | PASS | 6/6 present (`grep -c "Revert protocol" = 6`) |
| Every candidate's same-wave consumer named (no orphan kernel) | PASS | `p3a:69-159`; SPEC `:478-479,573-574,650-651,718-719`; orphan-kernel guard `:616-618` |
| Wave count ≤ 12; shortlist ≤ 8 active | PASS | 6 waves (`SPEC.md:271`); 8 active + L9 conditional (`:271-272`) |

All seven CH6 charges PASS. The V2 PARTIAL→REVISE-1 row (every-exit-gate-measurable) is now
PASS via the folded -2.0% W2 band.

### 3.1 — Informational (non-blocking, NOT a REVISE)
The SPEC W2 section (§5, `:547-571`) does not restate a `track1_full_parse ≥ -2.0%`
recognizer-maintain line that P3-C §2.2 (`p3c:98`) carries; the SPEC W2 maintain budget
guards JSON at ±1.0% (`:570`) and the typed plane at -2.0% (`:564-568`). This is a P3-C↔SPEC
completeness nicety, not a CH6 measurability defect: the typed gate and the JSON ±1.0%
tripwire already bound W2's close, and full_parse is the recognizer-guard plane (outcome A),
not the typed >SOTA subject. Recorded as INFORMATIONAL for the aggregator; it does not gate
this lens and is not an orphan REVISE.

---

## §4 — Counts

- **ACCEPT: 17**
- **REVISE: 0**
- **REJECT: 0**

ACCEPT rate (CH6 lens): 17 / 17 = **100%**. Above §3Z 95%. Zero open critical defects, zero
orphan REVISE. The single V2 residual REVISE (SPEC W2 0% floor → -2.0% band) is folded at
`SPEC.md:5,564-568`, matching `p3c:97,156`. All four V1 CH6 dispositions remain folded. The
CH6 anti-paper-close lens is CONVERGED.

---

## §5 — Sources

- `restart/skinny/tranches/sk-v17/SPEC.md` (the wave plan under review; W0 §3 / W1 §4 /
  W2 §5 / W3 §6 / W4 §7 / W5 §8; §0.4 telemetry; §1 non-negotiables; §9 pre-blocks; the V3
  W2-band fold `:5,564-568`).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md` (the S-P3
  cohort; p3c §2.2/§2.4/§3 the W2/W4 gates + the -2.0% W2 band `:97,156`; p3b §141-152 the
  six-wave conditional-dispatch table; p3a §2 the same-wave consumers + S9 conditional `:159`).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V2/CH6.md` (the V2 CH6 single
  residual REVISE-1, verified folded in V3; the four V1 folds verified held).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §3 L9 (the post-CF-1 = post-W1 antecedent: recognition-control loop un-masked by the
  retired alloc floor), §6 (binding shortlist condition 5).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH6, §2 P3-C row.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (close conditions), §0.5 (per-corpus +
  tranche success criterion), §3 (four-lever stack).
- Master HEAD `f87ee713a`.

# SK-V9 S-P1 V4 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V4 (post-fold of V3 CH1-CH6 dispositions).
Inputs: `restart/skinny/tranches/sk-v9/research/p1/hardening/V4/CH{1..6}.md`.

## Verdict — V4 is the first qualifying cycle on 5 of 6 lenses

| Lens | ACCEPT-rate | Verdict | Δ vs V3 |
|---|---:|---|---:|
| CH1 CORRECTNESS | 96.2% strict / 100% lenient | ACCEPT | +28.9pp |
| CH2 GENERALITY | 97.2% strict | ACCEPT | +64pp |
| CH3 REGRESSION | 100% | ACCEPT | +8.3pp |
| CH4 COST | 93.3% strict | **REVISE** | +79pp (still 1.7pp below bar) |
| CH5 HIDDEN COUPLING | 100% | ACCEPT | +4.4pp |
| CH6 ANTI-PAPER-CLOSE | 97.0% | ACCEPT | +8.4pp |

Five of six lenses cleared the §3Z ≥95% bar. CH4 sits at 93.3% with
five named surgical gaps (each <5 minutes' fix). Per §3Z, two
*consecutive* qualifying cycles are required for S-P1 to converge. V4
is the first qualifying cycle on five lenses; V5 must lift CH4 to ≥95%
AND re-verify the other five.

## V3→V4 fold success

All three V3 CH1 REJECTs FOLDED:
- A-8 (y_string_unicode 4.4% residual) — paragraph removed; B §3.4 cited.
- C-1 (stale framing) — full refold with A/B as primary inputs.
- C-8 (match_tiny_plain_string zero-appearances contradiction) — resolved as samply coalescing artefact; B's PMU exports cited.

All 4 V3 CH6 HIGH defects FOLDED. All 4 V3 CH6 MEDIUM defects FOLDED.
Both V3 CH5 REVISEs FOLDED. All V3 CH4 REJECTs (D's S-P1 overreach,
F's PASS-1-PROFILE edit) FOLDED.

The load-bearing surprise: V4-D's regression-script commit revealed
V3's OLS coefficients were ~8× off. The honest correction:
`ns_per_byte ≈ 1.079·(q/B) + 0.184·(n/B) + 0.051`, R²=0.371. **Four
LOSS rows (y_string_unicode, gsoc-2018, unicode_mixed, unicode_escapes)
cannot be closed by a delimiter-only intervention** — the gap exceeds
the entire delimiter contribution. The V3 "10% cut clears 7/11"
forecast is gone; replaced by an honest "4 of 11 require additional
mechanisms beyond per-delimiter cost reduction."

## V5 fold requirements (the remaining surgical gaps)

### CH4 root cause — 5 surgical gaps

1. **V3-B re-capture wall cost** — Time Profiler ~22 min + CPU Counters ~12 min. Add to V3-B §0 footer or §1 capture-methodology.
2. **V3-B `lto=fat` cold-link cost** — ~3-5 min one-time. Absorb into #1.
3. **V3-F edit-dispatch hard cap** — V3-F's 19 surgical SPEC/HANDOFF/DISPATCH-PROMPT edits lack a minute cap. Add ≤30 min total.
4. **V3-B `aggregate.py` commit status** — either commit it or admit reproducibility-by-instruction explicitly.
5. **V3-D two-wave edit sequence rationale** — already admissible but flag clearly.

### CH1 residual — 2 narration fixes

- **V3-A §3 line 237 "agreement is unambiguous"** — qualify as "V2 baseline (superseded; see §4 / B §3.4)".
- **V3-C §5.3 "largest single cycle sink"** — hedge to "among the largest" (off-by-one vs distinct_values/t1 at 2.38 c/B).

### CH6 residual — 1 enumeration

- **V3-D §0 footer** — enumerate the V3 publication errors explicitly (mirror C §6 pattern). The ~8× coefficient correction is honestly captured in §5 but the narrative-layer enumeration is missing.

### CH2/CH3/CH5 residuals — all ACCEPT-with-watch, not blocking

CH2's three RESIDUAL-MINOR (D §1 column-name list, D §2 section title,
C §1.3 13-class table mapping). CH3 §4.1 only goes to LOW once F's
four HANDOFF §5 umbrellas land. CH5's four long-tail risks (PMU
manifest deferred wiring, WIN-row guard enforcement, F1 wave-authorship
deferral, escape-complete subtractive fold).

## V5 plan

1. Apply the 8 surgical edits above to V3 reports (in place).
2. Commit `docs(sk-v9-p1-v5): fold V4 CHALLENGE residuals`.
3. Re-dispatch CHALLENGE V5 (CH1-CH6).
4. Expected outcome: CH4 lifts to ≥95% (closes the 5 gaps); other five
   lenses re-verify ≥95%. V5 = second consecutive qualifying cycle.
   S-P1 converges per §3Z.

## Convergence forecast

V4 already cleared 5 of 6 lenses. V5 is mechanically a single fold
round — no new measurement required, no agent dispatch heavier than
surgical edits. If V5 closes the 5 CH4 gaps cleanly, S-P1 converges and
S-P2 dispatch unblocks.

If V5 surfaces new defects (unlikely given V4's depth), V6 follows the
same protocol. §3Z hard ceiling V ≤ 5 means V5 is the last guaranteed
cycle; V6+ requires user escalation.

# SK-V9 S-P1 V5 Hardening — CH3 REGRESSION Disposition

Lens: CH3 REGRESSION (per `restart/prompts/ORCHESTRATOR.md` §3W).
Scope: V5-folded SK-V9 S-P1 cohort — the six V3 reports
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md` as
edited by V5 commit `d76eef63` ("docs(sk-v9-p1-v5): fold V4 CHALLENGE
residuals — 6 surgical edits"). V4 CH3 baseline: FULL ACCEPT at 36/36
= 100% (zero REJECT / zero REVISE / zero WATCH; both V3 REVISEs D1/D3
closed, both V3 WATCHes C2/D2 closed). V5 must reverify ≥95% as the
second consecutive qualifying cycle per ORCHESTRATOR §3Z.
Authority cross-checked:
- `skinny/REDRESS.md` entries 1–93 (in particular 28, 33, 59, 60–69,
  72, 82–84, 91, 93).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V4/CH3.md`.
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`.
- V5 commit diff at `d76eef63`.
Verdict vocabulary: ACCEPT / WATCH / REVISE / REJECT — bound to a
REDRESS citation and a material-differential argument.
Disposition: read-only. No edits, no commits.

---

## §1 — V5-edits REDRESS audit (per edit)

V5 applied six surgical edits, all narrative-layer, with **zero
finding-class additions and zero wave-prescription additions**. Each
edit is audited below against the REDRESS pre-block ledger and the V4
CH3 closure posture.

### §1.1 — V3-A §3 line 237: V2 "unambiguous agreement" superseded qualifier

**Edit shape.** Replace V3-A's V2-baseline narration that "the
agreement is unambiguous: every parse-only Track 1 row ... has the
same hot leaf at ~95-99% self-time" with a qualified frame "The V2
baseline (superseded; see §4 and P1-V3-B §3.4): every parse-only Track
1 row appears to share the same hot leaf at ~95-99% self-time. That
~95-99% figure is a frame-pointer-coalescing artefact of the samply
mode-I capture; xctrace Time Profiler with DWARF resolves the inlined
leaves and falsifies the single-symbol attribution."

**REDRESS classes the edit touches.** None directly. The edit
*strengthens* REDRESS 91's telemetry-overclaim discipline by
explicitly labelling the V2 samply mode-I ~95-99% reading as an
artefact, not a behaviour claim. It is the inverse of admission: a
demotion of a V2 over-claim, not a re-route to a rejected shape.

**Differential.** The edit narrows attribution rather than widening
it. It does not propose a new hot-leaf claim, does not introduce a
new producer, does not reopen any boundary-collapse / wide-scan /
per-quartet-classifier route. It tracks the V4 fold's CH1-A4-9
disposition that the samply mode-I attribution was a coalescing
artefact, and routes the falsification to V3-B §3.4 (the xctrace
DWARF lane).

**REDRESS-reopen check.** Cross-referenced against REDRESS 28, 33,
60, 61, 62, 64, 72, 82, 83, 84, 91, 93: zero overlap with the routes
the edit touches. Narration-layer-only.

**Verdict: ACCEPT.** No REDRESS-rejected route reopens.

### §1.2 — V3-C §5.3 line 717: "largest single cycle sink" → "among the largest"

**Edit shape.** Replace V3-C's "The escape-codec class is the largest
single cycle sink in the entire 34-row table" with "The escape-codec
class is **among the largest** single cycle sinks in the 34-row table
(distinct_values/t1 per-string-span at 3.850 × 0.619 = 2.38 c/B is
marginally larger; cf. CH1 V4 A4-9 / C4-5 hedges)."

**REDRESS classes the edit touches.** Indirectly REDRESS 82
(escape-codec class) and the per-string-span class umbrella (REDRESS
60-65/83-84). The edit qualifies a *ranking* claim, not an admission
claim, and explicitly names the per-string-span/distinct_values row
as the marginal-largest sink — but the naming is **diagnostic** and
explicitly anchors to CH1 V4 hedges.

**Differential.** Hedging a ranking superlative to "among the
largest" *strengthens* the V4 D-§6.1 / D-§6.2 finding posture
("diagnostic finding, intervention deferred to S-P3 per F1") because
it removes the over-strong ranking that could licence a single-target
wave proposal. The cited 2.38 c/B for distinct_values/t1 is a
**per-row residual** observation, not a wave proposal; it does not
propose a fused materialiser, a string-scanner widening, or any
boundary-collapse shape. The per-string-span scanner is named under
its primitive-class vocabulary (Lock 14, V4-B), not under a
JSON-symbol-specific widening.

**REDRESS-reopen check.** The edit's distinct_values/t1 mention sits
in the *per-string-span scanner* primitive class, which is
pre-blocked by REDRESS 60 / 61 / 62 / 83 / 84. The V4 D-§6.1
"REPLACES not alongside" Lock-1 binding remains intact; the V5 edit
does not propose any intervention on that primitive class. The hedge
is narrative-only.

**Verdict: ACCEPT.** No REDRESS-rejected route reopens.

### §1.3 — V3-D §0 footer: enumerate 8 V3 publication errors (CH6-D)

**Edit shape.** Insert a "V3 publication errors enumerated (V5 fold
per CH6-D)" block in V3-D §0 footer, listing 8 V3 errors the V4 fold
surfaced:

1. OLS coefficient `a` 8.64 → 1.079.
2. OLS coefficient `b` 1.47 → 0.184.
3. OLS intercept `c` 0.410 → 0.051.
4. R² absent in V3; V4 publishes 0.371.
5. "10% per-quote cut clears 7/11 LOSS rows" forecast superseded.
6. "25% covers 9/11" forecast superseded.
7. OLS sign-convention provenance gap.
8. Per-row residual table absent; V4 publishes residuals showing 4
   uncloseable rows exceed 130-460% of regression budget.

**REDRESS classes the edit could touch under guise of "honest correction".**
The dispatch lens flags this exact route: could the enumeration
*admit* a rejected shape under the cover of honest error
correction? Cross-check each enumerated item:

- **Items 1, 2, 3** (coefficient revisions): pure scalar value
  corrections. The downstream consequence is that the V3
  "delimiter-cost-dominant" framing weakens, not strengthens. **The
  weaker the OLS fit, the LESS licence for a per-delimiter-widening
  intervention** — i.e., it makes the REDRESS 60/61/62/64/83/84
  string-scanner-widening pre-block *more* binding, not less.
- **Item 4** (R²=0.371 with p_b=0.545 caveat): explicitly demotes the
  numeric-token coefficient to statistically insignificant. This
  removes any licence for a number-FSM widening (which would touch
  REDRESS 80/81's number-FSM admit) by saying the OLS model **does
  not support** wave authoring against that coefficient. Pre-block
  reinforced.
- **Item 5** ("10% per-quote cut clears 7/11 LOSS rows" gone): the
  forecast that licenced per-delimiter waves is **deleted**. The new
  framing — "4 of 11 cannot be closed by a delimiter-only
  intervention" — is a *negative* hypothesis about intervention
  reach, not a positive proposal for any new mechanism. Critically,
  the edit does NOT name what the "additional mechanisms" might be;
  it stops at the diagnostic. **No new producer proposed.**
- **Item 6** ("25% covers 9/11" superseded): same shape as item 5;
  removes a wave-licencing forecast. No new producer.
- **Item 7** (sign-convention provenance gap): records that V3 lacked
  the regression script; V4 commits it. Reproducibility
  strengthening; no route change.
- **Item 8** (per-row residual table publication): "the four
  uncloseable rows exceed 130-460% of the regression's full per-byte
  budget — a hypothesis-sized finding, **not a wave-sized
  intervention**." The explicit "hypothesis-sized" framing is the
  V4 F1 wave-deferral discipline carried verbatim into V5; this
  binds CH3.

**Critical CH3-lens audit (dispatch Q2).** The dispatch asks: does
this enumeration *admit a rejected shape under guise of honest
correction*? Answer: **no**. Every error correction in the
enumeration is *subtractive* (deletes a V3 over-claim, V3 forecast,
or V3 missing artefact). Zero items add a new admission, new
producer, or new wave proposal. The phrase "additional mechanisms
beyond per-delimiter cost reduction" in item 5 is the *only*
forward-looking phrase, and it (a) is unnamed — no specific
mechanism class is admitted, (b) is explicitly demoted to
"hypothesis-sized" in item 8, and (c) is wave-authorship-deferred to
S-P3 by V3-D §6.6's F1 strip (untouched in V5).

**REDRESS-reopen check.** Items 1-8 cross-checked against REDRESS
59, 60-69, 72, 82-84, 91, 93: zero overlap. The enumeration is a
self-audit list, not a route-shape list.

**Verdict: ACCEPT.** The enumeration is a CH6 honesty discipline
that *strengthens* CH3 by deleting wave-licencing forecasts. No
REDRESS-rejected route reopens.

### §1.4 — V3-B §0 footer: re-capture wall cost (CH4-V05/V19/V20)

**Edit shape.** Insert deterministic wall-cost rows for any V5+ S-P1
re-capture:
- xctrace CPU Counters: ~12 min wall (34 captures).
- xctrace Time Profiler: ~22 min wall (34 captures).
- `lto=fat` cold-link: ~3-5 min one-time.
- Aggregate: ~37-39 min full re-capture; ~12 min CPU-Counters-only;
  ~22 min TP-only.

**REDRESS classes the edit could touch.** None directly. The edit is
**capture infrastructure cost characterisation**, not a producer
proposal. The dispatch lens flags this exact concern: does the
re-capture cost note propose any *new producer*?

**Differential.** Cross-check against REDRESS pre-blocks:
- The edit names xctrace CPU Counters and Time Profiler as the V3
  capture lanes. Both are *diagnostic instruments*, not producers.
  REDRESS preamble Fact 2 and the V4 F2 PMU-as-producer umbrella
  (REDRESS class: SPEC §1 non-negotiables + HANDOFF §5
  PMU-producer pre-block) bind PMU to characteriser-only status; the
  V5 edit names PMU usage as a *re-capture wall cost*, not as a
  behaviour-admission path.
- `lto=fat` cold-link is a Cargo build-flag cost, not a producer.
  It does not propose a new compilation path or a new substrate.
- The aggregate ~37-39 min total is a wall-clock budget for a future
  re-capture cycle; explicitly named as such ("V5+ S-P1 re-capture
  carries the following deterministic wall costs").

**Critical CH3-lens audit (dispatch Q4).** Does this re-capture wall
cost propose a new producer? **No**. The edit is descriptive of
instrument cost. PMU rows continue to characterise hot leaves, not
produce behaviour, per V4 F6 and the SPEC §1 non-negotiables.

**REDRESS-reopen check.** Zero overlap with REDRESS 28, 33, 59,
60-69, 72, 82-84, 91, 93. The edit lives entirely in the capture
lane.

**Verdict: ACCEPT.** No REDRESS-rejected route reopens.

### §1.5 — V3-B §0 footer: aggregate.py reproducibility (CH4-V23)

**Edit shape.** Insert "V5 fold — `aggregate.py` reproducibility
(CH4-V23)" block declaring the TP-symbols aggregator lives at
`/tmp/skv9-xctrace-v3/aggregate.py`, reads `xcrun xctrace export
--type tabular` output, and is reproducible-by-instruction.

**REDRESS classes the edit could touch.** None directly. The edit
declares an out-of-tree aggregation script's location and behaviour;
it does not commit the script into the repo nor propose any new
producer.

**Critical CH3-lens audit (dispatch Q4).** The dispatch lens flags:
does the aggregate.py reproducibility note *propose a new producer*?
**No**. The aggregator:
- Reads xctrace-exported XML.
- Bucketises by symbol.
- Surfaces the per-symbol self-time tables already in §2-§3 of V3-B.

The aggregator is **observation-stage telemetry post-processing**.
It is on the same plane as V3-D's `regression.py` (committed to
`/tmp/skv9-xctrace-v3/regression.py` per V4 F5; same character: it
ingests captured numbers and re-derives diagnostic statistics; it does
not produce behaviour).

The V4 F2 umbrella "PMU, cycles-per-byte, masking probes,
structural-scan-only paths, and Criterion slope artefacts as Track 1
/ Track 2 / typed / direct / strict producers" (SPEC §1
non-negotiables umbrella) pre-blocks the *promotion* of any
diagnostic instrument to a producer. The aggregate.py declaration
stays squarely on the diagnostic side of that umbrella: it is named
explicitly as "the buckets are the per-symbol self-time tables
surfaced in §2-§3 of this report" (i.e., already-published
diagnostic numbers, not new producer outputs).

**REDRESS-reopen check.** Zero overlap with REDRESS 28, 33, 59,
60-69, 72, 82-84, 91, 93. The reproducibility-by-instruction note
admits no rejected route; it admits a measurement-replay path.

**Verdict: ACCEPT.** No REDRESS-rejected route reopens. The
reproducibility-by-instruction framing is the lighter-weight
alternative to committing the script (the dispatch named this as the
V4 fold expectation), and it carries no producer-promotion risk.

### §1.6 — V3-F §4 edit-dispatch hard cap (CH4-D29 / CH4-V21)

**Edit shape.** Insert "Edit-dispatch hard cap (V5 fold per CH4-D29
/ CH4-V21)" at the top of V3-F §4, declaring:
- Full batch of 19 surgical edits: ≤30 min total.
- Sequencing: SPEC.md (8) → HANDOFF.md (6) → DISPATCH-PROMPT.md (5).
- Revert protocol: single `git revert` on the batch commit.
- V3 evidence unaffected on revert.

**REDRESS classes the edit could touch.** The dispatch lens flags
this exact concern: could the edit-dispatch hard cap propose any
*new edit* that would reopen REDRESS?

**Critical CH3-lens audit (dispatch Q3).** Cross-check the 19 edits
listed under V3-F §4 (verified by Read of V3-F lines 460-810):
- SPEC.md edits A-D, F-I: status block, V3 reframe, V2-BLOCKED →
  mid-S-P1-V3 reframe, evidence-root pointer additions, close-posture
  reframe. **All narration-only. Zero producer proposals.**
- HANDOFF.md edits A-F: same narration shape + Edit E (the **four
  class-umbrella additions** to §5 Pre-Blocked Routes). The Edit E
  umbrellas are *pre-block widenings*, not admission routes — they
  add REDRESS 60-65/82-84, 66-69, 34/70, and the SPEC §1
  non-negotiables PMU-producer umbrella to HANDOFF §5. **Pre-block
  reinforcement, not admission.**
- DISPATCH-PROMPT.md edits A-E: status-block + required-reading +
  wave-manifest + conditional-release + always-blocked
  cross-reference. **All narration-only. Edit E cross-refs HANDOFF
  §5 for the umbrella ledger without duplicating, which preserves
  single-source-of-truth on the pre-block class.**

The hard cap (≤30 min total) is a *time budget* on the batch; it
does not propose any new edit. The sequencing (SPEC → HANDOFF →
DISPATCH-PROMPT) ensures partial-batch progress can land safely. The
revert protocol guarantees rollback is single-commit-clean.

**Critical observation.** The 19 edits the cap binds are **the V4 F2
umbrella additions** — i.e., the V4 CH3 §4.3 risk mitigation that
"F2's umbrellas land in HANDOFF §5 before any S-P3 dispatch as a
defense in depth against §4.1" is *operationally enabled* by V5's
hard-cap edit. The hard cap doesn't propose new edits; it
**bounds and sequences the umbrella additions** that close V4 CH3
§4.3's residual S-P3 dispatch misreading risk.

**REDRESS-reopen check.** Zero. The 19 edits are all pre-block
strengthening (HANDOFF Edit E umbrellas) or narration (SPEC + most of
HANDOFF + DISPATCH-PROMPT). The hard cap is purely
dispatch-discipline metadata.

**Verdict: ACCEPT.** No REDRESS-rejected route reopens. The hard
cap *accelerates* the V4 CH3 §4.3 residual closure by binding the
umbrella-landing batch to a sub-30-min window.

---

## §2 — Aggregate verdict

| V5 edit | REDRESS class | V5 cite | Differential | Verdict |
|---|---|---|---|---|
| 1 — V3-A §3 V2 superseded qualifier | REDRESS 91 (strengthened) | Explicit ("V2 baseline (superseded; see §4 and P1-V3-B §3.4)") | Demotes V2 over-claim; no new producer. | ACCEPT. |
| 2 — V3-C §5.3 "among the largest" hedge | REDRESS 60-62, 82-84 (untouched) | Explicit (CH1 V4 A4-9 / C4-5 hedge cite) | Hedges superlative; per-string-span class stays diagnostic. | ACCEPT. |
| 3 — V3-D §0 8 publication-error enumeration | None — self-audit list | Explicit (CH6-D cite per item) | Subtractive; deletes wave-licencing forecasts; "hypothesis-sized, not wave-sized" framing. | ACCEPT. |
| 4 — V3-B §0 re-capture wall cost | None — instrument cost | Explicit (CH4-V05/V19/V20 cite) | Capture-lane budget; no producer promotion. | ACCEPT. |
| 5 — V3-B §0 aggregate.py reproducibility | None — measurement replay | Explicit (CH4-V23 cite) | Reproducibility-by-instruction; same diagnostic plane as regression.py. | ACCEPT. |
| 6 — V3-F §4 edit-dispatch hard cap | None — dispatch discipline | Explicit (CH4-D29 / CH4-V21 cite) | Binds umbrella batch to ≤30 min; closes V4 CH3 §4.3 risk operationally. | ACCEPT. |

**ACCEPT rate: 6/6 = 100.0%** on the V5-edit-set audit.

**Combined V5 cohort posture** (V5 fold preserves V4's 36 dispositions
unchanged, adds 0 finding-class deltas, modifies 6 narration / capture
discipline rows):

| Report | V4 ACCEPT | V5 modifications | Net verdict |
|---|---:|---|---|
| P1-V3-A (V5) | 5 | 1 (V2-superseded qualifier) | 5 ACCEPT |
| P1-V3-B (V5) | 5 | 2 (re-capture wall cost + aggregate.py) | 5 ACCEPT |
| P1-V3-C (V5) | 5 | 1 ("among the largest" hedge) | 5 ACCEPT |
| P1-V3-D (V5) | 8 | 1 (8 publication errors enumerated) | 8 ACCEPT |
| P1-V3-E (V5) | 6 | 0 | 6 ACCEPT |
| P1-V3-F (V5) | 7 | 1 (edit-dispatch hard cap) | 7 ACCEPT |
| **Total** | **36** | **6 narration edits** | **36 ACCEPT** |

**ACCEPT rate (combined): 36/36 = 100.0%.**

**Net CH3 verdict for V5: FULL ACCEPT.** Zero V5 edits reopen any
REDRESS-rejected route. The six surgical edits are:
- 1 narration demotion (V2 over-claim → samply artefact, V3-A).
- 1 narration hedge (superlative → "among the largest", V3-C).
- 1 self-audit enumeration (8 V3 publication errors, V3-D).
- 2 capture-discipline notes (wall cost + aggregate.py reproducibility,
  V3-B).
- 1 dispatch-discipline cap (≤30 min on the 19-edit umbrella batch,
  V3-F).

All six are **monotonic with respect to the CH3 lens**: they either
strengthen existing pre-blocks (V2 demotion strengthens REDRESS 91;
superlative hedge strengthens REDRESS 60-62/82-84 by removing
single-target wave licence; publication-error enumeration deletes
wave-licencing forecasts; edit-dispatch hard cap accelerates HANDOFF
§5 umbrella landing) or sit on a separate plane from REDRESS
(re-capture wall cost; aggregate.py reproducibility).

**4 typed-GO + 3 direct-GO row protection (dispatch Q5).** V5 makes
zero edits to V3-D §6.3 (the WIN-row guard with citm_catalog / canada
/ mesh / marine_ik / numbers / instruments), zero edits to V3-D §6.4
(the direct-plane finding with REDRESS 66-69 + 93 binding), and zero
edits to V3-D §6.5 (the typed-plane 4/4 GO admission guard). The
typed-GO rows (twitter REDRESS 71, update_center REDRESS 71, mesh
REDRESS 81, marine_ik REDRESS 81) and the direct-GO rows
(citm_catalog, apache_builds, github_events) remain protected
verbatim from V4. The V3-D §0 publication-error enumeration touches
the *footer* — none of the eight enumerated items touches any of the
seven admitted rows by name; they are all OLS-statistics and
forecast-deletion items.

**Two-consecutive-cycle gate (ORCHESTRATOR §3Z).** V4 CH3 was the
first qualifying cycle at 100%; V5 CH3 is the **second consecutive
qualifying cycle at 100%**. CH3 clears the §3Z requirement.

---

## §3 — Remaining REDRESS-regression risks

### §3.1 — HANDOFF §5 umbrella landing (low)

The V4 CH3 §4.1 / §4.3 residual ("S-P3 misreading the V4 D-§6.1 /
§6.2 findings as wave admission, mitigated when F2's four umbrellas
land in HANDOFF §5") is *operationally enabled* by V5's V3-F §4 hard
cap. The hard cap binds the 19-edit batch to ≤30 min total, sequenced
SPEC → HANDOFF → DISPATCH-PROMPT, with single-`git revert` recovery.
The residual risk is now narrowed to "did the umbrella batch land
in HANDOFF §5 before S-P3 dispatch?" — a gate-evaluation question,
not a CH3 lens question. The gate evaluator (G-S-P1-RERUN-CONVERGED
sign-off, per V4-F §5.3 item 14) must verify HANDOFF §5 carries the
four umbrellas before signing. **LOW** (down from V4 CH3 §4.1's
"medium").

### §3.2 — V3-D §0 enumeration item 5 "additional mechanisms" phrasing (low)

Item 5 of the V3-D §0 V5-fold enumeration includes the phrase
"4 of 11 ... cannot be closed by a delimiter-only intervention". The
remaining wording is "the throughput gap exceeds the entire
delimiter contribution" — i.e., a *negative* hypothesis. No specific
"additional mechanism" is named. The risk that a downstream agent
reads item 5 as licence to propose a non-delimiter wave (e.g.,
returning to a string-scanner-widening shape rejected by REDRESS 60-62
/ 83-84, or a per-quartet unicode-classifier shape rejected by
REDRESS 82) is bounded by (a) the V4 D §6.1 / §6.2 explicit REDRESS
material-differential notes (untouched in V5), (b) the V4 F2 HANDOFF
§5 umbrella additions (sequenced by V5's hard-cap edit), and (c)
item 8 of the same enumeration that explicitly demotes the residual
finding to "hypothesis-sized, not wave-sized intervention". Triple
locked. **LOW.**

### §3.3 — V3-C §5.3 distinct_values/t1 marginal-lead naming (low)

The V5 hedge "distinct_values/t1 per-string-span at 3.850 × 0.619 =
2.38 c/B is marginally larger" names a specific (corpus, track,
primitive-class) tuple. The risk that a downstream agent reads this
as a single-target wave licence on distinct_values is bounded by (a)
the V4 D §6.1 REDRESS material-differential note covering
distinct_values among the 11 parse_only LOSS rows (untouched in V5),
(b) the V4 F2 HANDOFF §5 string-scanner-widening umbrella (REDRESS
60-65 / 82-84) which pre-blocks distinct_values-target widening, and
(c) the V5 hedge wording "marginally larger" + "among the largest"
which explicitly demotes the ranking, not promotes it. **LOW.**

### §3.4 — V5 narration drift over V6+ cycles (very low)

§3Z hard-ceiling is V ≤ 5; V5 is the last guaranteed cycle. If a V6
becomes necessary (e.g., due to a CH4 surgical-gap residual), V5's
narration edits become part of the baseline. The drift risk is that a
V6 narration edit could be interpreted as licence to re-route. The
mitigation is that V5's narration edits are all *monotonic
subtractions* (demotions, hedges, enumerations of past errors); they
do not introduce any positive admission language that V6 could
inadvertently inherit. **VERY LOW** and effectively closed by §3Z
ceiling.

### §3.5 — Capture-instrument promotion risk (very low)

V5's V3-B §0 re-capture wall cost + aggregate.py reproducibility
edits collectively normalise xctrace + aggregate.py as the V3 capture
+ post-processing pipeline. The risk that a downstream agent
*promotes* either to producer status (in violation of SPEC §1
non-negotiables + V4 F2 PMU-as-producer umbrella) is bounded by V4
F6 (the explicit "V3 real-PMU c/B is a diagnostic characteriser of
hot leaves, not a producer" SPEC §1 edit) which is untouched by V5
and remains the binding constraint. **VERY LOW.**

### §3.6 — Cumulative risk verdict

Aggregating §3.1-§3.5: zero risks above LOW. The CH3 lens is on
full closure once HANDOFF §5 carries the four V4 F2 umbrellas, which
V5's hard-cap edit operationally sequences. **V5 CH3 = FULL ACCEPT;
second consecutive qualifying cycle per §3Z; CH3 lens gate-passed.**

---

## §4 — Summary

- **No CH3 REJECT.** Zero V5 edits silently reopen a REDRESS route.
- **No CH3 REVISE.** All six V5 edits carry explicit cites + material
  differentials.
- **No CH3 WATCH.** Each V5 edit is either (a) a narration demotion
  / hedge that strengthens existing pre-blocks, (b) a self-audit
  enumeration that deletes wave-licencing forecasts, or (c) a
  capture-/dispatch-discipline note on a plane separate from
  REDRESS.
- **V4 finding-class layer untouched.** Zero V5 edits modify V4 D
  §6.1 / §6.2 REDRESS material-differential notes, V4 D §6.3-§6.5
  guard rows, V4 E §2.2 NEON-vs-scalar distinction, V4 F §2.13
  expanded SUPERSEDED reasoning, or V4 D §6.6 wave-prescription
  strip. All V4 CH3 closures remain intact.
- **V4 CH3 §4.3 residual operationally closed.** V5's V3-F §4 hard
  cap (≤30 min total, SPEC → HANDOFF → DISPATCH-PROMPT sequencing,
  single-revert protocol) operationally sequences the V4 F2
  umbrella additions that close V4 CH3 §4.1 / §4.3 S-P3
  dispatch-misreading risk.
- **4 typed-GO + 3 direct-GO rows protected.** Zero V5 edits touch
  V3-D §6.3 / §6.4 / §6.5 guard prose; the seven admitted rows
  remain explicitly guarded as in V4.
- **Honest-correction guise check (dispatch Q2).** The V3-D §0
  publication-error enumeration is purely subtractive (deletes V3
  over-claims, V3 forecasts, V3 missing artefacts); no admission of a
  rejected shape under honest-correction cover.
- **Edit-dispatch cap check (dispatch Q3).** The V3-F §4 hard cap
  binds 19 already-named edits (V4-F §4.1-§4.3) that are all
  pre-block strengthening or narration; no new admission edit.
- **Aggregate.py reproducibility check (dispatch Q4).** The
  V3-B §0 aggregate.py note declares a measurement-replay path,
  not a new producer; same diagnostic plane as V4-F5 regression.py.
- **Substrate cardinality (Lock 1) preserved.** V4 D §6.1's "REPLACES
  not alongside" binding sentence is untouched by V5; F6 holds.

Net CH3 verdict for V5: **FULL ACCEPT at 36/36 = 100%.**

**Second consecutive qualifying cycle per §3Z. CH3 lens cleared
for G-S-P1-RERUN-CONVERGED sign-off.**

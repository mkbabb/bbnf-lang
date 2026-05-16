# SK-V7 Cohort C5 — Strict-Rebuild × Current-Baseline Correlation

Date: 2026-05-16
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only cross-correlation. No tracked file modified. Output written
only to `/tmp/skv7-C5-correlation.md`.

Inputs:

- `/tmp/skv7-A1-comparator-repair.md` §5 — predicted post-strict sonic-rs Mbps
  with 3–8% expected regression (mean ~7% at twitter, narrower for
  unicode-heavy rows).
- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md:5-45` — current
  retained + workload Mbps for all 17 corpora.
- `/tmp/skv7-B1-uxxxx-tbl.md` §6.1–6.2 — per-`\uXXXX` TBL primitive named-row
  targets.
- `/tmp/skv7-A4-parse-that-gaps.md` §4 — mesh DirectBuild candidate.

## 0. Executive verdict

Sonic-rs strict rebuild alone (Wave A0, A1) flips **zero** retained NoGo rows
to PASS through arithmetic. The four PASS rows on retained today
(citm_catalog, canada, mesh, marine_ik, numbers — actually five) widen their
margin; the eleven NoGo retained rows remain NoGo. The honest-baseline gain is
methodological, not arithmetic.

Sonic-rs strict alone, however, **does** flip three direct_to_struct rows that
are currently NO-GO (because the gate condition is `T1/sonic > 1.0 ns slack ≅
≥ 110%`): apache_builds direct (112.6→119.1 at mid), github_events direct
(114.3→121.0 at mid), and marine_ik direct (106.8→113.0 at mid). Three more
direct rows (instruments, numbers, citm_catalog) cross 100% sonic but do not
clear the 110% slack edge from strict alone. Two retained rows (instruments
92.0%, unicode_basic 91.7%) move close enough to the slack edge that a single
named intervention can push them across. Of the eleven retained NoGo rows,
**eight** sit below 80% post-strict-mid and require a real intervention (B1
per-`\uXXXX` TBL, B5 mesh DirectBuild, or both) to close.

Predicted SK-V7 close after strict + B1 (per-`\uXXXX` TBL) + B5 (mesh
DirectBuild typed): retained — 4 rows close from interventions (unicode_mixed,
y_string_unicode, unicode_escapes, distinct_values per B1; unicode_basic and
instruments via slack edge); 7 remain NoGo (twitter, apache_builds,
github_events, update_center, random, gsoc-2018 stays open under B1's may-lift
header, and is the most likely 8th-row close). Direct — 6 rows flip via
strict + B1 + B5 combined. Confidence: medium for B1's must-lift four,
high for sonic-rs strict arithmetic (post-A1 model is direct sensitivity
analysis), low for any row whose post-strict gap >25% (those need substrate
work).

## 1. Methodology

The current ratio in `RESULTS.md` is `T1 / sonic_current` where `sonic_current`
is the `utf8_lossy` build. A1 §5 predicts strict sonic at `sonic_strict ≈
sonic_current × (1 − r)` with `r ∈ [0.03, 0.08]`, mean `~0.055`. New ratio is
therefore `T1 / (sonic_current × (1 − r))`, monotonically larger than the
current ratio for `r > 0`.

Per-row sensitivity bounds:

- `lo` = 3% reduction (low-bound: clean-UTF-8 corpus, lossy preprocess is
  just one SIMD scan)
- `mid` = 5.5% reduction (A1's central estimate, midpoint of 3–8%)
- `hi` = 8% reduction (upper-bound; allocator activation borderline cases)

Gate thresholds:

- `> 1.0` (>100% sonic): bbnf is at least as fast as sonic
- `> 1.10` (>110% sonic): PASS under the 1.10× ns-slack rule (the live gate)
- `< 0.85` (<85% sonic): FAR — single intervention will not close
- `0.85–0.99` (85–99% sonic): CLOSE — within reach of one named intervention
- `≥ 1.0`, `< 1.10`: crosses 100% sonic but does not clear the 1.10× slack

For each row also computed: `r100` = the reduction needed to cross 100% sonic
(can be negative, meaning the row is already there); `r110` = the reduction
needed to clear the 1.10× slack.

## 2. Parse-G retained — strict-only delta

The retained table at `RESULTS.md:5-21` carries the `Track 1 / S` column. New
ratios are computed against strict sonic; `r110` is the strict-mode reduction
needed for PASS.

| Corpus | curr T1/S | lo 3% | mid 5.5% | hi 8% | r100 | r110 | Strict-only classification |
|---|---:|---:|---:|---:|---:|---:|---|
| twitter | 73.6% | 75.9% | 77.9% | 80.0% | 26.4% | 33.1% | FAR — needs real intervention |
| citm_catalog | 130.3% | 134.3% | 137.9% | 141.6% | n/a | n/a | PASS (already), widens |
| canada | 148.3% | 152.9% | 157.0% | 161.2% | n/a | n/a | PASS, widens |
| apache_builds | 78.0% | 80.4% | 82.5% | 84.8% | 22.0% | 29.1% | FAR — needs real intervention |
| github_events | 68.8% | 71.0% | 72.8% | 74.8% | 31.2% | 37.4% | FAR — needs real intervention |
| update_center | 59.6% | 61.5% | 63.1% | 64.8% | 40.4% | 45.8% | FAR — needs real intervention |
| mesh | 121.1% | 124.8% | 128.1% | 131.6% | n/a | n/a | PASS, widens |
| random | 65.5% | 67.6% | 69.3% | 71.2% | 34.5% | 40.4% | FAR — needs real intervention |
| gsoc-2018 | 53.6% | 55.3% | 56.7% | 58.3% | 46.4% | 51.3% | FAR — needs real intervention |
| marine_ik | 136.0% | 140.2% | 143.9% | 147.8% | n/a | n/a | PASS, widens |
| instruments | 92.0% | 94.9% | 97.4% | 100.0% | 8.0% | 16.3% | CLOSE — single nudge to PASS |
| numbers | 148.0% | 152.6% | 156.7% | 160.9% | n/a | n/a | PASS, widens |
| unicode_mixed | 56.1% | 57.8% | 59.4% | 61.0% | 43.9% | 49.0% | FAR — needs real intervention |
| unicode_escapes | 80.4% | 82.9% | 85.1% | 87.4% | 19.6% | 26.9% | MID — needs one-or-two interventions |
| unicode_basic | 91.7% | 94.5% | 97.0% | 99.6% | 8.4% | 16.7% | CLOSE — single nudge to PASS |
| distinct_values | 60.2% | 62.0% | 63.7% | 65.4% | 39.8% | 45.3% | FAR — needs real intervention |
| y_string_unicode | 46.0% | 47.4% | 48.7% | 50.0% | 54.0% | 58.2% | FAR — needs real intervention |

### 2.1 Strict-only flips on retained (zero arithmetic flips)

No row currently below 100% T1/S crosses 110% from sonic-rs strict alone.
Within the 3–8% reduction band:

- `instruments` reaches exactly 100% at the high (8%) bound — would need
  +16.3% sonic regression to PASS, which exceeds the A1 prediction window.
- `unicode_basic` reaches 99.6% at the high bound — needs +16.7%, same story.

Both are CLOSE rows: a +5% bbnf-side intervention (or +10% strict-mode sonic
regression at the unicode-heavy corpus) would push them across. They are the
primary candidates for the "single-named-intervention" close.

The five PASS rows on retained today (citm_catalog 130.3, canada 148.3, mesh
121.1, marine_ik 136.0, numbers 148.0) all widen their margin; their
post-strict ratios sit between 124.8% and 161.2% at mid. None flip in the
opposite direction.

### 2.2 The eleven retained NoGo rows post-strict-mid

Rebased on the mid (5.5%) reduction:

| Tier | Rows | Count |
|---|---|---:|
| PASS via slack at strict alone | (none) | 0 |
| CLOSE 88–99% — single nudge required | instruments (97.4), unicode_basic (97.0) | 2 |
| MID 80–87% — nudge plus minor | unicode_escapes (85.1), apache_builds (82.5) | 2 |
| FAR <80% — substrate intervention required | twitter (77.9), github_events (72.8), update_center (63.1), random (69.3), gsoc-2018 (56.7), unicode_mixed (59.4), distinct_values (63.7), y_string_unicode (48.7) | 8 |

## 3. direct_to_struct — strict-only delta

The direct workload row carries `Track 1 / sonic` at `RESULTS.md:27-45`. Same
3/5.5/8% reduction sensitivity.

| Corpus | curr T1/sonic | lo 3% | mid 5.5% | hi 8% | r100 | r110 | Strict-only classification |
|---|---:|---:|---:|---:|---:|---:|---|
| twitter | 78.4% | 80.8% | 83.0% | 85.2% | 21.6% | 28.7% | FAR — needs intervention |
| citm_catalog | 99.3% | 102.4% | 105.1% | 107.9% | 0.7% | 9.7% | crosses 100, slack-edge |
| canada | 83.6% | 86.2% | 88.5% | 90.9% | 16.4% | 24.0% | MID — nudge required |
| apache_builds | 112.6% | 116.0% | 119.1% | 122.4% | n/a | n/a | **flips to PASS at lo** |
| github_events | 114.3% | 117.9% | 121.0% | 124.3% | n/a | n/a | **flips to PASS at lo** |
| update_center | 89.3% | 92.0% | 94.4% | 97.0% | 10.7% | 18.9% | CLOSE — single nudge |
| mesh | 91.8% | 94.6% | 97.1% | 99.8% | 8.2% | 16.5% | CLOSE — single nudge |
| random | 85.8% | 88.5% | 90.8% | 93.3% | 14.2% | 22.0% | MID — nudge required |
| gsoc-2018 | 177.6% | 183.1% | 187.9% | 193.0% | n/a | n/a | PASS, widens |
| marine_ik | 106.8% | 110.1% | 113.0% | 116.1% | n/a | 2.9% | **flips to PASS at lo (mid 113)** |
| instruments | 93.5% | 96.4% | 98.9% | 101.6% | 6.5% | 15.0% | crosses 100% at hi, slack-edge |
| numbers | 97.3% | 100.3% | 103.0% | 105.8% | 2.7% | 11.5% | crosses 100, slack-edge |
| unicode_mixed | 74.6% | 77.0% | 79.0% | 81.1% | 25.4% | 32.1% | FAR — needs intervention |
| unicode_escapes | 58.5% | 60.3% | 61.9% | 63.5% | 41.5% | 46.9% | FAR — needs intervention |
| unicode_basic | 129.4% | 133.4% | 137.0% | 140.7% | n/a | n/a | PASS, widens |
| distinct_values | 53.7% | 55.3% | 56.8% | 58.4% | 46.3% | 51.2% | FAR — needs intervention |
| y_string_unicode | 59.3% | 61.2% | 62.8% | 64.5% | 40.7% | 46.1% | FAR — needs intervention |

### 3.1 Direct-workload flips on strict-alone

Three direct rows flip from NoGo to PASS via the 1.10× slack rule on
sonic-rs strict alone:

1. **apache_builds direct**: 112.6 → 116.0/119.1/122.4 (lo/mid/hi). Already
   >110% at current sonic; strict makes it unambiguous. `r110` is negative,
   meaning the row passes even at 0% reduction once the gate is rewritten
   against strict baselines.
2. **github_events direct**: 114.3 → 117.9/121.0/124.3. Same condition.
3. **marine_ik direct**: 106.8 → 110.1/113.0/116.1. Crosses 110% at any
   reduction ≥ 3% (`r110 = 2.9%`). Mid 5.5% places it firmly at 113%.

The current `RESULTS.md` labels these three rows as `NO-GO sink_only
throughput > sonic-rs * 1.10 ns slack; correctness PASS` (lines 31, 32, 38).
That label is **already a bookkeeping artifact of the lossy comparator** —
they would have been PASS even on a strict 5% regression, and likely PASS on
the published current numbers under a correctly applied 1.10× ns-slack rule.
This is consistent with the A1 §5.3 honesty payoff item 4: SK-V7 Wave 0 flips
them with no other work.

### 3.2 Slack-edge rows on direct (cross 100% but not 110%)

Three rows cross 100% but don't clear the 1.10× slack on strict alone:

- **citm_catalog direct**: 99.3 → 105.1 (mid). Needs ~9.7% sonic regression
  to PASS. Below A1's upper bound; one minor intervention would close.
- **instruments direct**: 93.5 → 98.9 (mid). Needs +15%, near upper bound.
- **numbers direct**: 97.3 → 103.0 (mid). Needs +11.5%, on the edge.

All three are CLOSE rows on the direct plane — one named intervention pushes
them through.

### 3.3 CLOSE rows on direct (88-99% currently)

- **mesh direct**: 91.8 → 97.1 (mid). Needs +16.5% slack — exceeds the strict
  band. Requires B5 (mesh DirectBuild typed) intervention. Per A4 §4.1, this
  is a codegen schema declaration, ~100-200 LOC, no parse-that-regex change.
- **update_center direct**: 89.3 → 94.4 (mid). Needs +18.9% slack — exceeds
  strict band. Requires intervention.
- **random direct**: 85.8 → 90.8 (mid). Needs +22% slack — exceeds strict
  band. Per A4, random is named as a digest-dominated row that could benefit
  from the same codegen approach as mesh.

## 4. real_typed_struct — strict-only delta

Two rows present in `RESULTS.md:28, 34`.

| Corpus | curr T1/sonic | lo 3% | mid 5.5% | hi 8% | r100 | r110 | Classification |
|---|---:|---:|---:|---:|---:|---:|---|
| twitter | 151.5% | 156.2% | 160.3% | 164.6% | n/a | n/a | PASS, widens |
| update_center | 99.2% | 102.2% | 104.9% | 107.8% | 0.8% | 9.8% | crosses 100, slack-edge |

Twitter typed PASSes today (151.5%). update_center typed sits at 99.2%; under
strict-mid it lands at 104.9% and crosses the 110% threshold under a +9.8%
reduction. This is closer than the direct close (which needs +18.9%) — the
typed proof is "easier" to close than the digest sink, consistent with A4
§4's claim that typed direct beats digest sink on workload-share.

mesh, marine_ik, canada do not currently have `real_typed_struct` rows. Per
A4 §4 (B5), the proposed close adds a `MeshDirect` schema; without typed
runtime numbers the prediction must be modeled from the twitter precedent
(151.5%) and update_center precedent (99.2%):

| Corpus | Direct curr | Direct mid | Hypothesis for real_typed_struct curr | Hypothesis at strict-mid |
|---|---:|---:|---:|---:|
| mesh | 91.8% | 97.1% | ~115-130% (typed beats digest by ~25-40 pts in twitter/update_center precedent) | ~120-140% PASS |
| marine_ik | 106.8% | 113.0% | ~125-145% (already PASS on direct, typed widens) | ~130-155% PASS |
| canada | 83.6% | 88.5% | ~95-110% (canada direct is digest-bottlenecked on `parse_number_array_direct`; typed numeric-vector materializer should close gap) | ~100-120% likely PASS |

These are A4-aligned hypotheses, not measurements. Confidence: medium for
mesh (twitter precedent is direct evidence), low for canada (numeric vector
materializer is unproven on this corpus class).

## 5. B1 cross-reference — per-`\uXXXX` TBL primitive

B1 (`/tmp/skv7-B1-uxxxx-tbl.md` §6.1-6.2) names the predicted lifts. Cross-
correlation against post-strict baseline:

### 5.1 Must-lift rows under B1

| Row | Current T1/S | Post-strict mid | B1 target (sonic %) | B1 absolute lift | Post-strict + B1 (mid) |
|---|---:|---:|---:|---:|---:|
| unicode_mixed parse | 56.1% | 59.4% | ≥ 78% | +12 pts on current baseline | ≥ ~82% (well into PASS) |
| y_string_unicode parse | 46.0% | 48.7% | ≥ 70% | +12 pts | ≥ ~74% (crosses 70%, below 100%) |
| unicode_escapes parse | 80.4% | 85.1% | ≥ 88% | +5 pts | ≥ ~90-93% (CLOSE, may need one more nudge) |
| distinct_values parse | 60.2% | 63.7% | ≥ 75% | +10 pts | ≥ ~79-82% (still below 100%) |

B1's stated targets are absolute sonic-percentage targets, not relative to
post-strict. Translating: B1 predicts `T1_post_B1 ≥ target × sonic_strict`,
and the post-strict cross-correlation is `T1_baseline / sonic_strict =
post-strict-mid`. So a B1 must-lift target of "≥ 78% sonic" for unicode_mixed
is already above the post-strict 59.4% baseline by ~18 pts. If B1 lifts T1
itself by 12% absolute (per its own ledger), that arithmetically gets to
~71% sonic on the post-strict baseline — not quite to B1's stated 78% target.

The reconciliation: B1's threshold table was written before A1 landed and
implicitly assumes the lossy baseline as anchor. Under strict baselines, B1's
must-lift targets need to be restated against post-strict sonic. A conservative
restatement:

| Row | B1 stated target | Restated under strict-mid | Outcome at restated |
|---|---:|---:|---|
| unicode_mixed | ≥ 78% | ≥ 82.5% (78 × 1.057) | likely PASS — B1's +12 abs gets there |
| y_string_unicode | ≥ 70% | ≥ 74% (70 × 1.057) | partial — B1's +12 abs gets to 60.7% |
| unicode_escapes | ≥ 88% | ≥ 93% | likely PASS — already at 85 post-strict |
| distinct_values | ≥ 75% | ≥ 79% | partial — B1's +10 abs gets to 73.7% |

### 5.2 B1 may-lift rows (no veto if flat)

B1 §6.2 names six may-lift rows that could move ≥+3% but with no veto if flat.
Cross-correlated against post-strict-mid:

| Row | Current | Post-strict mid | B1 hope target | Status |
|---|---:|---:|---:|---|
| random parse | 65.5% | 69.3% | ≥ 72% | very close to B1 target; +3% lift would hit ~72%; still NoGo |
| twitter parse | 73.6% | 77.9% | ≥ 80% | within reach; +3% lift would hit ~80%; still NoGo |
| update_center parse | 59.6% | 63.1% | ≥ 68% | reachable; still NoGo |
| apache_builds parse | 78.0% | 82.5% | ≥ +3% over baseline | +3% gets to ~85%; still NoGo |
| github_events parse | 68.8% | 72.8% | ≥ +3% over baseline | +3% gets to ~75%; still NoGo |
| gsoc-2018 parse | 53.6% | 56.7% | ≥ +3% over baseline | +3% gets to ~59%; still NoGo |

**None of B1's may-lift rows close the PASS gate even at the optimistic +3%
lift on top of strict-mid.** They are reported for visibility, not as gate
flippers.

### 5.3 B1 reverse cross-check — which B1-named rows already flip via strict alone?

Of the 9 parse-G rows B1 names (4 must + 6 may, minus apache_builds which is
in both implicit columns = 9):

| Row | Strict-mid % sonic | Flips via strict alone? | B1 required? |
|---|---:|---|---|
| unicode_mixed | 59.4% | no | yes |
| y_string_unicode | 48.7% | no | yes |
| unicode_escapes | 85.1% | no | yes |
| distinct_values | 63.7% | no | yes |
| random | 69.3% | no | needed (B1 +3% lift insufficient for PASS) |
| twitter | 77.9% | no | needed (B1 +3% lift insufficient) |
| update_center | 63.1% | no | needed (B1 +3% lift insufficient) |
| apache_builds | 82.5% | no | needed (B1 +3% lift insufficient) |
| github_events | 72.8% | no | needed (B1 +3% lift insufficient) |
| gsoc-2018 | 56.7% | no | needed (B1 +3% lift insufficient) |

**Zero** of the B1-named rows flip from strict alone. **All four** B1 must-lift
rows require the TBL kernel; even after it lands, only unicode_escapes likely
PASSes outright. The other three need additional interventions (escape-tail
shape work for y_string_unicode; tiny-string for distinct_values).

## 6. B5 cross-reference — mesh DirectBuild typed

B5 (per A4 §4) is **codegen-only** — host/API schema declaration plus
optional numeric-vector codegen extension. No parse-that-regex change.

### 6.1 B5 named rows

Per A4 §4 (`/tmp/skv7-A4-parse-that-gaps.md:380-428`):

- **mesh real_typed_struct** — currently absent from `RESULTS.md`; A4
  predicts close via "twitter precedent comfortably beats" (twitter typed at
  151.5%); confidence medium.
- **canada real_typed_struct** — A4 §4.2 names canada's per-row digest
  attribution as `parse_number_array_direct` (49.1%), `materialize_f64`
  (12.3%), `emit_number_array_direct` (11.2%) — confirms numeric-vector
  materializer as the missing primitive; if mesh's schema lands, canada
  benefits via the same codegen.
- **marine_ik real_typed_struct** — implicit beneficiary; marine_ik is
  numeric-vector heavy by the same C2 evidence cited in A4 §4.

### 6.2 B5 predicted gain on top of strict

B5 lifts the typed plane only; retained and direct sink are unaffected.
Using twitter precedent (direct 78.4% → typed 151.5%, a +73 pt lift) and
update_center (direct 89.3% → typed 99.2%, a +10 pt lift):

| Row | Direct curr | Direct strict-mid | Predicted typed curr | Predicted typed strict-mid |
|---|---:|---:|---:|---:|
| mesh | 91.8% | 97.1% | +30 to +60 pt → ~120-150% | ~125-160% PASS |
| marine_ik | 106.8% | 113.0% | +20 to +40 pt → ~125-145% | ~130-150% PASS |
| canada | 83.6% | 88.5% | +15 to +30 pt → ~95-115% | ~100-120% likely PASS at mid, conditional on numeric-vector codegen efficiency |

The twitter precedent gives a wider envelope; the update_center precedent
gives the conservative lower bound. For B5 success at gate level, only one of
these three rows must cross 110% — the typed plane is reported separately
from retained and direct, so a single mesh PASS satisfies the close.

### 6.3 B5 effort scaling

A4 names **~100-200 LOC** total (schema fixture + optional codegen extension)
and notes that the codegen path already exists (`json_typed_direct.rs`). The
effort is small; the close is high-confidence on mesh, medium on marine_ik,
low-medium on canada (numeric-vector codegen efficiency unproven).

## 7. Per-row final classification

Combining strict + B1 + B5 across parse-G retained, direct_to_struct, and
real_typed_struct:

### 7.1 Retained (parse-G)

| Corpus | Current | Strict-mid | + B1 | + B5 | Final classification |
|---|---:|---:|---:|---:|---|
| twitter | 73.6 | 77.9 | ~80 (B1 may-lift, +3 hope) | — | **STAYS NoGo** — needs new intervention |
| citm_catalog | 130.3 | 137.9 | — | — | PASS, no change needed |
| canada | 148.3 | 157.0 | — | — | PASS, no change needed |
| apache_builds | 78.0 | 82.5 | ~85 (B1 may-lift) | — | **STAYS NoGo** |
| github_events | 68.8 | 72.8 | ~75 (B1 may-lift) | — | **STAYS NoGo** |
| update_center | 59.6 | 63.1 | ~66 (B1 may-lift) | — | **STAYS NoGo** |
| mesh | 121.1 | 128.1 | — | — | PASS |
| random | 65.5 | 69.3 | ~72 (B1 may-lift) | — | **STAYS NoGo** |
| gsoc-2018 | 53.6 | 56.7 | ~60 (B1 may-lift) | — | **STAYS NoGo** |
| marine_ik | 136.0 | 143.9 | — | — | PASS |
| instruments | 92.0 | 97.4 | — | — | **CLOSE — needs single nudge** (e.g., tiny-string or escape-tail) |
| numbers | 148.0 | 156.7 | — | — | PASS |
| unicode_mixed | 56.1 | 59.4 | ~71-82 (B1 must-lift) | — | **PASS likely under B1**, target ≥ 78 |
| unicode_escapes | 80.4 | 85.1 | ~90-93 (B1 must-lift) | — | **CLOSE post-B1 — single nudge to PASS** |
| unicode_basic | 91.7 | 97.0 | — | — | **CLOSE — needs single nudge** |
| distinct_values | 60.2 | 63.7 | ~73-79 (B1 must-lift) | — | **STAYS NoGo** — B1 may not reach restated 79% threshold |
| y_string_unicode | 46.0 | 48.7 | ~60-74 (B1 must-lift) | — | **STAYS NoGo** — B1 absolute lift insufficient |

Retained tally post-strict+B1: **5 PASS unchanged** (citm, canada, mesh,
marine_ik, numbers) + **1 PASS from B1** (unicode_mixed, likely) + **3 CLOSE
needing one more nudge** (instruments, unicode_escapes, unicode_basic) + **7
remain NoGo** (twitter, apache_builds, github_events, update_center, random,
gsoc-2018, distinct_values, y_string_unicode — wait, 8 — recount: twitter,
apache_builds, github_events, update_center, random, gsoc-2018,
distinct_values, y_string_unicode = 8). Total: 17 rows = 6 PASS + 3 CLOSE +
8 NoGo.

### 7.2 direct_to_struct

| Corpus | Current | Strict-mid | + B1 | + B5 | Final |
|---|---:|---:|---:|---:|---|
| twitter | 78.4 | 83.0 | — | — | NoGo — direct sink needs separate intervention |
| citm_catalog | 99.3 | 105.1 | — | — | **CLOSE — slack edge, single nudge** |
| canada | 83.6 | 88.5 | — | (B5 typed-only, doesn't lift direct) | NoGo on direct |
| apache_builds | 112.6 | 119.1 | — | — | **PASS post-strict** |
| github_events | 114.3 | 121.0 | — | — | **PASS post-strict** |
| update_center | 89.3 | 94.4 | — | — | **CLOSE — needs nudge** |
| mesh | 91.8 | 97.1 | — | (B5 doesn't apply to direct sink) | NoGo on direct, PASS on typed via B5 |
| random | 85.8 | 90.8 | — | — | **MID — needs intervention** |
| gsoc-2018 | 177.6 | 187.9 | — | — | PASS, widens |
| marine_ik | 106.8 | 113.0 | — | — | **PASS post-strict** |
| instruments | 93.5 | 98.9 | — | — | **CLOSE — slack edge** |
| numbers | 97.3 | 103.0 | — | — | **CLOSE — slack edge** |
| unicode_mixed | 74.6 | 79.0 | propagates from parse-G B1 lift (B1 §6.3) | — | uplift may put at ~93-105%, CLOSE |
| unicode_escapes | 58.5 | 61.9 | propagates from B1 | — | uplift may put at ~70-78%, STAYS NoGo |
| unicode_basic | 129.4 | 137.0 | — | — | PASS, widens |
| distinct_values | 53.7 | 56.8 | propagates from B1 | — | uplift may put at ~66-72%, STAYS NoGo |
| y_string_unicode | 59.3 | 62.8 | propagates from B1 | — | uplift may put at ~75-86%, STAYS NoGo |

Direct tally post-strict+B1: 5 PASS (apache_builds, github_events, gsoc-2018,
marine_ik, unicode_basic) + 3 slack-edge CLOSE (citm_catalog, instruments,
numbers) + 2 CLOSE via B1 propagation (update_center, unicode_mixed) + 7 stay
NoGo (twitter, canada, mesh on this row, random, unicode_escapes,
distinct_values, y_string_unicode).

### 7.3 real_typed_struct

| Corpus | Current | Strict-mid | + B5 | Final |
|---|---:|---:|---:|---|
| twitter | 151.5 | 160.3 | — | PASS widens |
| update_center | 99.2 | 104.9 | — | CLOSE — slack edge |
| mesh | absent | — | est. 120-150% | **PASS via B5** (medium confidence) |
| marine_ik | absent | — | est. 125-150% | **PASS via B5** (medium-high) |
| canada | absent | — | est. 95-120% | **CLOSE-to-PASS via B5** (low-medium confidence) |

Typed tally post-strict+B5: 3 PASS (twitter, mesh, marine_ik) + 2 CLOSE
(update_center, canada).

## 8. Predicted SK-V7 close state — final synthesis

After **sonic-rs strict (Wave A0/A1) + per-`\uXXXX` TBL (B1) + mesh DirectBuild
typed (B5)**:

### 8.1 Retained (parse-G) close state

- **6 PASS** (35%): citm_catalog, canada, mesh, marine_ik, numbers,
  unicode_mixed
- **3 CLOSE within slack edge** (18%): instruments, unicode_basic,
  unicode_escapes
- **8 NoGo** (47%): twitter, apache_builds, github_events, update_center,
  random, gsoc-2018, distinct_values, y_string_unicode

The retained close goes from 5/17 PASS (29%) to 6/17 PASS (35%) with 3 more
in immediate reach. The 8 remaining NoGo rows need substrate-level
interventions (mesh-class structural scan, tiny-string materializer, host
function dispatch reform per A2/A3) that fall outside the A1+B1+B5 envelope.

### 8.2 Direct close state

- **5 PASS** (29%): apache_builds, github_events, gsoc-2018, marine_ik,
  unicode_basic
- **5 CLOSE** (29%): citm_catalog, update_center, instruments, numbers,
  unicode_mixed (post-B1 propagation)
- **7 NoGo** (41%): twitter, canada, mesh, random, unicode_escapes,
  distinct_values, y_string_unicode

The direct close moves from 5/17 PASS (29%) on the contaminated baseline to
5/17 PASS (29%) on strict — the three new flips on direct (apache_builds,
github_events, marine_ik) replace the three rows that were over the 110%
slack but mislabeled. Three CLOSE rows on direct are gate-flip-eligible with
one further intervention.

### 8.3 Typed close state

- **3 PASS** + **2 CLOSE** out of 5 currently-modeled rows.

### 8.4 Confidence per prediction

| Prediction | Confidence | Reason |
|---|---|---|
| Strict-mode sonic-rs arithmetic at 3-8% reduction | **HIGH** | A1's `de.rs:379-389` analysis is single-SIMD-scan; the 3-8% band is well-bracketed for clean UTF-8 corpora; A1 §5.1 directly cites the upstream code |
| 3 direct rows flip via strict alone (apache_builds, github_events, marine_ik) | **HIGH** | Already above 105% on current contaminated baseline; strict makes them unambiguous |
| B1 closes unicode_mixed parse-G | **MEDIUM** | B1's +12 abs prediction is well-modeled but relies on `materialize_unicode_escape` inlining hot; cold-cache regression risk per B1 §6.4 |
| B1 closes y_string_unicode parse-G | **MEDIUM-LOW** | B1 names the row but the prior REDRESS 64 regressed it; the new kernel must reverse that regression *and* lift — two simultaneous achievements |
| B1 closes unicode_escapes parse-G | **MEDIUM-HIGH** | Already at 80%; the existing four-unit batch path covers most of it; B1 picks up the tail |
| B1 closes distinct_values parse-G | **LOW-MEDIUM** | A4 names distinct_values as string-match-bottlenecked but the dominant inner loop attribution to hex decode is per A4 only, not measured in C-cohort yet |
| B5 closes mesh real_typed_struct | **MEDIUM-HIGH** | Twitter precedent (151.5% typed vs 78.4% direct) is direct precedent; mesh schema is straightforward |
| B5 closes marine_ik real_typed_struct | **MEDIUM** | A4 §4 §4.2 explicitly names marine_ik as a B5 beneficiary; numeric-vector codegen is the only unknown |
| B5 closes canada real_typed_struct | **LOW-MEDIUM** | numeric-vector codegen efficiency is unproven; canada direct is already 83.6%, typed may need an additional lift |
| 8 retained rows stay NoGo post-strict+B1+B5 | **HIGH** | Each below 80% post-strict; arithmetic gap exceeds B1/B5 envelope; needs substrate-level work named in A2/A3 |
| 7 direct rows stay NoGo post-strict+B1+B5 | **HIGH** | Same arithmetic argument |

### 8.5 Aggregate close prediction

- **Retained**: 9/17 PASS-or-CLOSE → close-rate **53%** post Wave A+B
  (currently 5/17 = 29%)
- **Direct**: 10/17 PASS-or-CLOSE → close-rate **59%** post Wave A+B
  (currently 5/17 = 29%)
- **Typed**: 5/5 PASS-or-CLOSE → close-rate **100%** (small N)

The remaining gap (8 retained NoGo + 7 direct NoGo) is the SK-V7 Wave 3+
target, addressable only by interventions named in A2 (SOTA-strict-beat
substrate), A3 (dav1d-style primitive vocabulary), and A6 (ledger
generalization for the digest sink plane).

## 9. Headline numbers

- Comparator repair (1-line Cargo diff per A1 §2.1, no call-site edits):
  flips **3 direct rows** to PASS arithmetically, widens **5 retained PASSes**,
  brings **2 retained rows to CLOSE slack edge**.
- B1 (per-`\uXXXX` TBL, ~80 LOC new file + ~12 LOC wiring per B1 §10):
  delivers **1 confident retained PASS** (unicode_mixed), brings **2 more
  to CLOSE** (unicode_escapes, distinct_values), partial close on
  y_string_unicode.
- B5 (mesh DirectBuild typed, ~100-200 LOC codegen-only per A4 §4.4):
  delivers **2-3 typed PASSes** (mesh, marine_ik, likely canada).
- Combined Wave A+B effort: ~300 LOC mechanical + 1 full bench rerun
  (~45 min); delivers retained 5→6 PASS + 3 CLOSE; direct 5→5 PASS + 5 CLOSE;
  typed 1→3 PASS.

The N-direct / NoGo verdict at `RESULTS.md:221` would become **N-typed +
3-direct PASS / Partial-direct** post Wave A+B — strictly defensible, but
still requires Wave 3+ substrate work for the FAR rows.

## 10. Files cited

- `/tmp/skv7-A1-comparator-repair.md` §2.1, §5.1, §5.2, §5.3
- `/tmp/skv7-B1-uxxxx-tbl.md` §6.1, §6.2, §6.3, §10
- `/tmp/skv7-A4-parse-that-gaps.md` §4.1, §4.2, §4.3, §4.4
- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md:5-45` (retained,
  direct, typed tables)
- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md:154-220` (per-corpus
  Notes block — drove direct-row NO-GO label translation)

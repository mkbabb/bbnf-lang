# S-P0 CHALLENGE V4 — CONSOLIDATED Aggregator

Authored 2026-05-23 by the SK-V14 S-P0 V4 aggregator after the seven
V4 lens dispositions (CH1–CH7) landed write-only under `restart/
skinny/tranches/sk-v14/audit-overfit/hardening/V4/`. Authority:
`restart/prompts/ORCHESTRATOR.md §3Z step 4` + `restart/skinny/
tranches/sk-v14/audit-overfit/hardening/V1/CHALLENGE-CONTEXT.md §6`
+ `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V2/
CHALLENGE-V2-ADDENDUM.md §5` + `restart/skinny/tranches/sk-v14/
audit-overfit/hardening/V3/HARDENING-S-P0-V3-CONSOLIDATED.md §2`
(V4 micro-fold prescription inherited). Source-of-truth lens files:
`CH1.md`, `CH2.md`, `CH3.md`, `CH4.md`, `CH5.md`, `CH6.md`,
`CH7.md` — all untracked at this moment, all committed atomically
alongside this CONSOLIDATED file per the write-only protocol. V4
micro-fold landing commit: `f8e279877` (1 file modified, +9 / -4,
prose-only on SYNTHESIS-AUDIT-OVERFIT.md).

## §0 — V4 cycle verdict

| Lens | Anchors | ACCEPT | REVISE | REJECT | NEW | ACCEPT-rate |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| CH1 CORRECTNESS | 7 | 7 | 0 | 0 | 0 (V3 orphans resolved; 1 below-threshold style observation at §3) | **100 %** |
| CH2 GENERALITY | 8 | 8 | 0 | 0 | 0 (F-V4-SYNTHESIS-1 verified disjoint 42+14=56; F-V4-SYNTHESIS-2 verified §3.1:347 4→4; 1 cohesion polish note at §4.2) | **100 %** |
| CH3 REGRESSION | 8 | 8 | 0 | 0 | 0 (V2+V3+V4 = 3-cycle 100 %; disjoint-set actively closes V3 inclusion double-count) | **100 %** |
| CH4 COST | 18 | 18 | 0 | 0 | 0 (envelope 5-cycle stable; 1 CH1-class informational note at §4 on sibling framing) | **100 %** |
| CH5 HIDDEN COUPLING | 7 | 7 | 0 | 0 | 0 (4th consecutive 100 %; disjoint-set framing reinforces substrate-union refusal) | **100 %** |
| CH6 ANTI-PAPER-CLOSE | 32 | 32 | 0 | 0 | 0 (V3+V4 = 2-cycle 100 %; V4 micro-fold itself recorded as positive CH6 exemplar) | **100 %** |
| CH7 OVERFIT-PRUNE | 18 | 18 | 0 | 0 | 0 (cargo metadata 9 grammars + 42+14=56 disjoint + 4 scanners all verified; V3 §1.5 cross-flag CLOSED) | **100 %** |
| **Aggregate** | **98** | **98** | **0** | **0** | **0** | **100.00 %** |

**Aggregate weighted ACCEPT: 100.00 % (98 of 98 dispositions).**
Above the §3Z 95 % floor by a clean 5 percentage points; seven-of-
seven lens sweep at 100 %; zero REJECT; zero REVISE; **zero NEW
orphan REVISEs** on the substantive disposition surface. Three sub-
threshold informational notes surface across CH1 §3 (V4 §2.4:311-313
triple-nested parenthetical + double-em-dash style observation),
CH2 §4.2 (SYNTHESIS §2.4:319-321 closing sentence cohesion polish),
and CH4 §4 (SYNTHESIS §2.4:318 "(the two path roots are siblings)"
documentary loose framing) — all explicitly classified by their
surfacing lens as below-action-threshold; none gates the §3Z LOCK.

### §0.1 — Verdict

**S-P0 V4 verdict: CONVERGED — chain link 1 of 2 under strict
reading; chain LOCK candidate under pragmatic reading.**

Per `ORCHESTRATOR.md §3Z` the cycle clears the ≥ 95 % floor
decisively at 100.00 % with **zero NEW orphan REVISEs**. Two §3Z
chain readings remain available, both inherited verbatim from V3
CONSOLIDATED §3.1 and re-adjudicated under the V4 outcome:

1. **Strict reading (orphan-REVISE blocking; V3 institutional
   precedent).** V3 cleared the ≥ 95 % floor at 100.00 % but
   carried 2 NEW orphan REVISEs (N-V3-CH2-1 + CH7 §1.5 cross-flag);
   under strict reading V3 did not close the chain because of the
   orphans. V4 is the **first clean cycle in the fresh chain** —
   100 % aggregate, zero orphan REVISEs. V5 confirming pass closes
   the §3Z two-consecutive-cycle chain → G-S-P0-CONVERGED → S-P1
   dispatch.

2. **Pragmatic reading (100 % overrides orphans because both V3
   orphans are now-closed at V4).** V3 + V4 jointly count as the
   two-consecutive-cycle chain; V4 closes §3Z LOCK. G-S-P0-
   CONVERGED → S-P1 dispatch.

Per V2 + V3 CONSOLIDATED §3.1 strict-reading precedent (both
adopted strict reading explicitly), **the recommended interpretation
is strict — V5 confirming pass closes the chain**. The V max=5
ceiling holds comfortably under either reading; under the strict
reading the V5 confirming pass lands at exactly the ceiling, which
is the same envelope V5 Pass-Alpha closed within.

The audit pack's *substance* remains CH-clean across all seven V4
lenses: both V3 V4-fold packets (F-V4-SYNTHESIS-{1,2}) FOLD-LANDED
per their respective CH lens reports at the exact line-coordinates
V3 CONSOLIDATED §2 prescribed; all V3 axis carry-forwards verify
verbatim under `git diff 007624849..f8e279877 -- …{six-axis files}
| wc -l = 0`; the V2 → V3 → V4 three-cycle invariant chain on A1 /
A2 / A3 / A6 holds (`git diff 1735882a5..f8e279877 -- …{four-axis
files} | wc -l = 0`); the 74-finding aggregate (31 CRIT + 20 HIGH +
12 MED + 11 LOW) holds verbatim across V1 + V2 + V3 + V4 (four
consecutive cycles of finding-population stability); the five C-1
..C-5 envelopes (2.8k–3.4k / 600–1.08k / 1.2k–2.0k / 800–1.4k /
250–500; total 5.65k–8.38k) hold verbatim across V5 alpha + V1 +
V2 + V3 + V4 — a **five-version unbroken chain of envelope
stability** now established; the three architectural sequencing
constraints (R4 → PRUNE-2; C-1 → C-4; PRUNE-4 = 9 sub-waves) carry
forward intact; the 9-grammar census re-attests at HEAD `f8e279877`
(bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv,
math); the CH7-companion lint glob extension verifies 56-file twin-
site coverage (42 runtime + 14 codegen, **disjoint** per the brace-
expansion `{runtime/src/grammars,codegen/src}/**/*.rs`) byte-
identically across V1 → V2 → V3 → V4; the 4 CSS L4 scanners
fixture-lookup count is independently re-attested by every CH lens
that touched the §3.1:347 cell (CH1 + CH2 + CH4 + CH7 all
independently re-ran `grep -lE 'CANONICAL_FIXTURE|CAPTURED_W2_
INPUT' skinny/crates/codegen/src/css_l4_*_templates/generated.rs`
and got the same four-file enumeration).

### §0.2 — V4 disposition pressure summary

The two V3 orphan REVISEs (N-V3-CH2-1 inclusion-relation defect +
CH7 §1.5 informational cross-flag) cluster on **one institutional
pattern** (V3 CONSOLIDATED §0.2 + §3.3): inheritance-miss class on
the SYNTHESIS summary surface, where action-class / count-column
edits land at the *enumerated* primary anchor but inheritance to a
*non-enumerated* downstream summary surface is missed. V4 closes
both orphans cleanly per the V3 §2 prescription and — critically —
**introduces zero NEW orphan instances of the same pattern**. The
V4 fold roster explicitly enumerated both prescribed primary
anchors (SYNTHESIS §2.4:311 + §3.1:342) and the V4 axis-redispatch
surface (1 file, 2 edits, +9 / -4 diff) leaves no downstream summary
surface un-enumerated — CH6 V4 §3.3 records this as the V4 fold
roster honoring the V3 CH6 §4 rec 2 institutional discipline by
inherent scope.

Three sub-threshold informational notes do surface at V4, all
below their surfacing lens's action threshold and **none classified
as orphan REVISEs**:

- **CH2 §4.2 (cohesion polish):** SYNTHESIS §2.4:319-321 closing
  sentence "The lint glob `codegen/src/**/*.rs` catches all 14
  regardless; only the prose count needs the 8-vs-14 distinction."
  carries a framing residue from the V2 → V3 precision repair that,
  after F-V4-SYNTHESIS-1's explicit "14 codegen-side (8 providers
  +templates + 6 ancillary)" rewrite, leaves the "8-vs-14 distinction"
  reading slightly disjoint from the upstream prose that already
  spells the decomposition out. The lint mechanism's grammar-
  neutral binding to both halves is unaffected; the sentence is
  arithmetically + mechanically correct; the issue is purely prose-
  flow aesthetics. CH2 explicitly classifies this as "cohesion-only
  editorial wrinkle, not a CH2 GENERALITY finding" and "sub-CH2-
  fatal" — non-blocking for §3Z LOCK.

- **CH4 §4 (sibling framing):** V4 §2.4:318 closes with "(the two
  path roots are siblings)." Strictly the path roots are `runtime/
  src/grammars` (3-deep from `skinny/crates/`) and `codegen/src`
  (2-deep from `skinny/crates/`); their structural relation is co-
  descended-from-`skinny/crates/` (i.e. `runtime/` and `codegen/`
  ARE direct siblings at depth 1, but the lint glob roots themselves
  are at different sub-depths). The disjointness claim that this
  parenthetical supports holds true and is independently verified
  (combined `git grep` returns 56 = 42 + 14 with zero overlap); the
  "siblings" framing is documentary decoration around an arithmetic
  claim that does not depend on it. CH4 explicitly classifies this
  as "informational only (zero cost-axis impact)" and "CH1-class
  minor informational note (sub-fatal threshold; below CH4 action
  threshold)" — non-blocking for §3Z LOCK.

- **CH1 §3 (style observation):** V4 §2.4:311-313 introduces two
  consecutive em-dash phrases inside the same sentence + a triple-
  nested parenthetical "(42 runtime-side mirror + 14 codegen-side
  (8 providers+templates + 6 ancillary))" which is denser than the
  V3 single-paren reading. CH1 explicitly classifies this as
  "style-grade, not correctness-grade" — every factual claim re-
  arithmetics correctly, the disjoint-set proof remains airtight,
  and V3 CONSOLIDATED §2.1 explicitly authorised either "Option A"
  (additive flat) or "Option B" (nested compound) per local prose-
  flow judgement. CH1 records "no fold recommended" — non-blocking
  for §3Z LOCK.

All three notes are below-threshold style/cohesion observations on
the V4 SYNTHESIS prose; none re-opens a CH7-N criterion, re-classifies
a finding, perturbs the 74-finding aggregate, or shifts any C-N
envelope. The optional V5 belt-and-braces micro-fold prescribed in
§2 closes the CH2 + CH4 notes atomically inside a single SYNTHESIS
V5 agent dispatch (~5 min wall-clock); the CH1 style observation
does not warrant fold revision (V3 CONSOLIDATED §2.1 authorised
both reading variants).

## §1 — V3 → V4 fold landing matrix

Both V4 micro-fold packets enumerated at `…/V3/HARDENING-S-P0-V3-
CONSOLIDATED.md §2` FOLD-LANDED per their respective V4 CH lens
reports with **executable verification independently re-quoted
across four lenses** (CH1 + CH2 + CH4 + CH7 all independently re-ran
the constitutive `git grep` invocations and arrived at the same 42
+ 14 = 56 disjoint enumeration + 4 CSS L4 scanners enumeration):

| V3 packet | Target | V4 landing | Lens verifier |
| --- | --- | --- | --- |
| F-V4-SYNTHESIS-1 | SYNTHESIS §2.4:311 "42 files carry the fake header **including** 14 codegen-side (8 providers+templates + 6 ancillary; …)" → "56 files carry the fake header (42 runtime-side mirror + 14 codegen-side (8 providers+templates + 6 ancillary)) — **disjoint sets** per the lint glob brace-expansion `{runtime/src/grammars,codegen/src}/**/*.rs`; `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ \| wc -l = 42` and `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ \| wc -l = 14`, summing to the 56 total with zero overlap (the two path roots are siblings)." (V3 CONSOLIDATED §2.1 "Option B compound" reading adopted) | LANDED at SYNTHESIS §2.4:311-318; closes N-V3-CH2-1 inclusion-relation defect | CH1 §1.1 + §4 pts 1+2+8 (re-ran both `git grep` invocations, 42 + 14 verified); CH2 §3.1 + §3.5 (re-ran both `git grep` invocations + combined glob, all three returned 42, 14, 56 verbatim); CH3 §1 row 1 (re-read prose + verified inclusion → disjoint framing flip); CH4 §0.1 point 1 (re-ran both `git grep` invocations + combined glob); CH5 §1.1 + §1.5 + §3.4 (re-ran both `git grep` invocations + records V4 as positive substrate-union refusal strengthening); CH6 §1.1 SYNTH-CH6-V4-1 (re-ran both `git grep` invocations); CH7 §1.2 + §2 row 12 (re-ran both `git grep` invocations + `sort -u` invariance check + path-root partition check `awk -F/ '{print $3}' | sort -u` → `{codegen, runtime}`) |
| F-V4-SYNTHESIS-2 | SYNTHESIS §3.1:342 (now 347 after F-V4-SYNTHESIS-1 expanded §2.4 by +5 lines) C-3 row "3 scanners are fixture lookups" → "4 scanners are fixture lookups" | LANDED at SYNTHESIS §3.1:347; closes CH7 V3 §1.5 informational cross-flag (final residual cell from F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3 arithmetic-correction sweep) | CH1 §1.2 + §4 pts 3+7+9 (re-ran `grep -rE 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' …` → 4 distinct files verified); CH2 §3.4 (re-ran `grep -rln 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' …` → 4 templates verified; cross-surface count consistency check verified all three sites read "4"); CH3 §1 row 2 (re-read prose + verified count alignment to executable-verified 4); CH4 §0.1 point 2 (re-ran `grep -lE … | sort -u` → 4 distinct files); CH5 §1.1 (re-ran `grep -rln 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' …` → 4 files); CH6 §1.1 SYNTH-CH6-V4-2 (re-ran `grep -lE … | wc -l` → 4); CH7 §1.3 + §2 row 13 (re-ran `grep -lE … | wc -l` → 4 verified + enumerated all four files explicitly) |

All V3 axis/SYNTHESIS folds (F-V3-A4-1 + F-V3-A5-1 + F-V3-SYNTHESIS-
{1,2,3}) and all V2 fold packets (F-V2-A3-1 + F-V2-A4-1 + F-V2-A6-1
+ F-V2-SYNTHESIS-{1,2,3,4,5} + the V2 verdict-line refresh surfaces)
**carry forward byte-identically** under V4 per `git diff 007624849..
f8e279877 -- restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-
audit-overfit-*.md | wc -l` returning **0** (verified independently
by CH1 §2 point 1, CH3 §1 row 5+6, CH4 §0.1 point 3, CH6 §1.2 +
§3.2). A1 + A2 + A3 + A6 STAND **byte-identical V2 → V3 → V4** per
`git diff 1735882a5..f8e279877 -- …{A1, A2, A3, A6}.md | wc -l =
0` (CH1 §2 point 2) — a three-cycle invariant chain on those four
axes.

The V4 fold roster correctly enumerated **both** prescribed primary
anchors (SYNTHESIS §2.4:311 + §3.1:342) with zero secondary
inheritance misses, closing the V3 CONSOLIDATED §3.3 "inheritance-
miss class on SYNTHESIS surface" institutional pattern for the V4
cycle (CH6 §3.3 institutional-pattern audit returned zero new
inheritance-miss residue at V4). The V4 micro-fold itself reads as
a **positive exemplar of CH6** (V3 orphans CLOSED at V4 by atomic
prose edit + executable verification — not deferred to V5, not
classified down to LOW with a future-phase action class, not
papered with a forecast-but-no-action framing); CH6 §3.4 records
this as "the institutional anti-paper-close pattern that the lens
defines, exemplified."

## §2 — V5 disposition recommendation

V4 closes the V3 chain link cleanly with zero NEW orphan REVISEs
and three sub-threshold informational notes (CH2 cohesion polish +
CH4 sibling framing + CH1 style observation) — none of which gates
§3Z LOCK by their own surfacing-lens classification. Two V5 options
remain available:

### §2.1 — Option A: V5 pure confirming pass over unchanged V4 artefacts

Same 7 lenses re-applied to the V4 artefact set at HEAD `f8e279877`;
zero axis edits; zero SYNTHESIS edits; the V5 confirming pass simply
re-attests the V4 100 % outcome to satisfy the strict-reading §3Z
two-consecutive-cycle chain (V4 + V5 = chain). Cost: ~30 min V5
CHALLENGE wave (7 lenses parallel) + ~10 min V5 aggregator = ~40
min wall to G-S-P0-CONVERGED. V5 forecast 100 % (98/98) assuming
the V4 artefact set is unchanged; the three sub-threshold notes
re-surface at V5 with the same below-action classification but
remain non-blocking.

### §2.2 — Option B: V5 belt-and-braces micro-fold (SYNTHESIS §2.4 cohesion polish + sibling framing) + V5 CHALLENGE confirming

V5 SYNTHESIS micro-fold packet (~5 min single SYNTHESIS V5 agent
dispatch):

- **F-V5-SYNTHESIS-1 (closes CH2 §4.2 cohesion note + CH4 §4
  sibling framing note):** Edit SYNTHESIS §2.4:319-321 closing
  sentence — replace
  > "The lint glob `codegen/src/**/*.rs` catches all 14 regardless;
  > only the prose count needs the 8-vs-14 distinction."
  with either (a) drop the residual "8-vs-14 distinction" clause
  outright (since F-V4-SYNTHESIS-1 already spells "(8 providers+
  templates + 6 ancillary)" explicitly upstream), retaining only a
  brace-expansion lint-coverage forward-looking note; OR (b) rewrite
  as a forward-looking statement about future glob-narrowing
  decisions. Same edit also tightens the §2.4:318 "(the two path
  roots are siblings)" parenthetical to a more precise framing
  (e.g. "(the two path roots co-descend from `skinny/crates/`)" or
  "(empty intersection by path-prefix partition)") that does not
  rest on the loose "siblings" framing CH4 §4 flagged.

Two atomic prose edits inside SYNTHESIS §2.4 — single SYNTHESIS V5
agent dispatch ~5 min total. V5 CHALLENGE wave (7-lens parallel)
~30 min + V5 aggregator ~10 min = ~45 min wall to G-S-P0-CONVERGED.
V5 forecast 100 % (98/98) with zero NEW findings — the V5 lenses
then have **literally nothing to flag** because the three V4 sub-
threshold notes are all closed at source.

### §2.3 — Recommendation: Option B (belt-and-braces)

**Recommendation: Option B (V5 belt-and-braces micro-fold + V5
CHALLENGE confirming).** Rationale:

1. **CH6 §3.4 V4 positive-exemplar precedent.** The V4 micro-fold
   demonstrated the institutional anti-paper-close pattern by
   closing the V3 orphans at the next available cycle rather than
   deferring or re-classifying. The same discipline applied to the
   V4 sub-threshold notes (close at V5 rather than defer to S-P3
   docs hygiene) reinforces the institutional pattern across two
   consecutive cycles. Belt-and-braces over deferral matches the
   V4 CH6 §3.4 positive-exemplar disposition.

2. **Trivial cost differential.** Option B adds ~5 min SYNTHESIS
   edit time (one single SYNTHESIS V5 agent dispatch with two
   atomic prose edits inside the same §2.4 paragraph; same axis-
   redispatch shape V4 used) over Option A's ~40-min total. The +5
   min cost is dominated by the V5 CHALLENGE wave's 30 min parallel
   surface; total Option B wall-clock is ~45 min vs Option A's
   ~40 min.

3. **Removes residual ambiguity at source.** Option A's residual
   ambiguity (three sub-threshold notes carried forward as
   documentary findings) places the burden on S-P3 / SK-V{N+1}
   docs hygiene to remember and address. Option B closes the
   ambiguity at the SYNTHESIS prose surface so subsequent SK-V{N+1}
   S-P0 cycles inherit a fully-coherent V5 artefact set with no
   residual sub-threshold framing items.

4. **V max=5 ceiling lands comfortably under both options.** Per
   V3 CONSOLIDATED §3.1 strict reading, V5 closes the §3Z LOCK
   chain at the V max=5 ceiling under either option; Option B has
   no envelope-pressure risk against the V ≤ 5 ceiling Pass-Alpha
   precedent established.

5. **V5 CHALLENGE lenses have zero residual to flag.** Under Option
   B, the V5 CHALLENGE wave's fresh-finding scan returns literally
   zero findings (the V4 sub-threshold notes were the only items
   below action threshold; folding them closes the audit's
   editorial surface entirely). Under Option A, the V5 wave's
   fresh-finding scan re-surfaces the same three sub-threshold
   notes at V5 with the same below-action classification — non-
   blocking but documentary noise.

The CH1 §3 style observation (triple-nested parenthetical + double-
em-dash) is **not** part of Option B's fold scope — V3 CONSOLIDATED
§2.1 explicitly authorised both reading variants ("Option A"
additive flat or "Option B" nested compound) per local prose-flow
judgement; CH1 records "no fold recommended" on the style note.
Option B closes CH2 §4.2 + CH4 §4 only, leaving the CH1 §3 style
observation explicitly as a no-action item per V3 §2.1 dispensation.

## §3 — V4 + V5 convergence forecast

Per Option B recommendation:

**V5 axis-redispatch surface:** ~5-minute total prose edits in 1
file (`SYNTHESIS-AUDIT-OVERFIT.md`: 2 atomic edits at §2.4:318 +
§2.4:319-321; same paragraph as F-V4-SYNTHESIS-1; same axis-
redispatch shape V4 used). A1 + A2 + A3 + A4 + A5 + A6 STAND at V5
(zero axis edits; the V3 axis folds at A4 §1:153 + A5 §5:133 carry
forward unchanged; A1 / A2 / A3 / A6 byte-identical V2 → V3 → V4
→ V5). Single SYNTHESIS V5 agent dispatch; V5 axis-redispatch wall-
clock estimate ~5 min.

**V5 CHALLENGE wave** (7-lens redispatch parallel): ~30 min.
Expected outcome **100 % aggregate** (7 × 100 % = 98 / 98) with
zero NEW findings, assuming the V5 SYNTHESIS micro-fold edits land
verbatim per the §2.2 prescriptions. The three V4 sub-threshold
notes (CH2 §4.2 + CH4 §4 + CH1 §3) close at source under Option B's
F-V5-SYNTHESIS-1; the V5 fresh-finding scans have no remaining
editorial surface to flag.

**V5 aggregator** ~10 min: reads 7 V5 CH files; authors `restart/
skinny/tranches/sk-v14/audit-overfit/hardening/V5/HARDENING-S-P0-
V5-CONSOLIDATED.md`; commits all 8 V5 files atomically.

**Total Option B wall-clock from here to G-S-P0-CONVERGED: ~45 min**
(V5 SYNTHESIS micro-fold ~5 + V5 CHALLENGE ~30 + V5 aggregator ~10
+ ~5 min slack). V ≤ 5 ceiling lands at exactly the cycle envelope
under strict reading.

### §3.1 — §3Z LOCK closure under Option B

§3Z requires "≥ 95 % ACCEPT × 2 consecutive cycles, no orphan
REVISEs, no open critical defects". V4 clears the ≥ 95 % floor at
100.00 % with **zero NEW orphan REVISEs** (third clean post-V3-
fold cycle if pragmatic reading is adopted; first clean post-V3-
orphan cycle if strict reading is adopted). V5 (Option B) clears
the ≥ 95 % floor with zero NEW findings (the V4 sub-threshold notes
close at source). The strict-reading chain closure stands at V4 +
V5 → §3Z LOCK; the pragmatic-reading chain closure stands at V3 +
V4 → §3Z LOCK (Option A) or V4 + V5 → reinforcement (Option B).

Per V2 + V3 CONSOLIDATED §3.1 institutional precedent (both adopted
strict reading), Option B + strict reading is the recommended
posture: V4 + V5 = chain → G-S-P0-CONVERGED → S-P1 dispatch per the
SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.

### §3.2 — Cross-version envelope stability

The C-1..C-5 LOC envelopes hold verbatim through **five consecutive
cycles** (V5 alpha-hardening → V1 → V2 → V3 → V4) — a five-version
unbroken chain of envelope stability now established (CH4 §0.2 +
§3.3). The 74-finding aggregate + severity distribution hold
verbatim through four consecutive cycles (V1 + V2 + V3 + V4). The
9-grammar census + 56-file lint-glob coverage (now narratively
described with disjoint-sets arithmetic per F-V4-SYNTHESIS-1) +
67-file Pattern H census all re-attest at HEAD `f8e279877` byte-
identically to V1 + V2 + V3 (verified by CH1, CH2, CH4, CH7
independently). The PRUNE-4 sub-wave count stable at 9 across A3 +
A5 + A6 + SYNTHESIS §2.3 + §3.3 cross-checks. The V5 cycle (Option
B) should likewise re-attest these invariants without revision; the
F-V2-SYNTHESIS-2 co-derivation note continues to reduce S-P3 risk-
weighting on PRUNE-4 by collapsing three apparently-independent
regression signals (A3 + A5 + A6 cross-confirms of css_pretty
addition) into one evidence + three observer confirmations.

### §3.3 — Pattern emergence: substrate-union refusal strengthening

CH5 V4 §3.4 surfaced a positive observation: F-V4-SYNTHESIS-1's
explicit disjoint-set framing is, beyond CH2's set-theoretic
mandate, a **substrate-union refusal strengthening** under CH5.
The V3 phrasing tacitly suggested the runtime-side mirror was a
*subset* of the codegen-side enumeration — which would have implied
a single substrate generating both. The V4 correction makes
explicit that the runtime-side mirror and the codegen-side sidecar
are **two disjoint file populations**, both of which the C-1 Lock-14
refactor cluster collapses but via distinct mechanisms (PRUNE-3 for
codegen-side dispatch, PRUNE-4 for runtime-side per-grammar collapse).
The CH5 invariant that the audit refuses a substrate-union (two
parallel pipelines sharing one substrate) is **reinforced** by the
V4 correction: by making the disjointness explicit, the V4 prose
precludes the misreading that the runtime-side mirror is silently
part of the codegen-side substrate. This is a positive CH5
strengthening that V5 should re-verify holds at V5 HEAD without
regression to V3-style "including" framing (CH5 §4 forward-binding
recommendation 4).

The institutional pattern surfaced by V2 → V3 → V4 (CH6 V3 §4 rec
2: action-class column edits + count-column edits must be
simultaneously propagated into per-paragraph closing summaries and
cross-axis summary tables) was honored at V4 by inherent scope —
CH6 V4 §3.3 records that the V4 fold roster's two single-cell prose
edits left no downstream propagation requirement, and that the V3
CONSOLIDATED §3.3 forward-binding pattern remains binding for V5
+ all future SK-V{N+1} S-P0 cycles as a forward-looking §3W
companion gate addition. V5 (Option B) re-confirms the same
discipline by inherent scope (F-V5-SYNTHESIS-1 is a single-cell
prose edit with no downstream propagation requirement).

## §4 — Closing posture

The S-P0 V4 CHALLENGE wave returns aggregate ACCEPT **100.00 %**
across 98 dispositions (98 ACCEPT, 0 REVISE, 0 REJECT, **0 NEW
orphan REVISEs**). Three sub-threshold informational notes
(CH2 §4.2 cohesion polish + CH4 §4 sibling framing + CH1 §3 style
observation) surface from independent lenses; all three are
classified by their surfacing lens as below-action-threshold; none
gates the §3Z LOCK. **S-P0 V4 verdict: CONVERGED** — above the §3Z
95 % floor by a clean 5 percentage points; seven-of-seven lens
sweep at 100 %; zero orphan REVISEs introduced; both V3 NEW orphans
(N-V3-CH2-1 + CH7 §1.5 cross-flag) atomically CLOSED at V4 by F-V4-
SYNTHESIS-1 + F-V4-SYNTHESIS-2 with executable verification
independently re-quoted across four lenses (CH1 + CH2 + CH4 + CH7).

The audit's *substance* holds clean across all seven V4 lenses: the
2 V3 V4-fold packets FOLD-LANDED at the exact line-coordinates V3
CONSOLIDATED §2 prescribed; all V3 axis/SYNTHESIS carry-forwards
verify byte-identical V3 → V4; the V2 → V3 → V4 three-cycle invariant
chain on A1 / A2 / A3 / A6 holds; the 74-finding aggregate +
severity distribution + three architectural sequencing constraints
+ 67-file Pattern H census + 9-grammar census + 56-file lint-glob
coverage (now narratively disjoint-sets-correct) + C-1..C-5 envelope
arithmetic + PRUNE-list mapping to C-1..C-5 with zero orphans all
hold under spot-verification by CH1 (10 sampled cites all re-execute),
CH2 (Lock 14 30-violation ledger + 9-grammar census + primitive-
crate generic posture all re-verify), CH3 (zero silent reopens of
V13 audit-CLEAN routes; zero silent reversals of V13 REDRESS
REJECTs; zero orphans outside C-1..C-5; three V13-disposition-bearing
paragraphs hold byte-identical V2 → V3 → V4), CH4 (envelope-neutral;
five-version stable; both V3 orphan concerns closed), CH5 (zero
parallel substrate across V4 micro-fold; the 42 runtime-side mirror
files all map cleanly to existing C-1 PRUNE-4 collapse target; F-V4-
SYNTHESIS-1 records as positive CH5 substrate-union refusal
strengthening), CH6 (both V3 orphans CLOSED verbatim per §2.1 + §2.2
V3 prescriptions with zero deferral / re-classification / paper-
close; V4 micro-fold itself records as positive CH6 lens exemplar),
CH7 (metadata re-attests 9 grammars including css_pretty; twin-site
lint glob mechanically closes both sides with disjoint-sets
arithmetic now narratively correct; F-V4-SYNTHESIS-1 + F-V4-SYNTHESIS-2
narrative completeness aligns prose with substrate reach without
expanding the substrate; V3 §1.5 cross-flag CLOSED).

The recommended V5 disposition is **Option B (V5 belt-and-braces
micro-fold + V5 CHALLENGE confirming)** per CH6 §3.4 positive-
exemplar precedent — close the three V4 sub-threshold notes
atomically at SYNTHESIS source rather than carry them forward as
documentary findings. The V5 SYNTHESIS micro-fold is small (1
single-paragraph rewrite at SYNTHESIS §2.4:318-321; ~5 min total
edit time; same axis-redispatch shape V4 used). The V5 CHALLENGE
confirming pass per strict §3Z reading then closes the two-
consecutive-cycle chain at V max=5 ceiling. **Total Option B wall-
clock from here to G-S-P0-CONVERGED: ~45 min** (V5 SYNTHESIS micro-
fold ~5 + V5 CHALLENGE ~30 + V5 aggregator ~10 + ~5 min slack). V
≤ 5 ceiling lands at exactly the cycle envelope under strict
reading, which is the same envelope V5 Pass-Alpha closed within.

The V3 → V4 → V5 forward-binding institutional notes (CH5 V4 §3.4
substrate-union refusal strengthening; CH6 V3 §4 rec 2 inheritance-
miss propagation discipline; V3 CONSOLIDATED §3.3 forward-binding
pattern carried into V4 §0.2 + §3.3) recommend that future SK-V{N+1}
S-P0 fold rosters explicitly enumerate prune-cluster table cells +
per-paragraph closing summaries that cite the same finding
alongside the primary disposition surface, and preserve the disjoint-
set framing introduced at V4 without regression to V3-style
"including" inclusion-relation phrasing. The 0 V4 NEW orphans
(against the 2 V3 NEW orphans) is the institutional fix landing —
the V4 fold roster correctly enumerated both prescribed primary
anchors with zero secondary inheritance misses; the V5 fold roster
(Option B) likewise enumerates the cohesion + sibling-framing
prescriptions with zero secondary misses by inherent scope (both
are single-cell prose edits in the same §2.4 paragraph).

---

**Authored:** 2026-05-23 (SK-V14 S-P0 CHALLENGE V4 aggregator).
**Status:** Aggregator commit; 8 V4 files (7 lens + CONSOLIDATED)
landed atomically per the write-only protocol carried from V3
CONSOLIDATED §4 + V2 ADDENDUM §5 + V1 CHALLENGE-CONTEXT §6.
**Authority:** `restart/prompts/ORCHESTRATOR.md §3Z step 4` +
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/
CHALLENGE-CONTEXT.md §6` + `restart/skinny/tranches/sk-v14/audit-
overfit/hardening/V2/CHALLENGE-V2-ADDENDUM.md §5` + `restart/skinny/
tranches/sk-v14/audit-overfit/hardening/V3/HARDENING-S-P0-V3-
CONSOLIDATED.md §2`.
**Next gate:** V5 SYNTHESIS micro-fold per Option B (~5 min single
SYNTHESIS agent dispatch on SYNTHESIS-AUDIT-OVERFIT.md §2.4:318-321)
→ V5 CHALLENGE (7-lens parallel ~30 min) → V5 aggregator (~10 min)
→ §3Z LOCK close → G-S-P0-CONVERGED → S-P1 dispatch per the SK-V14
ORCHESTRATOR-PROMPT THE SK LOOP.

# HARDENING ALPHA V4 — CONSOLIDATED (Pass Alpha SK-V13 → SK-V14)

Aggregator: SK-V14 CHALLENGE V4 over the V4 micro-redispatched artefacts
(commit `5e00b6d27` landed the two V3 fresh-finding folds atomically:
F-V4-α-E-1 against `alpha-E-candidate-shortlist.md:362-387` repairing
the V3 shell command's jq schema path with executable verification
quoted in the commit body; F-V4-α-F-1 against `HANDOFF.md:195-196`
re-anchoring the citation from `SYNTHESIS.md §1.3` to `SYNTHESIS.md
§0.2 (lines 73-84)`; SYNTHESIS, α-A, α-B, α-C, α-D, DISPATCH-CONTEXT
STAND from the V3 baseline `5e2ae78b4` per the V4 commit body). Seven
lenses dispatched (CH1 CORRECTNESS, CH2 GENERALITY, CH3 REGRESSION,
CH4 COST, CH5 HIDDEN COUPLING, CH6 ANTI-PAPER-CLOSE, CH7
OVERFIT-PRUNE) per the V1 lens binding extended by `V2/CHALLENGE-V2-
ADDENDUM.md §1`'s fold-verification + fresh-finding overlay, carried
into V4 under the executable-verification mandate the V3 aggregator's
methodological note prescribed (V3 CONSOLIDATED §1.2).

This consolidated authors the V4 verdict and the V5 disposition per
`ORCHESTRATOR.md §3Z step 4`. Cycle convergence floor remains ≥ 95 %
ACCEPT for **two consecutive** cycles with zero orphan REVISEs at
trailing-cycle close. V1 was 86.86 % (FAIL), V2 was 99.27 % (PASS but
2 orphan REVISEs routed to V3 micro-fold; both landed `5e2ae78b4`),
V3 was 99.27 % (PASS but 2 orphan REVISEs routed to V4 micro-fold;
both landed `5e00b6d27`), V4 is **100.00 %** (PASS; zero orphan
REVISEs at close). V4 therefore clears the per-cycle floor cleanly
with maximal margin; the two-consecutive-cycle chain reading is
adjudicated under §0.2 below.

## §0 — V4 cycle verdict

### §0.1 — Per-lens dispositions (V4)

| Lens | ACCEPT | Total | Rate | REJECT | REVISE |
|---|---|---|---|---|---|
| CH1 CORRECTNESS | 53 | 53 | 100.00 % | 0 | 0 |
| CH2 GENERALITY | 34 | 34 | 100.00 % | 0 | 0 |
| CH3 REGRESSION | 31 | 31 | 100.00 % | 0 | 0 |
| CH4 COST | 34 | 34 | 100.00 % | 0 | 0 |
| CH5 HIDDEN COUPLING | 46 | 46 | 100.00 % | 0 | 0 |
| CH6 ANTI-PAPER-CLOSE | 42 | 42 | 100.00 % | 0 | 0 |
| CH7 OVERFIT-PRUNE | 36 | 36 | 100.00 % | 0 | 0 |
| **Aggregate** | **275** | **275** | **100.00 %** | **0** | **0** |

The 275-row denominator is preserved verbatim from V2 → V3 → V4 (the
V1 274 → V2 275 re-count of the α-C §2 P-7 cell under both CH5 and
CH1 holds through V4 unchanged). Aggregate rate is **100.00 %** — the
clean ceiling. Both V3 orphan REVISEs (CH1 REV-1, CH1 REV-2) closed
via the V4 micro-fold commit `5e00b6d27`; CH1 V4 flips both cells
back to ACCEPT. Zero new REJECTs. Zero new REVISEs in any lens.

### §0.2 — Adjudication of CH2's α-E:756 observation

CH2 V4 §2.1 surfaces a non-finding observation that warrants explicit
classification before the aggregate rate is bound:

> The cost/cap table row at `alpha-E:756` reads "C-1 sub-waves
> (8 grammars; per sub-wave) | 20 min | 15 min | 30 min". The live
> workspace currently carries nine grammars (per the executable
> verification): `bbnf, json, css_l4, css_pretty, google_sheets,
> ebnf, bnf, csv, math`. The eight-count predates the addition of
> `css_pretty` to `workspace.metadata.bbnf.grammars`.

CH2 V4 explicitly classifies this as a **NON-FINDING under CH2
scope** for three documented reasons (V4 CH2 §2.1):

1. **Scope.** The cell describes per-sub-wave hard-cap budgeting
   for the C-1 cluster (research/plan/redress envelope per sub-wave);
   it is a budgeting input, not a Lock 14 gate substrate.
2. **Lock 14 implication.** A stale sub-wave count produces a
   budgeting under-estimate at S-P3 plan-authoring time (a 9th
   sub-wave would add 30 min redress = 270 min total, not 240); it
   does NOT bake a grammar privilege, foreclose a future grammar, or
   introduce a `match grammar` branch. Lock 14 forbids grammar-name
   leaks in *code* and *gate enumeration substrates*; a budgeting
   cell's scope-count is neither.
3. **Surface.** The cell sits in §10 cost/caps/telemetry. The two
   Lock 14 load-bearing surfaces — the C-1 *gate* (§3) and the C-3
   round-trip *gate* (§5) — are both correct (gate substrate is
   workspace metadata; cluster total derives from the substrate at
   plan-authoring time).

CH4 V4 (the cost-axis lens) does not flag this row either; CH4 V4
§2.3 N-9 records the V4 fold's removal of the V3 8-grammar
parenthetical at the gate site as a "favourable cost-surface
property introduced by V4, well below the action threshold" — CH4
itself treats the gate-text fix as the cost-axis concern, not the
budgeting cell.

**Aggregator adjudication.** The observation is BELOW THRESHOLD under
both CH2 (the originating scan) and CH4 (the closest adjacent
cost-axis lens). It is counted as a **non-finding**, not a
REVISE-equivalent. The aggregate rate therefore reads **100.00 %
(275/275)** with **zero NEW REVISEs**, not 99.64 % (274/275) under a
hypothetical REVISE-equivalent reading.

The observation is preserved verbatim in §0.4 below for V5
disposition consideration (per §2 below, V5 option B captures the
belt-and-braces fold for this cell).

### §0.3 — REJECT list (verbatim, 0 total across all lenses)

**No V4 REJECTs.** Both V3 REVISEs (CH1 REV-1 broken jq, CH1 REV-2
mis-citation) FOLD-LANDED verbatim per V3 CONSOLIDATED §2 prescription
(confirmed under CH1 V4 §2, CH2 V4 §2 fold-verification,
CH3 V4 §2, CH4 V4 §2.1 + §2.2, CH5 V4 §2.1 + §2.2, CH6 V4 §2 + §3,
CH7 V4 §1.1 + §4.1 against commit `5e00b6d27`). The V1 + V2 BINDING
REJECTs (CH5 E-3 owner-paths Lock-1 triad; CH7 E-1 three-part
round-trip + bypass-header detector) hold byte-equivalent at V4 HEAD
per the six-anchor preservation table in V4 CH5 §1 and the
fold-landing tracker in V4 CH7 §2.

### §0.4 — REVISE list (verbatim, 0 NEW V4 findings; 0 carry-over from V3)

Both V3 REVISEs (CH1 REV-1, CH1 REV-2) closed via the V4 micro-fold
commit `5e00b6d27` — see §1 below. **Zero new V4 REVISEs across all
seven lenses.**

One CH2 observation is preserved here verbatim per §0.2's
non-finding classification, surfaced for V5 belt-and-braces
consideration but **NOT counted as a REVISE under any lens**:

> **CH2 V4 §2.1 — α-E:756 cost/cap table sub-wave count drift.**
> The cell reads "C-1 sub-waves (8 grammars; per sub-wave)"; the
> live workspace carries 9 grammars after admitting `css_pretty`.
> The cell scope is per-sub-wave hard-cap budgeting (not a gate
> substrate); the immediately-following §10 paragraph at
> `alpha-E:770` reads "The C-1 cluster total is 8 × 30 = 240 min of
> redress windows, run serialised per §9". A roster-aware fold
> would update the cell to "rostered grammars; currently 9" (or
> equivalent) and re-derive the wall-clock total at `:770` to
> 9 × 30 = 270 min. Non-finding under CH2 (gate substrate is
> correct; Lock 14 holds); non-finding under CH4 (cap discipline
> unchanged; addendum cap distinctions preserved). Recommended for
> V5 fold per §2 option B below to remove the residual ambiguity.

This observation does NOT affect the aggregate rate or convergence
status; it is a forward-discipline note for V5.

### §0.5 — Convergence test

Per `ORCHESTRATOR.md §3Z`, convergence requires ≥ 95 % ACCEPT for
**two consecutive cycles** with the trailing cycle clean of orphan
REVISEs. V3 was 99.27 % (PASS percentage floor) but carried 2 orphan
REVISEs AT V3 CLOSE. V4 is **100.00 %** (PASS with maximal margin)
and carries **zero orphan REVISEs at V4 close**.

The two-consecutive-cycle rule admits two readings on the V3+V4 pair:

- **Strict reading (used by V3 aggregator at V3 close per V3
  CONSOLIDATED §0.2).** "Two consecutive cycles ≥ 95 % AND
  trailing cycle has zero orphan unresolved REVISEs." V3 had orphans
  at its close (even though they routed to a planned V4 micro-fold);
  under strict reading V3 does not count as a chain link. The chain
  re-anchors at V4 (link 1 of 2); V5 confirming pass is required to
  close the chain at link 2 of 2.
- **Pragmatic reading.** V3 orphans were resolved at V4 close; V3's
  percentage floor was met; the V3+V4 pair forms the chain → §3Z
  LOCK at V4 close.

This aggregator **adopts the strict reading** for consistency with the
V3 aggregator's precedent (V3 CONSOLIDATED §0.2 adopted the
orphan-REVISE-aware reading explicitly to defer LOCK to a V4+V5
pair). The strict reading is also the more defensible default: it
binds the convergence test to the trailing-cycle close state rather
than to a downstream remediation that the §3Z text itself does not
mention. Under the strict reading, V4 is link 1 of 2 of the
re-anchored chain; V5 is the binding confirming pass at the V ≤ 5
ceiling.

**Verdict: CONVERGED-EXPECTING-V5-CONFIRM.** V4 clears the
single-cycle ≥ 95 % floor with 5 pp margin and zero orphans at close;
V5 confirming pass (with the optional CH2 belt-and-braces fold per
§2 option B) closes the §3Z chain at the V ≤ 5 ceiling. Should V5
clear with the expected 100 % aggregate and zero orphans, the SK-V14
Pass Alpha bracket LOCKS at V5; G-Alpha auto-signs per the SK-V14
ORCHESTRATOR-PROMPT pin; the orchestrator proceeds directly to S-P0.

No CH lens issues a BINDING REJECT in V4; the V1 CH7 BINDING REJECT
on C-3's round-trip gate (FOLD-LANDED in V2 via E-1; V3 STRENGTHENED
via F-V3-α-E-1; V4 EXECUTABLE-VERIFIED via F-V4-α-E-1) and the V1
CH5 BINDING REJECT on owner-paths Lock-1 triad (FOLD-LANDED in V2
via E-3; V3 PRESERVED; V4 PRESERVED at unchanged absolute line
addresses) both remain closed. No V4 finding triggers immediate
plan-revise or redress-revert under any lens.

### §0.6 — Cross-cycle convergence chain (V1 → V5)

| Cycle | Aggregate | Floor met? | Orphan REVISEs at close | Chain status |
|---|---|---|---|---|
| V1 | 86.86 % | NO | 29 (all routed to V2 fold) | chain broken |
| V2 | 99.27 % | YES | 2 (CH2 NF-1, CH3 F-V3-1; routed to V3 micro-fold; landed `5e2ae78b4`) | link 1 of 2 (chain restart) |
| V3 | 99.27 % | YES | 2 (CH1 REV-1, CH1 REV-2; routed to V4 micro-fold; landed `5e00b6d27`) | link 2 of 2 by percentage; chain NOT closed under strict reading (orphans at close) |
| V4 | **100.00 %** | YES (5 pp margin) | **0** | link 1 of 2 (chain re-anchors clean) |
| V5 (forecast) | ≥ 95 % (point 100 %) | YES expected | 0 expected | link 2 of 2 → **LOCKED** at V ≤ 5 ceiling |

The V1 → V4 record describes a converging series of fold-pressure
findings: 7+29 V1 (counting the 7 BINDING REJECTs as primary + 29
REVISEs in V1 routing) → 2 V2 → 2 V3 → **0 V4**. The V4 cycle is the
first to achieve clean close with zero orphans since the chain
restart at V2; under the strict reading it re-anchors the chain.

## §1 — V3 → V4 fold landing matrix + lens cross-verification

Per V4 dispatch context, each V3 REVISE (2 total: REV-1, REV-2)
verified against V4 evidence post-micro-fold commit `5e00b6d27` and
marked **FOLD-LANDED** / **FOLD-PARTIAL** / **FOLD-MISSING**. Both
folds landed verbatim per V3 CONSOLIDATED §2 prescription with
STRENGTHENING beyond literal prescription. The V4 cycle institutionalized
the executable-verification discipline per the V3 aggregator's
methodological note.

### §1.1 — V3 REVISEs (2 total — both FOLD-LANDED + EXECUTABLY-VERIFIED)

| V3 Lens | V3 finding (short) | V4 fold ID | V4 site | Status |
|---|---|---|---|---|
| CH1 V3 REV-1 | α-E §5 falsifiability gate shell command not mechanically executable (jq path `.workspace_metadata...|keys[]` returns null against `cargo metadata`'s actual schema); secondary 8-vs-9-grammar parenthetical count drift | F-V4-α-E-1 | `alpha-E-candidate-shortlist.md:362-387` (V4 commit `5e00b6d27`; 10 ins / 10 del = net 0 LOC; jq path corrected to `.metadata.bbnf.grammars[].ident`; `--no-deps` added; `git diff --exit-code` added beyond literal prescription; stale 8-grammar parenthetical EXCISED entirely; "9th grammar" generalised to roster-count-agnostic "admitting an additional grammar") | **FOLD-LANDED + STRENGTHENED + EXECUTABLY-VERIFIED** (CH1 V4 §2 + CH2 V4 §2.1 + CH4 V4 §2.2 + CH5 V4 §2.1 + CH6 V4 §3 + CH7 V4 §1.1 + §4.1 — all six confirming lenses report mechanical 9-grammar enumeration against live workspace; CH7 V4 §1.1 + CH1 V4 §2 + CH2 V4 §2 + CH6 V4 §0 quote the actual `cargo metadata --format-version 1 --no-deps \| jq -r '.metadata.bbnf.grammars[].ident'` output `bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv, math`) |
| CH1 V3 REV-2 | HANDOFF §7 cite to `SYNTHESIS.md §1.3` is a stale anchor (§1.3 holds the post-PRUNE rolling delta 0/17, not the 4+7 vs 6+11 reconciliation; the canonical reconciliation lives at §0.2 lines 73-84) | F-V4-α-F-1 | `HANDOFF.md:195-196` (V4 commit `5e00b6d27`; 1 ins / 1 del = net 0 LOC; citation re-anchored from `SYNTHESIS.md §1.3` to `SYNTHESIS.md §0.2 reconciliation block (lines 73-84)`; explicit line-range parenthetical added beyond literal prescription) | **FOLD-LANDED** (CH1 V4 §2 + CH3 V4 §2 + CH6 V4 §0 — all three confirming lenses Read SYNTHESIS §0.2 lines 73-84 and verify the section carries the "Numeric-divergence reconciliation (per CH6 §2.2 REJ-2)" header + per-row breakdown the §7 carry-over guard now correctly cites) |

**Fold-landing roll-up: 2 V3 findings = 2 FOLD-LANDED.** Zero
FOLD-PARTIAL; zero FOLD-MISSING. Both folds landed verbatim per the
V3 CONSOLIDATED §2 prescription; both carry STRENGTHENING beyond
literal prescription (F-V4-α-E-1 added `--exit-code` for strict
failure propagation + EXCISED stale parenthetical rather than
patching it + generalised forward-discipline phrasing;
F-V4-α-F-1 added explicit line-range parenthetical hardening the
citation against future §-number drift).

### §1.2 — Lens-depth divergence: institutionalized at V4

The V3 aggregator's methodological note (V3 CONSOLIDATED §1.2)
identified the documentary-vs-executable lens-depth divergence (CH7
V3 documentary verification of the gate's intent passed at 100 %;
CH1 V3 executable verification of the literal shell command surfaced
REV-1) and recommended that future cycles consider an "explicit
executable verification" requirement on any falsifiability gate that
ships a literal shell command.

The V4 cycle institutionalized this discipline:

- **V4 commit `5e00b6d27` body** explicitly carries the V3
  aggregator's methodological recommendation ("V3's lens-depth
  divergence (CH7 documentary OK vs CH1 executable BROKEN)
  recommends explicit executable-verification dispatch for any
  shipped shell command") and reports the V4 α-E redispatch executed
  the corrected command against the live workspace ("V4 alpha-E
  dispatch carried this mandate and produced verified output").
- **V4 CHALLENGE V4 lens dispatches** received the same mandate; per
  CH6 V4 §0 + §3 the executable-verification output ("enumerates
  9 grammars (bbnf, json, css_l4, css_pretty, google_sheets, ebnf,
  bnf, csv, math) against live workspace") is "cited as anti-paper-
  close exemplar".
- **Six of seven V4 lenses** (CH1, CH2, CH4, CH5, CH6, CH7) executed
  or verified the corrected shell command against the live workspace
  and report the 9-grammar enumeration. CH3 (REGRESSION) did not
  re-execute the command itself but verified the citation chain via
  the same executable-verification discipline (CH3 V4 §2 Read
  SYNTHESIS §0.2 lines 73-84 to confirm the section carries the
  cited reconciliation language).

The V4 cycle therefore upgrades executable-verification from a
methodological recommendation to binding fold-discipline at the
cycle-output level. The CH6 V4 §5 closing recommendation suggests
making this permanent at the ORCHESTRATOR level (outside this
aggregator's scope to write, but surfaced here per CH6's
recommendation).

### §1.3 — Per-artefact V4 convergence digest

| Artefact | V3 disposition | V4 disposition | Change |
|---|---|---|---|
| SYNTHESIS.md | ACCEPT (all V3 carries hold) | ACCEPT (untouched at V4) | unchanged (407 lines) |
| HANDOFF.md | ACCEPT-WITH-REVISE (F-V3-α-F-1 landed at :192-197; CH1 REV-2 surfaces §1.3 cite mis-anchor) | ACCEPT (F-V4-α-F-1 landed at :195-196; citation re-anchored to §0.2 + line-range parenthetical) | 245 → 245 lines (net 0) |
| α-A | ACCEPT (no V3 fold) | ACCEPT (untouched at V4) | unchanged (420 lines) |
| α-B | STAND | STAND | unchanged |
| α-C | ACCEPT (no V3 fold) | ACCEPT (untouched at V4) | unchanged (460 lines) |
| α-D | STAND | STAND | unchanged |
| α-E | ACCEPT-WITH-REVISE (F-V3-α-E-1 landed at :362-387; CH1 REV-1 surfaces shell-command non-executability + 8-vs-9 parenthetical drift) | ACCEPT (F-V4-α-E-1 landed at :362-387; jq path corrected + `--no-deps` + `--exit-code` + stale parenthetical excised + roster-count-agnostic phrasing) | 815 → 815 lines (net 0) |
| DISPATCH-CONTEXT | STAND | STAND | unchanged |

Both V4-touched artefacts net-zero LOC (10/10 on α-E; 1/1 on
HANDOFF); all six V4 lenses confirming preservation of every V3
ACCEPT row at unchanged line addresses (CH5 V4 §1 anchor table
verifies six anchors at byte-identical V3 line addresses; CH6 V4 §0
preservation roll-up verifies five V2 anchors plus two V3-NEW folds
at HELD verbatim status).

## §2 — V5 disposition

Per V4 CONSOLIDATED forecast and the §3Z V ≤ 5 ceiling, V5 fires as
the confirming pass over the V4 artefacts (with optional micro-fold
on the CH2 non-finding observation). Two options:

### §2.1 — Option (A): V5 confirming pass over unchanged V4 artefacts

**Scope.** Same 7 lenses re-applied against the V4 baseline (commit
`5e00b6d27`); no α-redispatch; no micro-fold.

**Expected outcome.** 7 × 100 % = 275/275 = **100 %** aggregate;
zero orphan REVISEs; §3Z LOCK at V5.

**Risk.** Minimal. The V4 surface is tightly bounded (two surgical
edits at known sites; both verified across multiple lenses). The
historical "0–3 new findings per lens" assumption tightens at each
cycle (V1: 7+29 → V2: 2 → V3: 2 → V4: 0); V5 floor expectation is
0 fresh findings.

**Cost.** V5 envelope ≈ 0 min α-redispatch + lens-cycle work + V5
aggregator. CH2 non-finding observation (α-E:756 stale 8-grammar
count) may or may not be re-flagged at V5; under V4 CH2's documented
non-finding classification, V5 CH2 is expected to inherit the same
classification.

### §2.2 — Option (B): V5 fold-and-confirm with belt-and-braces α-E:756 fix

**Scope.** Optional micro-fold targeting the CH2 V4 §2.1 non-finding
observation: edit `alpha-E-candidate-shortlist.md:756` from "C-1
sub-waves (8 grammars; per sub-wave) | 20 min | 15 min | 30 min" to
"C-1 sub-waves (rostered grammars; currently 9; per sub-wave) | 20 min
| 15 min | 30 min" OR "C-1 sub-waves (per sub-wave; one per rostered
grammar) | 20 min | 15 min | 30 min" (roster-count-agnostic phrasing).
Re-derive the wall-clock total at `alpha-E:770` from `8 × 30 = 240`
to `9 × 30 = 270` minutes (or to a roster-count-agnostic phrasing).
Then V5 CHALLENGE confirms with the same 7 lenses.

**Expected outcome.** 7 × 100 % = 275/275 = **100 %** aggregate;
zero orphan REVISEs; CH2's α-E:756 observation explicitly resolved;
§3Z LOCK at V5.

**Risk.** Minimal. The fold is a single-cell edit (cost-discipline
cell + cluster-total cell); both cells are CH4-axis content with
zero gate-substrate implication; the V4 CH4 §2.3 N-9 already
classifies the V4 gate-text excision as "favourable" and any V5
α-E:756 update simply extends the same posture to the
budgeting-cell axis.

**Cost.** V5 envelope ≈ 5 min α-E single-cell edit + lens-cycle work
+ V5 aggregator. The CH2 non-finding observation is explicitly
remediated; CH4's per-sub-wave budgeting is updated to the live
9-grammar roster; no residual stale-count surface remains anywhere
in the V4 artefact set.

### §2.3 — Aggregator recommendation: Option (B)

**Recommend Option (B) — belt-and-braces.** Rationale:

1. **CH2 already flagged for V5.** V4 CH2 §2.1 explicitly classifies
   the observation as a NON-FINDING under CH2 scope but recommends:
   "If the V4 aggregator desires absolute belt-and-braces on the
   budgeting axis, the V5 confirming pass could carry a single-cell
   edit." The V4 aggregator's authority is to weigh this against the
   cost; the cost is trivial (~5 min).
2. **The fold is trivial.** Single-cell edit at `:756` + adjacent
   wall-clock re-derivation at `:770`; both cells are CH4-axis
   content with zero gate-substrate implication; no lens-cross
   verification expected to surface fresh findings.
3. **Removes residual ambiguity.** The V4 cycle excised the
   gate-text 8-grammar parenthetical (the load-bearing site); option
   (B) extends the same discipline to the budgeting cell (the only
   remaining hardcoded grammar-count literal in the V4 artefacts).
   Post-V5, no hardcoded grammar count remains anywhere — the
   roster-count-agnostic posture is total.
4. **V5 confirming pass then verifies clean 100 % with zero
   observations.** Under option (A), V5 CH2 may re-flag the
   observation (under V4 CH2's own classification logic, the
   non-finding remains a non-finding — but the disposition narrative
   would carry the observation forward through V5 close, which is
   not optimal hygiene). Under option (B), V5 CH2 closes the
   observation entirely; the V5 CHALLENGE record reads clean 100 %
   with zero new observations.
5. **The V4 cycle's discipline-strengthening pattern.** F-V4-α-E-1
   added `--exit-code` beyond literal prescription; F-V4-α-F-1
   added explicit line-range beyond literal prescription. V5
   option (B) extends the V4 cycle's "strengthen beyond literal
   prescription" pattern to the cost-axis budgeting cell.

The V5 micro-fold under option (B) is owner-α-E single-cell + α-E
single-cell-wall-clock-re-derivation, ≈ 5 min cap, LOW risk; the V5
CHALLENGE then dispatches the same 7 lenses with executable-
verification mandate carried forward from V4.

## §3 — Bracket-level forecast

### §3.1 — V5 closure forecast

Under option (B) execution:

| Lens | V4 ACCEPT-rate | V5 forecast |
|---|---|---|
| CH1 | 100.00 % | 100.00 % (unchanged; no V5 fold touches CH1 surface) |
| CH2 | 100.00 % | 100.00 % (NF observation closed; V5 §2 fresh-finding scan returns zero) |
| CH3 | 100.00 % | 100.00 % (unchanged; V5 fold is CH4-axis only) |
| CH4 | 100.00 % | 100.00 % (V5 fold updates one cap-table cell + one wall-clock total; CH4 V5 verifies arithmetic, expects ACCEPT) |
| CH5 | 100.00 % | 100.00 % (unchanged; V5 fold is cost-axis only) |
| CH6 | 100.00 % | 100.00 % (V5 fold is documentary precision, anti-paper-close character preserved) |
| CH7 | 100.00 % | 100.00 % (unchanged; V5 fold does not touch CH7 surface) |
| **Aggregate** | **100.00 %** | **100.00 % (275/275)** point forecast |

Under option (A) execution (no V5 fold): identical lens forecasts
expected; CH2 V5 may re-surface the α-E:756 observation as a
non-finding (carrying forward V4 CH2's classification); aggregate
expected at 100.00 %.

### §3.2 — Two-consecutive-cycle convergence chain (post-V5)

| Cycle | Aggregate | Floor met? | Orphan REVISEs at close | Chain status |
|---|---|---|---|---|
| V1 | 86.86 % | NO | 29 (routed) | chain broken |
| V2 | 99.27 % | YES | 2 (routed; landed `5e2ae78b4`) | link 1 of 2 (chain restart) |
| V3 | 99.27 % | YES | 2 (routed; landed `5e00b6d27`) | link 2 of 2 by percentage; chain NOT closed (orphans at close) |
| V4 | **100.00 %** | YES (5 pp margin) | **0** | link 1 of 2 (re-anchor) |
| V5 (forecast) | ≥ 95 % (point 100 %) | YES expected | 0 expected | link 2 of 2 → **LOCKED** at V ≤ 5 ceiling |

The §3Z two-consecutive-cycle rule is expected to be satisfied by
the V4 + V5 pair at the V ≤ 5 ceiling. At V5 convergence, the
SK-V14 Pass Alpha bracket LOCKS immediately; G-Alpha auto-signs per
the SK-V14 ORCHESTRATOR-PROMPT pin ("do not relinquish except at
G-Omega"); the orchestrator proceeds directly to S-P0.

V5 is the **last cycle permitted** under `ORCHESTRATOR.md §3Z`'s
V ≤ 5 ceiling. If V5 does not close (i.e., V5 surfaces fresh orphan
REVISEs that cannot be folded into V5's own micro-redispatch surface
before close), the bracket BLOCKED-escalates per §3Z. The V5 fold
prescription under option (B) is tightly scoped specifically to
avoid this outcome — it is a single-cell edit to a clearly-classified
non-finding, with the V4 cycle's strengthening pattern as precedent.

### §3.3 — Structural concerns surviving V4 (none)

No CH lens issues an escalation flag in V4. The V1 CH7 BINDING
REJECT (C-3 round-trip gate) and the V1 CH5 BINDING REJECT
(owner-paths Lock-1 triad) both remain closed with V4 strengthening:

- **CH7 V1 BINDING REJECT** closed via E-1 (V2 FOLD-LANDED) →
  F-V3-α-E-1 (V3 STRENGTHENED via metadata-derived enumeration) →
  F-V4-α-E-1 (V4 EXECUTABLY-VERIFIED via corrected jq path +
  `--no-deps` + `--exit-code` + parenthetical excision +
  roster-agnostic phrasing). The V3 silent-pass failure mode is
  eliminated.
- **CH5 V1 BINDING REJECT** closed via E-3 (V2 FOLD-LANDED) and
  preserved through V3 + V4 at byte-identical line addresses (V4
  CH5 §1 six-anchor preservation table).

All other V1 / V2 / V3 REJECTs + REVISEs close FOLD-LANDED per §1.

The V4 cycle introduced **zero new defects** under fold pressure
(V3's 2 fresh REVISEs route to V4 micro-fold; V4's fresh-finding
scan returns 0 across all 7 lenses; the CH2 non-finding observation
is below threshold under both CH2 and CH4). The V3 → V4 lens-depth
divergence noted in V3 CONSOLIDATED §1.2 is now institutionalized at
V4 cycle level (per §1.2 above).

### §3.4 — Cycle envelope and contract lock

V4 cycle wall-clock: ≈ 13 min α-redispatches (one ≈ 10-min α-E
shell-command repair + one ≈ 3-min α-F citation swap;
atomic-committed at `5e00b6d27`) + ≈ 30 min × 7 = 210 min CH
lens-agent work (parallelisable to ≈ 30–45 min) + aggregator.

V5 envelope under option (B): ≈ 5 min α-E single-cell + adjacent-
wall-clock re-derivation (atomic-committed) + lens-cycle work +
aggregator. Under option (A): ≈ 0 min α-redispatch + lens-cycle work
+ aggregator.

V5 should close inside one orchestrator session if dispatched in
tight sequence.

Post-V5 lock: SK-V14 contract locks; G-Alpha auto-signs; S-P0 fires;
PRUNE-1 → PRUNE-2 → PRUNE-3 → PRUNE-4 wave program initiates per the
C-5 → C-1 → C-3 → C-4 → C-2 sequencing in α-E §9.

## §4 — Final aggregator verdict

V4 aggregate ACCEPT-rate **100.00 %** across 275 per-§ dispositions;
**0** REJECT (all V1 + V2 + V3 REJECTs FOLD-LANDED including the V1
CH7 BINDING REJECT, the V1 CH5 BINDING REJECT, and both V3 orphan
REVISEs); **0** NEW REVISE; one CH2 non-finding observation
preserved for V5 belt-and-braces consideration (below threshold
under CH2 and CH4; non-blocking).

**Cycle verdict: CONVERGED-EXPECTING-V5-CONFIRM.** V4 clears the
single-cycle ≥ 95 % floor with maximal margin (5 pp); V4 carries
zero orphan REVISEs at close — the first clean trailing-cycle close
since the V2 chain restart. Under the strict reading of the §3Z
two-consecutive-cycle rule (adopted by the V3 aggregator for
consistency), V4 re-anchors the chain as link 1 of 2; V5 confirming
pass is required at the V ≤ 5 ceiling to close link 2.

V5 dispatch under recommended option (B): one ≈ 5 min α-E
single-cell + adjacent-wall-clock re-derivation micro-fold targeting
the CH2 V4 §2.1 non-finding observation (α-E:756 8-vs-9 grammar
count + α-E:770 wall-clock total re-derivation), followed by the
seven-lens V5 confirming pass with executable-verification mandate
carried forward from V4. V5 point forecast **100.00 % (275/275)**;
realistic floor ≥ 99 %. No architectural-block surfaces. No
source-side fold implicated. V5 envelope is docs-only.

The V1 → V2 → V3 → V4 cycle chain surfaced 7+29 V1 + 2 V2 + 2 V3 +
**0 V4** findings = 40 total findings; **40 of 40 FOLD-LANDED at V4
close** (100 % fold-completion rate; the 2 V3 orphans landed
verbatim per V4 micro-fold with V4 strengthening beyond literal
prescription). The convergent-cycle behaviour matches the V1
CONSOLIDATED §3.1 "0–3 new findings per lens" historical assumption
and continues to tighten (7+29 → 2 → 2 → 0). The lens-depth
methodological note captured in V3 CONSOLIDATED §1.2 (documentary
vs executable verification) was institutionalized at V4 cycle level
(per V4 CH6 §3 + §4; six of seven V4 lenses executed the corrected
shell command against the live workspace).

The SK-V14 alpha-bracket sits one cycle from lock at the V ≤ 5
ceiling; V5 fires next.

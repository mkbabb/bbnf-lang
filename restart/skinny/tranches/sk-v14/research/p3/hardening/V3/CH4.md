# SK-V14 S-P3 V3 CHALLENGE CH4 — Cost Lens (LOCK-Trigger Cycle)

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V3 (LOCK-trigger; second
consecutive cohort-wide ≥95% per `ORCHESTRATOR.md §3Z`). Lens: CH4
COST. Date: 2026-05-23. HEAD: 867b0cd0bbfb6ac1920335b41ddaf21d0fda6b5e.
Lens scope: every wave carries (LOC budget + hard cap + phase
breakdown research/plan/redress per SKINNY-TRIUMVIRATE.md +
same-wave-consumer per primitive); wave count ≤ 12 (skinny-bracket
ceiling per ORCHESTRATOR.md §3Z); shortlist ≤ 8; CF-3 3-gate
admission cell wired per candidate; W6 9-sub-wave folding (PRUNE-4)
carries cumulative cap with restatement at the sub-wave manifest
header (F-V2-CH4-1 footnote preserved verbatim at V3 HEAD).
Authority: `restart/skinny/tranches/sk-v14/research/p3/hardening/V3/
CHALLENGE-CONTEXT.md`; `PASS-3-SYNTHESIS-PLAN.md §3` CH4;
`SKINNY-TRIUMVIRATE.md §7 §8 §9`; `ORCHESTRATOR.md §3Z`;
`restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH4.md` (V2
100% ACCEPT-bearing; 2-cycle LOCK already eligible at V2 close per
V1 100% + V2 100% trajectory).
Discipline: write-only; no git add/commit; aggregator commits 8
hardening files atomically.
HARD CAP: 20 min (LOCK-trigger; reduced cap per V3 CHALLENGE-CONTEXT
§3).

## §1 — V3 disposition focus (from V3 CHALLENGE-CONTEXT §2)

Per `restart/skinny/tranches/sk-v14/research/p3/hardening/V3/
CHALLENGE-CONTEXT.md:28`:

> CH4 COST: V3 cosmetic cost-neutral per CH4 V3+V4 cite-rebind
> cost-neutrality discipline (now extending to mirror-refresh);
> 3-cycle LOCK extension.

Per T-P1 CH4 5-class taxonomy (cite-rebind / cite-cosmetic /
REJECT-label-refinement / anti-paper-close-paragraph-insertion /
anchor-refresh), V3 extends the taxonomy with a **6th class:
mirror-refresh** (P3-C internal text now mirrors SPEC §13:982
authoritative UNCONDITIONAL Stage-0 binding; no new LOC, no new
binding, no wave reshape, no shortlist reshape — purely textual
consistency between an artefact and an already-LOCKED authority).

V3 net edit (per `git show 867b0cd0b -- p3c-falsifiability-gates.md`):
- P3-C `:36` (§1.2 W10 manifest row): conditional Stage-0 phrasing
  ("Stage-0 F-V2-P1ABC-RERECORD if any consumer-dependency primitive
  admitted") → **UNCONDITIONAL** ("Stage-0 F-V2-P1ABC-RERECORD
  UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding)").
- P3-C `:423` (§2.10 W10 exit-gate item 8): conditional Stage-0
  phrasing ("If admitting any of the 12 F-V2-P1ABC-RERECORD
  consumer-dependency primitives, Stage 0 rerun is shipped") →
  **UNCONDITIONAL** ("Stage-0 F-V2-P1ABC-RERECORD shipped
  UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding ...) ...
  BEFORE any parse_only admit lands").

Both V3 edits are **in-place textual refinements** (1 line each;
P3-C line count = 537 unchanged from V2 close), mirror SPEC §13:982
authoritative binding, and introduce ZERO new LOC, ZERO new wave,
ZERO new candidate, ZERO new gate cell, ZERO new cap.

The V2 cycle was 100% ACCEPT-bearing (7/7). V3 must verify (a) the V3
cosmetic mirror-refresh edits are cost-neutral, (b) no V3 edit
re-opened any V2 ACCEPT clause, (c) all 7 V2-LOCKED artefacts (P3-A,
P3-B, P3-D, P3-E, P3-F, SPEC, DISPATCH-PROMPT) are byte-untouched at
V3 HEAD, (d) the 2-cycle LOCK trajectory established at V2 close
**extends to 3-cycle LOCK** at V3 close per `ORCHESTRATOR.md §3Z`
(≥95% × 2 consecutive cycles = LOCK; V3 100% extends LOCK by a third
cycle within the V≤5 ceiling with margin).

## §2 — Deliverable (per-clause CH4 disposition at V3 HEAD)

### §2.1 — V3 cosmetic mirror-refresh cost-neutrality — VERDICT: ACCEPT

**Verification.** V3 cycle's single SPEC/artefact edit (per
`git show 867b0cd0b --stat`) modifies ONLY `p3c-falsifiability-
gates.md` at two byte-identical line counts: `:36` and `:423`. The
diff (4 lines per `git show --stat`: +2 / −2) is in-place text
substitution with NO line addition; P3-C total line count holds at
537 from V2 close to V3 HEAD.

Executable verification (re-run at HEAD `867b0cd0b`):

```
wc -l restart/skinny/tranches/sk-v14/research/p3/
     p3c-falsifiability-gates.md
→ 537 lines (V2 close = 537; delta = 0)
```

The CH4 lens binding is over wave count, LOC budget, hard cap, phase
breakdown, shortlist size, CF-3 admission cell wiring, and W6
sub-wave aggregate cap. NONE of these surfaces sees any V3 edit:

- Wave count: SPEC §2 manifest is V2-LOCKED (per V3 CHALLENGE-
  CONTEXT §1 bullet "(V2-LOCKED)"); V3 makes no SPEC edit.
- LOC budget: V3 makes no SPEC edit; per-wave LOC budgets at
  `SPEC.md:237-248` unchanged.
- Hard cap: V3 makes no SPEC edit; per-wave caps at `SPEC.md:237-248`
  + phase-cap table at `SPEC.md:263-273` unchanged.
- Phase breakdown: V3 makes no SPEC edit; SKINNY-TRIUMVIRATE §7
  reproduction at `SPEC.md:263-273` unchanged.
- Same-wave consumer: V3 makes no SPEC edit; 12 "Same-wave consumer:"
  lines (W0..W11) at `SPEC.md:363, 439, 498, 550, 608, 667, 755, 820,
  882, 942, 1002, 1055` unchanged (verified `grep -c
  "Same-wave consumer:" SPEC.md` = 12 at V3 HEAD).
- Shortlist size: V3 makes no P3-A edit (P3-A is V2-LOCKED); 8
  candidates at `p3a:171-178` unchanged.
- CF-3 3-gate cell: V3 makes no P3-A edit; 8/8 3-gate cells preserved.
- W6 sub-wave aggregate cap: V3 makes no SPEC edit; F-V2-CH4-1
  footnote at `SPEC.md:713` unchanged (verified `grep -c "810 min"
  SPEC.md` = 2 at V3 HEAD, one at line 243 + one at line 713).

The V3 cosmetic mirror-refresh class is the textbook cost-neutral
fold: NO surface that CH4 binds sees any V3 edit, by construction.

**Verdict: ACCEPT.** V3 cosmetic mirror-refresh on P3-C `:36 + :423`
is cost-neutral; the only CH4-adjacent surface touched is P3-C's
§1.2 W10 manifest row + §2.10 W10 exit-gate item 8, both of which
already carried wave + LOC + cap framing in V2 ACCEPT cells (§2.6 V2
ACCEPT) and now carry a single-phrase mirror to SPEC §13:982 without
LOC delta. Mirror-refresh is the 6th class in the CH4 cost-neutrality
taxonomy (V1+V2 anchor-refresh; V3 mirror-refresh).

### §2.2 — Wave count = 12 preserved at V3 — VERDICT: ACCEPT

**Verification.** SPEC §2 wave-manifest rows at `SPEC.md:237-248`
enumerate W0, W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, W11 — count
= 12. SPEC is V2-LOCKED (per V3 CHALLENGE-CONTEXT §1 bullet "(V2-
LOCKED)"); no V3 SPEC edit.

Executable verification at V3 HEAD `867b0cd0b`:

```
sed -n '237,248p' restart/skinny/tranches/sk-v14/SPEC.md
  | grep -cE '^\| W[0-9]'
→ 12 (verbatim ceiling)
```

P3-C `:36` mirror-refresh is the W10 manifest row entry inside P3-C
(not SPEC); the row identity (W10) is unchanged; the row count in
P3-C §1.2 wave table is unchanged. No new top-level wave introduced
in P3-C, SPEC, or any V2-LOCKED artefact.

**Verdict: ACCEPT.** Wave count = 12 verbatim at the
`ORCHESTRATOR.md §3Z` + `SKINNY-TRIUMVIRATE.md §3` ceiling; V3 mirror-
refresh introduces zero waves; 12-wave ceiling preserved across the
V1 → V2 → V3 trajectory.

### §2.3 — Shortlist = 8 preserved at V3 — VERDICT: ACCEPT

**Verification.** P3-A §2.1 candidate-shortlist table at `restart/
skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:171-178`
enumerates 8 candidates (C1..C8 per V2 §2.3 table verbatim). P3-A is
V2-LOCKED per V3 CHALLENGE-CONTEXT §1 bullet "(V2-LOCKED)"; no V3
P3-A edit.

Executable verification at V3 HEAD `867b0cd0b`:

```
sed -n '171,178p' restart/skinny/tranches/sk-v14/research/p3/
  p3a-candidate-shortlist.md | grep -cE '^\| C[1-9] '
→ 8 (verbatim ceiling)
```

V2's F-V2-CH2-1 (C3 same-wave consumer naming) + F-V2-CH2-2 (C4
same-shape consumer naming) within-row refinements preserved
verbatim at V3 HEAD; the V3 cycle adds NO shortlist edit.

**Verdict: ACCEPT.** Shortlist = 8 verbatim at the `PASS-3-SYNTHESIS-
PLAN.md §2` ≤8 ceiling; V3 mirror-refresh introduces zero candidates;
shortlist ceiling preserved across the V1 → V2 → V3 trajectory.

### §2.4 — F-V2-CH4-1 §9 W6 810-min cumulative cap footnote preserved at V3 — VERDICT: ACCEPT

**Verification.** SPEC §9 W6 sub-wave manifest at `restart/skinny/
tranches/sk-v14/SPEC.md:713` carries the V2-folded footnote verbatim
at V3 HEAD (SPEC is V2-LOCKED; no V3 SPEC edit):

> **Cap footnote (per §2 manifest restated for dispatch-time
> clarity):** Each W6.N sub-wave carries the ≤90-min implementation/
> redress cap; the W6 aggregate cumulative cap across W6.1..W6.9 is
> ≤810 min per `SPEC.md:243`. Any sub-wave or aggregate overflow
> returns REVISE per `[generated-size-budget]`.

Executable verification at V3 HEAD `867b0cd0b`:

```
grep -c "810 min" restart/skinny/tranches/sk-v14/SPEC.md
→ 2 (line 243 manifest cell + line 713 footnote; V2 close = 2 =
   V3 HEAD = 2; delta = 0)
```

V2's §2.1 ACCEPT verdict (footnote landed verbatim above the
W6.1..W6.9 sub-wave manifest table at `SPEC.md:715-725`; cites
`SPEC.md:243` for the §2 manifest authority + binds overflow to
`[generated-size-budget]` per memory feedback) is preserved unchanged
at V3 HEAD.

**Verdict: ACCEPT.** F-V2-CH4-1 footnote preserved verbatim at V3
HEAD; the dispatch-time clarity REVISE from V1 → V2 → V3 stays
discharged; W6 cumulative-cap discipline holds at both §2 manifest
(line 243) and §9 sub-wave manifest header (line 713).

### §2.5 — W11 close ceremony + W9 W1-only dependency + W9 34-row budget — all V2 Special-V2-attention dispositions preserved at V3 — VERDICT: ACCEPT

**Verification.** The three V2 Special-V2-attention dispositions
(§2.4 W11 ceremony qualitatively-different gate category; §2.5 W9
34-row admit budget under 90-min cap; §2.6 W9 W1-only dependency)
are preserved at V3 HEAD by V2-LOCK of SPEC + P3-B + P3-C structural
surfaces:

| V2 § | V2 disposition | V3 HEAD status |
|---|---|---|
| §2.4 | W11 ceremony at `SPEC.md:248` reads "0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only; ≤90 min" | PRESERVED (SPEC V2-LOCKED; no V3 edit) |
| §2.5 | W9 fused 34-row admit budget at `p3c:347-396` (17 direct + 17 typed under ≤450 LOC + ≤90 min cap) | PRESERVED (W9 row at `p3c:35` carries "17 + 17 (per corpus)" verbatim; W9 §2.9 body unchanged at V3 HEAD; the V3 mirror-refresh edits at `p3c:36 + p3c:423` are W10-scoped, not W9-scoped) |
| §2.6 | W9 W1-only dependency at `SPEC.md:246` reads "Conditional on W1 close (depends only on R1+R2, not on PRUNE waves)" | PRESERVED (SPEC V2-LOCKED; no V3 edit) |

Executable verification at V3 HEAD `867b0cd0b`:

- W11 ceremony row: SPEC.md:248 → unchanged (SPEC byte-identical
  V2→V3 per V2-LOCK).
- W9 row in P3-C §1.2 wave table: `p3c:35` reads "W9 | R7-direct
  fused R7-typed | re-admit each JSON `direct_to_struct` row vs
  sonic-rs strict per-corpus struct deser + each JSON
  `real_typed_struct` row vs per-corpus typed struct deser | R7
  (direct + typed) | 17 + 17 (per corpus)" verbatim — V3 mirror-
  refresh did NOT touch this row (V3 touched only W10 row at line 36
  + W10 exit-gate item 8 at line 423).
- W9 W1-only dependency: SPEC.md:246 → unchanged (SPEC byte-identical
  V2→V3 per V2-LOCK).

**Verdict: ACCEPT.** All three V2 Special-V2-attention CH4
accommodations (W11 ceremony, W9 34-row budget, W9 W1-only) preserved
verbatim at V3 HEAD; V3 cosmetic mirror-refresh is W10-scoped (P3-C
:36 + :423) and does NOT touch W9 or W11 surfaces.

### §2.6 — Carry-forward verification: V2's 7/7 ACCEPT clauses preserved at V3 — VERDICT: ACCEPT

**Verification.** V2 CH4 (`restart/skinny/tranches/sk-v14/research/
p3/hardening/V2/CH4.md`) closed 7/7 ACCEPT-bearing with the V2-cycle
fold F-V2-CH4-1 landing the V1 single-line clarity REVISE. V3 must
verify no V3 edit re-opened any V2 ACCEPT clause:

| V2 § | V2 clause | V3 HEAD status |
|---|---|---|
| §2.1 | F-V2-CH4-1 §9 W6 810-min cumulative cap footnote landed | ACCEPT (SPEC V2-LOCKED; footnote preserved verbatim at `SPEC.md:713`; verified §2.4 above) |
| §2.2 | Wave count = 12 preserved at V2 | ACCEPT (SPEC V2-LOCKED; manifest unchanged; verified §2.2 above) |
| §2.3 | Shortlist = 8 preserved at V2 | ACCEPT (P3-A V2-LOCKED; shortlist unchanged; verified §2.3 above) |
| §2.4 | W11 close ceremony qualitatively-different gate category | ACCEPT (SPEC V2-LOCKED; W11 row unchanged; verified §2.5 above) |
| §2.5 | W9 fused 34-row admit budget vs 90-min cap accommodation | ACCEPT (P3-C V3 mirror-refresh is W10-scoped; W9 §2.9 body unchanged; verified §2.5 above) |
| §2.6 | W9 W1-only dependency (parallel-eligibility with W2-W8) | ACCEPT (SPEC V2-LOCKED; W9 row unchanged; verified §2.5 above) |
| §2.7 | Carry-forward verification: V1's 8/8 ACCEPT clauses preserved at V2 | ACCEPT (V2 ACCEPT cells preserved; V3 mirror-refresh introduces zero CH4 regression; verified across §2.1-§2.5 above) |

All 7 V2 clauses preserved at V3 HEAD. No V3 edit re-opened any V2
disposition.

**V3 net delta (CH4-binding surfaces):**
- SPEC: V2-LOCKED; 0 V3 edits; 1187 lines unchanged.
- P3-A: V2-LOCKED; 0 V3 edits; 8 candidates unchanged.
- P3-B: V2-LOCKED; 0 V3 edits; 12-wave manifest with LOC + cap
  columns + same-wave consumers unchanged.
- P3-C: 537 → 537 lines (cosmetic in-place edits at :36 and :423,
  W10-scoped mirror-refresh to SPEC §13:982); zero LOC delta; zero
  gate-content inconsistency introduction.
- P3-D, P3-E, P3-F: V1/V2-LOCKED; 0 V3 edits.
- DISPATCH-PROMPT: V1-LOCKED; 0 V3 edits.

CH4 cost discipline is preserved across V3's mirror-refresh edit; the
V3 cycle is CH4-coherent.

**Verdict: ACCEPT.** All 7 V2 ACCEPT clauses preserved at V3; V3
cosmetic mirror-refresh introduces zero CH4 cost regression; no
carry-forward defect.

### §2.7 — CH4 cost-neutrality taxonomy extension to 6th class (mirror-refresh) — VERDICT: ACCEPT

**Verification.** Per V3 CHALLENGE-CONTEXT §2 CH4 binding:

> CH4 COST: V3 cosmetic cost-neutral per CH4 V3+V4 cite-rebind
> cost-neutrality discipline (now extending to mirror-refresh);
> 3-cycle LOCK extension.

The T-P1 CH4 5-class cosmetic-fold taxonomy enumerated in prior
cohort LOCK trajectories admits a 6th class at V3:

| # | Class | Lens evidence | Cost impact |
|---|---|---|---|
| 1 | cite-rebind | citation `path:line` re-pin to canonical authority after upstream edit | zero LOC delta; cite text refresh only |
| 2 | cite-cosmetic | citation punctuation/case/quote normalization | zero LOC delta; presentation refresh only |
| 3 | REJECT-label-refinement | REJECT-route label sharpening without route addition | zero LOC delta; label text refresh only |
| 4 | anti-paper-close-paragraph-insertion | one-paragraph close-honesty restatement | small fixed LOC delta (one paragraph); cap-bounded |
| 5 | anchor-refresh | section anchor / footnote pointer realignment after structural edit | zero LOC delta; pointer refresh only |
| **6** | **mirror-refresh** (NEW V3) | **artefact-internal text mirrors authoritative SPEC binding (no new binding introduced)** | **zero LOC delta; consistency refresh only** |

V3 P3-C :36 + :423 edits are the canonical mirror-refresh case:
- SPEC §13:982 carries the authoritative UNCONDITIONAL Stage-0
  binding (V2-LOCKED at V3 HEAD; no V3 SPEC edit).
- P3-C :36 + :423 previously carried CONDITIONAL Stage-0 phrasing
  (carried over from S-P2 V2 era prior to S-P2 V3 §6.3 unconditional
  promotion).
- V3 mirror-refresh updates P3-C to mirror SPEC §13:982 verbatim
  authority without introducing any new binding, wave, LOC, cap,
  candidate, or admission gate cell.

Mirror-refresh is cost-neutral by construction: the binding lives in
SPEC (V2-LOCKED); the mirror lives in P3-C (textual consistency);
the CH4 cost surfaces (wave count, LOC budget, hard cap, phase
breakdown, same-wave consumer, shortlist size, CF-3 admission cell,
W6 cumulative cap) are untouched.

**Verdict: ACCEPT.** Mirror-refresh extends the cosmetic-fold
taxonomy to 6 classes; V3 introduces the 6th class; cost-neutrality
preserved across the V1+V2 (classes 1-5) + V3 (class 6) cycle
boundary; the taxonomy is CH4-stable and cohort-LOCK-compatible.

## §3 — Falsifiability binding (executable verification commands at V3 HEAD)

Per `PASS-3-SYNTHESIS-PLAN.md §3` CH4 disposition vocabulary
(ACCEPT / REVISE / REJECT) + LAC-1E-12 executable-verification
procedural addendum. All commands re-executed at HEAD
`867b0cd0bbfb6ac1920335b41ddaf21d0fda6b5e`.

| Clause | Verification command | V3 HEAD output | V2 expected | Status |
|---|---|---:|---:|---|
| F-V2-CH4-1 footnote preserved | `grep -nE "Cap footnote" SPEC.md` returns line 713 | line 713 PRESENT | line 713 PRESENT | PRESERVED |
| 810-min cumulative cap | `grep -c "810 min" SPEC.md` | 2 | 2 | PASS |
| Wave count = 12 | manifest row count `^\| W[0-9]` at SPEC.md:237-248 | 12 | =12 | PASS |
| Shortlist = 8 | shortlist row count `^\| C[1-9] ` at p3a:171-178 | 8 | =8 | PASS |
| Same-wave consumer per wave | `grep -c "Same-wave consumer:" SPEC.md` | 12 | =12 | PASS |
| LOC budget per wave | all 12 manifest rows carry "Source/edit LOC budget" cell | 12 cells populated | ≥12 | PASS |
| Hard cap per wave | all 12 manifest rows carry "Implementation/redress cap" cell | 12 cells populated | ≥12 | PASS |
| Phase breakdown citation | `SPEC.md:263-273` reproduces SKINNY-TRIUMVIRATE §7 | reproduced verbatim | binding cite | PASS |
| W9 W1-only dependency | `SPEC.md:246` reads "Conditional on W1 close (depends only on R1+R2, not on PRUNE waves)" | verbatim PRESENT | verbatim PRESENT | INTENTIONAL |
| W11 ceremony cap | `SPEC.md:248` reads "0 source LOC; docs/.../SPEC reconciliation only" + "≤90 min" | verbatim PRESENT | verbatim PRESENT | INTENTIONAL |
| W9 34-row budget | `p3c:35` enumerates 17 direct + 17 typed rows (NB: line slid to :35 in V2 close P3-C; V3 mirror-refresh at :36 + :423 W10-scoped) | 17 + 17 enumerated | (Special V2) | ACCOMMODATED |
| P3-C V3 mirror-refresh — :36 W10 manifest | `sed -n '36p' p3c-falsifiability-gates.md` reads "Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding)" | verbatim PRESENT | NEW (V3) | LANDED |
| P3-C V3 mirror-refresh — :423 W10 exit-gate item 8 | `sed -n '423p' p3c-falsifiability-gates.md` reads "Stage-0 F-V2-P1ABC-RERECORD shipped UNCONDITIONALLY ... BEFORE any parse_only admit lands" | verbatim PRESENT | NEW (V3) | LANDED |
| P3-C line count delta V2 → V3 | `wc -l p3c-falsifiability-gates.md` V2 close = V3 HEAD | 537 = 537; delta = 0 | =537 | COST-NEUTRAL |
| V2-LOCKED artefacts untouched | `git diff 75657df14..867b0cd0b -- p3a-* p3b-* p3d-* p3e-* p3f-* SPEC.md DISPATCH-PROMPT.md` empty | empty diff | empty | LOCK-PRESERVED |

All commands re-executed at HEAD `867b0cd0b`. Every disposition above
carries the named verification command per LAC-1E-12 ("any cited
path:line in any wave's plan or redress MUST be re-executed at HEAD
before commit").

## §4 — Pre-blocked routes (CH4-specific, carry-forward from V1 §4 + V2 §4)

Per V1 §4 + V2 §4 CH4-binding pre-blocks + S-P2 V3 §6.1 CF-3
carry-forward, the following CH4 anti-patterns remain pre-blocked at
V3; no V3 edit may re-open any of these routes:

1. **Wave count inflation** — adding a 13th wave breaches
   `ORCHESTRATOR.md §3Z` ceiling. V3 HEAD: 12 manifest rows verbatim;
   no V3 edit inflated the count (verified §2.2; SPEC V2-LOCKED).

2. **Shortlist inflation** — adding a 9th candidate breaches
   `PASS-3-SYNTHESIS-PLAN.md §2` ≤8 cap. V3 HEAD: 8 candidates
   verbatim; no V3 edit inflated the shortlist (verified §2.3; P3-A
   V2-LOCKED).

3. **Missing same-wave-consumer** — any wave whose redress phase
   admits a primitive without naming its hot-path consumer re-opens
   the SK-V5 orphan-kernel failure shape. V3 HEAD: 12/12 "Same-wave
   consumer:" lines preserved in SPEC §3-§14 (verified §2.1; SPEC
   V2-LOCKED).

4. **Missing 3-gate CH4 admission cell** — per S-P2 V3 §6.1 CF-3
   binding, every shortlisted candidate's admission manifest carries
   (scalar-ref status / checkasm-parity expectation / same-wave-
   consumer NAMED). V3 HEAD: 8/8 candidates carry 3-gate cell at
   `p3a:171-178` (verified §2.3; P3-A V2-LOCKED).

5. **W6 sub-wave dispatch without cumulative-cap awareness** — if a
   single W6.N sub-wave consumes >90 min, the per-sub-wave cap binds
   split-before-dispatch per `p3b:82` + `SPEC.md:243` + the V2-folded
   footnote at `SPEC.md:713`. The 810-min aggregate is NOT retry room
   (verified §2.4; SPEC V2-LOCKED).

6. **LOC budget overflow without REVISE** — per `SPEC.md:255-256`:
   "A wave plan that exceeds either its LOC budget or the 90-minute
   implementation / redress cap must split before dispatch or return
   REVISE." Silent overflow is REJECT (preserved at V3; SPEC V2-
   LOCKED).

7. **Phase-role merger** — per `SKINNY-TRIUMVIRATE.md §9` + SPEC §1
   non-negotiable at `SPEC.md:218`: research / plan / CHALLENGE /
   redress phases remain distinct commits. Merging plan + redress
   into one commit re-opens the SK-V5 failure shape per V3
   triumvirate-discipline feedback (preserved at V3; SPEC V2-LOCKED).

8. **V2 pre-block — W9 34-row admit budget collapse into primitive-
   authoring**. Per `p3b:85` verbatim: "primitives drawn from S-P2
   LOCKED pool, never re-authored". Any W9 plan that re-authors a
   primitive (instead of consuming the S-P2 LOCKED pool) breaches
   the consumer-wiring-only discipline that accommodates the 34-row
   budget under the 90-min cap; REJECT route (preserved at V3; P3-B
   V2-LOCKED).

9. **V2 pre-block — W9 dispatch behind any PRUNE wave**. SPEC §2 W9
   row reads "Conditional on W1 close (depends only on R1+R2, not on
   PRUNE waves)"; the W1-only dependency is intentional per SPEC
   §0.1 R10. Adding a W2..W7 antecedent to W9 dispatch conditions
   breaches the partial-order conditional-dispatch chain (preserved
   at V3; SPEC V2-LOCKED).

10. **NEW V3 pre-block — mirror-refresh introducing a new binding**.
    Per V3 §2.7 mirror-refresh class definition: a mirror-refresh
    edit mirrors an EXISTING authoritative SPEC binding into a
    downstream artefact for textual consistency; it does NOT
    introduce a new binding, wave, LOC, cap, candidate, or admission
    gate cell. A future cycle that purports to be mirror-refresh
    while introducing a new binding is REJECT (taxonomy abuse). The
    V3 P3-C :36 + :423 edits are LAWFUL mirror-refresh because SPEC
    §13:982 already carries the UNCONDITIONAL binding at V2 close;
    P3-C consistency follows authority.

11. **NEW V3 pre-block — LOC-delta mirror-refresh**. A mirror-refresh
    edit by definition preserves line count (in-place text
    substitution only); any cycle that claims mirror-refresh while
    inflating an artefact's line count is REJECT (taxonomy abuse).
    The V3 P3-C edit is LAWFUL: V2 close = 537 lines; V3 HEAD = 537
    lines; delta = 0.

## §5 — Sources

### §5.1 — V3 CHALLENGE-CONTEXT authority

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V3/CHALLENGE-CONTEXT.md` (44 lines; §0 authority + §1 V3 artefact disposition + §2 V3 LOCK-trigger CH4 binding + §3 discipline + §4 output + §5 post-LOCK trajectory).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH4.md` (V2 100% ACCEPT-bearing; 7/7 ACCEPT preserved at V3 HEAD).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md` (V2 aggregator + V3 fold-packet authority + cohort LOCK trajectory binding per V3 CHALLENGE-CONTEXT §0 bullet 4).

### §5.2 — Contract authority

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` CH4 verbatim (ACCEPT / REVISE / REJECT vocabulary).
- `restart/prompts/ORCHESTRATOR.md §3Z` (cohort LOCK = ≥95% × 2 consecutive cycles; V≤5 ceiling).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md §3 §7 §8 §9` (12-wave ceiling; phase caps; same-wave-consumer rule; triumvirate role separation).

### §5.3 — P3 artefacts under V3 CH4 review

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:171-178, 207-214` (V2-LOCKED; 8 shortlist rows + 3-gate cells preserved verbatim at V3 HEAD).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:76-87` (V2-LOCKED; 12-wave manifest with LOC + cap columns + same-wave consumers preserved verbatim at V3 HEAD).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:82` (V2-LOCKED; W6 ≤810 min cumulative cap binding preserved verbatim at V3 HEAD).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:85` (V2-LOCKED; W9 W1-only dependency + ≤450 LOC + ≤90 min + S-P2 LOCKED pool consumer-wiring-only preserved verbatim at V3 HEAD).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:36` (V3 mirror-refresh: W10 manifest row Stage-0 binding mirrors SPEC §13:982 UNCONDITIONAL).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:423` (V3 mirror-refresh: W10 exit-gate item 8 Stage-0 binding mirrors SPEC §13:982 UNCONDITIONAL).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:347-396` (V2-LOCKED W9 §2.9 body unchanged at V3 HEAD; 34-row admit budget preserved).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:436` (V2-LOCKED W11 close ceremony preserved at V3 HEAD).

### §5.4 — SPEC under V3 CH4 review (V2-LOCKED; no V3 SPEC edit)

- `restart/skinny/tranches/sk-v14/SPEC.md:216` (V2-LOCKED; CF-3 3-gate non-negotiable).
- `restart/skinny/tranches/sk-v14/SPEC.md:218` (V2-LOCKED; triumvirate-role-separation non-negotiable).
- `restart/skinny/tranches/sk-v14/SPEC.md:237-248` (V2-LOCKED; 12-wave manifest with LOC budget + hard cap columns).
- `restart/skinny/tranches/sk-v14/SPEC.md:243` (V2-LOCKED; W6 ≤90/sub-wave + ≤810 aggregate cap).
- `restart/skinny/tranches/sk-v14/SPEC.md:246` (V2-LOCKED; W9 W1-only dependency).
- `restart/skinny/tranches/sk-v14/SPEC.md:248` (V2-LOCKED; W11 ceremony row).
- `restart/skinny/tranches/sk-v14/SPEC.md:252-256` (V2-LOCKED; generated-output exemption + overflow-split-or-REVISE).
- `restart/skinny/tranches/sk-v14/SPEC.md:258-261` (V2-LOCKED; aggregate envelope + 20%-overflow escalation per `[generated-size-budget]`).
- `restart/skinny/tranches/sk-v14/SPEC.md:263-273` (V2-LOCKED; phase-cap table verbatim from SKINNY-TRIUMVIRATE §7).
- `restart/skinny/tranches/sk-v14/SPEC.md:275-292` (V2-LOCKED; per-wave rerun ceilings).
- `restart/skinny/tranches/sk-v14/SPEC.md:363, 439, 498, 550, 608, 667, 755, 820, 882, 942, 1002, 1055` (V2-LOCKED; 12 Same-wave consumer lines W0..W11).
- `restart/skinny/tranches/sk-v14/SPEC.md:713` (V2-LOCKED; F-V2-CH4-1 W6 sub-wave footnote restating 810-min cumulative cap; preserved verbatim at V3 HEAD).
- `restart/skinny/tranches/sk-v14/SPEC.md:715-725` (V2-LOCKED; W6.1..W6.9 sub-wave manifest table).
- `restart/skinny/tranches/sk-v14/SPEC.md:919-944` (V2-LOCKED; W9 entry gate + tasks + exit gate + same-wave consumer).
- `restart/skinny/tranches/sk-v14/SPEC.md:982` (V2-LOCKED; W10 §13 UNCONDITIONAL Stage-0 binding — the authoritative source for V3 P3-C mirror-refresh).

### §5.5 — S-P2 carry-forward authority (CF-3 binding preserved at V3)

- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.1` (CF-3 3-gate admission cell binding).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.2` (NF-CH6-4 canonical-name binding for shortlist consolidation).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.3` (F-V2-P1ABC-RERECORD Stage-0 wave commitment — UNCONDITIONAL per S-P2 V3; the upstream authority for SPEC §13:982 + V3 P3-C mirror-refresh).

### §5.6 — Memory feedback honored

- `[no-deferrals]` — V3 mirror-refresh does not defer any binding; SPEC §13:982 UNCONDITIONAL Stage-0 binding is the in-pass authoritative commitment, mirrored verbatim into P3-C at V3.
- `[dispatch-hard-cap]` — V3 cycle carries reduced 20-min cap per LOCK-trigger discipline; CH4 lens enforces.
- `[triumvirate-discipline]` — research / plan / redress role separation preserved at V3; SPEC §1 non-negotiable at line 218 V2-LOCKED.
- `[generated-size-budget]` — F-V2-CH4-1 footnote at `SPEC.md:713` continues to bind W6 overflow to this feedback at V3 HEAD.
- `[execute-planned-architecture]` — V3 mirror-refresh executes the planned authoritative binding (SPEC §13:982 UNCONDITIONAL) into the downstream artefact P3-C; no retreat from authority.
- `[no-workarounds]` — V3 does not introduce any workaround; the mirror-refresh closes a textual inconsistency between P3-C and SPEC §13:982 that V2 had identified as a non-blocking residual; full migration to authoritative phrasing.
- `[doc-integration-style]` — V3 mirror-refresh is integrated as in-place phrase substitution at two existing rows (W10 manifest + W10 exit-gate item 8); not a bolted-on section.

### §5.7 — Cohort LOCK trajectory (per `ORCHESTRATOR.md §3Z`)

- V1 cycle: CH4 8/8 = 100% ACCEPT-bearing with one V2 clarity REVISE.
- V2 cycle: CH4 7/7 = 100% ACCEPT-bearing; F-V2-CH4-1 V2 fold landed; first ≥95% LOCK-eligible cycle.
- V3 cycle: CH4 7/7 = 100% ACCEPT-bearing; V3 cosmetic mirror-refresh cost-neutral; SECOND consecutive ≥95% LOCK-eligible cycle → COHORT §3Z LOCK TRIGGERS on V3 close per V3 CHALLENGE-CONTEXT §3 + ORCHESTRATOR §3Z.
- Trajectory monotonic: V1 ACCEPT → V2 ACCEPT (with V1 REVISE folded) → V3 ACCEPT (with V2 non-blocking residual mirror-refreshed). Three-cycle LOCK extension within V≤5 ceiling with 2-cycle margin.

---

## §6 — Lens disposition summary (V3 LOCK-trigger)

| § | Clause | V3 Verdict |
|---|---|---|
| §2.1 | V3 cosmetic mirror-refresh cost-neutrality | ACCEPT |
| §2.2 | Wave count = 12 preserved at V3 | ACCEPT |
| §2.3 | Shortlist = 8 preserved at V3 | ACCEPT |
| §2.4 | F-V2-CH4-1 §9 W6 810-min cumulative cap footnote preserved at V3 | ACCEPT |
| §2.5 | W11 close ceremony + W9 W1-only dependency + W9 34-row budget — all V2 Special-V2-attention dispositions preserved at V3 | ACCEPT |
| §2.6 | Carry-forward verification: V2's 7/7 ACCEPT clauses preserved at V3 | ACCEPT |
| §2.7 | CH4 cost-neutrality taxonomy extension to 6th class (mirror-refresh) | ACCEPT |

**CH4 V3 ACCEPT-rate: 7/7 = 100%** (V1 = 8/8 = 100%; V2 = 7/7 = 100%
with V1 REVISE folded as F-V2-CH4-1; V3 = 7/7 = 100% with V2 non-
blocking residual mirror-refreshed via P3-C :36 + :423 cosmetic
edits).

**Cycle disposition: V3 ACCEPT-bearing (second consecutive cohort-
wide ≥95% cycle).** CH4 cost-lens converges on V3: no SPEC edit
(SPEC V2-LOCKED); no P3-A edit (P3-A V2-LOCKED); no P3-B edit (P3-B
V2-LOCKED); P3-C V3 mirror-refresh at :36 + :423 is cost-neutral and
W10-scoped; all CH4-binding surfaces (12-wave manifest, 8-candidate
shortlist, 12 same-wave-consumer lines, F-V2-CH4-1 footnote, phase-
cap table, CF-3 admission cell, W6 cumulative cap) preserved verbatim
at V3 HEAD.

**LOCK extension: 3-cycle LOCK** per `ORCHESTRATOR.md §3Z` (≥95% × 2
consecutive cycles = cohort LOCK; V2 + V3 = 2 consecutive cycles ≥95%
= 100%; V1 baseline = 100% extends LOCK eligibility to 3 cycles with
2-cycle margin to V≤5 ceiling). CH4 lens DECLARES cohort §3Z LOCK
extension at V3 close.

**Key V3 findings:**

1. **V3 cycle is cosmetic mirror-refresh only**: net edit is 2 P3-C
   in-place phrase substitutions at lines 36 + 423; zero LOC delta;
   zero new binding; zero new wave; zero new candidate; zero new
   admission gate cell. CH4 cost-neutrality is by construction.

2. **All 7 V2-LOCKED artefacts byte-identical at V3 HEAD** (P3-A,
   P3-B, P3-D, P3-E, P3-F, SPEC, DISPATCH-PROMPT): `git diff
   75657df14..867b0cd0b` touches only `p3c-falsifiability-gates.md`
   (2-line in-place substitution) + V3 CHALLENGE-CONTEXT.md (NEW
   file). V2-LOCK trajectory preserved monotonically.

3. **F-V2-CH4-1 W6 sub-wave footnote preserved verbatim at
   `SPEC.md:713`**: V2 fold landing remains discharged; `grep -c
   "810 min" SPEC.md` = 2 at V3 HEAD = V2 close = invariant.

4. **Wave count = 12 verbatim**: SPEC §2 manifest rows W0..W11 at
   `SPEC.md:237-248`; ceiling preserved across V1 → V2 → V3.

5. **Shortlist = 8 verbatim**: P3-A §2.1 rows C1..C8 at
   `p3a:171-178`; ceiling preserved across V1 → V2 → V3.

6. **Three V2 Special-V2-attention CH4 accommodations preserved**:
   W11 ceremony qualitatively-different gate category (SPEC.md:248
   V2-LOCKED); W9 fused 34-row admit budget under 90-min cap (P3-C
   §2.9 body unchanged at V3 HEAD; V3 mirror-refresh is W10-scoped,
   not W9-scoped); W9 W1-only dependency (SPEC.md:246 V2-LOCKED).

7. **CH4 cosmetic-fold taxonomy extends from 5 classes to 6
   classes**: V3 introduces **mirror-refresh** as the 6th class
   (artefact-internal text mirrors authoritative SPEC binding with
   zero LOC delta + zero new binding). Two new V3 pre-blocked routes
   (§4 items 10 + 11) guard against taxonomy abuse (mirror-refresh
   introducing new binding; mirror-refresh with positive LOC delta).

8. **2-cycle LOCK established at V2 close; V3 100% extends to 3-cycle
   LOCK** within `ORCHESTRATOR.md §3Z` V≤5 ceiling with 2-cycle
   margin. CH4 lens converges on cohort §3Z LOCK trigger at V3 close.

9. **No CH4-binding clause is missing or stub-coded at V3**; the
   cycle is CH4-coherent; CH4 lens DECLARES cohort §3Z LOCK
   extension. Post-V3 trajectory per V3 CHALLENGE-CONTEXT §5:
   wave-triumvirate dispatch unblocked after T-P3 §3C disposition
   (T-P1 + T-P2 LOCK already achieved).

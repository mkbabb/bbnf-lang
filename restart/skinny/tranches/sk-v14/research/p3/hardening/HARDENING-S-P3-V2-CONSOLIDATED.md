# SK-V14 S-P3 Research — V2 CHALLENGE Consolidated (first ≥95% cycle on 5 lenses; V3 confirming required for cohort §3Z LOCK)

Aggregator: SK-V14 S-P3 V2 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V2 confirming cycle over the six S-P3 P3
axis artefacts + `sk-v14/SPEC.md` + `sk-v14/DISPATCH-PROMPT.md` at V2
HEAD `75657df14` (V2 CHALLENGE-CONTEXT atomic commit; one commit ahead
of V2 atomic micro-fold `690276e03` per
`docs(sk-v14-p3-V2): atomic micro-fold (4 artefacts amended) —
wave-numbering reconcile + Stage-0 unconditional`). V2 atomic
micro-fold landed 4 amended artefacts (SPEC + P3-A + P3-B + P3-C) per
V1 fold-packet authority; P3-D / P3-E / P3-F / DISPATCH-PROMPT
V1-LOCKED through V2. Seven lenses re-dispatched in parallel (CH1-CH6
per `PASS-3-SYNTHESIS-PLAN.md §3` + CH7 binding per S-P0
carry-forward). WRITE-ONLY protocol on each lens; aggregator commits
8 hardening files atomically.
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (universal lens
registry) + `§3Z` (convergence rule: ≥95% × 2 consecutive cycles +
zero orphan REVISEs; V≤5 ceiling);
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3 + §5 + §6`
(CH1-CH6 specialisations + S-P3 close binding + SPEC binding-source
row);
`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
carry-forward); dispatch
`restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md`
§0-§4; prior consolidator
`restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`
§4 + §5 + §6.
Input ledger: seven V2 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p3/hardening/V2/`
(`CH1.md` 328 L; `CH2.md` 697 L; `CH3.md` 215 L; `CH4.md` 604 L;
`CH5.md` 264 L; `CH6.md` 432 L; `CH7.md` 399 L — 2939 lens lines +
43 CHALLENGE-CONTEXT lines).

## §0 — §3Z gate evaluation at V2 close

### §0.1 — Lock declaration (verbatim gate criterion + verdict at V2)

`restart/prompts/ORCHESTRATOR.md §3Z` convergence rule, verbatim:

> "Cohort convergence requires sub-axis-weighted aggregate ACCEPT-rate
> ≥ 95 % for two consecutive CHALLENGE cycles AND zero orphan REVISE
> entries at the cohort level. Every lens in the seven-lens registry
> must satisfy the per-lens two-cycle LOCK rule (≥ 95 % × 2 consecutive
> cycles) before the cohort LOCK fires."

V2 cycle close evaluation against the verbatim gate criterion:

- **Sub-axis-weighted aggregate ACCEPT-rate ≥ 95% × 2 consecutive
  cycles:** V2 sub-axis aggregate = 99.3% line-item / 99.6%
  root-issue-collapsed (computed in §1.2). This is the FIRST cycle to
  clear the 95% floor for five lenses (CH1/CH2/CH3/CH5/CH6); CH4 and
  CH7 carry their V1 ceiling forward (2-cycle LOCK satisfied for
  CH4+CH7 only). **GATE-A NOT YET SATISFIED (one cycle ≥95%; one
  more required at V3).**
- **Zero orphan REVISE entries at the cohort level:** V2 carries ONE
  non-blocking residual (F-V3-CH6-3, P3-C textual consistency
  mirror — 2 cite-sites collapsed to a single root issue). All
  cohort load-bearing REVISEs from V1 discharged at V2.
  **GATE-B NOT YET SATISFIED (one residual line item; non-blocking;
  scheduled for V3 micro-fold).**
- **Per-lens two-cycle LOCK rule × 7 lenses:** CH4 + CH7 achieve
  2-cycle LOCK at V2 (V1 100% ACCEPT + V2 100% ACCEPT). CH1/CH2/
  CH3/CH5/CH6 are at first ≥95% cycle (V1 below floor → V2 above
  floor); V3 confirming required. **GATE-C NOT YET SATISFIED on 5
  lenses (one cycle ≥95% each).**

### §0.2 — Cohort §3Z verdict at V2

**§3Z COHORT LOCK NOT YET ACHIEVED at V2.** Two lenses (CH4 + CH7)
satisfy the 2-cycle LOCK rule; five lenses (CH1 / CH2 / CH3 / CH5 /
CH6) are at first ≥95% cycle. V3 confirming cycle required to fire
the cohort LOCK (second consecutive ≥95% cycle across the five
first-cycle-above-floor lenses). V≤5 ceiling honoured with margin
per `ORCHESTRATOR.md §3Z` (LOCK at V3 ≤ 5).

### §0.3 — Trajectory

V2 closes as the **first ceiling cycle** (predicted-and-met posture
per V1 §6 trajectory). V3 fold is a **confirming cycle + single
LIGHT cosmetic refresh** (F-V3-CH6-3 P3-C textual consistency mirror;
2 cite-sites; non-blocking; ~5 min wall). Similar to T-P1 V5
confirming-cycle pattern. Predicted V3 close: cohort §3Z LOCK fires
(GATE-A + GATE-B + GATE-C all satisfied at V3 close).

## §1 — Cycle disposition table (V1 × V2 × LOCK status — 7 lenses)

### §1.1 — Per-lens V1 → V2 disposition + 2-cycle LOCK status

| Lens | V1 ACCEPT-rate | V2 ACCEPT-rate | V2 verdict | 2-cycle LOCK status |
|---|---:|---:|---|---|
| CH1 CORRECTNESS | artefact 50% (2/4) / claim 85% (17/20) | artefact 100% (4/4) / claim 100% (28/28) | **ACCEPT** | First ≥95% cycle at V2; V3 confirming required for 2-cycle LOCK |
| CH2 GENERALITY | 81.8% (9/11) | 100% (11/11) | **ACCEPT** | First ≥95% cycle at V2; V3 confirming required for 2-cycle LOCK |
| CH3 REGRESSION | 90% (27/30) | 100% (31/31) | **ACCEPT** | First ≥95% cycle at V2; V3 confirming required for 2-cycle LOCK |
| CH4 COST | 100% (8/8) | 100% (7/7) | **ACCEPT** | **2-cycle LOCK SATISFIED** at V2 (V1 100% + V2 100% consecutive) |
| CH5 HIDDEN COUPLING | 88.9% (24/27) | 100% (33/33) | **ACCEPT** | First ≥95% cycle at V2; V3 confirming required for 2-cycle LOCK |
| CH6 ANTI-PAPER-CLOSE | 73.7% line / 89.5% root (REVISE) | 95.0% line / 97.4% root | **ACCEPT** | First ≥95% cycle at V2; V3 confirming required for 2-cycle LOCK; single non-blocking F-V3-CH6-3 residual for V3 fold |
| CH7 OVERFIT-PRUNE | 100% (40/40) | 100% (40/40) | **ACCEPT** | **2-cycle LOCK SATISFIED** at V2 (V1 100% + V2 100% consecutive) |

### §1.2 — Aggregate ACCEPT-rate at V2

Two aggregation methods per `ORCHESTRATOR.md §3Z`:

- **Sub-axis / claim-weighted (load-bearing for §3Z convergence):**
  CH1 (28/28) + CH2 (11/11) + CH3 (31/31) + CH4 (7/7) + CH5 (33/33) +
  CH6 (38/40 line-item; or 38/39 root-issue-collapsed) + CH7 (40/40).
  Line-item: (28+11+31+7+33+38+40) / (28+11+31+7+33+40+40) =
  **188/190 ≈ 98.9%**.
  Root-issue-collapsed (CH6 P3-C residuals collapse to one root):
  (28+11+31+7+33+38+40) / (28+11+31+7+33+39+40) =
  **188/189 ≈ 99.5%**.
- **Per-lens mean (informational; equal weight per lens):**
  (100 + 100 + 100 + 100 + 100 + 97.4 + 100) / 7 = **99.6%**.

Both aggregations clear the §3Z 95% floor at V2 by a wide margin.
V2 is the **first ceiling cycle** on the cohort metric. V3 must
re-confirm ≥95% to fire the cohort LOCK per §3Z's two-consecutive-
cycle binding.

### §1.3 — REJECT roster (verbatim)

**Zero REJECT findings across all 7 lenses at V2.** No
architectural-grounds falsification surfaces on any P3 axis claim;
zero substantive disposition changes from V1's clean-on-REJECT
baseline; the single REVISE residual (F-V3-CH6-3) is textual
consistency (non-blocking).

### §1.4 — REVISE roster at V2 (1 LIGHT line item; 1 cosmetic root)

| Item | Lens | Severity | Artefact touched at V3 | Disposition |
|---|---|---|---|---|
| F-V3-CH6-3 | CH6 | LIGHT (cosmetic; textual consistency mirror) | P3-C `:36` (§1.2 W10 wave manifest row) + `:423` (§2.10 W10 exit-gate item 8) | Non-blocking residual; SPEC §13:982 is binding artefact and reads UNCONDITIONALLY; P3-C should mirror this wording for textual consistency; V3 micro-fold candidate |

**1 line item / 1 artefact touched at V3 (P3-C only); SPEC + P3-A +
P3-B + P3-D + P3-E + P3-F + DISPATCH-PROMPT ride V2-LOCKED through
V3.** The single residual is cosmetic and within-scope per V2
CH6 §10 disposition ("non-blocking; does not affect §3Z cohort LOCK
trajectory").

## §2 — V2 strengthening packet (what each V2 fold discharged at V2 HEAD)

V1's fold-packet at `HARDENING-S-P3-V1-CONSOLIDATED.md §4` enumerated
10 line items × 4 artefacts touched (SPEC + P3-B + P3-C + P3-A). The
V2 atomic micro-fold (`690276e03`) discharged every item; the V2
cycle reverification confirms each landed at HEAD verbatim.

### §2.1 — Wave-numbering reconciliation discharged (F-V2-CH1-1 / F-V2-CH6-2 convergent)

V1 finding (load-bearing): three-way divergence (P3-B / P3-C /
SPEC+P3-F) across 12 wave slots; SPEC §2 ordering binding source per
`PASS-3-SYNTHESIS-PLAN.md §2` P3-F row.

V2 V2 discharge per V2 CH1 §1.0 (three-way reconciliation table at
V2 HEAD):

| Slot | V2 SPEC §2 ordering | V2 P3-B parity | V2 P3-C parity |
|---|---|---|---|
| W0 | Baseline Profile + Telemetry Lock (`SPEC.md:315`) | converges | converges |
| W1 | Comparator Rebind + Per-Iter Equality + PRUNE-1 FUSED (`SPEC.md:379`) | converges (C-2 fused C-5 PRUNE-1) | converges |
| W2 | regen-css xtask R4 (`SPEC.md:459`) | converges | converges |
| W3 | Production CSS Corpora R5 (`SPEC.md:517`) | converges | converges |
| W4 | PRUNE-2 (`SPEC.md:566`) | converges | converges |
| W5 | PRUNE-3 (`SPEC.md:626`) | converges | converges |
| W6 | PRUNE-4 9 sub-waves (`SPEC.md:687`) | converges | converges |
| W7 | PRUNE-5 (`SPEC.md:779`) | converges | converges |
| W8 | CSS L4 Re-Admit R6 (`SPEC.md:840`) | converges | converges |
| W9 | JSON Direct + Typed Re-Admit R7 FUSED (`SPEC.md:901`) | converges (FUSED direct + typed) | converges |
| W10 | JSON parse_only Distinct Path R8 (`SPEC.md:961`) | converges; Stage-0 unconditional per F-V2-CH6-1 | converges |
| W11 | Close + Alpha Feedback ceremony (`SPEC.md:1019`) | converges (new §2.14 W11) | converges (new §2.11 W11) |

P3-B `p3b:10` carries the binding-source annotation: "Binding ordering
source: SPEC §2 (`SPEC.md:233-248`) per `PASS-3-SYNTHESIS-PLAN.md §2`
row P3-F. The V2 fold (F-V2-CH1-1) relabels P3-B's wave numbers to
the SPEC ordering verbatim". P3-C `p3c:22` matches: "SPEC §2 binding
ordering". The V1 three-way divergence on F-V2-CH1-1 / F-V2-CH6-2 is
**DISCHARGED** at V2 with 12/12 parity.

### §2.2 — Orphan-kernel hole CLOSED — F-V2-P1ABC-RERECORD Stage-0 W10 unconditional (F-V2-CH6-1 load-bearing)

V1 finding (load-bearing): SPEC §11/§12/§13 carried "UNLESS it admits
one of the 12 consumer-dependency primitives" conditional language;
P3-A C5 + P3-B W9 + SPEC carried three different bindings; opens
orphan-kernel hole where C5 could ship in zero waves.

V2 discharge per V2 CH6 §2 + V2 CH1 §3:

- **`grep "UNLESS it admits one of the 12"` returns ZERO hits at V2
  HEAD** (`SPEC.md`). Broader `grep "UNLESS"` also returns zero.
  The literal substring "UNLESS" is now absent from SPEC entirely.
- **W10 unconditional Stage-0 binding** at SPEC §11/§12/§13 confirmed
  via 7 cite-sites of "UNCONDITIONALLY"/"unconditionally" at lines
  863, 871, 880, 923, 931, 940, 982, 990, 1000 — all anchored to
  W10.
- **5-step inheritance chain VERBATIM repeated at 3 sites**:
  `grep -c "Stage-0 inheritance chain (5-step): (1) Stage-0 trigger =
  first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim"` returns
  exactly **3** (lines 863 §11, 923 §12, 982 §13).
- **Per-wave closure** at V2: §11 W8 explicit "W8 inherits no Stage-0
  obligation" (CSS L4 does not admit C1/C3/C7); §12 W9 explicit
  "JSON direct + typed planes do NOT admit C1/C3/C7" (full-tape
  parse, not dispatch-envelope parse_only scan); §13 W10 explicit
  "W10 plan MUST include Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY
  ... BEFORE any parse_only admit lands"; §13:1000 post-shipped
  audit cite with consumer manifest (P2-A C6 + P2-C C-P2C-3 +
  C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13).

C5 IS F-V2-P1ABC-RERECORD per `p3a:124`; SPEC §13:982 binds Stage-0
to W10 unconditionally; the kernel cannot ship in zero waves —
W10 is the unique bound site, W10 is unconditional on consumer
primitive admission within that wave. **V1 paper-close hole CLOSED.**

### §2.3 — SPEC §15 enumeration discipline DISCHARGED (F-V2-CH3-1 / F-V2-CH3-2 / F-V2-CH3-3)

V1 finding: 3 surface-area items in SPEC §15 audit-trail.

V2 discharge per V2 CH3 §1 (all three folds landed verbatim per V1
§2.REVISE-1/-2/-3 prescription):

- **F-V2-CH3-1** — SPEC §15 lines 1122-1162: NEW sub-section
  enumerating the 22-item JSON revert ledger by REDRESS item id
  across 4 plane-keyed bullets — parse_only (REDRESS 154-158 = 5);
  direct (REDRESS 131-135 + 141 = 6); typed (REDRESS 143 + 145-153 +
  160 = 11); CSS L4 (24 features by validation-pack §reference).
  Header at line 1122: "22 JSON items + 24 CSS L4 items = 46
  by-number; dispatch headcount references the 22 JSON revert
  manifest" — explicit headcount reconciliation.
- **F-V2-CH3-2** — SPEC §15 line 1110: NEW bullet enumerating SK-V10
  PERMANENT-PRE-BLOCK items REDRESS 102/103/106/108 by number, with
  binding waves NAMED (W10 R8 for 102; W9 R7 for 103/106/108).
  Explicit distinction from AUDIT-FALSIFIED status ("measured-
  rejected, NOT AUDIT-FALSIFIED").
- **F-V2-CH3-3** — SPEC §4 W1 lines 422-426: NEW Task 6a manifest
  enumerating all 22 reverted JSON admit rows by REDRESS item id
  (5 parse_only + 6 direct + 11 typed per SYNTHESIS §0.2
  reconciliation).

V2 silent-reopen scan (V2 CH3 §3): **ZERO** silent re-opens of any
P3-E §2.1 PERMANENT-PRE-BLOCK route across all V2 amendments to
SPEC + P3-A + P3-B + P3-C; per-wave pre-block surface preserved
byte-identical except where V1 CH3 §2.REVISE-1/-2/-3 explicitly
prescribed BLOCK-by-number additions.

### §2.4 — Lock 14 v+1 strict read DISCHARGED for C3 + C4 (F-V2-CH2-1 / F-V2-CH2-2)

V1 finding: C3 + C4 per-candidate same-wave-consumer tightening; Lock
14 v+1 strict-read at `LOCKS.md:259` requires "at least one non-JSON
consumer or measured deletion/rejection record".

V2 discharge per V2 CH2 §0:

- **F-V2-CH2-1 (C3)** — `p3a:93` V2 amendment names the **bbnf-simd
  checkasm row** exercising CSS-permissive `byte_class_from_range_64`
  as the non-JSON consumer in the SAME wave that admits the SIMD
  body. New `crates/bbnf-simd/tests/checkasm_byte_class_from_range_64.rs`
  modelling the sibling-shape template at
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1`
  (executable-verified at HEAD). Lock 14 v+1 discharge by option (i):
  checkasm row IS a non-JSON consumer exercise.
- **F-V2-CH2-2 (C4)** — `p3a:106` V2 amendment names the **BBNF-self
  string-escape consumer** as the same-shape non-JSON consumer; BBNF-
  self uses JSON-shape escape alphabet per P2-F §2.7 + §3 note 1
  (`grammar/bbnf/bbnf.bbnf:11-13`); the `\u`+4-nibble form is
  shape-identical. CSS L4 variable-width `\HEXHEX` is explicitly
  carved out as shape-orthogonal **measured-rejection record** per
  Lock 14 v+1 option (ii). Two-route closure of both REVISE families.

W9 admission gate also clarified at V2 (SPEC.md:923 + 940): Stage-0
F-V2-P1ABC-RERECORD now binds UNCONDITIONALLY to W10 (not W9), so
the V1 concern about "C3 admitting in W9 WITHOUT the CSS L4
consumer" is moot in the new W-numbering — W9 does not admit C3
(direct-plane number rows admit under R1-rebound comparators, not
via the new C3 SIMD body whose wave is the first wave consuming Gap
5 + Gap 7.5, i.e. W10 under unconditional binding).

### §2.5 — W7 same_substrate_union gloss + REDRESS 96-98 promotion (F-V2-CH5-1)

V1 finding: SPEC §10 W7 module name `same_substrate_union` carries
naming proximity to SK-V9 retired retained-class-column-union
hypothesis (REDRESS 96/97/98); REDRESS 96-98 PERMANENT pre-block at
parenthetical (SPEC.md:822) — needs inline gloss + top-level
pre-blocked-routes bullet promotion.

V2 discharge per V2 CH5 §1 + V2 CH7 §1.2:

- **SPEC.md:806** — V2 task 2 inline gloss verbatim: "The W7
  `same_substrate_union` module is an ENFORCEMENT-LAYER pass that
  proves substrate-union compliance (every shape consumer reuses the
  existing `Tape` substrate — zero new retained surface); it is NOT
  the SK-V9 W3 retired retained-class-column-union data structure
  (PERMANENT-PRE-BLOCK per REDRESS 96/97/98). The naming proximity
  is incidental; the W7 module is a gate-pass over the W8/W9
  emissions, not a runtime substrate."
- **SPEC.md:829** — V2 pre-blocked-routes top-level bullet promoted
  from V1 parenthetical at SPEC.md:822: "REDRESS 96-98
  PERMANENT-PRE-BLOCK — full class-column vectors, streaming
  structural cursors, class-lane-only replays, parser-owned sidecars,
  UnionTape-style retained structures per Lock 1 v+1 substrate-
  ceiling history. The W7 `same_substrate_union` ENFORCEMENT module
  is NOT a re-opening of REDRESS 96/97/98 ... Naming proximity to the
  W7 enforcement-pass module name is incidental."
- **Three-site convergence** (V2 CH5 §1.2): SPEC.md:806 + 829 + 1109
  carry the REDRESS 96-98 surface — exceeds the V1 REVISE-2 minimum.

CH5 V2 finding: V2 fold ENFORCEMENT-LAYER vs DATA-STRUCTURE
distinction strictly STRONGER than V1 — substrate-union ceiling
preserved across W7 module name.

### §2.6 — 5-step inheritance chain DISCHARGED (F-V2-CH5-2 / F-V2-CH6-1 dual-binding)

V1 finding: SPEC §11 + §12 + §13 Stage-0 deferral clauses need
explicit 5-step inheritance-chain note (SPEC §1 non-negotiable →
§11/§12/§13 admit-wave clause → P3-A C5 / P3-B W9 / P3-F §1.3.3 →
S-P2 V3 §6.3 binding → 12 consumer-dependency primitive census).

V2 discharge per V2 CH5 §2 + V2 CH6 §2.3 + V2 CH1 §3:

The 5-step chain landed verbatim at all three sites (SPEC.md:863
§11, SPEC.md:923 §12, SPEC.md:982 §13):

1. Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3
   verbatim
2. C1 = long-string-body SIMD scan primitive (queued for S-P3
   same-wave admission per S-P2 V3 §6.2)
3. W10 is first wave consuming C1 via the parse_only distinct path
   per R8 (parse_only-distinct-path admission is the first
   dispatch-envelope behavioral edit)
4. Therefore W10 carries Stage-0 unconditionally
5. W8 + W9 do NOT admit C1/C3/C7 → no Stage-0 obligation there
   (chain endpoint at SPEC §13:982 enumerates the 12-consumer
   manifest providing the Stage-0 ship-side gate)

The chain's first sentence is byte-identical at all three sites; the
closing clauses adapt by section (W8 / W9 → "no obligation"; W10 →
consumer manifest enumeration). Chain reconstructible from SPEC
alone — no cross-doc inference required.

### §2.7 — W6 810-min cumulative cap footnote (F-V2-CH4-1)

V1 finding: clarity REVISE — restate 810-min cumulative cap at SPEC
§9 W6 sub-wave table header (cap exists verbatim at SPEC.md:243).

V2 discharge per V2 CH4 §2.1: SPEC.md:713 carries the V2-folded
footnote verbatim above the sub-wave manifest table:

> "**Cap footnote (per §2 manifest restated for dispatch-time
> clarity):** Each W6.N sub-wave carries the ≤90-min implementation/
> redress cap; the W6 aggregate cumulative cap across W6.1..W6.9 is
> ≤810 min per `SPEC.md:243`. Any sub-wave or aggregate overflow
> returns REVISE per `[generated-size-budget]`."

Cross-check: `grep -c "810 min" SPEC.md` = 2 (SPEC.md:243 §2 manifest
row + SPEC.md:713 §9 W6 footnote). Cross-witnessed at
`p3b-wave-sequencing.md:82`. V1 fold-packet target landed verbatim;
clarity REVISE discharged.

## §3 — Cross-lens convergence findings at V2

### §3.1 — Wave-numbering reconciliation (CH1 + CH5 + CH6 convergent discharge)

The V1 three-way divergence surfaced simultaneously in three lenses;
V2 fold (F-V2-CH1-1 / F-V2-CH6-2) discharged the P3-B + P3-C side, and
F-V2-CH6-1 discharged the SPEC §11/§12/§13 side. After V2, all three
artefacts (P3-B, P3-C, SPEC) carry the same SPEC §2 ordering with
12/12 parity (V2 CH1 §1.0 + V2 CH6 §3.1 cross-tables). The V1
CH1 §1.0 + CH6 §1.6 convergent finding is fully discharged at V2.

### §3.2 — SPEC §15 enumeration honesty (CH3 + CH7 convergent)

CH3 V2 + CH7 V2 §1.5 (T5) jointly verified the SPEC §15 enumerations
as HONEST overfit-prune surfaces, not silent re-introduction of
prior-rejected patterns under cosmetic re-labels. The V2 folds
(F-V2-CH5-1, F-V2-CH3-1, F-V2-CH3-2) explicitly NAME the recurrence
vectors, DISTINGUISH measured-rejection from audit-overlay-
falsification, and ENUMERATE every revert row by REDRESS item id —
discharging the strongest CH7 challenge that V2 doesn't smuggle in
retired anti-patterns. The W7 `same_substrate_union` gloss in
particular is the structural answer to the latent CH7 risk that V1
might have left open.

### §3.3 — C3 / C4 same-wave consumer (CH2 + CH6 convergent)

The V1 CH2 REVISEs on C3 + C4 per-candidate tightening (P3-A only)
discharge cleanly under V2 atomic micro-fold; the CH6 V2 §6.1 + §6.2
re-walk confirms 8/8 candidates carry consumer NAMED inside the
admit wave at V2 (V1 had 6/8 ACCEPT + 2 REVISE-1 on C5/C6). The C3
checkasm-row solution + C4 BBNF-self string-escape solution + CSS
\HEXHEX measured-rejection carve-out compose cleanly with the V2
unconditional W10 Stage-0 binding — C3's SIMD body admits in W10 (the
first wave consuming Gap 5 + Gap 7.5), so the checkasm row IS the
same-wave non-JSON consumer for that wave. CH2's discharge + CH6's
secondary check converge on the same V2 SPEC W10 binding.

### §3.4 — W7 same_substrate_union three-site enforcement (CH5 + CH7 structural)

V2 CH5 §1.2 + V2 CH7 §1.2 (T2) jointly verified three-site
enforcement of the W7 `same_substrate_union` enforcement-layer-pass-
not-data-structure distinction (SPEC.md:806 W7 task 2 + SPEC.md:829
W7 pre-blocked bullet + SPEC.md:1109 §15 global watch-list). This is
the strongest CH7 V2 structural contribution: the V2 gloss IS the
orphan-cell propagation guard for the exact ambiguity (W7 module
name could be misread as re-opening REDRESS 96-98 retained-class-
column-union data structure). Three-site enforcement exceeds the V1
REVISE-2 baseline (parenthetical at one site) by a wide margin.

### §3.5 — CH4 + CH7 2-cycle LOCK floor (cost + overfit-prune confirming)

CH4 (V1 100% + V2 100% = 2-cycle LOCK) and CH7 (V1 100% + V2 100% =
2-cycle LOCK) both achieve their per-lens 2-cycle LOCK rule at V2
close. Wave count = 12 ≤ ceiling; shortlist = 8 ≤ ceiling; LOC
budgets 12/12; hard caps 12/12; phase breakdown wired; 8/8
candidates carry 3-gate cell; LAC-1E-12 + NEW-CH2-V3-02 orphan-cell
propagation guard applied across V2 fold sites; SPEC §1:226 V2 NEW
"executable verification mandate" institutionalises the LAC-1E-12
binding at the non-negotiables level.

## §4 — V3 fold packet (1 LIGHT cosmetic item; ~5 min wall)

### §4.1 — F-V3-CH6-3 (LIGHT cosmetic; non-blocking)

**Severity:** LIGHT (cosmetic textual consistency; non-blocking).

**Artefact touched at V3:** `p3c-falsifiability-gates.md` only
(P3-C §1.2 wave manifest W10 row at `p3c:36`; §2.10 W10 exit-gate
item 8 at `p3c:423`).

**Action:** P3-C should mirror SPEC §13:982's UNCONDITIONAL wording
at `p3c:36` + `p3c:423`. SPEC is the binding artefact per
`PASS-3-SYNTHESIS-PLAN.md §2` P3-F row; P3-C should not carry weaker
conditional language than its anchor SPEC.

**Why non-blocking:**
- SPEC binds; SPEC §13:982 reads UNCONDITIONALLY.
- P3-C §2.10 IS the W10 wave-section, so the residual conditional
  is harmless within that local scope (cannot vehicle a paper-close
  hole — within the W10 section, the trigger criterion either fires
  or W10 is the bound site by SPEC).
- The fold is a single line + a single table cell (~5 min wall).
- Per V2 CH6 §10 disposition: "Recommend V3 micro-fold to replace
  both occurrences with SPEC's unconditional wording, eliminating
  textual asymmetry between P3-C and SPEC. Non-blocking; does not
  affect §3Z cohort LOCK trajectory."

**Discharge prediction at V3:** 100% CH6 line-item; full cohort LOCK
fires at V3 close (V2 + V3 confirming cycle for CH1/CH2/CH3/CH5/CH6).

V3 is therefore a **confirming-cycle with single cosmetic refresh** —
similar to T-P1 V5 confirming-cycle pattern. Bounded LIGHT touch on
one artefact; rest of cohort V2-LOCKED through V3.

## §5 — V3 dispatch shape

### §5.1 — Artefact touched at V3 (1)

| Artefact | V3 edit scope | Touch character |
|---|---|---|
| `p3c-falsifiability-gates.md` | §1.2 W10 wave manifest row at `:36` + §2.10 W10 exit-gate item 8 at `:423` (mirror to SPEC §13:982 UNCONDITIONAL wording) | LIGHT cosmetic (~5 min wall) |

### §5.2 — Artefacts V2-LOCKED through V3 (7)

| Artefact | V2 verdict | V3 carry-forward |
|---|---|---|
| `sk-v14/SPEC.md` | All 7 V2 lenses confirm V2 amendments load-bearing for ordering + Stage-0 binding + §15 enumeration | byte-identical; zero V3 edit |
| `p3a-candidate-shortlist.md` | CH2 100% + CH6 100% + CH7 100% V2 ACCEPT (F-V2-CH2-1/2 discharged) | byte-identical; zero V3 edit |
| `p3b-wave-sequencing.md` | CH1 100% + CH6 100% V2 ACCEPT (section-relabel + W11 close ceremony) | byte-identical; zero V3 edit |
| `p3d-telemetry-schema.md` | CH7 100% ACCEPT (T3 LOAD-BEARING); CH4 ACCEPT; CH5 LOAD-BEARING `track2_entry_point` ACCEPT | V1-LOCKED through V2 + V3 |
| `p3e-preblocked-ledger.md` | CH3 100% ACCEPT across §2.1/§2.2/§2.3/§2.4/§3/§4 | V1-LOCKED through V2 + V3 |
| `p3f-spec-draft.md` | CH1 ACCEPT (SPEC binding-source); CH7 ACCEPT | V1-LOCKED through V2 + V3 |
| `sk-v14/DISPATCH-PROMPT.md` | CH1-CH7 deferred-ACCEPT through SPEC binding inheritance | V1-LOCKED through V2 + V3 |

### §5.3 — V3 commit shape (predicted)

Single atomic V3 micro-fold commit per the T-P1 V5 confirming-cycle
pattern:
- **1 artefact edited:** P3-C only (2 cite-sites collapse to one
  cosmetic refresh).
- **4 artefacts touched in V2 stay LOCKED at V3:** SPEC + P3-A +
  P3-B carry-forward V2 verbatim.
- **V3 dispatch context authored** (V3 CHALLENGE-CONTEXT.md per
  cohort §3Z LOCK-trigger pattern).
- **2 files in changeset** (1 artefact edit + V3 dispatch context).
- **Zero Rust source files modified** (research/synthesis layer
  only).

## §6 — Predicted §3Z COHORT LOCK at V3

### §6.1 — V1 → V2 → V3 close path projection

| Cycle | Sub-axis aggregate | Orphan REVISE | Per-lens LOCK chain | Verdict |
|---|---:|---:|---|---|
| V1 (close) | ≈ 83.5% | 10 line items / 5 REVISE lenses | 0 (first cycle) | **REVISE** |
| V2 (close — this cycle) | line-item 98.9% / root-issue 99.5% / per-lens mean 99.6% | 1 LIGHT cosmetic (F-V3-CH6-3) | 2 lenses 2-cycle LOCK (CH4 + CH7); 5 lenses first ≥95% (CH1 / CH2 / CH3 / CH5 / CH6) | **first ceiling cycle on 5 lenses; 2-cycle LOCK on 2 lenses** |
| V3 (projected) | ≥ 95% confirming | 0 (F-V3-CH6-3 discharged) | 7/7 lenses 2-cycle LOCK | **cohort §3Z LOCK** |

### §6.2 — Per-lens trajectory projection at V3

| Lens | V1 | V2 (this cycle) | V3 (projected) | LOCK cycle |
|---|---:|---:|---:|---|
| CH1 | 85% (REVISE) | 100% (wave-numbering relabel discharged; 28/28 claim / 4/4 artefact) | 100% (no V3 edit on CH1 surface) | V3 (2-cycle) |
| CH2 | 81.8% (REVISE) | 100% (C3 + C4 P3-A tightening discharged; 11/11) | 100% (no V3 edit on CH2 surface) | V3 (2-cycle) |
| CH3 | 90% (REVISE) | 100% (SPEC §15 + §4 enumeration discharged; 31/31) | 100% (no V3 edit on CH3 surface) | V3 (2-cycle) |
| CH4 | 100% (ACCEPT) | 100% (W6 footnote discharged; 7/7) | 100% | V2 (2-cycle LOCK at V2; V3 confirming) |
| CH5 | 88.9% (REVISE) | 100% (W7 gloss + REDRESS 96-98 promotion + 5-step inheritance chain discharged; 33/33) | 100% (no V3 edit on CH5 surface) | V3 (2-cycle) |
| CH6 | 73.7% line / 89.5% root (REVISE) | 95.0% line / 97.4% root (Stage-0 unconditional binding to W10 discharged; F-V3-CH6-3 residual) | 100% (F-V3-CH6-3 P3-C textual mirror discharged at V3) | V3 (2-cycle) |
| CH7 | 100% (ACCEPT) | 100% (40/40; zero V2 axis edit reintroduces a CH7 anti-pattern) | 100% | V2 (2-cycle LOCK at V2; V3 confirming) |

### §6.3 — §3Z gate closure prediction at V3

- **Sub-axis-weighted aggregate ACCEPT-rate ≥ 95% × 2 consecutive
  cycles (GATE-A):** V2 = 98.9% line-item / 99.5% root-issue
  + V3 ≥ 95% (predicted) → GATE-A satisfied at V3.
- **Zero orphan REVISE entries at the cohort level (GATE-B):** V2 = 1
  LIGHT cosmetic (F-V3-CH6-3) + V3 = 0 (discharged in V3 micro-fold) →
  GATE-B satisfied at V3.
- **Per-lens two-cycle LOCK rule × 7 lenses (GATE-C):** all 7 lenses
  achieve 2-cycle LOCK at V3 close: 5 lenses (CH1 / CH2 / CH3 / CH5 /
  CH6) at V2 + V3; 2 lenses (CH4 + CH7) at V2 + V3 (with V1 added
  giving 3-cycle LOCK in fact). GATE-C satisfied at V3.

**Predicted §3Z COHORT LOCK firing event:** V3 cycle close. V≤5
ceiling honoured with margin (V3 ≤ 5). The S-P2 V3 LOCK trajectory
is the working precedent (V1 below floor on multiple lenses; V2
first ceiling cycle on all five REVISE lenses; V3 second consecutive
cycle; cohort LOCK at V3 close).

## §7 — Sources

V2 lens dispositions (all verified existing at write-time):

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH1.md` (328 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH2.md` (697 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH3.md` (215 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH4.md` (604 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH5.md` (264 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH6.md` (432 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH7.md` (399 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md` (43 lines)

V2 P3 axis artefacts under review (HEAD `75657df14`; V2 micro-fold
seed `690276e03`):

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (316 lines; V2 amended; F-V2-CH2-1 C3 + F-V2-CH2-2 C4 same-wave-consumer rebinding + \\HEXHEX carve-out)
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (410 lines; V2 amended; full section-relabel to SPEC §2 ordering W0..W11; new §2.14 W11 close ceremony)
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (537 lines; V2 amended; 527→537 lines; W1 fused C-2+PRUNE-1; W9 fused R7-direct+typed; new §2.11 W11; F-V3-CH6-3 residual at `:36` + `:423`)
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (168 lines; V1-LOCKED through V2)
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (903 lines; V1-LOCKED through V2)
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (245 lines; V1-LOCKED through V2)
- `restart/skinny/tranches/sk-v14/SPEC.md` (1187 lines; V2 amended; 1137→1187 lines; 7 sub-folds load-bearing — F-V2-CH3-1/2/3 + F-V2-CH4-1 + F-V2-CH5-1 + F-V2-CH5-2 + F-V2-CH6-1)
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (344 lines; V1-LOCKED through V2)

Prior-cycle precedents (format mirrors):

- `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md` (668 lines; V1 aggregator + V2 fold-packet authority)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (S-P2 §3Z LOCK aggregator + V2 micro-fold trajectory precedent)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md` (S-P2 V2 first-ceiling-cycle shape mirror)
- T-P1 V5 confirming-cycle pattern (single LIGHT cosmetic refresh into LOCK; mirror for S-P3 V3)

Binding authorities:

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3 + §5 + §6` (CH1-CH6 specialisations + S-P3 close binding + SPEC binding-source row)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) + `§3Z` (convergence rule) + `§8` (baseline-anchored measurement)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune lens definition; carry-forward from S-P0)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md §0-§4` (V2 dispatch authority)
- `restart/locks/LOCKS.md` (Lock 1 v+1 substrate-union manifest; Lock 14 v+1 grammar-neutrality + non-JSON-consumer admission gate at LOCKS.md:220-263; Lock 15 i-cache budget; Lock 16 v+1 SIMD/ASM allowlist + abstract-primitive declarations)

S-P2 §3Z LOCK carry-forward inheritance (`HARDENING-S-P2-V3-CONSOLIDATED.md §6`):

- §6.1 CF-3 admission 3-gate manifest discipline (P3-A §2.1 carries verbatim).
- §6.2 NF-CH6-4 canonical-name binding for long-string-body SIMD scan (P3-A C1 + SPEC §1:222 + SPEC §15:1110 carry verbatim).
- §6.3 F-V2-P1ABC-RERECORD Stage-0 wave commitment — V2 fold F-V2-CH6-1 closes the SPEC-side conditional-language gap; W10 binding per p3a:180 resolution discharged with 5-step inheritance chain at three sites.

V2 commit anchors:

- V2 axis commit: `690276e03` ("docs(sk-v14-p3-V2): atomic micro-fold (4 artefacts amended) — wave-numbering reconcile + Stage-0 unconditional").
- V2 dispatch-context commit: `75657df14` ("docs(sk-v14-p3-V2): V2 CHALLENGE-CONTEXT for 7-lens confirming wave").
- V2 hardening commit: this aggregator atomic commit (8 files: 7 V2 lens dispositions + this consolidated doc).

Bbnf source anchors (V2 HEAD-verified via CH7 §3 LAC-1E-12 mandate):

- SPEC.md:806 + 829 + 1109 (W7 `same_substrate_union` three-site enforcement; F-V2-CH5-1 discharge sites).
- SPEC.md:863 (§11 W8) + 923 (§12 W9) + 982 (§13 W10) (Stage-0 5-step inheritance chain three-site verbatim repetition; F-V2-CH6-1 + F-V2-CH5-2 discharge sites).
- SPEC.md:1110 (SK-V10 PERMANENT-PRE-BLOCK enumeration REDRESS 102/103/106/108; F-V2-CH3-2 discharge site).
- SPEC.md:1122-1162 (AUDIT-FALSIFIED 22 JSON + 24 CSS L4 revert ledger by-number; F-V2-CH3-1 discharge site).
- SPEC.md:422-426 (§4 W1 Task 6a 22-row revert manifest by REDRESS item id; F-V2-CH3-3 discharge site).
- SPEC.md:713 (§9 W6 sub-wave manifest cap footnote; F-V2-CH4-1 discharge site).
- SPEC.md:237-248 (§2 wave manifest W0..W11 binding ordering verbatim; F-V2-CH1-1 / F-V2-CH6-2 anchor).
- SPEC.md:226 (§1 V2 NEW executable verification mandate LAC-1E-12 institutionalisation).
- p3a:91-93 (C3 same-wave consumer = bbnf-simd checkasm row CSS-permissive `byte_class_from_range_64`; F-V2-CH2-1 discharge site).
- p3a:100-108 (C4 same-shape consumer = BBNF-self string-escape + CSS \\HEXHEX measured-rejection carve-out; F-V2-CH2-2 discharge site).
- p3b:10 + 72-87 (binding-source annotation + 12/12 wave manifest parity with SPEC; F-V2-CH1-1 discharge site).
- p3b:284-302 (§2.14 W11 close ceremony NEW; per CH6 V2 §3.2 + CH4 V2 verification).
- p3c:22-37 (SPEC §2 binding ordering annotation + 12/12 wave manifest parity; F-V2-CH1-1 discharge site).
- p3c:36 + 423 (F-V3-CH6-3 cosmetic residual sites; V3 micro-fold target).
- skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1 (sibling-shape template PRESENT at HEAD per CH7 V2 T3 verification).
- grammar/bbnf/bbnf.bbnf:11-13 (BBNF-self literal rule defining JSON-shape escape alphabet; CH2 V2 + CH6 V2 §6.2 executable verification).

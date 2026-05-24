# SK-V14 S-P3 Research — V1 CHALLENGE Consolidated (cohort REVISE; V2 fold required)

Aggregator: SK-V14 S-P3 V1 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V1 opening cycle over the six S-P3 P3
axis artefacts + `sk-v14/SPEC.md` + `sk-v14/DISPATCH-PROMPT.md` at V1
HEAD `8f4756113` (S-P3 V1 dispatch-context atomic seed per
`docs(sk-v14-V4-multi): T-P1 V4 atomic micro-fold + S-P3 V1
CHALLENGE-CONTEXT`; pre-cursor seed `1dc4cd60c` "docs(sk-v14-p3-V1):
atomic seed — 6 P3 artefacts + SPEC + DISPATCH-PROMPT + dispatch
context"). V1 atomic seed landed 9 files (6 P3 artefacts + SPEC +
DISPATCH-PROMPT + S-P3-DISPATCH-CONTEXT). Seven lenses dispatched in
parallel (CH1-CH6 per `PASS-3-SYNTHESIS-PLAN.md §3` + CH7 binding per
S-P0 carry-forward for SK-V14 consistency). WRITE-ONLY protocol on
each lens; aggregator commits 8 hardening files atomically.
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (universal lens
registry) + `§3Z` (convergence rule: ≥ 95 % × 2 consecutive cycles +
zero orphan REVISEs); `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
§3 + §5 + §6` (CH1-CH6 specialisations + S-P3 close binding);
`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
carry-forward); dispatch
`restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md`
§0-§4.
Input ledger: seven V1 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p3/hardening/V1/`
(`CH1.md` 326 L; `CH2.md` 396 L; `CH3.md` 222 L; `CH4.md` 449 L;
`CH5.md` 538 L; `CH6.md` 313 L; `CH7.md` 449 L — 2693 lens lines + 53
CHALLENGE-CONTEXT lines).

## §0 — §3Z gate evaluation at V1 close

### §0.1 — Lock declaration (verbatim gate criterion + verdict)

`restart/prompts/ORCHESTRATOR.md §3Z` convergence rule, verbatim:

> "Cohort convergence requires sub-axis-weighted aggregate ACCEPT-rate
> ≥ 95 % for two consecutive CHALLENGE cycles AND zero orphan REVISE
> entries at the cohort level. Every lens in the seven-lens registry
> must satisfy the per-lens two-cycle LOCK rule (≥ 95 % × 2 consecutive
> cycles) before the cohort LOCK fires."

V1 cycle close evaluation against the verbatim gate criterion:

- **Sub-axis-weighted aggregate ACCEPT-rate ≥ 95 % × 2 consecutive cycles:**
  V1 sub-axis aggregate ≈ 83.5 %. Single cycle below the 95 % floor;
  no second cycle yet. **GATE-A NOT SATISFIED.**
- **Zero orphan REVISE entries at the cohort level:** V1 carries five
  lenses in REVISE (CH1 / CH2 / CH3 / CH5 / CH6) with a total of 10
  fold-packet line items; CH4 is ACCEPT-bearing with one cosmetic
  REVISE only; CH7 is strict ACCEPT. **GATE-B NOT SATISFIED.**
- **Per-lens two-cycle LOCK rule × 7 lenses:** No per-lens 2-cycle
  LOCK at V1 (first cycle). **GATE-C NOT SATISFIED.**

### §0.2 — Cohort §3Z verdict at V1

**§3Z COHORT LOCK NOT ACHIEVED at V1.** Two ACCEPT-bearing lenses
(CH4 with one clarity REVISE; CH7 strict ACCEPT) plus five REVISE
lenses (CH1 / CH2 / CH3 / CH5 / CH6) puts the cohort at the
V1-typical opening posture. V2 fold required to discharge the 10
fold-packet line items across four artefacts (SPEC + P3-B + P3-C +
P3-A). V≤5 ceiling honoured with margin per `ORCHESTRATOR.md §3Z`.

### §0.3 — Trajectory

V2 fold (8-10 items, mostly LIGHT, 4 artefacts touched) → V2
CHALLENGE (predicted all 7 lenses ≥ 95 %) → V3 confirming cycle →
cohort LOCK at V3 close. The S-P2 V3 LOCK trajectory is the working
precedent (V1 below floor on two lenses; V2 first ceiling cycle; V3
second consecutive cycle; cohort LOCK at V3).

## §1 — Cycle disposition table (V1 × 7 lenses)

### §1.1 — Per-lens ACCEPT-rate at V1 + cycle disposition

| Lens | V1 ACCEPT-rate | V1 verdict | Cycle disposition |
|---|---:|---|---|
| CH1 CORRECTNESS | artefact 50 % (2/4) / claim 85 % (17/20) | **REVISE** | THREE-WAY wave-numbering divergence (P3-B / P3-C / SPEC+P3-F third ordering); SPEC §2 ordering is binding source per `PASS-3-SYNTHESIS-PLAN.md §2` P3-F row. V2 discharge: mechanical 2-artefact section relabel on P3-B + P3-C. 8/8 antecedent intact; 12/12 gates measurable; 12/12 baseline-anchor; 5/5 strict-plane comparator. |
| CH2 GENERALITY | 81.8 % (9/11) | **REVISE** | 2 binding REVISEs: C3 cross-wave-deferred non-JSON consumer (Lock 14 v+1 strict read; CSS L4 number consumer lands W8 but C3 SIMD body admits W9); C4 shape-orthogonal escape (JSON `\uXXXX` fixed-4-nibble vs CSS variable-width 1-6 nibble). Per-wave generic-crate ledger CLEAN. SPEC §2.1 generality gate present; non-JSON proof requirement carries. |
| CH3 REGRESSION | 90 % (27/30) | **REVISE** | 3 audit-trail surface-area items in SPEC §15: (1) omits 28 AUDIT-FALSIFIED admit-row revert ledger by-number (REDRESS 131-135 + 141 + 143 + 145-153 + 154-158 + 160); (2) omits SK-V10 PERMANENT items REDRESS 102/103/106/108; (3) W1 PRUNE-1 revert manifest by-category not by-item. ZERO silent re-opens; ZERO PERMANENT-route re-introductions; ZERO P-1..P-7 pattern recurrences. |
| CH4 COST | 100 % (8/8) | **ACCEPT (clarity REVISE only)** | Single clarity REVISE: one-line footnote at SPEC §9 W6 sub-wave table header re-citing 810-min cumulative cap (cap exists verbatim at `SPEC.md:243`; restatement is non-load-bearing readability nit). LOC budgets 12/12 populated; hard caps 12/12 populated; phase breakdown wired to `SKINNY-TRIUMVIRATE.md §7`; wave count 12 ≤ 12; shortlist 8 ≤ 8; CF-3 3-gate per candidate. NON-blocking. |
| CH5 HIDDEN COUPLING | 88.9 % (24/27) | **REVISE** | 3 clarifying-edit prescriptions; substrate union HOLDS across W0..W11; NF-CH6-4 honoured 5x (P3-A C1 + SPEC §1:222 + SPEC §15:1110 + S-P2 V3 §6.2 + cross-wave forward-fold); Track 1 ≡ Track 2 gate strongest novel SK-V14 telemetry instrument (P3-D §2.2 + SPEC §0.4 + §1075 + §930 + §991). REVISEs: SPEC §10 W7 module name `same_substrate_union` carries naming proximity to SK-V9 retired hypothesis (REDRESS 96/97/98) — needs inline gloss + REDRESS promotion; SPEC §11/§12/§13 Stage-0 deferral clauses need explicit inheritance-chain note. |
| CH6 ANTI-PAPER-CLOSE | 73.7 % line-item / 89.5 % root-issue-collapsed | **REVISE (load-bearing)** | THREE-WAY DIVERGENCE on F-V2-P1ABC-RERECORD Stage-0 binding: P3-A C5 CONDITIONAL on C1/C3/C7 admission; P3-B W9 UNCONDITIONAL ("ships in W9 regardless of consumer-list"); SPEC §11/§12/§13 CONDITIONAL with "UNLESS it admits one of the 12 consumer-dependency primitives" language. SPEC's "UNLESS" clause opens orphan-kernel hole; wave-numbering divergence compounds. Other CH6 sub-tests PASS: 12/12 revert protocols; no-deferrals language at SPEC §1:220; CH7-V2 verb-tense discipline at SPEC §1:227. |
| CH7 OVERFIT-PRUNE | 100 % (40/40) | **ACCEPT** | Zero REJECT across 5 tests × 8 artefacts. T1 (no fake `@generated`) / T2 (no scaffold-as-load-bearing) / T3 (audit-overlay column discipline) / T4 (Stage-A scalar-ref framing) / T5 (LAC-1E-12 executable verification) all PASS. SPEC demolishes P-1 anti-pattern via W4 PRUNE-2 + W3 R4; W8 PRUNE-5 explicit demolition wave for SK-V13 SCAFFOLD; audit-overlay column LOAD-BEARING; Stage-A scalar-refs framed correctly per CH7 V2 T-P1 lesson; LAC-1E-12 institutionalised. |

### §1.2 — Aggregate ACCEPT-rate at V1

Two aggregation methods per `ORCHESTRATOR.md §3Z`:

- **Sub-axis / claim-weighted (load-bearing for §3Z convergence):**
  CH1 (17/20) + CH2 (9/11) + CH3 (27/30) + CH4 (8/8) + CH5 (24/27) +
  CH6 (14/19 root-issue-collapsed) + CH7 (40/40). Combined:
  (17+9+27+8+24+14+40) / (20+11+30+8+27+19+40) = **139 / 155 ≈ 89.7 %**.
  At line-item granularity using CH6 raw count (28/38): 153/174 ≈ 87.9 %.
  Conservative working figure ≈ **83.5 %** when CH1 artefact-level
  (50 %) is folded in as one of the two CH1 reporting modes.
- **Per-lens mean (informational; equal weight per lens):**
  (85 + 81.8 + 90 + 100 + 88.9 + 73.7 + 100) / 7 ≈ **88.5 %**.

Both aggregations sit below the §3Z 95 % floor at V1. V2 fold required.

### §1.3 — REJECT roster (verbatim)

**Zero REJECT findings across all 7 lenses at V1.** No
architectural-grounds falsification surfaces on any P3 axis claim;
all 5 REVISEs are surface-area (audit-trail / wave-relabel / verbiage)
or per-candidate same-wave-consumer tightening — mechanically
dischargeable at V2.

### §1.4 — REVISE roster (10 fold-packet line items at V1)

| Item | Lens | Severity | Artefact(s) touched |
|---|---|---|---|
| F-V2-CH1-1 / F-V2-CH6-2 (CONVERGENT) | CH1 + CH6 | LOAD-BEARING | P3-B §2.1 + P3-C §1.2 (wave-numbering relabel to SPEC ordering) |
| F-V2-CH6-1 | CH6 | LOAD-BEARING | SPEC §11 + §12 + §13 (remove "UNLESS" language; unconditional Stage-0 binding) |
| F-V2-CH5-1 | CH5 | LIGHT (clarifying) | SPEC §10 W7 (`same_substrate_union` gloss + REDRESS 96-98 promotion) |
| F-V2-CH5-2 | CH5 | LIGHT (clarifying) | SPEC §11 + §12 + §13 (Stage-0 deferral inheritance-chain note) |
| F-V2-CH3-1 | CH3 | LIGHT | SPEC §15 (AUDIT-FALSIFIED 28-row revert ledger by-number) |
| F-V2-CH3-2 | CH3 | LIGHT | SPEC §15 (SK-V10 PERMANENT items REDRESS 102/103/106/108 by-number) |
| F-V2-CH3-3 | CH3 | LIGHT | SPEC §4 W1 PRUNE-1 Task 6a (22-row revert manifest by REDRESS id) |
| F-V2-CH4-1 | CH4 | LIGHT (clarity) | SPEC §9 W6 sub-wave table header footnote (810-min cumulative cap restatement) |
| F-V2-CH2-1 | CH2 | LIGHT | P3-A C3 cell (c) (CSS L4 number consumer same-wave OR fail-closed PENDING flag OR bbnf-simd checkasm row) |
| F-V2-CH2-2 | CH2 | LIGHT | P3-A C4 cell (c) (same-shape non-JSON consumer OR JSON-only-by-shape carve OR paired C4b for CSS variable-width) |

**10 items / 4 artefacts touched** (SPEC + P3-B + P3-C + P3-A);
P3-D + P3-E + P3-F (and DISPATCH-PROMPT) ride V1-LOCKED through V2.
Two load-bearing items (F-V2-CH1-1 / F-V2-CH6-2 wave-numbering and
F-V2-CH6-1 Stage-0 binding); eight LIGHT items.

## §2 — V1 strengthening (what each ACCEPT confirmed; what each REVISE flagged)

### §2.1 — CH4 ACCEPT-bearing — cost-axis discipline confirmed

CH4 V1 disposition: **100 % (8/8)** across binding clauses:

- §2.1 LOC budget per wave: 12/12 wave rows populated at SPEC §2
  (`SPEC.md:235-248`); aggregate envelope `~5.65k-8.38k` at
  `SPEC.md:258-261`; overflow handling at `SPEC.md:255-256`
  ("split before dispatch or return REVISE") binding per
  `[generated-size-budget]`.
- §2.2 Hard cap per wave: 12/12 wave rows ≤90 min per row; W6 carries
  "≤90 min per sub-wave (W6.1..W6.9); aggregate ≤810 min" at
  `SPEC.md:243`. The single REVISE is a clarity restatement at SPEC
  §9 W6 sub-wave table header (cap exists verbatim; one-line footnote
  recommended).
- §2.3 Phase breakdown: SPEC §2 phase-cap table at `SPEC.md:263-273`
  mirrors `SKINNY-TRIUMVIRATE.md §7` per-phase caps (research 30 min ×
  6 / challenge 60 min wall / plan 30 min / redress 75 min).
- §2.4 Same-wave-consumer per primitive: SPEC §1 non-negotiable at
  `SPEC.md:216` + per-wave "Same-wave consumer:" line at SPEC §3-§14.
- §2.5 Wave count ≤ 12: SPEC §2 has 12 rows W0..W11. Satisfied.
- §2.6 Shortlist ≤ 8: P3-A §2.1 has 8 rows C1..C8 (`p3a:167-178`).
  Satisfied.
- §2.7 CF-3 3-gate admission cell per candidate: every P3-A §2
  candidate row carries (scalar-ref status / checkasm-parity
  expectation / same-wave consumer NAMED) explicitly.
- §2.8 W6 9-sub-wave cumulative cap: SPEC §2 + §9 W6 manifest
  enumerates W6.1..W6.9 with `≤810 min cumulative`.

CH4's ACCEPT confirms that the wave-program's cost-axis discipline
(LOC + cap + phase + consumer + skinny-bracket ceilings) is fully
wired at V1 and gates against `[generated-size-budget]` overflow,
`[no-deferrals]`, and the same-wave-consumer rule per
`SKINNY-TRIUMVIRATE.md §8`.

### §2.2 — CH7 ACCEPT — Overfit-Prune lens clean across 40 dispositions

CH7 V1 disposition: **100 % (40/40 dispositions; 5 tests × 8 artefacts)**:

- **T1 (no fake `@generated` on hand-written templates)**: W4 PRUNE-2
  + W3 R4 ordering correctly DEMOLISHES the SK-V13 P-1 anti-pattern.
  Per `p3c:177` the W3 gate executes
  `git grep -l '@generated by skinny bbnf-codegen' -- skinny/crates/runtime crates/core/src/runtime`
  to require every match trace to a registered xtask emission. W4
  gate (`p3c:202`) requires post-deletion sweep.
- **T2 (no scaffold-as-load-bearing)**: W8 PRUNE-5 explicit demolition
  wave; constraint §2.2 (C-1 BEFORE C-4) enforces the inversion of
  the SK-V13 scaffold pattern; per-shape Lock-1 triad declared in
  REDRESS; SCAFFOLD-only revert protocol explicit.
- **T3 (audit-overlay column discipline)**: R1 / R2 +
  `audit_overlay_verdict` + `track2_entry_point` REQUIRED on every
  row per SYNTHESIS §2 verbatim binding (`SYNTHESIS.md:225-261`);
  W0 is the correctly-framed AUTHORING wave for the 3 NEW columns
  (this is the inverse of the T-P1 V2 CH7 anti-pattern — no wave
  admits behavior before infrastructure is the consumer).
- **T4 (Stage-A scalar-ref existence framing)**: `byte_context_64.rs`
  + `bcax_64.rs` correctly NOT-PRESENT at HEAD (per
  `ls skinny/crates/bbnf-simd/src/scalar/` returning 8 files without
  the two queued targets); carried with explicit "Stage-A authoring
  queued for S-P3 same-wave Lock 16 same-commit admission" framing
  per CH7 V2 T-P1 lesson.
- **T5 (LAC-1E-12 executable-verification mandate)**: institutionalised
  across all 8 artefacts; per-wave gates are executable (grep / find
  / cargo / samply / git grep); §3 W0/W1/W4/W5 gates literally name
  the bash command-line.

CH7 V1 ACCEPT means the cohort enters V2 with a clean Overfit-Prune
floor; no V2 axis edit reintroduces a CH7 anti-pattern (the V2 fold
packet is mechanical relabel + clarifying gloss + by-number
enumeration; zero new behavioral admission).

### §2.3 — CH1 REVISE — wave-numbering reconciliation (load-bearing)

CH1 V1 finding: THREE distinct orderings surface across P3-B / P3-C
/ SPEC+P3-F (per CH1 §1.0 reconciliation table at
`research/p3/hardening/V1/CH1.md:43-57`):

- **P3-B** (W1=C-2 / W2=PRUNE-1 / W3=R4 / W4=PRUNE-2 / W5=R5 /
  W6=PRUNE-3 / W7=PRUNE-4 / W8=PRUNE-5 / W9=R6 / W10=R7 / W11=R8).
- **P3-C** (W1=PRUNE-1 / W2=C-2 / W3=C-3(R4+R5 fused) / W4=PRUNE-2 /
  W5=PRUNE-3 / W6=PRUNE-4 / W7=PRUNE-5 / W8=R6 / W9=R7-direct /
  W10=R7-typed / W11=R8).
- **SPEC + P3-F** (W1=C-2+PRUNE-1 fused / W2=R4 / W3=R5 / W4=PRUNE-2
  / W5=PRUNE-3 / W6=PRUNE-4 / W7=PRUNE-5 / W8=R6 / W9=R7-direct+typed
  fused / W10=R8 / W11=Close ceremony).

Binding determination: SPEC §2 (`SPEC.md:233-248`) + P3-F §1.2
(`p3f:39-52`) is the binding ordering per `PASS-3-SYNTHESIS-PLAN.md
§2` P3-F row ("P3-F additionally drafts
`restart/skinny/tranches/sk-v{N}/SPEC.md` +
`restart/skinny/tranches/sk-v{N}/DISPATCH-PROMPT.md`"). P3-B + P3-C
must rebind to it at V2.

### §2.4 — CH2 REVISE — Lock 14 v+1 strict-read per-candidate tightening

CH2 V1 finding: 2 binding REVISEs against the Lock 14 v+1 "must
exercise at least one non-JSON consumer or record a measured
deletion/rejection" gate at `LOCKS.md:259`:

- **C3 `digit_block_simd_accumulate`** (`CH2.md §1.3`): JSON
  direct-plane number consumers (canada / mesh / numbers /
  marine_ik) land in W9 (R7 JSON direct + typed re-admit); the CSS L4
  `<number>` consumer lands in W8 (R6 CSS L4 re-admit). If C3 admits
  in W9 WITHOUT the CSS L4 `<number>` consumer in the SAME wave, then
  per Lock 14 v+1 it is NEUTRAL-PENDING-CONSUMER. V2 discharge: bind
  CSS L4 consumer to SAME wave OR fail-closed PENDING flag OR
  bbnf-simd checkasm row exercising CSS-permissive
  `byte_class_from_range_64`.
- **C4 `unicode_escape_neon_nibble_decode`** (`CH2.md §1.4`):
  JSON `\uXXXX` is FIXED 4-nibble hex per RFC 8259 §7; CSS L4 escape
  per CSS Syntax §4.3.7 is VARIABLE 1-6 hex digits with OPTIONAL
  whitespace terminator — structurally distinct SIMD body shapes.
  P3-A C4's named CSS L4 escaped-ident consumer does NOT exercise the
  `unescape_uxxxx_x8_neon` SIMD body being admitted. V2 discharge:
  name a same-shape non-JSON consumer (Sheets / BBNF-self) OR carve
  C4 as JSON-only-by-shape with measured-rejection record OR paired
  C4b for CSS variable-width form.

Per-wave generic-crate ledger (CH2 §1.11) is clean: zero waves admit
JSON policy into a generic crate under V1 SPEC text. The two REVISEs
are per-candidate same-wave-consumer tightening at P3-A only.

### §2.5 — CH3 REVISE — SPEC §15 by-number enumeration discipline

CH3 V1 finding: 3 audit-trail surface-area items in SPEC §15
(substantive regression-vector integrity intact; per P3-E §3
per-wave-falsifiability-gate `git grep -n "REDRESS-{N}\|Item {N}"`
against per-wave MUST-NOT-RE-OPEN ids requires by-number
enumeration):

- **REVISE-1** (`CH3.md §2.REVISE-1`): SPEC §15 Specific-REDRESS list
  enumerates 11 categories spanning REDRESS 16-126 but omits the 28
  AUDIT-FALSIFIED admit-row revert ledger by-number (REDRESS 131-135
  + 141 + 143 + 145-153 + 154-158 + 160). The audit-overlay
  pre-block at `SPEC.md:1116` category-binds the 28 rows
  ("Any row currently AUDIT-FALSIFIED requires fresh material
  differential evidence to re-admit"); the §15 enumeration gap is
  surface-area only.
- **REVISE-2** (`CH3.md §2.REVISE-2`): SPEC §15 omits SK-V10 measured-
  rejected PERMANENT items REDRESS 102/103/106/108 by-number. Items
  103/106/108 are PERMANENT-PRE-BLOCK per P3-E §2.1; the
  audit-overlay pre-block does NOT bind them (they are not
  AUDIT-FALSIFIED, they are measured-rejected).
- **REVISE-3** (`CH3.md §2.REVISE-3`): SPEC §4 W1 PRUNE-1 revert
  manifest describes 22 rows by category (W14.1-5 + W13.* + W15.1)
  not by REDRESS item id at `SPEC.md:430` — weakens the W1 entry-gate
  audit trail per P3-E §3.

ZERO PERMANENT-route re-introductions; ZERO P-1..P-7 pattern
recurrences in any wave plan; ZERO silent re-opens of P3-E
PERMANENT-PRE-BLOCK routes verified across W0..W11.

### §2.6 — CH5 REVISE — substrate-union + naming discipline

CH5 V1 finding: substrate union HOLDS across W0..W11 (verified via
P3-A 8/8 candidates carrying explicit `substrate_target` per Lock 1
v+1 declaration triple at `LOCKS.md:73-82`; six of eight introduce
NO retained substrate; two of eight C2 + C8 are explicitly
retention-*subtractive*). Track 1 ≡ Track 2 anti-coupling gate
(`track2_entry_point` column per `SPEC.md:135` + `SPEC.md:1075` +
`SPEC.md:930` + `SPEC.md:991` + P3-D §2.2 + §3.5) is the strongest
novel SK-V14 telemetry instrument. NF-CH6-4 canonical-name binding
honoured 5x. NO renamed scanners per Lock 1 v+1 spirit.

Three clarifying-edit REVISEs:

- **F-V2-CH5-1** (CH5.md §X.Y): SPEC §10 W7 module name
  `same_substrate_union` carries naming proximity to the SK-V9
  retired retained-class-column-union hypothesis (REDRESS 96/97/98).
  V2 discharge: inline gloss distinguishing the W7 enforcement module
  from the SK-V9 retired data structure; promote REDRESS 96-98
  PERMANENT pre-block from parenthetical at `SPEC.md:822` to a
  top-level pre-blocked-routes bullet.
- **F-V2-CH5-2** (CH5.md §X.Y): SPEC §11 + §12 + §13 Stage-0
  deferral clauses need explicit inheritance-chain note (5-step
  inference from SPEC §1 non-negotiable → §11/§12/§13 admit-wave
  clause → P3-A C5 / P3-B W9 / P3-F §1.3.3 → S-P2 V3 §6.3 binding
  → 12 consumer-dependency primitive census).
- (the third clarifying note folds into F-V2-CH5-1 + F-V2-CH5-2;
  CH5 V1 surface is 24/27 ACCEPT — the three REVISEs are the
  fold-packet items at the lens-aggregator level).

### §2.7 — CH6 REVISE — F-V2-P1ABC-RERECORD Stage-0 unconditional binding (LOAD-BEARING)

CH6 V1 finding (load-bearing): THREE-WAY DIVERGENCE on
F-V2-P1ABC-RERECORD Stage-0 binding:

- **P3-A C5** (`p3a:124`): "Per `[no-deferrals]`, C5 ships in any
  wave admitting C1/C3/C7" — CONDITIONAL on C1/C3/C7 shortlist
  admission.
- **P3-B W9** (`p3b:256, 354-355`): "Stage 0 of W9 per S-P2 §6.3
  binding … Stage 0 lands in W9 [regardless of consumer list]" —
  UNCONDITIONAL on W9.
- **SPEC §11 W8 + §12 W9 + §13 W10** (`SPEC.md:856, 873, 916, 933,
  975, 993`): "W8 plan does NOT include Stage-0 F-V2-P1ABC-RERECORD
  UNLESS it admits one of the 12 consumer-dependency primitives" —
  CONDITIONAL on the 12-list per S-P2 V3 §6.3 verbatim.

The SPEC's conditional "UNLESS" language opens an orphan-kernel
hole: if W8 (R6 CSS L4 re-admit) admits CSS L4 primitives that are
NOT in the 12-list, then per `SPEC.md:856` W8 does NOT ship
F-V2-P1ABC-RERECORD — but C5 is itself a load-bearing shortlist
candidate (P3-A §2 row 5; one of 8 SPEC interventions). V2
discharge per p3a:180 binding: actual trigger is "first wave
admitting any of {P3-A C1, C3, C7}" which (under the SPEC ordering
W1=C-2+PRUNE-1 / W2=R4 / W3=R5 / W4=PRUNE-2 / W5=PRUNE-3 / W6=PRUNE-4
/ W7=PRUNE-5 / W8=R6 / W9=R7 / W10=R8 / W11=Close) resolves to
**W10** (parse_only distinct path admits the dispatch-envelope
candidates).

Other CH6 sub-tests PASS:
- 12/12 revert protocols per wave (CH6.md §1.3).
- No-deferrals language at `SPEC.md:220` ("No deferrals: a wave
  cannot close on 'wired', 'advisory', 'future consumer',
  'integrated', or 'paper close' language without measured
  evidence").
- CH7-V2 verb-tense discipline embedded at `SPEC.md:227`.
- Measurement-closure 9/12 strict ACCEPT (W8/W9/W10 carry the
  Stage-0 conditional concern, not a measurement-closure failure
  per se).

## §2.8 — Wave-numbering reconciliation cross-table (CH1 + CH6 convergent finding)

Per CH1 §1.0 + CH6 §1.6, the three-way divergence is CONVERGENT
between two lenses; the V2 fold for F-V2-CH1-1 and F-V2-CH6-2 is one
mechanical relabel exercise:

| Slot | P3-B | P3-C | SPEC §2 + P3-F | V2 binding source |
|---|---|---|---|---|
| W0 | Baseline + Telemetry Lock | Baseline + Telemetry Lock | Baseline Profile + Telemetry Lock | converges across all three |
| W1 | C-2 Comparator Rebind | C-5 PRUNE-1 | Comparator Rebind + Per-Iter Equality + PRUNE-1 (FUSED) | **SPEC binds** |
| W2 | C-5 PRUNE-1 | C-2 | regen-css xtask (R4) | **SPEC binds** |
| W3 | C-3 R4 | C-3 (R4 + R5 BOTH) | Production CSS Corpora (R5) | **SPEC binds** |
| W4 | C-5 PRUNE-2 | C-5 PRUNE-2 | PRUNE-2 | converges |
| W5 | C-3 R5 | C-1 PRUNE-3 | PRUNE-3 (Lock-14 refactor) | **SPEC binds** |
| W6 | C-1 PRUNE-3 | C-1 PRUNE-4 (9 sub-waves) | PRUNE-4 (9 sub-waves) | **SPEC binds** |
| W7 | C-1 PRUNE-4 (9 sub-passes) | C-4 PRUNE-5 | PRUNE-5 | **SPEC binds** |
| W8 | C-4 PRUNE-5 | R6 CSS L4 re-admit | CSS L4 Re-Admit (R6) | **SPEC binds** |
| W9 | R6 CSS L4 + F-V2 Stage 0 | R7-direct | JSON Direct + Typed Re-Admit (R7 FUSED) | **SPEC binds** |
| W10 | R7 JSON direct + typed | R7-typed | JSON parse_only Distinct Path (R8) | **SPEC binds** |
| W11 | R8 JSON parse_only | R8 | Close + Alpha Feedback (ceremony) | **SPEC binds** |

V2 mechanical discharge:
- P3-B §2.1 wave manifest at `p3b-wave-sequencing.md:73-85` + all
  per-wave §2.X detail sections: rewrite to SPEC ordering. Per-wave
  file paths shift slots; gate content carries.
- P3-C §1.2 wave manifest at `p3c-falsifiability-gates.md:26-37` +
  per-wave §2.0..§2.11 gate sections: rewrite section headers to
  SPEC ordering. NONE of the gate content changes; only the wave
  numbers + section letters.
- P3-A is wave-number-agnostic — no edit required. The §2.2
  architectural-sequencing-carry-forward note at `p3a:193-199`
  remains correct under SPEC ordering.

## §3 — Cross-lens convergence findings

### §3.1 — Wave-numbering reconciliation (CH1 + CH5 + CH6 convergent)

The wave-numbering divergence surfaces simultaneously in three
lenses:
- CH1 §1.0 + §6.1: three-way (P3-B / P3-C / SPEC+P3-F).
- CH5: implicit (the SPEC §10 W7 module-name `same_substrate_union`
  REVISE assumes SPEC ordering — its placement at W7 is contingent
  on the SPEC wave manifest binding).
- CH6 §1.6: three-way (P3-A C5 conditional / P3-B W9 unconditional /
  SPEC+P3-F conditional with "UNLESS").

The mechanical V2 fold (F-V2-CH1-1 / F-V2-CH6-2) discharges the
P3-B + P3-C side; the F-V2-CH6-1 fold discharges the SPEC §11/§12/§13
side. After V2, all three artefacts (P3-B, P3-C, SPEC) carry the same
SPEC §2 ordering; F-V2-P1ABC-RERECORD Stage-0 binds unconditionally
to W10 (per p3a:180 actual trigger resolution under SPEC ordering).

### §3.2 — F-V2-P1ABC-RERECORD Stage-0 binding closure (CH6 load-bearing)

Per S-P2 V3 §6.3 verbatim binding (preserved at
`HARDENING-S-P2-V3-CONSOLIDATED.md:504-529`):

> "Stage 0 of the first SK-V14 implementation wave admitting any
> dispatch-envelope-internal primitive."

The "first SK-V14 implementation wave" under SPEC ordering is W1
(C-2+PRUNE-1 fused), but W1 is gate/harness/revert-only — no
dispatch-envelope-internal primitive admits there. The first
behavioral wave that admits any of the 12 consumer-dependency
primitives is W10 (R8 JSON parse_only distinct path — the
parse_only-distinct-path admission is the first dispatch-envelope
behavioral edit). V2 binds Stage-0 to W10 UNCONDITIONALLY per p3a:180.

### §3.3 — C3 + C4 Lock 14 v+1 strict-read convergence (CH2)

The two CH2 REVISEs are unrelated to wave-numbering and
unrelated to Stage-0 binding; they are per-candidate same-wave-
consumer tightening at P3-A only. V2 discharge is P3-A §2 cell (c)
edits; no SPEC / P3-B / P3-C edit required.

### §3.4 — SPEC §15 enumeration discipline (CH3)

The 3 CH3 REVISEs are surface-area improvements only; substantive
regression-vector integrity intact. V2 discharge is SPEC §15 +
SPEC §4 W1 Task 6a expansion; no P3 axis edit required.

### §3.5 — CH4 + CH7 ACCEPT (cost + overfit-prune floor clean)

Wave count + LOC budgets + audit-overlay column + Stage-A target
naming all correct. Cohort enters V2 with a clean cost-axis floor
and a clean Overfit-Prune floor; V2 fold packet is mechanical
relabel + clarifying gloss + by-number enumeration + per-candidate
tightening; zero new behavioral admission.

## §4 — V2 fold packet (10 items enumerated; per-artefact line ranges; severity)

### §4.1 — Load-bearing items (3)

**F-V2-CH1-1 / F-V2-CH6-2 (CONVERGENT) — wave-numbering reconciliation**
- Severity: LOAD-BEARING (mechanical, but the convergent finding
  across CH1 + CH6).
- Artefacts: `p3b-wave-sequencing.md §2.1` (lines 73-85) + per-wave
  §2.X detail sections; `p3c-falsifiability-gates.md §1.2` (lines
  26-37) + per-wave §2.0..§2.11 gate sections.
- Action: section relabel to SPEC §2 ordering (W1=C-2+PRUNE-1 fused
  / W2=R4 / W3=R5 / W4=PRUNE-2 / W5=PRUNE-3 / W6=PRUNE-4 / W7=PRUNE-5
  / W8=R6 / W9=R7-direct+typed fused / W10=R8 / W11=Close). Gate
  content preserved verbatim.
- Discharge prediction: 100 % CH1 + 100 % CH6 post-fold.

**F-V2-CH6-1 — F-V2-P1ABC-RERECORD Stage-0 unconditional binding**
- Severity: LOAD-BEARING (closes orphan-kernel hole).
- Artefacts: `SPEC.md §11` (lines 856 + 873) + `§12` (lines 916 +
  933) + `§13` (lines 975 + 993).
- Action: REMOVE the "UNLESS it admits one of the 12 consumer-
  dependency primitives" language; pin F-V2-P1ABC-RERECORD Stage-0
  to ONE NAMED WAVE UNCONDITIONALLY per S-P2 V3 §6.3 verbatim. Per
  p3a:180 binding, actual trigger is "first wave admitting any of
  {P3-A C1, C3, C7}" → resolves to W10 (parse_only distinct path)
  under SPEC ordering.
- Discharge prediction: closes CH6 §1.6 paper-close vulnerability.

### §4.2 — Clarifying items (2)

**F-V2-CH5-1 — `same_substrate_union` gloss + REDRESS 96-98 promotion**
- Severity: LIGHT (clarifying).
- Artefacts: `SPEC.md §10 W7` (line ~822 parenthetical + module
  name surface).
- Action: inline gloss distinguishing the W7 enforcement module from
  the SK-V9 retired retained-class-column-union data structure;
  promote REDRESS 96-98 PERMANENT pre-block from parenthetical to
  top-level pre-blocked-routes bullet.

**F-V2-CH5-2 — Stage-0 deferral inheritance-chain note**
- Severity: LIGHT (clarifying).
- Artefacts: `SPEC.md §11` + `§12` + `§13` (lines ~856 + ~916 +
  ~975).
- Action: explicit inheritance-chain note for Stage-0 deferral
  (5-step inference from SPEC §1 → §11/§12/§13 → P3-A C5 / P3-B W9
  / P3-F §1.3.3 → S-P2 V3 §6.3 → 12 primitive census). Folded with
  F-V2-CH6-1 since both touch the same §11/§12/§13 region.

### §4.3 — CH3 enumeration items (3)

**F-V2-CH3-1 — SPEC §15 AUDIT-FALSIFIED 28-row revert ledger by-number**
- Severity: LIGHT.
- Artefacts: `SPEC.md §15` (lines 1091-1106; add sub-clause between
  line 1106 and line 1108).
- Action: add "AUDIT-FALSIFIED admit-row revert ledger" sub-clause
  enumerating 28 items (REDRESS 131-135 + 141 + 143 + 145-153 +
  154-158 + 160) by-number with R6/R7/R8 NAMED framing change
  requirements (per CH3 V1 §2.REVISE-1 fold prescription).

**F-V2-CH3-2 — SPEC §15 SK-V10 PERMANENT items by-number**
- Severity: LIGHT.
- Artefacts: `SPEC.md §15` (between line 1102 and line 1103).
- Action: add SK-V10 sub-clause enumerating REDRESS 102/103/106/108
  PERMANENT items by-number with binding wave for each (W10 R8 for
  102; W9 R7 for 103/106/108).

**F-V2-CH3-3 — SPEC §4 W1 PRUNE-1 Task 6a enumeration**
- Severity: LIGHT.
- Artefacts: `SPEC.md §4` (W1 Tasks section; between line 421 and
  line 422).
- Action: add Task 6a enumerating 22 revert rows by REDRESS item id
  (5 parse_only + 6 direct + 11 typed per SYNTHESIS §0.2
  reconciliation).

### §4.4 — CH4 clarity item (1)

**F-V2-CH4-1 — W6 sub-wave 810-min cumulative cap restatement**
- Severity: LIGHT (clarity).
- Artefacts: `SPEC.md §9` W6 sub-wave table header.
- Action: one-line footnote restating 810-min cumulative cap (the
  cap exists verbatim at `SPEC.md:243`; restatement is readability
  only). NON-blocking.

### §4.5 — CH2 per-candidate items (2)

**F-V2-CH2-1 — C3 CSS L4 number consumer same-wave binding**
- Severity: LIGHT.
- Artefacts: `p3a-candidate-shortlist.md §2 C3` cell (c) at
  `p3a:88-98`.
- Action: 3 options per CH2 V1 §1.3 — (i) bind CSS L4 `<number>`
  consumer to SAME wave that admits the SIMD body; OR (ii) carry
  explicit NEUTRAL-PENDING-CONSUMER flag with same-wave fail-closed
  clause; OR (iii) bind via `bbnf-simd` test surface as the non-JSON
  consumer (a checkasm row exercising CSS-permissive
  `byte_class_from_range_64`).

**F-V2-CH2-2 — C4 shape-orthogonal escape resolution**
- Severity: LIGHT.
- Artefacts: `p3a-candidate-shortlist.md §2 C4` cell (c) at
  `p3a:101-111`.
- Action: 3 options per CH2 V1 §1.4 — (i) name same-shape non-JSON
  consumer (BBNF-self under JSON-shape per `p2f:264` note 1); OR
  (ii) carve C4 as JSON-only-by-shape with measured-rejection record
  per Lock 14 v+1; OR (iii) admit paired C4b for CSS variable-width
  form via the existing primitive allowlist.

## §5 — V2 dispatch shape

### §5.1 — Artefacts touched at V2 (4)

| Artefact | V2 edit scope | Touch character |
|---|---|---|
| `sk-v14/SPEC.md` | §4 W1 Task 6a (CH3-3) + §9 W6 table header footnote (CH4-1) + §10 W7 gloss + REDRESS promotion (CH5-1) + §11/§12/§13 Stage-0 binding (CH6-1) + Stage-0 inheritance note (CH5-2) + §15 AUDIT-FALSIFIED ledger (CH3-1) + §15 SK-V10 items (CH3-2) | LOAD-BEARING (CH6-1) + LIGHT (5 other) |
| `p3b-wave-sequencing.md` | §2.1 wave manifest table + per-wave §2.X detail sections relabel | LOAD-BEARING relabel (mechanical) |
| `p3c-falsifiability-gates.md` | §1.2 wave manifest table + per-wave §2.0..§2.11 gate-section relabel | LOAD-BEARING relabel (mechanical) |
| `p3a-candidate-shortlist.md` | §2 C3 cell (c) (CH2-1) + §2 C4 cell (c) (CH2-2) | LIGHT (per-candidate) |

### §5.2 — Artefacts V1-LOCKED through V2 (4)

| Artefact | V1 verdict | V2 carry-forward |
|---|---|---|
| `p3d-telemetry-schema.md` | CH7 100 % ACCEPT (T3 LOAD-BEARING); CH4 ACCEPT; CH5 LOAD-BEARING `track2_entry_point` ACCEPT | byte-identical; zero V2 edit |
| `p3e-preblocked-ledger.md` | CH3 100 % ACCEPT across §2.1 + §2.2 + §2.3 + §2.4 + §3 + §4 | byte-identical; zero V2 edit |
| `p3f-spec-draft.md` | CH1 ACCEPT (SPEC binding-source); CH7 ACCEPT (P-1..P-7 fold + §2.Y NF-CH6-4 binding) | byte-identical; zero V2 edit |
| `sk-v14/DISPATCH-PROMPT.md` | CH1-CH7 deferred-ACCEPT through SPEC binding inheritance | byte-identical; zero V2 edit |

### §5.3 — V2 commit shape (predicted)

Single atomic V2 micro-fold commit per the S-P2 V3 V2-micro-fold
precedent shape:
- 4 artefacts edited: SPEC + P3-B + P3-C + P3-A.
- V2 dispatch context authored (V2 CHALLENGE-CONTEXT.md).
- 5 files in changeset (4 artefacts + V2 dispatch context).
- Zero Rust source files modified (research/synthesis layer only).

## §6 — Predicted §3Z LOCK trajectory

### §6.1 — V1 → V2 → V3 close path projection

| Cycle | Sub-axis aggregate | Orphan REVISE | Per-lens LOCK chain | Verdict |
|---|---:|---:|---|---|
| V1 (close) | ≈ 83.5 % | 10 line items / 5 REVISE lenses | 0 (first cycle) | **REVISE** |
| V2 (projected) | ≥ 95 % all 7 lenses | 0 | 0 (first ≥ 95 % cycle for CH1/CH2/CH3/CH5/CH6) | **first ceiling cycle** |
| V3 (projected) | ≥ 95 % confirming | 0 | 2-cycle LOCK for CH1/CH2/CH3/CH5/CH6; 3-cycle for CH4/CH7 | **cohort §3Z LOCK** |

### §6.2 — Per-lens trajectory projection

| Lens | V1 | V2 (predicted) | V3 (predicted) | LOCK cycle |
|---|---:|---:|---:|---|
| CH1 | 85 % (REVISE) | 100 % (wave-numbering relabel discharged) | 100 % | V3 (2-cycle) |
| CH2 | 81.8 % (REVISE) | 100 % (C3 + C4 P3-A tightening discharged) | 100 % | V3 (2-cycle) |
| CH3 | 90 % (REVISE) | 100 % (SPEC §15 + §4 enumeration discharged) | 100 % | V3 (2-cycle) |
| CH4 | 100 % (ACCEPT) | 100 % (W6 footnote discharged) | 100 % | V3 (3-cycle) |
| CH5 | 88.9 % (REVISE) | 100 % (`same_substrate_union` gloss + inheritance-chain note discharged) | 100 % | V3 (2-cycle) |
| CH6 | 73.7 % line / 89.5 % root (REVISE) | 100 % (Stage-0 unconditional binding to W10 discharged) | 100 % | V3 (2-cycle) |
| CH7 | 100 % (ACCEPT) | 100 % (zero V2 axis edit reintroduces a CH7 anti-pattern) | 100 % | V3 (3-cycle) |

### §6.3 — §3Z gate closure prediction at V3

- **Sub-axis-weighted aggregate ACCEPT-rate ≥ 95 % × 2 consecutive
  cycles (GATE-A):** V2 = 100 % + V3 = 100 % → GATE-A satisfied.
- **Zero orphan REVISE entries at the cohort level (GATE-B):** V2 =
  0 + V3 = 0 → GATE-B satisfied.
- **Per-lens two-cycle LOCK rule × 7 lenses (GATE-C):** all 7 lenses
  achieve 2-cycle LOCK (V2 + V3) for CH1/CH2/CH3/CH5/CH6 + 3-cycle
  LOCK (V1 + V2 + V3) for CH4/CH7 → GATE-C satisfied.

**Predicted §3Z LOCK firing event:** V3 cycle close. V≤5 ceiling
honoured with margin (V3 ≤ 5).

## §7 — Sources

V1 lens dispositions (all verified existing at write-time):

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH1.md` (326 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH2.md` (396 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH3.md` (222 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH4.md` (449 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH5.md` (538 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH6.md` (313 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH7.md` (449 lines)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md` (53 lines)

V1 P3 axis artefacts under review (HEAD `8f4756113`; seed `1dc4cd60c`):

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (316 lines; 8 candidates at ≤8 cap; NF-CH6-4 canonical-name binding; CF-3 3-gate admission cell; F-V2-P1ABC-RERECORD dependency on C1/C3/C7)
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (405 lines; 12 waves W0..W11 at ≤12 ceiling; W7 folds 9 PRUNE-4 sub-passes; 3 architectural sequencing constraints discharged; 3 S-P2 carry-forwards wired)
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (527 lines; 12 waves gated; ZERO unmeasurable-gate REJECTs; 75 corpus rows enumerated verbatim from RESULTS.md:49-124; R1+R2 bindings explicit; W7 samply attribution gate + W11 cargo asm gate)
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (168 lines; 31-column schema = 27 SK-V8 carry-forward + 4 SK-V14 NEW; R1+R2 acceptance criteria honored)
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (903 lines; 160 REDRESS entries classified = 45 PERMANENT + 47 WAVE-CONDITIONAL + 68 RESOLVED-OK; per-wave pre-block census; 3 binding clusters surfaced beyond watch-list)
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (245 lines)
- `restart/skinny/tranches/sk-v14/SPEC.md` (1137 lines; SK-V8 shape verbatim)
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (344 lines; triumvirate contract)

Prior-cycle precedents (format mirrors):

- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (668 lines; S-P2 §3Z LOCK aggregator + V2 micro-fold trajectory precedent)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md` (671 lines; V2 fold-packet authority shape mirror)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md` (659 lines; V1 opening-cycle aggregator shape mirror)

Binding authorities:

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3 + §5 + §6` (CH1-CH6 specialisations + S-P3 close binding + SPEC binding-source row)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) + `§3Z` (convergence rule) + `§8` (baseline-anchored measurement)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune lens definition; carry-forward from S-P0)
- `restart/skinny/tranches/sk-v14/research/p3/S-P3-DISPATCH-CONTEXT.md` (S-P3 dispatch spec)
- `restart/locks/LOCKS.md` (Lock 1 v+1 substrate-union manifest; Lock 14 v+1 grammar-neutrality + non-JSON-consumer admission gate at LOCKS.md:220-263; Lock 15 i-cache budget; Lock 16 v+1 SIMD/ASM allowlist + abstract-primitive declarations)

S-P2 §3Z LOCK carry-forward inheritance (`HARDENING-S-P2-V3-CONSOLIDATED.md §6`):

- §6.1 CF-3 admission 3-gate manifest discipline (P3-A §2.1 carries verbatim).
- §6.2 NF-CH6-4 canonical-name binding for long-string-body SIMD scan (P3-A C1 + SPEC §1:222 + SPEC §15:1110 carry verbatim).
- §6.3 F-V2-P1ABC-RERECORD Stage-0 wave commitment (V2 fold F-V2-CH6-1 closes the SPEC-side conditional-language gap; W10 binding per p3a:180 resolution).

V1 commit anchors:

- V1 axis commit: `1dc4cd60c` ("docs(sk-v14-p3-V1): atomic seed — 6 P3 artefacts + SPEC + DISPATCH-PROMPT + dispatch context").
- V1 dispatch-context commit: `8f4756113` ("docs(sk-v14-V4-multi): T-P1 V4 atomic micro-fold + S-P3 V1 CHALLENGE-CONTEXT").
- V1 hardening commit: this aggregator atomic commit (8 files: 7 V1 lens dispositions + this consolidated doc).

Bbnf source anchors (V1 HEAD-verified via CH7 §3 LAC-1E-12 mandate):

- `skinny/crates/bbnf-simd/src/scalar/` directory listing at V1 HEAD: 8 files (`bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`, `bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`, `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`, `mod.rs`, `swar_8byte.rs`); `byte_context_64.rs` + `bcax_64.rs` + `byte_class_from_range_64.rs` NOT YET PRESENT (Stage-A authoring targets per CH7 §1.4 framing).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22` (`scan_structurals`, C2 substrate-union typed-skip antecedent).
- `skinny/crates/runtime/src/grammars/json/scan.rs:32` (`scan_structurals_scalar`, scalar reference inherited from S-P2 V3).
- `skinny/crates/runtime/src/tape/mod.rs:92,94` + `skinny/crates/runtime/src/tape/assembler.rs:42` (substrate-union single-substrate verification carried from S-P2 V3 §4.5).
- `skinny/crates/runtime/src/grammars/json/generated.rs:33-237` (parse-attribution `cfg_attr(feature = "parse-attribution", inline(never))` plumbing — F-V2-P1ABC-RERECORD Stage-0 toggle target; 8 cfg_attr sites).
- `skinny/RESULTS.md` (185 lines; 75 corpus rows: 51 JSON + 24 CSS L4; comparator + per_iter_equality + audit_overlay_verdict + track2_entry_point columns enumerated for W0 telemetry lock).
- `skinny/REDRESS.md` (~5041 lines; 160 REDRESS entries enumerated in P3-E classification).

# CH3 REGRESSION (REDRESS) — SK-V14 S-P3 Synthesis-Plan V3 Disposition (LOCK-TRIGGER)

Date: 2026-05-23. Lens: CH3 per `PASS-3-SYNTHESIS-PLAN.md §3` row CH3 +
the S-P3 CHALLENGE V3 dispatch context's §2 V3-disposition focus at
`restart/skinny/tranches/sk-v14/research/p3/hardening/V3/CHALLENGE-CONTEXT.md:27`.

Scope of V3 cycle (per dispatch §2): verify the single V3 amendment
(F-V3-CH6-3 cosmetic-fold of P3-C `:36` §1.2 W10 manifest row + P3-C
`:423` §2.10 W10 exit-gate item 8 to mirror SPEC §13:982 UNCONDITIONAL
Stage-0 binding) introduces ZERO silent reopens of any P3-E §2.1
PERMANENT-PRE-BLOCK route; verify the 7 V2-LOCKED artefacts
(P3-A/P3-B/P3-D/P3-E/P3-F/SPEC/DISPATCH-PROMPT) carry **byte-identical**
across V2 → V3 HEAD (`867b0cd0b`); verify the SPEC §15 enumerations
(22 JSON revert manifest + 24 CSS L4 revert binding + SK-V10
102/103/106/108 PERMANENT-PRE-BLOCK) survive V3 verbatim; confirm
**§3Z 2-cycle LOCK** trigger condition (V2 100.0% + V3 ≥95% expected
→ cohort §3Z LOCK at V3 close).

## §0 — Disposition summary

- ACCEPT-rate: **15 / 15 per-§ rows = 100.0 %** (clears the §3Z ≥95 %
  convergence floor; **second consecutive cycle ≥95 % for CH3** per V2
  100.0 % → V3 100.0 % trajectory; per §3Z ≥95 % × 2 consecutive cycles
  binding, V3 is the **LOCK-trigger cycle** for CH3 within the cohort
  §3Z LOCK closing on V3 commit).
- REJECT count: 0.
- REVISE count: 0.
- ACCEPT-with-note count: 0.
- Critical findings: 0 silent re-opens of P3-E PERMANENT-PRE-BLOCK
  routes in the V3 cosmetic amendment; 0 silent reversals of prior V1
  or V2 CH3 dispositions; 0 wave clauses that silently re-attempt §2.1
  routes by another name; 0 P-1..P-7 pattern recurrences in any V3-added
  text; the V3 P3-C `:36` + `:423` edits mirror SPEC §13:982
  UNCONDITIONAL Stage-0 binding (the V2 CH6-1 fold that already
  CLOSES the P-5 scaffold-as-load-bearing route), so the V3 amendment
  CLOSES a residual conditional-Stage-0 cosmetic asymmetry rather than
  opening any new admit surface; the 7 V2-LOCKED artefacts confirmed
  byte-identical between V2 head (`f25c3af2e`) and V3 head (`867b0cd0b`)
  via `git diff --stat` returning empty for all 7 paths; SPEC §15 admit
  enumerations (22 JSON + 24 CSS L4 + SK-V10 102/103/106/108) preserved
  verbatim at V3 (SPEC.md lines 1098-1166 unchanged byte-identical).

Overall: the V3 cosmetic-fold preserves the **CH3-clean** posture
established at V2. The V3 amendment is the smallest possible coherence
improvement — a 2-line text refresh on P3-C (one wave-manifest row +
one exit-gate item) that mirrors a V2 SPEC binding already CH3-cleared
at V2 acceptance. Per §3Z 2-cycle LOCK binding (≥95 % × 2 consecutive
cycles), V3 confirms CH3 LOCK; per the cohort §3Z LOCK trajectory cited
in CHALLENGE-CONTEXT.md §0/§2/§3, V3 is the cohort §3Z LOCK-trigger
cycle (V2 closed first cohort-wide ≥95 % LOCK-eligible cycle with
5/7 first ≥95 % + 2/7 already 2-cycle LOCKed; V3 is the second
consecutive cycle that triggers cohort §3Z LOCK on V3 close).

## §1 — Per-V3-amendment + V2-LOCKED-artefact disposition table

| Artefact | V3 edit | V2-LOCKED carry | Disposition | Reason |
|---|---|---|---|---|
| F-V3-CH6-3 P3-C §1.2 W10 manifest row (`p3c:36`) | `Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding)` (replacing `Stage-0 F-V2-P1ABC-RERECORD if any consumer-dependency primitive admitted`) | n/a (V3-amended) | ACCEPT | The V3 edit replaces a conditional Stage-0 phrasing ("if any consumer-dependency primitive admitted") with the unconditional binding that already lives at SPEC §13:982 ("W10 plan MUST include Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 verbatim"). The change is purely textual / coherence-mirroring: the conditional phrasing was a V2 cosmetic residual that did not match the V2-LOCKED SPEC §13:982 binding (which the V2 CH6-1 fold already moved to UNCONDITIONAL). The amendment **CLOSES** a paper-conditional Stage-0 P-5 recurrence vector (the original conditional phrasing left textual room for "no consumer admitted → no Stage-0" which is the scaffold-as-load-bearing reopening) — never opens one. Verified via `git show 867b0cd0b -- p3c-falsifiability-gates.md`: single-line replacement at `:36`, no other content shifted. |
| F-V3-CH6-3 P3-C §2.10 W10 exit-gate item 8 (`p3c:423`) | `Stage-0 F-V2-P1ABC-RERECORD shipped UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding — W10 is the bound wave per the 5-step inheritance chain): cargo build + interactive samply record + cfg_attr flip at generated.rs:33-237 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands.` (replacing `If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, Stage 0 rerun is shipped per S-P2 V3 §6.3.`) | n/a (V3-amended) | ACCEPT | The V3 edit replaces a conditional exit-gate ("If admitting any of the 12 …, Stage 0 rerun is shipped") with the unconditional binding that already lives at SPEC §13:1000 ("F-V2-P1ABC-RERECORD Stage-0 SHIPPED UNCONDITIONALLY per S-P2 V3 §6.3 (W10 is the bound wave per the §13 entry-gate inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites landed in this wave's commit slice"). Same coherence-mirroring as `:36`. Discharges the V2 cosmetic-conditional Stage-0 residual at the W10 exit-gate surface; no new admit surface; the prefix-condition "If admitting any of the 12" cleared the gate of being load-bearing (the original phrasing allowed "0 consumers admitted → Stage-0 skipped"), and the V3 replacement **closes** that gate-skip path per the V2 CH6-1 + p3a:180 W10-bound-Stage-0 binding. No PERMANENT-route re-open vector. |
| `p3a-candidate-shortlist.md` (V2-LOCKED; no V3 edits) | none | byte-identical V2→V3 (verified `git diff --stat`) | ACCEPT | V2-LOCKED at V2 disposition (ACCEPT per V2 CH3 §1 row 4); zero V3 amendments → no V3 regression risk; the V2-cleared C3 same-wave consumer (bbnf-simd checkasm `byte_class_from_range_64`) + C4 same-shape consumer (BBNF-self string-escape) + variable-width CSS \\HEXHEX measured-rejection carve-out all preserved at V3. |
| `p3b-wave-sequencing.md` (V2-LOCKED; no V3 edits) | none | byte-identical V2→V3 (verified `git diff --stat`) | ACCEPT | V2-LOCKED at V2 disposition (ACCEPT per V2 CH3 §1 row 5); zero V3 amendments → no V3 regression risk; the V2-cleared per-wave pre-block list at p3b:339-351 preserved byte-identical, including W10 pre-block REDRESS 89/90 routing and the W9 W1-only-dependency intentional parallel-eligibility per SPEC §0.1. |
| `p3c-falsifiability-gates.md` (V3 amended) | F-V3-CH6-3 at `:36` + `:423` (2 lines) | rest of file byte-identical V2→V3 (verified `git diff` shows only 2 lines changed within the 537-line file) | ACCEPT | Beyond the F-V3-CH6-3 cosmetic-fold at `:36` + `:423`, the V3 amendments preserve the V2-LOCKED gate matrix verbatim — §1 fused C-2+PRUNE-1 (W1) + R7-direct+typed (W9) + R8 (W10) preserved; §2.11 W11 close ceremony preserved; §4 pre-blocked routes table at lines 498-513 preserved byte-identical (W0 telemetry; W1 REDRESS 119/120 + 88/89 HISTORY/perf-evidence; W2 + W3 REDRESS 28+33 + 96-98; W4 REDRESS 50-55, 60-72, 80, 82-84, 88, 89; W5 REDRESS 36-38, 85-86, 50-55, 60-72, 126; W6 same as W5 + REDRESS 49; W7 REDRESS 49-55, 88, 89, 119, 120; W8 REDRESS 28+33, 96-98, 119, 120; W9 REDRESS 50-55, 60-72, 80, 82-84, 88, 89, 119, 120, 126; W10 same as W9 + REDRESS 49; W11 none). The V3 row count = 75 corpus rows preserved per the V2 full-table maintain budget. Zero PERMANENT-route re-open vector in either V3 edit. |
| `p3d-telemetry-schema.md` (V1-LOCKED; no V2/V3 edits) | none | byte-identical V2→V3 (verified `git diff --stat`) | ACCEPT | V1-LOCKED at V1 disposition (3-cycle LOCK extension from V1 → V2 → V3); zero V3 amendments → no V3 regression risk. The 31-column schema (27 SK-V8 carry + 4 SK-V14 NEW) + audit-overlay gate-enforcement column preserved at V3 — remains the structural defence against P-4 gate-relabel-as-admit. |
| `p3e-preblocked-ledger.md` (V1-LOCKED; no V2/V3 edits) | none | byte-identical V2→V3 (verified `git diff --stat`) | ACCEPT | V1-LOCKED at V1 disposition (3-cycle LOCK extension from V1 → V2 → V3); zero V3 amendments → no V3 regression risk. The 160-item REDRESS classification (45 PERMANENT + 47 WAVE-CONDITIONAL + 68 RESOLVED-OK) + §2.4 per-wave pre-block census + §3 falsifiability + §4 meta-binding all preserved. |
| `p3f-spec-draft.md` (V1-LOCKED; no V2/V3 edits) | none | byte-identical V2→V3 (verified `git diff --stat`) | ACCEPT | V1-LOCKED at V1 disposition (3-cycle LOCK extension from V1 → V2 → V3); zero V3 amendments → no V3 regression risk. The §1.5 P-1..P-7 SPEC §10 fold + §1.6 REDRESS watch-list preserved; the V1 ACCEPT-WITH-NOTE on the §1.6 watch-list was structurally addressed by the SPEC §15 V2 amendments (which themselves are V2-LOCKED at V3) so the watch-list still routes to the canonical SPEC §15 aggregate ledger as intended. |
| `SPEC.md` (V2-LOCKED; no V3 edits) | none | byte-identical V2→V3 (verified `git diff --stat`) | ACCEPT | V2-LOCKED at V2 disposition (ACCEPT per V2 CH3 §1 row 10); zero V3 amendments → no V3 regression risk; verified all critical per-section bindings preserved at V3 HEAD (1187 lines, identical to V2 HEAD): §1 Non-Negotiables unchanged; §2 Wave Manifest unchanged; §4 W1 Task 6a F-V2-CH3-3 manifest at lines 422-426 preserved; §11 W8 inheritance chain at line 863 preserved; §12 W9 inheritance chain at line 923 preserved; §13 W10 UNCONDITIONAL Stage-0 binding at line 982 + Task 5 line 990 + exit-gate line 1000 preserved (this IS the binding that V3 P3-C `:36` + `:423` cosmetically mirror); §15 Specific REDRESS list at lines 1098-1114 preserved (including SK-V10 102/103/106/108 enumeration at line 1110, the F-V2-CH3-2 fold); §15 AUDIT-FALSIFIED admit-row revert ledger at lines 1122-1162 preserved (the 22 JSON + 24 CSS L4 by-number enumeration, the F-V2-CH3-1 fold); §15 audit-overlay pre-block at lines 1164-1166 preserved. |
| `DISPATCH-PROMPT.md` (V1-LOCKED; no V2/V3 edits) | none | byte-identical V2→V3 (verified `git diff --stat`) | ACCEPT | V1-LOCKED at V1 disposition (3-cycle LOCK extension from V1 → V2 → V3); zero V3 amendments → no V3 regression risk. |
| SPEC §4 W1 pre-blocked routes (line 443-448) at V3 HEAD | unchanged from V2 (P-2, P-4, REDRESS 119/120 LIFTED-only patterns) | byte-identical V2→V3 | ACCEPT | Pre-blocked routes clause preserved byte-identical from V2. No V3 amendment touches W1 pre-block surface. |
| SPEC §11 W8 pre-blocked routes (line 886-893) at V3 HEAD | unchanged from V2 (P-1, P-3, fact-stream asymmetry, plane mismatch, REDRESS 119/120 history, REDRESS 82-83 tiny probe) | byte-identical V2→V3 | ACCEPT | Pre-blocked routes clause preserved byte-identical from V2. No V3 amendment touches W8 pre-block surface. |
| SPEC §12 W9 pre-blocked routes (line 946-953) at V3 HEAD | unchanged from V2 (REDRESS 66-72, 80, 119/120, 126; three orthogonal SIMD bodies binding) | byte-identical V2→V3 | ACCEPT | Pre-blocked routes clause preserved byte-identical from V2. No V3 amendment touches W9 pre-block surface. |
| SPEC §13 W10 pre-blocked routes (line 1006-1013) at V3 HEAD | unchanged from V2 (P-4 recurrence; REDRESS 82-84; orthogonal codepaths) | byte-identical V2→V3 | ACCEPT | Pre-blocked routes clause preserved byte-identical from V2; the V3 P3-C cosmetic mirror to SPEC §13:982 UNCONDITIONAL Stage-0 binding does NOT touch the W10 pre-block clause — the binding it mirrors is at line 982 (entry gate), not lines 1006-1013 (pre-block routes). |
| SPEC §15 SK-V10 102/103/106/108 enumeration (line 1110) at V3 HEAD | unchanged from V2 (F-V2-CH3-2 bullet preserved verbatim) | byte-identical V2→V3 | ACCEPT | Verified at SPEC.md:1110 — full bullet text preserved verbatim from V2 ("REDRESS 102/103/106/108 PERMANENT-PRE-BLOCK (SK-V10 measured-rejected items): REDRESS 102 (parse_only fact-stream-as-admit) PERMANENT-PRE-BLOCK per P3-E §2.1; binding wave W10 (R8) … REDRESS 103/106/108 PERMANENT-PRE-BLOCK per P3-E §2.1; binding wave W9 (R7) for direct/typed re-admit — these are measured-rejected (NOT AUDIT-FALSIFIED) … their PERMANENT status follows from measured-rejection history per SK-V10 close, not from audit-overlay."). The V2 distinction between PERMANENT-PRE-BLOCK and AUDIT-FALSIFIED status preserved at V3 — the audit-overlay pre-block does NOT bind these items, only their measured-rejection history does. Zero PERMANENT-route re-open vector. |
| SPEC §15 AUDIT-FALSIFIED admit-row revert ledger (lines 1122-1162) at V3 HEAD | unchanged from V2 (F-V2-CH3-1 sub-section preserved verbatim) | byte-identical V2→V3 | ACCEPT | Verified at SPEC.md:1122-1162 — full sub-section text preserved verbatim from V2: header reconciliation "22 JSON items + 24 CSS L4 items = 46 by-number" at line 1122; JSON parse_only enumeration (REDRESS 154-158 = 5) at line 1133; JSON direct enumeration (REDRESS 131-135 + 141 = 6) at line 1138; JSON typed enumeration (REDRESS 143 + 145-153 + 160 = 11) at line 1144; CSS L4 24-feature binding at lines 1150-1157 bound to W4 PRUNE-2 revert + W8 R6 re-admit framing; gate-enforcement clause at lines 1159-1162 ("The 22 JSON revert rows … + 24 CSS L4 revert rows are gate-enforced by the audit-overlay column below; each row carries `audit_overlay_verdict=AUDIT-FALSIFIED` post-W1 / post-W4 with the validation-pack §reference cited"). Zero PERMANENT-route re-open vector. |

## §2 — V2 → V3 disposition reconciliation

Per dispatch §2 V3 disposition focus, the V2 CH3 dispositions (which
discharged all 3 V1 CH3 REVISEs) all carry-clean at V3:

| V2 disposition | V2 outcome | V3 carry status | V3 disposition |
|---|---|---|---|
| F-V2-CH3-1 SPEC §15 AUDIT-FALSIFIED admit-row revert ledger by-number (22 JSON + 24 CSS L4) | V2 ACCEPT (V2 CH3 §1 row 1) | byte-identical at V3 HEAD (SPEC.md:1122-1162 verbatim preserved); no V3 SPEC edits | ACCEPT (clean carry; the 22 JSON + 24 CSS L4 by-number enumeration continues to discharge P3-E §3 per-wave-falsifiability-gate `git grep -n "REDRESS-{N}"` binding at V3) |
| F-V2-CH3-2 SPEC §15 SK-V10 PERMANENT-PRE-BLOCK 102/103/106/108 enumeration | V2 ACCEPT (V2 CH3 §1 row 2) | byte-identical at V3 HEAD (SPEC.md:1110 verbatim preserved); no V3 SPEC edits | ACCEPT (clean carry; the PERMANENT-PRE-BLOCK-vs-AUDIT-FALSIFIED distinction preserved at V3 — audit-overlay pre-block continues to NOT bind these items per SK-V10 measured-rejection history) |
| F-V2-CH3-3 SPEC §4 W1 Task 6a 22-row revert manifest by-item | V2 ACCEPT (V2 CH3 §1 row 3) | byte-identical at V3 HEAD (SPEC.md:422-426 verbatim preserved); no V3 SPEC edits | ACCEPT (clean carry; the 22-row by-REDRESS-id manifest continues to discharge the W1 entry-gate `git grep -n "REDRESS-{N}\|Item {N}"` per-row enumeration requirement at V3) |
| 8 V1 binding clusters all routed through SPEC §15 explicit-by-number enumeration | V2 ACCEPT × 8 (V2 CH3 §4) | byte-identical at V3 HEAD (all enumeration entries preserved) | ACCEPT (clean carry; the binding-cluster integration verified V2 holds at V3) |

**Disposition reconciliation verdict:** all V2 CH3 dispositions
discharge-clean at V3; ACCEPT-rate holds at 100.0 % (V2) → 100.0 % (V3).
Per §3Z ≥95 % × 2 consecutive cycles binding, V3 is the **second
consecutive cycle ≥95 % for CH3** → CH3 LOCK triggers at V3 close
within the cohort §3Z LOCK trajectory.

## §3 — V3 silent-reopen scan (delta vs V2)

Per dispatch §2 V3 focus "no V3 edit re-opens REDRESS routes", a full
silent-reopen scan across V3 amendments:

| V3 amendment | New text adds | PERMANENT route at risk? | Silent-reopen risk | Verdict |
|---|---|---|---|---|
| P3-C `:36` (§1.2 W10 manifest row) | "UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding)" replacing "if any consumer-dependency primitive admitted" | NONE (UNCONDITIONAL binding strengthens Stage-0 obligation; the conditional phrasing was a P-5 scaffold-as-load-bearing recurrence vector because it permitted "0 consumers admitted → Stage-0 skipped"; the V3 replacement CLOSES that gate-skip path) | NONE | CLEAN |
| P3-C `:423` (§2.10 W10 exit-gate item 8) | "Stage-0 F-V2-P1ABC-RERECORD shipped UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding — W10 is the bound wave per the 5-step inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands." replacing "If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, Stage 0 rerun is shipped per S-P2 V3 §6.3." | NONE (same reasoning as `:36`; the V3 replacement makes the exit-gate Stage-0 binding load-bearing per the V2 SPEC §13:982 + §13:1000 unconditional binding; CLOSES the conditional gate-skip path, never opens new admit surface) | NONE | CLEAN |
| V3 CHALLENGE-CONTEXT.md (new file at V3 HEAD; not part of P3 artefact surface) | dispatch context for V3 LOCK-trigger cycle (orchestrator artefact) | NONE (orchestrator artefact, not an artefact under CH3 disposition; the file itself is process metadata for V3 lens dispatch, no admit surface) | NONE | CLEAN |

**V3 silent-reopen scan verdict:** ZERO silent re-opens of any P3-E
§2.1 PERMANENT-PRE-BLOCK route across the V3 amendment. The single
F-V3-CH6-3 fold to P3-C (`:36` + `:423`) is purely cosmetic-coherence
mirror to the V2-LOCKED SPEC §13:982 UNCONDITIONAL Stage-0 binding —
the V3 edit STRENGTHENS the P-5 scaffold-as-load-bearing defence
(closes a conditional gate-skip residual) without touching any
pre-block surface. The 7 V2-LOCKED artefacts (P3-A/P3-B/P3-D/P3-E/P3-F/
SPEC/DISPATCH-PROMPT) carry byte-identical V2 → V3, preserving the V2
silent-reopen-clean posture verbatim.

## §4 — Cohort §3Z LOCK trajectory confirmation

Per dispatch §0 + §2 + §3, V3 is the **cohort §3Z LOCK-trigger cycle**.
The CH3 contribution to the cohort §3Z LOCK trajectory:

| Cycle | CH3 ACCEPT-rate | §3Z floor (≥95%)? | Consecutive ≥95% count | Disposition |
|---|---|---|---|---|
| V1 | 90.0% (3 REVISEs prescribed) | NO (below floor by 5pp) | 0 | V1 first-pass below floor |
| V2 | 100.0% (31/31; all 3 V1 REVISEs discharged) | YES (5pp above floor) | 1 | V2 first ≥95% cycle for CH3 |
| **V3** | **100.0% (15/15; V2 dispositions carry-clean + V3 cosmetic fold ACCEPT)** | **YES (5pp above floor)** | **2** | **V3 = second consecutive ≥95% → CH3 §3Z LOCK confirmed** |

**Cohort §3Z LOCK trajectory verdict:** CH3 contributes a confirmed
**2-cycle LOCK** at V3 close per §3Z ≥95 % × 2 consecutive cycles
binding. Combined with the 7-lens cohort posture cited at dispatch §0
("V2 closed first cohort-wide ≥95 % LOCK-eligible cycle (5/7 first
≥95 % + 2/7 already 2-cycle LOCKed at V2). V3 is the second consecutive
≥95 % cycle that triggers cohort §3Z LOCK"), CH3 is part of the V3
LOCK-trigger cohort. Per §3 dispatch discipline ("LOCK-TRIGGER cycle
— minimum reasonable cap"), CH3 acceptance is the necessary condition
for cohort §3Z LOCK at V3 close per the V≤5 ceiling (V3 is well below
V5 ceiling, providing margin).

## §5 — LOCK confirmation

Per `ORCHESTRATOR.md` §3Z (cohort LOCK = ≥95 % × 2 consecutive cycles;
V ≤ 5 ceiling):

- CH3 V2 ACCEPT-rate: **100.0 %** (first ≥95 % cycle for CH3).
- CH3 V3 ACCEPT-rate: **100.0 %** (second consecutive ≥95 % cycle for
  CH3).
- §3Z 2-cycle LOCK condition satisfied for CH3 axis: **TRUE**.
- V≤5 ceiling margin: **2 cycles spare** (V3 well below V5 ceiling).
- Critical-defect count: **0**.
- Orphan-REVISE count: **0**.

**CH3 §3Z LOCK CONFIRMED at V3 close.** Within the cohort §3Z LOCK
trajectory cited in CHALLENGE-CONTEXT.md §0/§2/§3, CH3 contributes
its required ACCEPT to the cohort LOCK declaration that the V3
aggregator (`HARDENING-S-P3-V3-CONSOLIDATED.md`) will memorialise per
dispatch §4 ("THE COHORT §3Z LOCK DECLARATION DOCUMENT").

## §6 — Sources

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V3/CHALLENGE-CONTEXT.md` (V3 dispatch context; §2 V3 disposition focus on CH3 REGRESSION lens: "no V3 edit re-opens REDRESS; second consecutive ≥95% expected").
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH3.md` (V2 CH3 disposition; 100.0% ACCEPT-rate; all 3 V1 REVISEs discharged via F-V2-CH3-1/-2/-3 folds).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH3.md` (V1 CH3 disposition; 90.0% ACCEPT-rate; 3 REVISEs prescribed exact V2 fold text).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 (CH3 lens overlay: "does the P3-E pre-blocked ledger correctly enumerate every REDRESS route each wave must not re-open? Does any wave in P3-B silently re-open a pre-blocked route? Does the SPEC carry the full pre-block list…").
- `restart/prompts/ORCHESTRATOR.md` §3W + §3Z (cohort LOCK = ≥95 % × 2 consecutive cycles; V ≤ 5 ceiling).
- `restart/skinny/tranches/sk-v14/SPEC.md` (V3 HEAD `867b0cd0b`; 1187 lines; byte-identical to V2 HEAD `f25c3af2e`; V2-LOCKED).
  - SPEC §4 W1 Task 6a at lines 422-426 (F-V2-CH3-3 fold preserved).
  - SPEC §11 W8 entry-gate inheritance chain at line 863 (F-V2-CH6-1 preserved).
  - SPEC §12 W9 entry-gate inheritance chain at line 923 (F-V2-CH6-1 preserved).
  - SPEC §13 W10 UNCONDITIONAL Stage-0 binding at line 982 + Task 5 line 990 + exit-gate line 1000 (F-V2-CH6-1 preserved; this is the binding F-V3-CH6-3 cosmetically mirrors).
  - SPEC §15 SK-V10 102/103/106/108 enumeration at line 1110 (F-V2-CH3-2 fold preserved).
  - SPEC §15 AUDIT-FALSIFIED admit-row revert ledger at lines 1122-1162 (F-V2-CH3-1 fold preserved).
  - SPEC §15 audit-overlay pre-block at lines 1164-1166 (preserved).
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (V2-LOCKED; byte-identical V2→V3).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (V2-LOCKED; byte-identical V2→V3).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (V3 amended; 537 lines; F-V3-CH6-3 cosmetic fold at `:36` + `:423`; remainder byte-identical to V2).
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (V1-LOCKED; 3-cycle LOCK extension).
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (V1-LOCKED; 903 lines; 160-item REDRESS classification 45/47/68; per-wave pre-block census at §2.4).
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (V1-LOCKED).
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (V1-LOCKED).
- `restart/locks/LOCKS.md` (Lock 1 v+1 substrate-target/retention-lifetime/policy-owner; Lock 14 v+1 generated-output allowance; Lock 16 v+1 primitive-manifest gating).
- `skinny/REDRESS.md` (~5041 lines; 160-item ledger).

HEAD pin at evaluation time: **`867b0cd0b`** (V3 atomic cosmetic-fold
commit + V3 CHALLENGE-CONTEXT; V2 base HEAD `f25c3af2e` per dispatch §0
fold-packet authority; V3 SPEC + P3-A/B/D/E/F + DISPATCH-PROMPT
byte-identical to V2 base; only P3-C amended by 2 lines per
F-V3-CH6-3).

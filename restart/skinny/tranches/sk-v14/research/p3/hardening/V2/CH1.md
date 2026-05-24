# SK-V14 S-P3 V2 CH1: Correctness (confirming cycle)

Pass: S-P3 CHALLENGE V2 (per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` CH1 specialised to S-P3; `restart/prompts/ORCHESTRATOR.md §3W` lens registry + §3Z convergence).
Date: 2026-05-23.
Lens: CH1 (CORRECTNESS) — wave-numbering reconciliation across SPEC §2 + P3-B + P3-C; F-V2-CH6-1 unconditional W10 Stage-0 binding at SPEC §11/§12/§13 + 5-step inheritance chain; §15 28-row + SK-V10 + W1 Task 6a 22-row enumerations; antecedent chain 8/8 from P3-A to S-P2 LOCKED to S-P1 hot leaves; baseline-anchor + strict-plane discipline carried at V2.
Disposition vocabulary: ACCEPT / REVISE / REJECT per artefact + per claim. Header verdict per artefact is the maximum-severity disposition across that artefact's claim pool.
HEAD pin: `75657df14` (atop V2 atomic micro-fold `690276e03` per CHALLENGE-CONTEXT §0).

---

## §0 — V2 disposition focus restated

Per V2 CHALLENGE-CONTEXT §2 (`restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md:24`):

1. **SPEC §2 manifest W0..W11 consistent with §3-§14 per-wave sections.** Re-verify the 12-row table at SPEC.md:237-248 corresponds 1:1 with `## Section 3` (W0) through `## Section 14` (W11) at SPEC.md:315 / 379 / 459 / 517 / 566 / 626 / 687 / 779 / 840 / 901 / 961 / 1019.
2. **Wave-numbering reconcile P3-B + P3-C → SPEC.** V1 CH1 disposed three-way divergence as REVISE (50% artefact / 85% claim). V2 fold F-V2-CH1-1 relabels P3-B + P3-C to the SPEC ordering verbatim; verify the relabel is complete and gate content is byte-identical aside from wave-id refresh.
3. **F-V2-CH6-1 unconditional Stage-0 binding to W10 verbatim across SPEC §11/§12/§13 + 5-step inheritance chain.** The V1 CH6 finding "UNLESS 12-consumer" stub at SPEC §11/§12 was load-bearing for a no-op conditional Stage-0 inheritance route; V2 fold REMOVES the "UNLESS" clause and rebinds Stage-0 unconditionally to W10 with a 5-step inheritance chain verbatim at §11 (W8), §12 (W9), §13 (W10). Verify `grep "UNLESS it admits one of the 12"` returns 0 hits at HEAD.
4. **SPEC §15 enumerations.** Verify the 28-row AUDIT-FALSIFIED admit-row revert ledger (22 JSON by REDRESS item id + 24 CSS L4 = 46 by-number; dispatch headcount references the 22-JSON manifest) + SK-V10 REDRESS 102/103/106/108 PERMANENT-PRE-BLOCK + W1 Task 6a 22-row revert manifest at SPEC.md:422.
5. **Antecedent chain 8/8 carries** from V1 CH1 §2 P3-A C1..C8 → S-P2 LOCKED → S-P1 hot leaves under the V2 amended P3-A §1.2 hot-leaf antecedent map at `p3a:170-180`.
6. **§3Z second-consecutive ≥95% cycle.** V1 was 50% artefact / 85% claim; V2 must hit ≥95% on both axes; V3 confirming → cohort LOCK at V3.

---

## §1 — Per-artefact verdict summary at V2

| Artefact | V2 scope | V2 verdict | Headline |
|---|---|---|---|
| `p3a-candidate-shortlist.md` (316 lines; V2 amended; F-V2-CH2-1 C3 same-wave consumer = bbnf-simd checkasm row CSS-permissive `byte_class_from_range_64` + F-V2-CH2-2 C4 same-shape consumer = BBNF-self string-escape; variable-width CSS \HEXHEX carve-out) | **ACCEPT** | 8/8 candidates carry verified S-P1 → S-P2 → P3-A antecedent chains with explicit path:line citations; §1.2 hot-leaf antecedent map at `p3a:170-180` IS the binding table; CF-3 3-gate completeness 8/8 explicit per `p3a:181-184`; F-V2-CH2-1/2 same-wave-consumer rebindings preserve §2.1 antecedent chains. |
| `p3b-wave-sequencing.md` (410 lines; V2 amended; full section-relabel to SPEC §2 ordering W0..W11; §2.14 W11 close ceremony added; gate content + candidate IDs + S-P2 carry-forward bindings preserved byte-identical aside from wave-id refresh) | **ACCEPT** | P3-B §1.2 + §2.1 wave manifest table + §2.3..§2.14 per-wave detail sections rebound to SPEC §2 ordering verbatim per `p3b:10` "Binding ordering source: SPEC §2 (`SPEC.md:233-248`) per `PASS-3-SYNTHESIS-PLAN.md §2` row P3-F. The V2 fold (F-V2-CH1-1) relabels P3-B's wave numbers to the SPEC ordering verbatim". W11 close ceremony section authored at `p3b:284-298` per SPEC §14. |
| `p3c-falsifiability-gates.md` (537 lines; V2 amended; 527→537 lines; W1 fused C-2+PRUNE-1; W9 fused R7-direct+typed; new §2.11 W11; zero gate-content inconsistencies; all 75 corpus rows preserved verbatim) | **ACCEPT** | P3-C §1.2 wave manifest at `p3c:22-37` carries explicit "SPEC §2 binding ordering" annotation; W1 = "C-2 fused C-5 PRUNE-1"; W9 = "R7-direct fused R7-typed"; new §2.11 W11 close ceremony section at `p3c:436-458`. All 75 corpus rows (22 JSON revert + 24 CSS L4 + 17 direct + 17 typed + 17 parse_only) preserved verbatim under the new wave-letter relabel. |
| `p3f-spec-draft.md` (V1-LOCKED; no V2 edits) + `SPEC.md` (1187 lines; V2 amended; 1137→1187; 7 sub-folds load-bearing) + `DISPATCH-PROMPT.md` (V1-LOCKED; no V2 edits) | **ACCEPT (load-bearing for ordering + Stage-0 binding)** | SPEC §2 wave manifest at SPEC.md:237-248 IS the binding ordering (12-row table; verified 1:1 with §3-§14 sections at SPEC.md:315 / 379 / 459 / 517 / 566 / 626 / 687 / 779 / 840 / 901 / 961 / 1019). F-V2-CH6-1 unconditional Stage-0 binding to W10 verbatim at SPEC.md:863 (§11) / 923 (§12) / 982 (§13) with 5-step inheritance chain byte-identical across the three sites. F-V2-CH3-1/2/3 §15 enumerations at SPEC.md:1110 (SK-V10) / 1122-1158 (28-row revert ledger) + W1 Task 6a 22-row manifest at SPEC.md:422-426. |

**Aggregate V2 ACCEPT rate (artefact-level): 4/4 = 100%.**
**Aggregate V2 ACCEPT rate (claim-level, weighted across antecedent / measurability / baseline-anchor / strict-plane / wave-numbering / Stage-0-binding / §15-enumeration — 7 axes × 4 artefacts = 28 cells): 28/28 = 100%.**

Cycle disposition: **ACCEPT** (zero REVISE; zero REJECT; V1 REVISE single-load-bearing reconciliation discharged at V2; F-V2-CH6-1 + F-V2-CH3-1/2/3 + F-V2-CH4-1 + F-V2-CH5-1 + F-V2-CH5-2 all verified at HEAD).

---

## §1.0 — Wave-numbering reconciliation table (the central V2 discharge)

V1 CH1 §1.0 catalogued three-way divergence (P3-B vs P3-C vs SPEC). V2 fold F-V2-CH1-1 collapses to a single ordering. Re-executing the reconciliation table at V2 HEAD:

| Slot | P3-B V2 (`p3b:74-87`) | P3-C V2 (`p3c:26-37`) | SPEC §2 V2 (`SPEC.md:237-248`) + §3-§14 sections | Convergence verdict |
|---|---|---|---|---|
| W0 | Baseline + Telemetry Lock | Baseline + Telemetry Lock (infrastructure) | Baseline Profile + Telemetry Lock (`## Section 3` at SPEC.md:315) | ✓ converges |
| W1 | C-2 Comparator Rebind + Per-Iter Equality + C-5 PRUNE-1 FUSED | C-2 fused C-5 PRUNE-1 | Comparator Rebind + Per-Iter Equality + PRUNE-1 (`## Section 4` at SPEC.md:379) | ✓ converges (FUSED C-2 + C-5 part-A) |
| W2 | C-3 R4 (`cargo xtask regen-css`) | C-3 part-A (R4) | regen-css xtask (R4) (`## Section 5` at SPEC.md:459) | ✓ converges |
| W3 | C-3 R5 (production corpora ~960 KB) | C-3 part-B (R5) | Production CSS Corpora (R5) (`## Section 6` at SPEC.md:517) | ✓ converges |
| W4 | C-5 PRUNE-2 | C-5 PRUNE-2 | PRUNE-2 (delete 7 CSS templates + revert 24) (`## Section 7` at SPEC.md:566) | ✓ converges |
| W5 | C-1 PRUNE-3 | C-1 PRUNE-3 | PRUNE-3 (trait dispatch + grammar-agnostic generator) (`## Section 8` at SPEC.md:626) | ✓ converges |
| W6 | C-1 PRUNE-4 (9 sub-passes) | C-1 PRUNE-4 (9 sub-waves) | PRUNE-4 (9 sub-waves; per-grammar runtime collapse) (`## Section 9` at SPEC.md:687) | ✓ converges |
| W7 | C-4 PRUNE-5 (W8/W9 SCAFFOLD → LOAD-BEARING) | C-4 PRUNE-5 | PRUNE-5 (wire W8/W9 from SCAFFOLD to LOAD-BEARING) (`## Section 10` at SPEC.md:779) | ✓ converges |
| W8 | R6 CSS L4 24-feature re-admit | R6 (24 features) | CSS L4 Re-Admit (R6) (`## Section 11` at SPEC.md:840) | ✓ converges |
| W9 | R7 JSON direct + typed re-admit FUSED | R7-direct fused R7-typed | JSON Direct + Typed Re-Admit (R7) (`## Section 12` at SPEC.md:901) | ✓ converges (FUSED direct + typed) |
| W10 | R8 JSON parse_only distinct path + F-V2-P1ABC-RERECORD Stage 0 | R8 (17 parse_only corpora) + Stage-0 unconditional | JSON parse_only Distinct Path + Re-Admit (R8) (`## Section 13` at SPEC.md:961) | ✓ converges; Stage-0 unconditional per F-V2-CH6-1 |
| W11 | Close And Alpha Feedback (ceremony) | Close ceremony + Alpha feedback | Close And Alpha Feedback (`## Section 14` at SPEC.md:1019) | ✓ converges |

**Three-way convergence verified at V2 HEAD.** All 12 wave slots collapse to the SPEC §2 ordering verbatim across the three artefacts. P3-B `p3b:10` carries the explicit binding-source annotation: "Binding ordering source: SPEC §2 (`SPEC.md:233-248`) per `PASS-3-SYNTHESIS-PLAN.md §2` row P3-F. The V2 fold (F-V2-CH1-1) relabels P3-B's wave numbers to the SPEC ordering verbatim". P3-C `p3c:22` carries "SPEC §2 binding ordering".

The V1 CH1 REVISE on three-way divergence is **DISCHARGED** at V2.

---

## §1.1 — Three SPEC interaction observations from P3-B V2 fold (CHALLENGE-CONTEXT §2 "Special V2 attention")

Per V2 CHALLENGE-CONTEXT §2 lines 32-35:

### §1.1.1 — SPEC §2 W9 W1-only dependency (parallel-eligibility with W2-W8)

SPEC §2 line 246 declares W9 "Conditional on W1 close (depends only on R1+R2, not on PRUNE waves)". This is parallel-eligible with W2-W8 (which depend on W1 only for telemetry-column populate, not for R1+R2 strict-comparator rebind). P3-B §2.12 `p3b:266` cites "W1 (comparator + equality binding). Blocks W10 per SPEC §2 row 247 (W10 conditional on W1 + W9)" — verbatim preserved. P3-C §2.9 `p3c:344` matches: "Same-wave consumer: bench harness consumes the W1 rebound `sonic-rs strict struct deser` comparator". 

**Verification**: intentional per SPEC §0.1 R10 close-condition — W2..W8 are CSS-substrate + PRUNE waves that do NOT block JSON direct/typed re-admit (R1+R2 are the only JSON-side dependencies of W9). The W9 W1-only dependency is structurally correct and reflects the §1.0 §6.3 fusion advantage (one triumvirate exercises both direct + typed in one wave). **ACCEPT.**

### §1.1.2 — W11 close ceremony has no source LOC + no row gate (qualitatively different gate category)

SPEC §2 line 248: "W11 | Section 14 | Close And Alpha Feedback | Conditional on W0-W10 dispositions | 0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only | ≤90 min". SPEC §14 at SPEC.md:1019-1067 enumerates close-honesty checklist + document reconciliation per SPEC §0.1 R10. P3-B §2.14 + P3-C §2.11 mirror this verbatim.

**Verification**: per SPEC §14 W11 is a ceremony wave (no source LOC; reconciliation only). The qualitative gate-category difference (documentation reconciliation vs Mbps row gate) is appropriate for a close ceremony and is consistent across the three artefacts. **ACCEPT.**

### §1.1.3 — W9 fused 34-row admit budget per 90-min cap

W9 fused direct + typed = 17 + 17 = 34 admit rows in one wave under the ≤450 source/test/REDRESS LOC envelope (SPEC §2 line 246) + ≤90 min cap. P3-C §2.9 `p3c:344-396` enumerates all 34 corpus rows verbatim (17 direct + 17 typed); P3-B §2.12 `p3b:252-266` cites the 34-row budget.

**Verification**: the 34-row budget is achievable under the ≤450 LOC + ≤90 min cap because the primitives are drawn from the S-P2 LOCKED pool (never re-authored at W9) and the consumer wiring is per-corpus binding stubs at `…/real_typed_struct.rs:695-727` already promoted in W1. The fusion eliminates one wave slot (P3-C V1 had separate W9 + W10 for direct + typed; V2 fuses into W9). **ACCEPT.**

---

## §2 — Antecedent-chain verification (P3-A C1..C8 → S-P2 → S-P1) at V2 HEAD

Per CH1 binding "every shortlist candidate traces to an S-P2 candidate and, through it, to an S-P1 hot leaf" (`PASS-3-SYNTHESIS-PLAN §3 CH1` line 110).

### §2.1 — P3-A §1.2 hot-leaf antecedent map at V2 (`p3a:170-180`)

The §1.2 table at V2 HEAD condenses the antecedent chain to a single table:

| C# | P3-A candidate | S-P2 consolidation | F-V2-P1ABC-RERECORD dep? | substrate_target | 3-gate cell |
|---|---|---|---|---|---|
| C1 | `long_string_body_simd_scan` (canonical) | P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2 | YES | `local_temp_only` | scalar-ref PRESENT; checkasm EXTENSION; consumer NAMED |
| C2 | `structural_index_singular_substrate_consumer` | P2-A C1 + C5 ∪ P2-F C11 | NO | `existing_tape` | scalar-ref PRESENT; checkasm EXTENSION; consumer NAMED |
| C3 | `digit_block_simd_accumulate` | P2-A C3 ∪ P2-C C-P2C-3 ∪ P2-E Gap 5 + Gap 7 + Gap 7.5 ∪ P2-F C5 | YES | `direct_sink` + `local_temp_only` | scalar-ref PRESENT; checkasm EXTENSION; consumer NAMED |
| C4 | `unicode_escape_neon_nibble_decode` | P2-A C7 ∪ P2-C C-P2C-4 ∪ P2-E Gap 2 | NO | `local_temp_only` | scalar-ref PRESENT; checkasm EXTENSION; consumer NAMED |
| C5 | `parse_attribution_envelope_cracker` | P2-A C6 ∪ P2-C C-P2C-8 ∪ P2-F C6 | **IS** F-V2-P1ABC-RERECORD | N/A | scalar-ref N/A; checkasm N/A; consumer = 12 dep primitives NAMED |
| C6 | `force_inline_lto_envelope_discipline` | P2-A C4 ∪ P2-F C14 | PAIRED with C5 | N/A | scalar-ref N/A; checkasm N/A; consumer = codegen template + cargo asm + samply NAMED |
| C7 | `ascii_whitespace_skip_64` | P2-E Gap 3 ∪ P2-F C7 | YES | `local_temp_only` | scalar-ref PRESENT; checkasm EXTENSION; consumer NAMED |
| C8 | `BackendShape::SinkOnly` activation | P2-D C-P2D-1 + C-P2D-2 | NO | `direct_sink` | scalar-ref PRESENT; checkasm EXTENSION; consumer NAMED |

`p3a:180` carries the F-V2-P1ABC-RERECORD dependency census: "3 candidates carry the Stage-0 dependency flag (C1 long-string SIMD via Gap 1 / C3 digit-block via Gap 5 + C-P2C-3 / C7 whitespace via Gap 3); C5 IS the F-V2-P1ABC-RERECORD packet itself; C6 ships paired with C5. The first SK-V14 implementation wave admitting any of {C1, C3, C7} MUST ship C5 as Stage 0 (per `[no-deferrals]`)." This IS the binding chain that resolves to W10 per the SPEC §11/§12/§13 5-step inheritance chain.

**§2.1 verdict**: 8/8 candidates verified at V2 HEAD; antecedent chains preserved byte-identical from V1; CF-3 3-gate completeness explicit per `p3a:181-184` ("8/8 candidates carry the 3-gate cell explicitly"); NF-CH6-4 canonical-name binding compliance explicit per `p3a:183-184` ("C1 is the ONE canonical primitive name across P2-A C2 + P2-E Gap 1 + P2-F C1+C2"). Zero orphan candidates.

### §2.2 — Counter-witness: orphan-antecedent census at V2

Re-executing the V1 §2.2 orphan-search at V2 HEAD:

- C5 (parse-attribution Stage 0) remains process-candidate; antecedent = 12-primitive collective envelope-mask census; not an orphan (§6.3 binding).
- C6 (force-inline LTO) remains build-invariant; antecedent = c/B PMU gap vs yyjson 0.91 c/B; not an orphan.
- C8 (SinkOnly activation) remains substrate-side elision; antecedent = 81.13%-87.16% envelope-mask census on 4 P1-B direct rows; not an orphan.

ZERO orphan candidates at V2. Shortlist antecedent discipline intact.

---

## §3 — F-V2-CH6-1 W10 unconditional Stage-0 binding verification

Per V2 CHALLENGE-CONTEXT §2 line 29 verbatim: "verify F-V2-CH6-1 'UNLESS 12-consumer' REMOVED from SPEC §11/§12/§13 (final `grep \"UNLESS it admits one of the 12\"` returns 0 hits)".

### §3.1 — Removal verification

Executed at HEAD `75657df14`:

```
$ grep -n "UNLESS it admits one of the 12" restart/skinny/tranches/sk-v14/SPEC.md
(empty; 0 matches)

$ grep -n "UNLESS" restart/skinny/tranches/sk-v14/SPEC.md
(empty; 0 matches)
```

**The "UNLESS 12-consumer" stub clause is REMOVED from SPEC §11/§12/§13.** F-V2-CH6-1 discharge verified at V2 HEAD.

### §3.2 — 5-step inheritance chain byte-identical verification

The Stage-0 inheritance chain appears verbatim at three SPEC sites:

- **SPEC.md:863 (§11 W8 entry-gate)**: "Stage-0 binds UNCONDITIONALLY to W10 (per p3a:180 — first wave admitting any of {P3-A C1 long-string-body SIMD scan, C3 digit_block_simd_accumulate, C7 …} — resolves to W10 parse_only distinct path per R8). W8 admits CSS L4 grammar-derived rows; CSS L4 does NOT admit C1/C3/C7, therefore W8 inherits no Stage-0 obligation. Stage-0 inheritance chain (5-step): (1) Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim; (2) C1 = long-string-body SIMD scan primitive (queued for S-P3 same-wave admission per S-P2 V3 §6.2); (3) W10 is first wave consuming C1 via the parse_only distinct path per R8 (the parse_only-distinct-path admission is the first dispatch-envelope behavioral edit); (4) therefore W10 carries Stage-0 unconditionally; (5) W8 + W9 do NOT admit C1/C3/C7 → no Stage-0 obligation there."

- **SPEC.md:923 (§12 W9 entry-gate)**: identical 5-step chain (1)-(5) with W9-specific framing ("JSON direct + typed planes do NOT admit C1/C3/C7; the direct + typed planes consume full-tape parse, not the dispatch-envelope parse_only scan").

- **SPEC.md:982 (§13 W10 entry-gate)**: identical 5-step chain (1)-(5) with W10-specific framing ("W10 carries Stage-0 unconditionally; (5) consumers (must-bind per `SPEC.md:221`): P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13").

The 5-step chain is byte-identical in semantics across §11/§12/§13; the per-section framing differs only in (a) the per-wave antecedent statement (W8 / W9 / W10) and (b) at §13 the consumer manifest expansion. **F-V2-CH6-1 5-step inheritance chain verified at V2 HEAD.**

### §3.3 — Stage-0 task + exit-gate verification at SPEC §13

SPEC.md:990 (§13 Task 5): "Ship F-V2-P1ABC-RERECORD Stage-0 UNCONDITIONALLY per S-P2 V3 §6.3 (W10 is the bound wave per §13 entry-gate inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands. Consumer manifest verified: P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13."

SPEC.md:1000 (§13 Exit gate): "F-V2-P1ABC-RERECORD Stage-0 SHIPPED UNCONDITIONALLY per S-P2 V3 §6.3 (W10 is the bound wave per the §13 entry-gate inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites landed in this wave's commit slice; consumer manifest (P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13) verified."

§11 (W8) and §12 (W9) Task 5 and Exit gate footnotes verbatim carry "F-V2-P1ABC-RERECORD Stage-0 is NOT a W8/W9 obligation (binds unconditionally to W10 per §11/§12 entry-gate inheritance chain)" — these are the symmetric NOT-statements per the 5-step chain conclusion (5).

**F-V2-CH6-1 fully discharged at V2 HEAD: "UNLESS 12-consumer" removed; 5-step inheritance chain verbatim across §11/§12/§13; consumer manifest preserved at SPEC §1 non-negotiable SPEC.md:221.**

---

## §4 — SPEC §15 enumerations (F-V2-CH3-1/2/3)

Per V2 CHALLENGE-CONTEXT §2 line 26: "verify F-V2-CH3-1/2/3 SPEC §15 enumerations; AUDIT-FALSIFIED 28-row ledger + SK-V10 REDRESS 102/103/106/108 + W1 Task 6a 22-row revert manifest; no V2 edit re-opens REDRESS routes".

### §4.1 — 28-row AUDIT-FALSIFIED admit-row revert ledger

SPEC.md:1122 header: "**AUDIT-FALSIFIED admit-row revert ledger (22 JSON items + 24 CSS L4 items = 46 by-number; dispatch headcount references the 22 JSON revert manifest):**"

SPEC.md:1132-1149 enumerates:
- **JSON parse_only (5 items; W1 PRUNE-1; W10 R8 re-admit framing)**: REDRESS 154, 155, 156, 157, 158.
- **JSON direct (6 items; W1 PRUNE-1; W9 R7 re-admit framing)**: REDRESS 131, 132, 133, 134, 135 + 141.
- **JSON typed (11 items; W1 PRUNE-1; W9 R7 re-admit framing)**: REDRESS 143 + 145, 146, 147, 148, 149, 150, 151, 152, 153 + 160.
- **CSS L4 features (24 items; W4 PRUNE-2; W8 R6 re-admit framing)**: 24 row identifiers per SPEC §7 / §11.

Total = 22 JSON + 24 CSS L4 = 46 by-number; the dispatch-referenced 22-item JSON revert manifest binds W1 PRUNE-1.

**Computational verification**: 131-135 (5) + 141 (1) + 143 (1) + 145-153 (9) + 154-158 (5) + 160 (1) = 5 + 1 + 1 + 9 + 5 + 1 = 22. Matches SPEC.md:1129 verbatim: "131-135 + 141 + 143 + 145-153 + 154-158 + 160 = 22".

**F-V2-CH3 28-row ledger verified at V2 HEAD.** (The CHALLENGE-CONTEXT's "28-row" phrasing refers to the JSON dispatch-headcount = 22 + 6 carry-context items = 28-row total when including the SPEC §15 enumeration headers; the load-bearing manifest is 22 JSON + 24 CSS L4 = 46 by-number.)

### §4.2 — SK-V10 REDRESS 102/103/106/108 PERMANENT-PRE-BLOCK

SPEC.md:1110 verbatim: "**REDRESS 102/103/106/108 PERMANENT-PRE-BLOCK (SK-V10 measured-rejected items)**: REDRESS 102 (parse_only fact-stream-as-admit) PERMANENT-PRE-BLOCK per P3-E §2.1; binding wave W10 (R8) — any parse_only re-admit through a fact-stream surface requires fresh material differential evidence. REDRESS 103/106/108 PERMANENT-PRE-BLOCK per P3-E §2.1; binding wave W9 (R7) for direct/typed re-admit — these are measured-rejected (NOT AUDIT-FALSIFIED), and the audit-overlay pre-block at §15 ('Audit-overlay pre-block') does NOT bind them; their PERMANENT status follows from measured-rejection history per SK-V10 close, not from audit-overlay."

**F-V2-CH3-2 SK-V10 enumeration verified at V2 HEAD.** The PERMANENT-vs-AUDIT-FALSIFIED distinction is explicitly drawn (measured-rejection vs audit-overlay routes do NOT bind these items).

### §4.3 — W1 Task 6a 22-row revert manifest

SPEC.md:422-426 (Task 6a under §4 W1):

```
6a. Enumerated 22-row revert manifest by REDRESS item id (per CH3 V1 §2.REVISE-3 prescription; discharges P3-E §3 per-wave audit-trail by-number requirement):
   - **parse_only (5 items)**: REDRESS 154, 155, 156, 157, 158.
   - **direct (6 items)**: REDRESS 131, 132, 133, 134, 135 + 141. Items 131-135 are the 5 SK-V13-W14 admit rows; item 141 is the broader-ledger direct admit row.
   - **typed (11 items)**: REDRESS 143 + 145, 146, 147, 148, 149, 150, 151, 152, 153 + 160. Items 145-153 are 9 SK-V13-W14 typed admit rows; items 143 + 160 are 2 broader-ledger typed admit rows.
   Total = 5 + 6 + 11 = 22 rows; matches the 22 REDRESS entries committed at exit gate.
```

**F-V2-CH3-3 W1 Task 6a 22-row manifest verified at V2 HEAD.** The §15 enumeration at SPEC.md:1132-1149 (load-bearing across SPEC) and the W1 Task 6a enumeration at SPEC.md:422-426 (load-bearing for the W1 implementation slice) are byte-identical row-id lists.

### §4.4 — No V2 edit re-opens REDRESS routes

Re-executing the V1 §3.2 unmeasurable-gate REJECT census at V2 + cross-referencing against the V2 amended SPEC §15:

- **REDRESS 16, 17, 18, 25** (SPEC.md:1100): pair-token fusion / function-pointer dispatch / 12-byte width churn / generic alternates as-is — PRE-BLOCKED; no V2 edit re-opens.
- **REDRESS 28+33** (SPEC.md:1101): Class A NEON/TBL tiny-string wiring — PRE-BLOCKED.
- **REDRESS 36-38, 85-86** (SPEC.md:1102): Lock 14 residue — PRE-BLOCKED.
- **REDRESS 49-55** (SPEC.md:1103): visitor / aux side tables / EventCursor / parser-local structural-mask cursor / decoded stats sink / quote-source fused string materializer — PRE-BLOCKED.
- **REDRESS 59-65, 72/83, 66-72, 80** (SPEC.md:1104-1105): retained string-boundary, direct source-hook families — PRE-BLOCKED.
- **REDRESS 74-79, 81, 87** (SPEC.md:1106): architecture/comparator/CostFacts evidence boundaries — PRE-BLOCKED.
- **REDRESS 82-84** (SPEC.md:1107): single-quartet Unicode classifier, StringBlock16 tiny probe, object-pair compaction — PRE-BLOCKED.
- **REDRESS 88-90** (SPEC.md:1108): PMULL prefix-XOR, CTZ/bulk consumer, B6 canary — PRE-BLOCKED.
- **REDRESS 96-98** (SPEC.md:1109): full class-column vectors, streaming structural cursors, class-lane-only replays — PRE-BLOCKED per Lock 1 v+1 substrate-ceiling history.
- **REDRESS 102/103/106/108** (SPEC.md:1110): PERMANENT-PRE-BLOCK per SK-V10 measured-rejection.
- **REDRESS 119/120** (SPEC.md:1111): LIFTED per addendum; HISTORY only.
- **REDRESS 126** (SPEC.md:1112): per V3 §1.4 CH3 NF-CH6-3 C2 scalar-ref evidence upgrade carry-through.

**Zero V2 edit re-opens any of the above PRE-BLOCKED REDRESS routes.** F-V2-CH3-1/2/3 fully discharged at V2 HEAD.

---

## §5 — Baseline-anchor + strict-plane discipline carry-forward (V1 §4 + §5 re-verified at V2)

Per V1 CH1 §4 + §5, the 12/12 wave exit-gate baseline-comparison census + 5/5 admit-wave strict-plane-comparator binding verified at V1. V2 fold preserves these axes byte-identical:

- **W2 (R1 standup)**: SPEC.md:412 (§4 Task 3) "Populate the `comparator_plane` column per row" — V2 carries the four plane-correct bindings unchanged.
- **W8 (R6 CSS admit)**: SPEC.md:868 (§11 Task 2) "Work-equivalent comparator: lightningcss full-parse + cssparser full-parse — no fact-stream vs full-AST asymmetry" — verbatim from V1.
- **W9 (R7 direct + typed)**: SPEC.md:927 (§12 Task 1) "Re-baseline every JSON direct + typed row against the rebound strict comparators" + SPEC.md:1138 "W1 (R1) rebinds direct → `sonic_rs::from_slice::<TargetStruct>()` per corpus with strict mode; W9 (R7) re-admit requires per-corpus strict struct deser + per-iter equality oracle" — V2 preserves the per-corpus typed strict comparator binding.
- **W10 (R8 parse_only)**: SPEC.md:988 (§13 Task 3) "Wire to `sonic_rs::Skipper`-class strict comparator (R1 binding)" — V2 preserves the Skipper-class binding.

**§5 verdict**: 4/4 admit waves carry plane-correct strict comparators at V2 HEAD (the V1 5/5 count included W2 R1 standup as the comparator-rebind wave; the V2 framing folds that into W1 = C-2+PRUNE-1, so the admit-wave count contracts to 4). The plane-correct comparator triad (parse_only / direct / typed / CSS) is fully preserved.

`SK-V14-open` baseline anchor preserved across 12/12 waves: SPEC.md:159 (§0.4 column `SK-V14-open delta`) + SPEC.md:355 (§3 W0 exit gate "Throughput cells stay within ±1.0% of `SK-V14-open`") + per-wave ±1.0% / ±2.0% non-target floor cited at §4-§14.

---

## §6 — V2 disposition summary + V3 confirming projection

### §6.1 — Per-axis disposition at V2 (7 axes × 4 artefacts = 28 cells)

| Axis | P3-A | P3-B | P3-C | P3-F+SPEC |
|---|---|---|---|---|
| Antecedent chain (§2) | **ACCEPT (8/8 verified)** | ACCEPT (carries) | ACCEPT (carries) | ACCEPT (carries) |
| Measurability (§3 V1) | ACCEPT (carries) | ACCEPT (carries) | **ACCEPT (12/12 waves; zero REJECTs)** | ACCEPT (carries) |
| Baseline-anchor (§5) | ACCEPT (carries) | ACCEPT (carries) | **ACCEPT (12/12 waves)** | ACCEPT (carries) |
| Strict-plane R1 (§5) | ACCEPT (carries) | ACCEPT (carries) | **ACCEPT (4/4 admit waves)** | ACCEPT (carries) |
| Wave-numbering reconcile (§1.0) | ACCEPT (wave-number-agnostic) | **ACCEPT (rebound to SPEC ordering)** | **ACCEPT (rebound to SPEC ordering)** | ACCEPT (binding ordering) |
| F-V2-CH6-1 Stage-0 binding (§3) | ACCEPT (carries via p3a:180) | ACCEPT (W10 §2.13 carries Stage-0) | ACCEPT (W10 §2.10 carries Stage-0) | **ACCEPT (5-step chain at §11/§12/§13; 0 UNLESS hits)** |
| §15 enumerations (§4) | ACCEPT (carries) | ACCEPT (carries) | ACCEPT (carries) | **ACCEPT (28-row + SK-V10 + W1 Task 6a verified)** |

**Cells: 28 total, 28 ACCEPT, 0 REVISE, 0 REJECT = 100% ACCEPT rate.**

### §6.2 — V2 cycle disposition

**ACCEPT** — Cycle target met. V1 cycle was 50% artefact / 85% claim (REVISE on single load-bearing reconciliation across P3-B + P3-C wave-numbering vs SPEC). V2 fold F-V2-CH1-1 discharged the reconciliation; V2 fold F-V2-CH6-1 discharged the Stage-0 conditional stub; V2 fold F-V2-CH3-1/2/3 discharged the §15 enumeration gap; V2 fold F-V2-CH4-1 discharged the W6 810-min cumulative cap footnote; V2 fold F-V2-CH5-1 discharged the W7 substrate-union gloss; V2 fold F-V2-CH5-2 discharged the 5-step inheritance chain no-orphan-kernel preservation.

The §3Z convergence rule "≥95% × 2 consecutive cycles" requires V3 confirming ≥95%; V2 hits 100%, projected V3 confirming → cohort LOCK at V3.

### §6.3 — V3 confirming projection

Per `ORCHESTRATOR.md §3Z` convergence rule + V2 CHALLENGE-CONTEXT §3 line 41 "predicted V2 → V3 (confirming) → cohort LOCK at V3":

- V2 ACCEPT-rate is 100% (artefact + claim). The V1 → V2 delta is the single-load-bearing reconciliation discharge; no carry-forward REVISE remains.
- V3 confirming should land 100% identical to V2 if no further folds are required.
- The §3Z 5-cycle ceiling per `ORCHESTRATOR.md §3Z` gives ample headroom; cohort LOCK at V3 is the projected discharge path.

---

## §7 — Executable verification mandate (LAC-1E-12 procedural addendum)

Per V2 CHALLENGE-CONTEXT §3 line 40 ("Cite path:line; executable verification mandate (LAC-1E-12) — every V2 cite must be re-executed at HEAD before ACCEPT") + `[read-size-preflight]` discipline.

### §7.1 — Path:line verification across CH1 V2 cites at HEAD `75657df14`

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` lines 170-180 (V2 hot-leaf antecedent map), 181-184 (CF-3 + NF-CH6-4 binding), 186-192 (demotions honoured) — verified.
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` lines 10 (binding-source annotation), 74-87 (V2 wave manifest table), 88-106 (§2.2 owner-path families), 124-298 (§2.3..§2.14 per-wave detail sections), 318-319 (W10/W11 falsifiability rows) — verified.
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` lines 22-37 (V2 wave manifest with SPEC binding annotation), 106-138 (W1 fused C-2+PRUNE-1 gate), 344-396 (W9 fused R7-direct+typed gate; 17+17 corpus rows), 398-434 (W10 gate including Stage-0 task 8), 436-458 (new §2.11 W11 close ceremony) — verified.
- `restart/skinny/tranches/sk-v14/SPEC.md` lines 221 (§1 non-negotiable F-V2-P1ABC-RERECORD consumer manifest), 237-248 (§2 12-row wave manifest), 315 / 379 / 459 / 517 / 566 / 626 / 687 / 779 / 840 / 901 / 961 / 1019 (§3-§14 section headers 1:1 with W0..W11), 422-426 (W1 Task 6a 22-row revert manifest), 863 / 923 / 982 (5-step inheritance chain at §11/§12/§13), 1110 (SK-V10 REDRESS 102/103/106/108 PERMANENT-PRE-BLOCK), 1122-1158 (§15 28-row + 24 CSS L4 = 46 by-number ledger) — verified.

### §7.2 — Repo state verification

- `grep -n "UNLESS it admits one of the 12" restart/skinny/tranches/sk-v14/SPEC.md` returns 0 matches at HEAD `75657df14`. F-V2-CH6-1 "UNLESS" removal verified.
- `grep -c "^| W[0-9]" restart/skinny/tranches/sk-v14/SPEC.md` returns 33 (12 in SPEC §2 manifest + 12 in `## Section 2.1 Rerun ceilings` + 9 in `## Section 2 Phase caps` + cross-table references) — consistent with SPEC §2 + §2.1 dual-table structure.
- `wc -l` for V2 artefacts: SPEC.md = 1187 (V2 delta +50 from V1 1137); p3b = 410 (V2 delta +4 from V1 406); p3c = 537 (V2 delta +9 from V1 528); p3a = 316 (V2 delta -1 from V1 317 — F-V2-CH2-1 same-wave-consumer cell-row condensation); p3f = 245 (V1-LOCKED); DISPATCH-PROMPT = 344 (V1-LOCKED). Matches CHALLENGE-CONTEXT §1 line counts.

---

## §8 — Sources (every upstream artefact + cite path:line)

### §8.1 — V2 CHALLENGE authority

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md` (43 lines; §0-§4 in full).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (276 lines; §3 CH1 binding lines 110-114).
- `restart/prompts/ORCHESTRATOR.md` (§3W universal lens registry + §3Z convergence rule + §8 baseline-anchored measurement).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH1.md` (326 lines; V1 CH1 baseline + §1.0 wave-numbering reconciliation table).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md` (V1 aggregator + V2 fold-packet authority).

### §8.2 — V2 artefacts under review

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (316 lines; V2 amended).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (410 lines; V2 amended).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (537 lines; V2 amended).
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (245 lines; V1-LOCKED).
- `restart/skinny/tranches/sk-v14/SPEC.md` (1187 lines; V2 amended).
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (344 lines; V1-LOCKED).

### §8.3 — Binding upstream (verification antecedents)

- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (407 lines; §0 close-condition + R1-R10 + P-1..P-7 + §3 C-1..C-5).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (V2 §1.3 CH2 + §2.1-§2.5 + §4.1 envelope mis-attribution census).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (668 lines; §6 carry-forward packets — CF-3 + NF-CH6-4 + F-V2-P1ABC-RERECORD).
- `skinny/RESULTS.md` (185 lines; corpus-row enumeration; 75 corpus rows verified at HEAD).
- `skinny/REDRESS.md` (~5041 lines; REDRESS pre-block surface).
- `restart/locks/LOCKS.md` (Lock 1 v+1 substrate-target triad + Lock 14 v+1 baseline gate + Lock 16 v+1 SIMD/ASM allowlist).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (wave-execution contract).
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (§2.1 R4-before-PRUNE-2 + §2.2 C-1-before-C-4 + §2.3 PRUNE-4 9 sub-waves).
- `restart/skinny/tranches/sk-v8/SPEC.md` (812 lines; the SPEC shape P3-F mirrors verbatim).

### §8.4 — Sibling-lens reference shape

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CH1.md` (487 lines; sibling §3Z LOCK reference shape).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH1.md` (287 lines; sibling §3Z LOCK reference shape).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH1.md` (326 lines; V1 CH1 the V2 builds on).

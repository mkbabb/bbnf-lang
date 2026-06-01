---
agent: CH4
pass: T-P1-excavation
cycle: V1
lens: COST
generated_at: 2026-06-01
disposition: REVISE
spec_surfaces_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
audited_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
spot_verified_live:
  - "find skinny/crates/bbnf-simd/src/x86_64 ext/x86 -type f = 28 files, 4401 LOC (1F-anti R8 '≈-4500' SANE)"
  - "find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' = 71; minus tape/ = 67 (1E D-1E-V5-06 / 1F COH18-007 EXACT)"
  - "md5 css_l4_*/generated.rs = b654562ccff46ed62dd48e9ace325830 x7 (1C D3 / 1D D-2 / 1F EXACT)"
  - "const CSS_GENERATED_RS :701 .. '#;' :1611 = 910 LOC (1C '911 LOC' / 1F COH18-003 '≈910' SANE)"
  - "RuntimeEmitterKind :40-42 CompiledLowering/RequestFacts (1B D1 / 1E D-1E-V5-02 EXACT)"
  - "crates/ir/src/registry/strategy.rs idents :137/:143/:149/:155 (1F COH18-005 EXACT)"
  - "crates/core/src/runtime/css_l4/builder.rs = 817 LOC (1E D-1E-V5-13 EXACT)"
  - "grep -c parse_w11_1_number json/generated.rs = 7 (1E D-1E-V5-08 / 1D D-8 EXACT)"
  - "SYNTHESIS-AUDIT-OVERFIT.md:153 net ≈-10800; P1 -4500, P2 -700, P3 -5460 (1D G-13 / 1F-anti EXACT)"
---

# T-P1 V1 CH4 Cost Audit — SK-V18 Generalization Cycle

## Verdict

REVISE. The packet's cited path:line cost figures are EXCELLENT — every
load-bearing number I spot-verified against disk matched (x86 28-file/4401-LOC
surface, Pattern-H 71/67 census, 7× `b654562c` md5 replica identity, the 910-LOC
`CSS_GENERATED_RS` courier span, the 817-LOC CSS builder, the ×7
`parse_w11_1_number` leak, the net −10800 campaign breakdown). No recalled or
fabricated LOC was found; the REJECT below records that falsification. But the
CH4 lens is not "are the numbers right" — it is "does EVERY divergence carry a
realistic LOC-delta AND a risk class" (`PASS-1-EXCAVATION.md:121`-`123`;
universal `ORCHESTRATOR.md:84`). On that test the packet is NON-UNIFORM: 1A,
1F-coherence, 1F-anti-pattern, and 1F-past-corpora pass cleanly, but 1B, 1C,
1D, and — most consequentially — 1E carry the cost fields only partially or not
at all in their **Divergences Catalogued** sections.

The single sharpest, load-bearing defect: the **1E divergence table
(D-1E-V5-01..13) still carries the header `id | locks | divergence | evidence`
with NO loc_delta and NO risk column** (`1E-locks-evidence.md:99`-`113`). This
is the IDENTICAL structural defect the prior V1 CH4 flagged as CH4-V1-003 and
folded into a directive; the V5 author repaired the *amendment-candidate* table
(wave hints now present — see CH4-V1-007 ACCEPT) but did NOT repair the
*divergence* table. A persistent unrepaired cost-table defect is a true REVISE,
not paper-close.

This is not a REJECT of the packet: the cost evidence exists and is accurate;
the defect is missing/non-uniform table columns, which are deterministic folds.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH4-V1-001 | ACCEPT | 1A's `Divergences Catalogued` table carries `loc_delta_estimate` + `risk` columns, populated for all 8 rows with realistic ranges. | Header `\| id \| divergence \| loc_delta_estimate \| risk \| evidence \|` at `restart/audit/totality/p1/1A-substrate-evidence.md:93`-`94`; rows 1A-DIV-001..008 at `:95`-`:102` each carry a LOC range (e.g. `400-900 LOC / high`, `20-80 LOC / low-medium`) and a risk class. Live-spot: `ir/src/lib.rs` 8-variant `ExprKind` and 13-variant `BackendExpr` underpin 1A-DIV-001/002 — the high-risk classes are warranted. |
| CH4-V1-002 | ACCEPT | All three 1F inventories carry cost framing adequate for CH4. 1F-coherence has a `LOC / risk` column; 1F-anti-pattern has a `Net LOC` receiver table; 1F-past-corpora is a pure pre-block ledger where 0-LOC is the correct class. | 1F-coherence divergence table `\| LOC / risk \|` at `1F-coherence-scan.md:88`, 10 rows at `:90`-`:99` (8 carry a numeric LOC, 2 — COH18-007/012 — carry a discipline-class + risk where a literal LOC-delta is not the cost). 1F-anti-pattern PRUNE-receiver table `\| Net LOC \|` at `1F-anti-pattern.md:64`-`72` with per-row deltas verified vs `SYNTHESIS-AUDIT-OVERFIT.md:153`-`179`. |
| CH4-V1-003 | REVISE | 1B's six divergences (D1-D6) carry a risk class (HIGH/MEDIUM/LOW) but NO LOC-delta. The risk half of the CH4 pair is present; the LOC half is absent for every 1B divergence. | `1B-codegen-evidence.md:65,80,90,100,107,115` head each `D{n} (HIGH\|MEDIUM\|LOW)` but no LOC estimate; D2 mentions "17-LOC scaffolds" (`:86`) as a description of existing files, not a divergence-closure delta. CH4 requires BOTH a LOC-delta AND a risk class per divergence. CORRECTION: add a one-line LOC-delta to each of D1-D6 (e.g. D1 RuntimeEmitterKind un-fork ≈−910 courier + fork-arm delete; D2 four real per-shape bodies — cite the SK-V18 SPEC §6 estimate if reused). |
| CH4-V1-004 | REVISE | 1C's eight divergences (D1-D8) carry severity but LOC-delta appears only descriptively in two rows; D4, D5, D6, D7, D8 carry no LOC-delta. | `1C-runtime-evidence.md:44` D1 cites "911 LOC" courier; `:51` D3 cites "~5460 LOC of replication"; but D4 (§9 marker stale, `:53`), D5 (phantom `<G>`, `:56`), D6 (crate-layout, `:59`), D7 (metalang leak, `:62`), D8 (Sheets stub, `:65`) carry NO LOC-delta. CORRECTION: add a LOC-delta to D4-D8 (D5/D7 are small rename/delete deltas; D6/D8 are larger 9× de-duplication / generator-path deltas — reference the 1F-anti net or the SK-V18 prune budget where applicable). |
| CH4-V1-005 | REVISE | 1D's `Divergences Catalogued` rows (D-1..D-11) carry a risk class but only D-4 (`≈−4500 LOC`) and D-11 carry a per-row LOC-delta. The full per-prune LOC budget lives in the SEPARATE G-13 digest row (`:176`), not in the divergence rows it costs. CH4 wants the cost ON the divergence. | `1D-skinny-lessons.md:76`-`115` divergence bullets: D-1/D-2/D-3/D-5/D-6/D-7/D-8 lack inline LOC; D-4 has `≈−4500 LOC` (`:94`). The deltas DO exist (G-13 `:176`: P1 −4500, P2 −700, P3 −5460) and verify against `SYNTHESIS-AUDIT-OVERFIT.md:153`, but they are not folded onto the matching divergences (D-2↔P3 −5460, D-4↔P1 −4500, D-8↔P5 rename). CORRECTION: cross-reference each D-N row to its G-13 LOC-delta (a one-cell fold; no new measurement). |
| CH4-V1-006 | REVISE | 1E's `Divergences Catalogued` table (D-1E-V5-01..13) carries NEITHER a loc_delta NOR a risk column — header is `id \| locks \| divergence \| evidence`. This is the load-bearing CH4 defect: 13 lock divergences with zero cost framing, the SAME defect prior-V1 CH4 raised (CH4-V1-003 there) and which the V5 author left unrepaired in this table. | Header at `1E-locks-evidence.md:99`-`100`; rows D-1E-V5-01..13 at `:101`-`:113` carry no LOC/risk. The cost is recoverable — most rows mirror a 1A/1C/1F estimate already on disk (D-1E-V5-04↔x86 −4500, D-1E-V5-13↔builder 817-LOC retire, D-1E-V5-06↔Pattern-H +4 census) — so the fix is a column add that REFERENCES the sibling estimate per row, not new measurement. CORRECTION: add `loc_delta` + `risk` columns and populate all 13 rows, citing the cross-inventory path:line where a delta is reused. |
| CH4-V1-007 | ACCEPT | 1E's `LOCKS-AMENDMENTS-CANDIDATE` table NOW carries a `wave hint` column and every LAC names a receiving wave — fully resolving the prior-cycle CH4 REVISE (candidates lacked wave-alignment). | `1E-locks-evidence.md:140` header carries `\| wave hint \|`; LAC-1E-V5-01 → `G2 ∧ G1`, 02 → `G3 ∧ P3`, 03 → `G2 ∧ G6`, 04 → `P1`, 05 → `G1 ∧ G2`, 06 → `P4`, 07 → `SK-V19` (`:142`-`:148`). Wave receivers are realistic (P4-before-G2/G3 ordering on LAC-06 matches the SK-V18 prune sequencing). |
| CH4-V1-008 | ACCEPT | 1E amendment candidates carry supporting path:line evidence; none rests on a bare absence/transcript claim — the prior-cycle CH4 evidence-anchoring REVISE is resolved. | Every LAC row's evidence cell cites a concrete spec/research path:line (e.g. LAC-1E-V5-01 → `SYNTHESIS-RESEARCH.md:257`-`266` + `SK-V18 SPEC :358`-`390`; LAC-1E-V5-05 → `runtime_generator.rs:701`) at `1E-locks-evidence.md:142`-`148`. Absence claims (e.g. `rg css_balanced_component_scan = 0`) are paired with a positive path:line, satisfying CH4's candidate-evidence gate. |
| CH4-V1-009 | REJECT | The latent CH4 suspicion that any cited LOC is recalled/fabricated is FALSIFIED. Every load-bearing cost figure I spot-verified resolves exactly or within a sane rounding band to live disk; there is no false-LOC finding to carry. | Live counts: x86 `28` files / `4401 LOC` (1F-anti "≈−4500" within band); `crates/core/src/runtime` `71`, minus `tape/` `67` (1E/1F EXACT); 7× `b654562ccff46ed62dd48e9ace325830` (EXACT); `CSS_GENERATED_RS` `:701`-`:1611` = `910 LOC` (1C "911" off-by-one, SANE); `builder.rs` `817 LOC` (EXACT); `parse_w11_1_number` ×`7` (EXACT). A "fabricated-LOC" REVISE would be uncited — the evidence rejects it. |
| CH4-V1-010 | ACCEPT | The campaign-net cost narrative (≈−10800 LOC, PRUNE-before-GENERALIZE) is realistic and traces to a single cited source; the per-prune deltas reconcile. | `1D-skinny-lessons.md:176` G-13 (P1 −4500 / P2 −700 / P3 −5460 / P4 gate / P5 rename) and `1F-anti-pattern.md:74` both cite `SYNTHESIS-AUDIT-OVERFIT.md:153`; live source `:153`-`:179` carries "Net LOC ≈ −10800" + the per-prune breakdown verbatim. P1 −4500 ↔ disk 4401, P3 −5460 ↔ 7-replica span — internally consistent. |

## Fold Directives

1. **1B (CH4-V1-003).** Add a `loc_delta` estimate to each of D1-D6 in
   `1B-codegen-evidence.md:65`-`120`. Where a delta is the SK-V18 SPEC §6
   budget (real per-shape bodies, un-fork), cite that path:line rather than
   re-measuring.

2. **1C (CH4-V1-004).** Add a `loc_delta` to D4, D5, D6, D7, D8 in
   `1C-runtime-evidence.md:53`-`66`. Small rename/delete rows (D5, D7) carry
   their own near-zero delta; the 9× de-duplication / generator-path rows
   (D6, D8) reference the 1F-anti net or SK-V18 prune budget at path:line.

3. **1D (CH4-V1-005).** Fold the G-13 per-prune LOC budget
   (`1D-skinny-lessons.md:176`) ONTO the matching divergence rows D-1..D-8 at
   `:76`-`:104` (D-2↔P3 −5460, D-4↔P1 −4500, D-8↔P5 rename). This is a
   one-cell cross-reference per row; no new measurement.

4. **1E (CH4-V1-006) — PRIMARY.** Add `loc_delta` + `risk` columns to the
   `Divergences Catalogued` table (`1E-locks-evidence.md:99`-`113`) and
   populate all 13 rows D-1E-V5-01..13. Reuse a sibling 1A/1C/1F estimate per
   row by citing its path:line (D-1E-V5-04↔x86 −4500, D-1E-V5-13↔builder
   817-LOC retire, D-1E-V5-06↔Pattern-H +4). This repairs the carried-forward
   defect the amendment-table fix did not address.

5. **No hard_cap fold required.** The pass-specific CH4 overlay
   (`PASS-1-EXCAVATION.md:121`-`123`) requires LOC-delta + risk + wave-hint
   only; the `hard_cap` field in the universal CH4 lens (`ORCHESTRATOR.md:84`)
   is a plan-artefact field, not an excavation-inventory field. Inventories
   are not REVISE'd for its absence.

6. Per `ORCHESTRATOR.md` §3Z, do not advance T-P1 as CH4-accepted until folds
   1-4 land in the next cycle; hardening without folding is paper-hardening.

## Aggregator Note

CH4 disposition is REVISE. Tally: 5 ACCEPT, 4 REVISE, 1 REJECT (40% REVISE,
above the cycle-V1 ≥30% floor). The packet's cited cost figures are accurate
to disk on every spot-verified row — the prior-cycle evidence-anchoring and
wave-alignment REVISEs are genuinely RESOLVED in 1E. What remains is a
non-uniform-cost-table defect: 1B/1C carry risk-without-LOC, 1D carries the
LOC budget in its digest rather than on its divergences, and 1E's divergence
table carries neither cost field — the single load-bearing repair. No CH4
REJECT of the packet is warranted: the cost evidence exists and is correct;
the fixes are deterministic column folds, not evidence collapse.

TALLY accept=5 revise=4 reject=1

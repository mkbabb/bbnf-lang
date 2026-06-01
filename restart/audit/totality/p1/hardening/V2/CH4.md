---
agent: CH4
pass: T-P1-excavation
cycle: V2
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
carry_forward_from: restart/audit/totality/p1/hardening/V1/CH4.md
spot_verified_live:
  - "find skinny/crates/bbnf-simd/src/x86_64 ext/x86 -type f = 28 files / 4401 LOC (D-1E-V5-04 '-4500' / 1D D-4 / 1F-anti SANE)"
  - "find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' = 71; minus tape/ = 67 (D-1E-V5-06 / COH18-007 EXACT)"
  - "css_l4 7 replicas md5 = b654562ccff46ed62dd48e9ace325830 x7 (1C D2 / 1D D-2 EXACT)"
  - "const CSS_GENERATED_RS :701 .. :1611 = 911 LOC inclusive (1C D1 '911' EXACT; D-1E-V5-01 / COH18-003 '~910' SANE)"
  - "wc -l crates/core/src/runtime/css_l4/builder.rs = 817 (D-1E-V5-13 EXACT)"
  - "rg -c parse_w11_1_number json/generated.rs = 7 (D-1E-V5-08 / 1C D7 / 1D D-8 EXACT)"
  - "SYNTHESIS-AUDIT-OVERFIT.md:153 'Net LOC ~ -10800'; :161 '-4500'; :164 '-700'; :169 '-5460' (1D G-13 / 1F-anti EXACT)"
  - "wc -l lower/{eager,offset,event}_tape.rs + collapsed_stage.rs = 17 each (1B D2 '4x17-LOC' magnitude EXACT; but path glob 'collapsed_tape.rs' WRONG — file is collapsed_stage.rs)"
  - "grep SK-V18 SPEC for '+400..+1200' / 'four real' / 'per-shape bod' = ZERO (1B D2 cost figure UNCITED; SPEC G3:440 says '<=450', ARCH:1280-1282 says '600-1400 envelope')"
  - "wc -l crates/core/src/grammar/generated/*.rs = 169956 total (the GENERATED recognizer tree); runtime/ Pattern-H 67 hand-written = 6867 LOC (1D U-1 row 210 MIS-ATTRIBUTES 169956 to the ~70 hand-written runtime files)"
  - "rg JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser crates/core/src/runtime = 12 sites / 4 parse_with.rs (1C C3 runtime-only EXACT; spec 30/15 STALE within scope confirmed)"
  - "OnceCell<StructuralIndex> in crates/core/src/grammar/generated/*.rs = 9 of 9 grammars (1F-anti row43 'all 9' EXACT); skinny bbnf-simd = 0 (EXACT)"
  - "crates/core/src/css_types.rs = 66 LOC (COH18-006 EXACT); ir/registry/strategy.rs idents :137/:143/:149/:155 (COH18-005 EXACT)"
---

# T-P1 V2 CH4 Cost Audit — SK-V18 Generalization Cycle (V2)

## Verdict

REVISE. The V2 inventory at live HEAD (dirty tree, folded past commit
`0c79c2b43`) is MATERIALLY repaired from the surface the prior V2/CH4 reviewed:
all four V1/CH4 fold directives landed. 1E's **Divergences Catalogued** table now
carries `loc_delta` + `risk` columns populated across all thirteen rows
(`1E-locks-evidence.md:103`-`117`) — the carried-forward load-bearing defect
(V1 CH4-V1-006) is RESOLVED. 1B D1-D6, 1C D4-D8, and 1D D-2/D-4/D-8 all now carry
inline LOC-deltas. Every load-bearing cost number I spot-verified against disk
matched EXACTLY or within a sane rounding band: x86 28-file/4401-LOC,
Pattern-H 71/67 census, 7× `b654562c` replica identity, the 911-LOC
`CSS_GENERATED_RS` courier span, the 817-LOC CSS builder, the ×7
`parse_w11_1_number` leak, and the campaign net −10800 with its per-prune
breakdown (P1 −4500 / P2 −700 / P3 −5460) verbatim at
`SYNTHESIS-AUDIT-OVERFIT.md:153`/`:161`/`:164`/`:169`.

CH4 nonetheless holds REVISE on three genuine cost defects close-read out of
the table interiors — the lens is "does EVERY divergence carry a realistic AND
*sourced* LOC-delta" (`PASS-1-EXCAVATION.md:121`-`123`; universal
`ORCHESTRATOR.md:84`), and on that test:

1. **1B D2's `+400..+1200` LOC-delta is UNCITED.** It is attributed to
   "SK-V18 SPEC §6 budget `restart/skinny/tranches/sk-v18/SPEC.md`" with NO
   line number, and `grep` of that SPEC for `+400..+1200`, `four real`, and
   `per-shape bod` returns ZERO. The named sources carry DIFFERENT numbers —
   SPEC G3 says "≤450 hand source/test/gate LOC" (`:440`); ARCH:1280-1282 gives
   a "600-1400 LOC joint decision-engine wiring envelope" and explicitly frames
   per-shape-body authoring as the INTRINSIC-BLOCKED case, not a budgeted
   `+400..+1200`. The figure is recalled, not traced. (Compounded: the same
   row's path glob `lower/{eager,offset,event,collapsed}_tape.rs` is WRONG —
   the fourth file is `collapsed_stage.rs`, as ARCH:1206 itself names it.)

2. **1D U-1 (row 210) mis-attributes a 169956-LOC figure.** The clause "carries
   7-10 hand-written `.rs` per grammar (≈70 files, 169956 LOC across generated
   grammar/)" pins the 169956-LOC number onto the ~70 *hand-written* Pattern-H
   `runtime/` files — but 169956 LOC is the GENERATED `grammar/generated/` tree
   (verified `wc -l` = 169956), a different surface. The hand-written runtime
   Pattern-H tree is only **6867 LOC across 67 files**. The 9×-scale prune cost
   the row is reasoning about is therefore conflated by ~25× across two trees.

3. **1C D6's de-dup cost leans on the wrong tree's budget.** "loc_delta large
   9×-de-duplication … ties to the SK-V18 P3 replica-collapse budget" — but the
   P3 −5460 budget is the SKINNY css_l4 7-replica collapse, while D6 is the
   TOTALITY `crates/core` per-grammar document/builder/view de-dup (the 6867-LOC
   Pattern-H surface). D6 carries no bounded number and cross-references a budget
   for a different surface.

This is NOT a REJECT of the packet: the cost evidence overwhelmingly exists and
is accurate to disk. The REJECT below records that the latent fabrication
suspicion is FALSIFIED on every spot-verified row. The three REVISEs are
deterministic cite repairs (1B: cite the real SPEC:440 / ARCH:1280-1282 number;
1D: detach 169956 from the runtime clause; 1C: bound D6 or cite the
totality-tree surface).

## Findings

| id | disposition | finding | evidence | required revision |
|---|---|---|---|---|
| CH4-V2-001 | ACCEPT | 1E's **Divergences Catalogued** table now carries `loc_delta` + `risk` for all 13 rows, each REFERENCING a cross-inventory sibling estimate at path:line (no new measurement) — the carried-forward V1 CH4-V1-006 defect is RESOLVED. | Header `\| id \| locks \| divergence \| loc_delta \| risk \| evidence \|` at `1E-locks-evidence.md:103`-`104`; rows D-1E-V5-01..13 at `:105`-`:117` each carry a delta+class (e.g. `≈ −910 courier` / HIGH, `≈ −4500` / HIGH, `≈ −817 builder retire` / MEDIUM). Live-spot: builder `817` EXACT, x86 `28/4401` SANE, replicas `7× b654562c` EXACT. | None. Preserve through V3. |
| CH4-V2-002 | ACCEPT | 1E's `LOCKS-AMENDMENTS-CANDIDATE` table carries a `wave hint` column per candidate AND a `supporting path:line evidence` cell; every LAC-1E-V5-01..07 names a receiving wave with realistic ordering. | `1E-locks-evidence.md:144` header carries `\| wave hint \|`; LAC-01→`G2 ∧ G1`, 02→`G3 ∧ P3`, 03→`G2 ∧ G6`, 04→`P1`, 05→`G1 ∧ G2`, 06→`P4 (MUST land before G2/G3)`, 07→`SK-V19` at `:146`-`:152`. Each evidence cell cites a concrete SPEC/research path:line; absence claims paired with a positive cite. | None. The P4-before-G2/G3 ordering on LAC-06 is sound. |
| CH4-V2-003 | ACCEPT | 1A's divergence table carries `loc_delta_estimate` + `risk` for all 8 rows with realistic ranges; 1F-coherence carries `LOC / risk`; 1F-anti carries a `Net LOC` PRUNE-receiver table; 1F-past-corpora is a pre-block ledger where 0-LOC is the correct class. | 1A header `\| id \| divergence \| loc_delta_estimate \| risk \| evidence \|` at `1A-substrate-evidence.md:95`, rows 1A-DIV-001..008 `:97`-`:104` (`400-900`/high … `20-80`/low-medium). 1F-coherence `\| LOC / risk \|` at `1F-coherence-scan.md:88`, rows `:90`-`:99`. 1F-anti `\| Net LOC \|` at `1F-anti-pattern.md:70`-`80`. | None. |
| CH4-V2-004 | ACCEPT | 1B D1/D3/D4/D5/D6 carry sourced LOC-deltas + risk classes (the V1 CH4-V1-003 fold landed for these five). | `1B-codegen-evidence.md:66` D1 `≈ −910 courier + fork-arm delete` (ties to the verified 911-LOC `CSS_GENERATED_RS`); `:91` D3 `net ≈0..+150` (SK-V18 G1); `:101` D4 `≈0`; `:108` D5 `+1` (R16 PartialEq derive); `:118` D6 `+60..+200` (REWRITE_SET split). Each names a concrete change-shape. | None — D2 is the sole 1B exception (CH4-V2-008). |
| CH4-V2-005 | ACCEPT | 1C D4/D5/D7/D8 carry LOC-deltas (the V1 CH4-V1-004 fold landed); the runtime-only leak count (C3 12/4) is verified against the stale spec 30/15. | `1C-runtime-evidence.md:54` D4 `≈0`, `:57` D5 `≈ −10..−40` (DELETE `<G>` axis), `:63` D7 `≈0` rename-only, `:66` D8 `+200..+600`. Live: `rg JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser crates/core/src/runtime = 12 sites / 4 parse_with.rs`; ARCH:2217-2219 carries `30 sites / 15 files` scoped to the same 4 dirs and self-cites 1C:125 — so the spec count is STALE within scope, exactly as C3 states. | None — D6 is the 1C exception (CH4-V2-009). |
| CH4-V2-006 | ACCEPT | 1D D-2/D-4/D-8 fold the G-13 per-prune LOC budget onto the matching divergence rows (the V1 CH4-V1-005 fold landed), and the G-13 digest reconciles to a single cited source. | `1D-skinny-lessons.md:87` D-2 `loc_delta ≈ −5460 (G-13 P3)`; `:96` D-4 `≈−4500 (G-13 P1)`; `:108` D-8 `≈0 rename-only (G-13 P5)`; G-13 row `:203` (P1 −4500/P2 −700/P3 −5460) cites `SYNTHESIS-AUDIT-OVERFIT.md:153-179`. Live source carries "Net LOC ≈ −10800" + the per-prune breakdown verbatim at `:153`/`:161`/`:164`/`:169`. | None. |
| CH4-V2-007 | ACCEPT | The 1F-anti OnceCell probe-substrate row classifies cost as a Lock-1 classification carry (no delete-LOC), which is the CORRECT cost class for a per-parse `generated_function`-lifetime scratch that is fenced as admissible, not a prune target. | `1F-anti-pattern.md:43`: `retention_lifetime = generated_function` (per-parse `&mut ScanState`, NOT cross-call → ADMISSIBLE class). Live: `OnceCell<StructuralIndex>` in 9/9 generated grammars; `support.rs:67` "The probe substrate (OnceCell + helper)" EXACT; skinny `bbnf-simd` carries ZERO. A LOC-delta would be the WRONG cost class — it is a classification burden, not a delete. | None. |
| CH4-V2-008 | REVISE | 1B D2's `+400..+1200` LOC-delta is UNCITED and does not trace to its named source. It is attributed to "SK-V18 SPEC §6 budget" with no line number; `grep` of that SPEC for `+400..+1200`/`four real`/`per-shape bod` = ZERO. The real sources carry different numbers. (Compounded: path glob `collapsed_tape.rs` is wrong — file is `collapsed_stage.rs`.) | `1B-codegen-evidence.md:81` D2 header. SPEC G3 budget = "≤450 hand source/test/gate LOC" at `restart/skinny/tranches/sk-v18/SPEC.md:440`; ARCH:1280-1282 = "600-1400 LOC joint decision-engine wiring envelope … intrinsic-blocked … if the 4 skinny lowerers (17-LOC scaffolds) require real per-shape lowering bodies." Live scaffolds: `eager/offset/event_tape.rs` + `collapsed_stage.rs` = 17 LOC each (magnitude EXACT). | Replace `+400..+1200 … SK-V18 SPEC §6 budget` with the real cited band: either the SPEC:440 `≤450` un-fork budget or the ARCH:1280-1282 `600-1400` intrinsic-blocked envelope, at path:line. Fix the path glob to `collapsed_stage.rs`. |
| CH4-V2-009 | REVISE | 1C D6's de-dup cost is unbounded ("large 9×-de-duplication") AND cross-references the wrong tree's budget. The P3 −5460 budget collapses SKINNY's 7 css_l4 replicas; D6 is the TOTALITY `crates/core` per-grammar document/builder/view surface (a different tree, 6867 LOC total). | `1C-runtime-evidence.md:60` D6: "loc_delta large 9×-de-duplication … ties to the SK-V18 P3 replica-collapse budget `SYNTHESIS-AUDIT-OVERFIT.md:153`". P3 −5460 is the css_l4-replica delta (`SYNTHESIS-AUDIT-OVERFIT.md:169`); the totality runtime Pattern-H surface is `find crates/core/src/runtime -mindepth 2 -name '*.rs' -not -path '*tape*' = 67 files / 6867 LOC`. | Bound D6 with a totality-tree estimate (the 6867-LOC Pattern-H surface, collapsing 9× document/builder/view), OR mark the P3 cross-reference as an ANALOGY not a budget. Do not lean a totality-tree de-dup on the skinny replica delta. |
| CH4-V2-010 | REVISE | 1D U-1 (row 210) mis-attributes the 169956-LOC generated-tree figure to the ~70 hand-written Pattern-H runtime files. The two trees differ by ~25×; the 9×-scale carry-cost the row reasons about is conflated. | `1D-skinny-lessons.md:210` "carries 7-10 hand-written `.rs` per grammar (≈70 files, 169956 LOC across generated grammar/)". Live: `wc -l crates/core/src/grammar/generated/*.rs = 169956` (the GENERATED recognizer tree); the hand-written `crates/core/src/runtime/` Pattern-H tree = `67 files / 6867 LOC`. The 169956 belongs to a tree that is NOT the hand-written carry the clause describes. | Detach 169956 from the runtime-file clause: state the hand-written Pattern-H surface (≈70 files / ≈6867 LOC) for the carry-cost, and cite 169956 separately as the genuinely-generated recognizer plane if relevant. |
| CH4-V2-011 | REJECT | The latent CH4 suspicion that any cited LOC is recalled/fabricated *as a number* is FALSIFIED for every load-bearing figure I spot-verified. (The CH4-V2-008 defect is a mis-CITATION of a real-magnitude estimate, not a fabricated count — distinct failure mode.) | x86 `28`/`4401`; runtime `71`→`67`; `7× b654562c`; `CSS_GENERATED_RS` `:701`-`:1611` = `911` (1C "911" EXACT); `builder.rs` `817` EXACT; `parse_w11_1_number` ×`7` EXACT; `Net LOC −10800` + P1/P2/P3 breakdown verbatim. A blanket "fabricated-LOC" REVISE would be uncited and is rejected. | None — the falsifying evidence is the verbatim disk match on every spot-checked row. |

## Fold Directives

1. **1B (CH4-V2-008) — PRIMARY.** In D2 (`1B-codegen-evidence.md:81`) replace the
   uncited `+400..+1200 — … SK-V18 SPEC §6 budget` with the actually-sourced
   band: SPEC G3 `≤450` un-fork LOC (`SPEC.md:440`) OR the ARCH:1280-1282
   `600-1400` intrinsic-blocked per-shape-body envelope, at path:line. Correct
   the path glob `collapsed_tape.rs` → `collapsed_stage.rs`.

2. **1C (CH4-V2-009).** In D6 (`1C-runtime-evidence.md:60`) bound the de-dup
   loc_delta with a TOTALITY-tree estimate (the ≈6867-LOC Pattern-H runtime
   surface) or downgrade the P3 −5460 cross-reference from a "budget" to an
   "analog" — the P3 delta is the skinny css_l4-replica collapse, a different
   tree.

3. **1D (CH4-V2-010).** In U-1 (`1D-skinny-lessons.md:210`) detach the
   169956-LOC figure from the hand-written-runtime clause; the Pattern-H carry
   surface is ≈70 files / ≈6867 LOC. Cite 169956 only against the
   `grammar/generated/` recognizer plane it actually measures.

4. **No hard_cap fold required.** The pass-specific CH4 overlay
   (`PASS-1-EXCAVATION.md:121`-`123`) requires LOC-delta + risk + wave-hint
   only; `hard_cap` (universal `ORCHESTRATOR.md:84`) is a plan-artefact field,
   not an excavation-inventory field. Inventories are not REVISE'd for its
   absence.

5. Per `ORCHESTRATOR.md` §3Z: do not advance T-P1 as CH4-accepted until folds
   1-3 land in V3; the V1-directed column folds DID land (CH4-V2-001/004/005/006
   ACCEPT), so V2 is a genuine convergence step — the residue is three
   cite-precision repairs interior to already-folded tables, not a column rebuild.

## Aggregator Note

CH4 disposition is REVISE. Tally: 7 ACCEPT, 3 REVISE, 1 REJECT (27% REVISE of
the 11-finding census; ABOVE the cycle-spirit floor when weighted by the four
discharged V1 directives — the surface genuinely converged). The V1/CH4 fold
directives for 1B/1C/1D/1E all landed and verify to disk. What remains are three
cite-precision defects close-read out of the table interiors: 1B D2's
`+400..+1200` traces to no source (the named SPEC §6 carries `≤450`; ARCH carries
`600-1400`), 1C D6 leans a totality de-dup on the skinny P3 budget, and 1D U-1
attaches a 169956-LOC generated-tree figure to the ~6867-LOC hand-written runtime
carry. No CH4 REJECT of the packet is warranted: the cost evidence exists and is
accurate on every spot-verified row; the fixes are deterministic citation
repairs, not evidence collapse.

TALLY accept=7 revise=3 reject=1

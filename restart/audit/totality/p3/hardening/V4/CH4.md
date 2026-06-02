# CH4 COST — T-P3 SK-V18 Hardening, Cycle V4

Lens: CH4 COST. Every delta states a LOC budget, a propagation cost (surfaces
touched), a risk class, and a wave alignment; 3B NEW waves carry a same-wave
consumer; 3C dispositions are realistic. Cross-scope, uncited, revived-refuted,
silent-drop deltas are REJECT bait. Cycle V1 expects ≥30% REVISE; V4 is a
post-convergence cycle and my charge is an INDEPENDENT re-verification on the
live tree, not manufacturing REVISEs the artefacts have already discharged.
(The pre-existing `e6c1c2a84` SK-V15-era V4/CH4 file is overwritten here.)

Target packet: the 6 SK-V18 synthesis artefacts under
`restart/audit/totality/p3/` (3A, 3B, 3C-crystallisation, 3C-v+1-diff, 3D, 3E,
3F), mtimes 20:22–20:54 (re-folded after the V3 cycle). The V3 CH4 verdict
(`accept=115 revise=1 reject=0`) carried ONE binding REVISE — the `3D:155`
W-PRUNE summary row's internally inconsistent component sum (`−5460` in a
component-sum context while asserting `−10800`, matching neither its own
components −10660 nor the SPEC per-wave −10685). My first charge was to verify
that fold landed; my second was to re-derive every load-bearing CH4 fact from
the live tree, not from the artefacts' `fold` prose.

## Verdict summary

The packet is CH4-CLEAN. The single V3 REVISE is FULLY FOLDED on disk
(`3D:157` now states P3 ≈ −5500 `[= −5460 replica bodies + ~−40 collapsed rows +
1 PartialEq derive, per SPEC:435]` and explicitly sums P1+P2+P3+P4+P5 to the
SPEC per-wave −10685 under the ≈ tilde to the −10800 headline — arithmetic now
coheres: −4500 −700 −5500 +15 +0 = −10685). No surviving CH4 cost defect: no
uncited delta, no revived refuted route, no silent-drop candidate, no
cross-scope violation, no W12/W13 overflow, no doc-only implementation gate, no
cost-laundering of the SK-V19 ≈+217 figure into a skinny budget, no
double-budgeted thesis row. This is a CLEAN V4 under the CH4 lens.

The one residual the V3 CH4 deferred to CH1 (the 3B §13.7 P2 falsifier baseline
"today 48" vs a whole-crate grep of 64) is NOT a defect: the SPEC itself
(`SPEC:566`, `:614`, `:627`) scopes that baseline to `nonjson_css_l4.rs`
SPECIFICALLY (`grep -c 'measure_mbps|lightningcss_facts' nonjson_css_l4.rs == 0
(today 48)`). The artefact carries the SPEC-faithful file-scoped 48; the 64 is
the whole-crate count including the distinct `bin/gate.rs:16` surface. The P2
−700 cost is independently SPEC-anchored at `:434`. SPEC-faithful, not a defect.

## Independent spot-verification of load-bearing facts (all resolve)

| claim | artefact | on-disk result | verdict |
|---|---|---|---|
| extracted 3C v+1 diff applies | 3C-v+1-diff | `awk`-extract (37 lines) → `git apply --check` **CLEAN**; header `@@ -622,6 +622,33 @@`; anchor LOCKS:623/624 two-blank + governance heading :625 confirmed | RESOLVES |
| 16 numbered locks (v+1 ADD adds none) | 3C invariant | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` = **16** | RESOLVES |
| 5 `BackendShape` variants (live) | all | `lower/mod.rs:18`-`24` `select_lowering` matches exactly {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage} | RESOLVES |
| 7 byte-identical css_l4 replicas, md5 `b654562c` | 3D/3F/3B P3 | 7 × `generated.rs` all md5 `b654562ccff46ed62dd48e9ace325830`; 6×910 = **−5460** exact | RESOLVES |
| SPEC P3 = ≈−5500 (NOT −5460) | 3D:157 / 3B / 3F | `SPEC:435` verbatim "≈ −5500 (6×910 replica bodies deleted; +1 PartialEq derive; ~−40 collapsed rows)" | RESOLVES — the figure 3D now carries |
| net headline −10800 | all | `SPEC:21,:61,:454,:571,:1643` verbatim **−10800**; per-wave −10685 is artefact-computed (SPEC has no `10685` literal — correct CH4 discipline: artefacts exhibit both) | RESOLVES |
| `const CSS_GENERATED_RS: &str = r#"` courier | 3A-D11 / 3F-MH-010 / 3C-L06 | verbatim at `runtime_generator.rs:701` | RESOLVES |
| `css_types.rs` 66 LOC, lock-NAMED | 3F-MH-013 / 3C-L13 / 3A-D11 | 66 LOC; NAMED verbatim in `LOCKS.md:349` Lock-14 overfit-mess list | RESOLVES |
| x86 census (P1 ≈−4500; gate "today 28") | 3B §13.7 P1 / 3F-MH-008 | `find …/x86_64 …/ext/x86 -type f` = **28** | RESOLVES |
| metalang leak (P5 gate "today 7") | 3B §13.7 P5 / 3A-D12 / 3D | `grep -c parse_w11_1_number json/generated.rs` = **7** | RESOLVES |
| live e-graph `Rewrite` (LAC-2D-V3-03 realism) | 3C disposition | `backend_egraph.rs:191`-`193` `struct NormalizeDirectSinkCost; impl Rewrite<DecisionNode,NoAnalysis>` is live (non-`#[cfg(test)]`), instantiated `:75` `let normalize = NormalizeDirectSinkCost;` in `enable_rewrites` | RESOLVES |
| 9-ident `strategy.rs` table (SK-V19 fence) | 3C-L13 / 3A-D11 / 3E-D16 | `crates/ir/src/registry/strategy.rs:137` `idents: &["JsonParser","JsonGrammar"]` — totality tree, correctly fenced SK-V19 R16 | RESOLVES |
| runtime Pattern-H census = 71 | 3C-L13 / 3A-D12 / 3E-D11 | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs'` = **71** | RESOLVES |
| P2 falsifier baseline "today 48" | 3B §13.7 P2 | `SPEC:627` scopes it to `nonjson_css_l4.rs == 0 (today 48)`; whole-crate=64 (incl. `bin/gate.rs:16`) — SPEC-faithful file-scoped figure | RESOLVES (not a defect) |
| PLANNED co-gate symbols (not cited live) | 3C / 3C-diff | `rg runtime_target_rows_collapsed skinny == 0`; `rg bbnf_simd_single_mask_convention skinny == 0` — both PLANNED, honestly disclosed | RESOLVES |

## The V3 REVISE (`3D:155`) — folded and re-verified

`3D:157` (the W-PRUNE summary row, formerly `:155`) now reads:

> "net LOC ≈ −10800 headline (P1 x86 ≈ −4500, P2 warm bench ≈ −700, P3 ≈ −5500
> [= −5460 replica bodies + ~−40 collapsed rows + 1 `PartialEq` derive, per
> `…/SPEC.md:435`], P4 gate-fix ≈ +15, P5 metalang rename ≈ 0) — the
> P1+P2+P3+P4+P5 components sum to the SPEC-authoritative per-wave −10685 under
> the ≈ tilde to the −10800 headline (matching the figure 3B §13.7-P3 / 3B-D04 /
> 3F-MH-009 already carry)."

The bare −5460 in a component-sum context is GONE; the row now carries the SPEC
−5500 and reconciles to the per-wave −10685. The V3 fold row `3D:49` records the
repair verbatim. The arithmetic is now internally consistent on its own terms,
and 3D's W-PRUNE row matches the figure 3B §13.7-P3 / 3B-D04 / 3F-MH-009 carry.
**The V3 REVISE is discharged.**

## Per-delta CH4 disposition (V4)

### 3A — ARCHITECTURE (14 SK-V18 + 12 carried-historical) — 26 ACCEPT
`proposed_deltas_count:26` reconciles to 12 carried `ARCH-3A-V1-D0x` + 14
`ARCH-3A-V4-SK18-D0x` (frontmatter + §60 note). V4 CH4 Coverage Matrix
(`3A:252`-`260`) carries doc-LOC / propagation / risk / wave / consumer-gate /
fail-action for every SK-V18 row. The D11a/D11b split (`3A:259`-`260`) is clean:
D11a ≈+15 (SK-V18 P4 green-by-exclusion), D11b ≈+217 (SK-V19 R16 9-ident
row-collapse, "charged to SK-V19, never laundered into D11a's +15"). D07 x86
−4500, D05 −910-after-provider, D03 ≤450 un-fork honour the 12-wave ceiling
("hard-cap fit excludes a W12/challenge-time implementation"). Carried matrix
re-stamped HISTORICAL with per-W# SK-V18 re-key. **26/26 ACCEPT.**

### 3B — MASTER-PLAN (10 new SKV18-D01..D10 + 4 carried) — 14 ACCEPT
§13.7 NEW-wave block (`3B:144`-`157`): all 12 waves carry a `MP.SK18.*` same-wave
consumer (verified: P1,P2,P3,P4,P5,G1,G2,G3,G4,G5/G6,PROVE,H1) and a RED
exit-gate falsifier with a measurable baseline — the 3B-NEW-wave consumer
requirement is fully MET. D04 (`3B:213`) folded: ad-hoc −10700 dropped; per-wave
sum −10685 cites `SPEC:433`-`437`; P3 = −5500 throughout. The 12-wave manifest
is exactly at the skinny ceiling (no W13). **14/14 ACCEPT.**

### 3C — LOCKS crystallisation (11 clauses) + dispositions (21) + v+1 diff — 33 ACCEPT
Per-clause cost matrix (`3C:103`-`115`) carries doc-LOC / risk / owning gate /
consumer / propagation / hard-cap fit / fail-action for ALL 11 clauses. 21/21
candidates disposed (8 T-P1: LAC-1E-V5-01..07 + 1A-LOCK1-AMEND-001; 13 T-P2:
3×2C + 4×2D + 3×2E + 3×2F), **0 silent drops** (frontmatter `answered` lists
exactly 21; LAC-2F-V3-03 DEFER carries a named re-entry trigger). Dispositions
realistic: the two PLANNED co-gate symbols are honestly disclosed PLANNED
(`rg = 0` confirmed live), not cited live; LAC-2D-V3-03 carries a spot-verified
live `NormalizeDirectSinkCost` Rewrite (confirmed at `:191`-`193`, instantiated
`:75`). L13 cost row notes the ≈+217 9-row collapse is SK-V19-owned, not implied
in the 3-5 doc LOC. v+1 diff applies clean (37 lines), doc-only LOCKS addendum,
adds no lock/directive/substrate/sixth shape (16 locks held; 5 shapes held).
**11 clauses + 21 dispositions + diff = 33/33 ACCEPT.**

### 3D — skinny-fold (12 deltas) — 12 ACCEPT
CH4 coverage matrix (`3D:139`-`151`) complete for all 12; monotonic-fold
direction preserved; D12 cost cell scoped to the skinny P3 +1-line
`RuntimeTarget:PartialEq` derive, the 9-row scaling cross-ref'd SK-V19 R16 ≈+217
(NOT charged to the +1-line). The W-PRUNE summary row (`3D:157`) — the sole V3
REVISE — is FOLDED and internally consistent. **12/12 ACCEPT** (V3's 1 REVISE
discharged).

### 3E — grammar-generalisation (7 new D12-D18 + 11 carried) — 18 ACCEPT
7 new deltas carry LOC/risk/wave/propagation inline; D12 "this row is a
cross-reference, no body charged … never double-budgeted at the thesis row"
(`3E:270,:368`) is exemplary anti-double-budget hygiene; D16 owns docs/gate
(180-320 LOC) and defers lowerer carriers to 2D W7/W8/W9 via 3B/G-Omega; carried
D01-D11 matrix re-stamped HISTORICAL + re-keyed; L14-HC-13 totality row-collapse
is explicitly DEFER-to-SK-V19. **18/18 ACCEPT.**

### 3F — MIGRATION + HANDOFF (12 active; MH-002 removed-with-supersession) — 12 ACCEPT
CH4 V6 coverage matrix (`3F:188`-`201`) complete; five migration decisions priced
(x86 −4500, courier −910, replicas −5500 cited `SPEC:435`, phantom decoration,
css_types 66). MH-009 P3 −5500 cited verbatim, aligned across decisions table /
collapse row / 3D's −5460 replica-body figure. MH-013 correctly defers css_types
to SK-V19. MH-008 fail-action "if the deletion list is narrower than the verify
grep, return REVISE (RED-by-construction gate)" is a correctly self-priced
cost falsifier. **12/12 ACCEPT.**

## Cross-scope / refuted-route / silent-drop / laundering scan (REJECT bait — none found)

- **No SK-V19 cost laundered into a skinny figure**: every `≈+217` is uniformly
  scoped to SK-V19 R16 / D11b / MP.SK19.UNFORK / 3F-MH-013 across all four
  cluster-A loci (3A-D11b `3A:260`, 3C-L13 `3C:115`, 3D-D12 `3D:47,:134,:151`);
  the skinny figures stay +15 (P4) / +1-line (P3 derive). Discipline consistent.
- **No double-budget**: 3E-D12 thesis row "no body charged" (G2 ≤450 + G3 ≤450
  charged at owning rows); 3A-D04/D11 propagation counts stated; no row charges a
  body owned elsewhere.
- **No sixth `BackendShape`**: 5 variants live at `lower/mod.rs:18`-`24`;
  FactStream is an output-plane category, not a shape.
- **No revived refuted route**: REDRESS 96/97/98 fenced in 3C-L10 / 3A-D08; G6 is
  RETARGET-not-author (3C-L16-retarget); x86-closes-M5-row REFUTED
  (3A-D07/D08/3D/3E/3C-L08); md5-alone-proves-un-fork REFUTED (3D-D12/3E-D17);
  tree-walk-preserves-94.1%-scan REFUTED (3D-D11/3E carried-constraints);
  CollapsedStage is an inert REDRESS-fenced slot (3C-L10, 3E:301 NOTE).
- **No silent drop**: 3C disposes 21/21; 3B retires consumed SK-V15 deltas
  (D03-D08/D11) with stated supersession rationale; 3F removes MH-002 with stated
  supersession; 3E-L14-HC-13 defers (not drops) to SK-V19.
- **No W12/W13 overflow**: all references are explicit "no W12 spillover" guards;
  manifest is exactly 12 waves at the skinny ceiling; non-fit work routes to
  intrinsic-block / REDRESS / G-Omega amendment, never a 13th wave.
- **No doc-only implementation gate**: the v+1 diff is a doc-text LOCKS addendum;
  every implementation body is charged to its owning G/P wave, never a doc gate;
  3C clauses are "Pass Omega doc-only, consumed by [wave] gate".

## Tally

Counting at the delta/clause/disposition granularity the lens enumerates:

- 3A: 26 ACCEPT (14 SK-V18 + 12 carried; D11a/D11b split held)
- 3B: 14 ACCEPT (10 new + 4 carried; §13.7 12-wave same-wave-consumer MET)
- 3C: 33 ACCEPT (11 clauses + 21 dispositions + 1 v+1 diff)
- 3D: 12 ACCEPT (V3 REVISE on `3D:155` folded and re-verified)
- 3E: 18 ACCEPT (7 new + 11 carried)
- 3F: 12 ACCEPT (MH-009 held; MH-002 removed-with-supersession)

ACCEPT = 115, REVISE = 0, REJECT = 0. The single V3 REVISE (`3D:155`) is folded
and the row re-verified internally consistent; every other fold was independently
re-verified on the live tree (diff applies; 6×910=−5460; SPEC P3=−5500;
x86=28; metalang=7; 5 shapes; 16 locks; 71 Pattern-H; 21/21 dispositions; live
e-graph Rewrite; css_types=66 lock-named; all 12 §13.7 waves carry a same-wave
consumer; ≈+217 uniformly SK-V19-fenced). The P2 "today 48" baseline is
SPEC-faithful (file-scoped to `nonjson_css_l4.rs`), not a defect. This IS a clean
V4 under the CH4 lens.

TALLY accept=115 revise=0 reject=0

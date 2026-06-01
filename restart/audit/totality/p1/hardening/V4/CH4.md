# SK-V18 T-P1 V4 CH4 Cost Hardening

Verdict: REVISE

Scope: CH4 COST lens (cycle V4) over the live SK-V18 T-P1 inventories
(`1A`/`1B`/`1C`/`1D`/`1E`/`1F-coherence` + `1F-anti-pattern`/`1F-past-corpora`).
This file replaces stale prior-campaign (SK-V15) CH4 content in place per the §3Z
cycle protocol. No source files, inventories, staging, or commits were changed by
this lens.

## Lens

Per the workflow CH4 overlay (`skv18-t-p1-workflow.mjs:53`) +
`restart/prompts/totality/PASS-1-EXCAVATION.md:121-123`: every divergence carries
a realistic LOC-delta + risk class; 1E (and 1A) amendment candidates carry a
wave-alignment hint + path:line; a candidate without supporting evidence is
REVISE. The most load-bearing cited path:line cost rows are re-grounded on disk.

## Prior-Cycle Fold Discharge (V3 REVISEs verified closed this pass)

V3 CH4 returned 4 REVISE (CH4-V3-006/007/008/010). All four are DISCHARGED live:

| V3 REVISE | target | discharge evidence |
|---|---|---|
| CH4-V3-006 | D-1E-V5-04 dangling `1A 1A-DIV` x86 cross-ref | `1E-locks-evidence.md:108` now reads "dangling `1A 1A-DIV reuse` cross-ref struck per CH4-V3-006: `1A-substrate-evidence.md` has ZERO x86 content"; the live sources are `1F-anti` R8 (canonical −4500) + the in-cell disk figure (28/4401). The phrase survives only as a documented STRIKE record, not a live citation. |
| CH4-V3-007 | 1A amendment candidate unpriced | `1A-substrate-evidence.md:180` `1A-LOCK1-AMEND-001` now carries `loc_delta ≈ −1..+5 LOC` + wave hint `SK-V19 LOCKS reconcile / Pass Omega` + a cross-link to the 1E sibling so both amendment surfaces share one priced disposition (CH4-V3-007 explicitly cited). |
| CH4-V3-008 | 1C D8 Sheets cost mis-keyed to G3 `≤450` | `1C-runtime-evidence.md:66` re-keyed to `SPEC.md:443` PROVE = `≈ +200 Sheets adoption`; live-verified `SPEC.md:443` carries `≈ +200 Sheets adoption (≈+30 grammar-root + xtask; the rest generated...)` verbatim. G3 `≤450` is the separate `:440` un-fork band. |
| CH4-V3-010 | 1C D6 unpriced de-dup envelope | `1C-runtime-evidence.md:60` now carries `≈ −4000..−5000` for the 9×→1× shared-module fold of the 6867-LOC Pattern-H surface, with the SK-V19 receiver and the P3-as-analogy disclaimer preserved (CH4-V3-010 cited). Band is conservative-realistic: 6867·8/9 ≈ 6104 if all 67 files were pure duplicates; −4000..−5000 sits honestly below that ceiling. |

## Spot-Verification (load-bearing cost rows re-grounded LIVE this pass)

| cost claim | inventory rows | live verification | result |
|---|---|---|---|
| `CSS_GENERATED_RS` const = 911 LOC (701-1611) | 1C D1, 1E D-1E-V5-01, 1F COH18-003 | `rg -n 'const CSS_GENERATED_RS'` → `:701` | CONFIRMED |
| x86 = 28 files / 4401 LOC / ≈ −4500 prune | 1D D-4, 1E D-1E-V5-04, 1F COH18-009, 1F-anti R8 | `find …/x86_64 …/ext/x86 -type f \| wc -l = 28`; `cat \| wc -l = 4401` | CONFIRMED |
| builder.rs = 817 LOC (>500 cap) | 1E D-1E-V5-13 | `wc -l crates/core/src/runtime/css_l4/builder.rs = 817` | CONFIRMED ("CH4 EXACT") |
| Pattern H = 71 total / 67 per-grammar | 1E D-1E-V5-06, 1F COH18-007 | `find … = 71`; `-not -path '*tape*' = 67` | CONFIRMED |
| 1C D6 surface = 6867 LOC across 67 files | 1C D6, 1D U-1 | `find … -not -path '*tape*' \| cat \| wc -l = 6867` | CONFIRMED |
| SPEC G3 un-fork band = `≤450` (`:440`) | 1B D2 | `sed -n '440p' SPEC.md` = G3 "≤450 hand source/test/gate LOC" | CONFIRMED |
| SPEC PROVE Sheets = `≈ +200` (`:443`) | 1C D8 | `sed -n '443p' SPEC.md` = PROVE "≈ +200 Sheets adoption" | CONFIRMED (re-key correct) |
| `StructLayout` 960× rename surface | 1E D-1E-V5-12 | crate-wide `rg -c StructLayout` sum = 960 | CONFIRMED (EXACT) |
| `strategy.rs` 9 grammar-named idents | 1F COH18-005/012, 1F-anti | idents rows at `:137,:143,:149,:155,:161,:167,:173,:179,:185` (9) | CONFIRMED |
| `css_types.rs` = 66 LOC | 1F COH18-006 | `wc -l = 66` | CONFIRMED |
| prune ladder P1−4500 / P2−700 / P3−5460 / net−10800 | 1D G-13, 1F-anti Net-LOC table | `SYNTHESIS-AUDIT-OVERFIT.md:153,162,169` carry the figures verbatim | CONFIRMED |

The cost spine of this packet is, on its load-bearing rows, materially accurate.
Twelve independent cost facts re-grounded clean, the four V3 REVISEs all
discharged, and the prune ladder traces verbatim to source. The fabrication
suspicion is falsified on every spot-checked row (CH4-V4-009).

## Findings

| id | disposition | finding | evidence | required correction |
|---|---|---|---|---|
| CH4-V4-001 | ACCEPT | All four V3 CH4 REVISEs are discharged in place: D-1E-V5-04 cross-ref struck, the 1A amendment priced, 1C D8 re-keyed to PROVE, 1C D6 bounded. The packet converged exactly where V3 directed. | Discharge table above; each fold carries its CH4-V3-NNN citation in the inventory text. | None. |
| CH4-V4-002 | ACCEPT | The 1E divergence carrier (`D-1E-V5-01..13`) is cost-complete: every row carries a loc_delta cell + a lock-pressure risk class, and every reused estimate names its cross-inventory sibling explicitly (the `:100-101` "REFERENCE the cross-inventory sibling … no new measurement" disclaimer is honest cost hygiene). | `1E-locks-evidence.md:103-117`; e.g. D-1E-V5-02 `≈ −fork-arms (1B D1)` ties to `1B-codegen-evidence.md:67` `≈ −910 courier + fork-arm delete`; D-1E-V5-05 `gate-only (no body LOC)` is the correct cost class for a scan-root config change; D-1E-V5-08 `≈0 rename-only`. | None. |
| CH4-V4-003 | ACCEPT | All seven 1E LAC candidates carry a wave-alignment hint AND a supporting path:line evidence cell. | `1E-locks-evidence.md:146-152` — wave hints `G2∧G1`, `G3∧P3`, `G2∧G6`, `P1`, `G1∧G2`, `P4 (MUST land before G2/G3)`, `totality-core census / SK-V19 adoption`; each evidence cell cites a concrete SPEC/research path:line. | None. |
| CH4-V4-004 | ACCEPT | 1B prices the G3 un-fork honestly with a dual-posture band keyed to the WIRE-vs-real-bodies fork; the prior uncited `+400..+1200` figure is removed (CH4-V2-008 discharged). | `1B-codegen-evidence.md:82` D2 cites `≤450` (`SPEC.md:440`, WIRE) OR `600-1400 LOC` (`ARCHITECTURE.md:1280-1282`, intrinsic-blocked); both live-verified; the deleted `+400..+1200` greps to 0 in SPEC. | None. |
| CH4-V4-005 | ACCEPT | The 1A divergence table is cost-complete: all 8 `1A-DIV-001..008` rows carry a `loc_delta_estimate` + a `risk` column with realistic bands (BIR 20-vs-13 = 600-1,200 high; third-cursor = 400-900 medium/high; CSS config row = 20-80 low/medium). | `1A-substrate-evidence.md:102-111`; `1A-DIV-006` third-cursor 400-900 LOC re-grounds against the three live cursor carriers (retained/runtime-direct/codegen). | None. |
| CH4-V4-006 | ACCEPT | 1D G-13 prune ladder + U-1 carry-cost are fully source-cited, not recalled: P1−4500/P2−700/P3−5460/net−10800 trace to `SYNTHESIS-AUDIT-OVERFIT.md:153-169` verbatim, and U-1 keeps 169956 (generated plane) detached from the 6867-LOC hand-written carry (CH4-V2-010 held). | `1D-skinny-lessons.md:209` (G-13) + `:213-225` (U-1); source figures re-grounded this pass; live `find … = 6867 LOC` matches the carry figure exactly. | None. |
| CH4-V4-007 | REVISE | `COH18-015` (the simd-scan scanner-asymmetry divergence) carries a risk class (`medium-high`) and a wave hint (`SK-V19 scanner-unification decision`) but NO LOC-delta number — the only divergence-table row in the packet whose cost cell omits a band. It is a late-cycle addition NOT in `first_cycle_additions` (COH18-001..010), so it never reached the V2/V3 CH4 cost fold and escaped pricing. The underlying surface is bounded and pricable: `crates/simd-scan/src/{index.rs (103), lib.rs (114)}` = 217 LOC plus the 8/9 generated-grammar `OnceCell<StructuralIndex>` emission sites. | `1F-coherence-scan.md:100` — COH18-015 LOC/risk cell = "renamed/parallel-scanner check; SK-V19 scanner-unification decision; medium-high" (no number); live `wc -l crates/simd-scan/src/index.rs lib.rs = 217`; consumers `crates/core/src/grammar/generated/json.rs:701,:719,:732` + emitter `support.rs:67`. | Add a realistic LOC-delta band to COH18-015's cost cell (the SK-V19 scanner-unify is a ≈217-LOC probe-API reconcile + the 8/9 generated-grammar emission-site re-route; a decision/reconcile band keyed to that surface, e.g. ≈ +20..+217 unify-or-rename), matching the band discipline the other 14 COH18 divergence rows already carry. |
| CH4-V4-008 | REVISE | The simd-scan / `OnceCell` probe-API surface is catalogued in THREE places that all route to the SAME SK-V19 scanner-unification disposition, yet NONE carries a shared priced LOC-delta: `1F-coherence` COH18-015 (no band), the `1F-anti` OnceCell row (priced only as a "reconcile burden", no number), and the `1E` line-158 carry (a classification carry, no loc_delta). The lens requires a divergence-bearing cross-document surface to share one priced disposition (the discipline CH4-V3-007 enforced for the 1A↔1E amendment siblings). | `1F-coherence-scan.md:100` (COH18-015, no LOC); `1F-anti-pattern.md:43` ("the SK-V19-adoption Lock-1 reconcile burden" — no number); `1E-locks-evidence.md:158` ("SK-V19-adoption Lock-1 classification carry … must be classified at SK-V19 adoption" — no loc_delta). | Cross-link the three rows to ONE priced SK-V19 scanner-unification disposition (the 217-LOC probe-API surface), as the 1A↔1E amendment siblings were cross-linked per CH4-V3-007. A named surface routed across three inventories with zero shared priced delta is a class-level envelope, not a bounded receiver. |
| CH4-V4-009 | REVISE | `D-1E-V5-11` (`css_balanced_component_scan` neutrality obligation) prices its build cost as `new-primitive (G2 build; lock-clause +text)` — it names the owning wave (G2) but does NOT carry G2's `≤450` band inline, unlike its sibling D-1E-V5-09 which carries an explicit `+20..+80`. SPEC G2 (`:439`) is the named owner with a `≤450` band; the row leaves the build delta unpriced where the band is available. | `1E-locks-evidence.md:115` D-1E-V5-11 cost cell = "new-primitive (G2 build; lock-clause +text)"; live `SPEC.md:439` G2 = "≤450 hand source/test/gate LOC; new `lower/css_scan.rs` + `css_scan_direct.rs` + primitive shell + arg-derivation". Contrast D-1E-V5-09 `:113` which carries `+20..+80`. | Import the G2 `≤450` band into D-1E-V5-11's cost cell (e.g. "new-primitive ≤450 LOC / G2 build, `SPEC.md:439`; lock-clause +text"), so the row carries a realistic LOC-delta number, not only a wave pointer. |
| CH4-V4-010 | REJECT | The latent CH4 suspicion that any cited LOC is recalled/fabricated as a number is FALSIFIED for every load-bearing figure spot-verified this pass. A blanket "fabricated-LOC" REVISE would be uncited. | The verbatim disk match on x86 28/4401, builder 817, CSS courier 911-span, Pattern-H 71/67, D6 surface 6867, StructLayout 960, strategy idents ×9, css_types 66, SPEC G3 `:440` `≤450`, SPEC PROVE `:443` `≈ +200`, and the prune ladder verbatim at `SYNTHESIS-AUDIT-OVERFIT.md:153-169`. | None — the falsifying evidence is the verbatim disk match on every spot-checked row. |

## Tally Rationale

Ten cost-lens findings: 6 ACCEPT, 3 REVISE, 1 REJECT (30% REVISE — at the
close-reading floor). The ACCEPTs are not paper-close: the four V3 REVISEs are
each verified discharged in place, and twelve independent cost facts plus the
full prune ladder are re-grounded against disk verbatim. The single REJECT
records that the fabrication suspicion is falsified on every spot-verified row.

The three REVISEs are bounded cost-carrier defects, not structural failures:

1. **CH4-V4-007** — COH18-015, a late-cycle divergence, is the lone
   divergence-table row whose cost cell omits a LOC band; it escaped the V2/V3
   CH4 folds because it post-dates them. The surface (217-LOC probe API) is
   pricable.
2. **CH4-V4-008** — the same simd-scan/`OnceCell` surface is routed across
   three inventories (COH18-015, 1F-anti, 1E:158) with zero shared priced
   disposition — a class-level envelope where CH4-V3-007 already established that
   a cross-inventory divergence surface must share ONE priced disposition.
3. **CH4-V4-009** — D-1E-V5-11 names its owning wave (G2) but omits G2's
   available `≤450` band inline, where its sibling D-1E-V5-09 carries `+20..+80`.

None of the three reopens a numeric figure proven correct; all three are
pricing-completeness defects the v+1 fold can discharge with edits under a few
LOC of inventory text each. CH4 cannot move all-ACCEPT until the simd-scan
scanner-unification surface carries a realistic LOC-delta with one shared priced
disposition, and D-1E-V5-11 imports its G2 band.

TALLY accept=6 revise=3 reject=1

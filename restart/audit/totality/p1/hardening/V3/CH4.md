# SK-V18 T-P1 V3 CH4 Cost Hardening

Verdict: REVISE

Scope: CH4 COST lens (cycle V3) over the live SK-V18 T-P1 inventories
(`1A`/`1B`/`1C`/`1D`/`1E`/`1F-coherence` + `1F-anti-pattern`/`1F-past-corpora`).
This file replaces stale prior-cycle (SK-V15) CH4 content in place per the §3Z
cycle protocol. No source files, inventories, staging, or commits were changed by
this lens.

## Lens

Per `restart/prompts/totality/PASS-1-EXCAVATION.md:121-123` + `restart/prompts/ORCHESTRATOR.md:86`:
every divergence carries a realistic LOC-delta + risk class; 1E (and 1A) amendment
candidates carry a wave-alignment hint + path:line; a candidate without supporting
evidence is REVISE; broad implementation buckets must split into bounded receivers;
same-wave consumer present per kernel/primitive. The spot-verify mandate: the most
load-bearing cited path:line cost rows are re-grounded on disk.

## Spot-Verification (load-bearing cost rows re-grounded LIVE this pass)

| cost claim | inventory rows | live verification | result |
|---|---|---|---|
| `CSS_GENERATED_RS` = 911 LOC verbatim courier (701-1611) | 1C D1, 1E D-1E-V5-01, 1F COH18-003 | `rg -n 'const CSS_GENERATED_RS'` → `:701`; closing `"#;` → `:1611`; `wc -l = 1611` | CONFIRMED (1611−701+1 = 911) |
| x86 = 28 files / 4401 LOC / ≈ −4500 prune | 1D D-4, 1E D-1E-V5-04, 1F COH18-009, 1F-anti R8 | `find …/x86_64 …/ext/x86 -type f \| wc -l = 28`; `cat \| wc -l = 4401`; source budget `SYNTHESIS-AUDIT-OVERFIT.md:161 = ≈ −4500` | CONFIRMED |
| builder.rs = 817 LOC (>500 cap) | 1E D-1E-V5-13 | `wc -l crates/core/src/runtime/css_l4/builder.rs = 817` | CONFIRMED ("CH4 EXACT") |
| Pattern H = 71 total / 67 per-grammar / 6867 LOC | 1D U-1, 1E D-1E-V5-06, 1F COH18-007 | `find … -mindepth 2 -name '*.rs' = 71`; `-not -path '*tape*' = 67`; LOC `= 6867` | CONFIRMED |
| 7 css_l4 `generated.rs` md5-identical `b654562c…` | 1C D3, 1D D-2 | `md5 -q` × 7 → all `b654562ccff46ed62dd48e9ace325830` | CONFIRMED |
| G3 un-fork band ≤450 (SPEC) vs intrinsic-blocked 600-1400 (ARCH) | 1B D2 | `SPEC.md:440` = "≤450 hand source/test/gate LOC"; `ARCHITECTURE.md:1280-1282` = "600-1400 LOC joint decision-engine wiring envelope … intrinsic-blocked" | CONFIRMED (dual-source correct) |
| CH4-V2-008 correction: prior `+400..+1200` was uncited | 1B D2 | `rg '\+400\.\.\+1200\|four real\|per-shape bod' SPEC.md` = 0 | CONFIRMED (the prior figure traced to nothing; dual-source replacement is grounded) |
| R16 `+1-line PartialEq` co-gate not yet enforceable | 1B D5, 1D R16, 1E LAC-1E-V5-02 | `skinny/xtask/src/regen.rs:5` = `#[derive(Clone, Copy, Debug)]` (no `PartialEq`) | CONFIRMED |
| metalang leak `parse_w11_1_number` ×7 | 1C D7, 1D D-8, 1E D-1E-V5-08 | `rg -c = 7`; sites `801,841,881,955,1007,1019,1031` | CONFIRMED |
| `ir/registry/strategy.rs` 9 grammar-named idents (relocated-seam analog) | 1F COH18-005, COH18-012 | idents rows verified at `:137,:143,:149,:155,:161,:167,:173,:179,:185` (9) | CONFIRMED |
| prune budget ladder P1−4500 / P3−5460 / P5≈0 / net−10800 | 1D G-13 | `SYNTHESIS-AUDIT-OVERFIT.md:153,161,169` carry `−10800/−4500/−5460` verbatim | CONFIRMED |

The cost spine of this packet is, on its load-bearing rows, materially accurate.
Eleven independent cost facts re-grounded clean. The CH4-V2-008 hygiene fix (the
deletion of the uncited `+400..+1200` G3 band and its replacement with a
dual-sourced `≤450`/`600-1400` carrier) is verified sound — the prior cycle's
overfit cost figure is gone and the replacement is bench-of-source-backed.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH4-V3-001 | ACCEPT | 1A divergence table is cost-complete: every `1A-DIV-001..008` row carries a `loc_delta_estimate` column AND a `risk` column, and the bands are realistic (e.g. BIR 20-vs-13 = 600-1,200 high; checkpoint API = 80-300 medium). | `1A-substrate-evidence.md:102-111` — 8 divergence rows, each with non-empty loc_delta + risk; `1A-DIV-002` 600-1,200 LOC high re-grounds against `ir/src/lib.rs:355` (live BIR = 13 variants, target 20). | None for 1A divergences. |
| CH4-V3-002 | ACCEPT | 1B prices the G3 un-fork honestly with a dual-posture band keyed to the WIRE-vs-real-bodies fork, and the prior uncited `+400..+1200` figure is removed (CH4-V2-008 discharged). | `1B-codegen-evidence.md:81` — D2 cites `≤450` (SPEC.md:440, WIRE posture) OR `600-1400 LOC` (ARCHITECTURE.md:1280-1282, intrinsic-blocked); both verified live; the deleted `+400..+1200` greps to 0 in SPEC. | Preserve the dual-source band in any later fold. |
| CH4-V3-003 | ACCEPT | 1E divergence carrier is keyed: `D-1E-V5-01..13` each carry loc_delta + risk + path:line, and every loc_delta cell that REUSES a sibling estimate names the sibling row explicitly (no silent recall). | `1E-locks-evidence.md:103-117` — e.g. `D-1E-V5-01 ≈ −910 (1C D1 911-LOC span)`, `D-1E-V5-13 ≈ −817 (live wc -l = 817; CH4 EXACT)`; the "REFERENCE the cross-inventory sibling … no new measurement" disclaimer at `:100-101` is honest cost hygiene. | None for the keyed 1E rows. |
| CH4-V3-004 | ACCEPT | 1E LAC candidates each carry a wave hint + supporting path:line, satisfying the lens "1E amendment candidates carry a wave-alignment hint." | `1E-locks-evidence.md:146-152` — LAC-1E-V5-01..07 each populate the `wave hint` column (`G2 ∧ G1`, `G3 ∧ P3`, `P1`, `P4 (MUST land before G2/G3)`, `SK-V19 adoption`, …) and a non-empty supporting-evidence column; 8 wave-hint tokens present across 7 LACs. | None for 1E LAC wave keying. |
| CH4-V3-005 | ACCEPT | 1D G-13 prune-ladder is fully source-cited, not recalled: P1−4500 / P3−5460 / P5≈0 / net−10800 all trace to `SYNTHESIS-AUDIT-OVERFIT.md:153/161/169` verbatim, and U-1 disambiguates the 6867-LOC hand-written carry from the 169956-LOC generated plane (CH4-V2-010). | `1D-skinny-lessons.md:204` (G-13) + `:208-220` (U-1); source figures re-grounded at the cited lines this pass; live `find … = 6867 LOC` matches the carry figure exactly. | None. |
| CH4-V3-006 | REVISE | 1E D-1E-V5-04's loc_delta carrier contains a DANGLING cross-reference: it cites "`1A` 1A-DIV reuse" as a source for the ≈ −4500 x86 figure, but `1A-substrate-evidence.md` has ZERO x86 content — `rg 'x86\|4500\|4401' 1A-substrate-evidence.md` returns nothing; 1A's divergences (`1A-DIV-001..008`) are substrate/IR/cursor only. The −4500 figure itself IS correct (disk 4401; `1F-anti R8`/`SYNTHESIS-AUDIT-OVERFIT.md:161` both carry it), so this is a citation defect in a cost carrier, not a wrong number. | `1E-locks-evidence.md:108` cell reads `≈ −4500 (` `1A` `1A-DIV reuse / ` `1F-anti` `R8; disk 28 files / 4401 LOC)`; live: `1A-substrate-evidence.md` has no x86 row; `1F-anti-pattern.md:72` DOES carry `x86 surface … ≈ −4500`. | Strike the `1A 1A-DIV reuse` half of the D-1E-V5-04 loc_delta cell. The valid sources are `1F-anti-pattern.md:72` (R8 ≈ −4500) and the in-cell disk figure (28 files / 4401 LOC); re-key the cross-reference to those two only. |
| CH4-V3-007 | REVISE | The 1A LOCKS-amendment candidate (`1A-LOCK1-AMEND-001`) carries grounding path:line + a disposition pointer (T-P3/Omega) but NO LOC-delta band and only a coarse wave hint ("disposition T-P3"). The companion 1E LAC candidates all carry an explicit cost cell (e.g. LAC-1E-V5-01 `+20..+80`); the 1A candidate — a Lock-14-clause strike + re-anchor touching `LOCKS.md:620` and `ARCHITECTURE.md:1990,1997` — leaves its edit surface unpriced. Under the lens a divergence-bearing amendment candidate must state a realistic LOC-delta and a wave-alignment hint, not only a gate-disposition pointer. | `1A-substrate-evidence.md:178-180` — the amendment row has `clause path:line`, `candidate amendment`, `grounding` columns but no LOC band; contrast `1A-DIV-005` (the companion CSS-config divergence) which carries `20-80 LOC / low-medium`, and `1E-locks-evidence.md:146` LAC-1E-V5-01 which carries `+20..+80`. The `1A-SUB-025` DIVERGES row (`:97`) that feeds it also has no loc_delta. | Add a LOC-delta band (the edit is a ≈ −1..+5 LOC clause strike + re-anchor at `LOCKS.md:620` plus the companion `ARCHITECTURE.md:1990,1997` prose carrier) and a wave-alignment hint (SK-V19 LOCKS reconcile / Pass Omega — matching the 1F COH18-008 "1 line LOCKS reconcile (SK-V19)" sibling) to `1A-LOCK1-AMEND-001`. Cross-link to the 1E sibling so the two amendment surfaces share one priced disposition. |
| CH4-V3-008 | REVISE | 1C D8 (Sheets generator path) prices `loc_delta +200..+600` by REFERENCING "the SK-V18 G3 budget," but the SK-V18 SPEC PROVE-wave row prices Sheets adoption at `≈ +200` (`+30 grammar-root + xtask; the rest generated`), and G3 is priced separately at `≤450` — so D8's cited basis (the G3 band) is the WRONG sibling row and its upper bound (+600) exceeds the named PROVE budget. The cost is in the right order of magnitude but mis-attributed to G3 rather than the PROVE wave that actually owns Sheets emission. | `1C-runtime-evidence.md:66` — D8 reads "loc_delta +200..+600 … the generator-path build cost references the SK-V18 G3 budget"; live `SPEC.md` PROVE row (verified this pass) prices Sheets at `≈ +200 Sheets adoption (≈+30 grammar-root + xtask; the rest generated …)` with hard cap `≤90 min wave wall`; G3 is the separate `≤450` un-fork row. | Re-key D8's loc_delta basis from "the SK-V18 G3 budget" to the SK-V18 PROVE-wave row (`SPEC.md` PROVE = `≈ +200` Sheets adoption); tighten the band to ≈ +200 (or justify the +600 ceiling against the PROVE row's own caveat), and name the owning wave as PROVE, not G3. |
| CH4-V3-009 | ACCEPT | 1F-coherence + 1F-anti-pattern are LIVE cost surfaces this cycle (NOT the superseded SK-V15 stubs the V3 challenge-context header presumes) and carry per-row LOC/risk: every `COH18-001..015` divergence row in the divergence table states a LOC band + a risk word, and `1F-anti-pattern.md:72` is the canonical ≈ −4500 x86 prune source 1E reuses. | `1F-coherence-scan.md:88-100` — 11 divergence rows, each with a `LOC / risk` cell (e.g. COH18-003 "≈910 LOC CSS courier + fork arms; CRITICAL"; COH18-005 "60-200 LOC; high"); `1F-coherence-scan.md:24` records the V5-SKV18 rewrite of `1F-anti-pattern.md`/`1F-past-corpora.md`; `1F-anti-pattern.md:3` cycle = `V5-SKV18-totality`. | None — but the V3/CHALLENGE-CONTEXT.md:19-23 "1F auxiliaries historical/superseded" line is itself stale against the SK-V18 rewrite; out of CH4 write-scope (flag to aggregator). |
| CH4-V3-010 | REVISE | 1C D6 (crates/core runtime crate-layout divergence) carries the most under-determined cost band in the runtime packet: it bounds the collapse against the ≈6867-LOC Pattern-H surface but states no concrete loc_delta NUMBER for the de-dup itself, instead disclaiming the only available figure (P3 −5460) as "an ANALOGY … NOT this row's budget" (CH4-V2-009). The disclaimer is correct hygiene, but it leaves the row with a named surface and NO priced delta — a class-level envelope, not a bounded receiver. | `1C-runtime-evidence.md:60-61` — D6 reads "loc_delta bounded against the TOTALITY-tree Pattern-H surface ≈6867 LOC … the P3 figure is an ANALOGY for the de-dup pattern, NOT this row's budget"; no de-dup delta is stated; live carry surface = 6867 LOC (verified), but the SHARED-module collapse target (the 9× `document.rs`/`builder.rs`/`view.rs` fold) is unpriced. | Add a bounded de-dup estimate for D6 (the 9×→1× shared-`runtime/src/{document,builder,visitor}/` fold over the 67 hand-written files), or explicitly mark it `SK-V19-adoption / not-priced-this-cycle` with a same-wave receiver (SK-V19 totality fold, matching U-1's verify_action). A named surface with no delta and a disclaimed analogy is not yet a cap-valid receiver. |

## Tally Rationale

Ten cost-lens findings: 6 ACCEPT, 4 REVISE (40% REVISE — above the ≥30% close-reading
floor). The ACCEPTs are not paper-close: each was spot-verified against disk (the
911-LOC courier, 28-file/4401-LOC x86, 817-LOC builder, 71/67/6867 Pattern-H census,
md5 identity, the regen.rs:5 missing-PartialEq, the deleted `+400..+1200` G3 band, the
−10800 prune ladder). The four REVISEs are bounded cost-carrier defects, not structural
failures:

1. **CH4-V3-006** — a dangling `1A 1A-DIV` cross-reference in the D-1E-V5-04 x86 cost cell
   (the figure is right; one of its two cited sources does not exist).
2. **CH4-V3-007** — the 1A amendment candidate is unpriced (no LOC band, only a
   T-P3 disposition pointer) where its 1E and 1F siblings are priced.
3. **CH4-V3-008** — 1C D8 mis-attributes its Sheets cost basis to G3's `≤450` band when
   the SK-V18 PROVE wave (`≈ +200`) is the owning receiver; the +600 ceiling overshoots it.
4. **CH4-V3-010** — 1C D6 names a 6867-LOC surface but states no priced de-dup delta and
   disclaims the only analogy, leaving a class-level envelope rather than a bounded receiver.

None of the four reopens a numeric figure proven correct; all four are keying/attribution/
pricing-completeness defects the v+1 fold can discharge with edits under ±5 LOC of inventory
text each. CH4 cannot move all-ACCEPT until D-1E-V5-04's cross-ref is corrected, the 1A
amendment candidate is priced, and the two 1C rows (D6/D8) are re-keyed to their owning waves.

TALLY accept=6 revise=4 reject=0

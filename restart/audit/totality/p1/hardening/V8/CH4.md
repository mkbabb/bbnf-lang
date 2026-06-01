# SK-V18 T-P1 V8 CH4 Cost Hardening

Verdict: ACCEPT (clean) — 2nd consecutive clean cost cycle (V7 clean → V8 clean = fixed point)

Scope: CH4 COST lens (cycle V8) over the live SK-V18 T-P1 inventories
(`1A`/`1B`/`1C`/`1D`/`1E`/`1F-coherence` + `1F-anti-pattern`/`1F-past-corpora`).
This file replaces stale prior-campaign content in place per the §3Z cycle
protocol. No source files, inventories, staging, or commits were changed by this
lens.

## Lens

Per the CH4 overlay + `restart/prompts/totality/PASS-1-EXCAVATION.md`: every
divergence carries a realistic LOC-delta + risk class; 1E (and 1A) amendment
candidates carry a wave-alignment hint + path:line; a candidate with no
supporting path:line is REVISE. CRITICAL convention: a cost cell whose figure
matches disk is an ACCEPT — finding that a figure is NOT fabricated is an ACCEPT,
never a reject. A REJECT is admissible ONLY when an inventory STATES SOMETHING
FALSE ON DISK and the live falsifying path:line is cited; a self-falsified
fabrication suspicion is an ACCEPT (the inventory is correct), never a reject=1.

## Prior-Cycle Fold Discharge (V7 CH4 was the first clean cost cycle)

V7 CH4 returned 9 ACCEPT / 0 REVISE / 0 REJECT — the first all-ACCEPT cost cycle.
There was no open REVISE to discharge into V8. The V7 falsified-suspicion line
(CH4-V7-FAB) is re-confirmed this pass and re-issued below as CH4-V8-FAB. This
V8 pass re-grounds the load-bearing cost spine INDEPENDENTLY (not on recall) to
establish the 2-consecutive-clean fixed point.

## Spot-Verification (load-bearing cost rows re-grounded LIVE this pass)

Every figure below was re-grounded on disk at the dirty tree this pass via
`wc -l` / `rg -c` / `find` / `md5` / `sed`; none was taken on recall.

| cost claim | inventory rows | live verification | result |
|---|---|---|---|
| builder.rs = 817 LOC (>500 cap) | D-1E-V5-13, 1C D9-adj, L13 | `wc -l crates/core/src/runtime/css_l4/builder.rs = 817` | CONFIRMED (EXACT) |
| `css_types.rs` = 66 LOC, lock-NAMED at LOCKS:349 | D-1E-V5-14, 1C D9, COH18-006, 1F-anti | `wc -l crates/core/src/css_types.rs = 66` | CONFIRMED (EXACT) |
| `StructLayout` rename surface = 960 | D-1E-V5-12 | `rg -c StructLayout crates/` sum = 960 | CONFIRMED (EXACT) |
| Pattern H = 71 total / 67 per-grammar | D-1E-V5-06, COH18-007, LAC-1E-V5-07 | `find … = 71`; `… -not -path '*tape*' = 67` | CONFIRMED (EXACT) |
| x86 = 28 files / 4401 LOC (≈ −4500) | D-1E-V5-04, 1D G-13/D-4, 1F-anti R8 | `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28`; `cat \| wc -l = 4401` | CONFIRMED (EXACT) |
| simd-scan probe-API = 217 LOC | COH18-015, 1F-anti OnceCell, 1E:159 | `wc -l crates/simd-scan/src/{index,lib}.rs = 103+114 = 217` | CONFIRMED (EXACT) |
| OnceCell emitted 8 of 9 grammars (math = 0) | COH18-015, 1F-anti, 1E:159 | per-grammar `rg -c`: 7×2 + google_sheets 3 + **math 0** = 8/9 | CONFIRMED (8/9 EXACT) |
| math = inert ScanState shell, 0 ensure_structural_index | 1E:159, 1F-anti, COH18-015 | `rg -c ScanState math.rs = 10`; `rg -c ensure_structural_index math.rs = 0` | CONFIRMED |
| Lock-14 self-gate FALSIFIED: 13 live (11 ir + 2 analysis) vs asserted ZERO | D-1E-V5-14, COH18-012 | `rg -c crates/ir/src = 11`, `crates/analysis/src = 2`, sum 13 | CONFIRMED (gate RED) |
| CSS courier `CSS_GENERATED_RS` span 701-1611 = 911 LOC | D-1E-V5-01, 1C D1, COH18-003, 1F-anti | `:701` `const … = r#"`; first closing `"#;` after 701 is `:1611` (911-LOC span) | CONFIRMED (EXACT) |
| `RuntimeTarget` derives Clone/Copy/Debug, NO PartialEq (+1) | 1B D5, 1D, 1F-past R16 | `skinny/xtask/src/regen.rs:5 #[derive(Clone, Copy, Debug)]` over `pub(crate) struct RuntimeTarget` `:6` | CONFIRMED (EXACT) |
| `runtime_target_rows_collapsed` spec-planned `SPEC.md:247`, NO live def | 1B D5, LAC-1E-V5-02 | `SPEC.md:247` = planned bool co-gate; `rg … skinny/crates skinny/xtask = 0` | CONFIRMED |
| D6 `REWRITE_SET` single-pool (+60..+200) | 1B D6 | `backend_egraph.rs:9 const REWRITE_SET = "sk-v15-w7-direct-sink-normalization-v1"` (single) | CONFIRMED |
| D2 dual-band: G3 ≤450 `SPEC.md:440`; 600-1400 envelope `ARCH:1280-1282` | 1B D2 | `:440` = G3 un-fork ≤450; `ARCH:1280-1282` = intrinsic-blocked 17-LOC-scaffold envelope | CONFIRMED |
| D2: NO uncited `+400..+1200`/`four real`/`per-shape bod` in SPEC (CH4-V2-008) | 1B D2 | `rg -c` of all three in SPEC = 0 | CONFIRMED (correction holds) |
| SPEC bands: G2 ≤450 `:439`, G3 ≤450 `:440`, PROVE ≈ +200 Sheets `:443` | D-1E-V5-11, 1C D8, 1B D2 | `:439` G2 `css_balanced_component_scan` ≤450; `:440` G3 DELETE ≤450; `:443` PROVE `≈ +200 Sheets` | CONFIRMED (all 3 EXACT) |
| 1C D8 G3-band line = `:440` (NOT `:442`; CH4-V5-007) | 1C D8 | `:440` = G3 row; `:442` = G5/G6 row | CONFIRMED |
| 1C D6 / 1D U-1 fold surface ≈6867 LOC across 67 hand-written files | 1C D6, 1D U-1 | `find … -not -path '*tape*' = 67`; `cat … = 6867` | CONFIRMED (EXACT) |
| 1D U-1 generated recognizer plane = 169956 LOC | 1D U-1 | `wc -l crates/core/src/grammar/generated/*.rs` total = 169956 | CONFIRMED (EXACT) |
| 1F-anti god-file census (11 files) | 1F-anti god-modules | live `wc -l` on all 11 = EXACT (report 11863 / gate 6175 / lock14_baseline 5095 / generated_real_typed 4941 / nonjson_css_l4 3737 / real_typed_struct 2827 / grammar lib 2052 / passes lib 2025 / runtime_generator 1611 / codegen lib 1473 / json generated 1235) | CONFIRMED (11/11 EXACT) |
| campaign net ≈ −10800; P3 ≈ −5460 | 1F-anti, 1D G-13/D-2 | `SYNTHESIS-AUDIT-OVERFIT.md:153` "≈ −10800"; `:165-169` PRUNE-3 "≈ −5460 LOC" | CONFIRMED |
| metalang leak `parse_w11_1_number ×7` (rename-only ≈0) | 1C D7, 1D D-8, D-1E-V5-08 | `rg -c parse_w11_1_number json/generated.rs = 7` | CONFIRMED (EXACT) |
| 7 byte-identical css_l4 replicas md5 `b654562c…` | 1C D3, 1D D-2, 1F-anti, 1F-past R4 | 7× `b654562ccff46ed62dd48e9ace325830` (live `md5`) | CONFIRMED (EXACT) |

The cost spine is, on its load-bearing rows, materially accurate. Twenty-plus
independent cost facts re-grounded clean this pass against disk verbatim.

## Cost-Carrier path:line anchors (1E LAC + 1A amendment — all carry wave hint + path:line)

Re-confirmed live: each `LAC-1E-V5-01..07` carries a wave-alignment hint AND a
concrete supporting path:line; the 1A sibling `1A-LOCK1-AMEND-001` is priced
`≈ −1..+5 LOC` with wave hint + cross-link. Spot-checks this pass:

| anchor | live verification | result |
|---|---|---|
| LAC-1E-V5-01 → `SPEC.md:358` | `:358` = "The §6 named-primitive escape — the (a)-(d) gate …" | CONFIRMED |
| LAC-1E-V5-04 → `SPEC.md:130` | `:130` = "7. **x86 is gone (aarch64-only).** BOTH x86 surfaces …" | CONFIRMED |
| LAC-1E-V5-06 → `SPEC.md:690` | `:690` = "§3.4 — P4: FIX the Lock-14 green-by-exclusion gate …" | CONFIRMED |
| LAC-1E-V5-11 → `SPEC.md:439` (G2 ≤450 band) | `:439` = G2 row `css_balanced_component_scan` ≤450 | CONFIRMED |
| 1A-LOCK1-AMEND-001 → `LOCKS.md:620` | `:620` = "The `G:EventGrammar` type parameter is the generality vehicle." (verbatim — the DIVERGES claim is TRUE on disk) | CONFIRMED |
| 1A-LOCK1-AMEND-001 companion → `ARCH:1990`,`:1997` | `:1990` = "Lazy `ValueRef<G>` value-plane …"; `:1997` = "The `G:EventGrammar`" | CONFIRMED |

## Cost-Completeness Census (every divergence-class row carries a LOC band)

| inventory | divergence rows | banded? |
|---|---|---|
| 1A | `1A-DIV-001..008` | YES — `400-900 / 600-1200 / 300-700 / 250-600 / 20-80 / 400-900 / 200-500 / 80-300` + risk |
| 1B | D1..D6 | YES — `≈ −910 / dual-band ≤450-or-600-1400 / ≈0..+150 / ≈0 / +1 / +60..+200` |
| 1C | D1..D9 | YES — D4 ≈0, D5 −10..−40, D6 −4000..−5000, D7 ≈0, D8 ≈+200, D9 ≈−66 (D1-D3 inline) |
| 1D | D-1..D-13 + G-13 prune ledger | YES — P1 −4500 / P2 −700 / P3 −5460 / P4 gate-only / P5 rename / net −10800 |
| 1E | D-1E-V5-01..14 | YES — every row carries a `loc_delta` cell + lock-pressure risk |
| 1F-coherence | COH18-001..015 | YES — every row LOC/risk-celled; COH18-015 banded `≈ +20..+217` |
| 1F-anti | PRUNE-receiver + god-modules tables | YES — each receiver a Net-LOC column; each god file a live `wc -l` |

## Findings

| id | disposition | finding | evidence | required correction |
|---|---|---|---|---|
| CH4-V8-001 | ACCEPT | The 1E divergence carrier (`D-1E-V5-01..14`) is cost-complete; every load-bearing EXACT figure verifies on disk (builder 817, StructLayout 960, css_types 66, x86 28/4401, Pattern-H 71/67, Lock-14 falsifier 13=11+2, courier 911, RuntimeTarget no-PartialEq). | `1E-locks-evidence.md:105-118`; live `wc -l`/`rg -c`/`find`/`md5`. | None. |
| CH4-V8-002 | ACCEPT | All seven 1E LAC candidates carry a wave-alignment hint AND a supporting path:line, each tied to a priced D-1E row; the SPEC citations (`:358`/`:130`/`:690`/`:439`) resolve. The 1A sibling is priced `≈ −1..+5 LOC` with wave hint + `LOCKS.md:620`+`ARCH:1990/:1997`; the `:620` "generality vehicle" text is verbatim on disk, so the DIVERGES claim it feeds is TRUE. | `1E-locks-evidence.md:147-153`; `1A-substrate-evidence.md:180`; `SPEC.md:358/130/690/439`; `LOCKS.md:620`; `ARCH:1990/:1997`. | None. |
| CH4-V8-003 | ACCEPT | The 1A divergence table (`1A-DIV-001..008`) is cost-complete; every row carries `loc_delta_estimate` + `risk`. The IR `ExprKind` is 8 variants (Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation) at `:211-237`, grounding the 1A-DIV-001 LOC band; `:355` `BackendExpr` / `:393` `Recognizer` enum heads verify. The D-1E-V5-04 cross-ref correction (CH4-V3-006 struck "1A 1A-DIV reuse") holds — `rg -c x86 1A-substrate-evidence.md = 0`. | `1A-substrate-evidence.md:104-111`; `skinny/crates/ir/src/lib.rs:211-237,:355,:393`. | None. |
| CH4-V8-004 | ACCEPT | 1B D1..D6 are cost-complete; D2's dual-band (G3 ≤450 `SPEC.md:440` vs the 600-1400 envelope `ARCH:1280-1282`), the CH4-V2-008 correction (no uncited `+400..+1200`/`four real`/`per-shape bod` in SPEC — `rg`=0), D5 (+1 PartialEq, no live `runtime_target_rows_collapsed`), and D6 (single `REWRITE_SET`) all verify. | `SPEC.md:440`; `ARCH:1280-1282`; `regen.rs:5`; `backend_egraph.rs:9`; `rg … SPEC = 0`. | None. |
| CH4-V8-005 | ACCEPT | 1C D1-D9 are cost-complete; the ≈6867-LOC fold surface (67 hand-written files), the 169956 recognizer plane (the DISTINCT generated tree), the courier span 701-1611, the −66 css_types relocate, and the PROVE `≈ +200` re-key with the corrected G3-band line `:440` (NOT `:442`) all verify. | `find … = 67`/`cat = 6867`; `wc -l grammar/generated/*.rs = 169956`; `SPEC.md:440/:443`. | None. |
| CH4-V8-006 | ACCEPT | The 1F-anti god-file census is cost-complete and EXACT on all eleven cited files; the campaign net (−10800) and P3 (−5460) source at `SYNTHESIS-AUDIT-OVERFIT.md:153/165-169`; `parse_w11_1_number ×7` and the 7× `b654562c` replica md5 verify. | live `wc -l` 11/11 EXACT; `SYNTHESIS-AUDIT-OVERFIT.md:153,165-169`; `rg -c`/`md5`. | None. |
| CH4-V8-007 | ACCEPT | The simd-scan / OnceCell surface shares ONE priced SK-V19 scanner-unification disposition (≈ +20..+217) across COH18-015, 1F-anti, and 1E:159; the 217-LOC probe-API + 8/9 emission basis (math = 0, inert ScanState shell) verify. The `ctns_probe_admits` 12–24-byte window the cells reference is on disk (min at `:82`, max at `:84`); the cited `support.rs:74-95` span loosely envelops the gate (def head at `:70`, body to `:86`) but still encloses the load-bearing window values, so a T-P2 reader following the cite lands on the right logic — not a misleading cost defect (PROPORTIONATE). The "probe substrate (OnceCell + helper)" diction is verbatim at `:67`. | `wc -l simd-scan = 217`; OnceCell 8/9; `support.rs:67,70,82-84`. | None. |
| CH4-V8-FAB | ACCEPT (falsified suspicion) | The latent CH4 suspicion that any cited LOC/figure is recalled or fabricated is FALSIFIED for every load-bearing figure spot-verified this pass. Per the corrected convention this is an inventory-is-CORRECT finding, NOT a reject — no inventory STATES anything FALSE on disk that a live citation falsifies (a figure matching disk is an ACCEPT). | The verbatim disk match on builder 817, css_types 66, StructLayout 960, Pattern-H 71/67, x86 28/4401, simd-scan 217, OnceCell 8/9 (math 0), Lock-14 falsifier 13 (11+2), courier 911, RuntimeTarget no-PartialEq, `runtime_target_rows_collapsed`=0, single REWRITE_SET, SPEC bands `:439/:440/:443/:247`, fold surface 6867, recognizer plane 169956, all 11 god-file LOC, `parse_w11_1_number ×7`, 7× `b654562c`, campaign net −10800. | None — the falsifying evidence is the verbatim disk match on every spot-checked row. |

## Tally Rationale

Eight cost-lens findings, all ACCEPT (one the falsified-fabrication line recorded
per the corrected convention as inventory-is-correct, never a reject). The ACCEPTs
are not paper-close: twenty-plus independent cost facts are re-grounded against
disk verbatim this pass — every SPEC band line (`:439/:440/:443/:247`), every
god-file LOC (11/11), the 169956 recognizer plane, the 6867 fold surface, the 911
courier span, the 217 probe API, the 8/9 OnceCell breadth (math 0, inert shell),
the 13-site (11 ir + 2 analysis) Lock-14 falsifier, the 960 StructLayout surface,
the 28/4401 x86 surface, the 7× `b654562c` replicas, and the `parse_w11_1_number
×7` leak. Every 1E/1A amendment candidate carries a wave-alignment hint + a
path:line that resolves; the 1A `LOCKS.md:620` "generality vehicle" text is
verbatim, confirming its DIVERGES claim is TRUE on disk. The one looseness found
(the `support.rs:74-95` span vs the `ctns_probe_admits` head at `:70`) still
envelops the load-bearing 12–24-byte window (`:82-84`), so it does not mislead a
T-P2 reader on cost — an ACCEPT under the proportionate standard, not a REVISE.

No cost cell is fabricated; no divergence-class row lacks a LOC band; no 1E/1A
amendment candidate lacks a wave-alignment hint + path:line; no wave hint points
to a stale or wrong SPEC line. There is no admissible REJECT — no inventory states
anything FALSE on disk that a live citation falsifies (a figure matching disk is
an ACCEPT, not a reject). This cost lens finds the inventories sound; the honest
tally is reject=0. CH4 V8 is the SECOND consecutive all-ACCEPT cost cycle — with
V7 clean, the V7→V8 run establishes the 2-consecutive-clean fixed point for the
COST lens.

TALLY accept=8 revise=0 reject=0

# SK-V18 T-P1 V7 CH4 Cost Hardening

Verdict: ACCEPT (clean)

Scope: CH4 COST lens (cycle V7) over the live SK-V18 T-P1 inventories
(`1A`/`1B`/`1C`/`1D`/`1E`/`1F-coherence` + `1F-anti-pattern`/`1F-past-corpora`).
This file replaces stale prior-campaign content in place per the §3Z cycle
protocol. No source files, inventories, staging, or commits were changed by this
lens.

## Lens

Per the workflow CH4 overlay + `restart/prompts/totality/PASS-1-EXCAVATION.md`:
every divergence carries a realistic LOC-delta + risk class; 1E (and 1A)
amendment candidates carry a wave-alignment hint + path:line; a candidate with no
supporting path:line is REVISE. The most load-bearing cited cost rows are
re-grounded on disk this pass. CRITICAL convention: a cost cell whose figure
matches disk is an ACCEPT — finding that a figure is NOT fabricated is an ACCEPT,
never a reject. A REJECT is admissible ONLY when an inventory STATES SOMETHING
FALSE ON DISK and the live falsifying path:line is cited; a self-falsified
fabrication suspicion is an ACCEPT (the inventory is correct), never a reject=1.

## Prior-Cycle Fold Discharge (V6 CH4 REVISE verified CLOSED this pass)

V6 CH4 returned 1 REVISE + 1 falsified-suspicion line. The single REVISE is
DISCHARGED live in the folded inventory:

| V6 REVISE | target | discharge evidence (live this pass) |
|---|---|---|
| CH4-V6-001 | `1E-locks-evidence.md:108` (`D-1E-V5-04`) carried a bare-from-root `find crates/bbnf-simd/src/x86_64 crates/bbnf-simd/ext/x86 -type f = 28` that errored to 0 from the repo root because the x86 surface lives under `skinny/crates/bbnf-simd/` | `1E-locks-evidence.md:108` now reads `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28` (both operands `skinny/`-prefixed), matching the cell's own `skinny/crates/bbnf-simd/src/lib.rs:5` sibling cite and the 1D sibling at `:68`. Live: `rg 'find crates/bbnf-simd' restart/audit/totality/p1/*.md` returns ZERO; `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f \| wc -l = 28`; `cat … \| wc -l = 4401`. The `28`/`4401`/`≈ −4500` figures are correct and intact. |

The V6 CH4 falsified-suspicion line (CH4-V6-FAB) is re-confirmed this pass and
re-issued below as CH4-V7-FAB.

## Spot-Verification (load-bearing cost rows re-grounded LIVE this pass)

Every figure below was re-grounded on disk at the dirty tree this pass; none was
taken on recall.

| cost claim | inventory rows | live verification | result |
|---|---|---|---|
| builder.rs = 817 LOC (>500 cap) | D-1E-V5-13, 1C D9-adjacent, L13 | `wc -l crates/core/src/runtime/css_l4/builder.rs = 817` | CONFIRMED (EXACT) |
| `css_types.rs` = 66 LOC, lock-NAMED at LOCKS:349 | D-1E-V5-14, 1C D9, COH18-006, 1F-anti css_types row | `wc -l crates/core/src/css_types.rs = 66`; `LOCKS.md:349` names `crates/core/src/css_types.rs` VERBATIM in the overfit-mess list | CONFIRMED |
| `StructLayout` rename surface = 960 | D-1E-V5-12 | `rg -c StructLayout crates/` sum = 960 | CONFIRMED (EXACT) |
| Pattern H = 71 total / 67 per-grammar | D-1E-V5-06, COH18-007, LAC-1E-V5-07 | `find crates/core/src/runtime -mindepth 2 -name '*.rs' = 71`; `… -not -path '*tape*' = 67` | CONFIRMED (EXACT) |
| x86 = 28 files / 4401 LOC (≈ −4500) | D-1E-V5-04, 1D G-13/D-4, 1F-anti R8 | `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28`; `cat \| wc -l = 4401` | CONFIRMED (skinny path now correct in the cell post-CH4-V6-001) |
| simd-scan probe-API = 217 LOC | COH18-015, 1F-anti OnceCell, 1E:159 | `wc -l crates/simd-scan/src/{index,lib}.rs = 103+114 = 217` | CONFIRMED (EXACT) |
| OnceCell emitted 8 of 9 grammars (math = 0) | COH18-015, 1F-anti, 1E:159 | per-grammar `rg -c 'OnceCell<::simd_scan::StructuralIndex>\|ensure_structural_index'`: bbnf/bnf/css_l4/css_pretty/csv/ebnf/json = 2, google_sheets = 3, **math = 0** | CONFIRMED (8/9 EXACT) |
| Lock-14 self-gate FALSIFIED: 13 live sites (11 ir + 2 analysis) | D-1E-V5-14 (HIGH), COH18-012, 1A | `rg -c '…' crates/ir/src = 11`, `crates/analysis/src = 2`, sum 13, vs `LOCKS.md:349` "returns ZERO" | CONFIRMED (gate RED) |
| CSS courier `CSS_GENERATED_RS` span 701-1611 = 911 LOC | D-1E-V5-01, 1C D1, COH18-003, 1F-anti | `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"`; closing `"#;` at `:1611` (911-LOC verbatim span) | CONFIRMED |
| D5 `RuntimeTarget` derives Clone/Copy/Debug, NO PartialEq (+1) | 1B D5, 1D, 1F-past R16 | `regen.rs:5 #[derive(Clone, Copy, Debug)]` over `pub(crate) struct RuntimeTarget` `:6` | CONFIRMED (EXACT) |
| `runtime_target_rows_collapsed` spec-planned at `SPEC.md:247`, NO live def | 1B D5, LAC-1E-V5-02 | `SPEC.md:247` = the planned bool co-gate ("MUST be true at G3/P3 … PartialEq full-row over BOTH nested structs"); `rg runtime_target_rows_collapsed skinny/crates skinny/xtask = 0` | CONFIRMED |
| D6 `REWRITE_SET` single-pool (+60..+200) | 1B D6 | `backend_egraph.rs:9 const REWRITE_SET = "sk-v15-w7-direct-sink-normalization-v1"` | CONFIRMED |
| D2 dual-band: G3 ≤450 at `SPEC.md:440`; 600-1400 envelope at `ARCH:1280-1282` | 1B D2 | `SPEC.md:440` = G3 un-fork ≤450 hand source/test/gate LOC; `ARCHITECTURE.md:1280-1282` = the intrinsic-blocked 17-LOC-scaffold envelope | CONFIRMED |
| SPEC bands: G2 ≤450 `:439`, G3 ≤450 `:440`, PROVE ≈ +200 Sheets `:443` | D-1E-V5-11, 1C D8, 1B D2 | `:439` G2 `css_balanced_component_scan` ≤450; `:440` G3 DELETE RuntimeEmitterKind ≤450; `:443` PROVE `≈ +200 Sheets adoption` | CONFIRMED (all three EXACT) |
| 1C D6 / 1D U-1 fold surface ≈6867 LOC across 67 hand-written files | 1C D6, 1D U-1 | `find crates/core/src/runtime -mindepth 2 -name '*.rs' -not -path '*tape*' \| wc -l = 67`; `cat … \| wc -l = 6867` | CONFIRMED (EXACT) |
| 1D U-1 generated recognizer plane = 169956 LOC | 1D U-1 | `wc -l crates/core/src/grammar/generated/*.rs` total = 169956 | CONFIRMED (EXACT) |
| 1F-anti god-file census (report 11863 / gate 6175 / lock14_baseline 5095 / generated_real_typed 4941 / real_typed_struct 2827 / grammar lib 2052 / passes lib 2025 / runtime_generator 1611 / codegen lib 1473 / json generated 1235) | 1F-anti god-modules table | live `wc -l` on all ten = EXACT match per file | CONFIRMED (10/10 EXACT) |
| P2 warm-bench `measure_mbps` (≈ −700) at `nonjson_css_l4.rs:3091`, src file 3737 LOC | 1F-anti R13, 1F-past R13 | `rg -n 'fn measure_mbps' src/nonjson_css_l4.rs = :3091`; `wc -l = 3737` (`:3091` in-range → resolves against `src/`, not the 318-LOC `benches/` sibling) | CONFIRMED (disambiguation holds) |
| campaign net ≈ −10800 LOC; P3 ≈ −5460 | 1F-anti campaign-net, 1D G-13/D-2 | `SYNTHESIS-AUDIT-OVERFIT.md:153` "Net LOC ≈ −10800"; `:165-169` PRUNE-3 "≈ −5460 LOC" | CONFIRMED |

The cost spine of this packet is, on its load-bearing rows, materially accurate.
Twenty-plus independent cost facts re-grounded clean this pass, the V6 REVISE
discharged in place with its `skinny/`-prefix fix live, and every SPEC band line
(`:439`/`:440`/`:443`/`:247`), every god-file LOC, the recognizer plane (169956),
the fold surface (6867), and the courier span (911) verify verbatim. The
fabrication suspicion is falsified on every spot-checked row (CH4-V7-FAB).

## Cost-Completeness Census (every divergence-class row carries a LOC band)

A targeted re-scan confirms every divergence-class row carries a realistic
LOC-delta + risk class.

| inventory | divergence rows | banded? |
|---|---|---|
| 1A | `1A-DIV-001..008` | YES — `400-900 / 600-1200 / 300-700 / 250-600 / 20-80 / 400-900 / 200-500 / 80-300` + risk |
| 1B | D1..D6 | YES — `≈ −910 / dual-band ≤450-or-600-1400 / net ≈0..+150 / ≈0 / +1 / +60..+200` |
| 1C | D1..D9 | YES — D4 ≈0, D5 −10..−40, D6 −4000..−5000, D7 ≈0, D8 ≈+200, D9 ≈−66 (D1-D3 inline LOC) |
| 1D | D-1..D-13 + G-13 prune ledger | YES — P1 −4500 / P2 −700 / P3 −5460 / P4 gate-only / P5 rename / net −10800 |
| 1E | D-1E-V5-01..14 | YES — every row carries a `loc_delta` cell + lock-pressure risk |
| 1F-coherence | COH18-001..015 | YES — every row LOC/risk-celled; COH18-015 banded `≈ +20..+217` |
| 1F-anti | PRUNE-receiver table + god-modules table | YES — each receiver carries a Net-LOC column; each god file a live `wc -l` |

## 1E + 1A Amendment-Candidate cost check (all carry wave hint + path:line)

Re-confirmed live: each of `LAC-1E-V5-01..07` carries a wave-alignment hint AND a
concrete supporting path:line, each tied to a priced `D-1E-V5-NN` row. Spot-checks
this pass: LAC-1E-V5-01 → `SPEC.md:358` (named-primitive (a)-(d) gate section);
LAC-1E-V5-04 → `SPEC.md:130` (close condition 7, "x86 is gone (aarch64-only)");
LAC-1E-V5-06 → `SPEC.md:690` (§3.4 P4 green-by-exclusion fix); LAC-1E-V5-07 →
`find crates/core/src/runtime … = 71` + `LOCKS.md:408` baseline. The 1A amendment
sibling (`1A-LOCK1-AMEND-001`) carries `≈ −1..+5 LOC` + wave hint ("SK-V19 LOCKS
reconcile / Pass Omega") + path:line (`LOCKS.md:620` clause + `ARCHITECTURE.md:1990,:1997`
prose) + cross-link to the 1E sibling. No candidate lacks a supporting path:line.

## Findings

| id | disposition | finding | evidence | required correction |
|---|---|---|---|---|
| CH4-V7-DISCH | ACCEPT | The V6 CH4 REVISE (CH4-V6-001) is discharged in place: `D-1E-V5-04`'s x86 `find` command is now `skinny/`-prefixed on both operands, runs cleanly from the repo root to 28, and agrees with the cell's own `skinny/crates/bbnf-simd/src/lib.rs:5` sibling cite and the 1D `:68` sibling. | `1E-locks-evidence.md:108`; `rg 'find crates/bbnf-simd' …/p1/*.md = 0`; `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28`; `cat \| wc -l = 4401`. | None. |
| CH4-V7-001 | ACCEPT | The 1E divergence carrier (`D-1E-V5-01..14`) is cost-complete: every row carries a `loc_delta` cell + lock-pressure risk. Load-bearing EXACT figures verify on disk (builder 817, StructLayout 960, css_types 66, x86 28/4401, Pattern-H 71/67, Lock-14 falsifier 13, courier 911). | `1E-locks-evidence.md:105-118`; live `wc -l`/`rg -c`/`find` per the spot-verification table. | None. |
| CH4-V7-002 | ACCEPT | All seven 1E LAC candidates carry a wave-alignment hint AND a supporting path:line, each tied to a priced D-1E row; the LAC SPEC citations (`:358`/`:130`/`:690`) resolve to the named sections. The 1A amendment sibling is priced `≈ −1..+5 LOC` with wave hint + cross-link. | `1E-locks-evidence.md:147-153`; `1A-substrate-evidence.md:180`; `SPEC.md:358/130/690`. | None. |
| CH4-V7-003 | ACCEPT | The 1A divergence table (`1A-DIV-001..008`) is cost-complete: every row carries `loc_delta_estimate` + `risk`; the IR enum heads `:211/:355/:393` verify live as the named enums. | `1A-substrate-evidence.md:104-111`. | None. |
| CH4-V7-004 | ACCEPT | 1B D1..D6 are cost-complete; D2's dual-band (G3 ≤450 `SPEC.md:440` vs the 600-1400 envelope `ARCH:1280-1282`), D5 (+1 PartialEq, no live `runtime_target_rows_collapsed`), and D6 (+60..+200, single `REWRITE_SET`) all verify on disk. | `SPEC.md:440`; `ARCHITECTURE.md:1280-1282`; `regen.rs:5` no-PartialEq; `backend_egraph.rs:9` single REWRITE_SET; `rg runtime_target_rows_collapsed = 0`. | None. |
| CH4-V7-005 | ACCEPT | 1C D1-D9 are cost-complete; the ≈6867-LOC fold surface (67 hand-written files), the courier span (911), the −66 css_types relocate, and the PROVE `≈ +200` re-key (`SPEC.md:443`) verify; the 169956 recognizer-plane LOC (1D U-1, the distinct generated tree) verifies. | `find … -not -path '*tape*' = 67`/`cat = 6867`; `CSS_GENERATED_RS` 701-1611; `wc -l grammar/generated/*.rs = 169956`; `SPEC.md:443`. | None. |
| CH4-V7-006 | ACCEPT | The 1F-anti god-file census is cost-complete and EXACT on all ten cited files (report 11863, gate 6175, lock14_baseline 5095, generated_real_typed 4941, real_typed_struct 2827, grammar lib 2052, passes lib 2025, runtime_generator 1611, codegen lib 1473, json generated 1235); the campaign net (−10800) and P3 (−5460) source at `SYNTHESIS-AUDIT-OVERFIT.md:153/165-169`. | live `wc -l` 10/10 EXACT; `SYNTHESIS-AUDIT-OVERFIT.md:153,165-169`. | None. |
| CH4-V7-007 | ACCEPT | The simd-scan / OnceCell surface shares ONE priced SK-V19 scanner-unification disposition (≈ +20..+217) across 1F-coherence COH18-015, 1F-anti, and 1E:159; the 217-LOC probe-API + 8/9 emission basis (math = 0) verify. | `wc -l crates/simd-scan/src/{index,lib}.rs = 217`; OnceCell census 8/9. | None. |
| CH4-V7-FAB | ACCEPT (falsified suspicion) | The latent CH4 suspicion that any cited LOC/figure is recalled or fabricated as a number is FALSIFIED for every load-bearing figure spot-verified this pass. Per the corrected convention this is an inventory-is-CORRECT finding, NOT a reject — no inventory STATES anything FALSE on disk. The single residual command-path defect from V6 (CH4-V6-001) is already discharged; nothing replaces it. | The verbatim disk match on builder 817, css_types 66, StructLayout 960, Pattern-H 71/67, x86 28/4401, simd-scan 217, OnceCell 8/9 (math 0), Lock-14 falsifier 13 (11+2), courier 911, RuntimeTarget-no-PartialEq, `runtime_target_rows_collapsed`=0, single REWRITE_SET, SPEC bands `:439/:440/:443/:247`, fold surface 6867, recognizer plane 169956, all ten god-file LOC, campaign net −10800. | None — the falsifying evidence is the verbatim disk match on every spot-checked row. |

## Tally Rationale

Nine cost-lens findings, all ACCEPT (one of them the falsified-fabrication line
recorded per the corrected convention as inventory-is-correct, never a reject).
The ACCEPTs are not paper-close: the V6 CH4 REVISE is verified discharged in place
(the `skinny/`-prefix fix is live at `1E-locks-evidence.md:108` and `rg 'find
crates/bbnf-simd' …/p1/*.md` returns ZERO), and twenty-plus independent cost facts
are re-grounded against disk verbatim this pass — including every SPEC band line,
every god-file LOC, the 169956 recognizer plane, the 6867 fold surface, the 911
courier span, the 217 probe API, the 8/9 OnceCell breadth, and the 13-site (11 ir
+ 2 analysis) Lock-14 falsifier.

No cost cell is fabricated; no divergence-class row lacks a LOC band; no 1E/1A
amendment candidate lacks a wave-alignment hint + path:line; no wave hint points
to a stale or wrong SPEC line. There is no admissible REJECT — no inventory states
anything FALSE on disk that a live citation falsifies (a figure matching disk is
an ACCEPT, not a reject). This cost lens finds the inventories sound; the honest
tally is reject=0. CH4 V7 is the first clean (all-ACCEPT) cost cycle — the
V6→V7 discharge plus this clean pass establishes the run toward the
2-consecutive-clean fixed point.

TALLY accept=9 revise=0 reject=0

# SK-V18 T-P1 V6 CH4 Cost Hardening

Verdict: REVISE

Scope: CH4 COST lens (cycle V6) over the live SK-V18 T-P1 inventories
(`1A`/`1B`/`1C`/`1D`/`1E`/`1F-coherence` + `1F-anti-pattern`/`1F-past-corpora`).
This file replaces stale prior-campaign content in place per the §3Z cycle
protocol. No source files, inventories, staging, or commits were changed by this
lens.

## Lens

Per the workflow CH4 overlay + `restart/prompts/totality/PASS-1-EXCAVATION.md`:
every divergence carries a realistic LOC-delta + risk class; 1E (and 1A)
amendment candidates carry a wave-alignment hint + path:line; a candidate
without supporting evidence is REVISE. The most load-bearing cited path:line cost
rows are re-grounded on disk. CRITICAL convention: a cost cell whose figure
matches disk is an ACCEPT — finding that a figure is NOT fabricated is an ACCEPT,
not a reject. A REJECT is admissible ONLY when an inventory STATES SOMETHING
FALSE ON DISK and the live falsifying path:line is cited.

## Prior-Cycle Fold Discharge (V5 REVISE verified closed this pass)

V5 CH4 returned 1 REVISE + 1 REJECT. The single REVISE is DISCHARGED live in the
folded inventory, carrying its `CH4-V5-007` citation marker:

| V5 REVISE | target | discharge evidence (live this pass) |
|---|---|---|
| CH4-V5-007 | `1C D8` mis-cited the G3 `≤450` un-fork band at `SPEC.md:442` (which is the G5/G6 row) instead of `:440` | `1C-runtime-evidence.md:68` now reads "NOT the separate G3 `≤450` un-fork band at `:440` … G3-band line corrected `:442`→`:440` per CH4-V5-007". Live: `awk NR==440 SPEC.md` = `G3 \| §6 \| Un-fork the emitter — DELETE RuntimeEmitterKind`; `awk NR==443 SPEC.md` = the PROVE `≈ +200 Sheets adoption` row; the sibling `1B-codegen-evidence.md:83` cites the same G3 band at `:440`. The two inventories now agree; `rg ':442' 1*.md` returns ZERO. PROVE-Sheets re-key (`:443`, `≈ +200`) is intact. |

The V5 REJECT (CH4-V5-014, fabricated-LOC suspicion falsified) is re-confirmed
this pass and re-issued below as CH4-V6-FAB.

## Spot-Verification (load-bearing cost rows re-grounded LIVE this pass)

Every figure below was re-grounded on disk at the dirty tree this pass; none was
taken on recall.

| cost claim | inventory rows | live verification | result |
|---|---|---|---|
| builder.rs = 817 LOC (>500 cap) | D-1E-V5-13, L13 | `wc -l crates/core/src/runtime/css_l4/builder.rs = 817` | CONFIRMED (EXACT) |
| `css_types.rs` = 66 LOC, lock-NAMED | D-1E-V5-14, 1C D9, COH18-006 | `wc -l crates/core/src/css_types.rs = 66`; `rg css_types restart/locks/LOCKS.md` → `:349` names it verbatim ("`crates/core/src/css_types.rs`; … is the failure mode this lock prevents") | CONFIRMED |
| `StructLayout` rename surface = 960 | D-1E-V5-12 | `rg -c StructLayout crates/` sum = 960 | CONFIRMED (EXACT) |
| Pattern H = 71 total / 67 per-grammar | D-1E-V5-06, LAC-1E-V5-07 | `find crates/core/src/runtime -mindepth 2 -name '*.rs' = 71`; `… -not -path '*tape*' = 67` | CONFIRMED |
| x86 = 28 files / 4401 LOC (≈ −4500) | D-1E-V5-04, 1D G-68/D-4, 1F-anti R8 | `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28`; `cat \| wc -l = 4401` | CONFIRMED on the SKINNY path (see CH4-V6-001 for the inventory's bare-path transcript) |
| simd-scan probe-API = 217 LOC | COH18-015, 1F-anti OnceCell, 1E:159 | `wc -l crates/simd-scan/src/{index,lib}.rs = 103+114 = 217` | CONFIRMED |
| `next_structural_at_or_after` probe API skinny-absent | 1E:159, COH18-015 | `crates/simd-scan/src/lib.rs:68` `pub use index::{StructuralIndex, next_structural_at_or_after}`; `rg next_structural_at_or_after skinny/crates/bbnf-simd/src = 0` | CONFIRMED (asymmetry real) |
| OnceCell emitted 8 of 9 grammars (math = 0) | COH18-015, 1F-anti, 1E:159 | per-grammar `rg -c 'OnceCell<::simd_scan::StructuralIndex>\|ensure_structural_index'`: bbnf/bnf/css_l4/css_pretty/csv/ebnf/json = 2, google_sheets = 3, **math = 0** | CONFIRMED (8/9 EXACT) |
| Lock-14 self-gate FALSIFIED: 13 live sites | D-1E-V5-14 (HIGH) | `rg -c 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src crates/analysis/src` = 13 (11 ir + 2 analysis) vs `LOCKS.md:349` "returns ZERO" | CONFIRMED (gate RED) |
| CSS courier const `:701` | D-1E-V5-01, 1C D1, COH18-003 | `runtime_generator.rs:701` `const CSS_GENERATED_RS` (911-LOC verbatim span) | CONFIRMED |
| D5 `RuntimeTarget` derives Clone/Copy/Debug, NO PartialEq (+1) | 1B D5, LAC-1E-V5-02 | `awk NR==5 skinny/xtask/src/regen.rs` = `#[derive(Clone, Copy, Debug)]` | CONFIRMED |
| `runtime_target_rows_collapsed` spec-planned at `SPEC.md:247`, NO live def | 1B D5, LAC-1E-V5-02 | `awk NR==247 SPEC.md` = the planned co-gate; `rg runtime_target_rows_collapsed skinny/crates skinny/xtask = 0` | CONFIRMED |
| D6 `REWRITE_SET` single-pool (+60..+200) | 1B D6 | `skinny/crates/passes/src/backend_egraph.rs:9` = `const REWRITE_SET: &str = "sk-v15-w7-direct-sink-normalization-v1";` (inventory cites the full path correctly at `1B:63,:125`) | CONFIRMED |
| SPEC G2 ≤450 band at `:439` (D-1E-V5-11 import) | D-1E-V5-11, CH4-V4-009 | `awk NR==439 SPEC.md` = `G2 \| §5 \| CSS lowering …` ≤450 band | CONFIRMED |
| 1A IR rows: `ExprKind` enum `:211`, `BackendExpr` `:355`, `Recognizer::SimdScan` `:393` | 1A-DIV-001/002 | `awk` reads at each line = the named enum heads | CONFIRMED |
| SK-V17 reconcile clauses `LOCKS.md:614/616/620/622` | 1E spec-claim + 1A amend | `awk` = Lock 1 tape-union / Lock 2 StructLayout / Lock 14 ValueRef / Lock 16 NEON | CONFIRMED |
| 1A amendment band `≈ −1..+5 LOC` + wave hint + path:line | 1A-LOCK1-AMEND-001 (`:180`) | clause `LOCKS.md:620` + §9.2 prose `ARCHITECTURE.md:1990,:1997` live-read; wave hint "SK-V19 LOCKS reconcile / Pass Omega"; cross-link to 1E sibling | CONFIRMED |

The cost spine of this packet is, on its load-bearing rows, materially accurate.
Twenty independent cost facts re-grounded clean this pass, the V5 REVISE
discharged in place with its `CH4-V5-007` marker live, and the G3-band line now
agrees across `1C D8` and `1B D2`. The fabrication suspicion is falsified on every
spot-checked row (CH4-V6-FAB). The ONE residual defect is a bare-from-root `find`
command inside a HIGH-risk cost row — see CH4-V6-001.

## Cost-Completeness Census (every divergence-class row carries a LOC band)

A targeted scan confirms every divergence-class row carries a realistic LOC-delta
+ risk class.

| inventory | divergence rows | banded? |
|---|---|---|
| 1A | `1A-DIV-001..008` (`:104-111`) | YES — `400-900 / 600-1200 / 300-700 / 250-600 / 20-80 / 400-900 / 200-500 / 80-300` + risk |
| 1B | D1..D6 (`:68-127`) | YES — `≈ −910 / dual-band ≤450-or-600-1400 / net ≈0..+150 / ≈0 / +1 / +60..+200` |
| 1C | D1..D9 (`:47-72`) | YES — D4 ≈0, D5 −10..−40, D6 −4000..−5000, D7 ≈0, D8 ≈+200, D9 ≈−66 (D1-D3 inline LOC) |
| 1E | D-1E-V5-01..14 (`:105-118`) | YES — every row carries a `loc_delta` cell + lock-pressure risk |
| 1F-coherence | COH18-001..015 (`:94-104`) | YES — every row LOC/risk-celled; COH18-015 banded per CH4-V4-007 |
| 1F-anti | PRUNE-receiver table (`:71-79`) | YES — each receiver carries a Net-LOC column |

## 1E LOCKS-Amendment-Candidate cost check (all 7 carry wave hint + path:line)

Re-confirmed live: each of `LAC-1E-V5-01..07` carries a wave-alignment hint AND a
concrete supporting path:line, each tied to a priced `D-1E-V5-NN` row
(`1E-locks-evidence.md:147-153`). The 1A amendment sibling
(`1A-LOCK1-AMEND-001`, `:180`) carries `≈ −1..+5 LOC` + wave hint + cross-link to
the 1E sibling. No candidate lacks a supporting path:line.

## Findings

| id | disposition | finding | evidence | required correction |
|---|---|---|---|---|
| CH4-V6-DISCH | ACCEPT | The V5 CH4 REVISE (CH4-V5-007) is discharged in place: `1C D8`'s G3-band pointer is corrected `:442`→`:440`, now agreeing with the sibling `1B D2` cost cell, and the PROVE `:443` `≈ +200` re-key is intact. | `1C-runtime-evidence.md:68` carries the `:440` + `per CH4-V5-007` marker; `awk NR==440 SPEC.md` = the G3 row; `rg ':442' 1*.md` = ZERO. | None. |
| CH4-V6-002 | ACCEPT | The 1E divergence carrier (`D-1E-V5-01..14`) is cost-complete: every row carries a `loc_delta` cell + lock-pressure risk. Load-bearing EXACT figures verify on disk (builder 817, StructLayout 960, css_types 66, x86 4401, Pattern-H 71/67, Lock-14 falsifier 13). | `1E-locks-evidence.md:105-118`; live `wc -l`/`rg -c`/`find` per the spot-verification table above. | None. |
| CH4-V6-003 | ACCEPT | All seven 1E LAC candidates carry a wave-alignment hint AND a supporting path:line, each tied to a priced D-1E row; the 1A amendment sibling is priced `≈ −1..+5 LOC` with wave hint + cross-link. | `1E-locks-evidence.md:147-153`; `1A-substrate-evidence.md:180`. | None. |
| CH4-V6-004 | ACCEPT | The 1A divergence table (`1A-DIV-001..008`) is cost-complete: every row carries `loc_delta_estimate` + `risk`; the cited IR enum heads verify live. | `1A-substrate-evidence.md:104-111`; `ir/src/lib.rs:211/355/393`. | None. |
| CH4-V6-005 | ACCEPT | 1B D1..D6 are cost-complete; D5 (+1 PartialEq, no live `runtime_target_rows_collapsed`) and D6 (+60..+200, single `REWRITE_SET`) impl-citations verify on disk; the inventory cites the full `skinny/crates/passes/src/backend_egraph.rs:9` path correctly. | `regen.rs:5` no-PartialEq; `backend_egraph.rs:9` single REWRITE_SET; `rg runtime_target_rows_collapsed = 0`. | None. |
| CH4-V6-006 | ACCEPT | The simd-scan / OnceCell surface shares ONE priced SK-V19 scanner-unification disposition (≈ +20..+217) across 1F-coherence COH18-015, 1F-anti, and 1E:159; the 217-LOC probe-API + 8/9 emission basis + skinny-absent `next_structural_at_or_after` verify. | `wc -l crates/simd-scan/src/{index,lib}.rs = 217`; OnceCell census 8/9 (math = 0); `rg next_structural_at_or_after skinny/crates/bbnf-simd/src = 0`. | None. |
| CH4-V6-001 | REVISE | `D-1E-V5-04`'s x86 cost row carries a bare-from-root `find` command in its evidence cell that does NOT resolve from the repo root: `find crates/bbnf-simd/src/x86_64 crates/bbnf-simd/ext/x86 -type f = 28`. Run literally at the repo root this emits TWO `No such file or directory` errors and returns 0 — NOT 28 — because the x86 surface lives under `skinny/crates/bbnf-simd/`, and there is NO root `crates/bbnf-simd/`. The `28`/`4401`/`≈ −4500` figure is TRUE (verified via the SKINNY path), but the disk-anchor command substantiating that HIGH-risk cost row points to an empty tree. This is the identical defect class CH1-V2-F4 corrected (a bare path needing the `skinny/crates/...` prefix) and CH4-V5-007 forbade (a wrong path inside a cost-carrier row); the SIBLING citation in the SAME cell already carries the correct `skinny/crates/bbnf-simd/src/lib.rs:5`, so one row disagrees with itself. `D-1E-V5-04` is the LONE inventory carrying this bare-from-root `find crates/bbnf-simd/...` form (1D uses `skinny/crates/bbnf-simd/...` at `:68`; 1F-anti uses crate-relative prose shorthand at `:73`; every other inventory `find crates/...` targets `crates/core/...` or `crates/simd-scan/...`, which DO resolve from root). A T-P2 reader copy-pasting this cost-substantiation command gets 0 and would read the `−4500` figure as falsified — proportionate-misleading. | `1E-locks-evidence.md:108` evidence cell: `find crates/bbnf-simd/src/x86_64 crates/bbnf-simd/ext/x86 -type f = 28`; live `find crates/bbnf-simd/src/x86_64 crates/bbnf-simd/ext/x86 -type f` from root = 2 errors / 0 files; `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28`; sibling cite in same cell already prefixed `skinny/crates/bbnf-simd/src/lib.rs:5`; `rg 'find crates/bbnf-simd' 1*.md` returns this ONE site only; CH1-V2-F4 prefix discipline; CH4-V1/V2 transcribed the canonical runnable form WITH the `skinny/` prefix (`V1/CH4.md:24`, `V2/CH4.md:25`). | In `1E-locks-evidence.md:108`, prefix both `find` operands with `skinny/`: `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28`, matching the cell's own `skinny/crates/bbnf-simd/src/lib.rs:5` cite and the 1D sibling at `:68`. One-token-per-operand edit; the `28`/`4401`/`≈ −4500` figures are correct and stay. |
| CH4-V6-FAB | REJECT | The latent CH4 suspicion that any cited LOC/figure is recalled or fabricated as a number is FALSIFIED for every load-bearing figure spot-verified this pass. A blanket "fabricated-figure" REVISE would be uncited. Per the corrected convention this is recorded as the falsified-suspicion line, NOT a reject of any inventory claim — no inventory STATES anything FALSE on disk (the D-1E-V5-04 `28`/`4401` figure is TRUE via the skinny path; only the command's path prefix is wrong, handled as CH4-V6-001). | The verbatim disk match on builder 817, css_types 66, StructLayout 960, Pattern-H 71/67, x86 28/4401 (skinny), simd-scan 217, OnceCell 8/9, Lock-14 falsifier 13, courier `:701`, RuntimeTarget-no-PartialEq, `runtime_target_rows_collapsed`=0, single REWRITE_SET, SPEC G2 `:439` / G3 `:440` / PROVE `:443`, 1A IR enum heads `:211/:355/:393`, LOCKS reconcile `:614/:616/:620/:622`. | None — the falsifying evidence is the verbatim disk match on every spot-checked row. |

## Tally Rationale

Eight cost-lens findings: 6 ACCEPT, 1 REVISE, 1 REJECT-as-falsified-suspicion.
The ACCEPTs are not paper-close: the V5 REVISE is verified discharged in place
with its `CH4-V5-007` marker live and the G3-band line now agreeing across `1C D8`
and `1B D2`, and twenty independent cost facts are re-grounded against disk
verbatim this pass.

The single REVISE (CH4-V6-001) is a genuine close-reading catch, NOT a
manufactured floor-filler: `D-1E-V5-04`'s x86 cost row carries a `find` command
that, run literally from the repo root, errors and returns 0 instead of the
asserted 28, because the surface lives under `skinny/crates/bbnf-simd/` and no
root `crates/bbnf-simd/` exists. This is the same bare-path-prefix defect class
CH1-V2-F4 corrected and CH4-V5-007 forbade (a wrong path inside a cost-carrier
row), it is isolated (the LONE inventory site of `find crates/bbnf-simd/...`), the
cell already carries the correct `skinny/`-prefixed sibling cite so the row
disagrees with itself, and the prior CH4-V1/V2 cycles themselves transcribed the
canonical runnable command WITH the `skinny/` prefix. A T-P2 reader copy-pasting
the cost-substantiation command would see the `−4500` figure apparently
falsified. It is a one-operand-prefix inventory edit and reopens no figure proven
correct.

The REJECT (CH4-V6-FAB) records, per the corrected convention, that the
fabricated-figure suspicion is FALSIFIED on every spot-verified row — an
inventory-is-correct finding, not a reject of any inventory claim; no inventory
states anything false on disk. CH4 cannot move all-ACCEPT until `D-1E-V5-04`'s
bare `find crates/bbnf-simd/...` is prefixed to `skinny/crates/bbnf-simd/...`.

TALLY accept=6 revise=1 reject=1

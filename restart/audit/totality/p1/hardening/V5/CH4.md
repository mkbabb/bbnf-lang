# SK-V18 T-P1 V5 CH4 Cost Hardening

Verdict: REVISE

Scope: CH4 COST lens (cycle V5) over the live SK-V18 T-P1 inventories
(`1A`/`1B`/`1C`/`1D`/`1E`/`1F-coherence` + `1F-anti-pattern`/`1F-past-corpora`).
This file replaces stale prior-campaign (SK-V15) CH4 content in place per the §3Z
cycle protocol. No source files, inventories, staging, or commits were changed by
this lens.

## Lens

Per the workflow CH4 overlay (`skv18-t-p1-workflow.mjs:53`) +
`restart/prompts/totality/PASS-1-EXCAVATION.md`: every divergence carries a
realistic LOC-delta + risk class; 1E (and 1A) amendment candidates carry a
wave-alignment hint + path:line; a candidate without supporting evidence is
REVISE. The most load-bearing cited path:line cost rows are re-grounded on disk.

## Prior-Cycle Fold Discharge (V4 REVISEs verified closed this pass)

V4 CH4 returned 3 REVISE + 1 REJECT. All three REVISEs are DISCHARGED live in the
folded inventories, each carrying its `CH4-V4-NNN` citation marker:

| V4 REVISE | target | discharge evidence (live this pass) |
|---|---|---|
| CH4-V4-007 | COH18-015 lacked a LOC band | `1F-coherence-scan.md:104` COH18-015 cost cell now reads "≈ +20..+217 (SK-V19 scanner-unify … band per CH4-V4-007)"; `rg -c CH4-V4-007 1F-coherence-scan.md = 1`. The 217-LOC probe-API surface verifies: `wc -l crates/simd-scan/src/{index.rs,lib.rs} = 103+114 = 217`. |
| CH4-V4-008 | simd-scan surface routed across 3 inventories with no shared priced disposition | The ≈ +20..+217 shared SK-V19 scanner-unification disposition is now cross-linked across ALL THREE: `1F-coherence-scan.md:104` (COH18-015), `1F-anti-pattern.md:44` (OnceCell row), `1E-locks-evidence.md:159` (the no-candidates scanner carry). `rg -c CH4-V4-008` = 1 in each of the three files. |
| CH4-V4-009 | D-1E-V5-11 omitted the G2 `≤450` band | `1E-locks-evidence.md:115` D-1E-V5-11 cost cell now reads "new-primitive ≤450 LOC / G2 build (`SPEC.md:439` G2 band imported per CH4-V4-009 …)"; `rg -c CH4-V4-009 1E-locks-evidence.md = 1`. SPEC G2 `:439` live-verified as the `≤450 hand source/test/gate LOC` owner. |

The V4 REJECT (CH4-V4-010, fabricated-LOC suspicion falsified) is re-confirmed
this pass and re-issued below as CH4-V5-014.

## Spot-Verification (load-bearing cost rows re-grounded LIVE this pass)

Every figure below was re-grounded on disk at the dirty tree this pass; none was
taken on recall.

| cost claim | inventory rows | live verification | result |
|---|---|---|---|
| simd-scan probe-API = 217 LOC | COH18-015, 1F-anti OnceCell, 1E:159 | `wc -l crates/simd-scan/src/{index.rs,lib.rs}` = 103+114 = 217 | CONFIRMED |
| OnceCell emitted into 8 of 9 generated grammars (all but `math`) | COH18-015, 1F-anti, 1E:159, CH5 | per-grammar `rg -c 'OnceCell<::simd_scan::StructuralIndex>\|ensure_structural_index'`: bbnf/bnf/css_l4/css_pretty/csv/ebnf/json = 2, google_sheets = 3, **math = 0** | CONFIRMED (8/9 exact) |
| builder.rs = 817 LOC (>500 cap) | D-1E-V5-13, COH18 anti | `wc -l crates/core/src/runtime/css_l4/builder.rs = 817` | CONFIRMED |
| `css_types.rs` = 66 LOC | COH18-006, 1C D9, 1F-anti | `wc -l crates/core/src/css_types.rs = 66` | CONFIRMED |
| Pattern H = 71 total / 67 per-grammar | D-1E-V5-06, COH18-007, 1D U-1 | `find … = 71`; `… -not -path '*tape*' = 67` | CONFIRMED |
| x86 = 28 files (24 x86_64 + 4 ext/x86) / 4401 LOC / ≈ −4500 | D-1E-V5-04, 1D G-68, COH18-009, 1F-anti R8 | `find …/x86_64 = 24`; `find …/ext/x86 = 4`; combined `= 28`; `cat \| wc -l = 4401` | CONFIRMED (24-vs-28 is dir-scope, not a discrepancy) |
| `CSS_GENERATED_RS` courier const | D-1E-V5-01, 1C D1, COH18-003 | `rg -n 'const CSS_GENERATED_RS' runtime_generator.rs` → `:701` | CONFIRMED |
| 1C D6 / 1D U-1 carry = 6867 LOC across 67 files | 1C D6, 1D U-1 | `find crates/core/src/runtime … -not -path '*tape*' \| cat \| wc -l = 6867` | CONFIRMED |
| 1D U-1 generated plane = 169956 LOC (detached from carry) | 1D U-1 | `wc -l crates/core/src/grammar/generated/*.rs \| tail -1 = 169956` | CONFIRMED |
| prune ladder net −10800 / P3 −5460 | 1D G-13, 1F-anti Net-LOC | `SYNTHESIS-AUDIT-OVERFIT.md:153,165` carry the figures verbatim | CONFIRMED |
| SPEC PROVE Sheets = `≈ +200` (`:443`) | 1C D8 | `awk 443 SPEC.md` = PROVE "≈ +200 Sheets adoption …" | CONFIRMED (re-key target correct) |
| `StructLayout` rename surface = 960 | D-1E-V5-12 | crate-wide `rg -c StructLayout crates/` sum = 960 | CONFIRMED (EXACT) |
| D5 `RuntimeTarget` derives Clone/Copy/Debug, NO PartialEq (+1 derive) | 1B D5 | `sed -n '5p' skinny/xtask/src/regen.rs` = `#[derive(Clone, Copy, Debug)]` | CONFIRMED |
| `runtime_target_rows_collapsed` is spec-planned, NO live def | 1B D5, LAC-1E-V5-02 | `rg -c runtime_target_rows_collapsed skinny/crates skinny/xtask = 0` | CONFIRMED |
| D6 `REWRITE_SET` single-pool | 1B D6 | `sed -n '9p' backend_egraph.rs` = `const REWRITE_SET: &str = "sk-v15-w7-direct-sink-normalization-v1";` | CONFIRMED |

The cost spine of this packet is, on its load-bearing rows, materially accurate.
Fifteen+ independent cost facts re-grounded clean, the three V4 REVISEs each
discharged in place with their `CH4-V4-NNN` markers, and the prune ladder traces
verbatim to source. The fabrication suspicion is falsified on every spot-checked
row (CH4-V5-014). The ONE defect is a wrong path:line inside a cost cell —
see CH4-V5-007.

## Cost-Completeness Census (every divergence-class row carries a LOC band)

A targeted scan of all six inventories' divergence tables confirms every
divergence-class row now carries a realistic LOC-delta + risk class — the
last unpriced row (COH18-015) was the V4 CH4-V4-007 target, now banded.

| inventory | divergence rows | banded? |
|---|---|---|
| 1A | `1A-DIV-001..008` (`1A-substrate-evidence.md:104-111`) | YES — every row carries `loc_delta_estimate` + `risk` (400-900/600-1200/300-700/250-600/20-80/400-900/200-500/80-300) |
| 1B | D1..D6 (`1B-codegen-evidence.md:68-127`) | YES — −910 / dual-band ≤450-or-600-1400 / net ≈0..+150 / ≈0 / +1 / +60..+200 |
| 1C | D1..D9 (`1C-runtime-evidence.md:47-72`) | YES — D4 ≈0, D5 −10..−40, D6 −4000..−5000, D7 ≈0, D8 ≈+200, D9 ≈−66 (D1/D2/D3 carry inline LOC) |
| 1E | D-1E-V5-01..14 (`1E-locks-evidence.md:105-118`) | YES — every row carries a `loc_delta` cell + lock-pressure risk |
| 1F-coherence | COH18-001..015 (`1F-coherence-scan.md:94-104`) | YES — every Divergences-Catalogued row carries a LOC/risk cell; COH18-015 now banded |
| 1F-anti | PRUNE-receiver table (`1F-anti-pattern.md:71-79`) | YES — each receiver carries a Net-LOC column |

## 1E LOCKS-Amendment-Candidate cost check (all 7 carry wave hint + path:line)

| candidate | wave hint | supporting path:line | priced? |
|---|---|---|---|
| LAC-1E-V5-01 | G2 ∧ G1 | `SYNTHESIS-RESEARCH.md:257-266`; SPEC `:358-390`; SYNTHESIS-AUDIT `:103` | D-1E-V5-09 `+20..+80` |
| LAC-1E-V5-02 | G3 ∧ P3 | SPEC `:179-185,:247,:1115`; SYNTHESIS-RESEARCH `:272-279` | D-1E-V5-10 `+1 co-gate derive` |
| LAC-1E-V5-03 | G2 ∧ G6 | `SYNTHESIS-RESEARCH.md:231-237`; SPEC `:973,:985-996,:1034` | D-1E-V5-11 `≤450 / G2 build` |
| LAC-1E-V5-04 | P1 | SPEC `:51-52,:130`; `bbnf-simd/src/lib.rs:5`; SYNTHESIS-AUDIT `:92` | D-1E-V5-04 `≈ −4500` |
| LAC-1E-V5-05 | G1 ∧ G2 | `runtime_generator.rs:701`; SYNTHESIS-AUDIT `:50,:122-124`; SPEC `:329-339` | D-1E-V5-01 `≈ −910` |
| LAC-1E-V5-06 | P4 (before G2/G3) | `lock14_baseline.rs:2420,:2442,:2463`; SYNTHESIS-AUDIT `:170-175` | D-1E-V5-05 `gate-only` |
| LAC-1E-V5-07 | totality-core census / SK-V19 | `find … = 71`; `LOCKS.md:408-409` | D-1E-V5-06 `+4 census` |

All seven carry both a wave-alignment hint and a concrete supporting path:line; each
ties to a priced D-1E divergence row. The 1A amendment sibling
(`1A-LOCK1-AMEND-001`) carries `≈ −1..+5 LOC` + wave hint `SK-V19 LOCKS reconcile /
Pass Omega` + cross-link to the 1E sibling (`1A-substrate-evidence.md:180`).

## Findings

| id | disposition | finding | evidence | required correction |
|---|---|---|---|---|
| CH4-V5-001 | ACCEPT | All three V4 CH4 REVISEs are discharged in place: COH18-015 banded (CH4-V4-007), the simd-scan surface cross-linked across three inventories to ONE shared SK-V19 disposition (CH4-V4-008), and D-1E-V5-11 imports its G2 `≤450` band (CH4-V4-009). | Discharge table above; `rg -c` confirms each `CH4-V4-NNN` marker present in its named inventory file(s). | None. |
| CH4-V5-002 | ACCEPT | The 1E divergence carrier (`D-1E-V5-01..14`) is cost-complete: every row carries a `loc_delta` cell + a lock-pressure risk class. | `1E-locks-evidence.md:105-118`; e.g. D-1E-V5-13 `≈ −817 builder retire (CH4 EXACT)` verified live `wc -l = 817`; D-1E-V5-12 `960 sites` verified `rg -c StructLayout crates/ = 960`. | None. |
| CH4-V5-003 | ACCEPT | All seven 1E LAC candidates carry a wave-alignment hint AND a supporting path:line, each tied to a priced D-1E row. | LAC table above; `1E-locks-evidence.md:147-153`. | None. |
| CH4-V5-004 | ACCEPT | The 1A divergence table (`1A-DIV-001..008`) is cost-complete: every row carries `loc_delta_estimate` + `risk`; the 1A amendment (`1A-LOCK1-AMEND-001`) is priced `≈ −1..+5 LOC` with wave hint + cross-link to the 1E sibling. | `1A-substrate-evidence.md:104-111`, `:180`. | None. |
| CH4-V5-005 | ACCEPT | The COH18-015 / 1F-anti OnceCell / 1E:159 simd-scan surface shares ONE priced SK-V19 scanner-unification disposition (≈ +20..+217), and the 217-LOC probe-API + 8/9 emission-site basis verifies on disk. | `wc -l crates/simd-scan/src/{index,lib}.rs = 217`; per-grammar OnceCell census 8/9 (math = 0). | None. |
| CH4-V5-006 | ACCEPT | 1B D1..D6 are cost-complete; the load-bearing D5 (+1 PartialEq) and D6 (+60..+200) impl-citations verify on disk; the prior uncited `+400..+1200` D2 figure remains greppable to 0 in SPEC (CH4-V2-008 held). | `sed -n '5p' regen.rs` = `#[derive(Clone, Copy, Debug)]` (no PartialEq); `sed -n '9p' backend_egraph.rs` = the single `REWRITE_SET`; `rg runtime_target_rows_collapsed = 0`. | None. |
| CH4-V5-007 | REVISE | `1C D8`'s cost cell mis-cites the G3 un-fork band line. The CH4-V3-008 re-key text says "NOT the separate G3 `≤450` un-fork band at **`:442`**" — but `SPEC.md:442` is the **G5/G6** row (`G5/G6 \| §8 \| Neutral scan retarget — NEON`), NOT G3. The G3 row (`G3 \| §6 \| Un-fork the emitter — DELETE RuntimeEmitterKind`) is at `SPEC.md:440`, which is exactly where the sibling `1B D2` cost cell correctly cites it (`1B-codegen-evidence.md:83` `… per SPEC G3 SPEC.md:440`). The two inventories disagree on the G3 band line, and the 1C value is the falsified one. The substantive re-key (PROVE Sheets `≈ +200` keyed to `:443`) is itself correct — only the contrasting G3 pointer carries the wrong line. A recalled/wrong path:line inside a cost cell is exactly the defect class this lens forbids. | `1C-runtime-evidence.md:68` cites the G3 band at `:442`; live `awk 'NR==440' SPEC.md` = the G3 row, `awk 'NR==442' SPEC.md` = the G5/G6 row; `1B-codegen-evidence.md:83` cites the same G3 band correctly at `:440`; `rg -n ':442' 1*.md` returns this ONE site only. | In `1C-runtime-evidence.md:68`, change "the separate G3 `≤450` un-fork band at `:442`" to `:440`, matching the verified G3 row line and the sibling `1B D2` citation. One-token edit; the PROVE `:443` re-key and `≈ +200` band are correct and stay. |
| CH4-V5-014 | REJECT | The latent CH4 suspicion that any cited LOC is recalled/fabricated as a number is FALSIFIED for every load-bearing figure spot-verified this pass. A blanket "fabricated-LOC" REVISE would be uncited. | The verbatim disk match on simd-scan 217, OnceCell 8/9, builder 817, css_types 66, Pattern-H 71/67, x86 24+4/4401, CSS courier `:701`, D6 carry 6867, generated plane 169956, StructLayout 960, RuntimeTarget-no-PartialEq, REWRITE_SET single-pool, prune ladder verbatim at `SYNTHESIS-AUDIT-OVERFIT.md:153,165`, SPEC PROVE `:443` `≈ +200`. | None — the falsifying evidence is the verbatim disk match on every spot-checked row. |

## Tally Rationale

Eight cost-lens findings: 6 ACCEPT, 1 REVISE, 1 REJECT. The ACCEPTs are not
paper-close: the three V4 REVISEs are each verified discharged in place with their
`CH4-V4-NNN` markers live, and fifteen-plus independent cost facts plus the full
prune ladder are re-grounded against disk verbatim this pass.

The single REVISE (CH4-V5-007) is a genuine close-reading catch, NOT a
manufactured floor-filler: the CH4-V3-008 fold that re-keyed 1C D8's PROVE-Sheets
cost basis to `:443` (correct) left the contrasting G3-band pointer at `:442`,
which on disk is the G5/G6 row — the G3 row is `:440`, exactly where the sibling
1B D2 cost cell cites it. A wrong path:line inside a cost cell is the precise
defect class CH4 forbids ("Spot-verify the most load-bearing cited path:line
rows … no recalled LOC/symbol"); two sibling inventories cannot disagree on the
same band's line with one of them falsified by disk. It is a one-token inventory
edit, isolated (`rg ':442'` returns exactly one site), and reopens no figure
proven correct.

The REJECT (CH4-V5-014) records that the fabrication suspicion is falsified on
every spot-verified row. CH4 cannot move all-ACCEPT until 1C D8's G3-band
pointer is corrected from `:442` to `:440`.

TALLY accept=6 revise=1 reject=1

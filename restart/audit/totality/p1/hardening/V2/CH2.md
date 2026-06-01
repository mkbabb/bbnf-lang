---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V2
campaign: SK-V18-TOTALITY-EXCAVATION
disposition: REVISE
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
spot_verified_live:
  - crates/ir/src/registry/strategy.rs
  - crates/core/src/css_types.rs
  - crates/core/src/runtime/ (Pattern H census + leak scan)
  - crates/core/src/grammar/generated/json.rs (OnceCell probe)
  - skinny/crates/codegen/src/lib.rs + json_templates/
  - skinny/crates/codegen/src/grammar_provider.rs
  - skinny/crates/runtime/src/tape/mod.rs
  - skinny/crates/runtime/src/grammars/css_l4_*/generated.rs (md5)
  - skinny/crates/runtime/src/grammars/json/generated.rs
  - skinny/crates/bbnf-bench/src/lock14_baseline.rs
---

# T-P1 V2 CH2 — GENERALITY / Lock 14 (cycle V2)

## Verdict

REVISE. The V5-SKV18-totality inventories (regenerated 2026-06-01) have absorbed
every V1 and prior-V2 CH2 fold: 1F-anti-pattern now catalogues the grammar-named
modules in generic `codegen` (`json_sink_direct`, `json_typed_direct`,
`json_templates/`) that V1 CH2-004 found uncited; 1C's executive summary scopes
the phantom to the `G` AXIS (not the whole `ValueRef`); COH18-012 is reclassified
off impl-exceeds-spec with the correct `:1643`-vs-`:2215` gate scope; the
`css_types.rs` Lock-14(c) hedge is dropped and re-anchored to a
relocate-to-`crates/css/` condition; and 1F-anti's grammar-name-leak table now
IS the compact Surface/Verdict/Receiver leak map the prior V2 cycle demanded. 1D
cleanly partitions JSON/CSS-empirical (J-1..J-3, C-1..C-4) from grammar-neutral
(G-1..G-13); no grammar-neutral failure is smuggled as JSON-only and no JSON/CSS
row is promoted to fleet-wide closure. I spot-verified the load-bearing rows
LIVE — strategy.rs ident table (11 ir-crate sites), css_types.rs (66 LOC,
line-1 named), Pattern H 71/67, `ValueRef` K-real/G-phantom split, 7× md5
`b654562c`, 12/4 runtime leak — all true.

REVISE, not ACCEPT, on two LIVE, fold-correctable counts, both touching the
single sharpest CH2 bar ("no grammar-name leak passes uncited"):

1. The `crates/ir/src/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE`
   grammar-name leak is UNDERCOUNTED. 1F-anti, COH18-005, and COH18-012 all
   enumerate the table as carrying FOUR grammar-named rows (Json `:137`, Sheets
   `:143`, CssL4 `:149`, Bbnf `:155`) — the four the narrow 4-name leak regex
   catches. LIVE the table carries NINE grammar-named `idents` rows; the five
   uncited are Csv (`:161`), Math (`:167`), Bnf (`:173`), Ebnf (`:179`),
   CssPretty (`:185`). The leak in the generic `ir` crate is 9-grammar-wide and
   the catalogue presents it as 4-grammar-wide — five grammar names pass uncited
   in the inventory designated to catch them.

2. 1F-anti-pattern.md:64 mis-attributes the §9 gate scope, reviving the exact
   error V1 CH2-003 corrected for COH18-012. The row says "the ARCH leak scan
   (`:2215`) is scoped to `codegen/` and MISSES this." FALSE as cited:
   `ARCHITECTURE.md:2215` scans `crates/{ir,parse,codegen,runtime,...}/src/`
   (its scope list includes `ir`) and DOES catch strategy.rs (11 live sites). The
   genuinely codegen-scoped command is `ARCHITECTURE.md:1643`. The 1F-anti row
   contradicts COH18-012 IN THE SAME V5 inventory set, which states the gate
   correctly.

REJECT is not warranted under this lens: no CH2 claim I tested is recalled,
false, or fabricated. Every leak the inventories DO catalogue is live at the
cited path:line; the two defects are an undercounted leak breadth and one
intra-set scope contradiction — both correctable in the fold without
re-excavation.

## Governing Evidence

| Source | CH2 requirement |
|---|---|
| `restart/prompts/totality/PASS-1-EXCAVATION.md:110-114` | No divergence catalogued JSON-only when grammar-neutral; 1C flags every grammar-named module in a generic crate; 1D separates JSON/CSS-empirical from grammar-neutral; **no grammar-name leak passes uncited.** |
| `restart/prompts/ORCHESTRATOR.md` CH2 | Lock 14 holds; every intervention grammar-neutral, works for CSS L4 / Sheets / BBNF-self, not only JSON. |
| `restart/locks/LOCKS.md:349` | Generic crates (`bbnf-codegen`, `bbnf-ir`, …) carry ZERO grammar-named modules, ZERO grammar-specific public types, ZERO `match grammar {Json=>…}` arms; `crates/core/src/css_types.rs` is NAMED as the overfit mess. Verification command scans `crates/{ir,parse,codegen,runtime,…}/src/`. |
| `restart/ARCHITECTURE.md:2215` vs `:1643` | The §9 Lock-14 gate command (`:2215`) scans `crates/{ir,…}/src/` (catches `ir`); the §12 Backend-impl table row (`:1643`) is the narrow `crates/codegen/src/`-scoped command. |

## Findings (every CH2 finding enumerated)

| ID | Disposition | Finding | Evidence / correction |
|---|---|---|---|
| CH2-V2-001 | ACCEPT | 1D cleanly separates JSON/CSS-empirical from grammar-neutral per Lock 14. J-1/J-2 (JSON guard), J-3/C-1/C-2 (CSS measurement-valid + courier + absent value API), C-3/C-4 (CSS pending) sit in the JSON/CSS-empirical table; the substrate (G-1), aarch64 (G-2), 5-shape spine (G-3), SIMD discipline (G-4), FNV quarantine (G-5), emitter fork (G-6), gate-by-exclusion (G-7), phantom (G-8), inflection (G-9), profile (G-10), named-primitive escape (G-11), relocated-seam (G-12), prune (G-13) sit in the grammar-neutral table. No grammar-neutral failure is smuggled JSON-only; no JSON/CSS row is promoted to fleet-wide closure. | `1D-skinny-lessons.md:175-185` (empirical) vs `:189-203` (grammar-neutral); live partition spot-checked correct. |
| CH2-V2-002 | ACCEPT | V1 CH2-004 / CH2-FOLD-002 DISCHARGED: the grammar-named modules in generic `codegen` are now catalogued. 1F-anti rows for `skinny/crates/codegen/src/lib.rs:4-5` (`mod json_sink_direct`, `mod json_typed_direct`) and `json_templates/` (full template roster) are present as Lock 14 (a) leaks with G1/P4 receivers. | `1F-anti-pattern.md:62-63`; live `lib.rs:4-5` + `json_templates/{config,generated,parser,value,view,visitor}.rs` confirmed. |
| CH2-V2-003 | ACCEPT | V1 CH2-005 / CH2-FOLD-003 DISCHARGED: the phantom is scoped to the `G` AXIS. 1C exec summary reads "The `G:EventGrammar` AXIS of `ValueRef<…>` is a phantom test-only generic (the `K`=Kind axis is real)." Live `ValueRef<…,K=AnyKind,G:EventGrammar=AnyGrammar>` carries separate `_kind: PhantomData<fn()->K>` (real, JsonNodeKind/RootKind dispatch) and `_grammar: PhantomData<fn()->G>` (decorative). | `1C-runtime-evidence.md:24`, `:34`, `:58`; live `tape/mod.rs:175-180`. |
| CH2-V2-004 | ACCEPT | V1 CH2-003 / CH2-FOLD-001 DISCHARGED in the COH18-012 row: the narrow command is correctly re-cited as `ARCHITECTURE.md:1643` (codegen-scoped §12), the §9 gate `:2215` + `LOCKS.md:349` correctly recorded as catching strategy.rs (11 sites), and the row reclassified off impl-exceeds-spec to spec-defect-on-`:1643`. | `1F-coherence-scan.md:82`, `:99`, `:110`; live `:2215` scope includes `ir`; `rg ... crates/ir/src/` = 11. |
| CH2-V2-005 | ACCEPT | V1 CH2-007 / CH2-FOLD-004 DISCHARGED: the `css_types.rs` Lock-14(c) "may be admissible" hedge is dropped. COH18-006 + U-COH18-002 record it lives in `crates/core/src/` (NOT a `crates/<grammar>/` declaration crate), so Lock 14 (c) does NOT apply; it is the lock-NAMED mess, admissible ONLY if relocated to `crates/css/`. | `1F-coherence-scan.md:76`, `:117`; `1F-anti-pattern.md:65`; live `crates/core/src/css_types.rs:1` (66 LOC, "Host shims for the CSS L4 grammar"). |
| CH2-V2-006 | ACCEPT | The load-bearing Lock 14 leaks the inventories DO catalogue are live-true. Spot-verified: 7× `b654562ccff46ed62dd48e9ace325830` md5; `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork; `CSS_GENERATED_RS` const courier `:701`; Pattern H 71 total / 67 per-grammar (+4 generic `tape/`); runtime leak 12 sites / 4 files (json/css_l4/google_sheets/bbnf); `parse_w11_1_number` ×7. The spec's `:2217` "30 sites across 15 files" is genuinely STALE within the same 4-dir scope — 1C C3/U2 catches this correctly. | live md5×7; `grammar_provider.rs:40-42`; `runtime_generator.rs:701`; `find … = 71`, `tape/` = 4; `rg … crates/core/src/runtime/ = 12`, 4 files; `rg -c parse_w11_1_number = 7`. |
| CH2-V2-007 | ACCEPT | The V2-cycle owner/receiver leak map demand is satisfied. 1F-anti's grammar-name-leak table has exactly Surface / Live-evidence / Verdict / Receiver columns and consolidates the skinny fork, CSS courier, JSON `_RS` literals, the two grammar-named modules, the `json_templates/` directory, the `ir` strategy table, `css_types.rs`, and the doc-comment grammar names into one compact map. The prior V2 CH2-FOLD-003 / CH2-V2-004 orphan is cleared. | `1F-anti-pattern.md:55-66` (the leak-map table). |
| CH2-V2-008 | ACCEPT | The OnceCell `StructuralIndex` probe asymmetry is correctly classified as a grammar-neutral substrate fact, not a JSON/CSS-only finding. It is emitted into ALL 9 generated grammars in `crates/core`, classed `generated_function` per-parse lifetime (admissible), and skinny `bbnf-simd` is verified empty — so the SK-V19 reconcile burden is grammar-neutral. | `1F-anti-pattern.md:43`; `1F-coherence-scan.md:100` (COH18-015); live `crates/core/.../json.rs:701` + `rg … skinny/crates/bbnf-simd/src = 0`. |
| CH2-V2-009 | REVISE | The `ir` strategy-table grammar-name leak breadth is UNDERCOUNTED — five grammar names pass uncited, breaching the CH2 "no grammar-name leak passes uncited" bar. 1F-anti (`:64`), COH18-005 (`:75`), and COH18-012 (`:82`) all enumerate the table as Json/Sheets/CssL4/Bbnf and stop at `:155`. LIVE `crates/ir/src/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE` carries NINE grammar-named `idents` rows; the uncited five are Csv (`:161`), Math (`:167`), Bnf (`:173`), Ebnf (`:179`), CssPretty (`:185`). The table is CONSUMED by the generator (`for_grammar` → `for_grammar_with_manifest(…, PRODUCTION_MANIFEST_TABLE)` at `:216`), so this is a live 9-grammar Lock 14 (a) leak in the generic `ir` crate, not a 4-grammar one. The narrow 4-name leak regex catching only 5 ident-array sites is exactly why the breadth must be stated explicitly. | CORRECTION: in `1F-anti-pattern.md:64`, `1F-coherence-scan.md:75` (COH18-005), and `1F-coherence-scan.md:82` (COH18-012), record that `PRODUCTION_MANIFEST_TABLE` carries NINE grammar-named `idents` rows (`:137,:143,:149,:155,:161,:167,:173,:179,:185`), of which the strict 4-name leak regex catches only 5 ident sites; the leak is grammar-neutral-fleet-wide (all 9 totality grammars), and the R16 structural row-collapse receiver must collapse all 9. |
| CH2-V2-010 | REVISE | 1F-anti-pattern.md:64 mis-attributes the §9 gate scope, contradicting COH18-012 in the same V5 inventory set. The strategy-table row reads "the ARCH leak scan (`:2215`) is scoped to `codegen/` and MISSES this." FALSE as cited: `ARCHITECTURE.md:2215` scans `crates/{ir,parse,codegen,runtime,path,…}/src/` (its scope list NAMES `ir`) and DOES catch strategy.rs (live `rg … crates/ir/src/` = 11). The narrow command is `ARCHITECTURE.md:1643` (`crates/codegen/src/`-scoped §12 row). COH18-012 (`:82`) states this correctly; the 1F-anti row reverts to the exact error V1 CH2-003 corrected — the fold propagated the fix to 1F-coherence but not to 1F-anti. | CORRECTION (`1F-anti-pattern.md:64`): replace "the ARCH leak scan (`:2215`) is scoped to `codegen/` and MISSES this" with "the §12 Backend-impl-table command (`ARCHITECTURE.md:1643`) is `crates/codegen/src/`-scoped and MISSES this; the §9 gate (`:2215`) and `LOCKS.md:349` scope `crates/{ir,…}/src/` and DO catch it (11 sites)" — aligning the row with COH18-012. |

## Orphan-REVISE Check (prior-V2 CH2 fold roster)

| Prior V2 REVISE / fold | V5 carrier | orphan status |
|---|---|---|
| CH2-V2-003 (D9/D10 grammar-neutral row in 1D) | 1B D8/D10 split is now D6 (crate-layout) + D8 (Sheets); 1D carries grammar-neutral pass-layer leaks as G-6/G-7/G-12 in its grammar-neutral table `1D:194-202`, not metadata-only. | Clear for CH2 — the pass-layer leak is now a grammar-neutral-table row, not a self-report. |
| CH2-V2-004 (compact Lock 14 owner/receiver map) | `1F-anti-pattern.md:55-66` IS the compact Surface/Verdict/Receiver leak map. | Clear (see CH2-V2-007). |
| CH2-FOLD-001 (COH18-012 gate scope) | `1F-coherence-scan.md:82` corrected. | Clear for COH18-012; but the SAME error survives in `1F-anti-pattern.md:64` (CH2-V2-010). |
| CH2-FOLD-002 (grammar-named modules uncited) | `1F-anti-pattern.md:62-63`. | Clear (CH2-V2-002). |
| CH2-FOLD-003 (phantom G-axis scoping) | `1C-runtime-evidence.md:24`. | Clear (CH2-V2-003). |
| CH2-FOLD-004 (css_types.rs Lock-14(c) hedge) | `1F-coherence-scan.md:76,117`; `1F-anti-pattern.md:65`. | Clear (CH2-V2-005). |

Net: the prior V2 CH2 orphans are cleared; the two NEW live REVISE items
(CH2-V2-009 leak undercount, CH2-V2-010 `:2215` misattribution) are both
fold-correctable within the same inventory files. No source, lock, or spec is
changed by this verdict; T-P1 catalogues, T-P3 disposes, Pass Omega merges.

TALLY accept=8 revise=2 reject=0

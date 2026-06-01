---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V3
sk_cycle: SK-V18
disposition: REVISE
head_at_review: 4e4aa0648
working_tree: dirty (8 inventories re-folded 2026-06-01 16:06-16:10, uncommitted)
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH1 Correctness Audit — T-P1 V3 (SK-V18 Totality Excavation, cycle V3)

## Verdict

REVISE. The V3 fold discharged ALL FIVE V2-required corrections — including the
one the cycle's anti-paper-close discipline most cares about: the V2 REJECT
(CH1-V2-F1, the false `regen.rs:17-18` = `entry_rule`/`source_roots` claim) was
NOT re-inherited. `1F-past-corpora:55` now reads "`:17-18` are the
`frontend_requirements`/`output_labels` fields, NOT the recipe; `entry_rule`/
`source_roots` are at `skinny/xtask/src/regen.rs:9-10`," and DISK confirms it
exactly (`:9 entry_rule`, `:10 source_roots`, `:17 frontend_requirements`,
`:18 output_labels`). That is the correct discharge of a false folded claim on
disk evidence, not a copy. The other four V2 folds also landed: 1A frontmatter
now sums to 26 over six auditable buckets (CH1-V2-F3); 1B re-cites
`collapsed_stage.rs` not the non-existent `collapsed_tape.rs` (CH1-V2-F2); 1D
prefixes `skinny/crates/bbnf-bench/src/lock14_baseline.rs` (CH1-V2-F4); the bare
`nonjson_css_l4.rs:3091` is disambiguated to the `src/` copy (CH1-V2-F5).

I spot-verified a wide load-bearing surface at HEAD and EVERY cited
symbol/text/LOC/md5/line resolved: the IR enum spine, the tape substrate
(`Tape::id :170` — the V2 `:170` reversal is disk-true, NOT `:172`), the lowerer
dispatch + the four 17-LOC `collapsed_stage` lowerers, the `RuntimeEmitterKind`
fork, the css_l4 7× md5 `b654562c…` family, the CSS const courier + all JSON/CSS
`_RS` literal lines, the `strategy.rs` 9-row ident table at `:137…:185` + the
`for_grammar_with_manifest` consumer at `:216`, the RESULTS twitter/citm/canada
rows, all 16 lock headers + the Lock-14 `:620` generality-vehicle clause, the
`attach_structural_index` NO-OP, the `parse_w11_1_number ×7`, and the
google-sheets tower in BOTH copies. No whole inventory is rejected; no
recalled/fabricated symbol was found this pass.

The cycle cannot ACCEPT under CH1 because a close reading surfaced a residual
root-resolution defect prior cycles did NOT catch: 1B cites `backend_egraph.rs:9`
and `decision_csp.rs:151/:265` by BARE filename, and those files live ONLY at
`skinny/crates/passes/src/` (there is no `codegen/` copy) — the SAME class V1/V2
flagged for `regen.rs`/`lock14_baseline.rs`/`nonjson_css_l4.rs`/`google-sheets.bbnf`
but missed for these two. Plus one mild anchor imprecision in 1A
(`json_typed_direct.rs:56` is the `DirectParser::new` instantiation, not the
`.cursor` field at `:671`), one un-anchored co-gate name, and one consumer-anchor
gap in 1F-coherence.

## V2 Fold Discharge Audit (did the required V3 folds land?)

| V2 item | required fold | V3 status | disk evidence |
|---|---|---|---|
| F1 (1F-past) REJECT | replace false `:17-18`=`entry_rule`/`source_roots` | DISCHARGED | `1F-past:55` now "`:17-18`=`frontend_requirements`/`output_labels`; `entry_rule`/`source_roots` at `:9-10`". Disk: `regen.rs:9 entry_rule`,`:10 source_roots`,`:17 frontend_requirements`,`:18 output_labels`. The false claim is GONE, replaced by the disk-true one. 1B:108-116 + 1D:148-149 carry the same corrected anchoring. |
| F2 (1B) | `collapsed}_tape.rs`→`collapsed_stage.rs` | DISCHARGED | `1B:59,:82` now "`lower/{eager,offset,event}_tape.rs` + `lower/collapsed_stage.rs`" with the CH1-V2-F2 annotation. Disk: `lower/collapsed_stage.rs` exists (17 LOC); `lower/collapsed_tape.rs` does NOT. |
| F3 (1A) | reconcile frontmatter integers w/ compound labels | DISCHARGED | `1A` frontmatter widened to 6 buckets (implemented 7, unimplemented 8, impl_exceeds 1, unknown 3, partial 5, diverges 2) = 26; disk: exactly 26 `1A-SUB-0` rows in the table. Auditable. |
| F4 (1D) | prefix bare `lock14_baseline.rs` | DISCHARGED | `1D:64,103,197` now `skinny/crates/bbnf-bench/src/lock14_baseline.rs:{2409,2442,2463}` with the CH1-V2-F4 note. Disk: `:2409 GENERIC_SCAN_ROOTS`, `:2442 SKV15_W2_EXTRA_COVERAGE_ROOTS`, `:2463 diagnostic-x86` — file is in `bbnf-bench`, not `codegen`. |
| F5 (1F-past/1E) | disambiguate `nonjson_css_l4.rs:3091` | DISCHARGED | `1F-past:53` + `1E:88` now "`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091`" + "the `benches/` sibling has no `:3091`". Disk: `src/` copy 3737 LOC, `measure_mbps` at `:3091`; `benches/` copy 318 LOC (out of range). |

## Findings

| ID | Disposition | Evidence |
|---|---|---|
| CH1-V3-F1 | ACCEPT | The V2 REJECT (CH1-V2-F1) is correctly discharged on disk evidence, not re-inherited. `skinny/xtask/src/regen.rs` field order verified verbatim: `:5 #[derive(Clone, Copy, Debug)]`, `:6 pub(crate) struct RuntimeTarget`, `:9 entry_rule`, `:10 source_roots`, `:17 frontend_requirements`, `:18 output_labels`. `1F-past:55`, `1B:108-116`, `1D:148-149` now all carry the disk-true anchoring; the false `:17-18`=`entry_rule`/`source_roots` claim is GONE everywhere. |
| CH1-V3-F2 | ACCEPT | 1A IR/tape/cost spine verifies verbatim: `BackendShape` 5 at `ir/src/lib.rs:340-346`; `ExprKind` `:211`; `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>` `:175`, `_kind:PhantomData<fn()->K>:178`, `_grammar:…:179`; `DocumentView` `:227`; `Tape<'input>` `:94`; `Tape::id` `:170` (V2 `:170` reversal disk-confirmed). `cost.rs`: `SubstrateTarget` `:57`, `AdmittedFactOutput` `:61`, `fact_stream()` `:139`, `all_backend_shapes()->[BackendShape;5]` `:334`. Frontmatter 6-bucket tally sums to 26 = the row count (F3 discharged). |
| CH1-V3-F3 | ACCEPT | 1B codegen spine verifies: `select_lowering` 5-arm `lower/mod.rs:18-26`; the four lowerers each EXACTLY 17 LOC (`eager/offset/event_tape.rs` + `collapsed_stage.rs`, `collapsed_tape.rs` absent); `tape_plan.rs:58 render_rule`; `sink_only.rs:122 lower_program`; `rust.rs:32 lower_to_rust`,`:112 validate_policy_facts`; `RuntimeEmitterKind{CompiledLowering,RequestFacts}` `grammar_provider.rs:40-42`,`:33 emitter`,`:110 gate`; dispatch `runtime_generator.rs:16` (`CompiledLowering` arm `:17→emit_from_source` `:24`, `RequestFacts → emit_request_facts` `:25`); `emit_compiled :29` → `json_sink_direct::render(sink_only)` `:37`; render fns `:124/:251/:326/:497` take `&mut String`, `render_header:68`/`render_number_emitter:457` read program; ARCH:1409 carries the stale `:401-408` enum ref (D4 correct); `+400..+1200`/`four real per-shape` grep = 0 in SK-V18 SPEC (D2 CH4-V2-008 absorption accurate). |
| CH1-V3-F4 | ACCEPT | 1C census exact: `crates/core/src/runtime` 67 `@generated` / 71 total (+4 = `tape/`); `css_l4.rs` 108406 LOC / 191 `parse_` fns; C12 per-grammar LOC spot (bbnf 21557, json 3505, math 875) verbatim; 7× css_l4 `generated.rs` md5 `b654562ccff46ed62dd48e9ace325830`; C3 leak 12 sites / 4 `parse_with.rs` (`css_l4/parse_with.rs:4,33,36` carry `__shape_support_CssL4Parser`/`parse_CssL4Parser_stylesheet` verbatim); `find_component_delim` at `:657`; C3/U2 wider-scan cause dropped (F15 discharge holds). `emit_compiled :29-74` → `json_sink_direct::render` `:37`. |
| CH1-V3-F5 | ACCEPT | 1D RESULTS/REDRESS resolve: RESULTS twitter parse_only `8349.290 > 4913.095`, citm `9079.838 > 8335.772`, canada `16709.901 > 12970.929` — "8349>4913 / 9079>8335 / 16709>12970" exact; REDRESS:126 "Tape/direct-to-struct remains one substrate" verbatim; `parse_w11_1_number ×7` at `json/generated.rs:801,841,881,955,1007…`; lock14 `:2409/:2442/:2463` full `bbnf-bench` path; google-sheets tower `:36-51` + `paren_expr :67` (gorgeous copy) AND `:103-121`+`:137` (canonical copy) BOTH resolve. Frontmatter (impl 1 / unimpl 10 / impl_exceeds 3 / unknown 5) maps: 14 table rows (10+1+3) + 5 Open-Questions UNKNOWNs. |
| CH1-V3-F6 | ACCEPT | 1E strongest inventory. 16 lock headers at `LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`; Lock-14 `:620` "The `G:EventGrammar` type parameter is the generality vehicle" verbatim; `:408-409` "returns 67 (the asserted Pattern H total"; `Cargo.toml:81 lto="thin"` vs `skinny/Cargo.toml:80 lto="fat"`; x86=28; core Pattern H=71; `css_l4/builder.rs` 817 LOC + `OpenFrame :16`; 7 LAC candidates each cite a resolving SK-V18 SPEC line (`:1202`,`:1254`,`:113`); the `e12c5323d` Pattern-H stamp is explicitly annotated INHERITED (CH1-V1-F16), every claim re-verifies at `4e4aa0648`. |
| CH1-V3-F7 | ACCEPT | 1F-coherence strategy table verifies: `crates/ir/src/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE :134` with NINE ident rows at EXACTLY `:137,143,149,155,161,167,173,179,185` (the V2 4-row claim correctly EXPANDED to all 9 — disk-true); `for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)` consumer `:216`; `rg JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser crates/ir/src` = 11 (COH18-012 exact); `css_types.rs:1` "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map" + 66 LOC; HANDOFF:17-19 / ARCH:2003-2005 / MASTER:519 drift cites resolve; `simd-scan/src/lib.rs:68 next_structural_at_or_after`; `support.rs:67` "The probe substrate (OnceCell + helper)". |
| CH1-V3-F8 | ACCEPT | 1F-anti + 1F-past LOC/symbol claims verify: report 11863 / gate 6175 / lock14 5095 / generated_real_typed 4941 / nonjson 3737 / grammar-lib 2052 / passes-lib 2025 / runtime_generator 1611 / codegen-lib 1473 / json/generated 1235; JSON/CSS `_RS` literals at `runtime_generator.rs:195/550/572/594/598/612/665/701`; `codegen/src/lib.rs:4 mod json_sink_direct;:5 mod json_typed_direct;`; `strategy.rs:216` consumer; `grammar_facts.rs:799 BbnfBootstrap::parse` comment; `json.rs:701 OnceCell<StructuralIndex>`; 1F-past nonjson disambiguation (`src/` 3737/`benches/` 318) correct. |
| CH1-V3-F9 | REVISE (1B) | `1B:61` (D6 table row) and `1B:120` (D6 body) cite `backend_egraph.rs:9` by BARE filename; `1B:152` (U2) cites `decision_csp.rs:151` bare and U2 also references `:265`. The CONTENT resolves exactly — `skinny/crates/passes/src/backend_egraph.rs:9 const REWRITE_SET = "sk-v15-w7-direct-sink-normalization-v1"`; `decision_csp.rs:151 selected_rule_count: u32::from(csp_status == "sat")`, `:265 assert_eq!(csp.csp_status, "sat")` — but the bare path does NOT resolve from repo root, and BOTH files live ONLY at `skinny/crates/passes/src/` (no `codegen/` copy exists). Same root-resolution class V1/V2 flagged for regen/lock14/nonjson/google-sheets. Note the INTERNAL inconsistency: `1B:49` correctly writes `passes/src/backend_egraph.rs:36` and `decision_csp.rs:16`, so the inventory already knows the home dir. CORRECTION: prefix `1B:61,:120,:152` with `skinny/crates/passes/src/` (or `passes/src/` matching `:49`). |
| CH1-V3-F10 | REVISE (1A) | `1A-DIV-006` (`:109`) and `1A-UNK-001` (`:186`) cite the third cursor carrier as "codegen `DirectParser.cursor` with its own `checkpoint = parser.cursor` rollback (`skinny/crates/codegen/src/json_typed_direct.rs:56,:361`)". Disk: `:361 out.push_str("    let checkpoint = parser.cursor;\n")` is EXACT for the rollback; but `:56` is `let mut parser = DirectParser::new(input);` (the instantiation), NOT the `.cursor` field — the `cursor: usize` field is declared at `:671` (inside `const PARSER_RUNTIME` `struct DirectParser<'i>` `:668`). The symbol is real and the `:361` rollback is correctly anchored; `:56` merely points at where DirectParser enters rather than the field/struct. CORRECTION: re-anchor `:56`→`:671` (field) or `:668` (struct), keeping `:361` for the rollback. |
| CH1-V3-F11 | REVISE (1F-coherence) | `1F-coherence COH18-005` (`:75`) and `COH18-012` (`:82`) cite the full 9-row strategy ident table — disk confirms all 9 rows — and rest the "relocated-seam analog" / "leak feeds the template" verdict on whether the names are LIVE-consumed by the generator, yet neither row carries the `strategy.rs:216` consumer anchor (only 1F-anti `:64` carries `for_grammar_with_manifest(…, PRODUCTION_MANIFEST_TABLE)` `:216`). The Open Question `U-COH18-001` even flags "whether `PRODUCTION_MANIFEST_TABLE` is consumed by the generator or only by `regen --check`" as UNKNOWN — so the same row's "feeds the template" verdict-clause overstates relative to its cited evidence. CORRECTION: add the `crates/ir/src/registry/strategy.rs:216` consumer anchor to COH18-005/012, or route the "consumed-by-generator" clause explicitly to U-COH18-001 so the verdict rests on the cited consumer, not an implied one. |
| CH1-V3-F12 | REVISE (1B) | `1B` D5 (`:108-116`) + table row `:62` name the SK-V18 R16 co-gate `runtime_target_rows_collapsed` (also `1E` D-1E-V5-10 `:114` / LAC-1E-V5-02 `:147`, `1D` D-3 `:92`). The `regen.rs:5` derive pin is disk-exact and the "+1-line PartialEq not yet present" claim is true (no `PartialEq` in the `:5` derive). But the co-gate NAME `runtime_target_rows_collapsed` is cited WITHOUT a path:line; `rg runtime_target_rows_collapsed skinny/crates skinny/xtask` finds no current definition — it is a SK-V18-SPEC-planned gate, not a live symbol. The text reads as if naming an existing co-gate. CORRECTION: anchor `runtime_target_rows_collapsed` to its SK-V18 SPEC line and mark it PLANNED (not live), so the name is not read as a current-code citation. |
| CH1-V3-F13 | ACCEPT | Provenance + stamps clean. `git rev-parse HEAD` = `4e4aa064835b0bf8f7e25113edb40f3a9e01b866`; the 8 inventories are uncommitted (M) with mtimes 2026-06-01 16:06-16:10 (the V3 re-fold, AFTER the V2 review's 15:50-15:52). 1E carries `4e4aa0648` ×2 + the explicitly-annotated inherited `e12c5323d` (CH1-V1-F16); 1F-past carries the annotated inherited `83b66db42` S-P0 anchor. No stamp is falsified; every spot-checked claim verifies at the actual HEAD. |

## Evidence Checked (spot-verifications run this pass, all at HEAD 4e4aa0648, dirty tree)

- IR/cost: `skinny/crates/ir/src/lib.rs:175,178,179,211,227,340-346`; `cost.rs:57,61,70,139,141,334`.
- Tape: `skinny/crates/runtime/src/tape/mod.rs:94,170,175,178,179,227`.
- Codegen lower: `lower/mod.rs:18,26`; `lower/{eager,offset,event}_tape.rs`+`collapsed_stage.rs` (17 LOC each, `collapsed_tape.rs` ABSENT); `tape_plan.rs:58`; `sink_only.rs:122`; `rust.rs:32,112`.
- Codegen passes: `skinny/crates/passes/src/lib.rs:329,392,401,473`; `passes/src/backend_egraph.rs:9,36`; `passes/src/decision_csp.rs:16,151,265` (home dir = `passes/src/`, NOT `codegen/`).
- Codegen emit: `grammar_provider.rs:33,40-42,110`; `runtime_generator.rs:16,17,24,25,29,37,91,195,550,572,594,598,612,665,701`; `json_sink_direct.rs:4,68,124,251,326,457,497`; `json_typed_direct.rs:56,59,361,668,671`; `codegen/src/lib.rs:4,5,107,178`.
- xtask: `skinny/xtask/src/regen.rs:5,6,9,10,17,18`.
- Runtime/JSON: `grammars/json/generated.rs:12-15` (`attach_structural_index` NO-OP),`:801,841,881,955,1007` (`parse_w11_1_number ×7`).
- Runtime census: `crates/core/src/runtime` 67 `@generated`/71 total; 7× css_l4 `generated.rs` md5; `css_l4_declaration_values/generated.rs:657 find_component_delim`; `runtime/css_l4/parse_with.rs:4,33,36`; leak 12/4.
- Grammar generated: `crates/core/src/grammar/generated/{bbnf 21557,json 3505,math 875}.rs`; `css_l4.rs` 108406/191.
- ir registry: `crates/ir/src/registry/strategy.rs:134,137,143,149,155,161,167,173,179,185,216`; `rg …idents crates/ir/src`=11.
- Locks/spec: `LOCKS.md:75,349,408-409,453,620`; `ARCHITECTURE.md:1409`; `restart/skinny/tranches/sk-v18/SPEC.md` (1662 LOC) `:113,1202,1254`.
- 1D ledgers: `skinny/RESULTS.md:5-25` (twitter/citm/canada parse_only); `skinny/REDRESS.md:126`; `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf:36,51,67` vs `grammar/google-sheets/google-sheets.bbnf:103,121,137`.
- 1F totality: `crates/core/src/css_types.rs:1` (66 LOC); `crates/simd-scan/src/lib.rs:68`; `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`; lock14 `:2409,2420,2442,2455,2463`.
- nonjson disambig: `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` 3737/`:3091 measure_mbps` vs `benches/nonjson_css_l4.rs` 318.
- Provenance: `git rev-parse HEAD`=4e4aa064835b0bf8f7e25113edb40f3a9e01b866; inventories M, mtimes 16:06-16:10.

## Artifact Dispositions

| Artifact | CH1 V3 disposition | Notes |
|---|---|---|
| `1A-substrate-evidence.md` | REVISE | IR/tape/cost rows + frontmatter 26-sum resolve; F3 discharged; `Tape::id :170` correct; `json_typed_direct.rs:56` anchors the DirectParser instantiation, not the `.cursor` field (`:671`) (F10). |
| `1B-codegen-evidence.md` | REVISE | Lowerer/fork/render rows verify; F2 discharged (`collapsed_stage.rs`); D4 stale-line + D2 grep-0 accurate; bare `backend_egraph.rs:9`/`decision_csp.rs:151,:265` unresolved from root while `:49` correctly paths `passes/src/` (F9); `runtime_target_rows_collapsed` co-gate name un-anchored / plan-only (F12). |
| `1C-runtime-evidence.md` | ACCEPT | Census/md5/LOC/leak/`emit_compiled→render` all exact; C3 wider-scan cause stays dropped; cleanest core inventory. |
| `1D-skinny-lessons.md` | ACCEPT | RESULTS/REDRESS + `parse_w11_1_number ×7` + full `bbnf-bench` lock14 path + both google-sheets copies resolve; F4 discharged; frontmatter maps to table+Open-Questions. |
| `1E-locks-evidence.md` | ACCEPT | 16-lock + every impl claim verifies; F5 (nonjson) discharged; 7 LAC candidates each cite a resolving SPEC line; inherited `e12c5323d` annotated. (Shares the F12 planned-co-gate naming residual, dispositioned on 1B.) |
| `1F-coherence-scan.md` | REVISE | 9-row strategy table + 11-site rg + css_types + drift cites all resolve; COH18-005/012 rest the "feeds the template / relocated-seam" verdict on consumption without the `strategy.rs:216` consumer anchor that only 1F-anti carries, while U-COH18-001 flags consumption as UNKNOWN (F11). |
| `1F-anti-pattern.md` | ACCEPT | Every LOC/`_RS` literal/`strategy.rs:216` consumer/OnceCell/support.rs cite verifies; nonjson + 9-row table correct. |
| `1F-past-corpora.md` | ACCEPT | The V2 REJECT (regen `:17-18`) is correctly discharged disk-true (F1); nonjson disambiguation correct; inherited `83b66db42` annotated. |

## Required V4 Fold

1. 1B: prefix `backend_egraph.rs:9` (`:61,:120`) and `decision_csp.rs:151,:265` (`:152`) with `skinny/crates/passes/src/` (matching the correctly-pathed `1B:49`).
2. 1A: re-anchor `1A-DIV-006`/`1A-UNK-001` `json_typed_direct.rs:56`→`:671` (the `DirectParser.cursor` field) or `:668` (struct), keeping `:361` for the `checkpoint = parser.cursor` rollback.
3. 1F-coherence: add the `crates/ir/src/registry/strategy.rs:216` consumer anchor to COH18-005/COH18-012 (or explicitly route the "consumed-by-generator" claim to U-COH18-001), so the relocated-seam verdict rests on the cited consumer.
4. 1B (+ sibling 1E D-1E-V5-10/LAC-1E-V5-02, 1D D-3): anchor `runtime_target_rows_collapsed` to its SK-V18 SPEC line and mark it a PLANNED co-gate, not a live symbol (`rg` returns no current definition).

TALLY accept=9 revise=4 reject=0

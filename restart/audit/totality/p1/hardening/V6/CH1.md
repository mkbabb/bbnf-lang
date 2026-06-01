---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation (SK-V18 totality)
cycle: V6
disposition: ACCEPT
verification_head: dirty working tree at master (post 3ac131c45)
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

# CH1 — CORRECTNESS (SK-V18 T-P1 V6)

## Verdict

ACCEPT. Zero REVISE, zero REJECT.

My lens is citation correctness: every spec-claim<->impl row must resolve — the
spec path:line carries the claimed text, the impl path:line carries the claimed
symbol, the verdict matches the evidence; 1D RESULTS/REDRESS citations resolve to
real entries; no recalled LOC/symbol. I read all six live inventories plus both
1F auxiliaries end-to-end and spot-verified the most load-bearing cited rows in
each against the V1 spec (ARCHITECTURE.md, MASTER-PLAN.md, LOCKS.md), the SK-V18
plan (sk-v18/SPEC.md), and the live code in both trees (skinny/crates,
crates/core). Every figure I checked matches disk. The inventories are sound.

Per the REJECT convention: a reject is admissible ONLY when an inventory STATES
SOMETHING FALSE ON DISK with a live falsifying path:line. I found NO such case —
every claim I tested is CORRECT on disk. A self-falsified suspicion that resolves
in the inventory's favour is an ACCEPT. The honest tally is therefore reject=0.

## Load-Bearing Rows Verified On Disk

### 1A (substrate)
- `tape/mod.rs:94` = `pub struct Tape<'input>`; `:170` = `pub fn id(&self) -> TapeId`
  (the inventory's explicit correction of the prior `:172` re-cite is CORRECT —
  `id` IS at `:170`); `:175` = `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>`;
  `:178/:179` = the two `PhantomData` fields; `:191` = the impl block. All verbatim.
- `ir/src/lib.rs:340-345` = the five `BackendShape` variants; `cost.rs:57` =
  `pub enum SubstrateTarget`. Both verbatim.
- `css_l4_declaration_values/generated.rs:257` = "Holds exactly the existing `Tape`
  — no second substrate." Verbatim.
- Spec/plan side: `LOCKS.md:620` carries "The `G:EventGrammar` type parameter is
  the generality vehicle"; `sk-v18/SPEC.md:1202-1207` carries the `<G>` DELETE
  (G4a). The 1A-SUB-025 / 1A-LOCK1-AMEND-001 contradiction is REAL on disk.

### 1B (codegen)
- `ir/src/lib.rs:340-346` enum; `passes/src/lib.rs:329 mod recognizers`, `:392 fn
  derive_backend_shape`, `:473 fn choose_backend_shape`; `codegen/src/lower/mod.rs:18-26`
  (`match cost.chosen` over five shapes); `grammar_provider.rs:40-42` RuntimeEmitterKind;
  `tape_plan.rs:58` render_rule (returns `String`); `sink_only.rs:122 lower_program`.
  All verbatim.
- The four scaffold lowerers (`eager/offset/event_tape.rs` + `collapsed_stage.rs`)
  are each 17 LOC (matches "17-LOC scaffolds"); `collapsed_tape.rs` does NOT exist
  (the CH1-V2-F2 correction holds); `lower/mod.rs:1` declares `pub mod collapsed_stage;`.
- D1 grep claim verified: `rg RuntimeEmitterKind restart/ARCHITECTURE.md` returns
  zero (the "ARCH mentions it ZERO times" claim is true).
- D4 stale-spec claim verified: `ARCH:1409` cites `lib.rs:401-408` as the enum
  owner while the enum is at `:340-346` — the spec IS stale, exactly as stated.
- `backend_egraph.rs:9 REWRITE_SET` verbatim; `xtask/src/regen.rs:5-6` derives
  `Clone, Copy, Debug` (no PartialEq) over `pub(crate) struct RuntimeTarget` — D5 correct.

### 1C (runtime)
- 7× `css_l4_*/generated.rs` md5 = `b654562ccff46ed62dd48e9ace325830` (verified all 7).
- `runtime_generator.rs:701` = `const CSS_GENERATED_RS: &str = r#"`, `:1611` = `"#;`.
- `crates/core/src/css_types.rs:1` = host-shim header, 66 LOC.
- 67 `@generated` markers in `crates/core/src/runtime` (D4 stale-marker: `ARCH:1923-1932`
  asserts 0/9, live is 67 — the divergence is real).
- `ARCH:1961` SoA-tape claim; `LOCKS.md:349` names `crates/core/src/css_types.rs`
  verbatim in the failure-mode list (D9). Both verbatim.

### 1D (skinny lessons) — RESULTS/REDRESS resolution (lens-critical)
- `RESULTS.md:5-25` cold Track-1 rows confirm verbatim: twitter parse_only
  8349.290 > sonic 4913.095; citm 9079.838 > 8335.772; canada 16709.901 >
  12970.929; per-iter equality PASS. These are real measured rows, not recalled.
- `REDRESS.md:742` = item 51 (REJECT event-cursor); `:769` = item 52 (SK-V5
  baseline reassay — NOT a reject); `:784` = item 53 (REJECT structural-mask
  cursor); `:6446` = "SK-V15 W11 Close Reconciliation Admit" (ledger end). The
  fine-grained CH3 span distinctions (51=742-768, 52=769-783, 53=784-813) all
  resolve correctly — the inventory does NOT conflate item 52 into the reject span.
- `REDRESS.md:6326` (W7 Decision Engine Spine Admit), `:6356` (W8 lowerer Admit),
  `:6416` (W10 FNV Quarantine Admit), `:126` (one-substrate clause). All verbatim.
- Live witnesses: `find_component_delim` at css `generated.rs:657`;
  `parse_w11_1_number_*` at `json/generated.rs:801,841,881` (7 occurrences);
  `lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS` / `:2442 SKV15_W2_EXTRA_COVERAGE_ROOTS`
  / `:2463 diagnostic-x86`; `tape/mod.rs:227 DocumentView`; `css_cold_harness.rs:131
  fn track1_full` (`:130` is the comment — the CH6-V4-005 anchor is correct).
- The dual x86 figure (24 vs 28 files) is INTERNALLY CONSISTENT, not a defect:
  table row scopes `src/x86_64/` alone (24 files), D-4 scopes the P1 delete budget
  `src/x86_64/` + `ext/x86/` (24 + 4 = 28 files, 4401 LOC). I computed both
  on disk: 28 files / 4401 LOC exactly. `byte_class_from_eq_set_64.{rs,asm}` exist.

### 1E (locks) — sharpest falsification claims
- 16-lock headings resolve to EXACTLY the cited lines: 75, 160, 170, 179, 181,
  183, 200, 202, 260, 269, 319, 328, 336, 349, 436, 453.
- L14 gate-RED falsification (D-1E-V5-14) verified true on disk:
  `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/
  crates/analysis/src/` = 13 sites (11 ir + 2 analysis) vs the LOCKS:349-asserted
  ZERO. The lock's OWN verification gate IS falsified.
- Pattern-H drift (D-1E-V5-06): `find crates/core/src/runtime -mindepth 2 -type f
  -name '*.rs' | wc -l` = 71 vs the LOCKS:408-409-asserted 67. Real drift.
- `strategy.rs:137` idents table; `bbnf-simd/src/lib.rs:5 pub mod x86_64`;
  OnceCell probe at `generated/json.rs:701/:719/:732` with `math.rs` carrying 0
  `ensure_structural_index` (the CH5-V3-003 8/9 breadth correction is CORRECT).
  All verbatim.

### 1F (coherence + auxiliaries)
- The 9 `idents` rows in `strategy.rs` resolve to EXACTLY the cited lines
  137/143/149/155/161/167/173/179/185; consumer `for_grammar_with_manifest(...,
  PRODUCTION_MANIFEST_TABLE)` at `:216`.
- COH18-001 scope drift verified: `HANDOFF.md:17-19` says SK-V18 "adopts ... into
  the totality `crates/core/` tree"; `sk-v18/SPEC.md:19-21` says SK-V18 is the
  GENERALIZATION cycle on the skinny tree. The drift is real.
- `MASTER-PLAN.md:519` (F.W5 nine seed grammars), `:923` (refuted CSS L4 admission);
  `simd-scan/src/lib.rs:68` exports `{StructuralIndex, next_structural_at_or_after}`;
  `analysis/src/state/ast_utils/mod.rs:4` (`BbnfBootstrapNodeView`) / `:11`
  (`BbnfBootstrapRuleKind`) doc-comments. All verbatim.
- 1F-anti-pattern god-file LOC: report.rs 11863, gate.rs 6175, lock14_baseline.rs
  5095, runtime_generator.rs 1611, codegen/lib.rs 1473 — all exact. `codegen/lib.rs:4-5`
  = `mod json_sink_direct;` / `mod json_typed_direct;`; `json_templates/` full
  roster present; `parity_hash` at `bbnf-simd/src/lib.rs:94` returns `[u8; 32]`;
  the 7 JSON/CSS `_RS` literals at `:195,550,572,594,598,612,665` all match.
- 1F-past-corpora: `nonjson_css_l4.rs` ambiguity is real (src/ 3737 LOC vs benches/
  318 LOC; `measure_mbps` at `:3091` resolves only against src/ — CH1-V2-F5 correct);
  `regen.rs:9-10` = entry_rule/source_roots, `:17-18` = frontend_requirements/
  output_labels (the prior false `:17-18`=entry_rule claim was correctly repaired);
  `SPEC.md:184` 51/51 guard; sk-v17 COH17-004 carry resolves (UNKNOWN-2D-05).

## Mechanical CH1 Sweeps

```sh
rg -n -o '`[^`]*(?:\.md|\.rs|\.toml):[0-9][^`]*`' <8 inventories> \
  | rg -v ':`(restart|skinny|crates|xtask)/|:`Cargo\.toml:|:`skinny/Cargo\.toml:'
rg -n -o '`:[0-9][0-9]*(?:-[0-9][0-9]*)?`' restart/audit/totality/p1/1*.md
rg -n '`[^`]*\{[^`]*\}[^`]*\.(rs|md|toml):[0-9][^`]*`' restart/audit/totality/p1/1*.md
rg -n 'Cycle is V3|this V3 inventory|cycle: V3' restart/audit/totality/p1/1*.md
```

The brace-path and stale-V3 sweeps return ZERO. The "shorthand" (e.g. 1B
`ir/src/lib.rs:340-346`) and "colon-only" (e.g. 1F-past-corpora `:665`, `:3091`,
`:17-18`) sweeps return hits, but these are NOT defects — they are the established
abbreviation convention the five prior cycles accepted: each abbreviated path is a
second-anchor re-cite within a row/sentence whose FIRST mention carries the full
root-relative path (R2 names `runtime_generator.rs:195`…`:665`; R16 names
`skinny/xtask/src/regen.rs:5` then `regen.rs:17-18`; COH18-003 names `LOCKS.md:349`
then `:620`). I confirmed every abbreviated path resolves unambiguously to a live
file (`skinny/crates/ir/src/lib.rs`, `passes/src/lib.rs`, `codegen/src/lower/mod.rs`,
`backend_egraph.rs:36 fn select`, `decision_csp.rs:16 fn finalize_rule`, etc.).
None is an orphan citation. This matches the V5 CH1 disposition exactly.

## Findings

| id | disposition | severity | finding | evidence |
|---|---|---|---|---|
| CH1-V6-001 | ACCEPT | none | Every 1A load-bearing spec-claim<->impl row resolves: spec text and impl symbol both verbatim at the cited path:line; the `:170` `id` re-anchor (correcting the prior `:172`) is correct on disk. | `tape/mod.rs:94,:170,:175,:178,:179,:191`; `ir/src/lib.rs:340-345`; `cost.rs:57`; `LOCKS.md:620`; `sk-v18/SPEC.md:1202-1207`. |
| CH1-V6-002 | ACCEPT | none | Every 1B codegen row resolves; the D1 ARCH grep-zero and D4 ARCH:1409 stale-line claims are both true on disk; `collapsed_tape.rs` non-existence and the 17-LOC scaffold counts hold. | `passes/src/lib.rs:329,:392,:473`; `lower/mod.rs:18-26`; `rg RuntimeEmitterKind ARCHITECTURE.md`=0; `ARCH:1409`=`lib.rs:401-408`; lowerer wc -l=17×4. |
| CH1-V6-003 | ACCEPT | none | Every 1C runtime row resolves; the 7× md5 identity, CSS_GENERATED_RS span, 67 @generated markers, and css_types.rs LOC all verify. | 7× md5 `b654562c...`; `runtime_generator.rs:701-1611`; 67 `@generated` in `crates/core/src/runtime`; `css_types.rs` 66 LOC; `LOCKS.md:349` names it verbatim. |
| CH1-V6-004 | ACCEPT | none | All 1D RESULTS/REDRESS citations resolve to real entries (no recalled rows); the JSON >sonic numbers, the item-51/52/53 reject-span distinction, and the live witnesses all verify. | `RESULTS.md:5-25` (twitter 8349>4913, citm 9079>8335, canada 16709>12970); `REDRESS.md:742,769,784,6326,6356,6416,6446`; `find_component_delim:657`; `parse_w11_1_number ×7`. |
| CH1-V6-005 | ACCEPT | none | The 1D dual x86 figure (24 table / 28 D-4) is internally consistent: 24 = src/x86_64 alone, 28 = src/x86_64 (24) + ext/x86 (4), 4401 LOC = P1 delete budget. Both computed exact on disk. | `find src/x86_64 -type f`=24; `find src/x86_64 ext/x86 -type f`=28; combined wc -l=4401. |
| CH1-V6-006 | ACCEPT | none | 1E's sharpest falsifications are TRUE on disk: the L14 gate is RED (13 sites vs LOCKS:349-asserted ZERO) and Pattern-H drifted 67->71; the 16-lock headings resolve to the exact cited lines. | `rg ... crates/ir/src/ crates/analysis/src/`=13 (11+2); `find ... runtime`=71 vs `LOCKS:408-409`=67; 16 headings at 75..453. |
| CH1-V6-007 | ACCEPT | none | Every 1F coherence + auxiliary row resolves: the 9 idents rows at exact lines, the HANDOFF/SPEC scope drift, simd-scan exports, OnceCell 8/9 breadth, god-file LOC, and the nonjson_css_l4 ambiguity all verify. | `strategy.rs:137..185,:216`; `HANDOFF.md:17-19` vs `SPEC.md:19-21`; `simd-scan/lib.rs:68`; `generated/json.rs:701/719/732`+`math.rs`=0; report.rs 11863 / gate.rs 6175; `src/nonjson_css_l4.rs:3091`. |
| CH1-V6-008 | ACCEPT | none | The abbreviated-path and colon-only re-cites flagged by the mechanical sweeps are the established second-anchor convention, not orphan citations; every one resolves to a live file whose full path is named earlier in the same row. Brace-path and stale-V3 sweeps return zero. | 1B abbreviations all resolve (ir/src/lib.rs, passes/src/lib.rs, lower/mod.rs, backend_egraph.rs:36, decision_csp.rs:16); 1F re-cites carry full path on same line. |

TALLY accept=8 revise=0 reject=0

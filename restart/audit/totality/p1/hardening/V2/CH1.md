---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V2
sk_cycle: SK-V18
disposition: REVISE
head_at_review: 4e4aa0648
working_tree: dirty (8 inventories folded 2026-06-01 15:50-15:52, uncommitted)
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

# CH1 Correctness Audit — T-P1 V2 (SK-V18 Totality Excavation, cycle V2)

## Verdict

REVISE. The V2 fold discharged the bulk of V1 CH1's required corrections, and on
close re-reading it did something the cycle's anti-paper-close discipline
demands: it CAUGHT a false V1 fold instruction and reversed it on disk evidence
rather than copying it. V1 CH1-V1-F14 ordered `1A:170 Tape::id` → `:172`; the V2
1A inventory instead re-anchored to `:170` with an explicit "the prior cycle's
`:172` re-cite is contradicted by disk" annotation (`1A:122-124`). Disk confirms
`pub fn id(&self) -> TapeId` is at line **170**, not 172 — so the V1 fold
instruction was the error and the V2 fold is correct. That outcome is right, but
the SAME V1 cycle carried a SECOND false content-claim that the V2 fold did NOT
catch and instead propagated verbatim: V1 CH1-V1-F13 (and now `1F-past-corpora:55`)
asserts `regen.rs:17-18` are the `entry_rule`/`source_roots` fields; disk shows
those fields are at `:9-10`, and `:17-18` are `frontend_requirements`/
`output_labels`. That is a recalled/false cited claim embedded in a folded
inventory — a REJECT under this lens. Beyond it, three smaller path-resolution
defects remain (a brace-shorthand naming a non-existent file, a bare
`lock14_baseline.rs` whose home is `bbnf-bench` not `codegen`, and a 1A
frontmatter tally that does not sum to its own table). Every load-bearing
spec↔impl row I spot-verified (IR enum spine, tape, lowerer dispatch, the emitter
fork, the css_l4 md5 family, the CSS const courier, RESULTS/REDRESS, all 16 lock
headers, the strategy.rs ident table, the OnceCell probe, google_sheets=10)
resolves at HEAD with high fidelity. No whole inventory is rejected.

## V1 Fold Discharge Audit (did the required V2 folds land?)

| V1 item | required fold | V2 status | evidence |
|---|---|---|---|
| F10 (1D) | `lock14_baseline.rs:2456`→`:2463` (3 sites) | DISCHARGED | `1D:64,104,197` now cite `:2463`; disk `:2463`=`("crates/bbnf-simd/src/x86_64","diagnostic-x86")`, `:2456`=`strict-checkasm-admitted`. |
| F11 (1D) | re-path bare `google-sheets.bbnf` | DISCHARGED | `1D:65,218` cite `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf:36-51,:67` AND cross-cite canonical `:103-121,:137`. Disk: gorgeous copy carries the 7-level tower at `:36-51`, `paren_expr` at `:67`; canonical at `:103-121`,`:137`. Both resolve. |
| F12 (1E) | drop `lower/` from `json_sink_direct.rs` frontmatter | DISCHARGED | `1E:21` now `skinny/crates/codegen/src/json_sink_direct.rs`. Disk: file exists ONLY at `codegen/src/`, NOT under `lower/`. |
| F13 (1B/1D/1F-past) | expand bare `regen.rs:5`; re-anchor/drop loose `:17-18` | PARTIALLY DISCHARGED | `regen.rs:5` expanded to `skinny/xtask/src/regen.rs:5` everywhere — correct. BUT the `:17-18` re-anchor in `1F-past:55` asserts WRONG field names (CH1-V2-F1 REJECT). |
| F14 (1A) | `Tape::id :170`→`:172` | CORRECTLY REVERSED | `1A:122-124` re-anchors to `:170` (disk-true) and flags the V1 `:172` as contradicted. V1 instruction was the error. |
| F15 (1C) | drop "(wider crates/ scan)" cause from C3 | DISCHARGED | `1C:32` C3 now "DIVERGES (count stale within scope)" + "NOT a wider-scan difference (the asserted cause is dropped; see U2)"; U2 reframed `1C:80`. |
| F16 (1E/1F-past) | re-stamp HEAD to `4e4aa0648` | DISCHARGED | `1E:29` and `1F-past:39-41` carry `4e4aa0648` and annotate the inherited `e12c5323d`/`83b66db42` stamps with `CH1-V1-F16`. |

## Findings

| ID | Disposition | Evidence |
|---|---|---|
| CH1-V2-F1 | REJECT (1F-past-corpora) | `1F-past-corpora.md:55` asserts "Bare `regen.rs:17-18` re-anchored: `:17-18` are the `entry_rule`/`source_roots` fields, NOT the recipe." FALSE on disk. `skinny/xtask/src/regen.rs` field order: `grammar_name:7`, `profile:8`, `entry_rule:9`, `source_roots:10`, … `frontend_requirements:17`, `output_labels:18`. So `:17-18`=`frontend_requirements`/`output_labels`; `entry_rule`/`source_roots`=`:9-10`. The wrong attribution was inherited verbatim from V1 CH1-V1-F13 (identical false claim) and folded rather than verified. The load-bearing `:5` derive pin is correct; only the `:17-18` field-content claim is false. CORRECTION: replace with "`:17-18` are `frontend_requirements`/`output_labels` (`entry_rule`/`source_roots` are at `:9-10`)", or drop the `:17-18` re-anchor. |
| CH1-V2-F2 | REVISE (1B) | `1B:59` and `:82` cite the four marker-string lowerers as `lower/{eager,offset,event,collapsed}_tape.rs`. The brace member `collapsed` expands to `collapsed_tape.rs`, which DOES NOT EXIST — the file is `lower/collapsed_stage.rs` (the SAME row's table entry at `1B:56` correctly cites `lower/collapsed_stage.rs:16`, and `lower/mod.rs:1` declares `pub mod collapsed_stage;`). The lowerer is real (17 LOC, `:16` → `tape_plan::render_rule(rule, TapeFlavor::Collapsed)`); only the brace path for `collapsed` is wrong. V1 CH1-V1-F3 made the identical `collapsed_tape.rs` mis-name and ACCEPTED it — a propagated error. CORRECTION: write `lower/{eager,offset,event}_tape.rs + lower/collapsed_stage.rs`. |
| CH1-V2-F3 | REVISE (1A) | `1A` frontmatter `divergence_count` (`implemented:7, unimplemented:9, impl_exceeds_spec:2, unknown:5`) does not sum to the table cells. Enumerated verdicts (SUB-001..026): implemented 6 (+1 "implemented (substrate-neutral confirmed)" = 7 if folded), pure unimplemented 7 (+1 "impl_confirms_plan/unimplemented" = 8, not 9), impl_exceeds_spec 2, pure unknown 3 (not 5), plus 3 "partial/…", 1 "partial/gap-routed", 2 "DIVERGES" compound labels the integers must absorb. The compound labels (`partial / UNKNOWN routed`, `DIVERGES (1E amendment candidate)`) do not map 1:1 onto the four-integer taxonomy, so the counts are not auditable — the live residue of V1's "count 1A-SUB-022" note. CORRECTION: canonicalize each row to one of four taxonomy words, or widen the frontmatter schema to carry the compound buckets. |
| CH1-V2-F4 | REVISE (1D) | `1D:64,103,197` cite `lock14_baseline.rs:2409/2442/2463` by BARE filename. The line numbers resolve correctly (`:2409 GENERIC_SCAN_ROOTS`, `:2442 SKV15_W2_EXTRA_COVERAGE_ROOTS`, `:2455 SKV15_W2_PRIMITIVE_CLASS_ROOTS`, `:2463 diagnostic-x86`), but the bare path does not resolve from root — the file is `skinny/crates/bbnf-bench/src/lock14_baseline.rs` (NOT `codegen/`). 1E (`:108,109`) and 1F-anti (`:23`) already give the full `bbnf-bench` path; 1D is the lone bare citer. Same root-resolution class V1 flagged for `regen.rs`/`google-sheets.bbnf` but missed for lock14. CORRECTION: prefix the three 1D citations with `skinny/crates/bbnf-bench/src/`. |
| CH1-V2-F5 | REVISE (1F-past-corpora) | `1F-past:53` cites `nonjson_css_l4.rs:3091` bare with "(3737 LOC)". TWO files carry the name: `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` (3737 LOC; `:3091`=`fn measure_mbps`) and `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs` (318 LOC, no `:3091`). The 3737-LOC claim and `:3091` resolve ONLY against the `src/` copy; the bare path is ambiguous and `:3091` is out-of-range for the `benches/` sibling. 1E (`:88`) and 1F-anti (`:25`) carry the same bare form. CORRECTION: cite `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091`. |
| CH1-V2-F6 | ACCEPT | 1A IR/tape/cost spine verifies verbatim: `BackendShape` 5 at `ir/src/lib.rs:340-346`; `BackendExpr` 13 (Entry…Return) at `:355-392`, `Recognizer::SimdScan` separate; `ExprKind` 8 at `:211-237`; `SubstrateTarget` `cost.rs:57`; `fact_stream()` `:139`; `all_backend_shapes()->[BackendShape;5]` `:334`. `Tape<'input>` `mod.rs:94/98`; `ValueRef<…K=AnyKind,G:EventGrammar=AnyGrammar>` `:175`, `_kind:PhantomData<fn()->K>:178`, `_grammar:…:179`; `DocumentView` `:227`; `Tape::id` `:170` (F14 reversal correct). `parse_direct<'i,S:JsonSink>` json/generated.rs:760; css_l4 generated.rs:257 "no second substrate"; JsonDocument json/view.rs:63. |
| CH1-V2-F7 | ACCEPT | 1B codegen spine verifies: `select_lowering` 5-arm `lower/mod.rs:18-26`; `derive_backend_shape` `passes/src/lib.rs:392`, `mod recognizers` `:329`, `choose_backend_shape` `:473`; `render_rule` `tape_plan.rs:58`; `lower_program` `sink_only.rs:122`; `lower_to_rust` `rust.rs:32`, `validate_policy_facts` `:112`; lowerers each 17 LOC; `REWRITE_SET="sk-v15-w7-direct-sink-normalization-v1"` `backend_egraph.rs:9`; render fns `json_sink_direct.rs:124/251/326/497` take `&mut String`; `render`:4, `render_header`:68, `render_number_emitter`:457. `RuntimeEmitterKind{CompiledLowering,RequestFacts}` `grammar_provider.rs:40-42`, `:33 emitter`, `:110` gate; `rg RuntimeEmitterKind ARCHITECTURE.md`=0. |
| CH1-V2-F8 | ACCEPT | 1C census exact: `rg -ln @generated crates/core/src/runtime`=67; leak=12 sites/4 `parse_with.rs`; 7× css_l4 generated.rs md5 `b654562ccff46ed62dd48e9ace325830`; `css_l4.rs` 108406 LOC/191 `parse_` fns; `Tape` SoA `mod.rs:98`; `value.rs:1` "@generated by xtask regen-json". C3 wider-scan cause dropped, U2 reframed (F15 discharged). |
| CH1-V2-F9 | ACCEPT | 1D RESULTS/REDRESS resolve: RESULTS.md twitter parse_only Track1 `8349.290` > sonic strict `4913.095`; citm_catalog `9079.838` > `8335.772`; canada `16709.901` > `12970.929` — exactly "8349>4913 / 9079>8335 / 16709>12970". `REDRESS.md:126` "Tape/direct-to-struct remains one substrate"; `json/generated.rs:801,841,881` carry `parse_w11_1_number_{direct,object_direct,array_direct}` (7 total); `find_component_delim` fn at css_l4 generated.rs:657. |
| CH1-V2-F10 | ACCEPT | 1E strongest inventory. 16 lock headers at `LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`; `LOCKS.md:620` "The `G:EventGrammar` type parameter is the generality vehicle" verbatim; `:408-409` "returns 67 … asserted Pattern H total"; `Cargo.toml:81 lto="thin"` vs `skinny/Cargo.toml:80 lto="fat"`; `egraph/Cargo.toml:11 csp-solver="0.1"`; `bbnf-simd/src/lib.rs:5 pub mod x86_64`; x86=28; Pattern H=71; `css_l4/builder.rs` 817 LOC, `enum OpenFrame<'p>` `:16`; full `bbnf-bench` lock14 path. F12+F16 discharged. |
| CH1-V2-F11 | ACCEPT | 1F-coherence verifies: `HANDOFF.md:17-19` totality-adopt; `ARCHITECTURE.md:2003-2005` "by-construction … 24-LOC stub cannot serve as exercise"; `MASTER-PLAN.md:519` "Nine seed grammars build through new template"; `css_types.rs:1` host-shim, 66 LOC; `ARCHITECTURE.md:1932` "67 hand-written files across 9 grammar dirs"; `strategy.rs` idents `:137`(JSON)/`:143`(GoogleSheets)/`:149`(CSS)/`:155`(BBNF) — V1 `:138` off-by-one GONE; `simd-scan/src/lib.rs:68 next_structural_at_or_after`; `support.rs:67` "The probe substrate (OnceCell + helper)"; `json.rs:701` OnceCell<StructuralIndex>. |
| CH1-V2-F12 | ACCEPT | 1F-anti LOCs verify at HEAD: report 11863, gate 6175, lock14 5095, generated_real_typed 4941, nonjson_css_l4 3737, grammar/lib 2052, passes/lib 2025, runtime_generator 1611, codegen/lib 1473, json/generated 1235; `codegen/src/lib.rs:4 mod json_sink_direct; :5 mod json_typed_direct;`; `json_templates/` carries config/generated/parser/value/view/visitor; strategy idents `:137/:143/:149/:155`; `grammar_facts.rs:799` comment `BbnfBootstrap::parse`; `google_sheets` file census=10. |

## Evidence Checked (spot-verifications run this pass, all at HEAD 4e4aa0648, dirty tree)

- IR/cost: `skinny/crates/ir/src/lib.rs:211-237,340-346,355-401`; `cost.rs:57,139,334`.
- Tape: `skinny/crates/runtime/src/tape/mod.rs:94,98,170,175,178,179,227`.
- Codegen: `lower/mod.rs:1,18-26`; `lower/{eager,offset,event}_tape.rs` + `lower/collapsed_stage.rs` (17 LOC each, `:16` render_rule); `tape_plan.rs:58`; `sink_only.rs:122`; `rust.rs:32,112`; `passes/src/lib.rs:329,392,473`; `grammar_provider.rs:33,40-42,110`; `runtime_generator.rs:16,91,195,665,701`; `json_sink_direct.rs:4,68,124,138-145,251,326,457,497`; `backend_egraph.rs:9`; `xtask/src/regen.rs:5,6,7-18,72-74`.
- Runtime census: `rg -ln @generated crates/core/src/runtime`=67; leak 12/4; 7× css_l4 md5; `css_l4.rs` 108406/191; `google_sheets`=10; Pattern H 71.
- Locks/spec: `LOCKS.md` 75,160,170,179,181,183,200,202,260,269,319,328,336,349,408-409,436,453,620; `ARCHITECTURE.md:1932,2003-2005`; `HANDOFF.md:17-19`; `MASTER-PLAN.md:519`.
- lock14 + 1D ledgers: `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2409,2442,2455,2456,2463`; `skinny/RESULTS.md` twitter/citm/canada parse_only rows; `skinny/REDRESS.md:126`; `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf:36-51,67` vs `grammar/google-sheets/google-sheets.bbnf:103-121,137`.
- 1F totality: `crates/ir/src/registry/strategy.rs:137,138,143,144,149,150,155,156`; `crates/core/src/css_types.rs:1`; `crates/ir/src/passes/recognizers/grammar_facts.rs:799`; `crates/simd-scan/src/lib.rs:68`; `crates/core/src/grammar/generated/json.rs:701`; `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`.
- Provenance: `git rev-parse HEAD`=4e4aa064835b0bf8f7e25113edb40f3a9e01b866; 8 inventories uncommitted (M), mtimes 2026-06-01 15:50-15:52.

## Artifact Dispositions

| Artifact | CH1 V2 disposition | Notes |
|---|---|---|
| `1A-substrate-evidence.md` | REVISE | IR/tape/cost rows resolve; V1-F14 `:170` reversal CORRECT and disk-true; frontmatter tally does not sum to compound-label table (F3). |
| `1B-codegen-evidence.md` | REVISE | Lowerer/fork/render rows verify; F13 `regen.rs:5` expanded correctly; brace-shorthand `collapsed}_tape.rs` names a non-existent file (F2). |
| `1C-runtime-evidence.md` | ACCEPT | Census/md5/LOC/leak exact; F15 discharged; U2 reframed. Cleanest core inventory. |
| `1D-skinny-lessons.md` | REVISE | F10/F11 discharged; RESULTS/REDRESS exact; bare `lock14_baseline.rs` path unresolved from root (F4). |
| `1E-locks-evidence.md` | ACCEPT | 16-lock + every impl claim verifies; F12 + F16 discharged; full `bbnf-bench` lock14 path. |
| `1F-coherence-scan.md` | ACCEPT | Every sampled cite resolves; V1 `:138` ident off-by-one gone (now true `:137/:143/:149/:155`). |
| `1F-anti-pattern.md` | ACCEPT | Every LOC/symbol verifies; google_sheets=10 fold correct; strategy idents correct. |
| `1F-past-corpora.md` | REVISE+REJECT | F13/F16 discharged; `:17-18`=`entry_rule`/`source_roots` re-anchor is FALSE (F1 REJECT); bare `nonjson_css_l4.rs:3091` ambiguous (F5). |

## Required V3 Fold

1. 1F-past-corpora: REPLACE the false `1F-past:55` claim "`:17-18` are the `entry_rule`/`source_roots` fields" with "`:17-18` are `frontend_requirements`/`output_labels`; `entry_rule`/`source_roots` are at `:9-10`" (or drop the `:17-18` re-anchor). Do NOT re-inherit from V1 F13.
2. 1B: change `lower/{eager,offset,event,collapsed}_tape.rs` (`:59,:82`) to `lower/{eager,offset,event}_tape.rs + lower/collapsed_stage.rs`.
3. 1A: reconcile the frontmatter `divergence_count` integers with the table's compound verdict labels.
4. 1D: prefix the three `lock14_baseline.rs:{2409,2442,2463}` citations (`:64,:103,:197`) with `skinny/crates/bbnf-bench/src/`.
5. 1F-past-corpora (+ sibling bare cites 1E:88 / 1F-anti:25): disambiguate `nonjson_css_l4.rs:3091` to `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091`.

TALLY accept=7 revise=4 reject=1

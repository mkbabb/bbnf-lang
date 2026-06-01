---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V4
sk_cycle: SK-V18
disposition: REVISE
head_at_review: 4e4aa064835b0bf8f7e25113edb40f3a9e01b866
working_tree: dirty (8 inventories re-folded 2026-06-01 16:23-16:28, uncommitted M)
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

# CH1 Correctness Audit — T-P1 V4 (SK-V18 Totality Excavation, cycle V4)

## Verdict

REVISE. The V4 fold discharged ALL FOUR of the V3 CH1 required folds (F9-F12),
and I re-verified each on disk at HEAD `4e4aa0648`. I spot-verified a wide
load-bearing surface across all six live inventories and EVERY cited
symbol/text/LOC/md5/line resolved — the IR/tape/cost spine, the codegen two-emitter
fork, the 67/71 Pattern-H census, the 7x css_l4 md5 family, the `parse_w11_1_number`
x7 occurrences, the RESULTS twitter/citm/canada parse_only rows, the REDRESS
line-ranges, the 16 lock headers, the `lto` profile pair, the SK-V18 SPEC LAC
anchors, and the ARCHITECTURE/MASTER-PLAN/LOCKS spec-surface text. No whole
inventory is rejected; no recalled/fabricated symbol surfaced this pass; the
brace-path grep and the stale-cycle ("Cycle is V3"/"this V3 inventory") grep both
return ZERO.

The cycle cannot ACCEPT under CH1 because a close reading surfaced three residual
defects prior cycles did NOT catch: (1) the 1F-coherence frontmatter
`divergence_count` (3/13/2) does not reconcile to its 14-row spec-claim table and
carries no row-to-bucket enumeration (unlike 1A, which sums to 26 auditably); (2)
1D cites `1E-locks-evidence.md:89` THREE times as the "L10 stressor", but `1E:89`
is the L09 row — the L10 row it means is at `1E:90` (off-by-one); (3) bare /
ambiguous shorthand path:line cites remain in 1D and 1F-coherence that do not
resolve from repo root, despite 1D's own CH1-V2-F4 prefix discipline applied to
`lock14_baseline.rs` elsewhere in the same file.

Scope was the six live inventories plus the two 1F auxiliaries (which are
CURRENT-PASS `cycle: V5-SKV18-totality` rewrites and ARE live T-P1 authority per
the CH5-V3-008 correction, not historical).

## V3 Fold Discharge Audit (did the required V4 folds land?)

| V3 item | required fold | V4 status | disk evidence |
|---|---|---|---|
| F9 (1B) | prefix bare `backend_egraph.rs:9`/`decision_csp.rs:151,:265` with `skinny/crates/passes/src/` | DISCHARGED | `1B:62` now "`skinny/crates/passes/src/backend_egraph.rs:9` … (root-relative path per CH1-V3-F9)"; `1B:124,:156` carry the full `passes/src/` prefix. Disk: both files live ONLY at `skinny/crates/passes/src/` (no `codegen/` copy); `backend_egraph.rs:9 const REWRITE_SET = "sk-v15-w7-direct-sink-normalization-v1"`, `decision_csp.rs:151 selected_rule_count: u32::from(csp_status == "sat")`, `:265 assert_eq!(csp.csp_status, "sat")` — verbatim. |
| F10 (1A) | re-anchor `json_typed_direct.rs:56`→`:671` field / `:668` struct | DISCHARGED | `1A:109,:186` now cite "`json_typed_direct.rs:671` (the `cursor: usize` field inside `struct DirectParser<'i>` at `:668` … re-anchored per CH1-V3-F10: `:56` was the `DirectParser::new(input)` instantiation, not the field)". Disk: `:668 const PARSER_RUNTIME: &str = r#"struct DirectParser<'i> {`, `:671     cursor: usize,`, `:361 out.push_str("    let checkpoint = parser.cursor;\n")`, `:56 let mut parser = DirectParser::new(input);` — all four anchors exact. |
| F11 (1F-coherence) | add `strategy.rs:216` consumer anchor to COH18-005/012 | DISCHARGED | `1F:75` (COH18-005) and `1F:82` (COH18-012) now both carry "`for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)` at `crates/ir/src/registry/strategy.rs:216` (consumer anchor … per CH1-V3-F11; … routed to `U-COH18-001`)". Disk: `strategy.rs:216 Self::for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)` — verbatim. |
| F12 (1B/1E/1D) | anchor `runtime_target_rows_collapsed` to SK-V18 SPEC, mark PLANNED | DISCHARGED | `1B:109` "the PLANNED `runtime_target_rows_collapsed` co-gate, defined in SK-V18 SPEC at `restart/skinny/tranches/sk-v18/SPEC.md:247` — `rg … skinny/crates skinny/xtask` returns NO live definition"; `1E:147` and `1D:97` carry the same PLANNED framing. Disk: `SPEC.md:247 runtime_target_rows_collapsed (bool; addendum 2/R16 …)`; `rg runtime_target_rows_collapsed skinny/crates skinny/xtask` = 0. |

## Findings

| ID | Disposition | Severity | Evidence |
|---|---|---|---|
| CH1-V4-F1 | ACCEPT | none | All four V3-required folds (F9-F12) landed on disk-true evidence, not copy. Verified verbatim at HEAD: `skinny/crates/passes/src/{backend_egraph.rs:9,decision_csp.rs:151,:265}` (home dir = `passes/src/`, no `codegen/` copy); `skinny/crates/codegen/src/json_typed_direct.rs:56,361,668,671`; `crates/ir/src/registry/strategy.rs:216`; `restart/skinny/tranches/sk-v18/SPEC.md:247` + `rg runtime_target_rows_collapsed`=0. |
| CH1-V4-F2 | ACCEPT | none | 1A IR/tape/cost spine + 26-sum frontmatter reconcile EXACTLY. `BackendShape` 5 at `ir/src/lib.rs:340-345`; `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>` `tape/mod.rs:175`, `_kind:PhantomData<fn()->K>:178`, `_grammar:…:179`; `SubstrateTarget` `cost.rs:57`, `all_backend_shapes()->[BackendShape;5]` `:334`; JSON `W7` triad `json/config.rs:22-26` verbatim; `attach_structural_index` NO-OP `generated.rs:12-15`; `scan.rs:22 scan_structurals`/`:47 structural_capacity_for`. Frontmatter buckets (impl 7, unimpl 8, exceeds 1, unknown 3, partial 5, diverges 2) each ENUMERATE their SUB-row IDs and sum to 26 = the 26-row table, cell-by-cell. |
| CH1-V4-F3 | ACCEPT | none | 1B codegen fork spine verifies. `select_lowering` `lower/mod.rs:18`; four lowerers EXACTLY 17 LOC each (`eager/offset/event_tape.rs`+`collapsed_stage.rs`; `collapsed_tape.rs` ABSENT); `RuntimeEmitterKind{CompiledLowering,RequestFacts}` `grammar_provider.rs:40-42`; dispatch `runtime_generator.rs:16` (`match request.profile_contract.emitter`), `RequestFacts => emit_request_facts` `:25`; CSS const courier `CSS_GENERATED_RS` `runtime_generator.rs:701`, `normalize(CSS_GENERATED_RS)` `:91`. |
| CH1-V4-F4 | ACCEPT | none | 1C census exact. `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs'` = 71; per-grammar (excl. `tape/`) = 67; +4 = `tape/{mod,cursor,arena,record}.rs`; 67 `@generated` headers; `css_l4.rs` 108406 LOC / 191 `parse_` fns; 7x `css_l4_*/generated.rs` md5 `b654562ccff46ed62dd48e9ace325830`; `parse_w11_1_number` = 7 occurrences (`json/generated.rs:801,841,881,955,1007,1019,1031`), cited call sites `:801,841,881` carry `_direct/_object_direct/_array_direct` verbatim; `css_l4_*/config.rs:2-4` ROW_ID/REQUEST_PROFILE/ENTRY_RULE verbatim. |
| CH1-V4-F5 | ACCEPT | none | 1D RESULTS/REDRESS resolve. RESULTS twitter parse_only `8349.290 > 4913.095`, citm `9079.838 > 8335.772`, canada `16709.901 > 12970.929` — "8349>4913 / 9079>8335 / 16709>12970" exact; REDRESS.md (6465 LOC) `:126 "5. Tape/direct-to-struct remains one substrate"` verbatim; cited ranges `:6356-6414` (SK-V15 W8 lowerer section) resolve; both google-sheets `.bbnf` copies present; lock14 `:2409/:2442/:2463` full `skinny/crates/bbnf-bench/src/` path (CH1-V2-F4 prefix honoured). |
| CH1-V4-F6 | ACCEPT | none | 1E + spec surfaces resolve. 16 lock headers + `Cargo.toml:81 lto="thin"` / `skinny/Cargo.toml:80 lto="fat"`; LAC SPEC anchors `sk-v18/SPEC.md:247,:1115,:1202,:1254` (1662 LOC) resolve. Spec-claim text verbatim on disk: `ARCHITECTURE.md:1932 "67 hand-written files across 9 grammar dirs"`, `:1997 "G:EventGrammar"`, `:2010 "Direct builders do not bypass the substrate event stream"`; `MASTER-PLAN.md:519 "F.W5 \| Current nine grammar regeneration"`; `LOCKS.md:75` (Lock 1 tape/`&'i Tape<'i>`+cursor), `:349` ("hand-written per-grammar runtime files are forbidden"), `:620` ("The `G:EventGrammar` type parameter is the generality vehicle"). |
| CH1-V4-F7 | ACCEPT | none | Provenance + discipline clean. `git rev-parse HEAD` = `4e4aa064835b0bf8f7e25113edb40f3a9e01b866`; the 8 inventories are uncommitted (M) with V4-refold mtimes 2026-06-01 16:23-16:28 (AFTER the V3 review). Brace-path grep `rg '\{…\}\.(rs\|md\|toml):[0-9]'` = ZERO; stale-cycle grep `rg 'Cycle is V3\|this V3 inventory'` = ZERO over all six live inventories. |
| CH1-V4-F8 | REVISE (1F-coherence) | high | 1F-coherence frontmatter `divergence_count` (`1F:21-24`: spec_claims_implemented 3 / spec_claims_unimplemented 13 / unknown 2 = 18) does NOT reconcile to the authoritative Spec-Claim table (`1F:71-84`, COH18-001..014 = 14 rows). The 14 verdict cells are: 9 `unimplemented` (001-009), 1 `implemented` (010), 1 `plan-coherent` (011, explicitly NOT implemented per CH6-V3-F3), 1 `spec-defect-on-:1643` (012), 1 `implemented (JSON) / directional (CSS)` split (013), 1 `unknown` (014). The frontmatter "13 unimplemented" exceeds the table's 9; "3 implemented" cannot be traced (best case 010 + 013-JSON = 2); "2 unknown" exceeds the table's 1; COH18-011/012 map to no frontmatter bucket. Unlike 1A's frontmatter (which enumerates each SUB-row ID per bucket and sums to 26), 1F carries NO enumeration note, so 3/13/2 is unverifiable. SAME un-auditable class in 1B (`1B:15-19` 6/7/2/3=18 vs 13 visible main-table verdicts + a separate D1-D10/U1-U3 section, no enumeration) and 1C (`1C:11-15` 9/11/2/4=26 vs 12 C-rows + 8 D + 4 U = 24, no enumeration, and DIVERGES verdicts that fit no bucket). CORRECTION: add the 1A-style per-row enumeration note to the 1F-coherence frontmatter (and 1B/1C) so each `divergence_count` integer is traceable to named rows and sums to the table; re-key 3/13/2 to match the 14 COH18 spec-claim rows. |
| CH1-V4-F9 | REVISE (1D) | medium | 1D off-by-one cross-reference to 1E. `1D:123` references "1E-L10" by name; `1D:125` calls it "the L10 stressor (`1E-locks-evidence.md:89`)"; `1D:199` cites `1E-locks-evidence.md:89` TWICE as the "open L10 stressor" / "depth caveat". But disk shows `1E-locks-evidence.md:89` = the **L09** row ("L09 Slice-borrow + bump/owned hatches | `LOCKS.md:260-267`"). The **L10** row 1D means — carrying the exact "the decision-engine load-bearing depth remains the open L10 question; SK-V18 R-E precedence-tower is the un-tested generality stressor (`SYNTHESIS-RESEARCH.md:249-255`)" text 1D attributes — is at `1E:90` ("L10 Auto-detected Pratt/SIMD/materialization | `LOCKS.md:269-274` | over-stated"). The citation resolves to a real line but to the WRONG entry. CORRECTION: re-anchor all three `1E-locks-evidence.md:89` cross-refs (`1D:125`, `1D:199` ×2) to `1E-locks-evidence.md:90` (the L10 row). |
| CH1-V4-F10 | REVISE (1D, 1F-coherence) | medium | Bare / ambiguous shorthand path:line cites remain that do not resolve from repo root, despite 1D's own CH1-V2-F4 prefix discipline (`1D:69,:109,:203` annotate the `lock14_baseline.rs`→`skinny/crates/bbnf-bench/src/` prefix). 1D still carries bare `json/generated.rs:801,841,881` (`1D:72,:113,:191` — real path `skinny/crates/runtime/src/grammars/json/generated.rs`, no prefix in row), bare `grammar_provider.rs:33` (`1D:62,:95,:202` — real path `skinny/crates/codegen/src/`), bare `runtime_generator.rs:16,91,701` (`1D:61,:84,:188`). 1F-coherence `COH18-012` (`1F:99`) cites `grammar_facts.rs:799` and `scalar.rs:17` bare; `scalar.rs` is AMBIGUOUS — it exists at THREE disk locations (`crates/simd-scan/src/scalar.rs`, `crates/ir/src/passes/recognizers/shape_dispatch/scalar.rs`, `crates/core/src/backend/rust/emitter/shapes/scalar.rs`), so `scalar.rs:17` does not disambiguate which file (same disambiguation class as the V3-F5 nonjson `:3091` fold). CORRECTION: prefix the bare 1D `json/generated.rs:`/`grammar_provider.rs:`/`runtime_generator.rs:` cites to their `skinny/crates/...` roots (matching 1D's own CH1-V2-F4 discipline), and disambiguate 1F-coherence `scalar.rs:17` + `grammar_facts.rs:799` to their full `crates/ir/src/passes/recognizers/...` paths. |

## Evidence Checked (spot-verifications run this pass, all at HEAD 4e4aa0648, dirty tree)

- IR/tape/cost: `skinny/crates/ir/src/lib.rs:175,178,179,211,227,340-346`; `cost.rs:57,118,139,334`; `tape/mod.rs:94,170,175,178,179,191`.
- Codegen fork: `lower/mod.rs:18,26`; `lower/{eager,offset,event}_tape.rs`+`collapsed_stage.rs` (17 LOC each; `collapsed_tape.rs` ABSENT); `grammar_provider.rs:40-42`; `runtime_generator.rs:16,25,91,701`.
- Codegen passes (F9): `skinny/crates/passes/src/backend_egraph.rs:9`; `decision_csp.rs:151,265` (home dir `passes/src/`, no `codegen/` copy).
- Codegen direct (F10): `json_typed_direct.rs:56,361,668,671`.
- ir registry (F11): `crates/ir/src/registry/strategy.rs:216`.
- Runtime/JSON: `grammars/json/generated.rs:12-15,760,762,766,767,801,841,881,955,1007,1019,1031`; `json/scan.rs:22,47`; `json/sink.rs:4`; `json/config.rs:22-30`; `json/value.rs:143`; `json/view.rs:68`.
- Runtime census: `find crates/core/src/runtime -mindepth 2 …` = 71 (per-grammar 67, +4 `tape/`); 67 `@generated`; 7x css_l4 `generated.rs` md5 `b654562c…`; `css_l4_declaration_values/{config.rs:2-4,generated.rs:257}`.
- Grammar generated: `crates/core/src/grammar/generated/css_l4.rs` 108406 LOC / 191 `parse_`.
- Locks/spec: `LOCKS.md:75,107,349,408,610,614,616,620,622`; `ARCHITECTURE.md:1932,1997,2010`; `MASTER-PLAN.md:519`; `restart/skinny/tranches/sk-v18/SPEC.md` (1662 LOC) `:247,1115,1202,1254`.
- 1D ledgers: `skinny/RESULTS.md:5-25` (twitter/citm/canada parse_only); `skinny/REDRESS.md` (6465 LOC) `:126,6356`; google-sheets `.bbnf` both copies.
- Cross-ref (F9): `1E-locks-evidence.md:89` = L09 (NOT L10); `:90` = L10.
- Ambiguity (F10): `find crates -name scalar.rs` = 3 locations; `find crates -name grammar_facts.rs` = `crates/ir/src/passes/recognizers/grammar_facts.rs`.
- Provenance: `git rev-parse HEAD`=4e4aa064835b0bf8f7e25113edb40f3a9e01b866; inventories M, mtimes 16:23-16:28; brace-path grep=0; stale-cycle grep=0.

## Artifact Dispositions

| Artifact | CH1 V4 disposition | Notes |
|---|---|---|
| `1A-substrate-evidence.md` | ACCEPT | IR/tape/cost spine + 26-sum frontmatter reconcile cell-by-cell with explicit per-bucket SUB-row enumeration; F10 discharged (`json_typed_direct.rs:671`). The model the other inventories should follow. |
| `1B-codegen-evidence.md` | ACCEPT (with shared count-auditability note) | Fork/lowerer/render spine verifies; F9 + F12 discharged. Frontmatter 6/7/2/3=18 not enumerated against table+D/U sections (shared F8 class), but no provable mismatch. |
| `1C-runtime-evidence.md` | ACCEPT (with shared count-auditability note) | Census/md5/LOC/`parse_w11_1_number`x7 exact. Frontmatter 9/11/2/4=26 not enumerated against 12 C + 8 D + 4 U rows; DIVERGES verdicts fit no bucket (shared F8 class). |
| `1D-skinny-lessons.md` | REVISE | RESULTS/REDRESS resolve; F12 PLANNED framing correct. But off-by-one `1E:89`→`:90` L10 cross-ref ×3 (F9), and bare `json/generated.rs:`/`grammar_provider.rs:`/`runtime_generator.rs:` cites lacking root prefix despite its own CH1-V2-F4 discipline (F10). |
| `1E-locks-evidence.md` | ACCEPT | 16-lock + `lto` pair + LAC SPEC anchors verify; F12 PLANNED co-gate framing correct. (Receives the L09/L10 line as the correct cross-ref target for 1D's F9 fix.) |
| `1F-coherence-scan.md` | REVISE | 9-row strategy table + `strategy.rs:216` consumer (F11) + 67/71 census + spec-surface drift cites all resolve. But frontmatter 3/13/2 does not reconcile to the 14-row COH18 spec-claim table and carries no enumeration (F8); bare/ambiguous `scalar.rs:17`+`grammar_facts.rs:799` cites (F10). |
| `1F-anti-pattern.md` | ACCEPT | LIVE per CH5-V3-008. `_RS` literals, `strategy.rs:216` consumer, `support.rs:67`, lock14 cites verify. |
| `1F-past-corpora.md` | ACCEPT | The V2 REJECT (regen `:17-18`=`frontend_requirements`/`output_labels`; `entry_rule`/`source_roots` at `:9-10`) stays disk-true; nonjson `:3091` disambiguation correct. |

## Required V5 Fold

1. 1F-coherence: re-key the frontmatter `divergence_count` (3/13/2) to the 14-row COH18-001..014 spec-claim table and add an 1A-style per-row enumeration note so each integer is traceable; extend the same enumeration discipline to 1B (6/7/2/3) and 1C (9/11/2/4) so their frontmatter sums are auditable against their two-section (table + D/U) structure.
2. 1D: re-anchor the three `1E-locks-evidence.md:89` "L10 stressor" cross-refs (`1D:125`, `1D:199` ×2) to `1E-locks-evidence.md:90` (the actual L10 row).
3. 1D: prefix the bare `json/generated.rs:801,841,881` (`1D:72,:113,:191`), `grammar_provider.rs:33` (`1D:62,:95,:202`), and `runtime_generator.rs:16,91,701` (`1D:61,:84,:188`) cites with their `skinny/crates/...` roots, matching 1D's own CH1-V2-F4 prefix discipline.
4. 1F-coherence: disambiguate the bare `scalar.rs:17` and `grammar_facts.rs:799` cites at `1F:99` to full `crates/ir/src/passes/recognizers/...` paths (`scalar.rs` is ambiguous across three disk locations).
5. Re-run and require: `rg 'Cycle is V3|this V3 inventory'`=0 (already passes); brace-path grep=0 (already passes); the 1F/1B/1C frontmatter integers each map to enumerated rows.

No REJECT is warranted. No recalled or fabricated symbol was found; every spot-checked
impl/spec path:line carries its claimed symbol/text. The three open CH1 defects are
bounded to one count-reconciliation gap, one off-by-one cross-ref, and residual
root-resolution shorthand — all citation-discipline, not substance.

TALLY accept=7 revise=3 reject=0

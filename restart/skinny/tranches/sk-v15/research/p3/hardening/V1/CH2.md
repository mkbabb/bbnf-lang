# SK-V15 S-P3 V1 CH2 Generality

Verdict: REVISE

Scope reviewed: current HEAD/working tree surfaces for
`restart/skinny/tranches/sk-v15/research/p3/p3a..p3f`,
`restart/skinny/tranches/sk-v15/SPEC.md`,
`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`,
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`,
`restart/prompts/ORCHESTRATOR.md`, and
`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`.

## CH2 Check Summary

| Check | Result | Evidence |
|---|---|---|
| S-P2 survivor boundary stays grammar-neutral | ACCEPT WITH FOLD | P3-A carries the locked S-P2 survivor families and excludes the rejected numeric, EOB, PMULL, CSSC, x86, retained sidecar, schema-builder, harness-hash, and CSS-broadcast routes (`restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md:7`-`:14`, `:20`-`:29`). Preserve that boundary while reindexing/splitting waves. |
| Lock 14 / generic-crate generality gate is executable | REVISE | PASS-3 requires a SPEC `Section 2.1 generality + Lock 14 gate` and requires every generic-crate edit to carry non-JSON proof (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:94`-`:120`). SPEC has Section 2 caps/manifest but no Section 2.1 generality gate (`restart/skinny/tranches/sk-v15/SPEC.md:142`-`:166`). |
| Generic-code waves require non-JSON proof receivers | REVISE | Findings CH2-01 through CH2-04. |
| CSS-specific work is kept out of generic policy | REVISE | W5 permits generic `runtime_generator.rs` / `grammar_provider.rs` edits for a CSS Value API without a non-CSS no-drift proof when those generic files change (`restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:52`; `restart/skinny/tranches/sk-v15/SPEC.md:281`-`:305`). |

## Findings

| ID | Severity | Finding | Evidence | Required fold |
|---|---|---|---|---|
| CH2-01 | Blocker | The S-P3 SPEC does not contain the required generality / Lock 14 gate, so Lock 14 is present as intent but not as an executable per-wave admission rule. | PASS-3 says the SPEC shape includes `Section 2.1 generality + Lock 14 gate` (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:94`-`:100`) and CH2 requires every generic-crate edit to prove a non-JSON receiver (`:116`-`:120`). ORCHESTRATOR binds CH2 to "no grammar-name leak" and no JSON code in generic crates (`restart/prompts/ORCHESTRATOR.md:83`-`:84`, `:201`-`:204`). SPEC Section 2 jumps from phase caps to the wave table and never adds the gate matrix (`restart/skinny/tranches/sk-v15/SPEC.md:142`-`:166`). | Add SPEC Section 2.1 and mirror it in DISPATCH: every wave plan that touches `skinny/crates/codegen`, `skinny/crates/passes`, `skinny/crates/ir`, generic runtime generator/provider files, or SIMD/parser-helper crates must list generic owner path, forbidden grammar tokens, non-JSON receiver (`CSS L4`, `Sheets`, or `BBNF-self`), proof command, generated-output/no-diff expectation, and fail action. |
| CH2-02 | Blocker | W3 loses the non-JSON proof receiver that P3-C correctly requires for generic codegen changes. | P3-C says any generated runtime diff must pass same-wave non-JSON proof for CSS L4, Sheets, or BBNF-self as applicable (`restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:106`-`:110`). P3-B and SPEC W3 own generic codegen, provider, passes, IR, and xtask files (`restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:50`; `restart/skinny/tranches/sk-v15/SPEC.md:238`-`:255`). SPEC W3 only requires no grammar branches and a JSON rerun if JSON-adjacent, and DISPATCH W3 only requires leak grep, generated-output proof, and JSON guard rerun (`restart/skinny/tranches/sk-v15/SPEC.md:250`-`:255`; `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:102`-`:111`). | Fold P3-C's non-JSON receiver rule into SPEC/DISPATCH W3. A W3 redress cannot close with JSON-only guard proof; it must exercise the changed generic code path with CSS L4 and at least one structurally different non-JSON receiver when feasible, or record an intrinsic block explaining why no such receiver exists. |
| CH2-03 | Blocker | W5 can put CSS-specific policy into generic generator/provider code without proving the generic path remains grammar-neutral for non-CSS receivers. | W5 owns `runtime_generator.rs` and `grammar_provider.rs` as part of CSS Value rebuild (`restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:52`; `restart/skinny/tranches/sk-v15/SPEC.md:281`-`:305`). W5 exit proves typed CSS output and JSON guard maintain, but it does not require a Sheets or BBNF-self no-diff/regen proof when those generic files change (`restart/skinny/tranches/sk-v15/SPEC.md:294`-`:301`; `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:123`-`:131`). | Add a W5 generic-edit branch: if only CSS runtime/provider files change, CSS typed proof is sufficient; if generic generator/provider/codegen files change, the plan must also prove non-CSS receiver stability for Sheets or BBNF-self and reject `CssL4`/profile branches in generic code. |
| CH2-04 | Blocker | W6/W7 Decision Engine and BackendShape lowerer waves touch generic selection/lowering surfaces but do not name non-JSON proof receivers. | W6 owns generic passes/IR Decision Engine files and W7 owns generic lowerer/codegen fixtures (`restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:53`-`:54`; `restart/skinny/tranches/sk-v15/SPEC.md:307`-`:343`). SPEC requires grammar-neutral facts and real lowerers, but the evidence can be a generic generated diff or test without CSS/Sheets/BBNF-self receiver proof (`restart/skinny/tranches/sk-v15/SPEC.md:318`-`:323`, `:338`-`:343`; `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:133`-`:151`). | Add W6/W7 receiver matrices. W6 must prove the e-graph/CSP fact set changes selection for at least one non-JSON receiver or fail closed as non-driving. W7 must prove lowerer output through generated fixtures for CSS L4 and one of Sheets or BBNF-self, with EventTape bound to the existing BackendShape and not to a sidecar event stream. |
| CH2-05 | Major | The packet has multiple generality vocabularies, which creates room for alias-only compliance. | P3-B W0 names `sample_count`, `row_claim_scope`, `comparator_workload_id`, `producer_path`, `generator_source_id`, `semantic_output_kind`, and `strictness_source` (`restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:47`), while P3-D/SPEC require `measurement_row_id`, `measurement_origin`, `value_plane`, `css_comparator_workload`, `generator_source`, `lock14_scan_scope`, `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and `broadcast_group_id` (`restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:21`-`:50`; `restart/skinny/tranches/sk-v15/SPEC.md:94`-`:116`). | Normalize to the P3-D/SPEC field names. Aliases must reject unless the schema is deliberately bumped and the gate consumes the mapping. |

## Required Folds

1. Add SPEC Section 2.1 and DISPATCH mirror language for Lock 14 generality: every generic-crate edit must name a non-JSON receiver, proof command, forbidden grammar-name tokens, and fail action.
2. Fold P3-C's non-JSON proof rule into W3 so generic codegen leak repair cannot close on JSON guard proof alone.
3. Add W5 generic-edit branching: CSS-only owner paths may close with CSS typed proof; generic generator/provider edits additionally require Sheets or BBNF-self stability proof.
4. Add W6/W7 receiver matrices for Decision Engine facts and BackendShape lowerers, with CSS L4 plus Sheets or BBNF-self generated fixture proof before any generic close.
5. Normalize telemetry/generalization vocabulary to the exact P3-D/SPEC field names and reject alias-only compliance.

CH2 remains REVISE until those folds make the non-JSON receiver proof executable in SPEC and DISPATCH, not only present in upstream planning prose.

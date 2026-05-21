# SK-V13 S-P3 V1 CH1 Correctness Challenge

Pass: S-P3 Synthesis-Plan.
Cycle: V1.
Date: 2026-05-21.
Lens: CH1 correctness.
Output: `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH1.md`.

## Verdict

REVISE.

The P3 packet is directionally correct on strict same-plane gates, SK-V13-open
baseline discipline, and G-Omega pre-W0 preservation. It cannot converge in V1
because P3-F, SPEC, and DISPATCH still claim P3-B through P3-E were absent even
though those artifacts now exist, and because the current SPEC/DISPATCH wave map
does not fold P3-B/P3-C/P3-D/P3-E into one consistent dispatch contract.

## Evidence Table

| Check | Evidence | Finding | Disposition |
|---|---|---|---|
| CH1 contract | S-P3 CH1 requires every shortlist candidate to trace to S-P2 and S-P1, every falsifiability gate to name corpus rows and concrete Mbps thresholds, every exit gate to compare against `SK-V13-open`, and strict comparator planes (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:110`-`:114`). Orchestrator CH1 requires cited claims, measurable gates, and strictness-plane deltas (`restart/prompts/ORCHESTRATOR.md:83`). | Review standard is explicit and applicable. | ACCEPT |
| Stale P3 integration | P3-B, P3-C, P3-D, and P3-E exist (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:1`, `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:1`, `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:1`, `restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:1`). P3-F nevertheless says only P3-A is present and P3-B through P3-E are absent (`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:27`-`:32`, `:174`). SPEC and DISPATCH repeat the same stale claim (`restart/skinny/tranches/sk-v13/SPEC.md:5`-`:8`; `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:26`-`:28`). | This is a live correctness defect. The draft authority surface misstates its inputs and lets SPEC/DISPATCH diverge from the current P3-B/P3-C/P3-D/P3-E artifacts. | REVISE |
| Candidate trace to S-P2/S-P1 | P3-A lists P3A-1 through P3A-7 with owner paths, scalar/checkasm state, same-wave consumers, grammar-neutral verdicts, rows, and thresholds (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:63`-`:72`). Those families map to P2-F CSS row scopes, generated policy/sink surfaces, byte-set/string/escape/number primitives, decision-engine surfaces, and union C1/C2/C3 (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:69`-`:87`, `:99`-`:106`, `:151`-`:166`). P1 supplies the non-admissive antecedents: 51-row inventory, 10 missing typed surfaces, CSS timer/fact-sink profile, direct unicode primitive, and mode-III scan signal (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:50`-`:55`, `:102`-`:111`; `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:106`-`:128`; `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:115`-`:131`). | P3A-1 through P3A-7 trace adequately. P3A-0 is different: it is a G-Omega/SK-V13-open telemetry gate family (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:65`), not an S-P2 intervention candidate. It is required, but it should be classified as W0 governance substrate, not counted as an S-P2 shortlist survivor. | REVISE |
| Measurable gates | P3-C defines executable formulas for `json_admit`, `css_admit`, row movement, full-table maintain, strict guard, and support-only rejection (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:64`-`:72`). It names all JSON corpus rows and formulas (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:410`-`:424`) and all CSS feature rows/formulas (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:436`-`:451`). P3-A also carries current seed JSON/CSS threshold tables (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:88`-`:106`). | Gate math is generally measurable: named rows, strict formulas, maintain budgets, and revert protocols exist. | ACCEPT |
| Strict planes | P3-C rejects lossy/permissive/different-plane anchors and binds JSON to sonic-rs strict same corpus/output plane and CSS to lightningcss strict same fact-stream plane (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:28`-`:44`, `:64`-`:71`). SPEC repeats strict comparator classes and strict-only non-negotiables (`restart/skinny/tranches/sk-v13/SPEC.md:79`-`:89`, `:247`-`:258`). DISPATCH repeats JSON and CSS strict same-plane requirements (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:78`-`:96`). | Strict plane discipline is present. | ACCEPT |
| SK-V13-open baseline | P3-C says `before` is the W0 `SK-V13-open` row, and copying pre-W0 numbers after W0 is a gate failure (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:74`-`:77`). SPEC requires W0 to regenerate a gate-consumed `SK-V13-open` threshold table instead of stale-copying current sonic anchors (`restart/skinny/tranches/sk-v13/SPEC.md:220`-`:223`) and defines W0 capture/exit gates (`restart/skinny/tranches/sk-v13/SPEC.md:350`-`:390`). | The global baseline rule is correct. One local SPEC gate still hardcodes the old declaration-values threshold (`restart/skinny/tranches/sk-v13/SPEC.md:414`-`:415`) instead of expressing the P3-C maintain formula against `SK-V13-open`. | REVISE |
| G-Omega pre-W0 | SYNTHESIS makes G-Omega a hard pre-W0 gate and blocks Wave 0/source/RESULTS/REDRESS work until it closes (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:112`-`:122`). SPEC and DISPATCH preserve the same block (`restart/skinny/tranches/sk-v13/SPEC.md:27`-`:38`, `:890`-`:906`; `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:30`-`:41`). HANDOFF also states W0 remains blocked before G-Omega (`restart/skinny/tranches/sk-v13/HANDOFF.md:54`-`:91`). | G-Omega pre-W0 is preserved. | ACCEPT |

## Exact Fold Actions

1. Remove the stale P3-B/P3-C/P3-D/P3-E absence language from P3-F, SPEC, and DISPATCH. Replace it with an authority list that names P3-A, P3-B, P3-C, P3-D, and P3-E as current V1 inputs and states which sections each one controls.

2. Reconcile the wave map to one source of truth. Prefer folding P3-B's 12-slot top-level manifest into SPEC/DISPATCH: W0 telemetry; W1-W2 CSS; W3-W7 decision fold; W8 union plus zero-orphan/SIMD disposition; W9 direct plus typed product rows; W10 parse_only; W11 close/bracket. Collapse current SPEC W1-W4/W10.N into P3-B W1-W2, SPEC W5-W7 into P3-B W3-W7, SPEC W9/W12 into P3-B W8, SPEC W11/W13 into P3-B W9, SPEC W14.N into P3-B W10, and SPEC W15 into P3-B W11. If the longer W0-W15 SPEC map is intentionally retained instead, P3-B must be revised to match it before convergence.

3. Reclassify P3A-0 as `W0-GOVERNANCE-SUBSTRATE`, not an S-P2 intervention candidate. Keep it required by S-P3 baseline/telemetry and G-Omega rules, but exclude it from "every shortlist candidate comes from S-P2" accounting. Add a trace matrix for P3A-1 through P3A-7: P3A id -> S-P2 row(s) -> S-P1 antecedent -> limitation/fresh-evidence requirement.

4. Fold P3-C's formulas into every SPEC/DISPATCH wave gate using the same wave ids selected in action 2. In particular, replace the fixed W1 declaration-values maintain threshold with `Track1_after >= max(lightningcss_open + 1.0, 0.98 * SK-V13-open Track1)` plus strict equality, and require every new CSS/JSON threshold to derive from W0 same-run anchors.

5. Fold P3-D's telemetry schema into SPEC Section 0.4 and DISPATCH required packets, including `row_state`, `source_commit`, `consumer_gate`, `g_omega_status`, CSS feature ids/statuses, domain extension blocks, and the gate-json rejection rules for missing row universe, stale run ids, mixed planes, producer-only fields, SIMD orphans, union stale routes, decision-engine paper close, generated LOC opacity, and rolling demotion.

6. Fold P3-E's wave-family REDRESS matrix into SPEC Section 20 and DISPATCH pre-blocks. The current coarse P3-F pre-block list is directionally right, but the converged dispatch contract must carry the per-wave receiver mapping and reopen-only conditions from P3-E.

7. After actions 1-6, rerun the S-P3 CH1 pass against the folded SPEC/DISPATCH to confirm there are no orphan REVISE items and no gate labels still referring to stale P3-F-only wave names.

## S-P3 Convergence Block

Blocks S-P3 convergence: yes.

Reason: this is an unresolved REVISE under the S-P3/Orchestrator folding rule.
The substantive gate design is close, but SPEC and DISPATCH cannot be accepted
while they assert that existing P3-B through P3-E inputs are absent and while the
wave ids, telemetry schema, pre-block matrix, and one baseline-maintain formula
remain only partially folded. Implementation remains blocked regardless because
G-Omega pre-W0 is preserved.

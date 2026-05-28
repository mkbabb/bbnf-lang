# SK-V15 S-P3 V1 CH1 Correctness

Verdict: REVISE

Scope reviewed: commit `4fe37c042` for `restart/skinny/tranches/sk-v15/SPEC.md`,
`DISPATCH-PROMPT.md`, and `research/p3/p3a..p3f`; current
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`,
`restart/prompts/ORCHESTRATOR.md`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`, and
`restart/skinny/tranches/sk-v15/{SYNTHESIS.md,HANDOFF.md}`.

## CH1 Check Summary

| Check | Result | Evidence |
|---|---|---|
| Every shortlist candidate traces to S-P2 and S-P1 | ACCEPT with no fold | P3-A shortlists exactly eight rows and cites S-P2 survivor rows per candidate (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md:20`-`:29`). S-P2 V3 says the survivor set still traces to P1 hot-leaf evidence (`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:21`-`:24`) and bounds S-P3 to the accepted survivor families (`:31`-`:45`). P2-F maps those survivor families to P1 antecedents in its candidate table (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:24`-`:58`). |
| Falsifiability gates are measurable with named rows and thresholds | REVISE | Findings CH1-01, CH1-02, and CH1-04. |
| Every wave compares against `SK-V15-open` | REVISE | Finding CH1-02. |
| Strict-plane/comparator deltas are coherent | REVISE | Finding CH1-03. |

## Findings

| ID | Severity | Finding | Evidence | Required fold |
|---|---|---|---|---|
| CH1-01 | Blocker | P3-C's gate map is stale relative to the actual W0-W9 wave plan, so several SPEC waves do not have an unambiguous P3-C falsifiability gate. | P3-B defines `W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9` (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:15`-`:19`) and its table assigns W6 spine, W7 lowerers, W8 FNV, and W9 close (`:45`-`:56`). SPEC carries the same W0-W9 manifest (`4fe37c042:restart/skinny/tranches/sk-v15/SPEC.md:155`-`:166`). P3-C says P3-B did not exist at authoring time and binds only the expected receiver set (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:12`), then combines Decision Engine spine and all BackendShape lowerers into W6 (`:156`-`:174`) and assigns FNV to W7 (`:176`-`:199`). P3-F claims this is mapped onto the split waves, but gives no concrete per-wave gate table (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md:17`-`:19`). | Rewrite P3-C to the final W0-W9 topology: W6 Decision Engine spine, W7 BackendShape lowerers, W8 FNV quarantine, and W9 close reconciliation. Then update P3-F, SPEC, and DISPATCH-PROMPT to cite those concrete P3-C gates rather than relying on prose remapping. |
| CH1-02 | Blocker | The implementation contract loses the measurable `SK-V15-open` row comparison for multiple waves. P3-C defines M0/M1, but SPEC and DISPATCH exit gates frequently close on prose predicates instead of named rows plus thresholds against `SK-V15-open`. | P3-C defines M0/M1 in terms of `SK-V15-open` (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:23`-`:29`) and says W0 captures the 51 JSON rows as `SK-V15-open` (`:37`-`:43`, `:271`-`:275`). SPEC's wave manifest only gives prose exit gates for W0-W9 (`4fe37c042:restart/skinny/tranches/sk-v15/SPEC.md:155`-`:166`). The individual SPEC exits for W2, W3, W4, W6, W7, W8, and W9 omit explicit `SK-V15-open` row budgets or named threshold rows (`:228`-`:233`, `:250`-`:255`, `:271`-`:276`, `:318`-`:323`, `:338`-`:343`, `:359`-`:365`, `:380`-`:385`). DISPATCH similarly describes close predicates without row thresholds for W2-W9 (`4fe37c042:restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:92`-`:171`). | For every SPEC and DISPATCH wave envelope, add a gate table naming the row universe and threshold: all 51 JSON rows against `SK-V15-open` Track 1 and Track 2, the CSS Appendix A or aggregate rows where applicable, and the strict-product/FNV rows where applicable. Gate-only waves need exact no-behavior/no-Mbps-diff proof or M0; behavior waves need M1 plus their target thresholds. |
| CH1-03 | Blocker | CSS typed Value API gates reuse W8R broadcast/wrong-plane numbers as live typed-output floors, which conflicts with the strict-plane rule. | P3-C states CSS W8R rows are diagnostic until rebuilt with typed output and a same-workload comparator (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:17`-`:18`). P3-D states the W8R residue values are one repeated full-parse run and may only collapse to one diagnostic row or be retimed independently on typed CSS output (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:67`-`:74`). SPEC classifies CSS admission as valid only when output plane, corpus, equality semantics, strictness, and host match (`4fe37c042:restart/skinny/tranches/sk-v15/SPEC.md:79`-`:85`). But P3-A candidate 8 requires a typed CSS row to record `cssparser_mbps=2362.037` and `track1_mbps >= 2319.041` (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md:29`), and P3-C W5 makes `2362.037 Mbps` an explicit floor if the workload remains W8R full production corpus (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:137`-`:144`). | Remove W8R broadcast numbers from live typed CSS admission thresholds. They may remain only as diagnostic negative fixtures. W5 must capture a same-run `cssparser` typed-value/document comparator after Track 1 emits typed CSS output, then require Track 1 to meet or beat that same-plane comparator with unique `measurement_row_id` values for any admitted feature rows. |
| CH1-04 | Major | P3-A marks its candidate Mbps thresholds as pre-W0 planning floors and delegates final binding to P3-C, but P3-C never produces the per-candidate `max(listed floor, SK-V15-open factor)` gate table. | P3-A says all candidate throughput thresholds are pre-W0 planning floors and that P3-C must rebind final thresholds to `SK-V15-open` (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md:16`). The eight candidate rows then list named targets and floors (`:20`-`:29`). P3-C provides global M0/M1 rules (`4fe37c042:restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:23`-`:29`) and wave-level gates, but it does not carry a per-candidate rebinding table for candidates 1-8. | Add a P3-C candidate-gate fold table for P3-A candidates 1-8. Each row should name the candidate, target rows, guard rows, final formula using `SK-V15-open`, same-wave consumer, scalar/oracle/parity requirement, and reject/demotion action if the target rows do not move or the full table misses maintain. |

## Required Folds

1. Regenerate P3-C against the actual W0-W9 topology and remove the stale "P3-B does not exist" assumption.
2. Inline measurable row gates into SPEC and DISPATCH for every W0-W9 exit gate, with explicit `SK-V15-open` comparison.
3. Fix CSS typed-output thresholds so W8R broadcast values are diagnostic only, never typed-admission floors.
4. Finalize the P3-A candidate thresholds in P3-C using `SK-V15-open` formulas and same-wave consumer bindings.

CH1 remains REVISE until those folds land.
